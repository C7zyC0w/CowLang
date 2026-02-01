import tkinter as tk
from tkinter import filedialog, messagebox
import subprocess
import threading
import os
import time
import re
import sys
import shutil
import tempfile
import webbrowser
import textwrap

# =========================
# Settings
# =========================
INDENT = "  "  # 2 spaces for auto-indentation and block bodies

# === CONFIG ===
COWLANG_EXE = r"C:\Users\kaise\Documents\GitHub\CowLang\CowLangTranslator\CLT 0.2.1.exe"
COWLANG_DIR = os.path.dirname(COWLANG_EXE)

# Explicit R & Python paths (user-provided)
RSCRIPT_EXE = r"C:\Program Files\R\R-4.5.2\bin\x64\Rscript.exe"

# If this folder does not contain python.exe, we'll fall back to sys.executable
PYTHON_EXE_DIR = r"C:\Users\kaise\AppData\Local\Python\pythoncore-3.14-64"
PYTHON_EXE = os.path.join(PYTHON_EXE_DIR, "python.exe")

# === COWLANG SYNTAX ===
KEYWORDS = [
    # program structure
    "prog", "program", "end", "end prog",

    # io
    "print", "read",

    # control flow
    "if", "then", "else", "elif",
    "for", "do", "dowhile", "while",

    # defs (future)
    "def", "subr", "class",

    # types / variables
    "string", "str",
    "integer", "int",
    "real",
    "floatation", "float",
    "booleon", "bool",
    "character",

    # programming languages (highlighting convenience)
    "cl", "cl2", "clp", "r", "python", "c", "cpp", "cs", "java",
    "javascript", "js", "ruby", "perl", "swift", "go",

    # misc
    "implicit", "none",
    "stop", "return", "use", "using"
]

# --------------------------------------------------------------------------------------
# Helpers for R/Python discovery
# --------------------------------------------------------------------------------------
def find_rscript():
    if RSCRIPT_EXE and os.path.exists(RSCRIPT_EXE):
        return RSCRIPT_EXE
    rscript = shutil.which("Rscript")
    if rscript:
        return rscript
    candidates = [
        r"C:\Program Files\R\R-4.5.2\bin\x64\Rscript.exe",
        r"C:\Program Files\R\R-4.5.2\bin\Rscript.exe",
        r"C:\Program Files\R\R-4.5.1\bin\x64\Rscript.exe",
        r"C:\Program Files\R\R-4.5.0\bin\x64\Rscript.exe",
    ]
    for c in candidates:
        if os.path.exists(c):
            return c
    return None

def find_python_exe():
    if PYTHON_EXE and os.path.exists(PYTHON_EXE):
        return PYTHON_EXE
    return sys.executable

# --------------------------------------------------------------------------------------
# Quoting-aware comment stripping for parsing and highlighting
# '!' starts a comment only when OUTSIDE quotes.
# --------------------------------------------------------------------------------------
def code_before_bang(line: str) -> str:
    in_s = False  # inside single quotes
    in_d = False  # inside double quotes
    esc = False
    for idx, ch in enumerate(line):
        if esc:
            esc = False
            continue
        if ch == '\\':
            esc = True
            continue
        if ch == "'" and not in_d:
            in_s = not in_s
            continue
        if ch == '"' and not in_s:
            in_d = not in_d
            continue
        if ch == '!' and not in_s and not in_d:
            return line[:idx]
    return line

# --------------------------------------------------------------------------------------
# Inline language extraction (generic)
# Supports:
#   - "use <pl>" declaration (stripped, no execution)
#   - "using <pl> do" ... "end <pl>"
#   - "<pl>.<code>" single line anywhere
# Applies detection on the code part BEFORE '!' (outside quotes); preserves comments for CowLang.
# --------------------------------------------------------------------------------------
LANG_NAME = r'[A-Za-z_]\w*'

def extract_lang_actions(src: str):
    lines = src.splitlines(True)  # keep line endings
    out_lines = []
    actions = []

    i = 0
    while i < len(lines):
        full_line = lines[i]
        code_line = code_before_bang(full_line)

        # 1) Declarations: "use <pl>" (strip only)
        m_decl = re.match(rf'^\s*use\s+({LANG_NAME})\s*$', code_line, re.IGNORECASE)
        if m_decl:
            i += 1
            continue

        # 2) Block form: "using <pl> do" ... "end <pl>"
        m_block_start = re.match(rf'^\s*using\s+({LANG_NAME})\s+do\s*$', code_line, re.IGNORECASE)
        if m_block_start:
            lang = m_block_start.group(1).lower()
            i += 1
            block = []
            while i < len(lines):
                inner_full = lines[i]
                inner_code = code_before_bang(inner_full)
                if re.match(rf'^\s*end\s+{lang}\s*$', inner_code, re.IGNORECASE):
                    i += 1  # consume terminator
                    break
                # Keep raw inner lines (they belong to the third-party language)
                block.append(inner_full.rstrip("\r\n"))
                i += 1
            actions.append({"lang": lang, "lines": block})
            continue

        # 3) Single line prefixed: "<pl>.<code>"
        m_single = re.match(rf'^\s*({LANG_NAME})\.(.+)$', code_line, re.IGNORECASE)
        if m_single:
            lang = m_single.group(1).lower()
            code = m_single.group(2).strip()
            if code:
                actions.append({"lang": lang, "lines": [code]})
            i += 1
            continue

        # Otherwise feed through to CowLang translator (preserve original line incl. comments)
        out_lines.append(full_line)
        i += 1

    stripped_src = "".join(out_lines)
    return actions, stripped_src

# --------------------------------------------------------------------------------------
# Python import hoisting: persist imports into next Python block
# --------------------------------------------------------------------------------------
def is_python_import_line(s: str) -> bool:
    """True if s looks like a Python import statement."""
    return re.match(
        r'^\s*(import\s+[\w\.,\s]+(\s+as\s+\w+)?|from\s+[\w\.]+\s+import\s+.+)$',
        s
    ) is not None

def fuse_python_imports(actions):
    """
    Hoist standalone python import lines forward into the next python action,
    so earlier `python.import ...` become available to a later `using python do` block,
    even if an R action is in between.
    """
    fused = []
    buffer_imports = []
    for act in actions:
        if act['lang'] == 'python':
            if act['lines'] and all(is_python_import_line(l) for l in act['lines']):
                buffer_imports.extend(act['lines'])
                continue
            if buffer_imports:
                act = {'lang': 'python', 'lines': buffer_imports + act['lines']}
                buffer_imports = []
            fused.append(act)
        else:
            fused.append(act)
    if buffer_imports:
        fused.append({'lang': 'python', 'lines': buffer_imports})
    return fused

# --------------------------------------------------------------------------------------
# Executors
# --------------------------------------------------------------------------------------
def run_r_block(lines, log_fn, base_dir=None):
    """
    Runs R code via Rscript.
    - Detects plotting and captures to PNG.
    - Supports: save("file.png") sugar.
    """
    rscript = find_rscript()
    log_fn(f"[R] using: {rscript if rscript else 'NOT FOUND'}")
    if not rscript:
        log_fn("ERROR: Rscript.exe not found. Update RSCRIPT_EXE.")
        return 1

    # save("...") sugar (proper capture of quoted filename)
    save_path = None
    new_lines = []
    save_re = re.compile(r'^\s*save\s*\(\s*([\'"])(.*?)\1\s*\)\s*$', re.IGNORECASE)
    for ln in lines:
        m = save_re.match(ln)
        if m:
            save_path = m.group(2)
            continue  # do not pass this line to R
        new_lines.append(ln)
    lines = new_lines

    code = "\n".join(lines)
    plotting = any(p in code for p in ("plot(", "hist(", "boxplot(", "barplot(", "ggplot("))

    # Choose output file
    if save_path:
        out_png_fs = os.path.join(base_dir, save_path) if (base_dir and not os.path.isabs(save_path)) else save_path
        os.makedirs(os.path.dirname(os.path.abspath(out_png_fs)), exist_ok=True)
    else:
        tmp_dir = tempfile.mkdtemp(prefix="cowlang_r_")
        out_png_fs = os.path.join(tmp_dir, "plot.png")
    out_png_r = out_png_fs.replace("\\", "/")  # R prefers forward slashes

    # Compose R code
    if plotting:
        r_code = f'''
        tryCatch({{
            png("{out_png_r}", width=900, height=600, res=110)
            {code}
            dev.off()
        }}, error=function(e) {{
            message("R ERROR: ", e)
            quit(status=2)
        }})
        '''
    else:
        r_code = f'''
        tryCatch({{
            {code}
        }}, error=function(e) {{
            message("R ERROR: ", e)
            quit(status=2)
        }})
        '''

    try:
        proc = subprocess.Popen(
            [rscript, "-e", r_code],
            stdout=subprocess.PIPE,
            stderr=subprocess.STDOUT,
            text=True
        )
        for line in proc.stdout:
            log_fn("[R] " + line.rstrip())
        proc.wait()

        if proc.returncode == 0 and plotting and os.path.exists(out_png_fs):
            log_fn(f"[R] Plot saved to: {out_png_fs}")
            try:
                os.startfile(out_png_fs)
            except Exception:
                webbrowser.open('file://' + out_png_fs.replace("\\", "/"))
        return proc.returncode
    except Exception as e:
        log_fn(f"ERROR running R: {e}")
        return 1

def run_python_block(lines, log_fn):
    """
    Runs Python code in a separate interpreter (avoids Tk clashes with the IDE).
    Normalizes indentation (tabs -> spaces) and dedents to avoid TabError.
    Also makes 'tk' globally available via a top-level import (if tkinter exists).
    """
    python_exe = find_python_exe()
    log_fn(f"[PY] using: {python_exe}")

    # Normalize indentation (avoid TabError)
    norm_lines = [(ln.replace('\t', '    ')) for ln in lines]
    code = textwrap.dedent("\n".join(norm_lines))

    helper = '''
# --- helpers injected by IDE (global scope) ---
try:
    import tkinter as tk
except Exception:
    tk = None

def tkinter_window(title="Tk Window from CowLang", size="400x220"):
    if tk is None:
        raise ImportError("tkinter is not available on this Python interpreter.")
    root = tk.Tk()
    root.title(title)
    root.geometry(size)
    tk.Label(root, text="Hello from Python block!", font=("Consolas", 12)).pack(padx=20, pady=30)
    tk.Button(root, text="Close", command=root.destroy).pack(pady=10)
    root.mainloop()
# --- end helpers ---
'''.lstrip("\n")

    tmp_dir = tempfile.mkdtemp(prefix="cowlang_py_")
    script = os.path.join(tmp_dir, "block.py")
    with open(script, "w", encoding="utf-8", newline="\n") as f:
        f.write(helper)
        f.write("\n# === user code ===\n")
        f.write(code)
        f.write("\n")

    try:
        proc = subprocess.Popen(
            [python_exe, script],
            stdout=subprocess.PIPE,
            stderr=subprocess.STDOUT,
            text=True
        )
        for line in proc.stdout:
            log_fn("[PY] " + line.rstrip())
        proc.wait()
        return proc.returncode
    except Exception as e:
        log_fn(f"ERROR running Python: {e}")
        return 1

def exec_dispatch(action, log_fn, base_dir):
    lang = action["lang"]
    lines = action["lines"]
    if lang == "r":
        return run_r_block(lines, log_fn, base_dir=base_dir)
    elif lang == "python":
        return run_python_block(lines, log_fn)
    else:
        log_fn(f"[{lang}] unsupported language — skipped. Add an executor in exec_dispatch().")
        return 0  # non-fatal

# --------------------------------------------------------------------------------------
# Custom Text with <<Change>> event via Tcl proxy (used for line numbers)
# --------------------------------------------------------------------------------------
class CustomText(tk.Text):
    def __init__(self, *args, **kwargs):
        tk.Text.__init__(self, *args, **kwargs)

        # Create a proxy to catch all changes and scrolls
        self._orig = self._w + "_orig"
        self.tk.call("rename", self._w, self._orig)
        self.tk.createcommand(self._w, self._proxy)

    def _proxy(self, *args):
        # Call the real widget command
        try:
            result = self.tk.call((self._orig,) + args)
        except Exception as e:
            raise

        # If the widget content or view might have changed, generate <<Change>>
        cmd = args[0]
        if (cmd in ("insert", "replace", "delete") or
            cmd in ("see", "mark", "xview", "yview")):
            self.event_generate("<<Change>>", when="tail")
        return result

# --------------------------------------------------------------------------------------
# Line number gutter
# --------------------------------------------------------------------------------------
class LineNumbers(tk.Canvas):
    def __init__(self, master, **kwargs):
        super().__init__(master, **kwargs)
        self.text_widget = None

    def attach(self, text_widget: tk.Text):
        self.text_widget = text_widget

    def redraw(self, *args):
        if not self.text_widget:
            return
        self.delete("all")

        i = self.text_widget.index("@0,0")  # index of top visible line
        while True:
            dline = self.text_widget.dlineinfo(i)
            if dline is None:
                break
            y = dline[1]
            line_num = str(i).split(".")[0]
            self.create_text(2, y, anchor="nw",
                             text=line_num,
                             fill="#6c6c6c",
                             font=("Consolas", 10))
            i = self.text_widget.index(f"{i}+1line")

# --------------------------------------------------------------------------------------
# IDE
# --------------------------------------------------------------------------------------
class CowLangIDE:
    def __init__(self, root):
        self.root = root
        self.root.title("CowLang IDE 🐄")
        self.root.geometry("1000x650")

        self.current_file = None
        self.setup_ui()
        self.setup_tags()
        self.bind_keys()

    # === UI ===
    def setup_ui(self):
        menubar = tk.Menu(self.root)

        filemenu = tk.Menu(menubar, tearoff=0)
        filemenu.add_command(label="New", command=self.new_file)
        filemenu.add_command(label="Open", command=self.open_file)
        filemenu.add_command(label="Save", command=self.save_file)
        filemenu.add_command(label="Save As", command=self.save_as_file)
        filemenu.add_separator()
        filemenu.add_command(label="Exit", command=self.root.quit)
        menubar.add_cascade(label="File", menu=filemenu)

        runmenu = tk.Menu(menubar, tearoff=0)
        runmenu.add_command(label="Run CowLang ▶ (F5)", command=self.run_cowlang)
        menubar.add_cascade(label="Run", menu=runmenu)

        self.root.config(menu=menubar)

        # --- Editor frame with line numbers + scrollbar ---
        editor_frame = tk.Frame(self.root, bg="#0d0d0d")
        editor_frame.pack(fill="both", expand=True)

        self.linenumbers = LineNumbers(editor_frame, width=40, bg="#0d0d0d", highlightthickness=0)
        self.linenumbers.pack(side="left", fill="y")

        self.editor = CustomText(
            editor_frame,
            wrap="none",
            undo=True,
            font=("Consolas", 11),
            bg="#0d0d0d",
            fg="#eaeaea",
            insertbackground="white",
            padx=4,
            pady=4
        )
        self.editor.pack(side="left", fill="both", expand=True)

        self.vsb = tk.Scrollbar(editor_frame, orient="vertical", command=self.editor.yview)
        self.vsb.pack(side="right", fill="y")
        self.editor.configure(yscrollcommand=self._on_yscroll)

        # Attach line numbers to text and keep them in sync
        self.linenumbers.attach(self.editor)
        self.editor.bind("<<Change>>", lambda e: self.linenumbers.redraw())
        self.editor.bind("<Configure>", lambda e: self.linenumbers.redraw())
        self.editor.bind("<MouseWheel>", lambda e: self.linenumbers.redraw())
        self.editor.bind("<Button-1>", lambda e: self.linenumbers.redraw())
        self.editor.bind("<KeyRelease>", self.highlight)  # also kicks redraw via <<Change>>

        # --- Terminal ---
        self.terminal = tk.Text(
            self.root,
            height=12,
            bg="#111",
            fg="#00ff9c",
            insertbackground="white",
            font=("Consolas", 10)
        )
        self.terminal.pack(fill="x")

        self.log("CowLang IDE ready 🐄 (stdin + inline third‑party language calls + line numbers + auto-end)")

    def _on_yscroll(self, *args):
        self.vsb.set(*args)
        self.linenumbers.redraw()

    # === TAGS ===
    def setup_tags(self):
        self.editor.tag_config("keyword", foreground="#4fc3f7")
        self.editor.tag_config("type", foreground="#81c784")
        self.editor.tag_config("string", foreground="#ffb74d")
        self.editor.tag_config("comment", foreground="#777777")
        self.editor.tag_config("number", foreground="#ba68c8")
        self.editor.tag_config("operator", foreground="#f06292")
        self.editor.tag_config("language", foreground="#ff9a6c")

    # === KEYBINDS ===
    def bind_keys(self):
        self.root.bind("<Control-s>", lambda e: self.save_file())
        self.root.bind("<Control-Shift-S>", lambda e: self.save_as_file())

        self.root.bind("<Control-z>", lambda e: self.editor.edit_undo())
        self.root.bind("<Control-y>", lambda e: self.editor.edit_redo())
        self.root.bind("<Control-Shift-Z>", lambda e: self.editor.edit_redo())

        self.root.bind("<F5>", lambda e: self.run_cowlang())

        # Auto indentation / auto-end on Enter
        self.editor.bind("<Return>", self._handle_return)

        # Optional: Turn Tab into spaces (aligns with INDENT setting)
        self.editor.bind("<Tab>", self._handle_tab)

    # === AUTO INDENT + AUTO END ===
    def _handle_tab(self, event):
        self.editor.insert("insert", INDENT)
        return "break"

    def _get_current_line_text(self):
        start = self.editor.index("insert linestart")
        end = self.editor.index("insert lineend")
        return self.editor.get(start, end)

    def _handle_return(self, event):
        """Auto-indent and auto-insert matching 'end ...' for 'prog X' and 'using <pl> do'."""
        line_text_full = self._get_current_line_text()
        code_line = code_before_bang(line_text_full)
        leading_ws = re.match(r'\s*', line_text_full).group(0)

        # Patterns
        m_prog = re.match(r'^\s*prog\s+(.+?)\s*$', code_line, re.IGNORECASE)
        m_using = re.match(r'^\s*using\s+([A-Za-z_]\w*)\s+do\s*$', code_line, re.IGNORECASE)

        if m_prog:
            name = m_prog.group(1)
            inner = leading_ws + INDENT
            closing = f"{leading_ws}end prog {name}"
            self.editor.insert("insert", "\n" + inner + "\n" + closing)
            # place cursor on the indented empty line
            cur_line = int(float(self.editor.index("insert")).__floor__())
            # After insertion, the cursor is at end; move to the line we just created (current line + 1)
            self.editor.mark_set("insert", f"{cur_line}.{len(inner)}")
            return "break"

        if m_using:
            pl = m_using.group(1)
            inner = leading_ws + INDENT
            closing = f"{leading_ws}end {pl}"
            self.editor.insert("insert", "\n" + inner + "\n" + closing)
            cur_line = int(float(self.editor.index("insert")).__floor__())
            self.editor.mark_set("insert", f"{cur_line}.{len(inner)}")
            return "break"

        # Default: keep same indentation as current line
        self.editor.insert("insert", "\n" + leading_ws)
        return "break"

    # === SYNTAX HIGHLIGHTING (quotes-aware comments) ===
    def highlight(self, event=None):
        text = self.editor.get("1.0", tk.END)

        for tag in ["keyword", "type", "string", "comment", "number", "operator", "language"]:
            self.editor.tag_remove(tag, "1.0", tk.END)

        # Walk line by line to compute comments & strings correctly.
        offset = 0
        for line in text.splitlines(True):
            code_part = code_before_bang(line)
            comment_idx = len(code_part)
            if comment_idx < len(line):
                self.editor.tag_add(
                    "comment",
                    f"1.0+{offset + comment_idx}c",
                    f"1.0+{offset + len(line)}c"
                )

            # Strings only before comment
            i = 0
            in_s = in_d = False
            esc = False
            start_idx = None
            while i < comment_idx:
                ch = line[i]
                if esc:
                    esc = False
                    i += 1
                    continue
                if ch == '\\':
                    esc = True
                    i += 1
                    continue
                if ch == "'" and not in_d:
                    if not in_s:
                        in_s = True
                        start_idx = i
                    else:
                        in_s = False
                        self.editor.tag_add("string", f"1.0+{offset + start_idx}c", f"1.0+{offset + i + 1}c")
                        start_idx = None
                    i += 1
                    continue
                if ch == '"' and not in_s:
                    if not in_d:
                        in_d = True
                        start_idx = i
                    else:
                        in_d = False
                        self.editor.tag_add("string", f"1.0+{offset + start_idx}c", f"1.0+{offset + i + 1}c")
                        start_idx = None
                    i += 1
                    continue
                i += 1

            offset += len(line)

        # numbers
        for match in re.finditer(r"\b\d+(\.\d+)?\b", text):
            self.editor.tag_add("number", f"1.0+{match.start()}c", f"1.0+{match.end()}c")

        # operator ::
        for match in re.finditer(r"::", text):
            self.editor.tag_add("operator", f"1.0+{match.start()}c", f"1.0+{match.end()}c")

        # language tokens
        for match in re.finditer(r"\b(cl|cl2|clp|r|python|c|cpp|cs|java|javascript|js|ruby|perl|swift|go)\b", text, re.IGNORECASE):
            self.editor.tag_add("language", f"1.0+{match.start()}c", f"1.0+{match.end()}c")

        # keywords / types
        for kw in KEYWORDS:
            pattern = rf"\b{re.escape(kw)}\b"
            for match in re.finditer(pattern, text, re.IGNORECASE):
                tag = "type" if kw in [
                    "string","str","integer","int","real",
                    "floatation","float","booleon","bool","character"
                ] else "keyword"
                self.editor.tag_add(tag, f"1.0+{match.start()}c", f"1.0+{match.end()}c")

        # keep line numbers fresh too
        self.linenumbers.redraw()

    # === FILE OPS ===
    def new_file(self):
        self.editor.delete("1.0", tk.END)
        self.current_file = None
        self.root.title("CowLang IDE 🐄 - New File")
        self.linenumbers.redraw()

    def open_file(self):
        path = filedialog.askopenfilename(
            filetypes=[("CowLang Files", "*.cow"), ("All Files", "*.*")]
        )
        if not path:
            return
        with open(path, "r", encoding="utf-8") as f:
            self.editor.delete("1.0", tk.END)
            self.editor.insert(tk.END, f.read())
        self.current_file = path
        self.highlight()
        self.root.title(f"CowLang IDE 🐄 - {os.path.basename(path)}")
        self.linenumbers.redraw()

    def save_file(self):
        if not self.current_file:
            self.save_as_file()
            return
        with open(self.current_file, "w", encoding="utf-8") as f:
            f.write(self.editor.get("1.0", tk.END))
        self.log(f"saved: {self.current_file}")

    def save_as_file(self):
        path = filedialog.asksaveasfilename(
            defaultextension=".cow",
            filetypes=[("CowLang Files", "*.cow"), ("All Files", "*.*")]
        )
        if not path:
            return
        self.current_file = path
        self.save_file()
        self.root.title(f"CowLang IDE 🐄 - {os.path.basename(path)}")

    # === RUN ===
    def run_cowlang(self):
        if not self.current_file:
            messagebox.showerror("bruh", "save the file first 💀")
            return

        self.save_file()
        self.terminal.delete("1.0", tk.END)
        self.log("feeding CowLang via stdin...\n")

        threading.Thread(target=self._run_process, daemon=True).start()

    def _run_process(self):
        try:
            with open(self.current_file, "r", encoding="utf-8") as f:
                cow_code = f.read()

            start = time.time()

            # 1) Extract inline third-party language actions (quotes-aware comments)
            actions, stripped_src = extract_lang_actions(cow_code)

            # 2) Hoist python imports into the next python block
            actions = fuse_python_imports(actions)

            base_dir = os.path.dirname(self.current_file) if self.current_file else None

            # 3) Execute actions in order
            for action in actions:
                rc = exec_dispatch(action, self.log, base_dir)
                lang = action['lang']
                self.log(f"[{lang}] exit code: {rc}")

            # 4) Run remaining CowLang via stdin (as before)
            process = subprocess.Popen(
                [COWLANG_EXE],
                cwd=COWLANG_DIR,
                stdin=subprocess.PIPE,
                stdout=subprocess.PIPE,
                stderr=subprocess.STDOUT,
                text=True
            )

            process.stdin.write(stripped_src)
            if not stripped_src.endswith("\n"):
                process.stdin.write("\n")
            process.stdin.close()

            for line in process.stdout:
                self.log(line.rstrip())

            process.wait()
            elapsed = int((time.time() - start) * 1000)
            self.log(f"\nfinished in {elapsed} ms | exit code {process.returncode}")

        except Exception as e:
            self.log(f"ERROR: {e}")

    # === LOG ===
    def log(self, msg):
        self.terminal.insert(tk.END, msg + "\n")
        self.terminal.see(tk.END)


if __name__ == "__main__":
    root = tk.Tk()
    CowLangIDE(root)
    root.mainloop()