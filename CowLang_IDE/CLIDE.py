"""
CLIDE — CowLang Integrated Development Environment  v1.1.0
===========================================================
Requirements:  pip install customtkinter
Run:           python clide.py

Supported file types:
  .cow  — CL pre-release / beta
  .cl   — CL full release
  .cowp — CLP pre-release / beta
  .clp  — CLP full release
"""

import customtkinter as ctk
import tkinter as tk
from tkinter import messagebox
import threading, subprocess, re, time, json, platform
from pathlib import Path

ctk.set_appearance_mode("dark")
ctk.set_default_color_theme("blue")

VERSION = "1.1.0"

# ── Colours ──────────────────────────────────────────────────────────────────
BG          = "#0d1117"
PANEL       = "#161b22"
EDITOR_BG   = "#0d1117"
GUTTER_BG   = "#161b22"
TERMINAL_BG = "#090c10"
TAB_BG      = "#010409"
TAB_ACTIVE  = "#0d1117"
TEXT        = "#e6edf3"
MUTED       = "#8b949e"
ACCENT      = "#39d353"
ACCENT2     = "#58a6ff"
WARN        = "#e3b341"
DANGER      = "#f85149"
BORDER      = "#30363d"
SYN_KW      = "#ff7b72"
SYN_TYPE    = "#79c0ff"
SYN_STR     = "#a5d6ff"
SYN_CMNT    = "#8b949e"
SYN_NUM     = "#f2cc60"
SYN_LANG    = "#d2a8ff"

# ── CowLang file extensions ───────────────────────────────────────────────────
CL_EXTS = {".cow", ".cl", ".cowp", ".clp"}

FILETYPES = [
    ("CowLang Files",            "*.cow *.cl *.cowp *.clp"),
    ("CL Source — pre/beta",     "*.cow"),
    ("CL Full Release",          "*.cl"),
    ("CLP Source — pre/beta",    "*.cowp"),
    ("CLP Full Release",         "*.clp"),
    ("All Files",                "*.*"),
]

FILE_ICONS = {
    ".cow": "🐄", ".cl": "🐄", ".cowp": "🐄", ".clp": "🐄",
    ".py": "🐍", ".txt": "📄", ".md": "📝", ".json": "📋",
    ".c": "⚙", ".cpp": "⚙", ".h": "⚙", ".sh": "💲",
    "dir": "📁", "default": "📄",
}

# ── Keywords ─────────────────────────────────────────────────────────────────
KEYWORDS = [
    "prog","program","end","end prog",
    "print","read",
    "if","then","else","elif",
    "for","do","dowhile","while",
    "def","subr","class","func",
    "implicit","none","stop","return","use","using",
]
TYPES = [
    "string","str","integer","int","real",
    "floatation","float","booleon","bool","character",
]
LANG_TOKENS = [
    "cl","cl2","clp","r","python","c","cpp","cs",
    "java","javascript","js","ruby","perl","swift","go",
]
ALL_KEYWORDS = KEYWORDS + TYPES + LANG_TOKENS
INDENT = "  "

# ── Config paths ─────────────────────────────────────────────────────────────
def _cfg_dir()  -> Path: return Path.home() / ".config" / "clide"
def _cfg_file() -> Path: return _cfg_dir() / "settings.json"

DEFAULT_EXE = str(
    Path.home() / "CowLang" / "CowLang-main" / "CowLangTranslator" /
    ("CLT_0.3.0.exe" if platform.system() == "Windows" else "CLT_0.3.0")
)

def _find_exe() -> str:
    p = Path.home() / "CowLang" / "cowlang.json"
    if p.exists():
        try:
            d = json.loads(p.read_text())
            if d.get("translator") and Path(d["translator"]).exists():
                return d["translator"]
        except Exception:
            pass
    return DEFAULT_EXE

def _strip_comment(line: str) -> str:
    in_s = in_d = esc = False
    for i, ch in enumerate(line):
        if esc:        esc = False; continue
        if ch == "\\": esc = True;  continue
        if ch == "'" and not in_d: in_s = not in_s; continue
        if ch == '"' and not in_s: in_d = not in_d; continue
        if ch == "!" and not in_s and not in_d: return line[:i]
    return line


# ═══════════════════════════════════════════════════════════════════════════════
#  Custom File Dialog
# ═══════════════════════════════════════════════════════════════════════════════
class CLIDEFileDialog(ctk.CTkToplevel):
    def __init__(self, parent, mode="open", title=None,
                 initial_dir=None, default_ext=".cow", initial_file=""):
        super().__init__(parent)
        self.result      = None
        self.mode        = mode
        self.default_ext = default_ext
        self._cwd        = Path(initial_dir) if initial_dir else Path.home()
        self._history    = [self._cwd]
        self._hist_idx   = 0
        self._entries: list = []

        titles = {"open": "Open File", "save": "Save As", "dir": "Select Folder"}
        self.title(title or titles.get(mode, "Browse"))
        self.geometry("800x560")
        self.minsize(600, 400)
        self.configure(fg_color=BG)
        self.resizable(True, True)

        self._ext_var = tk.StringVar(value="CowLang Files")
        self._build_ui(initial_file)
        self._populate()
        self.update_idletasks()
        self.deiconify()
        try:
            self.grab_set()
        except Exception:
            pass
        self.wait_window()

    def _build_ui(self, initial_file):
        nav = tk.Frame(self, bg=PANEL, height=42)
        nav.pack(fill="x"); nav.pack_propagate(False)
        for sym, fn in [("←", self._go_back), ("→", self._go_forward),
                        ("↑", self._go_up),   ("⟳", self._populate)]:
            lb = tk.Label(nav, text=sym, bg=PANEL, fg=MUTED,
                          font=("Courier New", 14), cursor="hand2", padx=6)
            lb.bind("<Button-1>", lambda e, f=fn: f())
            lb.bind("<Enter>",    lambda e, l=lb: l.configure(fg=TEXT))
            lb.bind("<Leave>",    lambda e, l=lb: l.configure(fg=MUTED))
            lb.pack(side="left", pady=6)
        self._path_var = tk.StringVar(value=str(self._cwd))
        pe = tk.Entry(nav, textvariable=self._path_var,
                      bg="#21262d", fg=TEXT, insertbackground=ACCENT2,
                      font=("Courier New", 11), relief="flat", bd=4,
                      highlightthickness=1, highlightbackground=BORDER,
                      highlightcolor=ACCENT2)
        pe.pack(side="left", fill="x", expand=True, padx=4, pady=8)
        pe.bind("<Return>", self._on_path_enter)
        ext_names = [row[0] for row in FILETYPES]
        om = tk.OptionMenu(nav, self._ext_var, *ext_names,
                           command=lambda _: self._populate())
        om.config(bg="#21262d", fg=TEXT, activebackground=BORDER,
                  activeforeground=TEXT, highlightthickness=0,
                  font=("Courier New", 10), bd=0, relief="flat")
        om["menu"].config(bg="#21262d", fg=TEXT, font=("Courier New", 10),
                          activebackground=ACCENT2, activeforeground=BG)
        om.pack(side="right", padx=8, pady=8)

        pane = tk.PanedWindow(self, orient="horizontal", bg=BORDER,
                              sashwidth=4, sashrelief="flat",
                              handlepad=0, handlesize=0)
        pane.pack(fill="both", expand=True)

        places_frm = tk.Frame(pane, bg=PANEL, width=175)
        pane.add(places_frm, minsize=120, width=175, stretch="never")
        tk.Label(places_frm, text=" PLACES", bg=PANEL, fg=MUTED,
                 font=("Courier New", 9, "bold"), anchor="w", pady=6).pack(fill="x")
        self._places_lb = tk.Listbox(places_frm, bg=PANEL, fg=TEXT,
            selectbackground=ACCENT2, selectforeground=BG,
            font=("Courier New", 11), relief="flat", bd=0,
            activestyle="none", exportselection=False, highlightthickness=0)
        self._places_lb.pack(fill="both", expand=True)
        self._places_lb.bind("<ButtonRelease-1>", self._on_place_click)
        self._places = []
        self._fill_places()

        files_frm = tk.Frame(pane, bg=BG)
        pane.add(files_frm, stretch="always")
        hdr = tk.Frame(files_frm, bg="#21262d", height=26)
        hdr.pack(fill="x"); hdr.pack_propagate(False)
        tk.Label(hdr, text="  Name", bg="#21262d", fg=MUTED,
                 font=("Courier New", 10, "bold"), anchor="w"
                 ).pack(side="left", fill="x", expand=True)
        for t, w in [("Size", 9), ("Type", 14)]:
            tk.Label(hdr, text=t, bg="#21262d", fg=MUTED,
                     font=("Courier New", 10, "bold"), width=w).pack(side="left")

        lf = tk.Frame(files_frm, bg=BG)
        lf.pack(fill="both", expand=True)
        sb = tk.Scrollbar(lf, bg=PANEL, troughcolor=BG, relief="flat", bd=0, width=12)
        sb.pack(side="right", fill="y")
        self._file_lb = tk.Listbox(lf, bg=BG, fg=TEXT,
            selectbackground="#1f3a5f", selectforeground=TEXT,
            font=("Courier New", 12), relief="flat", bd=0,
            activestyle="none", exportselection=False,
            highlightthickness=0, yscrollcommand=sb.set)
        self._file_lb.pack(fill="both", expand=True)
        sb.configure(command=self._file_lb.yview)
        self._file_lb.bind("<Double-Button-1>", self._on_double)
        self._file_lb.bind("<Return>",          self._on_double)
        self._file_lb.bind("<ButtonRelease-1>", self._on_single)

        tk.Frame(self, bg=BORDER, height=1).pack(fill="x", side="bottom")
        bot = tk.Frame(self, bg=PANEL, height=46)
        bot.pack(fill="x", side="bottom"); bot.pack_propagate(False)
        lbl = "Folder:" if self.mode == "dir" else "File name:"
        tk.Label(bot, text=lbl, bg=PANEL, fg=MUTED,
                 font=("Courier New", 11)).pack(side="left", padx=(12, 4))
        self._fname_var = tk.StringVar(value=initial_file)
        fe = tk.Entry(bot, textvariable=self._fname_var,
                      bg="#21262d", fg=TEXT, insertbackground=ACCENT2,
                      font=("Courier New", 11), relief="flat", bd=4,
                      highlightthickness=1, highlightbackground=BORDER,
                      highlightcolor=ACCENT2, width=34)
        fe.pack(side="left", fill="x", expand=True, padx=4, pady=8)
        fe.bind("<Return>", self._confirm)
        cancel = tk.Label(bot, text="Cancel", bg=PANEL, fg=MUTED,
                          font=("Courier New", 11), cursor="hand2", padx=14, pady=8)
        cancel.bind("<Button-1>", lambda e: self.destroy())
        cancel.bind("<Enter>",    lambda e: cancel.configure(fg=TEXT))
        cancel.bind("<Leave>",    lambda e: cancel.configure(fg=MUTED))
        cancel.pack(side="right", padx=(4, 14))
        ok_text = {"open": "Open", "save": "Save", "dir": "Select"}[self.mode]
        ok = tk.Label(bot, text=ok_text, bg=ACCENT, fg=BG,
                      font=("Courier New", 11, "bold"), cursor="hand2",
                      padx=16, pady=8)
        ok.bind("<Button-1>", self._confirm)
        ok.bind("<Enter>",    lambda e: ok.configure(bg="#2ea043"))
        ok.bind("<Leave>",    lambda e: ok.configure(bg=ACCENT))
        ok.pack(side="right", padx=4)

    def _fill_places(self):
        home = Path.home()
        cands = [
            ("Home",        home),
            ("Root",        Path("/")),
            ("Documents",   home / "Documents"),
            ("Downloads",   home / "Downloads"),
            ("Pictures",    home / "Pictures"),
            ("Music",       home / "Music"),
            ("Desktop",     home / "Desktop"),
        ]
        mnt = Path("/mnt")
        if mnt.exists():
            try:
                for d in sorted(mnt.iterdir()):
                    if d.is_dir():
                        cands.append((f"/mnt/{d.name}", d))
            except PermissionError:
                pass
        for label, path in cands:
            if path.exists():
                self._places.append(path)
                self._places_lb.insert("end", f"  {label}")

    def _navigate(self, path):
        if not path.exists(): return
        self._cwd = path
        self._path_var.set(str(path))
        self._history = self._history[:self._hist_idx + 1]
        self._history.append(path)
        self._hist_idx = len(self._history) - 1
        self._populate()

    def _go_back(self):
        if self._hist_idx > 0:
            self._hist_idx -= 1
            self._cwd = self._history[self._hist_idx]
            self._path_var.set(str(self._cwd))
            self._populate()

    def _go_forward(self):
        if self._hist_idx < len(self._history) - 1:
            self._hist_idx += 1
            self._cwd = self._history[self._hist_idx]
            self._path_var.set(str(self._cwd))
            self._populate()

    def _go_up(self):
        p = self._cwd.parent
        if p != self._cwd: self._navigate(p)

    def _on_path_enter(self, _=None):
        p = Path(self._path_var.get())
        if p.is_dir():   self._navigate(p)
        elif p.is_file(): self._fname_var.set(p.name); self._navigate(p.parent)

    def _on_place_click(self, _=None):
        sel = self._places_lb.curselection()
        if sel: self._navigate(self._places[sel[0]])

    def _get_filter_exts(self):
        sel = self._ext_var.get()
        for name, pattern in FILETYPES:
            if name == sel:
                if pattern == "*.*": return set()
                return {e.lstrip("*") for e in pattern.split()}
        return set()

    def _populate(self, _=None):
        self._file_lb.delete(0, "end")
        self._entries = []
        filter_exts = self._get_filter_exts()
        try:
            raw = sorted(self._cwd.iterdir(),
                         key=lambda p: (p.is_file(), p.name.lower()))
        except PermissionError:
            self._file_lb.insert("end", "  Permission denied"); return
        for e in raw:
            if e.name.startswith("."): continue
            if self.mode == "dir" and e.is_file(): continue
            ext = e.suffix.lower()
            if e.is_file() and filter_exts and ext not in filter_exts: continue
            icon = FILE_ICONS.get(ext if e.is_file() else "dir", FILE_ICONS["default"])
            size = self._fmt_size(e) if e.is_file() else ""
            kind = (ext.upper().lstrip(".") + " file" if ext else "File") if e.is_file() else "Folder"
            self._file_lb.insert("end", f"  {icon} {e.name:<40} {size:>9}   {kind}")
            self._entries.append(e)
        for i, e in enumerate(self._entries):
            if   e.is_dir():                  self._file_lb.itemconfig(i, fg=ACCENT2)
            elif e.suffix.lower() in CL_EXTS: self._file_lb.itemconfig(i, fg=ACCENT)

    def _fmt_size(self, p):
        try:
            s = p.stat().st_size
            for u in ("B", "KB", "MB", "GB"):
                if s < 1024: return f"{s:.0f} {u}"
                s /= 1024
            return f"{s:.1f} TB"
        except Exception: return ""

    def _on_single(self, _=None):
        sel = self._file_lb.curselection()
        if sel and self._entries[sel[0]].is_file():
            self._fname_var.set(self._entries[sel[0]].name)

    def _on_double(self, _=None):
        sel = self._file_lb.curselection()
        if not sel: return
        e = self._entries[sel[0]]
        if e.is_dir(): self._navigate(e)
        else:          self._fname_var.set(e.name); self._confirm()

    def _confirm(self, _=None):
        fname = self._fname_var.get().strip()
        if self.mode == "dir":
            self.result = str(self._cwd); self.destroy(); return
        if not fname: return
        p = Path(fname) if Path(fname).is_absolute() else self._cwd / fname
        if self.mode == "open":
            if p.is_dir(): self._navigate(p); return
            if not p.exists():
                messagebox.showerror("CLIDE", f"File not found:\n{p}", parent=self); return
            self.result = str(p)
        elif self.mode == "save":
            if p.is_dir(): self._navigate(p); return
            if not p.suffix: p = p.with_suffix(self.default_ext)
            if p.exists():
                if not messagebox.askyesno("Overwrite?",
                        f"'{p.name}' already exists. Overwrite?", parent=self): return
            self.result = str(p)
        self.destroy()


def ask_open_file(parent, initial_dir=None):
    return CLIDEFileDialog(parent, mode="open", initial_dir=initial_dir).result

def ask_save_file(parent, initial_dir=None, default_ext=".cow", initial_file=""):
    return CLIDEFileDialog(parent, mode="save", initial_dir=initial_dir,
                           default_ext=default_ext, initial_file=initial_file).result

def ask_directory(parent, initial_dir=None):
    return CLIDEFileDialog(parent, mode="dir", initial_dir=initial_dir).result

def ask_open_exe(parent, initial_dir=None):
    return CLIDEFileDialog(parent, mode="open",
                           title="Select CowLang Translator",
                           initial_dir=initial_dir or str(Path.home())).result


# ═══════════════════════════════════════════════════════════════════════════════
#  Autocomplete popup
# ═══════════════════════════════════════════════════════════════════════════════
class AutoComplete(tk.Toplevel):
    def __init__(self, parent, words, on_select, x, y):
        super().__init__(parent)
        self.overrideredirect(True)
        self.configure(bg=PANEL)
        self._on_select = on_select
        lb = tk.Listbox(self, bg=PANEL, fg=TEXT,
                        selectbackground=ACCENT2, selectforeground=BG,
                        font=("Courier New", 12), relief="flat", bd=0,
                        activestyle="none", exportselection=False,
                        height=min(len(words), 8), highlightthickness=0)
        lb.pack(fill="both", expand=True, padx=1, pady=1)
        for w in words: lb.insert("end", w)
        lb.selection_set(0)
        lb.bind("<Return>",          lambda _: self._pick(lb))
        lb.bind("<Double-Button-1>", lambda _: self._pick(lb))
        lb.bind("<Escape>",          lambda _: self.destroy())
        self.geometry(f"+{x}+{y}")
        lb.focus_set()

    def _pick(self, lb):
        sel = lb.curselection()
        if sel: self._on_select(lb.get(sel[0]))
        self.destroy()


# ═══════════════════════════════════════════════════════════════════════════════
#  File Browser sidebar
# ═══════════════════════════════════════════════════════════════════════════════
class FileBrowser(ctk.CTkFrame):
    def __init__(self, parent, on_open, **kw):
        super().__init__(parent, fg_color=PANEL, **kw)
        self.on_open    = on_open
        self._root_path = Path.home()
        self._entries   = []

        hdr = ctk.CTkFrame(self, fg_color="#21262d", corner_radius=0, height=30)
        hdr.pack(fill="x"); hdr.pack_propagate(False)
        ctk.CTkLabel(hdr, text="EXPLORER",
                     font=ctk.CTkFont("Courier New", 10, "bold"),
                     text_color=MUTED).pack(side="left", padx=8)
        ctk.CTkButton(hdr, text="⟳", width=24, height=24, fg_color="transparent",
                      hover_color="#30363d", font=ctk.CTkFont("Courier New", 12),
                      text_color=MUTED, command=self.refresh).pack(side="right", padx=4)
        ctk.CTkButton(hdr, text="📁", width=24, height=24, fg_color="transparent",
                      hover_color="#30363d", font=ctk.CTkFont("Courier New", 12),
                      text_color=MUTED, command=self._browse_dir).pack(side="right")

        self._path_var = ctk.StringVar(value=str(self._root_path))
        ctk.CTkLabel(self, textvariable=self._path_var,
                     font=ctk.CTkFont("Courier New", 9),
                     text_color=MUTED, wraplength=170, anchor="w"
                     ).pack(fill="x", padx=6, pady=2)

        self._lb = tk.Listbox(self, bg=PANEL, fg=TEXT,
            selectbackground=ACCENT2, selectforeground=BG,
            font=("Courier New", 11), relief="flat", bd=0,
            activestyle="none", exportselection=False, highlightthickness=0)
        self._lb.pack(fill="both", expand=True)
        self._lb.bind("<Double-Button-1>", self._on_select)
        self._lb.bind("<Return>",          self._on_select)
        self.refresh()

    def set_dir(self, path):
        p = Path(path)
        d = p.parent if p.is_file() else p
        if d.is_dir():
            self._root_path = d
            self._path_var.set(str(d))
            self.refresh()

    def _browse_dir(self):
        d = ask_directory(self.winfo_toplevel(), str(self._root_path))
        if d: self.set_dir(d)

    def refresh(self, _=None):
        self._lb.delete(0, "end")
        self._entries = []
        try:
            entries = sorted(self._root_path.iterdir(),
                             key=lambda p: (p.is_file(), p.name.lower()))
            for e in entries:
                if e.name.startswith("."): continue
                icon = "🐄 " if (e.is_file() and e.suffix.lower() in CL_EXTS) else \
                       "📁 " if e.is_dir() else "📄 "
                self._lb.insert("end", f"  {icon}{e.name}")
                color = ACCENT if (e.is_file() and e.suffix.lower() in CL_EXTS) else \
                        ACCENT2 if e.is_dir() else TEXT
                self._lb.itemconfig(len(self._entries), fg=color)
                self._entries.append(e)
        except PermissionError:
            pass

    def _on_select(self, _=None):
        sel = self._lb.curselection()
        if not sel: return
        e = self._entries[sel[0]]
        if e.is_dir(): self.set_dir(str(e))
        else:          self.on_open(str(e))


# ═══════════════════════════════════════════════════════════════════════════════
#  Per-tab state
# ═══════════════════════════════════════════════════════════════════════════════
class EditorTab:
    def __init__(self, path=None):
        self.path     = path
        self.content  = ""
        self.modified = False

    @property
    def display_name(self):
        base = Path(self.path).name if self.path else "untitled"
        return ("● " + base) if self.modified else base


# ═══════════════════════════════════════════════════════════════════════════════
#  Tab bar
# ═══════════════════════════════════════════════════════════════════════════════
class TabBar(tk.Frame):
    H = 34
    def __init__(self, parent, on_switch, on_close, **kw):
        super().__init__(parent, bg=TAB_BG, height=self.H, **kw)
        self.pack_propagate(False)
        self.on_switch = on_switch
        self.on_close  = on_close
        self._tabs   = []
        self._active = -1
        self._frames = []

    def add_tab(self, tab):
        idx = len(self._tabs)
        self._tabs.append(tab)
        self._redraw()
        return idx

    def remove_tab(self, idx):
        if 0 <= idx < len(self._tabs):
            self._tabs.pop(idx)
            if self._active >= len(self._tabs):
                self._active = max(0, len(self._tabs) - 1)
            self._redraw()

    def set_active(self, idx):
        self._active = idx
        self._redraw()

    def mark_modified(self, idx, v):
        if 0 <= idx < len(self._tabs):
            self._tabs[idx].modified = v
            self._redraw()

    def _redraw(self):
        for f in self._frames: f.destroy()
        self._frames = []
        for i, tab in enumerate(self._tabs):
            active = (i == self._active)
            bg = TAB_ACTIVE if active else TAB_BG
            frm = tk.Frame(self, bg=bg, cursor="hand2")
            frm.pack(side="left", fill="y", padx=(0, 1))
            if active:
                tk.Frame(frm, bg=ACCENT, height=2).pack(side="bottom", fill="x")
            lbl = tk.Label(frm, text=tab.display_name, bg=bg,
                           fg=TEXT if active else MUTED,
                           font=("Courier New", 11), padx=12, pady=6)
            lbl.pack(side="left")
            lbl.bind("<Button-1>", lambda e, i=i: self._click(i))
            x = tk.Label(frm, text="✕", bg=bg, fg=MUTED,
                         font=("Courier New", 10), padx=6)
            x.pack(side="left")
            x.bind("<Button-1>",  lambda e, i=i: self.on_close(i))
            x.bind("<Enter>",     lambda e, l=x: l.configure(fg=DANGER))
            x.bind("<Leave>",     lambda e, l=x: l.configure(fg=MUTED))
            self._frames.append(frm)

    def _click(self, idx):
        self.set_active(idx)
        self.on_switch(idx)


# ═══════════════════════════════════════════════════════════════════════════════
#  Main IDE
# ═══════════════════════════════════════════════════════════════════════════════
class CLIDE(ctk.CTk):
    MAX_RECENT = 14

    def __init__(self):
        super().__init__()
        self.title(f"CLIDE v{VERSION} — CowLang IDE")
        self.geometry("1280x820")
        self.configure(fg_color=BG)
        self.minsize(800, 560)

        self._tabs        = []
        self._active_idx  = -1
        self.autosave     = False
        self._autocomplete = None
        self._find_visible = False
        self._cowlang_exe = _find_exe()
        self._recent      = []
        self._term_height = 200

        self._load_settings()
        self._build_ui()
        self._apply_keybindings()
        self._schedule_autosave()

    @property
    def current_file(self):
        t = self._active_tab
        return t.path if t else None

    @property
    def _active_tab(self):
        if 0 <= self._active_idx < len(self._tabs):
            return self._tabs[self._active_idx]
        return None

    # ── Settings ─────────────────────────────────────────────────────────────
    def _load_settings(self):
        p = _cfg_file()
        if not p.exists(): return
        try:
            d = json.loads(p.read_text())
            self.autosave     = d.get("autosave",    False)
            self._cowlang_exe = d.get("cowlang_exe", self._cowlang_exe)
            self._recent      = d.get("recent",      [])
            self._term_height = d.get("term_height", 200)
            for f in d.get("open_files", []):
                if Path(f).exists():
                    self._tabs.append(EditorTab(f))
            if not self._tabs and d.get("lastfile") and Path(d["lastfile"]).exists():
                self._tabs.append(EditorTab(d["lastfile"]))
            if self._tabs:
                self._active_idx = min(d.get("active_tab", 0), len(self._tabs) - 1)
        except Exception:
            pass

    def _save_settings(self):
        try:
            _cfg_dir().mkdir(parents=True, exist_ok=True)
            try:
                sash_y = self.vert_pane.sash_coord(0)[1]
                total  = self.vert_pane.winfo_height()
                self._term_height = max(60, total - sash_y)
            except Exception:
                pass
            _cfg_file().write_text(json.dumps({
                "autosave":    self.autosave,
                "cowlang_exe": self._cowlang_exe,
                "recent":      self._recent[:self.MAX_RECENT],
                "term_height": self._term_height,
                "open_files":  [t.path for t in self._tabs if t.path],
                "active_tab":  self._active_idx,
            }, indent=2))
        except Exception:
            pass

    def _push_recent(self, path):
        if path in self._recent: self._recent.remove(path)
        self._recent.insert(0, path)
        self._recent = self._recent[:self.MAX_RECENT]

    # ── Build UI ─────────────────────────────────────────────────────────────
    def _build_ui(self):
        # Toolbar — pure tk.Frame so no CTk rendering issues on fullscreen
        tb = tk.Frame(self, bg=PANEL, height=46)
        tb.pack(fill="x", side="top"); tb.pack_propagate(False)
        self.toolbar = tb

        def tbtn(text, cmd, bg_col=None):
            b = tk.Label(tb, text=text, bg=bg_col or "#21262d", fg=TEXT if not bg_col else BG,
                         font=("Courier New", 11), cursor="hand2",
                         relief="flat", padx=10, pady=6,
                         highlightthickness=1, highlightbackground=BORDER)
            b.bind("<Button-1>", lambda e: cmd())
            hover = "#2ea043" if bg_col == ACCENT else "#30363d"
            b.bind("<Enter>", lambda e, l=b, h=hover: l.configure(bg=h))
            b.bind("<Leave>", lambda e, l=b, ob=bg_col or "#21262d": l.configure(bg=ob))
            return b

        tbtn("New",         self.new_file).pack(side="left", padx=(8,2), pady=7)
        tbtn("Open",        self.open_file).pack(side="left", padx=2, pady=7)
        tbtn("Save",        self.save_file).pack(side="left", padx=2, pady=7)
        tbtn("Save As",     self.save_file_as).pack(side="left", padx=2, pady=7)
        tbtn("Recent ▾",    self._open_recent_menu).pack(side="left", padx=2, pady=7)
        tk.Frame(tb, bg=BORDER, width=1).pack(side="left", fill="y", padx=8, pady=8)
        tbtn("Find/Replace",self.toggle_find).pack(side="left", padx=2, pady=7)
        tbtn("⚙ Settings",  self._open_settings).pack(side="left", padx=2, pady=7)

        # Autosave — plain tk.Checkbutton avoids CTkCheckBox spoiler-box rendering bug
        self._autosave_var = tk.BooleanVar(value=self.autosave)
        tk.Checkbutton(
            tb, text="Autosave",
            variable=self._autosave_var,
            command=self._on_autosave_toggle,
            bg=PANEL, fg=TEXT, selectcolor="#21262d",
            activebackground=PANEL, activeforeground=TEXT,
            font=("Courier New", 11), bd=0, relief="flat", cursor="hand2",
        ).pack(side="right", padx=10, pady=10)

        # Run button
        run = tk.Label(tb, text="▶  Run (F5)", bg=ACCENT, fg=BG,
                       font=("Courier New", 12, "bold"), cursor="hand2",
                       padx=14, pady=6, relief="flat")
        run.bind("<Button-1>", lambda e: self.run_cowlang())
        run.bind("<Enter>",    lambda e: run.configure(bg="#2ea043"))
        run.bind("<Leave>",    lambda e: run.configure(bg=ACCENT))
        run.pack(side="right", padx=(2,10), pady=7)
        tk.Frame(tb, bg=BORDER, width=1).pack(side="right", fill="y", padx=6, pady=8)

        self.title_lbl = tk.Label(tb, text="No file open", bg=PANEL, fg=MUTED,
                                  font=("Courier New", 11, "italic"))
        self.title_lbl.pack(side="left", padx=8)

        # Find/Replace bar (hidden initially)
        self.find_bar    = tk.Frame(self, bg=PANEL, height=36)
        self.find_var    = tk.StringVar()
        self.replace_var = tk.StringVar()
        for tv, ph in [(self.find_var, "Find…"), (self.replace_var, "Replace…")]:
            tk.Entry(self.find_bar, textvariable=tv,
                     bg="#21262d", fg=TEXT, insertbackground=ACCENT2,
                     font=("Courier New", 11), relief="flat", bd=4,
                     highlightthickness=1, highlightbackground=BORDER,
                     highlightcolor=ACCENT2, width=22
                     ).pack(side="left", padx=4, pady=4)
        for lbl_text, cmd in [("Find Next",self._find_next),
                               ("Replace",  self._replace_one),
                               ("Replace All",self._replace_all)]:
            b = tk.Label(self.find_bar, text=lbl_text, bg="#21262d", fg=TEXT,
                         font=("Courier New", 10), cursor="hand2",
                         padx=8, pady=4, relief="flat",
                         highlightthickness=1, highlightbackground=BORDER)
            b.bind("<Button-1>", lambda e, c=cmd: c())
            b.bind("<Enter>",    lambda e, l=b: l.configure(bg="#30363d"))
            b.bind("<Leave>",    lambda e, l=b: l.configure(bg="#21262d"))
            b.pack(side="left", padx=2, pady=4)
        xb = tk.Label(self.find_bar, text="✕", bg=PANEL, fg=MUTED,
                      font=("Courier New",12), cursor="hand2", padx=8)
        xb.bind("<Button-1>", lambda e: self.toggle_find())
        xb.bind("<Enter>",    lambda e: xb.configure(fg=DANGER))
        xb.bind("<Leave>",    lambda e: xb.configure(fg=MUTED))
        xb.pack(side="right", padx=6)

        # Status bar
        status = tk.Frame(self, bg=PANEL, height=22)
        status.pack(fill="x", side="bottom"); status.pack_propagate(False)
        self.status_var = tk.StringVar(value="Ready")
        tk.Label(status, textvariable=self.status_var, bg=PANEL, fg=MUTED,
                 font=("Courier New", 10), anchor="w").pack(side="left", padx=8)
        self.cursor_var = tk.StringVar(value="Ln 1, Col 1")
        tk.Label(status, textvariable=self.cursor_var, bg=PANEL, fg=MUTED,
                 font=("Courier New", 10)).pack(side="right", padx=8)

        # Main area
        main = tk.Frame(self, bg=BG)
        main.pack(fill="both", expand=True)

        self.sidebar = FileBrowser(main, on_open=self._open_from_browser, width=195)
        self.sidebar.pack(side="left", fill="y")
        tk.Frame(main, bg=BORDER, width=1).pack(side="left", fill="y")

        right = tk.Frame(main, bg=BG)
        right.pack(side="left", fill="both", expand=True)

        self.tab_bar = TabBar(right, on_switch=self._switch_tab, on_close=self._close_tab)
        self.tab_bar.pack(fill="x")
        tk.Frame(right, bg=BORDER, height=1).pack(fill="x")

        self.vert_pane = tk.PanedWindow(right, orient="vertical", bg=BORDER,
                                        sashwidth=5, sashrelief="flat",
                                        handlepad=0, handlesize=0, showhandle=False)
        self.vert_pane.pack(fill="both", expand=True)

        # Editor pane
        editor_outer = tk.Frame(self.vert_pane, bg=EDITOR_BG)
        self.vert_pane.add(editor_outer, stretch="always", minsize=120)
        self.gutter = tk.Canvas(editor_outer, bg=GUTTER_BG, width=44,
                                highlightthickness=0, bd=0)
        self.gutter.pack(side="left", fill="y")
        tk.Frame(editor_outer, bg=BORDER, width=1).pack(side="left", fill="y")
        esy = ctk.CTkScrollbar(editor_outer, orientation="vertical")
        esy.pack(side="right", fill="y")
        esx = ctk.CTkScrollbar(editor_outer, orientation="horizontal")
        esx.pack(side="bottom", fill="x")
        self._esy = esy
        self.editor = tk.Text(editor_outer,
            bg=EDITOR_BG, fg=TEXT, insertbackground=ACCENT,
            selectbackground="#264f78", font=("Courier New", 13),
            wrap="none", relief="flat", bd=0, undo=True, maxundo=-1,
            yscrollcommand=self._ey_scroll, xscrollcommand=esx.set,
            padx=8, pady=4, spacing1=2, spacing3=2,
            exportselection=True, highlightthickness=0)
        self.editor.pack(side="left", fill="both", expand=True)
        esy.configure(command=self._ey_view)
        esx.configure(command=self.editor.xview)
        self._setup_tags()

        # Terminal pane
        term_outer = tk.Frame(self.vert_pane, bg=TERMINAL_BG)
        self.vert_pane.add(term_outer, stretch="never",
                           minsize=60, height=self._term_height)
        thdr = tk.Frame(term_outer, bg="#0d1117", height=24)
        thdr.pack(fill="x"); thdr.pack_propagate(False)
        tk.Label(thdr, text="OUTPUT / TERMINAL", bg="#0d1117", fg=MUTED,
                 font=("Courier New", 9, "bold"), anchor="w", padx=8).pack(side="left")
        tk.Label(thdr, text="⠿ drag to resize", bg="#0d1117", fg="#30363d",
                 font=("Courier New", 8)).pack(side="left", padx=4)
        cl = tk.Label(thdr, text="Clear", bg="#0d1117", fg=MUTED,
                      font=("Courier New", 9), cursor="hand2", padx=8)
        cl.bind("<Button-1>", lambda e: self._clear_terminal())
        cl.bind("<Enter>",    lambda e: cl.configure(fg=TEXT))
        cl.bind("<Leave>",    lambda e: cl.configure(fg=MUTED))
        cl.pack(side="right", padx=4)
        tscr = ctk.CTkScrollbar(term_outer, orientation="vertical")
        tscr.pack(side="right", fill="y")
        self.terminal = tk.Text(term_outer, bg=TERMINAL_BG, fg=ACCENT,
            insertbackground=ACCENT, font=("Courier New", 12),
            wrap="word", relief="flat", bd=0, state="disabled",
            yscrollcommand=tscr.set, padx=8, pady=4, highlightthickness=0)
        self.terminal.pack(fill="both", expand=True)
        tscr.configure(command=self.terminal.yview)
        tinput_row = tk.Frame(term_outer, bg=TERMINAL_BG, height=28)
        tinput_row.pack(fill="x"); tinput_row.pack_propagate(False)
        tk.Label(tinput_row, text="$", bg=TERMINAL_BG, fg=ACCENT,
                 font=("Courier New", 12, "bold"), width=2).pack(side="left", padx=(8,0))
        self.term_input = tk.Entry(tinput_row, bg=TERMINAL_BG, fg=TEXT,
            insertbackground=ACCENT, font=("Courier New", 12),
            relief="flat", bd=2, highlightthickness=0)
        self.term_input.pack(side="left", fill="x", expand=True, padx=4)
        self.term_input.bind("<Return>", self._run_shell_cmd)

        self.after(120, self._restore_sash)

        self.editor.bind("<KeyRelease>",    self._on_key_release)
        self.editor.bind("<KeyPress>",      self._on_key_press)
        self.editor.bind("<ButtonRelease>", self._update_cursor)
        self.editor.bind("<Configure>",     self._redraw_gutter)

        if self._tabs:
            for t in self._tabs:
                self.tab_bar.add_tab(t)
            self.tab_bar.set_active(self._active_idx)
            self._load_tab(self._active_idx)
        else:
            self._new_tab()

        self.protocol("WM_DELETE_WINDOW", self._on_close)

    def _restore_sash(self):
        try:
            total = self.vert_pane.winfo_height()
            self.vert_pane.sash_place(0, 0, max(120, total - self._term_height))
        except Exception:
            pass

    def _ey_scroll(self, *a):
        self._esy.set(*a)
        self._redraw_gutter()

    def _ey_view(self, *a):
        self.editor.yview(*a)
        self._redraw_gutter()

    def _redraw_gutter(self, _=None):
        self.gutter.delete("all")
        i = self.editor.index("@0,0")
        while True:
            dl = self.editor.dlineinfo(i)
            if dl is None: break
            self.gutter.create_text(38, dl[1]+2, anchor="ne",
                text=i.split(".")[0], fill=MUTED, font=("Courier New", 11))
            ni = self.editor.index(f"{i}+1line")
            if ni == i: break
            i = ni

    def _setup_tags(self):
        self.editor.tag_configure("keyword",      foreground=SYN_KW)
        self.editor.tag_configure("type",         foreground=SYN_TYPE)
        self.editor.tag_configure("string",       foreground=SYN_STR)
        self.editor.tag_configure("comment",      foreground=SYN_CMNT,
                                   font=("Courier New", 13, "italic"))
        self.editor.tag_configure("number",       foreground=SYN_NUM)
        self.editor.tag_configure("operator",     foreground=SYN_KW)
        self.editor.tag_configure("language",     foreground=SYN_LANG)
        self.editor.tag_configure("current_line", background="#1c2128")
        self.editor.tag_configure("search_hit",   background=WARN, foreground=BG)

    def _highlight(self):
        text = self.editor.get("1.0", "end")
        for tag in ("keyword","type","string","comment","number","operator","language"):
            self.editor.tag_remove(tag, "1.0", "end")
        offset = 0
        for line in text.splitlines(True):
            cp = _strip_comment(line)
            cs = len(cp)
            if cs < len(line):
                self.editor.tag_add("comment",
                    f"1.0+{offset+cs}c", f"1.0+{offset+len(line)}c")
            i = 0; in_s = in_d = esc = False; si = None
            while i < cs:
                ch = line[i]
                if esc:        esc = False; i+=1; continue
                if ch == "\\": esc = True;  i+=1; continue
                if ch=="'" and not in_d:
                    if not in_s: in_s=True; si=i
                    else:
                        in_s=False
                        self.editor.tag_add("string",f"1.0+{offset+si}c",f"1.0+{offset+i+1}c")
                        si=None
                    i+=1; continue
                if ch=='"' and not in_s:
                    if not in_d: in_d=True; si=i
                    else:
                        in_d=False
                        self.editor.tag_add("string",f"1.0+{offset+si}c",f"1.0+{offset+i+1}c")
                        si=None
                    i+=1; continue
                i+=1
            offset += len(line)
        for m in re.finditer(r"\b\d+(\.\d+)?\b", text):
            self.editor.tag_add("number", f"1.0+{m.start()}c", f"1.0+{m.end()}c")
        for m in re.finditer(r"::|==|!=|<=|>=|<|>|\+|-|\*|/|=", text):
            self.editor.tag_add("operator", f"1.0+{m.start()}c", f"1.0+{m.end()}c")
        lp = r"\b("+"|".join(re.escape(t) for t in LANG_TOKENS)+r")\b"
        for m in re.finditer(lp, text, re.IGNORECASE):
            self.editor.tag_add("language", f"1.0+{m.start()}c", f"1.0+{m.end()}c")
        tp = r"\b("+"|".join(re.escape(t) for t in TYPES)+r")\b"
        for m in re.finditer(tp, text, re.IGNORECASE):
            self.editor.tag_add("type", f"1.0+{m.start()}c", f"1.0+{m.end()}c")
        for kw in sorted(KEYWORDS, key=lambda k: -len(k)):
            for m in re.finditer(rf"\b{re.escape(kw)}\b", text, re.IGNORECASE):
                self.editor.tag_add("keyword", f"1.0+{m.start()}c", f"1.0+{m.end()}c")
        self.editor.tag_remove("current_line", "1.0", "end")
        ln = self.editor.index("insert").split(".")[0]
        self.editor.tag_add("current_line", f"{ln}.0", f"{ln}.0 lineend+1c")

    def _on_key_release(self, _=None):
        self._highlight(); self._redraw_gutter(); self._update_cursor()
        tab = self._active_tab
        if tab:
            tab.content  = self.editor.get("1.0","end-1c")
            tab.modified = True
            self.tab_bar.mark_modified(self._active_idx, True)
        if self.autosave and self.current_file:
            self._autosave_debounce()

    def _on_key_press(self, event):
        if event.keysym == "Return":  return self._handle_return()
        if event.keysym == "Tab":
            self.editor.insert("insert", INDENT); return "break"
        if event.state & 0x4 and event.keysym == "space":
            self._show_autocomplete(); return "break"
        return None

    def _handle_return(self):
        lt = self.editor.get("insert linestart", "insert")
        cl = _strip_comment(lt)
        ws = re.match(r"\s*", lt).group(0)
        mp = re.match(r"^\s*prog\s+(.+?)\s*$",                    cl, re.IGNORECASE)
        mu = re.match(r"^\s*using\s+([A-Za-z_]\w*)\s+do\s*$",     cl, re.IGNORECASE)
        mb = re.match(r"^\s*(if|for|while|dowhile|do|else|elif)\b",cl, re.IGNORECASE)
        if mp:
            inner=ws+INDENT; closing=f"{ws}end prog"
            self.editor.insert("insert",f"\n{inner}\n{closing}")
            ln=int(self.editor.index("insert").split(".")[0])
            self.editor.mark_set("insert",f"{ln-1}.{len(inner)}")
            self.editor.see("insert")
        elif mu:
            block=mu.group(1); inner=ws+INDENT; closing=f"{ws}end {block}"
            self.editor.insert("insert",f"\n{inner}\n{closing}")
            ln=int(self.editor.index("insert").split(".")[0])
            self.editor.mark_set("insert",f"{ln-1}.{len(inner)}")
            self.editor.see("insert")
        elif mb:
            self.editor.insert("insert",f"\n{ws}{INDENT}")
        else:
            self.editor.insert("insert",f"\n{ws}")
        self._highlight(); return "break"

    def _show_autocomplete(self):
        if self._autocomplete:
            try: self._autocomplete.destroy()
            except Exception: pass
        line  = self.editor.get("insert linestart","insert")
        m     = re.search(r"\b(\w+)$", line)
        pre   = m.group(1).lower() if m else ""
        words = [k for k in ALL_KEYWORDS if k.startswith(pre)]
        if not words: return
        bb = self.editor.bbox("insert")
        if not bb: return
        x = self.editor.winfo_rootx()+bb[0]
        y = self.editor.winfo_rooty()+bb[1]+bb[3]
        self._autocomplete = AutoComplete(self, words, self._insert_completion, x, y)

    def _insert_completion(self, word):
        line = self.editor.get("insert linestart","insert")
        m = re.search(r"\b(\w+)$", line)
        if m: self.editor.delete(f"insert-{len(m.group(1))}c","insert")
        self.editor.insert("insert", word)
        self._highlight()

    def toggle_find(self):
        if self._find_visible:
            self.find_bar.pack_forget(); self._find_visible=False
        else:
            self.find_bar.pack(fill="x", after=self.toolbar); self._find_visible=True

    def _find_next(self):
        q = self.find_var.get()
        if not q: return
        self.editor.tag_remove("search_hit","1.0","end")
        pos = self.editor.search(q, self.editor.index("insert")+"+1c",
                                 stopindex="end", nocase=True)
        if not pos: pos = self.editor.search(q,"1.0",stopindex="end",nocase=True)
        if pos:
            end = f"{pos}+{len(q)}c"
            self.editor.tag_add("search_hit",pos,end)
            self.editor.mark_set("insert",end)
            self.editor.see(pos)

    def _replace_one(self):
        q = self.find_var.get()
        if not q: return
        self.editor.tag_remove("search_hit","1.0","end")
        pos = self.editor.search(q,"1.0",stopindex="end",nocase=True)
        if pos:
            self.editor.delete(pos,f"{pos}+{len(q)}c")
            self.editor.insert(pos, self.replace_var.get())
            self._highlight()

    def _replace_all(self):
        q = self.find_var.get()
        if not q: return
        c = self.editor.get("1.0","end-1c")
        self.editor.delete("1.0","end")
        self.editor.insert("1.0",
            re.sub(re.escape(q), self.replace_var.get(), c, flags=re.IGNORECASE))
        self._highlight()

    # ── Tabs ─────────────────────────────────────────────────────────────────
    def _new_tab(self, path=None):
        tab = EditorTab(path)
        idx = self.tab_bar.add_tab(tab)
        self._tabs.append(tab)
        self._active_idx = idx
        self.tab_bar.set_active(idx)
        if path:
            self._do_load_file(tab, path)
        else:
            self.editor.delete("1.0","end")
            self.editor.edit_reset()
            self.title_lbl.configure(text="untitled")
            self.title(f"CLIDE v{VERSION} — untitled")

    def _switch_tab(self, idx):
        if 0 <= self._active_idx < len(self._tabs):
            self._tabs[self._active_idx].content = self.editor.get("1.0","end-1c")
        self._active_idx = idx
        self.tab_bar.set_active(idx)
        self._load_tab(idx)

    def _load_tab(self, idx):
        if not (0 <= idx < len(self._tabs)): return
        tab = self._tabs[idx]
        self.editor.delete("1.0","end")
        if tab.path and not tab.content:
            try: tab.content = Path(tab.path).read_text(encoding="utf-8",errors="replace")
            except Exception: pass
        self.editor.insert("1.0", tab.content)
        self.editor.edit_reset()
        name = Path(tab.path).name if tab.path else "untitled"
        self.title_lbl.configure(text=name)
        self.title(f"CLIDE v{VERSION} — {name}")
        if tab.path: self.sidebar.set_dir(tab.path)
        self._highlight(); self._redraw_gutter()

    def _close_tab(self, idx):
        if len(self._tabs) == 1:
            t = self._tabs[0]
            t.path=None; t.content=""; t.modified=False
            self.tab_bar.mark_modified(0,False)
            self.tab_bar._redraw()
            self.editor.delete("1.0","end")
            self.title_lbl.configure(text="untitled")
            self.title(f"CLIDE v{VERSION} — untitled")
            return
        tab = self._tabs[idx]
        if tab.modified:
            name = Path(tab.path).name if tab.path else "untitled"
            ans  = messagebox.askyesnocancel("Unsaved changes",
                       f"'{name}' has unsaved changes. Save before closing?")
            if ans is None: return
            if ans and tab.path:
                Path(tab.path).write_text(tab.content, encoding="utf-8")
        self._tabs.pop(idx)
        self.tab_bar.remove_tab(idx)
        new = max(0, idx-1)
        self._active_idx = new
        self.tab_bar.set_active(new)
        self._load_tab(new)

    # ── File ops ──────────────────────────────────────────────────────────────
    def new_file(self): self._new_tab()

    def open_file(self):
        initial = str(Path(self.current_file).parent) if self.current_file else str(Path.home())
        path = ask_open_file(self, initial_dir=initial)
        if path: self._open_path(path)

    def _open_from_browser(self, path): self._open_path(path)

    def _open_path(self, path):
        for i, t in enumerate(self._tabs):
            if t.path == path: self._switch_tab(i); return
        tab = self._active_tab
        if tab and not tab.path and not tab.modified:
            tab.path = path
            self._do_load_file(tab, path)
            self.tab_bar._redraw()
        else:
            self._new_tab(path)
        self._push_recent(path)
        self._save_settings()

    def _do_load_file(self, tab, path):
        try:
            content = Path(path).read_text(encoding="utf-8",errors="replace")
            tab.content=content; tab.path=path; tab.modified=False
            self.editor.delete("1.0","end")
            self.editor.insert("1.0",content)
            self.editor.edit_reset()
            name = Path(path).name
            self.title_lbl.configure(text=name)
            self.title(f"CLIDE v{VERSION} — {name}")
            self.sidebar.set_dir(path)
            self._highlight(); self._redraw_gutter()
            self.log(f"Opened: {path}")
        except Exception as e:
            messagebox.showerror("Open Error", str(e))

    def save_file(self):
        tab = self._active_tab
        if not tab: return
        if not tab.path: self.save_file_as(); return
        self._write_file(tab)

    def save_file_as(self):
        tab = self._active_tab
        if not tab: return
        initial_file = Path(tab.path).name if tab.path else ""
        initial_dir  = str(Path(tab.path).parent) if tab.path else str(Path.home())
        path = ask_save_file(self, initial_dir=initial_dir,
                             default_ext=".cow", initial_file=initial_file)
        if path:
            tab.path = path
            self._write_file(tab)
            name = Path(path).name
            self.title_lbl.configure(text=name)
            self.title(f"CLIDE v{VERSION} — {name}")
            self.tab_bar._redraw()
            self._push_recent(path)
            self._save_settings()

    def _write_file(self, tab):
        try:
            content = self.editor.get("1.0","end-1c")
            tab.content=content; tab.modified=False
            Path(tab.path).write_text(content, encoding="utf-8")
            self.tab_bar.mark_modified(self._active_idx, False)
            self.log(f"Saved: {tab.path}")
            self._save_settings()
        except Exception as e:
            messagebox.showerror("Save Error", str(e))

    # ── Open Recent ───────────────────────────────────────────────────────────
    def _open_recent_menu(self):
        if not self._recent:
            messagebox.showinfo("CLIDE","No recent files yet 🐄"); return
        menu = tk.Menu(self, tearoff=0, bg="#21262d", fg=TEXT,
                       activebackground=ACCENT2, activeforeground=BG,
                       font=("Courier New",11), bd=0, relief="flat")
        for path in self._recent:
            p = Path(path)
            menu.add_command(label=f"  {p.name}  —  {str(p.parent)}",
                             command=lambda p=path: self._open_path(p))
        menu.add_separator()
        menu.add_command(label="  Clear recent list", command=self._clear_recent)
        try:
            menu.tk_popup(self.winfo_pointerx(), self.winfo_pointery())
        finally:
            menu.grab_release()

    def _clear_recent(self):
        self._recent = []; self._save_settings()

    # ── Autosave ──────────────────────────────────────────────────────────────
    def _on_autosave_toggle(self):
        self.autosave = self._autosave_var.get()
        self._save_settings()

    def _autosave_debounce(self):
        if hasattr(self,"_as_timer"):
            try: self.after_cancel(self._as_timer)
            except Exception: pass
        self._as_timer = self.after(2000, self._do_autosave)

    def _do_autosave(self):
        if self.autosave and self.current_file: self.save_file()

    def _schedule_autosave(self):
        if not self.winfo_exists(): return
        if self.autosave and self.current_file: self.save_file()
        self._as_job = self.after(30_000, self._schedule_autosave)

    # ── Run CowLang ───────────────────────────────────────────────────────────
    def run_cowlang(self):
        if not self.current_file:
            messagebox.showerror("CLIDE","Save the file first, fr 💀"); return
        self.save_file()
        self._clear_terminal()
        self.log(f"Running: {self.current_file}")
        self.log(f"Translator: {self._cowlang_exe}\n")
        threading.Thread(target=self._run_thread, daemon=True).start()

    def _run_thread(self):
        try:
            if not Path(self._cowlang_exe).exists():
                self.log(f"ERROR: translator not found:\n  {self._cowlang_exe}")
                self.log("Run the CowLang Installer or set path in Settings.")
                return
            clt_dir = str(Path(self._cowlang_exe).parent)
            src     = Path(self.current_file).read_text(encoding="utf-8")
            start   = time.time()

            # Pass file path as argv[1] AND pipe source via stdin.
            # This satisfies both CLT builds:
            #   - builds that check argc/argv open the file directly
            #   - builds that read stdin get the source piped in
            # Either way the GFX demo is only triggered when BOTH are absent.
            proc = subprocess.Popen(
                [self._cowlang_exe, self.current_file],
                cwd=clt_dir,
                stdin=subprocess.PIPE,
                stdout=subprocess.PIPE,
                stderr=subprocess.STDOUT,
                text=True,
            )
            stdout, _ = proc.communicate(input=src)
            elapsed   = int((time.time()-start)*1000)

            # Filter known GFX-demo noise lines
            GFX_NOISE = {
                "no input file provided. starting gfx demo only.",
                "no input file provided.",
                "starting gfx demo only.",
                "gfx demo",
                "cowlang graphical demo",
            }
            for line in stdout.splitlines():
                if line.lower().strip() in GFX_NOISE: continue
                self.log(line)
            self.log(f"\n── done in {elapsed}ms | exit {proc.returncode} ──")

        except Exception as e:
            self.log(f"ERROR: {e}")

    # ── Terminal ──────────────────────────────────────────────────────────────
    def log(self, msg):
        def _do():
            self.terminal.configure(state="normal")
            self.terminal.insert("end", msg+"\n")
            self.terminal.see("end")
            self.terminal.configure(state="disabled")
        self.after(0, _do)

    def _clear_terminal(self):
        self.terminal.configure(state="normal")
        self.terminal.delete("1.0","end")
        self.terminal.configure(state="disabled")

    def _run_shell_cmd(self, _=None):
        cmd = self.term_input.get().strip()
        if not cmd: return
        self.term_input.delete(0,"end")
        self.log(f"$ {cmd}")
        cwd = str(Path(self.current_file).parent) if self.current_file else None
        threading.Thread(target=self._shell_thread, args=(cmd,cwd), daemon=True).start()

    def _shell_thread(self, cmd, cwd):
        try:
            proc = subprocess.Popen(cmd, shell=True, cwd=cwd,
                stdout=subprocess.PIPE, stderr=subprocess.STDOUT, text=True)
            for line in proc.stdout: self.log(line.rstrip())
            proc.wait()
            self.log(f"(exit {proc.returncode})")
        except Exception as e: self.log(f"Shell error: {e}")

    def _update_cursor(self, _=None):
        idx = self.editor.index("insert")
        ln, col = idx.split(".")
        self.cursor_var.set(f"Ln {ln}, Col {int(col)+1}")

    # ── Fullscreen ────────────────────────────────────────────────────────────
    def _toggle_fullscreen(self):
        self.attributes("-fullscreen", not self.attributes("-fullscreen"))
        self.after(60, self._force_repaint)

    def _on_configure(self, event=None):
        if event and event.widget is not self: return
        self.after(25, self._force_repaint)

    def _force_repaint(self):
        try:
            self.update_idletasks()
            def _walk(w):
                try:
                    if hasattr(w,"_draw"): w._draw(no_color_updates=False)
                except Exception: pass
                for c in w.winfo_children(): _walk(c)
            _walk(self)
            self.update_idletasks()
        except Exception: pass

    # ── Settings dialog ───────────────────────────────────────────────────────
    def _open_settings(self):
        dlg = ctk.CTkToplevel(self)
        dlg.title("CLIDE Settings"); dlg.geometry("520x230")
        dlg.configure(fg_color=PANEL); dlg.grab_set()
        ctk.CTkLabel(dlg, text="CowLang Translator Path",
                     font=ctk.CTkFont("Courier New",12,"bold"),
                     text_color=TEXT).pack(anchor="w",padx=16,pady=(16,4))
        exe_var = ctk.StringVar(value=self._cowlang_exe)
        row = ctk.CTkFrame(dlg, fg_color="transparent")
        row.pack(fill="x", padx=16)
        ctk.CTkEntry(row, textvariable=exe_var, font=ctk.CTkFont("Courier New",11),
                     fg_color="#21262d", border_color=BORDER, text_color=TEXT,
                     height=32).pack(side="left",fill="x",expand=True,padx=(0,8))
        def _browse():
            p = ask_open_exe(dlg, str(Path(self._cowlang_exe).parent))
            if p: exe_var.set(p)
        ctk.CTkButton(row, text="Browse", width=70, height=32,
                      fg_color="#21262d", hover_color="#30363d", text_color=ACCENT2,
                      border_color=BORDER, border_width=1,
                      font=ctk.CTkFont("Courier New",11),
                      command=_browse).pack(side="left")
        def _save():
            self._cowlang_exe=exe_var.get(); self._save_settings(); dlg.destroy()
        ctk.CTkButton(dlg, text="Save", command=_save, height=36,
                      fg_color=ACCENT, hover_color="#2ea043", text_color=BG,
                      font=ctk.CTkFont("Courier New",12,"bold")
                      ).pack(padx=16,pady=16,fill="x")

    # ── Keybindings ───────────────────────────────────────────────────────────
    def _apply_keybindings(self):
        self.bind("<Control-s>",     lambda e: self.save_file())
        self.bind("<Control-S>",     lambda e: self.save_file_as())
        self.bind("<Control-o>",     lambda e: self.open_file())
        self.bind("<Control-n>",     lambda e: self.new_file())
        self.bind("<Control-w>",     lambda e: self._close_tab(self._active_idx))
        self.bind("<Control-Tab>",   lambda e: self._switch_tab(
                                         (self._active_idx+1) % max(1,len(self._tabs))))
        self.bind("<Control-f>",     lambda e: self.toggle_find())
        self.bind("<F5>",            lambda e: self.run_cowlang())
        self.bind("<F11>",           lambda e: self._toggle_fullscreen())
        self.bind("<Control-z>",     lambda e: self.editor.edit_undo())
        self.bind("<Control-y>",     lambda e: self.editor.edit_redo())
        self.bind("<Control-slash>", self._toggle_comment)
        self.bind("<Configure>",     self._on_configure)

    def _toggle_comment(self, _=None):
        try:
            s = int(self.editor.index("sel.first").split(".")[0])
            e = int(self.editor.index("sel.last").split(".")[0])
        except tk.TclError:
            s = e = int(self.editor.index("insert").split(".")[0])
        for ln in range(s, e+1):
            line = self.editor.get(f"{ln}.0",f"{ln}.end")
            nl   = line.replace("!","",1) if line.lstrip().startswith("!") else "!"+line
            self.editor.delete(f"{ln}.0",f"{ln}.end")
            self.editor.insert(f"{ln}.0",nl)
        self._highlight()

    def _on_close(self):
        unsaved = [t for t in self._tabs if t.modified and t.path]
        if unsaved:
            names = ", ".join(Path(t.path).name for t in unsaved)
            ans   = messagebox.askyesnocancel("Unsaved changes",
                        f"Unsaved changes in: {names}\nSave all before closing?")
            if ans is None: return
            if ans:
                for t in unsaved:
                    try: Path(t.path).write_text(t.content, encoding="utf-8")
                    except Exception: pass
        self._save_settings()
        for attr in ("_as_timer","_as_job"):
            job = getattr(self,attr,None)
            if job:
                try: self.after_cancel(job)
                except Exception: pass
        self.destroy()


if __name__ == "__main__":
    app = CLIDE()
    app.mainloop()