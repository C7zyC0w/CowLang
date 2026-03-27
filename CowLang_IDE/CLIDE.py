"""
CLIDE — CowLang Integrated Development Environment  v1.2.0
===========================================================
Requirements:  pip install customtkinter pillow
Run:           python clide.py
"""

import customtkinter as ctk
import tkinter as tk
from tkinter import messagebox
import threading, subprocess, re, time, json, platform, base64, io
from pathlib import Path

try:
    from PIL import Image as PILImage, ImageTk
    _PIL = True
except ImportError:
    _PIL = False

ctk.set_appearance_mode("dark")
ctk.set_default_color_theme("blue")
VERSION = "1.3.0"

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

# ── Embedded icon data (base64 PNG) ──────────────────────────────────────────
# .cow — CLT pixel cow logo
_COW_16 = 'iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAABCGlDQ1BJQ0MgUHJvZmlsZQAAeJxjYGA8wQAELAYMDLl5JUVB7k4KEZFRCuwPGBiBEAwSk4sLGHADoKpv1yBqL+viUYcLcKakFicD6Q9ArFIEtBxopAiQLZIOYWuA2EkQtg2IXV5SUAJkB4DYRSFBzkB2CpCtkY7ETkJiJxcUgdT3ANk2uTmlyQh3M/Ck5oUGA2kOIJZhKGYIYnBncAL5H6IkfxEDg8VXBgbmCQixpJkMDNtbGRgkbiHEVBYwMPC3MDBsO48QQ4RJQWJRIliIBYiZ0tIYGD4tZ2DgjWRgEL7AwMAVDQsIHG5TALvNnSEfCNMZchhSgSKeDHkMyQx6QJYRgwGDIYMZAKbWPz9HbOBQAAAA4ElEQVR4nKWSsQ3CMBBFv6NUqdA1TMAUKajSZob0LOUZaFMxBxPQfDFBPkWwMeaCkPiSlcvdu++zkwBA+EPNP80fBrfrBbfrZRP26u4EnsmWcUsyv5hZBknCzD4a9ocjyp4WAHbnc24qzZLKPMnM38cRAYBKoDbwajWjcpHUqe8kSSRFUpJ06jvVLAAFVP9B2tHM3uJvcp0BSPM60TfG/YwkscQIYL3gFHtqAEDza6WmWsmkZpFGTSMtMean5rVW5mq+9cZaYkQzTXnXMq4Vno6vxIBNuJmmfIwwFAYu/aMehOuy+fwYk7gAAAAASUVORK5CYII='
_COW_24 = 'iVBORw0KGgoAAAANSUhEUgAAABgAAAAYCAYAAADgdz34AAABCGlDQ1BJQ0MgUHJvZmlsZQAAeJxjYGA8wQAELAYMDLl5JUVB7k4KEZFRCuwPGBiBEAwSk4sLGHADoKpv1yBqL+viUYcLcKakFicD6Q9ArFIEtBxopAiQLZIOYWuA2EkQtg2IXV5SUAJkB4DYRSFBzkB2CpCtkY7ETkJiJxcUgdT3ANk2uTmlyQh3M/Ck5oUGA2kOIJZhKGYIYnBncAL5H6IkfxEDg8VXBgbmCQixpJkMDNtbGRgkbiHEVBYwMPC3MDBsO48QQ4RJQWJRIliIBYiZ0tIYGD4tZ2DgjWRgEL7AwMAVDQsIHG5TALvNnSEfCNMZchhSgSKeDHkMyQx6QJYRgwGDIYMZAKbWPz9HbOBQAAAB30lEQVR4nL2VwWoUQRCGv+7ZHNSDs0MgCOJNfAlBBHNTn2EewFfwRTxokycwe5GwiMTX2LMhEugFhRgwU5VD78z09Oz2JodNQTPTXVV/Vf1dU2MAZYdidwl+LwEm6UEBnC1Ow0aEgxevswAGy/niR7c/eP5qoM9WoMUeRRYcLJJNYFTBAEAbzhano6xaOW8rzQXw3o8Oq6rid+Tc2lRVtRGoTSLFmwBcTSwPmtCtDy+vgkaU0MEGY8IaiQiBqL7TW7tCLNemCQGe/PuLfvseLHRvlMV0Oh1hpzbt/vHxcYiN4c/7d334dVS1ElOTs1vnl/8OxKArZlKKjDE0VkCzfdJ3UXqB3nuMbfj49hmqgePlctnpy7Lkw8tH/C/g08/LjQGGN5QESIOvO9smOx8VECrYugyozlHvvXrvb+Wz8suPa+895WyGKtinNfrLdTpb11uzj1ogZUvCDWVZtGCFMI4Su0KgWQXYt3BxMhxa5hBQKGdfN8KL+xzC1DU6H/qLsRRvwpnu28AvEXfinOrRUffefHGqc7olzq1sbfQ+xrn/H04sKoI4h61rLAXXznU6W9dItN8khkARFyeJ4jA8bwMSd5POe1+IKhBjCe3QPqHAIqwZ0xnpcaIK7oRwR7kByWjlb+4PE90AAAAASUVORK5CYII='
# .cl — CL logo
_CL_16  = 'iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAABCGlDQ1BJQ0MgUHJvZmlsZQAAeJxjYGA8wQAELAYMDLl5JUVB7k4KEZFRCuwPGBiBEAwSk4sLGHADoKpv1yBqL+viUYcLcKakFicD6Q9ArFIEtBxopAiQLZIOYWuA2EkQtg2IXV5SUAJkB4DYRSFBzkB2CpCtkY7ETkJiJxcUgdT3ANk2uTmlyQh3M/Ck5oUGA2kOIJZhKGYIYnBncAL5H6IkfxEDg8VXBgbmCQixpJkMDNtbGRgkbiHEVBYwMPC3MDBsO48QQ4RJQWJRIliIBYiZ0tIYGD4tZ2DgjWRgEL7AwMAVDQsIHG5TALvNnSEfCNMZchhSgSKeDHkMyQx6QJYRgwGDIYMZAKbWPz9HbOBQAAABIUlEQVR4nN2TvUoDQRDHfzN7l4/CVoi14EtEAmktrSwEOx8sASHkDQIWAZ/CPthaJFxytzMWe+YST9G0DiwLM/v/2J1ZWQ1HjmypJOeUcIFupWQAg9tHODs/iYD1G6vZlAwMBldQFCCkBVQKwds4ibDrQKd3iTjJAbsCeRiDK4gBCVxOn9vWA3TvxvhkgYvVBAAOYPUOMfx8d6Fx2hAI2KSt+FvoyYj/R3DUBZemYEBmScPF9jWNx8PREARI78f7g/2ovM4XXNxc0+sHqtqri7YJihyISfWzvxsxyAJoypvVCDHIdS+UAXQixKcl7o29Khjd95I4W1LOX4i76sCsY14mAkfR7RqygwcAQpXcBN/CZs3XwVRTQJHVcPTNl/l7fACb+2VCCBXNJwAAAABJRU5ErkJggg=='
_CL_24  = 'iVBORw0KGgoAAAANSUhEUgAAABgAAAAYCAYAAADgdz34AAABCGlDQ1BJQ0MgUHJvZmlsZQAAeJxjYGA8wQAELAYMDLl5JUVB7k4KEZFRCuwPGBiBEAwSk4sLGHADoKpv1yBqL+viUYcLcKakFicD6Q9ArFIEtBxopAiQLZIOYWuA2EkQtg2IXV5SUAJkB4DYRSFBzkB2CpCtkY7ETkJiJxcUgdT3ANk2uTmlyQh3M/Ck5oUGA2kOIJZhKGYIYnBncAL5H6IkfxEDg8VXBgbmCQixpJkMDNtbGRgkbiHEVBYwMPC3MDBsO48QQ4RJQWJRIliIBYiZ0tIYGD4tZ2DgjWRgEL7AwMAVDQsIHG5TALvNnSEfCNMZchhSgSKeDHkMyQx6QJYRgwGDIYMZAKbWPz9HbOBQAAACKUlEQVR4nN2Vz04UQRDGf1Xds/yJRALBRIPowZvxAYxEMJ7kYnwAr8ZH8OprmHhSDnJSoye5GHgQwoHEi4EILDszXeVh1mVnZgUl68UvqUxSU13fV9Xd1bK3vOLqhmkCm2JccDEEIwIUqogFgvnYCAgFEIi44qIsPn8BZEMRNqRGEQPkPNXVVwz2Xr1EMNRFub78EGcCU8PUyKORRwYmbiCGn2GVIBvEXlt9BIAKJdy4hWji8PgECRkdMjI5NWKATudM9aaQH3fpFTmgsHQTgBhMITMIkcvP1hAHR+urxbC3X88kEIejKzPMP7lP8foLUasWn2YqCvB+H8VqNulg4ZwNGEIcqjbWZcD2+ntuZ7MNdYp1C/QvSNoEInQc7kzMMdMtG2EGF0gONJs9fvxPBD7GMTGSQC62iX9O8I9QI8gForU5i+C4ULvlMqKjam1fbDoOJkFSncQEOj3jcBpyhbkudLO2kJNWtgaBOCw9XqGUupQsKfmbTS6t3SVNB1jfYvrpasU8hDCi4xWBQJmq25ssIcBwBwyv3oJORhIIDiTAfPBEVK5EQHFNA3/89Tdq4NunbcqyOSZgqoDUS/z4vEWejAWF3Y1NYmz3JOQJ6dUqMPi+j19dYP6g21rQLxAEZo+sP21hsQfeK0bGm4LuHwCgpcLOh3eIg7oOrJ+nMlfEleap/l28urLzcaMSt3fvgSslahFlxDm7IJIaSZSIGIZiYXzJK1TV/gTWYODAAyrY8gAAAABJRU5ErkJggg=='

# Cache for PhotoImage objects (must stay alive to avoid GC)
_IMG_CACHE: dict = {}

def _get_photo(b64: str, key: str):
    if key in _IMG_CACHE:
        return _IMG_CACHE[key]
    if not _PIL:
        return None
    try:
        raw = base64.b64decode(b64)
        pimg = PILImage.open(io.BytesIO(raw)).convert("RGBA")
        photo = ImageTk.PhotoImage(pimg)
        _IMG_CACHE[key] = photo
        return photo
    except Exception:
        return None

def get_cow_icon(size=16):
    return _get_photo(_COW_16 if size == 16 else _COW_24, f"cow{size}")

def get_cl_icon(size=16):
    return _get_photo(_CL_16 if size == 16 else _CL_24, f"cl{size}")

# ── CowLang file extensions ───────────────────────────────────────────────────
CL_EXTS = {".cow", ".cl", ".cowp", ".clp", ".ox"}
FILETYPES = [
    ("CowLang Files",         "*.cow *.cl *.cowp *.clp *.ox"),
    ("CL Source — pre/beta",  "*.cow"),
    ("CL Full Release",       "*.cl"),
    ("CLP Source — pre/beta", "*.cowp"),
    ("CLP Full Release",      "*.clp"),
    ("OX Files",              "*.ox")
    ("All Files",             "*.*"),
]

def _ext_emoji(ext: str) -> str:
    return "🐄" if ext in CL_EXTS else "📄"

# ── Keywords ─────────────────────────────────────────────────────────────────
KEYWORDS = [
    "prog","program","end","end prog",
    "print","read","if","then","else","elif",
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

def _cfg_dir()  -> Path: return Path.home() / ".config" / "clide"
def _cfg_file() -> Path: return _cfg_dir() / "settings.json"

DEFAULT_EXE = str(
    Path.home() / "CowLang" / "CowLang-main" / "CowLangTranslator" /
    ("CLT_0.4.0.exe" if platform.system() == "Windows" else "CLT_0.4.0")
)

def _find_exe() -> str:
    p = Path.home() / "CowLang" / "cowlang.json"
    if p.exists():
        try:
            d = json.loads(p.read_text())
            if d.get("translator") and Path(d["translator"]).exists():
                return d["translator"]
        except Exception: pass
    return DEFAULT_EXE

def _strip_comment(line: str) -> str:
    in_s = in_d = esc = False
    for i, ch in enumerate(line):
        if esc:        esc=False; continue
        if ch=="\\":   esc=True;  continue
        if ch=="'" and not in_d: in_s=not in_s; continue
        if ch=='"' and not in_s: in_d=not in_d; continue
        if ch=="!" and not in_s and not in_d: return line[:i]
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
        self._entries    = []
        self._row_imgs   = []   # keep photo refs alive

        titles = {"open":"Open File","save":"Save As","dir":"Select Folder"}
        self.title(title or titles.get(mode,"Browse"))
        self.geometry("800x560"); self.minsize(600,400)
        self.configure(fg_color=BG); self.resizable(True,True)
        self._ext_var = tk.StringVar(value="CowLang Files")
        self._build_ui(initial_file)
        self._populate()
        self.update_idletasks(); self.deiconify()
        try: self.grab_set()
        except Exception: pass
        self.wait_window()

    def _build_ui(self, initial_file):
        nav = tk.Frame(self, bg=PANEL, height=42)
        nav.pack(fill="x"); nav.pack_propagate(False)
        for sym, fn in [("←",self._go_back),("→",self._go_forward),
                        ("↑",self._go_up),("⟳",self._populate)]:
            lb = tk.Label(nav,text=sym,bg=PANEL,fg=MUTED,font=("Courier New",14),cursor="hand2",padx=6)
            lb.bind("<Button-1>", lambda e,f=fn: f())
            lb.bind("<Enter>",    lambda e,l=lb: l.configure(fg=TEXT))
            lb.bind("<Leave>",    lambda e,l=lb: l.configure(fg=MUTED))
            lb.pack(side="left",pady=6)
        self._path_var = tk.StringVar(value=str(self._cwd))
        pe = tk.Entry(nav,textvariable=self._path_var,bg="#21262d",fg=TEXT,insertbackground=ACCENT2,
                      font=("Courier New",11),relief="flat",bd=4,
                      highlightthickness=1,highlightbackground=BORDER,highlightcolor=ACCENT2)
        pe.pack(side="left",fill="x",expand=True,padx=4,pady=8)
        pe.bind("<Return>",self._on_path_enter)
        ext_names = [r[0] for r in FILETYPES]
        om = tk.OptionMenu(nav,self._ext_var,*ext_names,command=lambda _:self._populate())
        om.config(bg="#21262d",fg=TEXT,activebackground=BORDER,activeforeground=TEXT,
                  highlightthickness=0,font=("Courier New",10),bd=0,relief="flat")
        om["menu"].config(bg="#21262d",fg=TEXT,font=("Courier New",10),
                          activebackground=ACCENT2,activeforeground=BG)
        om.pack(side="right",padx=8,pady=8)

        pane = tk.PanedWindow(self,orient="horizontal",bg=BORDER,sashwidth=4,sashrelief="flat",
                              handlepad=0,handlesize=0)
        pane.pack(fill="both",expand=True)

        places_frm = tk.Frame(pane,bg=PANEL,width=175)
        pane.add(places_frm,minsize=120,width=175,stretch="never")
        tk.Label(places_frm,text=" PLACES",bg=PANEL,fg=MUTED,
                 font=("Courier New",9,"bold"),anchor="w",pady=6).pack(fill="x")
        self._places_lb = tk.Listbox(places_frm,bg=PANEL,fg=TEXT,
            selectbackground=ACCENT2,selectforeground=BG,font=("Courier New",11),
            relief="flat",bd=0,activestyle="none",exportselection=False,highlightthickness=0)
        self._places_lb.pack(fill="both",expand=True)
        self._places_lb.bind("<ButtonRelease-1>",self._on_place_click)
        self._places = []
        self._fill_places()

        files_frm = tk.Frame(pane,bg=BG)
        pane.add(files_frm,stretch="always")
        hdr = tk.Frame(files_frm,bg="#21262d",height=26)
        hdr.pack(fill="x"); hdr.pack_propagate(False)
        tk.Label(hdr,text="  Name",bg="#21262d",fg=MUTED,
                 font=("Courier New",10,"bold"),anchor="w").pack(side="left",fill="x",expand=True)
        for t,w in [("Size",9),("Type",14)]:
            tk.Label(hdr,text=t,bg="#21262d",fg=MUTED,
                     font=("Courier New",10,"bold"),width=w).pack(side="left")

        lf = tk.Frame(files_frm,bg=BG)
        lf.pack(fill="both",expand=True)
        sb = tk.Scrollbar(lf,bg=PANEL,troughcolor=BG,relief="flat",bd=0,width=12)
        sb.pack(side="right",fill="y")
        self._file_lb = tk.Listbox(lf,bg=BG,fg=TEXT,selectbackground="#1f3a5f",selectforeground=TEXT,
            font=("Courier New",12),relief="flat",bd=0,activestyle="none",exportselection=False,
            highlightthickness=0,yscrollcommand=sb.set)
        self._file_lb.pack(fill="both",expand=True)
        sb.configure(command=self._file_lb.yview)
        self._file_lb.bind("<Double-Button-1>",self._on_double)
        self._file_lb.bind("<Return>",self._on_double)
        self._file_lb.bind("<ButtonRelease-1>",self._on_single)

        tk.Frame(self,bg=BORDER,height=1).pack(fill="x",side="bottom")
        bot = tk.Frame(self,bg=PANEL,height=46)
        bot.pack(fill="x",side="bottom"); bot.pack_propagate(False)
        lbl = "Folder:" if self.mode=="dir" else "File name:"
        tk.Label(bot,text=lbl,bg=PANEL,fg=MUTED,font=("Courier New",11)).pack(side="left",padx=(12,4))
        self._fname_var = tk.StringVar(value=initial_file)
        fe = tk.Entry(bot,textvariable=self._fname_var,bg="#21262d",fg=TEXT,
                      insertbackground=ACCENT2,font=("Courier New",11),relief="flat",bd=4,
                      highlightthickness=1,highlightbackground=BORDER,highlightcolor=ACCENT2,width=34)
        fe.pack(side="left",fill="x",expand=True,padx=4,pady=8)
        fe.bind("<Return>",self._confirm)
        cancel = tk.Label(bot,text="Cancel",bg=PANEL,fg=MUTED,font=("Courier New",11),
                          cursor="hand2",padx=14,pady=8)
        cancel.bind("<Button-1>",lambda e:self.destroy())
        cancel.bind("<Enter>",   lambda e:cancel.configure(fg=TEXT))
        cancel.bind("<Leave>",   lambda e:cancel.configure(fg=MUTED))
        cancel.pack(side="right",padx=(4,14))
        ok_text = {"open":"Open","save":"Save","dir":"Select"}[self.mode]
        ok = tk.Label(bot,text=ok_text,bg=ACCENT,fg=BG,font=("Courier New",11,"bold"),
                      cursor="hand2",padx=16,pady=8)
        ok.bind("<Button-1>",self._confirm)
        ok.bind("<Enter>",   lambda e:ok.configure(bg="#2ea043"))
        ok.bind("<Leave>",   lambda e:ok.configure(bg=ACCENT))
        ok.pack(side="right",padx=4)

    def _fill_places(self):
        home = Path.home()
        for label,path in [("Home",home),("Root",Path("/")),
                            ("Documents",home/"Documents"),("Downloads",home/"Downloads"),
                            ("Pictures",home/"Pictures"),("Music",home/"Music"),
                            ("Desktop",home/"Desktop")]:
            if path.exists():
                self._places.append(path)
                self._places_lb.insert("end",f"  {label}")
        mnt = Path("/mnt")
        if mnt.exists():
            try:
                for d in sorted(mnt.iterdir()):
                    if d.is_dir():
                        self._places.append(d)
                        self._places_lb.insert("end",f"  /mnt/{d.name}")
            except PermissionError: pass

    def _navigate(self,path):
        if not path.exists(): return
        self._cwd=path; self._path_var.set(str(path))
        self._history=self._history[:self._hist_idx+1]
        self._history.append(path); self._hist_idx=len(self._history)-1
        self._populate()
    def _go_back(self):
        if self._hist_idx>0:
            self._hist_idx-=1; self._cwd=self._history[self._hist_idx]
            self._path_var.set(str(self._cwd)); self._populate()
    def _go_forward(self):
        if self._hist_idx<len(self._history)-1:
            self._hist_idx+=1; self._cwd=self._history[self._hist_idx]
            self._path_var.set(str(self._cwd)); self._populate()
    def _go_up(self):
        p=self._cwd.parent
        if p!=self._cwd: self._navigate(p)
    def _on_path_enter(self,_=None):
        p=Path(self._path_var.get())
        if p.is_dir(): self._navigate(p)
        elif p.is_file(): self._fname_var.set(p.name); self._navigate(p.parent)
    def _on_place_click(self,_=None):
        sel=self._places_lb.curselection()
        if sel: self._navigate(self._places[sel[0]])

    def _get_filter_exts(self):
        sel=self._ext_var.get()
        for name,pattern in FILETYPES:
            if name==sel:
                if pattern=="*.*": return set()
                return {e.lstrip("*") for e in pattern.split()}
        return set()

    def _populate(self,_=None):
        self._file_lb.delete(0,"end")
        self._entries=[]; self._row_imgs=[]
        filter_exts=self._get_filter_exts()
        try:
            raw=sorted(self._cwd.iterdir(),key=lambda p:(p.is_file(),p.name.lower()))
        except PermissionError:
            self._file_lb.insert("end","  Permission denied"); return
        cow_img = get_cow_icon(16); cl_img = get_cl_icon(16)
        for e in raw:
            if e.name.startswith("."): continue
            if self.mode=="dir" and e.is_file(): continue
            ext=e.suffix.lower()
            if e.is_file() and filter_exts and ext not in filter_exts: continue
            # pick text icon (image icons require a canvas-based listbox, not standard)
            if e.is_dir():   icon="📁"
            elif ext==".cow":icon="🐄"
            elif ext==".cl": icon="🐄"
            elif ext==".cowp":icon="🐄"
            elif ext==".clp": icon="🐄"
            elif ext==".py":  icon="🐍"
            else:             icon="📄"
            size=self._fmt_size(e) if e.is_file() else ""
            kind=(ext.upper().lstrip(".")+" file" if ext else "File") if e.is_file() else "Folder"
            self._file_lb.insert("end",f"  {icon} {e.name:<40}{size:>9}   {kind}")
            self._entries.append(e)
        for i,e in enumerate(self._entries):
            if   e.is_dir():                  self._file_lb.itemconfig(i,fg=ACCENT2)
            elif e.suffix.lower() in CL_EXTS: self._file_lb.itemconfig(i,fg=ACCENT)

    def _fmt_size(self,p):
        try:
            s=p.stat().st_size
            for u in ("B","KB","MB","GB"):
                if s<1024: return f"{s:.0f} {u}"
                s/=1024
            return f"{s:.1f} TB"
        except Exception: return ""

    def _on_single(self,_=None):
        sel=self._file_lb.curselection()
        if sel and self._entries[sel[0]].is_file():
            self._fname_var.set(self._entries[sel[0]].name)
    def _on_double(self,_=None):
        sel=self._file_lb.curselection()
        if not sel: return
        e=self._entries[sel[0]]
        if e.is_dir(): self._navigate(e)
        else: self._fname_var.set(e.name); self._confirm()
    def _confirm(self,_=None):
        fname=self._fname_var.get().strip()
        if self.mode=="dir": self.result=str(self._cwd); self.destroy(); return
        if not fname: return
        p=Path(fname) if Path(fname).is_absolute() else self._cwd/fname
        if self.mode=="open":
            if p.is_dir(): self._navigate(p); return
            if not p.exists():
                messagebox.showerror("CLIDE",f"File not found:\n{p}",parent=self); return
            self.result=str(p)
        elif self.mode=="save":
            if p.is_dir(): self._navigate(p); return
            if not p.suffix: p=p.with_suffix(self.default_ext)
            if p.exists():
                if not messagebox.askyesno("Overwrite?",f"'{p.name}' already exists. Overwrite?",parent=self): return
            self.result=str(p)
        self.destroy()

def ask_open_file(parent,initial_dir=None):
    return CLIDEFileDialog(parent,mode="open",initial_dir=initial_dir).result
def ask_save_file(parent,initial_dir=None,default_ext=".cow",initial_file=""):
    return CLIDEFileDialog(parent,mode="save",initial_dir=initial_dir,
                           default_ext=default_ext,initial_file=initial_file).result
def ask_directory(parent,initial_dir=None):
    return CLIDEFileDialog(parent,mode="dir",initial_dir=initial_dir).result
def ask_open_exe(parent,initial_dir=None):
    return CLIDEFileDialog(parent,mode="open",title="Select CowLang Translator",
                           initial_dir=initial_dir or str(Path.home())).result


# ═══════════════════════════════════════════════════════════════════════════════
#  Autocomplete
# ═══════════════════════════════════════════════════════════════════════════════
class AutoComplete(tk.Toplevel):
    def __init__(self,parent,words,on_select,x,y):
        super().__init__(parent)
        self.overrideredirect(True); self.configure(bg=PANEL)
        self._on_select=on_select
        lb=tk.Listbox(self,bg=PANEL,fg=TEXT,selectbackground=ACCENT2,selectforeground=BG,
                      font=("Courier New",12),relief="flat",bd=0,activestyle="none",
                      exportselection=False,height=min(len(words),8),highlightthickness=0)
        lb.pack(fill="both",expand=True,padx=1,pady=1)
        for w in words: lb.insert("end",w)
        lb.selection_set(0)
        lb.bind("<Return>",         lambda _:self._pick(lb))
        lb.bind("<Double-Button-1>",lambda _:self._pick(lb))
        lb.bind("<Escape>",         lambda _:self.destroy())
        self.geometry(f"+{x}+{y}"); lb.focus_set()
    def _pick(self,lb):
        sel=lb.curselection()
        if sel: self._on_select(lb.get(sel[0]))
        self.destroy()


# ═══════════════════════════════════════════════════════════════════════════════
#  File Browser Sidebar
# ═══════════════════════════════════════════════════════════════════════════════
class FileBrowser(ctk.CTkFrame):
    def __init__(self,parent,on_open,**kw):
        super().__init__(parent,fg_color=PANEL,**kw)
        self.on_open=on_open
        self._root_path=Path.home()
        self._entries=[]

        hdr=ctk.CTkFrame(self,fg_color="#21262d",corner_radius=0,height=30)
        hdr.pack(fill="x"); hdr.pack_propagate(False)
        ctk.CTkLabel(hdr,text="EXPLORER",font=ctk.CTkFont("Courier New",10,"bold"),
                     text_color=MUTED).pack(side="left",padx=8)
        ctk.CTkButton(hdr,text="⟳",width=24,height=24,fg_color="transparent",
                      hover_color="#30363d",font=ctk.CTkFont("Courier New",12),
                      text_color=MUTED,command=self.refresh).pack(side="right",padx=4)
        ctk.CTkButton(hdr,text="📁",width=24,height=24,fg_color="transparent",
                      hover_color="#30363d",font=ctk.CTkFont("Courier New",12),
                      text_color=MUTED,command=self._browse_dir).pack(side="right")
        self._path_var=ctk.StringVar(value=str(self._root_path))
        ctk.CTkLabel(self,textvariable=self._path_var,font=ctk.CTkFont("Courier New",9),
                     text_color=MUTED,wraplength=170,anchor="w").pack(fill="x",padx=6,pady=2)
        self._lb=tk.Listbox(self,bg=PANEL,fg=TEXT,selectbackground=ACCENT2,selectforeground=BG,
            font=("Courier New",11),relief="flat",bd=0,activestyle="none",
            exportselection=False,highlightthickness=0)
        self._lb.pack(fill="both",expand=True)
        self._lb.bind("<Double-Button-1>",self._on_select)
        self._lb.bind("<Return>",self._on_select)
        self.refresh()

    def set_dir(self,path):
        p=Path(path); d=p.parent if p.is_file() else p
        if d.is_dir():
            self._root_path=d; self._path_var.set(str(d)); self.refresh()
    def _browse_dir(self):
        d=ask_directory(self.winfo_toplevel(),str(self._root_path))
        if d: self.set_dir(d)
    def refresh(self,_=None):
        self._lb.delete(0,"end"); self._entries=[]
        try:
            entries=sorted(self._root_path.iterdir(),key=lambda p:(p.is_file(),p.name.lower()))
            for e in entries:
                if e.name.startswith("."): continue
                ext=e.suffix.lower() if e.is_file() else ""
                if ext==".cow":   icon="🐄 "
                elif ext==".cl":  icon="🐄 "
                elif ext in CL_EXTS: icon="🐄 "
                elif e.is_dir():  icon="📁 "
                else:             icon="📄 "
                self._lb.insert("end",f"  {icon}{e.name}")
                color=ACCENT if (e.is_file() and e.suffix.lower() in CL_EXTS) else \
                      ACCENT2 if e.is_dir() else TEXT
                self._lb.itemconfig(len(self._entries),fg=color)
                self._entries.append(e)
        except PermissionError: pass
    def _on_select(self,_=None):
        sel=self._lb.curselection()
        if not sel: return
        e=self._entries[sel[0]]
        if e.is_dir(): self.set_dir(str(e))
        else:          self.on_open(str(e))


# ═══════════════════════════════════════════════════════════════════════════════
#  EditorTab
# ═══════════════════════════════════════════════════════════════════════════════
class EditorTab:
    def __init__(self,path=None):
        self.path=path; self.content=""; self.modified=False
    @property
    def display_name(self):
        base=Path(self.path).name if self.path else "untitled"
        return ("● "+base) if self.modified else base
    @property
    def ext(self):
        return Path(self.path).suffix.lower() if self.path else ""


# ═══════════════════════════════════════════════════════════════════════════════
#  Tab Bar — drag-to-reorder support
# ═══════════════════════════════════════════════════════════════════════════════
class TabBar(tk.Frame):
    H=34
    def __init__(self,parent,on_switch,on_close,on_reorder,**kw):
        super().__init__(parent,bg=TAB_BG,height=self.H,**kw)
        self.pack_propagate(False)
        self.on_switch  = on_switch
        self.on_close   = on_close
        self.on_reorder = on_reorder   # callback(from_idx, to_idx)
        self._tabs   = []
        self._active = -1
        self._frames = []
        self._drag_from = None

    def add_tab(self,tab):
        idx=len(self._tabs); self._tabs.append(tab); self._redraw(); return idx
    def remove_tab(self,idx):
        if 0<=idx<len(self._tabs):
            self._tabs.pop(idx)
            if self._active>=len(self._tabs): self._active=max(0,len(self._tabs)-1)
            self._redraw()
    def set_active(self,idx):
        self._active=idx; self._redraw()
    def mark_modified(self,idx,v):
        if 0<=idx<len(self._tabs):
            self._tabs[idx].modified=v; self._redraw()

    def _tab_index_at(self, x):
        """Return the tab index whose frame contains screen-x."""
        for i, frm in enumerate(self._frames):
            fx = frm.winfo_x()
            fw = frm.winfo_width()
            if fx <= x < fx + fw:
                return i
        return -1

    def _redraw(self):
        for f in self._frames: f.destroy()
        self._frames=[]
        for i,tab in enumerate(self._tabs):
            active=(i==self._active)
            bg=TAB_ACTIVE if active else TAB_BG
            frm=tk.Frame(self,bg=bg,cursor="hand2")
            frm.pack(side="left",fill="y",padx=(0,1))
            if active:
                tk.Frame(frm,bg=ACCENT,height=2).pack(side="bottom",fill="x")

            # icon label with image if PIL available
            ext = tab.ext
            img = None
            if ext == ".cow":  img = get_cow_icon(16)
            elif ext == ".cl": img = get_cl_icon(16)
            if img:
                icon_lbl = tk.Label(frm, image=img, bg=bg)
                icon_lbl.pack(side="left", padx=(8,2), pady=6)

            lbl=tk.Label(frm,text=tab.display_name,bg=bg,
                         fg=TEXT if active else MUTED,
                         font=("Courier New",11),padx=8 if img else 12,pady=6)
            lbl.pack(side="left")

            # Click to switch
            for w in (frm, lbl):
                w.bind("<Button-1>",      lambda e,i=i: self._on_press(e,i))
                w.bind("<B1-Motion>",     lambda e,i=i: self._on_drag(e,i))
                w.bind("<ButtonRelease-1>",lambda e,i=i: self._on_release(e,i))

            x_btn=tk.Label(frm,text="✕",bg=bg,fg=MUTED,font=("Courier New",10),padx=6)
            x_btn.pack(side="left")
            x_btn.bind("<Button-1>",  lambda e,i=i: self.on_close(i))
            x_btn.bind("<Enter>",     lambda e,l=x_btn: l.configure(fg=DANGER))
            x_btn.bind("<Leave>",     lambda e,l=x_btn: l.configure(fg=MUTED))
            self._frames.append(frm)

    def _on_press(self,event,idx):
        self._drag_from=idx
        self._click(idx)

    def _on_drag(self,event,orig_idx):
        if self._drag_from is None: return
        # convert widget-relative x to TabBar-relative x
        rx = event.widget.winfo_x() + event.x
        target = self._tab_index_at(rx)
        if target != -1 and target != self._drag_from:
            self.on_reorder(self._drag_from, target)
            self._drag_from = target   # update so continuous drag works

    def _on_release(self,event,idx):
        self._drag_from=None

    def _click(self,idx):
        self.set_active(idx); self.on_switch(idx)


# ═══════════════════════════════════════════════════════════════════════════════
#  Single editor panel (editor + gutter + scrollbars)
#  Used for both main and split views
# ═══════════════════════════════════════════════════════════════════════════════
class EditorPanel(tk.Frame):
    def __init__(self,parent,on_key_release,on_key_press,on_cursor,**kw):
        super().__init__(parent,bg=EDITOR_BG,**kw)
        self._on_key_release = on_key_release
        self._on_key_press   = on_key_press
        self._on_cursor      = on_cursor

        self.gutter = tk.Canvas(self, bg=GUTTER_BG, width=44,
                                highlightthickness=0, bd=0)
        self.gutter.pack(side="left", fill="y")
        tk.Frame(self, bg=BORDER, width=1).pack(side="left", fill="y")

        self._esy = ctk.CTkScrollbar(self, orientation="vertical")
        self._esy.pack(side="right", fill="y")
        esx = ctk.CTkScrollbar(self, orientation="horizontal")
        esx.pack(side="bottom", fill="x")

        self.text = tk.Text(self,
            bg=EDITOR_BG, fg=TEXT, insertbackground=ACCENT,
            selectbackground="#264f78", font=("Courier New",13),
            wrap="none", relief="flat", bd=0, undo=True, maxundo=-1,
            yscrollcommand=self._ey_scroll, xscrollcommand=esx.set,
            padx=8, pady=4, spacing1=2, spacing3=2,
            exportselection=True, highlightthickness=0)
        self.text.pack(side="left", fill="both", expand=True)
        self._esy.configure(command=self._ey_view)
        esx.configure(command=self.text.xview)

        self._setup_tags()
        self.text.bind("<KeyRelease>",    self._key_release)
        self.text.bind("<KeyPress>",      self._on_key_press)
        self.text.bind("<ButtonRelease>", self._on_cursor)
        self.text.bind("<Configure>",     self._redraw_gutter)

    def _ey_scroll(self, *a):
        self._esy.set(*a); self._redraw_gutter()
    def _ey_view(self, *a):
        self.text.yview(*a); self._redraw_gutter()

    def _redraw_gutter(self, _=None):
        self.gutter.delete("all")
        i = self.text.index("@0,0")
        while True:
            dl = self.text.dlineinfo(i)
            if dl is None: break
            self.gutter.create_text(38, dl[1]+2, anchor="ne",
                text=i.split(".")[0], fill=MUTED, font=("Courier New",11))
            ni = self.text.index(f"{i}+1line")
            if ni == i: break
            i = ni

    def _setup_tags(self):
        t = self.text
        t.tag_configure("keyword",      foreground=SYN_KW)
        t.tag_configure("type",         foreground=SYN_TYPE)
        t.tag_configure("string",       foreground=SYN_STR)
        t.tag_configure("comment",      foreground=SYN_CMNT,
                         font=("Courier New",13,"italic"))
        t.tag_configure("number",       foreground=SYN_NUM)
        t.tag_configure("operator",     foreground=SYN_KW)
        t.tag_configure("dcolon",       foreground="#e8c46a")   # :: — warm gold
        t.tag_configure("language",     foreground=SYN_LANG)
        t.tag_configure("current_line", background="#1c2128")
        t.tag_configure("search_hit",   background=WARN, foreground=BG)
        # Tag priority: higher raise() = painted on top
        # string and comment must win over operator/keyword/number
        for tag in ("keyword","type","number","operator","dcolon","language"):
            t.tag_lower(tag, "string")
            t.tag_lower(tag, "comment")

    def highlight(self):
        t    = self.text
        text = t.get("1.0","end")

        # Clear all tags first
        for tag in ("keyword","type","string","comment","number",
                    "operator","dcolon","language"):
            t.tag_remove(tag,"1.0","end")

        # ── First pass: strings, comments, :: ──────────────────────────────
        # We track exact character spans so later passes can avoid them.
        # string_spans and comment_spans are lists of (start_char, end_char)
        # in absolute char-offset space.
        string_spans  = []   # ranges that are inside a string literal
        comment_spans = []   # ranges that are comments

        offset = 0
        for line in text.splitlines(True):
            line_len = len(line)
            # --- comment detection (everything after unquoted !) ---
            cp = _strip_comment(line)
            cs = len(cp)
            if cs < line_len:
                # colour from ! to end of line (excluding the \n itself is fine)
                t.tag_add("comment",
                          f"1.0+{offset+cs}c",
                          f"1.0+{offset+line_len}c")
                comment_spans.append((offset+cs, offset+line_len))

            # --- string detection within the code portion ---
            # double-quoted strings extend to end of line if unclosed
            # single-quoted strings also extend to end of line if unclosed
            i = 0
            while i < cs:
                ch = line[i]
                if ch == '"':
                    # find closing " on same line, or colour to EOL
                    j = i + 1
                    while j < cs:
                        if line[j] == '"': break
                        j += 1
                    end = j + 1 if j < cs else cs  # include closing quote if found
                    t.tag_add("string",
                              f"1.0+{offset+i}c",
                              f"1.0+{offset+end}c")
                    string_spans.append((offset+i, offset+end))
                    i = end
                    continue
                if ch == "'":
                    j = i + 1
                    while j < cs:
                        if line[j] == "'": break
                        j += 1
                    end = j + 1 if j < cs else cs
                    t.tag_add("string",
                              f"1.0+{offset+i}c",
                              f"1.0+{offset+end}c")
                    string_spans.append((offset+i, offset+end))
                    i = end
                    continue
                i += 1

            offset += line_len

        # Helper: is absolute char position inside a string or comment?
        def _in_protected(pos, end_pos=None):
            ep = end_pos if end_pos is not None else pos+1
            for s,e in string_spans:
                if pos < e and ep > s: return True
            for s,e in comment_spans:
                if pos < e and ep > s: return True
            return False

        # ── Second pass: :: (dcolon) ────────────────────────────────────────
        for m in re.finditer(r"::", text):
            if not _in_protected(m.start(), m.end()):
                t.tag_add("dcolon", f"1.0+{m.start()}c", f"1.0+{m.end()}c")

        # ── Third pass: numbers ─────────────────────────────────────────────
        for m in re.finditer(r"\b\d+(\.\d+)?\b", text):
            if not _in_protected(m.start(), m.end()):
                t.tag_add("number", f"1.0+{m.start()}c", f"1.0+{m.end()}c")

        # ── Fourth pass: operators (excluding :: which has its own colour) ──
        # Pattern excludes :: by using a negative-lookahead/behind approach
        op_pat = re.compile(r"(?<!:):(?!:)|==|!=|<=|>=|<(?!=)|>(?!=)|\+(?!=)|-(?!=)|\*|/(?!/)|(?<!:=)=(?!=)")
        for m in op_pat.finditer(text):
            if not _in_protected(m.start(), m.end()):
                t.tag_add("operator", f"1.0+{m.start()}c", f"1.0+{m.end()}c")

        # ── Fifth pass: language tokens ─────────────────────────────────────
        lp = r"\b(" + "|".join(re.escape(x) for x in LANG_TOKENS) + r")\b"
        for m in re.finditer(lp, text, re.IGNORECASE):
            if not _in_protected(m.start(), m.end()):
                t.tag_add("language", f"1.0+{m.start()}c", f"1.0+{m.end()}c")

        # ── Sixth pass: types ───────────────────────────────────────────────
        tp = r"\b(" + "|".join(re.escape(x) for x in TYPES) + r")\b"
        for m in re.finditer(tp, text, re.IGNORECASE):
            if not _in_protected(m.start(), m.end()):
                t.tag_add("type", f"1.0+{m.start()}c", f"1.0+{m.end()}c")

        # ── Seventh pass: keywords (longest first) ──────────────────────────
        for kw in sorted(KEYWORDS, key=lambda k: -len(k)):
            for m in re.finditer(rf"\b{re.escape(kw)}\b", text, re.IGNORECASE):
                if not _in_protected(m.start(), m.end()):
                    t.tag_add("keyword", f"1.0+{m.start()}c", f"1.0+{m.end()}c")

        # ── Current line highlight ──────────────────────────────────────────
        t.tag_remove("current_line","1.0","end")
        ln = t.index("insert").split(".")[0]
        t.tag_add("current_line", f"{ln}.0", f"{ln}.0 lineend+1c")

    def _key_release(self, event):
        self.highlight(); self._redraw_gutter(); self._on_key_release(event)

    def load(self, content: str):
        self.text.delete("1.0","end")
        self.text.insert("1.0", content)
        self.text.edit_reset()
        self.highlight(); self._redraw_gutter()

    def get_content(self) -> str:
        return self.text.get("1.0","end-1c")


# ═══════════════════════════════════════════════════════════════════════════════
#  Main IDE
# ═══════════════════════════════════════════════════════════════════════════════
class CLIDE(ctk.CTk):
    MAX_RECENT = 14

    def __init__(self):
        super().__init__()
        self.title(f"CLIDE v{VERSION} — CowLang IDE")
        self.geometry("1280x820"); self.configure(fg_color=BG); self.minsize(800,560)

        self._tabs          = []
        self._active_idx    = -1
        self.autosave       = False
        self._autocomplete  = None
        self._find_visible  = False
        self._cowlang_exe   = _find_exe()
        self._recent        = []
        self._term_height   = 200
        self._split         = False
        self._split_idx     = -1
        self._last_save_dir = str(Path.home())

        self._load_settings()
        self._build_ui()
        self._apply_keybindings()
        self._schedule_autosave()

    @property
    def current_file(self):
        t=self._active_tab; return t.path if t else None
    @property
    def _active_tab(self):
        if 0<=self._active_idx<len(self._tabs): return self._tabs[self._active_idx]
        return None
    @property
    def editor(self):
        """The currently-focused editor panel's Text widget."""
        return self._main_panel.text

    # ── Settings ─────────────────────────────────────────────────────────────
    def _load_settings(self):
        p=_cfg_file()
        if not p.exists(): return
        try:
            d=json.loads(p.read_text())
            self.autosave     =d.get("autosave",False)
            self._cowlang_exe =d.get("cowlang_exe",self._cowlang_exe)
            self._recent      =d.get("recent",[])
            self._term_height =d.get("term_height",200)
            self._last_save_dir=d.get("last_save_dir", str(Path.home()))
            for f in d.get("open_files",[]):
                if Path(f).exists(): self._tabs.append(EditorTab(f))
            if not self._tabs and d.get("lastfile") and Path(d["lastfile"]).exists():
                self._tabs.append(EditorTab(d["lastfile"]))
            if self._tabs:
                self._active_idx=min(d.get("active_tab",0),len(self._tabs)-1)
        except Exception: pass

    def _save_settings(self):
        try:
            _cfg_dir().mkdir(parents=True,exist_ok=True)
            try:
                sash_y=self.vert_pane.sash_coord(0)[1]
                total=self.vert_pane.winfo_height()
                self._term_height=max(60,total-sash_y)
            except Exception: pass
            _cfg_file().write_text(json.dumps({
                "autosave":self.autosave,"cowlang_exe":self._cowlang_exe,
                "recent":self._recent[:self.MAX_RECENT],
                "term_height":self._term_height,
                "last_save_dir":self._last_save_dir,
                "open_files":[t.path for t in self._tabs if t.path],
                "active_tab":self._active_idx,
            },indent=2))
        except Exception: pass

    def _push_recent(self,path):
        if path in self._recent: self._recent.remove(path)
        self._recent.insert(0,path); self._recent=self._recent[:self.MAX_RECENT]

    # ── Build UI ─────────────────────────────────────────────────────────────
    def _build_ui(self):
        # Toolbar
        tb=tk.Frame(self,bg=PANEL,height=46)
        tb.pack(fill="x",side="top"); tb.pack_propagate(False)
        self.toolbar=tb

        def tbtn(text,cmd,bg_col=None):
            b=tk.Label(tb,text=text,bg=bg_col or "#21262d",fg=TEXT if not bg_col else BG,
                       font=("Courier New",11),cursor="hand2",relief="flat",
                       padx=10,pady=6,highlightthickness=1,highlightbackground=BORDER)
            b.bind("<Button-1>",lambda e:cmd())
            hover="#2ea043" if bg_col==ACCENT else "#30363d"
            b.bind("<Enter>",lambda e,l=b,h=hover:l.configure(bg=h))
            b.bind("<Leave>",lambda e,l=b,ob=bg_col or "#21262d":l.configure(bg=ob))
            return b

        tbtn("New",       self.new_file).pack(side="left",padx=(8,2),pady=7)
        tbtn("Open",      self.open_file).pack(side="left",padx=2,pady=7)
        tbtn("Save",      self.save_file).pack(side="left",padx=2,pady=7)
        tbtn("Save As",   self.save_file_as).pack(side="left",padx=2,pady=7)
        tbtn("Recent ▾",  self._open_recent_menu).pack(side="left",padx=2,pady=7)
        tk.Frame(tb,bg=BORDER,width=1).pack(side="left",fill="y",padx=8,pady=8)
        tbtn("Find/Replace",self.toggle_find).pack(side="left",padx=2,pady=7)
        tbtn("⚙ Settings",  self._open_settings).pack(side="left",padx=2,pady=7)
        self._split_btn = tbtn("⊟ Split", self._toggle_split)
        self._split_btn.pack(side="left",padx=2,pady=7)

        self._autosave_var=tk.BooleanVar(value=self.autosave)
        tk.Checkbutton(tb,text="Autosave",variable=self._autosave_var,
                       command=self._on_autosave_toggle,bg=PANEL,fg=TEXT,
                       selectcolor="#21262d",activebackground=PANEL,activeforeground=TEXT,
                       font=("Courier New",11),bd=0,relief="flat",cursor="hand2"
                       ).pack(side="right",padx=10,pady=10)
        run=tk.Label(tb,text="▶  Run (F5)",bg=ACCENT,fg=BG,
                     font=("Courier New",12,"bold"),cursor="hand2",padx=14,pady=6)
        run.bind("<Button-1>",lambda e:self.run_cowlang())
        run.bind("<Enter>",   lambda e:run.configure(bg="#2ea043"))
        run.bind("<Leave>",   lambda e:run.configure(bg=ACCENT))
        run.pack(side="right",padx=(2,10),pady=7)
        tk.Frame(tb,bg=BORDER,width=1).pack(side="right",fill="y",padx=6,pady=8)
        self.title_lbl=tk.Label(tb,text="No file open",bg=PANEL,fg=MUTED,
                                font=("Courier New",11,"italic"))
        self.title_lbl.pack(side="left",padx=8)

        # Find bar
        self.find_bar=tk.Frame(self,bg=PANEL,height=36)
        self.find_var=tk.StringVar(); self.replace_var=tk.StringVar()
        for tv,ph in [(self.find_var,"Find…"),(self.replace_var,"Replace…")]:
            tk.Entry(self.find_bar,textvariable=tv,bg="#21262d",fg=TEXT,
                     insertbackground=ACCENT2,font=("Courier New",11),relief="flat",bd=4,
                     highlightthickness=1,highlightbackground=BORDER,
                     highlightcolor=ACCENT2,width=22).pack(side="left",padx=4,pady=4)
        for lt,cmd in [("Find Next",self._find_next),("Replace",self._replace_one),
                       ("Replace All",self._replace_all)]:
            b=tk.Label(self.find_bar,text=lt,bg="#21262d",fg=TEXT,font=("Courier New",10),
                       cursor="hand2",padx=8,pady=4,relief="flat",
                       highlightthickness=1,highlightbackground=BORDER)
            b.bind("<Button-1>",lambda e,c=cmd:c())
            b.bind("<Enter>",   lambda e,l=b:l.configure(bg="#30363d"))
            b.bind("<Leave>",   lambda e,l=b:l.configure(bg="#21262d"))
            b.pack(side="left",padx=2,pady=4)
        xb=tk.Label(self.find_bar,text="✕",bg=PANEL,fg=MUTED,font=("Courier New",12),
                    cursor="hand2",padx=8)
        xb.bind("<Button-1>",lambda e:self.toggle_find())
        xb.bind("<Enter>",   lambda e:xb.configure(fg=DANGER))
        xb.bind("<Leave>",   lambda e:xb.configure(fg=MUTED))
        xb.pack(side="right",padx=6)

        # Status bar
        status=tk.Frame(self,bg=PANEL,height=22)
        status.pack(fill="x",side="bottom"); status.pack_propagate(False)
        self.status_var=tk.StringVar(value="Ready")
        tk.Label(status,textvariable=self.status_var,bg=PANEL,fg=MUTED,
                 font=("Courier New",10),anchor="w").pack(side="left",padx=8)
        self.cursor_var=tk.StringVar(value="Ln 1, Col 1")
        tk.Label(status,textvariable=self.cursor_var,bg=PANEL,fg=MUTED,
                 font=("Courier New",10)).pack(side="right",padx=8)

        # Main area
        main=tk.Frame(self,bg=BG)
        main.pack(fill="both",expand=True)
        self.sidebar=FileBrowser(main,on_open=self._open_from_browser,width=195)
        self.sidebar.pack(side="left",fill="y")
        tk.Frame(main,bg=BORDER,width=1).pack(side="left",fill="y")

        right=tk.Frame(main,bg=BG)
        right.pack(side="left",fill="both",expand=True)

        self.tab_bar=TabBar(right,on_switch=self._switch_tab,
                            on_close=self._close_tab,on_reorder=self._reorder_tab)
        self.tab_bar.pack(fill="x")
        tk.Frame(right,bg=BORDER,height=1).pack(fill="x")

        # Vertical pane (editors / terminal)
        self.vert_pane=tk.PanedWindow(right,orient="vertical",bg=BORDER,
                                      sashwidth=5,sashrelief="flat",
                                      handlepad=0,handlesize=0,showhandle=False)
        self.vert_pane.pack(fill="both",expand=True)

        # Horizontal pane for split view
        self._horiz_pane=tk.PanedWindow(self.vert_pane,orient="horizontal",bg=BORDER,
                                         sashwidth=5,sashrelief="flat",
                                         handlepad=0,handlesize=0,showhandle=False)
        self.vert_pane.add(self._horiz_pane,stretch="always",minsize=120)

        # Main editor panel
        self._main_panel=EditorPanel(self._horiz_pane,
                                      on_key_release=self._on_key_release,
                                      on_key_press=self._on_key_press,
                                      on_cursor=self._update_cursor)
        self._horiz_pane.add(self._main_panel,stretch="always",minsize=200)
        self._split_panel=None   # created on demand

        # Terminal
        term_outer=tk.Frame(self.vert_pane,bg=TERMINAL_BG)
        self.vert_pane.add(term_outer,stretch="never",minsize=60,height=self._term_height)
        thdr=tk.Frame(term_outer,bg="#0d1117",height=24)
        thdr.pack(fill="x"); thdr.pack_propagate(False)
        tk.Label(thdr,text="OUTPUT / TERMINAL",bg="#0d1117",fg=MUTED,
                 font=("Courier New",9,"bold"),anchor="w",padx=8).pack(side="left")
        tk.Label(thdr,text="⠿ drag to resize",bg="#0d1117",fg="#30363d",
                 font=("Courier New",8)).pack(side="left",padx=4)
        # right-side buttons — Clear and Copy, same style
        cl=tk.Label(thdr,text="Clear",bg="#0d1117",fg=MUTED,font=("Courier New",9),
                    cursor="hand2",padx=8)
        cl.bind("<Button-1>",lambda e:self._clear_terminal())
        cl.bind("<Enter>",   lambda e:cl.configure(fg=TEXT))
        cl.bind("<Leave>",   lambda e:cl.configure(fg=MUTED))
        cl.pack(side="right",padx=4)
        cp=tk.Label(thdr,text="Copy",bg="#0d1117",fg=MUTED,font=("Courier New",9),
                    cursor="hand2",padx=8)
        cp.bind("<Button-1>",lambda e:self._copy_terminal())
        cp.bind("<Enter>",   lambda e:cp.configure(fg=TEXT))
        cp.bind("<Leave>",   lambda e:cp.configure(fg=MUTED))
        cp.pack(side="right",padx=0)
        tscr=ctk.CTkScrollbar(term_outer,orientation="vertical")
        tscr.pack(side="right",fill="y")
        self.terminal=tk.Text(term_outer,bg=TERMINAL_BG,fg=ACCENT,insertbackground=ACCENT,
            font=("Courier New",12),wrap="word",relief="flat",bd=0,state="disabled",
            yscrollcommand=tscr.set,padx=8,pady=4,highlightthickness=0)
        self.terminal.pack(fill="both",expand=True)
        tscr.configure(command=self.terminal.yview)
        self._setup_terminal_tags()
        # Ctrl+C and Ctrl+Shift+C copy terminal content — bound directly on the
        # widget so they fire even when terminal has focus (tk.Text normally
        # intercepts Ctrl+C before it reaches the window-level bind)
        self.terminal.bind("<Control-c>",       lambda e: self._copy_terminal())
        self.terminal.bind("<Control-C>",       lambda e: self._copy_terminal())
        self.terminal.bind("<Control-Shift-c>", lambda e: self._copy_terminal())
        self.terminal.bind("<Control-Shift-C>", lambda e: self._copy_terminal())
        tinput_row=tk.Frame(term_outer,bg=TERMINAL_BG,height=28)
        tinput_row.pack(fill="x"); tinput_row.pack_propagate(False)
        tk.Label(tinput_row,text="$",bg=TERMINAL_BG,fg=ACCENT,
                 font=("Courier New",12,"bold"),width=2).pack(side="left",padx=(8,0))
        self.term_input=tk.Entry(tinput_row,bg=TERMINAL_BG,fg=TEXT,insertbackground=ACCENT,
            font=("Courier New",12),relief="flat",bd=2,highlightthickness=0)
        self.term_input.pack(side="left",fill="x",expand=True,padx=4)
        self.term_input.bind("<Return>",self._run_shell_cmd)

        self.after(120,self._restore_sash)

        if self._tabs:
            for t in self._tabs: self.tab_bar.add_tab(t)
            self.tab_bar.set_active(self._active_idx)
            self._load_tab(self._active_idx)
        else:
            self._new_tab()

        self.protocol("WM_DELETE_WINDOW",self._on_close)

    def _restore_sash(self):
        try:
            total=self.vert_pane.winfo_height()
            self.vert_pane.sash_place(0,0,max(120,total-self._term_height))
        except Exception: pass

    # ── Split view ────────────────────────────────────────────────────────────
    def _toggle_split(self):
        if self._split:
            self._close_split()
        else:
            self._open_split()

    def _open_split(self):
        if self._split: return
        self._split=True
        self._split_btn.configure(text="⊠ Split")
        # Create a second read-only panel showing the same tab (or last tab)
        self._split_panel=EditorPanel(self._horiz_pane,
                                       on_key_release=self._on_split_key_release,
                                       on_key_press=self._on_key_press,
                                       on_cursor=self._update_cursor)
        self._horiz_pane.add(self._split_panel,stretch="always",minsize=200)
        # Default: show same file as main
        self._split_idx=self._active_idx
        content=self._main_panel.get_content()
        self._split_panel.load(content)
        # Equal split
        self.after(50,lambda:self._horiz_pane.sash_place(0,0,
            max(200,self._horiz_pane.winfo_width()//2)))

    def _close_split(self):
        if not self._split: return
        self._split=False
        self._split_btn.configure(text="⊟ Split")
        if self._split_panel:
            self._horiz_pane.forget(self._split_panel)
            self._split_panel.destroy()
            self._split_panel=None
        self._split_idx=-1

    def _on_split_key_release(self, event):
        """When user types in split panel, sync content back to that tab."""
        if not self._split or self._split_panel is None: return
        if 0<=self._split_idx<len(self._tabs):
            t=self._tabs[self._split_idx]
            t.content=self._split_panel.get_content()
            t.modified=True
            self.tab_bar.mark_modified(self._split_idx,True)
        self._update_cursor(event)

    def _sync_split(self):
        """Refresh split panel content from its tab."""
        if self._split and self._split_panel and 0<=self._split_idx<len(self._tabs):
            content=self._tabs[self._split_idx].content
            self._split_panel.load(content)

    # ── Tab reorder ───────────────────────────────────────────────────────────
    def _reorder_tab(self, from_idx, to_idx):
        if from_idx==to_idx: return
        tab=self._tabs.pop(from_idx)
        self._tabs.insert(to_idx,tab)
        # update active index
        if self._active_idx==from_idx:
            self._active_idx=to_idx
        elif from_idx<self._active_idx<=to_idx:
            self._active_idx-=1
        elif to_idx<=self._active_idx<from_idx:
            self._active_idx+=1
        self.tab_bar._tabs=list(self._tabs)
        self.tab_bar.set_active(self._active_idx)

    # ── Tab management ────────────────────────────────────────────────────────
    def _new_tab(self,path=None):
        tab=EditorTab(path)
        idx=self.tab_bar.add_tab(tab)
        self._tabs.append(tab)
        self._active_idx=idx
        self.tab_bar.set_active(idx)
        if path: self._do_load_file(tab,path)
        else:
            self._main_panel.text.delete("1.0","end")
            self._main_panel.text.edit_reset()
            self.title_lbl.configure(text="untitled")
            self.title(f"CLIDE v{VERSION} — untitled")

    def _switch_tab(self,idx):
        if 0<=self._active_idx<len(self._tabs):
            self._tabs[self._active_idx].content=self._main_panel.get_content()
        self._active_idx=idx
        self.tab_bar.set_active(idx)
        self._load_tab(idx)

    def _load_tab(self,idx):
        if not (0<=idx<len(self._tabs)): return
        tab=self._tabs[idx]
        if tab.path and not tab.content:
            try: tab.content=Path(tab.path).read_text(encoding="utf-8",errors="replace")
            except Exception: pass
        self._main_panel.load(tab.content)
        name=Path(tab.path).name if tab.path else "untitled"
        self.title_lbl.configure(text=name)
        self.title(f"CLIDE v{VERSION} — {name}")
        if tab.path: self.sidebar.set_dir(tab.path)

    def _close_tab(self,idx):
        if len(self._tabs)==1:
            t=self._tabs[0]; t.path=None; t.content=""; t.modified=False
            self.tab_bar.mark_modified(0,False); self.tab_bar._redraw()
            self._main_panel.text.delete("1.0","end")
            self.title_lbl.configure(text="untitled")
            self.title(f"CLIDE v{VERSION} — untitled"); return
        tab=self._tabs[idx]
        if tab.modified:
            name=Path(tab.path).name if tab.path else "untitled"
            ans=messagebox.askyesnocancel("Unsaved changes",
                f"'{name}' has unsaved changes. Save before closing?")
            if ans is None: return
            if ans and tab.path: Path(tab.path).write_text(tab.content,encoding="utf-8")
        self._tabs.pop(idx); self.tab_bar.remove_tab(idx)
        new=max(0,idx-1); self._active_idx=new
        self.tab_bar.set_active(new); self._load_tab(new)

    # ── File ops ──────────────────────────────────────────────────────────────
    def new_file(self): self._new_tab()
    def open_file(self):
        initial=str(Path(self.current_file).parent) if self.current_file else str(Path.home())
        path=ask_open_file(self,initial_dir=initial)
        if path: self._open_path(path)
    def _open_from_browser(self,path): self._open_path(path)
    def _open_path(self,path):
        for i,t in enumerate(self._tabs):
            if t.path==path: self._switch_tab(i); return
        tab=self._active_tab
        if tab and not tab.path and not tab.modified:
            tab.path=path; self._do_load_file(tab,path); self.tab_bar._redraw()
        else: self._new_tab(path)
        self._push_recent(path); self._save_settings()

    def _do_load_file(self,tab,path):
        try:
            content=Path(path).read_text(encoding="utf-8",errors="replace")
            tab.content=content; tab.path=path; tab.modified=False
            self._main_panel.load(content)
            name=Path(path).name
            self.title_lbl.configure(text=name)
            self.title(f"CLIDE v{VERSION} — {name}")
            self.sidebar.set_dir(path)
            self.log(f"Opened: {path}")
        except Exception as e: messagebox.showerror("Open Error",str(e))

    def save_file(self):
        tab=self._active_tab
        if not tab: return
        # Never auto-save a file that has never been saved to disk
        if not tab.path: return
        self._write_file(tab)

    def save_file_as(self):
        tab=self._active_tab
        if not tab: return
        # Use last save-as dir, falling back to tab's current dir or home
        if tab.path:
            initial_file=Path(tab.path).name
            initial_dir=str(Path(tab.path).parent)
        else:
            initial_file=""
            initial_dir=getattr(self,"_last_save_dir",str(Path.home()))
        path=ask_save_file(self,initial_dir=initial_dir,default_ext=".cow",initial_file=initial_file)
        if path:
            tab.path=path
            self._last_save_dir=str(Path(path).parent)  # remember for next time
            self._write_file(tab)
            name=Path(path).name
            self.title_lbl.configure(text=name)
            self.title(f"CLIDE v{VERSION} — {name}")
            self.tab_bar._redraw(); self._push_recent(path); self._save_settings()

    def _write_file(self,tab):
        try:
            content=self._main_panel.get_content()
            tab.content=content; tab.modified=False
            Path(tab.path).write_text(content,encoding="utf-8")
            # Immediately redraw so the ● dot disappears at once
            self.tab_bar.mark_modified(self._active_idx,False)
            self.tab_bar._redraw()
            self.log(f"Saved: {tab.path}"); self._save_settings()
        except Exception as e: messagebox.showerror("Save Error",str(e))

    # ── Recent ────────────────────────────────────────────────────────────────
    def _open_recent_menu(self):
        if not self._recent: messagebox.showinfo("CLIDE","No recent files yet 🐄"); return
        menu=tk.Menu(self,tearoff=0,bg="#21262d",fg=TEXT,activebackground=ACCENT2,
                     activeforeground=BG,font=("Courier New",11),bd=0,relief="flat")
        for path in self._recent:
            p=Path(path)
            menu.add_command(label=f"  {p.name}  —  {str(p.parent)}",
                             command=lambda p=path:self._open_path(p))
        menu.add_separator()
        menu.add_command(label="  Clear recent list",command=self._clear_recent)
        try: menu.tk_popup(self.winfo_pointerx(),self.winfo_pointery())
        finally: menu.grab_release()
    def _clear_recent(self): self._recent=[]; self._save_settings()

    # ── Key events ────────────────────────────────────────────────────────────
    def _on_key_release(self,_=None):
        tab=self._active_tab
        if tab:
            tab.content=self._main_panel.get_content(); tab.modified=True
            self.tab_bar.mark_modified(self._active_idx,True)
        if self.autosave and self.current_file: self._autosave_debounce()
        # Sync split if it's showing the same tab
        if self._split and self._split_idx==self._active_idx:
            self._sync_split()

    def _on_key_press(self,event):
        # Figure out which panel sent this
        focused = self.focus_get()
        panel = self._main_panel if (not self._split_panel or focused==self._main_panel.text) \
                else self._split_panel
        txt = panel.text
        if event.keysym=="Return":   return self._handle_return(txt)
        if event.keysym=="Tab":      txt.insert("insert",INDENT); return "break"
        if event.state&0x4 and event.keysym=="space":
            self._show_autocomplete(); return "break"
        return None

    def _handle_return(self,txt):
        lt=txt.get("insert linestart","insert")
        cl=_strip_comment(lt); ws=re.match(r"\s*",lt).group(0)
        mp=re.match(r"^\s*prog\s+(.+?)\s*$",cl,re.IGNORECASE)
        mu=re.match(r"^\s*using\s+([A-Za-z_]\w*)\s+do\s*$",cl,re.IGNORECASE)
        mb=re.match(r"^\s*(if|for|while|dowhile|do|else|elif)\b",cl,re.IGNORECASE)
        if mp:
            inner=ws+INDENT; closing=f"{ws}end prog"
            txt.insert("insert",f"\n{inner}\n{closing}")
            ln=int(txt.index("insert").split(".")[0])
            txt.mark_set("insert",f"{ln-1}.{len(inner)}"); txt.see("insert")
        elif mu:
            block=mu.group(1); inner=ws+INDENT; closing=f"{ws}end {block}"
            txt.insert("insert",f"\n{inner}\n{closing}")
            ln=int(txt.index("insert").split(".")[0])
            txt.mark_set("insert",f"{ln-1}.{len(inner)}"); txt.see("insert")
        elif mb: txt.insert("insert",f"\n{ws}{INDENT}")
        else:    txt.insert("insert",f"\n{ws}")
        return "break"

    def _show_autocomplete(self):
        if self._autocomplete:
            try: self._autocomplete.destroy()
            except Exception: pass
        txt=self.editor
        line=txt.get("insert linestart","insert")
        m=re.search(r"\b(\w+)$",line)
        pre=m.group(1).lower() if m else ""
        words=[k for k in ALL_KEYWORDS if k.startswith(pre)]
        if not words: return
        bb=txt.bbox("insert")
        if not bb: return
        x=txt.winfo_rootx()+bb[0]; y=txt.winfo_rooty()+bb[1]+bb[3]
        self._autocomplete=AutoComplete(self,words,self._insert_completion,x,y)

    def _insert_completion(self,word):
        txt=self.editor
        line=txt.get("insert linestart","insert")
        m=re.search(r"\b(\w+)$",line)
        if m: txt.delete(f"insert-{len(m.group(1))}c","insert")
        txt.insert("insert",word)

    # ── Find & Replace ────────────────────────────────────────────────────────
    def toggle_find(self):
        if self._find_visible:
            self.find_bar.pack_forget(); self._find_visible=False
        else:
            self.find_bar.pack(fill="x",after=self.toolbar); self._find_visible=True

    def _find_next(self):
        q=self.find_var.get()
        if not q: return
        t=self.editor; t.tag_remove("search_hit","1.0","end")
        pos=t.search(q,t.index("insert")+"+1c",stopindex="end",nocase=True)
        if not pos: pos=t.search(q,"1.0",stopindex="end",nocase=True)
        if pos:
            end=f"{pos}+{len(q)}c"; t.tag_add("search_hit",pos,end)
            t.mark_set("insert",end); t.see(pos)

    def _replace_one(self):
        q=self.find_var.get()
        if not q: return
        t=self.editor; t.tag_remove("search_hit","1.0","end")
        pos=t.search(q,"1.0",stopindex="end",nocase=True)
        if pos:
            t.delete(pos,f"{pos}+{len(q)}c"); t.insert(pos,self.replace_var.get())

    def _replace_all(self):
        q=self.find_var.get()
        if not q: return
        t=self.editor; c=t.get("1.0","end-1c")
        t.delete("1.0","end")
        t.insert("1.0",re.sub(re.escape(q),self.replace_var.get(),c,flags=re.IGNORECASE))

    # ── Autosave ──────────────────────────────────────────────────────────────
    # Autosave fires only when the user has made a change AND the file has
    # already been saved to disk at least once (tab.path exists).
    # It uses a debounce: waits 1.5 s of typing inactivity before saving.
    # There is no periodic timer — it only triggers on actual edits.
    def _on_autosave_toggle(self):
        self.autosave=self._autosave_var.get(); self._save_settings()

    def _autosave_debounce(self):
        """Called on every key-release when autosave is on. Restarts the timer."""
        if hasattr(self,"_as_timer"):
            try: self.after_cancel(self._as_timer)
            except Exception: pass
        # 1500 ms of silence → save
        self._as_timer=self.after(1500,self._do_autosave)

    def _do_autosave(self):
        tab=self._active_tab
        # Only autosave if: autosave on, file has a path, and it's actually modified
        if self.autosave and tab and tab.path and tab.modified:
            self._write_file(tab)

    def _schedule_autosave(self):
        # No-op: we no longer use a periodic timer.
        # Kept so old call sites don't crash.
        pass

    # ── Run CowLang ───────────────────────────────────────────────────────────
    def run_cowlang(self):
        if not self.current_file:
            messagebox.showerror("CLIDE","Save the file first, fr 💀"); return
        self.save_file(); self._clear_terminal()
        self.log(f"▶  {Path(self.current_file).name}")
        self.log(f"   Translator: {self._cowlang_exe}\n")
        threading.Thread(target=self._run_thread,daemon=True).start()

    def _run_thread(self):
        try:
            if not Path(self._cowlang_exe).exists():
                self.log(f"✗  Translator not found:\n   {self._cowlang_exe}")
                self.log("   Run the CowLang Installer or set path in Settings."); return
            clt_dir=str(Path(self._cowlang_exe).parent)
            src=Path(self.current_file).read_text(encoding="utf-8")
            start=time.time()

            # Pass source via stdin only — no argv
            proc=subprocess.Popen(
                [self._cowlang_exe],
                cwd=clt_dir,
                stdin=subprocess.PIPE,
                stdout=subprocess.PIPE,
                stderr=subprocess.PIPE,   # separate stderr so we can suppress CLT debug noise
                text=True)
            stdout, stderr = proc.communicate(input=src)
            elapsed=int((time.time()-start)*1000)

            any_output=False
            for line in stdout.splitlines():
                self.log(line)
                any_output=True

            # Show stderr if CLT produced any (compile errors etc.)
            for line in stderr.splitlines():
                if line.strip():
                    self.log_error(f"  {line}")

            # Exit code line — colour it green (0) or red (non-zero)
            code=proc.returncode
            elapsed_s=f"{elapsed}ms"
            if code==0:
                self.log_ok(f"\n✓  done in {elapsed_s} | exit 0")
            else:
                self.log_error(f"\n✗  done in {elapsed_s} | exit {code}")
                if not any_output:
                    self.log_error("   (no output — check translator path in Settings)")

        except Exception as e:
            self.log_error(f"✗  ERROR: {e}")

    # ── Terminal ──────────────────────────────────────────────────────────────
    def log(self,msg):
        def _do():
            self.terminal.configure(state="normal")
            self.terminal.insert("end",msg+"\n")
            self.terminal.see("end")
            self.terminal.configure(state="disabled")
        self.after(0,_do)

    def log_ok(self,msg):
        """Log a success line in green."""
        def _do():
            self.terminal.configure(state="normal")
            self.terminal.insert("end",msg+"\n","_ok")
            self.terminal.see("end")
            self.terminal.configure(state="disabled")
        self.after(0,_do)

    def log_error(self,msg):
        """Log an error line in red."""
        def _do():
            self.terminal.configure(state="normal")
            self.terminal.insert("end",msg+"\n","_err")
            self.terminal.see("end")
            self.terminal.configure(state="disabled")
        self.after(0,_do)

    def _setup_terminal_tags(self):
        self.terminal.tag_configure("_ok",  foreground=ACCENT)   # green
        self.terminal.tag_configure("_err", foreground=DANGER)   # red
    def _clear_terminal(self):
        self.terminal.configure(state="normal")
        self.terminal.delete("1.0","end")
        self.terminal.configure(state="disabled")

    def _copy_terminal(self):
        """Copy all terminal text (or selection if any) to clipboard."""
        try:
            # If there's a selection, copy just that
            sel = self.terminal.get("sel.first","sel.last")
            self.clipboard_clear()
            self.clipboard_append(sel)
        except tk.TclError:
            # No selection — copy everything
            content = self.terminal.get("1.0","end-1c")
            self.clipboard_clear()
            self.clipboard_append(content)
    def _run_shell_cmd(self,_=None):
        cmd=self.term_input.get().strip()
        if not cmd: return
        self.term_input.delete(0,"end"); self.log(f"$ {cmd}")
        cwd=str(Path(self.current_file).parent) if self.current_file else None
        threading.Thread(target=self._shell_thread,args=(cmd,cwd),daemon=True).start()
    def _shell_thread(self,cmd,cwd):
        try:
            proc=subprocess.Popen(cmd,shell=True,cwd=cwd,
                stdout=subprocess.PIPE,stderr=subprocess.STDOUT,text=True)
            for line in proc.stdout: self.log(line.rstrip())
            proc.wait(); self.log(f"(exit {proc.returncode})")
        except Exception as e: self.log(f"Shell error: {e}")

    def _update_cursor(self,_=None):
        try:
            idx=self.editor.index("insert"); ln,col=idx.split(".")
            self.cursor_var.set(f"Ln {ln}, Col {int(col)+1}")
        except Exception: pass

    # ── Fullscreen ────────────────────────────────────────────────────────────
    def _toggle_fullscreen(self):
        self.attributes("-fullscreen",not self.attributes("-fullscreen"))
        self.after(60,self._force_repaint)
    def _on_configure(self,event=None):
        if event and event.widget is not self: return
        self.after(25,self._force_repaint)
    def _force_repaint(self):
        try:
            self.update_idletasks()
            def _walk(w):
                try:
                    if hasattr(w,"_draw"): w._draw(no_color_updates=False)
                except Exception: pass
                for c in w.winfo_children(): _walk(c)
            _walk(self); self.update_idletasks()
        except Exception: pass

    # ── Settings dialog ───────────────────────────────────────────────────────
    def _open_settings(self):
        dlg=ctk.CTkToplevel(self); dlg.title("CLIDE Settings")
        dlg.geometry("520x230"); dlg.configure(fg_color=PANEL); dlg.grab_set()
        ctk.CTkLabel(dlg,text="CowLang Translator Path",
                     font=ctk.CTkFont("Courier New",12,"bold"),
                     text_color=TEXT).pack(anchor="w",padx=16,pady=(16,4))
        exe_var=ctk.StringVar(value=self._cowlang_exe)
        row=ctk.CTkFrame(dlg,fg_color="transparent"); row.pack(fill="x",padx=16)
        ctk.CTkEntry(row,textvariable=exe_var,font=ctk.CTkFont("Courier New",11),
                     fg_color="#21262d",border_color=BORDER,text_color=TEXT,
                     height=32).pack(side="left",fill="x",expand=True,padx=(0,8))
        def _browse():
            p=ask_open_exe(dlg,str(Path(self._cowlang_exe).parent))
            if p: exe_var.set(p)
        ctk.CTkButton(row,text="Browse",width=70,height=32,fg_color="#21262d",
                      hover_color="#30363d",text_color=ACCENT2,border_color=BORDER,
                      border_width=1,font=ctk.CTkFont("Courier New",11),
                      command=_browse).pack(side="left")
        def _save(): self._cowlang_exe=exe_var.get(); self._save_settings(); dlg.destroy()
        ctk.CTkButton(dlg,text="Save",command=_save,height=36,fg_color=ACCENT,
                      hover_color="#2ea043",text_color=BG,
                      font=ctk.CTkFont("Courier New",12,"bold")).pack(padx=16,pady=16,fill="x")

    # ── Keybindings ───────────────────────────────────────────────────────────
    def _apply_keybindings(self):
        self.bind("<Control-s>",     lambda e:self.save_file())
        self.bind("<Control-S>",     lambda e:self.save_file_as())
        self.bind("<Control-o>",     lambda e:self.open_file())
        self.bind("<Control-n>",     lambda e:self.new_file())
        self.bind("<Control-w>",     lambda e:self._close_tab(self._active_idx))
        self.bind("<Control-Tab>",   lambda e:self._switch_tab(
                                         (self._active_idx+1)%max(1,len(self._tabs))))
        self.bind("<Control-f>",     lambda e:self.toggle_find())
        self.bind("<F5>",            lambda e:self.run_cowlang())
        self.bind("<F11>",           lambda e:self._toggle_fullscreen())
        self.bind("<Control-backslash>",lambda e:self._toggle_split())
        self.bind("<Control-z>",     lambda e:self.editor.edit_undo())
        self.bind("<Control-y>",     lambda e:self.editor.edit_redo())
        self.bind("<Control-slash>", self._toggle_comment)
        self.bind("<Configure>",     self._on_configure)

        # ── Extended keyboard support ─────────────────────────────────────────
        # All bindings are wrapped in try/except — if the keysym doesn't exist
        # on the current platform/driver it is silently skipped.
        #
        # ── IBM Model M 122-key (M122 / F122 / 3270 keyboard) ────────────────
        # The TOP double row (F13-F24) is the extended function row.
        # The LEFT COLUMN (labelled F1-F10 physically) sends F13-F22 on most
        # modern USB/PS2 converters running in 3270/5250 mode, OR sends the
        # same F1-F10 as the main keyboard in AT mode (already bound above).
        # Binding F13-F22 here covers the 3270-mode left column:
        #   Left col:  F13=Redo  F14=Undo  F15=Copy  F16=Paste  F17=Cut
        #              F18=SelectAll  F19=NewSave  F20=CloseOpen  F21=Find  F22=Format
        #   Top row:   same F13-F24 but in AT mode some adapters send these instead:
        #   F13=New  F14=Open  F15=Save  F16=SaveAs  F17=Run  F18=Find
        #   F19=CloseTab  F20=Split  F21=Undo  F22=Redo  F23=Settings  F24=Fullscreen
        # We map to the left-column meanings (document/edit actions) since that
        # matches the physical labels and is more useful in an IDE context.
        IBM_BINDINGS = {
            # Left column (3270-mode, F13-F22)
            "<F13>": lambda: self.editor.edit_redo(),          # Redo
            "<F14>": lambda: self.editor.edit_undo(),          # Undo
            "<F15>": lambda: self._clipboard_copy(),           # Copy
            "<F16>": lambda: self._clipboard_paste(),          # Paste
            "<F17>": lambda: self._clipboard_cut(),            # Cut
            "<F18>": lambda: self._select_all(),               # Select All
            "<F19>": self.save_file,                           # New/Save → Save
            "<F20>": lambda: self._close_tab(self._active_idx),# Close/Open → Close tab
            "<F21>": self.toggle_find,                         # Find
            "<F22>": lambda: None,                             # Format (reserved)
            # Top row extras (F23-F24)
            "<F23>": self._open_settings,                      # Settings
            "<F24>": self._toggle_fullscreen,                  # Fullscreen
        }

        # ── Sun Type 6 / Type 7 keyboard extra keys ───────────────────────────
        # These send proper X11 keysyms regardless of driver mode.
        # Physical locations: left of main area and above arrow cluster.
        SUN_BINDINGS = {
            "<Help>":   self.open_file,                        # Help key → Open
            "<Undo>":   lambda: self.editor.edit_undo(),       # dedicated Undo
            "<Again>":  lambda: self.editor.edit_redo(),       # Again = Redo
            "<Find>":   self.toggle_find,                      # Find
            "<Props>":  self._open_settings,                   # Props = Settings
            "<Front>":  self._toggle_split,                    # Front = Split view
            "<Open>":   self.open_file,                        # Open
            "<Cut>":    lambda: self._clipboard_cut(),         # Cut
            "<Copy>":   lambda: self._clipboard_copy(),        # Copy
            "<Paste>":  lambda: self._clipboard_paste(),       # Paste
            "<Stop>":   lambda: None,                          # Stop (reserved)
        }

        # ── Shogun Electronics custom keyboard (E1-E10) ───────────────────────
        # E-keys are the two columns of 5 macro keys on the left side.
        # Layout from the keyboard image (top to bottom, right col then left):
        #   E1 = Redo          E6  = Select All
        #   E2 = Undo          E7  = Format (reserved)
        #   E3 = Copy          E8  = New/Save → New file
        #   E4 = Paste         E9  = Close/Open → Close tab
        #   E5 = Delete/Cut    E10 = Find
        #
        # Recommended xmodmap / QMK firmware keysym assignments:
        #   E1  → XF86Launch1      E6  → XF86Launch6
        #   E2  → XF86Launch2      E7  → XF86Launch7
        #   E3  → XF86Launch3      E8  → XF86Launch8
        #   E4  → XF86Launch4      E9  → XF86Explorer
        #   E5  → XF86Launch5      E10 → XF86Search
        #
        # Add to ~/.xmodmap or your QMK keymap to activate these bindings.
        SHOGUN_BINDINGS = {
            "<XF86Launch1>":  lambda: self.editor.edit_redo(),          # E1 Redo
            "<XF86Launch2>":  lambda: self.editor.edit_undo(),          # E2 Undo
            "<XF86Launch3>":  lambda: self._clipboard_copy(),           # E3 Copy
            "<XF86Launch4>":  lambda: self._clipboard_paste(),          # E4 Paste
            "<XF86Launch5>":  lambda: self._clipboard_cut(),            # E5 Delete/Cut
            "<XF86Launch6>":  lambda: self._select_all(),               # E6 Select All
            "<XF86Launch7>":  lambda: None,                             # E7 Format (reserved)
            "<XF86Launch8>":  self.new_file,                            # E8 New Save → New
            "<XF86Explorer>": lambda: self._close_tab(self._active_idx),# E9 Close/Open
            "<XF86Search>":   self.toggle_find,                         # E10 Find
        }

        for seq, cmd in {**IBM_BINDINGS, **SUN_BINDINGS, **SHOGUN_BINDINGS}.items():
            try:
                self.bind(seq, lambda e, c=cmd: c())
            except Exception:
                pass  # keysym not supported on this platform — skip silently

    # ── Clipboard helpers (used by extended key bindings) ─────────────────────
    def _clipboard_copy(self):
        try: self.editor.event_generate("<<Copy>>")
        except Exception: pass

    def _clipboard_cut(self):
        try: self.editor.event_generate("<<Cut>>")
        except Exception: pass

    def _clipboard_paste(self):
        try: self.editor.event_generate("<<Paste>>")
        except Exception: pass

    def _select_all(self):
        try:
            self.editor.tag_add("sel","1.0","end")
            self.editor.mark_set("insert","end")
        except Exception: pass

    def _toggle_comment(self,_=None):
        t=self.editor
        try:
            s=int(t.index("sel.first").split(".")[0])
            e=int(t.index("sel.last").split(".")[0])
        except tk.TclError:
            s=e=int(t.index("insert").split(".")[0])
        for ln in range(s,e+1):
            line=t.get(f"{ln}.0",f"{ln}.end")
            nl=line.replace("!","",1) if line.lstrip().startswith("!") else "!"+line
            t.delete(f"{ln}.0",f"{ln}.end"); t.insert(f"{ln}.0",nl)

    def _on_close(self):
        unsaved=[t for t in self._tabs if t.modified and t.path]
        if unsaved:
            names=", ".join(Path(t.path).name for t in unsaved)
            ans=messagebox.askyesnocancel("Unsaved changes",
                f"Unsaved changes in: {names}\nSave all before closing?")
            if ans is None: return
            if ans:
                for t in unsaved:
                    try: Path(t.path).write_text(t.content,encoding="utf-8")
                    except Exception: pass
        self._save_settings()
        for attr in ("_as_timer","_as_job"):
            job=getattr(self,attr,None)
            if job:
                try: self.after_cancel(job)
                except Exception: pass
        self.destroy()


if __name__ == "__main__":
    app = CLIDE()
    app.mainloop()
