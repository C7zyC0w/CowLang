import customtkinter as ctk
import tkinter as tk
from tkinter import messagebox
from tkinter import PhotoImage
import threading, subprocess, re, time, platform, base64, io, json, os
from pathlib import Path

try:
    from PIL import Image as PILImage, ImageTk
    _PIL = True
except ImportError:
    _PIL = False

ctk.set_appearance_mode("dark")
ctk.set_default_color_theme("blue")
VERSION = "1.4.3"

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
    ("OX Files",              "*.ox"),
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

DEFAULT_EXE = str(
    Path.home() / "CowLang" / "CowLang-main" / "CowLangTranslator" /
    ("CLT_0.4.2.exe" if platform.system() == "Windows" else "CLT_0.4.2.bin")
)

def _default_workspace_dir() -> Path:
    home = Path.home()
    for candidate in (
        home / "Documents" / "CowLangFiles",
        home / "documents" / "CowLangFiles",
        home / "CowLangFiles",
    ):
        try:
            candidate.mkdir(parents=True, exist_ok=True)
            return candidate
        except Exception:
            continue
    return home

def _settings_dir() -> Path:
    base = Path(os.environ.get("XDG_DATA_HOME", Path.home() / ".local" / "share"))
    return base / "CLIDE"

def _settings_path() -> Path:
    return _settings_dir() / "settings.json"

def _themes_path() -> Path:
    return _settings_dir() / "themes.json"

def _strip_comment(line: str) -> str:
    in_s = in_d = esc = False
    for i, ch in enumerate(line):
        if esc:        esc=False; continue
        if ch=="\\":   esc=True;  continue
        if ch=="'" and not in_d: in_s=not in_s; continue
        if ch=='"' and not in_s: in_d=not in_d; continue
        if ch=="!" and not in_s and not in_d: return line[:i]
    return line

# ── Dropdown Menu Management ──────────────────────────────────────────────────
_active_dropdown_menu = None

def _close_dropdown():
    """Close any currently open dropdown menu"""
    global _active_dropdown_menu
    if _active_dropdown_menu:
        try:
            _active_dropdown_menu.unpost()
        except:
            pass
        _active_dropdown_menu = None

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
        self._active_menu = None
        self._icon_cache = {}

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
    """Open OS native file browser window for selecting files"""
    import subprocess
    import os
    initial_dir = initial_dir or str(Path.home())
    
    try:
        if platform.system() == "Darwin":  # macOS — use native file dialog
            result = subprocess.run(
                ["osascript", "-e",
                 'tell application (path to frontmost application as text)\n'
                 '  set chosen_file to choose file\n'
                 '  if chosen_file is not false then\n'
                 '    return POSIX path of chosen_file\n'
                 '  end if\n'
                 'end tell'],
                capture_output=True, text=True, timeout=30
            )
            if result.returncode == 0 and result.stdout.strip():
                path = result.stdout.strip()
                if path and Path(path).exists():
                    return path
        
        elif platform.system() == "Linux":
            # Use zenity (available on most Linux distros with file managers)
            # Suppress X11 notifications by redirecting stderr to devnull
            result = subprocess.run(
                ["zenity", "--file-selection", "--title=Open File",
                 f"--filename={initial_dir}/"],
                capture_output=True, text=True, timeout=30,
                env={**os.environ, "NOTIFY_DISABLE": "1"}
            )
            if result.returncode == 0 and result.stdout.strip():
                return result.stdout.strip()
        
        elif platform.system() == "Windows":
            # Use native Windows file dialog via PowerShell
            result = subprocess.run(
                ["powershell", "-NoProfile", "-Command",
                 '[System.Reflection.Assembly]::LoadWithPartialName("System.windows.forms") | Out-Null;'
                 '$dialog = New-Object System.Windows.Forms.OpenFileDialog;'
                 f'$dialog.InitialDirectory = "{initial_dir}";'
                 '$dialog.Filter = "All files (*.*)|*.*|CowLang (*.cow; *.cl; *.cowp; *.clp; *.ox)|*.cow;*.cl;*.cowp;*.clp;*.ox";'
                 'if ($dialog.ShowDialog() -eq "OK") { Write-Host $dialog.FileName }'],
                capture_output=True, text=True, timeout=30,
                stderr=subprocess.DEVNULL
            )
            if result.returncode == 0 and result.stdout.strip():
                return result.stdout.strip()
    except Exception as e:
        pass
    
    return None

def ask_save_file(parent,initial_dir=None,default_ext=".cow",initial_file=""):
    """Open OS native file browser window for saving files"""
    import subprocess
    import os
    initial_dir = initial_dir or str(Path.home())
    
    try:
        if platform.system() == "Darwin":  # macOS
            result = subprocess.run(
                ["osascript", "-e",
                 'tell application (path to frontmost application as text)\n'
                 f'  set chosen_file to choose file name with prompt "Save file:" default name "{initial_file}"\n'
                 '  if chosen_file is not false then\n'
                 '    return POSIX path of chosen_file\n'
                 '  end if\n'
                 'end tell'],
                capture_output=True, text=True, timeout=30
            )
            if result.returncode == 0 and result.stdout.strip():
                return result.stdout.strip()
        
        elif platform.system() == "Linux":
            result = subprocess.run(
                ["zenity", "--file-selection", "--save", "--title=Save File",
                 f"--filename={initial_dir}/{initial_file}"],
                capture_output=True, text=True, timeout=30,
                env={**os.environ, "NOTIFY_DISABLE": "1"}
            )
            if result.returncode == 0 and result.stdout.strip():
                return result.stdout.strip()
        
        elif platform.system() == "Windows":
            result = subprocess.run(
                ["powershell", "-NoProfile", "-Command",
                 '[System.Reflection.Assembly]::LoadWithPartialName("System.windows.forms") | Out-Null;'
                 '$dialog = New-Object System.Windows.Forms.SaveFileDialog;'
                 f'$dialog.InitialDirectory = "{initial_dir}";'
                 f'$dialog.FileName = "{initial_file}";'
                 '$dialog.Filter = "CowLang (*.cow; *.cl; *.cowp; *.clp; *.ox)|*.cow;*.cl;*.cowp;*.clp;*.ox|All files (*.*)|*.*";'
                 'if ($dialog.ShowDialog() -eq "OK") { Write-Host $dialog.FileName }'],
                capture_output=True, text=True, timeout=30,
                stderr=subprocess.DEVNULL
            )
            if result.returncode == 0 and result.stdout.strip():
                return result.stdout.strip()
    except Exception as e:
        pass
    
    return None

def ask_directory(parent,initial_dir=None):
    """Open OS native folder browser window for selecting directories"""
    import subprocess
    import os
    initial_dir = initial_dir or str(Path.home())
    
    try:
        if platform.system() == "Darwin":  # macOS
            result = subprocess.run(
                ["osascript", "-e",
                 'tell application (path to frontmost application as text)\n'
                 '  set chosen_folder to choose folder\n'
                 '  if chosen_folder is not false then\n'
                 '    return POSIX path of chosen_folder\n'
                 '  end if\n'
                 'end tell'],
                capture_output=True, text=True, timeout=30
            )
            if result.returncode == 0 and result.stdout.strip():
                return result.stdout.strip()
        
        elif platform.system() == "Linux":
            # Try zenity first
            result = subprocess.run(
                ["zenity", "--file-selection", "--directory", "--title=Open Folder",
                 f"--filename={initial_dir}/"],
                capture_output=True, text=True, timeout=30,
                env={**os.environ, "NOTIFY_DISABLE": "1"}
            )
            if result.returncode == 0 and result.stdout.strip():
                return result.stdout.strip()
        
        elif platform.system() == "Windows":
            result = subprocess.run(
                ["powershell", "-NoProfile", "-Command",
                 '[System.Reflection.Assembly]::LoadWithPartialName("System.windows.forms") | Out-Null;'
                 '$dialog = New-Object System.Windows.Forms.FolderBrowserDialog;'
                 f'$dialog.SelectedPath = "{initial_dir}";'
                 'if ($dialog.ShowDialog() -eq "OK") { Write-Host $dialog.SelectedPath }'],
                capture_output=True, text=True, timeout=30,
                stderr=subprocess.DEVNULL
            )
            if result.returncode == 0 and result.stdout.strip():
                return result.stdout.strip()
    except Exception as e:
        pass
    
    return None

def ask_open_exe(parent,initial_dir=None):
    """Open OS native file browser for selecting executables"""
    import subprocess
    import os
    initial_dir = initial_dir or str(Path.home())
    
    try:
        if platform.system() == "Darwin":  # macOS
            result = subprocess.run(
                ["osascript", "-e",
                 'tell application (path to frontmost application as text)\n'
                 '  set chosen_file to choose file with prompt "Select CowLang Translator:"\n'
                 '  if chosen_file is not false then\n'
                 '    return POSIX path of chosen_file\n'
                 '  end if\n'
                 'end tell'],
                capture_output=True, text=True, timeout=30
            )
            if result.returncode == 0 and result.stdout.strip():
                return result.stdout.strip()
        
        elif platform.system() == "Linux":
            result = subprocess.run(
                ["zenity", "--file-selection", "--title=Select Translator",
                 f"--filename={initial_dir}/"],
                capture_output=True, text=True, timeout=30,
                env={**os.environ, "NOTIFY_DISABLE": "1"}
            )
            if result.returncode == 0 and result.stdout.strip():
                return result.stdout.strip()
        
        elif platform.system() == "Windows":
            result = subprocess.run(
                ["powershell", "-NoProfile", "-Command",
                 '[System.Reflection.Assembly]::LoadWithPartialName("System.windows.forms") | Out-Null;'
                 '$dialog = New-Object System.Windows.Forms.OpenFileDialog;'
                 f'$dialog.InitialDirectory = "{initial_dir}";'
                 '$dialog.Filter = "Executables (*.exe; *.bin; *.bat)|*.exe;*.bin;*.bat|All files (*.*)|*.*";'
                 'if ($dialog.ShowDialog() -eq "OK") { Write-Host $dialog.FileName }'],
                capture_output=True, text=True, timeout=30,
                stderr=subprocess.DEVNULL
            )
            if result.returncode == 0 and result.stdout.strip():
                return result.stdout.strip()
    except Exception as e:
        pass
    
    return None


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
    def __init__(self,parent,on_open,root_path=None,**kw):
        super().__init__(parent,fg_color=PANEL,**kw)
        self.on_open=on_open
        self._root_path=Path(root_path) if root_path else _default_workspace_dir()
        try:
            self._root_path.mkdir(parents=True, exist_ok=True)
        except Exception:
            pass
        self._path_var=ctk.StringVar(value=str(self._root_path))
        self._node_map = {}  # Maps tree item IDs to Path objects

        # ── Header with folder name and buttons ──────────────────────────────
        hdr=ctk.CTkFrame(self,fg_color="#21262d",corner_radius=0,height=40)
        hdr.pack(fill="x"); hdr.pack_propagate(False)
        
        hdr_left = ctk.CTkFrame(hdr, fg_color="#21262d")
        hdr_left.pack(side="left", fill="x", expand=True, padx=8, pady=8)
        
        ctk.CTkLabel(hdr_left, text="EXPLORER", 
                     font=ctk.CTkFont("Courier New", 10, "bold"),
                     text_color=MUTED).pack(side="left", anchor="w")
        
        # Current folder path (smaller, below EXPLORER)
        path_lbl = ctk.CTkLabel(hdr_left, textvariable=self._path_var,
                                font=ctk.CTkFont("Courier New", 8),
                                text_color="#8b949e")
        path_lbl.pack(side="left", anchor="w", pady=(2,0))
        
        hdr_right = ctk.CTkFrame(hdr, fg_color="#21262d")
        hdr_right.pack(side="right", padx=4, pady=8)
        
        ctk.CTkButton(hdr_right, text="📁", width=24, height=24, 
                      fg_color="transparent", hover_color="#30363d",
                      font=ctk.CTkFont("Courier New", 12),
                      text_color=MUTED, command=self._browse_dir).pack(side="left", padx=2)
        ctk.CTkButton(hdr_right, text="⟳", width=24, height=24,
                      fg_color="transparent", hover_color="#30363d",
                      font=ctk.CTkFont("Courier New", 12),
                      text_color=MUTED, command=self.refresh).pack(side="left", padx=2)

        # ── Tree view for folder hierarchy (like VS Code) ──────────────────────
        from tkinter import ttk
        
        # Style the treeview to match VS Code
        style = ttk.Style()
        style.theme_use('clam')
        style.configure("Treeview", 
                       background=PANEL, 
                       foreground=TEXT,
                       fieldbackground=PANEL,
                       font=("Courier New", 10),
                       rowheight=20,
                       relief="flat",
                       bd=0)
        style.configure("Treeview.Heading", background="#21262d", foreground=MUTED)
        style.map("Treeview", 
                 background=[('selected', ACCENT2)])
        
        self._tree = ttk.Treeview(self, style="Treeview", height=25)
        self._tree.pack(fill="both", expand=True, padx=0, pady=0)
        
        # Bind events
        self._tree.bind("<Double-1>", self._on_select)
        self._tree.bind("<Return>", self._on_select)
        self._tree.bind("<<TreeviewOpen>>", self._on_expand)
        
        # Hide the # column (row numbers)
        self._tree['show'] = 'tree'
        
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
        if d:
            self.set_dir(d)

    def _get_icon(self, path):
        if path.is_dir():
            return ""  # no emoji, let tree handle folder icon

        ext = path.suffix.lower()

        # 🐄 CowLang files
        if ext in CL_EXTS:
            return "🐄 "
        
        # Common code file types
        elif ext in {'.py', '.js', '.java', '.cpp', '.c', '.h', '.go', '.rs', '.rb', '.php'}:
            return "⚙️  "
        
        # Document/config files
        elif ext in {'.txt', '.md', '.json', '.yaml', '.yml', '.toml', '.xml', '.html', '.css', '.csv'}:
            return "📄 "
        
        # Default: paper with text emoji for unknown types
        else:
            return "📝 "

    def _path_to_id(self, path):
        """Convert Path to a unique ID for tree items"""
        return str(path).replace(str(self._root_path), "root")

    def _id_to_path(self, item_id):
        """Convert tree item ID back to Path"""
        if item_id in self._node_map:
            return self._node_map[item_id]
        return None

    def refresh(self, _=None):
        """Refresh the tree view"""
        self._tree.delete(*self._tree.get_children())
        self._node_map.clear()
        
        # Add root folder
        root_id = self._tree.insert("", "end", "root", 
                                    text=f" {self._root_path.name}",
                                    open=True)
        self._node_map["root"] = self._root_path
        
        # Populate children of root
        self._populate_dir("root", self._root_path)

    def _populate_dir(self, parent_id, dir_path):
        """Recursively populate tree items for a directory"""
        try:
            entries = sorted(dir_path.iterdir(), 
                            key=lambda p: (p.is_file(), p.name.lower()))
            
            for entry in entries:
                if entry.name.startswith("."):
                    continue
                
                item_id = self._path_to_id(entry)
                
                if entry.is_dir():
                    icon = "🗀 "  # folder icon
                    
                    text = f"{icon}{entry.name}"
                    node_id = self._tree.insert(parent_id, "end",
                                                text=text,
                                                open=False,
                                                values=[str(entry)])
                    
                    self._node_map[node_id] = entry

                    # dummy child so arrow shows
                    self._tree.insert(node_id, "end", text="(loading...)")

                else:
                    icon = self._get_icon(entry)
                    color = ACCENT if entry.suffix.lower() in CL_EXTS else TEXT
                    text = f"{icon}{entry.name}"
                    
                    node_id = self._tree.insert(parent_id, "end",
                                                text=text,
                                                values=[str(entry)])
                    
                    self._node_map[node_id] = entry

                    # Color the item
                    self._tree.tag_configure(f"color_{node_id}", foreground=color)
                    self._tree.item(node_id, tags=[f"color_{node_id}"])
        
        except (PermissionError, OSError):
            pass

    def _on_expand(self, event=None):
        """Handle folder expansion (lazy loading)"""
        item = self._tree.focus()  # IMPORTANT: use focused item, not selection

        path = self._node_map.get(item)
        if not path or not path.is_dir():
            return

        children = self._tree.get_children(item)

        # check for dummy
        if children:
            first = children[0]
            if self._tree.item(first, "text").startswith("(loading"):
                # remove dummy
                self._tree.delete(*children)

                # populate real contents
                self._populate_dir(item, path)

    def _on_select(self, _=None):
        """Handle file/folder selection"""
        selection = self._tree.selection()
        if not selection:
            return
        
        item_id = selection[0]
        path = self._id_to_path(item_id)
        
        if not path:
            return
        
        if path.is_dir():
            # Expand/collapse folder
            is_open = self._tree.item(item_id, "open")
            self._tree.item(item_id, open=not is_open)
            self._on_expand()
        else:
            # Open file
            self.on_open(str(path))


# ═══════════════════════════════════════════════════════════════════════════════
#  EditorTab
# ═══════════════════════════════════════════════════════════════════════════════
class EditorTab:
    def __init__(self,path=None):
        self.path=path; self.content=""; self.modified=False
    @property
    def display_name(self):
        base = Path(self.path).name if self.path else "untitled"

        # get icon (fallback if no path)
        if self.path:
            ext = Path(self.path).suffix.lower()
            if ext in [".cow", ".cl", ".cowp", ".clp", ".ox"]:
                icon = "🐄 "
            else:
                icon = "📄 "
        else:
            icon = "📄 "

        name = f"{icon}{base}"

        return ("● " + name) if self.modified else name
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
        self._redraw_pending = False  # Flag to batch redraws

    def add_tab(self,tab):
        idx=len(self._tabs); self._tabs.append(tab); self._schedule_redraw(); return idx
    def remove_tab(self,idx):
        if 0<=idx<len(self._tabs):
            self._tabs.pop(idx)
            if self._active>=len(self._tabs): self._active=max(0,len(self._tabs)-1)
            self._schedule_redraw()
    def set_active(self,idx):
        self._active=idx; self._schedule_redraw()
    def mark_modified(self,idx,v):
        if 0<=idx<len(self._tabs):
            self._tabs[idx].modified=v; self._schedule_redraw()
    
    def _schedule_redraw(self):
        """Schedule a redraw on the next idle cycle to batch updates and prevent flicker"""
        if not self._redraw_pending:
            self._redraw_pending = True
            self.after_idle(self._do_redraw)
    
    def _do_redraw(self):
        """Actual redraw implementation"""
        self._redraw_pending = False
        self._redraw()

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
        
        # Force X11 repaint to prevent rendering glitches
        try:
            self.update_idletasks()
        except:
            pass

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
        """Optimized highlighting: only highlight visible lines + buffer (like VS Code)"""
        t = self.text
        
        # For small files (<50KB), highlight everything
        # For large files, only highlight visible range + buffer
        text = t.get("1.0", "end")
        file_size = len(text)
        
        if file_size > 50000:  # Large file optimization
            # Get visible line range
            try:
                first_visible = t.index("@0,0")
                last_visible = t.index("@0,+10000c")  # ~visible window
                first_line = int(first_visible.split(".")[0])
                last_line = int(last_visible.split(".")[0])
                
                # Add buffer (highlight lines before and after visible)
                buffer_lines = 50
                start_line = max(1, first_line - buffer_lines)
                end_line = min(len(text.splitlines()), last_line + buffer_lines)
                
                # Get just the portion to highlight
                lines = text.splitlines(True)
                highlight_start = start_line - 1
                highlight_end = min(end_line, len(lines))
                
                # Only highlight visible portion
                partial_text = "".join(lines[highlight_start:highlight_end])
                start_offset = len("".join(lines[:highlight_start]))
            except:
                # Fallback to full file if visible range calc fails
                partial_text = text
                start_offset = 0
        else:
            partial_text = text
            start_offset = 0

        # Clear relevant tags in the highlight region
        if file_size > 50000:
            try:
                for tag in ("keyword","type","string","comment","number",
                            "operator","dcolon","language"):
                    start_idx = f"1.0+{start_offset}c"
                    end_idx = f"1.0+{start_offset + len(partial_text)}c"
                    t.tag_remove(tag, start_idx, end_idx)
            except:
                pass
        else:
            for tag in ("keyword","type","string","comment","number",
                        "operator","dcolon","language"):
                t.tag_remove(tag,"1.0","end")

        # ── First pass: strings and comments ────────────────────────────────
        string_spans  = []
        comment_spans = []

        offset = start_offset
        for line in partial_text.splitlines(True):
            line_len = len(line)
            cp = _strip_comment(line)
            cs = len(cp)
            if cs < line_len:
                t.tag_add("comment",
                          f"1.0+{offset+cs}c",
                          f"1.0+{offset+line_len}c")
                comment_spans.append((offset+cs, offset+line_len))

            i = 0
            while i < cs:
                ch = line[i]
                if ch == '"':
                    j = i + 1
                    while j < cs:
                        if line[j] == '"': break
                        j += 1
                    end = j + 1 if j < cs else cs
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

        def _in_protected(pos, end_pos=None):
            ep = end_pos if end_pos is not None else pos+1
            for s,e in string_spans:
                if pos < e and ep > s: return True
            for s,e in comment_spans:
                if pos < e and ep > s: return True
            return False

        # ── Remaining passes (optimized for partial text) ──────────────────────
        # Second pass: ::
        for m in re.finditer(r"::", partial_text):
            if not _in_protected(start_offset + m.start(), start_offset + m.end()):
                t.tag_add("dcolon", f"1.0+{start_offset+m.start()}c", f"1.0+{start_offset+m.end()}c")

        # Third pass: numbers
        for m in re.finditer(r"\b\d+(\.\d+)?\b", partial_text):
            if not _in_protected(start_offset + m.start(), start_offset + m.end()):
                t.tag_add("number", f"1.0+{start_offset+m.start()}c", f"1.0+{start_offset+m.end()}c")

        # Fourth pass: operators
        op_pat = re.compile(r"(?<!:):(?!:)|==|!=|<=|>=|<(?!=)|>(?!=)|\+(?!=)|-(?!=)|\*|/(?!/)|(?<!:=)=(?!=)")
        for m in op_pat.finditer(partial_text):
            if not _in_protected(start_offset + m.start(), start_offset + m.end()):
                t.tag_add("operator", f"1.0+{start_offset+m.start()}c", f"1.0+{start_offset+m.end()}c")

        # Fifth pass: language tokens
        lp = r"\b(" + "|".join(re.escape(x) for x in LANG_TOKENS) + r")\b"
        for m in re.finditer(lp, partial_text, re.IGNORECASE):
            if not _in_protected(start_offset + m.start(), start_offset + m.end()):
                t.tag_add("language", f"1.0+{start_offset+m.start()}c", f"1.0+{start_offset+m.end()}c")

        # Sixth pass: types
        tp = r"\b(" + "|".join(re.escape(x) for x in TYPES) + r")\b"
        for m in re.finditer(tp, partial_text, re.IGNORECASE):
            if not _in_protected(start_offset + m.start(), start_offset + m.end()):
                t.tag_add("type", f"1.0+{start_offset+m.start()}c", f"1.0+{start_offset+m.end()}c")

        # Seventh pass: keywords
        for kw in sorted(KEYWORDS, key=lambda k: -len(k)):
            for m in re.finditer(rf"\b{re.escape(kw)}\b", partial_text, re.IGNORECASE):
                if not _in_protected(start_offset + m.start(), start_offset + m.end()):
                    t.tag_add("keyword", f"1.0+{start_offset+m.start()}c", f"1.0+{start_offset+m.end()}c")

        # Current line highlight
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


# ── Colour key list (used by theme editor) ───────────────────────────────────
COLOUR_KEYS = [
    ("BG",          "Background"),
    ("PANEL",       "Panel / toolbar"),
    ("TERMINAL_BG", "Terminal background"),
    ("TAB_BG",      "Tab bar background"),
    ("TAB_ACTIVE",  "Active tab background"),
    ("TEXT",        "Primary text"),
    ("MUTED",       "Muted / secondary text"),
    ("ACCENT",      "Accent (green / run)"),
    ("ACCENT2",     "Accent 2 (blue / links)"),
    ("WARN",        "Warning colour"),
    ("DANGER",      "Danger / error colour"),
    ("BORDER",      "Border colour"),
    ("SYN_KW",      "Syntax: keywords"),
    ("SYN_TYPE",    "Syntax: types"),
    ("SYN_STR",     "Syntax: strings"),
    ("SYN_CMNT",    "Syntax: comments"),
    ("SYN_NUM",     "Syntax: numbers"),
    ("SYN_LANG",    "Syntax: language tokens"),
]

# Built-in themes
_BUILTIN_THEMES = {
    "CLIDE Dark (default)": {
        "BG":"#0d1117","PANEL":"#161b22","TERMINAL_BG":"#090c10",
        "TAB_BG":"#010409","TAB_ACTIVE":"#0d1117",
        "TEXT":"#e6edf3","MUTED":"#8b949e",
        "ACCENT":"#39d353","ACCENT2":"#58a6ff",
        "WARN":"#e3b341","DANGER":"#f85149","BORDER":"#30363d",
        "SYN_KW":"#ff7b72","SYN_TYPE":"#79c0ff","SYN_STR":"#a5d6ff",
        "SYN_CMNT":"#8b949e","SYN_NUM":"#f2cc60","SYN_LANG":"#d2a8ff",
    },
    "CLIDE Light": {
        "BG":"#ffffff","PANEL":"#f6f8fa","TERMINAL_BG":"#f0f0f0",
        "TAB_BG":"#eaeef2","TAB_ACTIVE":"#ffffff",
        "TEXT":"#24292f","MUTED":"#57606a",
        "ACCENT":"#1a7f37","ACCENT2":"#0969da",
        "WARN":"#9a6700","DANGER":"#cf222e","BORDER":"#d0d7de",
        "SYN_KW":"#cf222e","SYN_TYPE":"#0550ae","SYN_STR":"#0a3069",
        "SYN_CMNT":"#57606a","SYN_NUM":"#953800","SYN_LANG":"#8250df",
    },
    "Monokai": {
        "BG":"#272822","PANEL":"#1e1f1c","TERMINAL_BG":"#1a1b18",
        "TAB_BG":"#1e1f1c","TAB_ACTIVE":"#272822",
        "TEXT":"#f8f8f2","MUTED":"#75715e",
        "ACCENT":"#a6e22e","ACCENT2":"#66d9e8",
        "WARN":"#e6db74","DANGER":"#f92672","BORDER":"#3e3d32",
        "SYN_KW":"#f92672","SYN_TYPE":"#66d9e8","SYN_STR":"#e6db74",
        "SYN_CMNT":"#75715e","SYN_NUM":"#ae81ff","SYN_LANG":"#a6e22e",
    },
    "Solarized Dark": {
        "BG":"#002b36","PANEL":"#073642","TERMINAL_BG":"#001e26",
        "TAB_BG":"#073642","TAB_ACTIVE":"#002b36",
        "TEXT":"#839496","MUTED":"#586e75",
        "ACCENT":"#859900","ACCENT2":"#268bd2",
        "WARN":"#b58900","DANGER":"#dc322f","BORDER":"#073642",
        "SYN_KW":"#859900","SYN_TYPE":"#268bd2","SYN_STR":"#2aa198",
        "SYN_CMNT":"#586e75","SYN_NUM":"#d33682","SYN_LANG":"#6c71c4",
    },
}

def _load_custom_themes() -> dict:
    try:
        p = _themes_path()
        if p.exists():
            return json.loads(p.read_text(encoding="utf-8"))
    except Exception: pass
    return {}

def _save_custom_themes(themes: dict):
    try:
        sd = _settings_dir(); sd.mkdir(parents=True, exist_ok=True)
        _themes_path().write_text(json.dumps(themes, indent=2), encoding="utf-8")
    except Exception: pass

def _all_themes() -> dict:
    """Built-ins first, then custom (custom can override built-in names)."""
    t = dict(_BUILTIN_THEMES)
    t.update(_load_custom_themes())
    return t

def _apply_colour_dict(d: dict):
    """Write a colour dict into the global colour variables."""
    g = globals()
    for k, v in d.items():
        if k in g: g[k] = v

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
        self._cowlang_exe   = DEFAULT_EXE
        self._recent        = []
        self._term_height   = 200
        self._split         = False
        self._split_idx     = -1
        self._workspace_dir = _default_workspace_dir()
        self._last_save_dir = str(self._workspace_dir)
        self._running_proc  = None
        self._settings_win  = None
        self._theme         = "dark"
        self._colour_theme  = "CLIDE Dark (default)"

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
        self._workspace_dir = getattr(self, "_workspace_dir", _default_workspace_dir())
        self._workspace_dir.mkdir(parents=True, exist_ok=True)

        settings = {}
        try:
            sp = _settings_path()
            if sp.exists():
                settings = json.loads(sp.read_text(encoding="utf-8"))
        except Exception:
            settings = {}

        self.autosave = bool(settings.get("autosave", self.autosave))
        #self._cowlang_exe = settings.get("cowlang_exe", self._cowlang_exe)
        self._last_save_dir = settings.get("last_save_dir", self._last_save_dir)
        
        # Don't restore workspace dir on startup — let user pick via file explorer
        # But keep the setting saved for reference if needed
        # ws = settings.get("workspace_dir")
        # if ws:
        #     try:
        #         self._workspace_dir = Path(ws).expanduser()
        #         self._workspace_dir.mkdir(parents=True, exist_ok=True)
        #     except Exception:
        #         pass

        # Theme
        self._theme = settings.get("theme", "dark")
        ctk.set_appearance_mode(self._theme)

        # Active colour theme
        self._colour_theme = settings.get("colour_theme", "CLIDE Dark (default)")
        themes = _all_themes()
        if self._colour_theme in themes:
            _apply_colour_dict(themes[self._colour_theme])

        # Custom file extensions
        saved_exts = settings.get("cl_exts")
        if isinstance(saved_exts, list) and saved_exts:
            CL_EXTS.clear()
            CL_EXTS.update(saved_exts)

        recent = settings.get("recent_files", [])
        if isinstance(recent, list):
            cleaned = []
            for item in recent:
                try:
                    p = Path(item).expanduser()
                    if p.exists() and str(p) not in cleaned:
                        cleaned.append(str(p))
                except Exception:
                    pass
            self._recent = cleaned

    def _save_settings(self):
        try:
            sd = _settings_dir()
            sd.mkdir(parents=True, exist_ok=True)
            
            # Add current workspace folder to recents (so it appears in Recent ▾)
            # This way the last folder is not auto-restored, but is available in recents
            recent_with_folder = self._recent.copy()
            ws_str = str(self._workspace_dir)
            if ws_str in recent_with_folder:
                recent_with_folder.remove(ws_str)
            recent_with_folder.insert(0, ws_str)
            recent_with_folder = recent_with_folder[:self.MAX_RECENT]
            
            data = {
                "autosave": self.autosave,
                "cowlang_exe": self._cowlang_exe,
                "last_save_dir": self._last_save_dir,
                "workspace_dir": str(self._workspace_dir),
                "recent_files": recent_with_folder,  # Include workspace folder in recents
                "theme": self._theme,
                "cl_exts": sorted(CL_EXTS),
                "colour_theme": getattr(self, "_colour_theme", "CLIDE Dark (default)"),
            }
            _settings_path().write_text(json.dumps(data, indent=2), encoding="utf-8")
        except Exception:
            pass

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

        self._new_btn = tbtn("New ▾", self._open_new_menu)
        self._new_btn.pack(side="left",padx=(8,2),pady=7)
        
        self._open_btn = tbtn("Open ▾", self._open_open_menu)
        self._open_btn.pack(side="left",padx=2,pady=7)

        tbtn("Save",      self.save_file).pack(side="left",padx=2,pady=7)
        tbtn("Save As",   self.save_file_as).pack(side="left",padx=2,pady=7)
        self._recent_btn=tbtn("Recent ▾",self._open_recent_menu)
        self._recent_btn.pack(side="left",padx=2,pady=7)
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
        self.sidebar=FileBrowser(main,on_open=self._open_from_browser,root_path=self._workspace_dir,width=195)
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
        self._prompt_lbl=tk.Label(tinput_row,text="$",bg=TERMINAL_BG,fg=ACCENT,
                 font=("Courier New",12,"bold"),width=2)
        self._prompt_lbl.pack(side="left",padx=(8,0))
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
        initial=str(Path(self.current_file).parent) if self.current_file else str(self._workspace_dir)
        path=ask_open_file(self,initial_dir=initial)
        if path: self._open_path(path)
    # ── New / Open Menus ──────────────────────────────────────────────────────
    def _open_new_menu(self):
        global _active_dropdown_menu
        _close_dropdown()  # Close any existing dropdown first
        
        menu = tk.Menu(self, tearoff=0, bg="#21262d", fg=TEXT, activebackground=ACCENT2,
                     activeforeground=BG, font=("Courier New", 11), bd=0, relief="flat")
        menu.add_command(label="  New File", command=self.new_file)
        menu.add_command(label="  New Folder", command=self.new_folder)
        btn = self._new_btn
        
        _active_dropdown_menu = menu
        menu.tk_popup(btn.winfo_rootx(), btn.winfo_rooty() + btn.winfo_height())

    def new_folder(self):
        dialog = ctk.CTkInputDialog(text="Enter folder name:", title="New Folder")
        name = dialog.get_input()
        if name:
            # Create the folder inside the directory currently shown in the sidebar
            target = self.sidebar._root_path / name
            try:
                target.mkdir(parents=True, exist_ok=True)
                self.sidebar.refresh()
                self.log(f"Created folder: {target}")
            except Exception as e:
                messagebox.showerror("Error", f"Could not create folder:\n{e}")

    def _open_open_menu(self):
        global _active_dropdown_menu
        _close_dropdown()  # Close any existing dropdown first
        
        menu = tk.Menu(self, tearoff=0, bg="#21262d", fg=TEXT, activebackground=ACCENT2,
                     activeforeground=BG, font=("Courier New", 11), bd=0, relief="flat")
        menu.add_command(label="  Open File", command=self.open_file)
        menu.add_command(label="  Open Folder", command=self.open_folder)
        btn = self._open_btn
        
        _active_dropdown_menu = menu
        menu.tk_popup(btn.winfo_rootx(), btn.winfo_rooty() + btn.winfo_height())

    def open_folder(self):
        initial = str(self._workspace_dir)
        path = ask_directory(self, initial_dir=initial)
        if path:
            self.sidebar.set_dir(path)
            self._workspace_dir = Path(path)
            self._save_settings()
            self.log(f"Workspace set to: {path}")
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
            initial_dir=getattr(self,"_last_save_dir",str(self._workspace_dir))
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
        global _active_dropdown_menu
        if not self._recent: messagebox.showinfo("CLIDE","No recent files yet 🐄"); return
        _close_dropdown()  # Close any existing dropdown first
        
        menu=tk.Menu(self,tearoff=0,bg="#21262d",fg=TEXT,activebackground=ACCENT2,
                     activeforeground=BG,font=("Courier New",11),bd=0,relief="flat")
        for path in self._recent:
            p=Path(path)
            menu.add_command(label=f"  {p.name}  —  {str(p.parent)}",
                             command=lambda p=path:self._open_path(p))
        menu.add_separator()
        menu.add_command(label="  Clear recent list",command=self._clear_recent)
        
        btn=self._recent_btn
        _active_dropdown_menu = menu
        menu.tk_popup(btn.winfo_rootx(), btn.winfo_rooty()+btn.winfo_height())
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
                self.log("   Run the CowLang Installer or set path in Settings.")
                return

            clt_dir = str(Path(self._cowlang_exe).parent)
            m = re.search(r"CLT[_\-]?(\d+\.\d+[\.\d]*)", Path(self._cowlang_exe).name, re.IGNORECASE)
            clt_ver = m.group(1) if m else "unknown"
            self.log(f"   CLT version: {clt_ver}")

            # 🔒 block non-CowLang files
            ext = Path(self.current_file).suffix.lower()
            if ext not in CL_EXTS:
                self.log_error(f"✗  Cannot run '{Path(self.current_file).name}' — not a CowLang file.")
                return

            src = Path(self.current_file).read_text(encoding="utf-8")
            start = time.time()

            # Launch CLT subprocess
            proc = subprocess.Popen(
                [self._cowlang_exe],
                cwd=clt_dir,
                stdin=subprocess.PIPE,
                stdout=subprocess.PIPE,
                stderr=subprocess.STDOUT,
                text=True,
                bufsize=1
            )
            self._running_proc = proc
            self._program_started = True

            # Send source code once
            src_to_send = src if src.endswith("\n") else src + "\n"
            proc.stdin.write(src_to_send)
            proc.stdin.flush()

            # Set terminal prompt
            self.after(0, lambda: self._prompt_lbl.configure(text=">>>", fg=WARN))
            self.after(0, lambda: self.term_input.focus_set())

            # Stream stdout line by line
            any_output = [False]
            buf = []

            while True:
                ch = proc.stdout.read(1)
                if not ch:
                    if proc.poll() is not None:
                        break
                    continue

                any_output[0] = True
                if ch == "\n":
                    line = "".join(buf).rstrip()
                    buf.clear()

                    # Filter out CLT's own >>> prompt
                    if line.startswith(">>>"):
                        continue

                    self.log(line)

                    # If the program requests input, show prompt manually
                    if line.lower().startswith("read") or line.endswith(":"):
                        self.after(0, lambda: self._prompt_lbl.configure(text=">>>"))

                else:
                    buf.append(ch)

            # Flush any remaining partial line
            if buf:
                line = "".join(buf).rstrip()
                if not line.startswith(">>>"):
                    self.log(line)
                buf.clear()

            proc.wait()
            elapsed = int((time.time() - start) * 1000)
            code = proc.returncode
            if code == 0:
                self.log_ok(f"\n✓  done in {elapsed}ms | exit 0")
            else:
                self.log_error(f"\n✗  done in {elapsed}ms | exit {code}")
                if not any_output[0]:
                    self.log_error("   (no output — check translator path in Settings)")

        except Exception as e:
            self.log_error(f"✗  ERROR: {e}")

        finally:
            self._running_proc = None
            self.after(0, lambda: self._prompt_lbl.configure(text="$", fg=ACCENT))

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

    def _send_running_input(self, text: str):
        if (
            self._running_proc
            and self._running_proc.poll() is None
            and getattr(self, "_program_started", False)
        ):
            try:
                self._running_proc.stdin.write(text + "\n")
                self._running_proc.stdin.flush()
                return True
            except Exception as e:
                self.log_error(f"Input error: {e}")
        return False

    def _run_shell_cmd(self,_=None):
        text=self.term_input.get().strip()
        if not text: return
        self.term_input.delete(0,"end")
        # If a CowLang program is running and waiting for input, pipe to it
        if self._send_running_input(text):
            # don't log >>> manually, prompt label already shows it
            self.log(f">>> {text}")
            self.term_input.bind("<Return>", self._send_input_repl)
            return
        # Otherwise run as a shell command
        self.log(f"$ {text}")
        cwd=str(Path(self.current_file).parent) if self.current_file else None
        threading.Thread(target=self._shell_thread,args=(text,cwd),daemon=True).start()
    def _shell_thread(self, cmd, cwd):
        try:
            proc = subprocess.Popen(
                cmd, shell=True, cwd=cwd,
                stdout=subprocess.PIPE, stderr=subprocess.STDOUT, text=True
            )
            for line in proc.stdout:
                self.log(line.rstrip())  # filter >>> here if needed: if line.startswith(">>>"): continue
            proc.wait()
            self.log(f"(exit {proc.returncode})")
        except Exception as e:
            self.log(f"Shell error: {e}")

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
        # singleton guard
        if self._settings_win is not None:
            try: self._settings_win.focus_force(); return
            except Exception: self._settings_win = None

        dlg = tk.Toplevel(self)
        self._settings_win = dlg

        def _on_close():
            self._settings_win = None
            dlg.destroy()

        dlg.protocol("WM_DELETE_WINDOW", _on_close)
        dlg.title("CLIDE Settings")
        dlg.geometry("640x560")
        dlg.configure(bg=PANEL)
        dlg.resizable(False, False)
        try: dlg.grab_set()
        except Exception: pass

        # ── manual tab bar ────────────────────────────────────────────────────
        TAB_NAMES = ["General", "Appearance", "File Types", "Keybindings"]
        _active_tab = tk.StringVar(value="General")
        _panes = {}

        tab_bar = tk.Frame(dlg, bg=TAB_BG, height=32)
        tab_bar.pack(fill="x"); tab_bar.pack_propagate(False)
        tk.Frame(dlg, bg=BORDER, height=1).pack(fill="x")

        pane_area = tk.Frame(dlg, bg=BG)
        pane_area.pack(fill="both", expand=True)

        _tab_btns = {}

        def _switch(name):
            _active_tab.set(name)
            for n, p in _panes.items():
                p.pack_forget() if n != name else p.pack(fill="both", expand=True)
            for n, b in _tab_btns.items():
                b.configure(bg=BG if n==name else TAB_BG,
                            fg=TEXT if n==name else MUTED,
                            relief="flat")

        for name in TAB_NAMES:
            b = tk.Label(tab_bar, text=f"  {name}  ", bg=TAB_BG, fg=MUTED,
                         font=("Courier New",11), cursor="hand2", pady=6,
                         relief="flat", highlightthickness=0)
            b.bind("<Button-1>", lambda e, n=name: _switch(n))
            b.bind("<Enter>",    lambda e, l=b, n=name: l.configure(
                                     fg=TEXT) if n!=_active_tab.get() else None)
            b.bind("<Leave>",    lambda e, l=b, n=name: l.configure(
                                     fg=MUTED) if n!=_active_tab.get() else None)
            b.pack(side="left")
            _tab_btns[name] = b

        for name in TAB_NAMES:
            f = tk.Frame(pane_area, bg=BG)
            _panes[name] = f

        # ── shared helpers ────────────────────────────────────────────────────
        def section(parent, text):
            tk.Label(parent, text=text, bg=BG, fg=MUTED,
                     font=("Courier New",9,"bold")).pack(anchor="w",padx=12,pady=(14,2))
            tk.Frame(parent, bg=BORDER, height=1).pack(fill="x",padx=12,pady=(0,8))

        def entry_row(parent, label, var, browse_cmd=None):
            tk.Label(parent, text=label, bg=BG, fg=TEXT,
                     font=("Courier New",11)).pack(anchor="w",padx=12,pady=(0,2))
            r = tk.Frame(parent, bg=BG); r.pack(fill="x",padx=12,pady=(0,10))
            tk.Entry(r, textvariable=var, bg="#21262d", fg=TEXT,
                     insertbackground=ACCENT2, font=("Courier New",11),
                     relief="flat", bd=4, highlightthickness=1,
                     highlightbackground=BORDER,
                     highlightcolor=ACCENT2).pack(side="left",fill="x",expand=True)
            if browse_cmd:
                b = tk.Label(r, text="Browse", bg="#21262d", fg=ACCENT2,
                             font=("Courier New",10), cursor="hand2",
                             padx=10, pady=4,
                             highlightthickness=1, highlightbackground=BORDER)
                b.bind("<Button-1>", lambda e: browse_cmd())
                b.bind("<Enter>",    lambda e: b.configure(bg="#30363d"))
                b.bind("<Leave>",    lambda e: b.configure(bg="#21262d"))
                b.pack(side="left", padx=(6,0))

        # ════════════════════════════════════════════════════════════════════
        # TAB: GENERAL
        # ════════════════════════════════════════════════════════════════════
        gen = _panes["General"]

        section(gen, "COWLANG TRANSLATOR")
        exe_var = tk.StringVar(value=self._cowlang_exe)
        def _browse_exe():
            p = ask_open_exe(dlg, str(Path(self._cowlang_exe).parent))
            if p: exe_var.set(p)
        entry_row(gen, "Translator executable path", exe_var, _browse_exe)

        section(gen, "WORKSPACE")
        ws_var = tk.StringVar(value=str(self._workspace_dir))
        def _browse_ws():
            p = ask_directory(dlg, str(self._workspace_dir))
            if p: ws_var.set(p)
        entry_row(gen, "Default workspace directory", ws_var, _browse_ws)

        section(gen, "BEHAVIOUR")
        as_var = tk.BooleanVar(value=self.autosave)
        asf = tk.Frame(gen, bg=BG); asf.pack(anchor="w", padx=12, pady=4)
        tk.Checkbutton(asf, text="Autosave  (saves 1.5 s after you stop typing)",
                       variable=as_var, bg=BG, fg=TEXT, selectcolor="#21262d",
                       activebackground=BG, activeforeground=TEXT,
                       font=("Courier New",11), bd=0, relief="flat",
                       cursor="hand2").pack(side="left")

        # ════════════════════════════════════════════════════════════════════
        # TAB: APPEARANCE
        # ════════════════════════════════════════════════════════════════════
        app_tab = _panes["Appearance"]

        # section(app_tab, "THEME")
        # theme_var = tk.StringVar(value=self._theme)
        # theme_row = tk.Frame(app_tab, bg=BG)
        # theme_row.pack(anchor="w", padx=12, pady=(0,6))
        # for val, label in [("dark","🌙  Dark (default)"),("light","☀️  Light")]:
        #     tk.Radiobutton(theme_row, text=label, variable=theme_var, value=val,
        #                    bg=BG, fg=TEXT, selectcolor="#21262d",
        #                    activebackground=BG, activeforeground=TEXT,
        #                    font=("Courier New",12), bd=0, relief="flat",
        #                    cursor="hand2").pack(side="left", padx=(0,24))
        # tk.Label(app_tab, text="  Note: editor & terminal colours fully apply after restart.",
        #          bg=BG, fg=MUTED, font=("Courier New",9)).pack(anchor="w", padx=12)

        section(app_tab, "FONT SIZE")
        font_size_var = tk.IntVar(value=getattr(self,"_font_size",12))
        fsf = tk.Frame(app_tab, bg=BG); fsf.pack(anchor="w", padx=12, pady=4)
        tk.Label(fsf, text="Editor font size:", bg=BG, fg=TEXT,
                 font=("Courier New",11)).pack(side="left", padx=(0,10))
        for sz in (10,11,12,13,14,16):
            tk.Radiobutton(fsf, text=str(sz), variable=font_size_var, value=sz,
                           bg=BG, fg=TEXT, selectcolor="#21262d",
                           activebackground=BG, activeforeground=TEXT,
                           font=("Courier New",11), bd=0, relief="flat",
                           cursor="hand2").pack(side="left", padx=4)

        section(app_tab, "CUSTOMISE THEME")

        # ── theme selector row ────────────────────────────────────────────
        all_themes = _all_themes()
        theme_names = list(all_themes.keys())
        sel_theme_var = tk.StringVar(value=getattr(self,'_colour_theme','CLIDE Dark (default)'))
        if sel_theme_var.get() not in theme_names:
            sel_theme_var.set(theme_names[0])

        tsel_row = tk.Frame(app_tab, bg=BG); tsel_row.pack(fill='x', padx=12, pady=(0,6))
        tk.Label(tsel_row, text='Active theme:', bg=BG, fg=TEXT,
                 font=('Courier New',11)).pack(side='left', padx=(0,8))

        # scrollable dropdown via OptionMenu
        tdd = tk.OptionMenu(tsel_row, sel_theme_var, *theme_names)
        tdd.configure(bg='#21262d', fg=TEXT, activebackground=BORDER,
                      activeforeground=TEXT, highlightthickness=0,
                      font=('Courier New',11), bd=0, relief='flat', width=28)
        tdd['menu'].configure(bg='#21262d', fg=TEXT, font=('Courier New',11),
                              activebackground=ACCENT2, activeforeground=BG)
        tdd.pack(side='left')

        # colour swatch grid
        swatch_frame = tk.Frame(app_tab, bg=BG)
        swatch_frame.pack(fill='x', padx=12, pady=(4,0))

        # dict of key -> StringVar for the colour editor
        colour_vars = {}
        swatch_labels = {}

        def _build_swatches(d):
            for w in swatch_frame.winfo_children(): w.destroy()
            colour_vars.clear(); swatch_labels.clear()
            cols = 2
            for i, (key, label) in enumerate(COLOUR_KEYS):
                val = d.get(key, '#ffffff')
                r = i // cols; c = i % cols
                cell = tk.Frame(swatch_frame, bg=BG)
                cell.grid(row=r, column=c, sticky='ew', padx=(0,8), pady=2)
                swatch_frame.columnconfigure(c, weight=1)
                sw = tk.Label(cell, bg=val, width=3, relief='flat',
                              highlightthickness=1, highlightbackground=BORDER)
                sw.pack(side='left', padx=(0,4))
                swatch_labels[key] = sw
                var = tk.StringVar(value=val)
                colour_vars[key] = var
                ent = tk.Entry(cell, textvariable=var, width=9,
                               bg='#21262d', fg=TEXT, insertbackground=ACCENT2,
                               font=('Courier New',10), relief='flat', bd=2,
                               highlightthickness=1, highlightbackground=BORDER,
                               highlightcolor=ACCENT2)
                ent.pack(side='left', padx=(0,4))
                def _update_sw(e, k=key, v=var, s=sw):
                    try: s.configure(bg=v.get())
                    except Exception: pass
                ent.bind('<KeyRelease>', _update_sw)
                tk.Label(cell, text=label, bg=BG, fg=MUTED,
                         font=('Courier New',9)).pack(side='left')

        def _on_theme_select(*_):
            name = sel_theme_var.get()
            themes = _all_themes()
            if name in themes:
                _build_swatches(themes[name])

        sel_theme_var.trace_add('write', _on_theme_select)
        _build_swatches(all_themes.get(sel_theme_var.get(),
                                       list(all_themes.values())[0]))

        # save-as custom theme row
        saveas_row = tk.Frame(app_tab, bg=BG)
        saveas_row.pack(fill='x', padx=12, pady=(8,4))
        saveas_var = tk.StringVar(value='')
        saveas_ent = tk.Entry(saveas_row, textvariable=saveas_var, width=22,
                              bg='#21262d', fg=TEXT, insertbackground=ACCENT2,
                              font=('Courier New',11), relief='flat', bd=3,
                              highlightthickness=1, highlightbackground=BORDER,
                              highlightcolor=ACCENT2)
        saveas_ent.insert(0, 'My Theme')
        saveas_ent.pack(side='left', padx=(0,6))

        def _save_custom_theme():
            name = saveas_var.get().strip()
            if not name: return
            d = {k: colour_vars[k].get() for k in colour_vars}
            custom = _load_custom_themes()
            custom[name] = d
            _save_custom_themes(custom)
            # rebuild dropdown
            new_names = list(_all_themes().keys())
            tdd['menu'].delete(0, 'end')
            for n in new_names:
                tdd['menu'].add_command(label=n,
                    command=lambda v=n: sel_theme_var.set(v))
            sel_theme_var.set(name)

        def _delete_custom_theme():
            name = sel_theme_var.get()
            if name in _BUILTIN_THEMES:
                messagebox.showinfo('CLIDE', 'Cannot delete a built-in theme.')
                return
            custom = _load_custom_themes()
            if name in custom:
                del custom[name]
                _save_custom_themes(custom)
            new_names = list(_all_themes().keys())
            tdd['menu'].delete(0, 'end')
            for n in new_names:
                tdd['menu'].add_command(label=n,
                    command=lambda v=n: sel_theme_var.set(v))
            sel_theme_var.set(new_names[0])

        for lbl, cmd in [('Save as new theme', _save_custom_theme),
                          ('Delete', _delete_custom_theme)]:
            b = tk.Label(saveas_row, text=lbl, bg='#21262d', fg=TEXT,
                         font=('Courier New',10), cursor='hand2',
                         padx=8, pady=4,
                         highlightthickness=1, highlightbackground=BORDER)
            b.bind('<Button-1>', lambda e, c=cmd: c())
            b.bind('<Enter>',    lambda e, l=b: l.configure(bg='#30363d'))
            b.bind('<Leave>',    lambda e, l=b: l.configure(bg='#21262d'))
            b.pack(side='left', padx=(0,4))

        tk.Label(app_tab,
                 text='  Colours apply after Save + restart. Save as new theme to keep edits.',
                 bg=BG, fg=MUTED, font=('Courier New',9)).pack(anchor='w', padx=12, pady=(2,8))

        # ════════════════════════════════════════════════════════════════════
        # TAB: FILE TYPES
        # ════════════════════════════════════════════════════════════════════
        ft_tab = _panes["File Types"]

        section(ft_tab, "COWLANG EXTENSIONS")
        tk.Label(ft_tab,
                 text="  Built-in extensions (read-only). Cannot be modified.",
                 bg=BG, fg=MUTED, font=("Courier New",9)).pack(anchor="w",padx=12,pady=(0,8))

        ft_frame = tk.Frame(ft_tab, bg=BG)
        ft_frame.pack(fill="both", expand=True, padx=12, pady=(0,8))

        ft_lb = tk.Listbox(ft_frame, bg="#21262d", fg=ACCENT,
                            selectbackground=ACCENT2, selectforeground=BG,
                            font=("Courier New",12), relief="flat", bd=0,
                            activestyle="none", exportselection=False,
                            highlightthickness=0, height=7, state="disabled")
        ft_lb.pack(side="left", fill="both", expand=True)
        ft_lb.configure(state="normal")
        for ext in sorted(CL_EXTS): ft_lb.insert("end", ext)
        ft_lb.configure(state="disabled")

        ft_right = tk.Frame(ft_frame, bg=BG)
        ft_right.pack(side="left", padx=(10,0), anchor="n")
        
        # Disabled Add/Remove buttons (greyed out)
        tk.Label(ft_right, text="Add", bg="#21262d", fg="#5a6370",
                 font=("Courier New",10), padx=8, pady=4,
                 highlightthickness=1, highlightbackground=BORDER).pack(fill="x", pady=2)
        tk.Label(ft_right, text="Remove", bg="#21262d", fg="#5a6370",
                 font=("Courier New",10), padx=8, pady=4,
                 highlightthickness=1, highlightbackground=BORDER).pack(fill="x", pady=2)

        # ════════════════════════════════════════════════════════════════════
        # TAB: KEYBINDINGS
        # ════════════════════════════════════════════════════════════════════
        kb_tab = _panes["Keybindings"]

        section(kb_tab, "KEYBOARD SHORTCUTS")

        DEFAULT_KEYBINDS = [
            ("Ctrl+S",                "Save file"),
            ("Ctrl+Shift+S",          "Save As"),
            ("Ctrl+O",                "Open file"),
            ("Ctrl+N",                "New file"),
            ("Ctrl+W",                "Close tab"),
            ("Ctrl+Tab",              "Next tab"),
            ("Ctrl+F",                "Find / Replace"),
            ("Ctrl+/",                "Toggle line comment"),
            ("Ctrl+Z",                "Undo"),
            ("Ctrl+Y",                "Redo"),
            ("Ctrl+\\",             "Toggle split view"),
            ("Ctrl+Space",            "Autocomplete"),
            ("F5",                    "Run CowLang program"),
            ("F11",                   "Toggle fullscreen"),
            ("Tab",                   "Indent (2 spaces)"),
            ("Enter (in block)",      "Auto-indent to block depth"),
            ("Ctrl+C / Ctrl+Shift+C", "Copy terminal output"),
        ]

        kb_frame = tk.Frame(kb_tab, bg=BG)
        kb_frame.pack(fill="both", expand=True, padx=12, pady=(0,8))

        # Keybindings list with edit fields
        kb_vars = {}
        kb_scroll = tk.Canvas(kb_frame, bg=BG, highlightthickness=0)
        kb_scroll.pack(side="left", fill="both", expand=True)
        
        kb_inner = tk.Frame(kb_scroll, bg=BG)
        kb_scroll_window = kb_scroll.create_window((0, 0), window=kb_inner, anchor="nw")
        
        def _on_kb_configure(event):
            kb_scroll.configure(scrollregion=kb_scroll.bbox("all"))
            kb_scroll.itemconfig(kb_scroll_window, width=event.width)
        
        kb_inner.bind("<Configure>", _on_kb_configure)
        kb_scroll.bind("<MouseWheel>", lambda e: kb_scroll.yview_scroll(-1 if e.delta > 0 else 1, "units"))
        kb_scroll.bind("<Button-4>", lambda e: kb_scroll.yview_scroll(-1, "units"))
        kb_scroll.bind("<Button-5>", lambda e: kb_scroll.yview_scroll(1, "units"))

        for key, desc in DEFAULT_KEYBINDS:
            row = tk.Frame(kb_inner, bg=BG)
            row.pack(fill="x", pady=2)
            
            tk.Label(row, text=desc, bg=BG, fg=TEXT, font=("Courier New",10), width=30, anchor="w").pack(side="left", padx=(0,8))
            
            var = tk.StringVar(value=key)
            kb_vars[desc] = var
            
            entry = tk.Entry(row, textvariable=var, bg="#21262d", fg=ACCENT2,
                            insertbackground=ACCENT2, font=("Courier New",10),
                            relief="flat", bd=2, highlightthickness=1,
                            highlightbackground=BORDER, highlightcolor=ACCENT2, width=18)
            entry.pack(side="left")

        kb_right = tk.Frame(kb_tab, bg=BG)
        kb_right.pack(side="right", anchor="n", padx=(0,12), pady=(0,8))
        
        def _reset_keybinds():
            if messagebox.askyesno("Reset Keybindings", "Reset all keybindings to defaults?"):
                for key, desc in DEFAULT_KEYBINDS:
                    kb_vars[desc].set(key)

        kb_reset_lbl = tk.Label(kb_right, text="Reset to Default", bg="#21262d", fg=TEXT,
                               font=("Courier New",10), cursor="hand2",
                               padx=8, pady=4,
                               highlightthickness=1, highlightbackground=BORDER)
        kb_reset_lbl.bind("<Button-1>", lambda e: _reset_keybinds())
        kb_reset_lbl.bind("<Enter>",    lambda e: kb_reset_lbl.configure(bg="#30363d"))
        kb_reset_lbl.bind("<Leave>",    lambda e: kb_reset_lbl.configure(bg="#21262d"))
        kb_reset_lbl.pack(fill="x", pady=2)

        # ════════════════════════════════════════════════════════════════════
        # BOTTOM BAR — Apply / Undo / Reset / Cancel
        # ════════════════════════════════════════════════════════════════════
        tk.Frame(dlg, bg=BORDER, height=1).pack(fill="x")
        bot = tk.Frame(dlg, bg=PANEL); bot.pack(fill="x", padx=12, pady=8)

        # Store original values for undo
        original_exe = exe_var.get()
        original_ws = ws_var.get()
        original_autosave = as_var.get()
        original_theme = getattr(self, "_theme", "dark")
        original_colour = getattr(self, "_colour_theme", "CLIDE Dark (default)")
        original_kb = {desc: var.get() for desc, var in kb_vars.items()}

        def _apply():
            self._cowlang_exe = exe_var.get()
            try:
                self._workspace_dir = Path(ws_var.get()).expanduser()
                self._workspace_dir.mkdir(parents=True, exist_ok=True)
                self.sidebar.set_dir(str(self._workspace_dir))
                self._last_save_dir = str(self._workspace_dir)
            except Exception: pass
            self.autosave = as_var.get()
            self._autosave_var.set(self.autosave)
            new_theme = getattr(self, "_theme", "dark")  # Not using theme toggle in this version
            chosen = sel_theme_var.get()
            self._colour_theme = chosen
            themes = _all_themes()
            if chosen in themes:
                _apply_colour_dict(themes[chosen])
            # Note: CL_EXTS not modified (read-only)
            # Note: Keybindings saved but not reloaded (would need restart)
            self._save_settings()
            messagebox.showinfo("Settings", "Settings applied successfully!")

        def _undo():
            if messagebox.askyesno("Undo Changes", "Discard all unsaved changes?"):
                exe_var.set(original_exe)
                ws_var.set(original_ws)
                as_var.set(original_autosave)
                sel_theme_var.set(original_colour)
                for desc, var in kb_vars.items():
                    var.set(original_kb.get(desc, ""))

        def _reset_all():
            if messagebox.askyesno("Reset Settings", "Reset all settings to defaults?"):
                exe_var.set(DEFAULT_EXE)
                ws_var.set(str(_default_workspace_dir()))
                as_var.set(True)
                sel_theme_var.set("CLIDE Dark (default)")
                for key, desc in DEFAULT_KEYBINDS:
                    kb_vars[desc].set(key)

        # Button styles
        button_style = {
            "font": ("Courier New", 11),
            "cursor": "hand2",
            "padx": 12,
            "pady": 6,
            "relief": "flat",
            "bd": 0,
            "highlightthickness": 1,
            "highlightbackground": BORDER
        }

        cancel_lbl = tk.Label(bot, text="Cancel", bg=PANEL, fg=MUTED, **button_style)
        cancel_lbl.bind("<Button-1>", lambda e: _on_close())
        cancel_lbl.bind("<Enter>",    lambda e: cancel_lbl.configure(fg=TEXT))
        cancel_lbl.bind("<Leave>",    lambda e: cancel_lbl.configure(fg=MUTED))
        cancel_lbl.pack(side="right", padx=(4,0))

        reset_lbl = tk.Label(bot, text="Reset", bg="#21262d", fg=TEXT, **button_style)
        reset_lbl.bind("<Button-1>", lambda e: _reset_all())
        reset_lbl.bind("<Enter>",    lambda e: reset_lbl.configure(bg="#30363d"))
        reset_lbl.bind("<Leave>",    lambda e: reset_lbl.configure(bg="#21262d"))
        reset_lbl.pack(side="right", padx=4)

        undo_lbl = tk.Label(bot, text="Undo", bg="#21262d", fg=TEXT, **button_style)
        undo_lbl.bind("<Button-1>", lambda e: _undo())
        undo_lbl.bind("<Enter>",    lambda e: undo_lbl.configure(bg="#30363d"))
        undo_lbl.bind("<Leave>",    lambda e: undo_lbl.configure(bg="#21262d"))
        undo_lbl.pack(side="right", padx=4)

        apply_lbl = tk.Label(bot, text="  Apply  ", bg=ACCENT, fg=BG,
                            font=("Courier New", 12, "bold"), cursor="hand2",
                            padx=8, pady=6)
        apply_lbl.bind("<Button-1>", lambda e: _apply())
        apply_lbl.bind("<Enter>",    lambda e: apply_lbl.configure(bg="#2ea043"))
        apply_lbl.bind("<Leave>",    lambda e: apply_lbl.configure(bg=ACCENT))
        apply_lbl.pack(side="right")
        save_lbl.bind("<Leave>",    lambda e: save_lbl.configure(bg=ACCENT))
        save_lbl.pack(side="right", padx=(4,0))

        # activate first tab
        _switch("General")
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