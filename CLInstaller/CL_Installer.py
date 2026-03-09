"""
CowLang Installer
=================
A CustomTkinter GUI installer that downloads and sets up CowLang
from https://github.com/C7zyC0w/CowLang

Requirements:
    pip install customtkinter requests

Run:
    python cowlang_installer.py
"""

import customtkinter as ctk
import threading
import subprocess
import sys
import os
import shutil
import platform
import urllib.request
import zipfile
import tarfile
import stat
import json
from pathlib import Path

# ─── Appearance ──────────────────────────────────────────────────────────────
ctk.set_appearance_mode("dark")
ctk.set_default_color_theme("blue")

GITHUB_API   = "https://api.github.com/repos/C7zyC0w/CowLang"
GITHUB_ZIP   = "https://github.com/C7zyC0w/CowLang/archive/refs/heads/main.zip"

# Default install dir
DEFAULT_INSTALL = str(Path.home() / "CowLang")

# Detect OS
IS_LINUX   = platform.system() == "Linux"
IS_WINDOWS = platform.system() == "Windows"
IS_MAC     = platform.system() == "Darwin"

# ─── Colours ─────────────────────────────────────────────────────────────────
BG          = "#0d1117"
PANEL       = "#161b22"
ACCENT      = "#39d353"   # cow-green
ACCENT2     = "#58a6ff"   # highlight blue
TEXT        = "#e6edf3"
MUTED       = "#8b949e"
DANGER      = "#f85149"
WARN        = "#e3b341"


class InstallerApp(ctk.CTk):
    def __init__(self):
        super().__init__()

        self.title("CowLang Installer")
        self.geometry("680x640")
        self.minsize(680, 600)
        self.resizable(True, True)
        self.configure(fg_color=BG)

        # State
        self.install_dir = ctk.StringVar(value=DEFAULT_INSTALL)
        self.add_to_path  = ctk.BooleanVar(value=True)
        self.install_ide  = ctk.BooleanVar(value=True)
        self._running     = False

        self._build_ui()

    # ─── UI ──────────────────────────────────────────────────────────────────
    def _build_ui(self):
        # ── IMPORTANT: pack bottom widgets FIRST so they always show ──

        # Status bar (very bottom)
        self.status_var = ctk.StringVar(value="Ready.")
        ctk.CTkLabel(
            self,
            textvariable=self.status_var,
            font=ctk.CTkFont("Courier New", 10),
            text_color=MUTED,
            anchor="w",
            height=22,
        ).pack(fill="x", side="bottom", padx=24, pady=(0, 6))

        # Progress bar
        self.progress = ctk.CTkProgressBar(
            self,
            fg_color=PANEL,
            progress_color=ACCENT,
            height=8,
        )
        self.progress.set(0)
        self.progress.pack(fill="x", side="bottom", padx=24, pady=(0, 4))

        # Install button (above progress)
        self.install_btn = ctk.CTkButton(
            self,
            text="🐄  Install CowLang",
            font=ctk.CTkFont("Courier New", 14, "bold"),
            height=52,
            fg_color=ACCENT,
            hover_color="#2ea043",
            text_color=BG,
            corner_radius=8,
            command=self._start_install,
        )
        self.install_btn.pack(fill="x", side="bottom", padx=24, pady=(0, 8))

        # ── Header (top) ──
        header = ctk.CTkFrame(self, fg_color=PANEL, corner_radius=0, height=90)
        header.pack(fill="x", side="top")
        header.pack_propagate(False)

        ctk.CTkLabel(
            header,
            text="🐄  CowLang",
            font=ctk.CTkFont("Courier New", 32, "bold"),
            text_color=ACCENT,
        ).pack(side="left", padx=24, pady=16)

        ctk.CTkLabel(
            header,
            text="Fortran. Simplified.\nInstaller v1.0",
            font=ctk.CTkFont("Courier New", 11),
            text_color=MUTED,
            justify="left",
        ).pack(side="left", padx=0, pady=16)

        ctk.CTkLabel(
            header,
            text=f"{platform.system()} {platform.machine()}",
            font=ctk.CTkFont("Courier New", 10),
            text_color=MUTED,
        ).pack(side="right", padx=20)

        # ── Body (fills remaining space between header and bottom widgets) ──
        body = ctk.CTkFrame(self, fg_color=BG)
        body.pack(fill="both", expand=True, padx=24, pady=(16, 8))

        # Install path row
        ctk.CTkLabel(
            body, text="Install directory",
            font=ctk.CTkFont("Courier New", 12, "bold"),
            text_color=TEXT, anchor="w",
        ).pack(fill="x", pady=(0, 4))

        path_row = ctk.CTkFrame(body, fg_color="transparent")
        path_row.pack(fill="x", pady=(0, 12))

        ctk.CTkEntry(
            path_row,
            textvariable=self.install_dir,
            font=ctk.CTkFont("Courier New", 12),
            fg_color=PANEL,
            border_color=ACCENT2,
            text_color=TEXT,
            height=36,
        ).pack(side="left", fill="x", expand=True, padx=(0, 8))

        ctk.CTkButton(
            path_row,
            text="Browse",
            width=80, height=36,
            fg_color=PANEL,
            hover_color="#21262d",
            border_color=ACCENT2,
            border_width=1,
            text_color=ACCENT2,
            font=ctk.CTkFont("Courier New", 11),
            command=self._browse,
        ).pack(side="left")

        # Options
        ctk.CTkLabel(
            body, text="Options",
            font=ctk.CTkFont("Courier New", 12, "bold"),
            text_color=TEXT, anchor="w",
        ).pack(fill="x", pady=(4, 6))

        ctk.CTkCheckBox(
            body,
            text="Add CowLang to PATH",
            variable=self.add_to_path,
            font=ctk.CTkFont("Courier New", 12),
            text_color=TEXT,
            fg_color=ACCENT, hover_color="#2ea043",
            checkmark_color=BG, border_color=MUTED,
        ).pack(anchor="w", pady=2)

        ctk.CTkCheckBox(
            body,
            text="Also install CLIDE (CowLang IDE) dependencies",
            variable=self.install_ide,
            font=ctk.CTkFont("Courier New", 12),
            text_color=TEXT,
            fg_color=ACCENT, hover_color="#2ea043",
            checkmark_color=BG, border_color=MUTED,
        ).pack(anchor="w", pady=2)

        # Separator
        ctk.CTkFrame(body, fg_color=PANEL, height=1).pack(fill="x", pady=12)

        # Log label
        ctk.CTkLabel(
            body, text="Installation log",
            font=ctk.CTkFont("Courier New", 12, "bold"),
            text_color=TEXT, anchor="w",
        ).pack(fill="x")

        # Log box — expand=True so it fills whatever space is left
        self.log_box = ctk.CTkTextbox(
            body,
            font=ctk.CTkFont("Courier New", 11),
            fg_color=PANEL,
            text_color=ACCENT,
            border_color="#30363d",
            border_width=1,
            wrap="word",
            state="disabled",
        )
        self.log_box.pack(fill="both", expand=True, pady=(4, 0))

    # ─── Helpers ─────────────────────────────────────────────────────────────
    def _browse(self):
        from tkinter import filedialog
        d = filedialog.askdirectory(title="Select install directory")
        if d:
            self.install_dir.set(d)

    def _log(self, msg, color=None):
        """Thread-safe log append."""
        def _do():
            self.log_box.configure(state="normal")
            # Prepend prefix
            self.log_box.insert("end", f"  {msg}\n")
            self.log_box.see("end")
            self.log_box.configure(state="disabled")
        self.after(0, _do)

    def _set_status(self, msg):
        self.after(0, lambda: self.status_var.set(msg))

    def _set_progress(self, v: float):
        self.after(0, lambda: self.progress.set(v))

    def _finish(self, success: bool):
        def _do():
            self._running = False
            if success:
                self.install_btn.configure(
                    text="✅  Installation Complete!",
                    fg_color="#2ea043",
                    state="disabled",
                )
                self.status_var.set("Done! CowLang is installed.")
            else:
                self.install_btn.configure(
                    text="❌  Installation Failed — Retry",
                    fg_color=DANGER,
                    state="normal",
                )
                self.status_var.set("Installation failed. Check the log above.")
        self.after(0, _do)

    # ─── Install Logic ────────────────────────────────────────────────────────
    def _start_install(self):
        if self._running:
            return
        self._running = True
        self.install_btn.configure(state="disabled", text="Installing…")
        self.log_box.configure(state="normal")
        self.log_box.delete("1.0", "end")
        self.log_box.configure(state="disabled")
        threading.Thread(target=self._install_thread, daemon=True).start()

    def _install_thread(self):
        try:
            install_path = Path(self.install_dir.get())
            self._log(f"Target: {install_path}")
            self._set_status("Creating directories…")
            self._set_progress(0.05)

            install_path.mkdir(parents=True, exist_ok=True)
            self._log("✓ Directory ready")

            # ── 1. Download zip ──
            zip_path = install_path / "cowlang_src.zip"
            self._set_status("Downloading CowLang source…")
            self._log(f"Downloading {GITHUB_ZIP}")
            self._set_progress(0.1)

            def reporthook(count, block_size, total_size):
                if total_size > 0:
                    frac = min(count * block_size / total_size, 1.0)
                    self._set_progress(0.1 + frac * 0.35)

            urllib.request.urlretrieve(GITHUB_ZIP, zip_path, reporthook)
            self._log("✓ Download complete")
            self._set_progress(0.45)

            # ── 2. Extract ──
            self._set_status("Extracting archive…")
            self._log("Extracting…")
            with zipfile.ZipFile(zip_path, "r") as zf:
                zf.extractall(install_path)
            zip_path.unlink()
            self._log("✓ Extracted")
            self._set_progress(0.55)

            # The zip extracts as CowLang-main/
            src_dir = install_path / "CowLang-main"

            # ── 3. Find / build translator ──
            self._set_status("Locating CowLang translator…")
            translator_dir = src_dir / "CowLangTranslator"
            exe_name = "CLT_0.3.0" if IS_LINUX or IS_MAC else "CLT_0.3.0.exe"
            exe_path = translator_dir / exe_name

            if exe_path.exists():
                # Make executable on Unix
                if not IS_WINDOWS:
                    exe_path.chmod(exe_path.stat().st_mode | stat.S_IEXEC | stat.S_IXGRP | stat.S_IXOTH)
                self._log(f"✓ Found translator: {exe_path.name}")
            else:
                # Try to find any CLT* binary
                found = list(translator_dir.glob("CLT*"))
                if found:
                    exe_path = found[0]
                    if not IS_WINDOWS:
                        exe_path.chmod(exe_path.stat().st_mode | stat.S_IEXEC | stat.S_IXGRP | stat.S_IXOTH)
                    self._log(f"✓ Found translator: {exe_path.name}")
                else:
                    self._log("⚠  No pre-built translator found — you may need to build from source.")
                    self._log("   See CowLangTranslator/ for build instructions.")
                    exe_path = None

            self._set_progress(0.65)

            # ── 4. Write config file ──
            config = {
                "install_dir": str(install_path),
                "translator": str(exe_path) if exe_path else "",
                "version": "main",
            }
            config_path = install_path / "cowlang.json"
            config_path.write_text(json.dumps(config, indent=2))
            self._log(f"✓ Config written to {config_path.name}")
            self._set_progress(0.72)

            # ── 5. Install Python IDE deps (optional) ──
            if self.install_ide.get():
                self._set_status("Installing IDE Python dependencies…")
                self._log("Installing customtkinter, requests via pip…")
                result = subprocess.run(
                    [sys.executable, "-m", "pip", "install",
                     "customtkinter", "requests", "--break-system-packages", "-q"],
                    capture_output=True, text=True,
                )
                if result.returncode == 0:
                    self._log("✓ IDE dependencies installed")
                else:
                    self._log(f"⚠  pip warning: {result.stderr.strip()[:120]}")
            self._set_progress(0.82)

            # ── 6. Add to PATH ──
            if self.add_to_path.get() and exe_path:
                self._add_to_path(str(exe_path.parent))
            self._set_progress(0.92)

            # ── 7. Create launcher script ──
            self._write_launcher(install_path, exe_path)
            self._set_progress(1.0)

            self._log("")
            self._log("════════════════════════════════")
            self._log("  🐄  CowLang installed!        ")
            self._log(f"  Location: {install_path}     ")
            if exe_path:
                self._log(f"  Translator: {exe_path.name}  ")
            self._log("  Run 'cowlang' from terminal   ")
            self._log("════════════════════════════════")
            self._set_status("Installation complete!")
            self._finish(True)

        except Exception as e:
            self._log(f"ERROR: {e}")
            self._set_status("Error occurred.")
            self._finish(False)

    def _add_to_path(self, folder: str):
        """Add folder to PATH persistently (best-effort)."""
        self._log(f"Adding {folder} to PATH…")
        if IS_LINUX or IS_MAC:
            profile = Path.home() / (".bashrc" if IS_LINUX else ".zprofile")
            line = f'\nexport PATH="{folder}:$PATH"  # Added by CowLang Installer\n'
            try:
                with open(profile, "a") as f:
                    f.write(line)
                self._log(f"✓ Added to {profile.name} (restart terminal to apply)")
            except Exception as e:
                self._log(f"⚠  Could not write to {profile}: {e}")
        elif IS_WINDOWS:
            try:
                import winreg
                key = winreg.OpenKey(
                    winreg.HKEY_CURRENT_USER,
                    r"Environment",
                    0, winreg.KEY_READ | winreg.KEY_WRITE,
                )
                current, _ = winreg.QueryValueEx(key, "PATH")
                if folder not in current:
                    winreg.SetValueEx(key, "PATH", 0, winreg.REG_EXPAND_SZ, f"{current};{folder}")
                winreg.CloseKey(key)
                self._log("✓ Added to Windows user PATH")
            except Exception as e:
                self._log(f"⚠  Could not update registry PATH: {e}")

    def _write_launcher(self, install_path: Path, exe_path):
        """Write a small shell/bat launcher so user can type 'cowlang'."""
        if IS_LINUX or IS_MAC:
            launcher = install_path / "cowlang"
            if exe_path:
                launcher.write_text(
                    f'#!/bin/sh\nexec "{exe_path}" "$@"\n'
                )
                launcher.chmod(launcher.stat().st_mode | stat.S_IEXEC | stat.S_IXGRP | stat.S_IXOTH)
                self._log(f"✓ Launcher written: {launcher}")
        elif IS_WINDOWS:
            launcher = install_path / "cowlang.bat"
            if exe_path:
                launcher.write_text(
                    f'@echo off\n"{exe_path}" %*\n'
                )
                self._log(f"✓ Launcher written: {launcher}")


# ─── Entry point ─────────────────────────────────────────────────────────────
if __name__ == "__main__":
    app = InstallerApp()
    app.mainloop()