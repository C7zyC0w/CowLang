# 🐄 CowLang Tools — CLIDE & Installer

## Requirements

```bash
pip install customtkinter requests
```

---

## 1. CowLang Installer (`cowlang_installer.py`)

Downloads CowLang from GitHub and sets it up.

```bash
python cowlang_installer.py
```

**What it does:**
- Downloads the CowLang repo zip from GitHub
- Extracts to `~/CowLang/` (or wherever you choose)
- Makes the translator executable
- Optionally adds CowLang to your PATH
- Optionally installs IDE dependencies (`customtkinter`)
- Writes a `cowlang.json` config (CLIDE reads this automatically)

---

## 2. CLIDE — CowLang IDE (`clide.py`)

Full IDE for CowLang with syntax highlighting, file browser, and terminal.

```bash
python clide.py
```

**Features:**
| Feature | How |
|---|---|
| Run CowLang | F5 or Run button |
| Open/Save | Ctrl+O / Ctrl+S / Ctrl+Shift+S |
| New file | Ctrl+N |
| Find/Replace | Ctrl+F |
| Autocomplete | Ctrl+Space |
| Toggle comment | Ctrl+/ |
| Undo/Redo | Ctrl+Z / Ctrl+Y |
| Shell command | Type in terminal input box |
| Change translator path | ⚙ Settings button |

**Settings saved to:** `~/.config/clide/settings.json`

---

## Translator path

CLIDE auto-detects the translator from:
1. `~/CowLang/cowlang.json` (written by installer)
2. Fallback: `~/CowLang/CowLang-main/CowLangTranslator/CLT_0.3.0`
3. Manual override in ⚙ Settings

---

## Notes

- CowLang files use `.cow` extension
- Comments start with `!` outside of quotes
- Auto-indent triggers on `prog`, `using ... do`, `if`, `for`, `while`
- Block closers (`end prog X`, `end python`) are inserted automatically
