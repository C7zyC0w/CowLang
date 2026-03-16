#!/usr/bin/env python3

import subprocess
import shutil
import sys

def run(cmd, capture=False):
    if capture:
        return subprocess.run(cmd, shell=True,
                              stdout=subprocess.PIPE,
                              stderr=subprocess.PIPE,
                              text=True)
    return subprocess.run(cmd, shell=True)

def check_program(name):
    return shutil.which(name) is not None

def safe_input(prompt, default=None):
    try:
        v = input(prompt).strip()
    except EOFError:
        v = ""
    if v == "" and default is not None:
        return default
    return v

def main():
    if not check_program("git"):
        print("Error: git is not installed or not in PATH.")
        sys.exit(1)

    print("\nGit Sync Helper\n-------------------\n")

    check = run("git rev-parse --is-inside-work-tree", capture=True)
    if check.returncode != 0:
        print("Error: This folder is not a Git repository.")
        return

    branch = run("git rev-parse --abbrev-ref HEAD", capture=True).stdout.strip()
    print(f"Current branch: {branch}\n")

    status = run("git status --porcelain", capture=True).stdout.strip()
    if status:
        print("⚠ You have uncommitted changes.")
        cont = safe_input("Continue anyway and try pulling? [y/N] ", "n").lower()
        if cont not in ("y", "yes"):
            print("Aborted.")
            return

    print("-> Fetching latest changes...")
    run("git fetch")

    print("\nChanges from remote:")
    log = run(f"git log HEAD..origin/{branch} --oneline", capture=True).stdout
    if log.strip():
        print(log)
    else:
        print("Your branch is already up to date.\n")

    mode = safe_input("Pull merge or rebase? [m/r] (default: m) ", "m").lower()
    if mode not in ("m", "r"):
        mode = "m"

    if mode == "m":
        print(f"-> git pull origin {branch}")
        run(f"git pull origin {branch}")
    else:
        print(f"-> git pull --rebase origin {branch}")
        run(f"git pull --rebase origin {branch}")

    print("\nDone! Your local repo is synced.")

if __name__ == "__main__":
    main()
