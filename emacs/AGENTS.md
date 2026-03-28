# Emacs Configuration - Agent Guide

This document provides essential information for AI agents working in this Emacs configuration repository.

## Project Overview

This is a personal Emacs configuration repository containing:
- Main configuration file: `.emacs` (Elisp)
- Custom plugins: `plugins/` directory
- Code snippets: `snippets/` directory (YASnippet format)
- Setup scripts: Shell scripts for installation and package management
- Documentation: `README.md`, `usage.md`

## Directory Structure

```
emacs/
├── .emacs                    # Main Emacs configuration file (Elisp)
├── .luarc.json               # Lua language server configuration
├── README.md                 # Setup instructions (Chinese)
├── usage.md                  # Key bindings and usage guide
├── install_lua_lsp.sh        # Script to install Lua language server
├── manager_git_package.sh    # Git package management script
├── plugins/                  # Custom/local Elisp plugins
│   ├── column-marker.el
│   ├── markdown-mode.el
│   ├── paredit.el           # Structural editing for Lisp (v24 beta)
│   └── parenface.el
└── snippets/                 # YASnippet code snippets
    └── latex-mode/
        ├── enumerate.yas
        ├── itemize.yas
        ├── lstinline.yas
        ├── lstlisting.yas
        ├── table.yas
        └── todo.yas
```

## Key Configuration Files

### `.emacs` (Main Config)

The main configuration file is organized into sections:

1. **Startup Optimizations** (lines 87-120)
   - GC tuning for performance
   - Bidirectional text handling
   - Scrolling optimizations

2. **Package Management** (lines 122-147)
   - Uses USTC mirrors for Chinese users
   - `use-package` for declarative package management
   - Auto-installs missing packages

3. **Global Settings** (lines 150-232)
   - Font/curser settings
   - Tab width: 4 spaces
   - Indentation: spaces (not tabs)
   - Line numbers enabled for many modes
   - UTF-8 encoding throughout

4. **Performance Plugins** (lines 265-309)
   - `benchmark-init`: Startup time profiling
   - `so-long`: Handle long lines
   - `ws-butler`: Auto-trim trailing whitespace

5. **Language Support** (lines 311-366)
   - C/C++ (with clangd)
   - Go, Rust, Lua, Python
   - OCaml (tuareg + merlin)
   - Racket, Haskell
   - Markdown

6. **LSP Configuration** (lines 674-719)
   - `eglot` for LSP support
   - Servers: clangd, pyright/ty, lua-language-server, gopls
   - `company` for completion

7. **Search/Navigation** (lines 722-834)
   - `deadgrep`: Ripgrep-based search
   - `consult`: Fast navigation with vertico
   - `better-jumper`: Enhanced mark ring

8. **UI/Theme** (lines 837-892)
   - Theme: doom-nova
   - `doom-modeline`: Modern mode line
   - `treemacs`/`neotree`: File tree views
   - `centaur-tabs`: Tab bar

9. **Git Integration** (lines 869-886)
   - `magit`: Git porcelain
   - `git-gutter`: Inline diff display

### `.luarc.json` (Lua LSP Config)

```json
{
    "hint.enable": true,
    "hint.paramType": true,
    "hint.setType": true,
    "type.inferParamType": true
}
```

## Essential Commands

### Setup Scripts

**Install Lua Language Server:**
```bash
./install_lua_lsp.sh
```
- Downloads lua-language-server 3.14.0 to `~/.emacs.d/plugins/lua-lsp`

**Manage Git Packages:**
```bash
./manager_git_package.sh <command>
```
Commands:
- `ls` - List repo directory info
- `backup` - Create timestamped backup tarball
- `clear` - Remove backup files
- `clone` - Clone all configured repos
- `pull` - Update all repos (git pull)
- `list` - List all repo remote URLs

Managed repos include: hl-todo, parenface, find-file-in-project, dimmer.el, goto-line-preview, breadcrumb, indent-bars, auto-save, llvm-mode, super-hint.el, disaster

### Emacs Commands

**Session Management:**
- `M-x save-buffers-kill-emacs` - Save and exit
- `C-x C-b` - Buffer list
- `C-x k` - Kill buffer (no confirm)

**Navigation:**
- `C-M-f` / `C-M-b` - Forward/backward S-expression
- `M-s <direction>` - Move to other window
- `M-g g` - Go to line
- `C-c l` - Consult goto line
- `C-c SPC .` - Consult ripgrep (project search)
- `C-c SPC b` - Consult buffer
- `C-c SPC r` - Recent files
- `C-c SPC f` - Find file in project

**Editing:**
- `M-o` - Expand region (expand-region)
- `M-c` - Avy jump to word
- `C-c /` - Comment/uncomment
- `C-c <up/down>` - Move lines up/down
- `C-c e l` - Multiple cursors (edit lines)
- `C-c ;` - Iedit mode (edit all occurrences)

**LSP/Completion:**
- `C-c h` - Eldoc (show documentation)
- `C-c TAB` - Company complete common
- `M-s i` - Symbol overlay put
- `M-s k` / `M-s j` - Navigate symbol overlays

**Window/Tab Management:**
- `C-c v n` - New tab
- `C-c v c` - Close tab
- `C-c v j` / `C-c v k` - Previous/next tab
- `C-c d` - Toggle treemacs
- `C-c =` - Toggle neotree
- `C-c i` - Toggle imenu list

**Git:**
- `M-x magit` - Open magit status
- `M-s M-up/down` - Previous/next git hunk
- `M-s M-left` - Popup git hunk
- `M-s M-right` - Mark git hunk
- `M-s g <up/down>` - Stage/revert hunk

**Custom Functions:**
- `C-c m p` - Show file path (also copies to clipboard)
- `C-c m c` - Open emacs config
- `C-c m n` - Open fast-note.md
- `C-c m t` - Save region to temp file
- `C-c m w` - Copy word at point
- `C-c m k` - Kill all file buffers
- `C-c m f` - Query replace in project
- `C-c m a` - Align regexp
- `C-c z` - Convert Chinese punctuation to ASCII

**Search/Replace:**
- `C-c SPC s` - Deadgrep (project search)
- `C-c SPC G` - Consult grep
- `C-c SPC l` - Consult line (current buffer)
- In deadgrep buffer: `v` - View in other window

**Comment Commands (from usage.md):**
- `C-c ! n` - Next error
- `C-c ! p` - Previous error
- `C-c ! l` - List all errors
- `C-c ! c` - Start syntax check

**Special Modes:**
- `M-x deadgrep-edit-mode` - Make deadgrep buffer editable for replacements
- `M-x query-replace` - Interactive replace in deadgrep

## Code Patterns & Conventions

### Elisp Style

1. **Use-Package Pattern:**
```elisp
(use-package package-name
  :ensure t          ; Auto-install
  :defer t           ; Lazy load
  :demand t          ; Load immediately
  :bind ("C-c x" . command)  ; Key bindings
  :hook (mode-hook . command) ; Mode hooks
  :config            ; Configuration code
    (setq variable value))
```

2. **Hook Pattern:**
```elisp
(add-hook 'mode-hook
          (lambda ()
            (local-configuration)))
```

3. **Conditional Configuration:**
```elisp
(if (feature-check)
    (enable-feature)
  (disable-feature))
```

4. **Advice Pattern:**
```elisp
(defadvice function-name (around/specific/after name activate)
  "Documentation"
  ;; advice body
  ad-do-it)
```

### Key Binding Conventions

- `C-c m <key>` - Custom personal functions
- `C-c SPC <key>` - Search-related commands
- `C-c v <key>` - View/tab management
- `C-c d` - File tree (treemacs)
- `C-c =` - Toggle neotree
- `C-c i` - Imenu
- `C-c g` - Goto line preview
- `C-c b <key>` - Bookmark commands
- `M-s <key>` - Symbol/navigation commands

### Indentation

- **Tab width:** 4 spaces
- **Indent style:** Spaces (not tabs)
- **C/C++ offset:** 4 spaces
- **Comment style:** `//` for C-like languages

## External Dependencies

### System Packages

```bash
# C/C++ development
apt install clang clangd

# Python LSP
npm install -g pyright  # NOT pip3

# Go LSP
go install golang.org/x/tools/gopls@latest
export PATH=$PATH:$(go env GOPATH)/bin

# OCaml
opam install merlin
opam user-setup install

# Racket
raco pkg install racket-langserver
```

### Emacs Packages (Git)

Managed by `manager_git_package.sh`:
- hl-todo
- parenface
- find-file-in-project
- dimmer.el
- goto-line-preview
- breadcrumb
- indent-bars
- auto-save
- llvm-mode
- super-hint.el
- disaster

## Important Gotchas

### 1. Paredit Version

The repository includes `paredit.el` version 24 (beta). This is a BETA version not suitable for distribution via package managers. Key points:
- Located in `plugins/paredit.el`
- Has structural editing commands for Lisp
- Commands: `paredit-open-round`, `paredit-close-round`, `paredit-wrap-sexp`, etc.

### 2. Package Sources

Configuration uses USTC mirrors (China):
```elisp
(setq package-archives '(("gnu" . "http://mirrors.ustc.edu.cn/elpa/gnu/")
                         ("melpa" . "http://mirrors.ustc.edu.cn/elpa/melpa/")
                         ("nongnu" . "http://mirrors.ustc.edu.cn/elpa/nongnu/")))
```
For international users, uncomment the official MELPA sources in `.emacs`.

### 3. Lua Language Server Path

The Lua LSP is installed to a custom path:
```elisp
(add-to-list 'eglot-server-programs '((lua-mode) "~/.emacs.d/plugins/lua-lsp/bin/lua-language-server"))
```
Must run `install_lua_lsp.sh` first.

### 4. Performance Optimizations

Several aggressive performance optimizations are enabled:
- GC threshold manipulation during startup
- Bidirectional text reordering disabled
- Fast scrolling enabled
- Long line threshold: 1000 chars

If experiencing display issues, check these settings.

### 5. Terminal vs GUI Differences

Configuration handles both:
- Font height: 230 (GUI) vs 130 (terminal)
- Windows Terminal: Ctrl+Space remapped for mark command
- WSL: Windows clipboard integration functions available

### 6. Daemon Mode Recommended

Setup in shell config:
```bash
alias em="emacsclient -t"
export ALTERNATE_EDITOR=""
```
Start Emacs with `emacs --daemon` for fast client startup.

### 7. Custom Plugin Load Paths

Some plugins load from local paths:
- `~/.emacs.d/plugins/llvm-mode`
- `~/.emacs.d/plugins/disaster`
- `~/.emacs.d/plugins/breadcrumb`
- `~/.emacs.d/plugins/hl-todo`
- `~/.emacs.d/plugins/dimmer.el`
- `~/.emacs.d/plugins/goto-line-preview`
- `~/.emacs.d/plugins/super-hint.el/`
- `~/.emacs.d/plugins/parenface`
- `~/.emacs.d/plugins/indent-bars`

These are managed by `manager_git_package.sh clone`.

### 8. YASnippet Snippets

LaTeX snippets in `snippets/latex-mode/`:
- `enumerate` - Enumerate environment
- `itemize` - Itemize environment
- `lstinline` - Inline code
- `lstlisting` - Code listing
- `table` - Table environment
- `todo` - TODO comment

## Testing & Validation

There are no automated tests in this configuration. Validation approaches:

1. **Startup Test:**
```bash
emacs -q -l ~/.emacs  # Load config and check for errors
```

2. **Byte Compilation:**
```bash
M-x byte-compile-file RET ~/.emacs RET
```

3. **Package Check:**
```bash
M-x package-list-packages RET Ux  # Update all packages
```

## Common Tasks

### Adding a New Package

1. Add to `.emacs` using use-package:
```elisp
(use-package new-package
  :ensure t
  :defer t
  :bind ("C-c x" . new-package-command)
  :config
  (setq new-package-option value))
```

2. Restart Emacs or evaluate the buffer

### Adding a New Git Plugin

1. Edit `manager_git_package.sh`, add to `repos` array:
```bash
repos=(
    "git@github.com:user/repo.git"
    # ... existing repos
)
```

2. Run:
```bash
./manager_git_package.sh clone
```

### Adding LSP Support for New Language

1. Install the language server externally
2. Add to eglot configuration in `.emacs`:
```elisp
(add-to-list 'eglot-server-programs '((new-mode) "language-server-command"))
(add-hook 'new-mode-hook 'eglot-ensure)
```

### Modifying Key Bindings

Search for existing bindings with:
```bash
grep -n "global-set-key" .emacs
grep -n ":bind" .emacs
```

Add new bindings in the appropriate section or use `global-set-key` at the end of `.emacs`.

## Troubleshooting

### Emacs Won't Start

1. Check for syntax errors:
```bash
emacs -q --eval "(load-file \"~/.emacs\")"
```

2. Check package initialization:
```bash
M-x package-refresh-contents RET
```

### LSP Not Working

1. Verify language server is installed and in PATH
2. Check eglot server program configuration
3. Run `M-x eglot` manually to start
4. Check `M-x eglot-events` for debug info

### Slow Startup

1. Check `benchmark-init` results: `M-x benchmark-init-show-results`
2. Consider disabling unused features
3. Use `:defer t` in use-package declarations

### Package Installation Fails

1. Try alternative mirrors (comment/uncomment in package-archives)
2. Manually install: `M-x package-install RET package-name RET`
3. For git packages: `./manager_git_package.sh clone`

### Display/Font Issues

1. GUI vs terminal font settings differ
2. Check `set-face-attribute 'default` settings
3. Terminal may need UTF-8 configuration

## References

- [Emacs Lisp Intro](http://smacs.github.io/elisp/)
- [Emacs builtin modes](https://emacs-china.org/t/emacs-builtin-mode/11937/68)
- [use-package manual](https://phenix3443.github.io/notebook/emacs/modes/use-package-manual.html)
- [NeoTree Chinese Wiki](https://www.emacswiki.org/emacs/NeoTree_%E4%B8%AD%E6%96%87wiki)
- [Company Mode](https://www.emacswiki.org/emacs/CompanyMode)
- [compile_commands.json](https://zhuanlan.zhihu.com/p/145430576)
