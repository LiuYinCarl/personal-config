;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 常用快捷键
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; C-x k 关闭 buffer

;; M-< 跳到文件开头
;; M-> 跳到文件结尾

;; C-M f 跳到表达式/括号的结尾
;; C-M b 跳到表达式/括号的开头

;; M-h 选中当前段
;; M-g g 跳转到指定行, 当选择的行大于最大行数，会跳转到最后一行

;; 更换主题配色
;; M-x customize-themes

;; 开启括号自动补全模式
;; M-x electric-pair-mode

;; 在一个 buffer 中保存与回到光标位置
;; C-spc C-spc 设置一个 mark, 第二次是为了去掉高亮
;; C-u C-spc 回到上一个 makr 的位置

;; 多行缩进
;; C-spc 选中多行，然后 C-x TAB , 然后 left 或 right 进行移动

;; 文件编码转换
;; C-x RET r 编码类型(gbk,utf-8 ...)

;; 查看选中的区域内的行数、单词数、字符数
;; M-=

;; 全量更新 melpa 包
;; M-x package-list-packages RET Ux

;; 书签使用 bm 插件

;; Dired Mode
;; C-x d 进入Dired Mode
;; q 退出Dired Mode
;; + 创建目录
;; g 刷新目录
;; D 删除目录
;; C 拷贝
;; R 重命名
;; d 标记删除
;; u 取消标记
;; x 执行所有的标记

;; Occur-mode
;; M-s o 或者 M-x occur 进入
;; 在 Occur buffer 中按 C-c C-f 开启 next-error-follow-mirror-mode
;; 在 Occur buffer 中使用 M-p/M-n 切换上一个/下一个匹配项目
;; 配置在 Occur Buffer 中展示匹配项的前后 n 行
;; (setq list-matching-lines-default-context-lines n)

;; Usage package 文档
;; https://phenix3443.github.io/notebook/emacs/modes/use-package-manual.html

;; 打开最近的项目
;; M-x project-switch-project (C-x p p)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 此配置的外部程序依赖
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; apt install clang clangd fzf
;; npm install -g pyright  dont use pip3 install pyright, it's not a good choose
;; for update pyright: npm update -g pyright
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 启动优化配置
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; 避免启动时 GC
(setq gc-cons-threshold most-positive-fixnum ; 2^61 bytes
      gc-cons-percentage 0.6)

;; 统计 Emacs 启动时间，放在文件开头
(add-hook 'emacs-startup-hook
	  (lambda ()
	    (message "Emacs ready in %s with %d garbage collections."
		     (format "%.2f seconds"
			     (float-time
			      (time-subtract after-init-time before-init-time)))
		     gcs-done)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 包管理配置
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'package)
;; 官方源 安装 Emacs 的机器在外网时使用
;; (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
;; (add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/"))
;; 腾讯源 安装 Emacs 的机器在国内时使用
;; (add-to-list 'package-archives '("gnu" . "http://mirrors.cloud.tencent.com/elpa/gnu/"))
;; (add-to-list 'package-archives '("melpa" . "http://mirrors.cloud.tencent.com/elpa/melpa/"))
;; USTC
(setq package-archives '(("gnu" . "http://mirrors.ustc.edu.cn/elpa/gnu/")
                         ("melpa" . "http://mirrors.ustc.edu.cn/elpa/melpa/")
                         ("nongnu" . "http://mirrors.ustc.edu.cn/elpa/nongnu/")))


(setq package-check-signature nil) ;;个别时候会出现签名校验失败
(unless (bound-and-true-p package--initialized)
  (package-initialize)) ;; 刷新软件源索引
(unless package-archive-contents
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; 自动安装系统没有的 package
(setq use-package-always-ensure t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 全局配置
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; 设置默认字体大小 1 = 1/10 pt
(if (display-graphic-p)
    (set-face-attribute 'default nil :height 190) ;; GUI
  (set-face-attribute 'default nil :height 130))  ;; 终端

;; 退出 Emacs 时保存工程状态
(desktop-save-mode -1)

;; 设置光标颜色
(set-cursor-color "white")

;; 默认开启折行
;; (global-visual-line-mode t)

;; 选中即复制功能
(setq x-select-enable-primary t)

;; 状态栏显示列数
(column-number-mode 1)

;; 不显示工具栏
(tool-bar-mode -1)

;; 不显示菜单栏
(menu-bar-mode -1)

;; 不显示启动界面
(setq inhibit-splash-screen t)

;; 将yes/no 作为确认改成 y/n
(fset 'yes-or-no-p 'y-or-n-p)

;; 关闭备份文件功能
(setq make-backup-files nil)

;; 关闭自动保存文件功能
(setq auto-save-default nil)

;; 行号的显示格式
(setq linum-format "%4d\u2502")
(global-linum-mode t)

;; 某些模式不使用行号
(setq linum-disabled-modes-list '(eshell-mode deadgrep-mode))
(defun linum-on ()
  (unless (or (minibufferp)
	      (member major-mode linum-disabled-modes-list))
    (linum-mode 1)))

;; 记录上次打开文件时 cursor 停留的位置
(save-place-mode 1)

;; 高亮当前行
(global-hl-line-mode 1)

;; 展示匹配的括号
(show-paren-mode 1)

;; 括号补全
(electric-pair-mode t)

;; 保存最近打开的文件
(use-package recentf
  :config
  (setq recentf-max-menu-items 100)
  (setq recentf-max-saved-items 100)
  :hook (after-init . recentf-mode))

;; (global-set-key "\C-x\ \C-r" 'recentf-open-files)

;; 自动清除行尾空格
;; (add-hook 'before-save-hook 'delete-trailing-whitespace)
;; (add-hook 'before-save-hook 'whitespace-cleanup)

;; 字符编码优先级设置，最下面的作为最优先选择的编码类型
(prefer-coding-system 'cp950)
(prefer-coding-system 'gb2312)
(prefer-coding-system 'cp936)
(prefer-coding-system 'gb18030)
(prefer-coding-system 'utf-16)
(prefer-coding-system 'utf-8-dos)
(prefer-coding-system 'utf-8-unix)

;; 向上/向下翻半页
(autoload 'View-scroll-half-page-forward "view")
(autoload 'View-scroll-half-page-backward "view")
(global-set-key (kbd "C-v") 'View-scroll-half-page-forward)
(global-set-key (kbd "M-v") 'View-scroll-half-page-backward)

;; 解决粘贴中文出现乱码的问题
(set-clipboard-coding-system 'utf-8)
;; 终端中文乱码
(set-terminal-coding-system 'utf-8)
(modify-coding-system-alist 'process "*" 'utf-8)
(setq default-process-coding-system '(utf-8 . utf-8))

;; kill current buffer without verify
(global-set-key (kbd "C-x k") 'kill-current-buffer)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Emacs 优化插件
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; 防止超长行卡死 emacs
(use-package so-long
  :config (global-so-long-mode 1))

;; Emacs 内部打开的文件如果被外部修改，可以自动更新对应的 buffer
(use-package autorevert
  :ensure nil
  :hook (after-init . global-auto-revert-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 编程语言插件
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package cmake-mode
  :defer t)

(use-package go-mode
  :defer t)

(use-package lua-mode
  :defer t)

(use-package rust-mode
  :defer t)

;; haskell 代码补全
;; (use-package dante
;;   :ensure t
;;   :after haskell-mode
;;   :commands 'dante-mode
;;   :config
;;   (flycheck-add-next-checker 'haskell-dante '(info . haskell-hlint))
;;   :init
;;   (add-hook 'haskell-mode-hook 'flycheck-mode)
;;   ;; OR for flymake support:
;;   (add-hook 'haskell-mode-hook 'flymake-mode)
;;   (remove-hook 'flymake-diagnostic-functions 'flymake-proc-legacy-flymake)
;;   (add-hook 'haskell-mode-hook 'dante-mode))

(use-package markdown-mode
  :defer t
  :config
  (setq markdown-fontify-code-blocks-natively t)  ;; 语法高亮
  (add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
  (add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
  (add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode)))

;; OCaml
(let ((opam-share (ignore-errors (car (process-lines "opam" "var" "share")))))
  (when (and opam-share (file-directory-p opam-share))
    ;; Register Merlin
    (add-to-list 'load-path (expand-file-name "emacs/site-lisp" opam-share))
    (autoload 'merlin-mode "merlin" nil t nil)
    ;; Automatically start it in OCaml buffers
    (add-hook 'tuareg-mode-hook 'merlin-mode t)
    (add-hook 'caml-mode-hook 'merlin-mode t)
    ;; Use opam switch to lookup ocamlmerlin binary
    (setq merlin-command 'opam)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 文本编辑插件
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; tree-sitter 进行语法高亮
(use-package tree-sitter
  :defer t
  :if (fboundp 'module-load) ; 需要 Emacs 支持 Dynamic module
  :config
  (global-tree-sitter-mode)
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))

(use-package tree-sitter-langs
  :defer t
  :if (fboundp 'module-load) ; 需要Emacs 支持 Dynamic Module
  )

;; 快速选中区块 拓展顺序 字符 单词 句子 代码块 函数 全文件，按一次快捷键拓展一次
(use-package expand-region
  :defer t
  :bind ("M-o" . er/expand-region))

;; https://github.com/winterTTr/ace-jump-mode
(use-package ace-jump-mode
  :defer t
  :bind (("M-s j" . ace-jump-char-mode)
	 ("M-s k" . ace-jump-word-mode)
	 ("M-s l" . ace-jump-line-mode)))

;; 注释/反注释
(use-package newcomment
  :defer t
  :ensure nil
  :bind ([remap comment-dwim] . #'comment-or-uncomment)
  :config
  (defun comment-or-uncomment ()
    (interactive)
    (if (region-active-p)
	(comment-or-uncomment-region (region-beginning) (region-end))
      (if (save-excursion
	    (beginning-of-line)
	    (looking-at "\\s-*$"))
	  (call-interactively 'comment-dwim)
	(comment-or-uncomment-region (line-beginning-position) (line-end-position)))))
  :custom
  (comment-auto-fill-only-comments t))
(global-set-key (kbd "C-c /") 'comment-or-uncomment)

;; 快捷移动文本段和复制文本段
(use-package move-dup
  :defer t
  :bind (("C-c <up>"     . move-dup-move-lines-up)
	 ("C-c <down>"   . move-dup-move-lines-down)
	 ("C-c c <up>"   . move-dup-duplicate-up)
	 ("C-c c <down>" . move-dup-duplicate-down)))

(use-package multiple-cursors
  :defer t
  :bind(("C-c l" . mc/edit-lines)
	("C-c j" . mc/mark-previous-like-this)
	("C-c k" . mc/mark-next-like-this)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 窗口布局，文件管理，buffer 管理插件
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; M-x windresize 启动，然后使用方向键调整窗口大小
;; 用 i 调整步长，o键或者M-S-<up>/<left>跳到其它窗口，? 显示帮助，调整完了按RET退出即可
(use-package windresize
  :defer t)

(use-package windmove
  :defer t
  :ensure nil
  :bind (("M-s <up>"    . windmove-up)
	 ("M-s <down>"  . windmove-down)
	 ("M-s <left>"  . windmove-left)
	 ("M-s <right>" . windmove-right))
  :config (setq windmove-wrap-around t))  ;; 在边缘的窗口进行循环跳转，最左窗口跳到最右窗口等

(use-package indent-guide
  :defer t
  :config
  (indent-guide-global-mode t)
  (setq indent-guide-delay 0.0  ;; 展示对齐线的延迟时间
	indent-guide-recursive t))

;; 资源管理器 https://www.emacswiki.org/emacs/NeoTree_%E4%B8%AD%E6%96%87wiki
;; neotree 窗口有效
;; U       进入上层目录
;; g       刷新树。
;; A       最大/最小化 NeoTree 窗口
;; H       切换显示隐藏文件。
;; C-c C-n 创建文件，若以 / 结尾则表示创建文件夹。
;; C-c C-d 删除文件或目录。
;; C-c C-r 重命名文件或目录。
;; C-c C-c 改变根目录
(use-package neotree
  :defer t
  :bind (("C-c =" . neotree-show)
	 ("C-c -" . neotree-hide)
	 ("C-c d" . neotree-dir)
	 ("C-c +" . neotree-find)
	 ("C-c _" . neotree-ffip-project-dir)))

;; 将代码结构展示在右侧窗口
;; C-c C-j 可以在下方打开一个 imenu 窗口快速查找
(use-package imenu-list
  :bind ("C-c m" . imenu-list-smart-toggle)
  :config (setq imenu-list-focus-after-activation t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 程序交互插件
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; shell-pop 是在 Term Mode 之下的 term-char-mode
;; 如果想要操作 buffer，比如看历史记录，需要切换到 term-line-mode
;; C-c C-j 切换到 term-line-mode
;; C-c C-k 切换到 term-char-mode
(use-package shell-pop
  :init
  (setq shell-pop-default-directory "./"
	shell-pop-shell-type (quote ("ansi-term" "*ansi-term*" (lambda nil (ansi-term shell-pop-term-shell))))
	shell-pop-term-shell "/bin/bash"
	shell-pop-universal-key "C-t"
	shell-pop-window-size 30
	shell-pop-full-span t
	shell-pop-window-position "bottom"
	shell-pop-autocd-to-working-dir t
	shell-pop-restore-window-configuration t
	shell-pop-cleanup-buffer-at-process-exit t))

;; 使用 shell-pop 的时候避免退出 Emacs 时再确认一次
(require 'cl-lib)
(defadvice save-buffers-kill-emacs (around no-query-kill-emacs activate)
  "Prevent annoying \"Active processes exist\" query when you quit Emacs."
  (cl-letf (((symbol-function #'process-list) (lambda ())))
    ad-do-it))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 符号管理插件
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; 高亮光标下的符号并前后跳转
;; (use-package highlight-symbol
;;   :bind(("C-<f3>" . highlight-symbol)
;; 	("<f3>" . highlight-symbol-next)
;; ("<f4>" . highlight-symbol-prev)))

;; https://zhuanlan.zhihu.com/p/26471685
;; 在 symbol-overlay-mode 中的时候，可使用如下快捷键操作
;; "i" -> symbol-overlay-put                ; 高亮或取消高亮当前symbol
;; "n" -> symbol-overlay-jump-next          ; 跳转到下一个位置
;; "p" -> symbol-overlay-jump-prev          ; 跳转到上一个位置
;; "w" -> symbol-overlay-save-symbol        ; 复制当前symbol
;; "t" -> symbol-overlay-toggle-in-scope    ; 切换高亮范围到作用域
;; "e" -> symbol-overlay-echo-mark          ; 撤销上一次跳转
;; "d" -> symbol-overlay-jump-to-definition ; 跳转到定义
;; "s" -> symbol-overlay-isearch-literally  ; 切换为isearch并搜索当前symbol
;; "q" -> symbol-overlay-query-replace      ; 查找替换当前symbol
;; "r" -> symbol-overlay-rename             ; 对symbol直接重命名
(use-package symbol-overlay
  :defer t
  :bind (("M-i"  . symbol-overlay-put)
	 ("M-k"  . symbol-overlay-switch-forward)
	 ("M-j"  . symbol-overlay-switch-backward)
	 ("<f7>" . symbol-overlay-mode)
	 ("<f8>" . symbol-overlay-remove-all))
  :bind (:map symbol-overlay-map
	      ("i" . symbol-overlay-put)
	      ("n" . symbol-overlay-jump-next)
	      ("p" . symbol-overlay-jump-prev)
	      ("w" . symbol-overlay-save-symbol)
	      ("t" . symbol-overlay-toggle-in-scope)
	      ("e" . symbol-overlay-echo-mark)
	      ("d" . symbol-overlay-jump-to-definition)
	      ("s" . symbol-overlay-isearch-literally)
	      ("q" . symbol-overlay-query-replace)
	      ("r" . symbol-overlay-rename)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; LSP
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; 给 clangd 生成项目配置文件的工具 compile_commands.json
;; https://zhuanlan.zhihu.com/p/145430576
;; https://github.com/rizsotto/Bear
(when (not (version<= emacs-version "26.1"))
  (use-package eglot
    :ensure t
    :demand t
    :bind (("C-c h" . eldoc))
    :config
    (add-to-list 'eglot-server-programs '((c++-mode c-mode) "clangd"))
    (add-to-list 'eglot-server-programs '((python-mode)  "pyright-langserver" "--stdio"))
    (add-hook 'c-mode-hook 'eglot-ensure)
    (add-hook 'c++-mode-hook 'eglot-ensure)
    (add-hook 'python-mode-hook 'eglot-ensure)
    ;; 修改 eldoc-mode 的展示延迟时间，避免光标移动一下 eldoc 就展示新的内容，影响阅读
    (setq eldoc-idle-delay 1000000)))

;; 自动补全 https://www.emacswiki.org/emacs/CompanyMode
(use-package company
  :ensure t
  :demand t
  :bind (("C-c TAB" . company-complete-common))
  :config
  (setq company-idle-delay       0     ;; 延迟补全时间
	company-mode             t
	company-dabbrev-downcase nil)  ;; 补全区分大小写
  (add-hook 'after-init-hook 'global-company-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 搜索功能插件
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package youdao-dictionary
  :defer t
  :bind (("C-c f"   . youdao-dictionary-search-at-point+)
	 ("C-c w"   . youdao-dictionary-search-at-point-tooltip)
	 ("C-c SPC" . youdao-dictionary-search-from-input))
  :config
  (setq url-automatic-caching t
	youdao-dictionary-search-history-file "~/.emacs.d/.youdao"))

;; RET 在当前窗口打开文件
;; o   在其他窗口打开文件
;; n/p      上下移动，以行为单位
;; M-n/M-p  上下移动，以文件为单位
;; S 改变搜索关键字
;; D 改变搜索目录
;; g 重新搜索
;; C-c C-k 停止搜索
(use-package deadgrep
  :defer t
  :bind (("C-c s" . deadgrep)))

(if (version< emacs-version "27.1")
    (use-package ivy
      :config
      (ivy-mode 1)
      ;; Add recent files and bookmarks to the ivy-switch-buffer
      ;; (setq ivy-use-virtual-buffers t)
      (setq ivy-count-format "%d/%d ")
      )
  (use-package vertico
    :init
    (vertico-mode 1)
    :config
    (setq vertico-scroll-margin 0)
    (setq vertico-count 10)
    ;; (setq vertico-resize t)
    ;; (setq vertico-cycle t)
    ))

;; need by vertico
;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist
  :init
  (savehist-mode 1))

;; need by vertico
(use-package emacs
  :init
  (defun crm-indicator (args)
    (cons (format "[CRM%s] %s"
                  (replace-regexp-in-string
                   "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                   crm-separator)
                  (car args))
          (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)
  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)
  ;; Enable recursive minibuffers
  (setq enable-recursive-minibuffers t))

(use-package fzf
  :defer t
  :load-path "~/.emacs.d/plugins/fzf.el/"
  :bind (("M-s p" . fzf-find-file)
	 ("M-s -" . fzf-git)
	 ("M-s =" . fzf-recentf)
	 ("M-s b" . fzf-find-in-buffer)
	 ("M-s d" . fzf-find-file-current-dir))
  :config
  (setq fzf/args "-x --print-query --margin=0,0"
        fzf/executable "fzf"
        fzf/git-grep-args "-i --line-number %s"
        ;; command used for `fzf-grep-*` functions
        ;; example usage for ripgrep:
        fzf/grep-command "rg --no-heading -nH"
        ;; fzf/grep-command "grep -nrH"
        fzf/position-bottom t
        fzf/window-height 15))

(use-package goto-line-preview
  :load-path "~/.emacs.d/plugins/goto-line-preview"
  :demand t
  :bind (("M-g g" . goto-line-preview)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 主题配置插件
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package atom-one-dark-theme
  :demand t
  :config
  (load-theme 'atom-one-dark t))

;; 让括号变得不显眼
(use-package parenface
  :load-path "~/.emacs.d/plugins/parenface"
  :config
  (set-face-foreground 'paren-face "DimGray"))

;; 修改光标所在括号内的块的背景颜色
(use-package highlight-blocks
  :config
  (setq highlight-blocks-delay 0.05)
  ;; 设置背景颜色，第一个是显示的颜色
  (setq highlight-blocks--rainbow-colors
	'("#464641" "#000000" "#464646" "#FFCACA" "#FFFFBA"))
  (setq highlight-blocks-max-face-count
	(length highlight-blocks--rainbow-colors)))

(defun highlight-blocks--get-bounds ()
  (let ((result '())
	(parse-sexp-ignore-comments t))
    (condition-case nil
	(let* ((parse-state (syntax-ppss))
	       (starting-pos (if (or (nth 3 parse-state)
				     (nth 4 parse-state))
				 (nth 8 parse-state)
			       (point)))
	       (begins (nreverse (nth 9 parse-state)))
	       (end starting-pos)
	       (i 0))
	  (while (or (eq highlight-blocks-max-innermost-block-count t)
		     (< i highlight-blocks-max-innermost-block-count))
	    (setq end (scan-lists end 1 1))
	    (push (cons (pop begins) end) result)
	    (setq i (1+ i))))
      (scan-error))
    (last result)))

(defun highlight-blocks--define-rainbow-colors (colors)
  (dotimes (i (length colors))
    (face-spec-set
     (intern (format "highlight-blocks-depth-%d-face" (1+ i)))
     `((((class color) (background dark))  :background ,(nth i colors))
       (((class color) (background light)) :background ,(nth i colors)))
     'face-defface-spec)))

(highlight-blocks--define-rainbow-colors highlight-blocks--rainbow-colors)

(add-hook 'emacs-lisp-mode-hook       'highlight-blocks-mode)
(add-hook 'lisp-interaction-mode-hook 'highlight-blocks-mode)
(add-hook 'lisp-mode-hook             'highlight-blocks-mode)

;; 切换窗口时未获得焦点的窗口失去高光
(use-package dimmer
  :load-path "~/.emacs.d/plugins/dimmer.el"
  :config
  (dimmer-mode t))

(use-package bm
  :ensure t
  :init
  ;; restore on load (even before you require bm)
  (setq bm-restore-repository-on-load t)
  :config
  ;; Allow cross-buffer 'next'
  (setq bm-cycle-all-buffers t)
  ;; where to store persistant files
  (setq bm-repository-file "~/.emacs.d/bm-repository")

  (setq-default bm-buffer-persistence t)
  (add-hook 'after-init-hook 'bm-repository-load)
  (add-hook 'kill-buffer-hook #'bm-buffer-save)
  (add-hook 'kill-emacs-hook #'(lambda nil
				 (bm-buffer-save-all)
				 (bm-repository-save)))
  (add-hook 'after-save-hook #'bm-buffer-save)
  (add-hook 'find-file-hooks   #'bm-buffer-restore)
  (add-hook 'after-revert-hook #'bm-buffer-restore)
  (add-hook 'vc-before-checkin-hook #'bm-buffer-save)
  :bind (("M-<right>" . bm-next)
	 ("M-<left>" . bm-previous)
	 ("<f2>" . bm-toggle)
	 ("M-<up>" . bm-show-all)
	 ("M-<down>" . bm-show-quit-window)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 版本管理插件
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package diff-hl
  :config
  (global-diff-hl-mode)
  (diff-hl-flydiff-mode)
  ;; (diff-hl-margin-mode) ;; 终端下使用,不会自动刷新，需要手动调用
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 未分类插件
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; https://github.com/manateelazycat/awesome-tab
(use-package awesome-tab
  :demand t
  :load-path "~/.emacs.d/plugins/awesome-tab"
  :bind (("M-k" . awesome-tab-forward-tab)
	 ("M-j" . awesome-tab-backward-tab))
  :config
  (awesome-tab-mode t)
  ;; 颜色配置
  (setq awesome-tab-terminal-dark-select-background-color    "#708090")
  (setq awesome-tab-terminal-dark-select-foreground-color    "#FFFAFA")
  (setq awesome-tab-terminal-dark-unselect-background-color  "#1C1C1C")
  (setq awesome-tab-terminal-dark-unselect-foreground-color  "#FFFAFA")
  (setq awesome-tab-terminal-light-select-background-color   "#708090")
  (setq awesome-tab-terminal-light-select-foreground-color   "#FFFAFA")
  (setq awesome-tab-terminal-light-unselect-background-color "#1C1C1C")
  (setq awesome-tab-terminal-light-unselect-foreground-color "#FFFAFA"))

(defun hideshow-folded-overlay-fn (ov)
  (when (eq 'code (overlay-get ov 'hs))
    (let* ((nlines (count-lines (overlay-start ov) (overlay-end ov)))
	   (info (format "[%d]" nlines)))
      (overlay-put ov 'display (propertize info 'face hideshow-folded-face)))))

;; 显示被折叠的行数 这里额外启用了 :box t 属性使得提示更加明显
(defconst hideshow-folded-face '((t (:inherit 'font-lock-comment-face :box t))))

;; 函数折叠
(use-package hideshow
  :ensure nil
  :diminish hs-minor-mode
  :bind (:map prog-mode-map
	      ("ESC -" . hs-toggle-hiding)
	      ("ESC =" . hs-show-all))
  :hook (prog-mode . hs-minor-mode)
  :config (setq hs-set-up-overlay 'hideshow-folded-overlay-fn)
  :custom
  (hs-special-modes-alist
   (mapcar 'purecopy
	   '((c-mode "{" "}" "/[*/]" nil nil)
	     (c++-mode "{" "}" "/[*/]" nil nil)
	     (rust-mode "{" "}" "/[*/]" nil nil)))))

;; eshell 清屏
(add-hook
 'eshell-mode-hook
 (lambda ()
   (local-set-key (kbd "C-l")
		  (lambda ()
		    (interactive)
		    (let ((eshell-buffer-maximum-lines 0))
		      (eshell-truncate-buffer))))))

;; 退出 eshell
(add-hook
 'eshell-mode-hook
 (lambda ()
   (local-set-key (kbd "C-d")
		  (lambda (arg)
		    "Delete a character or quit eshell if there's nothing to delete."
		    (interactive "p")
		    (if (and (eolp) (looking-back eshell-prompt-regexp nil))
			(eshell-life-is-too-much)
		      (delete-char arg))))))

;; 一键格式化
(defun indent-whole ()
  (interactive)
  (indent-region (point-min) (point-max))
  (message "format successfully"))
(global-set-key [f10] 'indent-whole)

;; WSL Emacs 与 Windows 共通剪切板
(defun wsl-copy-region-to-clipboard (start end)
  "Copy region to Windows clipboard."
  (interactive "r")
  (call-process-region start end "clip.exe" nil 0))

(defun wsl-cut-region-to-clipboard (start end)
  (interactive "r")
  (call-process-region start end "clip.exe" nil 0)
  (kill-region start end))

(defun wsl-clipboard-to-string ()
  "Return Windows clipboard as string."
  (let ((coding-system-for-read 'dos))
    (substring; remove added trailing \n
     (shell-command-to-string
      "powershell.exe -Command Get-Clipboard") 0 -1)))

(defun wsl-paste-from-clipboard (arg)
  "Insert Windows clipboard at point. With prefix ARG, also add to kill-ring"
  (interactive "P")
  (let ((clip (wsl-clipboard-to-string)))
    (insert clip)
    (if arg (kill-new clip))))

;; (define-key global-map (kbd "C-x C-y") 'wsl-paste-from-clipboard)
(define-key global-map (kbd "C-c M-c") 'wsl-copy-region-to-clipboard)
;; (define-key global-map (kbd "C-x C-w") 'wsl-cut-region-to-clipboard)

;; 删除光标下左右两侧的空侧
(define-key global-map (kbd "M-s SPC") 'delete-horizontal-space)

;; GUI 下标题栏显示文件名
(setq frame-title-format
      '(buffer-file-name "%f" (dired-directory dired-directory "%b")))

(defun show-file-name ()
  "Show the full path file name in the minibuffer."
  (interactive)
  (message (concat "file path: " (buffer-file-name)))
  (kill-new (buffer-file-name))) ;; copy path to clipboard
(global-set-key (kbd "M-s n") 'show-file-name)

(defun open-emacs-config ()
  "Open .emacs file."
  (interactive)
  (find-file-read-only "~/.emacs"))
(global-set-key (kbd "M-s c") 'open-emacs-config)

(defun save-region-to-tmp-file ()
  (interactive)
  (write-region (region-beginning) (region-end) "~/.emacs.d/.tmp_copy_region"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 外观配置
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(bm-face ((t (:background "DimGray"))))
 '(bm-fringe-face ((t (:background "DimGray"))))
 '(bm-fringe-persistent-face ((t (:background "DimGray"))))
 '(bm-persistent-face ((t (:background "DimGray"))))
 '(deadgrep-filename-face ((t (:foreground "Orange"))))
 '(deadgrep-match-face ((t (:foreground "Green")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 自动生成的东西
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
