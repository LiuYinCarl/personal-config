;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 常用快捷键
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; | 快捷键/函数                        | 功能说明                                    |
;; | ---------------                    | --------------------                        |
;; | C-M f                              | 跳到表达式/括号的结尾                       |
;; | C-M b                              | 跳到表达式/括号的开头                       |
;; | C-h k HotKey                       | 查看按键 -> 命令映射                        |
;; | C-h w  Command                     | 查看命令 -> 按键映射                        |
;; | C-x RET r [gbk/utf-8/...]          | 文件编码转换                                |
;; | M-<                                | 跳到文件开头                                |
;; | M->                                | 跳到文件末尾                                |
;; | M-s 方向键                         | 跳到其他窗口                                |
;; | M-g g                              | 跳转到指定行                                |
;; | M-=                                | 查看选中的区域内的行数,单词数,字符数        |
;; | M-x customize-themes               | 更换主题配色                                |
;; | M-x package-list-packages RET Ux   | 全量更新 melpa 包                           |
;; | set-frame-font                     | 设置字体                                    |
;; | M-x project-switch-project/C-x p p | 打开最近的项目                              |
;; | C-u 10 C-x TAB                     | 整体右移 10 个字符,要先选中文本区域         |
;; | C-u -2 C-x TAB                     | 整体左移 2 个字符,要先选中文本区域          |
;; | C-x TAB [left/right]               | 向左/右移动一个字符位置,要先选中文本区域    |
;; | C-x r r                            | 复制矩形区域到寄存器,要先选中文本区域       |
;; | C-x r k                            | 剪切矩形块,要先选中文本区域                 |
;; | C-x r y                            | 粘贴矩形块,要先选中文本区域                 |
;; | C-x r o                            | 插入矩形块,要先选中文本区域                 |
;; | C-x r c                            | 清除矩形块(使其变成空白),要先选中文本区域   |
;; | C-x r t                            | 在选定区域所有列前插入字符,要先选中文本区域 |
;; | C-spc C-spc                        | 设置一个 mark, 第二次是为了去掉高亮         |
;; |C-u C-spc                           | 回到上一个 makr 的位置                      |
;; | M-s .                              | isearch-forward-symbol 开始搜索当前符号     |
;; | C-s/C-r                            | isearch-forward/backward 向后查找/向前查找  |
;;

;; 项目内进行全局搜索替换
;; 1. 执行 deadgrep 进行搜索，搜索结果都展示在 deadgrep buffer
;; 2. 执行 deadgrep-edit-mode, 使得可以编辑 deadgrep buffer
;; 3. 执行 query-replace 对 deadgrep 内文本进行替换，? 查看快捷键

;; 筛选/过滤文件中的指定行
;; keep-lines/flush-lines

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

;; 使用 gdb-mode 进行调试
;; step1: gdb
;; step2: gdb-many-windows

;; interactive 函数绑定了快捷键如何传递参数
;; 先按 C-u，然后输入参数，最后再按快捷键

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 此配置的外部程序依赖
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; apt install clang clangd
;; npm install -g pyright  dont use pip3 install pyright, it's not a good choose
;; for update pyright: npm update -g pyright

;; 安装 Golang LSP Server
;; go env -w GOPROXY=https://goproxy.cn,direct # 可选
;; go install golang.org/x/tools/gopls@latest
;; PATH=$PATH:$(go env GOPATH)/bin # 添加到 .bashrc

;; opam install merlin
;; opam user-setup install
;; 查看 ocamlmerlin path: whereis ocamlmerlin

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 启动优化配置
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; 避免启动时 GC
(setq gc-cons-threshold most-positive-fixnum ; 2^61 bytes
      gc-cons-percentage 0.6)

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
;; USTC 源
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
    (set-face-attribute 'default nil :height 180) ;; GUI
  (set-face-attribute 'default nil :height 130))  ;; 终端

;; 设置光标颜色
(set-cursor-color "white")
;; 选中即复制功能
(setq x-select-enable-primary t)
;; 状态栏显示列数
(column-number-mode 1)
;; GUI 不显示工具栏
(tool-bar-mode -1)
;; GUI 不显示菜单栏
(menu-bar-mode -1)
;; 不显示启动界面
(setq inhibit-splash-screen t)
;; 将yes/no 作为确认改成 y/n
(fset 'yes-or-no-p 'y-or-n-p)
;; 关闭备份文件功能
(setq make-backup-files nil)
;; 设置备份文件目录
(setq backup-directory-alist (quote (("." . "~/.emacs.d/backup_files"))))
;; 关闭自动保存文件功能
(setq auto-save-default nil)
;; 设置 tab 宽度
(setq default-tab-width 4)
;; 设置将 tab 替换为空格
(setq-default indent-tabs-mode nil)
;; 设置 c 语言缩进
(setq c-basic-offset 4)
;; 设置 C 语言注释格式为 // 而不是 /* */
(add-hook 'c-mode-hook (lambda () (c-toggle-comment-style -1)))
;; 显示行尾空格
(setq-default show-trailing-whitespace t)
(add-hook 'term-mode-hook (lambda () (setq show-trailing-whitespace nil)))
;; 行号展示，26以下可以使用 linum
(global-display-line-numbers-mode)
;; 括号补全
(electric-pair-mode t)
;; 字符编码优先级设置，优先选择的编码类型
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
;; 解决大文件打开慢的问题
(setq-default bidi-display-reordering nil)
(setq bidi-inhibit-bpa t
      long-line-threshold 1000
      large-hscroll-threshold 1000
      syntax-wholeline-max 1000)
;; kill current buffer without verify
(global-set-key (kbd "C-x k") 'kill-current-buffer)
;; 去掉行尾的续行符号
(set-display-table-slot standard-display-table 'wrap ?\ )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Emacs 优化插件
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; 统计各个插件的启动消耗时间
(use-package benchmark-init)

;; 防止超长行卡死 emacs
(use-package so-long
  :config (global-so-long-mode 1))

;; Emacs 内部打开的文件如果被外部修改，可以自动更新对应的 buffer
(use-package autorevert
  :hook (after-init . global-auto-revert-mode))

(use-package exec-path-from-shell
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize))
  (when (daemonp)
    (exec-path-from-shell-initialize)))

;; 展示匹配的括号
(use-package paren
  :hook (after-init . show-paren-mode)
  :config
  (setq show-paren-when-point-inside-paren t
        show-paren-when-point-in-periphery t))

;; 保存最近打开的文件
(use-package recentf
  :config
  (setq recentf-max-menu-items 50
	recentf-max-saved-items 50)
  :hook (after-init . recentf-mode))

;; 选中文本后直接输入可删除选中文本并输入，省去删除被选中文本的操作
(use-package delsel
  :hook (after-init . delete-selection-mode))

;; 记录上次打开文件时 cursor 停留的位置
(use-package saveplace
  :hook (after-init . save-place-mode))

;; 查看和管理 minor-mode
(use-package manage-minor-mode)

;; 文件保存时删除编辑过的行的行尾空格
(use-package ws-butler
  :ensure t
  :config
  (add-hook 'prog-mode-hook #'ws-butler-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 编程语言插件
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package cmake-mode
  :defer t)

(use-package go-mode
  :defer t
  :config
  (add-hook 'go-mode-hook (lambda() (setq tab-width 4))))

(use-package rust-mode
  :defer t)

(use-package lua-mode
  :defer t)

(use-package markdown-mode
  :defer t
  :config
  (setq markdown-fontify-code-blocks-natively t)  ;; 语法高亮
  (add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
  (add-to-list 'auto-mode-alist '("\\.md\\'"       . markdown-mode)))

;; OCaml
(use-package tuareg
  :defer t)

;; needed only if ocamlmerlin not already in your PATH
(setq merlin-command "~/.opam/5.0.0/bin/ocamlmerlin")
(add-hook 'tuareg-mode-hook #'merlin-mode)
(add-hook 'caml-mode-hook #'merlin-mode)

;; (use-package haskell-mode
;;   :ensure t)

;; ;; haskell 代码补全
;; (use-package dante
;;   :ensure t
;;   :after haskell-mode
;;   :commands 'dante-mode
;;   :config
;;   (flycheck-add-next-checker 'haskell-dante '(info . haskell-hlint))
;;   :init
;;   ;; (add-hook 'haskell-mode-hook 'flycheck-mode)
;;   ;; OR for flymake support:
;;   (add-hook 'haskell-mode-hook 'flymake-mode)
;;   (remove-hook 'flymake-diagnostic-functions 'flymake-proc-legacy-flymake)
;;   (add-hook 'haskell-mode-hook 'dante-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 文本编辑插件
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; 撤销修改
;; f   to go forward
;; b   to go backward
;; n   to go to the node below when you at a branching point
;; p   to go to the node above
;; a   to go back to the last branching point
;; e   to go forward to the end/tip of the branch
;; q   to quit, you can also type C-g
;; C-c C-s (or whatever binding you used for save-buffer)
;;     to save the buffer at the current undo state
(use-package vundo
  :if (not (version<= emacs-version "28.0"))
  :bind ("M-s u" . vundo))

;; 语法高亮
(use-package tree-sitter
  :defer t
  :if (fboundp 'module-load) ; 需要 Emacs 支持 Dynamic module
  :config
  (global-tree-sitter-mode)
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))

(use-package tree-sitter-langs
  :defer t
  :if (fboundp 'module-load)) ; 需要Emacs 支持 Dynamic Module

;; 快速选中区块 拓展顺序 字符 单词 句子 代码块 函数 全文件，按一次快捷键拓展一次
(use-package expand-region
  :defer t
  :bind ("M-o" . er/expand-region))

(use-package smartparens
  :defer t
  :config
  (require 'smartparens-config)
  (add-hook 'js-mode-hook     #'smartparens-mode)
  (add-hook 'c-mode-hook      #'smartparens-mode)
  (add-hook 'c++-mode-hook    #'smartparens-mode)
  (add-hook 'go-mode-hook     #'smartparens-mode)
  (add-hook 'python-mode-hook #'smartparens-mode)
  :bind (("M-s [" . beginning-of-defun)
         ("M-s ]" . end-of-defun)))

;; (use-package avy
;;   :defer t
;;   :bind ("M-c" . avy-goto-word-1))

;; 代替 switch-to-buffer
(use-package ace-jump-buffer
  :defer t
  :bind ("C-x b" . ace-jump-buffer))

;; 替代 avy, 可以跳转中文
(use-package ace-pinyin
  :defer t
  :init (ace-pinyin-global-mode 1)
  :bind ("M-c" . avy-goto-word-1))

;; 注释/反注释
(use-package newcomment
  :ensure nil
  :defer t
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
  (comment-auto-fill-only-comments t)
  :bind (("C-c /" . comment-or-uncomment)))

;; 快捷移动文本段和复制文本段
(use-package move-dup
  :defer t
  :bind (("C-c <up>"     . move-dup-move-lines-up)
	 ("C-c <down>"   . move-dup-move-lines-down)
	 ("C-c c <up>"   . move-dup-duplicate-up)
	 ("C-c c <down>" . move-dup-duplicate-down)))

(use-package multiple-cursors
  :bind(("C-c l" . mc/edit-lines)
	("C-c j" . mc/mark-previous-like-this)
	("C-c k" . mc/mark-next-like-this)))

;; 自动找到当前光标下的关键字并进行编辑
(use-package iedit
  :demand t
  :bind (("C-c ;" . iedit-mode) ;; 进入和退出编辑
         ("C-c '" . iedit-show/hide-context-lines))) ;; 只展示匹配行

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 窗口布局，文件管理，buffer 管理插件
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; M-x windresize 启动，然后使用方向键调整窗口大小
;; 用 i 调整步长，o键或者M-S-<up>/<left>跳到其它窗口，? 显示帮助，调整完了按RET退出即可
(use-package windresize
  :defer t)

;; 在窗口间移动
(use-package ace-window
  :defer t
  :bind ("M-s p" . ace-window))

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
  :config
  (setq neo-hidden-regexp-list '("\\.pyc" "~$" "^#.*#$" "\\.elc$" "__pycache__" "\\.o$" "\\.git" "\\.clangd" "\\.gdb.*$"))
  :bind (("C-c =" . neotree-toggle)
         ("C-c -" . neotree-find)
	 ("C-c d" . neotree-dir)))

;; 将代码结构展示在右侧窗口
(use-package imenu-list
  :bind ("C-c i" . imenu-list-smart-toggle)
  :config (setq imenu-list-focus-after-activation t))

(use-package dashboard
  :ensure t
  :config
  (dashboard-setup-startup-hook)
  (setq dashboard-startup-banner 2)
  (setq dashboard-center-content t)
  (setq dashboard-set-footer nil)
  (setq dashboard-items '((recents  . 15)
                          (projects . 2)))
  (setq dashboard-projects-backend 'project-el))

;; 恢复之前的窗口布局
(use-package winner-mode
  :ensure nil
  :bind (("C-c <left>"  . winner-undo)
	 ("C-c <right>" . winner-redo))
  :hook (after-init . winner-mode))

(use-package ediff
  :hook (ediff-quit . winner-undo))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 程序交互插件
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; shell-pop 是在 Term Mode 之下的 term-char-mode
;; 如果想要操作 buffer，比如看历史记录，需要切换到 term-line-mode
;; C-c C-j 切换到 term-line-mode
;; C-c C-k 切换到 term-char-mode
(use-package shell-pop
  :demand t
  :init
  (setq shell-pop-default-directory "./"
	shell-pop-shell-type (quote ("ansi-term" "*ansi-term*" (lambda nil (ansi-term shell-pop-term-shell))))
	shell-pop-term-shell "/bin/bash"
	shell-pop-universal-key "C-t"
	shell-pop-window-size 50
	shell-pop-full-span t
	shell-pop-window-position "bottom"
	shell-pop-autocd-to-working-dir t
	shell-pop-restore-window-configuration t
	shell-pop-cleanup-buffer-at-process-exit t))

;; 使用 shell-pop 的时候避免退出 Emacs 时再确认一次
(require 'cl-lib)
(defadvice save-buffers-kill-emacs (around no-query-kill-emacs activate)
  (cl-letf (((symbol-function #'process-list) (lambda ())))
    ad-do-it))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 符号管理插件
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package symbol-overlay
  :defer t
  :bind (("M-s i" . symbol-overlay-put)
	 ("M-s k" . symbol-overlay-switch-forward)
	 ("M-s j" . symbol-overlay-switch-backward)
	 ("M-s 9" . symbol-overlay-mode)
	 ("M-s 0" . symbol-overlay-remove-all))
  :bind
  (:map symbol-overlay-map
	("i" . symbol-overlay-put)                ; 高亮或取消高亮当前symbol
	("n" . symbol-overlay-jump-next)          ; 跳转到下一个位置
	("p" . symbol-overlay-jump-prev)	  ; 跳转到上一个位置
	("w" . symbol-overlay-save-symbol)        ; 复制当前symbol
	("t" . symbol-overlay-toggle-in-scope)    ; 切换高亮范围到作用域
	("e" . symbol-overlay-echo-mark)	  ; 撤销上一次跳转
	("d" . symbol-overlay-jump-to-definition) ; 跳转到定义
	("s" . symbol-overlay-isearch-literally)  ; 切换为isearch并搜索当前symbol
	("q" . symbol-overlay-query-replace)      ; 查找替换当前symbol
	("r" . symbol-overlay-rename)             ; 对symbol直接重命名
	))

(use-package hl-todo
  :load-path "~/.emacs.d/plugins/hl-todo"
  :config
  (global-hl-todo-mode t))

;; 面包屑导航
(use-package breadcrumb
  :demand t
  :load-path "~/.emacs.d/plugins/breadcrumb"
  :config
  (breadcrumb-mode t))

(use-package yasnippet
  :demand t
  :config
  (yas-global-mode t)
  (setq auto-save-disable-predicates
        '((lambda ()
            (string-suffix-p
             "yas"
             (file-name-extension (buffer-name)) t))))
  :bind (("C-c y" . yas-expand)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; LSP
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package eglot
  :if (not (version<= emacs-version "26.1"))
  :ensure t
  :demand t
  :bind (("C-c h" . eldoc))
  :config
  (add-to-list 'eglot-server-programs '((c++-mode c-mode) "clangd"))
  (add-to-list 'eglot-server-programs '((python-mode)  "pyright-langserver" "--stdio"))
  (add-hook 'c-mode-hook       'eglot-ensure)
  (add-hook 'c++-mode-hook     'eglot-ensure)
  (add-hook 'python-mode-hook  'eglot-ensure)
  (add-hook 'go-mode-hook      'eglot-ensure)
  (add-hook 'haskell-mode-hook 'eglot-ensure)
  (add-hook 'rust-mode-hook    'eglot-ensure)
  ;; (add-hook 'eglot-managed-mode-hook (lambda () (eglot-inlay-hints-mode -1))) ;; 关闭行内函数参数展示
  (setq eldoc-idle-delay 1000000)  ;; 修改 eldoc-mode 的展示延迟时间
  (setq completion-ignore-case t)  ;; company-capf匹配时不区分大小写
  (setq-default eglot-workspace-configuration
                '((haskell
                   (plugin
                    (stan
                     (globalOn . :json-false)))))))  ;; disable stan

;; 自动补全 https://www.emacswiki.org/emacs/CompanyMode
(use-package company
  :ensure t
  :demand t
  :bind (("C-c TAB" . company-complete-common))
  :config (setq company-idle-delay       0     ;; 延迟补全时间
		company-mode             t
		company-dabbrev-downcase nil)  ;; 补全区分大小写
  (add-hook 'after-init-hook 'global-company-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 搜索功能插件 搜索功能的快捷键前缀保持为 C-c SPC
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package isearch
  :ensure nil
  :config (setq isearch-lazy-highlight t
		isearch-lazy-count t
		lazy-count-prefix-format "[%s/%s] "
		lazy-count-suffix-format ""))

;; RET     ; 在当前窗口打开文件
;; o       ; 在其他窗口打开文件
;; n/p     ; 上下移动，以行为单位
;; M-n/M-p ; 上下移动，以文件为单位
;; S       ; 改变搜索关键字
;; D       ; 改变搜索目录
;; g       ; 重新搜索
;; C-c C-k ; 停止搜索
(defun pp/deadgrep-view-file ()
  "View result under cursor in other window."
  (interactive)
  (deadgrep-visit-result-other-window)
  (other-window 1))

(defadvice deadgrep (around deadgrep-with-spaces activate)
  "Save current position before call deadgrep"
  (better-jumper-set-jump)
  ad-do-it)

(use-package deadgrep
  :defer t
  :bind (("C-c SPC s" . deadgrep)
	 :map deadgrep-mode-map
         ("v" . pp/deadgrep-view-file)))

(use-package counsel
  :config
  (setq ivy-use-virtual-buffers t)
  :bind (("C-c SPC i" . counsel-imenu)     ;; 搜索 imenu list
	 ("C-c SPC k" . counsel-rg)        ;; 搜索项目内关键字
         ("C-c SPC r" . counsel-recentf)   ;; 搜索最近打开的文件
         ("C-c SPC g" . counsel-git)       ;; 遵循 .gitignore 在项目中搜索文件
         ("C-c SPC d" . counsel-find-file) ;; 从目录搜索文件
         ("C-c SPC b" . counsel-grep-or-swiper) ;; 搜索 buffer 内关键字
         ))

(use-package ivy
  :config
  (ivy-mode 0) ;; just load config, not use
  (setq ivy-height 20)
  (setq ivy-count-format "%d/%d "))

(if (version< emacs-version "27.1")
    (ivy-mode 1)
  (use-package vertico
    :init (vertico-mode 1)
    :config (setq vertico-scroll-margin 0
		  vertico-count 20)))

;; need by vertico
(use-package savehist
  :init (savehist-mode 1))

;; need by vertico. Optionally use the `orderless' completion style.
(use-package orderless
  :init
  (setq completion-styles '(orderless basic)
        orderless-matching-styles '(orderless-flex orderless-literal)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

;; for company mode
;; (defun just-one-face (fn &rest args)
;;   (let ((orderless-match-faces [completions-common-part]))
;;     (apply fn args)))
;; (advice-add 'company-capf--candidates :around #'just-one-face)

(defun company-completion-styles (capf-fn &rest args)
  (let ((completion-styles '(basic partial-completion)))
    (apply capf-fn args)))
(advice-add 'company-capf :around #'company-completion-styles)

;; need by vertico
(use-package emacs
  :init
  ;; Add prompt indicator to `completing-read-multiple'.
  ;; We display [CRM<separator>], e.g., [CRM,] if the separator is a comma.
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
  ;; Emacs 28: Hide commands in M-x which do not work in the current mode.
  ;; Vertico commands are hidden in normal buffers.
  (setq read-extended-command-predicate
        #'command-completion-default-include-p)
  ;; Enable recursive minibuffers
  (setq enable-recursive-minibuffers t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 主题配置插件
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (load-theme 'misterioso)
;; (use-package ef-themes
;;   :demand t
;;   :config (load-theme 'ef-dark t))

(use-package doom-themes
  :demand t
  :config (load-theme 'doom-tokyo-night t))

;; 让括号变得不显眼
(use-package parenface
  :load-path "~/.emacs.d/plugins/parenface"
  :config (set-face-foreground 'paren-face "#909595"))

;; 切换窗口时未获得焦点的窗口失去高光
(use-package dimmer
  :load-path "~/.emacs.d/plugins/dimmer.el"
  :config
  (dimmer-configure-which-key)
  (dimmer-configure-company-box)
  (dimmer-configure-gnus)
  (dimmer-configure-posframe)
  (dimmer-mode t))

;; 显示被编辑过的行和 git 操作
(use-package git-gutter
  :init
  (global-git-gutter-mode t)
  :config
  (custom-set-variables
   '(git-gutter:update-interval 1.0))
  :bind (("M-s <up>" . git-gutter:previous-hunk)
         ("M-s <down>" . git-gutter:next-hunk)
         ("M-s <left>" . git-gutter:popup-hunk)
         ("M-s <right>" . git-gutter:mark-hunk)
         ("M-s g <up>" . git-gutter:stage-hunk)
         ("M-s g <down>" . git-gutter:revert-hunk)))

;; 文本居中，专注模式
(use-package olivetti
  :ensure t
  :init (setq olivetti-body-width 0.6) ;; 设置页面宽度比例
  :bind (("C-c m o" . olivetti-mode)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 跳转插件
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package goto-line-preview
  :load-path "~/.emacs.d/plugins/goto-line-preview"
  :demand t
  :bind (("C-c g" . goto-line-preview)))

;; ivy-push-view/ivy-pop-view/ivy-switch-view 功能类似
(use-package better-jumper
  :bind (("C-c p" . better-jumper-set-jump)
         ("C-c [" . better-jumper-jump-backward)
         ("C-c ]" . better-jumper-jump-forward)
         ("C-c \\" . better-jumper-clear-jumps)))

(use-package bm
  :ensure t
  :demand t
  :init
  (setq bm-restore-repository-on-load t)
  :config
  (setq bm-cycle-all-buffers t)
  (setq bm-repository-file "~/.emacs.d/bm-repository")
  (setq-default bm-buffer-persistence t)
  (add-hook 'after-init-hook        'bm-repository-load)
  (add-hook 'kill-buffer-hook	    #'bm-buffer-save)
  (add-hook 'after-save-hook	    #'bm-buffer-save)
  (add-hook 'find-file-hooks	    #'bm-buffer-restore)
  (add-hook 'after-revert-hook	    #'bm-buffer-restore)
  (add-hook 'vc-before-checkin-hook #'bm-buffer-save)
  (add-hook 'kill-emacs-hook	    #'(lambda nil
					(bm-buffer-save-all)
					(bm-repository-save)))
  :bind (("C-c b <right>" . bm-next)
	 ("C-c b <left>"  . bm-previous)
	 ("C-c b SPC"     . bm-toggle)
	 ("C-c b <up>"    . bm-show-all)
	 ("C-c b <down>"  . bm-show-quit-window)))

(use-package fanyi
  :ensure t
  :custom
  (fanyi-providers
   '(
     fanyi-haici-provider ;; 海词
     fanyi-youdao-thesaurus-provider ;; 有道同义词词典
     ;; fanyi-etymon-provider ;; Etymonline
     fanyi-longman-provider)) ;; Longman
  :bind (("C-c SPC t" . fanyi-dwim2)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 未分类插件
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package which-key
  :demand t
  :config (which-key-mode))

(use-package auto-save
  :demand t
  :load-path "~/.emacs.d/plugins/auto-save"
  :config
  (auto-save-enable)
  (setq auto-save-silent t))

(use-package delete-block
  :demand t
  :load-path "~/.emacs.d/plugins/delete-block"
  :config
  (defun delete-block-at-point ()
    (interactive)
    (delete-block-backward)
    (delete-block-forward))
  :bind (("M-s DEL" . delete-block-at-point)
         ;; 删除光标下左右两侧的空侧
         ;; 这个函数是 Emacs 自带的，放这里只是为了对称
         ("M-s SPC" . delete-horizontal-space)))

(use-package quickrun
  :config
  (quickrun-add-command "c++/c17"
    '((:command . "g++")
      (:exec    . ("%c -std=c++17 %o -o %e %s" "%e %a"))
      (:remove  . ("%e")))
    :default "c++")
  (quickrun-add-command "python"
    '((:command . "python3")
      (:exec    . ("%c %s")))
    :mode 'python-mode)
  :bind (("<f5>" . quickrun)))

(use-package centaur-tabs
  :demand t
  :config
  (centaur-tabs-mode t)
  (setq centaur-tabs-cycle-scope 'tabs)
  (setq centaur-tabs-show-new-tab-button nil)
  (setq centaur-tabs-gray-out-icons 'buffer) ;; 未被选中 tab 置灰
  (setq centaur-tabs-set-bar 'left)          ;; 被选中 tab 左侧展示特殊标志
  (setq centaur-tabs-set-close-button nil)   ;; 展示关闭 tab 按钮
  (setq centaur-tabs-set-modified-marker t)  ;; 文件修改后显示修改符号
  :bind
  ("M-h" . centaur-tabs-ace-jump)
  ("M-j" . centaur-tabs-backward)
  ("M-k" . centaur-tabs-forward))

(defun hideshow-folded-overlay-fn (ov)
  (when (eq 'code (overlay-get ov 'hs))
    (let* ((nlines (count-lines (overlay-start ov) (overlay-end ov)))
	   (info (format " #[%d]... " nlines)))
      (overlay-put ov 'display (propertize info 'face hideshow-folded-face)))))

;; 显示被折叠的行数 这里额外启用了 :box t 属性使得提示更加明显
(defconst hideshow-folded-face '((t (:inherit 'font-lock-comment-face :box t))))

;; 函数折叠
(use-package hideshow
  :diminish hs-minor-mode
  :bind (:map prog-mode-map
	      ("C-c (" . hs-toggle-hiding)
	      ("C-c )" . hs-show-all))
  :hook (prog-mode . hs-minor-mode)
  :config (setq hs-set-up-overlay 'hideshow-folded-overlay-fn))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 自定义函数
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; align-regexp 使用空格而不是 tab 对齐
(defadvice align-regexp (around align-regexp-with-spaces activate)
  (let ((indent-tabs-mode nil))
    ad-do-it))

;; 一键格式化
(defun my-indent-whole ()
  (interactive)
  (indent-region (point-min) (point-max))
  (message "format success"))
(global-set-key [f10] 'my-indent-whole)

(defun my-show-file-name ()
  "Show the full path file name in the minibuffer."
  (interactive)
  (message (concat "file path: " (buffer-file-name)))
  (kill-new (buffer-file-name))) ;; copy path to clipboard

(defun my-open-emacs-config ()
  "Open .emacs file."
  (interactive)
  (find-file-read-only "~/.emacs"))

(defun my-fast-note ()
  "open ~/fast-note.md in other window"
  (interactive)
  (find-file-other-window "~/fast-note.md"))

(defun my-save-region-to-tmp-file ()
  "save region to ~/.emacs.d/.tmp_copy_region"
  (interactive)
  (let ((content (buffer-substring (region-beginning) (region-end))))
    (with-temp-buffer
      (insert content)
      (insert "\n")
      (write-file "~/.emacs.d/.tmp_copy_region"))))

(defun my-copy-word-at-point ()
  "copy word at point"
  (interactive)
  (let* ((sym (symbol-at-point))
	 (sym-name (when sym
		     (substring-no-properties (symbol-name sym)))))
    (message (concat "copy symbol: " sym-name))
    (kill-new sym-name)))

(defun my-kill-all-file-buffers ()
  "Kills all buffers that are open to files. Does not kill
modified buffers or special buffers."
  (interactive)
  (mapc 'kill-buffer (cl-loop for buffer being the buffers
                              when (and (buffer-file-name buffer)
                                        (not (buffer-modified-p buffer)))
                              unless (eq buffer (current-buffer))
                              collect buffer)))

(defun my-query-and-replace ()
  "find string in project, then repalce by select."
  (interactive)
  (let* ((w1 (read-string "query replace: "))
         (w2 (read-string (format "query replace '%s' with: " w1))))
    (deadgrep w1)
    (while buffer-read-only
      (condition-case err
          (progn
            (message "deadgrep is still searching, waiting...")
            (sit-for 0.2)
            (deadgrep-edit-mode))
        (user-error
         (message "deadgrep user-error occur, ignore...")
         (ignore))))
    (query-replace w1 w2)))

(global-set-key (kbd "C-c m n") 'my-show-file-name)
(global-set-key (kbd "C-c m c") 'my-open-emacs-config)
(global-set-key (kbd "C-c m n") 'my-fast-note)
(global-set-key (kbd "C-c m t") 'my-save-region-to-tmp-file)
(global-set-key (kbd "C-c m w") 'my-copy-word-at-point)
(global-set-key (kbd "M-s s")   'my-copy-word-at-point) ;; 额外映射一个方便的按键
(global-set-key (kbd "C-c m k") 'my-kill-all-file-buffers)
(global-set-key (kbd "C-c m f") 'my-query-and-replace)
(global-set-key (kbd "C-c m a") 'align-regexp)

;; auto full screen on GUI mode
(add-hook 'window-setup-hook #'toggle-frame-maximized t)

;; Windows Terminal 下 Ctrl+Space 无效，但是这个很常用，所以映射一下
;; TODO: remove this hack when bug fixed: https://github.com/PowerShell/Win32-OpenSSH/issues/1842
;; Add this to your Windows Terminal settings.json
;; {
;;   "command":
;;   { "action": "sendInput",
;;     "input": "\u001b[9~"
;;   },
;;   "keys": "ctrl+space"
;; }
(global-set-key "\e[9~" 'set-mark-command)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 外观配置
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(aw-leading-char-face ((t (:background "Black" :foreground "Orange" :height 180.0))))
 '(bm-face ((t (:background "DimGray"))))
 '(bm-fringe-face ((t (:background "DimGray"))))
 '(bm-fringe-persistent-face ((t (:background "DimGray"))))
 '(bm-persistent-face ((t (:background "DimGray"))))
 '(centaur-tabs-selected ((t (:inherit bold :foreground "yellow"))))
 '(deadgrep-filename-face ((t (:foreground "Orange"))))
 '(deadgrep-match-face ((t (:foreground "Green"))))
 '(font-lock-comment-face ((t (:foreground "Green" :inherit nil))))
 '(font-lock-doc-face ((t (:foreground "Blue" :inherit nil))))
 '(goto-line-preview-hl ((t (:background "DimGray"))))
 '(hl-fill-column-face ((t (:background "DimGray")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 工具生成的配置，不同机器差异不大
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-hook
 'tuareg-mode-hook
 (lambda ()
   ;; use opam user-setup install to generate
   ;; ## added by OPAM user-setup for emacs / base ## 56ab50dc8996d2bb95e7856a6eddb17b ## you can edit, but keep this line
   (require 'opam-user-setup "~/.emacs.d/opam-user-setup.el")
   ;; ## end of OPAM user-setup addition for emacs / base ## keep this line
   ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 自动生成的东西
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
