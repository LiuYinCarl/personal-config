;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 常用快捷键
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; M-< 跳到文件开头
;; M-> 跳到文件结尾

;; C-M f 跳到表达式/括号的结尾
;; C-M b 跳到表达式/括号的开头

;; M-h 选中当前段
;; M-g g 跳转到指定行, 当选择的行大于最大行数，会跳转到最后一行

;; 更换主题配色
;; M-x customize-themes

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

;; 查看按键 -> 命令映射  C-h k HotKey
;; 查看命令 -> 按键映射  C-h w  Command

;; 以C-x r开头的命令来进行矩形操作。先用C-space或者C-@设一个mark，
;; 移动光标到另一点，用以下命令进行列操作
;; C-x r r 复制一个矩形区域到寄存器
;; C-x r k 剪切一个矩形块
;; C-x r y 粘贴一个矩形块
;; C-x r o 插入一个矩形块
;; C-x r c 清除一个矩形块(使其变成空白)
;; C-x r t 在选定区域的所有列前插入样的字符

;; 先选中文本区域，然后执行如下命令使选中区域进行左右移动
;; C-u 10 C-x TAB  ;; 整体右移 10 个字符
;; C-u -2 C-x TAB  ;; 整体左移 2 个字符

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 此配置的外部程序依赖
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; apt install clang clangd fzf
;; npm install -g pyright  dont use pip3 install pyright, it's not a good choose
;; for update pyright: npm update -g pyright

;; 安装 Golang LSP Server
;; go env -w GOPROXY=https://goproxy.cn,direct # 可选
;; go install golang.org/x/tools/gopls@latest
;; PATH=$PATH:$(go env GOPATH)/bin # 添加到 .bashrc

;; OCaml
;; opam install merlin
;; opam user-setup install
;; 查看 ocamlmerlin Path: whereis ocamlmerlin

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

;; 设置光标颜色
(set-cursor-color "white")

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

;; 设置每行最大长度
(setq-default display-fill-column-indicator-column 80)

;; 显示行尾空格
(setq-default show-trailing-whitespace t)
(add-hook 'term-mode-hook
	  (lambda () (setq show-trailing-whitespace nil)))

;; 行号的显示格式
(setq linum-format "%4d\u2502")
(global-linum-mode t)
;; 某些模式不使用行号
(setq linum-disabled-modes-list '(eshell-mode deadgrep-mode))
(defun linum-on ()
  (unless (or (minibufferp)
	      (member major-mode linum-disabled-modes-list))
    (linum-mode 1)))

;; 高亮当前行
;; (global-hl-line-mode 1)

;; 括号补全
(electric-pair-mode t)

;; 展示匹配的括号
(use-package paren
  :ensure nil
  :hook (after-init . show-paren-mode)
  :config
  (setq show-paren-when-point-inside-paren t
        show-paren-when-point-in-periphery t))

;; 记录上次打开文件时 cursor 停留的位置
(use-package saveplace
  :ensure nil
  :hook (after-init . save-place-mode))

;; 保存最近打开的文件
(use-package recentf
  :config
  (setq recentf-max-menu-items 100
	recentf-max-saved-items 100)
  :hook (after-init . recentf-mode))

;; 选中文本后直接输入，省去删除被选中文本的操作
(use-package delsel
  :ensure nil
  :hook (after-init . delete-selection-mode))

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

(use-package exec-path-from-shell
  :ensure t
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize))
  (when (daemonp)
    (exec-path-from-shell-initialize)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 编程语言插件
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package cmake-mode
  :defer t)

(use-package go-mode
  :defer t
  :config
  (add-hook 'go-mode-hook (lambda() (setq tab-width 4))))

(use-package lua-mode
  :defer t)

(use-package rust-mode
  :defer t)

(use-package markdown-mode
  :defer t
  :config
  (setq markdown-fontify-code-blocks-natively t)  ;; 语法高亮
  (add-to-list 'auto-mode-alist '("\\.text\\'"     . markdown-mode))
  (add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
  (add-to-list 'auto-mode-alist '("\\.md\\'"       . markdown-mode)))

;; OCaml
(use-package tuareg)

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

;; tree-sitter 进行语法高亮
(use-package tree-sitter
  :ensure t
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
	 ("M-s ]" . end-of-defun))
  )

;; (use-package avy
;;   :defer t
;;   :bind ("M-c" . avy-goto-word-1))

;; 替代 avy, 可以跳转中文
(use-package ace-pinyin
  :defer t
  :init (ace-pinyin-global-mode 1)
  :bind ("M-c" . avy-goto-word-1))

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
  ;; 在边缘的窗口进行循环跳转，最左窗口跳到最右窗口等
  :config (setq windmove-wrap-around t))

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
(use-package imenu-list
  :bind ("C-c m" . imenu-list-smart-toggle)
  :config (setq imenu-list-focus-after-activation t))

(use-package dashboard
  :ensure t
  :config
  (dashboard-setup-startup-hook)
  (setq dashboard-startup-banner 2)
  (setq dashboard-center-content t)
  (setq dashboard-set-footer nil)
  (setq dashboard-items '((recents  . 5)
                          (projects . 5)))
  (setq dashboard-projects-backend 'project-el))

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
	shell-pop-window-size 50
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; LSP
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(when (not (version<= emacs-version "26.1"))
  (use-package eglot
    :ensure t
    :demand t
    :bind (("C-c h" . eldoc))
    :config
    (add-to-list 'eglot-server-programs '((c++-mode c-mode) "clangd"))
    (add-to-list 'eglot-server-programs '((python-mode)  "pyright-langserver" "--stdio"))
    (add-hook 'c-mode-hook      'eglot-ensure)
    (add-hook 'c++-mode-hook    'eglot-ensure)
    (add-hook 'python-mode-hook 'eglot-ensure)
    (add-hook 'go-mode-hook     'eglot-ensure)
    (add-hook 'haskell-mode-hook 'eglot-ensure)
    (setq eldoc-idle-delay 1000000)  ;; 修改 eldoc-mode 的展示延迟时间
    (setq completion-ignore-case t)  ;; company-capf匹配时不区分大小写
    (setq-default eglot-workspace-configuration
                  '((haskell
                     (plugin
                      (stan
                       (globalOn . :json-false))))))  ;; disable stan

    ))

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
;; 搜索功能插件
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; RET     ; 在当前窗口打开文件
;; o       ; 在其他窗口打开文件
;; n/p     ; 上下移动，以行为单位
;; M-n/M-p ; 上下移动，以文件为单位
;; S       ; 改变搜索关键字
;; D       ; 改变搜索目录
;; g       ; 重新搜索
;; C-c C-k ; 停止搜索
(use-package deadgrep
  :defer t
  :bind (("C-c s" . deadgrep)))

(use-package counsel
  :bind (("C-c SPC" . counsel-imenu)))

(use-package swiper
  :defer t
  :bind (("C-c b" . swiper-isearch)))

(if (version< emacs-version "27.1")
    (use-package ivy
      :config
      (ivy-mode 1)
      ;; Add recent files and bookmarks to the ivy-switch-buffer
      ;; (setq ivy-use-virtual-buffers t)
      (setq ivy-height 20)
      (setq ivy-count-format "%d/%d "))
  (use-package vertico
    :init (vertico-mode 1)
    :config (setq vertico-scroll-margin 0
		  vertico-count 20
		  ;; setq vertico-resize t
		  ;; setq vertico-cycle t
		  )))

;; need by vertico
;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist
  :init (savehist-mode 1))

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
  :config (setq fzf/args          "-x --print-query --margin=0,0"
		fzf/executable    "fzf"
		fzf/git-grep-args "-i --line-number %s"
		fzf/grep-command  "rg --no-heading -nH"
		fzf/position-bottom t
		fzf/window-height 15))

(use-package goto-line-preview
  :load-path "~/.emacs.d/plugins/goto-line-preview"
  :demand t
  :bind (("M-g g" . goto-line-preview)))

(use-package isearch
  :ensure nil
  :config (setq isearch-lazy-highlight t
		isearch-lazy-count t
		lazy-count-prefix-format "[%s/%s] "
		lazy-count-suffix-format ""))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 主题配置插件
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package ef-themes
  :demand t
  :config (load-theme 'ef-dark t))

;; (load-theme 'misterioso)

;; 让括号变得不显眼
(use-package parenface
  :load-path "~/.emacs.d/plugins/parenface"
  :config (set-face-foreground 'paren-face "#909595"))

;; 修改光标所在括号内的块的背景颜色
;; (use-package highlight-blocks
;;   :config
;;   (setq highlight-blocks-delay 0.05
;; 	;; 设置背景颜色，第一个是显示的颜色
;; 	highlight-blocks--rainbow-colors '("#606060" "#000000" "#464641")
;; 	highlight-blocks-max-face-count (length highlight-blocks--rainbow-colors)))

;; (defun highlight-blocks--get-bounds ()
;;   (let ((result '())
;; 	(parse-sexp-ignore-comments t))
;;     (condition-case nil
;; 	(let* ((parse-state (syntax-ppss))
;; 	       (starting-pos (if (or (nth 3 parse-state)
;; 				     (nth 4 parse-state))
;; 				 (nth 8 parse-state)
;; 			       (point)))
;; 	       (begins (nreverse (nth 9 parse-state)))
;; 	       (end starting-pos)
;; 	       (i 0))
;; 	  (while (or (eq highlight-blocks-max-innermost-block-count t)
;; 		     (< i highlight-blocks-max-innermost-block-count))
;; 	    (setq end (scan-lists end 1 1))
;; 	    (push (cons (pop begins) end) result)
;; 	    (setq i (1+ i))))
;;       (scan-error))
;;     (last result)))

;; (defun highlight-blocks--define-rainbow-colors (colors)
;;   (dotimes (i (length colors))
;;     (face-spec-set
;;      (intern (format "highlight-blocks-depth-%d-face" (1+ i)))
;;      `((((class color) (background dark))  :background ,(nth i colors))
;;        (((class color) (background light)) :background ,(nth i colors)))
;;      'face-defface-spec)))

;; (highlight-blocks--define-rainbow-colors highlight-blocks--rainbow-colors)

;; (add-hook 'emacs-lisp-mode-hook       'highlight-blocks-mode)
;; (add-hook 'lisp-interaction-mode-hook 'highlight-blocks-mode)
;; (add-hook 'lisp-mode-hook             'highlight-blocks-mode)
;; (add-hook 'scheme-mode-hook           'highlight-blocks-mode)

;; 切换窗口时未获得焦点的窗口失去高光
(use-package dimmer
  :load-path "~/.emacs.d/plugins/dimmer.el"
  :config
  (dimmer-mode t))

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
  :bind (("M-<right>" . bm-next)
	 ("M-<left>"  . bm-previous)
	 ("<f2>"      . bm-toggle)
	 ("M-<up>"    . bm-show-all)
	 ("M-<down>"  . bm-show-quit-window)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 版本管理插件
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (use-package diff-hl
;;   :config
;;   (global-diff-hl-mode)
;;   (diff-hl-flydiff-mode)
;;   ;; (diff-hl-margin-mode) ;; 终端下使用,不会自动刷新，需要手动调用
;;   )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 未分类插件
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package breadcrumb
  :demand t
  :load-path "~/.emacs.d/plugins/breadcrumb"
  :config
  (breadcrumb-mode t))

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
	   (info (format " #[%d]..." nlines)))
      (overlay-put ov 'display (propertize info 'face hideshow-folded-face)))))

;; 显示被折叠的行数 这里额外启用了 :box t 属性使得提示更加明显
(defconst hideshow-folded-face '((t (:inherit 'font-lock-comment-face :box t))))

;; 函数折叠
(use-package hideshow
  :ensure nil
  :diminish hs-minor-mode
  :bind (:map prog-mode-map
	      ("C-c (" . hs-toggle-hiding)
	      ("C-c )" . hs-show-all))
  :hook (prog-mode . hs-minor-mode)
  :config (setq hs-set-up-overlay 'hideshow-folded-overlay-fn)
  :custom
  (hs-special-modes-alist
   (mapcar 'purecopy
	   '((c-mode    "{" "}" "/[*/]" nil nil)
	     (c++-mode  "{" "}" "/[*/]" nil nil)
	     (rust-mode "{" "}" "/[*/]" nil nil)))))

;; 一键格式化
(defun my-indent-whole ()
  (interactive)
  (indent-region (point-min) (point-max))
  (message "format successfully"))
(global-set-key [f10] 'my-indent-whole)

;; align-regexp 使用空格对其
(defadvice align-regexp (around align-regexp-with-spaces activate)
  (let ((indent-tabs-mode nil))
    ad-do-it))

;; 删除光标下左右两侧的空侧
(define-key global-map (kbd "M-s SPC") 'delete-horizontal-space)

(defun my-show-file-name ()
  "Show the full path file name in the minibuffer."
  (interactive)
  (message (concat "file path: " (buffer-file-name)))
  (kill-new (buffer-file-name))) ;; copy path to clipboard
(global-set-key (kbd "M-s n") 'my-show-file-name)

(defun my-open-emacs-config ()
  "Open .emacs file."
  (interactive)
  (find-file-read-only "~/.emacs"))
(global-set-key (kbd "M-s c") 'my-open-emacs-config)

(defun my-fast-note ()
  (interactive)
  (find-file-other-window "~/fast-note.md"))
(global-set-key (kbd "M-n") 'my-fast-note)

(defun my-save-region-to-tmp-file ()
  (interactive)
  (let ((content (buffer-substring (region-beginning) (region-end))))
    (with-temp-buffer
      (insert content)
      (insert "\n")
      (write-file "~/.emacs.d/.tmp_copy_region"))))

(defun my-copy-word-at-point ()
  (interactive)
  (let* ((sym (symbol-at-point))
	 (sym-name (when sym
		     (substring-no-properties (symbol-name sym)))))
    (message (concat "copy symbol: " sym-name))
    (kill-new sym-name)))
(global-set-key (kbd "M-s s") 'my-copy-word-at-point)

(defun my-kill-word-at-point ()
  (interactive)
  (backward-word)
  (kill-word 1))
(global-set-key (kbd "M-DEL") 'my-kill-word-at-point)

;; auto fullscreen on GUI mode
(add-hook 'window-setup-hook #'toggle-frame-maximized t)

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
 '(deadgrep-match-face ((t (:foreground "Green"))))
 '(hl-fill-column-face ((t (:background "DimGray")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 工具生成的配置，不同机器差异不大
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; use opam user-setup install to generate
;; ## added by OPAM user-setup for emacs / base ## 56ab50dc8996d2bb95e7856a6eddb17b ## you can edit, but keep this line
(require 'opam-user-setup "~/.emacs.d/opam-user-setup.el")
;; ## end of OPAM user-setup addition for emacs / base ## keep this line

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 自动生成的东西
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
