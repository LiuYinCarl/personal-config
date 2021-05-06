;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 更少的配置 更高的稳定性
;; Less configuration, higher stability
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; 统计 Emacs 启动时间
(add-hook 'emacs-startup-hook
	  (lambda ()
	    (message "Emacs ready in %s with %d garbage collections."
		     (format "%.2f seconds"
			     (float-time
			      (time-subtract after-init-time before-init-time)))
		     gcs-done)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 内置功能使用方法
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 更换主题配色
;; M-x customize-themes 

;; 开启括号自动补全模式
;; M-x electric-pair-mode

;; 文件编码转换
;; C-x RET r 编码类型(gbk,utf-8 ...)

;; 全量更新 melpa 包
;; M-x package-list-packages RET Ux

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 包加载配置
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'package)
;; (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
;; (add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/"))
(add-to-list 'package-archives '("gnu" . "http://mirrors.cloud.tencent.com/elpa/gnu/"))
(add-to-list 'package-archives '("melpa" . "http://mirrors.cloud.tencent.com/elpa/melpa/"))

(setq package-check-signature nil) ;;个别时候会出现签名校验失败
(require 'package) ;; 初始化包管理器
(unless (bound-and-true-p package--initialized)
  (package-initialize)) ;; 刷新软件源索引
(unless package-archive-contents
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Emacs 内置功能的使用
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 状态栏显示列数
(column-number-mode 1)

(defun terminal-init-screen ()
  "Terminal initialization function for screen."
  ;; Use the xterm color initialization code.
  (xterm-register-default-colors)
  (tty-set-up-initial-frame-faces))

;; 将yes/no 作为确认改成 y/n
(fset 'yes-or-no-p 'y-or-n-p)

;; 关闭备份文件功能
(setq make-backup-files nil)

;; 行号的显示格式
(setq linum-format "%4d\u2502 ")
(global-linum-mode t)

;; 记录上次打开文件时 cursor 停留的位置
(save-place-mode 1)

;; 高亮当前行
(global-hl-line-mode 1)

;; 展示匹配的括号
(show-paren-mode 1)

;; 括号补全
(electric-pair-mode t)

;; 字符编码优先级设置，最下面的作为最优先选择的编码类型
(prefer-coding-system 'cp950)
(prefer-coding-system 'gb2312)
(prefer-coding-system 'cp936)
(prefer-coding-system 'gb18030)
(prefer-coding-system 'utf-16)
(prefer-coding-system 'utf-8-dos)
(prefer-coding-system 'utf-8-unix)

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

;; [expand-region] 快速选中区块 拓展顺序 字符 单词 句子 代码块 函数 全文件
;; 按一次快捷键拓展一次
(require 'expand-region)
(global-set-key (kbd "C-c p")'er/expand-region)


;; 一键切换 .h/.cpp 文件
;; https://blog.flowlore.com/passages/emacs-switch-cpp-h-file/
(defun switch-cpp ()
  (global-set-key [f9] 'ffap)
  (global-set-key [f9] 'ff-find-other-file)
  )
(add-hook 'c-mode-hook 'switch-cpp)
(add-hook 'c++-mode-hook 'switch-cpp)
;; 一键开关eldoc-mode
(global-set-key [f4] 'eldoc-mode)

;; 一键格式化
(defun indent-whole ()
  (interactive)
  (indent-region (point-min) (point-max))
  (message "format successfully"))
;;绑定到F10键
(global-set-key [f10] 'indent-whole)

;; 在菜单栏添加 imenu Index
;; https://www.emacswiki.org/emacs/ImenuMode
;; (add-hook 'c-mode-hook 'imenu-add-menubar-index)

;; 注释/反注释
(use-package newcomment
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
(global-set-key (kbd "C-x /") 'comment-or-uncomment)

;; 函数折叠
(use-package hideshow
  :ensure nil
  :diminish hs-minor-mode
  :bind (:map prog-mode-map
	      ("ESC -" . hs-toggle-hiding)
	      ("ESC =" . hs-show-all))
  :hook (prog-mode . hs-minor-mode)
  :custom
  (hs-special-modes-alist
   (mapcar 'purecopy
	   '((c-mode "{" "}" "/[*/]" nil nil)
	     (c++-mode "{" "}" "/[*/]" nil nil)
	     (rust-mode "{" "}" "/[*/]" nil nil)))))
;; 显示被折叠的行数 这里额外启用了 :box t 属性使得提示更加明显
(defconst hideshow-folded-face '((t (:inherit 'font-lock-comment-face :box t))))

(defun hideshow-folded-overlay-fn (ov)
  (when (eq 'code (overlay-get ov 'hs))
    (let* ((nlines (count-lines (overlay-start ov) (overlay-end ov)))
	   (info (format " ... #%d " nlines)))
      (overlay-put ov 'display (propertize info 'face hideshow-folded-face)))))
(setq hs-set-up-overlay 'hideshow-folded-overlay-fn)

;; 防止超长行卡死 emacs
(use-package so-long
  :ensure nil
  :config (global-so-long-mode 1))

;; Emacs 内部打开的文件如果被外部修改，可以自动更新对应的 buffer
(use-package autorevert
  :ensure nil
  :hook (after-init . global-auto-revert-mode))

;; 向上/向下翻半页
;; https://emacs.stackexchange.com/questions/27698/how-can-i-scroll-a-half-page-on-c-v-and-m-v
(autoload 'View-scroll-half-page-forward "view")
(autoload 'View-scroll-half-page-backward "view")
(global-set-key (kbd "C-v") 'View-scroll-half-page-forward)
(global-set-key (kbd "M-v") 'View-scroll-half-page-backward)

;; 设置换行，避免切分单词
(visual-line-mode t)
(setq-default word-wrap t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 插件
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; [indent-guide]
(require 'indent-guide)
(indent-guide-global-mode)
;; (setq indent-guide-delay 0.1)
(setq indent-guide-recursive t)

;; [keyfreq] 按键统计
;; use keyfreq-show to see how many times you used a command
(require 'keyfreq)
(keyfreq-mode 1)
(keyfreq-autosave-mode 1)

;; [eglot]
(require 'eglot)
(add-to-list 'eglot-server-programs '((c-mode c++-mode) "clangd-11"))
(add-hook 'c-mode-hook 'eglot-ensure)
;; 不加下面的会跳转不到C++标准库文件
(add-hook 'c++-mode-hook 'eglot-ensure)
;; 关闭eldoc
(global-eldoc-mode -1)

;; [dante] haskell 代码补全
(use-package dante
  :ensure t
  :after haskell-mode
  :commands 'dante-mode
  :config
  (flycheck-add-next-checker 'haskell-dante '(info . haskell-hlint))
  :init
  (add-hook 'haskell-mode-hook 'flycheck-mode)
  ;; OR for flymake support:
  (add-hook 'haskell-mode-hook 'flymake-mode)
  (remove-hook 'flymake-diagnostic-functions 'flymake-proc-legacy-flymake)

  (add-hook 'haskell-mode-hook 'dante-mode)
  )

;; [paredit]括号补全 使用 M-x paredit-mode 开启
(add-to-list 'load-path "~/.emacs.d/plugins")
(autoload 'paredit-mode "paredit"
  "Minor mode for pseudo-structurally editing Lisp code."
  t)

;; [etags]
(defun create-tags (dir-name)
  "Create tags file."
  (interactive "DDirectory: ")
  (eshell-command
   (format "find %s -type f -name \"*.c\" -o -name \"*.h\" -o -name \"*.cpp\" -o -name \"*.hpp\" | etags -C -" dir-name)))

(defadvice find-tag (around refresh-etags activate)
  "Rerun etags and reload tags if tag not found and redo find-tag.              
   If buffer is modified, ask about save before running etags."
  (let ((extension (file-name-extension (buffer-file-name))))
    (condition-case err
	ad-do-it
      (error (and (buffer-modified-p)
		  (not (ding))
		  (y-or-n-p "Buffer is modified, save it? ")
		  (save-buffer))
	     (er-refresh-etags extension)
	     ad-do-it))))

(defun er-refresh-etags (&optional extension)
  "Run etags on all peer files in current dir and reload them silently."
  (interactive)
  (shell-command (format "etags *.%s" (or extension "el")))
  (let ((tags-revert-without-query t))  ; don't query, revert silently
    (visit-tags-table default-directory nil)))
;; 为当前目录的 .h .c 文件生成 tags, find 参数含义 -o(or) -a(and) -not(not)
;; find . -name "*.h" -o -name "*.c" | etags -
;; etags 常用快捷键[Emacs version >= 25] https://www.emacswiki.org/emacs/EmacsTags
;; 生成 etags 文件 https://www.emacswiki.org/emacs/BuildTags
;; 在当前目录下寻找 etags 生成的 tags 文件
;; (setq tags-file-name "/home/lzh/github/imgui/TAGS")
;; 如何使用 tags 文件
;; 1 使用 create_tag 函数创建 tags 文件
;; 2 使用 visit-tags-table 函数找到 tags 文件
;; 3 使用 find-tag(其他函数找不到，不知道为啥) 找到 tag
(global-set-key (kbd "M-/") 'find-tag)

;; [NeoTree]资源管理器 https://www.emacswiki.org/emacs/NeoTree_%E4%B8%AD%E6%96%87wiki
(use-package neotree)
;; (global-set-key [f5] 'neotree-dir)
(global-set-key (kbd "C-c =") 'neotree-show)
(global-set-key (kbd "C-c -") 'neotree-hide)

;; [parenface]让括号变得不显眼
(require 'parenface)
(set-face-foreground 'paren-face "DimGray")

;; [company]自动补全 https://www.emacswiki.org/emacs/CompanyMode
(use-package company)
;; 将显示延时关掉
(setq company-idle-delay 0)
;; 开启补全
(setq company-mode t)
;; 添加全局补全
(add-hook 'after-init-hook 'global-company-mode)
(global-set-key (kbd "C-x p") 'company-complete-common)
;; 补全的时候区分大小写
(setq company-dabbrev-downcase nil)

;; [markdown-mode]
(use-package markdown-mode)

;; [youdao-dictionary]有道翻译
(setq url-automatic-caching t)
(global-set-key (kbd "C-c SPC") 'youdao-dictionary-search-at-point+)
(global-set-key (kbd "C-c C-SPC") 'youdao-dictionary-search-from-input)
(setq youdao-dictionary-search-history-file "~/.emacs.d/.youdao")

;; riggrep [rg]
;; 用法 https://rgel.readthedocs.io/en/2.0.3/usage.html#searching
;; C-c s r (rg)
;; C-c s t (rg-literal)
;; C-c s p (rg-project)
;; M-n / M-p Move to next/prev line with a match
;; n / p Move to next/prev line with a match, show that file in other buffer
;; M-N / M-P rg-next-file / rg-prev-file
(use-package rg)
(rg-enable-default-bindings)

;; [atom-one-dark-theme]
(require 'atom-one-dark-theme)
(load-theme 'atom-one-dark t)

;; [highlight-thing] 高亮光标下的单词
(require 'highlight-thing)
(global-highlight-thing-mode t)
(setq highlight-thing-what-thing 'symbol)
(setq highlight-thing-delay-seconds 0.1)
(setq highlight-thing-limit-to-defun t)
(setq highlight-thing-case-sensitive-p t)
;; https://github.com/fgeller/highlight-thing.el/issues/9
(set-face-foreground 'highlight-thing "green")
(set-face-background 'highlight-thing "black")

;; [snails] 模糊搜索
(add-to-list 'load-path "~/.emacs.d/plugins/snails") ; add snails to your load-path
(require 'snails)
;; 不使用浮动窗口， wsl下用不了
(setq snails-show-with-frame 1)
(global-set-key (kbd "M-p") 'snails)

;; [awesome-tab] https://github.com/manateelazycat/awesome-tab
(use-package awesome-tab
  :load-path "~/.emacs.d/plugins/awesome-tab"
  :config
  (awesome-tab-mode t))
(global-set-key (kbd "M-k") 'awesome-tab-forward-tab)
(global-set-key (kbd "M-j") 'awesome-tab-backward-tab)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 针对文件类型设置模式
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 使用 M-x align 进行缩进
;; Align-region 规则
(add-hook 'align-load-hook (lambda ()
			     (add-to-list 'align-rules-list
					  '(haskell-types
					    (regexp . "\\(\\s-+\\)\\(::\\|∷\\)\\s-+")
					    (modes quote (haskell-mode literate-haskell-mode))))))
(add-hook 'align-load-hook (lambda ()
			     (add-to-list 'align-rules-list
					  '(haskell-assignment
					    (regexp . "\\(\\s-+\\)=\\s-+")
					    (modes quote (haskell-mode literate-haskell-mode))))))
(add-hook 'align-load-hook (lambda ()
			     (add-to-list 'align-rules-list
					  '(haskell-arrows
					    (regexp . "\\(\\s-+\\)\\(->\\|→\\)\\s-+")
					    (modes quote (haskell-mode literate-haskell-mode))))))
(add-hook 'align-load-hook (lambda ()
			     (add-to-list 'align-rules-list
					  '(haskell-left-arrows
					    (regexp . "\\(\\s-+\\)\\(<-\\|←\\)\\s-+")
					    (modes quote (haskell-mode literate-haskell-mode))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 自动生成的东西
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(haskell-mode-hook (quote (interactive-haskell-mode company-mode)))
 '(package-selected-packages
   (quote
    (dante company-ghci indent-guide haskell-mode keyfreq etable highlight-thing youdao-dictionary use-package so-long rg project neotree markdown-mode ggtags eglot company atom-one-dark-theme))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
