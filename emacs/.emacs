;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 内置功能使用方法
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; 更换主题配色
;; M-x customize-themes

;; 开启括号自动补全模式
;; M-x electric-pair-mode

;; 文件编码转换
;; C-x RET r 编码类型(gbk,utf-8 ...)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 包加载配置
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'package)
;; (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
;; (add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/"))
(add-to-list 'package-archives '("gnu" . "http://mirrors.cloud.tencent.com/elpa/gnu/"))
(add-to-list 'package-archives '("melpa" . "http://mirrors.cloud.tencent.com/elpa/melpa/"))
;; (package-initialize)

(setq package-check-signature nil) ;;个别时候会出现签名校验失败
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

;; 关闭备份文件功能
(setq make-backup-files nil)
;; 行号的显示格式
(setq linum-format "%4d\u2502 ")
(global-linum-mode t)
;; 记录上次打开文件时 cursor 停留的位置
(save-place-mode 1)
;; 高亮当前行
;; (global-hl-line-mode 1)
;; 展示匹配的括号
(show-paren-mode 1)
;; 括号补全
(electric-pair-mode t)
;; 最下面的作为最优先选择的编码类型
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

;; 显示被折叠的行数
;; 这里额外启用了 :box t 属性使得提示更加明显
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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 插件
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; 括号补全 [paredit]
;; 使用 M-x paredit-mode 开启
(add-to-list 'load-path "~/.emacs.d/plugins")

(autoload 'paredit-mode "paredit"
  "Minor mode for pseudo-structurally editing Lisp code."
  t)

;; 资源管理器 [NeoTree]
;; 快捷键 https://www.emacswiki.org/emacs/NeoTree_%E4%B8%AD%E6%96%87wiki
(require 'neotree)
(global-set-key [f5] 'neotree-dir)
(global-set-key [f6] 'neotree-show)
(global-set-key [f7] 'neotree-hide)
(global-set-key [f8] 'neotree-find)

;; 让括号变得不显眼 [parenface]
(require 'parenface)
(set-face-foreground 'paren-face "DimGray")

;; 自动补全 [company]
;; https://www.emacswiki.org/emacs/CompanyMode
(require 'company)
;; 将显示延时关掉
(setq company-idle-delay 0)
;; 开启补全
(setq company-mode t)
;; 添加全局补全
(add-hook 'after-init-hook 'global-company-mode)
(global-set-key (kbd "C-x p") 'company-complete-common)

;; markdowm 支持 [markdowm-mode]
(autoload 'markdown-mode "markdown-mode"
  "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

;; 有道翻译 [youdao-dictionary]
(setq url-automatic-caching t)
(global-set-key (kbd "C-c y") 'youdao-dictionary-search-at-point)
(setq youdao-dictionary-search-history-file "~/.emacs.d/.youdao")

;; riggrep [rg]
;; 用法 https://rgel.readthedocs.io/en/2.0.3/usage.html#searching
;; C-c s r (rg)
;; C-c s t (rg-literal)
;; C-c s p (rg-project)
;; M-n / M-p Move to next/prev line with a match
;; n / p Move to next/prev line with a match, show that file in other buffer
;; M-N / M-P rg-next-file / rg-prev-file
(require 'rg)
(rg-enable-default-bindings)

;; 修复 256 色不可用的终端下颜色显示异常 [color-theme-approximate]
(require 'color-theme-approximate)
(color-theme-approximate-on)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 自动生成的东西
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;XS

(setq default-major-mode 'text-mode)
(add-hook 'text-mode-hook 'turn-on-auto-fill)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(custom-enabled-themes (quote (misterioso)))
 '(package-selected-packages
   (quote
    (color-theme-approximate neotree rg youdao-dictionary so-long company tabbar session pod-mode muttrc-mode mutt-alias markdown-mode initsplit htmlize graphviz-dot-mode folding eproject diminish csv-mode browse-kill-ring boxquote bm bar-cursor apache-mode))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
