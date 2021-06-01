;; 统计 Emacs 启动时间，放在文件开头
(add-hook 'emacs-startup-hook
	  (lambda ()
	    (message "Emacs ready in %s with %d garbage collections."
		     (format "%.2f seconds"
			     (float-time
			      (time-subtract after-init-time before-init-time)))
		     gcs-done)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 更少的配置 更高的稳定性
;; Less configuration, higher stability
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; use-package 资料
;; https://rand01ph.github.io/blog/use-package/
;; https://phenix3443.github.io/notebook/emacs/modes/use-package-manual.html#orga40eccf


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 常用快捷键
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; M-h 选中当前段

;; 更换主题配色
;; M-x customize-themes 

;; 开启括号自动补全模式
;; M-x electric-pair-mode

;; 文件编码转换
;; C-x RET r 编码类型(gbk,utf-8 ...)

;; 全量更新 melpa 包
;; M-x package-list-packages RET Ux


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 包加载配置
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Emacs 内置功能的使用
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; 状态栏显示列数
(column-number-mode 1)

;; shift + 方向键实现在窗口之间跳转
(windmove-default-keybindings)
;; 在边缘的窗口进行循环跳转，最左窗口跳到最右窗口等
(setq windmove-wrap-around t)

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

;; 在菜单栏添加 imenu Index
;; https://www.emacswiki.org/emacs/ImenuMode
;; (add-hook 'c-mode-hook 'imenu-add-menubar-index)

;; 向上/向下翻半页
;; https://emacs.stackexchange.com/questions/27698/how-can-i-scroll-a-half-page-on-c-v-and-m-v
(autoload 'View-scroll-half-page-forward "view")
(autoload 'View-scroll-half-page-backward "view")
(global-set-key (kbd "C-v") 'View-scroll-half-page-forward)
(global-set-key (kbd "M-v") 'View-scroll-half-page-backward)

;; 设置换行，避免切分单词
(visual-line-mode t)
(setq-default word-wrap t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 插件
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; 自动安装系统没有的 package
(setq use-package-always-ensure t)

;; 主动更新安装的包
;; (use-package auto-package-update
;;   :config
;;   (setq auto-package-update-delete-old-version t)
;;   (setq quto-package-update-hide-results t)
;;   (auto-package-update-maybe))

;; M-x windresize 启动，然后使用方向键调整窗口大小
;; 用 i 调整步长，o键或者M-S-<up>/<left>跳到其它窗口，? 显示帮助，调整完了按RET退出即可
(use-package windresize
  :defer t)

(use-package cmake-mode
  :defer t)

(use-package go-mode
  :defer t)

;; 防止超长行卡死 emacs
(use-package so-long
  :ensure nil
  :config (global-so-long-mode 1))

;; Emacs 内部打开的文件如果被外部修改，可以自动更新对应的 buffer
(use-package autorevert
  :ensure nil
  :hook (after-init . global-auto-revert-mode))

;; 快速选中区块 拓展顺序 字符 单词 句子 代码块 函数 全文件，按一次快捷键拓展一次
(use-package expand-region
  :bind ("M-o" . er/expand-region))

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
  :bind (("M-i"  . symbol-overlay-put)
	 ("M-k"  . symbol-overlay-switch-forward)
	 ("M-j"  . symbol-overlay-switch-backward)
	 ("<f7>" . symbol-overlay-mode)
	 ("<f8>" . symbol-overlay-remove-all)
	 :map symbol-overlay-mode 
	 ("i" . symbol-overlay-put)
	 ("n" . symbol-overlay-jump-next)
	 ("p" . symbol-overlay-jump-prev)
	 ("w" . symbol-overlay-save-symbol)
	 ("t" . symbol-overlay-toggle-in-scope)
	 ("e" . symbol-overlay-echo-mark)
	 ("d" . symbol-overlay-jump-to-definition)
	 ("s" . symbol-overlay-isearch-literally)
	 ("q" . symbol-overlay-query-replace)
	 ("r" . symbol-overlay-rename))
  )

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
(global-set-key (kbd "C-c /") 'comment-or-uncomment)

;; 快捷移动文本段和复制文本段
(use-package move-dup
  :bind (("C-c <up>"     . move-dup-move-lines-up)
	 ("C-c <down>"   . move-dup-move-lines-down)
	 ("C-c c <up>"   . move-dup-duplicate-up)
	 ("C-c c <down>" . move-dup-duplicate-down)))

(use-package indent-guide
  :config
  (indent-guide-global-mode t)
  (setq indent-guide-delay 0.1  ;; 展示对齐线的延迟时间
        indent-guide-recursive t))

(use-package eglot
  :config
  (add-to-list 'eglot-server-programs '((c-mode c++-mode) "clangd-11"))
  (add-hook 'c-mode-hook 'eglot-ensure)
  ;; 不加下面的会跳转不到C++标准库文件
  (add-hook 'c++-mode-hook 'eglot-ensure))

;; haskell 代码补全
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

;; 资源管理器 https://www.emacswiki.org/emacs/NeoTree_%E4%B8%AD%E6%96%87wiki
(use-package neotree
  :bind (("C-c =" . neotree-show)
	 ("C-c -" . neotree-hide)
	 ("C-c 0" . neotree-dir)))

;; 自动补全 https://www.emacswiki.org/emacs/CompanyMode
(use-package company
  :demand
  :bind (("C-c p" . company-complete-common))
  :config
  (setq company-idle-delay       0     ;; 延迟补全时间
	company-mode             t
	company-dabbrev-downcase nil)  ;; 补全区分大小写
  (add-hook 'after-init-hook 'global-company-mode)
  )

(use-package markdown-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
  (add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
  (add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
  )

(use-package youdao-dictionary
  :bind (("C-c SPC" . youdao-dictionary-search-at-point+)
	 ("C-c C-SPC" . youdao-dictionary-search-from-input))
  :config
  (setq url-automatic-caching t
	youdao-dictionary-search-history-file "~/.emacs.d/.youdao")
  )

;; 用法 https://rgel.readthedocs.io/en/2.0.3/usage.html#searching
;; C-c s r (rg)
;; C-c s t (rg-literal)
;; C-c s p (rg-project)
;; M-n / M-p Move to next/prev line with a match
;; n / p Move to next/prev line with a match, show that file in other buffer
;; M-N / M-P rg-next-file / rg-prev-file
(use-package rg
  :demand
  :config
  (rg-enable-default-bindings))

(use-package atom-one-dark-theme
  :demand
  :config
  (load-theme 'atom-one-dark t))

;; 结构化括号编辑器
;; (use-package paredit
;;   :load-path "~/.emacs.d/plugins/"
;;   :config
;;   (paredit-mode t))

;; 让括号变得不显眼
(use-package parenface
  :load-path "~/.emacs.d/plugins"
  :config
  (set-face-foreground 'paren-face "DimGray"))

;; 模糊搜索
(use-package snails
  :load-path "~/.emacs.d/plugins/snails/"
  :bind ("M-p" . snails)
  :config (setq snails-show-with-frame t))

;; https://github.com/manateelazycat/awesome-tab
(use-package awesome-tab
  :demand
  :load-path "~/.emacs.d/plugins/awesome-tab"
  :bind (("M-k" . awesome-tab-forward-tab)
	 ("M-j" . awesome-tab-backward-tab))
  :config (awesome-tab-mode t))

(defun hideshow-folded-overlay-fn (ov)
  (when (eq 'code (overlay-get ov 'hs))
    (let* ((nlines (count-lines (overlay-start ov) (overlay-end ov)))
	   (info (format " ... #%d " nlines)))
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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 未使用 use-package 的插件以及部分函数
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
(global-set-key [f10] 'indent-whole)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 使用 M-x align 进行缩进
;; Align-region 规则
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 自动生成的东西
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

