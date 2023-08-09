## flycheck key binding




| Key		 | Binding								|
| -----		 | ---------							|
| C-c ! ?	 | Describe a Flycheck Checker			|
| C-c ! C-c	 | Compile using checker				|
| C-c ! C-w	 | Copy error point is on to kill ring	|
| C-c ! C	 | Clear all highlights from buffer		|
| C-c ! V	 | Report Flycheck version				|
| C-c ! c	 | Start syntax checking current buffer |
| C-c ! e	 | Change Flycheck executable			|
| C-c ! i	 | Open Flycheck info manual			|
| C-c ! l	 | List all Flycheck errors				|
| C-c ! n	 | Jump to next error					|
| C-c ! p	 | Jump to previous error				|
| C-c ! s	 | Change Flycheck checker				|
| C-c ! v	 | Verifies the Flycheck checker works	|
| C-c ! x	 | Disable Flycheck checker in buffer	|




## 给 clangd 生成项目配置文件的工具

compile_commands.json
https://zhuanlan.zhihu.com/p/145430576
https://github.com/rizsotto/Bear


## 不常用功能

```lisp

(use-package nov
  :config
  (add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode)))

;; 80 列显示标记
(if (not (version< emacs-version "27.1"))
    (progn
      (setq-default display-fill-column-indicator-column 81)
      (global-display-fill-column-indicator-mode 0))) ;; 默认不开启

;; 80 列显示标记
(use-package hl-fill-column
  :load-path "~/.emacs.d/plugins/hl-fill-column/"
  :demand t
  :config
  (setq-default fill-column 40)
  (global-hl-fill-column-mode 1)
  )
;; 设置face
'(hl-fill-column-face ((t (:background "DimGray"))))

;; 统计 Emacs 启动时间，放在文件开头
(add-hook 'emacs-startup-hook
	  (lambda ()
	    (message "Emacs ready in %s with %d garbage collections."
		     (format "%.2f seconds"
			     (float-time
			      (time-subtract after-init-time before-init-time)))
		     gcs-done)))


;; GUI 下标题栏显示文件名
(setq frame-title-format
      '(buffer-file-name "%f" (dired-directory dired-directory "%b")))


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


;; 高亮光标下的符号并前后跳转
(use-package highlight-symbol
  :bind(("C-<f3>" . highlight-symbol)
	    ("<f3>" . highlight-symbol-next)
        ("<f4>" . highlight-symbol-prev)))



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
  (add-hook 'haskell-mode-hook 'dante-mode))


;; 自动清除行尾空格
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(add-hook 'before-save-hook 'whitespace-cleanup)


;; 打开最近文件快捷键
(global-set-key "\C-x\ \C-r" 'recentf-open-files)



;; 默认开启折行
(global-visual-line-mode t)


;; 退出 Emacs 时保存工程状态
(desktop-save-mode -1)

;; 修改光标所在括号内的块的背景颜色
(use-package highlight-blocks
  :config
  (setq highlight-blocks-delay 0.05
	;; 设置背景颜色，第一个是显示的颜色
	highlight-blocks--rainbow-colors '("#606060" "#000000" "#464641")
	highlight-blocks-max-face-count (length highlight-blocks--rainbow-colors)))

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
(add-hook 'scheme-mode-hook           'highlight-blocks-mode)


;; ivy 单独使用时候的配置
(use-package ivy
  :init (ivy-mode 1)
  :config
  ;; Add recent files and bookmarks to the ivy-switch-buffer
  ;; (setq ivy-use-virtual-buffers t)
  (setq ivy-height 20)
  (setq ivy-count-format "%d/%d "))

(custom-set-faces
 '(ivy-current-match ((t (:background "Dimgray"))))
 '(ivy-minibuffer-match-face-2 ((t (:foreground "Blue")))))

```
