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
| | |

;; C-x (        开启宏记录
;; C-x )        关闭宏记录
;; C-x e        执行刚刚录制的宏
;; C-u n C-x e  循环执行n次刚刚录制的宏
;; C-u n C-x e  循环执行n次刚刚录制的宏
;; C-u n C-x e  循环执行n次刚刚录制的宏

;; Occur-mode
;; M-s o 或者 M-x occur 进入
;; 在 Occur buffer 中按 C-c C-f 开启 next-error-follow-mirror-mode
;; 在 Occur buffer 中使用 M-p/M-n 切换上一个/下一个匹配项目
;; 配置在 Occur Buffer 中展示匹配项的前后 n 行
;; (setq list-matching-lines-default-context-lines n)

;; Usage package 文档
;; https://phenix3443.github.io/notebook/emacs/modes/use-package-manual.html



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


;; 不移动当前光标的情况下把屏幕上卷/下卷几行
(defun my-scroll-up-lines()
  "Scroll up 4 lines."
  (interactive)
  (read-only-mode 1)
  (scroll-up 4)
  (read-only-mode 0))

(defun my-scroll-down-lines()
  "Scroll down 4 lines."
  (interactive)
  (read-only-mode 1)
  (scroll-down 4)
  (read-only-mode 0))

;; (global-set-key (kbd "C-c 9") 'my-scroll-down-lines)
;; (global-set-key (kbd "C-c 0") 'my-scroll-up-lines)


;; 位置跳转和记录
(defvar point-stack nil
  "The stack position.")

(defun my-point-stack-clear ()
  "Clear point stack."
  (interactive)
  (setq point-stack nil)
  (message "Clear point stack, now stack size: %d" (length point-stack)))

(defun my-point-stack-push ()
  "Push current point in stack."
  (interactive)
  (setq point-stack (cons (list (current-buffer) (point)) point-stack))
  (message "Location marked, now stack size: %d" (length point-stack)))

(defun my-point-stack-pop ()
  "Pop point from stack."
  (interactive)
  (if (null point-stack)
      (message "Stack is empty.")
    (switch-to-buffer (caar point-stack))
    (goto-char (cadar point-stack))
    (setq point-stack (cdr point-stack))
    (message "Location poped, now stack size: %d" (length point-stack))))

(global-set-key (kbd "C-c p") 'my-point-stack-push)
(global-set-key (kbd "C-c [") 'my-point-stack-pop)
(global-set-key (kbd "C-c ]") 'my-point-stack-clear)


(defun my-query-and-replace ()
  "查找项目内所有关键字并进行替换"
  (interactive)
  ;; 查找关键字
  (call-interactively 'deadgrep)
  ;; 循环直到 deadgrep 查询完毕
  (while buffer-read-only
    (condition-case err
        (progn
          (message "deadgrep is still searching, waiting...")
          (sit-for 0.2)
          ;; 使 deadgrep buffer 可以编辑
          (call-interactively 'deadgrep-edit-mode))
      ;; 当 deadgrep 还没有查找完成时调用 deadgrep-edit-mode
      ;; 会触发 user-error, 这里进行忽略
      (user-error
       (message "deadgrep user-error occur, ignore...")
       (ignore))))
  ;; 对 deadgrep buffer 进行替换
  (call-interactively 'query-replace))


;; 窗口跳转
(use-package windmove
  :defer t
  :bind (("M-s <up>"    . windmove-up)
	 ("M-s <down>"  . windmove-down)
	 ("M-s <left>"  . windmove-left)
	 ("M-s <right>" . windmove-right))
  ;; 在边缘的窗口进行循环跳转，最左窗口跳到最右窗口等
  :config (setq windmove-wrap-around t))


(use-package awesome-tab
  :demand t
  :load-path "~/.emacs.d/plugins/awesome-tab"
  :bind (("M-h" . awesome-tab-ace-jump)
	 ("M-j" . awesome-tab-backward-tab)
	 ("M-k" . awesome-tab-forward-tab))
  :config
  (awesome-tab-mode t)
  (setq awesome-tab-terminal-dark-select-foreground-color    "#FFFAFA")
  (setq awesome-tab-terminal-dark-select-background-color    "#708090")
  (setq awesome-tab-terminal-dark-unselect-background-color  "#1C1C1C")
  (setq awesome-tab-terminal-dark-unselect-foreground-color  "#FFFAFA"))


;; 对齐线 某些系统的GUI和终端(如 Manjaro) 下会导致分割的窗口排版混乱
(use-package indent-bars
  :load-path "~/.emacs.d/plugins/indent-bars"
  :custom
  (indent-bars-treesit-support t)
  (indent-bars-no-descend-string t)
  (indent-bars-treesit-ignore-blank-lines-types '("module"))
  (indent-bars-treesit-wrap '((python argument_list parameters ; for python, as an example
                                      list list_comprehension
                                      dictionary dictionary_comprehension
                                      parenthesized_expression subscript)))
  :hook ((python-mode yaml-mode c-mode c++-mode rust-mode go-mode lua-mode ocaml-mode)
         . indent-bars-mode)
  :config
  (setq indent-bars-color  '("DimGray" :face-bg t :blend 0.6)) ;; 设置颜色
  (setq indent-bars-color-by-depth nil)) ;; 不按照嵌套深度改变颜色


;; 让括号变得不显眼
(use-package parenface
  :load-path "~/.emacs.d/plugins/parenface"
  :config (set-face-foreground 'paren-face "#909595"))


;; TODO: remove this hack when bug fixed: https://github.com/PowerShell/Win32-OpenSSH/issues/1842
;; Add this to your Windows Terminal settings.json
;; {
;;   "command":
;;   { "action": "sendInput",
;;     "input": "\u001b[9~"
;;   },
;;   "keys": "ctrl+space"
;; }

```
