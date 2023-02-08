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



```
