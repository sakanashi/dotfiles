(package-initialize)
(require 'cask "~/.cask/cask.el")
;(require 'cask)
(cask-initialize)

;; anzu
(global-anzu-mode t)
(setq anzu-search-threshold 1000)
(global-set-key (kbd "M-%") 'anzu-query-replace)
(global-set-key (kbd "C-M-%") 'anzu-query-replace-regexp)

;; バッファ内の表示
;;行番号を常に表示
(global-linum-mode t)
;; 行番号の見た目
(set-face-attribute 'linum nil
                    :foreground "eee8d5"
                    :height 0.9)
;; 行番号の後ろにスペース
(setq linum-format "%4d| ")
;; 現在行と桁をハイライト
;;(require 'crosshairs)
;;(crosshairs-mode 1)
;; 対応する括弧を表示
(show-paren-mode t)
;; ミニバッファの表示
;; directory 名を追加
(require 'uniquify)
;; filename<dir> 形式のバッファ名にする
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)
;; バッファの操作に関する設定
(setq x-select-enable-clipboard t)

;; "C-t" でウインドウを切り替える
(define-key global-map (kbd "C-t") 'other-window)
;; C-x 3 で自動的にfollow-modeにする
(global-set-key "\C-x3" 'follow-delete-other-windows-and-split)

(global-set-key "\C-m" 'newline-and-indent)
(electric-pair-mode 1)

;; ==================================================
;; Indent
;; ==================================================
(setq c-default-style '((java-mode . "java") (other . "linux")))
;(setq-default tab-width 2 indent-tabs-mode nil)
;(setq c-basic-offset 2)

;; インデントでタブを使わない
(setq-default indent-tabs-mode nil)

;; タブ, 全角スペース表示
(setq whitespace-style
                  '(tabs tab-mark spaces space-mark))
(setq whitespace-space-regexp "\\(\x3000+\\)")
(setq whitespace-display-mappings
      '((space-mark ?\x3000 [?\□]
                    )(tab-mark   ?\t   [?\xBB ?\t])
                     ))
(require 'whitespace)
(global-whitespace-mode 1)
(set-face-foreground 'whitespace-space "LightSlateGray")
(set-face-background 'whitespace-space "DarkSlateGray")
(set-face-foreground 'whitespace-tab "LightSlateGray")
(set-face-background 'whitespace-tab "DarkSlateGray")

;; 行末空白表示
(defface my-face '((t (:background "DarkSlateGray"))) nil)
(defvar my-face 'my-face)
(defadvice font-lock-mode (before my-font-lock-mode ())
     (font-lock-add-keywords
           major-mode
                 '(("[ \t]+$" 0 my-face append))))
(ad-enable-advice 'font-lock-mode 'before 'my-font-lock-mode)
(ad-activate 'font-lock-mode)

;; ==================================================
;; language
;; ==================================================
(set-language-environment 'Japanese)(prefer-coding-system 'utf-8)

(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-buffer-file-coding-system 'utf-8)
;; (setq default-buffer-file-coding-system 'utf-8)
(setq buffer-file-coding-system 'utf-8)


;;(require 'undo-tree)
;;(global-undo-tree-mode t)
;;(global-set-key (kbd "C-x C-u") 'undo-tree-redo)


;;(require 'web-mode)
;;(add-to-list 'auto-mode-alist '("\\.jsx\\'" . web-mode))


;(add-to-list 'load-path "~/.emacs.d/lisp/")
;(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.ya?ml$" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.liquid$" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.dig$" . yaml-mode))
(add-hook 'yaml-mode-hook
          '(lambda () (define-key yaml-mode-map "\C-m" 'newline-and-indent)))
(add-to-list 'auto-mode-alist '("Jenkinsfile" . groovy-mode))
(add-to-list 'auto-mode-alist '("build.gradle". groovy-mode))

;(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))

; ==============
; ruby indent setting (https://qiita.com/hiconyan/items/582e27128decfe9d249e)
; なんか動かないので保留
; =============
;; (autoload 'ruby-mode "ruby-mode")
;; (autoload 'ruby-electric-mode "ruby-electric")
;; (global-set-key (kbd "C-c r b") 'ruby-mode)
;; (add-to-list 'auto-mode-alist '("\\.rb$" ruby-mode))
;; (setq ruby-deep-indent-paren-style nil)
;; (add-to-list 'interpreter-mode-alist '("ruby" . ruby-mode))
;; (add-hook-fn 'ruby-mode-hook
;;              (ruby-electric-mode)
;;              (setq ruby-indent-level 2)
;;              (define-key ruby-mode-map "\C-m" 'ruby-reindent-then-newline-and-indent))
;; (defadvice ruby-indent-line (after unindent-closing-paren activate)
;;   (let ((column (current-column))
;;         indent offset)
;;     (save-excursion
;;       (back-to-indentation)
;;       (let ((state (syntax-ppss)))
;;         (setq offset (- column (current-column)))
;;         (when (and (eq (char-after) ?\))
;;                    (not (zerop (car state))))
;;           (goto-char (cadr state))
;;           (setq indent (current-indentation)))))
;;     (when indent
;;       (indent-line-to indent)
;;       (when (> offset 0) (forward-char offset)))))

;; fly-check
(autoload 'flycheck-mode "flycheck")
(add-hook 'ruby-mode-hook 'flycheck-mode)
(setq flycheck-check-syntax-automatically '(idle-change mode-enabled new-line save))

;; ruby-block.el --- highlight matching block
(require 'ruby-block)
(ruby-block-mode t)
(setq ruby-block-highlight-toggle t)
;; highlight end
(require 'ruby-end)

;;; theme ;;;
(set-face-background 'linum "#2f2f2f")
(custom-set-variables
 )
(custom-set-faces
 '(default ((t (:inherit nil :stipple nil :background "#000000" :foreground "brightwhite" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 1 :width normal :foundry "default" :family "default"))))
 '(font-lock-comment-face ((t (:foreground "cyan" :slant italic))))
 '(font-lock-string-face ((t (:foreground "green"))))
 '(font-lock-variable-name-face ((t (:foreground "yellow")))))
