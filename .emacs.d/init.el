(package-initialize)
(require 'cask "~/.cask/cask.el")
;(require 'cask)
(cask-initialize)

;; eaw
(add-to-list 'load-path "~/.emacs.d/site-lisp")
(require 'eaw)
(eaw-fullwidth)

;; ;; anzu
;; (global-anzu-mode t)
;; (setq anzu-search-threshold 1000)
;; (global-set-key (kbd "M-%") 'anzu-query-replace)
;; (global-set-key (kbd "C-M-%") 'anzu-query-replace-regexp)

;; avy
(global-set-key (kbd "M-j") 'avy-goto-word-1)
(global-set-key (kbd "C-M-j") 'avy-goto-char)

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

;; dumb-jump
(require 'dumb-jump)
(use-package dumb-jump
  :bind (("M-g o" . dumb-jump-go-other-window)
         ("M-g j" . dumb-jump-go)
         ("M-g i" . dumb-jump-go-prompt)
         ("M-g x" . dumb-jump-go-prefer-external)
         ("M-g z" . dumb-jump-go-prefer-external-other-window))
  :config (setq dumb-jump-selector 'ivy) ;; (setq dumb-jump-selector 'helm)
  :ensure)
;; ;(setq dumb-jump-mode t)
;; ;(setq dumb-jump-selector 'ivy) ;; 候補選択をivyに任せます
;; ;; (setq dumb-jump-use-visible-window nil)
;; ;; (define-key global-map [(super d)] 'dumb-jump-go) ;; go-to-definition!
;; ;; (define-key global-map [(super shift d)] 'dumb-jump-back)
;; ;; これをしないとホームディレクトリ以下が検索対象になる
;; (setq dumb-jump-default-project "")
;; ;; 日本語を含むパスだとgit grepがちゃんと動かない…
;; ;(setq dumb-jump-force-searcher 'rg)
;; ;; 標準キーバインドを有効にする
;; (dumb-jump-mode)

;; ==================================================
;; Indent
;; ==================================================
(setq c-default-style '((java-mode . "java") (other . "linux")))
;(setq-default tab-width 2 indent-tabs-mode nil)
;(setq c-basic-offset 2)

;; インデントでタブを使わない
(setq-default indent-tabs-mode nil)

;; タブ, 全角スペース表示
(require 'whitespace)
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

(require 'web-mode)
;;; 適用する拡張子
(add-to-list 'auto-mode-alist '("\\.phtml$"     . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jsp$"       . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x$"   . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb$"       . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?$"     . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'"     . web-mode))

;;; インデント数
(add-hook 'web-mode-hook 'web-mode-hook)
(defun web-mode-hook ()
    "Hooks for Web mode."
    (setq web-mode-html-offset   2)
    (setq web-mode-css-offset    2)
    (setq web-mode-script-offset 2)
    (setq web-mode-php-offset    2)
    (setq web-mode-java-offset   2)
    (setq web-mode-asp-offset    2))
(add-to-list 'auto-mode-alist '("\\.jsx\\'" . web-mode))


;;(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.ya?ml$" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.liquid$" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.dig$" . yaml-mode))
(add-hook 'yaml-mode-hook
          '(lambda () (define-key yaml-mode-map "\C-m" 'newline-and-indent)))
(add-to-list 'auto-mode-alist '("Jenkinsfile" . groovy-mode))
(add-to-list 'auto-mode-alist '("build.gradle". groovy-mode))


;; ;; helm
;; (require 'helm-config)
;; (helm-mode 1)

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
;; flycheck
(use-package flycheck
    :ensure t
      :init (global-flycheck-mode))
;(add-hook 'after-init-hook #'global-flycheck-mode)

;;(add-hook 'after-init-hook 'global-flycheck-mode)
;; (autoload 'flycheck-mode "flycheck")
;; (add-hook 'ruby-mode-hook 'flycheck-mode)
;; (setq flycheck-check-syntax-automatically '(idle-change mode-enabled new-line save))

;; ruby-block.el --- highlight matching block
(require 'ruby-block)
(ruby-block-mode t)
(setq ruby-block-highlight-toggle t)
;; highlight end
(require 'ruby-end)

;;; theme ;;;
(set-face-background 'linum "#2f2f2f")
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "#000000" :foreground "brightwhite" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 1 :width normal :foundry "default" :family "default"))))
 '(font-lock-comment-face ((t (:foreground "color-144" :slant italic))))
 '(font-lock-string-face ((t (:foreground "green"))))
 '(font-lock-variable-name-face ((t (:foreground "yellow")))))
