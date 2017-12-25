(package-initialize)
(require 'cask "~/.cask/cask.el")
(cask-initialize)

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

;; --------------------------------------------------
;; ruby-block
;; endにカーソルを合わせると、そのendに対応する行をハイライトする
;; --------------------------------------------------
(require 'ruby-block)
(ruby-block-mode t)
(setq ruby-block-highlight-toggle t)

;; ==================================================
;; language
;; ==================================================
(set-language-environment 'Japanese)
(prefer-coding-system 'utf-8)

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
