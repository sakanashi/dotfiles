;; packages http://catcher-in-the-tech.net/55/
;;(require 'package)
;; (add-to-list 'package-archives
;; 	     '("melpa" . "http://melpa.milkbox.net/packages/"))
;; (add-to-list 'package-archives
;; 	     '("marmalade" . "http://marmalade-repo.org/packages/"))
(package-initialize)
(require 'cask "~/.cask/cask.el")
(cask-initialize)

;; color theme
;(add-to-list 'load-path "./color-theme-6.6.0")
;(require 'color-theme)
;;(eval-after-load "color-theme"
 ;; '(progn
   ;;  (color-theme-initialize)
    ;; (color-theme-solarized))

;(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/solarized/emacs-color-solarized")
;(load-theme 'emacs-colors-solarized.el t)

;;(require 'color-theme-solarized)
;;(color-theme-solarized)
;;(require 'zenburn)


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
(add-to-list 'auto-mode-alist '("Jenkinsfile$" . groovy-mode))
