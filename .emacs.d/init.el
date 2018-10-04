(package-initialize)
(require 'cask "~/.cask/cask.el")
(cask-initialize)

;; eaw
(add-to-list 'load-path "~/.emacs.d/site-lisp")
(require 'eaw)
(eaw-fullwidth)

;; anzu
(when (require 'anzu)
  (global-anzu-mode t)
  (setq anzu-search-threshold 1000)
  (global-set-key (kbd "M-%") 'anzu-query-replace)
  (global-set-key (kbd "C-M-%") 'anzu-query-replace-regexp)
  )

;; avy
(global-set-key (kbd "M-j") 'avy-goto-word-1)
(global-set-key (kbd "C-M-j") 'avy-goto-char)

;; line-number
(when (version<= "26.0.50" emacs-version )
  (global-display-line-numbers-mode))
(when (version<= emacs-version "26.0.50" )
  (global-linum-mode t)
  (set-face-attribute 'linum nil
                      :foreground "eee8d5"
                      :height 0.9)
  (setq linum-format "%4d| ")
  )

;; line hilight
(global-hl-line-mode t)
(custom-set-faces
'(hl-line ((t (:background "color-238"))))
)

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

;; company-mode https://qiita.com/kod314/items/9a56983f0d70f57420b1
(with-eval-after-load 'company
  (global-company-mode)
  (setq company-auto-expand t) ;; auto expand the first one
  (setq company-transformers '(company-sort-by-backend-importance)) ;; sort
  (setq company-idle-delay 0)
  (setq company-minimum-prefix-length 2)
  (setq company-selection-wrap-around t) 
  (setq completion-ignore-case t)
  (setq company-dabbrev-downcase nil)
  (global-set-key (kbd "C-M-i") 'company-complete)
  (define-key company-active-map (kbd "C-n") 'company-select-next)
  (define-key company-active-map (kbd "C-p") 'company-select-previous)
  (define-key company-active-map [tab] 'company-complete-selection)
  (define-key company-active-map (kbd "C-h") nil)
  (define-key company-active-map (kbd "C-S-h") 'company-show-doc-buffer)

  ; unselected items
  (set-face-attribute 'company-tooltip nil
                      :foreground "black" :background "lightgrey")
  ; unselected items and match
  (set-face-attribute 'company-tooltip-common nil
                      :foreground "black" :background "lightgrey")
  ;; selected item
  (set-face-attribute 'company-tooltip-selection nil
                      :foreground "white" :background "steelblue")
  ;; selected items and match
  (set-face-attribute 'company-tooltip-common-selection nil
                      :foreground "lightblue" :background "steelblue")
  ;; (set-face-attribute 'company-preview-common nil
  ;;                   :background nil :foreground "lightgrey" :underline t)
  (set-face-attribute 'company-scrollbar-fg nil
                      :background "orange")
  (set-face-attribute 'company-scrollbar-bg nil
                      :background "gray40")
  )

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

;; insert a line break at the end of the file
(setq require-final-newline t)

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
(add-to-list 'auto-mode-alist '("\\.as[cp]x$"   . web-mode))
(add-to-list 'auto-mode-alist '("\\.css?\\'"    . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb$"       . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'"     . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?$"     . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'"   . web-mode))
(add-to-list 'auto-mode-alist '("\\.js?\\'"     . web-mode))
(add-to-list 'auto-mode-alist '("\\.jsp$"       . web-mode))
(add-to-list 'auto-mode-alist '("\\.jsx\\'"     . web-mode))
(add-to-list 'auto-mode-alist '("\\.phtml$"     . web-mode))
(add-to-list 'auto-mode-alist '("\\.scss?\\'"   . web-mode))
(add-to-list 'auto-mode-alist '("\\.sass?\\'"   . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php$" . web-mode))

;;; インデント数
(add-hook 'web-mode-hook 'web-mode-hook)
(defun web-mode-hook ()
    "Hooks for Web mode."
    (setq web-mode-asp-offset           2)
    (setq web-mode-code-indent-offset   2)
    (setq web-mode-css-indent-offset    2)
    (setq web-mode-css-offset           2)
    (setq web-mode-html-offset          2)
    (setq web-mode-java-offset          2)
    (setq web-mode-markup-indent-offset 2)
    (setq web-mode-php-offset           2)
    (setq web-mode-script-offset        2))

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

;; robe
;; (autoload 'robe-mode "robe" "Code navigation, documentation lookup and completion for Ruby" t nil)
;; (autoload 'robe-ac-setup "robe-ac" "robe auto-complete" nil nil)
;; (add-hook 'robe-mode-hook 'robe-ac-setup)

;; do endなどの補完
;; (require 'ruby-electric)
;; (add-hook 'ruby-mode-hook '(lambda ()
;;           (ruby-electric-mode t)))
;(setq ruby-electric-expand-delimiters-list nil)

;; 補完機能
;; robe-modeの有効化とcompanyとの連携
(add-hook 'ruby-mode-hook 'robe-mode)
(autoload 'robe-mode "robe" "Code navigation, documentation lookup and completion for Ruby" t nil)
(eval-after-load 'company
  '(push 'company-robe company-backends))

(add-hook 'ruby-mode-hook (lambda()
      (company-mode)
      (setq company-auto-expand t)
      (setq company-transformers '(company-sort-by-backend-importance)) ;; ソート順
      (setq company-idle-delay 0) ; 遅延なしにすぐ表示
      (setq company-minimum-prefix-length 1) ; 何文字打つと補完動作を行うか設定
      (setq company-selection-wrap-around t) ; 候補の最後の次は先頭に戻る
      (setq completion-ignore-case t)
      (setq company-dabbrev-downcase nil)
      (global-set-key (kbd "C-M-i") 'company-complete)
      ;; C-n, C-pで補完候補を次/前の候補を選択
      (define-key company-active-map (kbd "C-n") 'company-select-next)
      (define-key company-active-map (kbd "C-p") 'company-select-previous)
      (define-key company-active-map (kbd "C-s") 'company-filter-candidates) ;; C-sで絞り込む
      (define-key company-active-map [tab] 'company-complete-selection) ;; TABで候補を設定
      (define-key emacs-lisp-mode-map (kbd "C-M-i") 'company-complete) ;; 各種メジャーモードでも C-M-iで company-modeの補完を使う
      ))

;; git-gutter
(require 'git-gutter)
(global-git-gutter-mode t)
;(git-gutter:linum-setup)

;; fly-check
;; flycheck
(require 'flycheck)
(setq flycheck-check-syntax-automatically '(mode-enabled save))
(add-hook 'ruby-mode-hook 'flycheck-mode)
;; (use-package flycheck
;;     :ensure t
;;       :init (global-flycheck-mode))
;(add-hook 'after-init-hook #'global-flycheck-mode)

;; ruby-block.el --- highlight matching block
;(require 'ruby-block)
;(ruby-block-mode t)
;(setq ruby-block-highlight-toggle t)
;; highlight end
(require 'ruby-end)

;;; theme ;;;
;(set-face-background 'linum "#2f2f2f")
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
