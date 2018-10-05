(package-initialize)
(require 'cask "~/.cask/cask.el")
(cask-initialize)

;; ==================================================
;; Language
;; ==================================================
(set-language-environment 'Japanese)(prefer-coding-system 'utf-8)

(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-buffer-file-coding-system 'utf-8)
;; (setq default-buffer-file-coding-system 'utf-8)
(setq buffer-file-coding-system 'utf-8)

;; East Asian Ambiguous Width
(add-to-list 'load-path "~/.emacs.d/site-lisp")
(require 'eaw)
(eaw-fullwidth)

;; ==================================================
;; Looks
;; ==================================================
;; line-number
(when (version<= "26.0.50" emacs-version )
  (global-display-line-numbers-mode)
  )
(when (version<= emacs-version "26.0.50" )
  (global-linum-mode t)
  (set-face-attribute 'linum nil
                      :foreground "eee8d5"
                      :height 0.9)
  (setq linum-format "%4d| ")
  )

;; line hilight
(global-hl-line-mode t)

;; 対応する括弧を表示
(show-paren-mode t)
;; ミニバッファの表示
;; directory 名を追加
(require 'uniquify)
;; filename<dir> 形式のバッファ名にする
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)
;; バッファの操作に関する設定
(setq x-select-enable-clipboard t)
;; C-x 3 で自動的にfollow-modeにする
(global-set-key "\C-x3" 'follow-delete-other-windows-and-split)
;; auto insert pair bracket
(electric-pair-mode 1)

;; rainbow-delimiters
(require 'rainbow-delimiters)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

;; customize mode line
(setq sml/theme 'respectful)
(setq sml/no-confirm-load-theme t)
(sml/setup)
(set-face-background 'mode-line "color-236")

;; ==================================================
;; Indent
;; ==================================================
(setq c-default-style '((java-mode . "java") (other . "linux")))

;; not use tab for indent
(setq-default indent-tabs-mode nil)

;; ==================================================
;; which-key
;; ==================================================
(require 'which-key)
(which-key-setup-side-window-bottom)        ; miniBuffer
;; (which-key-setup-side-window-right)         ; right
;; (which-key-setup-side-window-right-bottom)  ; both
(which-key-mode 1)
(setq which-key-idle-delay 0.5)

;; =================================================
;; color with spaces and tabs
;; =================================================
(require 'whitespace)
(setq whitespace-style '(face           ; visualize
                         trailing       ; end of line
                         tabs           ; tab
                         spaces         ; spaces
                         empty          ; empty line with first/last
                         space-mark     ; mapping
                         tab-mark
                         ))

(setq whitespace-display-mappings
      '( ;(space-mark ?\u3000 [?\u25a1])
        (tab-mark ?\t [?\u00BB ?\t] [?\\ ?\t])))

;; show only double-byte spaces
(setq whitespace-space-regexp "\\(\u3000+\\)")

;; auto cleanup before save
(setq whitespace-action '(auto-cleanup))

(global-whitespace-mode 1)

(defvar my/bg-color "#4f4f4f")
(set-face-attribute 'whitespace-trailing nil
                    :background my/bg-color
                    :foreground "DeepPink"
                    :underline t)
(set-face-attribute 'whitespace-tab nil
                    :background my/bg-color
                    :foreground "LightSkyBlue"
                    :underline t)
(set-face-attribute 'whitespace-space nil
                    :background my/bg-color
                    :foreground "GreenYellow"
                    :weight 'bold)
(set-face-attribute 'whitespace-empty nil
                    :background my/bg-color)


;; insert a line break at the end of the file
(setq require-final-newline t)

;; ==============================================
;; Search / Jump
;; ==============================================
;; anzu
(when (require 'anzu)
  (global-anzu-mode t)
  (setq anzu-mode-lighter "")
  (setq anzu-deactivate-region t)
  (setq anzu-search-threshold 1000)
  (global-set-key (kbd "M-%") 'anzu-query-replace)
  (global-set-key (kbd "C-M-%") 'anzu-query-replace-regexp)
  (set-face-attribute 'anzu-mode-line nil
                      :foreground "yellow" :weight 'bold)
  (define-key isearch-mode-map [remap isearch-query-replace]  #'anzu-isearch-query-replace)
  (define-key isearch-mode-map [remap isearch-query-replace-regexp] #'anzu-isearch-query-replace-regexp)
  )

;; avy
(global-set-key (kbd "M-j") 'avy-goto-word-1)
(global-set-key (kbd "C-M-j") 'avy-goto-char)

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

;;==============================================
;; Search / Jump (helm)
;;==============================================
(require 'ag)
(require 'helm-config)
(helm-mode 1)
(require 'helm-files)
(require 'helm-ag)
(defun helm-ag-project-root ()
  (interactive)
  (let ((rootdir (helm-ag--project-root)))
    (unless rootdir
      (error "Could not find the project root. Create a git, hg, or svn repository there first. "))
    (helm-ag rootdir)))

(defun helm-ag--project-root ()
  (cl-loop for dir in '(".git/" ".hg/" ".svn/" ".git")
           when (locate-dominating-file default-directory dir)
           return it))
(global-set-key (kbd "M-s /") 'helm-ag-project-root)

;; ==============================================
;; company-mode
;; ==============================================
(require 'company)
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
                      :foreground "black" :background "steelblue")
  ;; selected items and match
  (set-face-attribute 'company-tooltip-common-selection nil
                      :foreground "white" :background "steelblue")
  (set-face-attribute 'company-preview-common nil
                    :background nil :foreground "lightgrey" :underline t)
  (set-face-attribute 'company-scrollbar-fg nil
                      :background "orange")
  (set-face-attribute 'company-scrollbar-bg nil
                      :background "gray40")

;; (require 'company-web-html)                          ; load company mode html backend
;; ;; and/or
;; (require 'company-web-jade)                          ; load company mode jade backend
;; (require 'company-web-slim)                          ; load company mode slim backend
;; ;; (setq company-minimum-prefix-length 0)            ; WARNING, probably you will get perfomance issue if min len is 0!
;; (setq company-tooltip-limit 20)                      ; bigger popup window
;; (setq company-tooltip-align-annotations 't)          ; align annotations to the right tooltip border
;; (setq company-idle-delay .3)                         ; decrease delay before autocompletion popup shows
;; (setq company-begin-commands '(self-insert-command)) ; start autocompletion only after typing
;; (global-set-key (kbd "C-c /") 'company-files)        ; Force complete file names on "C-c /" key
;; (defun my-web-mode-hook ()
;;   "Hook for `web-mode'."
;;     (set (make-local-variable 'company-backends)
;;          '(company-tern company-web-html company-yasnippet company-files)))

;; (add-hook 'web-mode-hook 'my-web-mode-hook)

;; ;; Enable JavaScript completion between <script>...</script> etc.
;; (advice-add 'company-tern :before
;;             #'(lambda (&rest _)
;;                 (if (equal major-mode 'web-mode)
;;                     (let ((web-mode-cur-language
;;                           (web-mode-language-at-pos)))
;;                       (if (or (string= web-mode-cur-language "javascript")
;;                               (string= web-mode-cur-language "jsx"))
;;                           (unless tern-mode (tern-mode))
;;                         (if tern-mode (tern-mode -1)))))))

;; ==================================================
;; undo
;; ==================================================
(require 'undo-tree)
(global-undo-tree-mode t)
(global-set-key (kbd "M-/") 'undo-tree-redo)

;; ==================================================
;; Programming
;; ==================================================
;; ----- Web-mode -----
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

(eval-after-load "web-mode"
  (add-hook 'web-mode-hook
            '(lambda ()
               (setq web-mode-asp-offset           2)
               (setq web-mode-code-indent-offset   2)
               (setq web-mode-css-indent-offset    2)
               (setq web-mode-css-offset           2)
               (setq web-mode-html-offset          2)
               (setq web-mode-java-offset          2)
               (setq web-mode-markup-indent-offset 2)
               (setq web-mode-php-offset           2)
               (setq web-mode-script-offset        2)

               (setq web-mode-auto-close-style 2)
               (setq web-mode-enable-auto-closing t)
               (setq web-mode-enable-auto-pairing t)
               (setq web-mode-enable-auto-quoting t)
               (setq web-mode-enable-current-column-highlight t)
               (setq web-mode-enable-current-element-highlight t)
               (setq web-mode-tag-auto-close-style 2)
               ))
  )

(custom-set-faces
 '(web-mode-doctype-face           ((t (:foreground "#4A8ACA"))))
 '(web-mode-html-tag-face          ((t (:foreground "#4A8ACA"))))
 '(web-mode-html-tag-bracket-face  ((t (:foreground "#4A8ACA"))))
 '(web-mode-html-attr-name-face    ((t (:foreground "#87CEEB"))))
 '(web-mode-html-attr-equal-face   ((t (:foreground "#FFFFFF"))))
 '(web-mode-html-attr-value-face   ((t (:foreground "#D78181"))))
 '(web-mode-comment-face           ((t (:foreground "#587F35"))))
 '(web-mode-server-comment-face    ((t (:foreground "#587F35"))))

 '(web-mode-css-at-rule-face       ((t (:foreground "#DFCF44"))))
 '(web-mode-comment-face           ((t (:foreground "#587F35"))))
 '(web-mode-css-selector-face      ((t (:foreground "#DFCF44"))))
 '(web-mode-css-pseudo-class       ((t (:foreground "#DFCF44"))))
 '(web-mode-css-property-name-face ((t (:foreground "#87CEEB"))))
 '(web-mode-css-string-face        ((t (:foreground "#D78181"))))
 )

(require 'rainbow-mode)
(add-hook 'css-mode-hook 'rainbow-mode)
(add-hook 'less-mode-hook 'rainbow-mode)
(add-hook 'web-mode-hook 'rainbow-mode)
(add-hook 'html-mode-hook 'rainbow-mode)

;; ----- yaml-mode -----
(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.ya?ml$" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.liquid$" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.dig$" . yaml-mode))
(add-hook 'yaml-mode-hook
          '(lambda () (define-key yaml-mode-map "\C-m" 'newline-and-indent)))
(add-to-list 'auto-mode-alist '("Jenkinsfile" . groovy-mode))
(add-to-list 'auto-mode-alist '("build.gradle". groovy-mode))

;; ----- ruby-mode ------
;; robe-mode with company-mode https://qiita.com/kod314/items/9a56983f0d70f57420b1
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

(require 'ruby-end)

;; ================================================
;; git-gutter
;; ================================================
(require 'git-gutter)
(global-git-gutter-mode t)

;; ================================================
;; flycheck
;; ================================================
(require 'flycheck)
(setq flycheck-check-syntax-automatically '(mode-enabled save))
(add-hook 'ruby-mode-hook 'flycheck-mode)
;; (use-package flycheck
;;     :ensure t
;;       :init (global-flycheck-mode))
;(add-hook 'after-init-hook #'global-flycheck-mode)

;; ================================================
;;  Theme
;; ================================================
;(set-face-background 'linum "#2f2f2f")
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("c74e83f8aa4c78a121b52146eadb792c9facc5b1f02c917e3dbb454fca931223" "3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" default)))
 '(package-selected-packages
   (quote
    (zenburn-theme yasnippet yaml-mode web-mode use-package tss tide solarized-theme smex smartparens smart-mode-line scss-mode sass-mode ruby-end ruby-electric robe projectile prodigy prettier-js powerline popwin pallet nyan-mode multiple-cursors moe-theme js2-mode idle-highlight-mode htmlize highlight-symbol groovy-mode gitignore-mode gitconfig-mode git-gutter flycheck-cask expand-region exec-path-from-shell dumb-jump drag-stuff dockerfile-mode cyberpunk-theme company-web color-theme coffee-mode avy anzu adoc-mode))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "#000000" :foreground "brightwhite" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 1 :width normal :foundry "default" :family "default"))))
 '(font-lock-comment-face ((t (:foreground "color-144" :slant italic))))
 '(font-lock-string-face ((t (:foreground "green"))))
 '(font-lock-variable-name-face ((t (:foreground "yellow"))))
 '(hl-line ((t (:background "color-235"))))
 '(shadow ((t (:foreground "grey70")))))
