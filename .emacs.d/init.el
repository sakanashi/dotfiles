;;; package --- Summary
(setq warning-suppress-log-types '((package reinitialization)))
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
;; (require 'initchart)
;; (initchart-record-execution-time-of load file)
;; (initchart-record-execution-time-of require feature)

(add-to-list 'load-path
              "~/.emacs.d/plugins/yasnippet")
(require 'yasnippet)
(yas-global-mode 1)

;; ==================================================
;; Theme
;; ==================================================
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(setq custom-theme-directory "~/.emacs.d/themes")
(load-theme 'qiita t)
;(load-theme 'monokai t)
;; ==================================================
;; Looks
;; ==================================================
;; line-number
(when (version<= "26.0.50" emacs-version )
  (global-display-line-numbers-mode)
  (column-number-mode t)
  )
(when (version<= emacs-version "26.0.50" )
  (global-linum-mode t)
  (set-face-attribute 'linum nil
                      :foreground "eee8d5"
                      :height 0.9)
  (setq linum-format "%4d| ")
  )

(if window-system (menu-bar-mode 1) (menu-bar-mode -1))

;; line hilight
(global-hl-line-mode t)
;(setq hl-line-face 'underline) ; 下線

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
;; (global-set-key "\C-x3" 'follow-delete-other-windows-and-split)

;; auto insert pair bracket
(electric-pair-mode 1)

;; show indent vertical line
;; (require 'indent-guide)
;; (indent-guide-global-mode)
(add-to-list 'load-path "~/.emacs.d/site-lisp/Highlight-Indentation-for-Emacs")
(require 'highlight-indentation)
;; (highlight-indentation-mode)
(highlight-indentation-mode)

;; color
;; (require 'kurecolor)
;; (use-package kurecolor
;;   :bind (("<f7>" . kurecolor-increase-hue-by-step)
;;          ("M-<f7>" . kurecolor-decrease-hue-by-step)
;;          ("<f8>" . kurecolor-increase-saturation-by-step)
;;          ("M-<f8>" . kurecolor-decrease-saturation-by-step)
;;          ("<f9>" . kurecolor-increase-brightness-by-step)
;;          ("M-<f9>" . kurecolor-decrease-brightness-by-step)))

;; rainbow-delimiters
(require 'rainbow-delimiters)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

;; customize mode line
(setq sml/theme 'respectful)
(setq sml/no-confirm-load-theme t)
(sml/setup)
(set-face-background 'mode-line "color-232")

;; ==================================================
;; Indent
;; ==================================================
(setq c-default-style '((java-mode . "java") (other . "linux")))

;; not use tab for indent
;; (setq-default indent-tabs-mode nil)
(setq-default tab-width 2 indent-tabs-mode nil)
(setq c-basic-offset 2)
(require 'smart-tab)
(setq smart-tab-using-hippie-expand t)

;; do not aut indent
;; (electric-indent-mode 0)

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


;; ;; pangu-spaceing
;; ;;http://rubikitch.com/tag/packagepangu-spacing/
;;   ;;; chinse-two-byte→japaneseに置き換えるだけで日本語でも使える
;; (setq pangu-spacing-chinese-before-english-regexp
;;       (rx (group-n 1 (category japanese))
;;           (group-n 2 (in "a-zA-Z0-9"))))
;; (setq pangu-spacing-chinese-after-english-regexp
;;       (rx (group-n 1 (in "a-zA-Z0-9"))
;;           (group-n 2 (category japanese))))
;;   ;;; 見た目ではなくて実際にスペースを入れる
;; (setq pangu-spacing-real-insert-separtor t)
;; ;; text-modeやその派生モード(org-mode等)のみに使いたいならこれ
;; (add-hook 'text-mode-hook 'pangu-spacing-mode)
;; ;; すべてのメジャーモードに使ってみたい人はこれを
;; ;; (global-pangu-spacing-mode 1)
;; ;

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
;; (global-set-key (kbd "C-M-j") 'avy-goto-char)
(global-set-key (kbd "M-g M-g") 'avy-goto-line)
;; (global-set-key (kbd "M-g M-c") 'avy-goto-char-timer)
(setq avy-background t)

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
(helm-mode 1)
(global-set-key (kbd "M-x") 'helm-M-x)
(setq helm-M-x-fuzzy-match t)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
;; For find-file etc.
(define-key helm-read-file-map (kbd "TAB") 'helm-execute-persistent-action)
;; For helm-find-files etc.
(define-key helm-find-files-map (kbd "TAB") 'helm-execute-persistent-action)

(require 'ag)
(require 'helm-config)
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

;; (require 'lsp-mode)
;; (add-hook 'enh-ruby-mode-hook #'lsp)
;; (add-hook 'web-mode-hook #'lsp)

;; ==================================================
;; undo
;; ==================================================
(require 'undo-tree)
(global-undo-tree-mode t)
(global-set-key (kbd "M-/") 'undo-tree-redo)

;; ==================================================
;; Multiple cursors
;; ==================================================
;; multiple-cursors & smartrep
(require 'multiple-cursors)
(require 'region-bindings-mode)
(region-bindings-mode-enable)

(global-set-key (kbd "C-M-m") 'mc/edit-lines)
(define-key region-bindings-mode-map "a" 'mc/mark-all-like-this)
(define-key region-bindings-mode-map "d" 'mc/mark-all-dwim)
(define-key region-bindings-mode-map "n" 'mc/mark-next-like-this)
(define-key region-bindings-mode-map "N" 'mc/mark-previous-like-this)
(define-key region-bindings-mode-map "p" 'mc/mark-previous-like-this)
(define-key region-bindings-mode-map "m" 'mc/mark-more-like-this-extended)
(define-key region-bindings-mode-map "u" 'mc/unmark-next-like-this)
(define-key region-bindings-mode-map "U" 'mc/unmark-previous-like-this)
(define-key region-bindings-mode-map "s" 'mc/skip-to-next-like-this)
(define-key region-bindings-mode-map "S" 'mc/skip-to-previous-like-this)
(define-key region-bindings-mode-map "i" 'mc/insert-numbers)
(define-key region-bindings-mode-map "h" 'mc-hide-unmatched-lines-mode)
(define-key mc/keymap (kbd "C-c h") 'mc-hide-unmatched-lines-mode)

;; ==================================================
;; Programming
;; ==================================================
;; ----- Web-mode -----
(autoload 'web-mode "web-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.as[cp]x$"   . web-mode))
(add-to-list 'auto-mode-alist '("\\.css?\\'"    . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb$"       . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'"     . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?$"     . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'"   . web-mode))
(add-to-list 'auto-mode-alist '("\\.jsp$"       . web-mode))
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

(require 'rainbow-mode)
(add-hook 'css-mode-hook 'rainbow-mode)
(add-hook 'less-mode-hook 'rainbow-mode)
(add-hook 'web-mode-hook 'rainbow-mode)
(add-hook 'html-mode-hook 'rainbow-mode)
(add-hook 'emacs-lisp-mode-hook 'rainbow-mode)

;; ----- yaml-mode -----
(autoload 'yaml-mode "yaml-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.ya?ml$" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.liquid$" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.dig$" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.dig.erb$" . yaml-mode))
(add-hook 'yaml-mode-hook
          '(lambda () (define-key yaml-mode-map "\C-m" 'newline-and-indent)))

;; ------ groovy-mode -----
(use-package groovy-mode
  :ensure t
  :init
    (setq groovy-indent-offset 2)
    ;; (add-hook 'groovy-mode-hook #'lsp)
    )
(provide 'init-groovy)
(add-to-list 'auto-mode-alist '("Jenkinsfile" . groovy-mode))
(add-to-list 'auto-mode-alist '("build.gradle". groovy-mode))

;; ----- json-mode ------
(add-hook 'json-mode-hook
          (lambda ()
            (make-local-variable 'js-indent-level)
            (setq js-indent-level 2)))

;; ----- ruby-mode ------
(autoload 'enh-ruby-mode "enh-ruby-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.rb$" . enh-ruby-mode))
(add-to-list 'auto-mode-alist '("\\.rb.erb$" . enh-ruby-mode))
;; robe-mode with company-mode https://qiita.com/kod314/items/9a56983f0d70f57420b1
(add-hook 'enh-ruby-mode-hook 'robe-mode)
(autoload 'robe-mode "robe" "Code navigation, documentation lookup and completion for Ruby" t nil)
(eval-after-load 'company
  '(push 'company-robe company-backends))
(setq enh-ruby-add-encoding-comment-on-save nil)

(add-hook 'enh-ruby-mode-hook (lambda()
      (company-mode)
      (setq company-auto-expand t)
      (setq company-transformers '(company-sort-by-backend-importance)) ;; ソート順
      (setq company-idle-delay 0) ; 遅延なしにすぐ表示
      (setq company-minimum-prefix-length 2) ; 何文字打つと補完動作を行うか設定
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
      (require 'ruby-end)
      ))

;; ----- markdown-mode -----
(eval-after-load "markdown-mode"
  (add-hook 'markdown-mode-hook
            '(lambda ()
               (setq whitespace-action '(nil))
               )))

;; ----- adoc-mode -----
(autoload 'adoc-mode "adoc-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.adoc$" . adoc-mode))

;; ;; ------ sql-mode -----
(add-to-list 'auto-mode-alist '("\\.sql.erb$"       . sql-mode))
(add-to-list 'auto-mode-alist '("\\.sql.erb\\'"     . sql-mode))
;; ;; (eval-after-load "sql"
;; ;;   '(load-library "sql-indent"))
;; ;; C-c C-c : 'sql-send-paragraph
;; ;; C-c C-r : 'sql-send-region
;; ;; C-c C-s : 'sql-send-string
;; ;; C-c C-b : 'sql-send-buffer
;; (require 'sql)

;; (add-hook 'sql-interactive-mode-hook
;;           '(lambda ()
;;               (interactive)
;;               ;; (set-buffer-process-coding-system 'sjis-unix 'sjis-unix )
;;               ;; (setq show-trailing-whitespace nil)))
;;               ;; (setq tab-width 4)
;;               ;; (c-set-style "bsd")             ; c-basic-offset を4に設定し、いくつか追加する
;;               ;; (c-set-offset 'case-label '+)   ; PG に合わせてcase 文のインデントを変更する
;;               ;; (setq indent-tabs-mode t)      ; インデント処理時にタブを保持させる
;;               ))

;; ;; starting SQL mode loading sql-indent / sql-complete
;; (eval-after-load "sql"
;;   '(progn
;;      (load-library "sql-indent")
;;      (load-library "sql-complete")
;;      (load-library "sql-transform")))

;; (setq auto-mode-alist
;;       (cons '("\\.sql$" . sql-mode) auto-mode-alist))

;; (sql-set-product-feature
;;  'ms :font-lock 'sql-mode-ms-font-lock-keywords)

;; ================================================
;; highlight
;; ================================================
(require 'highlight-symbol)
;; 0.3秒後自動ハイライトされるようになる
(setq highlight-symbol-idle-delay 0.3)
;(setq highlight-symbol-colors '("yellow"))
(setq highlight-symbol-colors '("DarkOrange" "DodgerBlue1" "DeepPink1"))
;;; 自動ハイライトをしたいならば
(add-hook 'enh-ruby-mode-hook 'highlight-symbol-mode)
(add-hook 'yaml-mode-hook 'highlight-symbol-mode)
(add-hook 'json-mode-hook 'highlight-symbol-mode)
(add-hook 'sql-mode-hook 'highlight-symbol-mode)

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
(add-hook 'after-init-hook #'global-flycheck-mode)
(setq flycheck-check-syntax-automatically '(idle-change mode-enabled new-line save))
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(eval-after-load 'js2-mode
  '(add-hook 'js2-mode-hook ;#'add-node-modules-path
             (lambda ()
               (setq my-js-mode-indent-num 2)
               (setq js2-basic-offset my-js-mode-indent-num)
               (setq js-switch-indent-offset my-js-mode-indent-num)
               (setq js-indent-level my-js-mode-indent-num)
               '(flycheck-disabled-checkers '(javascript-jshint javascript-jscs))
               (flycheck-select-checker 'javascript-eslint)
;               (flycheck-mode t)
             )))
;; (add-hook 'js2-mode-hook
;;           (lambda ()
;;              (setq my-js-mode-indent-num 2)
;;              (setq js2-basic-offset my-js-mode-indent-num)
;;              (setq js-switch-indent-offset my-js-mode-indent-num)
;;              (flycheck-select-checker 'javascript-eslint)
;;  ;            (flycheck-mode t)
;;              ))
(flycheck-define-checker textlint
  "A linter for prose."
  :command ("textlint" "--format" "unix" source-inplace)
 :error-patterns
  ((warning line-start (file-name) ":" line ":" column ": "
            (id (one-or-more (not (any " "))))
            (message (one-or-more not-newline)
                     (zero-or-more "\n" (any " ") (one-or-more not-newline)))
            line-end))
  :modes (text-mode markdown-mode))

(add-to-list 'flycheck-checkers 'textlint)
(add-hook 'markdown-mode-hook 'flycheck-mode)

(flycheck-define-checker textlint
  "A linter for prose."
  :command ("textlint" "--format" "unix" source-inplace)
 :error-patterns
  ((warning line-start (file-name) ":" line ":" column ": "
            (id (one-or-more (not (any " "))))
            (message (one-or-more not-newline)
                     (zero-or-more "\n" (any " ") (one-or-more not-newline)))
            line-end))
  :modes (text-mode adoc-mode))
(eval-after-load 'adoc-mode
  '(add-hook 'adoc-mode-hook #'add-node-modules-path))
(add-hook 'adoc-mode-hook 'flycheck-mode)
;(add-hook 'adoc-mode-hook 'pangu-spacing-mode)

;; auto cleanup before save
(setq whitespace-action '(auto-cleanup))

(global-whitespace-mode 1)


;; ================================================
;;  Theme
;; ================================================
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (csv-mode kurecolor hexrgb zenburn-theme yasnippet yaml-mode web-mode use-package tss tide solarized-theme smex smartparens smart-mode-line scss-mode sass-mode ruby-end ruby-electric robe projectile prodigy prettier-js powerline popwin pallet nyan-mode multiple-cursors moe-theme js2-mode idle-highlight-mode htmlize highlight-symbol groovy-mode gitignore-mode gitconfig-mode git-gutter flycheck-cask expand-region exec-path-from-shell dumb-jump drag-stuff dockerfile-mode cyberpunk-theme company-web color-theme coffee-mode avy anzu adoc-mode))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
