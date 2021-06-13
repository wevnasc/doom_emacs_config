(setq exec-paths (list "/usr/local/bin"
                       "~/.asdf/shims"
                       "~/rust-analyzer/target/release"))

(let ((nudev-emacs-path "~/dev/nu/nudev/ides/emacs/"))
  (when (file-directory-p nudev-emacs-path)
    (add-to-list 'load-path nudev-emacs-path)
    (require 'nu)))

(dolist (path exec-paths)
  (when (file-directory-p path)
    (add-to-list 'exec-path path)
    (setenv "PATH" (concat (getenv "PATH") ":" path))))

(setq project-paths (list "~/dev/nu"
                          "~/dev/wevnasc"))
(setq default-directory "~"
      mac-command-modifier 'meta
      projectile-project-search-path project-paths)

(autoload 'enable-paredit-mode "paredit" "Turn on pseudo-structural editing of Lisp code." t)
(add-hook 'clojure-mode-hook #'enable-paredit-mode)
(add-hook 'clojurescript-mode-hook #'enable-paredit)

(setq nrepl-popup-on-error nil)
(setq nrepl-popup-stacktraces-in-repl t)

(add-hook 'nrepl-interaction-mode-hook 'nrepl-turn-on-eldoc-mode)
(add-hook 'nrepl-mode-hook 'paredit-mode)

(after! cider
 (set-popup-rule! "^\\*cider-repl" :select nil :width 85 :side 'right :slot 1))

(use-package! clojure-mode
  :config
  (setq clojure-indent-style 'align-arguments
        clojure-thread-all-but-last t
        clojure-align-forms-automatically t
        comment-start ";"
        yas-minor-mode 1))

(use-package! lsp-mode
  :commands lsp
  :hook ((clojure-mode . lsp)
         (rustic-mode . lsp))
  :config
  (setq lsp-headerline-breadcrumb-enable nil
        lsp-lens-enable t
        lsp-semantic-tokens-enable t
        lsp-signature-auto-activate nil)
  (push "[/\\\\][^/\\\\]*\\.\\(json\\|pyc\\|class\\)$" lsp-file-watch-ignored-directories)
  (dolist (clojure-all-modes '(clojure-mode
                               clojurec-mode
                               clojurescript-mode
                               clojurex-mode))
    (add-to-list 'lsp-language-id-configuration `(,clojure-all-modes . "clojure")))
  (advice-add #'lsp-rename :after (lambda (&rest _) (projectile-save-project-buffers))))

(setq lsp-rust-server 'rust-analyzer)

(use-package! lsp-ui-mode
  :after lsp-mode
  :commands lsp-ui-mode
  :config
  (setq lsp-ui-doc-include-signature nil
        lsp-ui-doc-position 'top
        lsp-ui-doc-header t
        lsp-ui-doc-enable t
        lsp-ui-doc-include-signature t
        lsp-ui-doc-use-childframe t
        lsp-ui-sideline-show-hover t
        lsp-ui-sideline-ignore-duplicate t
        lsp-ui-sideline-show-code-actions nil
        lsp-ui-sideline-show-symbol t
        lsp-ui-doc-border (doom-color 'fg)
        lsp-ui-peek-fontify 'always))

(setq disable-evil-keys (list "<up>" "<down>" "<left>" "<right>"))

(dolist (key disable-evil-keys)
  (define-key evil-insert-state-map (kbd key) 'nope)
  (define-key evil-normal-state-map (kbd key) 'nope))

(map! :ne "M-/" #'comment-or-uncomment-region)
(map! :ne "SPC v" #'er/expand-region)
