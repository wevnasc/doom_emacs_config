;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Weverson Nascimento"
      user-mail-address "weverson.sn@gmail.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
;; (setq doom-font (font-spec :family "monospace" :size 12 :weight 'semi-light)
;;       doom-variable-pitch-font (font-spec :family "sans" :size 13))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-xcode
      doom-font (font-spec :family "Fira Code" :size 14 :weight 'regular)
      doom-big-font (font-spec :family "Fira Code" :size 24)
      doom-variable-pitch-font (font-spec :family "Fira Code"))

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type 'relative)
(add-hook 'window-setup-hook 'toggle-frame-maximized t)
;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

(let ((nudev-emacs-path "~/dev/nu/nudev/ides/emacs/"))
  (when (file-directory-p nudev-emacs-path)
    (add-to-list 'load-path nudev-emacs-path)
    (require 'nu)))

;; Environment
(setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin"))
(setenv "PATH" (concat (getenv "PATH") ":/Users/weverson.nascimento/.asdf/shims"))
(add-to-list 'exec-path "/usr/local/bin")
(add-to-list 'exec-path "~/.asdf/shims")
(add-to-list 'exec-path "~/rust-analyzer/target/release")

(setq default-directory "~"
      mac-command-modifier 'meta
      projectile-project-search-path '("~/dev/nu" "~/dev/projects" "~/dev/personal"))

;; Clojure
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

;; LSP
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

;; Shortcuts
(map! :ne "M-/" #'comment-or-uncomment-region)
(map! :ne "SPC v" #'er/expand-region)

(define-key evil-insert-state-map (kbd "<up>") 'nope)
(define-key evil-insert-state-map (kbd "<down>") 'nope)
(define-key evil-insert-state-map (kbd "<left>") 'nope)
(define-key evil-insert-state-map (kbd "<right>") 'nope)

(define-key evil-normal-state-map (kbd "<up>") 'nope)
(define-key evil-normal-state-map (kbd "<down>") 'nope)
(define-key evil-normal-state-map (kbd "<left>") 'nope)
(define-key evil-normal-state-map (kbd "<right>") 'nope)
