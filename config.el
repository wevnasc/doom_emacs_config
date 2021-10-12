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
(setq doom-theme 'doom-horizon
      doom-dark+-blue-modeline t
      doom-themes-treemacs-theme "doom-colors"
      doom-font (font-spec :family "JetBrains Mono" :size 14 :weight 'light)
      doom-big-font (font-spec :family "JetBrais Mono" :size 24)
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


;; load executable files on emacs
(dolist (path '("/usr/local/bin" "~/.asdf/shims"))
  (when (file-directory-p path)
    (add-to-list 'exec-path path)
    (setenv "PATH" (concat (getenv "PATH") ":" path))))

;; load nubank config
(let ((nudev-emacs-path "~/dev/nu/nudev/ides/emacs/"))
  (when (file-directory-p nudev-emacs-path)
    (add-to-list 'load-path nudev-emacs-path)
    (require 'nu)))

;; Load project folders to be discovery by projectile
(setq projectile-project-search-path '("~/dev/nu" "~/dev/wevnasc"))

;; Enable paredit for clojure, clojurescript and repl mode
(autoload 'enable-paredit-mode "paredit" "Turn on pseudo-structural editing of Lisp code." t)
(add-hook 'clojure-mode-hook #'enable-paredit-mode)
(add-hook 'clojurescript-mode-hook #'enable-paredit)
(add-hook 'nrepl-mode-hook 'paredit-mode)

;; Cider config
(setq cider-font-lock-dynamically '(macro core function var))

(use-package! clojure-mode
  :config
  (setq clojure-indent-style 'align-arguments
        clojure-thread-all-but-last t
        clojure-align-forms-automatically t
        comment-start ";"
        yas-minor-mode t))

(after! lsp-ui
  (setq lsp-ui-doc-enable 't
        lsp-ui-doc-max-width 300
        lsp-ui-doc-delay 4
        lsp-ui-doc-position 'at-point))

;; keybinds
(map! :ne "M-/" #'comment-or-uncomment-region)
(map! :ne "SPC v" #'er/expand-region)
