;;; -*- lexical-binding: t -*-

;; General
(use-package flycheck
  :load-path "pkgs/flycheck"
  :commands flycheck-mode
  :init (setq flycheck-mode-line-prefix "F"))

;; Rust
(use-package rust-mode
  :load-path "pkgs/rust-mode"
  :mode ("\\.rs\\'"   . rust-mode)
        ("\\.rlib\\'" . rust-mode))

(use-package flycheck-rust
  :load-path "pkgs/flycheck-rust"
  :after rust-mode
  :hook ((rust-mode . flycheck-mode)
         (rust-mode . flycheck-rust-setup)))

;; Clojure
(use-package clojure-mode
  :load-path "pkgs/clojure-mode"
  :mode ("\\.clj\\'"  . clojure-mode)
        ("\\.cljs\\'" . clojurescript-mode)
        ("\\.cljc\\'" . clojurec-mode))

(use-package rainbow-delimiters
  :load-path "pkgs/rainbow-delimiters"
  :after (:any clojure-mode lisp-mode)
  :hook ((clojure-mode . rainbow-delimiters-mode)
         (emacs-lisp-mode . rainbow-delimiters-mode)))

(use-package cider
  :load-path "pkgs/cider"
  :after clojure-mode)

;; Haskell
(use-package haskell-mode
  :load-path "pkgs/haskell-mode"
  :mode ("\\.hs\\'" . haskell-mode)
  :hook (haskell-mode . interactive-haskell-mode))

(use-package flycheck-haskell
  :load-path "pkgs/flycheck-haskell"
  :after haskell-mode
  :hook ((haskell-mode . flycheck-mode)
         (haskell-mode . flycheck-haskell-setup)))

;; Elm
(use-package elm-mode
  :load-path "pkgs/elm-mode"
  :mode ("\\.elm\\'" . elm-mode))

;; Web mode
(use-package web-mode
  :load-path "pkgs/web-mode"
  :mode
  ("\\.html?\\'" . web-mode)
  ("\\.js[x]?\\'" . web-mode)
  ("\\.ts[x]?\\'" . web-mode)
  ("\\.json?\\'" . web-mode)
  :config
  (add-to-list 'web-mode-indentation-params '("lineup-args" . nil))
  (add-to-list 'web-mode-indentation-params '("lineup-calls" . nil))
  (add-to-list 'web-mode-indentation-params '("lineup-concats" . nil))
  (add-to-list 'web-mode-indentation-params '("lineup-ternary" . nil))
  (setq web-mode-enable-auto-quoting nil)
  (setq web-mode-content-types-alist
    '(("jsx" . "/\\(rn\\|component\\)[s]?/.*\\.js[x]?\\'")))
  )

;; Markdown
(use-package markdown-mode
  :load-path "pkgs/markdown-mode"
  :mode
  ("\\.md?\\'" . markdown-mode))

(provide 'lang)
