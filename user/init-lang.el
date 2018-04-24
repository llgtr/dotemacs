;;; -*- lexical-binding: t -*-

;; General
(use-package flycheck
  :load-path "lock/flycheck"
  :commands flycheck-mode
  :init (setq flycheck-mode-line-prefix "F"))

;; Rust
(use-package rust-mode
  :load-path "lock/rust-mode"
  :mode ("\\.rs\\'"   . rust-mode)
        ("\\.rlib\\'" . rust-mode))

(use-package flycheck-rust
  :load-path "lock/flycheck-rust"
  :after rust-mode
  :hook ((rust-mode . flycheck-mode)
         (rust-mode . flycheck-rust-setup)))

;; Clojure
(use-package clojure-mode
  :load-path "lock/clojure-mode"
  :mode ("\\.clj\\'"  . clojure-mode)
        ("\\.cljs\\'" . clojure-mode)
        ("\\.cljc\\'" . clojure-mode))

(use-package rainbow-delimiters
  :load-path "lock/rainbow-delimiters"
  :after (:any clojure-mode lisp-mode)
  :hook ((clojure-mode . rainbow-delimiters-mode)
         (emacs-lisp-mode . rainbow-delimiters-mode)))

(use-package cider
  :load-path "lock/cider"
  :after clojure-mode)

;; Haskell
(use-package haskell-mode
  :load-path "lock/haskell-mode"
  :mode ("\\.hs\\'" . haskell-mode)
  :hook (haskell-mode . interactive-haskell-mode))

(use-package flycheck-haskell
  :load-path "lock/flycheck-haskell"
  :after haskell-mode
  :hook ((haskell-mode . flycheck-mode)
         (haskell-mode . flycheck-haskell-setup)))

(use-package web-mode
  :load-path "lock/web-mode"
  :mode
  ("\\.html?\\'" . web-mode))

(use-package js2-mode
  :load-path "lock/js2-mode"
  :mode
  ("\\.js?\\'" . js2-mode))

(use-package rjsx-mode
  :load-path "lock/rjsx-mode"
  :requires js2-mode
  :mode
  ("\\.jsx?\\'" . rjsx-mode))

(provide 'init-lang)
