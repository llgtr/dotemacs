;;; -*- lexical-binding: t -*-

;; Path for `use-package` and its dependency
(let ((default-directory (expand-file-name "pkgs" user-emacs-directory)))
  (normal-top-level-add-to-load-path '("use-package")))

(eval-when-compile
  (require 'use-package))

;; Libs
(use-package dash
  :load-path "pkgs/dash"
  :defer t)

(use-package pkg-info
  :load-path "pkgs/pkg-info"
  :defer t)

(use-package spinner
  :load-path "pkgs/spinner"
  :defer t)

(use-package queue
  :load-path "pkgs/queue"
  :demand t)

(use-package undo-tree
  :load-path "pkgs/evil/lib"
  :demand t
  :config
  (setq undo-tree-enable-undo-in-region nil))

(use-package f
  :load-path "pkgs/f"
  :defer t)

(use-package s
  :load-path "pkgs/s"
  :defer t)

(use-package async
  :load-path "pkgs/async"
  :defer t)

(use-package ghub
  :load-path "pkgs/ghub"
  :defer t)

(use-package magit-popup
  :load-path "pkgs/magit-popup"
  :defer t)

(use-package with-editor
  :load-path "pkgs/with-editor"
  :defer t)

(use-package graphql
  :load-path "pkgs/graphql"
  :defer t)

(use-package treepy
  :load-path "pkgs/treepy"
  :defer t)

;; Packages
(use-package evil
  :load-path "pkgs/evil"
  :config
  (evil-mode))

(use-package base16-theme
  :load-path "pkgs/base16-theme"
  :init
  (add-to-list 'custom-theme-load-path (expand-file-name "pkgs/base16-theme/build" user-emacs-directory))
  :config
  (load-theme 'base16-gruvbox-dark-pale t))

(use-package ivy
  :load-path "pkgs/ivy"
  :config
  (ivy-mode)
  (setq ivy-use-virtual-buffers t)
  (setq ivy-height 10))

(use-package counsel
  :load-path "pkgs/ivy"
  :commands
  (counsel-yank-pop
   counsel-describe-variable
   counsel-describe-function
   counsel-describe-face
   counsel-git-grep
   counsel-ag))

(use-package avy
  :load-path "pkgs/avy"
  :config
  (setq avy-background t))

(use-package ace-window
  :load-path "pkgs/ace-window"
  :defer t
  :commands (ace-window ace-swap-window)
  :config
  (setq aw-scope 'frame))

(use-package which-key
  :load-path "pkgs/which-key"
  :demand t
  :config
  (which-key-mode)
  (setq which-key-idle-secondary-delay 0))

(use-package projectile
  :load-path "pkgs/projectile"
  :config
  (setq projectile-completion-system 'ivy)
  (projectile-mode))

(use-package yasnippet
  :load-path "pkgs/yasnippet"
  :defer t
  :hook
  ((web-mode . yas-minor-mode)
   (latex-mode . yas-minor-mode)
   (markdown-mode . yas-minor-mode)
   (org-mode . yas-minor-mode)
   (sh-mode . yas-minor-mode))
  :config (yas-reload-all))

(use-package smartparens
  :load-path "pkgs/smartparens"
  :config
  (smartparens-global-mode))

(use-package editorconfig
  :load-path "pkgs/editorconfig"
  :config (editorconfig-mode))

(use-package ox-hugo
  :load-path "pkgs/ox-hugo"
  :after ox)

(use-package magit
  :load-path "pkgs/magit/lisp"
  :commands magit-status)

(use-package evil-magit
  :load-path "pkgs/evil-magit"
  :after magit)

(require 'init-keybinds)
(require 'init-mode-line)
(require 'init-lang)

(provide 'init-packages)
