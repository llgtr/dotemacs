;;; -*- lexical-binding: t -*-

;; Path for `use-package` and its dependency
(let ((default-directory (expand-file-name "pkgs" user-emacs-directory)))
  (normal-top-level-add-to-load-path '("use-package")))

(eval-when-compile
  (require 'use-package))

;; Libs
(use-package dash
  :load-path "lib/dash"
  :defer t)

(use-package pkg-info
  :load-path "lib/pkg-info"
  :defer t)

(use-package spinner
  :load-path "lib/spinner"
  :defer t)

(use-package queue
  :load-path "lib/queue"
  :demand t)

(use-package undo-tree
  :load-path "pkgs/evil/lib"
  :demand t
  :config
  (setq undo-tree-enable-undo-in-region nil))

(use-package f
  :load-path "lib/f"
  :defer t)

(use-package s
  :load-path "lib/s"
  :defer t)

(use-package popup
  :load-path "lib/popup"
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
   counsel-ag
   counsel-rg)
  :config
  (setq counsel-ag-base-command "ag --nocolor --nogroup --width 200 %s")
  (setq counsel-rg-base-command "rg -i -M 200 --no-heading --line-number --color never %s ."))

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

(use-package dumb-jump
  :load-path "pkgs/dumb-jump"
  :defer t
  :commands (dumb-jump-go dumb-jump-back dumb-jump-quick-look)
  :config
  (setq dumb-jump-selector 'ivy)
  (setq dumb-jump-force-searcher 'rg))

(provide 'packages)
