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
  :demand t)

(use-package f
  :load-path "lib/f"
  :defer t)

(use-package s
  :load-path "lib/s"
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
  (load-theme 'base16-eighties t))

(use-package ivy
  :load-path "pkgs/ivy"
  :config
  (ivy-mode)
  (setq ivy-use-virtual-buffers t)
  (setq ivy-height 10))

(use-package avy
  :load-path "pkgs/avy"
  :config
  (setq avy-background t))

(use-package smartparens
  :load-path "pkgs/smartparens"
  :config
  (require 'smartparens-config)
  (smartparens-global-mode t))

(use-package which-key
  :load-path "pkgs/which-key"
  :demand t
  :config
  (which-key-mode)
  (setq which-key-idle-secondary-delay 0))

(use-package rainbow-mode
  :load-path "pkgs/rainbow-mode"
  :defer t
  :commands (rainbow-mode))

(use-package projectile
  :load-path "pkgs/projectile"
  :config
  (setq projectile-completion-system 'ivy)
  (projectile-mode))

(require 'init-keybinds)
(require 'init-mode-line)
(require 'init-lang)

(provide 'init-packages)
