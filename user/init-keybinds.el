;;; -*- lexical-binding: t -*-

(use-package general
  :load-path "pkgs/general"
  :config
  (general-define-key
   :states '(normal visual insert emacs)
   :prefix "SPC" ;; Leader key
   :non-normal-prefix "M-SPC" ;; Alternative leader when in insert or emacs mode
   :keymaps 'override

   ;; General binds
   "SPC" '(execute-extended-command :wk "M-x")
   "TAB" '(other-window :wk "Cycle windows")

   ;; Applications
   "a" '(:ignore t :wk "Applications")
   "au" 'undo-tree-visualize
   "at" 'tetris

   ;; Buffers
   "b" '(:ignore t :wk "Buffers")
   "bd" 'kill-buffer
   "bw" 'read-only-mode
   "bl" 'list-buffers
   "bp" 'previous-buffer
   "bn" 'next-buffer

   ;; Comments
   "c" '(:ignore t :wk "Comments")
   "cr" 'comment-region
   "cl" 'comment-line

   ;; Files
   "f" '(:ignore t :wk "Files")
   "fr" 'recentf-open-files

   ;; Help
   "h" '(:ignore t :wk "Help")
   "hw" 'which-key-show-top-level
   "hm" 'which-key-show-major-mode
   "hdv" 'describe-variable
   "hdf" 'describe-function
   "hdF" 'describe-face
   "hdk" 'describe-key
   "hdp" 'describe-package
   "hdm" 'describe-mode

   ;; Insert
   "i" '(:ignore t :wk "Insert")
   "ic" 'insert-char

   ;; Jump
   "j" '(:ignore t :wk "Jump")
   "jc" 'avy-goto-char
   "jw" 'avy-goto-word-1
   "jl" 'avy-goto-line

   ;; Projectile / project
   "p" '(:ignore t :wk "Project")
   "p!" 'projectile-run-shell-command
   "pD" 'projectile-dired
   "pf" 'projectile-find-file

   ;; Toggles
   "t" '(:ignore t :wk "Toggles")
   "tw" 'whitespace-mode

   "T" '(:ignore t :wk "UI toggles")
   "Tt" 'tool-bar-mode
   "Tm" 'menu-bar-mode

   ;; Windows
   "w" '(:ignore t :wk "Windows")
   "w\"" 'split-window-below
   "w%" 'split-window-right
   "wd" 'delete-window
   "wD" 'delete-other-windows)

  ;; Org-mode
  (general-define-key
    :states '(normal visual insert emacs)
    :prefix ","
    :non-normal-prefix "M-,"
    :keymaps 'org-mode-map
    "t" 'org-todo
    "e" 'org-export-dispatch
    "s" 'org-schedule
    )
  )

(provide 'init-keybinds)
