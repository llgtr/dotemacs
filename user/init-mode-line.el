;;; -*- lexical-binding: t -*-

;; Override default value (%12b)
(setq-default mode-line-buffer-identification
  (propertized-buffer-identification "%b"))

(set-face-attribute 'mode-line nil :foreground "#393939" :background "#515151" :weight 'ultra-bold)
(set-face-attribute 'mode-line-buffer-id nil :foreground "#99cc99")
(set-face-attribute 'mode-line-highlight nil :box nil :foreground "#bb77bb")
(set-face-attribute 'mode-line-inactive  nil :inherit 'default)

(defface mode-line-directory
  '((t :foreground "#99cc99" :weight light))
  ""
  :group 'mode-line-faces
  :group 'basic-faces)

(defface mode-line-basic
  '((t :foreground "#f99157" :weight normal))
  ""
  :group 'mode-line-faces
  :group 'basic-faces)

(defface mode-line-accent1
  '((t :foreground "#ffcc66" :weight bold))
  ""
  :group 'mode-line-faces
  :group 'basic-faces)

(defface mode-line-accent2
  '((t :foreground "#6699cc" :weight normal))
  ""
  :group 'mode-line-faces
  :group 'basic-faces)

(defun simple-mode-line-render (left right)
  "Return a string of `window-total-width' length containing LEFT, and RIGHT
 aligned respectively."
  (let* ((available-width (- (window-total-width) (length left) 2)))
    (format (format " %%s %%%ds " available-width) left right)))

(defun shorten-directory (dir max-length)
  "Show up to `max-length' characters of a directory name `dir'."
  (let ((path (reverse (split-string (abbreviate-file-name dir) "/")))
               (output ""))
       (when (and path (equal "" (car path)))
         (setq path (cdr path)))
       (while (and path (< (length output) (- max-length 4)))
         (setq output (concat (car path) "/" output))
         (setq path (cdr path)))
       (when path
         (setq output (concat ".../" output)))
       output))

(defun mode-line-directory ()
  (if (buffer-file-name)
      (concat " " (shorten-directory default-directory 20)) " "))


;; The functions below are heavily borrowed from doom-emacs

(defun buffer-info ()
  "Show info about current buffer's state"
  (cond (buffer-read-only
                  "")
                 ((buffer-modified-p)
                  "*")
                 ((and buffer-file-name
                       (not (file-exists-p buffer-file-name)))
                  "∅")
                 ("-")))

(defun eol-info ()
  "Show info about encoding and line endings"
  (concat " "
          (if (memq
               (plist-get (coding-system-plist buffer-file-coding-system) :category)
               '(coding-category-undecided coding-category-utf-8))
              "U")
          " "
          (pcase (coding-system-eol-type buffer-file-coding-system)
            (0 "LF")
            (1 "CRLF")
            (2 "CR"))))

;; This accommodates git only
(defun vc-info ()
  "Show vc state"
  (let ((state (vc-state buffer-file-name (vc-backend buffer-file-name))))
    (concat
     " "
     (substring vc-mode 5)
     " "
     (if (memq state '(edited added)) "(*)" "(-)"))))

(defun flycheck-error-info ()
  "Show Flycheck's state"
    (pcase flycheck-last-status-change
      ('finished (if flycheck-current-errors
                     (let-alist (flycheck-count-errors flycheck-current-errors)
                       (let ((sum (+ (or .error 0) (or .warning 0))))
                         (number-to-string sum)))
                   "✔"))
      ('running "⚡")
      ('no-checker "∅")
      ('errored "✘")
      ('interrupted "‽")))

(setq-default
 mode-line-format
 (list '(:eval (simple-mode-line-render
                (concat (format-mode-line (upcase (prin1-to-string evil-state)) "mode-line-accent1")
                        " |"
                        (if vc-mode (concat (format-mode-line (vc-info) "mode-line-accent2") " |"))
                        (format-mode-line (eol-info) "mode-line-basic")
                        " "
                        (format-mode-line (buffer-info) "mode-line-basic")
                        " |"
                        (format-mode-line (mode-line-directory) "mode-line-directory")
                        (format-mode-line 'mode-line-buffer-identification))
                (concat (when (boundp 'flycheck-last-status-change)
                          (concat (format-mode-line (flycheck-error-info) "mode-line-basic") " | "))
                        (format-mode-line 'mode-name "mode-line-accent2")
                        " | "
                        (format-mode-line "%l:%c " "mode-line-accent1"))))))

(provide 'init-mode-line)
