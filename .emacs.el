; -*- lexical-binding: t -*-
(package-initialize)

(defun my/common-text-hook ()
  (turn-on-auto-fill))

;;;; Org-mode
(add-hook 'org-mode-hook 'my/common-text-hook)

;;;; Markdown-mode
(require 'markdown-mode)
(add-hook 'markdown-mode-hook 'my/common-text-hook)
(add-hook 'text-mode-hook 'my/common-text-hook)

(defun my/go-dark ()
  (interactive)
  (setq my/dark (if (boundp 'my/dark) (not my/dark) nil))
  (if my/dark
      (load-theme 'solarized-light)
    (load-theme 'solarized-dark)))

;;; Switch large default font on and off
(defvar my/large-font nil "When t, default is switched to large font")

(let ((original-size (face-attribute 'default :height)))
  (defun my/switch-large-font ()
    (interactive)
    (if my/large-font
        (set-face-attribute 'default nil :height original-size)
      (set-face-attribute 'default nil :height (round (* 1.5 original-size))))
    (setq my/large-font (not my/large-font))))

;;; Switchable whitespace-cleanup hook
(defun my/brittany ()
  (interactive)
  (shell-command (concat "brittany --write-mode inplace " (buffer-file-name))))

(defvar my/whitespace-cleanup-switch t
  "When nil, `my/whitespace-cleanup' does nothing.")
(make-variable-buffer-local 'my/whitespace-cleanup-switch)

(defun my/whitespace-cleanup ()
  (interactive)
  (if my/whitespace-cleanup-switch
      (progn
        (whitespace-cleanup)
        (when (eq major-mode 'sql-mode)
          (sqlformat-buffer)))
    (message "whitespace-cleanup has been ignored")))

(defun my/whitespace-cleanup-switch ()
  (interactive)
  (setq my/whitespace-cleanup-switch (not my/whitespace-cleanup-switch))
  (message
   "whitespace-cleanup is now %s"
   (if my/whitespace-cleanup-switch "on" "OFF")))

(eval-after-load "org-present"
  '(progn
     (add-hook 'org-present-mode-hook
               (lambda ()
                 (org-present-big)
                 (org-display-inline-images)
                 (org-present-hide-cursor)
                 (org-present-read-only)
                 (hide-mode-line-mode)))
     (add-hook 'org-present-mode-quit-hook
               (lambda ()
                 (org-present-small)
                 (org-remove-inline-images)
                 (org-present-show-cursor)
                 (org-present-read-write)
                 (hide-mode-line-mode)))))

(require 'flycheck)
(require 'intero)
(flycheck-add-next-checker 'intero
                           '(warning . haskell-hlint))

(add-hook 'python-mode-hook #'pipenv-mode)

(add-hook 'image-mode-hook #'eimp-mode)

(load "~/.emacs-custom.el")
(load "~/.emacs-keys.el")

(push "~/.emacs.d/lisp" load-path)
(load-library "mermaid.el")

(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
