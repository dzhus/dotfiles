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
(defvar my/whitespace-cleanup-switch t
  "When nil, `my/whitespace-cleanup' does nothing.")
(make-variable-buffer-local 'my/whitespace-cleanup-switch)

(defun my/whitespace-cleanup ()
  (interactive)
  (if my/whitespace-cleanup-switch
      (whitespace-cleanup)
    (message "whitespace-cleanup has been ignored")))

(defun my/whitespace-cleanup-switch ()
  (interactive)
  (setq my/whitespace-cleanup-switch (not my/whitespace-cleanup-switch))
  (message
   "whitespace-cleanup is now %s"
   (if my/whitespace-cleanup-switch "on" "OFF")))

(require 'flycheck)
(require 'intero)
(flycheck-add-next-checker 'intero
                           '(warning . haskell-hlint))

(require 'elpy)
(add-hook 'python-mode-hook #'pipenv-mode)
(elpy-enable)
(when (require 'flycheck nil t)
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  (add-hook 'elpy-mode-hook 'flycheck-mode))

;; (require 'lsp-ui)
;; (require 'lsp-haskell)
;; (add-hook 'lsp-mode-hook 'lsp-ui-mode)
;; (add-hook 'haskell-mode-hook 'flycheck-mode)
;; (add-hook 'haskell-mode-hook #'lsp-haskell-enable)

(add-hook 'image-mode-hook #'eimp-mode)

(load "~/.emacs-custom.el")
(load "~/.emacs-keys.el")

(push "~/.emacs.d/lisp" load-path)
(load-library "mermaid.el")

(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
