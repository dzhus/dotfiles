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

;;;; Octave-mode
(add-to-list 'auto-mode-alist
             '("\\.oct$" . octave-mode))
(add-to-list 'auto-mode-alist
             '("\\.m$" . octave-mode))

;; ;;; Show VC diffs in magit colors
;; (require 'magit)

;; (setq diff-added-face             'magit-diff-added)
;; (setq diff-indicator-added-face   'magit-diff-added)
;; (setq diff-removed-face           'magit-diff-removed)
;; (setq diff-indicator-removed-face 'magit-diff-removed)
;; (setq diff-hunk-header-face       'magit-diff-hunk-heading)
;; (setq diff-function-face          'magit-diff-hunk-heading)

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
(require 'elpy)
(flycheck-add-next-checker 'intero
                           '(warning . haskell-hlint))

;; Enable completion in REPLs
(add-hook 'cider-repl-mode-hook #'company-mode)
(add-hook 'cider-mode-hook #'company-mode)

(add-hook 'python-mode-hook #'pipenv-mode)
(elpy-enable)
(when (require 'flycheck nil t)
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  (add-hook 'elpy-mode-hook 'flycheck-mode))

(add-hook 'image-mode-hook #'eimp-mode)

(load "~/.emacs-custom.el")
(load "~/.emacs-keys.el")

(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)