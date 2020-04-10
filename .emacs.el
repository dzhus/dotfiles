; -*- lexical-binding: t -*-
(package-initialize)

(defun my/common-text-hook ()
  (turn-on-auto-fill))

(use-package org-mode
  :ensure t
  :init
  (add-hook 'org-mode-hook 'my/common-text-hook))

;;;; Markdown-mode
(use-package markdown-mode
  :ensure t
  :init
  (add-hook 'markdown-mode-hook 'my/common-text-hook)
  (add-hook 'text-mode-hook 'my/common-text-hook))

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

(use-package flycheck :ensure t)
(use-package projectile :ensure t)

;;;; Haskell

(use-package dante
  :ensure t
  :after haskell-mode
  :commands 'dante-mode
  :init
  (add-hook 'haskell-mode-hook 'flycheck-mode)
  (add-hook 'haskell-mode-hook 'dante-mode)
  (add-hook 'dante-mode-hook
            '(lambda () (flycheck-add-next-checker 'haskell-dante
                '(warning . haskell-hlint)))))

;;;; Typescript

(use-package tide
  :ensure t)

(use-package typescript-mode
  :ensure t
  :init
  (add-hook 'typescript-mode-hook
            (lambda ()
              (tide-mode)
              (tide-restart-server)
              (company-mode))))

;;; Commands

(defun sql-threads ()
  (interactive)
  (let ((env (completing-read "Environment: " '("staging" "production")))
        (user (completing-read "User: " '("analyticsro"))))
    (let ((creds
           (with-temp-buffer
             (call-process "tscripts" nil t nil
                           "ssm" "get-all" "-e" env
                           "--path" (concat "/databases/rds-pg-threads-main/threads_main/" user))
             (goto-char (point-min))
             (json-read))))
      (setq-default sql-database (alist-get 'db creds))
      (setq-default sql-user (alist-get 'user creds))
      (setq-default sql-server (replace-regexp-in-string
                                "main" "main-replica-backups"
                                (alist-get 'host creds)))
      (setenv "PGPASSWORD" (alist-get 'password creds))
      (sql-postgres)
      (setenv "PGPASSWORD"))))

;; (add-hook 'python-mode-hook #'pipenv-mode)

(add-hook 'image-mode-hook #'eimp-mode)

(load "~/.emacs-custom.el")
(load "~/.emacs-keys.el")

(push "~/.emacs.d/lisp" load-path)
(load-library "mermaid.el")

(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
