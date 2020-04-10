; -*- lexical-binding: t -*-
(package-initialize)

;;;; Custom helpers and commands

(defun my/common-text-hook ()
  (turn-on-auto-fill))

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

;;;; Packages

(require 'use-package-ensure)
(setq use-package-always-ensure t)

(use-package counsel
  :bind
  (("M-x" . counsel-M-x)
   ("C-c f" . counsel-git-grep)))

(use-package dante
  :after haskell-mode
  :commands 'dante-mode
  :init
  (add-hook 'haskell-mode-hook 'flycheck-mode)
  (add-hook 'haskell-mode-hook 'dante-mode)
  (add-hook 'dante-mode-hook
            '(lambda () (flycheck-add-next-checker 'haskell-dante
                '(warning . haskell-hlint)))))

(use-package flycheck
  :bind (("<f7>" . flycheck-mode)))

(use-package magit
  :bind (("C-x v =" . magit-diff-buffer-file)
         ("C-x v l" . magit-log-buffer-file)
         ("<f5>" . magit-status)
         ;; MBP Touch bar workaround
         ("C-5" . magit-status)))

(use-package markdown-mode
  :init
  (add-hook 'markdown-mode-hook 'my/common-text-hook)
  :bind (:map
         markdown-mode-map
         ("C-c a" . auto-fill-mode)))

(use-package org
  :init
  (add-hook 'org-mode-hook 'my/common-text-hook))

(use-package projectile
  :bind
  (("<f12>" . projectile-compile-project)
   ("C-c g" . projectile-find-file))
  :bind-keymap
  ("C-c p" . projectile-command-map))

(use-package tide)

(use-package typescript-mode
  :init
  (add-hook 'typescript-mode-hook
            (lambda ()
              (tide-mode)
              (tide-restart-server)
              (company-mode))))

;;;; Built-in modes

(add-hook 'text-mode-hook 'my/common-text-hook)

;;;; Key bindings

;; MS NEK 4k bindings
(global-set-key (kbd "<XF86Forward>")
                (lambda ()
                  (interactive)
                  (select-window (next-window))))

(global-set-key (kbd "<XF86Back>")
                (lambda ()
                  (interactive)
                  (select-window (previous-window))))

(global-set-key (kbd "<C-XF86Back>")
                'previous-buffer)

(global-set-key (kbd "<C-XF86Forward>")
                'next-buffer)

(global-set-key (kbd "<XF86ApplicationRight>")
                'forward-sexp)

(global-set-key (kbd "<XF86ApplicationLeft>")
                'backward-sexp)

(global-set-key (kbd "<XF86Calculator>")
                'calc)

(global-set-key (kbd "<f1>") #'(lambda () (interactive)
                                 (find-file "~/mobile-sync/org/TODO.org")))

(global-set-key (kbd "C-c u")
                'browse-url)



(require 'ibuffer)
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; Windows & navigation

(global-set-key (kbd "C-c h")  'windmove-left)
(global-set-key (kbd "C-c l") 'windmove-right)
(global-set-key (kbd "C-c k")    'windmove-up)
(global-set-key (kbd "C-c j")  'windmove-down)

(global-set-key (kbd "C-c [")  'winner-undo)
(global-set-key (kbd "C-c ]")  'winner-redo)

;; Fun row

(global-set-key (kbd "<f2>") 'eshell)
(global-set-key (kbd "<f6>") 'my/switch-large-font)
(global-set-key (kbd "C-<f6>") 'my/go-dark)
(global-set-key (kbd "<f8>") 'highlight-thing-mode)
(global-set-key (kbd "<f10>") 'whitespace-mode)
(global-set-key (kbd "C-<f10>") 'my/whitespace-cleanup-switch)

;; British keyboard workarounds
(global-set-key (kbd "£") '(lambda () (interactive) (insert "#")))
(define-key isearch-mode-map (kbd "£")
  '(lambda () (interactive) (isearch-process-search-char ?\#)))

(push "~/.emacs.d/lisp" load-path)
(load-library "mermaid.el")

(load "~/.emacs-custom.el")

(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
