; -*- lexical-binding: t -*-

;; Bootstrap use-package (from
;; https://github.com/jwiegley/use-package/issues/313)
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))

(package-initialize)

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

;;;; Custom helpers and commands

(defun my/common-text-hook ()
  (turn-on-auto-fill))

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

;;;; Packages

(require 'use-package-ensure)
(setq use-package-always-ensure t)

(use-package company
  :hook ((haskell-mode . company-mode)
         (cider-mode . company-mode)
         (cider-repl-mode . company-mode)
         (racket-mode . company-mode)
         (racket-repl-mode . company-mode)))

(use-package counsel
  :bind
  (("C-c f" . counsel-git-grep))
  :delight
  :init (counsel-mode 1))

(use-package cider
  :hook (clojure-mode . cider-mode))

(use-package clojure-mode)
(use-package delight)
(use-package docker)
(use-package dockerfile-mode)
(use-package ein)
(use-package exec-path-from-shell)

(use-package flycheck
  :after haskell-mode
  :bind (("<f7>" . flycheck-mode))
  :hook (haskell-mode . flycheck-mode))

(use-package git-link)
(use-package graphviz-dot-mode)
(use-package hide-mode-line)
(use-package highlight-thing)
(use-package idris-mode)

(use-package ivy
  :init (ivy-mode 1)
  :delight)

(use-package json-mode)

(use-package lsp-mode
  :hook (haskell-mode . lsp)
  :commands lsp)

(use-package lsp-haskell)
(use-package lua-mode)

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

(use-package markdown-toc)
(use-package no-emoji)

(use-package org
  :init
  (add-hook 'org-mode-hook 'my/common-text-hook))

(use-package origami)

(use-package projectile
  :bind
  (("<f12>" . projectile-compile-project)
   ("C-c g" . projectile-find-file))
  :bind-keymap
  ("C-c p" . projectile-command-map))

(use-package racket-mode)
(use-package restclient)
(use-package solarized-theme)

(use-package swiper
  :bind (("C-s" . swiper-isearch)))

(use-package sqlformat)
(use-package terraform-mode)

(use-package tex
  :ensure auctex)

(use-package yaml-mode)

;;;; Built-in modes

(add-hook 'text-mode-hook 'my/common-text-hook)

(add-hook 'eshell-mode-hook
          #'(lambda ()
              (local-set-key (kbd "M-r") 'counsel-esh-history)))

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

;; (push "~/.emacs.d/lisp" load-path)
;; (load-library "mermaid.el")


;; Fix $PATH
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

;; Load .emacs-custom from the same directory as this file (this is to
;; make it work on CI)
(load (expand-file-name ".emacs-custom.el"
                        (when load-file-name
                          (file-name-directory load-file-name))))

(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

;;;; Lights out at 6PM

(defvar my/theme (car custom-enabled-themes) "Last theme loaded with Custom or `my/change-sides'.")

(defun my/change-sides ()
  (interactive)
  (setq my/theme (cond ((and (boundp 'my/theme) (not (eq my/theme 'solarized-dark))) 'solarized-dark)
                       (t 'solarized-light)))
  (load-theme my/theme))

(defun my/theme-of-hour ()
  "Load dark theme between 6PM and 6AM."
  (interactive)
  (let* ((hour (elt (parse-time-string (current-time-string)) 2))
         (theme (if (or (>= hour 18) (< hour 6))
                    'solarized-dark
                  'solarized-light)))
    (when (not (eq my/theme theme))
      (setq my/theme theme)
      (load-theme my/theme))))

(run-at-time "10 sec" 10 #'my/theme-of-hour)
