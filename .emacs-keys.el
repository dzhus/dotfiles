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
                                 (find-file "~/Phone/Sync/org/TODO.org")))

;; Generic bindings

(global-set-key (kbd "C-c u")
                'browse-url)

(add-hook 'markdown-mode-hook #'(lambda ()
                                  (define-key markdown-mode-map "\C-ca"
                                    'auto-fill-mode)))

(require 'ibuffer)
(global-set-key (kbd "C-x C-b")
                'ibuffer)

;; Windows & navigation

(global-set-key (kbd "C-c h")  'windmove-left)
(global-set-key (kbd "C-c l") 'windmove-right)
(global-set-key (kbd "C-c k")    'windmove-up)
(global-set-key (kbd "C-c j")  'windmove-down)

(global-set-key (kbd "C-c [")  'winner-undo)
(global-set-key (kbd "C-c ]")  'winner-redo)

;; Fun row

(global-set-key (kbd "<f2>") 'eshell)

(global-set-key (kbd "<f5>") 'magit-status)

(global-set-key (kbd "<f6>") 'my/switch-large-font)

(global-set-key (kbd "<f7>") 'flycheck-mode)

(global-set-key (kbd "<f8>") 'highlight-thing-mode)

(global-set-key (kbd "<f10>") 'whitespace-mode)

(global-set-key (kbd "C-<f10>") 'my/whitespace-cleanup-switch)

(global-set-key (kbd "<f12>") 'projectile-compile-project)

;; MBP Touch bar workarounds

(global-set-key (kbd "C-5") 'magit-status)

;; British keyboard workarounds
(global-set-key (kbd "£") '(lambda () (interactive) (insert "#")))
(define-key isearch-mode-map (kbd "£")
  '(lambda () (interactive) (isearch-process-search-char ?\#)))
(global-set-key (kbd "M-c") 'kill-ring-save) ;; ? why do I need this 2018/07 ?

;; Ivy

(global-set-key (kbd "M-x") 'counsel-M-x)

(global-set-key (kbd "C-c j") 'counsel-git-grep)

;; Projectile

(global-set-key (kbd "C-c g") 'projectile-find-file)

(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

;; C-x v l is actually better than magit's log because in VC log entry
;; diffs contain only changes to the file
;;
;; (global-set-key
;;  (kbd "C-x v l")
;;  #'(lambda ()
;;      (interactive)
;;      (if (eq 'Git (vc-backend (buffer-file-name (current-buffer))))
;;          (magit-log-buffer-file)
;;        (vc-print-log))))

(require 'magit)
(global-set-key (kbd "C-x v =") 'magit-diff-buffer-file)
