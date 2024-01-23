(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(TeX-PDF-mode t)
 '(TeX-engine 'xetex)
 '(ansi-color-faces-vector
   [default bold shadow italic underline bold bold-italic bold])
 '(auto-fill-inhibit-regexp nil)
 '(auto-insert-mode t)
 '(auto-revert-mode-text "")
 '(before-save-hook
   '(my/whitespace-cleanup tide-format-before-save markdown-toc-refresh-toc))
 '(bibtex-field-delimiters 'double-quotes)
 '(browse-url-browser-function 'browse-url-default-browser)
 '(calendar-week-start-day 1)
 '(cider-lein-parameters "with-profile -user repl :headless :host localhost")
 '(cider-prompt-for-symbol nil)
 '(cider-repl-pop-to-buffer-on-connect 'display-only)
 '(clojure-align-forms-automatically t)
 '(clojure-indent-style 'always-align)
 '(coffee-tab-width 2)
 '(color-theme-illegal-default-attributes '(:height :width :family))
 '(company-idle-delay 0.2)
 '(company-selection-wrap-around t)
 '(compilation-scroll-output 'first-error)
 '(completion-on-separator-character t)
 '(custom-enabled-themes '(solarized-light))
 '(custom-file "~/.emacs-custom.el")
 '(custom-safe-themes
   '("2809bcb77ad21312897b541134981282dc455ccd7c14d74cc333b6e549b824f3" "c433c87bd4b64b8ba9890e8ed64597ea0f8eb0396f4c9a9e01bd20a04d15d358" "0598c6a29e13e7112cfbc2f523e31927ab7dce56ebb2016b567e1eff6dc1fd4f" "d91ef4e714f05fff2070da7ca452980999f5361209e679ee988e3c432df24347" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "a8245b7cc985a0610d71f9852e9f2767ad1b852c2bdea6f4aadc12cce9c4d6d0" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "4cf3221feff536e2b3385209e9b9dc4c2e0818a69a1cdb4b522756bcdf4e00a4" default))
 '(dante-methods-alist '((stack "stack.yaml" ("stack" "repl" dante-target))))
 '(default-input-method "russian-computer")
 '(dired-dwim-target t)
 '(dired-listing-switches "-l")
 '(dired-recursive-deletes 'always)
 '(doc-view-resolution 120)
 '(ediff-merge-split-window-function 'split-window-vertically)
 '(ediff-window-setup-function 'ediff-setup-windows-plain)
 '(elpy-modules
   '(elpy-module-company elpy-module-eldoc elpy-module-sane-defaults))
 '(eshell-history-size 1024)
 '(eshell-output-filter-functions
   '(eshell-handle-ansi-color eshell-handle-control-codes eshell-watch-for-password-prompt eshell-truncate-buffer))
 '(eshell-scroll-to-bottom-on-input 'this)
 '(eshell-scroll-to-bottom-on-output 'this)
 '(face-font-selection-order '(:weight :height :width :slant))
 '(fancy-splash-image nil)
 '(fill-individual-varying-indent t)
 '(font-lock-global-modes t)
 '(fringe-mode '(12 . 0) nil (fringe))
 '(git-link-use-commit t)
 '(graphviz-dot-auto-preview-on-save t)
 '(graphviz-dot-indent-width 4)
 '(ibuffer-mode-hook '(hl-line-mode))
 '(idris-interpreter-path "idris2")
 '(imenu-auto-rescan t)
 '(imenu-sort-function 'imenu--sort-by-name)
 '(indent-tabs-mode nil)
 '(inferior-octave-program "octave-cli")
 '(ispell-extra-args '("-d" "en_GB"))
 '(ispell-highlight-face 'flyspell-incorrect)
 '(kill-ring-max 100)
 '(longlines-show-hard-newlines t)
 '(longlines-wrap-follows-window-size t)
 '(lsp-haskell-process-args-hie nil)
 '(lsp-haskell-process-path-hie "ghcide")
 '(lsp-haskell-server-path "haskell-language-server-wrapper")
 '(lua-indent-level 2)
 '(magit-status-sections-hook
   '(magit-insert-status-headers magit-insert-merge-log magit-insert-rebase-sequence magit-insert-am-sequence magit-insert-sequencer-sequence magit-insert-bisect-output magit-insert-bisect-rest magit-insert-bisect-log magit-insert-untracked-files magit-insert-unstaged-changes magit-insert-staged-changes magit-insert-stashes magit-insert-unpulled-from-upstream magit-insert-unpulled-from-pushremote magit-insert-unpushed-to-upstream magit-insert-unpushed-to-pushremote))
 '(mail-user-agent 'gnus-user-agent)
 '(make-backup-files nil)
 '(markdown-command "pandoc")
 '(markdown-fontify-code-blocks-natively t)
 '(markdown-reference-location 'end)
 '(maxima-indent-style 'perhaps-smart)
 '(menu-bar-mode nil)
 '(mermaid-options "-w 1200")
 '(mermaid-output-format 'png)
 '(message-citation-line-format "%N wrote:")
 '(message-citation-line-function 'message-insert-formatted-citation-line)
 '(message-directory "~/gnus/mail")
 '(message-send-mail-function 'message-smtpmail-send-it)
 '(message-signature t)
 '(mouse-wheel-progressive-speed nil)
 '(mouse-wheel-scroll-amount '(2))
 '(ns-command-modifier 'meta)
 '(nxml-auto-insert-xml-declaration-flag t)
 '(org-agenda-files '("~/org/tasks.org"))
 '(org-agenda-restore-windows-after-quit t)
 '(org-babel-load-languages '((emacs-lisp . t) (latex . t)))
 '(org-export-ascii-underline '(61 45))
 '(org-export-html-expand nil)
 '(org-export-with-section-numbers nil)
 '(org-fontify-done-headline t)
 '(org-hide-leading-stars t)
 '(org-log-done t)
 '(org-preview-latex-default-process 'imagemagick)
 '(org-priority-lowest 68)
 '(org-todo-keywords '("TODO" "IN PROGRESS" "DONE"))
 '(outline-regexp "[\\#\\*]+" t)
 '(package-selected-packages
   '(tide typescript-mode plantuml-mode parinfer-rust-mode parinfer forge csv-mode edit-server flycheck-clj-kondo company git-link origami json-mode flycheck lsp-haskell lsp exec-path-from-shells yaml-mode use-package terraform-mode sqlformat solarized-theme restclient racket-mode projectile no-emoji markdown-toc magit idris-mode highlight-thing hide-mode-line graphviz-dot-mode ein dockerfile-mode delight counsel cider auctex))
 '(paradox-github-token t)
 '(plantuml-default-exec-mode 'executable)
 '(po-auto-edit-with-msgid nil)
 '(pr-faces-p t)
 '(preview-scale-function 1.2)
 '(preview-transparent-border nil)
 '(preview-transparent-color "gray90")
 '(projectile-completion-system 'ivy)
 '(projectile-git-command "git ls-files -zc --exclude-standard")
 '(projectile-git-submodule-command "")
 '(projectile-global-mode t)
 '(projectile-mode t nil (projectile))
 '(projectile-mode-line-prefix " ")
 '(projectile-use-git-grep t)
 '(python-indent-guess-indent-offset nil)
 '(restclient-content-type-modes
   '(("text/xml" . xml-mode)
     ("text/plain" . text-mode)
     ("application/xml" . xml-mode)
     ("application/json" . js-mode)
     ("image/png" . image-mode)
     ("image/jpeg" . image-mode)
     ("image/jpg" . image-mode)
     ("image/gif" . image-mode)
     ("text/html" . html-mode)
     ("application/yaml" . yaml-mode)))
 '(safe-local-variable-values
   '((haskell-indentation-where-pre-offset . 4)
     (haskell-indentation-where-post-offset . 4)
     (haskell-indentation-starter-offset . 4)
     (haskell-indentation-left-offset . 4)
     (haskell-indentation-layout-offset . 4)))
 '(scroll-bar-mode nil)
 '(scroll-conservatively 1000)
 '(scroll-step 1)
 '(sentence-end-double-space nil)
 '(server-raise-frame t)
 '(server-window nil)
 '(sgml-basic-offset 2)
 '(show-paren-delay 0.3)
 '(show-paren-mode t)
 '(show-paren-style 'mixed)
 '(smiley-regexp-alist nil)
 '(solarized-use-variable-pitch nil)
 '(sqlformat-command 'pgformatter)
 '(tool-bar-mode nil)
 '(tooltip-delay 0.4)
 '(tramp-chunksize 500)
 '(typescript-indent-level 2)
 '(use-file-dialog nil)
 '(use-package-always-ensure t)
 '(user-full-name "Dmitry Dzhus")
 '(user-mail-address "dima@dzhus.org")
 '(winner-mode t)
 '(woman-use-own-frame nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(antlr-ruledef ((t (:inherit font-lock-function-name-face :weight bold))))
 '(antlr-ruleref ((t (:inherit font-lock-function-name-face))))
 '(antlr-syntax ((t (:inherit font-lock-builtin-face :weight bold))))
 '(antlr-tokendef ((t (:inherit font-lock-type-face :weight bold))))
 '(antlr-tokenref ((t (:inherit font-lock-type-face))))
 '(ein:cell-input-area ((t nil)))
 '(ein:cell-input-prompt ((t (:inherit header-line :weight bold))))
 '(ein:cell-output-area ((t (:inherit highlight))) t))
