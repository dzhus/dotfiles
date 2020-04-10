(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(TeX-PDF-mode t)
 '(TeX-engine (quote xetex))
 '(ansi-color-faces-vector
   [default bold shadow italic underline bold bold-italic bold])
 '(auto-fill-inhibit-regexp nil)
 '(auto-insert-mode t)
 '(before-save-hook (quote (my/whitespace-cleanup)))
 '(bibtex-field-delimiters (quote double-quotes))
 '(browse-url-browser-function (quote browse-url-default-browser))
 '(calendar-week-start-day 1)
 '(coffee-tab-width 2)
 '(color-theme-illegal-default-attributes (quote (:height :width :family)))
 '(comint-password-prompt-regexp
   "\\(^ *\\|\\( SMB\\|'s\\|Bad\\|CVS\\|Enter\\(?: \\(?:\\(?:sam\\|th\\)e\\)\\)?\\|Kerberos\\|LDAP\\|New\\|Old\\|Repeat\\|UNIX\\|\\[sudo]\\|enter\\(?: \\(?:\\(?:sam\\|th\\)e\\)\\)?\\|login\\|new\\|old\\) +\\)\\(?:\\(?:adgangskode\\|contrase\\(?:\\(?:ny\\|ñ\\)a\\)\\|geslo\\|h\\(?:\\(?:asł\\|esl\\)o\\)\\|iphasiwedi\\|jelszó\\|l\\(?:ozinka\\|ösenord\\)\\|m\\(?:ot de passe\\|ật khẩu\\)\\|PIN\\|pa\\(?:rola\\|s\\(?:ahitza\\|s\\(?: phrase\\|code\\|ord\\|phrase\\|wor[dt]\\)\\|vorto\\)\\)\\|s\\(?:alasana\\|enha\\|laptažodis\\)\\|wachtwoord\\|лозинка\\|пароль\\|ססמה\\|كلمة السر\\|गुप्तशब्द\\|शब्दकूट\\|গুপ্তশব্দ\\|পাসওয়ার্ড\\|ਪਾਸਵਰਡ\\|પાસવર્ડ\\|ପ୍ରବେଶ ସଙ୍କେତ\\|கடவுச்சொல்\\|సంకేతపదము\\|ಗುಪ್ತಪದ\\|അടയാളവാക്ക്\\|රහස්පදය\\|ពាក្យសម្ងាត់\\|パスワード\\|密[码碼]\\|암호\\)\\|Response\\)\\(?:\\(?:, try\\)? *again\\| (empty for no passphrase)\\| (again)\\)?\\(?: for [^:：៖]+\\)?[:：៖]\\s *\\'")
 '(company-idle-delay 0.2)
 '(company-selection-wrap-around t)
 '(compilation-scroll-output (quote first-error))
 '(completion-on-separator-character t)
 '(custom-enabled-themes (quote (solarized-light)))
 '(custom-file "~/.emacs-custom.el")
 '(custom-safe-themes
   (quote
    ("2809bcb77ad21312897b541134981282dc455ccd7c14d74cc333b6e549b824f3" "c433c87bd4b64b8ba9890e8ed64597ea0f8eb0396f4c9a9e01bd20a04d15d358" "0598c6a29e13e7112cfbc2f523e31927ab7dce56ebb2016b567e1eff6dc1fd4f" "d91ef4e714f05fff2070da7ca452980999f5361209e679ee988e3c432df24347" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "a8245b7cc985a0610d71f9852e9f2767ad1b852c2bdea6f4aadc12cce9c4d6d0" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "4cf3221feff536e2b3385209e9b9dc4c2e0818a69a1cdb4b522756bcdf4e00a4" default)))
 '(dante-methods-alist
   (quote
    ((stack "stack.yaml"
            ("stack" "repl" dante-target)))))
 '(default-input-method "russian-computer")
 '(dired-dwim-target t)
 '(dired-listing-switches "-l")
 '(dired-recursive-deletes (quote always))
 '(doc-view-resolution 120)
 '(ediff-window-setup-function (quote ediff-setup-windows-plain))
 '(elpy-modules
   (quote
    (elpy-module-company elpy-module-eldoc elpy-module-sane-defaults)))
 '(eshell-output-filter-functions
   (quote
    (eshell-handle-ansi-color eshell-handle-control-codes eshell-watch-for-password-prompt eshell-truncate-buffer)))
 '(eshell-prompt-function
   (lambda nil
     (concat
      (car
       (last
        (split-string
         (eshell/pwd)
         "/")))
      (if
          (=
           (user-uid)
           0)
          " # " " $ "))))
 '(eshell-scroll-to-bottom-on-input (quote this))
 '(eshell-scroll-to-bottom-on-output (quote this))
 '(face-font-selection-order (quote (:weight :height :width :slant)))
 '(fancy-splash-image nil)
 '(fill-individual-varying-indent t)
 '(font-lock-global-modes t)
 '(fringe-mode (quote (12 . 0)) nil (fringe))
 '(graphviz-dot-auto-preview-on-save t)
 '(graphviz-dot-indent-width 4)
 '(ibuffer-mode-hook (quote (hl-line-mode)))
 '(ido-enable-flex-matching t)
 '(ido-mode (quote both) nil (ido))
 '(imenu-auto-rescan t)
 '(imenu-sort-function (quote imenu--sort-by-name))
 '(indent-tabs-mode nil)
 '(inferior-octave-program "octave-cli")
 '(ispell-extra-args (quote ("-d" "en_GB")))
 '(ispell-highlight-face (quote flyspell-incorrect))
 '(kill-ring-max 100)
 '(longlines-show-hard-newlines t)
 '(longlines-wrap-follows-window-size t)
 '(lua-indent-level 2)
 '(magit-status-sections-hook
   (quote
    (magit-insert-status-headers magit-insert-merge-log magit-insert-rebase-sequence magit-insert-am-sequence magit-insert-sequencer-sequence magit-insert-bisect-output magit-insert-bisect-rest magit-insert-bisect-log magit-insert-untracked-files magit-insert-unstaged-changes magit-insert-staged-changes magit-insert-stashes magit-insert-unpulled-from-upstream magit-insert-unpulled-from-pushremote magit-insert-unpushed-to-upstream magit-insert-unpushed-to-pushremote)))
 '(mail-user-agent (quote gnus-user-agent))
 '(make-backup-files nil)
 '(markdown-command "pandoc")
 '(markdown-fontify-code-blocks-natively t)
 '(markdown-reference-location (quote end))
 '(maxima-indent-style (quote perhaps-smart))
 '(menu-bar-mode nil)
 '(mermaid-options "-w 1200")
 '(mermaid-output-format (quote png))
 '(message-citation-line-format "%N wrote:")
 '(message-citation-line-function (quote message-insert-formatted-citation-line))
 '(message-directory "~/gnus/mail")
 '(message-send-mail-function (quote message-smtpmail-send-it))
 '(message-signature t)
 '(mouse-wheel-progressive-speed nil)
 '(mouse-wheel-scroll-amount (quote (2)))
 '(ns-command-modifier (quote meta))
 '(nxml-auto-insert-xml-declaration-flag t)
 '(org-agenda-files (quote ("~/org/tasks.org")))
 '(org-agenda-restore-windows-after-quit t)
 '(org-babel-load-languages (quote ((emacs-lisp . t) (latex . t))))
 '(org-export-ascii-underline (quote (61 45)))
 '(org-export-html-expand nil)
 '(org-export-with-section-numbers nil)
 '(org-fontify-done-headline t)
 '(org-hide-leading-stars t)
 '(org-log-done t)
 '(org-lowest-priority 68)
 '(org-preview-latex-default-process (quote imagemagick))
 '(org-todo-keywords (quote ("TODO" "IN PROGRESS" "DONE")))
 '(outline-regexp "[\\#\\*]+" t)
 '(package-archives
   (quote
    (("gnu" . "http://elpa.gnu.org/packages/")
     ("melpa" . "http://melpa.org/packages/")
     ("melpa-stable" . "http://melpa-stable.milkbox.net/packages/"))))
 '(package-selected-packages
   (quote
    (solarized-theme use-package dante sqlformat no-emoji tss hide-mode-line org-present tide ein projectile auctex graphviz-dot-mode lua-mode company highlight-thing idris-mode docker dockerfile-mode json-mode restclient terraform-mode yaml-mode counsel ido-ubiquitous markdown-mode magit haskell-mode)))
 '(paradox-github-token t)
 '(password-word-equivalents
   (quote
    ("password" "passcode" "passphrase" "pass phrase" "암호" "パスワード" "ପ୍ରବେଶ ସଙ୍କେତ" "ពាក្យសម្ងាត់" "adgangskode" "contraseña" "contrasenya" "geslo" "hasło" "heslo" "iphasiwedi" "jelszó" "lösenord" "lozinka" "mật khẩu" "mot de passe" "parola" "pasahitza" "passord" "passwort" "pasvorto" "salasana" "senha" "slaptažodis" "wachtwoord" "كلمة السر" "ססמה" "лозинка" "пароль" "गुप्तशब्द" "शब्दकूट" "પાસવર્ડ" "సంకేతపదము" "ਪਾਸਵਰਡ" "ಗುಪ್ತಪದ" "கடவுச்சொல்" "അടയാളവാക്ക്" "গুপ্তশব্দ" "পাসওয়ার্ড" "රහස්පදය" "密码" "密碼" "PIN")))
 '(po-auto-edit-with-msgid nil)
 '(pr-faces-p t)
 '(preview-scale-function 1.2)
 '(preview-transparent-border nil)
 '(preview-transparent-color "gray90")
 '(projectile-completion-system (quote ivy))
 '(projectile-git-command "git ls-files -zc --exclude-standard")
 '(projectile-global-mode t)
 '(projectile-mode t nil (projectile))
 '(projectile-use-git-grep t)
 '(python-indent-guess-indent-offset nil)
 '(restclient-content-type-modes
   (quote
    (("text/xml" . xml-mode)
     ("text/plain" . text-mode)
     ("application/xml" . xml-mode)
     ("application/json" . js-mode)
     ("image/png" . image-mode)
     ("image/jpeg" . image-mode)
     ("image/jpg" . image-mode)
     ("image/gif" . image-mode)
     ("text/html" . html-mode)
     ("application/yaml" . yaml-mode))))
 '(safe-local-variable-values
   (quote
    ((haskell-indentation-where-pre-offset . 4)
     (haskell-indentation-where-post-offset . 4)
     (haskell-indentation-starter-offset . 4)
     (haskell-indentation-left-offset . 4)
     (haskell-indentation-layout-offset . 4))))
 '(scroll-bar-mode nil)
 '(scroll-conservatively 1000)
 '(scroll-step 1)
 '(sentence-end-double-space nil)
 '(server-raise-frame t)
 '(server-window nil)
 '(sgml-basic-offset 2)
 '(show-paren-delay 0.3)
 '(show-paren-mode t)
 '(show-paren-style (quote mixed))
 '(smiley-regexp-alist nil)
 '(solarized-use-variable-pitch nil)
 '(sqlformat-command (quote pgformatter))
 '(tooltip-delay 0.4)
 '(tramp-chunksize 500)
 '(use-file-dialog nil)
 '(user-full-name "Dmitry Dzhus")
 '(user-mail-address "dima@dzhus.org")
 '(winner-mode t)
 '(woman-use-own-frame nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ein:cell-input-area ((t nil)))
 '(ein:cell-input-prompt ((t (:inherit header-line :weight bold))))
 '(ein:cell-output-area ((t (:inherit highlight))) t))
