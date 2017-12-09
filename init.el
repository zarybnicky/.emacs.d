; Basic Environment
(server-start)

(require 'package)

(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (url (concat (if no-ssl "http" "https") "://melpa.org/packages/")))
  (add-to-list 'package-archives (cons "melpa" url) t))
(add-to-list 'package-archives
             '("gnu" . "http://elpa.gnu.org/packages/"))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)

(setq my-packages
      '(ag
        auctex
        auto-complete
        column-marker
        expand-region
        flx
        flx-ido
        flycheck
        fuzzy
        helm
        projectile
        pretty-symbols
        rainbow-delimiters
        sicp
        smex
        smart-mode-line
        solarized-theme
        typing
        unbound
        yasnippet))

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

                                        ; Aesthetics
(setq inhibit-splash-screen t)
(setq inhibit-startup-message t)

(tool-bar-mode 0)
(menu-bar-mode 0)
(scroll-bar-mode 0)

(setq-default indent-tabs-mode nil)
(setq-default show-trailing-whitespace t)
(setq-default indicate-empty-lines t)
;;(add-hook 'before-save-hook 'delete-trailing-whitespace)

(setq line-number-mode t)
(setq column-number-mode t)
(setq visible-bell t)
(blink-cursor-mode 0)
(transient-mark-mode 1)
(setq-default cursor-type 'box)
(global-font-lock-mode 1)
(setq font-lock-maximum-decoration t)
(show-paren-mode t)
(defalias 'yes-or-no-p 'y-or-n-p)
(setq confirm-kill-emacs 'yes-or-no-p)

(setq frame-title-format
      '("Emacs - " (buffer-file-name "%f" (dired-directory dired-directory "%b"))))
(setq custom-safe-themes t)
(load-theme 'solarized-dark t)

(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(load-library "iso-transl")

(use-package powerline
    :ensure t
    :defer 1
    :config
    (sml/setup)
    (sml/apply-theme 'respectful))

                                        ; Aids
(use-package ido
    :ensure t
    :config
    (ido-mode 1)
    (ido-everywhere 1)
    (setq ido-max-directory-size 100000)
    (setq ido-use-faces nil)
    (setq gc-cons-threshold 20000000)
    (use-package ido-completing-read+
        :ensure t
        :config (ido-ubiquitous-mode 1))
    (use-package flx-ido
        :ensure t
        :config (flx-ido-mode 1))
    (use-package smex
        :ensure t
        :bind (("M-x" . smex)
               ("M-X" . smex-major-mode-commands))
        :config
        (smex-initialize)))

                                        ; guide-key
(use-package guide-key
    :ensure t
    :diminish guide-key-mode
    :config
    (setq guide-key/guide-key-sequence t)
    (setq guide-key/recursive-key-sequence-flag t)
    (guide-key-mode 1))

(use-package discover
    :ensure t
    :config (global-discover-mode 1))

                                        ; desktop
(use-package desktop
    :ensure t
    :config
    (setq desktop-restore-frames t)
    (desktop-save-mode 1))

(use-package bookmark+
    :ensure t
    :defer 2
    :config
    (setq bmkp-desktop-jump-save-before-flag t)
    (add-hook 'kill-emacs-hook 'bmkp-desktop-save-as-last))

(require 'uniquify)

                                        ; autosave
(setq
 backup-by-copying t
 backup-directory-alist
 (cons (cons "." (concat user-emacs-directory "saves")) [])
 delete-old-versions t
 kept-new-versions 6
 kept-old-versions 2
 version-control t)

                                        ; moccur
(use-package color-moccur
    :ensure t
    :commands (isearch-moccur isearch-all)
    :bind (("M-s O" . moccur)
           :map isearch-mode-map
           ("M-o" . isearch-moccur)
           ("M-O" . isearch-moccur-all))
    :init
    (setq isearch-lazy-highlight t)
    :config
    (use-package moccur-edit
        :ensure t))

                                        ; Global shortcuts
(global-set-key
 (kbd "<f5>")
 'compile)

(global-set-key
 (kbd "<f6>")
 (lambda ()
   (interactive)
   (find-file "~/.emacs.d/init.el")))

(global-set-key
 (kbd "<f11>")
 'toggle-frame-fullscreen)

(global-set-key
 (kbd "<f12>")
 (lambda ()
   (interactive)
   (revert-buffer nil t nil)))

                                        ; Prog mode hooks
(use-package rainbow-delimiters
    :ensure t
    :config (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

                                        ; Auto fill with column-marker
(setq-default fill-column 80)
(add-hook 'prog-mode-hook 'turn-on-auto-fill)
(require 'column-marker)
(set-face-background 'column-marker-1 "red")
(add-hook 'prog-mode-hook
          (lambda ()
            (interactive)
            (column-marker-1 fill-column)))

                                        ; Magit
(use-package magit
    :ensure t
    :bind (("C-x M-g" . magit-dispatch-popup)
           ("C-x g" . magit-status)))

                                        ; Auto fill
(add-hook 'text-mode-hook 'turn-on-auto-fill)
(setq-default fill-column 80)

                                        ; Dired - Find marked files
(eval-after-load "dired"
  '(progn
    (define-key dired-mode-map "F" 'my-dired-find-file)
    (defun my-dired-find-file (&optional arg)
      "Open each of the marked files, or the file under the point, or when prefix arg, the next N files "
      (interactive "P")
      (let* ((fn-list (dired-get-marked-files nil arg)))
        (mapc 'find-file fn-list)))))

                                        ; Python
(use-package python
    :mode ("\\.py\\'" . python-mode)
    :interpreter ("python" . python-mode)
    :ensure t
    :config
    (add-hook 'python-mode-hook 'flycheck-mode)
    (add-hook 'python-mode-hook 'pretty-symbols-mode)
    (use-package jedi
        :ensure t
        :config
        (jedi:setup)
        (setq jedi:complete-on-dot t)))

                                        ; PHP
(defun setup-ts ()
    (tide-setup)
    (flycheck-mode +1)
    (setq typescript-indent-level 2)
    (setq flycheck-check-syntax-automatically '(save mode-enabled))
    (eldoc-mode +1)
    (company-mode +1)
    (add-hook 'before-save-hook 'tide-format-before-save)
    (setq company-tooltip-align-annotations t)
    (tide-hl-identifier-mode))

(use-package web-mode
    :ensure t
    :mode "\\.inc\\'" "\\.html?\\'" "\\.php\\'" "\\.tsx\\'"
    :init
    (setq web-mode-engines-alist '(("php" . "\\.inc\\'")))
    (setq web-mode-enable-block-face t)
    (setq web-mode-enable-part-face t)
    (setq web-mode-enable-css-colorization t)
    (use-package flycheck
      :config
      (flycheck-add-mode 'php 'web-mode)
      (flycheck-add-mode 'php-phpcs 'web-mode)
      (flycheck-add-mode 'php-phpmd 'web-mode)
      (flycheck-add-mode 'typescript-tslint 'web-mode))
    (add-hook 'web-mode-hook 'flycheck-mode)
    (add-hook 'web-mode-hook 'projectile-mode)
    (add-hook 'web-mode-hook
          (lambda ()
            (when (string-equal "tsx" (file-name-extension buffer-file-name))
              (setup-ts)))))

(use-package js-mode
  :mode "\\.js\\'"
  :init
  (add-hook 'js-mode-hook 'flycheck-mode)
  (add-hook 'js-mode-hook 'projectile-mode)
  (setq-default js-switch-indent-offset 2))

(use-package json-mode
  :ensure t
  :mode "\\.json\\'")

(use-package yaml-mode
  :ensure t
  :mode "\\.json\\'")

(use-package nix-mode
  :ensure t
  :mode "\\.nix\\'")

(use-package tide
  :ensure t
  :mode ("\\.ts\\'" . typescript-mode)
  :init
  (setq-default typescript-indent-level 2)
  (setq-default typescript-expr-indent-offset 2)
  :config
  (add-hook 'typescript-mode-hook 'setup-ts))

(setq-default flycheck-checker-error-threshold 2000)

(use-package php-mode
    :ensure t
;;    :mode ("\\.php" . php-mode)
    :bind (:map php-mode-map
                ("C-c <left>"  . hs-hide-block)
                ("C-c <right>" . hs-show-block)
                ("C-c <up>"    . hs-hide-all)
                ("C-c <down>"  . hs-show-all))
    :init
    (setq-default php-mode-coding-style 'symfony2)
    (setq-default flycheck-phpmd-rulesets (list (concat user-emacs-directory "phpmd.xml")))
    (setq-default flycheck-phpcs-standard (concat user-emacs-directory "phpcs.xml"))
    (setq-default flycheck-phpcs-standard "PSR2")
    :config
    (use-package hideshow
        :diminish)
    (add-hook 'php-mode-hook 'flycheck-mode)
    (add-hook 'php-mode-hook 'projectile-mode)
    (add-hook 'php-mode-hook (lambda () (hs-minor-mode t))))

(setq-default nxml-child-indent 4)

(require 'projectile)
(setq-default projectile-tags-command "ctags-exuberant -Re -f %s %s")
(add-to-list 'projectile-globally-ignored-directories "build")

                                        ; AUCTeX
(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq TeX-PDF-mode t)
(setq-default TeX-master nil)
(add-hook 'LaTeX-mode-hook 'auto-fill-mode)
(add-hook 'LaTeX-mode-hook 'flyspell-mode)
(add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
(setq reftex-plug-into-AUCTeX t)

                                        ; Haskell
(use-package haskell-mode
    :ensure t
    :mode ("\\.hs" . haskell-mode)
    :init
    (add-hook 'haskell-mode-hook 'projectile-mode)
    (add-hook 'haskell-mode-hook 'flycheck-mode)
    (add-hook 'haskell-mode-hook 'dante-mode)
    (add-hook 'haskell-mode-hook 'hindent-mode)
    ;; (use-package intero
    ;;   :ensure t
    ;;   :commands 'intero-mode
    ;;   :config
    ;;   (flycheck-add-next-checker 'intero '(warning . haskell-hlint)))
    (use-package dante
      :ensure t
      :commands 'dante-mode
      :config
      (add-to-list 'flycheck-checkers 'haskell-dante 'append))
    (use-package hindent
      :ensure t
      :commands 'hindent-mode))

(define-prefix-command 'haskell-extra-map)
(global-set-key (kbd "C-c b") 'haskell-extra-map)

(use-package hasky-extensions
  :ensure t
  :bind ("C-c b y" . hasky-extensions))

(use-package hasky-stack
  :ensure t
  :bind (("C-c b i" . hasky-stack-new)
         ("C-c b s" . hasky-stack-execute)))

(use-package hindent
    :defer t
    :init
    (add-hook 'haskell-mode-hook #'hindent-mode)
    (setq-default hindent-style "johan-tibell"))

                                        ; Autocomplete
(require 'auto-complete-config)
(ac-config-default)
(global-auto-complete-mode)

                                        ; Undotree
(use-package undo-tree
    :ensure t
    :diminish undo-tree-mode
    :config (global-undo-tree-mode))

(use-package ace-jump-mode
    :ensure t
    :diminish ace-jump-mode
    :config
    (define-key global-map (kbd "C-c SPC") 'ace-jump-mode)
    (ace-jump-mode-enable-mark-sync)
    (define-key global-map (kbd "C-x SPC") 'ace-jump-mode-pop-mark)
    (setq ace-jump-mode-submode-list
          '(ace-jump-line-mode
            ace-jump-word-mode
            ace-jump-char-mode)))

                                        ; Helm
(require 'helm-config)

(global-set-key (kbd "C-c h") 'helm-command-prefix)
(global-unset-key (kbd "C-x c"))

(define-key helm-command-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to run persistent action
(define-key helm-command-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB works in terminal
(define-key helm-command-map (kbd "C-z")  'helm-select-action) ; list actions using C-z

(setq helm-quick-update                     t ; do not display invisible candidates
      helm-split-window-in-side-p           t ; open helm buffer inside current window, not occupy whole other window
      helm-buffers-fuzzy-matching           t ; fuzzy matching buffer names when non--nil
      helm-move-to-line-cycle-in-source     t ; move to end or beginning of source when reaching top or bottom of source.
      helm-ff-search-library-in-sexp        t ; search for library in `require' and `declare-function' sexp.
      helm-scroll-amount                    8 ; scroll 8 lines other window using M-<next>/M-<prior>
      helm-ff-file-name-history-use-recentf t)

(helm-mode 1)

                                        ; Fuzzy in-file search
(require 'fuzzy)
(turn-on-fuzzy-isearch)

                                        ; Windmove
(defun ignore-error-wrapper (fn)
  "Funtion return new function that ignore errors.
     The function wraps a function with `ignore-errors' macro."
  (lexical-let ((fn fn))
    (lambda ()
      (interactive)
      (ignore-errors
        (funcall fn)))))

(global-set-key (kbd "S-<left>") (ignore-error-wrapper 'windmove-left))
(global-set-key (kbd "S-<right>") (ignore-error-wrapper 'windmove-right))
(global-set-key (kbd "S-<up>") (ignore-error-wrapper 'windmove-up))
(global-set-key (kbd "S-<down>") (ignore-error-wrapper 'windmove-down))

                                        ; Expand-region
(use-package expand-region
    :ensure t
    :config (global-set-key (kbd "C-=") 'er/expand-region))

                                        ; Editorconfig
(use-package editorconfig
    :ensure t
    :diminish editorconfig-mode
    :init
    (add-hook 'prog-mode-hook (editorconfig-mode 1))
    (add-hook 'text-mode-hook (editorconfig-mode 1)))

                                        ; C
(add-hook 'c-mode-common-hook 'flycheck-mode)
(add-hook 'c-mode-hook
          (lambda () (setq flycheck-gcc-language-standard "gnu99")))

(use-package slime
    :defer t
    :init (setq inferior-lisp-program "sbcl")
    :commands (lisp-mode))

(defun unfill-paragraph (&optional region)
  "Takes a multi-line paragraph and makes it into a single line of text."
  (interactive (progn (barf-if-buffer-read-only) '(t)))
  (let ((fill-column (point-max))
        (emacs-lisp-docstring-fill-column t))
    (fill-paragraph nil region)))
(define-key global-map "\M-Q" 'unfill-paragraph)

;; Open files in Docker containers like so: /docker:drunk_bardeen:/etc/passwd
(require 'tramp)
(push
 (cons
  "docker"
  '((tramp-login-program "docker")
    (tramp-login-args (("exec" "-it") ("%h") ("/bin/bash")))
    (tramp-remote-shell "/bin/sh")
    (tramp-remote-shell-args ("-i") ("-c"))))
 tramp-methods)

(defadvice tramp-completion-handle-file-name-all-completions
  (around dotemacs-completion-docker activate)
  "(tramp-completion-handle-file-name-all-completions \"\" \"/docker:\" returns
    a list of active Docker container names, followed by colons."
  (if (equal (ad-get-arg 1) "/docker:")
      (let* ((dockernames-raw (shell-command-to-string "docker ps | awk '$NF != \"NAMES\" { print $NF \":\" }'"))
             (dockernames (cl-remove-if-not
                           #'(lambda (dockerline) (string-match ":$" dockerline))
                           (split-string dockernames-raw "\n"))))
        (setq ad-return-value dockernames))
    ad-do-it))

(defun sudo-edit (&optional arg)
  "Edit currently visited file as root.

With a prefix ARG prompt for a file to visit.
Will also prompt for a file to visit if current
buffer is not visiting a file."
  (interactive "P")
  (if (or arg (not buffer-file-name))
      (find-file (concat "/sudo:root@localhost:"
                         (ido-read-file-name "Find file(as root): ")))
    (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))

(global-set-key (kbd "C-x C-r") 'sudo-edit)

(use-package flyspell
  :ensure t
  :commands (flyspell-mode flyspell-prog-mode)
  :init
  ;; (remove-hook 'prog-mode-hook 'flyspell-prog-mode)
  ;; (add-hook 'text-mode-hook 'flyspell-mode)
  :config
  (cond
   ((executable-find "aspell")
    (setq ispell-program-name "aspell"
          ispell-personal-dictionary "/Program Files (x86)/Aspell"
          ispell-extra-args '("--sug-mode=ultra")))
   ((executable-find "hunspell")
    (setq ispell-program-name "hunspell")))
  (with-eval-after-load 'auto-complete
    (ac-flyspell-workaround))

  (let ((langs '("czech" "english")))
    (setq lang-ring (make-ring (length langs)))
    (dolist (elem langs) (ring-insert lang-ring elem)))
  (defun cycle-ispell-languages ()
    (interactive)
    (let ((lang (ring-ref lang-ring -1)))
      (ring-insert lang-ring lang)
      (ispell-change-dictionary lang)))
  (global-set-key [f7] 'cycle-ispell-languages))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(bmkp-last-as-first-bookmark-file "~\\.emacs.d\\bookmarks")
 '(package-selected-packages
   (quote
    (markdown-mode yaml-mode flycheck-yamllint tide shakespeare-mode hledger-mode alert mediawiki intero hindent dante hasky-stack hasky-extensions haskell-mode yasnippet web-mode vimgolf use-package undo-tree unbound typing solarized-theme smex smart-mode-line sicp rainbow-delimiters projectile pretty-symbols powerline php-mode magit js2-mode jedi ido-completing-read+ helm guide-key fuzzy flycheck flx-ido expand-region editorconfig discover column-marker color-moccur bookmark+ auctex ag ace-jump-mode)))
 '(safe-local-variable-values
   (quote
    ((eval progn
           (add-to-list
            (quote exec-path)
            (concat
             (locate-dominating-file default-directory ".dir-locals.el")
             "node_modules/.bin/")))
     (intero-project-root . "d:/projects/starlet")
     (haskell-process-use-ghci . t)
     (haskell-indent-spaces . 4)
     (intero-project-root . "d:/projects/hledger")
     (dante-project-root . "d:/projects/hledger")
     (dante-project-root . d:\\projects\\hledger)
     (intero-project-root . "d:/projects/tdd/haskell-calc-kata")
     (dante-project-root . "d:/projects/tdd/haskell-calc-kata")
     (intero-project-root . "d:/projects/tdd/haskell-calc")
     (dante-project-root . "d:/projects/tdd/haskell-calc")
     (dante-project-root . "d:/projects/opinion-extraction")
     (intero-project-root . "d:/projects/ner-czech")
     (dante-project-root . "d:/projects/ner-czech")))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
