;; MELPA package manager
(require 'package)
(add-to-list 'package-archives
  '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)

;; load git-commit (auto)
(unless (package-installed-p 'git-commit-mode)
  (package-refresh-contents) (package-install 'git-commit-mode))
(require 'git-commit-mode) 
(add-hook 'git-commit-mode-hook 'turn-on-flyspell)

;;org mode (auto)
(unless (package-installed-p 'org)
  (package-refresh-contents) (package-install 'org))
(require 'org)
;start git rebase in org-mode
(setq auto-mode-alist (cons '("git-rebase-todo" . org-mode) auto-mode-alist))
;activate graphiz in org-mode
(org-babel-do-load-languages
 'org-babel-load-languages
 '((dot . t))) ; this line activates dot
(setq org-confirm-babel-evaluate nil) ;turn off code execution confirmation

;; scala-mode2 (auto)
(unless (package-installed-p 'scala-mode2)
  (package-refresh-contents) (package-install 'scala-mode2))
(require 'scala-mode2)

;; color-theme-solarized (auto)
;(unless (package-installed-p 'color-theme-solarized)
  ;(package-refresh-contents) (package-install 'color-theme-solarized))
;(load-theme 'solarized t)
;(set-frame-parameter nil 'background-mode 'dark)

;; solarized-theme (auto)
;(unless (package-installed-p 'solarized-theme)
;  (package-refresh-contents) (package-install 'solarized-theme))
;(load-theme 'solarized-dark t)

;; zenburn-theme (auto)
(unless (package-installed-p 'zenburn-theme)
  (package-refresh-contents) (package-install 'zenburn-theme))
(load-theme 'zenburn t)

;;ensime (auto)
(unless (package-installed-p 'ensime)
  (package-refresh-contents) (package-install 'ensime))
(require 'ensime)
(add-hook 'scala-mode-hook 'ensime-scala-mode-hook)

;;magit (auto)
(unless (package-installed-p 'magit)
  (package-refresh-contents) (package-install 'magit))
(require 'magit)

;;helm (auto)
(unless (package-installed-p 'helm)
  (package-refresh-contents) (package-install 'helm))
(require 'helm-config)
(global-set-key (kbd "C-c h") 'helm-mini)
(global-set-key (kbd "C-x C-l") 'helm-locate)
(helm-mode 1)

;; graphviz-dot-mode (auto)
(unless (package-installed-p 'graphviz-dot-mode)
  (package-refresh-contents) (package-install 'graphviz-dot-mode))
(require 'graphviz-dot-mode)

;; markdown-mode (auto)
(unless (package-installed-p 'markdown-mode)
  (package-refresh-contents) (package-install 'markdown-mode))
(require 'markdown-mode)
(autoload 'markdown-mode "markdown-mode"
   "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

;;Java Development
(semantic-mode 1)

;; pretty print xml
(defun bf-pretty-print-xml-region (begin end)
  "Pretty format XML markup in region. You need to have nxml-mode
http://www.emacswiki.org/cgi-bin/wiki/NxmlMode installed to do
this.  The function inserts linebreaks to separate tags that have
nothing but whitespace between them.  It then indents the markup
by using nxml's indentation rules."
  (interactive "r")
  (save-excursion
      (nxml-mode)
      (goto-char begin)
      (while (search-forward-regexp "\>[ \\t]*\<" nil t) 
        (backward-char) (insert "\n"))
      (indent-region begin end))
    (message "Ah, much better!"))

;make emacs interact with the clipboard similar to other xwindow apps
;;(global-set-key "\C-w" 'clipboard-kill-region)
;;(global-set-key "\M-w" 'clipboard-kill-ring-save)
;;(global-set-key "\C-y" 'clipboard-yank)

;enable deleteing (not killing) of a region
(global-set-key "\C-xc" 'delete-region)

(setq inhibit-splash-screen t)

(setq frame-title-format "%b - emacs")

;setting slightly small text size
(set-default-font "-unknown-DejaVu Sans Mono-normal-normal-normal-*-18-*-*-*-m-0-iso10646-1")

;disable backup
(setq backup-inhibited t)
;disable auto save
(setq auto-save-default nil)

; org-mode settings
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(setq org-log-done t)
(setq org-agenda-files (list "~/org/TODO.org"))

;flyspelling for text mode
(dolist (hook '(text-mode-hook))
  (add-hook hook (lambda () (flyspell-mode 1))))

;word wrap for text mode
(dolist (hook '(text-mode-hook))
  (add-hook hook (lambda () (visual-line-mode 1))))

;line numbers for all buffers
(global-linum-mode t)

;set scroll bar to the right
(set-scroll-bar-mode 'right)
