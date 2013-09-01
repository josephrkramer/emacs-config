;; MELPA package manager
(require 'package)
(add-to-list 'package-archives
  '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)

;; load git-commit
;(require 'git-commit) 
(unless (package-installed-p 'git-commit-mode)
  (package-refresh-contents) (package-install 'git-commit-mode))
(add-hook 'git-commit-mode-hook 'turn-on-flyspell)

;; scala-mode2
(unless (package-installed-p 'scala-mode2)
  (package-refresh-contents) (package-install 'scala-mode2))

;; color-theme-solarized
(unless (package-installed-p 'color-theme-solarized)
  (package-refresh-contents) (package-install 'color-theme-solarized))

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
;(set-scroll-bar-mode 'right)

;start git rebase in org-mode
(setq auto-mode-alist (cons '("git-rebase-todo" . org-mode) auto-mode-alist))
