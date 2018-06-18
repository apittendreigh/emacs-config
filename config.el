(use-package which-key
  :ensure t
  :init (which-key-mode))

(use-package beacon
  :ensure t
  :init (beacon-mode 1))

(setq org-src-window-setup 'current-window)
(add-to-list 'org-structure-template-alist
	     '("el" "#+BEGIN_SRC emacs-lisp\n?\n#+END_SRC"))

(use-package org-bullets
  :ensure t
  :config (add-hook 'org-mode-hook (lambda () (org-bullets-mode))))

(setq ido-enable-flex-matching nil)
(setq ido-create-new-buffer 'always)
(setq ido-everywhere t)
(ido-mode 1)

(use-package ido-vertical-mode
  :ensure t
  :init (ido-vertical-mode 1))
(setq ido-vertical-define-keys 'C-n-and-C-p-only)

(use-package smex
  :ensure t
  :init (smex-initialize))

(global-set-key (kbd "C-x C-b") 'ido-switch-buffer)

(global-set-key (kbd "C-x b") 'ibuffer)

(setq ibuffer-expert t)

(use-package avy
  :ensure t
  :bind ("M-s" . avy-goto-char))

(use-package rainbow-mode
  :ensure t
  :init (dolist (hook '(css-mode-hook html-mode-hook sass-mode-hook))
	  (add-hook hook 'rainbow-mode)))

(use-package rainbow-delimiters
  :ensure t
  :init
  (add-hook 'prog-mode-hookd #'rainbow-delimiters-mode)
  (rainbow-delimiters-mode 1))

(use-package switch-window
  :ensure t
  :config
  (setq switch-window-input-style 'minibuffer)
  (setq switch-window-increasew 4)
  (setq switch-window-threshold 2)
  (setq switch-window-shortcut-style 'qwerty)
  (setq switch-window-qwerty-shortcuts
	'("a" "s" "d" "f" "h" "j" "k" "l"))
  :bind ([remap other-window] . switch-window))

(use-package hungry-delete
  :ensure t
  :config (global-hungry-delete-mode))

(use-package sudo-edit
  :ensure t
  :bind ("s-e" . sudo-edit))

(use-package dashboard
  :ensure t
  :config
  (dashboard-setup-startup-hook)
  (setq dashboard-items '((recents . 10)))
  (setq dashboard-banner-logo-title "Welcome to the world of Emacs!"))

(use-package company
  :ensure t
  :init
  (add-hook 'after-initial-hook 'global-company-mode))

(use-package spaceline
  :ensure t
  :config
  (require 'spaceline-config)
  (setq powerline-default-separator (quote arrow))
  (spaceline-spacemacs-theme))

(use-package diminish
  :ensure t
  :init
  (diminish 'hungry-delete-mode)
  (diminish 'beacon-mode)
  (diminish 'subword-mode)
  (diminish 'rainbow-mode)
  (diminish 'which-key-mode))

(use-package dmenu
  :ensure t
  :bind ("C-SPC" . 'dmenu))

(use-package symon
  :ensure t
  :bind ("M-s-h" . 'symon-mode))

(defun split-and-follow-horizontally ()
  (interactive)
  (split-window-below)
  (balance-windows)
  (other-window 1))

(defun split-and-follow-vertically ()
  (interactive)
  (split-window-right)
  (balance-windows)
  (other-window 1))

(defun kill-whole-word ()
  (interactive)
  (backward-word)
  (kill-word 1))

(defun config-visit ()
  (interactive)
  (find-file "~/.emacs.d/config.org"))

(defun config-reload ()
  (interactive)
  (org-babel-load-file (expand-file-name "~/.emacs.d/config.org")))

(defun kill-curr-buffer ()
  (interactive)
  (kill-buffer (current-buffer)))

(defun copy-whole-line ()
  (interactive)
  (save-excursion
    (kill-new
     (buffer-substring
      (point-at-bol)
      (point-at-eol)))))

(defun kill-all-buffers ()
  (interactive)
  (mapc 'kill-buffer (buffer-list)))

(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(rainbow-mode 1)
(global-company-mode 1)
(defalias 'yes-or-no-p 'y-or-n-p)
(display-time-mode 1)
(setq inhibit-startup-message t)
(setq column-number-mode t)
(setq line-number-mode t)
(setq scroll-conservatively 100)
(setq ring-bell-function 'ignore)
(when window-system (global-hl-line-mode t))
(when window-system (global-prettify-symbols-mode t))
(setq make-backup-file nil)
(setq auto-save-default nil)
(setq global-subword-mode t)

(setq electric-pair-pairs '(
			    (?\( . ?\))
			    (?\[ . ?\])
			    (?\{ . ?\})
			    (?\< . ?\>)))
(electric-pair-mode t)

(set-frame-position (selected-frame) 50 50)
(setq initial-frame-alist
      '(
	(width . 142) ; character
	(height . 47) ; lines
	))
(setq default-frame-alist
      '(
	(width . 140) ; character
	(height . 450) ; lines
	))

(defvar my-term-shell "/bin/zsh")
(defadvice ansi-term (before force-bash)
  (interactive (list my-term-shell)))
(ad-activate 'ansi-term)

(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "C-c e") 'config-visit)
(global-set-key (kbd "C-c r") 'config-reload)
(global-set-key (kbd "s-<return>") 'ansi-term)
(global-set-key (kbd "C-x 2") 'split-and-follow-horizontally)
(global-set-key (kbd "C-x 3") 'split-and-follow-vertically)
(global-set-key (kbd "C-c w w") 'kill-whole-word)
(global-set-key (kbd "C-x k") 'kill-curr-buffer)
(global-set-key (kbd "C-c w l") 'copy-whole-line)
(global-set-key (kbd "C-C k a") 'kill-all-buffers)
