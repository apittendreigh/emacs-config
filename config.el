(defvar my-term-shell "/bin/zsh")
(defadvice ansi-term (before force-bash)
  (interactive (list my-term-shell)))
(ad-activate 'ansi-term)

(use-package which-key
  :ensure t
  :init (which-key-mode))

(use-package beacon
  :ensure t
  :init (beacon-mode 1))

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

(setq inhibit-startup-message t)
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(defalias 'yes-or-no-p 'y-or-n-p)
(setq column-number-mode t)
(global-set-key (kbd "s-<return>") 'ansi-term)
(setq scroll-conservatively 100)
(setq ring-bell-function 'ignore)
(when window-system (global-hl-line-mode t))
(when window-system (global-prettify-symbols-mode t))
(setq make-backup-file nil)
(setq auto-save-default nil)
(global-set-key (kbd "M-x") 'smex)

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