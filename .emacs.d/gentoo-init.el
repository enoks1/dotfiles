(progn
  (require 'package)
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
  (package-initialize))

(use-package sudo-edit
  :ensure t)

(setq
 backup-directory-alist '(("." . "~/.emacs.d.bak"))
 backup-by-copying t
 version-control t
 delete-old-versions t)

(setq
 inhibit-splash-screen t)

(add-to-list 'default-frame-alist '(font . "Comic Code-12"))

(progn
  (menu-bar-mode -1)
  (scroll-bar-mode -1)
  (tool-bar-mode -1))

(fringe-mode '(0 . 0))

(add-hook 'prog-mode-hook #'display-line-numbers-mode)

(indent-tabs-mode -1)

(setq-default
 c-default-style "stroustrup"
 c-basic-offset 2
 tab-width 2
 indent-tabs-mode nil

 whitespace-display-mappings
 '((space-mark   ?\    [?\xB7]     [?.])
   (space-mark   ?\xA0 [?\xA4]     [?_])
   (newline-mark ?\n   [?\xB6 ?\n] [?$ ?\n]))

 whitespace-style
 '(face
   ;;tabs tab-mark
   spaces space-mark
   ;;newline newline-mark
   space-before-tab
   space-after-tab
   indentation
   trailing
   ;;lines
   empty
   missing-newline-at-eof))

(progn
  (global-set-key (kbd "C-c c") 'compile)
  (global-set-key (kbd "C-c b") 'eval-buffer)
  (global-set-key (kbd "C-c s") 'shell-command)
  (global-set-key (kbd "C-c i") (lambda () (interactive)
                                  (eldoc-minibuffer-message "Indenting buffer...")
                                  (indent-region 0 (point-max))
                                  (eldoc-minibuffer-message "Indenting buffer...done")))
  (global-set-key (kbd "C-c m") 'comment-or-uncomment-region)
  (global-set-key (kbd "C-c l") (lambda () (interactive)
                                  (load-file "~/.emacs.d/init.el"))))

(use-package zenburn-theme
  :ensure t
  :custom-face
  (whitespace-space ((t (:background "#3F3F3F" :foreground "#4F4F4F"))))
  :config
  (load-theme 'zenburn t))

(use-package vterm
  :load-path "/var/db/repos/gentoo/dev-libs/libvterm")

(use-package company
  :ensure t
  :config
  (setq
   company-idle-delay 0.0
   company-minimum-prefix-length 1)
  :hook ((prog-mode . company-mode)))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages '(zenburn-theme company sudo-edit)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
