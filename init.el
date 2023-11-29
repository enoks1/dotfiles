;; General
(setq debug-on-error t)
(setq make-backup-files nil)
(setq split-width-threshold nil)
;;
;;
;; MELPA
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

(package-initialize)
;;
;;
;; Look & Feel
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)

(setq mouse-wheel-progressive-speed nil)

(set-frame-font "Source Code Pro:size=22" nil t)

(use-package zenburn-theme
  :disabled t
  :ensure t

  :custom-face (whitespace-space ((t (:background "#3F3F3F" :foreground "#4F4F4F"))))
  :config (load-theme 'zenburn t))

(use-package spacemacs-theme
  :ensure t
  :config (load-theme 'spacemacs-dark t))

(use-package highlight-numbers
  :ensure t
  :custom-face (highlight-numbers-number ((t (:inherit font-lock-string-face))))
  :hook ((prog-mode . highlight-numbers-mode))
  )

(use-package rainbow-mode
  :ensure t
  :hook ((org-mode . rainbow-mode)))
;;
;;
;; Keybinding
(global-set-key (kbd "C-c c") 'compile)
(global-set-key (kbd "C-c b") 'eval-buffer)
(global-set-key (kbd "C-c s") 'shell-command)
(global-set-key (kbd "C-c i")
                (lambda ()
                  (interactive)
                  (indent-region 0 (point-max))))
;;
;;
;; Programming
(use-package company
  :ensure t
  :config (setq company-minimum-prefix-length 4
                company-idle-delay 0)
  :bind (("C-c SPC" . company-dabbrev))
  :hook ((prog-mode . company-mode)))

(add-hook 'prog-mode-hook
          (lambda ()
            (electric-pair-mode 1)
            (electric-indent-mode 1)
            (setq-default
             indent-tabs-mode nil
             c-default-style "bsd"
             c-basic-offset 4
             tab-width 4
             parens-require-spaces nil)
            (setq whitespace-style
                  '(face tabs spaces trailing space-before-tab
                         indentation empty space-after-tab
                         missing-newline-at-eof
                         space-mark tab-mark))
            (whitespace-mode 1)
            (setq display-line-numbers-type 'relative)
            (display-line-numbers-mode 1)))

(add-hook 'c-mode-hook (lambda ()))

(add-hook 'python-mode-hook (lambda ()
                              (setq python-indent-offset 4)))
;;
;;
;; Extra
(use-package vterm
  :ensure t)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("7fd8b914e340283c189980cd1883dbdef67080ad1a3a9cc3df864ca53bdc89cf" "37c8c2817010e59734fe1f9302a7e6a2b5e8cc648cf6a6cc8b85f3bf17fececf" default))
 '(package-selected-packages
   '(spacemacs-theme monokai-theme zenburn-theme vterm solarized-theme rainbow-mode rainbow-delimiters highlight-numbers evil company centered-window)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
