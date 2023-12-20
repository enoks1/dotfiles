;; TODO Drag
;; TODO Bind comment-or-uncomment
;; General
(setq debug-on-error t)
(setq make-backup-files nil)
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

(set-face-attribute 'default nil :font "Noto Sans Mono" :height 100)

(use-package gruvbox-theme
  :ensure t
  :config (load-theme 'gruvbox-dark-medium t))

(use-package rainbow-mode
  :ensure t
  :hook ((org-mode . rainbow-mode)))

(use-package rainbow-delimiters
  :ensure t
  :hook ((prog-mode . rainbow-delimiters-mode)
         (org-mode . rainbow-delimiters-mode)))
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
(add-hook 'text-scale-mode-hook
          (lambda() (face-remap--remap-face
                     'line-number)))
(add-hook 'text-scale-mode-hook
          (lambda() (face-remap--remap-face
                     'line-number-current-line)))
;;
;;
;; Programming
(use-package company
  :ensure t
  :hook ((prog-mode . company-mode)))
(use-package slime
  :ensure t
  :config (setq inferior-lisp-program "sbcl"))
(use-package rust-mode
  :ensure t)
(use-package haskell-mode
  :ensure t)
(add-hook 'prog-mode-hook
          (lambda ()
            (setq
             indent-tabs-mode nil
             c-default-style "bsd"
             c-basic-offset 4
             tab-width 4
             parens-require-spaces nil
             show-paren-style 'parenthesis
             display-line-numbers-type 'relative
             whitespace-style '(face spaces tabs trailing indentation empty
                                     missing-newline-at-eof))
            (whitespace-mode 1)
            (electric-pair-mode 1)
            (electric-indent-mode 1)
            (display-line-numbers-mode 1)
            ;;(global-prettify-symbols-mode 1)
            ))

(add-hook 'c-mode-hook (lambda ()))

(add-hook 'python-mode-hook (lambda () (setq python-indent-offset 4)))
;;
;;
;; Extra
;;(use-package vterm :ensure t)

;; (use-package tango-2-theme
;;   :ensure t
;;   :custom-face
;;   (default ((t (:background "#000000" :foreground "#aaaaaa"))))
;;   (whitespace-space ((t (:background "#000000" :foreground "#aaaaaa"))))
;;   (font-lock-variable-name-face ((t (:foreground "#aaaaaa"))))
;;   (font-lock-keyword-face ((t (:foreground "#fce94f"))))
;;   (font-lock-type-face ((t (:foreground "#75507b"))))
;;   (font-lock-comment-face ((t (:foreground "#34e2e2"))))
;;   :config (load-theme 'tango-2 t))

;; (use-package zenburn-theme
;;   :disabled t
;;   :ensure t
;;   :custom-face (whitespace-space ((t (:background "#3F3F3F" :foreground "#4F4F4F"))))
;;   :config (load-theme 'zenburn t))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("046a2b81d13afddae309930ef85d458c4f5d278a69448e5a5261a5c78598e012" default))
 '(package-selected-packages
   '(slime gruvbox-theme zenburn-theme vterm tango-2-theme spacemacs-theme solarized-theme rust-mode rainbow-mode rainbow-delimiters monokai-theme moe-theme leuven-theme highlight-numbers haskell-mode gruber-darker-theme evil company centered-window alect-themes)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
