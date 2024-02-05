;; TODOs:
;; Drag
;; Bind comment-or-uncomment globally

;; Debug? ;;
(setq debug-on-error t)

;; MELPA ;;
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

(package-initialize)

;; Sudo ;;
(use-package sudo-edit
  :ensure t)

;; Look & Feel ;;
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)

(defun transparency (value)
  "Sets the transparency of the frame window. 0 = transparent, 100 = opaque"
  (interactive "nTransparency Value 0 - 100 opaque:")
  (set-frame-parameter nil 'alpha-background value)
  (add-to-list 'default-frame-alist '(alpha-background . value)))

(setq mouse-wheel-progressive-speed nil
      text-scale-mode-step 1.2)

(use-package highlight-numbers
  :ensure t
  :hook ((prog-mode . highlight-numbers-mode)))

(set-face-attribute 'default nil :font "Iosevka Extended" :height 165 :slant 'normal :weight 'light)

(load-file "~/.emacs.d/masked-theme.el")

(require 'masked-theme)
(load-theme 'masked t)

(use-package powerline
  :ensure t
  :config
  (powerline-default-theme))

(use-package moe-theme
  :disabled t
  :ensure t
  :config
  (add-hook 'text-scale-mode-hook
            (lambda () (face-remap--remap-face
                        'line-number)))
  (add-hook 'text-scale-mode-hook
            (lambda () (face-remap--remap-face
                        'line-number-current-line)))
  (require 'powerline)
  (setq moe-theme-modeline-color 'black)
  (powerline-moe-theme)
  (load-theme 'moe-dark t))

(use-package rainbow-delimiters
  :disabled t
  :ensure t
