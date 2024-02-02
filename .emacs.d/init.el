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
  :hook ((prog-mode . rainbow-delimiters-mode)
         (org-mode . rainbow-delimiters-mode)))

(use-package rainbow-mode
  :ensure t)

;; Keybinding
(global-set-key (kbd "C-c c") 'compile)
(global-set-key (kbd "C-c b") 'eval-buffer)
(global-set-key (kbd "C-c t") 'transparency)
(global-set-key (kbd "C-c s") 'shell-command)
(global-set-key (kbd "C-c i")
                (lambda ()
                  (interactive)
                  (eldoc-minibuffer-message "Indenting buffer...")
                  (indent-region 0 (point-max))
                  (eldoc-minibuffer-message "Indenting buffer...done")))

;; Terminal ;;
(use-package multi-vterm
  :ensure t)

;; Org ;;
(add-hook 'org-mode-hook
          (lambda ()
            (setq
             display-line-numbers-type 'normal)
            (display-line-numbers-mode 1)))

;; Programming ;;
(add-hook 'prog-mode-hook
          (lambda ()
            (setq
             indent-tabs-mode nil
             c-default-style "bsd"
             c-basic-offset 4
             tab-width 4
             parens-require-spaces nil
             show-paren-style 'parenthesis
             whitespace-style '(face tabs spaces tab-mark space-mark trailing indentation empty missing-newline-at-eof))
            (whitespace-mode 1)
            (electric-pair-mode 0)
            (electric-indent-mode 1)
            (display-line-numbers-mode 1)))

(add-hook 'python-mode-hook (lambda () (setq python-indent-offset 4)))

(use-package company
  :ensure t
  :hook ((prog-mode . company-mode)))

(use-package slime
  :ensure t
  :config (setq inferior-lisp-program "sbcl"))

;; Custom ;;
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
