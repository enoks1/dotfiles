;; Enviroment Variables ;;
(setq debug-on-error nil
      mouse-wheel-progressive-speed nil
      text-scale-mode-step 1.2
      backup-by-copying t
      backup-directory-alist '(("." . "~/.backup.d/"))
      delete-old-versions t
      kept-new-versions 20
      kept-old-versions 10
      version-control t)

;; MELPA ;;
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

(package-initialize)

;; Look & Feel ;;
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)

(set-face-attribute 'default nil :font "Essential PragmataPro" :height 120 :slant 'normal :weight 'normal)

(icomplete-mode 1)

;; Local & Packages & Expressions;;
(load-file "~/.emacs.d/masked-theme.el")

;;(load-file "~/.emacs.d/zenburn-theme.el")
(load-theme 'masked t)

;; (use-package zenburn-theme
;;   :ensure t
;;   :config (load-theme 'zenburn t))

(use-package moe-theme
  :disabled t
  :ensure t
  :config (load-theme 'moe-dark t))

(use-package highlight-numbers
  :disabled
  :ensure t
  :hook ((prog-mode . highlight-numbers-mode)))

(use-package rainbow-mode
  :ensure t)

(use-package company
  :ensure t
  :hook ((prog-mode . company-mode)))

;; (use-package multi-vterm
;;   :ensure t
;;   :config
;;   (global-set-key (kbd "C-c v") 'multi-vterm-dedicated-open)
;;   (global-set-key (kbd "C-c d") 'multi-vterm-dedicated-open))

(use-package drag-stuff
  :ensure t
  :config (drag-stuff-global-mode)
  (global-set-key (kbd "C-c j") 'drag-stuff-down)
  (global-set-key (kbd "C-c k") 'drag-stuff-up)
  (global-set-key (kbd "C-c h") 'drag-stuff-left)
  (global-set-key (kbd "C-c l") 'drag-stuff-right))

(use-package sudo-edit
  :ensure t)

;; (use-package slime
;;   :ensure t
;;   :config (setq inferior-lisp-program "sbcl"))

(defun transparency (value)
  "Sets the transparency of the frame window. 0 = transparent, 100 = opaque"
  (interactive "nTransparency Value 0 - 100 opaque:")
  (set-frame-parameter nil 'alpha-background value)
  (add-to-list 'default-frame-alist '(alpha-background . value)))

;; Keybinding
(global-set-key (kbd "C-c c") 'compile)
(global-set-key (kbd "C-c b") 'eval-buffer)
(global-set-key (kbd "C-c t") 'transparency)
(global-set-key (kbd "C-c s") 'shell-command)
(global-set-key (kbd "C-c i") (lambda ()
                                (interactive)
                                (eldoc-minibuffer-message "Indenting buffer...")
                                (indent-region 0 (point-max))
                                (eldoc-minibuffer-message "Indenting buffer...done")))
(global-set-key (kbd "C-c m") 'comment-or-uncomment-region)

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
             whitespace-style '(spaces space-mark tabs tab-mark face trailing indentation empty missing-newline-at-eof)
             asm-comment-char ?#)
            (whitespace-mode 1)
            (electric-pair-mode 1)
            (electric-indent-mode 1)
            (rainbow-delimiters-mode 1)
            (display-line-numbers-mode 1)))

(add-hook 'org-mode-hook
          (lambda ()
            (setq display-line-numbers-type 'normal)
            (display-line-numbers-mode 1)))

(add-hook 'c-mode-hook
          (lambda ()
            (setq comment-start "//" comment-end "")))

(add-hook 'python-mode-hook (lambda () (setq python-indent-offset 4)))
