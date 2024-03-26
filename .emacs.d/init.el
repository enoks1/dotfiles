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

(set-face-attribute 'default nil :font "Iosevka" :height 165 :slant 'normal :weight 'normal)

(load-file "~/.emacs.d/masked-theme.el")

(require 'masked-theme)
(load-theme 'masked t)

;; Packages & Expressions ;;
(use-package powerline
  :ensure t
  :config
  (powerline-default-theme))

(use-package highlight-numbers
  :ensure t
  :hook ((prog-mode . highlight-numbers-mode)))

(use-package rainbow-mode
  :ensure t)

(use-package company
  :ensure t
  :hook ((prog-mode . company-mode)))

(use-package multi-vterm
  :ensure t
  :config
  (global-set-key (kbd "C-c v") 'multi-vterm-dedicated-open)
  (global-set-key (kbd "C-c d") 'multi-vterm-dedicated-open))

(use-package drag-stuff
  :ensure t
  :config (drag-stuff-global-mode)
  (global-set-key (kbd "C-c j") 'drag-stuff-down)
  (global-set-key (kbd "C-c k") 'drag-stuff-up)
  (global-set-key (kbd "C-c h") 'drag-stuff-left)
  (global-set-key (kbd "C-c l") 'drag-stuff-right))

(use-package sudo-edit
  :ensure t)

(use-package slime
  :ensure t
  :config (setq inferior-lisp-program "sbcl"))

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
             whitespace-style '(face tabs spaces tab-mark space-mark trailing indentation empty missing-newline-at-eof)
             asm-comment-char ?#)
            (whitespace-mode 1)
            (electric-pair-mode 0)
            (electric-indent-mode 1)
            (display-line-numbers-mode 1)))

(add-hook 'python-mode-hook (lambda () (setq python-indent-offset 4)))

;; Custom ;;
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("cd51c4e5a258ab9fd82de1fb4c0eee54326de5e307e3bf2895467ae103bc562b" "e27c9668d7eddf75373fa6b07475ae2d6892185f07ebed037eedf783318761d7" "1d9fd158c76450d754bdbc29634a78cfaa0e1eceb3b92095c934e085333f3efc" "70ec7646ae2d333d437e76e969a29389958cd312fda4f68eb3e0e36d057edb5d" "76f94e145b5313f14d159051d2b714478f52325a6465fce7ce46f44dffed386c" default))
 '(package-selected-packages
   '(drag-stuff vterm multi-vterm tangonov-theme zenburn-theme zenburn gruber-darker-theme rust-mode leuven-theme turkish sudo-edit slime rainbow-mode powerline highlight-numbers company)))
