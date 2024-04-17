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
(use-package modus-themes
  :ensure t
  :init
  (progn (setq modus-themes-to-toggle '(modus-operandi-tinted modus-vivendi-tinted))
         (load-theme 'modus-vivendi-tinted t))
  :bind (("C-c t" . modus-themes-toggle)))

(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)

(set-face-attribute 'default nil :font "Essential PragmataPro" :height 120 :slant 'normal :weight 'normal)

(icomplete-mode 1)

;; Local & Packages & Expressions;;
(use-package highlight-numbers
  :disabled
  :ensure t
  :hook ((prog-mode . highlight-numbers-mode)))

(use-package rainbow-mode
  :ensure t)

(use-package rainbow-delimiters
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
  :bind (("C-c j" . drag-stuff-down)
         ("C-c k" . drag-stuff-up)
         ("C-c h" . drag-stuff-left)
         ("C-c l" . drag-stuff-right)))

(use-package sudo-edit
  :ensure t)

;; (use-package slime
;;   :ensure t
;;   :config (setq inferior-lisp-program "sbcl"))

;; Keybinding
(global-set-key (kbd "C-c c") 'compile)
(global-set-key (kbd "C-c b") 'eval-buffer)
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
             whitespace-style '(face trailing indentation empty missing-newline-at-eof)
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

(add-hook 'python-mode-hook
          (lambda ()
            (setq python-indent-offset 4)))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("0f76f9e0af168197f4798aba5c5ef18e07c926f4e7676b95f2a13771355ce850" "703a3469ae4d2a83fd5648cac0058d57ca215d0fea7541fb852205e4fae94983" "64045b3416d83e5eac0718e236b445b2b3af02ff5bcd228e9178088352344a92" "c7a926ad0e1ca4272c90fce2e1ffa7760494083356f6bb6d72481b879afce1f2" "c1638a7061fb86be5b4347c11ccf274354c5998d52e6d8386e997b862773d1d2" "e27c9668d7eddf75373fa6b07475ae2d6892185f07ebed037eedf783318761d7" default))
 '(package-selected-packages
   '(modus-themes rust-mode rainbow-delimiters sudo-edit drag-stuff company rainbow-mode use-package)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(font-lock-keyword-face ((t (:inherit modus-themes-bold :foreground "#b6a0ff" :weight bold)))))
