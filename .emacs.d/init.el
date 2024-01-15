;; TODOs:
;; Drag
;; Bind comment-or-uncomment


;; Debug ;;

(setq debug-on-error t)


;; MELPA ;;

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

(package-initialize)


;; General ;;
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

(setq text-scale-mode-step 1.1)
(setq mouse-wheel-progressive-speed nil)

(use-package highlight-numbers
  :ensure t
  :hook
  ((prog-mode . highlight-numbers-mode)))

(set-face-attribute 'default nil :font "Comic Code" :height 165)

(load-file "~/.emacs.d/masked-theme.el")
(load-theme 'masked t)

;; (load-theme 'tango-dark t)
;; (load-theme 'manoj-dark t)
;; (load-theme 'light-blue)

(use-package zenburn-theme
  :disabled t
  :ensure t
  :config
  (load-theme 'zenburn t))

(use-package alect-themes
  :disabled t
  :ensure t
  :config
  (load-theme 'alect-dark t))

(use-package ef-themes
  :disabled t
  :ensure t
  :config
  (load-theme 'ef-autumn t))

(use-package powerline
  :disabled t
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
            (lambda() (face-remap--remap-face
                       'line-number-current-line)))
  (require 'powerline)
  (setq moe-theme-modeline-color 'black) ;; (Available colors: blue, orange, green ,magenta, yellow, purple, red, cyan, w/b.)
  (powerline-moe-theme)
  (load-theme 'moe-dark t))

(use-package rainbow-delimiters
  :disabled t
  :ensure t
  :hook ((prog-mode . rainbow-delimiters-mode)
         (org-mode . rainbow-delimiters-mode)))

(use-package rainbow-mode
  :ensure t)

(use-package all-the-icons
  ;;(all-the-icons-install-fonts)
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
             ;;tab-width 4
             parens-require-spaces nil
             show-paren-style 'parenthesis
             display-line-numbers-type 'normal
             whitespace-style '(face tabs spaces tab-mark space-mark trailing indentation empty missing-newline-at-eof))
            (whitespace-mode 1)
            (electric-pair-mode 0)
            (electric-indent-mode 1)
            (display-line-numbers-mode 1)))

(add-hook 'c-mode-hook (lambda ()))
(add-hook 'python-mode-hook (lambda () (setq python-indent-offset 4)))

(use-package company
  :ensure t
  :hook ((prog-mode . company-mode)))

(use-package slime
  :ensure t
  :config (setq inferior-lisp-program "sbcl"))

(use-package treemacs
  :ensure t
  :config (global-set-key (kbd "C-c ;") 'treemacs))

(use-package rust-mode
  :ensure t)

(use-package haskell-mode
  :ensure t)


;; Custom ;;

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("f7928022d109d02cae1f5d553510fc77bb994412ab1ae1d075f5360cb8584c65" "b7b9a74d248fdf304bc7207dc78c10b2fd632974e6f2d3d50ea4258859472581" "e3999eba4f25d912d7d61cbaaed1b551957e61da047279da89499d3bd1f1d007" "ab04c00a7e48ad784b52f34aa6bfa1e80d0c3fcacc50e1189af3651013eb0d58" "82b43e48862ecc7e3af29838ed843227e331b187865828dc4915021c5a74baa1" "7f34e5ab75ec580aff579b3b0f40379d280f8441e424b7a04322524ed7f348b6" "2cc1ac47eed7ac51d79d1aaf6218d52ec84d9c6eb8a448f221f592bddfe51550" "38457f8afb329ce87e1a41d31e155acb4dcdf5ee6a1ea703d401f2042747a69f" "4ae2387bb3bcfb3419d88f586b41c1fef3ff8620b80d06d53f98ec30df469407" "51f3fb81f9233280cb28ee3023e43e82c9307d59d158626881ca14f964d2abeb" "50bb891011dfe0c30cd463c65e898523788d4ac4e6df141eed75030a33da1135" "1b8df5c4f3364ebfbe9c0d3d859f6c31ab652ba518612ec27b12e462ce677731" "78be54fed89551db18cbeb645ab807c99181555a51405aaba366b56d913b6040" "8390abb2cc504d44f0c9dfdaf79d4e943f0328a933e20ceec74c74d17d65834f" "260ed5a03b9ed35b1ab1eb51cb06887870221490e0c5c91940dd2203b48ce60f" "184d32f815d68d6aadf7dde7d4f7b8d8059fdd177630e501a4a52989a568d785" "dc2e1b0abb9a5d2033f6d881618932dcdb9af7633d8fa44336f9c9a3484379bd" "e1990eeea39781f009b7f4634ca52a770d05bb7ce423a8fbbcd8a4f327efb626" "31804a8ea314e76b68f8b1c454212c3d9710c4294b8cfbaa008dd338c8d91773" "eb0f822891b90a730f3331959311439f01bb39da3cdf998b5693ecec877858d0" "e871f44a640f98523876f77dccdbf0e20747ca7e111f9f147fe23c9d5f4937c1" "242f33ba517c05f45e075d8ed3d13c0a7b7d1392e0c95d66830029e561607085" "e6b0ec96166bb3bb2843d83e56c0292308aab10ee5b79fb921d16ad2dbea5d5f" "406d7c11a38d7b0e6c305ea91515cbd0c89cd73c55d041da9545338df98f1db4" "714394050e703db8a773ed350ca6f9cb6636d4bf2e348514804a48929aafc762" "6cff32351bcb1726accf9dcf9c400367971eaa8bb1d163409b78ea9c9a6ae8d0" "4f6dc03105f64cd7e5a3f555ea7c6bac7d9447141473ef9ff3c23b63858066da" "d0f3adfe292c9d633930e35c3458cda77796073bb25af852689f999bbb3d9398" "45e409674661674c12070af5f8ef71741599eeb9fccd84557f1b822509f3b100" "2fcd2b44646836f0f4acbd42a13fa85123dac744628f0105a5e9f0f7dbbc936a" "2459d6e7e96aefaed9cebaf7fde590f64e76c96f48632d8310cfea5d10ec2bb1" default))
 '(package-selected-packages
   '(powerline sudo-edit multi-vterm ef-themes zenburn-theme vterm tango-2-theme spacemacs-theme solarized-theme slime rust-mode rainbow-mode rainbow-delimiters monokai-theme molokai-theme moe-theme ligature leuven-theme highlight-numbers haskell-mode gruvbox-theme gruber-darker-theme evil company centered-window all-the-icons alect-themes)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
