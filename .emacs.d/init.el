;; Enviroment Variables ;;
(setq-default debug-on-error nil
              mouse-wheel-progressive-speed nil
              text-scale-mode-step 1.2
              backup-by-copying t
              backup-directory-alist '(("." . "~/.backup.d/"))
              delete-old-versions t
              kept-new-versions 20
              kept-old-versions 10
              version-control t
              indent-tabs-mode nil
              tab-width 4
              indent-line-function 'insert-tab
              scheme-program-name "guile")

;; MELPA ;;
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

(package-initialize)

;; Look & Feel ;;
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)

(set-face-attribute 'default nil :font "Berkeley Mono" :height 120 :slant 'normal :weight 'normal)

(icomplete-mode 1)

(use-package zenburn-theme
  :ensure t
  :disabled
  :custom-face
  (whitespace-space ((t (:background "#3F3F3F" :foreground "#4F4F4F"))))
  (whitespace-hspace ((t (:background "#3F3F3F" :foreground "#4F4F4F"))))
  ;;(highlight-indentation-face ((t (:background "#3F3F3F" :foreground "#5F5F5F"))))
  :config (load-theme 'zenburn t))

(use-package color-theme-sanityinc-solarized
  :ensure t
  :custom-face
  (whitespace-space ((t (:background "#002b36" :foreground "#073642"))))
  (whitespace-hspace ((t (:background "#002b36" :foreground "#073642"))))
  (whitespace-newline ((t (:background "#002b36" :foreground "#073642"))))
  (line-number ((t (:inherit default :background "#073642" :foreground "#586e75"))))
  :config (load-theme 'sanityinc-solarized-dark t))

(use-package gruber-darker-theme
  :ensure t
  :disabled
  :config (load-theme 'gruber-darker t))

;; https://github.com/mickeynp/ligature.el/wiki#Berkeley%20Mono
(use-package ligature
  :ensure t
  :config
  (ligature-set-ligatures
   't
   '(; Group A
     ".." ".=" "..." "..<" "::" ":::" ":=" "::=" ";;" ";;;" "??" "???"
     ".?" "?." ":?" "?:" "?=" "**" "***" "/*" "*/" "/**"
                                        ; Group B
     "<-" "->" "-<" ">-" "<--" "-->" "<<-" "->>" "-<<" ">>-" "<-<" ">->"
     "<-|" "|->" "-|" "|-" "||-" "<!--" "<#--" "<=" "=>" ">=" "<==" "==>"
     "<<=" "=>>" "=<<" ">>=" "<=<" ">=>" "<=|" "|=>" "<=>" "<==>" "||="
     "|=" "//=" "/="
                                        ; Group C
     "<<" ">>" "<<<" ">>>" "<>" "<$" "$>" "<$>" "<+" "+>" "<+>" "<:" ":<"
     "<:<" ">:" ":>" "<~" "~>" "<~>" "<<~" "<~~" "~~>" "~~" "<|" "|>"
     "<|>" "<||" "||>" "<|||" "|||>" "</" "/>" "</>" "<*" "*>" "<*>" ":?>"
                                        ; Group D
     "#(" "#{" "#[" "]#" "#!" "#?" "#=" "#_" "#_(" "##" "###" "####"
                                        ; Group E
     "[|" "|]" "[<" ">]" "{!!" "!!}" "{|" "|}" "{{" "}}" "{{--" "--}}"
     "{!--" "//" "///" "!!"
                                        ; Group F
     "www" "@_" "&&" "&&&" "&=" "~@" "++" "+++" "/\\" "\\/" "_|_" "||"
                                        ; Group G
     "=:" "=:=" "=!=" "==" "===" "=/=" "=~" "~-" "^=" "__" "!=" "!==" "-~"
     "--" "---"))
  ;; Enables ligature checks globally in all buffers. You can also do it
  ;; per mode with `ligature-mode'.
  (global-ligature-mode t))

;; Local & Packages & Expressions;;
(use-package highlight-numbers
  :ensure t
  :hook ((prog-mode . highlight-numbers-mode)))

(use-package rainbow-delimiters
  :ensure t)

(use-package hl-todo
  :ensure t
  :hook ((prog-mode . hl-todo-mode)
         (elpy-mode . hl-todo-mode)))

(use-package rainbow-mode
  :ensure t)

(use-package company
  :ensure t
  :hook ((prog-mode . company-mode)))

(use-package magit
  :ensure t
  :after transient)

(use-package multi-vterm
  :ensure t
  :config
  (global-set-key (kbd "C-c v") 'multi-vterm-dedicated-open)
  (global-set-key (kbd "C-c d") 'multi-vterm-dedicated-open))

(use-package drag-stuff
  :ensure t
  :config (drag-stuff-global-mode)
  :bind (("C-c j" . drag-stuff-down)
         ("C-c k" . drag-stuff-up)
         ("C-c h" . drag-stuff-left)
         ("C-c l" . drag-stuff-right)))

(use-package sudo-edit
  :ensure t)

(use-package elpy
  :ensure t
  :init
  (elpy-enable))

(use-package pipenv
  :ensure t
  :hook (python-mode . pipenv-mode))

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
             c-default-style "gnu"
             c-basic-offset 4
             parens-require-spaces nil
             show-paren-style 'parenthesis
             whitespace-style
             '(face
               tabs tab-mark
               spaces space-mark
               newline newline-mark
               trailing indentation empty missing-newline-at-eof)
             whitespace-display-mappings
             '((space-mark   ?\    [?\xB7]     [?.])
               (space-mark   ?\xA0 [?\xA4]     [?_])
               (newline-mark ?\n   [?\xB6 ?\n] [?$ ?\n]))
             ;;whitespace-style '(face trailing indentation empty missing-newline-at-eof)
             asm-comment-char ?#
             display-line-numbers-type 'normal)
            (whitespace-mode 1)
            (electric-pair-mode 1)
            (electric-indent-mode 1)
            (rainbow-delimiters-mode 1)
            (display-line-numbers-mode 1)))

(add-hook 'org-mode-hook
          (lambda ()
            (let ((display-line-numbers-type 'normal))
              (display-line-numbers-mode 1))))

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
   '("d19f00fe59f122656f096abbc97f5ba70d489ff731d9fa9437bac2622aaa8b89" "4aee8551b53a43a883cb0b7f3255d6859d766b6c5e14bcb01bed572fcbef4328" "fee7287586b17efbfda432f05539b58e86e059e78006ce9237b8732fde991b4c" "841b6a0350ae5029d6410d27cc036b9f35d3bf657de1c08af0b7cbe3974d19ac" "088c0504a552e4b00f671369411919373e3563f16bb790a1e3e3fd7d39a0e050" "6ed98f47da7556a8ce6280346e5d8e1e25bede71dc5186aa2654b93bec42d2a6" "308fc0c8cee43c5fccf3efa360c9cdf7d6bbbebc5c2f76850f1b1c8ac8fbaca0" "689e6661c79e93fd14d1765850522dbfcb3e49e8f15266516b64cf4f04649a4a" "661f3ff21fd9adc9facde2b11cbdeef5f1f2d482e98a456b11922bf55653ceac" default))
 '(package-selected-packages
   '(magit multi-vterm color-theme-sanityinc-solarized zenburn-theme modus-themes gruber-darker-theme hl-todo highlight-numbers seq package-utils gnu-elpa-keyring-update ef-theme ef-themes pipenv elpy use-package sudo-edit rust-mode rainbow-mode rainbow-delimiters markdown-mode drag-stuff company)))
