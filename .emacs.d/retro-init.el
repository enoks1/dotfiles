;; Enviroment Variables
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
              use-package-always-ensure t
              inferior-lisp-program "sbcl"
              inhibit-startup-screen t
              fill-column 80
              whitespace-line-column nil)


;; MELPA
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

(package-initialize)

;; Look & Feel
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)

(icomplete-mode 1)

(use-package grandshell-theme
  :config (load-theme 'grandshell t))

(set-face-font 'default "-UW-Ttyp0-normal-normal-normal-*-15-*-*-*-c-80-iso10646-1")
(set-face-font 'font-lock-comment-delimiter-face "-jmk-Neep-normal-normal-normal-*-15-*-*-*-c-80-iso10646-1")

(use-package highlight-indentation)

;; Ligatures for 'Berkeley Mono'
(use-package ligature
  :config
  (ligature-set-ligatures
   't
   '(;; Group A
     ".." ".=" "..." "..<" "::" ":::" ":=" "::=" ";;" ";;;" "??" "???"
     ".?" "?." ":?" "?:" "?=" "**" "***" "/*" "*/" "/**"
     ;; Group B
     "<-" "->" "-<" ">-" "<--" "-->" "<<-" "->>" "-<<" ">>-" "<-<" ">->"
     "<-|" "|->" "-|" "|-" "||-" "<!--" "<#--" "<=" "=>" ">=" "<==" "==>"
     "<<=" "=>>" "=<<" ">>=" "<=<" ">=>" "<=|" "|=>" "<=>" "<==>" "||="
     "|=" "//=" "/="
     ;; Group C
     "<<" ">>" "<<<" ">>>" "<>" "<$" "$>" "<$>" "<+" "+>" "<+>" "<:" ":<"
     "<:<" ">:" ":>" "<~" "~>" "<~>" "<<~" "<~~" "~~>" "~~" "<|" "|>"
     "<|>" "<||" "||>" "<|||" "|||>" "</" "/>" "</>" "<*" "*>" "<*>" ":?>"
     ;; Group D
     "#(" "#{" "#[" "]#" "#!" "#?" "#=" "#_" "#_(" "##" "###" "####"
     ;; Group E
     "[|" "|]" "[<" ">]" "{!!" "!!}" "{|" "|}" "{{" "}}" "{{--" "--}}"
     "{!--" "//" "///" "!!"
     ;; Group F
     "www" "@_" "&&" "&&&" "&=" "~@" "++" "+++" "/\\" "\\/" "_|_" "||"
     ;; Group G
     "=:" "=:=" "=!=" "==" "===" "=/=" "=~" "~-" "^=" "__" "!=" "!==" "-~"
     "--" "---"))
  (global-ligature-mode -1))


;; Local Packages
(use-package highlight-numbers
  :disabled
  :hook ((prog-mode . highlight-numbers-mode)))

(use-package rainbow-delimiters
  :hook ((prog-mode . rainbow-delimiters-mode)))

(use-package hl-todo
  :hook ((prog-mode . hl-todo-mode)
         (elpy-mode . hl-todo-mode)))

(use-package rainbow-mode)

(use-package company
  :hook ((prog-mode . company-mode)))

(use-package magit)

(use-package multi-vterm
  :config
  (global-set-key (kbd "C-c v") 'multi-vterm-dedicated-open)
  (global-set-key (kbd "C-c d") 'multi-vterm-dedicated-open))

(use-package drag-stuff
  :config (drag-stuff-global-mode)
  :bind (("C-c j" . drag-stuff-down)
         ("C-c k" . drag-stuff-up)
         ("C-c h" . drag-stuff-left)
         ("C-c l" . drag-stuff-right)))


;; Keybinding
(global-set-key (kbd "C-c c") 'compile)
(global-set-key (kbd "C-c b") 'eval-buffer)
(global-set-key (kbd "C-c s") 'shell-command)
(global-set-key (kbd "C-c i") (lambda ()
                                (interactive)
                                (eldoc-minibuffer-message
                                 "Indenting buffer...")
                                (indent-region 0 (point-max))
                                (eldoc-minibuffer-message
                                 "Indenting buffer...done")))
(global-set-key (kbd "C-c m") 'comment-or-uncomment-region)


;; Programming
(add-hook 'prog-mode-hook
          (lambda ()
            (setq
             c-default-style "gnu"
             c-basic-offset 4
             parens-require-spaces nil
             show-paren-style 'parenthesis
             whitespace-display-mappings
             '((space-mark   ?\    [?\xB7]     [?.])
               (space-mark   ?\xA0 [?\xA4]     [?_])
               (newline-mark ?\n   [?\xB6 ?\n] [?$ ?\n]))
             whitespace-style
             '(face
               ;;tabs tab-mark
               ;;spaces space-mark
               ;;newline newline-mark
               space-before-tab
               space-after-tab
               indentation
               trailing
               lines
               empty
               missing-newline-at-eof)
             asm-comment-char ?#
             display-line-numbers-type 'normal)
            (whitespace-mode 1)
            (electric-pair-mode 1)
            (electric-indent-mode 1)
            (display-line-numbers-mode -1)
            (highlight-indentation-mode -1)
            (display-fill-column-indicator-mode -1)))

(add-hook 'org-mode-hook
          (lambda ()
            (let ((display-line-numbers-type 'normal))
              (display-line-numbers-mode -1))))

(add-hook 'c-mode-hook
          (lambda ()
            (setq comment-start "//" comment-end "")))

(add-hook 'python-mode-hook
          (lambda ()
            (setq python-indent-offset 4)))

(add-hook 'pascal-mode-hook
          (lambda ()
            (setq pascal-indent-level 4)))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("3860a842e0bf585df9e5785e06d600a86e8b605e5cc0b74320dfe667bcbe816c" "bbb13492a15c3258f29c21d251da1e62f1abb8bbd492386a673dcfab474186af" default))
 '(package-selected-packages
   '(grandshell-theme moe-theme multi-vterm drag-stuff magit company rainbow-mode hl-todo rainbow-delimiters highlight-numbers ligature highlight-indentation use-package gnu-elpa-keyring-update)))
