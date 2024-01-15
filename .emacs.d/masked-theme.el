;; GPL 3

(deftheme masked ()
          "Masked theme for GNU Emacs")

(let ((masked-bg          "#000000")
      (masked-bg-1        "#1f1f1f")
      (masked-bg-2        "#191919")

      (masked-red         "#a34443")
      (masked-green       "#8ba446")
      (masked-yellow      "#987d3e")
      (masked-blue        "#496f94")
      (masked-magenta     "#897399")
      (masked-cyan        "#518a8a")

      (masked-fg          "#bbbbbb")
      (masked-fg-1        "#969696")

      (masked-blue-alt    "#004daa")
      (masked-magenta-alt "#c617e6")

      (masked-gold        "#ffd700")
      (masked-black       "#000000")
      (masked-white       "#ffffff"))

  (custom-theme-set-faces
   'masked

   ;; general
   `(cursor ((t (:background ,masked-gold))))
   `(default ((t ,(list :foreground masked-fg :background masked-bg))))
   `(fringe ((t ,(list :foreground masked-fg :background masked-bg))))
   `(minibuffer-prompt ((t (:foreground ,masked-white))))
   `(region ((t (:foreground ,masked-white :background ,masked-blue-alt))))
   `(link ((t (:foreground ,masked-magenta-alt :underline t))))
   `(link-visited ((t (:foreground ,masked-magenta :underline t))))

   ;; font-lock
   `(font-lock-builtin-face ((t (:foreground ,masked-green))))
   `(font-lock-comment-face ((t (:foreground ,masked-blue))))
   `(font-lock-comment-delimiter-face ((t (:foreground ,masked-blue))))
   `(font-lock-constant-face ((t (:foreground ,masked-cyan))))
   `(font-lock-doc-face ((t (:foreground ,masked-green))))
   `(font-lock-function-name-face ((t (:foreground ,masked-fg))))
   `(font-lock-keyword-face ((t (:foreground ,masked-yellow :weight medium))))
   `(font-lock-preprocessor-face ((t (:foreground ,masked-magenta))))
   `(font-lock-string-face ((t (:foreground ,masked-red))))
   `(font-lock-type-face ((t (:foreground ,masked-magenta))))
   `(font-lock-variable-name-face ((t (:foreground ,masked-fg))))
   `(font-lock-warning-face ((t (:foreground ,masked-red))))
   `(font-lock-negation-char-face ((t (:foreground ,masked-red))))

   ;; mode-line
   `(mode-line ((t ,(list :foreground masked-black :background masked-yellow))))
   `(mode-line-inactive ((t ,(list :foreground masked-black :background masked-fg-1))))
   `(mode-line-buffer-id ((t ,(list :weight 'medium))))

   ;; line-numbers
   `(line-number ((t (:inherit default :foreground ,masked-bg-2 :background ,masked-bg))))
   `(line-number-current-line ((t (:inherit line-number :foreground ,masked-bg-1))))

   ;; whitespace
   `(whitespace-space ((t (:foreground ,masked-bg-2 :background ,masked-bg ))))
   `(whitespace-tab ((t (:foreground ,masked-bg-2 :background ,masked-bg ))))

   ;; org
   `(org-date ((t (:foreground ,masked-blue :background ,masked-bg))))
   `(org-hide ((t (:foreground ,masked-fg-1 :background ,masked-bg))))
   `(org-todo ((t (:foreground ,masked-red :background ,masked-bg))))
   `(org-done ((t (:foreground ,masked-green :background ,masked-bg))))
   `(org-headline-done ((t (:inherit org-done))))
   `(org-level-1 ((t (:foreground ,masked-blue :background ,masked-bg))))
   `(org-level-2 ((t (:foreground ,masked-cyan :background ,masked-bg))))
   `(org-level-3 ((t (:foreground ,masked-gold :background ,masked-bg))))
   `(org-level-4 ((t (:foreground ,masked-green :background ,masked-bg))))
   `(org-level-5 ((t (:foreground ,masked-magenta :background ,masked-bg))))

   ;; highlight-numbers
   `(highlight-numbers-number ((t (:foreground ,masked-red))))))

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'masked)
