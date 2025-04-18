;;; codys-custom-light-theme.el --- A custom light theme based on user-provided colors -*- lexical-binding: t; no-byte-compile: t; -*-

;;; Commentary:
;; This theme provides a light editing experience using a specific
;; color palette, mapped to conventional Base16-style light roles.

;;; Code:

;; Define the theme name
(deftheme codys-custom-light "A custom light theme with conventional color mapping.")

;; Define the color palette, mapped conventionally for a light theme
(let (;; Backgrounds (Lightest to Darkest)
      (bg-base   "#fdfcf0") ; Changed from #ffffff to a soft, slightly yellow off-white
      (bg-alt    "#e2e2e2") ; Original base06 - Slightly darker background (UI elements)
      (bg-select "#d3dae3") ; Original base05 - Selection background, slightly darker UI

      ;; Foregrounds (Lightest to Darkest)
      (fg-comment "#6e7179") ; Original base07 - Comments (contrast checked against new bg-base)
      (fg-default "#333333") ; Original base03 - Default text foreground
      (fg-dark    "#000") ; Original base02 - Darker foreground elements
      (fg-darkest "#000") ; Original base04 - Darkest foreground (UI text)
      (fg-cursor  "#00b0b0") ; Original base00 - Cursor foreground (contrast checked against new bg-base on cursor bg)

      ;; Accent Colors (Same as dark theme, contrast may vary)
      (accent-red     "#b00000") ; Original base08
      (accent-orange  "#e2e200") ; Original base09
      (accent-yellow  "#b0b000") ; Original base0A
      (accent-green   "#00b000") ; Original base0B
      (accent-cyan    "#00b0b0") ; Original base0C
      (accent-blue    "#0000d1") ; Original base0D
      (accent-magenta "#b000b0") ; Original base0E
      (accent-brown   "#b55d00")) ; Original base0F

  ;; Define the core faces using the conventional light mapping
  (custom-theme-set-faces
   'codys-custom-light

 ;; --- Basic Faces ---
   `(default ((t (:background ,bg-base :foreground ,fg-default))))
   `(cursor ((t (:background ,fg-darkest :foreground ,bg-base))))
   `(highlight ((t (:background ,bg-select :foreground ,fg-darkest :underline nil))))
   `(region ((t (:background ,bg-select :foreground ,fg-darkest :extend t))))
   `(secondary-selection ((t (:background ,bg-alt :foreground ,fg-darkest))))
   `(fringe ((t (:background ,bg-alt :foreground ,fg-comment)))) ; Make fringe match line numbers bg
   `(linum ((t (:background ,bg-alt :foreground ,fg-dark))))
   `(line-number ((t (:background ,bg-alt :foreground ,fg-dark))))
   `(line-number-current-line ((t (:background ,bg-select :foreground ,fg-darkest :bold t))))
   `(hl-line ((t (:background ,bg-alt))))

   ;; --- Mode Line ---
   `(mode-line ((t (:background ,bg-alt :foreground ,fg-dark :box (:line-width -1 :color ,fg-dark))))) ; Active mode line (dark bg)
   `(mode-line-inactive ((t (:background ,bg-alt :foreground ,fg-dark :box (:line-width -1 :color ,bg-alt))))) ; Inactive mode line (light bg)
   `(mode-line-buffer-id ((t (:foreground ,accent-blue :bold t))))
   `(mode-line-emphasis ((t (:foreground ,accent-red :bold t)))) ; Use a different accent for emphasis

   ;; --- Syntax Highlighting (font-lock) ---
   ;; Using same accents, check contrast visually
   `(font-lock-builtin-face ((t (:foreground ,accent-magenta))))
   `(font-lock-comment-face ((t (:foreground ,fg-comment :italic t)))) ; Lighter comment color
   `(font-lock-comment-delimiter-face ((t (:foreground ,fg-comment :italic t))))
   `(font-lock-constant-face ((t (:foreground ,accent-red))))
   `(font-lock-doc-face ((t (:foreground ,accent-green :italic t)))) ; Check contrast for green string
   `(font-lock-function-name-face ((t (:foreground ,accent-blue :bold nil))))
   `(font-lock-keyword-face ((t (:foreground ,accent-magenta :bold t))))
   `(font-lock-negation-char-face ((t (:foreground ,accent-red))))
   `(font-lock-preprocessor-face ((t (:foreground ,accent-cyan))))
   `(font-lock-regexp-grouping-backslash ((t (:foreground ,accent-orange))))
   `(font-lock-regexp-grouping-construct ((t (:foreground ,accent-magenta))))
   `(font-lock-string-face ((t (:foreground ,accent-green)))) ; Check contrast
   `(font-lock-type-face ((t (:foreground ,accent-yellow)))) ; Check contrast
   `(font-lock-variable-name-face ((t (:foreground ,accent-brown))))
   `(font-lock-warning-face ((t (:foreground ,accent-red :bold t :underline (:style wave :color ,accent-red)))))

   ;; --- Diffs (ediff, diff-mode) ---
   `(diff-added ((t (:foreground ,accent-green))))
   `(diff-removed ((t (:foreground ,accent-red))))
   `(diff-changed ((t (:foreground ,accent-orange))))
   `(diff-header ((t (:foreground ,accent-cyan :background ,bg-base))))
   `(diff-file-header ((t (:foreground ,accent-blue :background ,bg-base :bold t))))
   `(diff-hunk-header ((t (:foreground ,accent-yellow :background ,bg-alt))))
   `(diff-refine-added ((t (:background ,accent-green :foreground ,bg-base :bold t)))) ; Check contrast
   `(diff-refine-removed ((t (:background ,accent-red :foreground ,bg-base :bold t)))) ; Check contrast

   ;; --- Org Mode ---
   ;; Using same accents, check contrast visually
   `(org-level-1 ((t (:foreground ,accent-blue :bold t))))
   `(org-level-2 ((t (:foreground ,accent-yellow :bold t))))
   `(org-level-3 ((t (:foreground ,accent-green :bold t))))
   `(org-level-4 ((t (:foreground ,accent-cyan :bold t))))
   `(org-level-5 ((t (:foreground ,accent-magenta :bold t))))
   `(org-level-6 ((t (:foreground ,accent-red :bold t))))
   `(org-level-7 ((t (:foreground ,accent-orange :bold t))))
   `(org-level-8 ((t (:foreground ,accent-brown :bold t))))
   `(org-block ((t (:background ,bg-alt :extend t))))
   `(org-block-begin-line ((t (:background ,bg-alt :foreground ,fg-comment))))
   `(org-block-end-line ((t (:background ,bg-alt :foreground ,fg-comment))))
   `(org-code ((t (:foreground ,accent-green))))
   `(org-document-title ((t (:foreground ,fg-darkest :bold t :height 1.2))))
   `(org-document-info-keyword ((t (:foreground ,fg-comment))))
   `(org-tag ((t (:foreground ,accent-yellow :bold t))))
   `(org-link ((t (:foreground ,accent-blue :underline t))))
   `(org-done ((t (:foreground ,accent-green :bold t))))
   `(org-todo ((t (:foreground ,accent-red :bold t))))
   `(org-table ((t (:foreground ,accent-cyan))))
   `(org-verbatim ((t (:foreground ,accent-orange))))

   ;; --- Other UI Elements ---
   `(header-line ((t (:background ,bg-base :foreground ,fg-dark :box (:line-width -1 :color ,bg-alt)))))
   `(tooltip ((t (:background ,bg-alt :foreground ,fg-darkest))))
   `(vertical-border ((t (:foreground ,bg-alt))))
   `(minibuffer-prompt ((t (:foreground ,accent-blue :bold t))))
   `(escape-glyph ((t (:foreground ,accent-cyan))))
   `(error ((t (:foreground ,accent-red :bold t))))
   `(warning ((t (:foreground ,accent-orange :bold t))))
   `(success ((t (:foreground ,accent-green :bold t))))
   `(shadow ((t (:foreground ,fg-comment))))
   `(isearch ((t (:background ,accent-yellow :foreground ,fg-darkest :bold t)))) ; Check contrast
   `(lazy-highlight ((t (:background ,bg-select :foreground ,fg-darkest))))
   `(show-paren-match ((t (:background ,accent-magenta :foreground ,bg-base)))) ; Check contrast
   `(trailing-whitespace ((t (:background ,accent-red))))

   ;; Add more faces as needed
   ;; ...
   ))

;; Mark the theme as provided
(provide-theme 'codys-custom-light)

;; Provide the feature
(provide 'codys-custom-light-theme)

;;; codys-custom-light-theme.el ends here
