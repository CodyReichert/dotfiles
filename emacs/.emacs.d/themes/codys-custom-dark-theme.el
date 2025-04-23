;;; codys-custom-dark-theme.el ---  -*- lexical-binding: t; no-byte-compile: t; -*-

;;; Commentary:
;; This theme provides a dark editing experience using a specific
;; color palette, mapped to conventional Base16-style roles.

;;; Code:

;; Define the theme name
(deftheme codys-custom-dark "A custom dark theme with conventional color mapping.")

;; Define the color palette, mapped conventionally
(let (;; Backgrounds (Darkest to Lightest)
      (bg-base   "#000000") ; Original base00 - Deepest background
      (bg-alt    "#1e1e23") ; Original base04 - Slightly lighter background (e.g., line highlight, UI panels)
      (bg-select "#636363") ; Original base02 - Selection background, brighter UI elements

      ;; Foregrounds (Darkest to Brightest)
      (fg-comment "#6e7179") ; Original base03 - Comments, very dim UI text
      (fg-dark    "#c9c9c9") ; Original base07 - Darker foreground elements
      (fg-default "#d3dae3") ; Original base05 - Default text foreground
      (fg-light   "#e2e2e2") ; Original base06 - Lighter foreground elements (e.g., active mode line)
      (fg-bright  "#ffffff") ; Original base01 - Brightest foreground (cursor, highlights)

      ;; Accent Colors
      (accent-red     "#b00000") ; Original base08
      (accent-orange  "#e2e200") ; Original base09
      (accent-yellow  "#b0b000") ; Original base0A
      (accent-green   "#00b000") ; Original base0B
      (accent-cyan    "#00b0b0") ; Original base0C
      (accent-blue    "#2D82FC") ; Original base0D
      (accent-magenta "#b000b0") ; Original base0E
      (accent-brown   "#b55d00")) ; Original base0F

  ;; Define the core faces using the conventional mapping
  (custom-theme-set-faces
   'codys-custom-dark

   ;; --- Basic Faces ---
   `(default ((t (:background ,bg-base :foreground ,fg-default))))
   `(cursor ((t (:background ,accent-magenta :foreground ,accent-magenta)))) ; Bright cursor
   `(highlight ((t (:background ,bg-select :foreground ,fg-light :underline nil))))
   `(region ((t (:background ,bg-select :foreground ,fg-light :extend t))))
   `(secondary-selection ((t (:background ,bg-alt :foreground ,fg-light))))
   `(fringe ((t (:background ,bg-base :foreground ,fg-comment))))
   `(linum ((t (:background ,bg-base :foreground ,fg-comment))))
   `(line-number ((t (:background ,bg-base :foreground ,fg-comment))))
   `(line-number-current-line ((t (:background ,bg-alt :foreground ,fg-light :bold t))))
   `(hl-line ((t (:background ,bg-alt)))) ; Use the slightly lighter background

   ;; --- Mode Line ---
   `(mode-line ((t (:background ,bg-alt :foreground ,fg-light :box (:line-width -1 :color ,bg-select))))) ; Active mode line (brighter bg)
   `(mode-line-inactive ((t (:background ,bg-alt :foreground ,fg-comment :box (:line-width -1 :color ,bg-alt))))) ; Inactive mode line (dimmer bg)
   `(mode-line-buffer-id ((t (:foreground ,accent-cyan :bold t))))
   `(mode-line-emphasis ((t (:foreground ,fg-bright :bold t))))

   ;; --- Syntax Highlighting (font-lock) ---
   `(font-lock-builtin-face ((t (:foreground ,accent-magenta))))
   `(font-lock-comment-face ((t (:foreground ,fg-comment :italic t))))
   `(font-lock-comment-delimiter-face ((t (:foreground ,fg-comment :italic t))))
   `(font-lock-constant-face ((t (:foreground ,accent-red))))
   `(font-lock-doc-face ((t (:foreground ,accent-green :italic t))))
   `(font-lock-function-name-face ((t (:foreground ,accent-blue :bold nil))))
   `(font-lock-keyword-face ((t (:foreground ,accent-magenta :bold t))))
   `(font-lock-negation-char-face ((t (:foreground ,accent-red))))
   `(font-lock-preprocessor-face ((t (:foreground ,accent-cyan))))
   `(font-lock-regexp-grouping-backslash ((t (:foreground ,accent-orange))))
   `(font-lock-regexp-grouping-construct ((t (:foreground ,accent-magenta))))
   `(font-lock-string-face ((t (:foreground ,accent-green))))
   `(font-lock-type-face ((t (:foreground ,accent-yellow))))
   `(font-lock-variable-name-face ((t (:foreground ,accent-brown)))) ; Using brown for variables
   `(font-lock-warning-face ((t (:foreground ,accent-red :bold t :underline (:style wave :color ,accent-red)))))

   ;; --- Diffs (ediff, diff-mode) ---
   `(diff-added ((t (:foreground ,accent-green))))
   `(diff-removed ((t (:foreground ,accent-red))))
   `(diff-changed ((t (:foreground ,accent-orange))))
   `(diff-header ((t (:foreground ,accent-cyan :background ,bg-base))))
   `(diff-file-header ((t (:foreground ,accent-blue :background ,bg-base :bold t))))
   `(diff-hunk-header ((t (:foreground ,accent-yellow :background ,bg-alt))))
   `(diff-refine-added ((t (:background ,accent-green :foreground ,bg-base :bold t))))
   `(diff-refine-removed ((t (:background ,accent-red :foreground ,bg-base :bold t))))

   ;; --- Org Mode ---
   `(org-level-1 ((t (:foreground ,accent-blue :bold t))))
   `(org-level-2 ((t (:foreground ,accent-yellow :bold t))))
   `(org-level-3 ((t (:foreground ,accent-green :bold t))))
   `(org-level-4 ((t (:foreground ,accent-cyan :bold t))))
   `(org-level-5 ((t (:foreground ,accent-magenta :bold t))))
   `(org-level-6 ((t (:foreground ,accent-red :bold t))))
   `(org-level-7 ((t (:foreground ,accent-orange :bold t))))
   `(org-level-8 ((t (:foreground ,accent-brown :bold t))))
   `(org-block ((t (:background ,bg-alt :extend t)))) ; Code blocks background
   `(org-block-begin-line ((t (:background ,bg-alt :foreground ,fg-comment))))
   `(org-block-end-line ((t (:background ,bg-alt :foreground ,fg-comment))))
   `(org-code ((t (:foreground ,accent-green))))
   `(org-document-title ((t (:foreground ,fg-light :bold t :height 1.2))))
   `(org-document-info-keyword ((t (:foreground ,fg-comment))))
   `(org-tag ((t (:foreground ,accent-yellow :bold t))))
   `(org-link ((t (:foreground ,accent-blue :underline t))))
   `(org-done ((t (:foreground ,accent-green :bold t))))
   `(org-todo ((t (:foreground ,accent-red :bold t))))
   `(org-table ((t (:foreground ,accent-cyan))))
   `(org-verbatim ((t (:foreground ,accent-orange))))

   ;; --- Other UI Elements ---
   `(header-line ((t (:background ,bg-base :foreground ,fg-comment :box (:line-width -1 :color ,bg-alt)))))
   `(tooltip ((t (:background ,bg-alt :foreground ,fg-light))))
   `(vertical-border ((t (:foreground ,bg-alt)))) ; Window dividers
   `(minibuffer-prompt ((t (:foreground ,accent-blue :bold t))))
   `(escape-glyph ((t (:foreground ,accent-cyan))))
   `(error ((t (:foreground ,accent-red :bold t))))
   `(warning ((t (:foreground ,accent-orange :bold t))))
   `(success ((t (:foreground ,accent-green :bold t))))
   `(shadow ((t (:foreground ,fg-comment))))
   `(isearch ((t (:background ,accent-yellow :foreground ,bg-base :bold t))))
   `(lazy-highlight ((t (:background ,bg-select :foreground ,fg-light))))
   `(show-paren-match ((t (:background ,accent-magenta :foreground ,fg-bright)))) ; Use bright FG for contrast
   `(trailing-whitespace ((t (:background ,accent-red))))

   ;; Add more faces as needed for specific modes (e.g., magit, company, ivy, helm, etc.)
   ;; ...
   ))

;; Set Fira Code as the default font for text
(set-face-attribute 'default nil
                    :font "Fira Code"
                    :weight 'normal)

;; Set font for code, overriding the default
(set-face-attribute 'font-lock-keyword-face nil
                    :font "Fira Code"
                    :weight 'normal)

(set-face-attribute 'font-lock-builtin-face nil
                    :font "Fira Code"
                    :weight 'normal)

(set-face-attribute 'font-lock-function-name-face nil
                    :font "Fira Code"
                    :weight 'normal)

(set-face-attribute 'font-lock-type-face nil
                    :font "Fira Code"
                    :weight 'normal)

(set-face-attribute 'font-lock-string-face nil
                    :font "Fira Code"
                    :weight 'normal)

;;; Enable OpenType features for Fira Code
(when (display-graphic-p)
  (set-fontset-font t '(#x0000 . #xfffff) "Fira Code" nil '("liga" "dlig" "calt" "ss01" "ss02" "ss03" "ss04" "ss05" "ss06" "ss07" "ss08" "ss09" "ss10" "cv01" "cv02" "cv03" "cv04" "cv05" "cv06" "cv07" "cv08" "cv09" "cv10" "cv11" "cv12" "cv13" "cv14" "cv15" "cv16" "cv17" "cv18" "cv19" "cv20" "cv21" "cv22" "cv23" "cv24" "cv25" "cv26" "cv27" "cv28" "cv29" "cv30" "onum" "tnum" "zero")))

;;; Enable OpenType features for Fira Code
(when (display-graphic-p)
  (set-fontset-font t '(#x0000 . #xfffff) "Fira Code" nil '("liga" "dlig" "calt" "smcp" "onum" "tnum" "frac" "zero")))

;; You may want to set a specific size:
(set-face-attribute 'default nil :height 100) ; 15 points


;; Mark the theme as provided
(provide-theme 'codys-custom-dark)

;; Provide the feature
(provide 'codys-custom-dark-theme)

;;; codys-custom-dark-theme.el ends here
