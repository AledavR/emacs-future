;;; -*- lexical-binding: t -*-
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(citar-templates
   '((main . "${author editor:30%sn}     ${date year issued:4}     ${title:48}")
     (suffix
      . "          ${=key= id:15}    ${=type=:12}    ${tags keywords keywords:*}")
     (preview
      . "${author editor:%etal} (${year issued date}) ${title}, ${journal journaltitle publisher container-title collection-title}.\12")
     (note . "Notas sobre ${author editor:%etal}, ${title}")))
 '(doc-view-resolution 300)
 '(fortran-line-length 500)
 '(imagemagick-enabled-types
   '(3FR ARW AVS BMP BMP2 BMP3 CAL CALS CMYK CMYKA CR2 CRW CUR CUT DCM DCR DCX DDS
         DJVU DNG DPX EXR FAX FITS GBR GIF GIF87 GRB HRZ ICB ICO ICON J2C JNG
         JP2 JPC JPEG JPG JXL JPX K25 KDC MIFF MNG MRW MSL MSVG MTV NEF ORF OTB
         PBM PCD PCDS PCL PCT PCX PDB PEF PGM PICT PIX PJPEG PNG PNG24 PNG32
         PNG8 PNM PPM PSD PTIF PWP RAF RAS RBG RGB RGBA RGBO RLA RLE SCR SCT SFW
         SGI SR2 SRF SUN SVG SVGZ TGA TIFF TIFF64 TILE TIM TTF UYVY VDA VICAR
         VID VIFF VST WBMP WPG X3F XBM XC XCF XPM XV XWD YCbCr YCbCrA YUV))
 '(imagemagick-render-type 1)
 '(org-fold-catch-invisible-edits 'show)
 '(org-latex-preview-appearance-options
   '(:foreground auto :background "Transparent" :scale 1.4 :zoom 1.0 :page-width
                 0.8 :matchers ("begin" "$1" "$" "$$" "\\(" "\\[")))
 '(org-latex-preview-live '(inline block edit-special))
 '(org-latex-preview-live-debounce 0.5)
 '(org-latex-preview-live-display-type 'buffer)
 '(org-latex-preview-preamble
   "\\documentclass{minimal}\12[DEFAULT-PACKAGES]\12[PACKAGES]\12\\usepackage{amsmath}\12\\usepackage{amssymb}\12\\usepackage{xcolor}")
 '(org-pretty-entities-include-sub-superscripts t)
 '(org-safe-remote-resources
   '("\\`https://fniessen\\.github\\.io/org-html-themes/org/theme-bigblow\\.setup\\'"
     "\\`https://fniessen\\.github\\.io/org-html-themes/org/theme-readtheorg\\.setup\\'"))
 '(package-selected-packages
   '(ace-window auctex cape cdlatex circadian consult corfu dashboard diminish
                ef-themes eglot-java gnuplot htmlize kind-icon lua-mode magit
                marginalia markdown-mode meow olivetti orderless ox-pandoc
                pdf-tools rainbow-mode smartparens use-package vertico vterm
                vundo yasnippet))
 '(pdf-annot-default-annotation-properties
   '((t (label . "")) (text (color . "#ff0000") (icon . "Note"))
     (highlight (color . "yellow")) (underline (color . "blue"))
     (squiggly (color . "orange")) (strike-out (color . "red"))))
 '(pdf-annot-minor-mode-map-prefix [3 1])
 '(reftex-section-levels
   '(("part" . 0) ("chapter" . 1) ("section" . 2) ("subsection" . 3)
     ("subsubsection" . 4) ("paragraph" . 5) ("subparagraph" . 6)
     ("addchap" . -1) ("addsec" . -2) ("frametitle" . -2)))
 '(safe-local-variable-values
   '((eval add-hook 'after-save-hook (lambda nil (org-babel-tangle)) nil t)
     (eval add-hook 'after-save-hook (lambda nil (org-babel-detangle)) nil t)
     (ispell-dictionary . "en_US")))
 '(sentence-end "[a-z0-9)}][.:?][ ]")
 '(texmathp-tex-commands '(("alignedcase" env-on)))
 '(undo-tree-incompatible-major-modes '(image-mode term-mode))
 '(warning-minimum-level :error))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(corfu-default ((t (:inherit ef-themes-fixed-pitch :height 1.0))))
 '(ef-themes-fixed-pitch ((t (:height 0.8 :family "mononoki Nerd Font"))) t)
 '(fringe ((t :background "unspecified-bg")))
 '(header-line ((t :box (:line-width 4 :color "unspecified-bg" :style nil))))
 '(header-line-highlight ((t :box (:color "unspecified-fg"))))
 '(keycast-key ((t)))
 '(line-number ((t :background "unspecified-bg")))
 '(mode-line ((t :background "unspecified-bg" :overline "unspecified-fg" :box (:line-width 4 :color "unspecified-bg" :style nil))))
 '(mode-line-active ((t :background "unspecified-bg" :overline nil :box (:line-width 4 :color "unspecified-bg" :style nil))))
 '(mode-line-highlight ((t :box (:color "unspecified-fg"))))
 '(mode-line-inactive ((t :background "unspecified-bg" :overline nil :box (:line-width 4 :color "unspecified-bg" :style nil))))
 '(org-tag ((t nil)))
 '(tab-bar-tab ((t :box (:line-width 4 :color "grey" :style nil))))
 '(tab-bar-tab-inactive ((t :box (:line-width 4 :color "grey" :style nil))))
 '(tab-line-tab ((t)))
 '(tab-line-tab-active ((t)))
 '(tab-line-tab-current ((t)))
 '(tab-line-tab-inactive ((t)))
 '(variable-pitch ((t (:height 1.25 :family "ETBembo"))))
 '(vertical-border ((t :background "unspecified-bg" :foreground "unspecified-bg")))
 '(window-divider ((t (:background "unspecified-bg" :foreground "unspecified-bg"))))
 '(window-divider-first-pixel ((t (:background "unspecified-bg" :foreground "unspecified-bg"))))
 '(window-divider-last-pixel ((t (:background "unspecified-bg" :foreground "unspecified-bg")))))
