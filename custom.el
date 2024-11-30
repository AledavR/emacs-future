;;; -*- lexical-binding: t -*-
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(dired-listing-switches "-alh")
 '(doc-view-resolution 300)
 '(fortran-line-length 500)
 '(ignored-local-variable-values '((TeX-command-extra-options . "-shell-escape")))
 '(imagemagick-enabled-types
   '(3FR ARW AVS BMP BMP2 BMP3 CAL CALS CMYK CMYKA CR2 CRW CUR CUT DCM DCR DCX DDS
         DJVU DNG DPX EXR FAX FITS GBR GIF GIF87 GRB HRZ ICB ICO ICON J2C JNG
         JP2 JPC JPEG JPG JXL JPX K25 KDC MIFF MNG MRW MSL MSVG MTV NEF ORF OTB
         PBM PCD PCDS PCL PCT PCX PDB PEF PGM PICT PIX PJPEG PNG PNG24 PNG32
         PNG8 PNM PPM PSD PTIF PWP RAF RAS RBG RGB RGBA RGBO RLA RLE SCR SCT SFW
         SGI SR2 SRF SUN SVG SVGZ TGA TIFF TIFF64 TILE TIM TTF UYVY VDA VICAR
         VID VIFF VST WBMP WPG X3F XBM XC XCF XPM XV XWD YCbCr YCbCrA YUV))
 '(imagemagick-render-type 1)
 '(org-agenda-time-grid
   '((daily today require-timed) (800 1000 1200 1400 1600 1800 2000 2200) "......"
     "----------------") nil nil "Customized with use-package org")
 '(org-fold-catch-invisible-edits 'show)
 '(org-image-actual-width nil)
 '(org-latex-preview-appearance-options
   '(:foreground auto :background "Transparent" :scale 1.4 :zoom 1.0 :page-width
                 0.6 :matchers ("begin" "$1" "$" "$$" "\\(" "\\[")))
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
 '(reftex-section-levels
   '(("part" . 0) ("chapter" . 1) ("section" . 2) ("subsection" . 3)
     ("subsubsection" . 4) ("paragraph" . 5) ("subparagraph" . 6)
     ("addchap" . -1) ("addsec" . -2) ("frametitle" . -2)))
 '(safe-local-variable-values
   '((eval add-hook 'org-babel-post-tangle-hook 'rc/config-insert-footer)
     (eval add-hook 'after-save-hook (lambda nil (org-babel-tangle)) nil t)
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
 )
