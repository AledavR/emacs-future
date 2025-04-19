;;; -*- lexical-binding: t -*-
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(+citar-library-local-path "Files/Documents/library")
 '(+citar-library-port "2222")
 '(+citar-library-server "ubuntu@rcaled.mooo.com")
 '(+citar-local-library-path "/home/rcaled/Files/Documents/library-test")
 '(+citar-remote-library-path "Files/Documents/library")
 '(bibtex-files
   '("/home/rcaled/.sync/archive/articles.bib"
     "/home/rcaled/.sync/archive/books.bib"))
 '(citar-templates
   '((main . "${author editor:30%sn}     ${date year issued:4}     ${title:48}")
     (suffix
      . "          ${=key= id:15}    ${=type=:12}    ${tags keywords keywords:*}")
     (preview
      . "${author editor:%etal} (${year issued date}) ${title}, ${journal journaltitle publisher container-title collection-title}.\12")
     (note . "Notas sobre ${author editor:%etal}, ${title}")))
 '(doc-view-resolution 300)
 '(fortran-line-length 500)
 '(ibuffer-saved-filter-groups
   '(("Groups" ("system" (starred-name)) ("erc" (used-mode . erc-mode)))))
 '(ibuffer-saved-filters
   '(("programming"
      (or (derived-mode . prog-mode) (mode . ess-mode) (mode . compilation-mode)))
     ("text document" (and (derived-mode . text-mode) (not (starred-name))))
     ("TeX"
      (or (derived-mode . tex-mode) (mode . latex-mode) (mode . context-mode)
          (mode . ams-tex-mode) (mode . bibtex-mode)))
     ("web"
      (or (derived-mode . sgml-mode) (derived-mode . css-base-mode)
          (derived-mode . js-base-mode) (derived-mode . typescript-ts-base-mode)
          (mode . js2-mode) (derived-mode . haml-mode) (mode . sass-mode)))
     ("gnus"
      (or (mode . message-mode) (mode . mail-mode) (mode . gnus-group-mode)
          (mode . gnus-summary-mode) (mode . gnus-article-mode)))))
 '(imagemagick-enabled-types
   '(3FR ARW AVS BMP BMP2 BMP3 CAL CALS CMYK CMYKA CR2 CRW CUR CUT DCM DCR DCX DDS
         DJVU DNG DPX EXR FAX FITS GBR GIF GIF87 GRB HRZ ICB ICO ICON J2C JNG
         JP2 JPC JPEG JPG JXL JPX K25 KDC MIFF MNG MRW MSL MSVG MTV NEF ORF OTB
         PBM PCD PCDS PCL PCT PCX PDB PEF PGM PICT PIX PJPEG PNG PNG24 PNG32
         PNG8 PNM PPM PSD PTIF PWP RAF RAS RBG RGB RGBA RGBO RLA RLE SCR SCT SFW
         SGI SR2 SRF SUN SVG SVGZ TGA TIFF TIFF64 TILE TIM TTF UYVY VDA VICAR
         VID VIFF VST WBMP WPG X3F XBM XC XCF XPM XV XWD YCbCr YCbCrA YUV))
 '(imagemagick-render-type 1)
 '(org-latex-preview-appearance-options
   '(:foreground auto :background "Transparent" :scale 1.0 :zoom 1.2 :page-width
                 0.8 :matchers ("begin" "$1" "$" "$$" "\\(" "\\[")))
 '(org-latex-preview-live '(inline block edit-special))
 '(org-latex-preview-live-debounce 0.5)
 '(org-latex-preview-live-display-type 'buffer)
 '(org-pretty-entities-include-sub-superscripts nil)
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
 '(reftex-default-bibliography '("/home/rcaled/.sync/archive/articles.bib"))
 '(reftex-plug-into-AUCTeX t)
 '(reftex-section-levels
   '(("part" . 0) ("chapter" . 1) ("section" . 2) ("subsection" . 3)
     ("subsubsection" . 4) ("paragraph" . 5) ("subparagraph" . 6)
     ("addchap" . -1) ("addsec" . -2) ("frametitle" . -2)))
 '(safe-local-variable-values
   '((org-cdlatex-mode . t) (TeX-command-extra-options . "-shell-escape")
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
 '(org-latex-and-related ((t (:foreground "#6fcfd2" :family "mononoki Nerd Font")))))
