;; [[file:dotemacs.org::*Init config][Init config:1]]
(mapc (lambda(string)
        (add-to-list 'load-path (locate-user-emacs-file string)))
      '("ext" "lib"))

(require 'package-manager)
(require 'rca-macros)
(require 'rca-functions)
(require 'rca-ui)
(require 'rca-emacs)
(require 'rca-org)
(require 'rca-prog)
(require 'rca-completion)
(require 'rca-minibuffer)
(require 'rca-project)
(require 'rca-keyboard)
(require 'rca-tex)
(require 'rca-tools)
;; Init config:1 ends here
