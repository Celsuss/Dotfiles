;;; packages.el --- my-ejc-sql layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2022 Sylvain Benner & Contributors
;;
;; Author: Jens <jenslord@jenslord-HP-ZBook-Fury-15-6-inch-G8-Mobile-Workstation-PC>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; See the Spacemacs documentation and FAQs for instructions on how to implement
;; a new layer:
;;
;;   SPC h SPC layers RET
;;
;;
;; Briefly, each package to be installed or configured by this layer should be
;; added to `my-ejc-sql-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `my-ejc-sql/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `my-ejc-sql/pre-init-PACKAGE' and/or
;;   `my-ejc-sql/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:

(defconst my-ejc-sql-packages
  '(clomacs
    (ejc-sql)
    )

  ;; '(clomacs
  ;;   (ejc-sql :location local)
  ;;   )

  "The list of Lisp packages required by the my-ejc-sql layer.

Each entry is either:

1. A symbol, which is interpreted as a package to be installed, or

2. A list of the form (PACKAGE KEYS...), where PACKAGE is the
    name of the package to be installed or loaded, and KEYS are
    any number of keyword-value-pairs.

    The following keys are accepted:

    - :excluded (t or nil): Prevent the package from being loaded
      if value is non-nil

    - :location: Specify a custom installation location.
      The following values are legal:

      - The symbol `elpa' (default) means PACKAGE will be
        installed using the Emacs package manager.

      - The symbol `local' directs Spacemacs to load the file at
        `./local/PACKAGE/PACKAGE.el'

      - A list beginning with the symbol `recipe' is a melpa
        recipe.  See: https://github.com/milkypostman/melpa#recipe-format")

;; init function, this is the main layer the initializes ejc-sql
(defun my-ejc-sql/init-ejc-sql ()
  (use-package ejc-sql
    :defer t
    :commands (ejc-sql-mode)
    :init (message "ejc-sql-init")
    :config
    (progn
      (message "ejc-sql-config")
      (my-ejc-sql/load-connection-file
       *my-ejc-sql-connections-file*)
      ;; (spacemacs/set-leader-keys-for-major-mode 'sql-mode
      ;;   "xx" 'ejc-eval-user-sql-at-point
      ;;   "xr" 'ejc-eval-user-sql-region
      ;;   "td" 'ejc-describe-table
      ;;   "r" 'ejc-show-last-result
      ;;   "lt" 'ejc-show-tables-list
      ;;   "as" 'ejc-strinp-sql-at-point
      ;;   "aj" 'ejc-dress-sql-at-point
      ;;   "c" 'ejc-connect
      ;;   "q" 'ejc-quit-connection
      ;;   "af" 'ejc-format-sql-at-point
      ;;   "m" 'ejc-mark-this-sql
      ;;   "lc" 'ejc-show-constraints-list
      ;;   "lp" 'ejc-show-procedures-list
      ;;   "L" 'ejc-open-log
      ;;   "b" 'ejc-switch-to-sql-editor-buffer
      ;;   "C" 'ejc-invalidate-cache)
      )))

(defun my-ejc-sql/init-clomacs ()
  (use-package clomacs))
;; packages.el ends here
