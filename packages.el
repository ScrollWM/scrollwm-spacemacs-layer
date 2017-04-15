;;; packages.el --- scrollwm layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author:  Ole Jørgen and Tor Hedin
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;; Commentary:

;; See the Spacemacs documentation and FAQs for instructions on how to implement
;; a new layer:
;;
;;   SPC h SPC layers RET
;;
;;
;; Briefly, each package to be installed or configured by this layer should be
;; added to `scrollwm-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `scrollwm/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `scrollwm/pre-init-PACKAGE' and/or
;;   `scrollwm/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:

(defconst scrollwm-packages
  '(company
    js2-mode
    dbus
    (scrollwm-mode :location local)
    (company-scrollwm :location local)
    flycheck)
  "The list of Lisp packages required by the notion layer.

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

(defun scrollwm/init-dbus ()
  (use-package dbus
    :commands (dbus-call-method)))

(defun scrollwm/init-scrollwm-mode ()
  (use-package scrollwm-mode
    :commands (scrollwm-mode)
    :config
    (progn
      ;; (spacemacs/set-leader-keys-for-major-mode 'scrollwm-mode "db" 'scrollwm-send-buffer)
      (spacemacs/set-leader-keys-for-major-mode 'scrollwm-mode
        "sb" 'scrollwm-send-buffer
        "sf" 'scrollwm-send-proc
        "sl" 'scrollwm-send-current-line
        "sr" 'scrollwm-send-region
        "hh" 'scrollwm-look-up-function-at-point)
      (spacemacs/declare-prefix-for-mode 'scrollwm-mode "mh" "documentation")
      (spacemacs/declare-prefix-for-mode 'scrollwm-mode "ms" "send to Notion"))
    )
  )

(defun scrollwm/init-company-scrollwm ()
  (use-package company-scrollwm
    :if (configuration-layer/package-usedp 'company)
    :commands (company-scrollwm)
    :init
    (progn
      ;; (require 'company)
      (spacemacs|add-company-backends :backends company-scrollwm :modes scrollwm-mode)

      )))

(defun scrollwm/post-init-flycheck ()
  (spacemacs/add-flycheck-hook 'scrollwm-mode))

(defun scrollwm/post-init-scrollwm-mode ()
  ;; (spacemacs|add-company-hook scrollwm-mode)
  )

;;; packages.el ends here
