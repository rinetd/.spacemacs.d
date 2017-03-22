;;; packages.el --- rinetd-calendar layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2017 Sylvain Benner & Contributors
;;
;; Author: ubuntu <guanghui8827@gmail.com>
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
;; added to `rinetd-calendar-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `rinetd-calendar/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `rinetd-calendar/pre-init-PACKAGE' and/or
;;   `rinetd-calendar/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:

(defconst rinetd-calendar-packages
  '(calfw)
  "The list of Lisp packages required by the rinetd-calendar layer.

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
(defun rinetd-org/init-calfw ()
  (use-package calfw
    ;; :defer t
    :config
    (progn
      (setq cfw:org-overwrite-default-keybinding t)

      ;; Grid frame
      (setq cfw:fchar-junction ?╬
            cfw:fchar-vertical-line ?║
            cfw:fchar-horizontal-line ?═
            cfw:fchar-left-junction ?╠
            cfw:fchar-right-junction ?╣
            cfw:fchar-top-junction ?╦
            cfw:fchar-top-left-corner ?╔
            cfw:fchar-top-right-corner ?╗)

      (defun rinetd/cfw-render-toolbar (width current-view prev-cmd next-cmd)
        "Translate words: 'Month', 'Week', 'Day' and 'Two day' to Chinese"
        (let* ((prev (cfw:render-button " < " prev-cmd))
               (today (cfw:render-button "今天" 'cfw:navi-goto-today-command))
               (next (cfw:render-button " > " next-cmd))
               (month (cfw:render-button
                       "显示一月" 'cfw:change-view-month
                       (eq current-view 'month)))
               (tweek (cfw:render-button
                       "显示两周" 'cfw:change-view-two-weeks
                       (eq current-view 'two-weeks)))
               (week (cfw:render-button
                      "显示一周" 'cfw:change-view-week
                      (eq current-view 'week)))
               (day (cfw:render-button
                     "显示一天" 'cfw:change-view-day
                     (eq current-view 'day)))
               (sp  " ")
               (toolbar-text
                (cfw:render-add-right
                 width (concat sp prev sp next sp today sp)
                 (concat day sp week sp tweek sp month sp))))
          (cfw:render-default-content-face toolbar-text 'cfw:face-toolbar)))

      (advice-add 'cfw:render-toolbar :override #'rinetd/cfw-render-toolbar)

      (defun rinetd/calendar ()
        (interactive)
        (cfw:open-calendar-buffer
         :view 'month
         :contents-sources
         (list
          ;; orgmode source
          (cfw:org-create-source "Green"))))
      )))

;;; packages.el ends here
