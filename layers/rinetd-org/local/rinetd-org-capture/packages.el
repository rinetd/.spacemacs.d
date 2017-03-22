;;; packages.el --- rinetd-org-capture layer packages file for Spacemacs.
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
;; added to `rinetd-org-capture-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `rinetd-org-capture/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `rinetd-org-capture/pre-init-PACKAGE' and/or
;;   `rinetd-org-capture/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:

(defconst rinetd-org-capture-packages
  '(
    (org-capture :location built-in)
)
)

(defun rinetd-org-capture/init-org-capture ()
  (with-eval-after-load 'org-capture
    ; (require 'org-capture)

    ;; Capure 模板
    ;; 所有 caputre 的 task 都先暂存入 inbox.org 中，再 refile 到各个 org 文件中
    ;; 我们将 task 划分为一下几类:
    ;
    ; A phone call(p)
    ; A meeting (m)
    ; An email I need to respond to (r)
    ; A new task (t)
    ; A new note (n)
    ; An interruption (j)
    ; A new habit (h)
    ;
    ;; Capture templates for: TODO tasks, phone calls, meetings

    ;; the %i would copy the selected text into the template
    ;;http://www.howardism.org/Technical/Emacs/journaling-org.html
    ;;add multi-file journal
    (message "rinetd-org-capture")
    (setq org-capture-templates
      '(("t" "Todo" entry (file+headline (concat my-org-gtd-directory "inbox.org") "Tasks")
             "** TODO %? %^G\n  Created: %U \n  %i")
        ("T" "Scheduled Todo" entry (file+headline (concat my-org-gtd-directory "inbox.org") "Tasks")
             "** TODO %? %^G\n SCHEDULED: %^{ Sheduled: }T Created: %U \n  %i")
        ("m" "Meeting" entry (file+headline (concat my-org-gtd-directory "inbox.org") "Tasks")
             "* MEETING with %? :Meeting:\n%U" :clock-in t :clock-resume t)
        ("p" "Phone call" entry (file+headline (concat my-org-gtd-directory "inbox.org") "Tasks")
             "* PHONE %? :Phone:\n%U" :clock-in t :clock-resume t)
        ))

    ;; Set default column view headings: Task Effort Clock_Summary
    (setq org-columns-default-format
          ; "%50ITEM(Task) %10TODO %3PRIORITY %TAGS %10Effort(Effort){:} %10CLOCKSUM"
          "%38ITEM(Details) %TAGS(Context) %7TODO(To Do) %5Effort(Time){:} %6CLOCKSUM{Total}")
    ;; global Effort estimate values
    ;; global STYLE property values for completion
    (setq org-global-properties (quote (("Effort_ALL" . "0:15 0:30 0:45 1:00 2:00 3:00 4:00 5:00 6:00 8:00")
                                        ("STYLE_ALL" . "habit"))))

    ;; Tags with fast selection keys
    (setq org-tag-alist (quote ((:startgroup)
                                ("Office" . ?O)
                                ("Home" . ?H)
                                (:endgroup)
                                ("Computer" . ?c)
                                ("Reading" . ?r)
                                ("Project" . ?p))))
))
;;; packages.el ends here
