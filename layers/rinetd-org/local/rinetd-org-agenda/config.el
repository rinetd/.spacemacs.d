;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3
;(setq-default org-directory "~/org-notes/")

;(setq-default org-agenda-files (find-lisp-find-files 'org-directory "\.org$"))

;(setq org-default-notes-file (expand-file-name "notes.org" org-directory))

;(setq org-agenda-diary-file (f-join (cb-org-directory) "diary.org"))
;; 设置 agenda 的数据来源
;; org-agenda-files 中的元素还可以是目录，这时目录下的所有匹配
;; `org-agenda-file-regexp’的文件都自动加入 agenda .
(setq org-agenda-files
      (list
       (concat org-directory "inbox.org")
       (concat org-directory "project.org")

       (concat org-directory "task.org")
       (concat org-directory "finished.org")
       (concat org-directory "trash.org")
       (concat org-directory "memorial-day.org")
       ))
;;; config.el ends here
