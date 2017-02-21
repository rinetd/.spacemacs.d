;;; funcs.el --- zhexuany Layer packages File for Spacemacs
;;
;; Copyright (c) 2015-2016 zhexuany
;;
;; Author: zhexuany <guanghui8827@gmail.com>
;; URL: https://github.com/zhexuany/spacemacs-private
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3
(require 'cl)

(setq octopress-workdir (expand-file-name "~/4gamers.cn/"))

(defun zhexuany/octopress-rake (command)
  "run rake commands"
  (let ((command-str (format "/bin/bash -l -c 'source $HOME/.rvm/scripts/rvm && rvm use ruby 2.0.0  && cd %s && rake %s'" octopress-workdir command)))
    (shell-command-to-string command-str)))

(defun zhexuany/octopress-qrsync (command)
  (let ((command-str (format "/usr/local/bin/qrsync %s" command )))
    (shell-command-to-string command-str)))

(defun zhexuany/octopress-generate ()
  "generate jekyll site"
  (interactive)
  (zhexuany/octopress-rake "generate")
  (message "Generate site OK"))

(defun zhexuany/octopress-deploy ()
  "default deploy task"
  (interactive)
  (zhexuany/octopress-rake "deploy")
  (zhexuany/octopress-qrsync "/Users/guanghui/4gamers.cn/guanghui.json")
  (message "Deploy site OK"))

(defun zhexuany/octopress-gen-deploy ()
  "generate website and deploy"
  (interactive)
  (zhexuany/octopress-rake "gen_deploy")
  (zhexuany/octopress-qrsync "/Users/guanghui/4gamers.cn/guanghui.json")
  (message "Generate and Deploy OK"))

(defun zhexuany/octopress-upimg ()
  (interactive)
  (zhexuany/octopress-qrsync "/Users/guanghui/4gamers.cn/guanghui.json")
  (message "Up Img to Qiniu"))

(defun directory-parent (directory)
  (let ((parent (file-name-directory (directory-file-name directory))))
    (if (not (equal directory parent))
        parent)))

(defun zhexuany/jekyll-serve ()
  (interactive)
  (let* ((default-directory
           (if (string-match "_posts/$" default-directory)
               (directory-parent (directory-parent default-directory))
             (directory-parent default-directory)))
         (buffer (if (get-buffer "*jekyll*")
                     (switch-to-buffer "*jekyll*")
                   (ansi-term "/bin/zsh" "jekyll")))
         (proc (get-buffer-process buffer)))
    (term-send-string proc "rake generate && rake preview\n")
    (sit-for 4)
    (browse-url "http://localhost:4000")))

(defun zhexuany/hotspots ()
  "helm interface to my hotspots, which includes my locations,
org-files and bookmarks"
  (interactive)
  (helm :buffer "*helm: utities*"
        :sources `(,(zhexuany//hotspots-sources))))

(defun zhexuany//hotspots-sources ()
  "Construct the helm sources for my hotspots"
  `((name . "Mail and News")
   (candidates . (("Calendar" . (lambda ()  (browse-url "https://www.google.com/calendar/render")))
                  ("RSS" . elfeed)
                  ("Blog" . org-octopress)
                  ("Github" . (lambda() (helm-github-stars)))
                  ("Calculator" . (lambda () (helm-calcul-expression)))
                  ("Run current flie" . (lambda () (zhexuany/run-current-file)))
                  ("Agenda" . (lambda () (org-agenda "" "a")))
                  ("sicp" . (lambda() (browse-url "http://mitpress.mit.edu/sicp/full-text/book/book-Z-H-4.html#%_toc_start")))))
   (candidate-number-limit)
   (action . (("Open" . (lambda (x) (funcall x)))))))


;; Screenshot
(defun zhexuany//insert-org-or-md-img-link (prefix imagename)
  (if (equal (file-name-extension (buffer-file-name)) "org")
      (insert (format "[[%s%s]]" prefix imagename))
    (insert (format "![%s](%s%s)" imagename prefix imagename))))

(defun zhexuany/capture-screenshot (basename)
  "Take a screenshot into a time stamped unique-named file in the
  same directory as the org-buffer/markdown-buffer and insert a link to this file."
  (interactive "sScreenshot name: ")
  (if (equal basename "")
      (setq basename (format-time-string "%Y%m%d_%H%M%S")))
  (setq fullpath
        (concat (file-name-directory (buffer-file-name))
                "../images/posts/"
                (file-name-base (buffer-file-name))
                "_"
                basename))
  (setq relativepath
        (concat (file-name-base (buffer-file-name))
                "_"
                basename
                ".png"))
  (if (file-exists-p (file-name-directory fullpath))
      (progn
        (setq final-image-full-path (concat fullpath ".png"))
        (call-process "screencapture" nil nil nil "-s" final-image-full-path)
        (if (executable-find "convert")
            (progn
              (setq resize-command-str (format "convert %s -resize 800x600 %s" final-image-full-path final-image-full-path))
              (shell-command-to-string resize-command-str)))
        (zhexuany//insert-org-or-md-img-link "http://guanghuiqu.qiniudn.com/" relativepath))
    (progn
      (call-process "screencapture" nil nil nil "-s" (concat basename ".png"))
      (zhexuany//insert-org-or-md-img-link "./" (concat basename ".png"))))
  (insert "\n"))


;; insert ; at the end of current line
(defun zhexuany/insert-semicolon-at-the-end-of-this-line ()
  (interactive)
  (save-excursion
    (end-of-line)
    (insert ";")))

(defun zhexuany/insert-comma-at-the-end-of-this-line ()
  (interactive)
  (save-excursion
    (end-of-line)
    (insert ",")))

(defmacro zhexuany|toggle-company-backends (backend)
  "Push or delete the backend to company-backends"
  (let ((funsymbol (intern (format "zilong/company-toggle-%S" backend))))
    `(defun ,funsymbol ()
       (interactive)
       (if (eq (car company-backends) ',backend)
           (setq-local company-backends (delete ',backend company-backends))
         (push ',backend company-backends)))))

