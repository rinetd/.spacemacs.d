;;; packages.el --- zhexuan Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2014 Sylvain Benner
;; Copyright (c) 2014-2015 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;; List of all packages to install and/or initialize. Built-in packages
;; which require an initialization must be listed explicitly in the list.
(setq zhexuan-packages
      '(
        projectile
        (org :location built-in)
        prodigy
        org-tree-slide
        js2-mode
        find-file-in-project
        org-bullets
        ;; evil-escape
        (cc-mode :location built-in)
        youdao-dictionary
        ;; chinese-wbim
        multiple-cursors
        visual-regexp-steroids
        nodejs-repl
        company-c-headers
        ;; hydra
        lispy
        org-octopress
        helm-github-stars
        evil
        deft
        elfeed
        lua-mode
        ycmd
        mwe-log-commands
        org-pomodoro
        discover-my-major
        popwin
        ox-reveal
        org-mac-link
        ace-window
        avy
        4clojure
        persp-mode
        osx-dictionary
        litable
        ))

(defun zhexuan/init-litable ()
  (use-package litable
    :init))

(defun zhexuan/init-osx-dictionary ()
  (use-package osx-dictionary
    :init
    (progn
      (evilified-state-evilify osx-dictionary-mode osx-dictionary-mode-map)
      (setq osx-dictionary-use-chinese-text-segmentation t)
      (global-set-key (kbd "C-c d") 'osx-dictionary-search-pointer)
      )))


(defun zhexuan/init-4clojure ()
  (use-package 4clojure
    :init
    (progn
      (spacemacs/declare-prefix "o4" "4clojure")
      (evil-leader/set-key "o4q" '4clojure-open-question)
      (evil-leader/set-key "o4n" '4clojure-next-question)
      (evil-leader/set-key "o4p" '4clojure-previous-question)
      (evil-leader/set-key "o4c" '4clojure-check-answers)
      )))


(defun zhexuan/init-ox-reveal ()
  (use-package ox-reveal
    :defer t
    :init
    (progn
      (setq org-reveal-root "file:///Users/yzx/.emacs.d/reveal-js"))))

(defun zhexuan/init-org-mac-link ()
  (use-package org-mac-link
    :init
    (add-hook 'org-mode-hook
              (lambda ()
                (define-key org-mode-map (kbd "C-c g") 'org-mac-grab-link)))))

(defun zhexuan/post-init-avy ()
  (use-package avy
    :defer t
    :init
    (progn
      (global-set-key (kbd "C-s-'") 'avy-goto-char-2))))

(defun zhexuan/post-init-ace-window ()
  (use-package ace-window
    :defer t
    :init
    (global-set-key (kbd "C-x C-o") #'ace-window)))

(defun zhexuan/init-discover-my-major ()
  (use-package discover-my-major
    :defer t
    :init
    (progn
      (evil-leader/set-key (kbd "mhm") 'discover-my-major)

      (evilified-state-evilify makey-key-mode makey-key-mode-get-key-map))))

(defun zhexuan/init-mwe-log-commands ()
  (use-package mwe-log-commands
    :init
    (progn
      (spacemacs/declare-prefix "ol" "command log")
      (evil-leader/set-key
        "oll" 'mwe:log-keyboard-commands
        "olf" 'mwe:open-command-log-buffer))))

(defun zhexuan/post-init-ycmd ()
  (setq ycmd-tag-files 'auto)
  (setq ycmd-request-message-level -1)
  (set-variable 'ycmd-server-command `("python" ,(expand-file-name "~/github/ycmd/ycmd/__main__.py")))
  (setq company-backends-c-mode-common '((company-c-headers
                                          company-dabbrev-code
                                          company-keywords
                                          company-gtags :with company-yasnippet)
                                         company-files company-dabbrev ))

  (zhexuany|toggle-company-backends company-ycmd)
  (eval-after-load 'ycmd
    '(spacemacs|hide-lighter ycmd-mode))

  (evil-leader/set-key-for-mode 'c-mode
    "tb" 'zilong/company-toggle-company-ycmd)
  (evil-leader/set-key-for-mode 'c++-mode
    "tb" 'zilong/company-toggle-company-ycmd))

(defun zhexuan/post-init-lua-mode ()
  (use-package lua-mode
    :defer t
    :config
    (progn
      (when (configuration-layer/package-usedp 'company)
        (push 'company-dabbrev company-backends-lua-mode)
        (push 'company-etags company-backends-lua-mode))
      (add-hook 'lua-mode-hook 'evil-matchit-mode)
      (add-hook 'lua-mode-hook 'smartparens-mode)
      (setq lua-indent-level 4)

      (evil-leader/set-key-for-mode 'lua-mode
        "<tab>" 'hs-toggle-hiding
        "gg" 'helm-gtags-dwim
        "gr" 'helm-gtags-find-rtag
        "gs" 'helm-gtags-find-symbol
        "gf" 'helm-gtags-find-files))))

(defun zhexuan/init-elfeed ()
  (use-package elfeed
    :init
    (global-set-key (kbd "C-x w") 'elfeed)
    :defer t
    :config
    (progn

      (setq elfeed-feeds
            '("http://nullprogram.com/feed/"
              "http://z.caudate.me/rss/"
              "http://irreal.org/blog/?feed=rss2"
              "http://feeds.feedburner.com/LostInTheTriangles"
              "http://tonybai.com/feed/"
              "http://planet.emacsen.org/atom.xml"
              "http://feeds.feedburner.com/emacsblog"
              "http://blog.binchen.org/rss.xml"
              "http://oremacs.com/atom.xml"
              "http://blog.gemserk.com/feed/"
              "http://www.masteringemacs.org/feed/"
              "http://t-machine.org/index.php/feed/"
              "http://gameenginebook.blogspot.com/feeds/posts/default"
              "http://feeds.feedburner.com/ruanyifeng"
              "http://coolshell.cn/feed"
              "http://blog.devtang.com/atom.xml"
              "http://emacsist.com/rss"
              "http://puntoblogspot.blogspot.com/feeds/2507074905876002529/comments/default"
              "http://angelic-sedition.github.io/atom.xml"))

      ;; (evilify elfeed-search-mode elfeed-search-mode-map)
      (evilified-state-evilify-map elfeed-search-mode-map
        :mode elfeed-search-mode
        :bindings
        "G" 'elfeed-update
        "g" 'elfeed-search-update--force)

      (defun elfeed-mark-all-as-read ()
        (interactive)
        (mark-whole-buffer)
        (elfeed-search-untag-all-unread))

      (define-key elfeed-search-mode-map (kbd "R") 'elfeed-mark-all-as-read)

      (defadvice elfeed-show-yank (after elfeed-show-yank-to-kill-ring activate compile)
        "Insert the yanked text from x-selection to kill ring"
        (kill-new (x-get-selection)))

      (ad-activate 'elfeed-show-yank))))

(defun zhexuan/post-init-evil ()
  (use-package evil
    :init
    (progn
      (push "TAGS" spacemacs-useless-buffers-regexp)

      ;; make underscore as word_motion.
      (modify-syntax-entry ?_ "w")
      ;; ;; change evil initial mode state
      (loop for (mode . state) in
            '((shell-mode . normal))
            do (evil-set-initial-state mode state))

      ;;mimic "nzz" behaviou in vim
      (defadvice evil-ex-search-next (after advice-for-evil-search-next activate)
        (evil-scroll-line-to-center (line-number-at-pos)))

      (defadvice evil-ex-search-previous (after advice-for-evil-search-previous activate)
        (evil-scroll-line-to-center (line-number-at-pos)))

      (define-key evil-normal-state-map (kbd ",/") 'evilnc-comment-or-uncomment-lines)

      (define-key evil-normal-state-map
        (kbd "Y") 'zhexuany/yank-to-end-of-line)

      ;; rebind g,k to gj and gk
      (define-key evil-normal-state-map (kbd "j") 'evil-next-visual-line)
      (define-key evil-normal-state-map (kbd "k") 'evil-previous-visual-line)

      (define-key evil-normal-state-map (kbd "[ SPC") (lambda () (interactive) (evil-insert-newline-above) (forward-line)))
      (define-key evil-normal-state-map (kbd "] SPC") (lambda () (interactive) (evil-insert-newline-below) (forward-line -1)))


      (define-key evil-normal-state-map (kbd "[ b") 'spacemacs/previous-useful-buffer)
      (define-key evil-normal-state-map (kbd "] b") 'spacemacs/next-useful-buffer)

      ;; (define-key evil-insert-state-map "\C-e" 'end-of-line)
      ;; (define-key evil-insert-state-map "\C-n" 'next-line)
      ;; (define-key evil-insert-state-map "\C-k" 'kill-line)
      (define-key evil-emacs-state-map (kbd "s-f") 'forward-word)
      (define-key evil-emacs-state-map (kbd "s-b") 'backward-word)

      (evil-leader/set-key "bi" 'ibuffer)
      (define-key evil-ex-completion-map "\C-a" 'move-beginning-of-line)
      (define-key evil-ex-completion-map "\C-b" 'backward-char)
      (define-key evil-ex-completion-map "\C-k" 'kill-line)
      (define-key minibuffer-local-map (kbd "C-w") 'evil-delete-backward-word)

      (define-key evil-visual-state-map (kbd ">") 'prelude-shift-right-visual)
      (define-key evil-visual-state-map (kbd "<") 'prelude-shift-left-visual)
      (define-key evil-visual-state-map (kbd ",/") 'evilnc-comment-or-uncomment-lines)
      ;; (define-key evil-visual-state-map (kbd "x") 'er/expand-region)
      ;; (define-key evil-visual-state-map (kbd "X") 'er/contract-region)
      (define-key evil-visual-state-map (kbd "C-r") 'zhexuany/evil-quick-replace)

      ;; in spacemacs, we always use evilify miscro state
      (evil-add-hjkl-bindings package-menu-mode-map 'emacs)

      ;; (define-key evil-emacs-state-map (kbd "C-w h") 'evil-window-left)
      (define-key evil-emacs-state-map (kbd "C-w") 'evil-delete-backward-word)
      ;; (define-key evil-emacs-state-map (kbd "C-w j") 'evil-window-down)
      ;; (define-key evil-emacs-state-map (kbd "C-w k") 'evil-window-up)
      ;; (define-key evil-emacs-state-map (kbd "C-w l") 'evil-window-right)

      ;; for emacs shell mode
      ;; (define-key evil-emacs-state-map (kbd "s-b") 'ido-switch-buffer)
      ;; (define-key evil-emacs-state-map (kbd "s-f") 'ido-find-file)
      (evil-define-key 'emacs term-raw-map (kbd "C-w")
        'evil-delete-backward-word)

      (evil-leader/set-key "fR" 'zhexuany/rename-file-and-buffer)
      (evil-leader/set-key "bms" 'bookmark-set)
      (evil-leader/set-key "bmr" 'bookmark-rename)
      (evil-leader/set-key "bmd" 'bookmark-delete)

      ;; enable hybrid editing style
      (defadvice evil-insert-state (around zhexuany/holy-mode activate)
        "Preparing the holy water flasks."
        (evil-emacs-state))
      ;; disable c-[ temporally
      ;; (define-key input-decode-map [?\C-\[] (kbd "<C-[>"))
      ;; (bind-keys ("<C-[>" . evil-normal-state))
      ;; (setq evil-emacs-state-cursor '("chartreuse3" (bar . 2)))
      (define-key evil-emacs-state-map [escape] 'evil-normal-state)


      )))

(defun zhexuan/init-helm-github-stars ()
  (use-package helm-github-stars
    :defer t
    :config
    (progn
      (setq helm-github-stars-username "zhexuany")
      (setq helm-github-stars-cache-file "~/.emacs.d/.cache/hgs-cache"))))

(defun zhexuan/init-org-octopress ()
  (use-package org-octopress
    :init
    (progn
      (evilified-state-evilify org-octopress-summary-mode org-octopress-summary-mode-map)
      (add-hook 'org-octopress-summary-mode-hook
                #'(lambda () (local-set-key (kbd "q") 'bury-buffer)))
      (setq org-blog-dir "~/4gamers.cn/")
      (setq org-octopress-directory-top org-blog-dir)
      (setq org-octopress-directory-posts (concat org-blog-dir "source/_posts"))
      (setq org-octopress-directory-org-top org-blog-dir)
      (setq org-octopress-directory-org-posts (concat org-blog-dir "blog"))
      (setq org-octopress-setup-file (concat org-blog-dir "setupfile.org"))

      (defun zhexuany/org-save-and-export ()
        (interactive)
        (org-octopress-setup-publish-project)
        (org-publish-project "octopress" t))

      (evil-leader/set-key "op" 'zhexuany/org-save-and-export)
      )))

(defun zhexuan/post-init-lispy ()
  (use-package lispy
    :defer t
    :config
    (progn
      (define-key lispy-mode-map (kbd "s-1") 'lispy-describe-inline)
      (define-key lispy-mode-map (kbd "s-2") 'lispy-arglist-inline))))

(defun zhexuan/init-hydra ()
  (use-package hydra
    :init
    (progn
      ;; major mode hydra is really cool, don't need to switch mode anymore
      ;; C-c [a-z] and s-[a-z] is very quick to pressed even in emacs-state and F1-F9 is also the same
      ;; If the command will change the buffer, they should be put in these groups.
      ;; otherwise, use which-key + spacems + user defined key mappings in evil normal mode
      (defhydra hydra-yasnippet (:color blue :hint nil)
        "
              ^YASnippets^
--------------------------------------------
  Modes:    Load/Visit:    Actions:

 _g_lobal  _d_irectory    _i_nsert
 _m_inor   _f_ile         _t_ryout
 _e_xtra   _l_ist         _n_ew
         _a_ll
"
        ("d" yas-load-directory)
        ("e" yas-activate-extra-mode)
        ("i" yas-insert-snippet)
        ("f" yas-visit-snippet-file :color blue)
        ("n" yas-new-snippet)
        ("t" yas-tryout-snippet)
        ("l" yas-describe-tables)
        ("g" yas/global-mode)
        ("m" yas/minor-mode)
        ("a" yas-reload-all))

      ;; (bind-key* "<f3>" 'hydra-yasnippet/body)

      (defhydra hydra-apropos (:color blue)
        "Apropos"
        ("a" apropos "apropos")
        ("c" apropos-command "cmd")
        ("d" apropos-documentation "doc")
        ("e" apropos-value "val")
        ("l" apropos-library "lib")
        ("o" apropos-user-option "option")
        ("u" apropos-user-option "option")
        ("v" apropos-variable "var")
        ("i" info-apropos "info")
        ("t" tags-apropos "tags")
        ("z" hydra-customize-apropos/body "customize"))

      (defhydra hydra-customize-apropos (:color blue)
        "Apropos (customize)"
        ("a" customize-apropos "apropos")
        ("f" customize-apropos-faces "faces")
        ("g" customize-apropos-groups "groups")
        ("o" customize-apropos-options "options"))

      (bind-key*  "<f4>" 'hydra-apropos/body)
      )))

(defun zhexuan/post-init-company-c-headers ()
  (use-package company-c-headers
    :defer t
    :init (progn
            (setq company-c-headers-path-system
                  (quote
                   ("/usr/include/" "/usr/local/include/" "/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/include/c++/v1")))
            (setq company-c-headers-path-user
                  (quote
                   ("/Users/zhexuan/cocos2d-x/cocos/platform" "/Users/zhexuan/cocos2d-x/cocos" "." "/Users/zhexuan/cocos2d-x/cocos/audio/include/"))))))

(defun zhexuan/post-init-nodejs-repl ()
  (progn
    (spacemacs/declare-prefix-for-mode 'js2-mode
                                       "ms" "REPL")
    (evil-leader/set-key-for-mode 'js2-mode
      "sb" 'nodejs-repl-eval-buffer
      "sf" 'nodejs-repl-eval-function
      "sd" 'nodejs-repl-eval-dwim)))

(defun zhexuan/post-init-visual-regexp-steroids ()
  (progn
    (define-key global-map (kbd "C-c r") 'vr/replace)
    (define-key global-map (kbd "C-c q") 'vr/query-replace)))

(defun zhexuan/init-multiple-cursors ()
  (use-package multiple-cursors
    :init
    (progn
      (bind-key* "C-s-l" 'mc/edit-lines)
      (bind-key* "C-s-f" 'mc/mark-all-dwim)
      (bind-key* "C-s-." 'mc/mark-next-like-this)
      (bind-key* "C-s-," 'mc/mark-previous-like-this)
      (bind-key* "s->" 'mc/unmark-next-like-this)
      (bind-key* "s-<" 'mc/unmark-previous-like-this)
      (bind-key* "C-c C-s-." 'mc/mark-all-like-this)

      ;; http://endlessparentheses.com/multiple-cursors-keybinds.html?source=rss
      (define-prefix-command 'endless/mc-map)
      ;; C-x m is usually `compose-mail'. Bind it to something
      ;; else if you use this command.
      (define-key ctl-x-map "m" 'endless/mc-map)
;;; Really really nice!
      (define-key endless/mc-map "i" #'mc/insert-numbers)
      (define-key endless/mc-map "h" #'mc-hide-unmatched-lines-mode)
      (define-key endless/mc-map "a" #'mc/mark-all-like-this)

;;; Occasionally useful
      (define-key endless/mc-map "d" #'mc/mark-all-symbols-like-this-in-defun)
      (define-key endless/mc-map "r" #'mc/reverse-regions)
      (define-key endless/mc-map "s" #'mc/sort-regions)
      (define-key endless/mc-map "l" #'mc/edit-lines)
      (define-key endless/mc-map "\C-a" #'mc/edit-beginnings-of-lines)
      (define-key endless/mc-map "\C-e" #'mc/edit-ends-of-lines)
      )))

(defun zhexuan/post-init-persp-mode ()
  (spacemacs|define-custom-layout "@Cocos2D-X"
    :binding "c"
    :body
    (find-file "~/cocos2d-x/cocos/ui/UIWidget.cpp")
    (split-window-right)
    (find-file "~/cocos2d-x/cocos/cocos2d.cpp")))

(defun zhexuan/post-init-chinese-wbim ()
  (progn
    ;; [[http://emacs.stackexchange.com/questions/352/how-to-override-major-mode-bindings][keymap - How to override major mode bindings - Emacs Stack Exchange]]
    (bind-key* ";" 'chinese-wbim-insert-ascii)
    (setq chinese-wbim-punc-translate-p nil)
    (spacemacs/declare-prefix "ot" "Toggle")
    (evil-leader/set-key
      "otp" 'chinese-wbim-punc-translate-toggle)
    (setq chinese-wbim-wb-use-gbk t)
    (add-hook 'chinese-wbim-wb-load-hook
              (lambda ()
                (let ((map (chinese-wbim-mode-map)))
                  (define-key map "-" 'chinese-wbim-previous-page)
                  (define-key map "=" 'chinese-wbim-next-page))))
    ))

(defun zhexuan/post-init-youdao-dictionary ()
  (evil-leader/set-key "oy" 'youdao-dictionary-search-at-point+))

(defun zhexuan/post-init-cc-mode ()
  (use-package cc-mode
    :init

    ;; http://stackoverflow.com/questions/23553881/emacs-indenting-of-c11-lambda-functions-cc-mode
    (defadvice c-lineup-arglist (around my activate)
      "Improve indentation of continued C++11 lambda function opened as argument."
      (setq ad-return-value
            (if (and (equal major-mode 'c++-mode)
                     (ignore-errors
                       (save-excursion
                         (goto-char (c-langelem-pos langelem))
                         ;; Detect "[...](" or "[...]{". preceded by "," or "(",
                         ;;   and with unclosed brace.
                         (looking-at ".*[(,][ \t]*\\[[^]]*\\][ \t]*[({][^}]*$"))))
                0                           ; no additional indent
              ad-do-it)))                   ; default behavior


    (setq c-default-style "linux") ;; set style to "linux"
    (setq c-basic-offset 4)
    (c-set-offset 'substatement-open 0)
    (with-eval-after-load 'c++-mode
      (define-key c++-mode-map (kbd "s-.") 'company-ycmd)))
  ;; company backend should be grouped
  )


(defun zhexuan/post-init-evil-escape ()
  (setq evil-escape-delay 0.2))

(defun zhexuan/post-init-org-bullets ()
  (setq org-bullets-bullet-list '("🐉" "🐠" "🐬" "🐤")))

(defun zhexuan/post-init-find-file-in-project ()
  (progn
    (defun zhexuany/search-in-fireball ()
      (interactive)
      (helm-do-ag (expand-file-name "~/github/fireball/")))
    (evil-leader/set-key "os" 'zhexuany/search-in-fireball)

    ;; If you use other VCS (subversion, for example), enable the following option
    ;;(setq ffip-project-file ".svn")
    ;; in MacOS X, the search file command is CMD+p
    (bind-key* "s-p" 'find-file-in-project)
    ;; for this project, I'm only interested certain types of files
    ;; (setq-default ffip-patterns '("*.html" "*.js" "*.css" "*.java" "*.xml" "*.js"))
    ;; if the full path of current file is under SUBPROJECT1 or SUBPROJECT2
    ;; OR if I'm reading my personal issue track document,
    (defadvice find-file-in-project (before my-find-file-in-project activate compile)
      (when (ffip-current-full-filename-match-pattern-p "\\(/fireball\\)")
        ;; set the root directory into "~/projs/PROJECT_DIR"
        (setq-local ffip-project-root "~/github/fireball")
        ;; well, I'm not interested in concatenated BIG js file or file in dist/
        (setq-local ffip-find-options "-not -size +64k -not -iwholename '*/bin/*'")
        ;; do NOT search files in below directories, the default value is better.
        ;; (setq-default ffip-prune-patterns '(".git" ".hg" "*.svn" "node_modules" "bower_components" "obj"))
        )
      (when (ffip-current-full-filename-match-pattern-p "\\(/cocos2d-x\\)")
        ;; set the root directory into "~/projs/PROJECT_DIR"
        (setq-local ffip-project-root "~/cocos2d-x")
        ;; well, I'm not interested in concatenated BIG js file or file in dist/
        (setq-local ffip-find-options "-not -size +64k -not -iwholename '*/bin/*'")
        ;; do NOT search files in below directories, the default value is better.
        ;; (setq-default ffip-prune-patterns '(".git" ".hg" "*.svn" "node_modules" "bower_components" "obj"))
        ))
    (ad-activate 'find-file-in-project)))

(defun zhexuany/post-init-deft ()
  (setq deft-use-filter-string-for-filename t)
  (evil-leader/set-key-for-mode 'deft-mode "q" 'quit-window)
  (setq deft-extension "org")
  (setq deft-directory "~/org-notes"))

(defun zhexuan/post-init-org-pomodoro ()
  (use-package org-pomodoro
    :init
    :defer t
    :config
    (progn
      (add-hook 'org-pomodoro-finished-hook '(lambda () (zhexuany/growl-notification "Pomodoro Finished" "☕️ Have a break!" t)))
      (add-hook 'org-pomodoro-short-break-finished-hook '(lambda () (zhexuany/growl-notification "Short Break" "🐝 Ready to Go?" t)))
      (add-hook 'org-pomodoro-long-break-finished-hook '(lambda () (zhexuany/growl-notification "Long Break" " 💪 Ready to Go?" t)))
      )))

(defun zhexuan/post-init-js2-mode ()
  (progn
    (setq company-backends-js2-mode '((company-dabbrev-code
                                       company-keywords
                                       company-etags) company-files company-dabbrev))

    (zhexuany|toggle-company-backends company-tern)


    (evil-leader/set-key-for-mode 'js2-mode
      "tb" 'zilong/company-toggle-company-tern)

    (evil-leader/set-key-for-mode 'js2-mode
      "ga" 'projectile-find-other-file
      "gA" 'projectile-find-other-file-other-window)

    (evil-leader/set-key-for-mode 'web-mode
      "ga" 'projectile-find-other-file
      "gA" 'projectile-find-other-file-other-window)
    (eval-after-load 'js2-mode
      '(progn
         (add-hook 'js2-mode-hook (lambda () (setq mode-name "JS2")))
         (define-key js2-mode-map   (kbd "s-.") 'company-tern)))))

(defun zhexuan/init-org-tree-slide ()
  (use-package org-tree-slide
    :init
    (evil-leader/set-key "oto" 'org-tree-slide-mode)))


(defun zhexuan/post-init-projectile ()
  (evil-leader/set-key "pf" 'zhexuany/open-file-with-projectile-or-lsgit))

(defun zhexuan/post-init-org ()
  (with-eval-after-load 'org
    (progn
      ;; define the refile targets
      (setq org-agenda-files (quote ("~/org-notes" )))
      (setq org-default-notes-file "~/org-notes/gtd.org")

      (with-eval-after-load 'org-agenda
        (define-key org-agenda-mode-map (kbd "P") 'org-pomodoro))
      ;; the %i would copy the selected text into the template
      ;;http://www.howardism.org/Technical/Emacs/journaling-org.html
      ;;add multi-file journal
      (setq org-capture-templates
            '(("t" "Todo" entry (file+headline "~/org-notes/gtd.org" "Workspace")
               "* TODO %?\n  %i\n"
               :empty-lines 1)
              ("n" "notes" entry (file+headline "~/org-notes/notes.org" "Quick notes")
               "* TODO [#C] %?\n  %i\n %U"
               :empty-lines 1)
              ("b" "Blog Ideas" entry (file+headline "~/org-notes/notes.org" "Blog Ideas")
               "* TODO %?\n  %i\n %U"
               :empty-lines 1)
              ("w" "work" entry (file+headline "~/org-notes/gtd.org" "Cocos2D-X")
               "* TODO %?\n  %i\n %U"
               :empty-lines 1)
              ("c" "Chrome" entry (file+headline "~/org-notes/notes.org" "Quick notes")
               "* TODO %?\n %(zhexuany/retrieve-chrome-current-tab-url)\n %i\n %U"
               :empty-lines 1)
              ("l" "links" entry (file+headline "~/org-notes/notes.org" "Quick notes")
               "* TODO %?\n  %i\n %a \n %U"
               :empty-lines 1)
              ("j" "Journal Entry"
               entry (file+datetree "~/org-notes/journal.org")
               "* %?"
               :empty-lines 1)))

      ;;An entry without a cookie is treated just like priority ' B '.
      ;;So when create new task, they are default 重要且紧急
      (setq org-agenda-custom-commands
            '(
              ("w" . "任务安排")
              ("wa" "重要且紧急的任务" tags-todo "+PRIORITY=\"A\"")
              ("wb" "重要且不紧急的任务" tags-todo "-Weekly-Monthly-Daily+PRIORITY=\"B\"")
              ("wc" "不重要且紧急的任务" tags-todo "+PRIORITY=\"C\"")
              ("b" "Blog" tags-todo "BLOG")
              ("p" . "项目安排")
              ("pw" tags-todo "PROJECT+WORK+CATEGORY=\"cocos2d-x\"")
              ("pl" tags-todo "PROJECT+DREAM+CATEGORY=\"zhexuany\"")
              ("W" "Weekly Review"
               ((stuck "")            ;; review stuck projects as designated by org-stuck-projects
                (tags-todo "PROJECT") ;; review all projects (assuming you use todo keywords to designate projects)
                ))))

      (defvar zhexuany-website-html-preamble
        "<div class='nav'>
<ul>
<li><a href='http://zhexuany.com'>博客</a></li>
<li><a href='/index.html'>Wiki目录</a></li>
</ul>
</div>")
      (defvar zhexuany-website-html-blog-head
        " <link rel='stylesheet' href='css/site.css' type='text/css'/> \n
    <link rel=\"stylesheet\" type=\"text/css\" href=\"/css/worg.css\"/>")
      (setq org-publish-project-alist
            `(
              ("blog-notes"
               :base-directory "~/org-notes"
               :base-extension "org"
               :publishing-directory "~/org-notes/public_html/"

               :recursive t
               :html-head , zhexuany-website-html-blog-head
               :publishing-function org-html-publish-to-html
               :headline-levels 4           ; Just the default for this project.
               :auto-preamble t
               :exclude "gtd.org"
               :exclude-tags ("ol" "noexport")
               :section-numbers nil
               :html-preamble ,zhexuany-website-html-preamble
               :author "zhexuany"
               :email "zhexuan8827@gmail.com"
               :auto-sitemap t               ; Generate sitemap.org automagically...
               :sitemap-filename "index.org" ; ... call it sitemap.org (it's the default)...
               :sitemap-title "我的wiki"     ; ... with title 'Sitemap'.
               :sitemap-sort-files anti-chronologically
               :sitemap-file-entry-format "%t" ; %d to output date, we don't need date here
               )
              ("blog-static"
               :base-directory "~/org-notes"
               :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf"
               :publishing-directory "~/org-notes/public_html/"
               :recursive t
               :publishing-function org-publish-attachment
               )
              ("blog" :components ("blog-notes" "blog-static"))))

      (defun org-summary-todo (n-done n-not-done)
        "Switch entry to DONE when all subentries are done, to TODO otherwise."
        (let (org-log-done org-log-states)  ; turn off logging
          (org-todo (if (= n-not-done 0) "DONE" "TODO"))))

      (add-hook 'org-after-todo-statistics-hook 'org-summary-todo)
      ;; used by org-clock-sum-today-by-tags
      (defun filter-by-tags ()
        (let ((head-tags (org-get-tags-at)))
          (member current-tag head-tags)))

      (defun org-clock-sum-today-by-tags (timerange &optional tstart tend noinsert)
        (interactive "P")
        (let* ((timerange-numeric-value (prefix-numeric-value timerange))
               (files (org-add-archive-files (org-agenda-files)))
               (include-tags '("WORK" "EMACS" "DREAM" "WRITING" "MEETING"
                               "LIFE" "PROJECT" "OTHER"))
               (tags-time-alist (mapcar (lambda (tag) `(,tag . 0)) include-tags))
               (output-string "")
               (tstart (or tstart
                           (and timerange (equal timerange-numeric-value 4) (- (org-time-today) 86400))
                           (and timerange (equal timerange-numeric-value 16) (org-read-date nil nil nil "Start Date/Time:"))
                           (org-time-today)))
               (tend (or tend
                         (and timerange (equal timerange-numeric-value 16) (org-read-date nil nil nil "End Date/Time:"))
                         (+ tstart 86400)))
               h m file item prompt donesomething)
          (while (setq file (pop files))
            (setq org-agenda-buffer (if (file-exists-p file)
                                        (org-get-agenda-file-buffer file)
                                      (error "No such file %s" file)))
            (with-current-buffer org-agenda-buffer
              (dolist (current-tag include-tags)
                (org-clock-sum tstart tend 'filter-by-tags)
                (setcdr (assoc current-tag tags-time-alist)
                        (+ org-clock-file-total-minutes (cdr (assoc current-tag tags-time-alist)))))))
          (while (setq item (pop tags-time-alist))
            (unless (equal (cdr item) 0)
              (setq donesomething t)
              (setq h (/ (cdr item) 60)
                    m (- (cdr item) (* 60 h)))
              (setq output-string (concat output-string (format "[-%s-] %.2d:%.2d\n" (car item) h m)))))
          (unless donesomething
            (setq output-string (concat output-string "[-Nothing-] Done nothing!!!\n")))
          (unless noinsert
            (insert output-string))
          output-string))
      (global-set-key (kbd "C-c a") 'org-agenda)
      (define-key org-mode-map (kbd "s-p") 'org-priority)
      (define-key global-map (kbd "<f9>") 'org-capture)
      (global-set-key (kbd "C-c b") 'org-iswitchb)
      (define-key evil-normal-state-map (kbd "C-c C-w") 'org-refile)
      (evil-leader/set-key-for-mode 'org-mode
        "owh" 'plain-org-wiki-helm
        "owf" 'plain-org-wiki)
      (setq org-mobile-directory "~/org-notes/org")
      )))

(defun zhexuan/post-init-prodigy ()
  (prodigy-define-tag
    :name 'jekyll
    :env '(("LANG" "en_US.UTF-8")
           ("LC_ALL" "en_US.UTF-8")))
  ;; define service
  (prodigy-define-service
    :name "Preview cocos2d-x web"
    :command "python"
    :args '("-m" "SimpleHTTPServer" "6001")
    :cwd "~/cocos2d-x/web"
    :tags '(work)
    :kill-signal 'sigkill
    :kill-process-buffer-on-stop t)

  (prodigy-define-service
    :name "Preview cocos2d-html5"
    :command "python"
    :args '("-m" "SimpleHTTPServer" "6004")
    :cwd "~/github/fireball/engine"
    :tags '(work)
    :kill-signal 'sigkill
    :kill-process-buffer-on-stop t)

  (prodigy-define-service
    :name "Hexo Server"
    :command "hexo"
    :args '("server")
    :cwd "~/4gamers.cn"
    :tags '(hexo server)
    :kill-signal 'sigkill
    :kill-process-buffer-on-stop t)

  (prodigy-define-service
    :name "Hexo Deploy"
    :command "hexo"
    :args '("deploy" "--generate")
    :cwd "~/4gamers.cn"
    :tags '(hexo deploy)
    :kill-signal 'sigkill
    :kill-process-buffer-on-stop t)

  (prodigy-define-service
    :name "Debug Fireball"
    :command "gulp"
    :args '("fireball" "--path" "/Users/zhexuan/workspace/fireball/HelloWorld/")
    :cwd "~/github/fireball/"
    :tags '(work)
    :kill-signal 'sigkill
    :kill-process-buffer-on-stop t)

  (prodigy-define-service
    :name "Org wiki preview"
    :command "python"
    :args '("-m" "SimpleHTTPServer" "8088")
    :cwd "~/org-notes/public_html"
    :tags '(org-mode)
    :init (lambda () (browse-url "http://localhost:8088"))
    :kill-signal 'sigkill
    :kill-process-buffer-on-stop t))

(defun zhexuan/init-moz-controller ()
  (use-package moz-controller
    :init
    (moz-controller-global-mode t)
    :diminish moz-controller-mode))
