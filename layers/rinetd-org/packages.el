;;; packages.el --- rinetd-org layer packages file for Spacemacs.
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
;; added to `rinetd-org-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `rinetd-org/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `rinetd-org/pre-init-PACKAGE' and/or
;;   `rinetd-org/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:

(defconst rinetd-org-packages
  '(
    deft
    ;; (epa-file :location built-in)
    (org-crypt :location built-in)
    (org :location built-in)
    (org-capture :location built-in)
    (org-agenda :location built-in)
    (org-archive :location built-in)
    (org-clock :location built-in)
    (org-faces :location built-in)
    (org-list :location built-in)
    (org-bullets :location built-in)
                                        ; org-password-manager
    (org-src :location built-in)
    (ob-core :location built-in)
                                        ; (ob-ditaa :location built-in)
                                        ; (ob-plantuml :location built-in)
                                        ; (ob-ledger :location built-in) ; 必须 init，才能使用
    ;; secretaria
    )
  )
;; List of packages to exclude.
(setq rinetd-org-excluded-packages '())


;; 借助 EasyPG package 原生支持 GnuPG 加密. 提供基本的 GnuPG 功能.
;;# -*- mode:org; epa-file-encrypt-to: ("me@mydomain.com") -*-
(defun rinetd-org/init-epa-file ()
  (use-package epa-file
    :defer t
    :config
    (progn
      (setq epg-gpg-home-directory "~/.gnupg/") ;slove untrusted key anyway
      (setq epg-gpg-program "gpg2")
      ;; 使用对称加密[输密码]（注：对称加密需要密码,非对称加密公钥+私钥）
      (setq epa-file-encrypt-to nil)
      (setq epa-file-encrypt-to 'rinetd)
      ;;是否彈出选择密码类型窗口, 如果我們想要使用自己設置的密碼加密文件时，就将其设置为nil; 当为nil时与epa-file-encrypt-to配合使用
      (setq epa-file-select-keys nil)
      ;; 只會在目前的 session 記住這個密碼 不要每次保存加密文件的时候，都让我输一遍密码！只在当前session有效。
      (setq epa-file-cache-passphrase-for-symmetric-encryption t)
      ;; 不允许自动保存
      (setq epa-file-inhibit-auto-save t)
      ;; non-GUI password dialog. Test: (getenv "GPG_AGENT_INFO")
      (setenv "GPG_AGENT_INFO" nil) ;; 使用Emacs自己的内部密码提示功能

      ;; Use org-mode for encrypted org file
      (add-to-list 'auto-mode-alist '("\\.org.gpg\\'" . org-mode)))))



;;;;;;;;;;;;;;;;;;;;
;; org-crypt 加密
;;;;;;;;;;;;;;;;;;;;

;; TODO improve approach to toggle between encryp/decrypt
(defun rinetd-org/init-org-crypt ()
  (use-package org-crypt
    :after org
    :init
    (spacemacs/set-leader-keys-for-major-mode 'org-mode
      "U" 'org-decrypt-entry  ; Mnemonic of Unravel
      "E" 'org-encrypt-entries)
    :config
    ;; Encrypt all entries before saving
    (org-crypt-use-before-save-magic)
    (setq org-crypt-tag-matcher "crypt"
          org-tags-exclude-from-inheritance '("crypt")
          org-crypt-key crypt-gpg-key
          org-crypt-disable-auto-save nil
          epa-file-encrypt-to org-crypt-key
          ;; use gpg2 to cache the passphrase with gpg-agent, otherwise it won't work
          epg-gpg-program "gpg2"
)))


(defun rinetd-org/post-init-org ()
  (add-hook 'org-mode-hook (lambda () (spacemacs/toggle-line-numbers-off)) 'append) ;关闭行号显示

  (with-eval-after-load 'org
    (add-to-list 'auto-mode-alist '("\\.\\(org\\|org_archive\\|txt\\)$" . org-mode))

    (spacemacs|disable-company org-mode)

    ;; 防止不小心编辑了省略部分的内容
    (setq org-catch-invisible-edits 'smart)
    ;; 启用 org-indent-mode
    (setq org-startup-indented t)
    ;; 不显示 headline 之间的空白行
    ;; (setq org-cycle-separator-lines 0)

         ;;{{ 仅仅显示斜体字就好
    ;; @see https://github.com/lujun9972/emacs-document/blob/master/org-mode/%E5%B0%86org%E7%9C%8B%E6%88%90%E6%96%87%E5%AD%97%E5%A4%84%E7%90%86%E5%99%A8.org
    ;; /org italic/ 看起来就好像是斜体字被正则表达式的分隔符所包围了一样. 隐藏这些标记很简单
    (setq org-hide-emphasis-markers t)
    ;; 记住,这些斜杠字符(用于标示粗体的星号等其他字符也是一样)依然存在的,只是没有显示出来而已.
    ;; 想要修改这些标记也很简单,只要在之上按退格键就行.
    ;;}}
    (setq org-hide-emphasis-markers t)  ;;隐藏字体样式标记
    (setq org-use-sub-superscripts nil)  ;;上下标默认不作用，需要时加{}
    ;; (setq org-confirm-babel-evaluate nil) ;;在用C-c C-c执行代码块时,不再提示“Do you want to execute”


    ;;=> if there is a #+ATTR.*: width="200", resize to 200, otherwise resize to 400
    (setq org-image-actual-width '(400))



    ;; Add new easy templates <sb TAB
    ; s	#+BEGIN_SRC ... #+END_SRC
    ; e	#+BEGIN_EXAMPLE ... #+END_EXAMPLE
    ;;文件内部排版
    ; q	#+BEGIN_QUOTE ... #+END_QUOTE 设置缩进
    ; v	#+BEGIN_VERSE ... #+END_VERSE
    ; c	#+BEGIN_CENTER ... #+END_CENTER
    ; l	#+BEGIN_LaTeX ... #+END_LaTeX
    ; L	#+LaTeX:
    ; h	#+BEGIN_HTML ... #+END_HTML
    ; H	#+HTML:
    ; a	#+BEGIN_ASCII ... #+END_ASCII
    ; A	#+ASCII:
    ; i	#+INDEX: line
    ; I	#+INCLUDE: line
    (setq org-structure-template-alist
          (append '(("ex" "#+BEGIN_EXAMPLE\n?\n#+END_EXAMPLE")
                    ("sb" "#+BEGIN_SRC bash\n?\n#+END_SRC")
                    ("se" "#+BEGIN_SRC emacs-lisp\n?\n#+END_SRC")
                    ("sp" "#+BEGIN_SRC python\n?\n#+END_SRC")
                    ("su" "#+BEGIN_SRC plantuml\n?\n#+END_SRC")
                    )
                  org-structure-template-alist))

          ;;;;;;;;;;;;;;;;;;;;
    ;; Logging
          ;;;;;;;;;;;;;;;;;;;;

    ;; task 完成后,自动记录完成时间
    ;; (setq org-log-done t)
    ;; @see ~/.emacs.d/layers/org/packages.el:99

    ;; 将 log 存入 drawer 中
    (setq org-log-into-drawer t)

    ;; 设置 log 存放在 task 的哪个位置
    (setq org-log-state-notes-insert-after-drawers nil)

    ;; todo keywords 的定义也与 log 息息相关

    ;; other

    ;; 完成重复任务时重设所有子任务
    (setq org-default-properties (cons "RESET_SUBTASKS" org-default-properties))

    ;; 在导出时,不导出时间戳
    (setq org-export-with-timestamps nil)

    ;; 让正文中的 plain list 也具有折叠的能力
    (setq org-cycle-include-plain-lists t)

    ;; Create unique IDs for tasks when linking
    (setq org-id-link-to-org-use-id 'create-if-interactive-and-no-custom-id)

    (setq org-tags-match-list-sublevels nil)



    ;;;;;;;;;;;;;;;;;;;;
    ;; TODO 状态触发器 TODO 状态改变触发一个 tag 改变
    ;;;;;;;;;;;;;;;;;;;;

    ;; 当 TODO 状态发生更改时,自动添加/删除特定的 TAG ,这样方便 agenda view 中过滤任务:
    ;; org-todo-state-tags-triggers 的格式为:
    ;; `(state-change (tag . flag) …….)’ falg 为 t 表示添加 tag
    ;; 这里 state-change 可以是一个表示 todo 状态的字符串,或者是符号 ’todo 或 ’done ,
    ;; 分别表示所有表示未完成任务的和以完成任务的 todo state
    (setq org-todo-state-tags-triggers
          (quote (("CANCELLED" ("CANCELLED" . t))
                  ("WAITING" ("WAITING" . t))
                  ("HOLD" ("WAITING") ("HOLD" . t))
                  (done ("WAITING") ("HOLD"))
                  ("TODO" ("WAITING") ("CANCELLED") ("HOLD"))
                  ("NEXT" ("WAITING") ("CANCELLED") ("HOLD"))
                  ("DONE" ("WAITING") ("CANCELLED") ("HOLD")))))

    ;; note:
    ;; * Moving a task to CANCELLED adds a CANCELLED tag
    ;; * Moving a task to WAITING adds a WAITING tag
    ;; * Moving a task to HOLD adds WAITING and HOLD tags
    ;; * Moving a task to a done state removes WAITING and HOLD tags
    ;; * Moving a task to TODO removes WAITING, CANCELLED, and HOLD tags
    ;; * Moving a task to DONE removes WAITING, CANCELLED, and HOLD tags
    ;;
    ;; =TODO= state keywords and colour settings:
    ;;
    ;; The tags are used as follows:
    ;;
    ;; TODO
    ;; The item is ready to be done at the earliest opportunity or at the date (and maybe time) indicated in the SCHEDULED tag. Some tasks are given a DEADLINE date which is useful for scheduling the tasks during my daily planning.
    ;; STARTED
    ;; I should use this tag when I start on a task, but if I clock in to a TODO item, I don't really need this task.
    ;; WAITING
    ;; I did some work on this task but I am waiting for a response. If I use this task I schedule the task into the future as a reminder to follow up with some notes in the body of the task.
    ;; APPT
    ;; Used to tag an activity that can only be done at the specified time and date, instead of tasks that can be completed at any time.
    ;; DONE
    ;; The task is completed.
    ;; CANCELLED
    ;; I decided not to do this task but have left the task on file with this status.
    ;; DEFERRED
    ;; Used to identify a task that will not be activated just yet. The reason will be included in the task notes.
    ;;
    ;; 括号中指定“！”（记录时间戳）或“@”（作一个记录），用于跟踪 TODO 状态变化
    (setq org-todo-keywords
          (quote ((sequence "TODO(t)" "STARTED(s)" "WAITING(w@/!)" "APPT(a)"
                            "|" "DONE(d)" "CANCELLED(c@/!)" "DEFERRED(f@/!)")
                  (sequence "MEETING" "PHONE"))))

    ;; (setq org-todo-keywords
    ;;       (quote ((sequence "TODO(t)" "STARTED(s)" "|" "DONE(d!/!)")
    ;;               (sequence "WAITING(w@/!)" "SOMEDAY(S)" "|" "CANCELLED(c@/!)" "MEETING(m)" "PHONE(p)"))))

    (setq org-todo-keywords
          '((sequence
             "INBOX(i)"                   ;; ideas, undecided
             "TODAY(T)"                       ;; needs to be done today
             "TODO(t)"                        ;; needs to be done
             "NEXT(n)"                        ;; next in line
             "HOLD(H)"                        ;; put on hold for various reasons
             "WIP(I)"
             "PROJ(p)"
             "PLAN(P)"                        ;; still under planning
             "FOLLOW(f)"                   ;; follow-up results
             "SOMEDAY(s)"                     ;; not now
             "|" "DONE(d)" "CANCELED(C)" "ABORT(A)" "FAILED(F)")))
    (setq org-todo-keyword-faces
          `(("INBOX" . (:weight bold :foreground ,(face-attribute 'font-lock-function-name-face :foreground)))
            ("TODAY" . (:weight bold :foreground ,(face-attribute 'font-lock-warning-face :foreground)))
            ("TODO" . (:weight bold :foreground ,(face-attribute 'font-lock-variable-name-face :foreground)))
            ("HOLD" . (:weight bold :foreground ,(face-attribute 'font-lock-function-name-face :foreground)))
            ("NEXT" . (:weight bold :foreground ,(face-attribute 'font-lock-constant-face :foreground)))
            ("FOLLOW" . (:weight bold :foreground ,(face-attribute 'font-lock-function-name-face :foreground)))
            ("PROJ" . (:weight bold :foreground ,(face-attribute 'font-lock-type-face :foreground)))
            ("WIP" . (:weight bold :foreground ,(face-attribute 'font-lock-type-face :foreground)))
            ("DONE" . (:weight bold :foreground ,(face-attribute 'font-lock-comment-face :foreground)))))

    ; Org 支持根据标签列表来插入标签。默认情况下，这个列表动态创建，包含当前缓冲中的所有标签。你也可以通过变量 org-tag-alist 来创建一个全局的标签列表。最后你可以通过下面的形式为一个文件设置默认的标签：
    ; #+TAGS: @work @home @tennisclus
    ; #+TAGS: laptop car pc sailboat
    ; 如果你已经用 org-tag-alist 定义了喜欢的全局标签， 但又想在一个特定的文件中使用动态标签，那么可以通过添加一个空的 TAGS 选项到这个文件来达到这个目的：
    ; #+TAGS
    ; 如果你想将预定义的全局标签添加到每一个文件的 TAGS 选项中，那么可以用 org-tag-persistent-alist 定义一个标签列表。这个选项可以在文件中通过 STARTUP 选项来关闭:
    ; #+STARTUP: noptag
    ;; #+TAGS: { @work(w)  @home(h)  @way(w) }
    (setq-default org-tag-alist '(
                                (:startgroup . nil)
                                ("" . nil)
                                ("@android" . ?a)
                                ("@php" . ?p)
                                ("@ios" . ?i)
                                (:endgroup . nil)
                                ))
    ;; Tags with fast selection keys
    (setq org-tag-alist (quote ((:startgroup)
                                ("Office" . ?O)
                                ("Home" . ?H)
                                (:endgroup)
                                ("Computer" . ?c)
                                ("Reading" . ?r)
                                ("Project" . ?p))))

    (setq org-tag-persistent-alist
          '((:startgroup . "group")
            ("CTW") ("WORK") ("HOME")
            (:endgroup . nil)
            ("WIN") ("MAC") ("LINUX")
            (:startgroup . "effort")
            ("MAJOR") ("MID") ("MINOR")
            (:endgroup . nil)
            (:startgroup . "progress")
            ("00" . ?0) ("25" . ?2) ("50" . ?5) ("75" . ?7) ("95" . ?9)
            (:endgroup . nil)
            (:startgroup . "actions")
            ("ISSUES") ("HAVE_A_LOOK") ("THINK") ("REFACTOR")
            (:endgroup . nil)))

    (setq org-tag-faces
          `(("CTW" . (:weight bold :foreground ,(face-attribute 'font-lock-function-name-face :foreground)))
            ("WORK" . (:weight bold :foreground ,(face-attribute 'font-lock-function-name-face :foreground)))
            ("HOME" . (:weight bold :foreground ,(face-attribute 'font-lock-constant-face :foreground)))
            ("HAVE_A_LOOK" . (:weight bold :foreground ,(face-attribute 'font-lock-variable-name-face :foreground)))
            ("MAJOR" . (:weight bold :foreground ,(face-attribute 'font-lock-warning-face :foreground)))
            ("MID" . (:weight bold :foreground ,(face-attribute 'font-lock-variable-name-face :foreground)))
            ("MINOR" . (:weight bold :foreground ,(face-attribute 'font-lock-string-face :foreground)))
            ("00" . (:weight bold :foreground "#deab0e"))
            ("25" . (:weight bold :foreground "#b58900"))
            ("50" . (:weight bold :foreground "#b58900"))
            ("75" . (:weight bold :foreground "#926e00"))
            ("95" . (:weight bold :foreground "#926e00"))))

    ))


;;;;;;;;;;;;;;;;;;;;
;; 配置 org-capture
;;;;;;;;;;;;;;;;;;;;

(defun rinetd-org/init-org-capture ()
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
    ;; %[file]     插入文件
         ;; %(sexp)     插入 sexp 的返回值，sexp 必须返回字符串
         ;; %<...>      插入时间戳信息
         ;; %t          插入日期
         ;; %T          插入日期与时间
         ;; %u, %U      同上，但时间戳用 [] 括起来
         ;; %i          调用 capture 命令时有选中的内容则插入选中的内容
         ;; %a          注记，通常是 org-store-link 创建的链接
         ;; %A          类似 %a，但提示输入链接的描述
         ;; %l          类似 %a，但仅插入文本链接
         ;; %c          当前 kill-ring 中的内容
         ;; %x          粘贴板的内容
         ;; %k          当前计时任务标题
         ;; %K          当前计时任务链接
         ;; %n          用户名，变量 user-full-name
         ;; %f          capture 命令调用时当前 buffer 对应文件名
         ;; %F          类似 %f，但显示全路径
         ;; %:keyword   Specific information for certain link types, see below.
         ;; %^g         提示输入 tag，target file 中的列表作为可选项
         ;; %^G         类似 %^g，但是有 agenda 中所有注册的文件中的 tag 作为可选项
         ;; %^t         类似 %t,但提示手动输入日期，类似还有 %^T， %^u， %^U                 You may define a prompt like %^{Birthday}t.
         ;; %^C         提示插入哪个 kill-ring 的内容
         ;; %^L         类似 %^C，但插入为链接
         ;; %^{prop}p   Prompt the user for a value for property prop.
         ;; %^{prompt}  prompt the user for a string and replace this sequence with it.
         ;;             You may specify a default value and a completion table with
         ;;             %^{prompt|default|completion2|completion3...}.
         ;;             The arrow keys access a prompt-specific history.
         ;; %\n         Insert the text entered at the nth %^{prompt}, where n is
         ;;             a number, starting from 1.
         ;; %?          After completing the template, position cursor here.

         (setq org-capture-templates
               '(

                 ("m" "MEMO"   entry      (file+datetree "journal.org") "*  %?")
                 ("d" "DISCUSS" checkitem  (file+headline "gtd.org" "讨论") " [ ] %?\n\n" :empty-lines 1 :prepend t :kill-buffer t)
                 ("t" "TODO SCHEDULED" entry (file+headline "gtd.org" "待办事项") "* TODO %?\n DEADLINE: %^T SCHEDULED: %t \n  %i\n")
                 ("s" "SOMEDAY" entry (file+headline "gtd.org" "将来/也许") "* MAYBE [#C]  %?\n  %i\n" )
                 ("n" "NEXT" entry (file+headline "gtd.org" "下一步行动") "* NEXT [#B] %?\n  %i\n")
                 ("w" "WAITING" entry (file+headline "gtd.org" "等待") "* WAITING [#A] %? %^G%^G%^G \n DEADLINE: %^T SCHEDULED: %t \n %i\n " :empty-lines 1 :prepend t)
                 ("g" "项目规范" checkitem  (file+headline "gtd.org" "规范") " * %?\n" :empty-lines 1 :prepend t :kill-buffer t)


                 ;; For capturing details of bills
                 ("b" "账单表格 Bill"      table-line (file+headline "gtd.org" "账单统计" ) "| %U | %^{people|耿晓斌|李振|马超|刘顺}| %^{物品} X%^{数量} | %^{价格}| " :prepend t :kill-buffer t)
                 ("k" "考勤清单 List"      item       (file+headline "gtd.org" "考勤") " %^{state|请假|加班} %^{people|密启超|葛绪强|李振|马超|刘顺} %^T -- %^T ")
                 ("c" "Code Snippet"      entry      (file "snippets.org") "* %?\t%^g\n#+BEGIN_SRC %^{language}\n\n#+END_SRC")
                 ("e" "English Snippet"      entry      (file "english.org") "* %?\t:eng: ")

                 ;; To capture ideas for my blog
                 ("b"                              ; key
                  "Blog"                           ; name
                  entry                            ; type
                  (file+headline "notes.org" "Blog") ; target
                  "* %^{Title} :blog:\n:PROPERTIES:\n:Created: %U\n:END:\n%i\n%?" ; template
                  :prepend t               ; properties
                  :empty-lines 1           ; properties
                  :created t               ; properties
                  :kill-buffer t)          ; properties

                 ("l" "links" entry (file+headline "~/org-notes/notes.org" "Quick notes")
                  "* TODO [#C] %?\n  %i\n %a \n %U"
                  :empty-lines 1)))
         ;; properties
         ;; :prepend 通常情况下,新捕获的内容会附加在 target location 的后面,而该属性会添加在 target location 的前面
         ;; :immediate-finish 该属性表示不需要显示 capture buffer 给用户输入更多的信息.直接返回就好. 若所有的信息都能够通过模板变量自动获得的情况下可以使用
         ;; :empty-lines 插入新捕获的内容时,前后空出多少个空行.
         ;; :clock-in 为新捕获的 item 开始计时
         ;; :clock-keep 若设置了 clock-in,则在 capture 动作完成后,依然保持计时器的继续运行
         ;; :clock-resume
         ;; 若 capture 操作中断了对之前任务的计时,则在完成 capture 操作之后继续对之前任务进行计时.
         ;; 需要注意的是,:clock-keep 的优先级高于:clock-resume,若两者都设置为 t,则当前计时器会启动,而前一个计时器不会继续运行.
         ;; :unnarrowed 不要 narrow target buffer,显示 target buffer 的所有内容. 默认情况下会 narrow target buffer,让它只显示捕获新事物的那节点内容
         ;; :table-line-pos 设置 capture 的内容插入到 table 的位置. 它的格式类似于”II-3”,表示它是表格中第二部分(以——-分隔)的第三行
         ;; :kill-buffer 若 target file 是未打开的状态,则在 capture 完成之后,自动 kill 掉新打开的 buffer

    ;; (setq org-capture-templates
    ;;       '(("t" "Todo" entry (file+headline (concat org-directory "inbox.org") "Tasks")
    ;;          "** TODO %? %^G\n  Created: %U \n  %i")
    ;;         ("T" "Scheduled Todo" entry (file+headline (concat org-directory "inbox.org") "Tasks")
    ;;          "** TODO %? %^G\n SCHEDULED: %^{ Sheduled: }T Created: %U \n  %i")
    ;;         ("m" "Meeting" entry (file+headline (concat org-directory "inbox.org") "Tasks")
    ;;          "* MEETING with %? :Meeting:\n%U" :clock-in t :clock-resume t)
    ;;         ("p" "Phone call" entry (file+headline (concat org-directory "inbox.org") "Tasks")
    ;;          "* PHONE %? :Phone:\n%U" :clock-in t :clock-resume t)
    ;;         ))

    ;; Set default column view headings: Task Effort Clock_Summary
    (setq org-columns-default-format
                                        ; "%50ITEM(Task) %10TODO %3PRIORITY %TAGS %10Effort(Effort){:} %10CLOCKSUM"
          "%38ITEM(Details) %TAGS(Context) %7TODO(To Do) %5Effort(Time){:} %6CLOCKSUM{Total}")
    ;; global Effort estimate values
    ;; global STYLE property values for completion
    (setq org-global-properties (quote (("Effort_ALL" . "0:15 0:30 0:45 1:00 2:00 3:00 4:00 5:00 6:00 8:00")
                                        ("STYLE_ALL" . "habit"))))



    ))

;;;;;;;;;;;;;;;;;;;;
;; org-agenda
;;;;;;;;;;;;;;;;;;;;

(defun rinetd-org/post-init-org-agenda()
  (with-eval-after-load 'org-agenda
    ;; Custom Key Bindings
    (spacemacs/set-leader-keys
      ;; refile task
      "or"  'org-agenda-refile)

    (setq org-agenda-inhibit-startup t)   ;; ~50x speedup
    (setq org-agenda-use-tag-inheritance nil) ;; 3-4x speedup
    (setq org-agenda-window-setup 'current-window)

    (add-to-list 'org-modules 'org-habit)
    (require 'org-habit)

    ;; 设置 agenda 的数据来源
    ;; org-agenda-files 中的元素还可以是目录，这时目录下的所有匹配
    ;; `org-agenda-file-regexp’的文件都自动加入 agenda .
    ;; (setq org-agenda-files
    ;;       (list
    ;;        (concat org-directory "inbox.org")
    ;;        (concat org-directory "project.org")
    ;;        (concat org-directory "task.org")
    ;;        (concat org-directory "finished.org")
    ;;        (concat org-directory "trash.org")
    ;;        (concat org-directory "memorial-day.org")
    ;;        ))

    (setq org-agenda-files (list org-directory))

    ;;;;;;;;;;;;;;;;;;;;
    ;; Refile Task
    ;;;;;;;;;;;;;;;;;;;;

    ;; 可以 refile 到`org-agenda-files'中的文件和当前文件中. 最多 9 层深度
    (setq org-refile-targets
          '((nil :maxlevel . 9)
            (org-agenda-files :maxlevel . 9)))

    ;; Use full outline paths for refile targets - we file directly with IDO
    ;; 这时,可以使用/level1/level2/level3 来表示一个三层的 headline
                                        ; (setq org-refile-use-outline-path t)
    (setq org-refile-use-outline-path 'file)

    ;; Targets complete directly with IDO
    (setq org-outline-path-complete-in-steps nil)

    ;; Allow refile to create parent tasks with confirmation
    (setq org-refile-allow-creating-parent-nodes 'confirm)

    ;; Use IDO for both buffer and file completion and ido-everywhere to t
    (setq org-completion-use-ido t)
    ;; Use the current window for indirect buffer display
    (setq org-indirect-buffer-display 'current-window)

    ;;;; Refile settings
    ;; Exclude DONE state tasks from refile targets
    (defun bh/verify-refile-target ()
      "Exclude todo keywords with a done state from refile targets"
      (not (member (nth 2 (org-heading-components)) org-done-keywords)))

    (setq org-refile-target-verify-function 'bh/verify-refile-target)

    ;;;;;;;;;;;;;;;;;;;;
    ;; 配置 agenda view
    ;;;;;;;;;;;;;;;;;;;;

    ;; Can be day, week, fortnight, month, year, or any number of days.
    (setq org-agenda-span 'month)

    ;; Do not dim blocked tasks
    (setq org-agenda-dim-blocked-tasks nil)

    ;; Compact the block agenda view
    (setq org-agenda-compact-blocks t)

    ;; Custom agenda command definitions
    ;; An entry without a cookie is treated just like priority ' B '.
    ;; So when create new task, they are default 重要且紧急
    (setq org-agenda-custom-commands
          '(
            ("T" . "任务安排")
            ("TA" "重要且紧急的任务" tags-todo "+PRIORITY=\"A\"")
            ("TB" "重要且不紧急的任务" tags-todo "-Weekly-Monthly-Daily+PRIORITY=\"B\"")
            ("TC" "不重要且紧急的任务" tags-todo "+PRIORITY=\"C\"")

            ("W" "Office & Work Lists"
             ((agenda "" ((org-agenda-ndays 1)))
              (tags-todo "Office" ((org-agenda-skip-function (quote (org-agenda-skip-entry-if 'scheduled 'deadline)))))
              (tags-todo "Phone" ((org-agenda-skip-function (quote (org-agenda-skip-entry-if 'scheduled 'deadline)))))
              (tags-todo "Email" ((org-agenda-skip-function (quote (org-agenda-skip-entry-if 'scheduled 'deadline)))))
              (tags-todo "Computer" ((org-agenda-skip-function (quote (org-agenda-skip-entry-if 'scheduled 'deadline)))))
              ))

            ("H" "Home List"
             ((agenda "" ((org-agenda-ndays 1)))
              (tags-todo "Home" ((org-agenda-skip-function (quote (org-agenda-skip-entry-if 'scheduled 'deadline)))))
              (tags-todo "Computer" ((org-agenda-skip-function (quote (org-agenda-skip-entry-if 'scheduled 'deadline)))))
              (tags-todo "Online" ((org-agenda-skip-function (quote (org-agenda-skip-entry-if 'scheduled 'deadline)))))
              (tags-todo "Reading" ((org-agenda-skip-function (quote (org-agenda-skip-entry-if 'scheduled 'deadline)))))
              ))

            ("D" "Daily Action List"
             ((agenda "" ((org-agenda-ndays 1)
                          (org-agenda-sorting-strategy
                           (quote ((agenda time-up priority-down tag-up) )))
                          (org-deadline-warning-days 0)
                          ))))

            ("A" "Tasks to be Archived" tags "LEVEL=2/DONE|CANCELLED" nil)
            ))

    ;; Include agenda archive files when searching for things
    (setq org-agenda-text-search-extra-files (quote (agenda-archives)))

    ;; Show all future entries for repeating tasks
    (setq org-agenda-repeating-timestamp-show-all t)

    ;; Show all agenda dates - even if they are empty
    (setq org-agenda-show-all-dates t)

    ;; Start the weekly agenda on Monday
    (setq org-agenda-start-on-weekday 1)

    ;; Display tags farther right
    (setq org-agenda-tags-column -102)

    (setq org-agenda-sorting-strategy
          (quote ((agenda time-up priority-down tag-up) (todo tag-up))))

    ;; 设置还有多少天到 deadline 的 task，显示到 agenda view 中
    ;; 要特殊设置某个 task 在 deadline 的前 N 天就显示在 agenda view 中，
    ;; 可以在该 task 的 deadline 上加上 `-Nd’. 例如:
    ;;
    ;; * TODO Pay Wages
    ;;   DEADLINE: <2009-07-01 Wed +1m -2d>
    ;;
    (setq org-deadline-warning-days 14)

    ;;{{ Keep tasks with timestamps visible on the global todo lists

    ;; Keep tasks with dates on the global todo lists
    (setq org-agenda-todo-ignore-with-date nil)

    ;; Keep tasks with deadlines on the global todo lists
    (setq org-agenda-todo-ignore-deadlines nil)

    ;; Keep tasks with scheduled dates on the global todo lists
    (setq org-agenda-todo-ignore-scheduled nil)

    ;; Keep tasks with timestamps on the global todo lists
    (setq org-agenda-todo-ignore-timestamp nil)

    ;; Remove completed deadline tasks from the agenda view
    (setq org-agenda-skip-deadline-if-done t)

    ;; Remove completed scheduled tasks from the agenda view
    (setq org-agenda-skip-scheduled-if-done t)

    ;; Remove completed items from search results
    (setq org-agenda-skip-timestamp-if-done t)

    ;; Skip scheduled items if they are repeated beyond the current deadline.
    (setq org-agenda-skip-scheduled-if-deadline-is-shown
          (quote repeated-after-deadline))
    ;;}}

    ;;{{ TODO 状态切换
    ;; 开启 fast todo selection，使得可以使用 `C-c C-t’ 直接选择 TODO 状态
    (setq org-use-fast-todo-selection t)

    ;; 当时用 S-left 和 S-rigth 更改 TODO 状态时，仅仅只是更改状态，
    ;; 而不要像正常的更改状态流程那样登记状态更改的时间戳,抓获切换状态时的上下文日志
    (setq org-treat-S-cursor-todo-selection-as-state-change nil)

    ;; 在子 task 都变为完成状态的前,不能切换父级 task 变为完成状态
    ;; 任何未完成的子任务会阻止父任务变为完成状态,若像临时屏蔽该功能,可以为该任务添加`:NOBLOCKING: t'属性
    ;; 若父任务中设置了属性`:ORDERED: t',则表示其子任务必须依照顺序从上到下完成
    (setq org-enforce-todo-dependencies t)
    ;;}}

    ;;;;;;;;;;;;;;;;;;;;
    ;; Time Reporting and Tracking
    ;;;;;;;;;;;;;;;;;;;;
    ;; Agenda clock report parameters
    (setq org-agenda-clockreport-parameter-plist
          '(:link t :maxlevel 5 :fileskip0 t :compact t :narrow 80))

    ;; Providing progress reports to others
    (setq org-agenda-log-mode-items '(closed state clock))

    ;;;;;;;;;;;;;;;;;;;;
    ;; Project definition and finding stuck projects
    ;;;;;;;;;;;;;;;;;;;;

    ;; 通过设置`org-stuck-projects’可以设定规则来表示哪些 task 是属于 project 的,
    ;; 哪些是 project 又是 stucked 的.

    ;; 所有有子任务的 task 都被认为是 project
    ;; 若 project 的子树中有"NEXT"状态 task 的,不认为是 stucked
    (setq org-stuck-projects '("+LEVEL=2/-DONE" ("NEXT") nil ""))
                                        ; (setq org-stuck-projects '("TODO={.+}/-DONE" nil nil "SCHEDULED:\\|DEADLINE:"))

    ;; `org-stuck-projects’是一个由 4 个元素组成的 list:
                                        ;
                                        ; 元素一为一个字符串,用来根据 tags/todo/projecty 来标示哪些 task 是 project
                                        ; 元素二为一个 TODO 关键字组成的 list, 若 project 的子树中有处于该状态的 sub-task,则不认为是 stuck project
                                        ; 元素三为一个由 TAG 组成的 list, 若 project 的子树中有标注该 tag 的 sub-task,则不认为是 stuck project
                                        ; 元素四为一个表示正则表达式的字符串,任何匹配该正则的 project,都不被认为是 stuck project
                                        ;
    ;;

    ;; Always hilight the current agenda line
    (add-hook 'org-agenda-mode-hook
              '(lambda () (hl-line-mode 1))
              'append)

    ;; org-agenda 在 calfw 中展示
    (use-package calfw-org :defer t)
    ))

;;;;;;;;;;;;;;;;;;;;
;; 归档
;;;;;;;;;;;;;;;;;;;;

(defun rinetd-org/init-org-archive ()
  (with-eval-after-load 'org-archive
                                        ; (require 'org-archive)

    ;; 归档时保持 TODO state 不变
    (setq org-archive-mark-done nil)

    ;; 通过设置`org-archive-mark-done’可以指定归档的位置
    (setq org-archive-location "%s_archive::* Archived Tasks")
    ;; 带有`Archive’ tag 的 entry,默认情况下不会被展开,但可以使用`C-TAB’强制展开
    ))



;;;;;;;;;;;;;;;;;;;;
;; Time Clocking
;;;;;;;;;;;;;;;;;;;;

(defun rinetd-org/init-org-clock ()
  (;progn
   with-eval-after-load 'org-clock
    ;; Clock setup
                                        ; (require 'org-clock)
    ;; Resume clocking task when emacs is restarted
    (org-clock-persistence-insinuate)

    ;; Show lot of clocking history so it's easy to pick items off the C-F11 list
    (setq org-clock-history-length 23)
    ;; Resume clocking task on clock-in if the clock is open
    (setq org-clock-in-resume t)
    ;; Change task state to STARTED when clocking in
    (setq org-clock-in-switch-to-state "STARTED")
    ;; Separate drawers for clocking and logs
    (setq org-drawers (quote ("PROPERTIES" "LOGBOOK")))
    ;; Save clock data and state changes and notes in the LOGBOOK drawer
    (setq org-clock-into-drawer t)
    ;; Sometimes I change tasks I'm clocking quickly - this removes clocked tasks with 0:00 duration
    (setq org-clock-out-remove-zero-time-clocks t)
    ;; Clock out when moving task to a done state
    (setq org-clock-out-when-done t)
    ;; Save the running clock and all clock history when exiting Emacs, load it on startup
    (setq org-clock-persist t)
    ;; Do not prompt to resume an active clock
    (setq org-clock-persist-query-resume nil)
    ;; Enable auto clock resolution for finding open clocks
    (setq org-clock-auto-clock-resolution (quote when-no-clock-is-running))
    ;; Include current clocking task in clock reports
    (setq org-clock-report-include-clocking-task t)
    ;; Resolve open clocks if the user is idle for more than 10 minutes.
    (setq org-clock-idle-time 10)

    ;; 设置 default clock in task

    ;; 使用 clock history 来 clock in 先前的 tasks

    ;; 修改 clock 记录的时间戳
    ;; 在时间戳上用 S-<up>可以增加时间戳的值, S-<down>可以减少时间戳的值.
    ;; 下面的配置说明当使用 S-<up>/S-<down>修改时间戳时，以１分钟为单位来修改
    (setq org-time-stamp-rounding-minutes '(1 1))

    ;; 设置 mode-line

    ;; 当总计的时间超过了预估的时间时,替换 mode-line 背景色为红色,以示提醒
    (custom-set-faces
     ;; custom-set-faces was added by Custom.
     ;; If you edit it by hand, you could mess it up, so be careful.
     ;; Your init file should contain only one such instance.
     ;; If there is more than one, they won't work right.
     '(org-mode-line-clock ((t (:foreground "red" :box (:line-width -1 :style released-button)))) t))

    ;; 通过设置`:clock-in t’使得在 captre task 时自动开始 clock in.
    ;; 设置`:clock-resume t’则使得 capture task 完成后,自动恢复原 task 的 clock in.
    ;; 但这就会产生一个问题,若 capture task 的时间小于 1 分钟,则可能有大量的计时为 0:00 的记录存在,
    ;; 这些记录需要清理

    ;; Remove empty LOGBOOK drawers on clock out
    (defun bh/remove-empty-drawer-on-clock-out ()
      (interactive)
      (save-excursion
        (beginning-of-line 0)
        ;; Following line from original document by Bernt Hansen
        ;; will lead to an error, next to it is the corrected form.
        ;; (org-remove-empty-drawer-at "LOGBOOK" (point))
        (org-remove-empty-drawer-at (point))))

    (add-hook 'org-clock-out-hook 'bh/remove-empty-drawer-on-clock-out 'append)

    ;;;;;;;;;;;;;;;;;;;;
    ;; 评估任务的工作量
    ;;;;;;;;;;;;;;;;;;;;

    ;; 通过为 task 增加 `Effort’ 属性，可以为任务设置一个评估的工作量，若 clock tracking
    ;; 的时间超过了这个评估的工作量,则会提出警告:
    ;;
                                        ; * NEXT Document my use of org-mode
                                        ;   :PROPERTIES:
                                        ;   :CLOCK_MODELINE_TOTAL: today
                                        ;   :Effort:   1:00
                                        ;   :END:
    ;;
    ;; 可以设置 clock tracking 的时间到达预估工作量时的提醒声音
    (setq org-clock-sound t)
    ))

;;;;;;;;;;;;;;;;;;;;
;; org-faces
;;;;;;;;;;;;;;;;;;;;

(defun rinetd-org/init-org-faces ()
  (with-eval-after-load 'org-faces
                                        ; (require 'org-faces)
    (setq org-todo-keyword-faces
          (quote (("TODO" :foreground "red" :weight bold)
                  ("STARTED" :foreground "blue" :weight bold)
                  ("WAITING" :foreground "orange" :weight bold)
                  ("APPT" :foreground "magenta" :weight bold)
                  ("DONE" :foreground "forest green" :weight bold)
                  ("CANCELLED" :foreground "forest green" :weight bold)
                  ("DEFERRED" :foreground "forest green" :weight bold)
                  ("MEETING" :foreground "forest green" :weight bold)
                  ("PHONE" :foreground "forest green" :weight bold))))

    ;; Priority
    (setq org-priority-faces
          '((?A . (:foreground "white" :background "dark red"
                               :box '(:color "red" :line-width 3 :style released-button)))
            (?B . (:foreground "white" :background "dark slate blue"
                               :box '(:color "white" :line-width 3 :style released-button)))
            (?C . (:foreground "white" :background "dim gray"
                               :box '(:color "dim gray" :line-width 3 :style released-button)))
            ))
    ;; (set-face-attribute 'org-priority nil
    ;;                     :box '(:color "red" :line-width 3 :style released-button)
    ;;                     :bold nil)

    ;; inline code face => src_ruby{require 'something'}
    ;;
    ;; (REGEXP . FACE)
    ;;     Highlight REGEXP with FACE
    ;; (REGEXP N FACE)
    ;;     Highlight group N in REGEXP with FACE
    ;; (REGEXP (N1 FACE1) (N2 FACE2) (N3 FACE3) …)
    ;;     Highlight group Ni in REGEXP with FACEi
    ;;
    ;; src_lang{code...}[:header arguments] / NOTE: override by `org-verbatim'.
    ;; result in following =[result]=
    (setq org-babel-exp-inline-code-template "src_%lang[%switches%flags]{%body}"
          org-babel-inline-result-wrap "=> (~%s~)" ; or "=%s=", "~%s~"
          )

    ))

(defun rinetd-org/init-org-list ()
  (progn
    (require 'org-list)

    ;; 允许使用字母作为 list bullet
    (setq org-list-allow-alphabetical t)

    ;; 自动切换 list bullet
    ;; 若每个层级的 list 都使用同样的 list bullet,则可能造成难于区分哪个 list entry
    ;; 是属于哪个层级的. org-mode 提供了当改变 list 层级时自动改变 list bullet 的机制
    (setq org-list-demote-modify-bullet '(("+" . "-")
                                          ("*" . "-")
                                          ("1." . "-")
                                          ("1)" . "-")
                                          ("A)" . "-")
                                          ("B)" . "-")
                                          ("a)" . "-")
                                          ("b)" . "-")
                                          ("A." . "-")
                                          ("B." . "-")
                                          ("a." . "-")
                                          ("b." . "-")))
    ))

;; 修改 org 文件中各层级的 headline 前显示的标志
(defun rinetd-org/post-init-org-bullets ()

  (use-package org-bullets
    :defer t
    :init (progn
            (add-hook 'org-mode-hook 'org-bullets-mode)
            ;; 修改 org 中的标题图标
            ;; 更好看的标题符号标记:
            ;; 🐉 : http://graphemica.com/1F409
            ;; 🕊 : http://graphemica.com/1F54A
            ;; 🐘 : http://graphemica.com/1F418
            ;; 🐍 : http://graphemica.com/1F40D
            ;; 🐳 : http://graphemica.com/1F433
            ;; 🐙 : http://graphemica.com/1F419
            ;; 🐬 : http://graphemica.com/1F42C
            ;; 🐠 : http://graphemica.com/1F420
            ;; 🐡 : http://graphemica.com/1F421
            ;; 🐟 : http://graphemica.com/1F41F
            ;; from https://emacs-china.org/t/topic/250/3?u=liu233w
            ;; (setq org-bullets-bullet-list '("🐉" "🕊" "🐘" "🐍" "🐳" "🐙" "🐬" "🐠" "🐡" "🐟"))
            ;; (setq org-bullets-bullet-list '("❀" "❁" "❃" "❊" "❋" "✱" "✼" "✾" "✿"))
            ;;(setq org-bullets-bullet-list '("✙" "♱" "♰" "☥" "✞" "✟" "✝" "†" "✠" "✚" "✜" "✛" "✢" "✣" "✤" "✥"))

            ;; (setq org-bullets-bullet-list '("✺" "✹" "✸" "✷" "✶" "✭" "✦" "■" "▲" "●" ))

            (setq org-bullets-bullet-list '(;;"⓪"
                                            "①" "②" "③"
                                            "④" "⑤" "⑥" "⑦"
                                            "⑧" "⑨" "⑩" "⑪"
                                            "⑫" "⑬" "⑭"
                                            "⑮" "⑯" "⑰"
                                            "⑱" "⑲" "⑳"))

            ;; (setq org-bullets-bullet-list '("☯" "☰" "☱" "☲" "☳" "☴" "☵" "☶" "☷" )) ;天 泽 火 雷 风 水 山 地
            ;;天干 1qián、地坤 8kūn、雷震 4zhèn、风巽 5xùn、水坎 6kǎn、火离 3lí、山艮 7gèn、泽兑 2duì

            )

    )
  )

(defun rinetd-org/init-org-password-manager ()
  (use-package org-password-manager
    :defer t
    :config
    (progn
      (add-hook 'org-mode-hook 'org-password-manager-key-bindings))))

(defun rinetd-org/init-org-src ()
  (;progn
   with-eval-after-load 'org-src
                                        ; (require 'org-src)
    ;; Use puml mode when editing plantuml blocks with C-c '
    (add-to-list 'org-src-lang-modes (quote ("plantuml" . puml)))

    ;; http://wenshanren.org/?p=327
    (defun rinetd-org/org-insert-src-block (src-code-type)
      "Insert a `SRC-CODE-TYPE' type source code block in org-mode."
      (interactive
       (let ((src-code-types
              '("emacs-lisp" "python" "bash" "java" "js" "css" "calc" "plantuml"
                "sql" "ditaa" "lisp" "org" "scheme" "sqlite")))
         (list (ido-completing-read "Source code type: " src-code-types))))
      (progn
        (newline-and-indent)
        (insert (format "#+BEGIN_SRC %s\n" src-code-type))
        (newline-and-indent)
        (insert "#+END_SRC\n")
        (previous-line 2)
        (org-edit-src-code)))

    (add-hook 'org-mode-hook
              '(lambda ()
                 ;; keybinding for editing source code blocks
                 (local-set-key (kbd "C-c s e") 'org-edit-src-code)
                 ;; keybinding for inserting code blocks
                 (local-set-key (kbd "C-c i s") 'rinetd-org/org-insert-src-block)))
    ))

;;;;;;;;;;;;;;;;;;;;
;; org-babel
;;;;;;;;;;;;;;;;;;;;
;; TODO: very slow!!!!
(defun rinetd-org/init-ob-core ()
  (with-eval-after-load 'org
                                        ; (require 'ob-core)



    ;; (setq-default org-export-babel-evaluate nil)

    (setq org-export-babel-evaluate (quote inline-only));;export org file的时候只执行inline的代码
    (setq org-src-fontify-natively t) ;; Org-mode 文本内语法高亮

    (setq org-src-preserve-indentation t)
    (setq org-src-tab-acts-natively t)
    (add-hook 'org-babel-after-execute-hook 'org-display-inline-images)
    (add-hook 'org-mode-hook 'org-display-inline-images)

    (defun org/display-inline-images ()
      (condition-case nil
          (org-display-inline-images)
        (error nil)))

    ;; (add-hook 'org-babel-after-execute-hook 'org/display-inline-images 'append)

    ;; Make babel results blocks lowercase
    (setq org-babel-results-keyword "results")

    ;; C-c C-c 执行代码块时,不需要确认
    ;; Do not prompt to confirm evaluation
    ;; This may be dangerous - make sure you understand the consequences
    ;; of setting this -- see the docstring for details
    (setq org-confirm-babel-evaluate nil)


    ;; 可以在 org 中自动加载的库（稍后可以直接在 src_block 里面执行代码）
    (org-babel-do-load-languages
     'org-babel-load-languages
     '((sh         . t)
       ;; (js         . t)
       (sh . t)
       (org .t)
       (emacs-lisp . t)
       ;; (perl       . t)
       ;; (scala      . t)
       ;; (clojure    . t)
       ;; (python     . t)
       ;; (ipython . t)
       ;; (ruby       . t)
       ;; (dot        . t)
       ;; (css        . t)
       ;; (ledger . t)
       ;; (org . t)
       ;; (latex .t)
       ;; (octave .t)
       ;; (perl .t)
       ;; (sh .t)
       ;; (R .t)
       ;; (sql . nil)
       (ditaa . t)
       ;; (plantuml   . t)
       (C . t)
       ))
    ))

(defun rinetd-org/init-ob-ipython ()
  "在 org 中使用 ipython"
  (use-package ob-ipython
    :defer t
    ))
;; ditaa 工具能够帮我们把 ASCII 图转成漂亮的架构图片
;; @see http://ditaa.sourceforge.net/
(defun rinetd-org/init-ob-ditaa ()
  (use-package ob-ditaa
    :config
    ;; (setq org-ditaa-jar-path "/opt/java-lib/ditaa0_9.jar")
    (setq org-ditaa-jar-path (directory-files "/opt/java-lib" t "ditaa[[:ascii:]]+\\.jar$"))
    ))

;; plantuml 工具能够帮我们把 UML 描述代码 转成漂亮的 UML 图片
;; @see http://plantuml.com/
(defun rinetd-org/init-ob-plantuml ()
  (use-package ob-plantuml
    :config
    (setq org-plantuml-jar-path "/opt/java-lib/plantuml.jar")))

(defun rinetd-org/init-ob-ledger ()
  (use-package ob-ledger))

(defun rinetd-org/init-my-org-mode ()
  (use-package my-org-mode))

(defun rinetd-org/init-secretaria ()
  (use-package secretaria
    :ensure nil
    :preface
    (use-package alert)
    (use-package f)
    (use-package s)
    :config
    ;; use this for getting a reminder every 30 minutes of those tasks scheduled
    ;; for today and which have no time of day defined.
    (add-hook 'after-init-hook #'secretaria-today-unknown-time-appt-always-remind-me)))
(defun rinetd-org/init-deft()
  (use-package deft
    :defer t
    :init
    (progn
      (setq deft-use-filter-string-for-filename t)
      (setq deft-recursive nil)
      (setq deft-extension "org")
      (setq deft-directory org-directory)))
      (setq deft-extensions '("org" "md" "txt" "gpg")
            deft-text-mode 'org-mode
            deft-use-filename-as-title t)
      (spacemacs/set-leader-keys "an" 'spacemacs/deft)
      ; (evil-leader/set-key-for-mode 'deft-mode "q" 'quit-window)
      (defun spacemacs/deft ()
        "Helper to call deft and then fix things so that it is nice and works"
        (interactive)
        (deft)
        ;; Hungry delete wrecks deft's DEL override
        (when (fboundp 'hungry-delete-mode)
          (hungry-delete-mode -1))
        ;; When opening it you always want to filter right away
        ; (spacemacs/normal-to-insert-state)
        )
    :config
    (progn
        ; (setq deft-recursive t
        ;      deft-directory notes-directory
        ;      deft-use-filename-as-title nil
        ;      deft-auto-save-interval 0
        ;      deft-org-mode-title-prefix t
        ;      deft-use-filter-string-for-filename t
        ;      deft-file-naming-rules '((noslash . "-")
        ;                               (nospace . "-")
        ;                               (case-fn . downcase)))

       (spacemacs|add-toggle deft-toggle-recursive
         :status deft-recursive
         :on (setq deft-recursive t) (deft-refresh)
         :off (setq deft-recursive nil) (deft-refresh)
         :documentation "Toggle recursively search for files in subdirectories"
         :evil-leader-for-mode (deft-mode . "t")
         )

       (spacemacs/set-leader-keys
         "nd" 'deft
         "nn" 'deft-find-file)

      ;  (evil-define-key 'normal deft-mode-map "q" 'quit-window)
       (with-eval-after-load 'deft
         (bind-map-set-keys deft-mode-map   "<S-return>" 'deft-new-file)
         (define-key deft-mode-map (kbd "C-g") 'quit-window)
         (spacemacs/set-leader-keys-for-major-mode 'deft-mode
           "a" 'deft-archive-file
           "c" 'deft-filter-clear
           "s" 'deft-toggle-sort-method
           "d" 'deft-delete-file
           "i" 'deft-toggle-incremental-search
           "n" 'deft-new-file
           "r" 'deft-rename-file)
           )
  ))
;; Local Variables:
;; coding: utf-8
;; no-byte-compile: nil
;; End:

;;; packages.el ends here


;;; packages.el ends here
