;;; packages.el --- rinetd-org-agenda layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2017 Sylvain Benner & Contributors
;;
;; Author: ubuntu <guanghui8827@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3


(defconst rinetd-org-agenda-packages
  '(
    (org-agenda :location built-in)
))

(defun rinetd-org-agenda/init-org-agenda()
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
;;; packages.el ends here
