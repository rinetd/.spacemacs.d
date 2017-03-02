;;; keybindings.el --- zilongshanren Layer packages File for Spacemacs
;;
;; Copyright (c) 2015-2016 zilongshanren
;;
;; Author: zilongshanren <guanghui8827@gmail.com>
;; URL: https://github.com/zilongshanren/spacemacs-private
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3


(define-key evil-normal-state-map (kbd "$") (kbd "$")) ; H goes to beginning of the line



(define-key global-map [help] 'help-command)

(spacemacs/declare-prefix "he" "emacs")
;; rename prefix name
;; (spacemacs/set-leader-keys "he" 'help-command)

;; git merge
(spacemacs/set-leader-keys "ge" 'smerge-ediff)

(spacemacs/set-leader-keys
  "<right>"  'evil-window-right
  "<left>"  'evil-window-left
  "<up>"  'evil-window-up
  "<down>"  'evil-window-down)

(global-set-key (kbd "<S-left>") 'evil-window-left)
(global-set-key (kbd "<S-right>") 'evil-window-right)
(global-set-key (kbd "<S-up>") 'evil-window-up)
(global-set-key (kbd "<S-down>") 'evil-window-down)

;; 移动
(define-key evil-normal-state-map (kbd "-") nil)
(define-key evil-normal-state-map (kbd "s") 'avy-goto-word-or-subword-1)
(define-key evil-normal-state-map (kbd "W") 'evil-forward-word-begin)
(define-key evil-normal-state-map (kbd "C-e") 'mwim-end-of-code-or-line)

;; 行插入
(global-set-key (kbd "<C-S-return>") 'evil-open-above)
(global-set-key (kbd "<C-return>") 'evil-open-below)
(with-eval-after-load 'lispy
  (define-key lispy-mode-map (kbd "<C-return>") 'evil-open-below))
;; 交换行
(global-set-key (kbd "<C-up>") 'move-text-line-up)
(global-set-key (kbd "<C-down>") 'move-text-line-down)

