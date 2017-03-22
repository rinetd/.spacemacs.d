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


;; (define-key evil-normal-state-map (kbd "$") (kbd "$"))
                                        ; H goes to beginning of the line

;; 逗号后面自动加空格
(global-set-key (kbd ",") #'(lambda () (interactive) (insert ", ")))

(define-key global-map [help] 'help-command)


;; git merge
(spacemacs/set-leader-keys "ge" 'smerge-ediff)

(spacemacs/set-leader-keys "ESC"    'ivy-switch-buffer)
(spacemacs/set-leader-keys "`"    'spacemacs/alternate-window)
(spacemacs/set-leader-keys "TAB"  'other-window)

(spacemacs/set-leader-keys "."  'spacemacs/fold-transient-state/body)

;;zilongshanren
(spacemacs/set-leader-keys "fl" 'find-file-literally-at-point)
(spacemacs/set-leader-keys ":" 'counsel-M-x)
(global-set-key (kbd "s-s") 'save-buffer)

(spacemacs/declare-prefix "he" "emacs")
;; rename prefix name
;; (spacemacs/set-leader-keys "he" 'help-command)
;; A complementary binding to the apropos-command (C-h a)
(define-key 'help-command "A" 'apropos)
(define-key 'help-command (kbd "C-f") 'find-function)
(define-key 'help-command (kbd "C-k") 'find-function-on-key)
(define-key 'help-command (kbd "C-v") 'find-variable)
(define-key 'help-command (kbd "C-l") 'find-library)
(define-key 'help-command (kbd "C-i") 'info-display-manual)

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
(define-key evil-normal-state-map (kbd "S") 'avy-goto-word-or-subword-1)
(define-key evil-normal-state-map (kbd "C-e") 'mwim-end-of-code-or-line)

;; atom 行插入
(global-set-key (kbd "C-S-d") 'spacemacs/duplicate-line-or-region) ;;复制行
(global-set-key (kbd "<C-S-return>") 'evil-open-above)   ;;上方插入空行
(global-set-key (kbd "<C-return>") 'evil-open-below)     ;;下方插入空行
(global-set-key [(shift return)] 'evil-open-below)

(with-eval-after-load 'lispy
  (define-key lispy-mode-map (kbd "<C-return>") 'evil-open-below))
;; 交换行
(global-set-key (kbd "<C-up>") 'move-text-line-up)
(global-set-key (kbd "<C-down>") 'move-text-line-down)
