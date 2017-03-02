;;; funcs.el --- zilongshanren Layer packages File for Spacemacs
;;
;; Copyright (c) 2015-2016 zilongshanren 
;;
;; Author: zilongshanren <guanghui8827@gmail.com>
;; URL: https://github.com/zilongshanren/spacemacs-private
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3
(with-eval-after-load 'evil
  (defalias #'forward-evil-word #'forward-evil-symbol))
;;(add-hook 'python-mode-hook #'(lambda () (modify-syntax-entry ?_ "w")))
;; (add-hook 'emacs-lisp-mode-hook #'(lambda ()
;;                                    (modify-syntax-entry ?- "w")
;;                                    (modify-syntax-entry ?_ "w")))
;; (toggle-debug-on-quit)
