;;; packages.el --- rinetd-engine layer packages file for Spacemacs.
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
;; added to `rinetd-engine-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `rinetd-engine/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `rinetd-engine/pre-init-PACKAGE' and/or
;;   `rinetd-engine/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:

(defconst rinetd-engine-packages
  '(engine-mode))

(defun rinetd-engine/post-init-engine-mode()

  (defengine icba "http://iciba.com/%s")
  (add-to-list 'search-engine-alist
			   '(icba
				 :name "iCIBA"
				 :url "http://iciba.com/%s"))

  (defengine baidu "http://baidu.com/s?wd=%s")
  (add-to-list 'search-engine-alist
			   '(baidu
                 :name "百度一下"
                 :url "http://www.baidu.com/s?wd=%s")
				 )

     )

;;; packages.el ends here
