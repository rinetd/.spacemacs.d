;;; packages.el --- Crypt Layer packages File for Spacemacs.
;;
;; Copyright (C) 2016  Eduardo Ruiz
;;
;; Author: Eduardo Ruiz <eduarbo@gmail.com>
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defconst crypt-packages
  '(
    (epa-file :location built-in)
    org-crypt
    ))



;; 借助 EasyPG package 原生支持 GnuPG 加密. 提供基本的 GnuPG 功能.
;;# -*- mode:org; epa-file-encrypt-to: ("me@mydomain.com") -*-
(defun crypt/init-epa-file ()
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
(defun crypt/init-org-crypt ()
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
          org-crypt-disable-auto-save 'encrypt
          epa-file-encrypt-to org-crypt-key
          ;; use gpg2 to cache the passphrase with gpg-agent, otherwise it won't work
          epg-gpg-program "gpg2")))

;; http://coldnew.github.io/blog/2013/07/13_5b094.html
;; 使用`org-crypt’库,可以自动将带”:secret:” tag 的 headline ,在写入时加密存储.
;; 该功能对于想要将密码等隐私消息存入 org 文件带来便利.
;; (defun crypt/post-init-org-crypt ()
;;   (;progn
;;    with-eval-after-load 'org-crypt
;;                                         ; (require 'org-crypt)

;;     ;; 保存前,自動加密回去
;;     (org-crypt-use-before-save-magic)

;;     ;; 設定要加密的 tag 標籤為 secret crypt
;;     (setq org-crypt-tag-matcher "crypt")

;;     ;; 设置 secret 标签不参与继承,避免造成重複加密
;;     ;; (但是子項目還是會被加密喔)
;;     (setq org-tags-exclude-from-inheritance (quote ("crypt")))

;;     ;; GPG key to use for encryption
;;     ;; Either the Key ID or set to nil to use symmetric encryption.
;;     (setq org-crypt-key nil)

;;     ;; 要想解密 headline,则需要在光标定位到加密内容处,然后执行`M-x org-decrypt-entry’
;;     ;; 默认情况下, Emacs 会定时自动保持在编辑的文件,
;;     ;; 若此时在编辑的文件为密码文件且内容已经被解密,则可能存在将解密后的文本保存到磁盘上,
;;     ;; 从而造成敏感信息泄露的情况,因此一般我们在编辑 crypt 文件时,取消自动保存功能
;;     (setq org-crypt-disable-auto-save t)
;;     ))
