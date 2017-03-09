
(defconst rinetd-chinese-packages
  '(
    ;;chinese-pyim
    chinese-pyim-greatdict
    ;; chinese-fonts-setup
    ))

(defun rinetd-chinese/init-chinese-pyim ()
  (use-package chinese-pyim
    :if (eq 'pinyin chinese-default-input-method)
    :init
    (progn
      (setq pyim-directory (concat dotspacemacs-directory ".cache/pyim/")
            pyim-page-length 10
            pyim-use-tooltip 'pos-tip   ; 使用 pos-tip 包来绘制选词框（这种选词框比较好看）
            x-gtk-use-system-tooltips t ; Linux 平台下，emacs 可以使用 GTK 来绘制选词框
            pyim-dcache-directory (concat dotspacemacs-directory ".cache/pyim/dcache")
            pyim-personal-file (concat pyim-directory "pyim-personal.txt")
            pyim-property-file (concat pyim-directory "pyim-words-property.txt")
            default-input-method "chinese-pyim")

      (evilified-state-evilify pyim-dicts-manager-mode pyim-dicts-manager-mode-map))
    :config
    (progn
      ;; 激活词库
      (setq pyim-dicts (quote
        ((:name "pyim-bigdict"
          :file "~/Dropbox/pyim/pyim-bigdict.pyim"
          :coding utf-8-unix
          :dict-type pinyin-dict)
         ; (:name "pyim-greatdict"
         ;  :file "~/Dropbox/pyim/pyim-greatdict.pyim"
         ;  :coding utf-8-unix
         ;  :dict-type pinyin-dict)
         )))

      ;; 为 isearch 开启拼音搜索功能
      (setq pyim-isearch-enable-pinyin-search t)
      ;; 强制关闭 isearch 搜索框中文输入（即使在 Chinese-pyim 激活的时候）
      (setq-default pyim-english-input-switch-functions
              '(pyim-probe-isearch-mode))
      ;; 禁用 dabberv 中文补全
      (setq pyim-company-complete-chinese-enable nil)
      )))

(defun rinetd-chinese/init-chinese-pyim-greatdict()
  (use-package chinese-pyim-greatdict)
  (chinese-pyim-greatdict-enable)
  )

(defun rinetd-chinese/init-chinese-fonts-setup()
  (use-package chinese-fonts-setup)
  (chinese-fonts-setup-enable)
  )

;;; packages.el ends here
