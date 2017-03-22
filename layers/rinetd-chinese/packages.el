
(defconst rinetd-chinese-packages
  '(
    (mule :location built-in)
    ;;chinese-pyim
    chinese-pyim-greatdict
    ;; chinese-fonts-setup
    pangu-spacing
    ))

;; Charset 设置
(defun rinetd-chinese/init-mule ()
  (use-package mule
    :config
    (progn
      ;; disable CJK coding/encoding (Chinese/Japanese/Korean characters)
      (setq utf-translate-cjk-mode nil)

      ;; Always, always, prefer UTF-8, anything else is insanity
      (when (or window-system (locale-is-utf8-p))
        ;; 影响 chinese-pyim, 造成不能输入中文的故障
        ;; set environment coding system
        ;; (set-language-environment "UTF-8")

        (set-charset-priority 'unicode)

        ;; @see https://github.com/hick/emacs-chinese
        (set-default-coding-systems 'utf-8)
        (set-buffer-file-coding-system 'utf-8-unix)
        (set-clipboard-coding-system 'utf-8-unix)
        (set-file-name-coding-system 'utf-8-unix)
        (set-keyboard-coding-system 'utf-8-unix)
        (set-selection-coding-system 'utf-8-unix)
        (set-next-selection-coding-system 'utf-8-unix)
        (set-terminal-coding-system 'utf-8-unix)

        (setq default-process-coding-system '(utf-8-unix . utf-8-unix))
        (setq locale-coding-system 'utf-8)

        ;; @see ~/.emacs.d/core/core-spacemacs.el:72
        ;; (prefer-coding-system 'utf-8)
        )
    )))
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
;; 覆盖 Chinese Layer 的 init 方法
(defun rinetd-chinese/init-pangu-spacing ()
  "覆盖 Chinese-layer 中的设置。默认关闭 pangu-spacing，只有在 buffer 比较小的时候才启动，
如果是启动之后再关闭的话就开的太慢了。"
  (use-package pangu-spacing
    :init
    (progn
      (global-pangu-spacing-mode -1)
      (spacemacs|hide-lighter pangu-spacing-mode)
      ;; Always insert `real' space in org-mode.
      (add-hook 'org-mode-hook
                '(lambda ()
                   (set (make-local-variable 'pangu-spacing-real-insert-separtor) t)))

      (defun enable-pangu-spacing-when-buffer-not-large ()
        "when buffer is not large, turn on it"
        (when (< (buffer-size) *large-buffer-threshold*)
          (pangu-spacing-mode 1)))

      (dolist (i '(org-mode-hook prog-mode-hook text-mode-hook))
        (add-hook i 'enable-pangu-spacing-when-buffer-not-large)))
    :config
    ;; add toggle options
    (spacemacs|add-toggle toggle-pangu-spaceing
      :status pangu-spacing-mode
      :on (global-pangu-spacing-mode)
      :off (global-pangu-spacing-mode -1)
      :documentation "Toggle pangu spacing mode"
      :evil-leader "tp")
    ))
;;; packages.el ends here
