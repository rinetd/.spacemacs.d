

    ;;;;;;;;;;;;;;;;;;;;
;; Custom Key Bindings
    ;;;;;;;;;;;;;;;;;;;;
(spacemacs/set-leader-keys
  ;; Go to next org file in org-agenda-files
  "oC"  'org-cycle-agenda-files

  ;; save
                                        ; "ocg" 'org-clock-goto
                                        ; "oci" 'org-clock-in
  "oS"  'org-save-all-org-buffers

  ;; toggle
  "oTb" 'org-hide-block-toggle-all
  "oTi" 'org-toggle-inline-images
  "oTl" 'org-toggle-link-display

  ;; other
  "ob" 'org-iswitchb)

(spacemacs/set-leader-keys-for-major-mode 'org-mode "C-o" 'org-toggle-inline-images)
(spacemacs/set-leader-keys-for-major-mode 'org-mode "it" 'org-insert-todo-heading)

(global-set-key (kbd "<f1>") 'org-toggle-inline-images);; 显示/隐藏图片

(with-eval-after-load 'org
  (define-key org-mode-map (kbd "RET") 'newline-and-indent) ;;回车后自动缩进
  )

(spacemacs/set-leader-keys-for-major-mode 'org-mode
  "tl" 'org-toggle-link-display)
