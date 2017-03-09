

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
