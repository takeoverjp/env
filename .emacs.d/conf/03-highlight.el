(global-hl-line-mode t)
(if (window-system)
    (progn
      (defface my-hl-line-face
        ;; 背景がdark => 行背景を紺に
        '((((class color) (background dark))
           (:background "glay13" t))
          ;; 背景がlight => 行背景を紺に
          (((class color) (background light))
           (:background "glay13" t))
          (t (:bold)))
        "hl-lin's my face")
      (setq hl-line 'my-hl-line-face))
  (progn
    (setq hl-line-face 'underline)))

;; 対応する括弧の強調
(setq show-paren-delay 0)
(show-paren-mode t)
