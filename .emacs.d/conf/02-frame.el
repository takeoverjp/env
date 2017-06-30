;; 起動時のwindow
;; (split-window-horizontally)

;; カラム番号も表示
(column-number-mode t)

;; ファイルサイズを表示
(size-indication-mode t)

;; リージョン内の行数と文字数をモードラインに表示する（範囲指定時のみ）
(defun count-lines-and-chars ()
  (if mark-active
      (format "%d lines,%d chars "
	      (count-lines (region-beginning) (region-end))
	      (- (region-end) (region-beginning))
	      "")))
(add-to-list 'default-mode-line-format
	     '(:eval (count-lines-and-chars)))

;; Shift + 上下左右 -> window move
(windmove-default-keybindings)

;; split window
(fset 'split2
   [?\C-x ?1 ?\C-x ?3 escape kp-3 kp-8 ?\C-x ?}])
(fset 'split3
   [?\C-x ?1 ?\C-x ?3 ?\C-x ?3 ?\C-x ?+])
(defun window-toggle-division ()
  "ウィンドウ 2 分割時に、縦分割<->横分割"
  (interactive)
  (unless (= (count-windows 1) 2)
    (error "ウィンドウが 2 分割されていません。"))
  (let ((before-height)
        (other-buf (window-buffer (next-window))))
    (setq before-height (window-height))
    (delete-other-windows)
    (if (= (window-height) before-height)
        (split-window-vertically)
      (split-window-horizontally))
    (other-window 1)
    (switch-to-buffer other-buf)
    (other-window -1)))
