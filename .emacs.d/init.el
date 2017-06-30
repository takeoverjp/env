(defvar user-emacs-directory "~/.emacs.d")

(defun add-to-load-path (&rest paths)
  (let (path)
    (dolist (path paths paths)
      (let ((default-directory
              (expand-file-name (concat user-emacs-directory path))))
        (add-to-list 'load-path default-directory)
        (if (fboundp 'normal-top-level-add-subdirs-to-load-path)
            (normal-top-level-add-subdirs-to-load-path))))))

(add-to-load-path "elisp" "conf")

;; ELPA(package.el)の設定 http://bit.ly/pkg-el23
(when (require 'package nil t)
  (package-initialize)
  (setq package-archives
	       '(("marmalade" . "http://marmalade-repo.org/packages/")
               ("ELPA" . "http://tromey.com/elpa/")
               ("melpa" . "http://melpa.org/packages/")
               ("org" . "http://orgmode.org/elpa/"))))

;; http://coderepos.org/share/browser/lang/elisp/init-loader/init-loader.el
(require 'init-loader)
(init-loader-load "~/.emacs.d/conf")


;; key bind
;; macro
(define-key global-map (kbd "C-c l") 'toggle-truncate-lines)
(define-key global-map (kbd "C-x C-b") 'buffer-menu)
;; 対応する括弧にジャンプ
(defun match-paren (arg)
  "Go to the matching paren if on a paren; otherwise insert %."
  (interactive "p")
  (cond ((looking-at "\\s\(") (forward-list 1) (backward-char 1))
        ((looking-at "\\s\)") (forward-char 1) (backward-list 1))
        (t (self-insert-command (or arg 1)))))
(define-key global-map (kbd "M-i") 'match-paren)
;; バッファの更新
(define-key global-map (kbd "C-c C-r") 'revert-buffer)
;; ミニバッファの履歴を `C-p' と `C-n' で辿れるようにする
(define-key minibuffer-local-must-match-map (kbd "C-p") 'previous-history-element)
(define-key minibuffer-local-must-match-map (kbd "C-n") 'next-history-element)
(define-key minibuffer-local-completion-map (kbd "C-p") 'previous-history-element)
(define-key minibuffer-local-completion-map (kbd "C-n") 'next-history-element)
(define-key minibuffer-local-map (kbd "C-p") 'previous-history-element)
(define-key minibuffer-local-map (kbd "C-n") 'next-history-element)
(define-key global-map (kbd "C-c ;") 'comment-region)      ; コメントアウト
(define-key global-map (kbd "C-c :") 'uncomment-region)    ; コメント解除
(define-key global-map (kbd "C-c i") 'indent-region)       ; インデント


;; ファイルが#!から始まる場合、+xをつけて保存する
(add-hook 'after-save-hook
	  'executable-make-buffer-file-executable-if-script-p)

(defun elisp-mode-hooks ()
  "lisp-mode-hooks"
  (when (require 'eldoc nil t)
    (setq eldoc-idle-delay 0.2)
    (setq eldoc-echo-area-use-multiline-p t)
    (turn-on-eldoc-mode)))
(add-hook 'emacs-lisp-mode-hook 'elisp-mode-hooks)

;; スタートアップを消す
(setq inhibit-startup-message t)

;;; 終了時にオートセーブファイルを消す
(setq delete-auto-save-files t)
;; tabではなく space を使う
(setq-default indent-tabs-mode nil)

;;; 再帰的にgrep
(require 'grep)
(setq grep-command-before-query "grep -InH -r -e ")
(defun grep-default-command ()
  (if current-prefix-arg
      (let ((grep-command-before-target
             (concat grep-command-before-query
                     (shell-quote-argument (grep-tag-default)))))
        (cons (if buffer-file-name
                  (concat grep-command-before-target
                          " *."
                          (file-name-extension buffer-file-name))
                (concat grep-command-before-target " ."))
              (+ (length grep-command-before-target) 1)))
    (car grep-command)))
(setq grep-command (cons (concat grep-command-before-query " .")
                         (+ (length grep-command-before-query) 1)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; C++ MODE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-hook 'c++-mode-hook
          '(lambda()
             (c-set-style "gnu")
             (setq indent-tabs-mode nil)     ; インデントは空白文字で行う（TABコードを空白に変換）
             (c-set-offset 'innamespace 0)   ; namespace {}の中はインデントしない
             (c-set-offset 'arglist-close 0) ; 関数の引数リストの閉じ括弧はインデントしない
             ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; compile
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-key global-map (kbd "C-c c") 'compile)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; GNU GLOBAL(gtags)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(autoload 'gtags-mode "gtags" "" t)
(setq gtags-mode-hook
      '(lambda ()
	 (local-set-key "\M-t" 'gtags-find-tag)
	 (local-set-key "\M-r" 'gtags-find-rtag)
	 (local-set-key "\M-s" 'gtags-find-symbol)
	 (local-set-key "\C-t" 'gtags-pop-stack)
	 ))
(add-hook 'c-mode-common-hook
	  '(lambda()
	     (gtags-mode 1) ;Cのソースを開いたら自動的にmodeをon
	     ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; DIFF MODE (change color)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-hook 'diff-mode-hook
	  (lambda ()
	    (set-face-foreground 'diff-file-header-face "blue1")
	    (set-face-foreground 'diff-index-face "thistle")
	    (set-face-foreground 'diff-hunk-header-face "white")
	    (set-face-foreground 'diff-removed-face "red")
	    (set-face-background 'diff-removed-face "black")
	    (set-face-foreground 'diff-added-face "green1")
	    (set-face-background 'diff-added-face "black")
	    (set-face-foreground 'diff-changed-face "DeepSkyBlue1")
	    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; c-eldoc
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(autoload 'c-eldoc "c-eldoc")
(add-hook 'c-mode-hook 'c-turn-on-eldoc-mode)
(add-hook 'c++-mode-hook 'c-turn-on-eldoc-mode)
(setq c-eldoc-buffer-regenerate-time 60)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; GIT MODE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(autoload 'magit-status "magit" "" t)
(define-key global-map "\C-xvs" 'magit-status)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Display Function Name
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;which-func (display-func)
(which-func-mode t)
(setq which-func-modes t)
(delete (assoc 'which-func-mode mode-line-format) mode-line-format)
(setq which-func-header-line-format
      '(which-func-mode
        ("" which-func-format)))
(defadvice which-func-ff-hook (after header-line activate)
  (when which-func-mode
    (delete (assoc 'which-func-mode mode-line-format) mode-line-format)
    (setq header-line-format which-func-header-line-format)))
