;; This buffer is for text that is not saved, and for Lisp evaluation.
;; To create a file, visit it with ‘C-x C-f’ and enter text in its buffer.

(dolist (p '(1 2 3))
  (format t "%s" "hello"))

(defun my-fun ()
  (interactive)
  (save-excursion
  (goto-line 30)
  (let ((start (point)))
    (goto-line 32)
    (delete-region start (line-end-position))))  
  (insert "ghc-options:")
  (save-buffer))


(setq default-directory "~/code/advent-code/advent-of-code-2015/")

(defun my-fun2 ()
  (interactive)
  (goto-line 31)
  (insert "- -Wall\n")
  (save-buffer))


;; ============= BUFfer MENU COnfiguring !! ===================
;; Buffer menu k = kill
;; Buffer menu x to execute
(defun my-fix ()
    (interactive)
  (dolist (p (number-sequence 6 25))
    (progn  
      (find-file-other-window (format "%s%s%d%s%d%s" default-directory "day" p "/aoc2015day" p "/package.yaml"))
      (my-fun2)
      (find-file-other-window "../../"))))

(my-fix)

(defun show-next (arg)
  "Show next line's buffer in another window."
  (interactive "p")
  (next-line arg)
  (Buffer-menu-switch-other-window))

(defun show-previous (arg)
  "Show previous line's buffer in another window."
  (interactive "p")
  (previous-line arg)
  (Buffer-menu-switch-other-window))

(define-key Buffer-menu-mode-map "\M-n" 'show-next)
(define-key Buffer-menu-mode-map "\M-p" 'show-previous)   



  (find-file-other-window (format "%s%s" default-directory "day6/aoc*/package.yaml"))
  (my-fun)
  (find-file-other-window "../../")
  (find-file-other-window (format "%s%s" default-directory "day7/aoc*/package.yaml"))
  (my-fun)
  (find-file-other-window "../../")
  (find-file-other-window (format "%s%s" default-directory "day8/aoc*/package.yaml"))
  (my-fun)
  (find-file-other-window "../../")
  (find-file-other-window (format "%s%s" default-directory "day9/aoc*/package.yaml"))
  (my-fun)
  (find-file-other-window "../../")
  (find-file-other-window (format "%s%s" default-directory "day10/aoc*/package.yaml"))
  (my-fun)
  (find-file-other-window "../../")
  (find-file-other-window (format "%s%s" default-directory "day11/aoc*/package.yaml"))
  (my-fun)
  (find-file-other-window "../../"))







