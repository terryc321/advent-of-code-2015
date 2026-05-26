
;; emacs lisp code to go into each package.yaml file and add required dependcies

;; - text
;; - bytestring
;; - containers
;; - utf8-string
;; - pureMD5


    (let ((day 3))
      )

	    
    (progn
      (let ((days '(6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25)))
	(dolist (day days)
	  (let ((dest (format "%s%s%d%s%d%s" "/home/terry/code/advent-code/advent-of-code-2015/" "day" day "/aoc2015day" day "/package.yaml")))
	    (find-file-other-window  dest)
	    (goto-line 24)
	    (insert "- text\n- bytestring\n- containers\n- utf8-string\n- pureMD5\n")
	    (save-buffer)))))

