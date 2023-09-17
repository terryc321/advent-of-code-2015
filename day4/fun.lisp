




(defparameter input "bgvyzdsv")

;; ;; almost an ideal batch processing problem
;; (system* "echo" "-n" input "|" "md5sum")


;;#for i in {1..10000000}; do echo -n "bgvyzdsv$i" | md5sum | grep 00000 ; done


#|
abcdef609043
pqrstuv1048970
    If your secret key is abcdef, the answer is 609043, because the MD5 hash of abcdef609043 starts with five zeroes (000001dbbfa...), and it is the lowest such number to do so.
    If your secret key is pqrstuv, the lowest number it combines with to make an MD5 hash starting with five zeroes is 1048970; that is, the MD5 hash of pqrstuv1048970 looks like 000006136ef....
|#


(defun foo ()
  (let ((i 0))
    (loop while t do 
      (let ((str (format nil "bgvyzdsv~a" i)))
	;;(format t "trying ~a ~%" str)
	(let ((md (sb-md5:md5sum-string str)))
	  (when
	      (and (= 0 (aref md 2))
		   (= 0 (aref md 1))
		   (= 0 (aref md 0))
		   )
	    (format t "possible solution [ ~a : ~a : ~a ] ~%" i str md )
	    (format t "~2x~%~%" md)
	    )
	  (incf i))))))



#|

CL-USER> (foo)
possible solution [ 10859 : bgvyzdsv10859 : #(0 0 116 4 108 119 103 158 120 172
                                              140 3 60 101 160 163) ] 
#(0 0 74 4 6C 77 67 9E 78 AC 8C 3 3C 65 A0 A3)

possible solution [ 112018 : bgvyzdsv112018 : #(0 0 49 113 147 95 3 61 200 101
                                                247 129 32 252 101 139) ] 
#(0 0 31 71 93 5F 3 3D C8 65 F7 81 20 FC 65 8B)

possible solution [ 132651 : bgvyzdsv132651 : #(0 0 70 245 95 70 146 124 66 196
                                                106 228 159 83 172 67) ] 
#(0 0 46 F5 5F 46 92 7C 42 C4 6A E4 9F 53 AC 43)

possible solution [ 254575 : bgvyzdsv254575 : #(0 0 4 179 13 72 22 98 185 203
                                                12 16 95 101 73 178) ] 
#(0 0 4 B3 D 48 16 62 B9 CB C 10 5F 65 49 B2)      <<<<<----------------------- looks like five zeros

possible solution [ 592903 : bgvyzdsv592903 : #(0 0 81 117 188 108 227 192 34
                                                137 123 202 30 41 111 232) ] 
#(0 0 51 75 BC 6C E3 C0 22 89 7B CA 1E 29 6F E8)


reckon 

254575

is number for day 4


|#

#|

six zeros solution 

CL-USER> (foo)
possible solution [ 1038736 : bgvyzdsv1038736 : #(0 0 0 177 182 75 245 235 85
                                                  170 216 153 134 18 105 83) ] 
#(0 0 0 B1 B6 4B F5 EB 55 AA D8 99 86 12 69 53) <<<<<<<<<<<<<<<

possible solution [ 10979646 : bgvyzdsv10979646 : #(0 0 0 224 102 216 8 174 231
                                                    251 208 175 122 216 26 26) ] 
#(0 0 0 E0 66 D8 8 AE E7 FB D0 AF 7A D8 1A 1A)

possible solution [ 47134621 : bgvyzdsv47134621 : #(0 0 0 101 124 147 72 111 22
                                                    118 193 45 61 14 134 110) ] 
#(0 0 0 65 7C 93 48 6F 16 76 C1 2D 3D E 86 6E)


reckon this is number required

1038736


trivial in sbcl 
|#








	  
	  

      
