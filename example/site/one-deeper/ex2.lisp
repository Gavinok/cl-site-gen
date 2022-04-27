(defun testing ()
  (flute:p "hello world"))

(flute:elem-str
 (flute:h
  (html (h1 "again")
        (testing))))
