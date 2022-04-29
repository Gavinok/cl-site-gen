;; Adding a couple reader macros to make this a little easier to read
;; and write.

(defun my-list-to-string (this)
  (etypecase this
    (string this)
    (symbol (symbol-name this))
    (list (reduce (lambda (x y) (concatenate (quote string) x " " y))
                  (mapcar (lambda (sy)
                            (etypecase sy
                              (symbol (symbol-name sy))
                              (string (write-to-string sy))
                              (number (format nil "~a" sy))
                              (list (my-list-to-string sy))))
                          this)))))

(defun string-reader (stream char)
  (declare (ignore char))
  (let ((this (let ((*readtable* (copy-readtable)))
                ;; Use case sensitive symbols
                (setf (readtable-case *readtable*) :preserve)
                (read stream t nil t)))
        (quote (get-macro-character #\')))
    (my-list-to-string this)))

(set-macro-character #\@ #'string-reader)
(-example-template
    "CL-SITE-GEN Home Page"
  (h1 @(Welcome to the home page 'of CL-SITE-GEN))
  (h2 @(What is CL-SITE-GEN?))
  (p  @(CL-SITE-GEN is a static site (generator) that takes valid Common
        Lisp code and uses that to generate html to create a static
        website.))

  (h2 @(Wait how is does that work?))

  (p @(The structure is very simple you first create a directory from
       which your site will be generated. In this case we will call it
       "example"))

  (p @(We will then create a file in the "example" directory called
       index.lisp. This will act as the basis for our first web
       page.))

  (p  @(By default CL-SITE-GEN uses)
      " " (href "https://github.com/ailisp/flute" "flute") " "

      @(but as you will soon see it isn’t an issue to change this In
            index.lisp we will write the following)
      ":")

  (pre (write-to-string
        '(flute:h
          (html
           (head
            (title "Hello"))
           (body
            (h1 "testing")
            (p "testing agains"))))))

  (p @(At first glance this may seem a bit painfull to work with but
       do not worry there are plenty of ways to make it easier to
       work with. Now start a REPL and quickload the CL-SITE-GEN
       project.))

  (pre (write-to-string
        '(ql:quickload :cl-site-gen)))

  (p @(Finally we can simply run) ":")

  (pre (write-to-string
        '(cl-site-gen:create-sites #p"example" #p"example-rec")))

  (p @(In this case the output directory would be example-rec.
       If we looked inside inside example-rec we would find a file
       called input.lisp containing the following html.))

  (pre "<!DOCTYPE html>
<html>
 <head><title>Hello</title></head>
 <body>
   <h1>testing</h1>
   <p>testing agains</p>
 </body>
</html>")

  (p @(This in fact matches the html that would be produced if we
       evaluated the contents of example/index.lisp. Feel free to
       give this a try for your self.)))

