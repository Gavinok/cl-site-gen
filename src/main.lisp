(uiop:define-package cl-site-gen
  (:use :cl)
  (:nicknames #:csg)
  (:import-from #:alexandria
                #:when-let
                #:write-string-into-file)
  (:import-from #:arrows
                #:->
                #:->>)
  (:import-from #:fset
                #:image
                #:seq
                #:with
                #:@)
  (:export #:create-sites
           #:ref))
(in-package :cl-site-gen)

(defun ref (path &key (new-extension ".html"))
  "A simple way to convert file paths from .lisp extensions to .html.
For use in users code.
e.g.
(ref #p\"doc.lisp\") => \"doc.html\""
  (replace-extension path :new-extension new-extension))

(defun replace-extension
    (path &key (start 0) (new-extension ".html"))
  "Change the file extension in the given PATH to the
NEW-EXTENSION."
  (let ((p (namestring path)))
    (concatenate 'string
                 (subseq p start (position #\. p :from-end t))
                 new-extension)))

(defun recursively-find-files (path &key (file-extension ".lisp"))
  "Recursively find all files ending with the given FILE-EXTENSION and
return it as an fset:seq."
  (if (uiop:directory-exists-p path)
      (->> (concatenate 'string (uiop:native-namestring (pathname path))
                        "/**/*"
                        file-extension)
           directory
           (fset:convert 'seq))
      (error (format nil "Directory not found ~a" path))))


(defun input-to-output-path
    (input-path output-path &key (file-extension ".html"))
  "Returns a function that takes a PATH and converts the start of it's
name from INPUT-PATH to OUTPUT-PATH with the FILE-EXTENSION appended
to the end."
  (let ((ipath (namestring (truename input-path)))
        (opath (-> (concatenate 'string (namestring output-path) "/")
                   ensure-directories-exist
                   truename
                   namestring)))
    (lambda (path)
      (let ((p (-> path
                   pathname
                   namestring)))

        (concatenate 'string
                     opath
                     (replace-extension p :start (length ipath)
                                          :new-extension file-extension))))))

(defun eval-file (file)
  "Evaluate a FILE and return the final evaluated flute output into a
string."
  (with-open-file (in file)
    (loop :for expr = (read in nil :eof)
          :when (eq expr :eof)
            :return (car (last xs))
          :collect (eval expr) :into xs)))

(defun eval-path (path)
  "Recusively evaluate files into html strings."
  (typecase path
    (pathname (fset:map (:path path)
                        (:content (eval-file path))))
    (seq (image #'eval-path path))))

(defun write-html-to-file (content file-path &key (if-exits :error) (if-does-not-exist :create))
  "Writes the string CONTENT into the file at FILE-PATH"
  (let ((file (ensure-directories-exist file-path)))
    (write-string-into-file content file
                            :if-exists if-exits
                            :if-does-not-exist if-does-not-exist)))

(defun create-sites
    (input-path output-path &key (if-exits :error) (if-does-not-exist :create) to-string-function)
  "Evaluate all of the lisp files under the INPUT-PATH directory and
generate a directory OUTPUT-PATH containing the resulting strings with
the file extension changed to `.html'.

The way that a file is converted from the input path to the output
path is determined by the TO-STRING-FUNCTION.
"
  (let ((output-contents (-> input-path recursively-find-files))
        (path-conv       (input-to-output-path input-path output-path)))

    (loop :with iter = (fset:iterator output-contents)
          :for i = (eval-path (funcall iter :get))
          :while (funcall iter :more?)
          :collect (when-let ((content (if to-string-function
                                           (funcall to-string-function (fset:@ i :content))
                                           (fset:@ i :content)))
                              (path (fset:@ i :path)))
                     (write-html-to-file (the string content)
                                         (funcall path-conv path)
                                         :if-exits if-exits
                                         :if-does-not-exist if-does-not-exist)))))

(defun main ()
  (let ((help (or (member "-h" uiop:*command-line-arguments* :test #'equal)
                  (member "--help" uiop:*command-line-arguments* :test #'equal)
                  (< 2 (length uiop:*command-line-arguments*))))
        (input-directory  (first uiop:*command-line-arguments*))
        (output-directory (second uiop:*command-line-arguments*)))
    (when help
      (format t "csg input-directory output-directory	Generate static site in output-directory.~% csg")
      (uiop:quit))
    (create-sites input-directory output-directory)
    (princ "Yay it worked")))
