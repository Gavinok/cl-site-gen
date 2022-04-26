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
  "Change the file extension the in the given PATH to the
NEW-EXTENSION."
  (let ((p (namestring path)))
    (concatenate 'string
                 (subseq p start (position #\. p :from-end t))
                 new-extension)))

(defun recursively-find-files (path &key (file-extension ".lisp"))
  "Recursively find all files ending with the given FILE-EXTENSION and
return it as an fset:seq."
  (if (and (pathnamep path) (uiop:directory-exists-p path))
      (->> (concatenate 'string (uiop:native-namestring path)
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
                     (replace-extension
                      p :start (length ipath)
                        :new-extension file-extension))))))

(defun eval-file (file)
  "Evaluate a FILE and return the final evaluated flute output into a
string."
  (with-open-file (in file)
    (when-let ((expr (read in nil)))
      (eval expr))))

(defun eval-path (path)
  "Recusively evaluate files into html strings."
  (typecase path
    (pathname (fset:map (:path path)
                        (:content (eval-file path))))
    (seq (image #'eval-path path))))

(defun create-sites
    (input-path output-path &key (if-exits :error) (write-to-string-function #'flute:elem-str))
  "Evaluate all of the lisp files under the INPUT-PATH directory and
generate a directory OUTPUT-PATH containing the resulting strings with
the file extension changed to .html"
  (let ((output-contents (-> input-path
                             recursively-find-files
                             eval-path))
        (path-conv       (input-to-output-path input-path output-path)))

    (loop :with iter = (fset:iterator output-contents)
          :for i = (funcall iter :get)
          :until (funcall iter :done?)
          :do (write-string-into-file (funcall write-to-string-function (fset:@ i :content))
                                      (ensure-directories-exist
                                       (funcall path-conv (fset:@ i :path)))
                                      :if-exists if-exits))))

(defun demo ()
  (create-sites
   #p"example"
   #p"example-res"))
