(in-package :weblocks-utils)

(defparameter *assets-directory* 
  (merge-pathnames 
    (make-pathname :directory '(:relative "assets"))
    (uiop:getcwd)))

(defvar *versions-file* 
  (merge-pathnames 
    (make-pathname :name "versions" :type "lisp-expr")
    *assets-directory*))

(defvar *assets-package-dir* nil)

(defvar *dispatchers-cache* (make-hash-table :test #'equal))

(defun file-get-contents(file &key (external-format :utf-8))
  (with-output-to-string (s)
    (with-open-file (in file :direction :input :external-format external-format)
      (loop for char = (read-char in nil) 
            while char do
            (write-char char s)))))

(defun cleanup-version (version)
  (string-trim 
    (format nil "~A~A~A" #\Newline #\Tab #\Space)
    version))

(defun get-version (repository-directory)
  (if (pathnamep repository-directory)
    (progn 
      (unless (cl-fad:directory-exists-p repository-directory)
        (error "Repository not found \"~A\"" repository-directory))
      (cleanup-version 
        (file-get-contents 
          (merge-pathnames 
            "version.txt"
            repository-directory))))
    (get-version-from-file repository-directory)))

(defun get-version-from-file (url)
  (let ((all-versions (get-all-versions-from-file))
        (version))

    (when all-versions
      (setf version (cdr (assoc url all-versions :test #'string=)))
      (when version 
        (return-from get-version-from-file version)))

    (setf version (cleanup-version (drakma:http-request (format nil "~A/version.txt" (string-right-trim "/" url)))))

    (push (cons url version) all-versions)
    (set-versions all-versions)
    version))

(defun get-all-versions-from-file ()
  (when (cl-fad:file-exists-p *versions-file*)
    (with-open-file (in *versions-file* :direction :input)
      (read in))))

(defun set-versions (versions)
  (with-open-file (out *versions-file* 
                       :direction :output 
                       :if-does-not-exist :create 
                       :if-exists :supersede)
    (pprint versions out)))

(defmacro with-directory (dir &body body)
  `(unwind-protect 
     (let ((last-dir (uiop:getcwd)))
       (uiop:chdir ,dir)
       ,@body
       (uiop:chdir last-dir))))

(defun fetch-file-to (url dest)
  (with-open-file (out dest 
                       :direction :output 
                       :if-does-not-exist :create 
                       :if-exists :supersede 
                       :element-type '(unsigned-byte 8))
    (let  ((input (drakma:http-request url :want-stream t)))
      (loop for byte = (read-byte input nil nil) 
            while byte 
            do (write-byte byte out))
      (close input))))

(defun copy-serve-file (url-or-file dest)
  (if (pathnamep url-or-file)
    (cl-fad:copy-file 
      (merge-pathnames (make-pathname :name "serve" :type "lisp") url-or-file)
      (merge-pathnames (make-pathname :name "serve" :type "lisp") dest))
    (fetch-file-to 
      (format nil "~A/serve.lisp" (string-right-trim "/" url-or-file))
      (merge-pathnames (make-pathname :name "serve" :type "lisp") dest))))

(defun copy-get-file (url-or-file dest)
  (if (pathnamep url-or-file)
    (cl-fad:copy-file 
      (merge-pathnames (make-pathname :name "get" :type "sh") url-or-file)
      (merge-pathnames (make-pathname :name "get" :type "sh") dest))
    (fetch-file-to 
      (format nil "~A/get.sh" (string-right-trim "/" url-or-file))
      (merge-pathnames (make-pathname :name "get" :type "sh") dest))))

(defun require-assets (url-or-file &key (webapp (current-webapp)))
  (let* ((version (get-version url-or-file))
         (*assets-package-dir* (merge-pathnames (make-pathname :directory `(:relative  ,version)) *assets-directory*)))

    (unless (cl-fad:directory-exists-p *assets-package-dir*)
      (format t "No package for ~A in ~A, trying to install~%" url-or-file *assets-package-dir*)
      (ensure-directories-exist *assets-package-dir*)

      (copy-get-file url-or-file *assets-package-dir*)

      (with-directory 
        *assets-package-dir* 
        (unless (zerop (uiop:run-program '("bash" "get.sh") :output nil))
          (error "Error occured during installing assets package \"~A\" please execute it manually~%cd ~A~%bash get.sh" url-or-file *assets-package-dir*)))

      (copy-serve-file url-or-file *assets-package-dir*)

      (format t "Successfully installed \"~A\"~%" url-or-file))

    (unless (cl-fad:file-exists-p (merge-pathnames (make-pathname :name "serve" :type "lisp") *assets-package-dir*))
      (restart-case 
        (error "It seems like you've installed package \"~A\" manually. If no, remove directory \"~A\" and restart " url-or-file *assets-package-dir*)
        (yes () :report "Yes, I've installed package manually, installation is ok" 
             (format t "Just copying serve.lisp file and move on")
             (copy-serve-file url-or-file *assets-package-dir*))
        (removed-directory () 
                           :report "I've removed directory, let's try installation again"
                           (return-from require-assets 
                                        (require-assets url-or-file :webapp webapp)))))

    (load-serve-file webapp (merge-pathnames (make-pathname :name "serve" :type "lisp") *assets-package-dir*))))

(defun explode (delimiter string)
  (ppcre:split 
    (ppcre:quote-meta-chars delimiter)
    string))

(defun serve-directory (url-or-dir &optional dir)
  (declare (special *assets-package-dir*))

  (let* ((request-path (format nil "~A/" (prepend-webapp-path url-or-dir)))
         (directory (merge-pathnames 
                      (make-pathname :directory (list* :relative (explode "/" (string-trim "/" (or dir url-or-dir)))))
                      *assets-package-dir*))
         (path-and-dir  (cons request-path directory)))

    (unless (gethash path-and-dir *dispatchers-cache*)
      (setf (gethash path-and-dir *dispatchers-cache*) t)
      (push 
        (weblocks:create-folder-dispatcher-and-handler 
          request-path
          directory)
        weblocks::*dispatch-table*))))

(defun serve-file (file &optional url)
  (let* ((request-path (prepend-webapp-path (or url file)))
         (path (merge-pathnames 
                 (parse-namestring file)
                 *assets-package-dir*))
         (path-and-file  (cons request-path path)))

    (unless (gethash path-and-file *dispatchers-cache*)
      (setf (gethash path-and-file *dispatchers-cache*) t)
      (push 
        (weblocks:create-static-file-dispatcher-and-handler 
          request-path
          path)
        weblocks::*dispatch-table*))))

(defun load-serve-file (app file)
  (let ((*package* (find-package :weblocks-utils)))
    (with-webapp app
                 (load file))))
