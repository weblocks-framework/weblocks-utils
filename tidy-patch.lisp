(in-package :cl-tidy)
(export '(clean-up-and-indent-html-part))

(defcfun ("tidyOptParseValue" tidy-opt-parse-value) boolean
  (tdoc tidy-doc)
  (opt :string)
  (val :string))

(defun clean-up-and-indent-html-part (string)
  "Indents html part."
  (declare (optimize debug))
  (without-interrupts
    (with-tidy-doc (doc)
                   (with-tidy-buffer (buf)
                                     (tidy-opt-set-bool doc :tidyshowwarnings :no)
                                     (tidy-opt-set-bool doc :xhtml-out :yes)
                                     (tidy-opt-parse-value doc "indent" "yes")
                                     (tidy-opt-parse-value doc "input-xml" "yes")

                                     (tidy-opt-set-bool doc :tidy-num-entities :yes)

                                     (tidy-opt-parse-value doc "show-body-only" "yes")
                                     (tidy-opt-parse-value doc "preserve-entities" "yes")
                                     (tidy-opt-parse-value doc "char-encoding" "raw")

                                     (tidy-parse-string doc string)
                                     (tidy-clean-and-repair doc)
                                     (tidy-save-buffer doc buf)
                                     (convert-from-foreign (foreign-slot-value buf 'tidy-buffer 'bp) :string)))))


