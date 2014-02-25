(in-package :weblocks-utils)

(ps:defpsmacro ps-with-scripts ((&rest scripts) &body body)
               "Wrapper around jQuery-seq withScripts,
                fixes scripts paths with `prepend-webapp-path` and calls withScripts"
  (let ((scripts 
          (loop for i in scripts collect 
                (if (ignore-errors (current-webapp))
                  (weblocks-utils:prepend-webapp-path i)
                  i))))
    `(with-scripts 
       ,@scripts
       (lambda ()
         ,@body))))

(ps:defpsmacro ps-with-styles ((&rest styles)
                               &body body)
   "Wrapper around jQuery-seq withStyles,
    fixes styles paths with `prepend-webapp-path` and calls withStyles"
  (let ((styles 
          (loop for i in styles collect 
                (if (ignore-errors (current-webapp))
                  (weblocks-utils:prepend-webapp-path i)
                  i))))
    `(with-styles 
       ,@styles
       (lambda ()
         ,@body))))

(ps:defpsmacro ps-with-scripts-and-styles 
               ((&rest scripts) (&rest styles) &body body)
   "Wrapper around jQuery-seq withStyles and withScripts, 
    fixes scripts paths and styles paths with `prepend-webapp-path` 
    and calls withScripts first and withStyles inside of withScripts call"
               `(ps-with-scripts ,scripts 
                                 ,(if styles 
                                    `(ps-with-styles ,styles
                                                     ,@body)
                                    body)))
