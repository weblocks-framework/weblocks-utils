Weblocks-utils - utilities for weblocks.
========================================

Contains useful utilities, mostly to work with database (inspired by rails ActiveRecord methods).

Used as any package, I prefer to put it into :use part of parent packages. Documentation is generated for example by 

```
(atdoc:generate-html-documentation 
  '(:weblocks-utils) "doc/" 
     :single-page-p t 
     :index-title "Weblocks utils" 
     :heading "Weblocks utils")
```

after requiring weblocks-utils and atdoc of course.
