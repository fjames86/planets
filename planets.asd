

(in-package :asdf)

(defsystem #:planets
  :name "PLANETS"
  :author "Frank James <frank.a.james@gmail.com>"
  :version "1"
  :maintainer "Frank James <frank.a.james@gmail.com>"
  :licence "Lisp Lesser General Public License (LLGPL)"
  :description "Interstellar planetary trading game"
  :long-description "An interstellar planetary trading game"

  :components
  ((:module
    :src 
    :components ((:file "package")
				 (:file "utils" :depends-on ("package"))
				 (:file "planets" :depends-on ("utils")))))
  :depends-on (:lispbuilder-sdl :bordeaux-threads))



	




