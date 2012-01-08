(require 'gray-streams)
(in-package :cl-user)

(defvar *acl-compat-directory* "d:/projects/lisp/portableaserve/acl-compat/")
(load (concatenate 'string *acl-compat-directory* "nregex.lisp"))
(load (concatenate 'string *acl-compat-directory* "meta.lisp"))
(load (concatenate 'string *acl-compat-directory* "acl-excl-corman.lisp"))
(load (concatenate 'string *acl-compat-directory* "acl-mp-corman.lisp"))
(load (concatenate 'string *acl-compat-directory* "acl-socket-corman.lisp"))
(load (concatenate 'string *acl-compat-directory* "uri.lisp"))
(load (concatenate 'string *acl-compat-directory* "packages.lisp"))

(pushnew :acl-compat *features*)