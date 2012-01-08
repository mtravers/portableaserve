;;;; AllegroServe loader for Corman Lisp - Version 1.0
;;;;
;;;; Copyright (C) 2000 Christopher Double. All Rights Reserved.
;;;; 
;;;; License
;;;; =======
;;;; This software is provided 'as-is', without any express or implied
;;;; warranty. In no event will the author be held liable for any damages
;;;; arising from the use of this software.
;;;;
;;;; Permission is granted to anyone to use this software for any purpose,
;;;; including commercial applications, and to alter it and redistribute
;;;; it freely, subject to the following restrictions:
;;;;
;;;; 1. The origin of this software must not be misrepresented; you must
;;;;    not claim that you wrote the original software. If you use this
;;;;    software in a product, an acknowledgment in the product documentation
;;;;    would be appreciated but is not required.
;;;;
;;;; 2. Altered source versions must be plainly marked as such, and must
;;;;    not be misrepresented as being the original software.
;;;;
;;;; 3. This notice may not be removed or altered from any source 
;;;;    distribution.
;;;;
;;;; More recent versions of this software may be available at:
;;;;   http://www.double.co.nz/cl
;;;;
;;;; Comments, suggestions and bug reports to the author, 
;;;; Christopher Double, at: chris@double.co.nz
;;;;
;;;; 03/03/2000 - 1.0 
;;;;              Initial release.
;;;;              Change the *as-source-directory* constant to
;;;;              point to the installation directory of 
;;;;              the AllegroServe install files.
;;;;              
;;;;
(in-package :cl-user)

(defconstant *as-source-directory* "d:/projects/lisp/portableaserve/")
(load (concatenate 'string *as-source-directory* "acl-compat/acl-compat-corman.lisp"))

(defconstant *as-files* 
	(list 
		"aserve/htmlgen/htmlgen.cl"
        "aserve/packages.cl"
		"aserve/macs.cl"
		"aserve/main.cl"
		"aserve/headers.cl"
        "aserve/parse.cl"
		"aserve/decode.cl"
		"aserve/publish.cl"
		"aserve/authorize.cl"
		"aserve/log.cl"
		"aserve/client.cl"
        "aserve/proxy.cl"))

(loop for file in *as-files* do (load (concatenate 'string *as-source-directory* file)))

(provide 'allegroserve)