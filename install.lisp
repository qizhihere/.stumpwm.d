(use-package 'common-lisp)

(defvar *user-home* (user-homedir-pathname))
(defvar *quicklisp-home* (merge-pathnames *user-home* ".quicklisp"))

;; install quicklisp
(quicklisp-quickstart:install :path (namestring *quicklisp-home*))

;; don't prompt
(setf ql-util::*do-not-prompt* t)
(ql:add-to-init-file)

;; prerequisites
(ql:quickload "clx")
(ql:quickload "cl-ppcre")

(ql:quickload :swank)

;; add ttf fonts support
(ql:quickload :clx-truetype)
(xft:cache-fonts)
