(require :stumpwm)
(in-package :stumpwm)

;; stumpwm configuration directory
(defvar *config-directory* (directory-namestring *load-truename*))

(defun load-conf (filename)
  "Load a file FILENAME in ~/.stumpwm.d/lisp."
  (let ((file (merge-pathnames (concat "lisp/" filename ".lisp")
                               *config-directory*)))
    (if (probe-file file)
        (load file)
        (format *error-output* "File '~a' doesn't exist." file))))

(load-conf "init-utils")

(load-conf "init-swank")
(load-conf "init-basic")
(load-conf "init-system")
(load-conf "init-app")
(load-conf "init-window")



;;----------------------------------------------------------------------------
;; window control
;;----------------------------------------------------------------------------
(defun send-key-other-window (dir)
  (stumpwm::send-fake-key
   (stumpwm::frame-window (stumpwm::tile-group-last-frame (current-group)))
   (kbd dir)))

(defcommand scroll-other-window-down () ()
            "Scroll other window down."
            (send-key-other-window "Next"))
(defcommand scroll-other-window-up () ()
            "Scroll other window down."
            (send-key-other-window "Prior"))
