;;;; kbd-layouts.lisp

(in-package #:kbd-layouts)

;;;;;;;;;;;;;;;
;; Variables ;;
;;;;;;;;;;;;;;;

(defvar *available-keyboard-layouts* nil)

;; Available options:
;; :normal -> CapsLock
;; :ctrl   -> Ctrl
;; :swapped-> Swap Ctrl and CapsLock
(defvar *caps-lock-behavior* nil)

;;;;;;;;;;;;;;;;;;;;;;
;; Helper functions ;;
;;;;;;;;;;;;;;;;;;;;;;

(defun keyboard-layout-list (&rest layouts)
  "Return a circular list of keyboard layouts (as given to
   setxkbmap). The first layout in the list will be the default."
  (rplacd (last layouts) layouts)
  (setf *available-keyboard-layouts* layouts)
  ;; Immediately select first layout from the list
  (switch-keyboard-layout))

(defun current-keyboard-layout (ml)
  "Return current keyboard layout"
  (declare (ignore ml))
  (let ((cmd "setxkbmap -print | awk -F'+' '/xkb_symbols/ {print $2}'"))
    (string-trim '(#\Newline) (run-shell-command cmd t))))

;; Make the current keyboard layout accessible from the modeline via %L.
(add-screen-mode-line-formatter #\L #'current-keyboard-layout)

;;;;;;;;;;;;;;
;; Commands ;;
;;;;;;;;;;;;;;

(defcommand switch-keyboard-layout () ()
  (let* ((layout (pop *available-keyboard-layouts*))
         (caps (ecase
                   *caps-lock-behavior*
                 (:normal "caps:capslock")
                 (:ctrl "ctrl:nocaps")
                 (:swapped "ctrl:swapcaps")))
         (cmd (format nil "setxkbmap ~a -option ~a" layout caps)))
    (run-shell-command cmd t)
    (message (format nil "Keyboard layout switched to: ~a" layout))))

;;;;;;;;;;;;;;
;; Defaults ;;
;;;;;;;;;;;;;;

(setf *caps-lock-behavior* :normal)

(keyboard-layout-list "us")

(define-key *top-map* (kbd "s-SPC") "switch-keyboard-layout")
