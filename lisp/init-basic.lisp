(in-package :stumpwm)

;; set default prefix key
(set-prefix-key (kbd "s-z"))

;; quick reload config file
(define-key *root-map* (kbd "F5") "loadrc")
(define-key *root-map* (kbd "C-F5") "reload")

(setf *startup-message* nil
      *suppress-frame-indicator* t
      *suppress-abort-messages* t
      *timeout-wait* 1
      *mouse-focus-policy* :click
      *message-window-gravity* :center
      *input-window-gravity* :center)

;; change cursor shape
(run-shell-command "xsetroot -cursor_name left_ptr")


(map-keys *top-map* '(("s-:" "eval")
                      ("s-;" "colon")
                      ("s-r" "run-shell-command")))

(map-keys *root-map* '(("q" "abort")
                       ("1" "only")))


;; launch auto-start apps
(defun launch-autostarts ()
  "launch auto-start apps."
  (when (boundp '*autostarts*)
    (dolist (cmd *autostarts*) (run-shell-command cmd))))

(add-hook *start-hook* 'launch-autostarts)
