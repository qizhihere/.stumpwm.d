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

(map-keys *top-map* '(("s-:" "eval")
                      ("s-;" "colon")
                      ("s-r" "run-shell-command")))

(map-keys *root-map* '(("q" "abort")
                       ("1" "only")))
