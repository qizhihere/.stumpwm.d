(in-package :stumpwm)

;;----------------------------------------------------------------------------
;; quickly launch apps
;;----------------------------------------------------------------------------
(defcommand goto-app (command class)
  ((:string "Command to run: ")
   (:string "Window Class to match: "))
  (run-or-raise command `(:class ,class)))

;; prefix key
(defvar *launch-app-map* (make-sparse-keymap))
(map-keys *root-map* '(("c" "goto-app chromium Chromium")
                       ("g" *launch-app-map*)))

(map-keys *top-map* '(("s-l"      "run-shell-command ~/scripts/i3lock.sh")
                      ("s-'"      "run-shell-command ~/scripts/random-wallpaper.sh")
                      ("s-e"      "run-shell-command thunar")
                      ("s-RET"    "goto-app termite Termite")
                      ("C-s-RET"  "run-shell-command termite")
                      ("s-F1"     "run-shell-command xfce4-popup-whiskermenu")
                      ("s-F3"     "run-shell-command xfce4-popup-whiskermenu")
                      ("Print"    "run-shell-command killall -9 shutter;shutter -f")
                      ("C-Print"    "run-shell-command killall -9 shutter;shutter -w")
                      ("Sys_Req"    "run-shell-command killall -9 shutter;shutter -s &")))

(map-keys *launch-app-map* '(("c"        "goto-app chromium Chromium")
                             ("e"        "goto-app emacs Emacs")
                             ("s"        "goto-app sublime_text_3_imfix Sublime")
                             ("t"        "goto-app termite Termite")
                             ("T"        "run-shell-command termite")))


;; applications to be launched after StumpWM startup
(add-autostart '("compton -b"
                 "~/scripts/random-wallpaper.sh"
                 "~/scripts/i3lock.sh"))


(defun launch-my-apps ()
  (setf init-group-apps '((0 . ("goto-app emacs Emacs"))
                          (1 . ("goto-app termite Emacs"))
                          (2 . ("goto-app chromium Chromium"))
                          (4 . ("goto-app thunar Thunar"))))

  (dolist (it init-group-apps)
    (with-group (nth-group (car it))
      (mapcar (lambda (x) (run-commands x)) (cdr it))))

  ;; return to the first group
  (switch-to-group (nth-group 0)))
(add-hook *start-hook* 'launch-my-apps)
