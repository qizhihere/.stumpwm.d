(in-package :stumpwm)

(defun launch-autostarts ()
  "launch autostart apps."
  (when (boundp '*autostarts*)
    (dolist (cmd *autostarts*)
      (run-shell-command cmd))))

(defun add-autostart (command)
  "Add autostart app."
  (if (listp command)
      (dolist (x command) (add-autostart x))
      (add-to-list '*autostarts* command)))

(add-hook *start-hook* 'launch-autostarts)

;;----------------------------------------------------------------------------
;; applications which will be launch after StumpWM startup
;;----------------------------------------------------------------------------
(add-autostart '("compton -b"
                 "~/scripts/random-wallpaper.sh"
                 "~/scripts/i3lock.sh"))


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
                      ("s-F3"     "run-shell-command xfce4-popup-whiskermenu")))

(map-keys *launch-app-map* '(("c"        "goto-app chromium Chromium")
                             ("e"        "goto-app emacs Emacs")
                             ("s"        "goto-app sublime_text_3_imfix Sublime")
                             ("t"        "goto-app termite Termite")
                             ("T"        "run-shell-command termite")))
