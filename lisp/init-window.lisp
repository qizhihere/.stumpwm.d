;;----------------------------------------------------------------------------
;; basic settings
;;----------------------------------------------------------------------------
;; set window display style
(setf *window-format* "%m%n. %60t"
      *maxsize-border-width* 0
      *transient-border-width* 0
      *normal-border-width* 0
      *window-border-style* :none
      *float-window-title-height* 0
      stumpwm.floating-group::*float-window-title-height* 0
      stumpwm.floating-group::*float-window-border* 0)

(set-bg-color "#2b303b")
(set-fg-color "#89ebca")
(set-border-color "#4f5b66")
(set-float-focus-color "#333d46")
(set-float-unfocus-color "#4f5b66")


;; mode line
(setf *screen-mode-line-format* "[%g]  %v  %20d"
      *time-modeline-string* "%Y/%m/%d ^B%H:%M^b"
      *mode-line-border-width* 0
      *mode-line-pad-x* 2
      *mode-line-pad-y* 0)

(define-key *top-map* (kbd "s-b") "mode-line")


;;----------------------------------------------------------------------------
;; window control
;;----------------------------------------------------------------------------
(defun global-window-list ()
  "Returns a list of the names of all the windows in the current screen."
  (let ((groups (sort-groups (current-screen)))
        (windows nil))
    (dolist (group groups)
      (dolist (window (group-windows group))
        (setf windows (append windows (list window)))))
    windows))

(defun global-switch-to-window (window)
  (let ((group (window-group window)))
    (switch-to-group group)
    (group-focus-window group window)))

(defcommand global-windowlist (&optional (fmt *window-format*)
                                         window-list) (:rest)
  "Select window from global window list."
  (let ((window-list (or window-list (global-window-list))))
    (if (null window-list)
        (message "No Managed Windows")
        (let ((window (select-window-from-menu window-list fmt)))
          (if window
              (global-switch-to-window window)
              (throw 'error :abort))))))

(defcommand generic-window-fullscreen () ()
  "Toggle tile/float window fullscreen."
  (fullscreen))

(defcommand hide-this-window () ()
  "Hide current window."
  (hide-window (current-window)))

(map-keys *top-map* '(("s-w"    "global-windowlist")
                      ("s-g"    "grouplist")
                      ("s-TAB"  "other")
                      ("s-C-BackSpace"  "delete")
                      ("F11"    "generic-window-fullscreen")
                      ;; minimize/maximize
                      ("s-n"    "hide-this-window")
                      ("s-N"    "pull-hidden-next")
                      ;; move between groups
                      ("C-s-Left"   "gprev-with-window")
                      ("C-s-Right"  "gnext-with-window")))

(map-keys *root-map* '(("w"    "global-windowlist")
                       ;; emacs style window shortcuts
                       ("1"    "only")
                       ("2"    "vsplit")
                       ("3"    "hsplit")
                       ("4"    "kill")
                       ("0"    "delete")
                       ("I"    "list-window-properties")
                       ("s-z"  "other")))

;; position control
(map-keys *tile-group-top-map*
          '(;; position exchange
            ("s-S-Up"       "exchange-direction up")
            ("s-S-Down"     "exchange-direction down")
            ("s-S-Left"     "exchange-direction left")
            ("s-S-Right"    "exchange-direction right")
            ;; frame control
            ("s-BackSpace"  "remove")))


;;----------------------------------------------------------------------------
;; group control
;;----------------------------------------------------------------------------
(defcommand move-this-window-to-group-n (group-number)
  ((:number "Please input the target group number: "))
  "Move current window to nth group."
  (let ((win (current-window)))
    (dolist (group (screen-groups (current-screen)))
      (when (= (group-number group) group-number)
        (return (progn (move-window-to-group win group)
                       (switch-to-group group)
                       (raise-window win)))))))

(map-keys *top-map* '(;; switch group
                      ("s-1"      "gselect 1")
                      ("s-2"      "gselect 2")
                      ("s-3"      "gselect 3")
                      ("s-4"      "gselect 4")
                      ("s-5"      "gselect 5")
                      ("s-6"      "gselect 6")
                      ("s-7"      "gselect 7")
                      ("s-8"      "gselect 8")
                      ("s-9"      "gselect 9")
                      ("s-Right"  "gnext")
                      ("s-Left"   "gprev")
                      ("s-C-Up"   "gnew-float")
                      ("s-Up"     "gnew")
                      ("s-Down"   "gkill")
                      ;; move window to group
                      ("s-!"      "move-this-window-to-group-n 1")
                      ("s-@"      "move-this-window-to-group-n 2")
                      ("s-#"      "move-this-window-to-group-n 3")
                      ("s-$"      "move-this-window-to-group-n 4")
                      ("s-%"      "move-this-window-to-group-n 5")
                      ("s-^"      "move-this-window-to-group-n 6")
                      ("s-&"      "move-this-window-to-group-n 7")
                      ("s-*"      "move-this-window-to-group-n 8")
                      ("s-("      "move-this-window-to-group-n 9")))

;; kill all groups
(dolist (group (screen-groups (current-screen)))
  (let ((first-group (current-group)))
   (unless (eql group first-group)
    (kill-group group first-group))))

;; custom groups
(setf *group-format* "%t")
(grename       "1. Emacs")
(gnewbg        "2. Term")
(gnewbg-float  "3. Net")
(gnewbg-float  "4. Docs")
(gnewbg-float  "5. Media")
