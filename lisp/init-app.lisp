(in-package :stumpwm)

;;----------------------------------------------------------------------------
;; quickly launch apps
;;----------------------------------------------------------------------------
(defcommand goto-app (command class)
  ((:string "Command to run: ")
   (:string "Window Class to match: "))
  (run-or-raise command `(:class ,class)))

(defcommand scrot (dir ext flags)
  ((:string "save directory: ")
   (:string "image extension: ")
   (:rest "scrot flags: "))
  (ensure-directories-exist dir)
  (let* ((dir (string-right-trim "/" dir))
         (fpath-gen (lambda () (concat dir "/" (random-string) ext)))
         (post-commands '("notify-send -i dialog-information -a scrot -t 5000 截图已保存至 \$f"
                          "echo \$f | xsel -bi"))
         (flags (concat " " flags " -e '" (reduce (lambda (a b) (concat a ";" b)) post-commands) "'"))
         (fpath (funcall fpath-gen)))
    (loop for i from 1 to 100
          until (not (probe-file fpath))
          do
          (setf fpath (funcall fpath-gen)))
    (run-shell-command (concat "scrot " fpath flags))))

(defcommand scrot-to-default-directory (flags)
  ((:rest "scrot flags: "))
  (run-commands (concat "scrot ~/Pictures/screenshots .png " flags)))

(defcommand scrot-to-blog-directory (flags)
  ((:rest "scrot flags: "))
  (run-commands (concat "scrot ~/blog/img .png " flags)))

(defun random-string (&optional (length 10))
  "Random string generator, default length is 10."
  (run-shell-command (concat "tr -dc a-zA-Z0-9 < /dev/urandom | head -c" (write-to-string length)) t))

;; prefix key
(defvar *launch-app-map* (make-sparse-keymap))
(map-keys *root-map* '(("c" "goto-app chromium Chromium")
                       ("C"        "run-shell-command chromium")
                       ("Print"    "scrot-to-blog-directory \"\"") ;; empty placeholder
                       ("C-Print"  "scrot-to-blog-directory -u")
                       ("Sys_Req"  "scrot-to-blog-directory -s")
                       ("M-Print"  "scrot-to-blog-directory -cd 5")
                       ("g" *launch-app-map*)))

(map-keys *top-map* '(("s-l"      "run-shell-command ~/scripts/i3lock.sh")
                      ("s-'"      "run-shell-command ~/scripts/random-wallpaper.sh")
                      ("s-e"      "run-shell-command thunar")
                      ("s-RET"    "goto-app termite Termite")
                      ("C-s-RET"  "run-shell-command termite")
                      ("s-F1"     "run-shell-command xfce4-popup-whiskermenu")
                      ("s-F3"     "run-shell-command xfce4-popup-whiskermenu")
                      ("Print"    "scrot-to-default-directory \"\"") ;; empty placeholder
                      ("C-Print"  "scrot-to-default-directory -u")
                      ("Sys_Req"  "scrot-to-default-directory -s")
                      ("M-Print"  "scrot-to-default-directory -cd 5")
                      ("C-M-S"  "run-shell-command gpick -p")))


(map-keys *launch-app-map* '(("c"        "goto-app chromium Chromium")
                             ("e"        "goto-app \"env LC_CTYPE=zh_CN.UTF-8\" Emacs")
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
