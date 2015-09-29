(in-package :stumpwm)

;; Volume Control
(add-to-load-path (concat *config-directory*  "modules/stumpwm-contrib/media/amixer"))
(load-module "amixer")

(map-keys *top-map* '(("XF86AudioLowerVolume"    "amixer-Master-1-")
                      ("XF86AudioRaiseVolume"    "amixer-Master-1+")
                      ("XF86AudioMute"           "amixer-Master-toggle")
                      ("s-XF86AudioLowerVolume"  "amixer-Master-toggle")
                      ("s-XF86AudioRaiseVolume"  "amixer-Master-toggle")
                      ("C-XF86AudioLowerVolume"  "amixer-Headphone-1-")
                      ("C-XF86AudioRaiseVolume"  "amixer-Headphone-1+")
                      ("C-XF86AudioMute"         "amixer-Headphone-toggle")))


;; use truetype fonts
(add-to-load-path (concat *config-directory*  "modules/stumpwm-contrib/util/ttf-fonts"))
(load-module "ttf-fonts")
(ignore-errors
  (or (stumpwm::set-font (make-instance 'xft:font :family "WenQuanYi Micro Hei" :subfamily "Regular" :size 13))
      (set-font "-misc-wenquanyi micro hei-medium-r-normal--0-0-0-0-p-0-iso10646-1")))


;; launch dbus
(run-shell-command "type -a dbus-update-activation-environment &>/dev/null && dbus-update-activation-environment --all")
