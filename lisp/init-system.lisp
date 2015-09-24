(in-package :stumpwm)

;; Volume Control
(add-to-load-path (concat *config-directory*  "modules/stumpwm-contrib/media/amixer"))
(load-module "amixer")

(map-keys *top-map* '(("XF86AudioLowerVolume"    "amixer-Master-1-")
                      ("XF86AudioRaiseVolume"    "amixer-Master-1+")
                      ("XF86AudioMute"           "amixer-Master-toggle")
                      ("C-XF86AudioLowerVolume"  "amixer-Headphone-1-")
                      ("C-XF86AudioRaiseVolume"  "amixer-Headphone-1+")
                      ("C-XF86AudioMute"         "amixer-Headphone-toggle")))
