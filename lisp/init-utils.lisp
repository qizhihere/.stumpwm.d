(in-package :stumpwm)

(defmacro map-keys (map keys)
  "Map a list of keys."
  `(dolist (pair ,keys)
    (define-key ,map (kbd (car pair)) (cadr pair))))

(defun add-to-list (list-var element &optional append)
  "Add an element to a list if it's not contained.
By default, the new element will be pushed to the beginning of the
list. If `APPEND' is non-nil, then it will be appended to the list."
  (when (not (boundp list-var))
    (eval `(defvar ,list-var nil)))
  (let ((val (symbol-value list-var)))
    (unless (member element val)
      (set list-var (if append (append val (list element))
                        (cons element val))))))

(defun nth-group (n &optional screen)
  (nth n (sort-groups (or screen (current-screen)))))

(defmacro with-group (group &rest body)
  "Select given gorup and execute `BODY'.
If `BODY' is nil, do nothing."
  `(when ,group
     (switch-to-group ,group)
     ,@body))

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

(defun next-visible-window (&optional group)
  "Find next visible window in current group."
  (let* ((group (or group (current-group)))
         (wins (group-windows group)))
    (find t wins :start (if (group-current-window group) 1 0)
          :test (lambda (a b) (not (window-hidden-p b))))))

(defun window-maximized-p (window)
  "Check if given window is currently maximized."
  (let* ((screen (current-screen)))
    (and (= (window-width window) (screen-width screen))
         (= (window-height window) (- (screen-height screen)
                                      (topbar-height))))))

(defun add-autostart (command)
  "Add autostart app."
  (if (listp command)
      (dolist (x command) (add-autostart x))
      (add-to-list '*autostarts* command)))
