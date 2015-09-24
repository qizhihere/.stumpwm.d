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

(defun other-groups (&optional group screen)
  "Get other groups in screen(or given screen) except given(or
current) group."
  (let ((current (or group (current-group))))
    (remove-if (lambda (x) (eql x current))
               (sort-these-groups (screen-groups (or screen (current-screen)))))))

(defun kill-other-groups (&optional group screen)
  "Kill all other groups."
  (mapcar #'kill-group (other-groups group screen)))

(defun sort-these-groups (groups &optional (sort-fn #'<) &key (key #'group-number))
  "Sort given groups."
  (apply 'sort groups sort-fn `(:key ,key)))

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

(defun next-visible-window (&optional group exclude-rules)
  "Find next visible window in current group."
  (let* ((group (or group (current-group)))
         (wins (group-windows group)))
    (find t wins :start (if (group-current-window group) 1 0)
          :test (lambda (a b) (and (not (window-hidden-p b))
                               (if exclude-rules (not (apply #'window-matches-properties-p
                                                        b exclude-rules))
                                   t))))))

(defun unfocus (window &optional exclude-rules)
  "Focus next visible window."
  (let ((group (window-group window)))
    (no-focus group (next-visible-window group exclude-rules))))

(defun window-maximized-p (window)
  "Check if given window is currently maximized."
  (let* ((screen (current-screen)))
    (and (= (window-width window) (screen-width screen))
         (= (window-height window) (- (screen-height screen)
                                      (topbar-height))))))

(defun move-window-to-corner (direction &optional (window (current-window))
                                          (screen (current-screen)))
  (let (x y w h)
    (dolist (direct (split-string direction " +"))
      (cond
        ((string= direct "top") (setf y (topbar-height)))
        ((string= direct "bottom") (setf y (- (screen-height screen)
                                              (window-height window))))
        ((string= direct "left") (setf x 0))
        ((string= direct "right") (setf x (- (screen-width screen)
                                             (window-width window))))))
    (stumpwm.floating-group::float-window-move-resize window :x x :y y)))

(defun add-autostart (command)
  "Add autostart app."
  (if (listp command)
      (dolist (x command) (add-autostart x))
      (add-to-list '*autostarts* command)))

;; handle window with corresponding handler if it matches a rule
(defvar *window-handlers* '())
(add-hook *new-window-hook* 'handle-window)

(defun handle-window (window)
  (let ((handlers (remove-if-not (lambda (it) (apply #'window-matches-properties-p window (car it)))
                                 *window-handlers*)))
    (dolist (it handlers)
      (and (cdr it)
           (funcall (cdr it) window (car it))))))

(defun add-window-handler (rule handler)
  "Append a new window handler to `*window-handlers*'."
  (add-to-list '*window-handlers* (cons rule handler) t))
