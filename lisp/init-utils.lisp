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

(print "hello!")
