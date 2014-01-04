(defun find-person (prison person)
  (loop :for i :from 0 :below (array-dimension prison 0) :do
     (loop for j :from 0 :below (array-dimension prison 1)
        :when (eql (aref prison i j) person) :do
          (return-from find-person (cons i j)))))

(defun move-robot (robot princess)
  (destructuring-bind ((rx . ry) (px . py)) (list robot princess)
    (cond
      ((< rx px) (values (cons (1+ rx) ry) 'left))
      ((< ry py) (values (cons rx (1+ ry)) 'up))
      ((> rx px) (values (cons (1- rx) ry) 'right))
      ((> ry py) (values (cons rx (1- ry)) 'down)))))

(defun find-path (prison)
  (loop
     :with princess := (find-person prison #\p)
     :and robot := (find-person prison #\m) :and dir := nil
     :do (multiple-value-bind (new-robot direction)
             (move-robot robot princess)
           (setf robot new-robot dir direction))
      :collect dir :until (equal robot princess)))

(defun read-table ()
  (let ((len (parse-integer (read-line))))
    (format
     t "~{~A~%~}"
     (find-path (make-array
                 (make-list 2 :initial-element len)
                 :initial-contents
                 (loop :for line := (read-line *standard-input* nil)
                    :and i :upfrom 1
                    :while (and line (not (zerop (length line))))
                    :collect (coerce line 'list)
                    :until (= i len)))))))

(read-table)
