#!/usr/bin/sbcl --script

(read-line)

(loop
   :with test-table := (make-hash-table)
   :for int :in
   (loop :for char :across (read-line)
      :with token := nil :and result := nil :do
      (case char
        (#\Space
         (push (parse-integer (coerce token 'string)) result)
         (setf token nil))
        (otherwise (push char token)))
      :finally
      (progn
        (when token (push (parse-integer (coerce token 'string)) result))
        (return result)))
   :do (setf (gethash int test-table) (1+ (gethash int test-table 0)))
   :finally
   (format t "~d~&"
    (loop :for key :being :the :hash-key :in test-table
       :using (:hash-value value)
       :until (= value 1)
       :finally (return key))))
