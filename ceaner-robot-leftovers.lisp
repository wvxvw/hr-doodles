(defun matrix-blit (x0 y0 x1 y1 matrix)
  ;; (format t "~&x0: ~d, x1: ~d, y0: ~d, y1: ~d" x0 x1 y0 y1)
  (iter
    (with result := (make-array (list (- y1 y0) (- x1 x0))))
    (for i :from x0 :below x1)
    (iter (for j :from y0 :below y1)
          (setf (aref result (- j y0) (- i x0)) (aref matrix j i)))
    (finally (return result))))

(defun split-in-four (board x y)
  (values (matrix-blit 0 0 5 y board) ; top
          (matrix-blit (1+ x) 0 5 5 board)   ; right
          (matrix-blit 0 (1+ y) 5 5 board)   ; bottom
          (matrix-blit 0 0 x 5 board)))

(defun count-dirt (board)
  (let ((dirt
         (iter (for y :from 0 :below (array-dimension board 0))
               (summing
                (iter (for x :from 0 :below (array-dimension board 1))
                      (summing (if (eql (aref board y x) #\d) 1 0)))))))
    (cond
      ((or (null dirt) (zerop dirt)) 0)
      (t (/ dirt (apply #'* (array-dimensions board)))))))

(defun decide (board previous-state)
  (if (eql previous-state #\d)
      'clean
      (multiple-value-bind (rx ry) (find-robot board)
        (multiple-value-bind (up right down left)
            (split-in-four board rx ry)
          ;; (format t "~&whole:~%~/pprint-board/~% ~
          ;;             up:~%~/pprint-board/~% ~
          ;;             right:~%~/pprint-board/~% ~
          ;;             down:~%~/pprint-board/~% ~
          ;;             left:~%~/pprint-board/"
          ;;         board up right down left)
          ;; (break)
          (destructuring-bind (up right down left)
              (mapcar #'count-dirt (list up right down left))
            (let ((best (max up right down left)))
              (cond
                ((= best up) 'up)
                ((= best right) 'right)
                ((= best down) 'down)
                ((= best left) 'left))))))))

(defun move-robot (board rx ry directive)
  (setf (aref board ry rx) #\-)
  (multiple-value-bind (nx ny)
      (case directive
        (up (values rx (1- ry)))
        (right (values (1+ rx) ry))
        (down (values rx (1+ ry)))
        (left (values (1- rx) ry))
        (clean (values rx ry)))
    (let ((result (aref board ny nx)))
      ;; (format t "~&=====~%~/pprint-board/~%result: ~s"
      ;;             board result)
      ;; (break)
      (setf (aref board ny nx) #\b) result)))

(defun test-robot (board)
  (format t "~&initial:~%~/pprint-board/" board)
  (iter (until (board-clean-p board))
        (with previous-state := #\-)
        (for directive := (decide board previous-state))
        (generate steps :upfrom 0)
        (generate cleans :upfrom 0)
        (if (eql directive 'clean) (next cleans) (next steps))
        (multiple-value-bind (rx ry) (find-robot board)
          (setf previous-state (move-robot board rx ry directive))
          (format t "~&action: ~s~%~/pprint-board/~%previous: ~s, steps: ~d, cleans: ~d"
                  directive board previous-state steps cleans))))

(defun test-cost-function ()
  (let ((board (random-board)))
    (multiple-value-bind (rx ry) (find-robot board)
      (format t "~&~/pprint-board/~%robot(x: ~d, y: ~d)" board rx ry)
      (cost-function rx ry board))))

(defun test-best-move ()
  (let ((board (random-board)))
    (format t "~&initial~%~/pprint-board/" board)
    (multiple-value-bind (rx ry) (find-robot board)
      (multiple-value-bind (move nboard) (best-move rx ry board)
        (format t "~&~/pprint-board/~%move: ~s" nboard move)))))
