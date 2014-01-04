(load "~/.sbclrc")
(ql:quickload :iterate)
(use-package :iterate)

(defun find-robot (board)
  (iter (for y :from 0 :below 5)
        (iter (for x :from 0 :below 5)
              (when (eql (aref board y x) #\b)
                (return-from find-robot (values x y))))))

(defun best-move (rx ry board cleanp)
  (if cleanp (values 'clean board nil)
      (iter
        (with tscore := 25)
        (with mscore := 8)
        (with btscore := tscore)
        (with bmscore := mscore)
        (with bmove := nil)
        (with bboard := nil)
        (with ncleanp := nil)
        (for (xmod ymod dir) :in
             '((identity 1- up) (identity 1+ down)
               (1+ identity right) (1- identity left)))
        (for test-board := (copy-board board))
        (for nx := (funcall xmod rx))
        (for ny := (funcall ymod ry))
        (when (and (< -1 nx 5) (< -1 ny 5))
          (setf (aref test-board ry rx) #\-
                (aref test-board ny nx) #\b)
          (multiple-value-bind (ntscore nmscore)
              (cost-function nx ny test-board)
            (when (or (< ntscore btscore)
                      (and (= ntscore btscore)
                           (< nmscore bmscore)))
              (setf bmove dir bboard test-board
                    btscore ntscore bmscore nmscore
                    ncleanp (eql (aref board ny nx) #\d))
              ;; (format t "~&what if I go: ~s, tscore: ~f, mscore: ~f"
              ;;         dir bmscore btscore)
              ))
          (finally (return (values bmove bboard ncleanp)))))))

(defun read-board ()
  ;; robot's position, needless junk
  (read-line)
  (make-array '(5 5)
              :initial-contents
              (iter
                (for line := (read-line))
                (for i :from 0 :below 5)
                (collect (coerce line 'list)))))

(defun board-cost (board)
  (iter (for x :from 0 :below 5)
        (summing
         (iter (for y :from 0 :below 5)
               (summing (if (eql (aref board x y) #\d) 1 0))))))

(defun min-distance-to-dirt (rx ry board)
  (iter
    (for y :from 0 :below 5)
    (minimizing
     (or (iter
           (for x :from 0 :below 5)
           (when (eql (aref board y x) #\d)
             (minimizing (+ (abs (- rx x)) (abs (- ry y)))))) 8))))

(defun cost-function (rx ry board)
  (let ((bcost (board-cost board))
        (mcost (min-distance-to-dirt rx ry board)))
    ;; (format t "~&bcost: ~f, mcost: ~f" bcost mcost)
    (values bcost mcost)))

(defun copy-board (board)
  (iter (with result := (make-array '(5 5)))
        (for x :from 0 :below 5)
        (iter (for y :from 0 :below 5)
              (setf (aref result x y) (aref board x y)))
        (finally (return result))))

(defun board-clean-p (board)
  (not
   (iter (for x :from 0 :below (array-dimension board 0))
         (finding x :such-that
                  (iter (for y :from 0 :below (array-dimension board 0))
                        (for cell := (aref board x y))
                        (finding cell :such-that (eql cell #\d)))))))

(defun random-board ()
  (iter (with result := (make-array '(5 5)))
        (for x :from 0 :below 5)
        (iter (for y :from 0 :below 5)
              (setf (aref result x y)
                    (nth (random 2) '(#\- #\d))))
        (finally
         (progn
           (setf (aref result (random 5) (random 5)) #\b)
           (return result)))))

(defun pprint-board (stream board &rest more-args)
  (declare (ignore more-args))
  (iter (for x :from 0 :below (array-dimension board 0))
        (unless (first-time-p) (terpri stream))
        (iter (for y :from 0 :below (array-dimension board 1))
              (write-char (aref board x y) stream))))

(defun test-robot-heuristic ()
  (iter
    (with board := (random-board))
    (with cleanp := nil)
    (until (board-clean-p board))
    (for (values rx ry) := (find-robot board))
    (for (values move nboard ncleanp) := (best-move rx ry board cleanp))
    (setf cleanp ncleanp)
    (setf board nboard)
    ;; (format t "~&~/pprint-board/~%move: ~s" nboard move)
    (collect move)))

(defun main ()
  (let ((board (read-board)))
    (iter
      (with board := board)
      (with cleanp := nil)
      (until (board-clean-p board))
      (for (values rx ry) := (find-robot board))
      (for (values move nboard ncleanp) := (best-move rx ry board cleanp))
      (setf cleanp ncleanp)
      (setf board nboard)
      (format t "~s~%" move))))

(main)
