;;; ==================================================================
;;;
;;; This is a Common Lisp port from the original in C.
;;; This LISP version was written in 2020 by Julianne Mosca.
;;;
;;; Original copyright notice below:
;;;
;;; ==================================================================
;;;
;;;
;;; Copyright (c) 2004-2019, Lode Vandevenne
;;;
;;; All rights reserved.
;;;
;;; Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:
;;;
;;;     * Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.
;;;     * Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.
;;;
;;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;;; "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;;; LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;;; A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR
;;; CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
;;; EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
;;; PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
;;; PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
;;; LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;; NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
;;;
;;;  ==================================================================


(defconstant +screen-width+ 640)
(defconstant +screen-height+ 480)
(defconstant +map-width+ 24)
(defconstant +map-height+ 24)

(defvar *world-map* #2A(
                         (1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1)
                         (1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1)
                         (1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1)
                         (1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1)
                         (1 0 0 0 0 0 2 2 2 2 2 0 0 0 0 3 0 3 0 3 0 0 0 1)
                         (1 0 0 0 0 0 2 0 0 0 2 0 0 0 0 0 0 0 0 0 0 0 0 1)
                         (1 0 0 0 0 0 2 0 0 0 2 0 0 0 0 3 0 0 0 3 0 0 0 1)
                         (1 0 0 0 0 0 2 0 0 0 2 0 0 0 0 0 0 0 0 0 0 0 0 1)
                         (1 0 0 0 0 0 2 2 0 2 2 0 0 0 0 3 0 3 0 3 0 0 0 1)
                         (1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1)
                         (1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1)
                         (1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1)
                         (1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1) 
                         (1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1) 
                         (1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1) 
                         (1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1) 
                         (1 4 4 4 4 4 4 4 4 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1) 
                         (1 4 0 4 0 0 0 0 4 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1) 
                         (1 4 0 0 0 0 5 0 4 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1) 
                         (1 4 0 4 0 0 0 0 4 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1) 
                         (1 4 0 4 4 4 4 4 4 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1) 
                         (1 4 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1) 
                         (1 4 4 4 4 4 4 4 4 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1) 
                         (1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1)
                         ))

(defstruct rc-state-t
  pos-x pos-y       ;; x and y start position
  dir-x dir-y       ;; initial direction vector
  plane-x plane-y   ;; the 2d raycaster version of camera plane
  time              ;; time of current frame
  old-time          ;; time of previous frame
  )

(defparameter *rc-state*
  (make-rc-state-t
    :pos-x 22.0d0   :pos-y 12.0d0
    :dir-x -1.0d0   :dir-y 0.0d0
    :plane-x 0.0d0  :plane-y 0.66d0
    :time 0.0d0     :old-time 0.0d0))

(defstruct line-info-t
  map-x
  map-y
  height
  side)

(defun draw-vertical-line (x y1 y2 color)
  (if (< y2 y1)
    (rotatef y1 y2))
  ;;(print (format t "y1 ~D" y1))
  ;;(print (format t "y2 ~D" y2))

  (if (or (< y2 0)
        (>= y1 +screen-height+)
        (< x 0)
        (>= x +screen-width+))
    (return-from draw-vertical-line nil))

  ;;(print (format t "x=~D - y1=~D - y2=~D" x y1 y2))
  (loop
    for i
    from y1
    to y2

    do (sdl:draw-pixel
         (sdl:point :x x :y i)
         :surface sdl:*default-display*
         :color color)
    ))

(defun comp-line-height ( map-x     map-y
                          pos-x     pos-y
                          step-x    step-y
                          ray-dir-x ray-dir-y
                          side)
  (let* (
          (perpendicular-wall-dist
            (cond
              ((= side 0)
                (/ (- map-x (+ pos-x (/ (- 1 step-x) 2))) ray-dir-x))
              (t
                (/ (- map-y (+ pos-y (/ (- 1 step-y) 2))) ray-dir-y))))
          )

    (floor (/ +screen-height+ perpendicular-wall-dist))))

;;; perform digital differential analysis
(defun comp-vertical-line-info-dda
  (
    pos-x         pos-y
    map-x         map-y
    side-dist-x   side-dist-y
    delta-dist-x  delta-dist-y
    step-x        step-y
    ray-dir-x     ray-dir-y)

  (let (
         (hit nil)
         (new-map-x map-x)
         (new-map-y map-y)
         (side 0))

    ;;(print (format t "ray-dir-y: ~F delta-dist-x: ~F delta-dist-y: ~F step-x: ~D step-y: ~D" ray-dir-y delta-dist-x delta-dist-y step-x step-y))
    ;;(print (format t "side-dist-x: ~F side-dist-y: ~F" side-dist-x side-dist-y))
    ;;(print (format t "world-map new-map-x new-map-y: [~D][~D]: ~D" new-map-x new-map-y (aref *world-map* new-map-x new-map-y)))
    (loop while (not hit)
      do (progn    
           ;; a bit of good 'ol structured programming here ;-P
           (if (< side-dist-x side-dist-y)
             (progn
               (setf side-dist-x (+ side-dist-x delta-dist-x))
               (setf new-map-x (+ new-map-x step-x))
               (setf side 0))
             (progn
               (setf side-dist-y (+ side-dist-y delta-dist-y))
               (setf new-map-y (+ new-map-y step-y))
               (setf side 1)))

           ;; check if ray has hit a wall
           ;;(print (format t "world-map new-map-x new-map-y: [~D][~D]: ~D" new-map-x new-map-y (aref *world-map* new-map-x new-map-y)))
           (setf hit (if (> (aref *world-map* new-map-x new-map-y) 0)
                       t
                       nil))
           )
      ) ;; end while

    (make-line-info-t
      :map-x new-map-x
      :map-y new-map-y
      :height ( comp-line-height
                new-map-x new-map-y
                pos-x     pos-y
                step-x    step-y
                ray-dir-x ray-dir-y
                side)
      :side side)

    ))

(defun comp-vertical-line-info (x)
  ;;(break "dir-x=~D" (rc-state-t-dir-x *rc-state*))
  (let* (

          ;; for convenience...
          (pos-x (rc-state-t-pos-x *rc-state*))
          (pos-y (rc-state-t-pos-y *rc-state*))

          ;; calculate ray position and direction
          (camera-x ; x-coordinate in camera space
            (- (/ (* 2 x) (coerce +screen-width+ 'double-float)) 1))
          (ray-dir-x
            (+ (rc-state-t-dir-x *rc-state*)
              (* (rc-state-t-plane-x *rc-state*) camera-x)))
          (ray-dir-y
            (if (= 0 (+ (rc-state-t-dir-y *rc-state*)
                       (* (rc-state-t-plane-y *rc-state*) camera-x)))

              0.001d0 ;; WTF???

              (+ (rc-state-t-dir-y *rc-state*)
                (* (rc-state-t-plane-y *rc-state*) camera-x))))

          ;; length of ray from one x or y-side to next x or y-side
          (delta-dist-x
            (abs (/ 1 ray-dir-x)))
          (delta-dist-y
            (abs (/ 1 ray-dir-y)))

          ;; what direction to step in x or y-direction (either +1 or -1)
          (step-x (if (< ray-dir-x 0) -1 1))
          (step-y (if (< ray-dir-y 0) -1 1))

          ;; which box of the map we're in
          (map-x (floor pos-x))
          (map-y (floor pos-y))

          ;; length of ray from current position to next x or y-side
          (side-dist-x (if (< ray-dir-x 0)
                         (* delta-dist-x (- pos-x map-x))
                         (* delta-dist-x (- (+ map-x 1.0) pos-x))))

          (side-dist-y (if (< ray-dir-y 0)
                         (* delta-dist-y (- pos-y map-y))
                         (* delta-dist-y (- (+ map-y 1.0) pos-y))))

          (line-info (comp-vertical-line-info-dda
                       pos-x          pos-y
                       map-x          map-y
                       side-dist-x    side-dist-y
                       delta-dist-x   delta-dist-y
                       step-x         step-y
                       ray-dir-x      ray-dir-y))

          )

    ;;(print (format t "X=~D" x))
    ;;(print (format t "ray-dir-x=~D" ray-dir-x))
    ;;(print (format t "ray-dir-y=~D" ray-dir-y))

    line-info
    ))

(defun calc-color-shadow (color-code)
  (let ((rgb (multiple-value-bind (r g b)
               (sdl:color-* color-code)
               (mapcar #'(lambda (x) (truncate (/ x 2))) (list r g b)))))

    (sdl:color :r (car rgb) :g (cadr rgb) :b (caddr rgb))))

(defun comp-color (side color-code)
  (let* ((col
           (cond
             ((= color-code 1) sdl:*red*)
             ((= color-code 2) sdl:*green*)
             ((= color-code 3) sdl:*blue*)
             ((= color-code 4) sdl:*white*)
             (t sdl:*yellow*))))

    (if (= 1 side)
      (calc-color-shadow col)
      col)))

(defun comp-and-draw-vertical-line (x)
  (let* (
          (line-info (comp-vertical-line-info x))
          (draw-start (+ (/ (- 0 (line-info-t-height line-info)) 2) (/ +screen-height+ 2)))
          (draw-end (+ (/ (line-info-t-height line-info) 2) (/ +screen-height+ 2)))
          (color (comp-color
                   (line-info-t-side line-info)
                   (aref *world-map*
                     (line-info-t-map-x line-info)
                     (line-info-t-map-y line-info)))))
    (draw-vertical-line x draw-start draw-end color)))

(defun get-frame-time ()
  (setf (rc-state-t-old-time *rc-state*) (rc-state-t-time *rc-state*))
  (setf (rc-state-t-time *rc-state*) (sdl::sdl-get-ticks))
  (/
    (-
      (rc-state-t-time *rc-state*)
      (rc-state-t-old-time *rc-state*))
    1000.0))

(defun move-back-n-forward (dir move-speed)
  (let ((op (cond
              ((eql dir :dir-forward) #'+)
              ((eql dir :dir-back) #'-)
              (t (error "invalid param")))))

    (macrolet
      (
        (dest-x () '(* (rc-state-t-dir-x *rc-state*) move-speed))
        (dest-y () '(* (rc-state-t-dir-y *rc-state*) move-speed)))

      (if (= 0
            (aref *world-map*
              (floor (funcall op (rc-state-t-pos-x *rc-state*) (dest-x)))
              (floor (rc-state-t-pos-y *rc-state*))))
        (setf (rc-state-t-pos-x *rc-state*) (funcall op (rc-state-t-pos-x *rc-state*) (dest-x))))
      (if (= 0
            (aref *world-map*
              (floor (rc-state-t-pos-x *rc-state*))
              (floor (funcall op (rc-state-t-pos-y *rc-state*) (dest-y)))))
        (setf (rc-state-t-pos-y *rc-state*) (funcall op (rc-state-t-pos-y *rc-state*) (dest-y))))
      )))

(defun move-left-n-right (rot-speed)
  (let (
         (old-dir-x (rc-state-t-dir-x *rc-state*))
         (old-plane-x (rc-state-t-plane-x *rc-state*)))

    (macrolet
      (
        (cam-rotate (vx vy old-vx)
          `(setf
             ,vx
             (-
               (* ,vx (cos rot-speed))
               (* ,vy (sin rot-speed)))
             )

          `(setf
             ,vy
             (+
               (* ,old-vx (sin rot-speed))
               (* ,vy (cos rot-speed)))
             )
          )
        )

      (cam-rotate
        (rc-state-t-dir-x *rc-state*)
        (rc-state-t-dir-y *rc-state*)
        old-dir-x)

      (cam-rotate
        (rc-state-t-plane-x *rc-state*)
        (rc-state-t-plane-y *rc-state*)
        old-plane-x)
      )))

(defun game-loop-func (k)

  ;; Clear the display each game loop
  (sdl:clear-display sdl:*black*)

  (loop for x
    below +screen-width+
    do (comp-and-draw-vertical-line x))

  (let* (
          (frame-time (get-frame-time))   ;; the time this frame has taken, in seconds
          (move-speed (* frame-time 5.0)) ;; the constant value is in squares/second
          (rot-speed (* frame-time 3.0))) ;; the constant value is in radians/second

    (cond
      ((sdl:key= k :sdl-key-up)       (move-back-n-forward  :dir-forward move-speed))
      ((sdl:key= k :sdl-key-down)     (move-back-n-forward  :dir-back move-speed))
      ((sdl:key= k :sdl-key-left)     (move-left-n-right    rot-speed))
      ((sdl:key= k :sdl-key-right)    (move-left-n-right    (- 0 rot-speed)))
      ))

  ;; Redraw the display
  (sdl:update-display))

(defun screen (width height text)
  (sdl:with-init ()
    (sdl:window width height :fullscreen nil :title-caption text)
    (setf (sdl:frame-rate) 60)
    (let ((k nil))
      (sdl:with-events ()
        (:quit-event () t)
        (:key-up-event () (setf k nil))
        (:key-down-event (:key key)
          (cond
            ((or
               (sdl:key= key :sdl-key-up)
               (sdl:key= key :sdl-key-down)
               (sdl:key= key :sdl-key-left)
               (sdl:key= key :sdl-key-right))
              (setf k key))
            ((sdl:key= key :sdl-key-escape) (sdl:push-quit-event))
            (t (setf k nil))))

        (:idle ()
          (game-loop-func k))))
    ))

(defun raycaster-untextured-main ()
  (screen +screen-width+ +screen-height+ "raycaster-untextured"))

