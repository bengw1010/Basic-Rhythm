#lang racket

(provide (all-defined-out))

(require 2htdp/image
         2htdp/universe
         rackunit)

(define SCENE-HEIGHT 400)
(define SCENE-WIDTH 400)
(define SCENE-CENTER-X 200)
(define NOTE-RADIUS 20)
(define NOTE-INIT-Y 0)
(define NOTE-VELOCITY 4)
(define TARGET-Y 320)


(define PENDING "pending")
(define HIT "hit")
(define MISSED "miss")
(define OUTOFSCENE "outofscene")

(define MAX-MISSES 10)  
(define MISS-DISPLAY-X 10)
(define MISS-DISPLAY-Y (- SCENE-HEIGHT 30))

;; Draw horizontal gray line at target Y position
(define (draw-horizontal-line scene)
  (add-line scene 0 TARGET-Y SCENE-WIDTH TARGET-Y "gray"))

;; Draw vertical gray line in the center
(define (draw-vertical-line scene)
  (add-line scene SCENE-CENTER-X 0 SCENE-CENTER-X SCENE-HEIGHT "gray"))

;; Create an empty scene with gray lines
(define EMPTY-SCENE
  (draw-horizontal-line (draw-vertical-line (empty-scene SCENE-WIDTH SCENE-HEIGHT))))

;; Creates a black outline circle for the target
(define/contract (target-circle)
  (-> image?)
  (circle NOTE-RADIUS "outline" "black"))

;; YCoord? : number -> boolean
;; Checks if a number is a non-negative integer
(define/contract (YCoord? y)
  (-> number? boolean?)
  (and (integer? y) (>= y 0)))

;; Velocity? : number -> boolean
;; Checks if a number is a valid Velocity
(define/contract (Velocity? v)
  (-> number? boolean?)
  (and (integer? v) (<= 1 v 10)))

;; NoteState? : string -> boolean
;; A NoteState is either: 
;; - PENDING : the state moving towards hit or miss
;; - HIT : When the NOTE is on the outline circle and space was pressed
;; - MISSED : When NOTE passes the outline circle when space was not pressed
;; - OUTOFSCENE : When the Note surpasses the edges of the scene
(define/contract (NoteState? state)
  (-> string? boolean?)
  (or (string=? state PENDING)
      (string=? state HIT)
      (string=? state MISSED)
      (string=? state OUTOFSCENE)))

;; A Note is a (make-Note [y : YCoord] [vel : Velocity] [state : NoteState]) where
;; y : represents y coordinate of note center
;; vel : velocity of note on y axis (positive = down)
;; state : state of the note, e.g., "hit" or "missed"
(struct Note (y vel state) #:transparent)
(define/contract (make-Note y vel state)
  (-> YCoord? Velocity? NoteState? Note?)
  (Note y vel state))

;; Notes? : List<X> -> Bool
(define/contract (Notes? lst)
  (-> (listof any/c) boolean?)
  (or (empty? lst)
      (and (Note? (first lst))
           (Notes? (rest lst)))))     

;; place-Note-in-scene : Note Image -> Image
;; Places the note image at a given y-coordinate in the scene
(define/contract (place-Note-in-scene n sc)
  (-> Note? image? image?)
  (match-define (Note y yv nstate) n)
  (place-image
   (make-Note-image nstate) SCENE-CENTER-X y sc))

;; note-dist : Note Note -> NonNeg Int
;; Computes distance between two Note centers
(define/contract (note-dist n1 n2)
  (-> Note? Note? nonnegative-integer?)
  (abs (- (Note-y n1) (Note-y n2))))

;; make-Note-image : NoteState -> Image
;; Creates a visual representation of a note based on its state
(define/contract (make-Note-image ns)
  (-> NoteState? image?)
  (cond
    [(string=? ns PENDING) (circle NOTE-RADIUS "solid" "green")]
    [(string=? ns HIT) (star-polygon NOTE-RADIUS 8 3 "solid" "blue")]
    [(string=? ns MISSED) (circle NOTE-RADIUS "outline" "red")]
    [else EMPTY-SCENE]))

;; out-Note? : Note -> boolean
;; Checks if the note is out of the scene
(define/contract (out-Note? n)
  (-> Note? boolean?)
  (>= (Note-y n) SCENE-HEIGHT))

;; overlap? : Note Note -> Bool
;; Determines if two notes overlap based on their distance
(define/contract (overlap? n1 n2)
  (-> Note? Note? boolean?)
  (< (note-dist n1 n2) (* 2 NOTE-RADIUS)))

;; prune-Notes : Listof Note? -> Listof Note?
;; Removes notes that are out of the scene
(define/contract (prune-Notes notes)
  (-> (listof Note?) (listof Note?))
  (filter (lambda (n) (not (out-Note? n))) notes))
;; (define notes-all-outofscene (list (make-Note 401 2 OUTOFSCENE) (make-Note 450 2 OUTOFSCENE)))
;; (check-equal? (prune-Notes notes-all-outofscene) '())

;; next-Note : Note -> Note
;; Updates the position of a note by increasing its y-coordinate
(define/contract (next-Note note)
  (-> Note? Note?)
  (make-Note (+ (Note-y note) (Note-vel note)) (Note-vel note) (Note-state note)))

;; next-Notes : Listof Note? -> Listof Note?
;; Applies the next-Note function to a list of notes
(define/contract (next-Notes notes)
  (-> (listof Note?) (listof Note?))
  (map next-Note notes))
;; (check-equal? (next-Notes (list (make-Note 10 4 PENDING) (make-Note 20 2 HIT)))
;;  (list (make-Note 14 4 PENDING) (make-Note 22 2 HIT)))

;; place-Notes-in-scene : Listof Note image? -> image?
;; Places each note in the scene, layering them from bottom to top
(define/contract (place-Notes-in-scene notes init-scene)
  (-> (listof Note?) image? image?)
  (foldr (lambda (note acc-scene)
           (place-Note-in-scene note acc-scene)) 
         init-scene 
         notes))
;;(define empty-notes '())
;;(define initial-scene (empty-scene SCENE-WIDTH SCENE-HEIGHT))
;;(check-equal? (place-Notes-in-scene empty-notes initial-scene) initial-scene)

;; insert-note? : -> boolean?
;; Determines whether a new note should be added, with a 1% chance
(define/contract (insert-note?)
  (-> boolean?)
  (= (random 100) 1))

;; maybe-add-Note : Listof Note? -> Listof Note?
;; Conditionally adds a new note at the top of the scene if there is no overlap
(define/contract (maybe-add-Note notes)
  (-> (listof Note?) (listof Note?))
  (let ((new-note (make-Note NOTE-INIT-Y NOTE-VELOCITY PENDING)))
    (if (and (insert-note?)
             (not (ormap (lambda (note) (overlap? new-note note)) notes)))
        (cons new-note notes)
        notes)))
;; (define existing-notes (list (make-Note 50 NOTE-VELOCITY PENDING)))
;; (define (insert-note?) #f) 
;; (check-equal? (maybe-add-Note existing-notes) existing-notes)

;; no-overlap? : Note Listof Notes -> Boolean
;; Returns true if the given list is empty or if the given note does not overlap with any note in the list
(define/contract (no-overlap? n notes)
  (-> Note? (listof Note?) boolean?)
  (or (empty? notes)
      (not (overlap? n (first notes)))))

;; Define WorldState: 
(struct WorldState (notes misses) #:transparent)

;; Creates a new WorldState
(define/contract (make-WorldState notes misses)
  (-> (listof Note?) nonnegative-integer? WorldState?)
  (WorldState notes misses))

(define INIT-MISS 0)

(define INIT-WORLDSTATE
  (make-WorldState (list (make-Note NOTE-INIT-Y NOTE-VELOCITY PENDING)) INIT-MISS))

;; misses-remaining : WorldState -> nonnegative-integer?
;; Computes the number of misses remaining before the game ends
(define/contract (misses-remaining ws)
  (-> WorldState? nonnegative-integer?)
  (max 0 (- 10 (WorldState-misses ws))))
;; (check-equal? (misses-remaining (make-WorldState (list (make-Note 10 5 PENDING)) 5)) 5)

;; add-score-to-scene : nonnegative-integer? image? image? -> image?
;; Adds the score display to the scene
(define/contract (add-score-to-scene misses sc)
  (-> nonnegative-integer? image? image?)
  (place-image (text (format "Misses left: ~a" misses) 20 "black") 10 10 sc))
;;(check-equal? (add-score-to-scene 5 EMPTY-SCENE) (place-image (text "Misses left: 6" 20 "black") 10 10 EMPTY-SCENE))

;; num-Notes : WorldState -> nonnegative-integer?
;; Returns the number of Notes in the WorldState
(define/contract (num-Notes ws)
  (-> WorldState? nonnegative-integer?)
  (length (WorldState-notes ws)))
;;(check-equal? (num-Notes (make-WorldState (list (make-Note 100 5 PENDING)) 2)) 1)

;; game-over? : WorldState -> boolean?
;; check if the game or not number < 10 is not over and >= 10 is over
(define/contract (game-over? ws)
  (-> WorldState? boolean?)
  (>= (WorldState-misses ws) MAX-MISSES))
;; (check-equal? (game-over? (make-WorldState (list (make-Note 100 5 PENDING)) 11)) #t)

;; mark-missed-notes : listof Note -> listof Note
;; keep track of missed notes in the list
(define/contract (mark-missed-notes notes)
  (-> (listof Note?) (listof Note?))
  (map (lambda (note)
         (if (and (out-Note? note) 
                  (not (string=? (Note-state note) HIT)))
             (make-Note (Note-y note) (Note-vel note) MISSED)  ; Mark as missed
             note))
       notes))
;;(mark-missed-notes (list (make-Note 100 2 PENDING) 
;;                          (make-Note 150 2 HIT) 
;;                         (make-Note 300 3 PENDING)))

;; next-WorldState : WorldState -> WorldState
;; updates the worldstate base on the notes
;; check for any misses
;; create new notes
;; check if current missed notes is higher than the max miss note
(define/contract (next-WorldState ws)
  (-> WorldState? WorldState?)
  (if (game-over? ws)
      ws  
      (let* ((updated-notes (next-Notes (WorldState-notes ws)))
             (miss-count (foldl (lambda (note acc)
                                   (if (and (out-Note? note)
                                            (not (string=? (Note-state note) HIT))
                                            (not (string=? (Note-state note) MISSED)))(+ acc 1)acc))0 updated-notes))
             (new-misses (+ (WorldState-misses ws) miss-count))
             (final-misses (min new-misses MAX-MISSES))  
             (new-notes (maybe-add-Note (prune-Notes updated-notes))) 
             (updated-notes-with-misses (mark-missed-notes new-notes)))  

        (make-WorldState updated-notes-with-misses final-misses))))
;;  (next-WorldState (make-WorldState (list (make-Note 100 5 PENDING) 
;;                                         (make-Note 200 3 PENDING)) 5))


;; life-remain : nonnegative-integer? -> image? -> image
;; display the number of misses you can have 
(define/contract (life-remain misses sc)
  (-> nonnegative-integer? image? image?)
  (place-image (text (format "Misses left: ~a" misses) 20 "black") 
               (- SCENE-WIDTH 100) 
               (- SCENE-HEIGHT 20) 
               sc))
;;(define sample-scene (empty-scene SCENE-WIDTH SCENE-HEIGHT))
;;(check-equal? (life-remain 7 sample-scene) 
;;               (place-image (text "Misses left: 7" 20 "black") 
;;                            (- SCENE-WIDTH 100) 
;;                            (- SCENE-HEIGHT 20) 
;;                            sample-scene))

;; render : WorldState -> image?
;; renders the current state of the game
(define/contract (render ws)
  (-> WorldState? image?)
  (if (game-over? ws)
      (render-last ws)  ; Call render-last only if the game is over
      (let* ((notes-image (place-Notes-in-scene (WorldState-notes ws) EMPTY-SCENE))
             (target-circle (target-circle))
             (misses-left (misses-remaining ws))) 
        (life-remain misses-left 
                      (place-image target-circle SCENE-CENTER-X TARGET-Y notes-image)))))
;; (check-equal? (render sw)
;;              (life-remain 8 (place-image (target-circle) SCENE-CENTER-X TARGET-Y 
;;                             (place-Notes-in-scene (WorldState-notes sw) EMPTY-SCENE))))

;; render-last : WorldState -> image?
;; Renders the "Game Over" message
(define/contract (render-last ws)
  (-> WorldState? image?)
  (let ((game-over-text (text "Game Over" 30 "black"))) 
    (place-image game-over-text SCENE-CENTER-X (/ SCENE-HEIGHT 2) EMPTY-SCENE))) 
;; (define sw (make-WorldState (list (make-Note 100 5 "pending")) 5))
;; (check-equal? (render-last sw)
;;              (place-image (text "Game Over" 30 "black") SCENE-CENTER-X (/ SCENE-HEIGHT 2) EMPTY-SCENE))

;; Create the target note
(define (make-target-note)
  (make-Note TARGET-Y NOTE-VELOCITY PENDING))

;; key-handler : WorldState? -> key-event? -> WorldState?
;; process key events 
(define/contract (key-handler ws ke)
  (-> WorldState? key-event? WorldState?)
  (cond
    [(key=? ke " ")
     (let* ((target-note (make-target-note))
            (updated-notes (map (lambda (note)
                                   (if (overlap? note target-note)
                                       (make-Note (Note-y note) (Note-vel note) HIT)
                                       note))
                                 (WorldState-notes ws)))
            (new-misses (WorldState-misses ws))) 
       (make-WorldState updated-notes new-misses))]
    [else 
     (let ((new-misses (+ 1 (WorldState-misses ws))))
       (make-WorldState (WorldState-notes ws) new-misses))]))

;; (define sw (make-WorldState (list (make-Note 100 NOTE-VELOCITY PENDING)) 0))
;; (check-equal? (key-handler sw " ")
;;              (make-WorldState (list (make-Note 100 NOTE-VELOCITY HIT)) 0))

(define (main)
  (big-bang INIT-WORLDSTATE
            [on-tick next-WorldState]
            [on-key key-handler]
            [on-draw render]))
