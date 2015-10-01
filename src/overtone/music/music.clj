
;; Author: Viveka Aggarwal
;; A remix version of 'In the End' by 'Linkin Park'
;; Help has been taken from github.com/overtone/overtone
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(ns overtone.first.tune
  (:use overtone.live
        overtone.inst.sampled-piano
        overtone.inst.piano))

;;for the theme 
(def piece [:E3 :B4 :B4 :G4 :F#3 :F#3 :F#3 :F#3 :G4])  

;; for the base
(def piece1 [:D#5 :C#5 :B5 ]) 

;; the basic piano player which takes in the time, speed and notes for the same. 
(defn piano-player                                             
  [ctime speed notes]
  (let [n      (first notes) notes  (next notes) ctime-next (+ ctime speed)]
    (when n (at ctime (sampled-piano (note n)))
      (apply-by ctime-next #'piano-player [ctime-next speed notes]))))


;; sounds for the drum beats
(def snare       (sample (freesound-path 26903)))
(def kick        (sample (freesound-path 2086)))
(def close-hihat (sample (freesound-path 802)))
(def open-hihat  (sample (freesound-path 26657)))
(def wop         (sample (freesound-path 85291)))
(def subby       (sample (freesound-path 25649)))
(def dirty-kick  (sample (freesound-path 30669)))


;; defining the beat pattern
(def pats { subby      [0 0 0 0 0 1 0 0]
            snare      [1 0 0 1 1 0 1 0]
            wop        [1 0 0 1 0 0 0 1]
            open-hihat [0 0 1 0 0 0 1 0]
            close-hihat[0 0 1 0 1 0 0 0]
            kick       [0 1 0 0 0 1 0 0]})
 
;; for the drum beats
(defn sequencer
 ([ctime speed patterns] (sequencer ctime speed patterns 0))
   ([ctime speed patterns beat]
      (doseq [[sound pattern] @patterns
              :when (= 1 (nth pattern (mod beat (count pattern))))]
        (at ctime (sound)))
      (let [new-t (+ ctime speed)]
        (apply-by new-t #'sequencer [new-t speed patterns (inc beat)]))))

;;initial and random notes
(let
      [time (now)]
    (at (+    0 time) (kick) )
    (at (+  200 time) (kick)  )
    (at (+ 600 time) (kick) )
    (at (+ 8700 time) (dirty-kick))
    (at (+ 30000 time) (stop))
    (at (+ 31000 time) (stop)) 
)

;;main play sequence
(do
(sequencer (+ 9000 (now)) 200 (atom pats))
(piano-player (now) 450 (take 37 (cycle piece)))
(piano-player (now) 450 (take 37 (cycle piece)))
(piano-player (+ (now) 19000) 500 (take 12 (cycle piece1))) )


;;(stop) to stop (at 30 seconds)
