turtles-own [ home-pos ]
patches-own [ pellet-grid? visible? real-pcolor]  ;; true/false: is a pellet here initially?

breed [ pellets pellet ]
 pellets-own [ powerup? ]



breed [ pacmans pacman ]
pacmans-own  [ new-heading ]



globals [
  score         ;; your score
                ;; incremented in the play function and displayed on screen
  atgoal        ;; has the pacman found the gold
                ;; used in the "depth-first search to stop the pacman 
  hide-memory2   ;; should the trail followed so far be shown
                ;; i use it in the play and various other functions - you don't need to worry about this
  direction-set  ;; has the next direction for the pac-man been set
                ;;-- I've created this for you to use if you want
                ;; remember it may not  have any particular value until you set it 
]

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; example code you can copy and re-use
;; remember to delete the ; which mark lines as comments

;; the following tests are available to you, which return TRUE or FALSE
;; can-turn-right?
;; can-go-forward?
;;  can-turn-left?
;; can-turn-north?
;; can-turn-south?
;; can-turn-east?
;; can-turn-west?
;; at-left-turn?
;; at-left-turn?
;; wall-ahead?
     
;; the following commands are available to you
;; turn-right
;; turn-left
;; go-straight-ahead
;; turn-around
;; face-north
;; face-east
;; face-west
;; face-south
     
;; there are two ways that you can build up commands
;; the first is to use nested ifelse statements like this
     
;; ifelse  test1?
;;   [ code executed if test1? returns true]
;;   [ code executed if test1? returns FALSE]
     
;;  you can "nest" if and ifelse structures, so if you want to test between three different cases when only one can be true you could say
;; ifelse test1?             -  test for first situation
;;   [ code executed if the first situation occurs
;;   [    ifelse test2?            - test to discriminate between remaining situations
;;            [  code executed in second case]         
;;            [   code executed in third case ] 
     
;;  the second, and perhaps  cleaner way to do it is to use a variable that says whether you have finished testing
;; that way you can use a series of simpler if tests
;; you need the variable because otherwise each one might get used
;;  the "and" word means that the test only returns true if both parts are true
;; you need to add your variable to the list of global variables at the top of the file - but you'll find one there you can use
;;  e.g.
;;
;;  set keep-testing = TRUE
;; if ( (test-for-case1 = TRUE) and (keep-testing = TRUE))
;;    [   
;;        code executed when case 1 is true
;;        set keep-testing = FALSE
;;    ]
;; if ( (test-for-case2 = TRUE) and (keep-testing = TRUE))
;;    [   
;;        code executed when case 2 is true
;;        set keep-testing = FALSE
;;    ]
;; if ( (test-for-case3 = TRUE) and (keep-testing = TRUE))
;;    [   
;;        code executed when case 3 is true
;;        set keep-testing = FALSE
;;    ]
;; if (keep-testing = TRUE)
 ;;  [ code that will be executed if none of the cases match i.e. all the tests fail]
     
     

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;; DON'T EDIT ANYTHING ABOVE THIS LINE                           




;;;;;;;;;;;;;;PUT YOUR CODE IN HERE;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; this is the function that controls the pacman

to depth-first-search-move-pacman  ;observer procedure


  ask pacmans   ; so the syntax here is that we are asking the pacman to do something
     [
       ifelse at-goal?               ;;;  this test just either:
         [stop ]                      ;; stops the pac-man when it gets to the goal
         [set direction-set FALSE]   ;; or creates a variable called direction-set and gives it the value FALSE    
         
         
     ;;;;;;;;;;;start of where your code should go
   
     ;;;;;;;;;;end of where your code should go      
           ;;once the direction has been set this command moves the pacman forward one pace and colours the trail behind him green
           ;; you shouldn't need to change these next lines
       if(direction-set = TRUE)
       [
       step-forward-and-colour-trail
       set score score + 1
       ]
     ]
end



;;;;;;;;;;;;;;;;;;;  DONT'T  EDIT ANYTHING BELOW THIS LINE         
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;these are the functions that make the pac man turn        

to turn-left
  lt 90
end

to turn-right
  rt 90
end 

to face-straight-ahead
 lt 0
end     

to turn-around
  let newheading  opposite heading 
  set heading newheading
end

to step-forward-and-colour-trail
  if can-go-forward?
  [
   set real-pcolor green    
   fd 1
  ]
end
       
     
to face-north
set heading 0
end

to face-east
set heading 90
end

to face-west
set heading 270
end

to face-south     
set heading 180
end     
       
;;;;;;
;; these are a bunch od useful tests   

       
to-report wall-ahead?
  ifelse  can-go-forward?
    [report  FALSE]
    [report  TRUE]
end

to-report opposite [dir]
  ifelse dir < 180
  [ report dir + 180 ]
  [ report dir - 180 ]
end




to-report can-turn-left?  
ifelse  ([real-pcolor] of patch-left-and-ahead 90 1 = blue)
    [report  FALSE]
    [report  TRUE]
end


to-report can-turn-right?  
ifelse ( [real-pcolor] of patch-right-and-ahead 90 1 = blue)
    [report  FALSE]
    [report  TRUE]
end


to-report can-go-forward?  
 ifelse ( [real-pcolor] of patch-ahead 1 = blue)
    [report  FALSE]
    [report  TRUE]
end


to-report can-turn-north?
 ifelse ( [real-pcolor] of patch-at 0 1 = blue)
    [report  FALSE]
    [report  TRUE]
end

to-report can-turn-south?
 ifelse ( [real-pcolor] of patch-at 0 -1 = blue)
    [report  FALSE]
    [report  TRUE]
end

to-report can-turn-east?
 ifelse ( [real-pcolor] of patch-at 1 0 = blue)
    [report  FALSE]
    [report  TRUE]
end
to-report can-turn-west?       
 ifelse ( [real-pcolor] of patch-at -1 0 = blue)
    [report  FALSE]
    [report  TRUE]
end      
       
       
to-report at-left-turn?
 ifelse( ([real-pcolor] of patch-left-and-ahead 90 1 != blue) and ([real-pcolor] of patch-ahead 1 != blue))
    [report  TRUE]
    [report  FALSE]
end

to-report at-right-turn?
 ifelse( ([real-pcolor] of patch-right-and-ahead 90 1 != blue) and ([real-pcolor] of patch-ahead 1 != blue))
    [report  TRUE]
    [report  FALSE]
end

to-report at-goal?
ifelse (([real-pcolor] of patch-ahead 1 = orange) or (real-pcolor = orange))
     [report  TRUE]
    [report  FALSE]
end











;;;;;;;;;;;;;;;;;;;;;;;;
;;; Setup Procedures ;;;
;;;;;;;;;;;;;;;;;;;;;;;;

to new  ;; Observer Button  - this is the code that is run when you press "new" - it just runs once
  clear-all
  set score 0
  set hide-memory FALSE 
  set atgoal FALSE
  let current-score score 
  ifelse (maze = 1)
   [import-world   "maze1.csv"]
   [import-world   "maze2.csv"]
 
    ask pacmans
    [ set home-pos list xcor ycor ]

end




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;   Runtime Procedures        ;;;
;;;  Don't alter any of these   ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to play  ;; Observer Forever Button ----- this is the one that happened when you press play - it is like a "main" function
   every (0.25)
    [ 
      set-visibility
      if (hide-memory = TRUE)
       [ remove-memory]

      display
      depth-first-search-move-pacman

      ask pacmans
      [
          ;; Animation  - rthis next if-else statement just switches between different  shapes every timestep
          ;;  I've left it in so you can see the syntax that gets used -
          ;;  first an "ifelse" command followed by an expession which gets evalauted (i.e. is it true that shape = pacman
          ;; next in square brackets comes the command that is run if the expressino is true
          ;; followed by the command that gets run if the exprtession is false
       ifelse at-goal? 
          [set shape "face happy"]
          [ifelse shape = "pacman"
             [ set shape "pacman open" ]
             [ set shape "pacman" ]
          ]
      ]      
  ]
end





to set-visibility ;; Observer Proceedure
      ask pacmans
           [
            ;; maze in front
            ;;ask patch-left-and-ahead 30 2 
                ;;[set visible? FALSE]
            ask patch-left-and-ahead 45 1
                [set visible? TRUE]
            ask patch-ahead 1 
                [set visible? TRUE]
            ;;ask patch-ahead 2 
                ;;[set visible? TRUE]
            ask patch-right-and-ahead 45 1 
                [set visible? TRUE]
            ;;ask patch-right-and-ahead 30 2
                ;;[ set visible? TRUE]
            ;; sides     
            ask patch-left-and-ahead 90 1 
                [set visible? TRUE]
            ask patch-right-and-ahead 90 1 
                [set visible? TRUE]
          ]
      ask patches
         [
           ifelse  (visible?)
             [  set pcolor real-pcolor    ]
             [  set pcolor 7 ]
         ]

end


to remove-memory ;; Observer Procedure
   ask pacmans
      [
                  ;;behind
            ask patch-left-and-ahead 45 -1 
                [set visible? FALSE]
            ask patch-left-and-ahead 30 -2 
                [set visible? FALSE]
           ask patch-ahead -1  
                [set visible? FALSE]
           ask patch-ahead -2  
                [set visible? FALSE]
           ask patch-right-and-ahead 45 -1  
               [set visible? FALSE]
           ask patch-right-and-ahead 30 -2 
                [set visible? FALSE]
        ]
    ask patches
        [
           ifelse  (visible?)
             [  set pcolor real-pcolor    ]
             [  set pcolor 7 ]
        ]     
 end       
        
        

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Interface Procedures ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to  move-up
  ask pacmans [ set new-heading 0 ]
end

to move-right
  ask pacmans [ set new-heading 90 ]
end

to move-down
  ask pacmans [ set new-heading 180 ]
end

to move-left
  ask pacmans [ set new-heading 270 ]
end


to show-maze  ;; Observer Button
  ask patches
  [    set visible?  TRUE]  
  display
end 



to hide-maze  ;; Observer Button
  ask patches
  [    set visible?  FALSE]  
  display
end 

to turn-memory-off ;; Observer Button
  set hide-memory TRUE
end
  

; *** NetLogo 4.0.4 Model Copyright Notice ***
;
; This model was created as part of the projects:
; PARTICIPATORY SIMULATIONS: NETWORK-BASED DESIGN FOR SYSTEMS LEARNING IN
; CLASSROOMS and/or INTEGRATED SIMULATION AND MODELING ENVIRONMENT.
; The project gratefully acknowledges the support of the
; National Science Foundation (REPP & ROLE programs) --
; grant numbers REC #9814682 and REC-0126227.
;
; Copyright 2001 by Uri Wilensky.  All rights reserved.
;
; Permission to use, modify or redistribute this model is hereby granted,
; provided that both of the following requirements are followed:
; a) this copyright notice is included.
; b) this model will not be redistributed for profit without permission
;    from Uri Wilensky.
; Contact Uri Wilensky for appropriate licenses for redistribution for
; profit.
;
; To refer to this model in academic publications, please use:
; Wilensky, U. (2001).  NetLogo Pac-Man model.
; http://ccl.northwestern.edu/netlogo/models/Pac-Man.
; Center for Connected Learning and Computer-Based Modeling,
; Northwestern University, Evanston, IL.
;
; In other publications, please use:
; Copyright 2001 Uri Wilensky.  All rights reserved.
; See http://ccl.northwestern.edu/netlogo/models/Pac-Man
; for terms of use.
;
; *** End of NetLogo 4.0.4 Model Copyright Notice ***



;*******UWE copyright notice**************
;
;copyright 2009 Jim Smith.
; This model is is an adaptation of the original Pac-Man model referred to above.
; authored by Jim Smith and Steve Cayzer as part of the UK HEA-ICS funded project "Teaching Problem Solving and AI with PacMan. "
;To refer to this model please cite:
;J.E.ÊSmith. Learning Through Programming Games: Teaching AI with Pacman and Netlogo. 2009. in Proc 5th UK Conference 
; on AI in Education. Higher Education Academy Information and Computer Sciences Subject Centre (HEA-ICS).
;J.E.ÊSmith and S.ÊCayzer. Teaching Problem Solving and AI with PacMan. 2010. in Proceedings of the 11th Annual Conference on 
; the Teaching of Computing,  Higher Education Academy Information and Computer Sciences Subject Centre (HEA-ICS).
;
;see above for terms and conditions relating to the original model
;; Permission to use, modify or redistribute this model is hereby granted,
; provided that both of the following requirements are followed:
; a) this copyright notice is included.
; b) this model will not be redistributed for profit without permission
;    from Jim Smith.
; Contact Jim Smith for appropriate licenses for redistribution for
; profit.

;****end of UWE copyright notice



@#$#@#$#@
GRAPHICS-WINDOW
243
10
694
482
10
10
21.0
1
10
1
1
1
0
1
0
1
-10
10
-10
10
1
1
0
ticks

CC-WINDOW
5
496
703
591
Command Center
0

MONITOR
11
41
121
86
Score
score
0
1
11

BUTTON
11
90
121
123
New
new
NIL
1
T
OBSERVER
NIL
N
NIL
NIL

BUTTON
121
90
231
123
Play
play
T
1
T
OBSERVER
NIL
P
NIL
NIL

BUTTON
88
180
143
213
Up
move-up
NIL
1
T
OBSERVER
NIL
I
NIL
NIL

BUTTON
143
213
198
246
Right
move-right
NIL
1
T
OBSERVER
NIL
L
NIL
NIL

BUTTON
88
213
143
246
Down
move-down
NIL
1
T
OBSERVER
NIL
K
NIL
NIL

BUTTON
33
213
88
246
Left
move-left
NIL
1
T
OBSERVER
NIL
J
NIL
NIL

BUTTON
128
291
222
324
Hide Maze
hide-maze
NIL
1
T
OBSERVER
NIL
H
NIL
NIL

BUTTON
24
291
122
324
Show Maze
show-maze
NIL
1
T
OBSERVER
NIL
S
NIL
NIL

SWITCH
44
366
185
399
hide-memory
hide-memory
1
1
-1000

CHOOSER
53
430
191
475
maze
maze
1 2
1

@#$#@#$#@
WHAT IS IT?
-----------

This is the first of a series of tutorials based on the classic arcade game, Pac-Man. 
That game involves  navigating Pac-Man through a maze, consuming white pellets and avoiding being eaten by ghosts. 

We will be using this model to demonstrate concepts in search, knowledge representation, reasoning and planning,
building up to the creation of an automated pac-man controller.

To begin with we will examine the concept of how an intelligent agent can begin to discover knowledge about a problem or environment, and to reach some goal. This is often described as a search problem, and we can typify it by the idea of searching a maze.

 Your objective is implement a depth-first search method so that Pac-Man discovers the large orange square.





THINGS TO TRY
-------------
Get depth -first working on maze 1.

Does it make any difference to the speed what protocol you adopt e.g. left or right first?

What difference does it make if you reason in "absolute" (north,east,south,west) terms rather than "relative" terms (left, ahead, right, behind)?
Does one of these assume more information is held in the "state"

Now try your algorithm on maze 2.
Does it still work.  What might this imply about depth-first search?  Can you modify your method to make it work?



HOW TO USE IT
-------------

I highly recommend that at some stage you download netlogo onto your home machine and look at the user manual- The three introductory tutorials are good.
However,  you don't need these for now,  as I have provided you with the basic script.
Going to the "procedures" tab you can see the script.
Anything after a ";" is a comment which is ignored by netlogo when it runs the script.

I have clearly marked where you should put your code,  and which bits you should not change.

To let you focus on how your method works, rather than on programming, I have provided you with the following:

1. the following tests, which return TRUE or FALSE:
can-turn-right?,  can-go-forward?,  can-turn-left?, can-turn-north?
can-turn-south?, can-turn-east?, can-turn-west?
at-left-turn?, at-left-turn?, wall-ahead?
     
2. a set of functions that you can give the packman to change which way it is facing:
turn-right, turn-left, go-straight-ahead, turn-around
 face-north, face-east, face-west, face-south

You should be able to build up your algorithm using a series of what programmers call "conditional blocks"
in other words
 if such-and-such do thing1

or,  if you want to choose between two behaviours accordingly:
if such-and-such do thing1 otherwise do thing2.

At the top of the script you will see i have put some examples you can copy and adapt to get the syntax right.

Once you have put in your code, use the "check" button to check whether you have got the syntax right, and adjust it if necessary.
Then go to the interface,  and use the new and play buttons as before.
you can use the "show maze" button if you want to see whether your packman is doing what you expect.


Monitors
-- SCORE shows your current score, this increases with every clock tick.


Buttons
-- NEW sets up a new game with a score of 0.
-- PLAY begins the game, you can toggle it on and off to pause the game.  
-- SHOW MAZE tells the system to display the whole maze.
-- HIDE MAZE tells the system to cover up the maze again, it will get uncovered again

Controls
-- UP, DOWN, LEFT, RIGHT control the direction Pac-Man moves.
   The keyboard shortcuts may not work on all system.
 as you (re)commence exploring

Switches
 -- HIDE MEMORY determines whether previously seen squares remain visible - maybe those of you with good memories, or a good strategy will not  need this turned off?

 -- MAZE lets you choose which maze to use.  Start with maze 1 - this is the one you did interactively last week.

THINGS TO NOTICE
----------------
If you go off the edge of the maze you will wrap around to the other side.

Identifying Things in the Maze:
-- Yellow Circle with a mouth:  This is Pac-Man - you.
-- White Circles:               These are Pellets - Collect all of these (including the Power-Pellets) to move on to the next level.
-- Large Gold Circles:         This is the gold. Once you have found this you have completed the maze.
-- Blue Squares:                These are the walls of the maze - The Pac-Man cannot move through the walls.



NETLOGO FEATURES
-----------------
This model makes use of breeds, create-<breed>, every, and user-message.

The "import-world" command is used to read in the different maze configurations (levels).


CREDITS AND REFERENCES
----------------------
To refer to the original  model in academic publications, please use:  Wilensky, U. (2001).  NetLogo Pac-Man model.  http://ccl.northwestern.edu/netlogo/models/Pac-Man.  Center for Connected Learning and Computer-Based Modeling, Northwestern University, Evanston, IL.

In other publications, please use:  Copyright 2001 Uri Wilensky.  All rights reserved.  See http://ccl.northwestern.edu/netlogo/models/Pac-Man for terms of use.

To refer to this model please cite:
J.E.ÊSmith. Learning Through Programming Games: Teaching AI with Pacman and Netlogo. 2009. in Proc 5th UK Conference 
 on AI in Education. Higher Education Academy Information and Computer Sciences Subject Centre (HEA-ICS).
J.E.ÊSmith and S.ÊCayzer. Teaching Problem Solving and AI with PacMan. 2010. in Proceedings of the 11th Annual Conference on 
 the Teaching of Computing,  Higher Education Academy Information and Computer Sciences Subject Centre (HEA-ICS).


Copyright Jim Smith, University of the West of England, Bristol.

@#$#@#$#@
default
true
0
Polygon -7500403 true true 150 5 40 250 150 205 260 250

airplane
true
0
Polygon -7500403 true true 150 0 135 15 120 60 120 105 15 165 15 195 120 180 135 240 105 270 120 285 150 270 180 285 210 270 165 240 180 180 285 195 285 165 180 105 180 60 165 15

arrow
true
0
Polygon -7500403 true true 150 0 0 150 105 150 105 293 195 293 195 150 300 150

box
false
0
Polygon -7500403 true true 150 285 285 225 285 75 150 135
Polygon -7500403 true true 150 135 15 75 150 15 285 75
Polygon -7500403 true true 15 75 15 225 150 285 150 135
Line -16777216 false 150 285 150 135
Line -16777216 false 150 135 15 75
Line -16777216 false 150 135 285 75

bug
true
0
Circle -7500403 true true 96 182 108
Circle -7500403 true true 110 127 80
Circle -7500403 true true 110 75 80
Line -7500403 true 150 100 80 30
Line -7500403 true 150 100 220 30

butterfly
true
0
Polygon -7500403 true true 150 165 209 199 225 225 225 255 195 270 165 255 150 240
Polygon -7500403 true true 150 165 89 198 75 225 75 255 105 270 135 255 150 240
Polygon -7500403 true true 139 148 100 105 55 90 25 90 10 105 10 135 25 180 40 195 85 194 139 163
Polygon -7500403 true true 162 150 200 105 245 90 275 90 290 105 290 135 275 180 260 195 215 195 162 165
Polygon -16777216 true false 150 255 135 225 120 150 135 120 150 105 165 120 180 150 165 225
Circle -16777216 true false 135 90 30
Line -16777216 false 150 105 195 60
Line -16777216 false 150 105 105 60

car
false
0
Polygon -7500403 true true 300 180 279 164 261 144 240 135 226 132 213 106 203 84 185 63 159 50 135 50 75 60 0 150 0 165 0 225 300 225 300 180
Circle -16777216 true false 180 180 90
Circle -16777216 true false 30 180 90
Polygon -16777216 true false 162 80 132 78 134 135 209 135 194 105 189 96 180 89
Circle -7500403 true true 47 195 58
Circle -7500403 true true 195 195 58

circle
false
0
Circle -7500403 true true 45 45 210

circle 2
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240

cow
false
0
Polygon -7500403 true true 200 193 197 249 179 249 177 196 166 187 140 189 93 191 78 179 72 211 49 209 48 181 37 149 25 120 25 89 45 72 103 84 179 75 198 76 252 64 272 81 293 103 285 121 255 121 242 118 224 167
Polygon -7500403 true true 73 210 86 251 62 249 48 208
Polygon -7500403 true true 25 114 16 195 9 204 23 213 25 200 39 123

cylinder
false
0
Circle -7500403 true true 0 0 300

dot
false
0
Circle -7500403 true true 90 90 120

eyes
false
0
Circle -1 true false 62 75 57
Circle -1 true false 182 75 57
Circle -16777216 true false 79 93 20
Circle -16777216 true false 196 93 21

face happy
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 255 90 239 62 213 47 191 67 179 90 203 109 218 150 225 192 218 210 203 227 181 251 194 236 217 212 240

face neutral
false
0
Circle -7500403 true true 8 7 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Rectangle -16777216 true false 60 195 240 225

face sad
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 168 90 184 62 210 47 232 67 244 90 220 109 205 150 198 192 205 210 220 227 242 251 229 236 206 212 183

fish
false
0
Polygon -1 true false 44 131 21 87 15 86 0 120 15 150 0 180 13 214 20 212 45 166
Polygon -1 true false 135 195 119 235 95 218 76 210 46 204 60 165
Polygon -1 true false 75 45 83 77 71 103 86 114 166 78 135 60
Polygon -7500403 true true 30 136 151 77 226 81 280 119 292 146 292 160 287 170 270 195 195 210 151 212 30 166
Circle -16777216 true false 215 106 30

flag
false
0
Rectangle -7500403 true true 60 15 75 300
Polygon -7500403 true true 90 150 270 90 90 30
Line -7500403 true 75 135 90 135
Line -7500403 true 75 45 90 45

flower
false
0
Polygon -10899396 true false 135 120 165 165 180 210 180 240 150 300 165 300 195 240 195 195 165 135
Circle -7500403 true true 85 132 38
Circle -7500403 true true 130 147 38
Circle -7500403 true true 192 85 38
Circle -7500403 true true 85 40 38
Circle -7500403 true true 177 40 38
Circle -7500403 true true 177 132 38
Circle -7500403 true true 70 85 38
Circle -7500403 true true 130 25 38
Circle -7500403 true true 96 51 108
Circle -16777216 true false 113 68 74
Polygon -10899396 true false 189 233 219 188 249 173 279 188 234 218
Polygon -10899396 true false 180 255 150 210 105 210 75 240 135 240

ghost
false
0
Circle -7500403 true true 61 30 179
Rectangle -7500403 true true 60 120 240 232
Polygon -7500403 true true 60 229 60 284 105 239 149 284 195 240 239 285 239 228 60 229
Circle -1 true false 81 78 56
Circle -16777216 true false 99 98 19
Circle -1 true false 155 80 56
Circle -16777216 true false 171 98 17

house
false
0
Rectangle -7500403 true true 45 120 255 285
Rectangle -16777216 true false 120 210 180 285
Polygon -7500403 true true 15 120 150 15 285 120
Line -16777216 false 30 120 270 120

leaf
false
0
Polygon -7500403 true true 150 210 135 195 120 210 60 210 30 195 60 180 60 165 15 135 30 120 15 105 40 104 45 90 60 90 90 105 105 120 120 120 105 60 120 60 135 30 150 15 165 30 180 60 195 60 180 120 195 120 210 105 240 90 255 90 263 104 285 105 270 120 285 135 240 165 240 180 270 195 240 210 180 210 165 195
Polygon -7500403 true true 135 195 135 240 120 255 105 255 105 285 135 285 165 240 165 195

line
true
0
Line -7500403 true 150 0 150 300

line half
true
0
Line -7500403 true 150 0 150 150

pacman
true
0
Circle -7500403 true true 0 0 300
Polygon -16777216 true false 105 0 148 149 195 0

pacman open
true
0
Circle -7500403 true true 0 0 300
Polygon -16777216 true false 255 0 149 152 45 0

pellet
true
0
Circle -7500403 true true 105 105 92

pentagon
false
0
Polygon -7500403 true true 150 15 15 120 60 285 240 285 285 120

person
false
0
Circle -7500403 true true 110 5 80
Polygon -7500403 true true 105 90 120 195 90 285 105 300 135 300 150 225 165 300 195 300 210 285 180 195 195 90
Rectangle -7500403 true true 127 79 172 94
Polygon -7500403 true true 195 90 240 150 225 180 165 105
Polygon -7500403 true true 105 90 60 150 75 180 135 105

plant
false
0
Rectangle -7500403 true true 135 90 165 300
Polygon -7500403 true true 135 255 90 210 45 195 75 255 135 285
Polygon -7500403 true true 165 255 210 210 255 195 225 255 165 285
Polygon -7500403 true true 135 180 90 135 45 120 75 180 135 210
Polygon -7500403 true true 165 180 165 210 225 180 255 120 210 135
Polygon -7500403 true true 135 105 90 60 45 45 75 105 135 135
Polygon -7500403 true true 165 105 165 135 225 105 255 45 210 60
Polygon -7500403 true true 135 90 120 45 150 15 180 45 165 90

scared
false
0
Circle -13345367 true false 61 30 179
Rectangle -13345367 true false 60 120 240 232
Polygon -13345367 true false 60 229 60 284 105 239 149 284 195 240 239 285 239 228 60 229
Circle -16777216 true false 81 78 56
Circle -16777216 true false 155 80 56
Line -16777216 false 137 193 102 166
Line -16777216 false 103 166 75 194
Line -16777216 false 138 193 171 165
Line -16777216 false 172 166 198 192

square
false
0
Rectangle -7500403 true true 30 30 270 270

square 2
false
0
Rectangle -7500403 true true 30 30 270 270
Rectangle -16777216 true false 60 60 240 240

star
false
0
Polygon -7500403 true true 151 1 185 108 298 108 207 175 242 282 151 216 59 282 94 175 3 108 116 108

target
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240
Circle -7500403 true true 60 60 180
Circle -16777216 true false 90 90 120
Circle -7500403 true true 120 120 60

tree
false
0
Circle -7500403 true true 118 3 94
Rectangle -6459832 true false 120 195 180 300
Circle -7500403 true true 65 21 108
Circle -7500403 true true 116 41 127
Circle -7500403 true true 45 90 120
Circle -7500403 true true 104 74 152

triangle
false
0
Polygon -7500403 true true 150 30 15 255 285 255

triangle 2
false
0
Polygon -7500403 true true 150 30 15 255 285 255
Polygon -16777216 true false 151 99 225 223 75 224

truck
false
0
Rectangle -7500403 true true 4 45 195 187
Polygon -7500403 true true 296 193 296 150 259 134 244 104 208 104 207 194
Rectangle -1 true false 195 60 195 105
Polygon -16777216 true false 238 112 252 141 219 141 218 112
Circle -16777216 true false 234 174 42
Rectangle -7500403 true true 181 185 214 194
Circle -16777216 true false 144 174 42
Circle -16777216 true false 24 174 42
Circle -7500403 false true 24 174 42
Circle -7500403 false true 144 174 42
Circle -7500403 false true 234 174 42

turtle
true
0
Polygon -10899396 true false 215 204 240 233 246 254 228 266 215 252 193 210
Polygon -10899396 true false 195 90 225 75 245 75 260 89 269 108 261 124 240 105 225 105 210 105
Polygon -10899396 true false 105 90 75 75 55 75 40 89 31 108 39 124 60 105 75 105 90 105
Polygon -10899396 true false 132 85 134 64 107 51 108 17 150 2 192 18 192 52 169 65 172 87
Polygon -10899396 true false 85 204 60 233 54 254 72 266 85 252 107 210
Polygon -7500403 true true 119 75 179 75 209 101 224 135 220 225 175 261 128 261 81 224 74 135 88 99

wheel
false
0
Circle -7500403 true true 3 3 294
Circle -16777216 true false 30 30 240
Line -7500403 true 150 285 150 15
Line -7500403 true 15 150 285 150
Circle -7500403 true true 120 120 60
Line -7500403 true 216 40 79 269
Line -7500403 true 40 84 269 221
Line -7500403 true 40 216 269 79
Line -7500403 true 84 40 221 269

x
false
0
Polygon -7500403 true true 270 75 225 30 30 225 75 270
Polygon -7500403 true true 30 75 75 30 270 225 225 270

@#$#@#$#@
NetLogo 4.0.4
@#$#@#$#@
new
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
default
0.0
-0.2 0 0.0 1.0
0.0 1 1.0 0.0
0.2 0 0.0 1.0
link direction
true
0
Line -7500403 true 150 150 90 180
Line -7500403 true 150 150 210 180

@#$#@#$#@
