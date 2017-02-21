turtles-own [ home-pos ]
patches-own [ pellet-grid? ]  ;; true/false: is a pellet here initially?

breed [ pellets pellet ]
pellets-own[powerup? ]

breed [ bonuses bonus ]
bonuses-own [ value countdown ]

breed [ pacmans pacman ]
pacmans-own  [ new-heading ]

breed [ghosts ghost]
ghosts-own[eaten?]

breed [ nodes node]
nodes-own[visited? ]
globals [
          ;; current level
  score         ;; your score
  lives         ;; remaining lives
  extra-lives   ;; total number of extra lives you've won
  scared        ;; time until ghosts aren't scared (0 means not scared)
  level-over?   ;; true when a level is complete
  dead?         ;; true when Pacman is still alive
  next-bonus-in ;; time until next bonus is created
  tool which-ghost ;; variables needed to properly load levels 4 and above.
  difficulty
  i
  pacmanx
  pacmany
  nodenumber
  nodex
  nodey
]

;;;;;;;;;;;;;;;;;;;;;;;;
;;; Setup Procedures ;;;
;;;;;;;;;;;;;;;;;;;;;;;;

to new  ;; Observer Button
  clear-all
  load-map
  set score 0
  set level-over? false
  reset-ticks
  ask pacmans [ set new-heading 90 ]
  set i 1
  set nodenumber 2
end

to load-map  ;; Observer Procedure
  ;; Filenames of Level Files
  let maps ["pacmap1.csv" "pacmap2.csv" "pacmap3.csv"
            "pacmap4.csv" "pacmap5.csv"]
  let current-score score
  let current-extra-lives extra-lives
 ; let current-difficulty difficulty

  ifelse ((level - 1) < length maps)
  [ import-world item (level - 1) maps
    set score current-score
    set extra-lives current-extra-lives
    set difficulty 4
    set dead? false
    ask pacmans
    [ set home-pos list xcor ycor
    set pacmanx xcor
    set pacmany ycor]
    create-nodes 1[
      setxy pacmanx pacmany
      ;set hidden? true
    ]
  ]
  [ set level 1
    load-map ]
end

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Runtime Procedures ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;

 ;depth first search
to-report depth-first-search [x y nodenumber1]
  ;let root-node node 2
  let current-node node nodenumber1

  let path []
  let stack[]
  show current-node
  ask current-node[
    set visited? true
  ]
  create-nodes 1 [
    set xcor [x] of current-node
    set ycor [y] of current-node
    set size 0.5
    set color green
    set shape "circle"
  ]
  set path lput current-node path
  if(current-node != pellet 1)[
    ask nodes [
      if([pcolor] of patch-at-heading-and-distance 90 1 = black ) [
        if not visited?[
          set stack lput nodenumber stack
          set nodenumber nodenumber + 1
          show stack
        ]
      ]
      if([pcolor] of patch-at-heading-and-distance 0 1 = black ) [
        if not visited?[
          set stack lput nodenumber stack
          set nodenumber nodenumber + 1
          show stack
        ]
      ]
      if([pcolor] of patch-at-heading-and-distance 270 1 = black ) [
        if not visited?[
          set stack lput nodenumber stack
          set nodenumber nodenumber + 1
          show stack
        ]
      ]
      if([pcolor] of patch-at-heading-and-distance 180 1 = black ) [
        if not visited?[
          set stack lput nodenumber stack
          set nodenumber nodenumber + 1
          show stack
        ]
      ]
      set current-node first stack
      show stack
      show current-node
    ]
;    foreach [ sort out-link-neighbors] of current-node[
;      if not visited?[
;        set stack lput self stack
;        set nodenumber nodenumber + 1
;        show stack
;        set stack but-last stack
;        set current-node first stack
;
;        ask current-node[
;          set nodex xcor
;          set nodey ycor
;        ]
;
;      ]
;
;    ]
    if(depth-first-search(nodex)(nodey)(nodenumber))[
      report true
    ]
  ]

    report false


;  if ( number > 0 ) [
;    create-pointers 1 [
;      set xcor [x] of current-node
;      set ycor [y] of current-node
;      set size 0.5
;      set color green
;      set shape "circle"
;    ]
;  ]

;  set stack lput root-node stack
;  show stack
;
;  while[length stack > 0] [
;    set current-node first stack
;    set stack but-first stack
;    show  current-node
;    ifelse current-node = pellet 1 [
;      ask pacmans [
;        move-to turtle 1
;      ]
;      report false
;
;    ][
;      foreach [sort out-link-neighbors] of current-node [
;        ;show ?
;        ;  ask ?[
;        if not visited [
;          set stack lput self stack
;          set number number + 1
;          show stack
;          ; ]
;        ]
;      ]
;      ask current-node [
;        set visited true
;      ]
;    ]
    ;    ask pacmans [
    ;      if([pcolor] of patch-at-heading-and-distance 90 1 = black ) [
    ;        set stack lput number stack
    ;        set number number + 1
    ;        show stack
    ;      ]
    ;      if([pcolor] of patch-at-heading-and-distance 0 1 = black ) [
    ;        set stack lput number stack
    ;        set number number + 1
    ;        show stack
    ;      ]
    ;      if([pcolor] of patch-at-heading-and-distance 270 1 = black ) [
    ;        set stack lput number stack
    ;        set number number + 1
    ;        show stack
    ;      ]
    ;      if([pcolor] of patch-at-heading-and-distance 180 1 = black ) [
    ;        set stack lput number stack
    ;        set number number + 1
    ;        show stack
    ;      ]
    ;]

 ; ]



  ; report false
end

to-report dfs
  ifelse (depth-first-search (pacmanx)(pacmany)(2)) [
    show pacmanx
    show pacmany
    show "searching for dots"
    report true
  ][
    show "route finded"
    report false
  ]
end


to play  ;; Observer Forever Button
  ;; Only true at this point if you died and are trying to continue
  ifelse(dfs)[
    show "yes"
  ]
  [stop]
  if dead?
  [stop]
  every (1 - difficulty / 5)[
    if ( level != 4 )[
     ; dfs
      ;ask pacmans
    ;move_to_next_dot]
    ]
  ]
  if floor (score / 35000) > extra-lives
  [ set lives lives + 1
    set extra-lives extra-lives + 1 ]
  if dead?
  [ ifelse lives = 0
    [ user-message word "Game Over!\nScore: " score ]
    [ set lives lives - 1
      ifelse lives = 0
      [ user-message "You died!\nNo lives left." ]
      [ ifelse lives = 1
        [ user-message "You died!\nOnly 1 life left." ]
        [ user-message (word "You died!\nOnly " lives " lives left.") ]
      ]
      ask pacmans
      [ setxy (item 0 home-pos) (item 1 home-pos)
        set heading 0
      ]
      set dead? false
    ]
    stop
  ]
  if level-over?
  [ user-message word "Level Complete!\nScore: " score  ;; \n means start a new line
    stop ]

;  every next-bonus-in
;  [ make-bonus ]
  display
end

to move_to_next_dot;Reflexny agent
  ask pacmans
  [ ;; move forward unless blocked by wall
    let old-heading heading
    set heading new-heading
    if[pcolor] of patch-ahead 1 = black[
      ifelse any? pellets-on patch-ahead 1[
        fd 1
        consume
      ]
      [set i i + 1]
    ]
    ;if[pcolor] of patch-ahead 1 != black
    ;[
    ask patch-at-heading-and-distance 90 i [
      if any? pellets-here
        [ move-right ]
    ]
    ask patch-at-heading-and-distance 0 i [
      if any? pellets-here
        [ move-up ]
    ]
    ask patch-at-heading-and-distance 180 i [
      if any? pellets-here
        [ move-down ]
    ]
    ask patch-at-heading-and-distance 270 i [
      if any? pellets-here
        [ move-left ]
    ]
    ;; Level ends when all pellets are eaten
    if not any? pellets
    [ set level-over? true ]
    ;; Animation
    ifelse shape = "pacman"
    [ set shape "pacman open" ]
    [ set shape "pacman" ]
    ;;Set score by Time
    if not any? pellets-here
    [set score score - 1 ]
    ; if not any? pellets-on patch-ahead 1
    ; [set i i + 1]
    if score <= -10
    [set dead? true]
  ]
end

to consume  ;; Pacman Procedure
  ;; Consume Pellets
  if any? pellets-here
;  [ ifelse [powerup?] of one-of pellets-here
;    [ set score score + 500
;      set scared 40
;      ask ghosts
;      [ if not eaten?
;        [ set shape "scared" ] ]
;    ]
    [ set score score + 10 ]
    ask pellets-here [ die ] ;]
end


;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Interface Procedures ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;
to-report can-move-up
   ifelse ([pcolor] of patch-at-heading-and-distance 0 1 = black)[
     report true]
  [
   report false
  ]

end
to-report can-move-right
  ifelse ([pcolor] of patch-at-heading-and-distance 90 1 = black)[
    report true
  ][
    report false
  ]
end
to-report can-move-left
   ifelse ([pcolor] of patch-at-heading-and-distance 270 1 = black)[
     report true
   ][
     report false
   ]
end
to-report can-move-down
   ifelse ([pcolor] of patch-at-heading-and-distance 180 1 = black)[
     report true
   ][
     report false
   ]
end
to move-up
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

; Copyright 2001 Uri Wilensky.
; See Info tab for full copyright and license.
@#$#@#$#@
GRAPHICS-WINDOW
243
10
398
166
-1
-1
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
-3
3
-3
3
0
0
0
ticks
30.0

MONITOR
111
19
221
76
Score
score
0
1
14

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
1

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
0

INPUTBOX
48
18
99
78
Level
4.0
1
0
Number

@#$#@#$#@
## WHAT IS IT?

This is the classic arcade game, Pac-Man.  The game involves navigating Pac-Man through a maze.  Your objective is that Pac-Man eat all of the pellets (white circles), while avoiding the ghosts that pursue him.

If a ghost ever catches Pac-Man then Pac-Man is defeated.  If this occurs, the level will reset, but this will happen only if Pac-Man still has some lives remaining. (The pellets already collected on the level remain collected.)

However, when Pac-Man eats a Power-Pellet (large white circle) he can turn the tide, and the ghosts will turn scared and flee from him, for with the power of the Power-Pellet, Pac-Man can eat the ghosts!  Once a ghost is eaten it will return to its base, where it is born again, immune to the Power-Pellet until Pac-Man can find a new one to consume.  Pac-Man had better do just that, because unfortunately, the power of the Power-Pellet does not last forever, and will begin to wear off over time. (You will see the ghosts start to flash back to their normal appearance during the last few seconds of the Power-Pellet's effectiveness.)

Finally, occasionally a bonus (rotating star) will appear in the maze.  This bonus gives Pac-Man extra points if he eats it, but it will disappear if Pac-Man doesn't get it within a limited amount of time.

## HOW TO USE IT

Monitors
-- SCORE shows your current score.  You get points for collecting pellets, eating ghosts, and collecting bonuses.  You will get an extra life after every 35,000 points.
-- LEVEL shows your current level.  Each level has a different map, if you complete all the maps, it will loop back to the first map and continue.
-- LIVES shows how many extra lives you have remaining.  If you are defeated by a ghost when this is at 0, the game is over.

Sliders
-- DIFFICULTY controls the speed of the game.  Lower numbers make both the ghosts and Pac-Man move slowly, giving you more time to react as you play.

Buttons
-- NEW sets up a new game on level 1, with 3 lives, and a score of 0.
-- PLAY begins the game.  The game will pause after each level, so you will need to hit PLAY again after each level to continue.

Controls
-- UP, DOWN, LEFT, RIGHT control the direction Pac-Man moves.

## THINGS TO NOTICE

If you go off the edge of the maze you will wrap around to the other side.

Identifying Things in the Maze:
-- Yellow Circle with a mouth:  This is Pac-Man - you.
-- White Circles:
               These are Pellets - Collect all of these (including the Power-Pellets) to move on to the next level.

-- Large White Circles:
         These are Power-Pellets - They allow you to eat the Ghosts for a limited ammount of time.

-- Blue Squares:
                These are the walls of the maze - Neither Pac-Man nor the Ghosts can move through the walls.

-- Gray Squares:
                These are the Ghost Gates - Only Ghosts can move through them, and if they do so after having been eaten they will be healed.

-- Rotating Colored Stars:
      These are Bonus Stars - They give you extra points when you eat them.

-- Colorful Ghost with Eyes:
    These are the active Ghosts - Watch out for them!

-- Blue Ghost Shape:
            These are the scared Ghosts - Eat them for Extra Points!

-- Two Small Eyes:
              These are the Ghosts after they've been eaten - They will not affect you, and you can't eat them again, so just ignore them, but try not to be near its base when it gets back there.

Scoring System
-- Eat a Pellet:
       100 Points

-- Eat a Power-Pellet: 500 Points
-- Eat a Scared Ghost: 500 Points
-- Eat a Bonus Star:   100-1000 Points (varies)

## THINGS TO TRY

Beat your Highest Score.

Can you write an automated program for Pac-Man that will get him safely through the maze and collect all the pellets?

## EXTENDING THE MODEL

Think of other power-ups or bonuses that might be fun to have and make them appear randomly in the maze.

Add new enemies that behave differently from the ghosts.

## NETLOGO FEATURES

This model makes use of breeds, create-<breed>, every, and user-message.

The "import-world" command is used to read in the different maze configurations (levels).

## HOW TO CITE

If you mention this model or the NetLogo software in a publication, we ask that you include the citations below.

For the model itself:

* Wilensky, U. (2001).  NetLogo Pac-Man model.  http://ccl.northwestern.edu/netlogo/models/Pac-Man.  Center for Connected Learning and Computer-Based Modeling, Northwestern University, Evanston, IL.

Please cite the NetLogo software as:

* Wilensky, U. (1999). NetLogo. http://ccl.northwestern.edu/netlogo/. Center for Connected Learning and Computer-Based Modeling, Northwestern University, Evanston, IL.

## COPYRIGHT AND LICENSE

Copyright 2001 Uri Wilensky.

![CC BY-NC-SA 3.0](http://ccl.northwestern.edu/images/creativecommons/byncsa.png)

This work is licensed under the Creative Commons Attribution-NonCommercial-ShareAlike 3.0 License.  To view a copy of this license, visit https://creativecommons.org/licenses/by-nc-sa/3.0/ or send a letter to Creative Commons, 559 Nathan Abbott Way, Stanford, California 94305, USA.

Commercial licenses are also available. To inquire about commercial licenses, please contact Uri Wilensky at uri@northwestern.edu.

This model was created as part of the projects: PARTICIPATORY SIMULATIONS: NETWORK-BASED DESIGN FOR SYSTEMS LEARNING IN CLASSROOMS and/or INTEGRATED SIMULATION AND MODELING ENVIRONMENT. The project gratefully acknowledges the support of the National Science Foundation (REPP & ROLE programs) -- grant numbers REC #9814682 and REC-0126227.

<!-- 2001 -->
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
Polygon -16777216 true false 105 -15 150 150 195 -15

pacman open
true
0
Circle -7500403 true true 0 0 300
Polygon -16777216 true false 270 -15 149 152 30 -15

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
NetLogo 6.0
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
0
@#$#@#$#@
