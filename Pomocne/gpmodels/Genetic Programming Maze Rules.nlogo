breeds [ codeturtles ]

globals 
[
  TYPE_COMMANDBLOCK
  TYPE_COMMAND
  TYPE_OPERATOR
  TYPE_REPORTER
  
  RTYPE_ANY ; used for functions like "tree-count-nodes", to match any node return type
  RTYPE_VOIDBLOCK
  RTYPE_VOID
  RTYPE_NUMERIC
  RTYPE_BOOLEAN
  
  gp-syntaxlist-void
  gp-syntaxlist-numeric
  gp-syntaxlist-boolean
  
  gp-randseed
  gp-generation
  gp-best-fitness-this-gen
  ;; add your own global variables here...
]

codeturtles-own 
[
  gp-codetree
  gp-compiledcode 
  gp-fitness 
  gp-raw-fitness
  
  lastx
  lasty
  lasthead
  ;; add your own codeturtle variables here
]

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                   START OF GP LIBRARY CODE                         ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; IMPORTANT:  To create your own genetic programming model, you shouldn't need to 
;;    modify any of the GP Library Code.  Instead, you should just change the 
;;    following procedures, which are found after the end of the gp library section:
;;      gpuser-setup-syntax    (determines the ingredients for randomlly generated code trees)
;;      gpuser-run-codeturtles  (determines what happens to the codeturtles during a generation)
;;      gpuser-initialize-codeturtle  (run when each codeturtle that is created, sets initial variables, etc)
;;      gpuser-calculate-raw-fitness   (reports some measure analogous to "distance to goal", where 0 is perfection)
;;      gpuser-calculate-adjusted-fitness (reports a value between 0 and 1, where 1 is perfection)
;;
;;   In addition, your setup should call "gp-setup" and your go should call "gp-go"
;;
;; Notes
;;
;;  Structure of a tree node:
;;    ["NODE_VALUE"  RTYPE_*  TYPE_*  CHILD1  CHILD2  ... ]
;;

to gp-setup
  ;; these are just constants -- they could just as easily be set to unique integers
  ;; however, making them abbreviated strings makes debugging easer.
  set TYPE_COMMANDBLOCK "cb"
  set TYPE_COMMAND "c"
  set TYPE_OPERATOR "o"
  set TYPE_REPORTER "r"
  
  set RTYPE_ANY ""
  set RTYPE_VOIDBLOCK "vb"
  set RTYPE_VOID "v"
  set RTYPE_NUMERIC "n"
  set RTYPE_BOOLEAN "b"

  gpuser-setup-syntax

  set gp-randseed randomseed
  if (fix-random-seed?)
  [  random-seed gp-randseed ]
  create-custom-codeturtles population-size [
    set gp-codetree (gp-tree-random RTYPE_VOIDBLOCK initial-code-max-depth branch-chance)  
    set gp-compiledcode (gp-tree-compile gp-codetree)
    gpuser-initialize-codeturtle
    gp-calculate-fitness
  ]
end  

;; reports the sum of the weights from a weighted list...
to-report gp-weight-sum-from-weighted-list [ thelist onlyterminals? ]
  let thesum 0  
  foreach thelist
  [
    if (not onlyterminals? or (length ? <= 3))
      [ set thesum (thesum + (last ?)) ]
  ]
  report thesum
end

;; reports a random entry from a syntaxlist, where the chance of an entry being chosen
;;  is weighted by the number that is the last entry of the element
;;  if onlyterminals is true, then it only chooses from terminals
to-report gp-random-from-weighted-list [ thelist thesum onlyterminals?]

  if (thesum <= 0) ;; if it can't find any terminals in the list, then choose a random nonterminal
  [  
    if-else (onlyterminals?)
    [ report (gp-random-from-weighted-list thelist (gp-weight-sum-from-weighted-list thelist false) false) ] 
    [ report [] ] 
  ]
  
  let choice (random-float thesum)
  let partsum 0
  foreach thelist
  [
    if (not onlyterminals? or (length ? <= 3))
      [ set partsum (partsum + (last ?)) ]
    if (partsum >= choice)
       [ report ? ] 
  ]
end

;; creates a random code tree
;; -- used to generate the initial population, and to create mutations
to-report gp-tree-random [node_rtype depthlimit branch_chance ]
  if (node_rtype = RTYPE_VOIDBLOCK)
  [
    if (depthlimit <= 1)
      [ report (list "[]" RTYPE_VOIDBLOCK TYPE_COMMANDBLOCK) ]
    if (depthlimit <= 3)
      [ set branch_chance 0 ]
    ifelse (random-float 1.0 < branch_chance)
    [
      report (sentence (list "[]" RTYPE_VOIDBLOCK TYPE_COMMANDBLOCK )
        (n-values 2 [ (gp-tree-random RTYPE_VOIDBLOCK (depthlimit - 1) (branch_chance * .90)) ]))     
    ]
    [
      report (sentence (list "[]" RTYPE_VOIDBLOCK TYPE_COMMANDBLOCK )
        (n-values 3 [ (gp-tree-random RTYPE_VOID (depthlimit - 1) (branch_chance * .90)) ]))     
    ]
  ]
  
  let syntaxlist []
    
  ifelse (node_rtype = RTYPE_VOID)
  [  set syntaxlist gp-syntaxlist-void ]
  [ifelse (node_rtype = RTYPE_NUMERIC)
  [  set syntaxlist gp-syntaxlist-numeric ]
  [if (node_rtype = RTYPE_BOOLEAN)
  [  set syntaxlist gp-syntaxlist-boolean ]]]
  let onlyterminals? false
  if (depthlimit <= 1 or branch_chance < .01 )
    [ set onlyterminals? true ]
  let lRecord (gp-random-from-weighted-list syntaxlist (gp-weight-sum-from-weighted-list syntaxlist onlyterminals?) onlyterminals?)
  
  report (sentence (list (item 0 lRecord) node_rtype (item 1 lRecord))
    (n-values (length lRecord - 3) [ (gp-tree-random (item (? + 2) lRecord) (depthlimit - 1) branch_chance) ]))

end

;; recursive routine to compile a codetree into valid netlogo code that can be run.
to-report gp-tree-compile-rec [ tree parenttype indent]
  let node_type (item 2 tree)
  if (length tree <= 3)
  [
    ifelse (node_type = TYPE_COMMANDBLOCK and parenttype = TYPE_COMMANDBLOCK)
     [ report "" ]
     [ report (item 0 tree) ]
  ] 
  ifelse (node_type = TYPE_OPERATOR)
  [
    report "(" + gp-tree-compile-rec (item 3 tree) node_type (indent + 1) + " " + item 0 tree + " " 
               + gp-tree-compile-rec (item 4 tree) node_type (indent + 1) + ")"
  ][
  ifelse (node_type = TYPE_REPORTER)
  [
    report "(" + (item 0 tree) + " " + (reduce [ ?1 + " " + ?2 ] (map [gp-tree-compile-rec ? node_type (indent + 1)] (sublist tree 3 (length tree)))) + ")"
  ][
  ifelse (node_type = TYPE_COMMAND)
  [  
    report (item 0 tree) + " " + (reduce [ ?1 + " " + ?2 ] (map [gp-tree-compile-rec ? node_type (indent + 1)] (sublist tree 3 (length tree)))) + " "
  ][
  ifelse (node_type = TYPE_COMMANDBLOCK)
  [
    let indentstr "" ;; how far to indent the code, as a result of nested blocks.
    if (parenttype = TYPE_COMMANDBLOCK)
      [ set indent (indent - 1) ]
    if (indent > 0)
      [ set indentstr (reduce [ ?1 + ?2 ] (n-values indent [ " " ])) ]
    let compiledargs (map [gp-tree-compile-rec ? node_type (indent + 1) ] (sublist tree 3 (length tree)))
    ifelse (parenttype = TYPE_COMMANDBLOCK)
    [ report (reduce [ ?1 + "\n" + indentstr + ?2 ] compiledargs) ]
    [ report "[\n" + indentstr + (reduce [ ?1 + "\n" + indentstr + ?2 ] compiledargs) + "\n" + indentstr + "]" ]
  ][
    report []
  ]]]]

end

;; routine to compile a codetree into valid netlogo code that can be run.
to-report gp-tree-compile [ tree ]
  report gp-tree-compile-rec tree TYPE_COMMANDBLOCK 0
end

;; reports the number of nodes (with a given RTYPE) in a code tree
to-report gp-tree-count-nodes [ tree rtype-filter ]
  let childcount 0
  if (length tree > 3) ; if node has children
    [ set childcount (reduce [ ?1 + ?2 ] (map [gp-tree-count-nodes ? rtype-filter] (sublist tree 3 (length tree)))) ]
  report childcount + (ifelse-value (rtype-filter = RTYPE_ANY or rtype-filter = (item 1 tree)) [ 1 ] [ 0 ])
end

;; recursive routine to report a chosen (choice) node of a given RTYPE from a code tree
to-report gp-tree-getnode-rec [ tree choice rtype-filter counter]
  if (rtype-filter = RTYPE_ANY or rtype-filter = (item 1 tree))
  [
    if (counter = choice)
      [ report (list tree (counter + 1)) ]
    set counter counter + 1
  ]

  if (length tree <= 3) ; terminal
    [ report (list [] counter) ]
  
  let pair []
  let mysublist (sublist tree 3 (length tree))
  let n 0
  while [ n < length mysublist ]
  [
    set pair (gp-tree-getnode-rec (item n mysublist) choice rtype-filter counter)
    set counter (item 1 pair)
    if (counter > choice)
      [ report pair ]
    set n (n + 1)
  ]
  report (list [] counter)
end

;; routine to report a chosen (choice) node of a given RTYPE from a code tree
to-report gp-tree-getnode [ tree choice rtype-filter]
  report (item 0 (gp-tree-getnode-rec tree choice rtype-filter 0))
end

;; recursive routine that replaces a chosen node with a given subtree, and reports the resulting tree
to-report gp-tree-replacenode-rec [ tree choice subtree rtype-filter counter ]
  if (rtype-filter = RTYPE_ANY or rtype-filter = (item 1 tree))
  [
    if (counter = choice)
      [ report (list subtree (counter + 1)) ]
    set counter counter + 1
  ]
 
  if (length tree <= 3) ; terminal
    [ report (list tree counter) ]
  
  let resulttree (sublist tree 0 3)
  let pair []
  let mysublist (sublist tree 3 (length tree))
  let n 0
  while [ n < length mysublist ]
  [
    set pair (gp-tree-replacenode-rec (item n mysublist) choice subtree rtype-filter counter)
    set counter (item 1 pair)
    set resulttree (lput (item 0 pair) resulttree)
    set n (n + 1)
  ]
  
  report (list resulttree counter)
end

;; routine that replaces a chosen node with a given subtree, and reports the resulting tree
to-report gp-tree-replacenode [ tree choice subtree rtype-filter]
  report (item 0 (gp-tree-replacenode-rec tree choice subtree rtype-filter 0))
end

;; recursive routine that reports a mutated version of a code tree
to-report gp-tree-mutate-rec [ tree choice counter ]
  if (counter = choice)
    [ report (list (gp-tree-random (item 1 tree) (initial-code-max-depth * .50) (branch-chance * .75)) counter) ]

  if (length tree <= 3) ; terminal
    [ report (list tree counter)]
  
  let resulttree (sublist tree 0 3)
  let pair []
  let mysublist (sublist tree 3 (length tree))
  let n 0
  while [ n < length mysublist ]
  [
    set pair (gp-tree-mutate-rec (item n mysublist) choice (counter + 1))
    set counter (item 1 pair)
    set resulttree (lput (item 0 pair) resulttree)
    set n (n + 1)
  ]
  
  report (list resulttree counter)
end

;; routine that reports a mutated version of a code tree
to-report gp-tree-mutate [ tree ]
  let choice random (gp-tree-count-nodes tree RTYPE_ANY)
  report (item 0 (gp-tree-mutate-rec tree choice 0))
end

;; routine that reports a new tree, generated by crossing tree1 with tree2
to-report gp-tree-crossover [ tree1 tree2 ]
  let choice random (gp-tree-count-nodes tree1 RTYPE_ANY)
  let subtree (gp-tree-getnode tree1 choice RTYPE_ANY)
 ;;debug;; print "choice=" + choice + " subt=" + subtree
  let num-attach-points (gp-tree-count-nodes tree2 (item 1 subtree))
  if (num-attach-points <= 0)
    [ report tree2 ]
  let attach-choice random num-attach-points
  report (gp-tree-replacenode tree2 attach-choice subtree (item 1 subtree))
end

;; codeturtle procedure -- calculates the fitness,  first raw, and then adjusted.
to gp-calculate-fitness
  gpuser-calculate-raw-fitness
  gpuser-calculate-adjusted-fitness
end

;; routine that creates a new codetree, given all of the code from the 
to gp-breed-new-codeturtle [ lastgenlist weightsum ]
  let gen-op-choice random-float (clone-chance + mutate-chance + crossover-chance)
  
  let ct1 (first (gp-random-from-weighted-list lastgenlist weightsum false))
  
  ifelse (gen-op-choice < clone-chance)
  [
    create-custom-codeturtles 1 [
      gpuser-initialize-codeturtle
      set gp-codetree (gp-codetree-of ct1)
      set gp-compiledcode (gp-compiledcode-of ct1)
      set color color-of ct1
      ]
  ][
  ifelse (gen-op-choice < clone-chance + mutate-chance)
  [
    create-custom-codeturtles 1 [
      gpuser-initialize-codeturtle
      set gp-codetree (gp-tree-mutate (gp-codetree-of ct1))
      set gp-compiledcode (gp-tree-compile gp-codetree)
      ;;set color color-of ct1
      ]
  ][ 
    ;; otherwise, do crossover
    let ct2 (first (gp-random-from-weighted-list lastgenlist weightsum false))

    create-custom-codeturtles 1 [
      gpuser-initialize-codeturtle            
      set gp-codetree (gp-tree-crossover (gp-codetree-of ct1) (gp-codetree-of ct2))
      set gp-compiledcode (gp-tree-compile gp-codetree)
      let rgbvector (map [ (?1 + ?2) / 2 ] (extract-rgb (color-of ct1)) (extract-rgb (color-of ct2)))      
      set color rgb (item 0 rgbvector) (item 1 rgbvector) (item 2 rgbvector)
    ]
  ]]
end

;; routine that creates a new generation of codeturtles
to gp-create-next-generation
  set gp-generation (gp-generation + 1)
 ;; ask codeturtles [ gp-calculate-fitness ]
  let lastgenlist values-from codeturtles [ (list self gp-fitness) ]
  let popsize (count codeturtles)
  let weightsum (gp-weight-sum-from-weighted-list lastgenlist false)
  
  repeat popsize
  [
    gp-breed-new-codeturtle lastgenlist weightsum
  ]
  
  foreach lastgenlist
  [
    ask first ? [ die ]
  ]
    
end

;; This is the main "go" procedure for the gp-library.
;;   Run iteratively, this procedure performs genetic programming.
to gp-go

  if (fix-random-seed?)
  [ 
    set gp-randseed gp-randseed + 1
    random-seed gp-randseed 
  ]

  gp-create-next-generation
  gpuser-run-codeturtles
    
  ask codeturtles [ gp-calculate-fitness ]
  gp-plot
end

;; plot the fitnesses of the current generation, and update the gp-best-fitness-this-gen variable
to gp-plot
  let fitnesslist values-from codeturtles [ gp-raw-fitness ]
  let bestfitness min fitnesslist
  let avgfitness mean fitnesslist
  let worstfitness max fitnesslist
  set gp-best-fitness-this-gen bestfitness
  
  set-current-plot "Fitness Plot"
  set-current-plot-pen "avg"
  plot avgfitness
  set-current-plot-pen "best"
  plot bestfitness
  set-current-plot-pen "worst"
  plot worstfitness
end

;; show the behavior of the last generation of codeturtles again
to gp-replay
 
  ask codeturtles
  [
    let savetree gp-codetree
    let savecompiled gp-compiledcode
    let savecolor color
    pu
    gpuser-initialize-codeturtle
    set gp-codetree savetree
    set gp-compiledcode savecompiled
    set color savecolor
  ]
     
  if (fix-random-seed?)
  [ random-seed gp-randseed ]

  gpuser-run-codeturtles
    
  ask codeturtles [ gp-calculate-fitness ]
   
end

;; displays the code of the best-this-gen codeturtle in the output box
to gp-showbest 
  clear-output
  ask one-of (codeturtles with-min [ gp-raw-fitness ])
  [
    output-print gp-compiledcode
  ]
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                    END OF GP LIBRARY CODE                          ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;  BEGIN MAZE CODE ;;;;;

;; These are codeturtle procedures and reporters, that help form the DNA of the codeturtles.  
;; (See the gp-setup-syntax procedure below for exactly what the DNA is made of.)
;;
to-report maze-wall-ahead?
  report ((pcolor-of patch-ahead 1) = blue)
end
to-report maze-wall-right?
  report ((pcolor-of patch-right-and-ahead 90 1) = blue)
end
to-report maze-wall-left?
  report ((pcolor-of patch-left-and-ahead 90 1) = blue)
end

to maze-turn-right
  rt 90
end
to maze-turn-left
  lt 90
end

;; This moves a codeturtle forward one square in the maze, if there isn't a wall there.
to maze-forward
  if (not maze-wall-ahead?)
    [ fd 1 ]
end

to gpuser-setup-syntax
  ;; Void ingredients for codeturtle's random code generation
  ;; This list contains procedures (which do not report a value) that code turtles can use.
  ;; (They can be either built-in NetLogo primitives or user-defined procedures in this model)
  set gp-syntaxlist-void (list
  (list "maze-turn-right" TYPE_COMMAND 10)
  (list "maze-turn-left" TYPE_COMMAND 10)    
  (list "ifelse" TYPE_COMMAND RTYPE_BOOLEAN RTYPE_VOIDBLOCK RTYPE_VOIDBLOCK 10)
  (list " " TYPE_COMMAND 5) ;; NO OPERATION
  )
  
  ;; Numeric ingredients for codeturtle's random code generation
  ;; This list contains reporters and operators which return numeric values
  set gp-syntaxlist-numeric [ ]
  
  ;; Boolean ingredients for codeturtle's random code generation
  ;; This list contains reporters and operators which return boolean values
  set gp-syntaxlist-boolean (list
  (list "maze-wall-ahead?" TYPE_REPORTER 10)  
  (list "maze-wall-right?" TYPE_REPORTER 10)  
  (list "maze-wall-left?" TYPE_REPORTER 10)  
  )

end

;; whenever codeturtles are created (at the beginning of each generation) this procedure is run
to gpuser-initialize-codeturtle
    set size 1.5
    set gp-raw-fitness 1000  ; large number (very unfit)
    ; this color may not always stick, since codeturtles pass color 
    ; from one generation to the next, in a hereditary way (somewhat)
    set color (who * 10 - 5)
    set xcor (pxcor-of one-of patches with [ pcolor = green ] ) ; find the start patch
    set ycor (pycor-of one-of patches with [ pcolor = green ] )
    set heading 0
    ;; keep track of last xcor, ycor, and heading, so that we can kill off unproductive codeturtles early...
    set lastx -1
    set lasty -1
    set lasthead -1
    pd        
end

;; This procedure controls what the codeturtles do during a given generation.
;; 
to gpuser-run-codeturtles
  ask codeturtles 
  [
    let stepnum 0
    let stepmax 200
    while [ stepnum < stepmax ]
    [
      set stepnum (stepnum + 1)
      carefully [  run (gp-compiledcode) ] [ print error-message print gp-compiledcode ]
      maze-forward
      gp-calculate-fitness
      if (gp-raw-fitness = 0)
        [ set shape "face happy"  stop ]

      ;; these are all tricks for speeding up the model, by weeding out codeturtles that we
      ;; don't think have winning strategies, and keeping the hopeful ones going.
      if (stepnum = 200 and gp-raw-fitness < 32)
        [ set stepmax 400 ]
      if (stepnum = 400 and gp-raw-fitness < 16)
        [ set stepmax 600 ]

      if (xcor = lastx and ycor = lasty and (heading = lasthead or (pcolor = green and stepnum = 5)))
        [ set shape "face sad"  stop ]
      set lastx xcor
      set lasty ycor
      set lasthead heading
    ]
    set shape "face sad"    
  ]
end

;; codeturtles procedure
;; Think of this reporter as "how far am I from my goal?"
;; Thus, a perfect codeturtle has a raw fitness of 0
;;       a bad codeturtle has an arbitrarily large raw fitness
;; 
to gpuser-calculate-raw-fitness
  let dist (distance-nowrap (one-of patches with [pcolor = red ]))
  
  if (dist < gp-raw-fitness)
    [ set gp-raw-fitness dist ]
end

;; codeturtle procedure -- Adjusts raw fitness so that now good turtles have higher fitness, 
;;  and the differences between codeturtles' fitnesses are distributed beneficially.
;;  a perfect codeturtle has a fitness of 1, and a worthless codeturtle has a fitness of 0
;;  (these fitnesses are used as weights for a probabilistically choosing turtles to reproduce)
;;
to gpuser-calculate-adjusted-fitness 
  set gp-fitness 1 / (1 + gp-raw-fitness)
end


;; loads a maze from a file
;;
to load-maze [ file ]
    file-open file

    while [ not file-at-end? ]
      [ set pcolor-of patch file-read file-read file-read   ]

    file-close
end

to setup
  ca
  load-maze maze-file
  gp-setup
end

;; This GO just calls the gp-go library procedure.  However, if you have any custom code that you need
;;    to run before or after a generation goes by, you can put them here.
to go

  gp-go
  ;; if we are "close enough" to a perfect turtle, then stop.
  if (gp-best-fitness-this-gen < .01)   
    [ stop ]
end

@#$#@#$#@
GRAPHICS-WINDOW
318
10
678
391
17
17
10.0
1
10
1
1
1
0
1
1
1

CC-WINDOW
5
407
870
502
Command Center
0

SLIDER
688
88
860
121
branch-chance
branch-chance
0
1.0
0.5
0.01
1
NIL

SLIDER
688
50
861
83
initial-code-max-depth
initial-code-max-depth
2
20
7
1
1
NIL

SWITCH
694
311
846
344
fix-random-seed?
fix-random-seed?
0
1
-1000

SLIDER
693
349
845
382
randomseed
randomseed
1
50000
17835
1
1
NIL

BUTTON
8
17
71
50
NIL
setup\n
NIL
1
T
OBSERVER
T
NIL

SLIDER
688
146
860
179
clone-chance
clone-chance
0
100
20
1
1
NIL

SLIDER
687
226
859
259
crossover-chance
crossover-chance
0
100
70
1
1
NIL

SLIDER
688
186
860
219
mutate-chance
mutate-chance
0
100
10
1
1
NIL

SLIDER
688
10
860
43
population-size
population-size
1
100
50
1
1
NIL

BUTTON
167
69
233
102
NIL
go
T
1
T
OBSERVER
T
NIL

BUTTON
7
68
70
101
step
go
NIL
1
T
OBSERVER
T
NIL

PLOT
8
118
311
268
Fitness Plot
generations
raw fitness
0.0
20.0
0.0
40.0
true
true
PENS
"worst" 1.0 0 -2674135 true
"avg" 1.0 0 -11221820 true
"best" 1.0 0 -8630108 true

MONITOR
246
287
303
336
best
gp-best-fitness-this-gen
3
1

CHOOSER
81
10
200
55
maze-file
maze-file
"maze0.txt" "maze1.txt" "maze2.txt" "maze3.txt" "maze4.txt"
2

MONITOR
254
62
311
111
gen #
gp-generation
3
1

OUTPUT
8
276
235
393

BUTTON
239
348
315
381
show-best
gp-showbest
NIL
1
T
OBSERVER
T
NIL

BUTTON
81
69
159
102
replay-step
gp-replay
NIL
1
T
OBSERVER
T
NIL

@#$#@#$#@
WHAT IS IT?
-----------
This model demonstrates the use of genetic programming to evolve movement rules that agents can use to solve mazes.  Genetic programming (sometimes abbreviated GP) is a technique in Computer Science that uses the concepts of natural selection, genetics, and evolution to generate computer programs to solve a particular problem.  In this case, the problem is navigating mazes.  (This model is similar to Genetic Programming Maze Marchers, but instead of evolving the steps an agent should take to solve a particular maze, this model evolve movement rules that are applicable to solving mazes in general.)

WHAT IS GENETIC PROGRAMMING
------------
In the world of biology, species evolve by means of natural selection and the interactions of DNA.  Genetic Programming takes these ideas, and applies them in the field of computer science.  Given a problem, the goal is to evolve a computer program that can solve the problem.  It starts with a population of randomly generated computer programs.  (The ingredients of these randomly generated computer programs are chosen based on the problem that is to be solved.)  Each program is run, and its performance is measured by a "fitness function", which reflects how good each program is at solving the problem.  Then programs are chosen from the population to "reproduce".  The programs are chosen randomly, but with a weighting mechanism that makes it more likely that the more "fit" programs are chosen.  (Analogously in the biological world, defective organisms can get lucky and pass on their DNA, but the more fit organisms have a better chance of doing so.)  There are three forms of reproduction that occur:

  * Cloning (the child program is identical to its parent)
  * Mutation (the child program has some of its code replaced by randomly generated code)
  * Crossover (two parent programs are chosen from the population, and the child program consists of a mixture of code from the two parents)

After a full new population of programs is formed, the old population is discarded, and each of the new programs is run and their fitness is measured.  Reproduction occurs, and the cycle continues, until a program with a certain level of fitness is found -- namely, a program that is good enough to solve the problem given.  The word "until" implies that such a state will be reached.  This isn't necessarily true.  For one thing, the problem posed might be an impossible one (e.g. "What is the answer to life the universe and everything?").  Or it could be solvable, but not with the ingredients that the programs are made from (e.g. "What is the solution to a quadratic equation?" with programs made only of "+" and "-" operators.)  But even if the solution is within the realm of possible programs generated by the genetic programming process, success is by no means guaranteed.  Genetic Programming is a stochastic, rather than deterministic, approach to solving problems.  Currently there are, in fact, no proofs that genetic programming works -- merely empirical evidence showing that in some situations it does.  The success of the genetic programming process is highly dependent on the choice of ingredients for the programs and a well designed "fitness function" that "leads" the population in the right direction toward the goal.  Other important parameters include the size of the population, the length of each program's code, and the relative probability of each of the reproduction mechanisms.  For more information on genetic programming, see these web sites:

http://www.geneticprogramming.com/Tutorial/index.html
http://www.genetic-programming.com/gpanimatedtutorial.html

HOW IT WORKS
------------
In many NetLogo models, agents are given predetermined rules, and then the emergent behaviors that form through the interactions of these agents are studied.  In contrast, this model is starts with a desired behavior (solving a maze) and works on trying to discover the agent's rules, through use of Genetic Programming, as described in the section above.

In this model, the maze-solving programs are represented by "codeturtles".  Codeturtles each have a piece of NetLogo code assigned to them, and it's their job to perform it.  Codeturtles will then be chosen, based on their fitness, to reproduce and create another generation of codeturtles.

The ingredients from which the code is built are fairly simple:

Four commands:  
* maze-turn-right  (think "rt 90")
* maze-turn-left   (think "lt 90")
* ifelse control structure
* "  "  blank command (does nothing)

Three reporters:
  maze-wall-ahead?   (Is there a wall in front of me?)
  maze-wall-right?   (Is there a wall to my right?)
  maze-wall-left?    (Is there a wall to my left?)

Thus, a small example program might look like:

| maze-turn-right
| ifelse maze-wall-ahead? [
|  maze-turn-left
|  ] [
|  maze-turn-right
|  ifelse maze-wall-right? [
|   maze-turn-left
|   maze-turn-left
|   ] [
|   maze-turn-left
|      
|   ]   
|   maze-turn-right
|  ] 

(The internal representation of the program is actually a tree structure, since this has been often found to produce better results for genetic programming.  The code trees are then compiled into the form you see above, to be run as NetLogo code.)

You may be wondering about the blank command.  Since it does nothing, what purpose could it possibly have in the program ingredients?  Basically, it provides a placeholder -- for instance, you can have an "if" without an "else", simply by having the else block be made up of blank commands.

You may also be wondering how it is the codeturtles move, since they have no "forward" command.  This is because each codeturtle's program is just the rules to decide where to go in each given step.  Codeturtles have a lifespan of 480 steps (enough to get them to the goal in most mazes, if they have a decent movement strategy).  During each step, the codeturtles execute their code, and then move forward one square in the direction they are pointing (unless a wall blocks their path).

The fitness function, which measures each codeturtles progress, is a simple one: "What is the geometric distance to the goal square?"  The lower this number is, the more fit a turtle is.  It is easy, of course, to create mazes where this is clearly not the case (e.g. load "maze4.txt")  but for many mazes, this distance measurement serves as a decent heuristic.  A better (in fact, perfect) fitness function would count the minimum number of open path squares that the codeturtle would have to cross to reach the goal.  However, if we had such a fitness function, then we would already have some algorithm for computing the solution to our maze!  And if we had such an algorithm, then why would we be trying to evolve codeturtles to solve it?  Using the solution to find the solution seems like a cheap trick that turns this model entirely into a toy problem.   Also, fitness is computed for each codeturtle after each step it takes -- not just at the end of the run.  Since fitness is calculated so often, the efficiency of computing fitness is important, and this is another advantage for geometric distance fitness, as opposed to true walking distance to goal fitness.

HOW TO USE IT
-------------

1. Set the MAZE-FILE chooser to the maze file you want to use.
2. Adjust the slider parameters (see below), or use the default settings.
3. Press the SETUP button.
4A. Press the GO button to begin the simulation 
4B. Press the STEP button to go through one generation at a time.
5. Watch the View, to see the codeturtles attempt to solve the maze with their given code DNA.
6. Watch the FITNESS plot, to see how the population is doing.
7. If a codeturtle successfully gets to the end of the maze, then the simulation will stop.  To stop it earlier, press the GO button again.  (It may take some time for the current generation to finish.)
8. Press the REPLAY-STEP button to watch the last generation run through the maze again.
9. Press the SHOW-BEST button to see the code for the current best (most fit) codeturtle.

Parameters:

MAZE-FILE: The maze file to be loaded.
POPULATION-SIZE: The number of codeturtles in each generation
INITIAL-CODE-MAX-DEPTH: The maximum depth of randomly generated code trees, that codeturtles start with.  (It also affects the size of mutations that occur).  In general, a larger number generally means longer programs are created.
BRANCH-CHANCE: Controls the amount of branching in the generation of random code trees.  Again, a larger number generally means longer programs are created.

CLONE-CHANCE, MUTATE-CHANCE, CROSSOVER-CHANCE:  These three sliders control the relative probability of each genetic operation occurring, with respect to the others. 
(Examples:  If the sliders are set to 10, 10, 10, then there is a 1/3 chance of each genetic operation happening.  If the sliders are set to 10, 10, 20, then there is a 25% chance of cloning, 25% chance of mutation, and 50% chance of crossover.  If the sum of these three sliders is 100, then each slider represents the percent chance of that genetic operation being chosen to create an offspring for the new generation.)

FIX-RANDOM-SEED?: If true, then RANDOMSEED is used to start the process.  This allows a particular run to be reproduced exactly, and thus examined more closely, (provided that the parameters are the same).  If false, then RANDOMSEED is not used.

RANDOMSEED:  This is the number used to seed the random number generator, if FIX-RANDOM-SEED? is true, to allow for reproducible results.


THINGS TO NOTICE
----------------

For humans, some mazes are easier to solve than others.  Likewise, some mazes are easier for this model to solve than others.  Which of the five included mazes are easiest for this model, and which are hardest?  Why might maze0.txt be easier than maze3.txt?  Think about the fitness function, as well as other factors.

The average and best fitness values shown in the plot sometimes go up and sometimes go down.  Why do you think this is?  Does genetic programming always find a solution?  

Usually the best fitness value makes a sudden jump down to the solution at the end.  Why is this?  Why aren't there codeturtles that get within 2 or 3 squares of the goal, but don't actually make it all the way?

Occasionally, a codeturtle that was in a very early generation, maybe even Generation 0, finds the solution.  What do you think this says about the difficulty of the problem?  Do you think that genetic programming is a good choice for solving this problem?

You may notice that during a generation, after a certain number of steps, many of the codeturtles have turned to sad faces, while the turtles that are still moving will speed up.  There is a reason for this.  Because the genetic programming process runs quite slowly, especially with large populations, some heuristics are applied in this model to stop codeturtles that are looking hopeless.  For instance, if a codeturtle doesn't move or change its heading in a given step, then it is stuck, (because it will do the same thing next turn) and we do not need to keep running it.  Weeding out bad turtles helps speed up the model.

The colors of the codeturtles have some meaning.  They are initially randomly colored, but:
* When a codeturtle results from cloning, it has the same color as its parent.
* When a codeturtle results from crossover, it has a color averaging its two parents.
* When a codeturtle is mutated, it has a random color.


THINGS TO TRY
-------------
Try changing the POPULATION-SIZE slider.  With a very small population, each generation moves by much more quickly, but it generally takes more generations to find a solution.  Also, small populations mean increased inbreeding.  What affect does this have on the process?  How low can the population go such that the process still works?

Try changing the INITIAL-CODE-MAX-DEPTH and the BRANCH-CHANCE.  Note that if INITIAL-CODE-MAX-DEPTH <= 3, then IFELSE statements can't form, meaning that the codeturtles are doomed.  Note also that if the codeturtles' code gets long, then the codeturtles run very slowly.

Crossover is usually the driving force of genetic programming.  Try moving the genetic operations sliders around, and run the model.  What happens if you only cloning, and no mutation or crossover?  Only mutation?  Only crossover?


EXTENDING THE MODEL
-------------------

There is a model called Genetic Programming Maze Maker, which can be used to create maze files.  Create several interesting maze files, and add them to the MAZE-FILE chooser.  What types of mazes does the model do well with?  What types of mazes are hard for the model to handle?  What types are impossible?

Sometimes over the generations, the code trees expand and get very large.  (In crossover, a new codeturtle can be made up of the larger part of its two parents, and basically double in size.  In mutation, a single node can be replaced by a medium-sized subtree.)  If one of these large-tree codeturtles is highly fit, the largeness can quickly spread to most of the population.  This can result in some incredibly slow performance for the model when this happens.  Thus, a nice extension to the GP Library would be to have a "maximum-tree-size" parameter, and when trees that are too large get created, they should be trimmed off somehow.

Right now, these turtles have no memory of where they've been.  They can only decide which direction to move based on which squares around them have adjacent walls.  This really only gives them one solution they can find -- the well known "right hand rule" (or the "left hand rule", of course).  (It is worth noting that this rule only works on a certain class of mazes.)  In any case, it would be interesting to give the codeturtles more information to base their decisions on.  Consider two reporters called "maze-already-traveled-ahead?" and "maze-already-traveled-here" which report true if the turtle has already traveled on the square they see in front of them, or the square they are currently on.  Would these additions be useful?  Would it be possible to give the turtles primitives such that they could learn to do a depth first search?  Or come up with your own codeturtle primitives, and see whether they help or hurt the efficiency of finding a solution.

NETLOGO FEATURES
----------------

The NetLogo feature on which this whole model stands is the ability to take a string of text, and run it as NetLogo code.  This is achieved through the "run" primitive.

Extensive use of recursion and lists has been employed, especially to deal with the tree structures which codeturtles use to store code.  Since trees are not natively supported in NetLogo, they have been implemented as nested lists.

It is also interesting to note that this model is built from two parts.  The first part is the "GP Library for NetLogo", which consists of a framework of procedures that are useful for any model that is using genetic programming.  The second part consists of procedures that are specific to this model.  Since NetLogo doesn't support any formal concept of code libraries, this separation is largely achieved through positioning of the code, naming conventions, and comments.

RELATED MODELS
--------------
"Genetic Programming Maze Marchers" - The brother model to this one.
"Genetic Programming Maze Maker" - A tool for loading/saving maze files this model uses.

"Genetic Programming Demo" - A simple model demonstrating how to use the GP Library.  Start here if you want to build your own genetic programming model.

There are several models out there that work with Genetic Algorithms, which are closely related to Genetic Programming.  See:  

"Echo" under Biology
"ARS-Genetics" and "BinaryGA" by Thomas Hills, in the User Community Models.

CREDITS AND REFERENCES
----------------------

Author:  Forrest Sondahl (forrest@northwestern.edu)
Date:  November 28, 2005
Project Web Page:  http://cs.northwestern.edu/~fjs750/netlogo/final/

Part of the Genetic Programming Library for NetLogo project, which consists of a library of code that makes it easier to write genetic programming models, as well as several sample models that demonstrate the use of the library.
Created for the course CS 460 Multi-Agent Modeling, at Northwestern University.
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
Circle -7500403 true true 0 0 300

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
NetLogo 3.0
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
