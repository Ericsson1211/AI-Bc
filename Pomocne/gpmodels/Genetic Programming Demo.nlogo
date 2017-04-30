breeds [ codeturtles ]

globals 
[
  TYPE_COMMANDBLOCK
  TYPE_COMMAND
  TYPE_OPERATOR
  TYPE_REPORTER
  
  RTYPE_ANY 
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


;;;;;  BEGIN DEMO-SPECIFIC CODE ;;;;;


;; These are codeturtle procedures that help form the DNA of the codeturtles.  
;; (See the gp-setup-syntax procedure below for exactly what the DNA is made of.)
;;

;; We can't use the regular division operation as a GP ingredient, because it can 
;;   have division by zero errors.  Instead we create a "safe" alternative to division.
;; Something similar can be done for sqrt, and in fact any other primitive that might have errors.
to-report safe-div [ num den ]
  if (den != 0)
    [ report num / den ]
  report 0
end

to demo-forward [ dist ]
  ;; only allow small forward movements
  fd ((abs dist) mod 10) / 10.0
end

to demo-drawshape [ sides dist ]
  let nsides (abs (round sides))
  if (nsides < 3)
    [ set nsides 3 ]
  if (nsides > 12)
    [ set nsides 12 ]
  
  repeat nsides
  [
    fd 4.0 / nsides
    rt 360.0 / nsides
  ]
end

to-report demo-distance
  report (distance (one-of patches with [pcolor = red ]))
end

to gpuser-setup-syntax
  ;; Note:  The last number of each list is a probability weight -- that is, how likely is it
  ;;        that this ingredient is used in the code, relative to the other ingredients?

  ;; The ingredients shown here are just sort of a random smattering of possible ones.
  ;; You are encouraged to choose your own ingredient set that makes sense for the problem you are trying to solve.
  ;; The ingredients shown here should give you an idea of what is possible, and how to write your own.

  ;; Void ingredients for codeturtle's random code generation
  ;; This list contains procedures (which do not report a value) that code turtles can use.
  ;; (They can be either built-in NetLogo primitives or user-defined procedures in this model)
  set gp-syntaxlist-void (list
  (list "demo-forward" TYPE_COMMAND RTYPE_NUMERIC 10)
  (list "rt" TYPE_COMMAND RTYPE_NUMERIC 10)
  (list "lt" TYPE_COMMAND RTYPE_NUMERIC 10)  
  (list "demo-drawshape" TYPE_COMMAND RTYPE_NUMERIC RTYPE_NUMERIC 2)
  ;(list "if" TYPE_COMMAND RTYPE_BOOLEAN RTYPE_VOIDBLOCK 5)
  ;(list "ifelse" TYPE_COMMAND RTYPE_BOOLEAN RTYPE_VOIDBLOCK RTYPE_VOIDBLOCK 5)
  )
  
  ;; Numeric ingredients for codeturtle's random code generation
  ;; This list contains reporters and operators which return numeric values
  set gp-syntaxlist-numeric (list 
  (list "+" TYPE_OPERATOR RTYPE_NUMERIC RTYPE_NUMERIC 10)
  (list "-" TYPE_OPERATOR RTYPE_NUMERIC RTYPE_NUMERIC 10)
  (list "*" TYPE_OPERATOR RTYPE_NUMERIC RTYPE_NUMERIC 5)  ;; weighted less
  (list "safediv" TYPE_REPORTER RTYPE_NUMERIC RTYPE_NUMERIC 1) ;; weighted even less
  (list "0" TYPE_REPORTER 10)
  (list "1" TYPE_REPORTER 10)
  (list "2" TYPE_REPORTER 10)
  (list "4" TYPE_REPORTER 10)
  (list "8" TYPE_REPORTER 10)
  (list "16" TYPE_REPORTER 10)
  (list "demo-distance" TYPE_REPORTER 5)
  (list "random-float" TYPE_REPORTER RTYPE_NUMERIC 1) ;; used very infrequently
  )
  
  ;; Boolean ingredients for codeturtle's random code generation
  ;; This list contains reporters and operators which return boolean values
  
  ;;; Currently, none of these are being used, they are just here as examples for how they could be used
  ;;; To use them, include a command like the IF statement, that has a boolean argument.
  set gp-syntaxlist-boolean (list
  (list "true" TYPE_REPORTER 10)  
  (list "false" TYPE_REPORTER 10)  
  (list ">" TYPE_OPERATOR RTYPE_NUMERIC RTYPE_NUMERIC 10)
  (list "<" TYPE_OPERATOR RTYPE_NUMERIC RTYPE_NUMERIC 10)
  (list ">=" TYPE_OPERATOR RTYPE_NUMERIC RTYPE_NUMERIC 10)
  (list "<=" TYPE_OPERATOR RTYPE_NUMERIC RTYPE_NUMERIC 10)
  (list "=" TYPE_OPERATOR RTYPE_NUMERIC RTYPE_NUMERIC 10)
  (list "!=" TYPE_OPERATOR RTYPE_NUMERIC RTYPE_NUMERIC 10)
  (list "and" TYPE_OPERATOR RTYPE_BOOLEAN RTYPE_BOOLEAN 5)
  (list "or" TYPE_OPERATOR RTYPE_BOOLEAN RTYPE_BOOLEAN 5)
  )
  
end

;; whenever codeturtles are created (at the beginning of each generation), this procedure is run
to gpuser-initialize-codeturtle
    set size 1.5
    set gp-raw-fitness 100000 ;; very large number, very unfit
    ; this color may not always stick, since codeturtles pass color 
    ; from one generation to the next, in a hereditary way (somewhat)
    set color (who * 10 - 5)
    set xcor 0
    set ycor 0
    set heading 0
    ht
    pd        
end

;; This procedure controls the actions that each generation of codeturtles do
;;
to gpuser-run-codeturtles
  ask codeturtles 
  [
     carefully [  run (gp-compiledcode) ] [ print error-message print gp-compiledcode ]
  ]
end

;; codeturtle procedure --
;; Think of this reporter as "how far am I from my goal?"
;; Thus, a perfect codeturtle has a raw fitness of 0
;;       a bad codeturtle has an arbitrarily large raw fitness
;; 
to gpuser-calculate-raw-fitness
  let dist (distance (one-of patches with [pcolor = red ]))
  set gp-raw-fitness dist
end

;; codeturtle procedure -- Adjusts raw fitness so that now good turtles have higher fitness, 
;;  and the differences between codeturtles' fitnesses are distributed beneficially.
;;  a perfect codeturtle has a fitness of 1, and a worthless codeturtle has a fitness of 0
;;  (these fitnesses are used as weights for a probabilistically choosing turtles to reproduce)
;;
to gpuser-calculate-adjusted-fitness 
  set gp-fitness 1 / (1 + gp-raw-fitness)
end

to setup
  ca
  if (fix-random-seed?)
  [ random-seed randomseed ]
  ask random-one-of patches [ set pcolor red ]
  gp-setup   ; gp library procedure -- must be called in your setup
end

;; This GO mainly calls the gp-go library procedure.  If you have custom code that you need
;;    to run before or after a generation goes by, you can put it here as well.
to go
  clear-drawing
  gp-go
  
  ask codeturtles with-min [ gp-raw-fitness ]
  [
    ifelse (gp-raw-fitness < .3)
      [ set shape "face happy" ]
      [ set shape "face neutral" ]
    st
  ]
  ;; if we found a good enough turtle, then stop.
  if (gp-best-fitness-this-gen < .3)   
    [ stop ]
end


@#$#@#$#@
GRAPHICS-WINDOW
326
10
686
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
0
0
1

CC-WINDOW
5
405
878
500
Command Center
0

SLIDER
696
99
868
132
branch-chance
branch-chance
0
1
0.85
0.01
1
NIL

SLIDER
695
59
868
92
initial-code-max-depth
initial-code-max-depth
2
20
12
1
1
NIL

SWITCH
705
308
857
341
fix-random-seed?
fix-random-seed?
0
1
-1000

SLIDER
704
346
856
379
randomseed
randomseed
1
50000
17837
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
697
159
869
192
clone-chance
clone-chance
0
100
20
1
1
NIL

SLIDER
696
239
868
272
crossover-chance
crossover-chance
0
100
70
1
1
NIL

SLIDER
697
199
869
232
mutate-chance
mutate-chance
0
100
10
1
1
NIL

SLIDER
694
10
866
43
population-size
population-size
1
100
20
1
1
NIL

BUTTON
84
63
157
96
NIL
go
T
1
T
OBSERVER
T
NIL

BUTTON
8
62
71
95
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
104
318
254
Fitness Plot
generations
raw fitness
0.0
20.0
0.0
20.0
true
true
PENS
"worst" 1.0 0 -2674135 true
"avg" 1.0 0 -11221820 true
"best" 1.0 0 -8630108 true

MONITOR
259
261
316
310
best
gp-best-fitness-this-gen
3
1

MONITOR
261
49
318
98
gen #
gp-generation
3
1

OUTPUT
8
260
246
373

BUTTON
250
323
322
356
show-best
gp-showbest
NIL
1
T
OBSERVER
T
NIL

@#$#@#$#@
WHAT IS IT?
-----------
This model is a simple demo of genetic programming, showing how to use the Genetic Programming Library for NetLogo.  Turtles start at home, and go out seeking a red goal patch.  Watch them converge toward the goal, and experiment with the genetic programming parameters.  The code contains some comments that might be useful for building your own genetic programming model.  Modelers are encouraged to use this model as a starting point for writing their own genetic programming models.

WHAT IS GENETIC PROGRAMMING?
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
In many NetLogo models, agents are given predetermined rules, and then the emergent behaviors that form through the interactions of these agents are studied.  In contrast, this model is starts with a desired behavior (navigating to the red square) and works on trying to discover the code that the agents should execute to achieve this behavior, through use of Genetic Programming, as described in the section above.

In this model, the goal-seeking programs are represented by "codeturtles".  Codeturtles each have a piece of NetLogo code assigned to them, and it's their job to perform it.  Codeturtles will then be chosen, based on their fitness, to reproduce and create another generation of codeturtles.

You can find the ingredients that make up the codeturtles' netlogo code by looking in the "gpuser-setup-syntax" procedure.  You can also get insight into what example code looks like by clicking the SHOW-BEST button.

The fitness function, which measures each codeturtles progress, is a simple one: "What is the geometric distance to the goal square?"  The lower this number is, the more fit a turtle is.

HOW TO USE IT
-------------

1. Adjust the slider parameters (see below), or use the default settings.
2. Press the SETUP button.
3A. Press the GO button to begin the simulation 
3B. Press the STEP button to go through one generation at a time.
4. Watch the View, to see the codeturtles execute their code.
5. Watch the FITNESS plot, to see how close the population is to the goal.
6. If a codeturtle successfully gets to the goal square, then the simulation will stop.  To stop it earlier, press the GO button again.
7. Press the SHOW-BEST button to see the code for the current best (most fit) codeturtle.

Parameters:

POPULATION-SIZE: The number of codeturtles in each generation
INITIAL-CODE-MAX-DEPTH: The maximum depth of randomly generated code trees, that codeturtles start with.  (It also affects the size of mutations that occur).  In general, a larger number generally means longer programs are created.
BRANCH-CHANCE: Controls the amount of branching in the generation of random code trees.  Again, a larger number generally means longer programs are created.

CLONE-CHANCE, MUTATE-CHANCE, CROSSOVER-CHANCE:  These three sliders control the relative probability of each genetic operation occurring, with respect to the others. 
(Examples:  If the sliders are set to 10, 10, 10, then there is a 1/3 chance of each genetic operation happening.  If the sliders are set to 10, 10, 20, then there is a 25% chance of cloning, 25% chance of mutation, and 50% chance of crossover.  If the sum of these three sliders is 100, then each slider represents the percent chance of that genetic operation being chosen to create an offspring for the new generation.)

FIX-RANDOM-SEED?: If true, then RANDOMSEED is used to start the process.  This allows a particular run to be reproduced exactly, and thus examined more closely, (provided that the parameters are the same).  If false, then RANDOMSEED is not used.

RANDOMSEED:  This is the number used to seed the random number generator, if FIX-RANDOM-SEED? is true, to allow for reproducible results.

Notes:
 You cannot see the codeturtles while they are moving even if you slow down the speed slider, because of the way their code is executed, so what you see on the View is their positions after they have moved for that generation.  They leave a colored trail behind them, which is cleared between generations.  This shows the path that the turtles took.

The best fit codeturtle (or codeturtles, when there is a tie) in each generation is shown as a neutral face.  If a codeturtle finds the goal, then it is shown as a smiley face.

THINGS TO NOTICE
----------------

The colors of the codeturtles have some meaning.  They are initially randomly colored, but:
* When a codeturtle results from cloning, it has the same color as its parent.
* When a codeturtle results from crossover, it has a color averaging its two parents.
* When a codeturtle is mutated, it has a random color.

THINGS TO TRY
-------------
Try playing with the various sliders.  See what difference they make.


EXTENDING THE MODEL
-------------------
The purpose of this model is to provide a jumping off point for writing your own genetic programming models.

Genetic programming is a fairly versatile instrument, and can be used to solve many different types of problems.  Be creative, and good luck!

NETLOGO FEATURES
----------------

The NetLogo feature on which this whole model stands is the ability to take a string of text, and run it as NetLogo code.  This is achieved through the "run" primitive.

Extensive use of recursion and lists has been employed, especially to deal with the tree structures which codeturtles use to store code.  Since trees are not natively supported in NetLogo, they have been implemented as nested lists.

This model is built in two parts.  The first part is the "GP Library for NetLogo", which consists of a framework of procedures that are useful for any model that is using genetic programming.  The second part consists of procedures that are specific to this model.  Since NetLogo doesn't support any formal concept of code libraries, this separation is largely achieved through positioning of the code, naming conventions, and comments.

RELATED MODELS
--------------
"Genetic Programming Maze Marchers" and "Genetic Programming Maze Rules"
-- two genetic programming approaches to solving mazes.

There are also several models out there that work with Genetic Algorithms, which are closely related to Genetic Programming.  See:  

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
NetLogo 3.0.2
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
