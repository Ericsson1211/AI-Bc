breed [nodes node]
nodes-own [
  name?
  distance-to-destination ;h(n)
  searched?
  total-cost ;f(n)
  cost-to-this-node ;g(n)
  active
  depth-level
  parent
]
undirected-link-breed [ edges edge]
edges-own[value]
breed [finders finder]

globals[
  Destination-Node
  Start-Node
  current-node
  path
  connection
  last-node
  previous-node
  graph
  childrens
  fringe

]

to Calculate-Distance
  ask nodes [
    set distance-to-destination distance Destination-Node * 10
    set distance-to-destination (precision distance-to-destination 0)
;    show name?
;    show distance-to-destination
  ]
  ask Start-Node
  [
    set color red
    ;set searched? true
    set cost-to-this-node 0
    set active true
    set total-cost 0
    set current-node self
    set last-node who ;asking node number for edge asking
   ; set path lput self path
    set previous-node self
    set depth-level 0
  ]
  ask Destination-Node [
    set color red
  ]
end

to Uniform-Cost-Search

  ; Calculate-Distance
   show "Finding Path with Unofrm cost search"
  let stack []
  set fringe []
  set path lput Start-Node path
  let parent-current-node 0
  while[current-node != Destination-Node][
    ask current-node [
      set color green
      set searched? true
      ask ([edge-neighbors] of current-node)[
        ;          show "neighbors"
        ;          show self
        set active true
        set stack lput self fringe
        if (parent = 0 or self = Destination-Node)[
          set parent current-node
        ]
          set connection edge-with current-node
          set cost-to-this-node [value] of connection + [cost-to-this-node] of current-node
          ;        set total-cost (cost-to-this-node) + distance-to-destination

      ]

      set stack sort-on [cost-to-this-node] (nodes with [active and not searched?])
      show "stackkkkk"
      show stack
      show "fringe"
      show fringe
      ask first stack[
        set current-node self
        ;set color red
        set parent-current-node ([parent] of current-node)
      ]
      foreach path [ [ag] ->
        ask ag[
          show "[parent] of self"
          show [parent] of self
              show "parent-current-node"
              show parent-current-node
              if(self = Start-Node)[
                set parent 0
              ]
              if([parent] of self = parent-current-node)[
                set path remove self path
                show "selfone"
                show self
              ]
            ]
        ]
        set path lput current-node path
        show "path"
        show path
      ]
    ]
  if(current-node = Destination-Node)
  [
    set path []
    set current-node Destination-Node
    while[current-node != Start-Node]
    [
      set path fput current-node path
      ask current-node[
        set current-node [parent] of self
      ]
    ]
    set path fput Start-node path
    show "path"
        show path
   Coloring-Path
  ]

end

to Breadth-First-Search
  ; show "Findig Path with Depth first Search"
  set fringe []
 ; let stack []
  let x 0
  let parent-current-node 0
  ;  ask nodes
  ;  [
  ;    set fringe lput self fringe
  ;    set active true
  ;  ]
  ;  set fringe sort-on [name?] (nodes with [active])
  ;  show fringe
  while[current-node != Destination-Node][
    let stack []
    ;set fringe []
    set x x + 1
    show "Finding Path with Depth First Search"
    ask current-node[
      set searched? true
      show "current"
      show self
      set color green
    ]
    ask ([edge-neighbors] of current-node)[
      set active true
      if(searched? = false)[
        set stack lput self stack
      ]
      if (parent = 0 )[
        set parent current-node ;set the node where we came from
      ]
    ]
    ;set stack sort-on [name?](nodes with [searched? = false and active])
    set stack sort-by [ [name?1 name?2] -> name?1 < name?2 ] stack
    ;set stack reverse stack
    show "stack"
    show stack
    ;set fringe fput stack fringe

    let need-to-search-fringe true
    foreach stack [ [ag] -> ;ask every node in the list path
      if(not member? ag fringe)[
      set fringe fput ag fringe
    ]
    ]
    foreach path [ [ag] -> ;ask every node in the list path
      ask ag[
        ;            show "[parent] of self"
        ;            show [parent] of self
        ;            show "parent-current-node"
        ;            show parent-current-node
        ;            show "self"
        ;            show self
        ;            show "Start-node"
        ;            show Start-Node
        if([parent] of self = parent-current-node)[ ; if there are two nodes in path with the same parent, remove the second one
          set path remove self path
          ;              show "selfone"
          ;              show self
        ]
      ]
    ]
    set path lput current-node path
    ask first fringe [
      set current-node self
    ]
    set fringe remove current-node fringe
    show "fringe"
    show fringe
    show "stack"
    show stack

  ]
  if(current-node = Destination-Node)
  [
   ; set path []
;    while[current-node != Start-Node]
;    [
;      set path fput current-node path
;      ask current-node[
;        set current-node [parent] of self
;      ]
;    ]
    set path fput Start-node path
    set path lput Destination-Node path
   Coloring-Path
  ]
end

to Depth-First-Search
  ; show "Findig Path with Depth first Search"
  set fringe []
 ; let stack []
  let x 0
  let parent-current-node 0
  ;  ask nodes
  ;  [
  ;    set fringe lput self fringe
  ;    set active true
  ;  ]
  ;  set fringe sort-on [name?] (nodes with [active])
  ;  show fringe
  while[current-node != Destination-Node][
    let stack []
    ;set fringe []
    set x x + 1
    show "Finding Path with Depth First Search"
    ask current-node[
      set searched? true
      show "current"
      show self
      set color green
    ]
    ask ([edge-neighbors] of current-node)[
      set active true
      if(searched? = false)[
        set stack lput self stack
      ]
      if (parent = 0 )[
        set parent current-node ;set the node where we came from
      ]
    ]
    ;set stack sort-on [name?](nodes with [searched? = false and active])
    set stack sort-by [ [name?1 name?2] -> name?1 < name?2 ] stack
    set stack reverse stack
    show "stack"
    show stack
    ;set fringe fput stack fringe

    let need-to-search-fringe true
    foreach stack [ [ag] -> ;ask every node in the list path
      if(not member? ag fringe)[
      set fringe fput ag fringe
    ]
    ]
    foreach path [ [ag] -> ;ask every node in the list path
      ask ag[
        ;            show "[parent] of self"
        ;            show [parent] of self
        ;            show "parent-current-node"
        ;            show parent-current-node
        ;            show "self"
        ;            show self
        ;            show "Start-node"
        ;            show Start-Node
        if([parent] of self = parent-current-node)[ ; if there are two nodes in path with the same parent, remove the second one
          set path remove self path
          ;              show "selfone"
          ;              show self
        ]
      ]
    ]
    set path lput current-node path
    ask first fringe [
      set current-node self
    ]
    set fringe remove current-node fringe
    show "fringe"
    show fringe
    show "stack"
    show stack

  ]
  if(current-node = Destination-Node)
  [
   ; set path []
;    while[current-node != Start-Node]
;    [
;      set path fput current-node path
;      ask current-node[
;        set current-node [parent] of self
;      ]
;    ]
    set path fput Start-node path
    set path lput Destination-Node path
   Coloring-Path
  ]
end

to Greedy-Search
  reset-timer
  let counter 0
  set fringe []
  set path lput Start-Node path
  let parent-current-node 0
  while[current-node != Destination-Node][ ;ask if the destination is finded
    set counter counter + 1
    if(counter > 30)[
      show "Nenasla sa cesta"
      ask Start-node[
        set color red
      ]
      stop

    ]
    ifelse(Search-Type  = "Tree")[
      show "Finding Path with Greedy in tree"
      ask current-node [
        set color green
        set searched? true

        ask ([edge-neighbors] of current-node)[
          set active true ;setting neighbors active
          set fringe lput self fringe
          set color green
          if (parent = 0 )[
            set parent current-node ;set the node where we came from
          ]
        ]
        set fringe sort-on [distance-to-destination] (nodes with [active])
        ask first fringe[ ;ask node with the lowest g(n)
          set current-node self
          ;set color red
          set parent-current-node ([parent] of current-node) ;set the parent of the current node for compare
        ]
        foreach path [ [ag] -> ;ask every node in the list path
          ask ag[
            if([parent] of self = parent-current-node)[ ; if there are two nodes in path with the same parent, remove the second one
              set path remove self path
            ]
          ]
        ]
        set path lput current-node path
      ]
    ][
      show "Finding Path with Greedy in Graph"
      ; set fringe []
      ask current-node [
        set color green
        set searched? true
        ask ([edge-neighbors] of current-node)[
          ;          show "neighbors"
          ;          show self
          set active true
          set color green
          set fringe lput self fringe
          if (parent = 0)[
            set parent current-node
          ]
        ]
        set fringe sort-on [distance-to-destination] (nodes with [active and not searched?]) ;sorting only not searched nodes cause we are working with tree
        ask first fringe[
          set current-node self
          ;set color red
          set parent-current-node ([parent] of current-node)
        ]
        foreach path [ [ag] ->
          ask ag[
            if(self = Start-Node)[
              set parent 0
            ]
            if([parent] of self = parent-current-node)[
              set path remove self path
            ]
          ]
        ]
        set path lput current-node path
      ]
    ]
  ]
  if(current-node = Destination-Node)
  [

    show "Seconds"
    show timer

   Coloring-Path
  ]

end

to Coloring-Path
  show "Path finded"
    foreach path [[ag] -> ;ask for every node in the list path
      let ag-position ([who] of ag) ;set number of the node
;      show "ag-postition"
;      show ag-position
      set path remove-item 0 path ;remove first item from the list path
;      show "path"
;      show path
      ask ag [
        set color red]
      if(not empty? path)[ ;if the list is not empty
        let next-edge-position-node (first path) ;set first node from list path into temporary variable
;        show "next position"
        ask next-edge-position-node [
          set color red
        ]
       ; show next-edge-position-node
        ifelse(is-edge? edge (ag-position) ([who] of next-edge-position-node) = true)[
;          show "path in ifelse"
;          show path
          ask edge (ag-position) ([who] of next-edge-position-node)[ ;ask edge of previous node and the first one from the list path
            set color red ;and set to red
            set thickness 0.2
          ]
        ][if(not member? Start-Node path)[
;          show "not true"
          set path fput Start-Node path
          show path
          ifelse(not is-edge? edge([who] of Start-Node) ([who] of next-edge-position-node))[
            set path remove next-edge-position-node path
          ][
          ask edge ([who] of Start-Node) ([who] of next-edge-position-node)[ ;ask edge of previous node and the first one from the list path
            set color red ;and set to red
            set thickness 0.2
          ]
              ]
        ]
        ]
      ]
    ]
end


to A*-Search
  ; Calculate-Distance
  reset-timer

  set fringe []
  set path lput Start-Node path
  let parent-current-node 0
  while[current-node != Destination-Node][
     show "Finding Path with A*"
      ask current-node [
        set searched? true
        set color green
        ask ([edge-neighbors] of current-node)[
;          show "neighbors"
;          show self

          set active true
          set fringe lput self fringe
          if (parent = 0)[
          set parent current-node
          set connection edge-with current-node
          set cost-to-this-node [value] of connection + [cost-to-this-node] of current-node
        set total-cost (cost-to-this-node) + distance-to-destination
          ]
        ]

        set fringe sort-on [total-cost] (nodes with [active and not searched?])
        ask first fringe[
          set current-node self
          ;set color red
          set parent-current-node ([parent] of current-node)
        ]
        foreach path [ [ag] ->
          ask ag[
;              show "[parent] of self"
;              show [parent] of self
;              show "parent-current-node"
;              show parent-current-node
              if(self = Start-Node)[
                set parent 0
              ]
              if([parent] of self = parent-current-node)[
                set path remove self path
;                show "selfone"
;                show self
              ]
            ]
        ]
        set path lput current-node path
;        show "path"
;        show path
      ]
    ]
  if(current-node = Destination-Node)
  [
    show "Seconds"
    show timer
   Coloring-Path
  ]

;  ask min-one-of (nodes with [active and not searched?])[total-cost]
;  [
;    set current-node self
;    show "current-node"
;    show current-node
;    set active false
;    set searched? true
;    set color blue
;    set path lput current-node path
;  ]
;
;  ifelse(current-node != Destination-Node)[
;    ask ([edge-neighbors] of current-node)
;    [
;      ; let searching []
;      if([searched?] of self = false)[
;;        show "self"
;;        show self
;        set active true
;        set color yellow
;        let location self
;        set connection edge-with current-node
;;        show "connection"
;;        show connection
;        set cost-to-this-node [value] of connection + [cost-to-this-node] of current-node
;        set total-cost (cost-to-this-node) + distance-to-destination
;        set path lput connection path
;       ; set path lput self path
;
;        ; set searching lput self ([searching] of current-node)
;        ;A*-Search
;      ]
;    ]
;    set path remove connection path
;    show "current-node"
;    show current-node
;    show "previous-node"
;    show previous-node
;;    if(current-node != Start-Node)[
;;      ask edge ([who] of current-node) ([who] of previous-node)
;;      [
;;        set color red
;;      ]
;;    ]
;    ;  let remove-edge-of-connection ([neighbors] of connection)
;    A*-Search
;
;  ][;and not any? nodes with [active])[
;    show "Path finded"
;    ask Destination-Node [
;      set color red
;    ]
;    ask Start-Node [
;      set color red
;    ]
;
;    show ([who] of current-node)
;    show ([who] of previous-node)
;
;;    ask edge ([who] of current-node) ([who] of previous-node)
;;      [
;;        show self
;;        set color red
;;
;;    ]
;
;
;
;  ]
;



  ;  while [[color] of Destination-Node != red and any? nodes with [active]]
  ;  [
  ;   ; set searched sort-by [ [total-cost1 total-cost2] -> total-cost1 < total-cost2] searched
  ;   ; ask min-one-of
  ;   ; set searched min-one-of (nodes with [active]) [total-cost]
  ;    ask min-one-of (nodes with [active]) [total-cost]
  ;    [
  ;      set color yellow
  ;      set active true
  ;      set current-node self
  ;      ask ([edge-neighbors] of current-node)
  ;      [
  ;        let connection edge-with current-node
  ;        set cost-to-this-node [value] of connection + [cost-to-this-node] of previous-node
  ;        set total-cost (cost-to-this-node ) + distance-to-destination
  ;      ]
  ;    ]
  ;  ]
  ;    show searched
  ;    set current-node min-one-of (nodes with [active]) [total-cost]
  ;    set searched remove-item 0 searched
  ;    show current-node
  ;    if(current-node = Destination-Node)
  ;    [
  ;      ask current-node[
  ;        set color red
  ;        set active false
  ;      ]
  ;    ]
  ;    ;    show "current-node in ask"
  ;    ;      show current-node
  ;    let previous-node current-node
  ;    ask ([edge-neighbors] of previous-node )
  ;    [
  ;      ask self [
  ;        set color yellow
  ;        set searched? true
  ;        set active true
  ;        let connection edge-with previous-node
  ;        set cost-to-this-node [value] of connection + [cost-to-this-node] of previous-node
  ;        set total-cost (cost-to-this-node ) + distance-to-destination
  ;        if(not member? self searched )[
  ;          ;let connection edge-with previous-node
  ;         ; set cost-to-this-node [value] of connection
  ;          ;set total-cost (cost-to-this-node ) + distance-to-destination
  ;          set searched lput self searched
  ;
  ;        ]
  ;      ]
  ;
  ;    ]
  ;  ]


  ;  let path []
  ;  let searched []
  ;  let finded? false
  ;
  ;  set searched lput Start-Node searched
  ;  set path lput Start-Node path
  ;
  ;  while[finded? != true][
  ;    ifelse length searched != 0[
  ;      ;organizacia listu aktualne prehladavanych miest
  ;      set searched sort-by [total-cost] searched
  ;      ;nastavenie aktualneho mesta
  ;      set current-node item 0 searched
  ;      ask ([edge-neighbors] of current-node)[
  ;        let c ([total-cost] of current-node)
  ;      ]
  ;
  ;
  ;    ][
  ;      user-message("Neexistuje cesta")
  ;    ]
  ;
  ;  ]

  ;  ask Start-Node [
  ;    set color red
  ;    set location myself
  ;    set searched? true
  ;    set cost 0
  ;    set total-cost cost
  ;
  ;]
  ;while[not any? finders with [location = Destination-Node] and any? finders with [searched?]]
  ;[
  ;  ask min-one-of (nodes with [searched?])[total-cost]
  ;  [
  ;    set searched? true
  ;    let actual-finder self
  ;
  ;  ]
  ;]
end


to setup
  ca
  ask patches [
    set pcolor white
  ]
  set path []
  set-default-shape nodes "square"
  create-nodes 1[;node 0
    setxy 4 -14
    set color black
    set name? "Arad"
  ]
  create-nodes 1[;node 1
    setxy 4 -23
    set name? "Timisoara"
    set color black
  ]
  create-nodes 1[;node 2
    setxy 7 -10
    set name? "Zerind"
    set color black
  ]
  create-nodes 1[;node 3
    setxy 11 -6
    set name? "Oradea"
    set color black
  ]
  create-nodes 1[;node 4
    setxy 13 -27
    set name? "Lugoj"
    set color black
  ]
  create-nodes 1[;node 5
    setxy 14 -31
    set name? "Mehadia"
    set color black
  ]
  create-nodes 1[;node 6
    setxy 14 -35
    set name? "Dobreta"
    set color black
  ]
  create-nodes 1[;node 7
    setxy 24 -36
    set name? "Craiova"
    set color black
  ]
  create-nodes 1[;node 8
    setxy 20 -23
    set name? "Rimnicu Vilcea"
    set color black
  ]
  create-nodes 1[;node 9
    setxy 18 -17
    set name? "Sibiu"
    set color black
  ]
  create-nodes 1[;node 10
    setxy 28 -18
    set name? "Fagaras"
    set color black
  ]
  create-nodes 1[;node 11
    setxy 30 -28
    set name? "Pitesti"
    set color black
  ]
  create-nodes 1[;node 12
    setxy 38 -32
    set name? "Bucharest"
    set color black
  ]
  create-nodes 1[;node 13
    setxy 35 -38
    set name? "Giurgiu"
    set color black
  ]
  create-nodes 1[;node 14
    setxy 44 -31
    set name? "Urziceni"
    set color black
  ]
  create-nodes 1[;node 15
    setxy 50 -31
    set name? "Hirsova"
    set color black
  ]
  create-nodes 1[;node 16
    setxy 54 -36
    set name? "Eforie"
    set color black
  ]
  create-nodes 1[;node 17
    setxy 48 -19
    set name? "Vaslui"
    set color black
  ]
  create-nodes 1[;node 18
    setxy 45 -12
    set name? "Iasi"
    set color black
  ]
  create-nodes 1[;node 19
    setxy 38 -9
    set name? "Neamt"
    set color black
  ]
  ask node 0 [create-edge-with node 1]
  ask edge 0 1 [set value 118]
  ask node 0 [create-edge-with node 2]
  ask edge 0 2 [set value 75]
  ask node 2 [create-edge-with node 3]
  ask edge 2 3 [set value 71]
  ask node 1 [create-edge-with node 4]
  ask edge 1 4 [set value 111]
  ask node 4 [create-edge-with node 5]
  ask edge 4 5 [set value 70]
  ask node 5 [create-edge-with node 6]
  ask edge 5 6 [set value 75]
  ask node 6 [create-edge-with node 7]
  ask edge 6 7 [set value 120]
  ask node 7 [create-edge-with node 8]
  ask edge 7 8 [set value 146]
  ask node 7 [create-edge-with node 11]
  ask edge 7 11[ set value 138]
  ask node 8 [create-edge-with node 9]
  ask edge 8 9 [set value 80]
  ask node 8 [create-edge-with node 11]
  ask edge 8 11 [set value 97]
  ask node 9 [create-edge-with node 0]
  ask edge 9 0 [set value 140]
  ask node 9 [create-edge-with node 3]
  ask edge 9 3 [set value 151]
  ask node 9 [create-edge-with node 10]
  ask edge 9 10 [set value 99]
  ask node 10 [create-edge-with node 12]
  ask edge 10 12 [set value 211]
  ask node 11 [create-edge-with node 12]
  ask edge 11 12 [set value 101]
  ask node 12 [create-edge-with node 13]
  ask edge 12 13 [set value 90]
  ask node 12 [create-edge-with node 14]
  ask edge 12 14 [set value 85]
  ask node 14 [create-edge-with node 15]
  ask edge 14 15 [set value 98]
  ask node 14 [create-edge-with node 17]
  ask edge 14 17 [set value 142]
  ask node 15 [create-edge-with node 16]
  ask edge 15 16 [set value 86]
  ask node 17 [create-edge-with node 18]
  ask edge 17 18 [set value 92]
  ask node 18 [create-edge-with node 19]
  ask edge 18 19 [set value 87]
  ask nodes [
    set plabel name?
    set plabel-color black
    set searched? false
    set active false
  ]
  ask edges [
    set label value
    set label-color black
  ]
  set Destination-Node one-of nodes with [name? = Destination]
  set Start-Node one-of nodes with [name? = Start]
  set childrens []
  Calculate-Distance
end
@#$#@#$#@
GRAPHICS-WINDOW
211
10
947
552
-1
-1
13.0
1
10
1
1
1
0
1
1
1
0
55
-40
0
0
0
1
ticks
30.0

BUTTON
75
32
141
65
Setup
Setup
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

CHOOSER
39
135
177
180
Start
Start
"Arad" "Timisoara" "Zerind" "Oradea" "Lugoj" "Mehadia" "Dobreta" "Craiova" "Rimnicu Vilcea" "Sibiu" "Fagaras" "Pitesti" "Bucharest" "Giurgiu" "Urziceni" "Hirsova" "Eforie" "Vaslui" "Iasi" "Neamt"
0

CHOOSER
39
194
177
239
Destination
Destination
"Arad" "Timisoara" "Zerind" "Oradea" "Lugoj" "Mehadia" "Dobreta" "Craiova" "Rimnicu Vilcea" "Sibiu" "Fagaras" "Pitesti" "Bucharest" "Giurgiu" "Urziceni" "Hirsova" "Eforie" "Vaslui" "Iasi" "Neamt"
12

BUTTON
62
267
157
300
NIL
A*-Search
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
48
327
172
360
NIL
Greedy-Search\n
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
34
387
185
420
NIL
Depth-First-Search
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

CHOOSER
39
79
177
124
Search-Type
Search-Type
"Graph" "Tree"
0

BUTTON
29
441
191
474
NIL
Breadth-First-Search
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
27
497
193
530
NIL
Uniform-Cost-Search
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

@#$#@#$#@
## WHAT IS IT?

(a general understanding of what the model is trying to show or explain)

## HOW IT WORKS

(what rules the agents use to create the overall behavior of the model)

## HOW TO USE IT

(how to use the model, including a description of each of the items in the Interface tab)

## THINGS TO NOTICE

(suggested things for the user to notice while running the model)

## THINGS TO TRY

(suggested things for the user to try to do (move sliders, switches, etc.) with the model)

## EXTENDING THE MODEL

(suggested things to add or change in the Code tab to make the model more complicated, detailed, accurate, etc.)

## NETLOGO FEATURES

(interesting or unusual features of NetLogo that the model uses, particularly in the Code tab; or where workarounds were needed for missing features)

## RELATED MODELS

(models in the NetLogo Models Library and elsewhere which are of related interest)

## CREDITS AND REFERENCES

(a reference to the model's URL on the web if it has one, as well as any other necessary credits, citations, and links)
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

sheep
false
15
Circle -1 true true 203 65 88
Circle -1 true true 70 65 162
Circle -1 true true 150 105 120
Polygon -7500403 true false 218 120 240 165 255 165 278 120
Circle -7500403 true false 214 72 67
Rectangle -1 true true 164 223 179 298
Polygon -1 true true 45 285 30 285 30 240 15 195 45 210
Circle -1 true true 3 83 150
Rectangle -1 true true 65 221 80 296
Polygon -1 true true 195 285 210 285 210 240 240 210 195 210
Polygon -7500403 true false 276 85 285 105 302 99 294 83
Polygon -7500403 true false 219 85 210 105 193 99 201 83

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

wolf
false
0
Polygon -16777216 true false 253 133 245 131 245 133
Polygon -7500403 true true 2 194 13 197 30 191 38 193 38 205 20 226 20 257 27 265 38 266 40 260 31 253 31 230 60 206 68 198 75 209 66 228 65 243 82 261 84 268 100 267 103 261 77 239 79 231 100 207 98 196 119 201 143 202 160 195 166 210 172 213 173 238 167 251 160 248 154 265 169 264 178 247 186 240 198 260 200 271 217 271 219 262 207 258 195 230 192 198 210 184 227 164 242 144 259 145 284 151 277 141 293 140 299 134 297 127 273 119 270 105
Polygon -7500403 true true -1 195 14 180 36 166 40 153 53 140 82 131 134 133 159 126 188 115 227 108 236 102 238 98 268 86 269 92 281 87 269 103 269 113

x
false
0
Polygon -7500403 true true 270 75 225 30 30 225 75 270
Polygon -7500403 true true 30 75 75 30 270 225 225 270
@#$#@#$#@
NetLogo 6.0
@#$#@#$#@
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
