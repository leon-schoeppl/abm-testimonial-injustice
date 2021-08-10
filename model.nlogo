breed[people person]

people-own[
  groupType
  credence
  quietenTendency
  expectedPatchConsensus
  expectedDivergence
  expectedUtility
  testimony
  input
]

globals[
  objectiveChance ;value of the proposition that agents have credences about
  countPeople ;total number of agents in the simulation

  ;------------------------------------------------------------------------------
  ; groups 0 (A) , 1 (B) and 2 (C) (, and 3 is sometimes the total of all groups)
  groupBiases
  groupColors
  groupCredences
  groupTestimonies
  groupPercentages ;how many % of all agents are in groups A, B, C
  groupCounts
  groupThresholds ;this will later be a summary of individual thresholds, not purely a group value



  meanDistance
  utilityMatrix
  smotheringCounter
  quietingCounter
]

to setup
  clear-all
  reset-ticks
  setupWorld
  setupAgents
  prepareGame
  printSetup
end

to go
  ;if learning function is toggled on, people will individually try and assess the average group opinions
  tick
  prepareGame
  ifelse allowInjustice = true [playGame][skipGame]
  doExperiments ;add a chance here? How often do agents collect data?
  printUpdate

end

to playGame
  ask patches [
    let participants turtles-here
    let countParticipants (list (count turtles-here with [groupType = 0]) (count turtles-here with [groupType = 1]) (count turtles-here with [groupType = 2]) (count participants))

    roundOne participants countParticipants
    roundTwo participants countParticipants
    roundThree participants countParticipants

  ]
end



to roundOne [participants countParticipants] ;Calculate expected patch consensus and expected divergence
  ask participants [
      ifelse groupType = 0[ ;possibly ommit own credence from this calculation?
        set expectedPatchConsensus (credence + ((item 0 countParticipants) - 1) * (item 0 groupCredences) + (item 1 countParticipants) * (item 1 groupCredences) + (item 2 countParticipants) * (item 2 groupCredences)) / (item 3 countParticipants)
      ][
        ifelse groupType = 1[
          set expectedPatchConsensus (credence + (item 0 countParticipants) * (item 0 groupCredences) + ((item 1 countParticipants) - 1) * (item 1 groupCredences) + (item 2 countParticipants) * (item 2 groupCredences)) / (item 3 countParticipants)
        ][
          set expectedPatchConsensus (credence + (item 0 countParticipants) * (item 0 groupCredences) + (item 1 countParticipants) * (item 1 groupCredences) + ((item 2 countParticipants) - 1) * (item 2 groupCredences)) / (item 3 countParticipants)
        ]
      ]
      set expectedDivergence abs (expectedPatchConsensus - credence)
    ]
end

to roundTwo [participants countParticipants] ;calculate expected utility and give testimony; refactor later
      ask participants [
      if groupType = 0[
        set expectedUtility 0

        if expectedDivergence > quietenThresholdB [
          set expectedUtility expectedUtility + (item 1 countParticipants) * penaltyPerPerson
        ]
        if expectedDivergence > quietenThresholdC [
          set expectedUtility expectedUtility + (item 2 countParticipants) * penaltyPerPerson
        ]
      ]

      if groupType = 1[
        set expectedUtility 0

        if expectedDivergence > quietenThresholdA [
          set expectedUtility expectedUtility + (item 0 countParticipants) * penaltyPerPerson
        ]
        if expectedDivergence > quietenThresholdC [
          set expectedUtility expectedUtility + (item 2 countParticipants) * penaltyPerPerson
        ]
      ]

      if groupType = 2[
        set expectedUtility 0

        if expectedDivergence > quietenThresholdA [
          set expectedUtility expectedUtility + (item 0 countParticipants) * penaltyPerPerson
        ]
        if expectedDivergence > quietenThresholdB [
          set expectedUtility expectedUtility + (item 1 countParticipants) * penaltyPerPerson
        ]
      ]


    ifelse expectedUtility < penaltySmothering [;negative values are being compared here
      set testimony (credence + expectedPatchConsensus) / 2 ;split the difference with majority consensus
      set smotheringCounter smotheringCounter + 1 ;reset when?
    ][
      set testimony credence
    ]
  ]
end

to roundThree [participants countParticipants]
  let patchConsensus 0
  ask participants [
    set patchConsensus patchConsensus + testimony
  ]
  if item 3 countParticipants > 0 [
   set patchConsensus patchConsensus / item 3 countParticipants
  ]

ask participants [
    set input 0
    let ownDivergenceConsensus abs (credence - patchConsensus)
    if (item 3 countParticipants) > 1 [ ;no agent updates their credence alone on a patch

      ask other participants [
        let divergenceConsensus abs (testimony - patchConsensus)
        let divergence abs (testimony - [credence] of myself)
        let groupTypeMyself [groupType] of myself
        let relevantThreshold item groupType groupThresholds


        ifelse
        (groupType != groupTypeMyself) ;first condition for quieting
        AND ownDivergenceConsensus < relevantThreshold
        AND divergenceConsensus > relevantThreshold ;maybe add adjustable boldness?
        AND (divergence > relevantThreshold)

        [; add here: its not just about divergence between agents. The patch consensus matters!
          ask myself [set input input + (([credence] of myself + credence) / 2)]
          set quietingCounter quietingCounter + 1 ;reset when?
          ;add additional quieting effects here
        ][
            ask myself [set input input + [credence] of myself]
        ]
      ]
      set input input / ((item 3 countParticipants) - 1)
      set credence (input + credence) / 2
    ]
  ]
end



to doExperiments

end



to skipGame ;if the simulation disallows testimonial injustice, everyone just updates by splitting the difference with the mean of the other participants
  ask patches [
    let participants turtles-here
    let countParticipants count participants

    ask participants[
      set input 0
      if countParticipants > 1 [
        ask other participants[
          ask myself [
            set input input + [credence] of myself
          ]
        ]
          set input input / (countParticipants - 1)
          set credence (input + credence) / 2

      ]
    ]
  ]
end

to setupAgents
  set groupColors (list orange blue brown)
  set groupBiases (list biasTypeA biasTypeB biasTypeC)
  set groupCounts (list countTypeA countTypeB countTypeC)
  set countPeople item 0 groupCounts + item 1 groupCounts + item 2 groupCounts
  set groupPercentages (list ((countTypeA / countPeople) * 100) ((countTypeB / countPeople) * 100) ((countTypeC / countPeople) * 100))
  set groupThresholds (list quietenThresholdA quietenThresholdB quietenThresholdC)

  ;creates the agents of each group
  setupGroup 0
  setupGroup 1
  setupGroup 2

 end

to setupWorld
  resize-world (0 -(worldDimensions / 2)) (worldDimensions / 2) (0 -(worldDimensions / 2)) (worldDimensions / 2) ;only few patches, s.t. people can properly be sorted into games
  set-patch-size 50
  ask patches [ ;checkerboard coloring
    ifelse (((pxcor + pycor) mod 2) = 0)[
      set pcolor grey
    ][
      set pcolor white
    ]
  ]
  set objectiveChance random-float 1
end

to prepareGame ;agents move, group credences, testimony and distance from the truth are updated and printed

  resetValues

  ask people[
    if credence > 1 [set credence 1] ;just in case
    if credence < 0 [set credence 0]
    forward random 10
    left (random 10) - 5
    set meanDistance meanDistance + abs (objectiveChance - credence)

    set groupCredences replace-item groupType groupCredences ((item groupType groupCredences) + credence)
    set groupTestimonies replace-item groupType groupTestimonies ((item groupType groupTestimonies) + testimony)

  ]

  ;refactor here?
  set groupCredences replace-item 0 groupCredences (item 0 groupCredences / item 0 groupCounts)
  set groupTestimonies replace-item 0 groupTestimonies (item 0 groupTestimonies / item 0 groupCounts)
  set groupCredences replace-item 1 groupCredences (item 1 groupCredences / item 1 groupCounts)
  set groupTestimonies replace-item 1 groupTestimonies (item 1 groupTestimonies / item 1 groupCounts)

  if item 2 groupCounts > 0 [
    set groupCredences replace-item 2 groupCredences (item 2 groupCredences / item 2 groupCounts)
    set groupTestimonies replace-item 2 groupTestimonies (item 2 groupTestimonies / item 2 groupCounts)
  ]

  set meanDistance meanDistance / countPeople
end

to resetValues
  set groupCredences (list 0 0 0)
  set groupTestimonies (list 0 0 0)
  set meanDistance 0
  set quietingCounter 0
  set smotheringCounter 0
end

to printUpdate
  print "--------------------------------------------------------------------------------------"
  print(word "After round " ticks " the average credence in P is " (precision (item 0 groupCredences) 3) " for group A, " (precision (item 1 groupCredences) 3) " for group B, and " (precision (item 2 groupCredences) 3) " for group C.")
  print(word "The average testimony given by members of group A is " (precision (item 0 groupTestimonies) 3) ", while for members of group B it is " (precision (item 1 groupTestimonies) 3) " and for members of group C it is " (precision (item 2 groupTestimonies) 3) ".")
  print(word "The mean distance from the truth for the whole population of agents is currently " (precision meanDistance 3) ".")
  print(word "This round " smotheringCounter " agents tailored their testimony, and a total of " quietingCounter " individual instances of quieting were committed.")
end

to printSetup
  print "--------------------------------------------------------------------------------------"
  print "--------------------------------------------------------------------------------------"
  print "--------------------------------------------------------------------------------------"
  print(word "New simulation started @ "date-and-time)
  print(word "In this simulation the objective value of the proposition is " (precision objectiveChance 3) ".")
  print (word "It contains " countPeople " agents, " (precision item 0 groupPercentages 1) "% group A, " (precision item 1 groupPercentages 1) "% group B and " (precision item 2 groupPercentages 1) "% group C.")
  print(word "The average credence in P is " (precision (item 0 groupCredences) 3) " for group A, " (precision (item 1 groupCredences) 3) " for group B, and " (precision (item 2 groupCredences) 3) " for group C.")
  print(word "The mean distance from the truth for the whole population of agents starts at " (precision meanDistance 3) ".")
end

to setupGroup [groupNumber]
  create-people item groupNumber groupCounts[
    set xcor random-xcor
    set ycor random-ycor
    set heading random 360
    set credence objectiveChance + random-float 1 * (item groupNumber groupBiases) ;later this bias might be featured in experiments
    set quietenTendency 0 ;with the learning function turned on, each individual might have their own tendency; until then just group values
    set groupType groupNumber
    set color item groupNumber groupColors
    set shape "person"
    set size 0.5
  ]
end
@#$#@#$#@
GRAPHICS-WINDOW
5
10
363
369
-1
-1
50.0
1
10
1
1
1
0
1
1
1
-3
3
-3
3
0
0
1
ticks
30.0

SLIDER
1
709
195
742
countTypeA
countTypeA
10
200
200.0
10
1
NIL
HORIZONTAL

BUTTON
6
578
79
611
NIL
setup
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

SLIDER
197
709
393
742
countTypeB
countTypeB
10
200
70.0
10
1
NIL
HORIZONTAL

SLIDER
1
744
195
777
biasTypeA
biasTypeA
-1
1
-1.0
0.1
1
NIL
HORIZONTAL

SLIDER
197
744
393
777
biasTypeB
biasTypeB
-1
1
1.0
0.1
1
NIL
HORIZONTAL

BUTTON
85
579
148
612
NIL
go
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

PLOT
572
14
928
164
Overall mean distance from the truth
NIL
NIL
0.0
1.0
0.0
1.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "plot meanDistance"

PLOT
571
167
930
569
Mean credences
NIL
NIL
0.0
1.0
0.0
1.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "plot objectiveChance"
"Group A" 1.0 0 -955883 true "" "plot item 0 groupCredences"
"Group B" 1.0 0 -13345367 true "" "plot item 1 groupCredences"
"Group C" 1.0 0 -6459832 true "" "plot item 2 groupCredences"

SWITCH
7
615
209
648
employLearningFunction
employLearningFunction
1
1
-1000

SLIDER
1003
60
1183
93
penaltyPerPerson
penaltyPerPerson
-10
0
-10.0
1
1
NIL
HORIZONTAL

TEXTBOX
980
27
1238
54
Utility Function Values
20
0.0
1

SLIDER
1001
96
1189
129
penaltySmothering
penaltySmothering
-10
0
-5.0
1
1
NIL
HORIZONTAL

SLIDER
156
579
328
612
worldDimensions
worldDimensions
2
10
6.0
2
1
NIL
HORIZONTAL

SLIDER
1
778
195
811
quietenThresholdA
quietenThresholdA
0.1
0.9
0.3
0.1
1
NIL
HORIZONTAL

SLIDER
197
778
393
811
quietenThresholdB
quietenThresholdB
0.1
0.9
0.9
0.1
1
NIL
HORIZONTAL

PLOT
932
167
1301
569
Mean testimonies
NIL
NIL
0.0
1.0
0.0
1.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "plot objectiveChance"
"Group A" 1.0 0 -955883 true "" "plot item 0 groupTestimonies"
"Group B" 1.0 0 -13345367 true "" "plot item 1 groupTestimonies"
"Group C" 1.0 0 -6459832 true "" "plot item 2 groupTestimonies"

TEXTBOX
220
815
370
839
Group Setup
20
0.0
1

SLIDER
395
709
593
742
countTypeC
countTypeC
0
200
0.0
10
1
NIL
HORIZONTAL

SLIDER
395
744
593
777
biasTypeC
biasTypeC
-1
1
-0.1
0.1
1
NIL
HORIZONTAL

TEXTBOX
83
678
100
702
A
20
25.0
1

SLIDER
395
778
593
811
quietenThresholdC
quietenThresholdC
0
0.9
0.3
0.1
1
NIL
HORIZONTAL

TEXTBOX
480
679
514
703
[C]
20
35.0
1

TEXTBOX
275
676
290
700
B
20
105.0
1

SWITCH
211
615
363
648
allowInjustice
allowInjustice
0
1
-1000

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
NetLogo 6.1.1
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
