breed[people person]

people-own[
  groupType ;which group does the agent belong to
  credence ;what is the agents credence in the proposition?
  quietenTendency ;how quick is the agent to quieten those of diverging identity?
  utilityQuieting ;how bad an instance of quieting committed by this agent is for the victim
  utilitySmothering ;how bad an instance of smothering is for this agent themself
  expectedPatchConsensus ;calculated in round 1 of each game, based on who else is present on the patch
  expectedDivergence ;calculated in round 1 of each game, based on the expectedPatchConsensus
  testimony ;is given in round two, based on expectedUtility and credence
  ;-------------------------------------------------------------------------------------------------------------------
  ;learning function
  averageTestimonies ;the average testimony given by members of this group to the agent
  encounters  ;the number of testimonies from these groups
  averageQuietingTendencies ;how big - on average - was this agents divergence from that groups testimony when they were quietened
  quietingCount ;how often - per group - has this agent been quietened?
  averageQuietingUtility ;how bad - on average - were the quieting instances this agent experienced
]

globals[
  objectiveChanceP ;value of the standpoint epistemological proposition that agents have credences about
  objectiveChanceQ ;value of the independent proposition
  countPeople ;total number of agents in the simulation
  ;-----------------------------------------------------------------------------------------------------------------
  ; groups 0 (A) , 1 (B) and 2 (C) (, and 3 is (if applicable) the total of all groups)
  groupBiases
  groupColors
  groupCredences
  groupTestimonies
  groupPercentages ;how many % of all agents are in groups A, B, C
  groupCounts
  groupThresholds
  ;------------------------------------------------------------------------------------------------------------------
  ;Values for plotting and updating the outputs
  meanDistance ;how far - on average - are agents from the truth
  smotheringCounter ;how often did agents smother themselves this round?
  quietingCounter ;how often did agents quieten others this round?
  smotheringCounterTotals ;how often did agents smother themselves in total so far? (list)
  quietingCounterTotals ;how often did agents quieten others in total so far? (list)
  ;------------------------------------------------------------------------------------------------------------------
  ;Values for plotting the learning functions
  credencesAboutA
  credencesAboutThresholdsA
  credencesAboutB
  credencesAboutThresholdsB
  credencesAboutC
  credencesAboutThresholdsC
  credencesAboutPenalty
  averagePenalty
  averageThresholds
]

to setup ;called at the start of each simulation
  clear-all
  reset-ticks
  setupWorld
  setupAgents
  prepareGame
  printSetup
end

to go ;called once per tick
  tick
  ifelse allowInjustice = true [playGame][skipGame]
  if experiment? = true [doExperiments]
  prepareGame
  printUpdate
  if ticks >= 300 [stop] ;usually little of interest happens after 30 ticks
end

to playGame ;determines the agent-sets for the game and lets them play three rounds
  ask patches [
    let participants turtles-here
    let countParticipants (list (count turtles-here with [groupType = 0]) (count turtles-here with [groupType = 1]) (count turtles-here with [groupType = 2]) (count participants))

    if item 3 countParticipants > 1 [ ;no solo-games
      ;-------------------------------------------------------------------------------------------
      ;-------------------------------------------------------------------------------------------
      ;" round 1"
      ask participants[
        set expectedPatchConsensus calculateExpectedPatchConsensus self countParticipants
        set expectedDivergence abs (expectedPatchConsensus - credence)
      ]
      ;-------------------------------------------------------------------------------------------
      ;" round 2"
      ask participants[
        let expectedUtility (calculateExpectedUtility self countParticipants)
        ifelse expectedUtility > utilitySmothering [;the bigger the value, the worse
          smother self
        ][
          set testimony credence ;give 'naive' testimony
        ]
      ]
      ;-------------------------------------------------------------------------------------------
      ;" round 3"
      let patchConsensus calculatePatchConsensus participants countParticipants

      ask participants [
        let relevantParticipantCount ((item 3 countParticipants) - 1)
        let inputFromThisGame 0

        ask other participants[
          ifelse (shouldQuieten? myself self patchConsensus) = TRUE [
            set inputFromThisGame quieten myself self inputFromThisGame
            if quietingType = "Ignore fully" AND relevantParticipantCount > 1 [
              set relevantParticipantCount relevantParticipantCount - 1
            ]
          ][

            ifelse [testimony] of myself = "NA" [
              set relevantParticipantCount relevantParticipantCount - 1
            ][
              ask myself [set inputFromThisGame inputFromThisGame + [testimony] of myself] ;add their testimony to the input

              if employLearningFunction = TRUE [
                updateKnowledgeOfCredences myself self
              ]
            ]
          ]

        ]
        set inputFromThisGame inputFromThisGame / relevantParticipantCount
        set credence (inputFromThisGame * weightOfInput) + (credence * (1 - weightOfInput));update credence after collecting all the input from the game
      ]
     ;------------------------------------------------------------------------------------------------
    ]
  ]
end


to doExperiments ;agents slowly get closer to the truth on their own
  ask people[
    ;needs a more realistic way of updating
    if experimentType = "Custom"[
      set credence credence + (objectiveChanceP - credence) * 0.1
      set credence (credence + item groupType groupBiases * 0.01 )
    ]
    if experimentType = "Douven"[
      ;put in later
    ]
  ]
end

to skipGame ;if the simulation disallows testimonial injustice, everyone just updates by splitting the difference with the mean of the other participants
  ask patches [
    let participants turtles-here
    let countParticipants count participants

    ask participants[
      let input 0
      if countParticipants > 1 [
        ask other participants[
          ask myself [
            set input input + [credence] of myself
          ]
        ]
        set input input / (countParticipants - 1)
        set credence (input * weightOfInput) + (credence * (1 - weightOfInput));update credence after collecting all the input from the game

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
  set-patch-size 450 / (worldDimensions + 1)
  ask patches [ ;checkerboard coloring
    ifelse (((pxcor + pycor) mod 2) = 0)[
      set pcolor grey
    ][
      set pcolor white
    ]
  ]
  ifelse staticOneHalfP = FALSE [set objectiveChanceP random-float 1][set objectiveChanceP 0.5]

  set quietingCounterTotals (list 0 0 0 0)
  set smotheringCounterTotals (list 0 0 0 0)

end

to prepareGame ;agents move, group credences, testimony and distance from the truth are updated
  resetValues

  set quietingCounterTotals replace-item 3 quietingCounterTotals (item 0 quietingCounterTotals + item 1 quietingCounterTotals + item 2 quietingCounterTotals)
  set smotheringCounterTotals replace-item 3 smotheringCounterTotals (item 0 smotheringCounterTotals + item 1 smotheringCounterTotals + item 2 smotheringCounterTotals)

  ask people[
    if credence > 1 [set credence 1] ;just in case
    if credence < 0 [set credence 0]
    forward random 10
    left (random 10) - 5
    set meanDistance meanDistance + abs (objectiveChanceP - credence)

    set groupCredences replace-item groupType groupCredences ((item groupType groupCredences) + credence)
    set groupTestimonies replace-item groupType groupTestimonies ((item groupType groupTestimonies) + testimony)


    ;update what agents believe about others
    set credencesAboutA replace-item groupType credencesAboutA ((item groupType credencesAboutA) + item 0 averageTestimonies)
    set credencesAboutB replace-item groupType credencesAboutB ((item groupType credencesAboutB) + item 1 averageTestimonies)
    set credencesAboutC replace-item groupType credencesAboutC ((item groupType credencesAboutC) + item 2 averageTestimonies)
    set credencesAboutThresholdsA replace-item groupType credencesAboutThresholdsA ((item groupType credencesAboutThresholdsA) + item 0 averageQuietingTendencies)
    set credencesAboutThresholdsB replace-item groupType credencesAboutThresholdsB ((item groupType credencesAboutThresholdsB) + item 1 averageQuietingTendencies)
    set credencesAboutThresholdsC replace-item groupType credencesAboutThresholdsC ((item groupType credencesAboutThresholdsC) + item 2 averageQuietingTendencies)
    set credencesAboutPenalty replace-item groupType credencesAboutPenalty ((item groupType credencesAboutPenalty) + averageQuietingUtility)



  ]

  ;----------------------------------------------------------------------------------------------------------------------------------------------------------------------
  ;refactor here!!!
  set groupCredences replace-item 0 groupCredences (item 0 groupCredences / item 0 groupCounts)
  set groupTestimonies replace-item 0 groupTestimonies (item 0 groupTestimonies / item 0 groupCounts)
  set groupCredences replace-item 1 groupCredences (item 1 groupCredences / item 1 groupCounts)
  set groupTestimonies replace-item 1 groupTestimonies (item 1 groupTestimonies / item 1 groupCounts)

  set credencesAboutA replace-item 0 credencesAboutA ((item 0 credencesAboutA) / item 0 groupCounts)
  set credencesAboutB replace-item 0 credencesAboutB ((item 0 credencesAboutB) / item 0 groupCounts)
  set credencesAboutC replace-item 0 credencesAboutC ((item 0 credencesAboutC) / item 0 groupCounts)
  set credencesAboutThresholdsA replace-item 0 credencesAboutThresholdsA ((item 0 credencesAboutThresholdsA) / item 0 groupCounts)
  set credencesAboutThresholdsB replace-item 0 credencesAboutThresholdsB ((item 0 credencesAboutThresholdsB) / item 0 groupCounts)
  set credencesAboutThresholdsC replace-item 0 credencesAboutThresholdsC ((item 0 credencesAboutThresholdsC) / item 0 groupCounts)
  set credencesAboutPenalty replace-item 0 credencesAboutPenalty ((item 0 credencesAboutPenalty) / item 0 groupCounts)

  set credencesAboutA replace-item 1 credencesAboutA ((item 1 credencesAboutA) / item 1 groupCounts)
  set credencesAboutB replace-item 1 credencesAboutB ((item 1 credencesAboutB) / item 1 groupCounts)
  set credencesAboutC replace-item 1 credencesAboutC ((item 1 credencesAboutC) / item 1 groupCounts)
  set credencesAboutThresholdsA replace-item 1 credencesAboutThresholdsA ((item 1 credencesAboutThresholdsA) / item 1 groupCounts)
  set credencesAboutThresholdsB replace-item 1 credencesAboutThresholdsB ((item 1 credencesAboutThresholdsB) / item 1 groupCounts)
  set credencesAboutThresholdsC replace-item 1 credencesAboutThresholdsC ((item 1 credencesAboutThresholdsC) / item 1 groupCounts)
  set credencesAboutPenalty replace-item 1 credencesAboutPenalty ((item 1 credencesAboutPenalty) / item 1 groupCounts)


  if item 2 groupCounts > 0 [
    set groupCredences replace-item 2 groupCredences (item 2 groupCredences / item 2 groupCounts)
    set groupTestimonies replace-item 2 groupTestimonies (item 2 groupTestimonies / item 2 groupCounts)
    set credencesAboutA replace-item 2 credencesAboutA ((item 2 credencesAboutA) / item 2 groupCounts)
    set credencesAboutB replace-item 2 credencesAboutB ((item 2 credencesAboutB) / item 2 groupCounts)
    set credencesAboutC replace-item 2 credencesAboutC ((item 2 credencesAboutC) / item 2 groupCounts)
    set credencesAboutThresholdsA replace-item 2 credencesAboutThresholdsA ((item 2 credencesAboutThresholdsA) / item 2 groupCounts)
    set credencesAboutThresholdsB replace-item 2 credencesAboutThresholdsB ((item 2 credencesAboutThresholdsB) / item 2 groupCounts)
    set credencesAboutThresholdsC replace-item 2 credencesAboutThresholdsC ((item 2 credencesAboutThresholdsC) / item 2 groupCounts)
    set credencesAboutPenalty replace-item 2 credencesAboutPenalty ((item 2 credencesAboutPenalty) / item 2 groupCounts)
  ]

  set meanDistance meanDistance / countPeople
end

to resetValues
  set groupCredences (list 0 0 0)
  set groupTestimonies (list 0 0 0)
  set credencesAboutA (list 0 0 0)
  set credencesAboutB (list 0 0 0)
  set credencesAboutC (list 0 0 0)
  set credencesAboutThresholdsA (list 0 0 0)
  set credencesAboutThresholdsB (list 0 0 0)
  set credencesAboutThresholdsC (list 0 0 0)
  set credencesAboutPenalty (list 0 0 0)
  set meanDistance 0
  set quietingCounter 0
  set smotheringCounter 0
end

to printUpdate
  print "--------------------------------------------------------------------------------------"
  print(word "After round " ticks " the average credence in P is " (precision (item 0 groupCredences) 3) " for group A, " (precision (item 1 groupCredences) 3) " for group B, and " (precision (item 2 groupCredences) 3) " for group C.")
  print(word "The average testimony given by members of group A is " (precision (item 0 groupTestimonies) 3) ", while for members of group B it is " (precision (item 1 groupTestimonies) 3) " and for members of group C it is " (precision (item 2 groupTestimonies) 3) ".")
  print(word "The mean distance from the truth for the whole population of agents is currently " (precision meanDistance 3) ".")
  print(word "This round " smotheringCounter " agents tailored their testimony (total of "item 3 smotheringCounterTotals ") , and  " quietingCounter " individual instances of quieting were committed (total of " item 3 quietingCounterTotals ").")
end

to printSetup
  print "--------------------------------------------------------------------------------------"
  print "--------------------------------------------------------------------------------------"
  print "--------------------------------------------------------------------------------------"
  print(word "New simulation started @ "date-and-time)
  print(word "In this simulation the objective value of the proposition is " (precision objectiveChanceP 3) ".")
  print (word "It contains " countPeople " agents, " (precision item 0 groupPercentages 1) "% group A, " (precision item 1 groupPercentages 1) "% group B and " (precision item 2 groupPercentages 1) "% group C.")
  print(word "The average credence in P is " (precision (item 0 groupCredences) 3) " for group A, " (precision (item 1 groupCredences) 3) " for group B, and " (precision (item 2 groupCredences) 3) " for group C.")
  print(word "The mean distance from the truth for the whole population of agents starts at " (precision meanDistance 3) ".")
end

to setupGroup [groupNumber]
  create-people item groupNumber groupCounts[
    ;-----------------------------------------------------------------------------------------------------
    ;basic features
    set xcor random-xcor
    set ycor random-ycor
    set heading random 360
    set shape "person"
    set size 0.3
    ;-----------------------------------------------------------------------------------------------------
    ;group specific features
    set credence objectiveChanceP + random-float 2 * (item groupNumber groupBiases) ; group bias influences distance from the truth
    set quietenTendency random-float 2 * (item groupNumber GroupThresholds) ; group tendency influences individual threshold
    set groupType groupNumber
    set color item groupNumber groupColors
    ;-----------------------------------------------------------------------------------------------------
    ;utility function features
    set utilityQuieting random-float (2 * penaltyPerPerson) ;this is how bad an act of quieting committed by this agent is
    set utilitySmothering random-float (2 * penaltySmothering) ;this is how badly this agent suffers from smothering
    ;-----------------------------------------------------------------------------------------------------
    ;learning function features
    set quietingCount (list 0 0 0 0)
    set encounters (list 0 0 0)
    if initialValues = "All 0"[
      set averageTestimonies (list 0 0 0 0)
      set averageQuietingTendencies (list 0 0 0)
      set averageQuietingUtility 0
    ]
    if initialValues = "Custom" [
      set averageTestimonies (list 0.5 0.5 0.5 0.5)
      set averageQuietingTendencies (list 0.5 0.5 0.5)
      set averageQuietingUtility 2
    ]
    if initialValues = "All random" [
      set averageTestimonies (list 0.5 0.5 0.5 0.5)
      let i random-float 1
      set averageQuietingTendencies (list i i i)
      set averageQuietingUtility random 10
    ]
  ]
end

to-report shouldQuieten? [aggressor victim patchConsensus]
  let divergenceAggressorConsensus 0
  let divergenceVictimConsensus 0
  let divergenceVictimAggressor 0
  let relevantThreshold 0

  if [testimony] of victim = "NA" [
        report FALSE
      ]

  ask aggressor [
    set divergenceAggressorConsensus abs (credence - patchConsensus)

    ask victim [

      set divergenceVictimConsensus abs (testimony - patchConsensus)
      set divergenceVictimAggressor abs (testimony - [credence] of myself)
      set relevantThreshold [quietenTendency] of myself


    ]


  ]

  ;conditions for quieting:
  ifelse ([groupType] of aggressor != [groupType] of victim) ;different group identities
  AND divergenceAggressorConsensus < divergenceVictimConsensus ;credence of aggressor is closer to the group consensus than testimony of victim
  AND divergenceVictimAggressor > relevantThreshold ;testimony of victim is far enough from credence of aggressor
  ;AND divergenseVictimConsensus > relevantThreshold ;testimony of victim is far enough from patch consensus
  [report true][report false]


end

to-report quieten [aggressor victim input ]


  ask victim[


    if employLearningFunction = TRUE [
      ;update the agents experience about quieting tendencies of other agents
      let difference 0
      if learningTendenciesType = "Difference to credence"[
        set difference abs (testimony - [credence] of aggressor)
      ]
      if learningTendenciesType = "Difference to testimony"[
        set difference abs (testimony - [testimony] of aggressor)
      ]
      set averageQuietingTendencies replace-item ([groupType] of aggressor) averageQuietingTendencies (((item ([groupType] of aggressor) averageQuietingTendencies) * item ([groupType] of aggressor) quietingCount + difference) / (item ([groupType] of aggressor) quietingCount + 1))

      ;updates the average severity of being quietened
      set averageQuietingUtility (averageQuietingUtility * item 3 quietingCount + [utilityQuieting] of aggressor) / (item 3 quietingCount + 1)

      set quietingCount replace-item 3 quietingCount (item 3 quietingCount + 1)
      set quietingCount replace-item ([groupType] of aggressor) quietingCount (item ([groupType] of aggressor) quietingCount + 1)

      ;updates the aggressors information about the average credences of each group
      ask aggressor [
        if learningCredencesType = "Update only on what one wants to hear"[;update with aggressor credence
          set averageTestimonies replace-item ([groupType] of victim) averageTestimonies ((item ([groupType] of victim) averageTestimonies * item ([groupType] of victim) encounters + credence) / (item ([groupType] of victim) encounters + 1))
          set encounters replace-item ([groupType] of victim) encounters (item ([groupType] of victim) encounters + 1) ;the aggressor counts the encounter


          ]
        if learningCredencesType = "Update on the actual testimony"[;update with victim testimony
          updateKnowledgeOfCredences aggressor victim
        ]

      ]
  ]



    if quietingType = "Slot in own credence"[
      ask aggressor [
        set input input + credence
      ]
    ]

    if quietingType = "Split the difference"[
      ask aggressor [
        set input input + (([testimony] of myself + credence) / 2)
      ]
    ]

    if quietingType = "Ignore fully"[
      ask aggressor [
        ;count down
      ]
    ]


    set quietingCounter quietingCounter + 1
    set quietingCounterTotals replace-item groupType quietingCounterTotals (item groupType quietingCounterTotals + 1) ;adds the quieting to the group type of the agent being quietened


  ]
  report input
end

to updateKnowledgeOfCredences [aggressor victim]
  set averageTestimonies replace-item ([groupType] of victim) averageTestimonies ((item ([groupType] of victim) averageTestimonies * item ([groupType] of victim) encounters + [testimony] of victim) / (item ([groupType] of victim) encounters + 1))
  set encounters replace-item ([groupType] of victim) encounters (item ([groupType] of victim) encounters + 1) ;the aggressor counts the encounter
end

to smother [agent]
  ask agent [
    if smotheringType = "Split the difference with expected Patch Consensus"[
      set testimony (credence + expectedPatchConsensus) / 2
    ]

    if smotheringType = "Utter expected Patch Consensus" [
      set testimony expectedPatchConsensus
    ]
    if smotheringType = "Withold testimony" [
      set testimony "NA"
      ;do nothing
      ;set testimony = "NA"
      ;then I would have to discount NAs from the patch consensus
      ;NAs don't get quietened
    ]


    set smotheringCounter smotheringCounter + 1

    set smotheringCounterTotals replace-item groupType smotheringCounterTotals (item groupType smotheringCounterTotals + 1)
  ]
end

to-report calculateExpectedPatchConsensus [agent countParticipants]
  let result 0


    ask agent [
    let data 0
    ifelse employLearningFunction = TRUE [ ;Do the agents have access to the objective averages, or do they have to rely on personal experience?
      set data averageTestimonies
    ][
      set data groupCredences
    ]


      if patchConsensusType = "Add own credence" [
        ifelse groupType = 0 [
          set expectedPatchConsensus (credence + ((item 0 countParticipants) - 1) * (item 0 data) + (item 1 countParticipants) * (item 1 data) + (item 2 countParticipants) * (item 2 data))
        ][
          ifelse groupType = 1[
            set result (credence + (item 0 countParticipants) * (item 0 data) + ((item 1 countParticipants) - 1) * (item 1 data) + (item 2 countParticipants) * (item 2 data))
          ][
            set result (credence + (item 0 countParticipants) * (item 0 data) + (item 1 countParticipants) * (item 1 data) + ((item 2 countParticipants) - 1) * (item 2 data))
          ]
        ]
        set result result / (item 3 countParticipants)
      ]

      if patchConsensusType = "Ommit own credence" [
        ifelse groupType = 0 [
          set result (((item 0 countParticipants) - 1) * (item 0 data) + (item 1 countParticipants) * (item 1 data) + (item 2 countParticipants) * (item 2 data)) / ((item 3 countParticipants) - 1)
        ][
          ifelse groupType = 1[
            set result ((item 0 countParticipants) * (item 0 data) + ((item 1 countParticipants) - 1) * (item 1 data) + (item 2 countParticipants) * (item 2 data)) / ((item 3 countParticipants) - 1)
          ][
            set result ((item 0 countParticipants) * (item 0 data) + (item 1 countParticipants) * (item 1 data) + ((item 2 countParticipants) - 1) * (item 2 data)) / ((item 3 countParticipants) - 1)
          ]
        ]
        set result result / ((item 3 countParticipants) - 1)
      ]

    ; possibly add additional versions of calculating the Patch consensus here

    ]


  report result

end

to-report calculatePatchConsensus [participants countParticipants]
  let result 0
  let relevantParticipantCount item 3 countParticipants
  ask participants [
    ifelse testimony = "NA"[
      set relevantParticipantCount relevantParticipantCount - 1
    ][
    set result result + testimony
    ]
  ]
  set result result / relevantParticipantCount
  report result
end

to-report calculateExpectedUtility [agent countParticipants] ;calculated in round 2 of each game, based on expected patch consensus and expected divergence ;add more conditions here
  let expectedUtility 0

  ask agent[
    let relevantPenalty 0
    let relevantThresholds 0

    ifelse employLearningFunction = TRUE [ ;Do the agents have access to the objective averages, or do they have to rely on personal experience?
      set relevantPenalty averageQuietingUtility
      set relevantThresholds averageQuietingTendencies
    ][
      set relevantPenalty penaltyPerPerson
      set relevantThresholds groupThresholds
    ]

    if groupType = 0[
      if expectedDivergence > item 1 relevantThresholds ;and abs (credence - item 1 groupCredences) > quietenThresholdB
      [
        set expectedUtility expectedUtility + (item 1 countParticipants) * relevantPenalty
      ]
      if expectedDivergence > item 2 relevantThresholds [
        set expectedUtility expectedUtility + (item 2 countParticipants) * relevantPenalty
      ]
    ]

    if groupType = 1[
      if expectedDivergence > item 0 relevantThresholds [
        set expectedUtility expectedUtility + (item 0 countParticipants) * relevantPenalty
      ]
      if expectedDivergence > item 2 relevantThresholds [
        set expectedUtility expectedUtility + (item 2 countParticipants) * relevantPenalty
      ]
    ]

    if groupType = 2[
      if expectedDivergence > item 0 relevantThresholds [
        set expectedUtility expectedUtility + (item 0 countParticipants) * relevantPenalty
      ]
      if expectedDivergence > item 1 relevantThresholds [
        set expectedUtility expectedUtility + (item 1 countParticipants) * relevantPenalty
      ]
    ]
  ]
  report expectedUtility
end
@#$#@#$#@
GRAPHICS-WINDOW
5
10
462
468
-1
-1
64.28571428571429
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
744
195
777
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
5
477
78
510
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
744
393
777
countTypeB
countTypeB
10
200
100.0
10
1
NIL
HORIZONTAL

SLIDER
1
779
195
812
biasTypeA
biasTypeA
-1
1
-0.2
0.1
1
NIL
HORIZONTAL

SLIDER
197
779
393
812
biasTypeB
biasTypeB
-1
1
0.2
0.1
1
NIL
HORIZONTAL

BUTTON
84
478
147
511
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
466
10
757
130
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
467
135
792
469
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
"default" 1.0 0 -16777216 true "" "plot objectiveChanceP"
"Group A" 1.0 0 -955883 true "" "plot item 0 groupCredences"
"Group B" 1.0 0 -13345367 true "" "plot item 1 groupCredences"
"Group C" 1.0 0 -6459832 true "" "plot item 2 groupCredences"

SWITCH
1122
512
1324
545
employLearningFunction
employLearningFunction
0
1
-1000

SLIDER
738
510
918
543
penaltyPerPerson
penaltyPerPerson
0
10
2.0
1
1
NIL
HORIZONTAL

TEXTBOX
758
475
923
501
Utility Function
20
0.0
1

SLIDER
736
546
924
579
penaltySmothering
penaltySmothering
0
10
5.0
1
1
NIL
HORIZONTAL

SLIDER
216
478
388
511
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
813
195
846
quietenThresholdA
quietenThresholdA
0.1
0.9
0.1
0.1
1
NIL
HORIZONTAL

SLIDER
197
813
393
846
quietenThresholdB
quietenThresholdB
0.1
0.9
0.1
0.1
1
NIL
HORIZONTAL

PLOT
795
135
1088
469
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
"default" 1.0 0 -16777216 true "" "plot objectiveChanceP"
"Group A" 1.0 0 -955883 true "" "plot item 0 groupTestimonies"
"Group B" 1.0 0 -13345367 true "" "plot item 1 groupTestimonies"
"Group C" 1.0 0 -6459832 true "" "plot item 2 groupTestimonies"

TEXTBOX
223
685
373
709
Group Setup
20
0.0
1

SLIDER
395
744
593
777
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
779
593
812
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
713
100
737
A
20
25.0
1

SLIDER
395
813
593
846
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
714
514
738
[C]
20
35.0
1

TEXTBOX
275
711
290
735
B
20
105.0
1

SWITCH
755
584
907
617
allowInjustice
allowInjustice
0
1
-1000

BUTTON
151
478
214
511
NIL
go
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

TEXTBOX
601
782
751
842
|\n| Average\n| Values\n|
12
0.0
1

CHOOSER
211
631
411
676
quietingType
quietingType
"Ignore fully" "Slot in own credence" "Split the difference"
1

TEXTBOX
934
513
1084
573
|\n| Average\n| Values\n|
12
0.0
1

CHOOSER
1
577
421
622
smotheringType
smotheringType
"Utter expected Patch Consensus" "Withhold testimony" "Split the difference with expected Patch Consensus"
0

SWITCH
1139
553
1283
586
experiment?
experiment?
0
1
-1000

CHOOSER
0
632
200
677
patchConsensusType
patchConsensusType
"Ommit own credence" "Add own credence"
0

TEXTBOX
91
547
483
573
Communication Settings
20
0.0
1

PLOT
1092
56
1520
262
Quieting of groups
NIL
NIL
0.0
10.0
0.0
10.0
true
false
"" ""
PENS
"Total" 1.0 0 -16777216 true "" "plot item 3 quietingCounterTotals"
"Group A" 1.0 0 -955883 true "" "plot item 0 quietingCounterTotals"
"Group B" 1.0 0 -13345367 true "" "plot item 1 quietingCounterTotals"
"Group C" 1.0 0 -6459832 true "" "plot item 2 quietingCounterTotals"

PLOT
1092
264
1520
469
Smotherings of groups
NIL
NIL
0.0
10.0
0.0
10.0
true
false
"" ""
PENS
"Total" 1.0 0 -16777216 true "" "plot item 3 smotheringCounterTotals"
"Group A" 1.0 0 -955883 true "" "plot item 0 smotheringCounterTotals"
"Group B" 1.0 0 -13345367 true "" "plot item 1 smotheringCounterTotals"
"Group C" 1.0 0 -6459832 true "" "plot item 2 smotheringCounterTotals"

SWITCH
393
477
556
510
staticOneHalfP
staticOneHalfP
0
1
-1000

CHOOSER
1094
592
1380
637
learningCredencesType
learningCredencesType
"Update only on what one wants to hear" "Update on the actual testimony"
0

CHOOSER
1123
646
1340
691
learningTendenciesType
learningTendenciesType
"Difference to credence" "Difference to testimony"
0

TEXTBOX
1142
482
1330
509
Learning Function
20
0.0
1

CHOOSER
1161
702
1299
747
initialValues
initialValues
"All 0" "All random" "Custom"
0

CHOOSER
1163
758
1302
803
experimentType
experimentType
"Custom" "Douven"
0

SLIDER
1141
810
1335
843
weightOfInput
weightOfInput
0.1
0.9
0.1
0.1
1
NIL
HORIZONTAL

PLOT
1526
41
1816
191
Credences About the Penalty
NIL
NIL
0.0
1.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "plot penaltyPerPerson "
"group A" 1.0 0 -955883 true "" "plot item 0 credencesAboutPenalty"
"group B" 1.0 0 -13345367 true "" "plot item 1 credencesAboutPenalty"
"group C" 1.0 0 -6459832 true "" "plot item 2 credencesAboutPenalty"

TEXTBOX
1668
10
1929
58
Learning Function Plots
20
0.0
1

PLOT
1528
198
1817
348
Credences about Credences of A
NIL
NIL
0.0
10.0
0.0
1.0
true
false
"" ""
PENS
"Actual Value" 1.0 0 -16777216 true "" "plot item 0 groupCredences"
"group A" 1.0 0 -955883 true "" "plot item 0 credencesAboutA"
"group B" 1.0 0 -13345367 true "" "plot item 1 credencesAboutA"
"group C" 1.0 0 -6459832 true "" "plot item 2 credencesAboutA"

PLOT
1820
197
2105
347
Credences about Thresholds of A
NIL
NIL
0.0
10.0
0.0
1.0
true
false
"" ""
PENS
"Actual Value" 1.0 0 -16777216 true "" "plot quietenThresholdA"
"group B" 1.0 0 -13345367 true "" "plot item 1 credencesAboutThresholdsA"
"group C" 1.0 0 -6459832 true "" "plot item 2 credencesAboutThresholdsA"

PLOT
1527
354
1818
504
Credences about Credences of B
NIL
NIL
0.0
10.0
0.0
1.0
true
false
"" ""
PENS
"Actual Value" 1.0 0 -16777216 true "" "plot item 1 groupCredences"
"group A" 1.0 0 -955883 true "" "plot item 0 credencesAboutB"
"group B" 1.0 0 -13345367 true "" "plot item 1 credencesAboutB"
"group C" 1.0 0 -6459832 true "" "plot item 2 credencesAboutB"

PLOT
1821
354
2106
504
Credences about Thresholds of B
NIL
NIL
0.0
10.0
0.0
1.0
true
false
"" ""
PENS
"actual Value" 1.0 0 -16777216 true "" "plot quietenThresholdA"
"group A" 1.0 0 -955883 true "" "plot item 0 credencesAboutThresholdsA"
"group C" 1.0 0 -6459832 true "" "plot item 2 credencesAboutThresholdsA"

PLOT
1527
508
1819
658
Credences about Credences of C
NIL
NIL
0.0
10.0
0.0
1.0
true
false
"" ""
PENS
"Actual Value" 1.0 0 -16777216 true "" "plot item 2 groupCredences"
"Group A" 1.0 0 -955883 true "" "plot item 0 credencesAboutC"
"Group B" 1.0 0 -13345367 true "" "plot item 1 credencesAboutC"
"Group C" 1.0 0 -6459832 true "" "plot item 2 credencesAboutC"

PLOT
1822
509
2106
659
Credences about Thresholds of C
NIL
NIL
0.0
10.0
0.0
10.0
true
false
"" ""
PENS
"Actual Value" 1.0 0 -16777216 true "" "plot quietenThresholdC"
"Group A" 1.0 0 -955883 true "" "plot item 0 credencesAboutC"
"Group B" 1.0 0 -13345367 true "" "plot item 1 credencesAboutC"

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
