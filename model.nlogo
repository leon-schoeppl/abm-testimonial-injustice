extensions [array] ;mutable and therefore faster than lists
breed[people person] ;makes everything easier to read

people-own[
  groupType ;which group does the agent belong to?
  credence ;what is the agents credence in the proposition?
  bias ;how biased is this particular agent?
  quietenTendency ;how quick is the agent to quieten those of diverging identity? (threshold value)
  utilityQuieting ;how bad is an instance of quieting committed by this agent for the victim?
  utilitySmothering ;how bad is an instance of smothering for this agent themself?
  expectedPatchConsensus ;calculated in round 1 of each game, based on who else is present on the patch
  expectedDivergence ;calculated in round 1 of each game, based on the expectedPatchConsensus
  testimony ;is given in round two, based on expectedUtility and credence - visible to other agents
  ;-------------------------------------------------------------------------------------------------------------------
  ;learning function
  averageTestimonies ;the average testimony given by members of this group to the agent
  encounters  ;the number of testimonies from these groups
  averageQuietingDelta ;used for split-the-means estimation of quieting thresholds
  averageNonQuietingDelta ;used for split-the-means estimation of quieting thresholds
  averageQuietingTendencies ;how big - on average - was this agents divergence from that groups testimony when they were quietened
  quietingCount ;how often - per group - has this agent been quietened?
  averageQuietingUtility ;how bad - on average - were the quieting instances this agent experienced
  quietingExperiences ;used for adjust-expectations estimations of quieting thresholds
  unexpectedQuietings ;used for adjust-expectations estimations of quieting thresholds
  unexpectedNonQuietings ;used for adjust-expectations estimations of quieting thresholds
]

globals[
  objectiveChanceP ;value of the standpoint epistemological proposition that agents have credences about
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
  credencesAboutA ;what do agents of the different groups believe about the mean credence of group A?
  credencesAboutThresholdsA ;what do agents of the different groups believe about the mean quietingThreshold of group A?
  credencesAboutB
  credencesAboutThresholdsB
  credencesAboutC
  credencesAboutThresholdsC
  credencesAboutPenalty ;what do agents of the different groups believe about the penalty for being quietened?
  averagePenalty ;what, actually, is the mean penalty for being quietened?
  averageThresholds ;What's the mean threshold for quieting for each group?
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
  if printUpdates? = TRUE [printUpdate]
  if ticks >= 300 [stop] ;usually little of interest happens after that many ticks
end

to playGame ;determines the agent-sets for the game and lets them play three rounds
  ask patches [
    let participants turtles-here
    let countParticipants array:from-list (list (count turtles-here with [groupType = 0]) (count turtles-here with [groupType = 1]) (count turtles-here with [groupType = 2]) (count participants))

    if array:item countParticipants 3 > 1 [ ;no solo-games
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
        let relevantParticipantCount ((array:item countParticipants 3) - 1); count only other participants
        let inputFromThisGame 0

        ask other participants[
          ifelse (shouldQuieten? myself self patchConsensus) = TRUE [
            updateAverageThresholds TRUE myself self patchConsensus
            set inputFromThisGame quieten myself self inputFromThisGame
            if quietingType = "Ignore fully" AND relevantParticipantCount > 1 [
              set relevantParticipantCount relevantParticipantCount - 1
            ]
          ][
            updateAverageThresholds FALSE myself self patchConsensus

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
    if biasType = "Perpetual" [
      set credence (w * (objectiveChanceP + random-float 1 * Bias) + (1 - w) * credence)
    ]

    if biasType = "Resolving"  [
      let delta abs (credence - objectiveChanceP)
      set credence ( w * (objectiveChanceP + bias * delta) + (1 - w) * credence)
    ]

    if biasType = "None" [
      set credence (w * objectiveChanceP + (1 - w) * credence)
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
  set groupColors array:from-list (list orange blue brown)
  set groupBiases array:from-list (list biasTypeA biasTypeB biasTypeC)
  set groupCounts array:from-list (list countTypeA countTypeB countTypeC)
  set countPeople array:item groupCounts 0 + array:item groupCounts 2 + array:item groupCounts 2
  set groupPercentages array:from-list (list ((countTypeA / countPeople) * 100) ((countTypeB / countPeople) * 100) ((countTypeC / countPeople) * 100))
  set groupThresholds array:from-list (list quietenThresholdA quietenThresholdB quietenThresholdC)

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

  set quietingCounterTotals array:from-list (list 0 0 0 0)
  set smotheringCounterTotals array:from-list (list 0 0 0 0)

end

to prepareGame ;agents move, group credences, testimony and distance from the truth are updated
  resetValues

  ; Update the all-groups totals of quieting and smothering
  array:set quietingCounterTotals 3 (array:item quietingCounterTotals 0 + array:item quietingCounterTotals 1 + array:item quietingCounterTotals 2)
  array:set smotheringCounterTotals 3 (array:item smotheringCounterTotals 0 + array:item smotheringCounterTotals 1 + array:item smotheringCounterTotals 2)

    ask people[

    if employLearningFunction = true [
      if calculateTendenciesType = "Adjust expectations"[
        foreach [0 1 2][
          x ->
          array:set averageQuietingTendencies x (array:item averageQuietingTendencies x + 0.1 * array:item unexpectedNonQuietings x - 0.1 * array:item unexpectedQuietings x)
          if array:item averageQuietingTendencies x < 0 [
            array:set averageQuietingTendencies x 0
          ]
          if array:item averageQuietingTendencies x > 1 [
            array:set averageQuietingTendencies x 1
          ]
        ]

        set unexpectedQuietings array:from-list (list 0 0 0)
        set unexpectedNonQuietings array:from-list (list 0 0 0)
      ]

      if calculateTendenciesType = "Know objective values"[
        foreach [0 1 2][
          x ->
          array:set averageQuietingTendencies x array:item groupThresholds x
        ]

      ]


      if calculateTendenciesType = "Split the means"[
        foreach [0 1 2][
          x ->
          array:set averageQuietingTendencies x ((array:item averageQuietingDelta x + array:item averageNonQuietingDelta x) / 2)
        ]
      ]
    ]


    if credence > 1 [set credence 1] ;just in case
    if credence < 0 [set credence 0]
    forward random 10
    left (random 10) - 5
    set meanDistance meanDistance + abs (objectiveChanceP - credence)

    array:set groupCredences groupType (array:item groupCredences groupType + credence)
    array:set groupTestimonies groupType (array:item groupTestimonies groupType + testimony)
    set averagePenalty averagePenalty + utilityQuieting



    ;update what agents believe about others
    array:set credencesAboutA groupType (array:item credencesAboutA groupType + array:item averageTestimonies 0)
    array:set credencesAboutB groupType (array:item credencesAboutB groupType + array:item averageTestimonies 1)
    array:set credencesAboutC groupType (array:item credencesAboutC groupType + array:item averageTestimonies 2)

    array:set credencesAboutThresholdsA groupType (array:item credencesAboutThresholdsA groupType + array:item averageQuietingTendencies 0)
    array:set credencesAboutThresholdsB groupType (array:item credencesAboutThresholdsB groupType + array:item averageQuietingTendencies 1)
    array:set credencesAboutThresholdsC groupType (array:item credencesAboutThresholdsC groupType + array:item averageQuietingTendencies 2)

    array:set credencesAboutPenalty groupType (array:item credencesAboutPenalty groupType + averageQuietingUtility)
    array:set averageThresholds groupType (array:item averageThresholds groupType + quietenTendency)


  ]

  foreach [0 1 2] [
    x ->
    if array:item groupCounts x > 0 [
      array:set groupCredences x (array:item groupCredences x / array:item groupCounts x)
      array:set groupTestimonies x (array:item groupTestimonies x / array:item groupCounts x)
      array:set credencesAboutA x (array:item credencesAboutA x / array:item groupCounts x)
      array:set credencesAboutB x (array:item credencesAboutB x / array:item groupCounts x)
      array:set credencesAboutC x (array:item credencesAboutC x / array:item groupCounts x)
      array:set credencesAboutThresholdsA x (array:item credencesAboutThresholdsA x / array:item groupCounts x)
      array:set credencesAboutThresholdsB x (array:item credencesAboutThresholdsB x / array:item groupCounts x)
      array:set credencesAboutThresholdsC x (array:item credencesAboutThresholdsC x / array:item groupCounts x)
      array:set credencesAboutPenalty x (array:item credencesAboutPenalty x / array:item groupCounts x)
      array:set averageThresholds x (array:item averageThresholds x / array:item groupCounts x)
          ]
  ]
  set averagePenalty averagePenalty / countPeople
  set meanDistance meanDistance / countPeople

end

to resetValues
  set groupCredences array:from-list (list 0 0 0)
  set groupTestimonies array:from-list (list 0 0 0)
  set credencesAboutA array:from-list (list 0 0 0)
  set credencesAboutB array:from-list (list 0 0 0)
  set credencesAboutC array:from-list (list 0 0 0)
  set credencesAboutThresholdsA array:from-list (list 0 0 0)
  set credencesAboutThresholdsB array:from-list (list 0 0 0)
  set credencesAboutThresholdsC array:from-list (list 0 0 0)
  set credencesAboutPenalty array:from-list (list 0 0 0)
  set meanDistance 0
  set quietingCounter 0
  set smotheringCounter 0
  set averagePenalty 0
  set averageThresholds array:from-list (list 0 0 0)
end

to printUpdate
  print "--------------------------------------------------------------------------------------"
  print(word "After round " ticks " the average credence in P is " (precision (array:item groupCredences 0) 3) " for group A, " (precision (array:item groupCredences 1) 3) " for group B, and " (precision (array:item groupCredences 2) 3) " for group C.")
  print(word "The average testimony given by members of group A is " (precision (array:item groupTestimonies 0) 3) ", while for members of group B it is " (precision (array:item groupTestimonies 1) 3) " and for members of group C it is " (precision (array:item groupTestimonies 2) 3) ".")
  print(word "The mean distance from the truth for the whole population of agents is currently " (precision meanDistance 3) ".")
  print(word "This round " smotheringCounter " agents tailored their testimony (total of " array:item smotheringCounterTotals 3 ") , and  " quietingCounter " individual instances of quieting were committed (total of " array:item quietingCounterTotals 3").")
end

to printSetup
  print "--------------------------------------------------------------------------------------"
  print "--------------------------------------------------------------------------------------"
  print "--------------------------------------------------------------------------------------"
  print(word "New simulation started @ "date-and-time)
  print(word "In this simulation the objective value of the proposition is " (precision objectiveChanceP 3) ".")
  print (word "It contains " countPeople " agents, " (precision (array:item groupPercentages 0) 1) "% group A, " (precision (array:item groupPercentages 1) 1) "% group B and " (precision (array:item groupPercentages 2) 1) "% group C.")
  print(word "The average credence in P is " (precision (array:item groupCredences 0) 3) " for group A, " (precision (array:item groupCredences 1) 3) " for group B, and " (precision (array:item groupCredences 2) 3) " for group C.")
  print(word "The mean distance from the truth for the whole population of agents starts at " (precision meanDistance 3) ".")
end

to setupGroup [groupNumber]
  let counter array:item groupCounts groupNumber
  while [counter > 0] [
    set counter counter - 1
  create-people 1[
    ;-----------------------------------------------------------------------------------------------------
    ;basic features
    set xcor random-xcor
    set ycor random-ycor
    set heading random 360
    set shape "person"
    set size 0.3
    ;-----------------------------------------------------------------------------------------------------
    ;group specific features
    ifelse allowOutliers = TRUE [

      set bias (random-float 2.2 * (array:item groupBiases groupNumber) - (0.1 * array:item groupBiases groupNumber)) ;some people are outliers in their group!
    ][
      set bias random-float 2 * (array:item groupBiases groupNumber)
    ]
    set credence objectiveChanceP + bias + (random-float 0.4 - 0.2)
    set quietenTendency random-float 2 * (array:item groupThresholds groupNumber) ; group tendency influences individual threshold
    set groupType groupNumber
    set color array:item groupColors groupNumber
    ;-----------------------------------------------------------------------------------------------------
    ;utility function features
    set utilityQuieting random-float (2 * penaltyPerPerson) ;this is how bad an act of quieting committed by this agent is
    set utilitySmothering random-float (2 * penaltySmothering) ;this is how badly this agent suffers from smothering
    ;-----------------------------------------------------------------------------------------------------
    ;learning function features
    set quietingCount array:from-list (list 0 0 0 0)
    set encounters array:from-list (list 0 0 0)
    set unexpectedQuietings array:from-list (list 0 0 0)
    set unexpectedNonQuietings array:from-list (list 0 0 0)

    if initialValues = "All 0"[ ;natural way to initialize, but will heavily skew the results 'downwards'
      set averageQuietingTendencies array:from-list (list 0 0 0)
      set averageTestimonies array:from-list (list 0 0 0 0)
      set averageQuietingDelta array:from-list (list 0 0 0)
      set averageNonQuietingDelta array:from-list (list 0 0 0)
      set averageQuietingUtility 0
    ]
    if initialValues = "Custom" [ ;these choices are explained in my essay
      let i random-float 1
      set averageQuietingTendencies array:from-list (list  i i i)
      set i 0.5 ;CHANGE THIS TO "set i random-float 1" FOR ALL PURPOSES EXCEPT CHECKING THE PAPER RESULTS @Rolf and Rush
      set averageTestimonies array:from-list (list i i i)
      set averageQuietingDelta array:from-list (list 1 1 1)
      set averageNonQuietingDelta array:from-list (list 0 0 0)
      set averageQuietingUtility 2
    ]
    if initialValues = "All random" [ ;a bit too random
      let i random-float 1
      set averageTestimonies array:from-list (list i i i i)
      set i random-float 1
      set averageQuietingDelta array:from-list (list i i i)
      set i random-float 1
      set averageQuietingTendencies array:from-list (list i i i)
      set i random-float 1
      set averageNonQuietingDelta array:from-list (list i i i)
      set averageQuietingUtility random 10
    ]
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

  let condition true
  if additionalQuietingCondition = TRUE [
    set condition (divergenceVictimConsensus > relevantThreshold)

  ]

  ;conditions for quieting:
  ifelse ([groupType] of aggressor != [groupType] of victim) ;different group identities
  AND divergenceAggressorConsensus < divergenceVictimConsensus ;credence of aggressor is closer to the group consensus than testimony of victim
  AND divergenceVictimAggressor > relevantThreshold ;testimony of victim is far enough from credence of aggressor
  AND condition ;testimony of victim is far enough from patch consensus
  [report true][report false]


end

to-report quieten [aggressor victim input ]


  ask victim[


    if employLearningFunction = TRUE [

      ;updates the average severity of being quietened and counts this instance of quieting
      set averageQuietingUtility (averageQuietingUtility * array:item quietingCount 3 + [utilityQuieting] of aggressor) / (array:item quietingCount 3 + 1)
      array:set quietingCount 3 (array:item quietingCount 3 + 1)
      array:set quietingCount ([groupType] of aggressor) (array:item quietingCount ([groupType] of aggressor) + 1)

      ;updates the aggressors information about the average credences of each group
      ask aggressor [
        if learningCredencesType = "Update only on what one wants to hear"[
          ;update with aggressor credence and count the encounter
          array:set averageTestimonies ([groupType] of victim) ((array:item averageTestimonies [groupType] of victim) * array:item encounters ([groupType] of victim) + credence) / (array:item encounters ([groupType] of victim) + 1)
          array:set encounters ([groupType] of victim) (array:item encounters ([groupType] of victim) + 1)
        ]
        if learningCredencesType = "Update on the actual testimony"[
          ;update with victim testimony
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
        ;if this option is chosen, the count of relevant participants is reduced by one in playGame, round 3
      ]
    ]


    set quietingCounter quietingCounter + 1

    ;adds the quieting to the group type of the victim
    array:set quietingCounterTotals groupType (array:item quietingCounterTotals groupType + 1)


  ]
  report input
end

to updateKnowledgeOfCredences [aggressor victim]
  array:set averageTestimonies ([groupType] of victim) ((array:item averageTestimonies ([groupType] of victim) * array:item encounters ([groupType] of victim) + [testimony] of victim) / (array:item encounters ([groupType] of victim) + 1))
  array:set encounters ([groupType] of victim) (array:item encounters ([groupType] of victim) + 1);the aggressor counts the encounter
end

to smother [agent] ;give tailored testimony/ withold testimony
  ask agent [
    if smotheringType = "Split the difference with expected Patch Consensus"[
      set testimony ((credence + expectedPatchConsensus) / 2)
    ]
    if smotheringType = "Utter expected Patch Consensus" [
      set testimony expectedPatchConsensus
    ]
    if smotheringType = "Withold testimony" [
      set testimony "NA"
    ]
    set smotheringCounter smotheringCounter + 1
    array:set smotheringCounterTotals groupType (array:item smotheringCounterTotals groupType + 1)
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

        set result (credence + (array:item countParticipants 0 - 1) * array:item data 0 + (array:item countParticipants 1) * (array:item data 1) + array:item countParticipants 2 * array:item data 2)

      ][
        ifelse groupType = 1[
            set result (credence + (array:item countParticipants 0) * array:item data 0 + (array:item countParticipants 1 - 1) * (array:item data 1) + (array:item countParticipants 2 * array:item data 2))

        ][
              set result (credence + (array:item countParticipants 0) * array:item data 0 + (array:item countParticipants 1) * (array:item data 1) + (array:item countParticipants 2 - 1) * array:item data 2)

        ]
      ]
      set result result / (array:item countParticipants 3)
    ]

    if patchConsensusType = "Omit own credence" [
      ifelse groupType = 0 [


        set result ((array:item countParticipants 0 - 1) * array:item data 0 + (array:item countParticipants 1) * (array:item data 1) + array:item countParticipants 2 * array:item data 2)
      ][
          ifelse groupType = 1[
           set result ((array:item countParticipants 0 ) * array:item data 0 + (array:item countParticipants 1 - 1) * (array:item data 1) + array:item countParticipants 2 * array:item data 2)
        ][
           set result ((array:item countParticipants 0 ) * array:item data 0 + (array:item countParticipants 1) * (array:item data 1) + ((array:item countParticipants 2 - 1)* array:item data 2))
        ]
      ]
      set result result / ((array:item countParticipants 3) - 1)
    ]
  ]

  report result
end

to-report calculatePatchConsensus [participants countParticipants]
  let result 0
  let relevantParticipantCount array:item countParticipants 3
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
      if expectedDivergence > array:item relevantThresholds 1
      [
        set expectedUtility expectedUtility + (array:item countParticipants 1) * relevantPenalty
      ]
      if expectedDivergence > array:item relevantThresholds 2  [
        set expectedUtility expectedUtility + (array:item countParticipants 2) * relevantPenalty
      ]
    ]

    if groupType = 1[
      if expectedDivergence > array:item relevantThresholds 0 [
        set expectedUtility expectedUtility + (array:item countParticipants 0) * relevantPenalty
      ]
      if expectedDivergence > array:item relevantThresholds 2 [
        set expectedUtility expectedUtility + (array:item countParticipants 2) * relevantPenalty
      ]
    ]

    if groupType = 2[
      if expectedDivergence > array:item relevantThresholds 0 [
        set expectedUtility expectedUtility + (array:item countParticipants 0) * relevantPenalty
      ]
      if expectedDivergence > array:item relevantThresholds 1 [
        set expectedUtility expectedUtility + (array:item countParticipants 1) * relevantPenalty
      ]
    ]
  ]
  report expectedUtility
end

to updateAverageThresholds [quieten? aggressor victim patchConsensus]
  ask victim [
    let delta 0

    if learningTendenciesType = "Difference to credence"[
      set delta abs (testimony - [credence] of aggressor)
    ]

    if learningTendenciesType = "Difference to testimony"[
      set delta abs (testimony - [testimony] of aggressor)
    ]

    let divergenceVictimConsensus abs (testimony - patchConsensus)

    if calculateTendenciesType = "Adjust expectations"[
      let expectedQuieting? ((array:item averageQuietingTendencies ([groupType] of aggressor) < delta) and (array:item averageQuietingTendencies ([groupType] of aggressor) < divergenceVictimConsensus))


      if quieten? = TRUE and expectedQuieting? = FALSE [

        array:set unexpectedQuietings ([groupType] of aggressor) (array:item unexpectedQuietings [groupType] of aggressor + 1 )
      ]

      if quieten? = FALSE and expectedQuieting? = TRUE [


        array:set unexpectedNonQuietings ([groupType] of aggressor) (array:item unexpectedNonQuietings [groupType] of aggressor + 1 )
      ]
    ]

    if calculateTendenciesType = "Know objective values"[
      ;do nothing
    ]

    if calculateTendenciesType = "Split the means" [
      ifelse quieten? = true[
        let violentEncounterCount (array:item quietingCount ([groupType] of aggressor) + 1)
        array:set averageQuietingDelta ([groupType] of aggressor) ((array:item averageQuietingDelta ([groupType] of aggressor) * (violentEncounterCount - 1) + delta) / violentEncounterCount)

      ][
        let nonViolentEncounterCount (array:item encounters ([groupType] of aggressor) + 1 - array:item quietingCount ([groupType] of aggressor))
        if nonViolentEncounterCount > 0 [
          array:set averageNonQuietingDelta ([groupType] of aggressor) (((array:item averageNonQuietingDelta ([groupType] of aggressor)) * (nonViolentEncountercount - 1) + delta) / nonViolentEncounterCount)
        ]
      ]
    ]
  ]
end
@#$#@#$#@
GRAPHICS-WINDOW
5
10
463
469
-1
-1
90.0
1
10
1
1
1
0
1
1
1
-2
2
-2
2
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
100.0
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
50.0
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
0.0
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
792
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
"set groupCredences array:from-list (list 0 0 0)" ""
PENS
"default" 1.0 0 -16777216 true "" "plot objectiveChanceP"
"Group A" 1.0 0 -955883 true "" "plot array:item groupCredences 0"
"Group B" 1.0 0 -13345367 true "" "plot array:item groupCredences 1"
"Group C" 1.0 0 -6459832 true "" "plot array:item groupCredences 2"

SWITCH
1089
519
1326
552
employLearningFunction
employLearningFunction
0
1
-1000

SLIDER
738
510
920
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
742
480
941
500
Epistemic Violence
20
0.0
1

SLIDER
738
546
920
579
penaltySmothering
penaltySmothering
0
10
3.0
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
4.0
2
1
NIL
HORIZONTAL

SLIDER
0
816
195
849
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
816
390
849
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
1125
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
"set groupTestimonies array:from-list (list 0 0 0)" ""
PENS
"default" 1.0 0 -16777216 true "" "plot objectiveChanceP"
"Group A" 1.0 0 -955883 true "" "plot array:item groupTestimonies 0"
"Group B" 1.0 0 -13345367 true "" "plot array:item groupTestimonies 1"
"Group C" 1.0 0 -6459832 true "" "plot array:item groupTestimonies 2"

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
0.0
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
393
816
593
849
quietenThresholdC
quietenThresholdC
0
0.3
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
738
582
983
615
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
605
785
755
845
|\n| Average\n| Values\n|
12
0.0
1

CHOOSER
174
625
374
670
quietingType
quietingType
"Ignore fully" "Slot in own credence" "Split the difference"
0

TEXTBOX
922
511
983
571
|\n| Average\n| Values\n|
12
0.0
1

CHOOSER
1
577
363
622
smotheringType
smotheringType
"Utter expected Patch Consensus" "Withhold testimony" "Split the difference with expected Patch Consensus"
0

SWITCH
735
700
907
733
experiment?
experiment?
0
1
-1000

CHOOSER
1
625
172
670
patchConsensusType
patchConsensusType
"Omit own credence" "Add own credence"
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
1128
56
1499
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
"set quietingCounterTotals array:from-list (list 0 0 0 0)" ""
PENS
"Total" 1.0 0 -16777216 true "" "plot array:item quietingCounterTotals 3"
"Group A" 1.0 0 -955883 true "" "plot array:item quietingCounterTotals 0"
"Group B" 1.0 0 -13345367 true "" "plot array:item quietingCounterTotals 1"
"Group C" 1.0 0 -6459832 true "" "plot array:item quietingCounterTotals 2"

PLOT
1128
264
1499
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
"set smotheringCounterTotals array:from-list (list 0 0 0 0)" ""
PENS
"Total" 1.0 0 -16777216 true "" "plot array:item smotheringCounterTotals 3"
"Group A" 1.0 0 -955883 true "" "plot array:item smotheringCounterTotals 0"
"Group B" 1.0 0 -13345367 true "" "plot array:item smotheringCounterTotals 1"
"Group C" 1.0 0 -6459832 true "" "plot array:item smotheringCounterTotals 2"

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
1089
555
1326
600
learningCredencesType
learningCredencesType
"Update only on what one wants to hear" "Update on the actual testimony"
1

CHOOSER
1089
649
1326
694
learningTendenciesType
learningTendenciesType
"Difference to credence" "Difference to testimony"
1

TEXTBOX
1116
486
1304
513
Learning Function
20
0.0
1

CHOOSER
1089
696
1327
741
initialValues
initialValues
"All 0" "All random" "Custom"
2

SLIDER
364
577
509
610
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
1504
56
2074
379
Credences About the Penalty
NIL
NIL
0.0
1.0
0.0
10.0
true
false
"set credencesAboutPenalty array:from-list (list 0 0 0)" ""
PENS
"default" 1.0 0 -16777216 true "" "plot averagePenalty"
"group A" 1.0 0 -955883 true "" "plot array:item credencesAboutPenalty 0"
"group B" 1.0 0 -13345367 true "" "plot array:item credencesAboutPenalty 1"
"group C" 1.0 0 -6459832 true "" "plot array:item credencesAboutPenalty 2"

TEXTBOX
1671
18
1932
66
Learning Function Plots
20
0.0
1

PLOT
1504
383
1793
533
Credences about Credences of A
NIL
NIL
0.0
10.0
0.0
1.0
true
false
"set credencesAboutA array:from-list (list 0 0 0)" ""
PENS
"Actual Value" 1.0 0 -16777216 true "" "plot array:item groupCredences 0"
"group A" 1.0 0 -955883 true "" "plot array:item credencesAboutA 0"
"group B" 1.0 0 -13345367 true "" "plot array:item credencesAboutA 1"
"group C" 1.0 0 -6459832 true "" "plot array:item credencesAboutA 2"

PLOT
1796
382
2074
532
Credences about Thresholds of A
NIL
NIL
0.0
10.0
0.0
1.0
true
false
"set averageThresholds array:from-list (list 0 0 0 0)\nset credencesAboutThresholdsA array:from-list (list 0 0 0)" ""
PENS
"Actual Value" 1.0 0 -16777216 true "" "plot array:item averageThresholds 0"
"group B" 1.0 0 -13345367 true "" "plot array:item credencesAboutThresholdsA 1"
"group C" 1.0 0 -6459832 true "" "plot array:item credencesAboutThresholdsA 2"

PLOT
1503
539
1794
689
Credences about Credences of B
NIL
NIL
0.0
10.0
0.0
1.0
true
false
"set credencesAboutB array:from-list (list 0 0 0)" ""
PENS
"Actual Value" 1.0 0 -16777216 true "" "plot array:item groupCredences 1"
"group A" 1.0 0 -955883 true "" "plot array:item credencesAboutB 0"
"group B" 1.0 0 -13345367 true "" "plot array:item credencesAboutB 1"
"group C" 1.0 0 -6459832 true "" "plot array:item credencesAboutB 2"

PLOT
1797
539
2075
689
Credences about Thresholds of B
NIL
NIL
0.0
10.0
0.0
1.0
true
false
"set credencesAboutThresholdsB array:from-list (list 0 0 0)" ""
PENS
"actual Value" 1.0 0 -16777216 true "" "plot array:item averageThresholds 1"
"group A" 1.0 0 -955883 true "" "plot array:item credencesAboutThresholdsB 0"
"group C" 1.0 0 -6459832 true "" "plot array:item credencesAboutThresholdsB 2"

PLOT
1503
693
1795
843
Credences about Credences of C
NIL
NIL
0.0
10.0
0.0
1.0
true
false
"set credencesAboutC array:from-list (list 0 0 0)" ""
PENS
"Actual Value" 1.0 0 -16777216 true "" "plot array:item groupCredences 2"
"Group A" 1.0 0 -955883 true "" "plot array:item credencesAboutC 0"
"Group B" 1.0 0 -13345367 true "" "plot array:item credencesAboutC 1"
"Group C" 1.0 0 -6459832 true "" "plot array:item credencesAboutC 2"

PLOT
1798
694
2075
844
Credences about Thresholds of C
NIL
NIL
0.0
10.0
0.0
10.0
true
false
"set credencesAboutThresholdsC array:from-list (list 0 0 0)" ""
PENS
"Actual Value" 1.0 0 -16777216 true "" "plot array:item averageThresholds 2"
"Group A" 1.0 0 -955883 true "" "plot array:item credencesAboutC 0"
"Group B" 1.0 0 -13345367 true "" "plot array:item credencesAboutC 1"

SWITCH
562
477
705
510
PrintUpdates?
PrintUpdates?
1
1
-1000

CHOOSER
1089
602
1326
647
calculateTendenciesType
calculateTendenciesType
"Split the means" "Adjust expectations" "Know objective values"
1

SLIDER
735
736
907
769
w
w
0.01
0.1
0.01
0.01
1
NIL
HORIZONTAL

CHOOSER
735
771
907
816
biasType
biasType
"None" "Perpetual" "Resolving"
1

SWITCH
737
616
982
649
additionalQuietingCondition
additionalQuietingCondition
1
1
-1000

TEXTBOX
757
671
907
695
Experiments
20
0.0
1

SWITCH
211
859
359
892
allowOutliers
allowOutliers
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
