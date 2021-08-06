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
  objectiveChance
  countPeople
  credenceTypeA
  credenceTypeB
  testimonyTypeA
  testimonyTypeB
  meanDistance
  utilityMatrix
]

to setup
  clear-all
  reset-ticks
  setupWorld
  setupAgents
end

to go
  ;if learning function is toggled on, people will individually try and assess the average group opinions
  prepareGame
  playGame
  tick
end

to playGame
  ask patches [
    let participants turtles-here
    let countParticipants count participants
    let countParticipantsA count turtles-here with [groupType = "A"]
    let countParticipantsB count turtles-here with [groupType = "B"]

    ask participants [
      ;-----------------------------------------------------------------------------------------------------------------------------------------------------
      ;playRound1: Calculate expected patch consensus and expected divergence

      ifelse groupType = "A"[ ;possibly ommit own credence from this calculation?
        set expectedPatchConsensus (credence + (countParticipantsA - 1) * credenceTypeA + countParticipantsB * credenceTypeB) / countParticipants
      ][
        set expectedPatchConsensus (credence + countParticipantsA * credenceTypeA + (countParticipantsB - 1) * credenceTypeB) / countParticipants
      ]
      set expectedDivergence abs (expectedPatchConsensus - credence)

      ;-----------------------------------------------------------------------------------------------------------------------------------------------------
      ;playRound2: calculate expected utility and give testimony
      ifelse groupType = "A"[
        ifelse expectedDivergence > quietenThresholdB [
          set expectedUtility countParticipantsB * penaltyPerPerson
        ][
          set expectedUtility 0

        ]
      ][
        ifelse expectedDivergence > quietenThresholdA [
          set expectedUtility countParticipantsA * penaltyPerPerson
        ][
          set expectedUtility 0
        ]
      ]

      ifelse expectedUtility < penaltySmothering [;negative values are being compared here
        set testimony (credence + expectedPatchConsensus) / 2 ;split the difference with majority consensus
      ][
        set testimony credence
      ]
    ]
    
      ;-----------------------------------------------------------------------------------------------------------------------------------------------------
      ;playRound3: quieten, (receive your utility if the learning function is turned on), update your credences
      
     ask participants [
      set input 0
      if countParticipants > 1 [
      ask other participants [
        let divergence abs (credence - [credence] of myself)
        let groupTypeMyself [groupType] of myself
        let relevantThreshold 0
        ifelse groupTypeMyself = "A" [ 
          set relevantThreshold quietenThresholdA
        ][
          set relevantThreshold quietenthresholdB
        ]
        
        ifelse (groupType != groupTypeMyself) AND (divergence > relevantThreshold) [
          ask myself [set input input + (([credence] of myself + credence) / 2)] ;ass additional quieting effects here
          
          
        ][
          ask myself [set input input + [credence] of myself]
        ]
        
      ]
      
        set input input / (countParticipants - 1)
     
      set credence (input + credence) / 2
      ]
    ]

  ]


end



to setupAgents
  set countPeople countTypeA + countTypeB
  let percentageA (countTypeA / countPeople) * 100
  print (word "This simulation contains " countPeople " agents, " percentageA "% of which are of type A")

  create-people countTypeA[
    set xcor random-xcor
    set ycor random-ycor
    set heading random 360
    set credence objectiveChance + random-float 1 * biasTypeA ;later this bias might be featured in experiments
    set quietenTendency 0 ;add type specific slider with reasonable values later
    set groupType "A"
    set color orange
    set shape "person"
    set size 0.5
  ]

  create-people countTypeB[
    set xcor random-xcor
    set ycor random-ycor
    set heading random 360
    set credence objectiveChance + random-float 1 * biasTypeB ;later this bias might be featured in experiments
    set quietenTendency 0 ;add type specific slider with reasonable values later
    set groupType "B"
    set color blue
    set shape "person"
    set size 0.5
  ]

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
  
  print "--------------------------------------------------------------------------------------"
  print "--------------------------------------------------------------------------------------"
  print "--------------------------------------------------------------------------------------"
  print(word "New simulation started @ "date-and-time)
  print(word "In this simulation the objective value of the proposition is " objectiveChance ".")
end

to prepareGame ;agents move, group credences, testimony and distance from the truth are updated and printed
  set meanDistance 0
  set credenceTypeA 0
  set credenceTypeB 0
  set testimonyTypeA 0
  set testimonyTypeB 0
  ask people[
    if credence > 1 [set credence 1] ;just in case
    if credence < 0 [set credence 0]
    forward random 10
    left (random 10) - 5
    set meanDistance meanDistance + abs (objectiveChance - credence)
    ifelse groupType = "A" [
      set credenceTypeA credenceTypeA + credence
      set testimonyTypeA testimonyTypeA + testimony
    ][
      set credenceTypeB credenceTypeB + credence
      set testimonyTypeB testimonyTypeB + testimony
    ]
  ]
  set meanDistance meanDistance / countPeople
  set credenceTypeA credenceTypeA / countTypeA
  set credenceTypeB credenceTypeB / countTypeB
  set testimonyTypeA testimonyTypeA  / countTypeA
  set testimonyTypeB testimonyTypeB  / countTypeB
  print "--------------------------------------------------------------------------------------"
  print(word "At the start of round " ticks " the average credence in P is " credenceTypeA " for group A, and " credenceTypeB " for group B.")
  print(word "The average testimony given by members of group A is " testimonyTypeA ", while for members of group B it is " testimonyTypeB ".")
  print(word "The mean distance from the truth for the whole population of agents is currently " meanDistance ".")
end
