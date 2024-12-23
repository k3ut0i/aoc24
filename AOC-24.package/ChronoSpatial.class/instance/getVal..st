accessing
getVal: comboOp
   ^ comboOp < 4
		ifTrue: [  comboOp ]
		ifFalse: [ comboOp = 4
						ifTrue: [ rega ]
						ifFalse: [ comboOp = 5
										ifTrue: [ regb ]
										ifFalse: [ comboOp = 6
														ifTrue: [ regc ]
														ifFalse: [ self error: 'comboOp cannot be 7']]]].