questionNames <- c(
	'Year',
	'MajDepProg',
	'Sex',
	'Country',
	'NativeLang',
	'Olcourses',
	'Olgrades',
	'PrefOnl',
	'CorrCt',
	'CorrAny',
	'Distract',
	'Enjoy',
	'SimExpClass',
	'BetExpClass',
	'WorseExpClass',
	'EnrolVid',
	'UseComp',
	'HelpAtt',
	'PosAspect',
	'NegAspect',
	'Gest',
	'Dull',
	'Tense',
	'Smile',
	'EyeCont',
	'Variety',
	'Relax'
)


KeyResponses <- function(answerMat, key) {
	# Converts matrix of answers to technical questions into response matrix
	# 
	# Args:
	# answerMat: matrix of responses to tech questions
	#            rows represent one student's responses
	#            cols represent a question that students answered
	# key: list of correct answers, organized in column order corresponding to questions
	#
	# Return: two-column matrix of responses
	#         first column: 0 for a student answering no question correctly, 1 else
	#         second col: number of questions answered correctly

	responses <- matrix(,nrow=nrow(answerMat),ncol=2)
	for(i in 1:nrow(answerMat)) {
		matches <- c()
		for(j in 1:length(key)) {
			matches <- c(matches, isTRUE(all.equal(answerMat[[i,j]],key[j])))
		}
		responses[i,1] <- length(which(matches == TRUE))
		if(responses[i,1] > 0) {
			responses[i,2] = 1
		}
		else {
			responses[i,2] = 0
		}
	}

	return(responses)
}




ReformatData <- function(inputFile, outputFile) {
	# Converts second-semester data contained in csv to format resembling data
	# collected from first semester
	#	Primarily collapses one-hot encoded columns for a question to a single column with data entries
	# {1,2,3,...,n} where there are n choices for a question
	#
	# Args:
	# inputFile - filepath containing second semester data to be converted
	# outputFile - filepath where converted data will be stored
	#
	# Return: none
	# Display: none

	require(MASS)

	dataset <- read.csv(inputFile, stringsAsFactors=FALSE)
	exceptionsList <- c(2,4,5,21,22)

	results <- matrix(dataset[[1]],nrow=nrow(dataset))

	for(i in 1:29) {
		searchExp <- paste('Q',i,'\\.',sep="")
		if(i %in% exceptionsList) {
			freeIndex <- grep(searchExp,colnames(dataset))
			results <- cbind(results,dataset[,freeIndex])
		}
		else {
			choiceIndices <- grep(searchExp, colnames(dataset))
			respVector <- c()
			for(j in 1:nrow(dataset)) {
				for(k in 1:length(choiceIndices)) {
					if(isTRUE(all.equal(dataset[j,choiceIndices[k]],1))) {
						respVector <- c(respVector, k)
						break
					}
				}
			}
			results <- cbind(results,respVector)
		}
	}

	results <- results[,-1]
	keyedCols <- KeyResponses(results[,9:12],c('3','1','4','1'))
	results <- cbind(results[,1:8],keyedCols,results[,13:29])
	rownames(results) <- c()
	colnames(results) <- questionNames
	write.csv(results,file=outputFile)
}

ReformatData("./Pers_data.csv","./comb_Pers.csv")
ReformatData("./Pip_data.csv","./comb_Pip.csv")