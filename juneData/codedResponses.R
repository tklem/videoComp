pipData <- read.csv("./PiPResults.csv", stringsAsFactors=FALSE)
persData <- read.csv("./PersResults.csv", stringsAsFactors=FALSE)

FisherSplitTest <- function(data, rowIndex, colIndex, rowSplitList, colSplitList) {
	# Prints the fisher test for a contigency table
	# 
	# Args:
	# data - dataframe containing entries to be split
	# rowIndex - index of row to use for split
	# colIndex - index of col to use for split
	# rowSplitList - list of row identifiers to split entries
	# colSplitList - list of col identifiers to split entries

	# Prints: if fisher test has p-value less than 0.05,
	#         prints contigency table and test results
	# Return: none

	matList <- vector()
	for(colSplit in colSplitList) {
		for(rowSplit in rowSplitList) {
			matList <- c(matList,length(intersect(which(data[[rowIndex]] %in% rowSplit),
													 									which(data[[colIndex]] %in% colSplit))))
		}
	}
	testMatrix <- matrix(matList,nrow=length(rowSplitList), ncol=length(colSplitList))
	rowIdList <- vector()
	for(rowSplit in rowSplitList) {
		rowIdList <- c(rowIdList, paste(rowSplit, collapse = ','))
	}
	rownames(testMatrix) <- paste('(',rowIdList,')',sep='')
	colIdList <- vector()
	for(colSplit in colSplitList) {
		colIdList <- c(colIdList, paste(colSplit, collapse = ','))
	}
	colnames(testMatrix) <- paste('(',colIdList,')',sep='')
	test <- fisher.test(testMatrix)
	
	if(test$p.value < 0.05) {
		cat("(",colnames(pipData)[[rowIndex]], ',', colnames(pipData)[[colIndex]] , ')\n')
		print(testMatrix)
		cat("P value: ",test$p.value, "\n\n")
	}
}

CodedFisherBundle <- function(data, positive, combined) {
	# Runs series of Fisher tests
	#
	# Args: 
	# data - data frame to execute test suite on
	# positive - boolean to indicate whether to execute tests on positive or negative responses
	# combined - boolean to indicate whether to combine 'PS' and 'PG' categories
	# Return: none
	if(positive) {
		responseIndex = 32
	}
	else {
		responseIndex = 34
	}

	if(combined) {
		for(i in c(22:31,36:42)) {
			FisherSplitTest(data,i,responseIndex,list(c(1,2),c(3,4)),list('I',c('PS','PG')))
		}
		FisherSplitTest(data,7,responseIndex,list('S','N'),list('I',c('PS','PG')))
		FisherSplitTest(data,9,responseIndex,list('M','F'),list('I',c('PS','PG')))
		FisherSplitTest(data,14,responseIndex,list(0,1),list('I',c('PS','PG')))
		FisherSplitTest(data,18,responseIndex,list(0,1),list('I',c('PS','PG')))
		FisherSplitTest(data,20,responseIndex,list(0,1),list('I',c('PS','PG')))
		FisherSplitTest(data,30,responseIndex,list(1,2,3,4),list('I',c('PS','PG')))
		FisherSplitTest(data,30,responseIndex,list(c(1,2),c(3,4)),list('I',c('PS','PG')))
	}

	else {
		for(i in c(22:31,36:42)) {
			FisherSplitTest(data,i,responseIndex,list(c(1,2),c(3,4)),list('I','PS','PG'))
		}
		FisherSplitTest(data,7,responseIndex,list('S','N'),list('I','PS','PG'))
		FisherSplitTest(data,9,responseIndex,list('M','F'),list('I','PS','PG'))
		FisherSplitTest(data,14,responseIndex,list(0,1),list('I','PS','PG'))
		FisherSplitTest(data,18,responseIndex,list(0,1),list('I','PS','PG'))
		FisherSplitTest(data,20,responseIndex,list(0,1),list('I','PS','PG'))
		FisherSplitTest(data,30,responseIndex,list(1,2,3,4),list('I','PS','PG'))
		FisherSplitTest(data,30,responseIndex,list(c(1,2),c(3,4)),list('I','PS','PG'))
	}
}

cat("========================================================================\n\n")
cat("Fisher's exact test to determine if the coded response of positive/negative\n")
cat("freeform questions is independent of responses to prior questions\n\n")

cat("\nPIP DATA NON-COMBINED POSITIVE\n\n====================================\n\n")
CodedFisherBundle(pipData, positive=TRUE, combined=FALSE)

cat("\nPIP DATA NON-COMBINED NEGATIVE\n\n====================================\n\n")
CodedFisherBundle(pipData, positive=FALSE, combined=FALSE)

cat("\nPERS DATA NON-COMBINED POSITIVE\n\n====================================\n\n")
CodedFisherBundle(persData, positive=TRUE, combined=FALSE)

cat("\nPERS DATA NON-COMBINED NEGATIVE\n\n====================================\n\n")
CodedFisherBundle(persData, positive=FALSE, combined=FALSE)

cat("\nPIP DATA COMBINED POSITIVE\n\n====================================\n\n")
CodedFisherBundle(pipData, positive=TRUE, combined=TRUE)

cat("\nPIP DATA COMBINED NEGATIVE\n\n====================================\n\n")
CodedFisherBundle(pipData, positive=FALSE, combined=TRUE)

cat("\nPERS DATA COMBINED POSITIVE\n\n====================================\n\n")
CodedFisherBundle(persData, positive=TRUE, combined=TRUE)

cat("\nPERS DATA COMBINED NEGATIVE\n\n====================================\n\n")
CodedFisherBundle(persData, positive=FALSE, combined=TRUE)