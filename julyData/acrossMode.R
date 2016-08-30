pipData <- read.csv("./comb_Pip.csv")
persData <- read.csv("./comb_Pers.csv")

cat("==================================================================\n")
cat("Two sample t-test to determine if mean number of questions correctly\n")
cat("answered by group of students in one mode differs from other mode.\n\n")

print(t.test(pipData$CorrCt, persData$CorrCt))
print(wilcox.test(pipData$CorrCt, persData$CorrCt))

cat("==================================================================\n")
cat("Two sample t-test to determine if mean number of score of instructor\n")
cat("quality by group of students in one mode differs from other mode.\n\n")
cat("Here, we use the formula established in the previous dataset.\n\n")
# 22 - Gest (+) 
# 23 - Dull (-)
# 24 - Tense (-)
# 25 - Smile (+)
# 26 - EyeCont (+)
# 27 - Variety (+)
# 28 - Relax (+)


pipAgg <- mapply('-',pipData[[22]],pipData[[23]])

pipAgg <- mapply('-',pipAgg,pipData[[24]])
pipAgg <- mapply('+',pipAgg,pipData[[25]])
pipAgg <- mapply('+',pipAgg,pipData[[26]])
pipAgg <- mapply('+',pipAgg,pipData[[27]])
pipAgg <- mapply('+',pipAgg,pipData[[28]])
pipAgg <- pipAgg + 21

persAgg <- mapply('-',persData[[22]],persData[[23]])

persAgg <- mapply('-',persAgg,persData[[24]])
persAgg <- mapply('+',persAgg,persData[[25]])
persAgg <- mapply('+',persAgg,persData[[26]])
persAgg <- mapply('+',persAgg,persData[[27]])
persAgg <- mapply('+',persAgg,persData[[28]])
persAgg <- persAgg + 21

print(t.test(pipAgg,persAgg))
print(wilcox.test(pipAgg,persAgg))

cat("==================================================================\n")
TwoGroupSplitTest <- function(pipCopy, persCopy, colIndex, colSplitList) {
	matList <- vector()
	for(colSplit in colSplitList) {
		matList <- c(matList,length(which(pipCopy[[colIndex]] %in% colSplit)))
		matList <- c(matList,length(which(persCopy[[colIndex]] %in% colSplit)))
	}
	testMatrix <- matrix(matList,nrow=2, ncol=length(colSplitList))
	rownames(testMatrix) <- c("PiP", "Pers")
	colIdList <- vector()
	for(colSplit in colSplitList) {
		colIdList <- c(colIdList, paste(colSplit, collapse = ','))
	}
	colnames(testMatrix) <- paste('(',colIdList,')',sep='')
	test <- fisher.test(testMatrix)
	#print(testMatrix)
	if(test$p.value < 0.05) {
		cat("(",colnames(pipData)[[rowIndex]], ',', colnames(pipData)[[colIndex]] , ')\n')
		print(testMatrix)
		cat("P value: ",test$p.value, "\n\n")
	}
}


TwoCatSplitTest <- function(data, rowIndex, colIndex, rowSplitList, colSplitList) {
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
cat("=====================================================================\n")
cat("Fisher test comparing agree/disagree responses to each individual question\n")
cat("to determine if there is a correlation between video mode and question response.\n\n")
for(i in c(12:19,22:28)) {
	TwoGroupSplitTest(pipData,persData,i,list(c(1,2),c(3,4)))
}
cat("=====================================================================\n")
cat("Fisher test comparing agree/disagree responses between pairs of questions\n")
cat("in each dataset.\n\n")
cat("===PIP DATA===\n\n")
for(i in c(12:19,22:28)) {
	for(j in c(12:19,22:28)) {
		if(isTRUE(all.equal(i,j))) {
			next
		}
		TwoCatSplitTest(pipData,i,j,list(c(1,2),c(3,4)),list(c(1,2),c(3,4)))
	}
}
cat("===PERS DATA===\n\n")
for(i in c(12:19,22:28)) {
	for(j in c(12:19,22:28)) {
		if(isTRUE(all.equal(i,j))) {
			next
		}
		TwoCatSplitTest(persData,i,j,list(c(1,2),c(3,4)),list(c(1,2),c(3,4)))
	}
}
