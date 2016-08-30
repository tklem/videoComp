ExpertSplitTest <- function(data, colIndex, colSplitList) {
	# Prints the fisher test for significant differences
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

	matList <- c()
	expert <- intersect(which(data[["OnlineEnrolYes"]] %in% 1),
											which(data[["UseComp"]] %in% c(3,4)))
	novice <- setdiff(1:length(data), expert)
	for(colSplit in colSplitList) {
		matList <- c(matList,length(intersect(novice, which(data[[colIndex]] %in% colSplit))))
		matList <- c(matList,length(intersect(expert, which(data[[colIndex]] %in% colSplit))))
	}
	testMatrix <- matrix(matList,nrow=2, ncol=length(colSplitList))
	rownames(testMatrix) <- c("Novice", "Expert")
	colIdList <- c()
	for(colSplit in colSplitList) {
		colIdList <- c(colIdList, paste(colSplit, collapse = ','))
	}
	colnames(testMatrix) <- paste('(',colIdList,')',sep='')
	test <- fisher.test(testMatrix)
	
	if(test$p.value < 0.05) {
		cat("(","Expertise", ',', colnames(pipData)[[colIndex]] , ')\n')
		print(testMatrix)
		cat("P value: ",test$p.value, "\n\n")
	}
}

cat("=======================================================================\n")
cat("Fisher test to determine if expertise is strongly associated with any\n")
cat("response given by the participants in either data set.\n")
cat("Here, expertise is defined by\n")
cat("Expert - 3 or 4 on UseComp AND 1 on OnlineEnrolYes\n")
cat("Novice - else")


ExpertSplitTest(pipData,32,list('I',c('PS','PG')))
ExpertSplitTest(pipData,34,list('I',c('PS','PG')))
ExpertSplitTest(persData,32,list('I',c('PS','PG')))
ExpertSplitTest(persData,34,list('I',c('PS','PG')))

ExpertSplitTest(pipData,32,list('I','PS','PG'))
ExpertSplitTest(pipData,34,list('I','PS','PG'))
ExpertSplitTest(persData,32,list('I','PS','PG'))
ExpertSplitTest(persData,34,list('I','PS','PG'))

for(i in c(22:29, 31,36:42)) {
	ExpertSplitTest(pipData,i,list(c(1,2),c(3,4)))
}

ExpertSplitTest(pipData,44,list(0,1))
ExpertSplitTest(pipData,46,list(0,1))

for(i in c(22:29, 31,36:42)) {
	ExpertSplitTest(persData,i,list(c(1,2),c(3,4)))
}

ExpertSplitTest(persData,44,list(0,1))
ExpertSplitTest(persData,46,list(0,1))