LeqTwoComp <- function(entry) { return(entry <= 2) }
BinaryComp <- function(entry) { return(entry == 1) }
StemComp <- function(entry) { return(entry == 'S') }
MaleComp <- function(entry) { return(entry == 'M') }

CrossTwo <- function(data, rowListIndex, colListIndex, RComp, CComp) {
	if(missing(RComp)) {
		RComp = LeqTwoComp
	}
	if(missing(CComp)) {
		CComp = LeqTwoComp
	}
	trTr <- 0
	falTr <- 0
	trFal <- 0
	falFal <- 0
	rowResponses <- data[[rowListIndex]]
	colResponses <- data[[colListIndex]]
	for(i in 1:length(rowResponses)) {
		if(RComp(rowResponses[[i]]) && CComp(colResponses[[i]])) {
			trTr <- trTr + 1
		}
		else if(!RComp(rowResponses[[i]]) && CComp(colResponses[[i]])) {
			falTr <- falTr + 1
		}
		else if(RComp(rowResponses[[i]]) && !CComp(colResponses[[i]])) {
			trFal <- trFal + 1
		}
		else {
			falFal <- falFal + 1
		}
	}
	
	crossMatrix <- matrix(c(trTr,falTr,trFal,falFal), 
						   nrow=2, 
						   ncol=2)
	return(crossMatrix)

}

CrossQuestionTest <- function(data) {

	for(i in c(22:31,36:42)) {
		crossMat <- CrossTwo(data,7,i,StemComp)
		rownames(crossMat) <- c("STEM", "Non-STEM")
		colnames(crossMat) <- c("<=2", ">2")
		test <- fisher.test(crossMat)
		if(test$p.value < 0.05) {
			cat("(",'STEM',",",colnames(data)[[i]],")\n")
			print(crossMat)
			cat("P-value: ", test$p.value, "\n\n")
		}		
	}
	for(i in c(22:31,36:42)) {
		crossMat <- CrossTwo(data,9,i,MaleComp)
		rownames(crossMat) <- c("Male", "Female")
		colnames(crossMat) <- c("<=2", ">2")
		test <- fisher.test(crossMat)
		if(test$p.value < 0.05) {
			cat("(",'Gender',",",colnames(data)[[i]],")\n")
			print(crossMat)
			cat("P-value: ", test$p.value, "\n\n")
		}		
	}
	for(i in c(22:31,36:42)) {
		crossMat <- CrossTwo(data,14,i,BinaryComp)
		rownames(crossMat) <- c("Online", "In-person")
		colnames(crossMat) <- c("<=2", ">2")
		test <- fisher.test(crossMat)
		if(test$p.value < 0.05) {
			cat("(",'Online Class Preferences',",",colnames(data)[[i]],")\n")
			print(crossMat)
			cat("P-value: ", test$p.value, "\n\n")
		}		
	}
}
cat("==========================================================\n")
cat("Fisher's exact test to compare the independence between\n")
cat("student's demographics and his/her responses.\n")
cat("instructor.\n\n")

cat("PiP Data\n")

CrossQuestionTest(pipData)


cat("==========================================================\n")
cat("Pers Data\n")

CrossQuestionTest(persData)