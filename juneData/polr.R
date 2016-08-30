require(foreign)
require(ggplot2)
require(MASS)
require(Hmisc)
require(reshape2)

pipData <- read.csv("./PiPResults.csv", stringsAsFactors=FALSE)
persData <- read.csv("./PersResults.csv", stringsAsFactors=FALSE)

GenderEncode <- function(genderLetter) {
	if(genderLetter == 'M') {
		return(1)
	}
	else {
		return(2)
	}
}

StemEncode <- function(stemLetter) {
	if(stemLetter == 'N') {
		return(1)
	}
	else {
		return(2)
	}
}

OLRTest <- function(data, GenderFunc, StemFunc, depColIndex) {
	genderCol <- sapply(data$Sex, GenderFunc)
	stemCol <- sapply(data$STEM, StemFunc)
	mat <- cbind(genderCol, stemCol, as.numeric(data$UseComp), as.numeric(data$HelpAtt))
	depId <- colnames(data)[[depColIndex]]
	colnames(mat) <- c("Sex", "STEM", "UseComp", depId)
	rownames(mat) <- c()
	capture.output(m <- polr(as.factor(data[[depColIndex]]) ~ Sex + STEM + UseComp, data = mat, 
		           Hess=TRUE))
	ci <- confint(m)
	return(ci)
}




cat("=============================================================\n")
cat("Partial ordered logistic regressions to determine if gender, being a STEM major,\n")
cat("and/or comfort with computer (UseComp) had a strong influence on responses.\n")
cat("95% confidence intervals for slope parameters are shown.\n")
cat("Gender coded as 1-male, 2-female\n")
cat("STEM major coded as 1-Non-STEM, 2-STEM\n\n")

cat("Pip Data\n")

for(i in c(22:29, 31,36:42)) {
	olr <- OLRTest(pipData,GenderEncode, StemEncode, i)
	for(j in 1:dim(olr)[1]) {
		if((olr[j,1] > 0 & olr[j,2] > 0) | (olr[j,1] < 0 & olr[j,2] < 0)) {
			cat('\n',colnames(pipData)[[i]],'\n')
			print(olr)
			cat('\n')
			break
		}
	}
}

cat("=============================================================\n")
cat("Pers Data\n")

for(i in c(22:29, 31,36:42)) {
	olr <- OLRTest(persData,GenderEncode, StemEncode, i)
	for(j in 1:dim(olr)[1]) {
		if((olr[j,1] > 0 & olr[j,2] > 0) | (olr[j,1] < 0 & olr[j,2] < 0)) {
			cat('\n',colnames(persData)[[i]],'\n')
			print(olr)
			cat('\n')
			break
		}
	}
}
