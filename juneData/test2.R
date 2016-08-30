pipData <- read.csv("./PiPResults.csv", stringsAsFactors=FALSE)
persData <- read.csv("./PersResults.csv", stringsAsFactors=FALSE)

GenderFunc <- function(genderLetter) {
	if(genderLetter == 'M') {
		return(1)
	}
	else {
		return(2)
	}
}

StemFunc <- function(stemLetter) {
	if(stemLetter == 'N') {
		return(1)
	}
	else {
		return(2)
	}
}

data = pipData
depColIndex = 30

genderCol <- sapply(data$Sex, GenderFunc)
print(length(genderCol))
stemCol <- sapply(data$STEM, StemFunc)
mat <- cbind(genderCol, stemCol, as.numeric(data$UseComp), as.numeric(data$HelpAtt))
depId <- colnames(data)[[depColIndex]]
colnames(mat) <- c("Sex", "STEM", "UseComp", depId)
rownames(mat) <- c()
print(mat)
m <- polr(as.factor(data[[depColIndex]]) ~ Sex + STEM + UseComp, data = mat, Hess=TRUE)
ci <- confint(m)

