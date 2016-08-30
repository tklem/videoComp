pipData <- read.csv("./PiPResults.csv", stringsAsFactors=FALSE)
persData <- read.csv("./PersResults.csv", stringsAsFactors=FALSE)

for(i in 7:7) {
	matList <- vector()
	
	matList <- c(matList,length(intersect(which(persData[[34]] == 'I'), which(persData[[i]] == 'S'))))
	matList <- c(matList,length(intersect(which(persData[[34]] == 'I'), which(persData[[i]] == 'N'))))
	matList <- c(matList,length(intersect(which(persData[[34]] %in% c('PS', 'PG')), which(persData[[i]] == 'S'))))
	matList <- c(matList,length(intersect(which(persData[[34]] %in% c('PS', 'PG')), which(persData[[i]] == 'N'))))
	testMatrix <- matrix(matList,nrow=2,ncol=2)
	rownames(testMatrix) <- c("0", "1")
	colnames(testMatrix) <- c("I","P")
	test <- fisher.test(testMatrix)
	print(testMatrix)
	if(test$p.value < 0.05) {
		cat("(",colnames(pipData)[[i]], ',', 'Coded positive responses )\n')
		
		print(test)
	}
	
}
