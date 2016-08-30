CrossLeqTwo <- function(rowListIndex, colListIndex, data) {
	leqLeq <- 0
	grLeq <- 0
	leqGr <- 0
	grGr <- 0
	rowResponses <- data[[rowListIndex]]
	colResponses <- data[[colListIndex]]
	for(i in 1:length(rowResponses)) {
		if(rowResponses[[i]] <= 2 && colResponses[[i]] <= 2) {
			leqLeq <- leqLeq + 1
		}
		else if(rowResponses[[i]] > 2 && colResponses[[i]] <= 2) {
			grLeq <- grLeq + 1
		}
		else if(rowResponses[[i]] <= 2 && colResponses[[i]] > 2) {
			leqGr <- leqGr + 1
		}
		else {
			grGr <- grGr + 1
		}
	}
	
	leqTwoMatrix <- matrix(c(leqLeq,grLeq,leqGr,grGr), 
						   nrow=2, 
						   ncol=2)
	colnames(leqTwoMatrix) <- c("<=2", ">2")
	rownames(leqTwoMatrix) <- c("<=2", ">2")
	return(leqTwoMatrix)

}

CrossQuestionTest <- function(data) {
	for(i in c(22:31)) {
		for(j in c(36:42)) {
			crossMat <- CrossLeqTwo(i,j,data)
			test <- fisher.test(crossMat)
			if(test$p.value < 0.05) {
				cat("(",colnames(data)[[i]],",",colnames(data)[[j]],")\n")
				print(crossMat)
				cat("P-value: ",test$p.value, "\n\n")
			}

		}
	}
}

cat("===============================\n")
cat("Fisher's exact test to compare the independence between\n")
cat("a question of presentation quality and a question of the\n")
cat("instructor.\n\n")

cat("PiP Data\n")

CrossQuestionTest(pipData)


cat("\n===============================\n")
cat("Pers Data\n")

CrossQuestionTest(persData)