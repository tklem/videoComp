cat("===============================\n")
cat("Fisher's exact test to test whether video mode\n")
cat("had an effect on combined scoring on questions about the quality\n")
cat("of the presentation.")

CountLeqTwo <- function(pipList1, persList2) {
	leqTwo1 <- 0
	grTwo1 <- 0
	leqTwo2 <- 0
	grTwo2 <- 0
	for(response in pipList1) {
		if(response <= 2) {
			leqTwo1 <- leqTwo1+1
		} else {
			grTwo1 <- grTwo1+1
		}
	}
	for(response in persList2) {
		if(response <= 2) {
			leqTwo2 <- leqTwo2+1
		} else {
			grTwo2 <- grTwo2+1
		}
	}
	leqTwoMatrix <- matrix(c(leqTwo1, grTwo1, leqTwo2, grTwo2), 
						   nrow=2, 
						   ncol=2)
	colnames(leqTwoMatrix) <- c("Pip", "Pers")
	rownames(leqTwoMatrix) <- c("<=2", ">2")
	return(leqTwoMatrix)

}


for(i in 22:31) {
	
	combMatrix <- CountLeqTwo(pipData[[i]],persData[[i]])
	cat("\n")
	test <- fisher.test(combMatrix)
	if(test$p.value < 0.05) {
		cat(colnames(pipData)[[i]])
		cat("\n\n")
		print(combMatrix)
		cat("P-value: ")
		cat(test$p.value)
		cat("\n")
	}
	
}
#print(merge(table(pipData[[22]],table(persData[[22]]))))

#barplot(table(pipData[[22]]), main="PiP Enjoy")
#barplot(table(persData[[22]]), main="Pers Enjoy")
cat("===============================\n")