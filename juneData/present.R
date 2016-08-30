cat("===============================\n")
cat("Fisher's exact test to test whether video mode\n")
cat("had an effect on non-combined scoring on each Likert scaled question.\n")

for(i in c(22:31,36:42)) {
	pipTable <- as.data.frame.table(table(pipData[[i]]))
	persTable <- as.data.frame.table(table(persData[[i]]))
	combTable <- merge(pipTable,persTable,by="Var1")
	colnames(combTable) <- c("", "PiP", "Pers")
	test <- fisher.test(as.matrix(combTable[,-1]))
	if(test$p.value < 0.05) {
		cat("\n")

		cat(colnames(pipData)[[i]])
		cat("\n\n")
		print(as.matrix(combTable[,-1]))
		cat("P-value: ")
		cat(test$p.value)
		cat("\n")
	}
}
#print(merge(table(pipData[[22]],table(persData[[22]]))))

#barplot(table(pipData[[22]]), main="PiP Enjoy")
#barplot(table(persData[[22]]), main="Pers Enjoy")
cat("===============================\n")