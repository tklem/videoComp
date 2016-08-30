pipData <- read.csv("comb_Pip.csv")
persData <- read.csv("comb_Pers.csv")

for(i in c(7:21, 24:30)) {
	print(colnames(pipData)[i])
	print(table(pipData[,i]))
}