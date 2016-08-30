cat("===============================\n")
cat("Two-proportion z-test for whether\n")
cat("a question was answered correctly\n")
persSuccess <- table(persData$CorrAny)["Y"]
pipSuccess <- table(pipData$CorrAny)["Y"]
persTotal <- length(persData$CorrAny)
pipTotal <- length(pipData$CorrAny)
print(prop.test(c(persSuccess,pipSuccess),c(persTotal,pipTotal)))
cat("===============================\n")
