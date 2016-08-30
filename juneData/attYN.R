cat("===============================\n")
cat("Two-proportion z-test for whether\n")
cat("a question was attempted\n")
persAtt <- table(persData$AttYN)["Y"]
pipAtt <- table(pipData$AttYN)["Y"]
persTotal <- length(persData$AttYN)
pipTotal <- length(pipData$AttYN)
print(prop.test(c(persAtt,pipAtt),c(persTotal,pipTotal)))
cat("===============================\n")
