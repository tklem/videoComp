cat("===============================\n")
cat("Two-sample t-test of mean score\n")
cat("of questions about the instructor\n")
cat("Using the formula:\n")
cat("score = 21+Sum(#1,#3,#4,#6,#7)-Sum(#2,#5)\n")
cat("where #n represents the Likert rating of the\n")
cat("nth question.\n")
#36 - Gest
#37 - Dull (-)
#38 - Tense
#39 - Smiles
#40 - EyeCont (-)
#41 - Variety
#42 - Relax
pipAgg <- mapply('-',pipData[[36]],pipData[[37]])

pipAgg <- mapply('+',pipAgg,pipData[[38]])
pipAgg <- mapply('+',pipAgg,pipData[[39]])
pipAgg <- mapply('-',pipAgg,pipData[[40]])
pipAgg <- mapply('+',pipAgg,pipData[[41]])
pipAgg <- mapply('+',pipAgg,pipData[[42]])
pipAgg <- pipAgg + 21

persAgg <- mapply('-',persData[[36]],persData[[37]])
persAgg <- mapply('+',persAgg,persData[[38]])
persAgg <- mapply('+',persAgg,persData[[39]])
persAgg <- mapply('-',persAgg,persData[[40]])
persAgg <- mapply('+',persAgg,persData[[41]])
persAgg <- mapply('+',persAgg,persData[[42]])
persAgg <- persAgg + 21

print(t.test(pipAgg,persAgg))

cat("===============================\n")