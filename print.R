cat("===================
Variable Importance
===================

One aspect of understanding the data and models that we build is what
variables play the most significant role in predicting the outcome.

We first list the variables that are actually found by the algorithm
to be effective in the model. Then we list all the variables and report
their relative importance in predicting the outcome.

")

# Load model and display some useful informatoin about the model.

suppressMessages(
{
library(rpart)        # Model: decision tree rpart().
})

# Load the pre-built model.

load("rain_rpart_model.RData")

# The following code based on rpart::printcp()
# Copyright (c) Brian Ripley

frame <- model$frame
leaves <- frame$var == "<leaf>"
used <- unique(frame$var[!leaves])

cat("Variables Used:  ", paste(sort(as.character(used)), collapse=", "), ".\n\n", sep="")

# The following code based on rpart:::summary.rpart()
# Copyright (c) Brian Ripley

cat("Relative Importance of Variables:\n\n")

varimp <- model$variable.importance
varimp <- round(100 * varimp/sum(varimp))

print(varimp[varimp>0])

cat("\nPress Enter to continue on to the Decision Tree: ")
invisible(readChar("stdin", 1))

cat("
====================
Actual Decision Tree
====================

The line begining with 'node)' is a legend. Split is a test, n observations,
loss is the error, yval the majority class, and yprob is class probability.

")

print(model)

cat("\nPress Enter to finish the demonstration: ")
invisible(readChar("stdin", 1))
