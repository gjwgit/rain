########################################################################
# Introduce the concept of decision tree model through MLHub
#
# Copyright 2018 Graham.Williams@togaware.com

library(mlhub)

inform_about("Predict Rain Tomorrow",
"Below we show the predictions after applying the pre-built decision tree
model to a random subset of a dataset of previously unseen daily observations.
This provides an insight into the performance of the model.
")

# Load required packages.

suppressMessages(
{
  library(rpart)       # Model: decision tree rpart().
  library(magrittr)    # Data pipelines: %>% %<>% %T>% equals().
  library(dplyr)       # Wrangling: tbl_df(), group_by(), print().
  library(tidyr)
  library(rattle)      # Support: normVarNames(), riskchart(), errorMatrix().
  library(ggplot2)
  library(tibble)
})

#-----------------------------------------------------------------------
# Load the pre-built model.
#-----------------------------------------------------------------------

load("rain_dt_model.RData")

set.seed(42354)

# Load a sample dataset, predict, and display a sample of predictions.

dsname <- "weatherAUS"
ds     <- get(dsname)

names(ds) %<>% normVarNames()

ds %<>%
  select(-date, -location, -risk_mm) %>%
  drop_na()

names(ds)[which(names(ds) == "rain_tomorrow")] <- "target"

ds %>% filter(target == "Yes") %>% sample_n(10) -> dsy
ds %>% filter(target == "No") %>% sample_n(10) -> dsn

ds <- rbind(dsn, dsy)

ds %>%
  predict(model, newdata=., type="class") %>%
  as.data.frame() %>%
  cbind(Actual=ds$target) %>%
  set_names(c("Predicted", "Actual")) %>%
  select(Actual, Predicted) %>%
  mutate(Error=ifelse(Predicted==Actual, "", "<----")) %T>%
  {sample_n(., 12) %>% print()} ->
ev

#-----------------------------------------------------------------------
# Explore the model itself - Variable Importance
#-----------------------------------------------------------------------

ask_continue()

inform_about("Variable Importance",
"One aspect of understanding the data and models that we build is what
variables play the most significant role in predicting the outcome.

We first list the variables that are actually found by the algorithm
to be effective in the model. Then we list all the variables and report
their relative importance in predicting the outcome.
")

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

#-----------------------------------------------------------------------
# Explore the model itself - Visual Variable Importance
#-----------------------------------------------------------------------

ask_continue()

inform_about("Visual Variable Importance",
"An understanding of the relative importance of each of the variables
adds further insight into the data.
")
  
fname <- "rain_dt_varimp.pdf"
pdf(fname)
print(ggVarImp(model))
invisible(dev.off())

preview_file(fname)

#-----------------------------------------------------------------------
# Explore the model itself - Textual Decision Tree
#-----------------------------------------------------------------------

ask_continue()

inform_about("Actual Decision Tree",
"The line begining with 'node)' is a legend. Split is a test, n observations,
loss is the error, yval the majority class, and yprob is class probability.
")

print(model)

#-----------------------------------------------------------------------
# Explore the model itself - Visual Decision Tree
#-----------------------------------------------------------------------

ask_continue()

inform_about("Visual Decision Tree",
"A visual representation of a model can often be more insightful
than the printed textual representation. For a decision tree
model, representing the discovered knowledge as a decision tree, we
read the tree from top to bottom, traversing the path corresponding
to the answer to the question presented at each node. The leaf node
has the final decision together with the class probabilities.
")

fname <- "rain_dt_model.pdf"
pdf(fname)
fancyRpartPlot(model, sub="")
invisible(dev.off())

preview_file(fname)

#-----------------------------------------------------------------------
# Produce confusion matrix using Rattle.
#-----------------------------------------------------------------------

ask_continue()

inform_about("Confusion Matrix",
"A confusion matrix summarises the performance of the model on this evluation
dataset. All figures in the table are percentages and are calculated across
the predicitions made by the model for each observation and compared to the
actual or known values of the target variable. The first column reports the
true negative and false negative rates whilst the second column reports the
false positive and true positive rates.

The Error column calculates the error across each class. We also report the
overall error which is calculated as the number of errors over the number of
observations. The average of the class errors is also reported. 
")

per <- errorMatrix(ev$Actual, ev$Predicted) %T>% print()

# Calculate the overall error percentage.

cat(sprintf("\nOverall error: %.0f%%\n", 100-sum(diag(per), na.rm=TRUE)))

# Calculate the averaged class error percentage.

cat(sprintf("Average class error: %.0f%%\n", mean(per[,"Error"], na.rm=TRUE)))

# Calculate data for the risk chart.

ev$Actual %>%
  as.integer() %>%
  subtract(1) ->
ac

ds %>%
  predict(model, newdata=., type="prob") %>%
  as.data.frame() %>%
  '['(,2) ->
pr

## ask_continue()

## Remove for now until better explained.

## inform_about("Complexity Tuning",
## "A decision tree model has a paramter called the complexity parameter.  It is
## used to control when to stop splitting the tree and so save computational time.
## For example, if a split does not increase the overall R-squared by the
## complexity parameter it is not considered any further, as chances are that the
## tree will be pruned at this point through the use of cross-validation.
## ")

## evaluateRisk(pr, ac) %>%
##   tibble::rownames_to_column("Complexity") %>%
##   mutate(Complexity=as.numeric(Complexity)) %>%
##   '*'(100) %>%
##   round(0) %>%
##   print()

# Display the risk chart.

ask_continue()

inform_about("Risk Chart",
"A risk chart presents a cumulative performance view of the model.

The x-axis is the days sorted (left to right) from the highest probability
of rain tomorrow to the lowest probability of rain tomorrow.

The y-axis is the expected performance of the model in predicting rain. It is
the percentage of the known days on which it rains that are predicted by the
model for the given recall (x-axis).

The more area under the curve the better the model performance. A perfect
model would follow the grey line. The Precision line represents
the lift offered by the model, with the lift values on the right hand axis.
")

fname <- "rain_dt_riskchart.pdf"
pdf(file=fname, width=5, height=5)
riskchart(pr, ac,
          title="Risk Chart for Decision Tree Model",
          recall.name="Rains Tomorrow",
          show.lift=TRUE,
          show.precision=TRUE,
          legend.horiz=FALSE) %>% print()
invisible(dev.off())

preview_file(fname)

