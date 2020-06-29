########################################################################
# Introduce the concept of decision tree model through MLHub
#
# Copyright 2018 Graham.Williams@togaware.com

library(mlhub)

mlcat("Predict Rain Tomorrow",
"The Random Forest model is currently being loaded. This is quite a large
model of 500 decision trees and after loading it into memory it can take
a few seconds to apply the model to a dataset.

Below we show the predictions after applying the pre-built decision tree
model to a random subset of a dataset of previously unseen daily observations.
This provides an insight into the performance of the model.
")

# Load required packages.

suppressMessages(
{
  library(randomForest) # Model: randomForest().
  library(magrittr)     # Data pipelines: %>% %<>% %T>% equals().
  library(dplyr)        # Wrangling: tbl_df(), group_by(), print().
  library(tidyr)
  library(rattle)       # Support: normVarNames(), riskchart(), errorMatrix().
  library(ggplot2)
})

#-----------------------------------------------------------------------
# Load the pre-built model.
#-----------------------------------------------------------------------

load("rain_rf_model.RData")

set.seed(42354)

# Load a sample dataset, predict, and display a sample of predictions.

dsname <- "weatherAUS"
ds     <- get(dsname)

names(ds) %<>% normVarNames()

ds %<>%
  select(-date, -location, -risk_mm) %>%
  drop_na()

names(ds)[which(names(ds) == "rain_tomorrow")] <- "target"

#ds %>% filter(target == "Yes") %>% sample_n(10) -> dsy
#ds %>% filter(target == "No") %>% sample_n(10) -> dsn

#ds <- rbind(dsn, dsy)

ds %>%
  predict(model, newdata=., type="class") %>%
  as.data.frame() %>%
  cbind(Actual=ds$target) %>%
  set_names(c("Predicted", "Actual")) %>%
  select(Actual, Predicted) %>%
  mutate(Error=ifelse(Predicted==Actual, "", "<----")) %T>%
  {sample_n(., 12) %>% print()} ->
ev

# List the importance of the variables.

mlask()

mlcat("Variable Importance",
"One aspect of understanding the data and models that we build is what
variables play the most significant role in predicting the outcome.

We list all the variables available and report their relative importance
in predicting the outcome across several measures.

When you press the Enter key below, a plot of the same data is presented.
A visual presentation can often be more effective.")

rn <- round(randomForest::importance(model), 2)
rn[order(rn[,3], decreasing=TRUE),]
 
fname <- "rain_rf_varimp.pdf"
pdf(file=fname, width=8, height=8)
p <- ggVarImp(model, log=TRUE)
print(p)
invisible(dev.off())
mlpreview(fname)

#-----------------------------------------------------------------------
# Produce confusion matrix using Rattle.
#-----------------------------------------------------------------------

mlask()

mlcat("Confusion Matrix",
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

mlask()

mlcat("Risk Chart",
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

ev$Actual %>%
  as.integer() %>%
  subtract(1) ->
ac

ds %>%
  predict(model, newdata=., type="prob") %>%
  as.data.frame() %>%
  '['(,2) ->
pr

## evaluateRisk(pr, ac) %>%
##   rownames_to_column("Complexity") %>%
##   mutate(Complexity=as.numeric(Complexity)) %>%
##   round(2) %>%
##   print()

# Display the risk chart.

fname <- "rain_rf_riskchart.pdf"
pdf(file=fname, width=5, height=5)
riskchart(pr, ac,
          title="Risk Chart for Decision Tree Model",
          recall.name="Rains Tomorrow",
          show.lift=TRUE,
          show.precision=TRUE,
          legend.horiz=FALSE) %>% print()
invisible(dev.off())

mlpreview(fname)

