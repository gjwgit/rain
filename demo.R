########################################################################
# Introduce the concept of decision tree model through MLHub
#
# Copyright 2018 Graham.Williams@togaware.com

cat("=====================
Predict Rain Tomorrow
=====================

Below we show the predictions after applying the pre-built model to a
random subset of a dataset of previously unseen daily observations.
This provides an insight into the performance of the model.

")

# Load required packages.

suppressMessages(
{
  library(rpart)
  library(magrittr)
  library(dplyr)
  library(tidyr)
  library(rattle)
})

#-----------------------------------------------------------------------
# Load the pre-built model.
#-----------------------------------------------------------------------

load("rain_rpart_model.RData")

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
# Produce confusion matrix using Rattle.
#-----------------------------------------------------------------------

cat("\nPress Enter to continue on to the Confusion Matrix: ")
invisible(readChar("stdin", 1))

cat("
================
Confusion Matrix
================

A confusion matrix summarises the performance of the model on this
dataset. The figures here are percentages, aggregating the actual versus
predicted outcomes. The Error column represents the class error.

Notice the model's error rate and note that the model is useful in
giving an indication of the prospect of it raining tomorrow.

")

per <- errorMatrix(ev$Actual, ev$Predicted) %T>% print()

# Calculate the overall error percentage.

cat(sprintf("\nOverall error: %.0f%%\n", 100-sum(diag(per), na.rm=TRUE)))

# Calculate the averaged class error percentage.

cat(sprintf("Average class error: %.0f%%\n", mean(per[,"Error"], na.rm=TRUE)))

# TODO: ROC PLOT 

cat("
Press Enter to finish the demonstration: ")
invisible(readChar("stdin", 1))
