# A sample model build using the weather dataset to predit rain tomorrow. We
# illustrate the model build and then save the model to file so that
# we can later load the model and use it to score new datasets.

suppressMessages(
{
  library(magrittr)
  library(dplyr)
  library(randomForest)
  library(rattle)
})

set.seed(42)

dsname <- "weatherAUS"
ds     <- get(dsname)

names(ds) %<>% normVarNames()

ds %<>%
  select(-date, -location, -risk_mm)
  

names(ds)[which(names(ds) == "rain_tomorrow")] <- "target"

cat("\n===========================\nBuild a Random Forest Model\n===========================\n\n")

model <- randomForest(target ~ ., data=ds, na.action=na.roughfix)

cat("====================\nModel Saved as RData\n====================\n\n")

save(model, file="rain_rf_model.RData")

# Suggest next step.

cat("\nYou may like to evaluate the model performance by running the demo:\n",
    "\n  $ ml demo rain\n\n")
