# The score command in general will allow the user to score their own
# data. For some models, this might mean they can apply a colourising
# model to their own black and white photos, for example.
#
# For this mode we will interactively request some data and report the
# prediction of rain tomorrow.
#
# TODO An extension to this command will allow a CSV file containing
# observations to be scored, returning the prediciton of rain for each
# observation.

# Overall: Load model, interactively request data, predict.

suppressMessages(
{
  library(mlhub)
  library(rpart)
  library(randomForest) # Model: randomForest() na.roughfix() for missing data.
  library(magrittr)
  library(dplyr)
  library(tidyr)
  library(rattle)
})

dsname <- "weatherAUS"
ds     <- get(dsname)
nobs   <- nrow(ds)

vnames        <- names(ds)
names(ds)    %<>% normVarNames()
names(vnames) <- names(ds)

vars   <- names(ds)
target <- "rain_tomorrow"
vars   <- c(target, vars) %>% unique() %>% rev()

for (v in which(sapply(ds, is.factor))) levels(ds[[v]]) %<>% normVarNames()

risk   <- "risk_mm"
id     <- c("date", "location")
ignore <- c(risk, id)
vars   <- setdiff(vars, ignore)

inputs <- setdiff(vars, target)

form   <- formula(ds[rev(vars)])

ds[vars] <- na.roughfix(ds[vars])

load("rain_dt_model.RData")

mlcat("Provide values for the following variables", end="")

# The following code based on rpart::printcp()
# Copyright (c) Brian Ripley, GPLv2 License.

frame <- model$frame
leaves <- frame$var == "<leaf>"
used <- unique(frame$var[!leaves])
unused <- setdiff(names(ds), used)

val <- vector()
for (i in seq_len(length(used)))
{
  v  <- used[i]
  cl <- class(ds[[v]])
  if (cl == "numeric")
  {
    cl <- sprintf("numeric %4.1f - %4.1f", min(ds[[v]]), max(ds[[v]]))
    asis <- "as.numeric"
  }
  cat(sprintf("%-15s [%s]: ", v, cl))
  entry <- scan("stdin", 0,  n=1, quiet=TRUE)
  val <- c(val, eval(parse(text=sprintf("%s(%s)", asis, entry))))
}

newdata <- ds[1,]
usedi <- sapply(used, function(x) which(x == names(ds)))
newdata[1,usedi] <- as.list(val)
unusedi <- sapply(unused, function(x) which(x == names(ds)))
newdata[1,unusedi] <- NA

pr <- predict(model, newdata=newdata)[,"yes"]
cat(sprintf("\nI predict the chance of rain tomorrow to be %2.0f%%.\n\n", 100*pr))
