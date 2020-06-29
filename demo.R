########################################################################
# Introduce the concept of decision tree model through MLHub
#
# Copyright 2018-2020 Graham.Williams@togaware.com

library(glue)        # Format strings: glue().
library(mlhub)

mlcat("Predicting Rain Tomorrow - Decision Tree",
"AI models can be built from historic data and deployed to provide some degree
of accuracy in their prediction. The model here is based on a dataset from a
collection of weather observations at different locations over several years.
How well it performs is dependent on the training data and the locations at
which the model is to be deployed.")

#-----------------------------------------------------------------------
# Setup
#-----------------------------------------------------------------------

# Load required packages.

suppressMessages(
{
  library(rpart)        # Model: decision tree rpart().
  library(randomForest) # Model: randomForest() na.roughfix() for missing data.
  library(magrittr)     # Data pipelines: %>% %<>% %T>% equals().
  library(dplyr)        # Wrangling: tbl_df() group_by() print().
  library(tidyr)
  library(rattle)       # Support: normVarNames() riskchart() errorMatrix().
  library(ggplot2)
  library(tibble)
  library(scales)       # Support: commas(), percent().
})

mlask()

#-----------------------------------------------------------------------
# Prepare the data.
#-----------------------------------------------------------------------

mlcat("Prepare the Data",
"The weatherAUS data comes from the Rattle package (https://rattle.togaware.com).
It covers some 50 weather stations in Australia with over 10 years of daily
observations of some 20 variables. The data is loaded, cleansed and wrangled, and
prepared for modelling, as explained in the OnePageR chapter on data templates:

https://onepager.togaware.com/Chapter_Data_Template.html.

A view of the data is shared below.", end="\n\n")

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

glimpse(ds)

PTR <- 0.7   # Proportion for training
PTU <- 0.15  # Proportion for tuning
PTE <- 0.15  # Proportion for testing

tr     <- sample(nobs, PTR*nobs)
tu     <- nobs %>% seq_len() %>% setdiff(tr) %>% sample(PTU*nobs)
te     <- nobs %>% seq_len() %>% setdiff(tr) %>% setdiff(tu)

target.tr <- ds %>% slice(tr) %>% pull(target)
target.tu <- ds %>% slice(tu) %>% pull(target)
target.te <- ds %>% slice(te) %>% pull(target)

risk.tr   <- ds %>% slice(tr) %>% pull(risk)
risk.tu   <- ds %>% slice(tu) %>% pull(risk)
risk.te   <- ds %>% slice(te) %>% pull(risk)

#-----------------------------------------------------------------------
# Fit the Model
#-----------------------------------------------------------------------

mlask()

mlcat("Fit the Model",
"Given the historic data which records the outcome we wish to predict
({target}) we can
fit a model based on that data so as to predict the outcome for new data.

The model will be built on a random sample of {round(PTR*100)}%
({comma(length(tr))}) of the observations. This is the training dataset.

The model is currently being built...", end="")

mtype <- "rpart"
mdesc <- "decision tree"

ds[tr,vars] %>%
  rpart(form, ., parms=list(prior=c(0.6, 0.4))) ->
model

#-----------------------------------------------------------------------
# Explore the model - Textual Decision Tree
#-----------------------------------------------------------------------

mlask(begin="\n\n")

mlcat("Display the Decision Tree",
"An AI model targets a specific knowledge respresentation langauge.
Here the knowledge is represented as a {mdesc}.
We can gain insight into the model through  a textual representation of the
{mdesc} as below.

The first line in the description
reports the number of observations in the training dataset.
The line begining with 'node)' is a legend. Split is a test condition, n is the
number of observations that have made there way to this node, the loss is the
error in the prediction at this node, the yval the majority class (i.e., the
prediction), and yprob is class probability.", end="\n\n")

print(model)

#-----------------------------------------------------------------------
# Explore the model - Visual Decision Tree
#-----------------------------------------------------------------------

mlask()

mlcat("Visual Decision Tree",
"A visual representation of a model can often be more insightful
than the printed textual representation.  A decision tree
model can readily be visualised as a tree structure as we will see.
The tree is read from top to bottom, traversing the path corresponding
to the answer to the question presented at each node. The leaf node
has the final decision together with the class probabilities.")

mlask()

fname <- "rain_dt_model.pdf"
pdf(fname)
fancyRpartPlot(model, sub="")
invisible(dev.off())

mlpreview(fname, begin="")

#-----------------------------------------------------------------------
# Explore the model - Variable Importance
#-----------------------------------------------------------------------

mlask()

# The following code based on rpart::printcp()
# Copyright (c) Brian Ripley

frame <- model$frame
leaves <- frame$var == "<leaf>"
used <- unique(frame$var[!leaves])
used_vars <- paste(sort(as.character(used)), collapse=", ")

mlcat("Variable Importance",
"One aspect of understanding the data and models that we build is what
variables play the most significant role in predicting the outcome.

The variables that are actually end up in the model are:
{used_vars}.

All variables are considered in the modelling and the relative importance
of each variable in predicting the outcome is determined. Note that in the
table below the actual numbers represent the relative importance
of that variable.", end="\n\n")

# The following code based on rpart:::summary.rpart()
# Copyright (c) Brian Ripley

cat("Relative Importance of Variables:\n\n")

varimp <- model$variable.importance
varimp <- round(100 * varimp/sum(varimp))

print(varimp[varimp>0])

#-----------------------------------------------------------------------
# Explore the model itself - Visual Variable Importance
#-----------------------------------------------------------------------

mlask()

mlcat("Visual Importance",
"Once again a visual presentation of the variable importance can be
more effective in conveying the relative importance.")

mlask()

fname <- "rain_dt_varimp.pdf"
pdf(fname)
print(ggVarImp(model))
invisible(dev.off())

mlpreview(fname, begin="")

#-----------------------------------------------------------------------
# Variable Selection
#-----------------------------------------------------------------------

mlask()

mlcat("Variable Selection",
"When the model was built, the algorithm chooses a variable for each node
of the resulting decision tree. An entropy, information theory or gini
based calculation is used to choose the variable. The variable with
the highest value according to this measure is chosen for the particular
node.

Below we will see the calculations that were made for the root node of the
tree (Node Number 1). A number of variables were considered and the variable
with the top score was chosen for this node. The improve= is the value
of the calculation.")

mlask()

# The following code based on rpart:::summary.rpart()
# Copyright (c) Brian Ripley

node <- 1
digits <- 4
cp <- 0

ff <- model$frame
ylevel <- attr(model, "ylevels")
id <- as.integer(row.names(ff))
parent.id <- ifelse(id == 1L, 1L, id%/%2L)
parent.cp <- ff$complexity[match(parent.id, id)]
rows <- seq_along(id)[parent.cp > cp]
rows <- if (length(rows)) rows[order(id[rows])] else 1L
is.leaf <- ff$var == "<leaf>"
index <- cumsum(c(1L, ff$ncompete + ff$nsurrogate + !is.leaf))
if (!all(is.leaf)) {
  sname <- rownames(model$splits)
  cuts <- character(nrow(model$splits))
  temp <- model$splits[, 2L]
  for (i in seq_along(cuts))
  {
    cuts[i] <- if (temp[i] == -1L) 
                 paste("<", format(signif(model$splits[i, 4L], digits)))
               else if (temp[i] == 1L) 
                 paste("<", format(signif(model$splits[i, 4L], digits)))
               else
                 paste("splits as ",
                       paste(c("L", "-", "R")[model$csplit[model$splits[i, 4L],
                                                       1:temp[i]]],
                             collapse = "", sep = ""),
                       collapse = "")
  }
  if (any(temp < 2L))
    cuts[temp < 2L] <- format(cuts[temp < 2L], justify = "left")
  cuts <- paste0(cuts, ifelse(temp >= 2L, ",",
                              ifelse(temp == 1L,
                                     " to the right,",
                                     " to the left, ")))
}
tmp <- if (is.null(ff$yval2)) ff$yval[rows] else ff$yval2[rows, , drop = FALSE]
tprint <- model$functions$summary(tmp, ff$dev[rows], ff$wt[rows], ylevel, digits)

i <- rows[node]
nn <- ff$n[i]
cat("Node number ", id[i], ": ", nn, " observations", sep = "")
if (ff$complexity[i] < cp || is.leaf[i]) cat("\n") else
  cat(",    complexity param=", format(signif(ff$complexity[i], 
                                              digits)), "\n", sep = "")
cat(tprint[node], "\n")
if (ff$complexity[i] > cp && !is.leaf[i]) {
  sons <- 2L * id[i] + c(0L, 1L)
  sons.n <- ff$n[match(sons, id)]
  cat("  left son=", sons[1L], " (", sons.n[1L], " obs)", 
      " right son=", sons[2L], " (", sons.n[2L], " obs)", 
      sep = "")
  j <- nn - (sons.n[1L] + sons.n[2L])
  if (j > 1L) 
    cat(", ", j, " observations remain\n", sep = "")
  else if (j == 1L) 
    cat(", 1 observation remains\n")
  else cat("\n")
  cat("  Primary splits:\n")
  j <- seq(index[i], length.out = 1L + ff$ncompete[i])
  temp <- if (all(nchar(cuts[j], "w") < 25L)) 
            format(cuts[j], justify = "left")
  else cuts[j]
  cat(paste("      ", format(sname[j], justify = "left"), 
            " ", temp,
            " improve=",
            format(signif(model$splits[j, 3L], digits)),
            ", (", nn - model$splits[j, 1L], 
            " missing)", sep = ""), sep = "\n")
  if (ff$nsurrogate[i] > 0L)
  {
    cat("  Surrogate splits:\n")
    j <- seq(1L + index[i] + ff$ncompete[i],
             length.out = ff$nsurrogate[i])
    agree <- model$splits[j, 3L]
    temp <- if (all(nchar(cuts[j], "w") < 25L)) 
              format(cuts[j], justify = "left")
    else cuts[j]
    adj <- model$splits[j, 5L]
    cat(paste("      ", format(sname[j], justify = "left"), 
              " ", temp, " agree=", format(round(agree, 3L)), 
              ", adj=", format(round(adj, 3L)),
              ", (", model$splits[j, 1L], " split)",
              sep = ""),
        sep = "\n")
  }
}

mlask()

mlcat("Predicting Rain Tomorrow",
"We now use the model to make predictions. The
decision tree model is applied to a previously unseen (by the mode)
random subset of the dataset of daily observations, the tuning dataset.
This dataset contains {comma(length(tu))} observations.

This provides an insight into the performance of the model on new/unseen
data. The performance
here is okay based on this dataset. Note any highlighted errors. No
model is perfect.", end="\n\n")

ds %>%
  predict(model, newdata=., type="class") %>%
  as.data.frame() %>%
  cbind(Actual=ds[[target]]) %>%
  set_names(c("Predicted", "Actual")) %>%
  select(Actual, Predicted) %>%
  mutate(Error=ifelse(Predicted==Actual, "", "<----")) %T>%
  {sample_n(., 12) %>% print()} ->
ev

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
", end="\n\n")

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

## mlask()

## Remove for now until better explained.

## mlcat("Complexity Tuning",
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

mlask()

mlcat("Risk Chart",
"A risk chart presents a cumulative performance view of the model.

The x-axis can be thought of as the days across the dataset, but sorting
(left to right) to days from the highest probability of rain tomorrow on the
left to the lowest probability of rain tomorrow on the right.

The y-axis is then the performance of the model in predicting whether it will
rain tomorrow. It is the percentage of the actual days on which it rains that
are predicted by the model as raining tomorrow. Thus, 100% (at the top) 
covers all days on which it rains. For the top 20% of the days with the
highest probability of rain tomorrow (Caseload = 20%), some 54% of the actual
days for which it rained are predicted by the model.

The more area under the curve the better the model performance. A perfect
model would follow the grey line. The Precision line represents
the lift offered by the model, with the lift values on the right hand axis.")

fname <- "rain_dt_riskchart.pdf"
pdf(file=fname, width=5, height=5)
riskchart(pr, ac,
          title="Risk Chart for Decision Tree Model",
          recall.name="Rains Tomorrow",
          show.lift=TRUE,
          show.precision=TRUE,
          legend.horiz=FALSE) %>% print()
invisible(dev.off())

mlpreview(fname)
cat("\n")

