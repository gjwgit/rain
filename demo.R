########################################################################
# Introduce the concept of decision tree model through MLHub
#
# Copyright 2018-2020 Graham.Williams@togaware.com

library(mlhub)

mlcat("Model to Predict Rain Tomorrow - Decision Tree",
"This prebuilt decision tree based model illustrates the simplicity with which
AI models can be built from historic data and deployed to provide some degree
of accuracy in their prediction. The model here is based on a dataset from a
collection of weather observations at different locations over several years.
How well it performs is dependent on the training data and the locations at
which the model is to be deployed.

The purpose here is to illustrate the process of reviewing the performance
of the pre-built model. You might find it useful to review MLHub's rainrf
model too, which provides a pre-built random forest which is considerably
more accurate than this model.")

mlask()

mlcat("Predict Rain Tomorrow",
"Below we show the predictions, that are being computed right now. The
pre-built decision tree model is being applied to a random subset of a
dataset of previously unseen daily observations.

This provides an insight into the performance of the model. The performance
here is just okay based on this datasate. Note the highlighted errors. No
model is perfect.
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

load("rain_model.RData")

set.seed(42354)

# Load a sample dataset, predict, and display a sample of predictions.

dsname <- "weatherAUS"
ds     <- get(dsname)

names(ds) %<>% normVarNames()

ds %<>%
  select(-date, -location, -risk_mm) %>%
  drop_na()

names(ds)[which(names(ds) == "rain_tomorrow")] <- "target"

#ds %>% filter(target == "Yes") %>% sample_n(100) -> dsy
#ds %>% filter(target == "No") %>% sample_n(10000) -> dsn

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

#-----------------------------------------------------------------------
# Explore the model itself - Textual Decision Tree
#-----------------------------------------------------------------------

mlask()

mlcat("Actual Decision Tree",
"We often want to gain insight into the models that the artificial intelligence
builds. Below is a text representation of the decision tree model that the
decision tree algorithm has built based on the training data provided to the
algorithm.

The first line reports the number of observations in the training dataset.
The line begining with 'node)' is a legend. Split is a test condition, n is the
number of observations that have made there way to this node, the loss is the
error in the prediction at this node, the yval the majority class (i.e., the
prediction), and yprob is class probability.
")

print(model)

#-----------------------------------------------------------------------
# Explore the model itself - Visual Decision Tree
#-----------------------------------------------------------------------

mlask()

mlcat("Visual Decision Tree",
"A visual representation of a model can often be more insightful
than the printed textual representation. For a decision tree
model, representing the discovered knowledge as a decision tree, we
read the tree from top to bottom, traversing the path corresponding
to the answer to the question presented at each node. The leaf node
has the final decision together with the class probabilities.")

mlask()

fname <- "rain_dt_model.pdf"
pdf(fname)
fancyRpartPlot(model, sub="")
invisible(dev.off())

mlpreview(fname, begin="")

#-----------------------------------------------------------------------
# Explore the model itself - Variable Importance
#-----------------------------------------------------------------------

mlask()

mlcat("Variable Importance",
"One aspect of understanding the data and models that we build is what
variables play the most significant role in predicting the outcome.

We first list the variables that are actually found by the algorithm
to be effective in the model. Then we list all the variables and report
their relative importance in predicting the outcome.

When you press the Enter key below, a plot of the same data is presented.
A visual presentation can often be more effective.
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
based calculation is performed to choose the variable. The variable with
the highest value according to this measure is chosen for the particular
node.

Below we can see the calculations that were made for the root node of the
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

