# Predicting Rain

This [MLHub](https://mlhub.ai) package uses the weatherAUS dataset
from R's [Rattle](https://rattle.togaware.com) package to train a
predictive model for the probability of it raining tomorrow based on
today's weather observations. The training dataset consists of daily
weather observations from weather stations across Australia capturing
the amount of sunshine, the humidity, the amount of rain today,
etc. This simplest of approaches uses the decision tree induction
algorithm to build a model that is represented as a decision
tree. Other (often more accurate but more complex) models include the
random forest which builds a forest (that is, a collection) of
decision trees and produces an ensemble model.  Ensembles have been
shown over many years to produce more accurate models (see, for
example, the original work on [multiple inductive
learning](http://togaware.com/papers/milai87.pdf)).

These models come from the Essentials of Data Science by Graham
Williams <https://bit.ly/essentials_data_science>. Further support
material is also available from <https://essentials.togaware.com/>.

The rain source code is available from
<https://github.com/gjwgit/rain>.

## Quick Start

## Usage

- To install mlhub (Ubuntu):

		$ pip3 install mlhub
		$ ml configure

- To install, configure, and run the demo:

		$ ml install   rain
		$ ml configure rain
		$ ml readme    rain
		$ ml commands  rain
		$ ml demo      rain
		
- Command line tools:

		$ ml predict rain

## Command Line Tools

### *predict*

The *predict* command can be run after the model has been built as
part of the *demo* command.

```console
$ ml predict rain
==========================================
Provide values for the following variables
==========================================

humidity_3pm    [numeric  0.0 - 100.0]: 100
rainfall        [numeric  0.0 - 474.0]: 0
wind_gust_speed [numeric  2.0 - 135.0]: 35

I predict the chance of rain tomorrow to be 72%.
```

## Demonstration

```console
========================================
Predicting Rain Tomorrow - Decision Tree
========================================

AI models can be built from historic data and deployed to provide some
degree of accuracy in their prediction. The model here is based on a
dataset from a collection of weather observations at different
locations over several years. How well it performs is dependent on the
training data and the locations at which the model is to be deployed.

Press Enter to continue: 

================
Prepare the Data
================

The weatherAUS data comes from the Rattle package
(https://rattle.togaware.com). It covers some 50 weather stations in
Australia with over 10 years of daily observations of some 20
variables. The data is loaded, cleansed and wrangled, and prepared for
modelling, as explained in the OnePageR chapter on data templates:

https://onepager.togaware.com/Chapter_Data_Template.html.

A view of the data is shared below.

Rows: 176,747
Columns: 24
$ date            <date> 2008-12-01, 2008-12-02, 2008-12-03, 2008-12-04, 2008…
$ location        <chr> "Albury", "Albury", "Albury", "Albury", "Albury", "Al…
$ min_temp        <dbl> 13.4, 7.4, 12.9, 9.2, 17.5, 14.6, 14.3, 7.7, 9.7, 13.…
$ max_temp        <dbl> 22.9, 25.1, 25.7, 28.0, 32.3, 29.7, 25.0, 26.7, 31.9,…
$ rainfall        <dbl> 0.6, 0.0, 0.0, 0.0, 1.0, 0.2, 0.0, 0.0, 0.0, 1.4, 0.0…
$ evaporation     <dbl> 4.8, 4.8, 4.8, 4.8, 4.8, 4.8, 4.8, 4.8, 4.8, 4.8, 4.8…
$ sunshine        <dbl> 8.5, 8.5, 8.5, 8.5, 8.5, 8.5, 8.5, 8.5, 8.5, 8.5, 8.5…
$ wind_gust_dir   <ord> w, wnw, wsw, ne, w, wnw, w, w, nnw, w, n, nne, w, sw,…
$ wind_gust_speed <dbl> 44, 44, 46, 24, 41, 56, 50, 35, 80, 28, 30, 31, 61, 4…
$ wind_dir_9am    <ord> w, nnw, w, se, ene, w, sw, sse, se, s, sse, ne, nnw, …
$ wind_dir_3pm    <ord> wnw, wsw, wsw, e, nw, w, w, w, nw, sse, ese, ene, nnw…
$ wind_speed_9am  <dbl> 20, 4, 19, 11, 7, 19, 20, 6, 7, 15, 17, 15, 28, 24, 4…
$ wind_speed_3pm  <dbl> 24, 22, 26, 9, 20, 24, 24, 17, 28, 11, 6, 13, 28, 20,…
$ humidity_9am    <dbl> 71, 44, 38, 45, 82, 55, 49, 48, 42, 58, 48, 89, 76, 6…
$ humidity_3pm    <dbl> 22, 25, 30, 16, 33, 23, 19, 19, 9, 27, 22, 91, 93, 43…
$ pressure_9am    <dbl> 1007.7, 1010.6, 1007.6, 1017.6, 1010.8, 1009.2, 1009.…
$ pressure_3pm    <dbl> 1007.1, 1007.8, 1008.7, 1012.8, 1006.0, 1005.4, 1008.…
$ cloud_9am       <dbl> 8, 5, 5, 5, 7, 5, 1, 5, 5, 5, 5, 8, 8, 5, 5, 0, 8, 8,…
$ cloud_3pm       <dbl> 5, 5, 2, 5, 8, 5, 5, 5, 5, 5, 5, 8, 8, 7, 5, 5, 1, 1,…
$ temp_9am        <dbl> 16.9, 17.2, 21.0, 18.1, 17.8, 20.6, 18.1, 16.3, 18.3,…
$ temp_3pm        <dbl> 21.8, 24.3, 23.2, 26.5, 29.7, 28.9, 24.6, 25.5, 30.2,…
$ rain_today      <fct> no, no, no, no, no, no, no, no, no, yes, no, yes, yes…
$ risk_mm         <dbl> 0.0, 0.0, 0.0, 1.0, 0.2, 0.0, 0.0, 0.0, 1.4, 0.0, 2.2…
$ rain_tomorrow   <fct> no, no, no, no, no, no, no, no, yes, no, yes, yes, ye…

Press Enter to continue: 

=============
Fit the Model
=============

Given the historic data which records the outcome we wish to predict
(rain_tomorrow) we can fit a model based on that data so as to predict
the outcome for new data.

The model will be built on a random sample of 70% (123,722) of the
observations. This is the training dataset.

The model is currently being built...

Press Enter to continue: 

=========================
Display the Decision Tree
=========================

An AI model targets a specific knowledge respresentation langauge. Here
the knowledge is represented as a decision tree. We can gain insight
into the model through a textual representation of the decision tree as
below.

The first line in the description reports the number of observations in
the training dataset. The line begining with 'node)' is a legend. Split
is a test condition, n is the number of observations that have made
there way to this node, the loss is the error in the prediction at this
node, the yval the majority class (i.e., the prediction), and yprob is
class probability.

n= 123722 

node), split, n, loss, yval, (yprob)
      * denotes terminal node

 1) root 123722 49488.800 no (0.6000000 0.4000000)  
   2) humidity_3pm< 64.5 92796 20592.860 no (0.7514612 0.2485388)  
     4) wind_gust_speed< 51 77929 13536.100 no (0.7989322 0.2010678) *
     5) wind_gust_speed>=51 14867  7056.761 no (0.5457407 0.4542593)  
      10) humidity_3pm< 45.5 8817  2742.284 no (0.6713901 0.3286099) *
      11) humidity_3pm>=45.5 6050  2875.072 yes (0.3998960 0.6001040) *
   3) humidity_3pm>=64.5 30926 11970.350 yes (0.2929151 0.7070849)  
     6) humidity_3pm< 79.5 20058  9709.632 yes (0.4119874 0.5880126)  
      12) rainfall< 1.15 12726  6607.019 no (0.5155528 0.4844472)  
        24) wind_gust_speed< 47 10144  4398.327 no (0.5749978 0.4250022) *
        25) wind_gust_speed>=47 2582  1080.620 yes (0.3285246 0.6714754) *
      13) rainfall>=1.15 7332  2678.388 yes (0.2697397 0.7302603) *
     7) humidity_3pm>=79.5 10868  2260.721 yes (0.1306888 0.8693112) *

Press Enter to continue: 

====================
Visual Decision Tree
====================

A visual representation of a model can often be more insightful than
the printed textual representation.  A decision tree model can readily
be visualised as a tree structure as we will see. The tree is read from
top to bottom, traversing the path corresponding to the answer to the
question presented at each node. The leaf node has the final decision
together with the class probabilities.

Press Enter to continue: 

Close the graphic window using Ctrl-W.
```
![](rain_dt_plot.png)

```console
Press Enter to continue: 

===================
Variable Importance
===================

One aspect of understanding the data and models that we build is what
variables play the most significant role in predicting the outcome.

The variables that are actually end up in the model are: humidity_3pm,
rainfall, wind_gust_speed.

All variables are considered in the modelling and the relative
importance of each variable in predicting the outcome is determined.
Note that in the table below the actual numbers represent the relative
importance of that variable.

Relative Importance of Variables:

   humidity_3pm wind_gust_speed        rainfall    humidity_9am       cloud_3pm 
             58               8               7               6               6 
       temp_3pm        sunshine  wind_speed_3pm  wind_speed_9am        max_temp 
              5               5               2               1               1 
   pressure_9am 
              1 

Press Enter to continue: 

=================
Visual Importance
=================

Once again a visual presentation of the variable importance can be more
effective in conveying the relative importance.

Press Enter to continue: 

Close the graphic window using Ctrl-W.
```
![](rain_dt_varimp.png)

```console
Press Enter to continue: 

==================
Variable Selection
==================

When the model was built, the algorithm chooses a variable for each
node of the resulting decision tree. An entropy, information theory or
gini based calculation is used to choose the variable. The variable
with the highest value according to this measure is chosen for the
particular node.

Below we will see the calculations that were made for the root node of
the tree (Node Number 1). A number of variables were considered and the
variable with the top score was chosen for this node. The improve= is
the value of the calculation.

Press Enter to continue: 

Node number 1: 123722 observations,    complexity param=0.342
  predicted class=no   expected loss=0.4  P(node) =1
    class counts: 97753 25969
   probabilities: 0.600 0.400 
  left son=2 (92796 obs) right son=3 (30926 obs)
  Primary splits:
      humidity_3pm < 64.5  to the left,  improve=11510, (0 missing)
      rainfall     < 0.35  to the left,  improve= 7486, (0 missing)
      rain_today   splits as  LR,        improve= 7133, (0 missing)
      cloud_3pm    < 6.5   to the left,  improve= 5030, (0 missing)
      humidity_9am < 73.5  to the left,  improve= 4535, (0 missing)
  Surrogate splits:
      cloud_3pm    < 7.5   to the left,  agree=0.778, adj=0.112, (0 split)
      humidity_9am < 87.5  to the left,  agree=0.775, adj=0.100, (0 split)
      sunshine     < 3.25  to the right, agree=0.772, adj=0.088, (0 split)
      temp_3pm     < 12.55 to the right, agree=0.771, adj=0.085, (0 split)
      rainfall     < 4.85  to the left,  agree=0.766, adj=0.063, (0 split)

Press Enter to continue: 

========================
Predicting Rain Tomorrow
========================

We now use the model to make predictions. The decision tree model is
applied to a previously unseen (by the mode) random subset of the
dataset of daily observations, the tuning dataset. This dataset
contains 26,512 observations.

This provides an insight into the performance of the model on
new/unseen data. The performance here is okay based on this dataset.
Note any highlighted errors. No model is perfect.

   Actual Predicted Error
1      no        no      
2      no       yes <----
3      no       yes <----
4      no        no      
5      no        no      
6      no        no      
7      no        no      
8     yes        no <----
9      no        no      
10     no        no      
11    yes        no <----
12     no        no      

Press Enter to continue: 

================
Confusion Matrix
================

A confusion matrix summarises the performance of the model on this
evluation dataset. All figures in the table are percentages and are
calculated across the predicitions made by the model for each
observation and compared to the actual or known values of the target
variable. The first column reports the true negative and false negative
rates whilst the second column reports the false positive and true
positive rates.

The Error column calculates the error across each class. We also report
the overall error which is calculated as the number of errors over the
number of observations. The average of the class errors is also
reported.

      Predicted
Actual   no  yes Error
   no  69.6  9.5  12.0
   yes  8.8 12.2  41.9

Overall error: 18%
Average class error: 27%

Press Enter to continue: 

==========
Risk Chart
==========

A risk chart presents a cumulative performance view of the model.

The x-axis can be thought of as the days across the dataset, but
sorting (left to right) to days from the highest probability of rain
tomorrow on the left to the lowest probability of rain tomorrow on the
right.

The y-axis is then the performance of the model in predicting whether
it will rain tomorrow. It is the percentage of the actual days on which
it rains that are predicted by the model as raining tomorrow. Thus,
100% (at the top) covers all days on which it rains. For the top 20% of
the days with the highest probability of rain tomorrow (Caseload =
20%), some 54% of the actual days for which it rained are predicted by
the model.

The more area under the curve the better the model performance. A
perfect model would follow the grey line. The Precision line represents
the lift offered by the model, with the lift values on the right hand
axis.

Close the graphic window using Ctrl-W.
```
![](rain_dt_risk.png)

## Resources

* [MLHub](https://mlhub.ai)

* [Rattle](https://rattle.togaware.com)

* [Multiple Inductive
  Learning](http://togaware.com/papers/milai87.pdf)
  
* [Github Repo](https://github.com/gjwgit/rain)

* [Data Science in R
  Template](https://onepager.togaware.com/Chapter_Data_Template.html)
