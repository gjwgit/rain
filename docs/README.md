Predicting Rain Tomorrow
========================

This [MLHub](https://mlhub.ai) package uses the weatherAUS dataset
from R's [Rattle](https://rattle.togaware.com) package to train a
predictive model for the probability of it raining tomorrow based on
today's weather observations. The training dataset consists of daily
weather observations from weather stations across Australia capturing
the amount of sunshine, the humidity, the amount of rain, etc. This
default model using the decision tree induction algorithm to build a
model that is represented as a decision tree. Other models include
decision a random forest which builds a forest of decision trees as an
ensemble type model
([rain_rf](https://github.com/gjwgit/rain/docs/blob/master/rainrf.md)).

These models come from the Essentials of Data Science by Graham
Williams <https://bit.ly/essentials_data_science>. Further support
material is also available from <https://essentials.togaware.com/>.

This repository provides an example of how to host multiple MLHub
packages within a single repository.

Visit the github repository for more details:
<https://github.com/gjwgit/rain>

Usage
-----

To install and run the pre-built model:

    $ ml install rain
    $ ml configure rain
    $ ml demo rain
