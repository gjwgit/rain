Predicting Rain Tomorrow: Random Forest
=======================================

This [MLHub](https://mlhub.ai) package uses the weatherAUS dataset
from R's [Rattle](https://rattle.togaware.com) package to train a
predictive model for the probability of it raining tomorrow based on
today's weather observations. The training dataset consists of daily
weather observations from weather stations across Australia capturing
the amount of sunshine, the humidity, the amount of rain, etc.

This model comes from the Essentials of Data Science by Graham Williams
<https://bit.ly/essentials_data_science>. Further support material is
also available from <https://essentials.togaware.com/>.

A simpler decision tree model,
[rain_dt](https://github.com/gjwgit/rain/blob/master/rain_dt.md), is
also available for this same task.

Visit the github repository for more details:
<https://github.com/gjwgit/rain/master/blob/rain_rf.md>

Usage
-----

To install and run the pre-built model:

    $ ml install gjwgit/rain:rain_rf.yaml
    $ ml configure rain_rf
    $ ml demo rain_rf
