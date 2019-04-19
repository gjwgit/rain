Predicting Rain Tomorrow
========================

These [MLHub](https://mlhub.ai) packages use the weatherAUS dataset
from R's [Rattle](https://rattle.togaware.com) package to train a
predictive model for the probability of it raining tomorrow based on
today's weather observations. The training dataset consists of daily
weather observations from weather stations across Australia capturing
the amount of sunshine, the humidity, the amount of rain, etc. Models
available include decision tree
([rain](https://github.com/gjwgit/rain/rain_dt.md)) and random forest
([rainrf](https://github.com/gjwgit/rain/rain_rf.md)).

These models come from the Essentials of Data Science by Graham
Williams <https://bit.ly/essentials_data_science>. Further support
material is also available from <https://essentials.togaware.com/>.

Visit the github repository for more details:
<https://github.com/gjwgit/rain>

Usage
-----

- To install mlhub (Ubuntu 18.04 LTS)

  ```shell
  $ pip3 install mlhub
  ```

- To install and configure the rain_dt (or rain_rf) pre-built model:

  ```shell
  $ ml install   rain_dt
  $ ml configure rain_dt
  ```

Demonstration
-------------

```console
$ ml demo rain_dt
```
