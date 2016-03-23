# Predicting Bike Share Demand: Learning by Eking Out Maximum Performance

The objective of this competition is to accurately predict demand for bikes in a bike sharing business using only historical data and a few other variables like the weather, season and working/holiday. This is a comprehensive solution to obtain a **top 20** entry on the leaderboard for the bike share competition on Kaggle. Note that there is no practical difference between the top 100 entries or so. My motivation in trying to place as high as I did was to learn as many techniques as I could. 

## Road to Top 20
Getting to top 20 on this competition took a lot of work. I had to engineer new features, learn new modeling approaches, explore new hyperparameter optimization methods and apply ensemble learning techniques. Here is a partial summary of the things that I tried:

* _Feature engineering the date and time variables_
* _Tried a huge array of ML models including random forests, extremely randomized trees, generalized linear models, elastic nets, generalized additive models and autoregressive models_
* _Applied bayesian hyperparameter optimization techniques to eke out every last ounce (micro-ounce?) of performance_
* _Used ensemble techniques to reduce variance and improve predictions_

## Dataset
The dataset consisted of the following variable:

* datetime - hourly date + timestamp  
* season -  1 = spring, 2 = summer, 3 = fall, 4 = winter 
* holiday - whether the day is considered a holiday
* workingday - whether the day is neither a weekend nor holiday
* weather - 
  * 1: Clear, Few clouds, Partly cloudy, Partly cloudy 
  * 2: Mist + Cloudy, Mist + Broken clouds, Mist + Few clouds, Mist 
  * 3: Light Snow, Light Rain + Thunderstorm + Scattered clouds, Light Rain + Scattered clouds 
  * 4: Heavy Rain + Ice Pallets + Thunderstorm + Mist, Snow + Fog 
* temp - temperature in Celsius
* atemp - "feels like" temperature in Celsius
* humidity - relative humidity
* windspeed - wind speed
* casual - number of non-registered user rentals initiated
* registered - number of registered user rentals initiated
* count - number of total rentals

## Metric
The metric is root mean square logarithmic error. I've reproduced the metric from the competition for reference.
![alt text](https://github.com/ganesh-krishnan/kaggleBikeShare/blob/master/rmsle.png)
