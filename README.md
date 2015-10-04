Regression Model
========================================================


# Data Import

As a first step we load several packages we need, and download data from the SQL database.


```r
require (MASS)
require (RODBC)
require (lubridate)
require (ggplot2)
require (sqldf)
require (dplyr)

channel <- odbcDriverConnect(connection = "DRIVER={SQL Server}; SERVER=DBACM\\ARCHIMEDES; DATABASE=LoadForecastingAnalytics")
data <- sqlQuery(channel, "SELECT * FROM ##OurData")

## delete Caledar month from initial data, you don't need it
#data$CalendarMonth <- NULL
#data$TwoSeasons <- ifelse(data$Season %in% c('Spring','Summer'),'SummerSeason','WinterSeason')
```


# Normalizing the volumes

Firstly we create the table Stat that for each account number will have its mean and standard deviation.


```r
Mean <- aggregate(DailyVolume~AccountNumberOID,data=data,FUN=mean)
names(Mean)[2] <- 'MeanVolume'
Std <- aggregate(DailyVolume~AccountNumberOID,data=data,FUN=sd)
names(Std)[2] <- 'StandardDev'

Stat <- cbind(Mean, Std$StandardDev)
names(Stat)[3] <- 'StandardDev'
head(Stat)
```

```
##   AccountNumberOID MeanVolume StandardDev
## 1          1534027  18.274603   5.8492102
## 2          1534032   4.678454   1.4709379
## 3          1534033  12.587353   3.5315759
## 4          1534457   2.311778   0.8118384
## 5          1535238  15.091061   3.0700753
## 6          1535239  18.489933   3.5460031
```

Then we join our data table to the table Stat on AccountNumberOID, and calculate normalized volumes for every account.

```r
data <- merge (data, Stat, by = 'AccountNumberOID')
data$NormalizedDailyVolume <- (data$DailyVolume - data$MeanVolume)/ data$StandardDev
head(data)
```

```
##   AccountNumberOID LDCAccountIdentifier LDCIdentifier
## 1          1534027         2.113121e+14             8
## 2          1534027         2.113121e+14             8
## 3          1534027         2.113121e+14             8
## 4          1534027         2.113121e+14             8
## 5          1534027         2.113121e+14             8
## 6          1534027         2.113121e+14             8
##   SourceSystemIdentifier CalendarMonth DailyVolume StationCode StationOID
## 1                  GBASS    2014-07-01    33.48824        KNYC         14
## 2                  GBASS    2014-02-01    16.94868        KNYC         14
## 3                  GBASS    2015-06-01    16.97761        KNYC         14
## 4                  GBASS    2015-05-01    12.05996        KNYC         14
## 5                  GBASS    2015-06-01    16.97761        KNYC         14
## 6                  GBASS    2015-01-01    17.63802        KNYC         14
##   Season AverageTemperature AverageDailyDewPointTemperature
## 1 Summer           76.09677                        61.09677
## 2 Winter           31.58929                        17.03571
## 3 Summer           73.61111                        58.11111
## 4 Spring           68.51613                        49.77419
## 5 Spring           70.14286                        57.09524
## 6 Winter           29.85484                        14.12903
##   AverageDailyRelativeHumidity AverageColdestHourlyWindChill
## 1                     63.48387                      49.32258
## 2                     58.03571                      17.03571
## 3                     60.22222                       0.00000
## 4                     56.22581                       0.00000
## 5                     67.76190                       0.00000
## 6                     50.00000                       0.00000
##   AverageDailyWindSpeed AverageDailyCloudCover
## 1                     5                     35
## 2                     7                     44
## 3                     4                     38
## 4                     3                     32
## 5                     4                     53
## 6                     6                     42
##   AverageDailyHeatingDegreeDays AverageDailyCoolingDegreeDays MeanVolume
## 1                      0.000000                     11.096774    18.2746
## 2                     33.500000                      0.000000    18.2746
## 3                      0.055555                      8.666666    18.2746
## 4                      1.290322                      4.806451    18.2746
## 5                      1.690476                      6.833333    18.2746
## 6                     35.145161                      0.000000    18.2746
##   StandardDev NormalizedDailyVolume
## 1     5.84921             2.6009734
## 2     5.84921            -0.2266839
## 3     5.84921            -0.2217387
## 4     5.84921            -1.0624752
## 5     5.84921            -0.2217387
## 6     5.84921            -0.1088319
```

# Model Selection

Firstly, we fit initial model for the stepwise regression.


```r
fit_all <- lm(NormalizedDailyVolume ~ 1, data = data)
summary(fit_all)
```

```
## 
## Call:
## lm(formula = NormalizedDailyVolume ~ 1, data = data)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -4.3339 -0.6991 -0.2108  0.6064  5.1300 
## 
## Coefficients:
##              Estimate Std. Error t value Pr(>|t|)
## (Intercept) 3.412e-16  2.299e-03       0        1
## 
## Residual standard error: 0.9816 on 182284 degrees of freedom
```

Then we run bidirectional stepwise regression in order to choose only important parameters for our model.
For now we are using only two weather parameters, we will test the others later.
This model selection is run for all the accounts together, and then we are going to fit every account individually using the selected model.


```r
stepAIC (fit_all, direct = 'both', scope = list(upper = ~ AverageDailyHeatingDegreeDays +
                                                  AverageDailyCoolingDegreeDays, low = ~1))
```

```
## Start:  AIC=-6766.11
## NormalizedDailyVolume ~ 1
## 
##                                 Df Sum of Sq    RSS    AIC
## + AverageDailyCoolingDegreeDays  1     31985 143656 -43408
## + AverageDailyHeatingDegreeDays  1      6986 168655 -14162
## <none>                                       175641  -6766
## 
## Step:  AIC=-43407.76
## NormalizedDailyVolume ~ AverageDailyCoolingDegreeDays
## 
##                                 Df Sum of Sq    RSS    AIC
## + AverageDailyHeatingDegreeDays  1      6021 137635 -51211
## <none>                                       143656 -43408
## - AverageDailyCoolingDegreeDays  1     31985 175641  -6766
## 
## Step:  AIC=-51210.56
## NormalizedDailyVolume ~ AverageDailyCoolingDegreeDays + AverageDailyHeatingDegreeDays
## 
##                                 Df Sum of Sq    RSS    AIC
## <none>                                       137635 -51211
## - AverageDailyHeatingDegreeDays  1      6021 143656 -43408
## - AverageDailyCoolingDegreeDays  1     31021 168655 -14162
```

```
## 
## Call:
## lm(formula = NormalizedDailyVolume ~ AverageDailyCoolingDegreeDays + 
##     AverageDailyHeatingDegreeDays, data = data)
## 
## Coefficients:
##                   (Intercept)  AverageDailyCoolingDegreeDays  
##                      -0.74835                        0.14958  
## AverageDailyHeatingDegreeDays  
##                       0.02016
```

Fit the model for all accounts together. This is for demonstration purpose only. We are not going to use these coefficients, as we will build different models for each account.


```r
aic_fit_all <- lm(NormalizedDailyVolume ~ AverageDailyHeatingDegreeDays +
                    AverageDailyCoolingDegreeDays, data = data)
summary(aic_fit_all)
```

```
## 
## Call:
## lm(formula = NormalizedDailyVolume ~ AverageDailyHeatingDegreeDays + 
##     AverageDailyCoolingDegreeDays, data = data)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -5.6169 -0.5543 -0.0965  0.5378  5.1087 
## 
## Coefficients:
##                                 Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                   -0.7483496  0.0056312  -132.9   <2e-16 ***
## AverageDailyHeatingDegreeDays  0.0201638  0.0002258    89.3   <2e-16 ***
## AverageDailyCoolingDegreeDays  0.1495789  0.0007380   202.7   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.8689 on 182282 degrees of freedom
## Multiple R-squared:  0.2164,	Adjusted R-squared:  0.2164 
## F-statistic: 2.517e+04 on 2 and 182282 DF,  p-value: < 2.2e-16
```

# Fitting selected model every account individually

Since we have many (around 27,000) account numbers in our data, it can take a long time for R to process all of them. So for now we just want to try to build models for the first 7,000 accounts. 


```r
#set.seed(1) # so random result would be the same every time you run
#data_testing <- subset (data, AccountNumberOID %in% sample(unique(data$AccountNumberOID),1000))
data_testing <- subset (data, AccountNumberOID %in% unique(data$AccountNumberOID)[1:7000])
```

Now we split the data by account number, creating a list of account numbers, each with its own data frame.

```r
data_split <- split(data_testing, data_testing$AccountNumberOID)
head(data_split,1)
```

```
## $`1534027`
##    AccountNumberOID LDCAccountIdentifier LDCIdentifier
## 1           1534027         2.113121e+14             8
## 2           1534027         2.113121e+14             8
## 3           1534027         2.113121e+14             8
## 4           1534027         2.113121e+14             8
## 5           1534027         2.113121e+14             8
## 6           1534027         2.113121e+14             8
## 7           1534027         2.113121e+14             8
## 8           1534027         2.113121e+14             8
## 9           1534027         2.113121e+14             8
## 10          1534027         2.113121e+14             8
## 11          1534027         2.113121e+14             8
## 12          1534027         2.113121e+14             8
## 13          1534027         2.113121e+14             8
## 14          1534027         2.113121e+14             8
## 15          1534027         2.113121e+14             8
## 16          1534027         2.113121e+14             8
## 17          1534027         2.113121e+14             8
## 18          1534027         2.113121e+14             8
## 19          1534027         2.113121e+14             8
## 20          1534027         2.113121e+14             8
## 21          1534027         2.113121e+14             8
## 22          1534027         2.113121e+14             8
## 23          1534027         2.113121e+14             8
## 24          1534027         2.113121e+14             8
## 25          1534027         2.113121e+14             8
##    SourceSystemIdentifier CalendarMonth DailyVolume StationCode StationOID
## 1                   GBASS    2014-07-01    33.48824        KNYC         14
## 2                   GBASS    2014-02-01    16.94868        KNYC         14
## 3                   GBASS    2015-06-01    16.97761        KNYC         14
## 4                   GBASS    2015-05-01    12.05996        KNYC         14
## 5                   GBASS    2015-06-01    16.97761        KNYC         14
## 6                   GBASS    2015-01-01    17.63802        KNYC         14
## 7                   GBASS    2015-03-01    17.26607        KNYC         14
## 8                   GBASS    2013-11-01    11.11866        KNYC         14
## 9                   GBASS    2013-12-01    20.60474        KNYC         14
## 10                  GBASS    2014-03-01    14.13589        KNYC         14
## 11                  GBASS    2014-10-01    13.33229        KNYC         14
## 12                  GBASS    2014-12-01    17.05762        KNYC         14
## 13                  GBASS    2015-04-01    10.08520        KNYC         14
## 14                  GBASS    2013-12-01    20.60474        KNYC         14
## 15                  GBASS    2014-03-01    14.13589        KNYC         14
## 16                  GBASS    2014-08-01    31.85607        KNYC         14
## 17                  GBASS    2015-02-01    22.49776        KNYC         14
## 18                  GBASS    2015-07-01    25.67126        KNYC         14
## 19                  GBASS    2014-01-01    19.40636        KNYC         14
## 20                  GBASS    2014-04-01    11.86198        KNYC         14
## 21                  GBASS    2014-09-01    22.43346        KNYC         14
## 22                  GBASS    2014-09-01    22.43346        KNYC         14
## 23                  GBASS    2014-11-01    13.94983        KNYC         14
## 24                  GBASS    2014-12-01    17.05762        KNYC         14
## 25                  GBASS    2015-03-01    17.26607        KNYC         14
##    Season AverageTemperature AverageDailyDewPointTemperature
## 1  Summer           76.09677                       61.096774
## 2  Winter           31.58929                       17.035714
## 3  Summer           73.61111                       58.111111
## 4  Spring           68.51613                       49.774193
## 5  Spring           70.14286                       57.095238
## 6  Winter           29.85484                       14.129032
## 7  Spring           39.09091                       21.090909
## 8    Fall           45.23333                       28.100000
## 9  Winter           39.70000                       25.500000
## 10 Winter           36.42500                       16.800000
## 11   Fall           59.62903                       46.483870
## 12   Fall           39.30952                       27.809523
## 13 Spring           54.25000                       32.866666
## 14   Fall           38.00000                       26.619047
## 15 Spring           40.63636                       21.545454
## 16 Summer           74.51613                       58.129032
## 17 Winter           23.94643                        8.571428
## 18 Summer           78.58065                       61.903225
## 19 Winter           28.64516                       14.935483
## 20 Spring           52.38333                       33.133333
## 21   Fall           67.37500                       53.500000
## 22 Summer           70.59091                       56.090909
## 23   Fall           45.30000                       28.200000
## 24 Winter           43.05000                       29.500000
## 25 Winter           37.55000                       19.750000
##    AverageDailyRelativeHumidity AverageColdestHourlyWindChill
## 1                      63.48387                      49.32258
## 2                      58.03571                      17.03571
## 3                      60.22222                       0.00000
## 4                      56.22581                       0.00000
## 5                      67.76190                       0.00000
## 6                      50.00000                       0.00000
## 7                      52.27273                       0.00000
## 8                      53.33333                      33.00000
## 9                      57.80000                      25.90000
## 10                     47.90000                      19.95000
## 11                     63.58064                       0.00000
## 12                     64.90476                       0.00000
## 13                     49.73333                       0.00000
## 14                     63.71428                      26.33333
## 15                     50.09091                      24.18182
## 16                     60.16129                       0.00000
## 17                     47.46428                       0.00000
## 18                     59.35484                      15.22581
## 19                     58.09677                      11.12903
## 20                     54.63333                      39.50000
## 21                     62.75000                       0.00000
## 22                     62.36364                       0.00000
## 23                     54.03333                       0.00000
## 24                     61.70000                       0.00000
## 25                     51.25000                       0.00000
##    AverageDailyWindSpeed AverageDailyCloudCover
## 1                      5                     35
## 2                      7                     44
## 3                      4                     38
## 4                      3                     32
## 5                      4                     53
## 6                      6                     42
## 7                      5                     55
## 8                      7                     43
## 9                      7                     48
## 10                     7                     53
## 11                     4                     48
## 12                     6                     67
## 13                     5                     38
## 14                     6                     58
## 15                     8                     46
## 16                     3                     30
## 17                     6                     42
## 18                     3                     31
## 19                     8                     50
## 20                     6                     37
## 21                     3                     25
## 22                     3                     33
## 23                     4                     44
## 24                     5                     48
## 25                     5                     45
##    AverageDailyHeatingDegreeDays AverageDailyCoolingDegreeDays MeanVolume
## 1                       0.000000                     11.096774    18.2746
## 2                      33.500000                      0.000000    18.2746
## 3                       0.055555                      8.666666    18.2746
## 4                       1.290322                      4.806451    18.2746
## 5                       1.690476                      6.833333    18.2746
## 6                      35.145161                      0.000000    18.2746
## 7                      25.909090                      0.000000    18.2746
## 8                      19.600000                      0.000000    18.2746
## 9                      25.300000                      0.100000    18.2746
## 10                     28.550000                      0.000000    18.2746
## 11                      5.967741                      0.596774    18.2746
## 12                     25.690476                      0.000000    18.2746
## 13                     10.900000                      0.150000    18.2746
## 14                     26.952380                      0.000000    18.2746
## 15                     24.181818                      0.000000    18.2746
## 16                      0.000000                      9.516129    18.2746
## 17                     41.053571                      0.000000    18.2746
## 18                      0.000000                     13.580645    18.2746
## 19                     36.322580                      0.000000    18.2746
## 20                     12.700000                      0.100000    18.2746
## 21                      1.062500                      3.437500    18.2746
## 22                      0.704545                      6.295454    18.2746
## 23                     19.700000                      0.000000    18.2746
## 24                     21.950000                      0.000000    18.2746
## 25                     27.450000                      0.000000    18.2746
##    StandardDev NormalizedDailyVolume
## 1      5.84921             2.6009734
## 2      5.84921            -0.2266839
## 3      5.84921            -0.2217387
## 4      5.84921            -1.0624752
## 5      5.84921            -0.2217387
## 6      5.84921            -0.1088319
## 7      5.84921            -0.1724216
## 8      5.84921            -1.2234033
## 9      5.84921             0.3983671
## 10     5.84921            -0.7075681
## 11     5.84921            -0.8449535
## 12     5.84921            -0.2080590
## 13     5.84921            -1.4000875
## 14     5.84921             0.3983671
## 15     5.84921            -0.7075681
## 16     5.84921             2.3219316
## 17     5.84921             0.7220042
## 18     5.84921             1.2645560
## 19     5.84921             0.1934883
## 20     5.84921            -1.0963235
## 21     5.84921             0.7110120
## 22     5.84921             0.7110120
## 23     5.84921            -0.7393782
## 24     5.84921            -0.2080590
## 25     5.84921            -0.1724216
```

Now we use lapply function in order to fit the models for all accounts.

```r
account_fits <- lapply(data_split, lm, formula = DailyVolume ~ AverageDailyCoolingDegreeDays + AverageDailyHeatingDegreeDays )
head(account_fits,2)
```

```
## $`1534027`
## 
## Call:
## FUN(formula = ..1, data = X[[1L]])
## 
## Coefficients:
##                   (Intercept)  AverageDailyCoolingDegreeDays  
##                        9.6641                         1.5739  
## AverageDailyHeatingDegreeDays  
##                        0.2647  
## 
## 
## $`1534032`
## 
## Call:
## FUN(formula = ..1, data = X[[2L]])
## 
## Coefficients:
##                   (Intercept)  AverageDailyCoolingDegreeDays  
##                        2.9127                         0.3868  
## AverageDailyHeatingDegreeDays  
##                        0.0402
```



# Analysis of models

## Determining R Squared and p-value For each individual account model
Now, after we fit all models for every account number, we want to analyze them and check how good they are.
We create a data frame, that we call "model_stats", that for every account number will show p-value for F-test and R squared. 


```r
##Analysis of the model
extract_rsq <- function(fits){
  
    model_stats <- data.frame()
    for (i in 1:length(fits)){
    
    f <- summary(fits[[i]])$fstatistic
    #calculate p-value
    pvalue <- 1-pf(f[1],f[2],f[3])
    r_sq <- summary(fits[[i]])$adj.r.squared
    
    model_stats<-rbind(model_stats,cbind(data_split[[i]]$AccountNumberOID[1],pvalue,r_sq))

  } 
  rownames(model_stats) <- NULL  
  names(model_stats)[1] <- 'AccountNumberOID'
  
  return(model_stats)
}

model_stats <- extract_rsq(account_fits)
```

## Weather-sensetive Accounts
We want to create regression model only for those account  numbers, for which weather has significant impact on the usage. So we filter only for the accounts that have p-value for F-statistics less than 5%. 


```r
#model_stats_weather_dependant <- subset(model_stats, pvalue < 0.05)

#let's include all for now, just to test
model_stats_weather_dependant <- model_stats
```

Total number of accounts is 6644, number of weather-sensetive accounts is 6644, which gives us **100.00%  of weather sensetive accounts**, that we want to use regression model for.

See below plots that demonstarte R squared distribution for weather dependant account:


```r
plot(model_stats_weather_dependant$r_sq, main = 'Plot of R Squared')
```

![plot of chunk unnamed-chunk-12](figure/unnamed-chunk-12-1.png) 

```r
hist(model_stats_weather_dependant$r_sq, main = 'Histogram of R Squared', breaks= 15)
```

![plot of chunk unnamed-chunk-12](figure/unnamed-chunk-12-2.png) 

```r
#mean(model_stats_weather_dependant$r_sq)
#median(model_stats_weather_dependant$r_sq)
```

## MAPE calculation

The result above shows us analysis of R sq, but we want to understand it in term of MAPEs. 
For our model we would like to get an accuracy of 6%, so our main question at this point is what kind of R sq we need to have in order to get 6% of MAPE.

To calculate MAPEs for every account, we need to find actual, and backcasted volumes.


```r
accounts_backcast <- lapply (account_fits, predict)
# fit$dataframe is the data frame for the fit. first column is Y-variable

#accounts_actual    <- subset (data_testing, select=c(AccountNumberOID,DailyVolume,CalendarMonth))
accounts_actual   <- lapply (account_fits, function(x) x$model[,1])
```

Now converting lists into data frames for convinient use, and combining actual and backcasted volumes into one data frame.

```r
dataframe_backcast <- data.frame(
  AccountNumberOID = rep(names(accounts_backcast), lapply(accounts_backcast, length)),
  backcast = unlist(accounts_backcast))

dataframe_actual <- data.frame(
  AccountNumberOID = rep(names(accounts_actual), lapply(accounts_actual, length)),
  actual = unlist(accounts_actual))

dataframe_all <- data.frame(dataframe_backcast,actual=dataframe_actual$actual)
head(dataframe_all)
```

```
##           AccountNumberOID backcast   actual
## 1534027.1          1534027 27.12983 33.48824
## 1534027.2          1534027 18.53129 16.94868
## 1534027.3          1534027 23.31968 16.97761
## 1534027.4          1534027 17.57073 12.05996
## 1534027.5          1534027 20.86686 16.97761
## 1534027.6          1534027 18.96676 17.63802
```

Aggregating all the data together, and calculating absolute error, that we need for MAPE calculation.


```r
dataframe_all <- aggregate(cbind(actual,backcast,abs(actual-backcast))~AccountNumberOID,data=dataframe_all,FUN=sum)
names(dataframe_all)[4]='error'
```

Calculating the mape:

```r
dataframe_all$mape <- with(dataframe_all,error/actual)
```

Now we want to merge all the data together, so we would have information about both R sq and MAPEs.


```r
dataframe_all <- merge(model_stats,dataframe_all)
head(dataframe_all)
```

```
##   AccountNumberOID       pvalue      r_sq    actual  backcast    error
## 1          1534027 7.323630e-05 0.5409557 456.86509 456.86509 75.43793
## 2          1534032 2.225132e-08 0.7362380 130.99671 130.99671 12.37032
## 3          1534033 8.848922e-12 0.8590144 352.44590 352.44590 26.28124
## 4          1534457 2.059797e-12 0.8640910  67.04158  67.04158  5.92535
## 5          1535238 4.634742e-06 0.5956981 422.54970 422.54970 43.03812
## 6          1535239 6.259636e-08 0.7134848 517.71811 517.71811 40.64391
##         mape
## 1 0.16512079
## 2 0.09443232
## 3 0.07456817
## 4 0.08838322
## 5 0.10185340
## 6 0.07850587
```

## MAPE vs R-squared

Let's see how MAPE depends on R sq.

Firstly we multiply percentage values by 100, so values on the graph would be more clear:

```r
dataframe_all$mape <- dataframe_all$mape*100
dataframe_all$r_sq <- dataframe_all$r_sq*100
```

And then we plot MAPE against R squared:

```r
ggplot(data = dataframe_all, aes(x=r_sq, y=mape))+
  geom_point(color = 'blue')+
  theme_bw() +
  xlab('R Squared, %') +
  ylab('MAPE, %') +
  ggtitle('MAPE vs R Squared') +
  scale_x_continuous(breaks=c(-10:20)*5)+
  scale_y_continuous(breaks=c(1:17)*5)
```

![plot of chunk unnamed-chunk-19](figure/unnamed-chunk-19-1.png) 

You can clearly see that for big R squared values MAPEs tend to be smaller, but we can't make good conclusions from this plot.
So we tried couple of different approaches.

### Avg MAPE vs R-sq threshold

The first approach is the following:
Instead of looking at the MAPEs for every particular value of R squared, we'll look at mean and median MAPEs for all R squared that are equal to or bigger than certain value.

Firstly we need to do some data preparation.

For values of R squared from 40% to 99.9% with a step of 0.5% we find mean and median of MAPE using "summary" function.

```r
threshold<-seq(40,99.9,0.5)

mape_stats <- 
  data.frame(
    cbind(
      threshold,
      t(
        sapply(
          threshold,
          function(th)
            summary(subset(dataframe_all,r_sq>th)$mape)
        )
      )
    )
  )[,c('threshold','Median','Mean')]
head(mape_stats)
```

```
##   threshold Median  Mean
## 1      40.0  10.27 11.40
## 2      40.5  10.26 11.38
## 3      41.0  10.25 11.35
## 4      41.5  10.24 11.31
## 5      42.0  10.22 11.30
## 6      42.5  10.22 11.28
```

Now we have to change format of the data frame in order to build ggplot:


```r
median      <- data.frame(rep('Median', nrow(mape_stats)))
median_data <- cbind (mape_stats[,c('threshold','Median')],median)
names(median_data) <- c('threshold','StatValue','StatName')


mean      <- data.frame(rep('Mean', nrow(mape_stats)))
mean_data <- cbind (mape_stats[,c('threshold','Mean')],mean)
names(mean_data) <- c('threshold','StatValue','StatName')

mape_stats_processed <- rbind(median_data,mean_data)
mape_stats_processed$StatValue <- mape_stats_processed$StatValue
mape_stats_processed$threshold <- mape_stats_processed$threshold

head(mape_stats_processed)
```

```
##   threshold StatValue StatName
## 1      40.0     10.27   Median
## 2      40.5     10.26   Median
## 3      41.0     10.25   Median
## 4      41.5     10.24   Median
## 5      42.0     10.22   Median
## 6      42.5     10.22   Median
```

Now we are ready to build our plot:

![plot of chunk unnamed-chunk-22](figure/unnamed-chunk-22-1.png) 


### Proportion of accounts with MAPE within a certain limit vs R-sq threshold

Let's now make another plot.

Our goal is to have MAPE of 6% or less, so let's build a plot, that shows probabilies for MAPE to be less than or equal to 6%.
We'll plot other MAPE values for comparison as well.

Firstly, we prepare the data for plotting.


```r
# a function for calculating a proportion of accounts with MAPE within a specified limit (for different R-sq thresholds)
prob_data_for_mape <- function(x)
{
  prob_data_for_mape <-    
    data.frame(
      cbind(
        x,
        threshold,
        sapply(
          threshold, 
          function(th) 
            length(subset(dataframe_all,r_sq>th & mape<=x)$mape)/length(subset(dataframe_all,r_sq>th)$mape)
        )
      )
    )
  
  names(prob_data_for_mape) <- c ('MAPE','threshold', 'prob')
  return(prob_data_for_mape)
}


# constructing proportions for different MAPE limits
prob_data_all_mapes <- rbind (prob_data_for_mape(4),
                              prob_data_for_mape(5),
                              prob_data_for_mape(6),
                              prob_data_for_mape(7),
                              prob_data_for_mape(8),
                              prob_data_for_mape(9),
                              prob_data_for_mape(10))


#prob_data_all_mapes$MAPE      <- prob_data_all_mapes$MAPE
prob_data_all_mapes$MAPE      <- factor(prob_data_all_mapes$MAPE, levels = c(10,9,8,7,6,5,4))
#prob_data_all_mapes$MAPE      <- factor(paste('\u2264', prob_data_all_mapes$MAPE, '%', sep =''))
prob_data_all_mapes$prob      <- prob_data_all_mapes$prob*100
prob_data_all_mapes$threshold <- prob_data_all_mapes$threshold
```

Now we are ready to build the plot:

![plot of chunk unnamed-chunk-24](figure/unnamed-chunk-24-1.png) 
