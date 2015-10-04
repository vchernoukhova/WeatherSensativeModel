require (MASS)
require (RODBC)
require (lubridate)
require (ggplot2)
require (sqldf)
require (dplyr)


channel <- odbcDriverConnect(connection = "DRIVER={SQL Server}; SERVER=DBACM\\ARCHIMEDES; DATABASE=LoadForecastingAnalytics")
data <- sqlQuery(channel, "SELECT * FROM ##JointData")

head(data)
data$Month <- month(data$SettlementMonth) 
#data_testing <- subset (data, AccountNumberOID %in% sample(unique(data$AccountNumberOID),7000))
data_testing <- data

data_split <- split(data_testing, data_testing$AccountNumberOID)
head(data_split,1)

account_fits <- lapply(data_split, lm, formula = UsageVolume ~ factor(Month):LoadProfileVolume-1)


##Analysis of the model
extract_rsq <- function(fits){
  
  model_stats <- data.frame()
  for (i in 1:length(fits)){
    
    f <- summary(fits[[i]])$fstatistic
    #calculate p-value
    pvalue <- 1-pf(f[1],f[2],f[3])
    r_adj <- summary(fits[[i]])$adj.r.squared
    r_sq <- summary(fits[[i]])$r.squared
    
    r_sq_real <- 1-(1-r_sq) * mean(fits[[i]]$model[,1]^2)/(sd(fits[[i]]$model[,1])^2*(nrow(fits[[i]]$model)-1)/nrow(fits[[i]]$model))
    
    r_adj_real <- 1 - (1-r_sq_real)*(nrow(fits[[i]]$model)-1)/(nrow(fits[[i]]$model)-length(fits[[i]]$coefficients))
    
    #pvalue_lp <- summary(fits[[i]])$coef['LoadProfileVolume',4]
    model_stats <- rbind(model_stats,cbind(data_split[[i]]$AccountNumberOID[1],pvalue,r_adj,r_sq,r_sq_real,r_adj_real))
    
  } 
  rownames(model_stats) <- NULL  
  names(model_stats)[1] <- 'AccountNumberOID'
  
  return(model_stats)
}

model_stats <- extract_rsq(account_fits)
#model_stats_weather_dependant <- subset(model_stats, pvalue < 0.05)


mean(model_stats$r_sq)
median( model_stats$r_sq)
hist(model_stats$r_sq, main = 'Histogram of R Squared', breaks= 15)


mean(model_stats$r_sq_real)
median( model_stats$r_sq_real)
hist(model_stats$r_sq_real, main = 'Histogram of R Squared', breaks= 15)
     
mean(model_stats$r_adj_real)
median( model_stats$r_adj_real)
hist(model_stats$r_adj_real, main = 'Histogram of R Squared', breaks= 15)

mean(model_stats$r_adj)
median( model_stats$r_adj)
hist(model_stats$r_adj, main = 'Histogram of R Squared', breaks= 15)


#mean( model_stats$r_adj)
#median( model_stats$r_adj)
#hist(model_stats$r_adj, main = 'Histogram of R Squared', breaks= 15)

##########################
accounts_backcast <- lapply (account_fits, predict)

accounts_actual   <- lapply (account_fits, function(x) x$model[,1])

dataframe_backcast <- data.frame(
  AccountNumberOID = rep(names(accounts_backcast), lapply(accounts_backcast, length)),
  backcast = unlist(accounts_backcast))

dataframe_actual <- data.frame(
  AccountNumberOID = rep(names(accounts_actual), lapply(accounts_actual, length)),
  actual = unlist(accounts_actual))

dataframe_all <- data.frame(dataframe_backcast,actual=dataframe_actual$actual)
head(dataframe_all)

dataframe_all <- aggregate(cbind(actual,backcast,abs(actual-backcast))~AccountNumberOID,data=dataframe_all,FUN=sum)
names(dataframe_all)[4]='error'

dataframe_all$mape <- with(dataframe_all,error/actual)

dataframe_all <- merge(model_stats,dataframe_all)
head(dataframe_all)


dataframe_all$mape <- dataframe_all$mape*100
dataframe_all$r_sq <- dataframe_all$r_sq*100


hist(dataframe_all$mape, main = 'Histogram of Mape', breaks= 50)
mean(dataframe_all$mape)
median(dataframe_all$mape)
#hist(dataframe_all$mape, main = 'Histogram of Mape', breaks= 15, xlim = c(0,20))

head(dataframe_all)

#dataframe_all$factors <- 'M_LP_CDD_HDD'



#channel <- odbcDriverConnect(connection = "DRIVER={SQL Server}; SERVER=DBACM\\ARCHIMEDES; DATABASE=LoadForecastingAnalytics")

#sqlSave(channel,dataframe_all, tablename = 'RegressionOutput', rownames = FALSE,
#        append=TRUE)
