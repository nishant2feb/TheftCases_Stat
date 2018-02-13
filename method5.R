# getSeasonalityindex <- function(df){
#   df = MonthlyData[MonthlyData$ConsumerNumber == 2000000871,]
#   library(zoo)
#   library(lubridate)
#   idx = as.yearmon(df$MoveInDate) <= as.yearmon(df$MonthofConsumption)
#   df = df[idx,]
#   df = df[ order(df[,"MonthofConsumption"]), ]
#   df$Consumption_kWh[is.na(df$Consumption_kWh)] = 0
#   days = df$Days
#   year = as.numeric(year(df$MonthofConsumption[1]))
#   month = as.numeric(month(df$MonthofConsumption[1]))
#   
#   
#   ts_data = ts(df$LoadFactor,start=c(year,month),frequency = 12)
#   # Remove seasonality
#   decompose_ts = decompose(ts_data, "additive")
#   adj_ts = ts_data - decompose_ts$seasonal
#   plot(ts_data)
#   plot(adj_ts)
#   library(forecast)
#   # First remove the trend
#   trend = ma(ts_data, order=12, centre=TRUE)
# }
# ar(ts_data, aic = TRUE)
# deseasonlise <- function(){
#   
# }
# 
# fit <- arima(ts_data,order=c(1,1,12))
# plot(fit$x,col="red")
# lines(fitted(fit),col="blue")
###################################################################
#MonthlyData = read.csv("Data\\PROCESSED\\MonthlyConsumptionData.csv", header = TRUE,
#                       stringsAsFactors = FALSE)

Method5<-function(MonthlyData){
set.seed(100)
MonthlyData = transform(MonthlyData, MoveInDate = as.Date(MoveInDate, '%Y-%m-%d'),
                        MonthofConsumption = as.Date(MonthofConsumption, '%Y-%m-%d'))

# Convert the negative consumption and load factor to zero
MonthlyData$Consumption_kWh[(MonthlyData$Consumption_kWh < 0)] = 0
MonthlyData$LoadFactor[(MonthlyData$LoadFactor < 0)] = 0

# Perform clustering on the load factor rather than absolute consumption
# Take the consumers with move-in Date before Jan 2015
ConsumptionDF = MonthlyData[MonthlyData$MoveInDate <= as.Date('2015-01-01','%Y-%m-%d'),]

# Select the desired parameters
select_cols = c("ConsumerNumber","ConsumerType","StreetType","MonthofConsumption",
                "LoadFactor")
library(tidyr)
DataMatrix = ConsumptionDF[,select_cols] %>%
  spread(MonthofConsumption,LoadFactor)

# Impute the NA vlaues using imputeTS
library(imputeTS)

for(i in seq(1,nrow(DataMatrix),1)){
  ts_data = ts(as.numeric(DataMatrix[i,-c(1,2,3)]), start=c(2015,01), frequency = 12)
  if(sum(is.na(ts_data)) >= 12){
    next
  }else{
    if(sum(ts_data,na.rm = TRUE) != 0){
      ts_data <- na.kalman(ts_data)
      ts_data[ts_data < 0] = 0
      DataMatrix[i,-c(1,2,3)] <- ts_data 
    }else{
      ts_data[is.na(ts_data)] = 0
    }
  }
}

# Remove the rows where imputation not performed
DataMatrix = DataMatrix[!rowSums(is.na(DataMatrix))>0,]
sum(is.na(DataMatrix))

DataMatrix$NewCategory = "OTHER"
DataMatrix$NewCategory[DataMatrix$ConsumerType == "LTIndustry"] = "LTIndustry"
DataMatrix$NewCategory[DataMatrix$ConsumerType == "Institutional"] = "Institutional"
DataMatrix$NewCategory[(DataMatrix$ConsumerType == "Domestic")&
                         (DataMatrix$StreetType == "RURAL")] = "DomesticRural"
DataMatrix$NewCategory[(DataMatrix$ConsumerType == "Domestic")&
                         (DataMatrix$StreetType != "RURAL")] = "DomesticNonRural"
DataMatrix$NewCategory[(DataMatrix$ConsumerType == "Commercial")&
                         (DataMatrix$StreetType == "RURAL")] = "CommercialRural"
DataMatrix$NewCategory[(DataMatrix$ConsumerType == "Commercial")&
                         (DataMatrix$StreetType != "RURAL")] = "CommercialNonRural"

library(mclust)
first = TRUE
first1 = TRUE
for(cat in unique(DataMatrix$NewCategory)){
  idx = DataMatrix$NewCategory == cat
  Data = DataMatrix[idx,-c(1,2,3,ncol(DataMatrix))]
  EMClusters = Mclust(Data)
  ConsumerClusters = data.frame(DataMatrix[idx,c("ConsumerNumber")],EMClusters$classification)
  names(ConsumerClusters) = c("ConsumerNumber","ClusterNumber")
  # Get the expected consumption for each month
  ExpectedConsumption = data.frame(EMClusters$parameters[['mean']])
  if(first == TRUE){
    ConsumerClusterList = ConsumerClusters
    first = FALSE
  }else{
    ConsumerClusterList = rbind(ConsumerClusterList,ConsumerClusters)
  }
  for(i in c(seq(1,ncol(ExpectedConsumption),1))){
    ConsumerType = c(rep(cat,nrow(ExpectedConsumption)))
    cluster_no = rep(i,nrow(ExpectedConsumption))
    month = as.character(names(Data))
    expected = ExpectedConsumption[,i]
    if(first1 == TRUE){
      ExpectedDF = data.frame(month,ConsumerType,cluster_no,expected)
      first1 = FALSE
    }else{
      ExpectedDF = rbind(ExpectedDF,data.frame(month,ConsumerType,cluster_no,expected))
    }
  }
  rm(ConsumerType,cluster_no,month,expected)
}
rm(first1,first,idx,Data,EMClusters,ConsumerClusters,ExpectedConsumption)

#write.csv(ConsumerClusterList,"ConsumerClusters.csv", row.names = FALSE)
#write.csv(ExpectedDF,"ExpectedCluster.csv",row.names = FALSE)
#write.csv(DataMatrix,"DataMatrix.csv",row.names = FALSE)

DataMatrix = merge(DataMatrix,
                   ConsumerClusterList,
                   by = "ConsumerNumber",
                   all.x = TRUE)


names(ExpectedDF) = c("Month","NewCategory","ClusterNumber","Expected")
library(reshape2)
ObservedData = DataMatrix %>%
  reshape2::melt(id = c("ConsumerNumber","ConsumerType","StreetType","NewCategory","ClusterNumber"),
       value.name = "Observed")
names(ObservedData)[names(ObservedData) == 'variable'] <- 'Month'
ObservedData$Month = as.character(ObservedData$Month)

# Convert Month into Date Format
ExpectedDF = transform(ExpectedDF, Month = as.Date(Month,"%Y-%m-%d"))
ObservedData = transform(ObservedData, Month = as.Date(Month,"%Y-%m-%d"))

# Now compute the seasonality index for each cluster in the different categories
ObservedData = merge(ObservedData,
                     ExpectedDF,
                     by = c("NewCategory","ClusterNumber","Month"),
                     all.x = TRUE)

# Compute the Q1 of expected Load factor of each year
library(lubridate)
Q1_Expected = ObservedData %>%
  dplyr::mutate(Year = year(Month)) %>%
  dplyr::group_by(NewCategory,ClusterNumber,Year) %>%
  dplyr::summarise(ExpectedQ1 = quantile(Expected, probs = c(0.25)))
  

ObservedData$Year = year(ObservedData$Month) 
ObservedData = merge(ObservedData,
                     Q1_Expected,
                     by=c("NewCategory","ClusterNumber","Year"),
                     all.x = TRUE)
#ObservedData$Divergence = (ObservedData$Observed - ObservedData$Expected)

# Combine the data with actual data-set
names(ObservedData)[names(ObservedData) == 'Month'] <- 'MonthofConsumption'
ObservedData = merge(ObservedData[,c("ConsumerNumber","MonthofConsumption","Expected","ExpectedQ1")],
                     MonthlyData,
                     by = c("ConsumerNumber","MonthofConsumption"))

# Compare the desired parameter with expected
#ObservedData$Divergence = ObservedData$LoadFactor - ObservedData$Expected
ObservedData$Year = year(ObservedData$MonthofConsumption)

# Compute the deviation score
library(dplyr)
ObservedData$DeviationFlag = ObservedData$ExpectedQ1 > ObservedData$LoadFactor
DeviationScore = ObservedData %>%
  dplyr::group_by(ConsumerNumber,Year) %>%
  dplyr::summarise(MonthsCount = sum(DeviationFlag))

#
DeviationScore$YearWiseScore = 0
DeviationScore[DeviationScore$Year == 2015,"YearWiseScore"] = 0.2 * DeviationScore[DeviationScore$Year == 2015,"MonthsCount"]
DeviationScore[DeviationScore$Year == 2016,"YearWiseScore"] = 0.3 * DeviationScore[DeviationScore$Year == 2016,"MonthsCount"]
DeviationScore[DeviationScore$Year == 2017,"YearWiseScore"] = 2*0.5 * DeviationScore[DeviationScore$Year == 2017,"MonthsCount"]

FinalScore = DeviationScore %>%
  dplyr::group_by(ConsumerNumber) %>%
  dplyr::summarise(Risk_Score = sum(YearWiseScore))

FinalScore$RiskCategory = "Low"
FinalScore$RiskCategory[FinalScore$Risk_Score >= 8] = "High"
FinalScore$RiskCategory[(FinalScore$Risk_Score >= 4)&(FinalScore$Risk_Score < 8)] = "Medium"

hist(FinalScore$Risk_Score)

Method5_Risk = FinalScore[,c("ConsumerNumber","RiskCategory")]
names(Method5_Risk) = c("ConsumerNumber","Risk5")

return(Method5_Risk)
}
#write.csv(Method1_Risk,"OUTPUT//Method1_Risk.csv",row.names = FALSE)
# Compute the Event Scores
