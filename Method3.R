Method3 <- function(MonthlyData,LastMonth='01-06-2017',EventData){
# File Paths
library(tidyr)
library(reshape2)
library(dplyr)

  EventData = transform(EventData, EVENTDATE = as.Date(EVENTDATE,"%Y-%m-%d"))
  
  # Take only the events with Close and Duration greater than 0.5 hours
  select_idx = (EventData$EVENT_CLOSE == TRUE) & (EventData$DURATION > 0.5) & (EventData$STATUS == 0)&
    (EventData$EVENTDATE >= as.Date('2015-01-01','%Y-%m-%d'))
  EventData = EventData[select_idx,]
  
  select_cols = c("ConsumerNumber","EVENTDATE","CODE","DURATION")
  EventData = EventData[,select_cols]
  
  # Generate the year from date in EventData
  library(lubridate)
  EventData$Year = year(EventData$EVENTDATE)
  
  library(dplyr)
  EventCounts = EventData %>%
    dplyr::group_by(ConsumerNumber,Year) %>%
    dplyr::summarise(TotalCount = n())
  
  # Compute the percentage of events every year
  library(tidyr)
  EventCountsMatrix = EventCounts %>%
    tidyr::spread(Year,TotalCount)
  EventCountsMatrix[is.na(EventCountsMatrix)] = 0
  
  # Compute the percentage of events every year
  library(reshape2)
  EventCounts = EventCountsMatrix %>%
    reshape2::melt(id = c("ConsumerNumber"),value.name="EventCounts")
  names(EventCounts)[names(EventCounts) == 'variable'] <- 'Year'
  
  EventCounts=EventCounts %>%
    dplyr::group_by(ConsumerNumber) %>%
    dplyr::mutate(countT= sum(EventCounts)) %>%
    dplyr::group_by(Year, add=TRUE) %>%
    dplyr::mutate(Percentage=round(100*EventCounts/countT,2))
  
  # 
  EventCounts$Adjusted = EventCounts$Percentage
  EventCounts$Adjusted[EventCounts$Year==2017] = (12/12)*EventCounts$Adjusted[EventCounts$Year==2017]
  
  #
  EventCounts$AdjustedScores = 0
  EventCounts$AdjustedScores[EventCounts$Year == 2015] = 0.2 * EventCounts$Adjusted[EventCounts$Year == 2015]
  EventCounts$AdjustedScores[EventCounts$Year == 2016] = 0.3 * EventCounts$Adjusted[EventCounts$Year == 2016]
  EventCounts$AdjustedScores[EventCounts$Year == 2017] = 0.5 * EventCounts$Adjusted[EventCounts$Year == 2017]
  
  # Compute the combined scores
  EventScores = EventCounts %>%
    dplyr::group_by(ConsumerNumber) %>%
    dplyr::summarise(Score = 100*sum(AdjustedScores)/sum(Adjusted))
  
  EventScores$Score = 100 *(EventScores$Score-min(EventScores$Score))/(max(EventScores$Score)-min(EventScores$Score))
  
  EventCounts = merge(EventScores,
                      EventCounts,
                      by = "ConsumerNumber",
                      all.x = TRUE)
  
# SuspiciousEventData = transform(SuspiciousEventData, EVENTDATE = as.Date(EVENTDATE,"%Y-%m-%d"))
# 
# # label the events
# SuspiciousEventData$EVENT_LABEL = ""
# cwv_idx = (SuspiciousEventData$CODE == 1)|(SuspiciousEventData$CODE == 2) |
#   (SuspiciousEventData$CODE == 3)
# cts_idx = (SuspiciousEventData$CODE == 19)
# ctb_idx = (SuspiciousEventData$CODE == 1201)
# cto_idx = (SuspiciousEventData$CODE == 23)| (SuspiciousEventData$CODE == 24) |
#   (SuspiciousEventData$CODE == 25) | (SuspiciousEventData$CODE == 26)
# cm_idx = (SuspiciousEventData$CODE == 30) | (SuspiciousEventData$CODE == 31) |
#   (SuspiciousEventData$CODE == 32)
# vimb_idx = (SuspiciousEventData$CODE == 14)
# ipa_idx = (SuspiciousEventData$CODE == 18) | (SuspiciousEventData$CODE == 66)
# nd_idx = (SuspiciousEventData$CODE == 28)
# lon_idx = (SuspiciousEventData$CODE == 1208)
# mt_idx = (SuspiciousEventData$CODE == 27)
# co_idx = (SuspiciousEventData$CODE == 60)
# 
# # Add labels
# SuspiciousEventData[cwv_idx,'EVENT_LABEL'] = "CURRENT_WITHOUT_VOLTAGE"
# SuspiciousEventData[cts_idx,'EVENT_LABEL'] = "CURRENT_TERMINAL_SHORT"
# SuspiciousEventData[ctb_idx,'EVENT_LABEL'] = "CURRENT_TERMINAL_BYPASS"
# SuspiciousEventData[cto_idx,'EVENT_LABEL'] = "CURRENT_TERMINAL_OPEN"
# SuspiciousEventData[cm_idx,'EVENT_LABEL'] = "CURRENT_MISSING"
# SuspiciousEventData[vimb_idx,'EVENT_LABEL'] = "VOLTAGE_IMBALANCE"
# SuspiciousEventData[ipa_idx,'EVENT_LABEL'] = "INVALID_VOLTAGE"
# SuspiciousEventData[nd_idx,'EVENT_LABEL'] = "NEUTRAL_DISTURBANCE"
# SuspiciousEventData[lon_idx,'EVENT_LABEL'] = "LOSS_OF_NEUTRAL"
# SuspiciousEventData[mt_idx,'EVENT_LABEL'] = "MAGNET_TAMPER"
# SuspiciousEventData[co_idx,'EVENT_LABEL'] = "COVER_OPEN"
# 
# rm(cwv_idx,cts_idx,ctb_idx,cto_idx,cm_idx,
#    vimb_idx,ipa_idx,nd_idx,lon_idx,mt_idx,co_idx)
# SuspiciousEventData = SuspiciousEventData[(SuspiciousEventData$EVENT_CLOSE == TRUE)&
#                                             (SuspiciousEventData$DURATION > 0.5)&
#                                             (SuspiciousEventData$STATUS == 0)&
#                                             (EventData$EVENTDATE >= as.Date('2015-01-01','%Y-%m-%d')),]
# 
# EventCounts = SuspiciousEventData %>%
#   dplyr::group_by(ConsumerNumber,EVENT_LABEL) %>%
#   dplyr::summarise(Count = n()) %>%
#   tidyr::spread(EVENT_LABEL,Count)

#getSeasonalityIndex <- function()
MonthlyData = transform(MonthlyData, MoveInDate = as.Date(MoveInDate, '%Y-%m-%d'),
                          MonthofConsumption = as.Date(MonthofConsumption, '%Y-%m-%d'))

# Take the consumers with MoveIn Date before June 20
library(lubridate)
MonthlyData$MonthIndex = month(MonthlyData$MonthofConsumption)

# Take only two years data
# From June,2015 to June, 2016
MonthlyData = MonthlyData[MonthlyData$MonthofConsumption >= as.Date('01-06-2015',"%d-%m-%Y") & 
                            (MonthlyData$MonthofConsumption <= as.Date(LastMonth,"%d-%m-%Y")),]

select_cols = c("ConsumerNumber","ConsumerType","MonthofConsumption",
                "Consumption_kWh")

library(tidyr)
MonthlyDataMatrix = MonthlyData[,select_cols] %>%
  tidyr::spread(MonthofConsumption,Consumption_kWh)

MonthlyDataMatrix = MonthlyDataMatrix[!(rowSums(is.na(MonthlyDataMatrix)) > 0),]

library(reshape2)
MonthlyData1 = MonthlyDataMatrix %>%
  reshape2::melt(id = c("ConsumerNumber","ConsumerType"), value.name = "Consumption_kWh")

names(MonthlyData1)[names(MonthlyData1) == 'variable'] <- 'Month'
MonthlyData1$Month = as.character(MonthlyData1$Month)
MonthlyData1 = transform(MonthlyData1, Month = as.Date(Month, '%Y-%m-%d'))

MonthlyData1$MonthIndex = month(MonthlyData1$Month)

# Compute the seasonality index
Total = MonthlyData1 %>%
  dplyr::group_by(ConsumerType) %>%
  dplyr::summarise(YearlyTotal = sum(Consumption_kWh))

MonthlyData1 = MonthlyData1 %>%
  dplyr::group_by(ConsumerType, MonthIndex) %>%
  dplyr::summarise(TotalConsumption = sum(Consumption_kWh))

# Compute number of days in a month
days = c(31,29,31,30,31,30,31,31,30,31,30,31)
MonthIndex = c(seq(1,12,1))
DaysInMonth = data.frame(MonthIndex,days)

MonthlyData1 = merge(MonthlyData1,
                     DaysInMonth,
                     by = "MonthIndex",
                     all.x = TRUE)
MonthlyData1 = merge(MonthlyData1,
                     Total,
                     by="ConsumerType",
                     all.x = TRUE)

MonthlyData1$Expected = MonthlyData1$days*MonthlyData1$YearlyTotal/366
MonthlyData1$SeasonalityIndex = MonthlyData1$TotalConsumption/MonthlyData1$Expected

# Take only Consumer Type and Seasonality Index
MonthlyData = merge(MonthlyData,
                    MonthlyData1[,c("ConsumerType","MonthIndex","SeasonalityIndex")],
                    by = c("ConsumerType","MonthIndex"),
                    all.x = TRUE)

MonthlyData$DeseasConsumption_kWh = MonthlyData$Consumption_kWh/MonthlyData$SeasonalityIndex

# Take only the conusmers with movein Date before June 2016
# and remove the consumers with SmallIndustries
MonthlyData = MonthlyData[MonthlyData$MoveInDate <= as.Date('2016-06-01',"%Y-%m-%d"),]
MonthlyData = MonthlyData[MonthlyData$ConsumerType != "SmallIndustries",]

# Compute the monthly average consumption in kWH
MonthlyAvgConsumption = MonthlyData %>%
  dplyr::group_by(ConsumerNumber,ConsumerType) %>%
  dplyr::summarise(AvgConsumption = sum(DeseasConsumption_kWh)/n())


# For Each consumer Type Compute the Q1,Q2,Q3
ConsumptionQuartiles = MonthlyAvgConsumption %>%
  dplyr::group_by(ConsumerType) %>%
  dplyr::summarise(Q1 = quantile(AvgConsumption,probs = c(0.25)),
            Q2 = quantile(AvgConsumption,probs = c(0.50)),
            Q3 = quantile(AvgConsumption,probs = c(0.75)))

MonthlyAvgConsumption = merge(MonthlyAvgConsumption,
                              ConsumptionQuartiles,
                              by = c("ConsumerType"),
                              all.x = TRUE)

MonthlyAvgConsumption$CSlabs = ""
# Slab1 , 2, 3, 4 indexs
slab1_idx = MonthlyAvgConsumption$AvgConsumption <= MonthlyAvgConsumption$Q1
slab2_idx = (MonthlyAvgConsumption$AvgConsumption > MonthlyAvgConsumption$Q1) & 
  (MonthlyAvgConsumption$AvgConsumption <= MonthlyAvgConsumption$Q2)
slab3_idx = (MonthlyAvgConsumption$AvgConsumption > MonthlyAvgConsumption$Q2) &
  (MonthlyAvgConsumption$AvgConsumption <= MonthlyAvgConsumption$Q3)
slab4_idx = (MonthlyAvgConsumption$AvgConsumption > MonthlyAvgConsumption$Q3)

MonthlyAvgConsumption$CSlabs[slab1_idx] = "Slab1"
MonthlyAvgConsumption$CSlabs[slab2_idx] = "Slab2"
MonthlyAvgConsumption$CSlabs[slab3_idx] = "Slab3"
MonthlyAvgConsumption$CSlabs[slab4_idx] = "Slab4"

MonthlyData = merge(MonthlyData,
                    MonthlyAvgConsumption[,c("ConsumerNumber","CSlabs")],
                    by="ConsumerNumber",
                    all.x = TRUE)

#Method1_Risk = read.csv("OUTPUT//Method1_Risk.csv", header = TRUE, stringsAsFactors = FALSE)

# Find the median consumption within each slab
MonthlyData = MonthlyData %>%
  dplyr::group_by(ConsumerType,CSlabs,MonthofConsumption) %>%
  dplyr::mutate(MedianSlabConsumption = median(DeseasConsumption_kWh))

# Compute correlation between median and deseasonlised consumption
ConsumptionStats = MonthlyData %>%
  dplyr::group_by(ConsumerNumber)%>%
  dplyr::summarise(Corr = cor(DeseasConsumption_kWh,MedianSlabConsumption),
            CoVar = sd(DeseasConsumption_kWh)/mean(DeseasConsumption_kWh),
            NoOfZeros = sum(DeseasConsumption_kWh == 0))
ConsumptionStats[is.na(ConsumptionStats)] = 0

# Compute the YoY growth w.r.t last year

YoY_Metric = MonthlyData %>%
  dplyr::mutate(Year = year(MonthofConsumption))%>%
  dplyr::group_by(ConsumerNumber,Year) %>%
  dplyr::summarise(AvgConsumption = mean(Consumption_kWh)) %>%
  tidyr::spread(Year,AvgConsumption)%>%
  dplyr::mutate(YoY = 100*(`2017`-`2016`)/`2016`)%>%
  dplyr::select(ConsumerNumber,YoY)

YoY_Metric[is.infinite(YoY_Metric$YoY),"YoY"] = 1
YoY_Metric[is.na(YoY_Metric$YoY),"YoY"] = 0

ConsumptionStats = merge(ConsumptionStats,
                         YoY_Metric,
                         by="ConsumerNumber",
                         all.x = TRUE)

# Compute the monthly quartiles for each month in each slab
MonthlyData = MonthlyData %>%
  dplyr::group_by(ConsumerType,CSlabs,MonthofConsumption) %>%
  dplyr::mutate(Q1 = quantile(DeseasConsumption_kWh, probs = c(0.25)),
         Q3 = quantile(DeseasConsumption_kWh, probs = c(0.75)))

# Compute the number of times the consumption goes below Q1 and above Q2
MonthlyData$OutsideQ1Q3 = 0
MonthlyData$OutsideQ1Q3[(MonthlyData$DeseasConsumption_kWh < MonthlyData$Q1) | 
                          (MonthlyData$DeseasConsumption_kWh > MonthlyData$Q3)] = 1


# Count the number of months for which consumption is outside Q1 & Q3
Q1Q3_Count = MonthlyData %>%
  dplyr::group_by(ConsumerNumber,ConsumerType) %>%
  dplyr::summarise(OutsideQ1Q3_Count = sum(OutsideQ1Q3))

# Now merge with consumption stats data-set
ConsumptionStats = merge(ConsumptionStats,
                         Q1Q3_Count,
                         by = "ConsumerNumber",
                         all.x = TRUE)

# Perform Min-max normalisation on ConsumptionStats
Normalised_FeatureValues = ConsumptionStats %>%
  dplyr::group_by(ConsumerType) %>%
  dplyr::summarise(Min_Corr = min(Corr),
                   Max_Corr = max(Corr),
                   Min_CoVar = min(CoVar),
                   Max_CoVar = max(CoVar),
                   Min_NoOfZeros = min(NoOfZeros),
                   Max_NoOfZeros = max(NoOfZeros),
                   Min_YoY = min(YoY),
                   Max_YoY = max(YoY),
                   Min_OutsideQ1Q3_Count = min(OutsideQ1Q3_Count),
                   Max_OutsideQ1Q3_Count = max(OutsideQ1Q3_Count))

NormFeatures = merge(ConsumptionStats,
                         Normalised_FeatureValues,
                         by = "ConsumerType",
                         all.x = TRUE) %>%
  dplyr::mutate(Norm_Corr = 100*(Corr-Min_Corr)/(Max_Corr - Min_Corr),
                Norm_CoVar = 100*(CoVar-Min_CoVar)/(Max_CoVar - Min_CoVar),
                Norm_NoOfZeros = 100*(NoOfZeros-Min_NoOfZeros)/(Max_NoOfZeros-Min_NoOfZeros),
                Norm_YoY = 100 * (YoY - Min_YoY)/(Max_YoY - Min_YoY),
                Norm_OutsideQ1Q3_Count = 100*(OutsideQ1Q3_Count)/(Max_OutsideQ1Q3_Count-Min_OutsideQ1Q3_Count)) %>%
  dplyr::select(ConsumerNumber,ConsumerType,
                Norm_Corr, Norm_CoVar, Norm_NoOfZeros,Norm_YoY,Norm_OutsideQ1Q3_Count)

NormFeatures$Score = NormFeatures$Norm_NoOfZeros + 
  (NormFeatures$Norm_CoVar/(NormFeatures$Norm_Corr+1))+
  (1/(NormFeatures$Norm_YoY+1))+
  NormFeatures$Norm_OutsideQ1Q3_Count

NormFeatures = NormFeatures %>%
  dplyr::group_by(ConsumerType) %>%
  dplyr::mutate(Norm_Score = 100*(Score - min(Score))/(max(Score)-min(Score)))

NormFeatures$Risk3 = "Low"
NormFeatures$Risk3[NormFeatures$Norm_Score > 25] = "Medium"
NormFeatures$Risk3[NormFeatures$Norm_Score > 50] = "High"

Method3_Risk = NormFeatures[,c("ConsumerNumber","Risk3")]

return(Method3_Risk)
#write.csv(NormFeatures,"NormFeatures.csv",row.names = FALSE)
}
# Create Final bins

###################################################################################
###################################################################################

# # Create ranks for each stats
# RankMetric = ConsumptionStats %>%
#   dplyr::group_by(ConsumerType) %>%
#   dplyr::mutate(Corr_Rank = dense_rank(Corr),
#                 CoVar_Rank = dense_rank(desc(CoVar)),
#                 NoOfZeros_Rank = dense_rank(desc(NoOfZeros)),
#                 OutsideQ1Q3_Rank = dense_rank(desc(OutsideQ1Q3_Count)),
#                 YoY_Rank = dense_rank(YoY))
# 
# # Compute the coefficient of variance in each category
# Coef_var = ConsumptionStats %>%
#   dplyr::group_by(ConsumerType) %>%
#   dplyr::summarise(var_corr = sd(Corr)/mean(Corr),
#                    var_CoVar = sd(CoVar)/mean(CoVar),
#                    var_Zeros = sd(NoOfZeros)/mean(NoOfZeros),
#                    var_Q1Q3 = sd(OutsideQ1Q3_Count)/mean(OutsideQ1Q3_Count))
# 
# Coef_var$Cor_wt = 0.5*Coef_var$var_corr/(Coef_var$var_corr+Coef_var$var_CoVar+Coef_var$var_Zeros+
#                                        Coef_var$var_Q1Q3)
# Coef_var$CoVar_wt = 0.5*Coef_var$var_CoVar/(Coef_var$var_corr+Coef_var$var_CoVar+Coef_var$var_Zeros+
#                                            Coef_var$var_Q1Q3)
# Coef_var$Zeros_wt = 0.5*Coef_var$var_Zeros/(Coef_var$var_corr+Coef_var$var_CoVar+Coef_var$var_Zeros+
#                                            Coef_var$var_Q1Q3)
# Coef_var$Q1Q3_wt = 0.5*Coef_var$var_Q1Q3/(Coef_var$var_corr+Coef_var$var_CoVar+Coef_var$var_Zeros+
#                                            Coef_var$var_Q1Q3)
# 
# # Merge the weights with Rank Metric and compute the final score
# Final_Score = merge(RankMetric[,c("ConsumerNumber","ConsumerType","Corr_Rank",
#                                   "CoVar_Rank","NoOfZeros_Rank","OutsideQ1Q3_Rank","YoY_Rank")],
#                     Coef_var[,c("ConsumerType","Cor_wt","CoVar_wt","Zeros_wt","Q1Q3_wt")],
#                     by = "ConsumerType",
#                     all.x = TRUE)
# 
# Final_Score$Score = Final_Score$Corr_Rank * Final_Score$Cor_wt +
#   Final_Score$CoVar_Rank * Final_Score$CoVar_wt +
#   Final_Score$NoOfZeros_Rank * Final_Score$Zeros_wt +
#   Final_Score$OutsideQ1Q3_Rank * Final_Score$Q1Q3_wt +
#   Final_Score$YoY_Rank * 0.2
