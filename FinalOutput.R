getSuspiciousEventCounts<- function(df){
  SuspiciousEventData = transform(df, EVENTDATE = as.Date(EVENTDATE,"%Y-%m-%d"))
  
  # label the events
  SuspiciousEventData$EVENT_LABEL = ""
  cwv_idx = (SuspiciousEventData$CODE == 1)|(SuspiciousEventData$CODE == 2) |
    (SuspiciousEventData$CODE == 3)
  cts_idx = (SuspiciousEventData$CODE == 19)
  ctb_idx = (SuspiciousEventData$CODE == 1201)
  cto_idx = (SuspiciousEventData$CODE == 23)| (SuspiciousEventData$CODE == 24) |
    (SuspiciousEventData$CODE == 25) | (SuspiciousEventData$CODE == 26)
  cm_idx = (SuspiciousEventData$CODE == 30) | (SuspiciousEventData$CODE == 31) |
    (SuspiciousEventData$CODE == 32)
  vimb_idx = (SuspiciousEventData$CODE == 14)
  ipa_idx = (SuspiciousEventData$CODE == 18) | (SuspiciousEventData$CODE == 66)
  nd_idx = (SuspiciousEventData$CODE == 28)
  lon_idx = (SuspiciousEventData$CODE == 1208)
  mt_idx = (SuspiciousEventData$CODE == 27)
  co_idx = (SuspiciousEventData$CODE == 60)
  
  # Add labels
  SuspiciousEventData[cwv_idx,'EVENT_LABEL'] = "CURRENT_WITHOUT_VOLTAGE"
  SuspiciousEventData[cts_idx,'EVENT_LABEL'] = "CURRENT_TERMINAL_SHORT"
  SuspiciousEventData[ctb_idx,'EVENT_LABEL'] = "CURRENT_TERMINAL_BYPASS"
  SuspiciousEventData[cto_idx,'EVENT_LABEL'] = "CURRENT_TERMINAL_OPEN"
  SuspiciousEventData[cm_idx,'EVENT_LABEL'] = "CURRENT_MISSING"
  SuspiciousEventData[vimb_idx,'EVENT_LABEL'] = "VOLTAGE_IMBALANCE"
  SuspiciousEventData[ipa_idx,'EVENT_LABEL'] = "INVALID_VOLTAGE"
  SuspiciousEventData[nd_idx,'EVENT_LABEL'] = "NEUTRAL_DISTURBANCE"
  SuspiciousEventData[lon_idx,'EVENT_LABEL'] = "LOSS_OF_NEUTRAL"
  SuspiciousEventData[mt_idx,'EVENT_LABEL'] = "MAGNET_TAMPER"
  SuspiciousEventData[co_idx,'EVENT_LABEL'] = "COVER_OPEN"
  
  rm(cwv_idx,cts_idx,ctb_idx,cto_idx,cm_idx,vimb_idx,ipa_idx,nd_idx,lon_idx,mt_idx,co_idx)
  
  SuspiciousEventData = SuspiciousEventData[(SuspiciousEventData$EVENT_CLOSE == TRUE)&
                                              (SuspiciousEventData$DURATION > -1)&
                                              (SuspiciousEventData$STATUS == 0),]
  
  library(dplyr)
  library(tidyr)
  EventCounts = SuspiciousEventData %>%
    dplyr::group_by(ConsumerNumber,EVENT_LABEL) %>%
    dplyr::summarise(Count = n()) %>%
    tidyr::spread(EVENT_LABEL,Count)
  
  EventCounts[is.na(EventCounts)] = 0
  
  return(EventCounts)
}


#===================After preprocessing===================
MonthlyDataDF<-read.csv("Data//PROCESSED//MonthlyConsumptionData.csv", header = TRUE, stringsAsFactors = FALSE)
EventDF<-read.csv("Data//PROCESSED//SuspiciousEventDuration.csv", header = TRUE, stringsAsFactors = FALSE)

MeterConsumerNumberDF = read.csv("Data//PROCESSED//MeterMasterDF.csv", header = TRUE, stringsAsFactors = FALSE)
MeterConsumerNumberDF = MeterConsumerNumberDF[,c("ConsumerNumber","METERNO")]

source("Method2.R")
Risk2 = Method2("Data//PROCESSED//MeterMasterDF.csv",
                "Data//PROCESSED//MonthlyConsumptionData.csv",
                "Data//PROCESSED//SuspiciousEventDuration.csv")

source("Method3.R")
Risk3 = Method3(MonthlyDataDF,'01-06-2017',EventDF)

source("Method5.R")
Risk5 = Method5(MonthlyDataDF)

CustomerMaster<- read.csv("Data//PROCESSED//CustomerMasterDF.csv",header = TRUE, stringsAsFactors = FALSE)


CustomerMaster = merge(CustomerMaster,
                       Risk2,
                       by = "ConsumerNumber",
                       all.x = TRUE)
CustomerMaster = merge(CustomerMaster,
                       Risk3,
                       by = "ConsumerNumber",
                       all.x = TRUE)

CustomerMaster = merge(CustomerMaster,
                       Risk5,
                       by = "ConsumerNumber",
                       all.x = TRUE)

# Fill the NA values with Not Recommended
CustomerMaster[is.na(CustomerMaster$Risk2),"Risk2"] = "NoRecommendation"
CustomerMaster[is.na(CustomerMaster$Risk3),"Risk3"] = "NoRecommendation"
CustomerMaster[is.na(CustomerMaster$Risk5),"Risk5"] = "NoRecommendation"

# Compute the finale Risk based on 5 risk categories
# Load final Risk mapping files
#FinalRisk
# Load Geo-tags
GeoCodeDF = read.csv("Data//RAW//Customer_Geocoordinates.csv", header = TRUE,
                     stringsAsFactors = FALSE)
CustomerMaster = merge(CustomerMaster,
                       GeoCodeDF[,c("ConsumerNumber","Lat","Long")],
                       by="ConsumerNumber",
                       all.x = TRUE)

Rules = read.csv("Data//PROCESSED//Rules.csv", header = TRUE,
                 stringsAsFactors = FALSE)

CustomerMaster = merge(CustomerMaster,
                        Rules,
                        by = c("Risk2","Risk3","Risk5"),
                        all.x = TRUE)
# 
# # Load the feedback file
# # Feedback Codes
# # 0. Not inspected
# # 1. Not Flag for next 6 months
# # 2. Flase positive
# # 3. Continue to flag
# # 4. Action needed from NPCL
# # 5. TD Done. Donot Flag in future.
FeedbackDF = read.csv("Data//PROCESSED//feedback.csv", header = TRUE,
                       stringsAsFactors = FALSE)
FeedbackDF = transform(FeedbackDF,
                       InspectionDate = as.Date(InspectionDate,'%d-%m-%Y'))
TodayDate = as.Date(format(Sys.Date(),'%d-%m-%Y'),'%d-%m-%Y')
# 
# # Out of the final recommended from high cases, remove the cases other than 0
# 
CustomerMaster = merge(CustomerMaster,
       FeedbackDF,
       by = "ConsumerNumber",
       all.x = TRUE)

 
MonthlyDataDF = transform(MonthlyDataDF, MoveInDate = as.Date(MoveInDate, '%Y-%m-%d'),
                        MonthofConsumption = as.Date(MonthofConsumption, '%Y-%m-%d'))
library(dplyr)
library(tidyr)
ConsumptionStats = MonthlyDataDF %>%
  dplyr::mutate(Year = year(MonthofConsumption)) %>%
  dplyr::filter(Year >= 2015) %>%
  dplyr::group_by(ConsumerNumber,Year)%>%
  dplyr::summarise(Avg = mean(Consumption_kWh)) %>%
  tidyr::spread(Year,Avg)

ConsumptionStats[is.na(ConsumptionStats)] = 0
names(ConsumptionStats) = c("ConsumerNumber","Average_2015","Average_2016","Average_2017")

ZeroConsumption = MonthlyDataDF %>%
  dplyr::group_by(ConsumerNumber)%>%
  dplyr::summarise(ZeroConsumptionMonths = sum(Consumption_kWh == 0))

CustomerMaster = merge(CustomerMaster,
                          ConsumptionStats,
                          by="ConsumerNumber",
                          all.x = TRUE)
CustomerMaster = merge(CustomerMaster,
                          ZeroConsumption,
                          by="ConsumerNumber",
                          all.x = TRUE)


# Get Event Counts

EventCountsDF = getSuspiciousEventCounts(EventDF)
n1 = ncol(CustomerMaster)
CustomerMaster = merge(CustomerMaster,
                       EventCountsDF,
                       by = "ConsumerNumber",
                       all.x = TRUE)
n2 = ncol(CustomerMaster)
events = CustomerMaster[,c(seq(n1+1,n2,1))]
events[is.na(events)] = 0
CustomerMaster[,c(seq(n1+1,n2,1))] = events
rm(n1,n2,events)
# Only Recommend which are yet to be inspected or it's threshold crosses the time of 6 months
idx = (CustomerMaster$FinalRisk == "High") & 
  ((CustomerMaster$FeedbackCode == 0) |
     ((CustomerMaster$FeedbackCode == 1) & as.numeric(TodayDate - CustomerMaster$InspectionDate)>180))
HighRiskCustomers = CustomerMaster[idx,]
rm(idx)
# Convert 
# Sort the high risk consumers by numbers of zero consumption days
HighRiskCustomers = HighRiskCustomers[ order(-HighRiskCustomers[,"ZeroConsumptionMonths"]), ]
cols = c("ConsumerNumber","Name","StreetType","ConsumerType","RateCategory",
         "Load","LoadUoM","MoveInDate","Risk2","Risk3","Risk5",
         "FinalRisk")

FinalOutput = CustomerMaster[,c("ConsumerNumber","Name","StreetType","ConsumerType","RateCategory",
  "Load","LoadUoM","MoveInDate","Average_2015","Average_2016","Average_2017",
  "CURRENT_MISSING","CURRENT_TERMINAL_BYPASS","CURRENT_TERMINAL_OPEN",
  "CURRENT_TERMINAL_SHORT","CURRENT_WITHOUT_VOLTAGE","INVALID_VOLTAGE",
  "LOSS_OF_NEUTRAL","MAGNET_TAMPER","NEUTRAL_DISTURBANCE","VOLTAGE_IMBALANCE",
  "ZeroConsumptionMonths","Risk2","Risk3","Risk5","FinalRisk","InspectionDate",
  "FeedbackComment")]

require(XLConnect)
wb = loadWorkbook("Data\\RAW\\CustomerMasterData.xlsx")
CustomerInfoDf = readWorksheet(wb, sheet = "Sheet1", header = TRUE)

# Get the top list of n customers for recommendation
n = 50
cols = c("Consumer.No.","Name","Care.Of","Rate.Category","Street","House.No.",
         "House.No.","Tel.No.","Mobile.No")
CustomerInfoDf = CustomerInfoDf[,cols]
names(CustomerInfoDf)[names(CustomerInfoDf) == 'Consumer.No.'] <- 'ConsumerNumber'

HighRiskCustomers = HighRiskCustomers[,c("ConsumerNumber","ConsumerType","StreetType","Lat","Long")]

# Select the high risk customers


CustomerInfoDf = merge(CustomerInfoDf,
                       HighRiskCustomers,
                       by=c("ConsumerNumber"))
cols = c("ConsumerNumber","Name","Care.Of","Rate.Category","ConsumerType","StreetType",
         "Street","House.No.","House.No.","Tel.No.","Mobile.No")

RecommendationFile = CustomerInfoDf[,cols]

write.csv(FinalOutput,"Data\\OUTPUT\\AllConsumerRisk.csv",row.names = FALSE)
write.csv(RecommendationFile,"Data\\OUTPUT\\RecommendationFile.csv", row.names = FALSE)

# First take the top columns with RU
# CustomerMaster = CustomerMaster[,cols]
# 
# # Take only the High RIsk Customers
# RecommendFile = CustomerMaster[CustomerMaster$FinalRisk=="High",]
# RecommendFile = RecommendFile[!is.na(RecommendFile$FinalRisk),]
# Recommend in order

