######################################################################################################
# Import Libraries
######################################################################################################
library(dplyr)
library(tidyr)
library(ggplot2)
# For year mon field
library(zoo)
# For extracting date compnents from Date format
library(lubridate)
# For fomating labels on axis
library(scales)
# Package for Gaussian Mixture Clustering
require(mclust)
# For time variables
require(chron)
# For loading BIRCH Clustering
library(MASS)
######################################################################################################
# Function for computing Average Consumption Per day from consumption data
######################################################################################################
getPerDayConsumption <- function(df){
  # compute the unique df
  #df = unique(df)
  df = transform(df,
                 MonthofConsumption = as.Date(MonthofConsumption, '%Y-%m-%d'),
                 ConsumptionFrom = as.Date(ConsumptionFrom,'%Y-%m-%d'),
                 ConsumptionTo = as.Date(ConsumptionTo,'%Y-%m-%d'),
                 RateCategory = as.factor(RateCategory))
  # Load library plyr
  library(plyr)
  groupColumns = c("ConsumerNumber","ConsumptionFrom","ConsumptionTo")
  dataColumns = c("ISU_Consumption_KVAH","ISU_Consumption_KWH","Consumption_TOD1",
                  "Consumption_TOD2","Consumption_TOD3")
  
  # Aggregate over Consumption
  temp2 = ddply(df, groupColumns, function(x) colSums(x[dataColumns]))
  # Aggregate over Demand
  groupColumns = c("ConsumerNumber","ConsumptionFrom","ConsumptionTo","UOM")
  dataColumns = c("BillableDemandConsumption","PenalDemand","RecordedDemand")
  
  
  temp2 = transform(temp2, ConsumptionFrom = as.Date(ConsumptionFrom,'%Y-%m-%d'),
                    ConsumptionTo = as.Date(ConsumptionTo,'%Y-%m-%d'))
  temp2$DaysofConsumption = as.numeric(temp2$ConsumptionTo - temp2$ConsumptionFrom)+1
  temp2$PerDayConsumption_KVAH = temp2$ISU_Consumption_KVAH/temp2$DaysofConsumption
  temp2$PerDayConsumption_KWH = temp2$ISU_Consumption_KWH/temp2$DaysofConsumption
  temp2$PerDayConsumption_TOD1 = temp2$Consumption_TOD1/temp2$DaysofConsumption
  temp2$PerDayConsumption_TOD2 = temp2$Consumption_TOD2/temp2$DaysofConsumption
  temp2$PerDayConsumption_TOD3 = temp2$Consumption_TOD3/temp2$DaysofConsumption
  #temp2 = temp2[temp2$ConsumptionFrom >= startdate,]
  
  first = TRUE
  for(i in 1:nrow(temp2)){
    print(i)
    Ndays = temp2$DaysofConsumption[i]
    ConsumerNumber = c(rep(temp2$ConsumerNumber[i],Ndays))
    ConsumptionDate = c(seq(temp2$ConsumptionFrom[i],temp2$ConsumptionTo[i],"day"))
    consumption_KVAH = c(rep(temp2$PerDayConsumption_KVAH[i],Ndays))
    consumption_KWH = c(rep(temp2$PerDayConsumption_KWH[i],Ndays))
    consumption_TOD1 = c(rep(temp2$PerDayConsumption_TOD1[i],Ndays))
    consumption_TOD2 = c(rep(temp2$PerDayConsumption_TOD2[i],Ndays))
    consumption_TOD3 = c(rep(temp2$PerDayConsumption_TOD3[i],Ndays))
    
    tempdf = data.frame(ConsumerNumber,ConsumptionDate,consumption_KVAH,consumption_KWH,
                        consumption_TOD1,consumption_TOD2,consumption_TOD3)
    
    if(first == TRUE){
      first = FALSE
      DayWiseConsumption = tempdf
    }else{
      DayWiseConsumption = rbind(DayWiseConsumption,tempdf)
    } 
    
  }
  rm(groupColumns,dataColumns,tempdf,Ndays,ConsumerNumber,ConsumptionDate,consumption_KVAH,
     consumption_KWH,consumption_TOD1,consumption_TOD2,consumption_TOD3,first)
  return(DayWiseConsumption)
}

checkForNAColumns <- function(df){
  named_cols = colSums(is.na(df))/nrow(df)
  for(i in 1:length(named_cols)){
    if(named_cols[i] != 0){
      print(named_cols[i])
    }
  }
  rm(named_cols,i)
}

getEventDurations <- function(df, meterconsumerdf){
  df$EVENTDATE = gsub('\\s+','',df$EVENTDATE)
  df$EVENTTIME = gsub('\\s+','',df$EVENTTIME)
  
  df = transform(df,
                 DATETIMESTAMP = strptime(DATETIMESTAMP, "%Y-%m-%d %H:%M:%S"),
                 EVENTDATE = as.Date(EVENTDATE, "%d-%b-%Y"))
  
  df = merge(df,
             meterconsumerdf,
             by = "METERNO", all.x = TRUE)
  
  # Compute the suspicious event duration
  error_idx = df$EVENTTIME == "00:00AM"
  
  df = df[!error_idx,]
  df = df[with(df,
               order(df[,"ConsumerNumber"],
                     df[,"METERNO"],
                     df[,"CODE"],
                     df[,"DATETIMESTAMP"])),]
  
  df$EVENT_CLOSE = FALSE
  df$DURATION = -1
  
  last_event_code = 0
  last_cnumber = 0
  last_event_status = -1
  last_event_ts = 0
  RESET_FLAG = FALSE
  for(i in 1:nrow(df)){
    print(i)
    if((i == 1) |(RESET_FLAG == TRUE)){
      # First Event Occured or Reset Flag is set
      last_idx = i
      last_event_code = df$CODE[i]
      last_cnumber = df$ConsumerNumber[i]
      last_meternumber = df$METERNO[i]
      last_event_status = df$STATUS[i]
      last_event_ts = df$DATETIMESTAMP[i]
      RESET_FLAG = FALSE
    }else{
      # After First Event row
      current_event_code = df$CODE[i]
      current_cnumber = df$ConsumerNumber[i]
      current_meternumber = df$METERNO[i]
      current_event_status = df$STATUS[i]
      current_event_ts = df$DATETIMESTAMP[i]
      
      if(current_cnumber == last_cnumber){
        # If event for same consumer, then next check for same meter number
        if(current_meternumber == last_meternumber){
          # If the next event is from the same meter, then check for event code
          if(current_event_code == last_event_code){
            # If same event is occured again , check for status code
            if(current_event_status != last_event_status){
              # If the event status is changed, i.e Occur to Restore / Restore to Occur
              if(current_event_status == 1){
                # If the previous event is restored, compute the time duration in 
                # Hours and set Event Close Flag for that event
                event_duration = as.numeric(difftime(current_event_ts,
                                                     last_event_ts,
                                                     units = "hours"))
                # Check for debugging
                if(event_duration < -2){
                  print(last_idx)
                  print(last_event_ts)
                  print(current_event_ts)
                  return(df)
                }
                #####################
                df$EVENT_CLOSE[last_idx] = TRUE
                df$DURATION[last_idx] = event_duration
                df$EVENT_CLOSE[i] = TRUE
                df$DURATION[i] = event_duration
                # Set Reset Flag ON and jump to next iteration
                RESET_FLAG = TRUE
                next
                # End of Event Status check
              }else{
                # Occur event received after Restore for same event code
                # Then ignore the last Restore and start monitoring current event code
                last_event_status = current_event_status
                last_event_code = current_event_code
                last_event_ts = current_event_ts
                last_idx = i
              } # End of change in event status check
            }else{
              # Restore / Occur event are receiving repeatidly , then ignore all except first
              # Set the Duration to -2 , to show that the events are repeatitive
              df$DURATION[i] = -2
              next
            }
          }else{ # End of event code check
            # If the another event occurs for same consumer
            # Then update the event monitoring parameters
            last_event_code = current_event_code
            last_event_status = current_event_status
            last_event_ts = current_event_ts
            last_idx = i
          }
        }else{ # End of Meter number check
          # If meter number changes for same consumer, then change start
          # monitoring the new event
          last_meternumber = current_meternumber
          last_event_code = current_event_code
          last_event_status = current_event_status
          last_event_ts = current_event_ts
          last_idx = i
        }
      }else{ # end of consumer number check
        # If consumer number changes then start monitoring current event
        last_meternumber = current_meternumber
        last_cnumber = current_cnumber
        last_event_code = current_event_code
        last_event_status = current_event_status
        last_event_ts = current_event_ts
        last_idx = i
      }
    }
  }
  return(df)
}

# Compute the monthly recorded peak demand
getMonthlyPeakDemand <- function(df){
  # Remove the duplicate entries
  #df = unique(df)
  
  # load library plyr
  library(plyr)
  
  # Add the duration field to the consumption data
  df$Duration = paste(df$ConsumptionFrom,df$ConsumptionTo,sep = " to ")
  
  # Covert all demand into KWH
  bhp_idx = df$UOM == "BHP"
  kva_idx = df$UOM == "KVA"
  kw_idx = df$UOM == "KW"
  
  df$RecordedDemand_KW = 0
  df$RecordedDemand_KW[bhp_idx] = 0.745699872 *
    df$BillableDemandConsumption[bhp_idx]
  df$RecordedDemand_KW[kva_idx] = 0.9 * df$BillableDemandConsumption[kva_idx]
  df$RecordedDemand_KW[kw_idx] = df$BillableDemandConsumption[kw_idx]
  rm(bhp_idx,kva_idx,kw_idx)
  
  # Group by consumer number and then by duration and compute the sum of
  # all the BillableDemandConsumption
  library(dplyr)
  PeakDemandDF = df %>%
    group_by(ConsumerNumber,Duration) %>%
    summarise(RecordedDemand_KW = sum(RecordedDemand_KW))
  
  # Now split the duration into startdate and last date
  library(stringr)
  PeakDemandDF$StartDate = str_split_fixed(PeakDemandDF$Duration," to ",2)[,1]
  PeakDemandDF$EndDate = str_split_fixed(PeakDemandDF$Duration," to ",2)[,2]
  
  # Convert the start Date and End date into date format
  PeakDemandDF = transform(PeakDemandDF,
                           StartDate = as.Date(StartDate,'%Y-%m-%d'),
                           EndDate = as.Date(EndDate,'%Y-%m-%d'))
  first = TRUE
  library(zoo)
  # Now generate new df with monthwise recorded demand
  for(i in 1:nrow(PeakDemandDF)){
    Month = c(as.yearmon(PeakDemandDF$StartDate[i],PeakDemandDF$EndDate[i],
                         "month"))
    n = length(Month)
    ConsumerNumber = c(rep(PeakDemandDF$ConsumerNumber[i],n))
    BillableDemand_KW = c(rep(PeakDemandDF$RecordedDemand_KW[i],n))
    
    tempDF = data.frame(ConsumerNumber,Month,BillableDemand_KW)
    if(first == TRUE){
      MonthlyBilledDemandDF = tempDF
      first = FALSE
    }else{
      MonthlyBilledDemandDF = rbind(MonthlyBilledDemandDF,tempDF)
    }
  }
  
  # Compute the max recorded demand for a month
  MonthlyBilledDemandDF = MonthlyBilledDemandDF %>%
    group_by(ConsumerNumber,Month)%>%
    summarise(BillableDemand_KW = max(BillableDemand_KW))
  
  MonthlyBilledDemandDF$MonthofConsumption = as.Date(MonthlyBilledDemandDF$Month)
  
  rm(first,i,tempDF,ConsumerNumber,n,BillableDemand_KW,Month)
  return(MonthlyBilledDemandDF)
}

getMonthlySanctionedLoad <- function(df){
  SanctionedLoadDF = data.frame(0,as.Date('2017-12-31','%Y-%m-%d'),0)
  names(SanctionedLoadDF) = c("ConsumerNumber","MonthofConsumption","Load")
  
  idx = df$ValidToDate == as.Date('2050-12-31','%Y-%m-%d')
  df[idx,"ValidToDate"] = as.Date('2020-06-01','%Y-%m-%d')
  
  for(i in 1:nrow(df)){
    date_seq = seq(as.Date(as.yearmon(df$ValidFromDate[i])),
                   as.Date(as.yearmon(df$ValidToDate[i])),by="month")
    consumer_no = rep(df$ConsumerNumber[i],length(date_seq))
    load = rep(df$SanctionedLoad_KW[i],length(date_seq))
    temp = data.frame(consumer_no,date_seq,load)
    names(temp) = c("ConsumerNumber","MonthofConsumption","Load")
    SanctionedLoadDF = rbind(SanctionedLoadDF,
                             temp)
  }
  SanctionedLoadDF = SanctionedLoadDF[-c(1),]
  row_idx = SanctionedLoadDF$MonthofConsumption >= as.Date('2015-01-01','%Y-%m-%d') &
    SanctionedLoadDF$MonthofConsumption <= as.Date('2017-06-01','%Y-%m-%d')
  
  SanctionedLoadDF = SanctionedLoadDF[row_idx,]
  SanctionedLoadDF = SanctionedLoadDF %>%
    group_by(ConsumerNumber,MonthofConsumption) %>%
    summarise(SanctionedLoadinKW = max(Load))
  
  rm(temp,i,row_idx,date_seq,consumer_no,load)
  return(SanctionedLoadDF)
}

computeLF <- function(df){
  
  # load lubridate for computing number of days in a month
  library(lubridate)
  
  df$NumberofDays = days_in_month(df$MonthofConsumption)
  
  # Compute the load factor
  # Load Factor = Total Consumption in kWh/(Number of Days * 24 * Peak Recorded Demand in KW)
  
}


MeterConsumerNumberDF = read.csv("Data//PROCESSED//MeterMasterDF.csv", header = TRUE, stringsAsFactors = FALSE)
MeterConsumerNumberDF = MeterConsumerNumberDF[,c("ConsumerNumber","METERNO")]

#source("DataPreparation.R")
dataconsumption = read.csv("Data//PROCESSED//ConsumptionDF.csv", header = TRUE, stringsAsFactors = FALSE)
MonthlyConsumptionDF = getPerDayConsumption(dataconsumption)
MonthlyConsumptionDF = MonthlyConsumptionDF[,c("ConsumerNumber","ConsumptionDate",
                                               "consumption_KVAH")]
library(dplyr)
library(zoo)
MonthlyConsumptionDF$MonthofConsumption = as.Date(as.yearmon(MonthlyConsumptionDF$ConsumptionDate))

MonthlyConsumptionDF = MonthlyConsumptionDF %>%
  dplyr::group_by(ConsumerNumber,MonthofConsumption) %>%
  dplyr::summarise(consumption_KVAH = sum(consumption_KVAH))

#source("DataPreparation.R")
dataEvent = read.csv("Data//PROCESSED//SuspiciousEventDF.csv", header = TRUE, stringsAsFactors = FALSE)
SuspiciousEventDuration = getEventDurations(dataEvent,MeterConsumerNumberDF)

write.csv(MonthlyConsumptionDF, "Data//PROCESSED//MonthlyConsumptionStandardisedDF.csv", row.names = FALSE)
write.csv(SuspiciousEventDuration, "Data//PROCESSED//SuspiciousEventDuration.csv", row.names = FALSE)
