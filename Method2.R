#===================After preprocessing===================
#setwd("D:\\NPCL_NTL_PROJECT")


Method2<-function(file1,file2,file3){
MeterConsumerNumberDF = read.csv(file1)
meterConsumer = MeterConsumerNumberDF[,c("ConsumerNumber","METERNO")]
master<-read.csv(file2)
meterdata<-read.csv(file3)
meterdata$MonthofConsumption<-as.Date(paste0(format(strptime(meterdata$EVENTDATE,"%Y-%m-%d"), "%Y-%m"), "-01"))
# meterdata<-merge(meterdata,meterConsumer,by=c("METERNO"),all.x = TRUE)
meterdata<-meterdata[meterdata$EVENT_CLOSE==TRUE,]
meterdata<-meterdata[meterdata$STATUS==0,]
meterdata<-meterdata[meterdata$DURATION>=0.5,]
require(plyr)
eventduration<-ddply(meterdata,.(ConsumerNumber),summarise, totaltime=sum(DURATION))
# eventduration<-dcast(eventduration,ConsumerNumber~EVENTDESC,sum)
# eventcount<-ddply(meterdata,.(ConsumerNumber,Event.Description,MonthofConsumption),summarize, count=length(timediff))
# eventcount<-dcast(eventcount,ConsumerNumber+MonthofConsumption~Event.Description,sum)
# eventcount$MonthofConsumption<-strptime(eventcount$MonthofConsumption, "%Y-%m-%d")
# master<-as.data.frame(master)
# collation<-merge(master,eventduration,by=c("ConsumerNumber","MonthofConsumption"),all.x=TRUE)
# master<-master[!duplicated(master[,c("ConsumerNumber","MonthofConsumption")]),]

percentile<-function(x,xi) {length(x[x < xi])/length(x)*100}
quan_em<-as.data.frame(eventduration[,c(-1)])
colnames(quan_em)<-c("max_score")
# for(j in 1:ncol(quan_em)){
#   col<-quan_em[,c(j)]
#   for(i in 1:nrow(quan_em)){
#     quan_em[i,j]<-percentile(col,quan_em[i,j])
#   }
# }
# quan_em$max_score<-0
# for(i in 1:nrow(quan_em)){
# quan_em$max_score[i]<-as.numeric(sum(quan_em[i,c(1:ncol(quan_em))]>80))
# }
# quan_em$max_score<-rowSums(quan_em)
quan_em$max_scorePer<-0
for(i in 1:nrow(quan_em)){
  quan_em$max_scorePer[i]<-percentile(quan_em$max_score,quan_em$max_score[i])
}

edPer<-cbind(eventduration[,c(1)],quan_em)
edPer<-as.data.frame(edPer)
colnames(edPer)[1]<-c("ConsumerNumber")

# reco<-read.xlsx("Deliverable.xlsx", sheetIndex = 2)
# consumers<-reco[reco$Status!="False Positive",]$CONSUMER_NO
# temp<-edPer[edPer$ConsumerNumber %in% dput(consumers),]


#================Monthwise factors without type and with type================
monthly_average<-ddply(master,.(MonthofConsumption), summarise,
                       avg_monthly_consumption = mean(Consumption_kWh))
require(lubridate)
monthly_average$month<-month(monthly_average$MonthofConsumption)
monthly_average<-ddply(monthly_average,.(month), summarise,
                       avg_monthly_consumption = mean(avg_monthly_consumption))
monthly_average$norm_factor<-0
for(i in 1:nrow(monthly_average)){
  monthly_average$norm_factor[i]<- monthly_average$avg_monthly_consumption[i]/mean(monthly_average$avg_monthly_consumption)
}
monthly_average<-monthly_average[,c("month","norm_factor")]
master$month<-month(master$MonthofConsumption)
master<-merge(master,monthly_average,by = c("month"))



# monthly_average<-ddply(master,.(MonthofConsumption,ConsumerType), summarise,
#                        avg_monthly_consumption = mean(Consumption_kWh))
# library(lubridate)
# monthly_average$month<-month(monthly_average$MonthofConsumption)
# monthly_average<-ddply(monthly_average,.(month,ConsumerType), summarise,
#                        avg_monthly_consumption = mean(avg_monthly_consumption))
# monthly_average$norm_factor<-0
# for(i in 1:nrow(monthly_average)){
#   typeaverage<-mean(monthly_average[monthly_average$ConsumerType==monthly_average$ConsumerType[i],]$avg_monthly_consumption)
#   monthly_average$norm_factor[i]<- monthly_average$avg_monthly_consumption[i]/typeaverage}
# monthly_average<-monthly_average[,c(1,2,4)]
# master$month<-month(master$MonthofConsumption)
# master<-merge(master,monthly_average,by = c("month","ConsumerType"))

#======================Calculation of deviations===========================
final<-master
require(Hmisc)
final$ndaysBilling<-final$Days
idx<-which(colnames(final)=="Consumption_kWh")
colnames(final)[idx]<-c("ISU_Consumption_KWH")
# colnames(final)[6]<-c("ISU_Consumption_KWH")
final$max_consumption<-24*final$ndaysBilling*final$LoadinKW
Consumers<-unique(final$ConsumerNumber)
# reco2<-read.xlsx("reco2.xlsx", sheetIndex = 1, header=FALSE)
# Consumers<-reco2$X1
risk<-data.frame(ConsumerNumber = rep(NA, length(Consumers)), 
                 Risk = rep(NA, length(Consumers)),
                 n_months = rep(NA, length(Consumers)),
                 ratio_NAs = rep(NA, length(Consumers)),
                 sum_cons_NA = rep(NA, length(Consumers)),
                 ratio_load_NAs = rep(NA, length(Consumers)),
                 ratio_cons_NAs = rep(NA, length(Consumers)))

final$pred_ISU_KWH<-0
final$consInd<-0
final$avg_consInd<-0
final$maxConsInd<-0
final<-final[order(final$ConsumerNumber,final$MonthofConsumption),]
for(j in 1:length(Consumers)){
  # for(j in 1:1){
  temp<-final[final$ConsumerNumber==Consumers[j],]
  # if(length(temp)>0)
  risk$ConsumerNumber[j]<-Consumers[j]
  NA_temp<-c(0,0)
  NA_freq<-c(0,0)
  count<-0
  for(i in c(1:nrow(temp))){
    # print(i)
    sum_KWH<-sum(temp$ISU_Consumption_KWH[c(1:i)])
    sum_days<-as.double(sum(temp$ndaysBilling[c(1:i)]))
    norm_temp<-temp$norm_factor[i]/mean(temp$norm_factor[c(1:i)])
    temp$pred_ISU_KWH[i]<-sum_KWH*as.double(temp$ndaysBilling[i])*norm_temp/sum_days
    if(temp$ISU_Consumption_KWH[i]>=0.80*temp$pred_ISU_KWH[i]){
      temp$consInd[i]<-temp$ISU_Consumption_KWH[i]
    }
    else{temp$consInd[i]<-NA}
    #     if(temp$ISU_Consumption_KWH[i]>=temp$average_exp_consump[i]){
    #       temp$avg_consInd[i]<-temp$ISU_Consumption_KWH[i]
    #     }else{temp$avg_consInd[i]<-NA}
    if(temp$ISU_Consumption_KWH[i]>=0.05*temp$max_consumption[i]){
      temp$maxConsInd[i]<-1
    }else{temp$maxConsInd[i]<-NA}
    if(!is.na(temp$consInd[i])){
      if(count>0){
        if(count>=NA_temp[2]){
          NA_temp[2]<-count
          # print("NA2")
          # print(NA_temp[2])
          NA_freq[2]<-1
          if(NA_temp[1]<NA_temp[2]){
            NA_temp[2]<-NA_temp[1]
            NA_temp[1]<-count
            NA_freq[1]<-1
            
          }
        }
        else if(count==NA_temp[1]||count==NA_temp[2]){
          if(count==NA_temp[1]){NA_freq[1]=NA_freq[1]+1}
          else{NA_freq[2]=NA_freq[2]+1}
        }
      }
      count = 0
    }
    else{
      count = count + 1
      # print("Count")
      # print(count)
      NA_temp[2]<-count
    }
  }
  sum_cons_NA<-NA_temp[1]+NA_temp[2]
  ratio_cons_NAs<-sum(is.na(temp$avg_consInd))/nrow(temp)
  ratio_NAs<-sum(is.na(temp$consInd))/nrow(temp)
  ratio_load_NAs<-sum(is.na(temp$maxConsInd))/nrow(temp)
  risk$ratio_NAs[j]<-ratio_NAs
  risk$sum_cons_NA[j]<-sum_cons_NA
  risk$ratio_cons_NAs[j]<-ratio_cons_NAs
  risk$n_months[j]<-nrow(temp)
  risk$ratio_load_NAs[j]<-ratio_load_NAs
}

#==========Boolean approach for risk classification==================

risk1<-merge(risk,edPer[,c("ConsumerNumber","max_scorePer")],by=c("ConsumerNumber"),all.x=TRUE)
risk1[is.na(risk1)]<-0
for(j in 1:nrow(risk1)){
  if(((risk1$ratio_NAs[j]>0.6&&risk1$ratio_load_NAs[j]>0.1)||risk1$ratio_load_NAs[j]>=1)){risk1$Risk[j]<-"High"}
  else if((risk1$ratio_NAs[j]>=0.6&&risk1$ratio_load_NAs[j]>=0.7)||(risk1$max_scorePer[j]>=70)){risk1$Risk[j]<-"High"}
  else if((risk1$ratio_NAs[j]>=0.2||risk1$ratio_load_NAs[j]>=0.3)&&(risk1$max_scorePer[j]>=30)){risk1$Risk[j]<-"Medium"}
  else{risk1$Risk[j]<-"Low"}
}
#count(risk1$Risk)
# consumers<-reco$CONSUMER_NO
# consumers<-reco[reco$Status=="False Positive",]$CONSUMER_NO
# risk_test<-risk1[(risk1$ConsumerNumber %in% consumers),]
# count(risk_test$Risk)
# (cm<-t(table(reco$Status,risk_test$Risk)))
# write.csv(table(reco$Status,risk_test$Risk),file="cm.csv")
#write.csv(risk1[,c(1,2,4,6,8)],file="risk1.csv",row.names=FALSE)

Method2_Risk = risk1[,c("ConsumerNumber","Risk")]
names(Method2_Risk) = c("ConsumerNumber","Risk2")
return(Method2_Risk)
}
#write.csv(Method2_Risk,"OUTPUT//Method2_Risk.csv",row.names = FALSE)

