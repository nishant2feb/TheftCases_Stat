#EventData = read.csv("Data\\SuspiciousEventDuration.csv",header = TRUE, stringsAsFactors = FALSE)

Method4<- function(EventData){
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
#EventCounts$Adjusted[EventCounts$Year==2017] = (12/7)*EventCounts$Adjusted[EventCounts$Year==2017]

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

# Score > 80 : Very High 
# Score > 50 : High
# Score > 30 : Medium
# Score <= 30 : Low
EventCounts$RiskClass = "Low"
EventCounts$RiskClass[EventCounts$Score > 30] = "Medium"
EventCounts$RiskClass[EventCounts$Score > 70] = "High"
EventCounts$RiskClass[EventCounts$countT <= 50] = "Low"

Method4_Risk = EventCounts[,c("ConsumerNumber","RiskClass")]
names(Method4_Risk) = c("ConsumerNumber","Risk4")
Method4_Risk = unique(Method4_Risk)
return(Method4_Risk)
}
#write.csv(Method4_Risk,"OUTPUT//Method4_Risk.csv",row.names = FALSE)
