path = "C:/Users/nsingh1/Desktop/FINAL_DELIVERABLE/Data/RAW/EVENT_DATA/2017/"

out.file<-""

file.names <- dir(path)

setwd(path)

for(i in 1:length(file.names)){
  

  file <- read.csv(file.names[i],
                   header=TRUE, sep = "\t",stringsAsFactors=FALSE)
  out.file <- rbind(out.file, file)
  
}

