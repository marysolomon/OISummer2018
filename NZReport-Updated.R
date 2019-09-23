#-----------LOAD PACKAGES---------------------------------#
start.time <- Sys.time()
# LOAD PACKAGES#
library(data.table)
library(tidyverse)
library(scales) #for percent formatting
library(xlsx)



#====================================USER INPUT=======================================================
DM_loc = "..."
IPU_loc = "..."
IPU_name = ".csv" 
Export_loc = "..."
Export_name = ".xlsx"

#Enter FP here
fpnum <- ...


#Enter Bottle Criteria Here
Date <- c("...")
StartTime <- c("...")
StopTime <- c("...")
Mould <- c()
Section <- c()
Cavity <- c()



#======================================READ DATA=================================================================
input <- data.frame(Date, StartTime, StopTime, Mould, Section, Cavity)


#DM DATA
setwd(DM_loc)
filelist <- list.files()
data <- rbindlist(lapply(filelist, function(x){if (file.size(x)!= 0){fread(x, header = FALSE, sep = "\t")}}))
DM <- separate(data = data, col = V1, into = c("Time", "Read", "Datamatrix"), sep = "\\;", extra = "drop")
DM$Datamatrix <- noquote(gsub("\\D", "", DM$Datamatrix))
DM$Read[DM$Read == "True"] <- "TRUE"
DM$Read[DM$Read == "False"] <- "FALSE"
DM$Datamatrix[DM$Datamatrix == ""] <- 0


#IPU
setwd(IPU_loc)
IPU <-fread(IPU_name)
IPU <- filter(IPU, FP==fpnum)
IPU$Datamatrix[IPU$Datamatrix == ""] <- 0

IPU <- mutate(IPU, Date = paste(as.numeric(substr(Datamatrix, 1, 2)),
                                as.numeric(substr(Datamatrix, 3, 4)),
                                as.numeric(substr(Datamatrix, 5, 6)) + 2000, sep = "/"),
              time = paste(substr(Datamatrix, 7, 8),
                           substr(Datamatrix, 9, 10),
                           substr(Datamatrix, 11, 12), sep = ":"),
              Section = as.numeric(substr(Datamatrix, 21, 22)),
              Gob = as.numeric(substr(Datamatrix, 23, 24)))


#======================================== TABLE 1 READ TOTALS===================================================================
#create table
readtotals <- function(){
  DM <- mutate(DM, Hour = as.numeric(substr(DM$Time, 12, 13)),
                  Minute = as.numeric(substr(DM$Time, 15, 16)),
                  Duplicatecodes = ifelse(DM$Datamatrix == DM$Datamatrix[-1], ifelse(DM$Datamatrix > 1, 1, 0), 0),
                  Badcode = ifelse(nchar(DM$Datamatrix) == 24, 0, ifelse(DM$Read == FALSE,0,1)))
  hour <- unique(DM$Hour)
  #If extracting Hour and Minute from Serial number
  #Comment out lines 69-72
  #Uncomment code below 
  #To Comment/Uncomnment code highlighting the code and Ctrl + Shift + C
  #DM <- mutate(DM, Hour = as.numeric(substr(DM$Datamatrix, 7, 8)),
  #                 Minute = as.numeric(substr(DM$Datamatrix, 9, 10)),
  #                 Duplicatecodes = ifelse(DM$Datamatrix == DM$Datamatrix[-1], ifelse(DM$Datamatrix > 1, 1, 0), 0),
  #                 Badcode = ifelse(nchar(DM$Datamatrix) == 24, 0, ifelse(DM$Read == FALSE,0,1)))
  
  Date <-c()
  GoodReads <- c()
  BadReads <- c()
  DuplicateReads <- c()
  BadCodes <- c()
  for(i in min(hour):max(hour)){
    Date <- append(Date, substr(filter(DM, Hour == i)[1,1],1,10))
    GoodReads <- append(GoodReads, nrow(filter(DM, Hour == i, Read == TRUE)))
    BadReads <- append(BadReads, nrow(filter(DM, Hour == i, Read == FALSE)))
    DuplicateReads <- append(DuplicateReads, nrow(filter(DM, Hour == i, Duplicatecodes == 1)))
    BadCodes <-  append(BadCodes, nrow(filter(DM, Hour == i, Badcode == 1)))
  }
  ## calculate totals and additional columns
  Hour <- c(min(hour):max(hour), "Total")
  Date <- append(Date, "")
  GoodReads <- append(GoodReads, nrow(filter(DM, Read == TRUE)))
  BadReads <- append(BadReads, nrow(filter(DM, Read == FALSE)))
  DuplicateReads <- append(DuplicateReads, nrow(filter(DM, Duplicatecodes == 1)))
  BadCodes <-  append(BadCodes, nrow(filter(DM, Badcode == 1)))
  TotalReads = GoodReads + BadReads
  RawReadRate = percent((GoodReads + DuplicateReads + BadCodes) / TotalReads)
  ## create Dataframe 
  x = data.frame(Hour, Date, GoodReads, BadReads, DuplicateReads, BadCodes, TotalReads, RawReadRate)
  x <- format(x, big.mark = ",") # add commas for numbers in the thousands.
  return(x) 
}
readtotals() -> Table1
Table1 <- cbind(Table1, About = c("This table reports the performance of the Spin Reader using the raw Spin Reader Data.", rep("", nrow(Table1)-1)))
#export


#======================================TABLE 2 REJECTION TRACKING========================================================
rejectTrack <- function(b = input){
  TotalInspected <- c()
  TotalRejected <- c()
  TotalNoCavity <- c()
  TotalAutoRejected <- c()
  
  for(i in 1:nrow(b)){ # use for filtering for Total Rejected
    temp <- filter(IPU, Date == b[i,1])
    if(is.na(b[i,2]) == FALSE & is.na(b[i,3]) == FALSE)
      temp <- filter(temp, as.numeric(gsub(":", "", substr(time,1,5))) >= as.numeric(gsub(":", "", b[i,2])),#compare time as numeric values.
                     as.numeric(gsub(":", "", substr(time,1,5))) <= as.numeric(gsub(":", "", b[i,3]))) #Ex: 1:15 and 2:20 are compared as 115 and 220 respectively
    if(is.na(b[i,4]) == FALSE)
      temp <- filter(temp, Cavity == b[i,4])
    if(is.na(b[i,5]) == FALSE & is.na(b[i,6]) == FALSE)
      temp <- filter(temp, Section == b[i,5], Gob == b[i,6])
    
    
    TotalInspected <- append(TotalInspected, nrow(temp))
    TotalRejected <- append(TotalRejected, nrow(filter(temp, Rejected == 1 | RejectList == 1 | DmTimeBasedReject == 1)))
    TotalNoCavity <- append(TotalNoCavity, nrow(filter(temp, Cavity == 0)))
    TotalAutoRejected <- append(TotalAutoRejected, nrow(filter(temp, AutoRejInv == 1)))
  }
  TotalError = TotalInspected - TotalRejected
  j <- data.frame(TotalInspected, TotalRejected, TotalError, TotalNoCavity, TotalAutoRejected)
  
  return(cbind(b,j))
}
rejectTrack()-> Table2
Table2 <- cbind(Table2, About = c("This table reviews the IPU data as well as user input on rejections and reports success/failure of rejections based on the bottle’s identity.", rep("", nrow(Table2)-1)))




#====================================TABLE 3 DMX CODE CHECK=========================================================
Table3 <- data.frame(CodeEmpty = nrow(filter(IPU, Datamatrix == 0)),
                     NoPacketReceived = nrow(filter(IPU, DmNoPacket == 1)),
                     BadRead = nrow(filter(IPU, DmBadRead == 1)),
                     CodeError = nrow(filter(IPU, DmCodeError == 1)),
                     CIDMisMatch = nrow(filter(IPU, DmCidMismatch == 1)))
Table3 <- cbind(Table3, About = c("This table reports the number of times a datamatrix code was not received by the IPU from the Spin Reader.", rep("", nrow(Table3)-1)))


#==================================TABLE 4 IPU VS READER============================================================
#SERIAL NUMBER TABLE
IPUvsDM <- function(){
  #TRANSFORM the data. 
  #TRUNCATE DM time to the second. 
  #SELECT IPU columns needed.
  #RENAME datamatrix columns to indicate the data it came from 
  IPU <- IPU %>% select(Time, Datamatrix) %>% rename(IPUCode = Datamatrix)
  DM <-  DM %>% mutate(Time = substr(Time, 1, 19)) %>% rename(DMCode = Datamatrix)
  
  #rows omitted
  dif = nrow(DM) - nrow(IPU)
  if(dif > 0){
    DM <- DM[1:nrow(IPU),]
    message(paste(abs(dif), "rows deleted from DM"))
  } #cut the DM data set down to match length of the IPU data
  if(dif < 0){
    IPU <- IPU[1:nrow(DM),]
    message(paste(abs(dif), "rows deleted from IPU"))
  }
  
  #GOAL: combine row for row. 
  IPU <- IPU %>% rename(IPUTime = Time) #make unique time column names 
  DM <- DM %>% rename(DMTime = Time)
  return(cbind(DM, IPU))
}
IPUvsDM() -> Serials

#CALCULATE FOR MATCHES
Serials <- mutate(Serials, MisMatch = ifelse(DMCode == IPUCode, 0, 1),
                  Error = ifelse(MisMatch == 0, 0,
                                 ifelse(DMCode != 0 & IPUCode != 0, 1,
                                        ifelse(DMCode == 0 & IPUCode != 0, 2,
                                               ifelse(DMCode != 0 & IPUCode == 0, 3, 0)))))
Serials <- cbind(Serials, About = c("This table reports when the Spin Reader and IPU datamatrix data do not match, and provides mismatch classifications.", rep("", nrow(Serials)-1)))

#MAKE LEGEND FOR ERROR COLUMN
Legend <- data.frame("Error" = c(0:3), "Total" = c(nrow(filter(Serials, Error == 0)), nrow(filter(Serials, Error == 1)),
                                                   nrow(filter(Serials, Error == 2)), nrow(filter(Serials, Error == 3))),
                     "Meaning" = c("NO ERROR | DM&IPU codes the same","Existing DM&IPU codes different",
                                   "DM code missing | IPU code exists","DM code exists | IPU code missing"))




#============================================EXPORT=========================================================
setwd(Export_loc)
write.xlsx(Table1, Export_name,sheetName = "Table 1-Read Totals", row.names = FALSE)
write.xlsx(Table2, Export_name, sheetName = "Table 2-Rejection Tracking", append = TRUE, row.names = FALSE)
write.xlsx(Table3, Export_name, sheetName = "Table 3-DMX Code Check", append = TRUE, row.names = FALSE)
fwrite(Serials, file = paste(substr(DM[1,1], 1,10),"Table4.csv",sep=""))
write.xlsx(Legend, Export_name, sheetName= "Legend for Table 4", append = TRUE, row.names = FALSE)

end.time = Sys.time()
print(end.time - start.time)

