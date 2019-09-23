#dir.create(Sys.getenv("R_LIBS"), recursive = TRUE)
#if(!require(bit64))install.packages("bit64", repos = "https://cloud.r-project.org/")
#if(!require(data.table))install.packages("data.table", repos = "https://cloud.r-project.org/")
suppressMessages(library(data.table))
#if(!require(plyr))install.packages("plyr", repos = "https://cloud.r-project.org/")
suppressMessages(library(plyr))
#if(!require(tidyverse))install.packages("tidyverse", repos = "https://cloud.r-project.org/")
suppressMessages(library(tidyverse))


#---------------- USER INPUT ------------------------------------------------------------
#for executable.
args <- commandArgs(trailingOnly = TRUE)
date = args

if(length(date)==0){
  message("Please provide a valid date")
  message("Make sure the format is yyyy-mm-dd.")
}

for(i in 1:length(date)){
  #--------------- READ DATA -------------------------------------------------------------
  data_in = paste("D:/AnalysisData/Import/", date[i], sep = "")
  export_name = paste("Merged", date[i], ".csv", sep="") 
  data_out = "D:/AnalysisData/Export"
  setwd(data_in)
  

  FP <- fread(list.files()[grep("FP", list.files())])
  
  
  H1 <- as.character( fread(list.files()[grep("MCS1", list.files())], nrows = 1, header = FALSE)) #extracts column names with NA as last element
  MCS1 <- fread(list.files()[grep("MCS1", list.files())], skip = 2, col.names = H1[-length(H1)])
  
  H2 <- as.character( fread(list.files()[grep("MCS2", list.files())], nrows = 1, header = FALSE)) #extracts column names
  MCS2 <- fread(list.files()[grep("MCS2", list.files())], skip = 2, col.names = H2[-length(H2)])
  
  
  setwd(paste(data_in, "/", list.files()[grep("DM", list.files())], sep = ""))
  filelist <- list.files()
  
  data <- rbindlist(lapply(filelist, function(x){if (file.size(x)!= 0){fread(x, header = FALSE, sep = "\t")}}))
  DM <- separate(data = data, col = V1, into = c("Time", "Read Value","SerialNumber", "Slant",
                                                 "Contrast", "Decoding Error", "Module Height", "Module Width",
                                                 "Reader Number", "Fill Line", "Fill Site", "Fill Country",
                                                 "Brand", "Bottle Design", "Batch Number", "Lot Number"), sep = "\\;",extra = "drop")

  
  #-------------- COMBINE MCS ------------------------------------------------------------------------
  MCS1 <- MCS1[ , !which(duplicated(colnames(MCS1))), with = FALSE]
  MCS2 <- MCS2[ , !which(duplicated(colnames(MCS2))), with = FALSE]
  MCS <- merge(MCS1, MCS2, by = "Time")
  
  #format Time column to match FP and DM Time format
  MCS <- mutate(MCS, Time = format(strptime(MCS$Time, format = "%m/%d/%Y %H:%M:%S"), "%Y-%m-%d %H:%M:%S"))
  names(MCS) <- gsub("S64 ", "", names(MCS))
  
  
  
  
  #separate into gobs A and B
  
  A <- select(MCS, 'Time', 'F-BTM', names(MCS)[grep("GA", names(MCS))])
  A <- A %>% mutate(Gob = rep("A", nrow(A)))
  names(A) <- gsub("GA-", "",names(A))
  
  
  B <- select(MCS, 'Time', 'F-BTM', names(MCS)[grep("GB", names(MCS))])
  B <- B %>% mutate(Gob = rep("B", nrow(B)))
  names(B) <- gsub("GB-", "",names(B))
  
  
  MCS <- rbind(A,B)
  
  #------------------------------------FORMAT DATA MATRIX READER---------------------------------------
  DM$SerialNumber <- as.character(noquote(gsub("\\D", "", DM$SerialNumber)))
  DM$`Read Value`[DM$`Read Value` == "True"] <- "TRUE"
  DM$`Read Value`[DM$`Read Value` == "False"] <- "FALSE"
  DM = mutate(DM, Time = substr(Time, 1, 19), #take off milliseconds. 
              JDay = substr(SerialNumber, 1, 5), #extract info from matrix code
              Hour = substr(SerialNumber, 6, 7),
              Minute = substr(SerialNumber, 8, 9),
              IndexCount = substr(SerialNumber,10, 12),
              Plant = substr(SerialNumber, 13, 14),
              Line = substr(SerialNumber, 15, 16),
              CreateTime = paste(as.Date(JDay, "%y%j"), #create time stamp for creation time.
                                 paste(Hour, Minute,"00", sep = ":"), sep = " "))
  
  
  
  
  #--------------FILTER/FORMAT FP DATA--------------------------------------------------------------
  #which FP are we looking at?
  #"Konatic003-70-004" == FP3
  FP <- filter(FP, FP == 3)} 
  
  
  #Format
  FP <- mutate(FP, Section = substr(Position, 1, 2), #add Section and Gob
               Gob = substr(Position, 3, 3))
  
 

  
  
  
  
  #-------------------------- MERGE DATA  ----------------------------------------------------------
  #match number of rows.
  dif = nrow(DM) - nrow(FP)
  if(dif > 0){
    DM <- DM[1:nrow(FP),]
    message(paste(abs(dif), "rows deleted from DM data"))
  } #cut the DM data set down to match length of the FP data
  if(dif < 0){
    FP <- FP[1:nrow(DM),]
    message(paste(abs(dif), "rows deleted from FP data"))
  }
  if(dif == 0){
    message("Datamatrix and Inspection reader have same number of rows")
  }
  
  temp <- cbind(DM, FP[,-1]) #have a unique Time column
  
  #Combine hot and cold end by gob
  
  names(MCS) <- sub('Time', 'CreateTime', names(MCS))
  merged <- join(MCS, temp, by = c('CreateTime', 'Gob'), type = 'right', match = 'all')
  head(merged) 
  
  #---------------------------- CALCULATED COLUMNS -------------------------------------------------
  merged <- merged %>% mutate(AvgShldr = (OTG_Shldr_Min_Thick + OTG_Shldr_Max_Thick)/2,
                              AvgHeel = (OTG_Heel_Min_Thick + OTG_Heel_Max_Thick) / 2,
                              ShldrRatio = round(OTG_Shldr_Max_Thick/OTG_Shldr_Min_Thick, digits = 3),
                              BodyRatio = round(OTG_Body_Max_Thick/OTG_Body_Min_Thick, digits = 3),
                              HeelRatio = round(OTG_Heel_Max_Thick/OTG_Heel_Min_Thick, digits = 3),
                              SplitFinishReject = ifelse(CHECK_VERT_1 == 1 | CHECK_VERT_2 == 1, 1, 0))
  # --------------------- EXPORT ----------------------------------------------------------------
  #Set location of the merged data.
  setwd(data_out)
  
  #export
  fwrite(merged, export_name)
  message(paste(export_name, file.exists(export_name)))
}
