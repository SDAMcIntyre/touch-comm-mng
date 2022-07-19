library(stringr)
library(plyr)
library(readxl)
library(reshape2)


read_spike_data <- function(spike.files) {
  header.labels <- c('Spike.time.sec','include','Comment.time.sec','Comment')
  spike.data <- c()
  for (i in seq_along(spike.files)) {
    #i <- i+1
    print(paste(i,'of',length(spike.files),':',spike.files[i]))
    spike.data.i <- read.table(spike.files[i], skip = '3', sep = '\t', col.names = header.labels, stringsAsFactors =   FALSE)
    spike.data.i$Spike.time.sec[spike.data.i$include != 1] <- NA
    spike.data.i <- subset(spike.data.i, include > 0, select = -include)
    unit.info <- str_split(spike.files[i],'_|\\.')[[1]]
    spike.data.i$UnitName <- unit.info[3]
    spike.data.i$UnitType <- unit.info[4]
    spike.data.i <- mutate(spike.data.i, StimNumber = as.numeric(as.factor(Comment.time.sec)))
    print(table(spike.data.i$StimNumber))
    spike.data <- rbind(spike.data, spike.data.i)
  }
  unique(tolower(spike.data$Comment))
  gestures <- c('attention','calming','gratitude','sadness','love','happiness')
  gestures.pattern <- paste0('(',paste(gestures, collapse = ')|('), ')')
  for (i in seq_along(spike.data$Comment)) {
    comment.i <- str_to_lower(spike.data$Comment[i])
    if (comment.i == 'happy') { comment.i <- 'happiness'}
    g <- which(grepl(comment.i,gestures))
    if (length(g) < 1) {
      comment.i.full <- str_extract(comment.i,gestures.pattern)
      if (is.na(comment.i.full)==FALSE) g <- which(grepl(comment.i.full, gestures))
    } 
    if (length(g) > 0) {
      spike.data$Stimulus[i] <- gestures[g]
    } else {spike.data$Stimulus[i] <- 'other'}
  }
  spike.data <- subset(spike.data, Stimulus != 'other')
  return(spike.data)
}

read_umea_isi <- function(data.folder, isi.file, info.file) {
  umea.isi <- read_xlsx(paste0(data.folder,isi.file), sheet = 2, col_types = 'numeric')
  umea.isi <- melt(umea.isi, variable.name = 'row.n', value.name = 'isi.ms', na.rm = TRUE)
  umea.isi$row.n <- as.numeric(as.character(umea.isi$row.n))
  umea.info <- read_xlsx(paste0(data.folder, info.file))
  umea.info$row.n <- seq_along(umea.info$UnitType)
  #umea.info <- umea.info[unique(umea.isi$row.n),]
  umea.info <- ddply(umea.info, .(UnitName), mutate, StimNumber = as.numeric(interaction(PeriodNumber,ZoomFile, drop = TRUE)))
  # ddply(umea.info, .(UnitName), summarise, nPeriods = length(unique(StimNumber)))
  umea.isi <- merge(umea.isi, umea.info[,c('UnitName','UnitType','StimulusDetails','ZoomFile','PeriodNumber','row.n')])[,-1]
  umea.isi <- ddply(umea.isi, .(UnitName), mutate, StimNumber = as.numeric(interaction(PeriodNumber,ZoomFile, drop = TRUE)))
  # ddply(umea.isi, .(UnitName), summarise, nPeriods = length(unique(StimNumber)))
  umea.isi[umea.isi$isi.ms==0, 'isi.ms'] <- NA
  umea.isi$UnitType[umea.isi$UnitType == 'Joint rec'] <- 'Joint'
  umea.isi$UnitType[umea.isi$UnitType == 'FAII'] <- 'FA-II'
  umea.isi$UnitType[umea.isi$UnitType == 'SAII'] <- 'SA-II'
  umea.isi$UnitType[umea.isi$UnitType == 'SAI'] <- 'SA-I'
  umea.isi$isi.sec <- umea.isi$isi.ms/1000
  names(umea.isi)[4] <- c('Stimulus')
  umea.isi$Stimulus <- tolower(umea.isi$Stimulus)
  umea.isi$Zoom <- paste(umea.isi$ZoomFile, umea.isi$PeriodNumber, sep = '.')
  return(umea.isi[,-c(1,5,6)])
  #return(umea.isi)
}