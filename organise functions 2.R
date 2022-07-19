library(stringr)
library(dplyr)
library(readxl)
library(readr)

read_spike_data <- function(spike.files) {
  header.labels <- c('Spike.time.sec','include','Comment.time.sec','Comment')
  spike.data <- c()
  for (i in seq_along(spike.files)) {
    #i <- i+1
    print(paste(i,'of',length(spike.files),':',spike.files[i]))

    unit.info <- str_split(spike.files[i],'_|\\.')[[1]]

    spike.data.i <-  read_tsv(spike.files[i], 
                              skip = 3, col_types = 'dddc',
                              col_names = header.labels) %>% 
      mutate(Spike.time.sec = if_else(include != 1, NA_real_, Spike.time.sec)) %>% 
      filter(include > 0 & !is.na(Comment)) %>% 
      select(-include) %>%
      mutate(
        UnitName = unit.info[2],
        UnitType = unit.info[3],
        StimNumber = as.numeric(as.factor(Comment.time.sec))
      )
    
    print(table(spike.data.i$StimNumber))
    spike.data <- rbind(spike.data, spike.data.i)
  }
  gestures <- c('attention','calming','gratitude','sadness','love','happiness')
  gestures.pattern <- paste0('(',paste(gestures, collapse = ')|('), ')')
  spike.data <- spike.data %>% 
    mutate( Comment = str_to_lower(Comment)) %>% 
    mutate( Comment = if_else(Comment == 'happy', 'happiness', Comment))
  
  return(spike.data)
}

get_stim_name <- function(x) {
  gestures <- c('attention','calming','gratitude','sadness','love','happiness')
  got <- gestures[str_starts(gestures, x)]
  if (length(got) == 0) got <- NA_character_
  if (length(got) > 1) got <- 'multiple'
  return(got)
}
