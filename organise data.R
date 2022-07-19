source('organise functions 2.R')

unitOrder <- c('CT', 'FA-II', 'Field', 'HFA', 'SA-I', 'SA-II', 'MS', 'Joint')

data.folder <- 'Gestures-ISIs/'
spike.files <- paste0(data.folder,dir(data.folder, 'txt'))
spike.data <- read_spike_data(spike.files) 


isi.data <- spike.data %>%
  rowwise() %>% mutate(Stimulus = get_stim_name(Comment)) %>% 
  filter(!is.na(Stimulus) & (Spike.time.sec > Comment.time.sec + 12) == FALSE) %>% 
  group_by(UnitName,UnitType,StimNumber,Stimulus) %>% 
  mutate(
    isi.sec = Spike.time.sec %>% as.numeric %>% diff %>% c(NA,.)
  ) %>% 
  ungroup() %>% 
  mutate(UnitType = factor(UnitType, levels = unitOrder)) 


# umea.isi <- read_umea_isi(data.folder, 'gestures-isis-umea-3.xlsx', 'gestures-isis-umea-info-3.xlsx')
#isi.data <- umea.isi
# isi.data <- rbind(isi.data, umea.isi)

write_tsv(isi.data,'isi-adi_data-2020November13.txt')

# spike.summary <- isi.data %>% 
#   group_by(UnitName,UnitType,StimNumber,Stimulus) %>% 
#   summarise(
#     n.spikes = length(na.omit(isi.sec))+1, 
#     mean.freq = 1/mean(isi.sec, na.rm = TRUE), 
#     peak.freq =1/min(isi.sec, na.rm = TRUE), 
#     mean.isi.sec = mean(isi.sec, na.rm = TRUE), 
#     sd.isi.sec = sd(isi.sec, na.rm = TRUE), 
#     cv.isi.sec = sd(isi.sec, na.rm = TRUE)/mean(isi.sec, na.rm = TRUE)
#   ) %>% 
#   ungroup() %>% 
#   mutate(mean.freq = coalesce(mean.freq, 0)) 
# 
# spike.summary %>% 
#   group_by(UnitName) %>% 
#   tally()
# 
# # write.table(spike.summary,'summary-data-2019August06.txt', sep = '\t', row.names = FALSE)
# 
# library(ggplot2)
# # change default plot appearance
# # theme_set(theme_classic(base_size=14))
# 
# windows()
# isi.data %>% 
#   filter(UnitType != 'Joint') %>% 
#   ggplot() +
#   facet_grid(UnitType~Stimulus) +
#   geom_histogram(aes(x = isi.sec, fill = Stimulus), binwidth = 0.2) +
#   scale_y_log10() +
#   coord_cartesian(xlim = c(0,6)) +
#   labs(x = 'Inter-spike interval (seconds)', y = 'Count')
# 
# 
# 
# spike.summary$StimLabel <- toupper(substring(spike.summary$Stimulus,1,1))
# ggplot(subset(spike.summary, UnitType != 'Joint')) +facet_wrap(~UnitType) +geom_point(aes(x = StimLabel, y = cv.isi.sec, colour = UnitName), shape = 4, size = 5, position = position_dodge(width = 0.5)) +labs(x = '', y = 'Coefficient of variation\n', colour = '') +theme(legend.position = 'none')
# 
# ggplot(subset(spike.summary, UnitType != 'Joint')) +facet_wrap(~Stimulus) +geom_point(aes(x = UnitType, y = cv.isi.sec, colour = UnitName), shape = 4, size = 5, position = position_dodge(width = 0.5)) +labs(x = '', y = 'Coefficient of variation\n', colour = '') +theme(legend.position = 'none')

