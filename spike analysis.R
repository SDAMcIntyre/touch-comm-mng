library(tidyverse)

May2019 <- read_tsv('isi-data-2018May25.txt')
Nov2020 <- read_tsv('isi-adi_data-2020November13.txt')

duplicates <- intersect(unique(May2019$UnitName), unique(Nov2020$UnitName))

spikes <- May2019 %>% 
  filter(UnitName %in% duplicates == FALSE) %>% 
  bind_rows(Nov2020 %>% select(names(May2019))) %>% 
  filter(!is.na(isi.sec) & UnitType != 'Joint') %>% 
  group_by(UnitName,UnitType,Stimulus) %>% 
  mutate(Presentation = as.numeric(as.factor(StimNumber))) %>% 
  group_by(UnitName,UnitType,Stimulus, Presentation) %>% 
  mutate(time.sec = cumsum(isi.sec),
         IFF = 1/isi.sec)

# write_csv(spikes, 'all-spike-data.csv')

# UnitsfromSpreadsheet <- c('13-2', '20-1', '20-2', '20-4',
#                           '21-1', '21-2', '22-2', '26-3',
#                           '27-3', '28-3', '28-4', '28-6',
#                           '29-1', '29-2', '39-3', '39-4',
#                           '42-1', '42-2', '43-1',
#                           '44-1', '44-2', '44-3', '17-01')

# unique(spikes$UnitName)

#### summary values ####
# features for random forest classifier

spike.summary <- spikes %>% 
  group_by(UnitName,UnitType,StimNumber,Stimulus) %>% 
  summarise(
    peak.freq =max(1/isi.sec), 
    mean.freq = mean(1/isi.sec), 
    sd.freq = sd(1/isi.sec), 
    n.spikes = length(isi.sec)+1, 
    n.bursts = sum(isi.sec > 1)
  ) %>% 
  ungroup()  

# write.table(spike.summary,'summary-data-2019August06.txt', sep = '\t', row.names = FALSE)


#### time trace for one unit ####
# should fill with zeros to make it look nice

uname <- 'SC-39-4'
utype <- spikes %>% filter(UnitName == uname) %>% pull(UnitType) %>% .[1]
spikes %>% 
  filter(UnitName == uname ) %>% 
  mutate(pane = paste(Presentation, Stimulus)) %>% 
  ggplot(aes(x = time.sec, y = IFF, colour = Stimulus)) +
  facet_wrap( ~ pane, ncol = 6, scales = 'free') +
  geom_point() +
  theme_classic(base_size = 14) +
  labs(title = paste(utype, uname))

#### histogram for one unit ####

spikes %>% group_by(UnitName, UnitType) %>% tally(sort = TRUE)

spikes %>% 
  filter(UnitName == 'SC-21-2') %>% 
  ggplot(aes(x = IFF)) +
  geom_histogram()

#### CIs over IFF for each unit ####

spikes %>% 
  mutate(Stim = str_trunc(Stimulus, width = 1, ellipsis = '') %>% toupper()) %>% 
  ggplot(aes(x = Stim, 
             y = IFF, colour = Stimulus, group = UnitName)) +
  facet_wrap(~ UnitType, scales = 'free', ncol = 2) +
  # scale_x_reordered() +
  # scale_y_log10() +
  stat_summary(fun.data = mean_cl_boot, geom = 'errorbar',
               size = 1.2, width = 0,
               position = position_dodge(0.4)) +
  theme_bw(base_size = 14) + theme(strip.background = element_blank()) +
  labs(x = NULL, y = 'IFF')


#### CV for each unit ####
  
cv <- function(x) {
  sd(x)/mean(x)
}

spikes %>% 
  mutate(Stim = str_trunc(Stimulus, width = 1, ellipsis = '') %>% toupper()) %>% 
  ggplot(aes(x = Stim, 
             y = IFF, fill = Stimulus, group = UnitName)) +
  facet_wrap(~ UnitType) +
  # scale_y_log10() +
  stat_summary(fun = cv, geom = 'point',
              shape = 21, alpha = 0.5, size = 3,
              position = position_dodge(0.2)) +
  # coord_cartesian(ylim = c(-1,2)) +
  theme_classic(base_size = 14) +
  labs(x = NULL, y = 'CV of IFF')

ggsave('IFF_CV.svg')
ggsave('IFF_CV.png')

spikes %>% 
  group_by(UnitType, Stimulus, UnitName) %>% 
  summarise(cv = cv(log(IFF))) %>% #View()
  group_by(UnitType, Stimulus) %>% 
  summarise(cv.range = diff(range(cv, na.rm = TRUE))) %>% 
  group_by(UnitType) %>% 
  summarise(mean.cv.range = mean(cv.range)) %>% 
  arrange(mean.cv.range)



#### ridge plot ####

library(ggridges)

lseq <- function(base = 10, ...) {
  base^(log(seq(...), base = base))
} #used this to space colours

spikes %>% 
  ggplot( aes(x = IFF, y = Stimulus)) +
  facet_wrap(~ UnitType, scales = 'free') +
  geom_density_ridges_gradient(
    aes(fill = ..x..), scale = 3, size = 0.3
  ) +
  scale_fill_gradientn(
    colours = c("#FDE333", "#53CC67", "#009B95", "#00588B", "#4B0055"),
    values = lseq(base = 6, 0.001, 1, length.out = 6),
    name = "IFF"
  ) +  theme_ridges()
ggsave('IFF_ridge.svg')
ggsave('IFF_ridge.png')


#### multimodality ####
# https://stats.stackexchange.com/questions/138223/how-to-test-if-my-distribution-is-multimodal

library(mclust)

# spikes %>% group_by(UnitName, Stimulus) %>% tally(sort = TRUE)

# take the example with highest N to start
ex.spikes <- spikes %>% 
  filter(UnitName == 'SC-38-4' & Stimulus == 'love') %>% 
  pull(IFF)

hist(ex.spikes)

ex.gmm <- Mclust(ex.spikes)
summary(ex.gmm)

# force to unimodal
ex.gmm.1 = Mclust(ex.spikes, G=1)

#force to bimodal
ex.gmm.2 = Mclust(ex.spikes, G=2)

# parametric bootstrap cross-fitting method

ex.gmm.2$parameters

B = 10000;    x2.d = vector(length=B);    x1.d = vector(length=B)
for(i in 1:B){
  x2      = c(rnorm(68, mean=12346.98, sd=sqrt( 4514863)), 
              rnorm(52, mean=23322.06, sd=sqrt(24582180)) )
  x1      = rnorm( 120, mean=17520.91, sd=sqrt(43989870))
  x2.d[i] = Mclust(x2, G=2)$loglik - Mclust(x2, G=1)$loglik
  x1.d[i] = Mclust(x1, G=2)$loglik - Mclust(x1, G=1)$loglik
}
x2.d = sort(x2.d);  x1.d = sort(x1.d)
summary(x1.d)


#### pleasantness ####
library(Hmisc)

pleas <- read_csv('all_pleasantness_data.csv')

spike.boot <- spike.summary %>% 
  group_by(UnitType, Stimulus) %>% 
  summarise(peak.freq = list(
    mean_cl_boot(peak.freq, B = 10000) %>%
      rename(peak.freq.mean = y, peak.freq.lwr = ymin, peak.freq.upr = ymax)),
    mean.freq = list(
      mean_cl_boot(mean.freq, B = 10000) %>%
        rename(mean.freq.mean = y, mean.freq.lwr = ymin, mean.freq.upr = ymax))
    ) %>% 
  unnest(cols = c(peak.freq, mean.freq)) 


pleas.boot <- pleas %>% 
  group_by(cued) %>% 
  summarise(rating = list(
    mean_cl_boot(response, B = 10000) %>%
      rename(rating.mean = y, rating.lwr = ymin, rating.upr = ymax))
    ) %>% 
  unnest(cols = rating) %>% 
  rename(Stimulus = cued)

pleas.ct <- spike.boot %>% 
  filter(UnitType == 'CT') %>% 
  full_join(pleas.boot)

pleas.ct %>% 
  ggplot(aes(x = rating.mean, y = peak.freq.mean, fill = Stimulus)) +
  geom_errorbar(aes(ymin = peak.freq.lwr, ymax = peak.freq.upr),
                width = 0) +
  geom_errorbarh(aes(xmin = rating.lwr, xmax = rating.upr),
                 height = 0) +
  geom_point(shape = 21, size = 4) +
  scale_x_continuous(limits = c(-4,6)) +
  theme_classic(base_size = 14) +
  labs(title = 'CT')