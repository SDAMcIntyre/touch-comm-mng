rm(list=ls())
library(tidyverse)
example <- read_delim('../../Umea setup/SC-03-1/export/SC03VFREYNERVE192.b00_001.TXT', 
                      delim = ' ', col_names = FALSE)
example$frame.no <- seq_along(example$X1)
example <- gather(example, 'channel', 'value', -frame.no)
ggplot(example) +
  facet_grid(channel~., scales = 'free_y') +
  geom_line(aes(x = frame.no, y = value, colour = channel)) +
  scale_color_brewer(palette = 'Set1')