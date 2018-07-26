library(tidyverse)

data <- read_csv('/Volumes/share/pi/speed-camera/speed-cam.csv', 
                 col_names = c('Date','Hour','Minute','Speed','Unit','Image','Loc1','Loc2','Loc3','Loc4','Loc5','Direction'),
                 col_types = cols(Date = col_date("%Y%m%d"))) %>% 
  rowwise() %>% 
  mutate(HMSS = str_split(Image, '-|\\.jpg')[[1]][5]) %>% 
  mutate(HMSS = as.numeric(HMSS)) %>% 
  filter(!grepl('calib', Image)) %>% 
  mutate(Time = paste0(Hour, Minute)) %>% 
  mutate(Time = as.numeric(Time))
  
# if within 1 second, group together
data$incident <- cumsum(c(1, diff(data$HMSS)) >= 10)

data %>% tail()

data %>% filter(Speed < 30) %>% group_by(Date, incident) %>% summarise(Speed = mean(Speed), Time = mean(Time)) %>% 
  ggplot(aes(x=Time, y=Speed)) + facet_wrap(Date) + geom_point()
