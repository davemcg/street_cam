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

data %>% 
  filter(Speed < 30) %>% 
  group_by(Date, incident) %>% 
  summarise(Speed = mean(Speed), 
            Time = mean(Time),
            Hour = min(Hour),
            Direction = names(which.max(table(Direction)))) %>%
  mutate(Direction = case_when(Direction == 'L2R' ~ 'Westward',
                               TRUE ~ 'Eastward')) %>% 
  ungroup() %>% 
  group_by(Date, Hour, Direction) %>% 
  summarise(Count = n()) %>% 
  ggplot(aes(x=Hour, y=Count, colour = Direction)) + 
  facet_wrap(~Date) +
  coord_cartesian(ylim=c(0,65)) +
  geom_step() +
  theme_minimal() +
  ggsci::scale_color_lancet()
