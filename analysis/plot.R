library(tidyverse)
#library(cowplot)
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

data_processed <- data %>% 
  filter(Speed < 30) %>% 
  group_by(Date, incident) %>% 
  summarise(Speed = mean(Speed), 
            Time = mean(Time),
            Hour = min(Hour),
            Direction = names(which.max(table(Direction)))) %>%
  mutate(Direction = case_when(Direction == 'L2R' ~ 'Westward',
                               TRUE ~ 'Eastward')) %>% 
  ungroup() 


# # counts by hour
# c_by_hour <- data_processed %>% 
#   group_by(Date, Hour) %>% 
#   summarise(Count = n()) %>% 
#   ungroup() %>% 
#   ggplot(aes(x=Hour, y=Count)) + 
#   facet_wrap(~Date, ncol = 1) +
#   geom_step() +
#   theme_minimal() +
#   ylab('Count of Vehicles by Hour') +
#   xlab('Time') +
#   ggsci::scale_color_lancet()

# avg speed by hour
s_by_hour <- data_processed %>% 
  group_by(Date, Hour) %>% 
  summarise(Speed = mean(Speed)) %>% 
  ungroup() %>% 
  ggplot(aes(x=Hour, y=Speed)) + 
  facet_wrap(~Date, ncol = 1) +
  geom_step() +
  theme_minimal() +
  xlab('Time') +
  ggsci::scale_color_lancet() + ylab('Average Speed (mph)')

# split by dir
c_by_hour_split_dir <- data_processed %>% 
  group_by(Date, Hour, Direction) %>% 
  summarise(Count = n()) %>% 
  ungroup() %>% 
  ggplot(aes(x=Hour, y=Count, colour = Direction)) + 
  facet_wrap(~Date, ncol = 1) +
  geom_step() +
  theme_minimal() +
  ylab('Count of Vehicles by Hour') +
  xlab('Time') +
  ggsci::scale_color_lancet()

# grab legend
legend_b <- cowplot::get_legend(c_by_hour_split_dir + theme(legend.position="right"))


cowplot::plot_grid(s_by_hour,
                   c_by_hour_split_dir + theme(legend.position = 'none'), 
                   legend_b,
                   ncol=3,
                   rel_widths = c(1,1,0.25))
