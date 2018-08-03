library(tidyverse)
#library(cowplot)
data <- read_csv('/home/pi/speed-camera/speed-cam.csv', 
                 col_names = c('Date','Hour','Minute','Speed','Unit','Image','Loc1','Loc2','Loc3','Loc4','Loc5','Direction'),
                 col_types = cols(Date = col_date("%Y%m%d"))) %>% 
  rowwise() %>% 
  mutate(HMSS = str_split(Image, '-|\\.jpg')[[1]][5]) %>% 
  mutate(HMSS = as.numeric(HMSS)) %>% 
  filter(!grepl('calib', Image)) %>% 
  mutate(Time = paste0(Hour, Minute)) %>% 
  mutate(Time = as.numeric(Time))

# if within 1.9 second, group together
data$incident <- cumsum(c(1, diff(data$HMSS)) >= 18)

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
  filter(Date >= (Sys.Date() - 7)) %>% 	
  group_by(Date, Hour) %>% 
  summarise(Speeders = sum(Speed > 25), Speed = mean(Speed)) %>% 
  ungroup() %>% 
  ggplot(aes(x=Hour, y=Speed, label = Speeders)) + 
  facet_wrap(~Date, ncol = 1) +
  geom_text(aes(y = Speed + 3, colour = Speeders)) +
  geom_line() +
  theme_minimal() +
  xlab('Time') + ylab('') +
  scale_color_gradient(low = 'black', high='red') + ggtitle('Average Speed (mph) per Hour\nCounts are number of cars\nover 25mph') +  + 
  scale_x_continuous(breaks=c(0,3,6,9,12,15,18,21,24)) + 
  theme(text = element_text(size=16),
        panel.grid.minor.x = element_blank()) +
  guides(fill=FALSE)

# split by dir
c_by_hour_split_dir <- data_processed %>% 
  filter(Date >= (Sys.Date() - 7)) %>% 
  group_by(Date, Hour, Direction) %>% 
  summarise(Count = n()) %>% 
  ungroup() %>% 
  ggplot(aes(x=Hour, y=Count, colour = Direction)) + 
  facet_wrap(~Date, ncol = 1) +
  geom_step() +
  theme_minimal() +
  ggtitle('Count of Vehicles by Hour') +
  xlab('Time') + ylab('') + 
  ggsci::scale_color_lancet() +
  scale_x_continuous(breaks=c(0,3,6,9,12,15,18,21,24)) + 
  theme(text = element_text(size=16),
        panel.grid.minor.x = element_blank())

# split by dir, counts per day
c_by_day_split_dir <- data_processed %>% 
  filter(Date >= (Sys.Date() - 7)) %>% 
  group_by(Date, Direction) %>% 
  summarise(Count = n()) %>% 
  ungroup() %>% 
  ggplot(aes(x=Direction, y=Count, fill = Direction, label=Count)) + 
  facet_wrap(~Date, ncol = 1) +
  geom_bar(stat='identity', position = position_dodge(), width=0.2) +
  geom_text(aes(y=Count+75)) +
  theme_minimal() +
  ggtitle('Count of\nVehicles\nby Day') + 
  xlab('Direction') + ylab('') +
  ggsci::scale_fill_lancet() +
  theme(text = element_text(size=16),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust=1), 
        axis.text.y=element_blank())

# grab legend
#legend_b <- cowplot::get_legend(c_by_hour_split_dir + theme(legend.position="right"))


# cowplot::plot_grid(s_by_hour,
#                    c_by_hour_split_dir + theme(legend.position = 'none'),
#                    c_by_day_split_dir + theme(legend.position = 'none'),
#                    ncol=3,
#                    rel_widths = c(1,1,0.6),
#                    align='h')
