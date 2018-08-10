library(tidyverse)
library(cowplot)
 data <- read_csv('/home/pi/speed-camera/speed-cam.csv', 
                  col_names = c('Date','Hour','Minute','Speed','Unit','Image','Loc1','Loc2','Loc3','Loc4','Loc5','Direction'),
                  col_types = cols(Date = col_date("%Y%m%d"))) %>% 
   rowwise() %>% 
   mutate(HMSS = str_split(Image, '-|\\.jpg')[[1]][5]) %>% 
   mutate(HMSS = as.numeric(HMSS)) %>% 
   filter(!grepl('calib', Image)) %>% 
   mutate(Time = paste0(Hour, Minute)) %>% 
   mutate(Time = as.numeric(Time))
 
# # if within 1.9 second, group together
 data$incident <- cumsum(c(1, diff(data$HMSS)) >= 18)
 
 # remove speed over 30, group by within 18 milliseconds
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


# add day of week
data_processed$Day <- weekdays(as.Date(data_processed$Date))
#data_processed <- data_processed %>% mutate(Date = paste(Day, "|", Date))
# mark as weekend or weekday
data_processed <- data_processed %>% mutate(Weekday = case_when(grepl('Sunday|Saturday', Day) ~ 0,
                                                                TRUE ~ 1))

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
  summarise(Speeders = sum(Speed > 25), 
            Speed = mean(Speed),
            Day = max(Day)) %>% 
  ungroup() %>% 
  mutate(Date = paste0(Date, " (", Day, ")"),
	 Date = factor(Date, levels = rev(levels(factor(Date))))) %>% 
  ggplot(aes(x=Hour + 0.5, y=Speed, label = Speeders)) + 
  facet_wrap(~Date, ncol = 1) +
  geom_line() +
  theme_minimal() +
  xlab('Time') + ylab('') +
  scale_color_gradient(low = 'black', high='red') + ggtitle('Average Speed (mph) per Hour')  + 
  scale_x_continuous(breaks=c(0,3,6,9,12,15,18,21,24)) + 
  theme(text = element_text(size=16),
        panel.grid.minor.x = element_blank()) +
  theme(legend.position="none")

# counts cars going over 24 per hour and plot numbers
counts_cars_over_24 <- data_processed %>%
  filter(Date >= (Sys.Date() - 7)) %>% 	
  group_by(Date, Hour) %>% 
  summarise(Speeders = sum(Speed > 24), 
            Speed = mean(Speed),
            Day = max(Day)) %>% 
  ungroup() %>% 
  mutate(Date = paste0(Date, " (", Day, ")"), 
	 Date = factor(Date, levels = rev(levels(factor(Date))))) %>% 
  ggplot(aes(x=Hour, y=Speeders, label = Speeders)) + 
  facet_wrap(~Date, ncol = 1) +
  geom_line() + 
  geom_text(aes(y = Speeders + 5, colour = Speeders)) + 
  coord_cartesian(ylim=c(0,20)) +
  #geom_line() +
  theme_minimal() +
  xlab('Time') + ylab('') +
  scale_color_gradient(low = 'black', high='red') + ggtitle('Number of cars\nover 24 mph by hour')  + 
  scale_x_continuous(breaks=c(0,3,6,9,12,15,18,21,24)) + 
  scale_x_continuous(breaks=c(0,3,6,9,12,15,18,21,24)) + 
  theme(text = element_text(size=16),
        panel.grid.minor = element_blank(),
        axis.text.y=element_blank()) +
  theme(legend.position="none")

# split by dir
c_by_hour_split_dir <- data_processed %>% 
  filter(Date >= (Sys.Date() - 7)) %>% 
  group_by(Date, Hour, Direction) %>% 
  summarise(Count = n(),
            Day = max(Day)) %>% 
  ungroup() %>% 
  mutate(Date = paste0(Date, " (", Day, ")"),
	 Date = factor(Date, levels = rev(levels(factor(Date))))) %>% 
  ggplot(aes(x=Hour + 0.5, y=Count, colour = Direction)) + 
  facet_wrap(~Date, ncol = 1) +
  geom_line() +
  theme_minimal() +
  ggtitle('Count of Vehicles by Hour') +
  xlab('Time') + ylab('') + 
  ggsci::scale_color_lancet() +
  scale_x_continuous(breaks=c(0,3,6,9,12,15,18,21,24)) + 
  theme(text = element_text(size=16),
        panel.grid.minor.x = element_blank())

# # busiest hour
# c_by_hour_split_dir_busiest_3_table <- data_processed %>% 
#   group_by(Date, Hour, Direction) %>% 
#   summarise(Count = n(),
#             Day = max(Day)) %>% 
#   ungroup() %>% 
#   mutate(Date = paste0(Date, " (", Day, ")")) %>% 
#   spread(Direction, Count) %>% 
#   mutate(Sum = Eastward + Westward) %>% 
#   arrange(-Sum) %>% 
#   top_n(3) %>% 
#   mutate(Date = as.character(Date)) %>% 
#   select(Date, Day, Hour, Eastward:Sum) %>% 
#   knitr::kable() %>% 
#   kableExtra::kable_styling()

# split by dir, counts per day
c_by_day_split_dir <- data_processed %>% 
  filter(Date >= (Sys.Date() - 7)) %>% 
  group_by(Date, Direction) %>% 
  summarise(Count = n(),
            Day = max(Day)) %>% 
  ungroup() %>% 
  mutate(Date = paste0(Date, " (", Day, ")"),
	Date = factor(Date, levels = rev(levels(factor(Date))))) %>% 
  ggplot(aes(x=Direction, y=Count, fill = Direction, label=Count)) + 
  facet_wrap(~Date, ncol = 1) +
  geom_bar(stat='identity', position = position_dodge(), width=0.2) +
  geom_text(aes(y=Count+75)) +
  scale_y_continuous(expand=c(0,90)) + 
  theme_minimal() +
  ggtitle('Count of\nVehicles\nby Day') + 
  xlab('Direction') + ylab('') +
  ggsci::scale_fill_lancet() +
  theme(text = element_text(size=16),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust=1), 
        axis.text.y=element_blank())

############################
# average weekday speeds
############################
s_by_hour_weekday <- data_processed %>%
  filter(Weekday == 1) %>% 	
  group_by(Hour) %>% 
  summarise(Speeders = sum(Speed > 25), 
            Speed = mean(Speed),
            Day = max(Day)) %>% 
  ungroup() %>% 
  ggplot(aes(x=Hour + 0.5, y=Speed, label = Speeders)) + 
  geom_line() +
  theme_minimal() +
  xlab('Time') + ylab('') +
  scale_color_gradient(low = 'black', high='red') + ggtitle('Average Speed\n(mph) per Hour')  + 
  scale_x_continuous(breaks=c(0,3,6,9,12,15,18,21,24)) + 
  coord_cartesian(ylim = c(10,25)) +
  theme(text = element_text(size=16),
        panel.grid.minor.x = element_blank()) +
  theme(legend.position="none")

# weekend
s_by_hour_weekend <- data_processed %>%
  filter(Weekday == 0) %>% 	
  group_by(Hour) %>% 
  summarise(Speeders = sum(Speed > 25), 
            Speed = mean(Speed),
            Day = max(Day)) %>% 
  ungroup() %>% 
  ggplot(aes(x=Hour + 0.5, y=Speed, label = Speeders)) + 
  geom_line() +
  theme_minimal() +
  xlab('Time') + ylab('') +
  scale_color_gradient(low = 'black', high='red') + ggtitle('')  + 
  scale_x_continuous(breaks=c(0,3,6,9,12,15,18,21,24)) + 
  coord_cartesian(ylim = c(10,25)) +
  theme(text = element_text(size=16),
        panel.grid.minor.x = element_blank()) +
  theme(legend.position="none")

######################
# average weekday counts
#######################
c_by_hour_split_dir_weekday <- data_processed %>% 
  filter(Weekday == 1) %>% 
  group_by(Date, Hour, Direction) %>% 
  summarise(Count = n()) %>% 
  ungroup() %>% 
  group_by(Hour, Direction) %>% 
  summarise(Count = mean(Count)) %>% 
  ggplot(aes(x=Hour + 0.5, y=Count, colour = Direction)) + 
  geom_line() +
  theme_minimal() +
  ggtitle('Average Count of Vehicles\nby Hour') +
  xlab('Time') + ylab('') + 
  ggsci::scale_color_lancet() +
  scale_x_continuous(breaks=c(0,3,6,9,12,15,18,21,24)) + 
  coord_cartesian(ylim = c(0,60)) +
  theme(text = element_text(size=16),
        panel.grid.minor.x = element_blank())

# weekend
c_by_hour_split_dir_weekend <- data_processed %>% 
  filter(Weekday == 0) %>% 
  group_by(Date, Hour, Direction) %>% 
  summarise(Count = n()) %>% 
  ungroup() %>% 
  group_by(Hour, Direction) %>% 
  summarise(Count = mean(Count)) %>% 
  ggplot(aes(x=Hour + 0.5, y=Count, colour = Direction)) + 
  geom_line() +
  theme_minimal() +
  ggtitle('') +
  xlab('Time') + ylab('') + 
  ggsci::scale_color_lancet() +
  scale_x_continuous(breaks=c(0,3,6,9,12,15,18,21,24)) + 
  coord_cartesian(ylim = c(0,60)) +
  theme(text = element_text(size=16),
        panel.grid.minor.x = element_blank())

########################
# average weekday/end full day counts
########################

# split by dir, counts per average weekday
c_by_day_split_dir_weekday <- data_processed %>% 
  filter(Weekday == 1) %>% 
  group_by(Date, Direction) %>% 
  summarise(Count = n(),
            Day = max(Day)) %>%
  filter(Count > 275) %>% 
  ungroup() %>% 
  group_by(Direction) %>% 
  summarise(Count = round(mean(Count), digits = 0)) %>% 
  ungroup() %>% 
  ggplot(aes(x=Direction, y=Count, fill = Direction, label=Count)) + 
  geom_bar(stat='identity', position = position_dodge(), width=0.2) +
  geom_text(aes(y=Count+75)) +
  scale_y_continuous(expand=c(0,90)) + theme_minimal() +
  ggtitle('Average Count of\nVehicles') + 
  xlab('Direction') + ylab('') +
  ggsci::scale_fill_lancet() +
  theme(text = element_text(size=16),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust=1), 
        panel.grid.minor = element_blank(),
        axis.text.y=element_blank())

# weekend
c_by_day_split_dir_weekend <- data_processed %>% 
  filter(Weekday == 0) %>% 
  group_by(Date, Direction) %>% 
  summarise(Count = n(),
            Day = max(Day)) %>% 
  filter(Count > 275) %>%
  ungroup() %>% 
  group_by(Direction) %>% 
  summarise(Count = round(mean(Count), digits = 0)) %>% 
  ungroup() %>% 
  ggplot(aes(x=Direction, y=Count, fill = Direction, label=Count)) + 
  geom_bar(stat='identity', position = position_dodge(), width=0.2) +
  geom_text(aes(y=Count+75)) +
  scale_y_continuous(expand=c(0,90)) +  
  theme_minimal() +
  ggtitle('') + 
  xlab('Direction') + ylab('') +
  ggsci::scale_fill_lancet() +
  theme(text = element_text(size=16),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust=1), 
        panel.grid.minor = element_blank(),
        axis.text.y=element_blank())
