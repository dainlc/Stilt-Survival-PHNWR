library(readr)
tunnel_data_2018_to_2020 <- read_csv("Hawaiian Stilt project/R code for processing data/tunnel data 2018 to 2020.csv")
head(tunnel_data_2018_to_2020)

Site <- tunnel_data_2018_to_2020$RefugeUnit
ti.mong <- tunnel_data_2018_to_2020$`TRACKING INDEX (Mongooses)`
ti.rat <- tunnel_data_2018_to_2020$`TRACKING INDEX (Rats)`
ti.mous <- tunnel_data_2018_to_2020$`TRACKING INDEX (Mice)`
t.deplo <- tunnel_data_2018_to_2020$`#Tunnels Deployed`
date <- tunnel_data_2018_to_2020$`Survey Date`

library(lubridate)
date <- mdy(date)


library(ggplot2)
library(dplyr)

tunnel_data_2018_to_2020 %>%
  filter(Site)


tunnel_data_2018_to_2020 %>%
  ggplot(aes(x = date, y = ti.mong , group = Site, linetype = Site)) +
  geom_line()+
  geom_point(aes (shape = Site, size = 1))+
  scale_x_date(labels = date, breaks = date)+
  guides(size = FALSE)+
  ylab ("Tracking Index")+
  xlab ("Date(yyyy-mm-dd)")+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(text = element_text(size = 20), axis.text = element_text(size = 20))+
  theme(legend.text = element_text(size = 15))+
  ggtitle("Mongoose")

tunnel_data_2018_to_2020 %>%
  ggplot(aes(x = date, y = ti.rat , group = Site, linetype = Site)) +
  geom_line()+
  geom_point(aes (shape = Site, size = 1))+
  scale_x_date(labels = date, breaks = date)+
  guides(size = FALSE)+
  ylab ("Tracking Index")+
  xlab ("Date(yyyy-mm-dd)")+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(text = element_text(size = 20), axis.text = element_text(size = 20))+
  ggtitle("Rat")

tunnel_data_2018_to_2020 %>%
  ggplot(aes(x = date, y = ti.mous , group = Site, linetype = Site)) +
  geom_line()+
  geom_point(aes (shape = Site, size = 1))+
  scale_x_date(labels = date, breaks = date)+
  guides(size = FALSE)+
  ylab ("Tracking Index")+
  xlab ("Date(yyyy-mm-dd)")+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(text = element_text(size = 20), axis.text = element_text(size = 20))+
  ggtitle("Mouse")



