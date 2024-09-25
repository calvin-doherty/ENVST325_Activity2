install.packages(c("dplyr","lubridate"))
library(dplyr)
library(lubridate)

streamH <- read.csv("/cloud/project/activtiy02/stream_gauge.csv")
siteinfo <- read.csv("/cloud/project/activtiy02/site_info.csv")

# parsing our date
streamH$dateF <- ymd_hm(streamH$datetime, tz="America/New_York")

year(streamH$dateF)

#join site info to stream gauge height
floods <- full_join(streamH, siteinfo, by="siteID")

floods_left <- left_join(streamH, siteinfo, by="siteID")

floods_right <- right_join(streamH, siteinfo, by="siteID")

floods_inner <- inner_join(streamH, siteinfo, by="siteID")

peace <- floods %>%
  filter(siteID == 2295637)

example <- floods %>%
  filter(gheight.ft >= 10)
plot(peace$dateF, peace$gheight.ft, type="l")

max.ht <- floods %>%
  group_by(names) %>%
  summarise(max.ht_ft=max(gheight.ft, na.rm=TRUE),
    mean_ft=mean(gheight.ft, na.rm=TRUE))

# Find the earliest date that each river reached flood stage
earlyDate <- floods %>%
  group_by(names) %>%
  filter(gheight.ft >= flood.ft) %>%
  summarise(min_date=min(dateF, na.rm=TRUE))

#Make plots of stream stage data for each river
Fisheating <- floods[floods$siteID == "2256500",]
Peace <- floods[floods$siteID == "2295637",]
SantaFe <- floods[floods$siteID == "2322500",]
Withla <- floods[floods$siteID == "2312000",]

Fisheating %>%
  ggplot()+
  aes(x=dateF, y=gheight.ft)+
  geom_point()+
  geom_line()+
  labs(x='Date', y='Stream Height (ft)')+
  theme_classic()

Peace %>%
  ggplot()+
  aes(x=dateF, y=gheight.ft)+
  geom_point()+
  geom_line(col='blue')+
  labs(x='Date', y='Stream Height (ft)')+
  theme_classic()

SantaFe %>%
  ggplot()+
  aes(x=dateF, y=gheight.ft)+
  geom_point()+
  geom_line(col='red')+
  labs(x='Date', y='Stream Height (ft)')+
  theme_classic()

Withla %>%
  ggplot()+
  aes(x=dateF, y=gheight.ft)+
  geom_point()+
  geom_line(col='green')+
  labs(x='Date', y='Stream Height (ft)')+
  theme_classic()

# Find the earliest date that each river reached each flood category
earlyDate_action <- floods %>%
  group_by(names) %>%
  filter(gheight.ft >= action.ft) %>%
  summarise(min_date=min(dateF, na.rm=TRUE))

earlyDate_moderate <- floods %>%
  group_by(names) %>%
  filter(gheight.ft >= moderate.ft) %>%
  summarise(min_date=min(dateF, na.rm=TRUE))

earlyDate_major <- floods %>%
  group_by(names) %>%
  filter(gheight.ft >= major.ft) %>%
  summarise(min_date=min(dateF, na.rm=TRUE))

# Find the river that highest stream stage above its listed major.ft
maxdiff_major <- floods %>%
  mutate(difference = (gheight.ft - major.ft)) %>%   
  group_by(names) %>%                     
  summarise(maxdiff = max(difference)) 




