# A script to process and join several datasets together

# 1. Load packages --------------------------------------------------------

library(janitor)
library(lubridate)
library(readxl)
library(plyr)
library(tidyverse)
library(reshape2)
library(data.table)
library(ggplot2)
library(zoo)

# 2. Read in relevant data files ------------------------------------------

data0<-fread("MillerPondABottom_Sep2021_raw_839638/Cat.TXT")
data<-clean_names(data0)
data$serial<-"839638"
data<-data[-1,]
data$temperature<-signif(as.numeric(data$temperature),digits=4)
data$dissolved_oxygen<-signif(as.numeric(data$dissolved_oxygen),digits=3)
data$dissolved_oxygen_saturation<-signif(as.numeric(data$dissolved_oxygen_saturation),digits=3)
data839638<-data

data0<-fread("MillerPondATop_Sep2021_raw_720098/Cat.TXT")
data<-clean_names(data0)
data$serial<-"720098"
data<-data[-1,]
data$temperature<-signif(as.numeric(data$temperature),digits=4)
data$dissolved_oxygen<-signif(as.numeric(data$dissolved_oxygen),digits=3)
data$dissolved_oxygen_saturation<-signif(as.numeric(data$dissolved_oxygen_saturation),digits=3)
data720098<-data

data0<-fread("MillerPondAMiddle_Sep2021_raw_387970/Cat.TXT")
data<-clean_names(data0)
data$serial<-"387970"
data<-data[-1,]
data$temperature<-signif(as.numeric(data$temperature),digits=4)
data$dissolved_oxygen<-signif(as.numeric(data$dissolved_oxygen),digits=3)
data$dissolved_oxygen_saturation<-signif(as.numeric(data$dissolved_oxygen_saturation),digits=3)
data387970<-data

data0<-fread("MillerPondBTop_Sep2021_raw_667163/Cat.TXT")
data<-clean_names(data0)
data$serial<-"667163"
data<-data[-1,]
data$temperature<-signif(as.numeric(data$temperature),digits=4)
data$dissolved_oxygen<-signif(as.numeric(data$dissolved_oxygen),digits=3)
data$dissolved_oxygen_saturation<-signif(as.numeric(data$dissolved_oxygen_saturation),digits=3)
data667163<-data

data0<-fread("MillerPondBMiddle_Sep2021_raw_675903/Cat.TXT")
data<-clean_names(data0)
data$serial<-"675903"
data<-data[-1,]
data$temperature<-signif(as.numeric(data$temperature),digits=4)
data$dissolved_oxygen<-signif(as.numeric(data$dissolved_oxygen),digits=3)
data$dissolved_oxygen_saturation<-signif(as.numeric(data$dissolved_oxygen_saturation),digits=3)
data675903<-data

data0<-fread("MillerPondBBottom_Sep2021_raw_738779/Cat.TXT")
data<-clean_names(data0)
data$serial<-"738779"
data<-data[-1,]
data$temperature<-signif(as.numeric(data$temperature),digits=4)
data$dissolved_oxygen<-signif(as.numeric(data$dissolved_oxygen),digits=3)
data$dissolved_oxygen_saturation<-signif(as.numeric(data$dissolved_oxygen_saturation),digits=3)
data738779<-data

# 3. get bucket data, see Minidot logbook

data839638_bucketbefore<-data839638 %>% filter(central_standard_time>"2021-06-11 14:46:00" & 
                                           central_standard_time<="2021-06-11 14:59:00")

data720098_bucketbefore<-data720098 %>% filter(central_standard_time>"2021-06-11 14:47:00" & 
                                                 central_standard_time<="2021-06-11 15:00:00")

data839638b_bucketbefore<-data839638b %>% filter(central_standard_time>"2021-06-25 11:02:00" & 
                                                 central_standard_time<="2021-06-25 11:13:00")

data720098b_bucketbefore<-data720098b %>% filter(central_standard_time>"2021-06-25 11:02:00" & 
                                                 central_standard_time<="2021-06-25 11:13:00")

data839638c_bucketbefore<-data839638c %>% filter(central_standard_time>"2021-08-01 14:31:00" & 
                                                 central_standard_time<="2021-08-01 14:42:00")

data720098c_bucketbefore<-data720098c %>% filter(central_standard_time>"2021-08-01 14:31:00" & 
                                                 central_standard_time<="2021-08-01 14:42:00")

data839638d_bucketbefore<-data839638d %>% filter(central_standard_time>"2021-08-10 20:42:00" & 
                                                 central_standard_time<="2021-08-10 20:55:00")

data720098d_bucketbefore<-data720098d %>% filter(central_standard_time>"2021-08-10 20:42:00" & 
                                                 central_standard_time<="2021-08-10 20:55:00")

data387970e_bucketbefore<-data387970d %>% filter(central_standard_time>"2021-08-10 20:42:00" & 
                                                 central_standard_time<="2021-08-10 20:55:00")

data839638e_bucketbefore<-data839638e %>% filter(central_standard_time>"2021-08-23 20:09:00" & 
                                                 central_standard_time<="2021-08-23 20:15:00")

data720098e_bucketbefore<-data720098e %>% filter(central_standard_time>"2021-08-23 20:09:00" & 
                                                 central_standard_time<="2021-08-23 20:15:00")

data387970f_bucketbefore<-data387970e %>% filter(central_standard_time>"2021-08-23 20:09:00" & 
                                                 central_standard_time<="2021-08-23 20:15:00")

data738779_bucketbefore<-data738779 %>% filter(central_standard_time>"2021-09-02 19:36:00" & 
                                                 central_standard_time<="2021-09-02 20:23:00")

data675903_bucketbefore<-data675903 %>% filter(central_standard_time>"2021-09-02 19:36:00" & 
                                                 central_standard_time<="2021-09-02 20:23:00")

data667163_bucketbefore<-data667163 %>% filter(central_standard_time>"2021-09-02 19:36:00" & 
                                                 central_standard_time<="2021-09-02 20:23:00")

#data839638_bucketafter<-data839638 %>% filter(central_standard_time>"2021-05-29 15:53:00" & 
#                                                  central_standard_time<="2021-05-29 16:24:00")

#data720098_bucketafter<-data720098 %>% filter(central_standard_time>"2021-05-29 15:53:00" & 
#                                                  central_standard_time<="2021-05-29 16:23:00")

# drop first and last obs

data839638_bucketbefore<-head(data839638_bucketbefore[-c(1:1),],-1)
data720098_bucketbefore<-head(data720098_bucketbefore[-c(1:1),],-1)
data387970_bucketbefore<-head(data387970_bucketbefore[-c(1:1),],-1)
data667163_bucketbefore<-head(data667163_bucketbefore[-c(1:1),],-1)
data675903_bucketbefore<-head(data675903_bucketbefore[-c(1:1),],-1)
data738779_bucketbefore<-head(data738779_bucketbefore[-c(1:1),],-1)
#data839638_bucketafter<-head(data839638_bucketafter[-c(1:1),],-1)
#data720098_bucketafter<-head(data720098_bucketafter[-c(1:1),],-1)

databucketbefore<-rbind(data839638_bucketbefore,data720098_bucketbefore,data387970_bucketbefore)
databucketbefore<-rbind(data667163_bucketbefore,data675903_bucketbefore,data738779_bucketbefore)
#databucketafter<-rbind(data839638_bucketafter,data720098_bucketafter)

# plot bucketdata

bucket_before_wtemp<-ggplot(databucketbefore, 
                            aes(x=strftime(central_standard_time, format = "%H:%M"),
                            temperature,color=serial)) +
  geom_point(alpha=0.5)+
  theme_bw()+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

bucket_before_do<-ggplot(databucketbefore, 
                            aes(x=strftime(central_standard_time, format = "%H:%M"),
                                dissolved_oxygen,color=serial)) +
  geom_point(alpha=0.5)+
  theme_bw()+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))



# 4. Use field data

data839638<-data839638 %>% filter(central_standard_time>"2021-09-01 00:00:00" & 
                                      central_standard_time<="2021-09-15 23:59:00")

data720098<-data720098 %>% filter(central_standard_time>"2021-09-01 00:00:00" & 
                                      central_standard_time<="2021-09-15 23:59:00")

data387970<-data387970 %>% filter(central_standard_time>"2021-09-01 00:00:00" & 
                                      central_standard_time<="2021-09-15 23:59:00")

data667163<-data667163 %>% filter(central_standard_time>"2021-09-03 00:00:00" & 
                                    central_standard_time<="2021-09-15 23:59:00")

data675903<-data675903 %>% filter(central_standard_time>"2021-09-03 00:00:00" & 
                                    central_standard_time<="2021-09-15 23:59:00")

data738779<-data738779 %>% filter(central_standard_time>"2021-09-03 00:00:00" & 
                                    central_standard_time<="2021-09-15 23:59:00")

# drop first and last few obs

data839638<-head(data839638[-c(1:10),],-10)
data720098<-head(data720098[-c(1:10),],-10)
data387970<-head(data387970[-c(1:10),],-10)
data839638$depth<-"bottom"
data387970$depth<-"middle"
data720098$depth<-"top"
databottom<-data839638
datamiddle<-data387970
datatop<-data720098

data667163<-head(data667163[-c(1:10),],-10)
data675903<-head(data675903[-c(1:10),],-10)
data738779<-head(data738779[-c(1:10),],-10)
data738779$depth<-"bottom"
data675903$depth<-"middle"
data667163$depth<-"top"
databottom<-data738779
datamiddle<-data675903
datatop<-data667163

dataAbottom<-rbind(data839638)
dataAmiddle<-rbind(data387970)
dataAtop<-rbind(data720098)
dataAbottom$pond<-dataAmiddle$pond<-dataAtop$pond<-"A"

dataBbottom<-rbind(data738779)
dataBmiddle<-rbind(data675903)
dataBtop<-rbind(data667163)
dataBbottom$pond<-dataBmiddle$pond<-dataBtop$pond<-"B"

dataAbottom$date<-date(dataAbottom$central_standard_time)
dataAtop$date<-date(dataAtop$central_standard_time)
dataAmiddle$date<-date(dataAmiddle$central_standard_time)
dataAbottom$hour<-hour(dataAbottom$central_standard_time)
dataAtop$hour<-hour(dataAtop$central_standard_time)
dataAmiddle$hour<-hour(dataAmiddle$central_standard_time)

dataBbottom$date<-date(dataBbottom$central_standard_time)
dataBtop$date<-date(dataBtop$central_standard_time)
dataBmiddle$date<-date(dataBmiddle$central_standard_time)
dataBbottom$hour<-hour(dataBbottom$central_standard_time)
dataBtop$hour<-hour(dataBtop$central_standard_time)
dataBmiddle$hour<-hour(dataBmiddle$central_standard_time)

data_alldepth<-rbind(dataAtop,dataAbottom,dataAmiddle,
                     dataBtop,dataBbottom,dataBmiddle)
#data_alldepth$depth<-as.factor(data_alldepth$depth)
#data_alldepth$hourdec<-hour(data_alldepth$central_standard_time)+
# minute(data_alldepth$central_standard_time)/60
data_alldepth$datetime<-as.POSIXct(data_alldepth$central_standard_time)

data_alldepth$datehourdec<-paste(data_alldepth$date,data_alldepth$hourdec)

data_hourly<-data_alldepth %>% select(depth,date,hour,temperature,dissolved_oxygen,dissolved_oxygen_saturation) %>% group_by(depth, date,hour) %>%
  dplyr::summarize(temperature=mean(temperature,na.rm=TRUE),
                   dissolved_oxygen=mean(dissolved_oxygen,na.rm=TRUE),
                   dissolved_oxygen_saturation=mean(dissolved_oxygen_saturation,na.rm=TRUE)) %>% 
  data.frame()


# plot field data

data_alldepth$date_text<-as.character(data_alldepth$date)
data_alldepth<-data_alldepth %>% filter(date_text %in% c("2021-09-03","2021-09-04","2021-09-05"))
data_alldepth_wtemp<-ggplot(data_alldepth, 
                            aes(x=datetime,
                                temperature,color=depth)) +
  geom_point(alpha=0.5,size=0.5)+
  ylab("water temperature (deg C)")+
  xlab("time")+
  theme_bw()+
  facet_wrap(~paste("pond",pond),nrow=2)+
  guides(color= guide_legend(override.aes=list(size=5,alpha=1)))
theme(panel.border = element_blank(), panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

data_alldepth_domgl<-ggplot(data_alldepth, 
                            aes(x=datetime,
                                dissolved_oxygen,color=depth)) +
  geom_point(alpha=0.5,size=0.5)+
  ylab("dissolved oxygen (mg/L)")+
  xlab("time")+
  theme_bw()+
  facet_wrap(~paste("pond",pond),nrow=2)+
  guides(color= guide_legend(override.aes=list(size=5,alpha=1)))
theme(panel.border = element_blank(), panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

data_alldepth_dosat<-ggplot(data_alldepth, 
                            aes(x=datetime,
                                dissolved_oxygen_saturation,color=depth)) +
  geom_point(alpha=0.5,size=0.5)+
  ylab("dissolved oxygen (% sat)")+
  xlab("time")+
  theme_bw()+
  facet_wrap(~paste("pond",pond),nrow=2)+
  guides(color= guide_legend(override.aes=list(size=5,alpha=1)))
theme(panel.border = element_blank(), panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))


data_alldepth_wtemp

data_alldepth_domgl

data_alldepth_dosat

png(filename="wtempSepAJASM.png",width = 7, height = 4, units = "in",res=300)
data_alldepth_wtemp
dev.off()

png(filename="domglSepAJASM.png",width = 7, height = 4, units = "in",res=300)
data_alldepth_domgl
dev.off()

png(filename="dosatSepAJASM.png",width = 7, height = 4, units = "in",res=300)
data_alldepth_dosat
dev.off()
