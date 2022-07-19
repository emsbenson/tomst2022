library(lubridate)
library(ggplot2)
library(dplyr)

#script to help read in this tomst data

#most recent tomst download
#assumes downloading all data
DirTOMST <- ("/Users/emilybenson/Desktop/campus_weather/TOMST") 

#Need to read in both files for control and removal plots due to sensor swap


Tomstrs <- "refsouth"
Tomstrm <- "refmid"
Tomstrn <- "refnorth"
Tomstss <- "sapsouth"
Tomstsm <- "sapmid"
Tomstsn <- "sapnorth"
Tomstrgs <- "rogerssouth"
Tomstrgm <- "rogersmid"
Tomstrgn <- "rogersnorth"
Tomstags <- "agfieldsouth"
Tomstagm <- "agfieldmid"
Tomstagn <- "agfieldnorth"
Tomstms <- "mowedsouth"
Tomstmm <- "mowedmid"
Tomstmn <- "mowednorth"

# File path for meter data






############################
########## TOMST-----------
#get files

tomstFilesrs <- list.files(paste0(DirTOMST,"/",Tomstrs))
tomstFilesrm <- list.files(paste0(DirTOMST,"/",Tomstrm))
tomstFilesrn <- list.files(paste0(DirTOMST,"/",Tomstrn))
tomstFilesss <- list.files(paste0(DirTOMST,"/",Tomstss))
tomstFilessm <- list.files(paste0(DirTOMST,"/",Tomstsm))
tomstFilessn <- list.files(paste0(DirTOMST,"/",Tomstsn))
tomstFilesrgs <- list.files(paste0(DirTOMST,"/",Tomstrgs))
tomstFilesrgm <- list.files(paste0(DirTOMST,"/",Tomstrgm))
tomstFilesrgn <- list.files(paste0(DirTOMST,"/",Tomstrgn))
tomstFilesags <- list.files(paste0(DirTOMST,"/",Tomstags))
tomstFilesagm <- list.files(paste0(DirTOMST,"/",Tomstagm))
tomstFilesagn <- list.files(paste0(DirTOMST,"/",Tomstagn))
tomstFilesms <- list.files(paste0(DirTOMST,"/",Tomstms))
tomstFilesmm <- list.files(paste0(DirTOMST,"/",Tomstmm))
tomstFilesmn <- list.files(paste0(DirTOMST,"/",Tomstmn))

TOMSTSensor <- data.frame(SN= c(94236488,
                                94236486,
                                94236493,
                                94236483,
                                94214744,
                                94214743,
                                94236481,
                                94236491,
                                94236490),
                          Height = c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0),
                          location=c("reforestation",
                                     "reforestation",
                                     "reforestation",
                                     "sapflow",
                                     "sapflow",
                                     "sapflow",
                                     "rogers",
                                     "rogers",
                                     "rogers",
                                     "agfield",
                                     "agfield",
                                     "agfield",
                                     "rgmowed",
                                     "rgmowed",
                                     "rgmowed"),
                          timeP = c(2,2,2,1,1,2,2,1,1,1,1,1,1,1,1))


#blank column in new firmware data
#read in files
TMSrs <-  read.csv(paste0(DirTOMST,"/",Tomstrs, "/",tomstFilesrs),
                    sep=";",header=FALSE)[,1:9]
TMSrm <-  read.csv(paste0(DirTOMST,"/",Tomstrm, "/",tomstFilesrm),
                    sep=";",header=FALSE)[,1:9]
TMSrn <-  read.csv(paste0(DirTOMST,"/",Tomstrn, "/",tomstFilesrn),
                    sep=";",header=FALSE)[,1:9]

TMSss <-  read.csv(paste0(DirTOMST,"/",Tomstss, "/",tomstFilesss),
                    sep=";",header=FALSE)
TMSsm <-  read.csv(paste0(DirTOMST,"/",Tomstsm, "/",tomstFilessm),
                    sep=";",header=FALSE)
TMSsn <-  read.csv(paste0(DirTOMST,"/",Tomstsn, "/",tomstFilessn),
                    sep=";",header=FALSE)
TMSrgs <-  read.csv(paste0(DirTOMST,"/",Tomstrgs, "/",tomstFilesrgs),
                    sep=";",header=FALSE)
TMSrgm <-  read.csv(paste0(DirTOMST,"/",Tomstrgm, "/",tomstFilesrgm),
                    sep=";",header=FALSE)
TMSrgn <-  read.csv(paste0(DirTOMST,"/",Tomstrgn, "/",tomstFilesrgn),
                    sep=";",header=FALSE)
TMSms <-  read.csv(paste0(DirTOMST,"/",Tomstms, "/",tomstFilesms),
                   sep=";",header=FALSE)
TMSmm <-  read.csv(paste0(DirTOMST,"/",Tomstmm, "/",tomstFilesmm),
                   sep=";",header=FALSE)
TMSmn <-  read.csv(paste0(DirTOMST,"/",Tomstmn, "/",tomstFilesmn),
                   sep=";",header=FALSE)
TMSags <-  read.csv(paste0(DirTOMST,"/",Tomstags, "/",tomstFilesags),
                    sep=";",header=FALSE)
TMSagm <-  read.csv(paste0(DirTOMST,"/",Tomstagm, "/",tomstFilesagm),
                    sep=";",header=FALSE)
TMSagn <-  read.csv(paste0(DirTOMST,"/",Tomstagn, "/",tomstFilesagn),
                    sep=";",header=FALSE)

#tms temps:  -6, +2 and +15cm
TMScols <- c("record","date","tz","Tm6","T2","T15","SM","shake","errFlag")

colnames(TMSrs) <- TMScols
colnames(TMSrm) <- TMScols
colnames(TMSrn) <- TMScols
colnames(TMSss) <- TMScols
colnames(TMSsm) <- TMScols
colnames(TMSsn) <- TMScols
colnames(TMSrgs) <- TMScols
colnames(TMSrgm) <- TMScols
colnames(TMSrgn) <- TMScols
colnames(TMSms) <- TMScols
colnames(TMSmm) <- TMScols
colnames(TMSmn) <- TMScols
colnames(TMSags) <- TMScols
colnames(TMSagm) <- TMScols
colnames(TMSagn) <- TMScols

#new tomst sensors use commas instead of periods for decimal

TMSrs$Tm6 <- as.numeric(gsub("\\,","\\.",TMSrs$Tm6))
TMSrs$T2 <- as.numeric(gsub("\\,","\\.",TMSrs$T2))
TMSrs$T15 <- as.numeric(gsub("\\,","\\.",TMSrs$T15))

TMSrm$Tm6 <- as.numeric(gsub("\\,","\\.",TMSrm$Tm6))
TMSrm$T2 <- as.numeric(gsub("\\,","\\.",TMSrm$T2))
TMSrm$T15 <- as.numeric(gsub("\\,","\\.",TMSrm$T15))

TMSrn$Tm6 <- as.numeric(gsub("\\,","\\.",TMSrn$Tm6))
TMSrn$T2 <- as.numeric(gsub("\\,","\\.",TMSrn$T2))
TMSrn$T15 <- as.numeric(gsub("\\,","\\.",TMSrn$T15))

TMSss$Tm6 <- as.numeric(gsub("\\,","\\.",TMSss$Tm6))
TMSss$T2 <- as.numeric(gsub("\\,","\\.",TMSss$T2))
TMSss$T15 <- as.numeric(gsub("\\,","\\.",TMSss$T15))

TMSsm$Tm6 <- as.numeric(gsub("\\,","\\.",TMSsm$Tm6))
TMSsm$T2 <- as.numeric(gsub("\\,","\\.",TMSsm$T2))
TMSsm$T15 <- as.numeric(gsub("\\,","\\.",TMSsm$T15))

TMSsn$Tm6 <- as.numeric(gsub("\\,","\\.",TMSsn$Tm6))
TMSsn$T2 <- as.numeric(gsub("\\,","\\.",TMSsn$T2))
TMSsn$T15 <- as.numeric(gsub("\\,","\\.",TMSsn$T15))

TMSrgs$Tm6 <- as.numeric(gsub("\\,","\\.",TMSrgs$Tm6))
TMSrgs$T2 <- as.numeric(gsub("\\,","\\.",TMSrgs$T2))
TMSrgs$T15 <- as.numeric(gsub("\\,","\\.",TMSrgs$T15))

TMSrgm$Tm6 <- as.numeric(gsub("\\,","\\.",TMSrgm$Tm6))
TMSrgm$T2 <- as.numeric(gsub("\\,","\\.",TMSrgm$T2))
TMSrgm$T15 <- as.numeric(gsub("\\,","\\.",TMSrgm$T15))

TMSrgn$Tm6 <- as.numeric(gsub("\\,","\\.",TMSrgn$Tm6))
TMSrgn$T2 <- as.numeric(gsub("\\,","\\.",TMSrgn$T2))
TMSrgn$T15 <- as.numeric(gsub("\\,","\\.",TMSrgn$T15))

TMSms$Tm6 <- as.numeric(gsub("\\,","\\.",TMSms$Tm6))
TMSms$T2 <- as.numeric(gsub("\\,","\\.",TMSms$T2))
TMSms$T15 <- as.numeric(gsub("\\,","\\.",TMSms$T15))

TMSmm$Tm6 <- as.numeric(gsub("\\,","\\.",TMSmm$Tm6))
TMSmm$T2 <- as.numeric(gsub("\\,","\\.",TMSms$T2))
TMSmm$T15 <- as.numeric(gsub("\\,","\\.",TMSmm$T15))

TMSmn$Tm6 <- as.numeric(gsub("\\,","\\.",TMSmn$Tm6))
TMSmn$T2 <- as.numeric(gsub("\\,","\\.",TMSmn$T2))
TMSmn$T15 <- as.numeric(gsub("\\,","\\.",TMSmn$T15))

TMSags$Tm6 <- as.numeric(gsub("\\,","\\.",TMSags$Tm6))
TMSags$T2 <- as.numeric(gsub("\\,","\\.",TMSags$T2))
TMSags$T15 <- as.numeric(gsub("\\,","\\.",TMSags$T15))

TMSagm$Tm6 <- as.numeric(gsub("\\,","\\.",TMSagm$Tm6))
TMSagm$T2 <- as.numeric(gsub("\\,","\\.",TMSagm$T2))
TMSagm$T15 <- as.numeric(gsub("\\,","\\.",TMSagm$T15))

TMSagn$Tm6 <- as.numeric(gsub("\\,","\\.",TMSagn$Tm6))
TMSagn$T2 <- as.numeric(gsub("\\,","\\.",TMSagn$T2))
TMSagn$T15 <- as.numeric(gsub("\\,","\\.",TMSagn$T15))



TMSrs$dateF <- ymd_hm(TMSrs$date)
TMSrs$estD <- with_tz(TMSrs$dateF,tzone="America/New_York" )

TMSrm$dateF <- ymd_hm(TMSrm$date)
TMSrm$estD <- with_tz(TMSrm$dateF,tzone="America/New_York" )

TMSrn$dateF <- ymd_hm(TMSrn$date)
TMSrn$estD <- with_tz(TMSrn$dateF,tzone="America/New_York" )

TMSss$dateF <- ymd_hm(TMSss$date)
TMSss$estD <- with_tz(TMSss$dateF,tzone="America/New_York" )

TMSsm$dateF <- ymd_hm(TMSsm$date)
TMSsm$estD <- with_tz(TMSsm$dateF,tzone="America/New_York" )

TMSsn$dateF <- ymd_hm(TMSsn$date)
TMSsn$estD <- with_tz(TMSsn$dateF,tzone="America/New_York" )

TMSrgs$dateF <- ymd_hm(TMSrgs$date)
TMSrgs$estD <- with_tz(TMSrgs$dateF,tzone="America/New_York" )

TMSrgm$dateF <- ymd_hm(TMSrgm$date)
TMSrgm$estD <- with_tz(TMSrgm$dateF,tzone="America/New_York" )

TMSrgn$dateF <- ymd_hm(TMSrgn$date)
TMSrgn$estD <- with_tz(TMSrgn$dateF,tzone="America/New_York" )

TMSms$dateF <- ymd_hm(TMSms$date)
TMSms$estD <- with_tz(TMSms$dateF,tzone="America/New_York" )

TMSmm$dateF <- ymd_hm(TMSmm$date)
TMSmm$estD <- with_tz(TMSmm$dateF,tzone="America/New_York" )

TMSmn$dateF <- ymd_hm(TMSmn$date)
TMSmn$estD <- with_tz(TMSmn$dateF,tzone="America/New_York" )

TMSags$dateF <- ymd_hm(TMSags$date)
TMSags$estD <- with_tz(TMSags$dateF,tzone="America/New_York" )

TMSagm$dateF <- ymd_hm(TMSagm$date)
TMSagm$estD <- with_tz(TMSagm$dateF,tzone="America/New_York" )

TMSagn$dateF <- ymd_hm(TMSagn$date)
TMSagn$estD <- with_tz(TMSagn$dateF,tzone="America/New_York" )

#separate out year and day number from estD
TMSrs$year <- year(TMSrs$estD)
TMSrs$doy <-  yday(TMSrs$estD)
TMSrs$DD <- yday(TMSrs$estD) + ((hour(TMSrs$estD)+(minute(TMSrs$estD)/60))/24)

TMSrm$year <- year(TMSrm$estD)
TMSrm$doy <-  yday(TMSrm$estD)
TMSrm$DD <- yday(TMSrm$estD) + ((hour(TMSrm$estD)+(minute(TMSrm$estD)/60))/24)

TMSrn$year <- year(TMSrn$estD)
TMSrn$doy <-  yday(TMSrn$estD)
TMSrn$DD <- yday(TMSrn$estD) + ((hour(TMSrn$estD)+(minute(TMSrn$estD)/60))/24)

TMSss$year <- year(TMSss$estD)
TMSss$doy <-  yday(TMSss$estD)
TMSss$DD <- yday(TMSss$estD) + ((hour(TMSss$estD)+(minute(TMSss$estD)/60))/24)

TMSsm$year <- year(TMSsm$estD)
TMSsm$doy <-  yday(TMSsm$estD)
TMSsm$DD <- yday(TMSsm$estD) + ((hour(TMSsm$estD)+(minute(TMSsm$estD)/60))/24)

TMSsn$year <- year(TMSsn$estD)
TMSsn$doy <-  yday(TMSsn$estD)
TMSsn$DD <- yday(TMSsn$estD) + ((hour(TMSsn$estD)+(minute(TMSsn$estD)/60))/24)

TMSrgs$year <- year(TMSrgs$estD)
TMSrgs$doy <-  yday(TMSrgs$estD)
TMSrgs$DD <- yday(TMSrgs$estD) + ((hour(TMSrgs$estD)+(minute(TMSrgs$estD)/60))/24)

TMSrgm$year <- year(TMSrgm$estD)
TMSrgm$doy <-  yday(TMSrgm$estD)
TMSrgm$DD <- yday(TMSrgm$estD) + ((hour(TMSrgm$estD)+(minute(TMSrgm$estD)/60))/24)

TMSrgn$year <- year(TMSrgn$estD)
TMSrgn$doy <-  yday(TMSrgn$estD)
TMSrgn$DD <- yday(TMSrgn$estD) + ((hour(TMSrgn$estD)+(minute(TMSrgn$estD)/60))/24)

TMSms$year <- year(TMSms$estD)
TMSms$doy <-  yday(TMSms$estD)
TMSms$DD <- yday(TMSms$estD) + ((hour(TMSms$estD)+(minute(TMSms$estD)/60))/24)

TMSmm$year <- year(TMSmm$estD)
TMSmm$doy <-  yday(TMSmm$estD)
TMSmm$DD <- yday(TMSmm$estD) + ((hour(TMSmm$estD)+(minute(TMSmm$estD)/60))/24)

TMSmn$year <- year(TMSmn$estD)
TMSmn$doy <-  yday(TMSmn$estD)
TMSmn$DD <- yday(TMSmn$estD) + ((hour(TMSmn$estD)+(minute(TMSmn$estD)/60))/24)

TMSags$year <- year(TMSags$estD)
TMSags$doy <-  yday(TMSags$estD)
TMSags$DD <- yday(TMSags$estD) + ((hour(TMSags$estD)+(minute(TMSags$estD)/60))/24)

TMSagm$year <- year(TMSagm$estD)
TMSagm$doy <-  yday(TMSagm$estD)
TMSagm$DD <- yday(TMSagm$estD) + ((hour(TMSagm$estD)+(minute(TMSagm$estD)/60))/24)

TMSagn$year <- year(TMSagn$estD)
TMSagn$doy <-  yday(TMSagn$estD)
TMSagn$DD <- yday(TMSagn$estD) + ((hour(TMSagn$estD)+(minute(TMSagn$estD)/60))/24)

#omit error flag data
#TMSss <- TMSss[TMSss$errFlag != 16, ]
#TMSsm <- TMSsm[TMSsm$errFlag != 16, ]

# omit data before new sensors were deployed
# original sensors set up at 7/2 10: 25
# but animal destruction impacted removal early on Subset to day where fixed

#TMSss <- TMSss[TMSss$estD >= "2021-07-08 00:00:00",]
#TMSsm <- TMSsm[TMSsm$estD >= "2021-07-08 00:00:00",]

#TMSrm <- TMSrm[TMSrm$estD >= "2021-08-03 10:15:00",]
#TMSrn <- TMSrn[TMSrn$estD >= "2021-08-03 10:15:00",]


#omit weird na columns
TMSss <- subset(TMSss, select = -10)
TMSsm <- subset(TMSsm, select = -10)
TMSsn <- subset(TMSsn, select = -10)
TMSrgs <- subset(TMSrgs, select = -10)
TMSrgm <- subset(TMSrgm, select = -10)
TMSrgn <- subset(TMSrgn, select = -10)
TMSms <- subset(TMSms, select = -10)
TMSmm <- subset(TMSmm, select = -10)
TMSmn <- subset(TMSmn, select = -10)
TMSags <- subset(TMSags, select = -10)
TMSagm <- subset(TMSagm, select = -10)
TMSagn <- subset(TMSagn, select = -10)

#start when we put in sensors
TMSrs <- TMSrs %>%
  filter(doy >= 163)

TMSrm <- TMSrm %>%
  filter(doy >= 163)

TMSrn <- TMSrn %>%
  filter(doy >= 183)

TMSss <- TMSss %>%
  filter(doy >= 160)

TMSsm <- TMSsm[TMSsm$doy >= 160 & TMSsm$year == 2022,]

TMSsn <- TMSsn[TMSsn$doy >= 160 & TMSsn$year == 2022,]

TMSrgs <- TMSrgs %>%
  filter(doy >= 165)

TMSrgm <- TMSrgm %>%
  filter(doy >= 165)

TMSrgn <- TMSrgn %>%
  filter(doy >= 165)

TMSrgm <- TMSrgm[TMSrgm$doy != 177,]

TMSags <- TMSags %>%
  filter(doy >= 183)

TMSagm <- TMSagm %>%
  filter(doy >= 183)

TMSagn <- TMSagn %>%
  filter(doy >= 183)

TMSms <- TMSags %>%
  filter(doy >= 187)

TMSmm <- TMSagm %>%
  filter(doy >= 187)

TMSmn <- TMSagn %>%
  filter(doy >= 187)


#individual plots

#ggplot(TMSrs, aes(x = DD, y = SM, color = location))+
  geom_line(aes(col = "soil moisture raw"))+
  ggtitle("Soil Moisture Levels (Raw)")+
  xlab("Date")+
  ylab("SM")


reforestation <- rbind(TMSrs, TMSrm, TMSrn)
reforestation$site <- rep("reforestation", nrow(reforestation))
sapflow <- rbind(TMSss,TMSsm, TMSsn)
sapflow$site <- rep("sapflow", nrow(sapflow))
rogersglen <- rbind(TMSrgs, TMSrgm, TMSrgn)
rogersglen$site <- rep("rogersglen", nrow(rogersglen))
mowed <- rbind(TMSms, TMSmm, TMSmn)
mowed$site <- rep("mowed", nrow(mowed))
agfield <- rbind(TMSags, TMSagm, TMSagn)
agfield$site <- rep("agfield", nrow(agfield))

reforestation$SM.cor <- (-0.00000002*(reforestation$SM^2)) + (0.0003*reforestation$SM) -0.2062
sapflow$SM.cor <- (-0.00000002*(sapflow$SM^2)) + (0.0003*sapflow$SM) -0.2062
rogersglen$SM.cor <- (-0.00000002*(rogersglen$SM^2)) + (0.0003*rogersglen$SM) -0.2062
mowed$SM.cor <- (-0.00000002*(mowed$SM^2)) + (0.0003*mowed$SM) -0.2062
agfield$SM.cor <- (-0.00000002*(agfield$SM^2)) + (0.0003*agfield$SM) -0.2062

soilAll <- rbind(reforestation, sapflow, rogersglen, mowed, agfield)

soilAvg <- soilAll %>%
  group_by(DD, site) %>%
  summarise(mean.sm = mean(SM),
            se = sd(SM)/sqrt(length(SM)))

#together by location
ggplot(sapflow, aes(x = DD, y = SM, color = location))+
  geom_line(data = TMSss, aes(col = "cadetblue3"))+
  geom_line(data = TMSsm, aes(col = "indianred1"))+
  geom_line(data =  TMSsn, aes(col = "dark green"))+
  scale_color_identity(name = "Sensor Location",
                       labels = c("South", "North", "Middle"),
                       guide = "legend")+
  ggtitle("Soil Moisture Levels in Sapflux Site (Raw)")+
  xlab("Date")+
  ylab("SM")
#average
ggplot(sapflow, aes(x = DD, y = SM, color = "cadetblue4"))+
  stat_summary(geom = "line", fun = mean)+
  scale_color_identity(name = "Sensor Location",
                       guide = "legend")+
  ggtitle("Soil Moisture Levels in Sapflux Site (Raw)")+
  xlab("Date")+
  ylab("SM")

ggplot(rogersglen, aes(x = DD, y = SM, color = location))+
  geom_line(data = TMSrgs, aes(col = "cadetblue3"))+
  geom_line(data = TMSrgm, aes(col = "indianred1"))+
  geom_line(data = TMSrgn, aes(col = "dark green"))+
  scale_color_identity(name = "Sensor Location",
                       labels = c("South", "North", "Middle"),
                       guide = "legend")+
  ggtitle("Soil Moisture Levels in Rogers Glen (Raw)")+
  xlab("Date")+
  ylab("SM")

#average
ggplot(rogersglen, aes(x = DD, y = SM, color = "olivedrab4"))+
  stat_summary(geom = "line", fun = mean)+
  scale_color_identity(name = "Sensor",
                       labels = "Sensor")+
  ggtitle("Soil Moisture Levels in Rogers Glen Site (Raw)")+
  xlab("Date")+
  ylab("SM")


ggplot(reforestation, aes(x = DD, y = SM, color = location))+
  geom_line(data = TMSrm, aes(col = "indianred1"))+
  geom_line(data = TMSrn, aes(col = "dark green"))+
  geom_line(data = TMSrs, aes(col = "dark blue"))+
  scale_color_identity(name = "Sensor Location",
                       labels = c("South", "North", "Middle"),
                       guide = "legend")+
  ggtitle("Soil Moisture Levels in Reforestation Site (Raw)")+
  xlab("Date")+
  ylab("SM")

#average
ggplot(reforestation, aes(x = DD, y = SM, color = "darkred"))+
  stat_summary(geom = "line", fun = mean)+
  scale_color_identity(name = "Sensor",
                       labels = "Sensor")+
  ggtitle("Soil Moisture Levels in Reforestation Site (Raw)")+
  xlab("Date")+
  ylab("SM")


ggplot(mowed, aes(x = DD, y = SM, color = location))+
  geom_line(data = TMSmm, aes(col = "indianred1"))+
  geom_line(data = TMSmn, aes(col = "dark green"))+
  geom_line(data = TMSms, aes(col = "dark blue"))+
  scale_color_identity(name = "Sensor Location",
                       labels = c("South", "North", "Middle"),
                       guide = "legend")+
  ggtitle("Soil Moisture Levels in Mowed Area (Raw)")+
  xlab("Date")+
  ylab("SM")

#average

ggplot(mowed, aes(x = DD, y = SM, color = "darkgreen"))+
  stat_summary(geom = "line", fun = mean)+
  scale_color_identity(name = "Sensor",
                       labels = "Sensor")+
  ggtitle("Soil Moisture Levels in Mowed Site (Raw)")+
  xlab("Date")+
  ylab("SM")


ggplot(agfield, aes(x = DD, y = SM, color = location))+
  geom_line(data = TMSagm, aes(col = "indianred1"))+
  geom_line(data = TMSagn, aes(col = "dark green"))+
  geom_line(data = TMSags, aes(col = "dark blue"))+
  scale_color_identity(name = "Sensor Location",
                       labels = c("South", "North", "Middle"),
                       guide = "legend")+
  ggtitle("Soil Moisture Levels in Field Site (Raw)")+
  xlab("Date")+
  ylab("SM")

#average
ggplot(agfield, aes(x = DD, y = SM, color = "darkmagenta"))+
  stat_summary(geom = "line", fun = mean)+
  scale_color_identity(name = "Sensor",
                       labels = "Sensor")+
  ggtitle("Soil Moisture Levels in Rogers Field Site (Raw)")+
  xlab("Date")+
  ylab("SM")


#geom_point(data = TMSrs, aes(col = "cadetblue3"))+
  #geom_line(data = TMSrs, aes(col = "cadetblue3"))+


#sapflow info
ggplot(sapflow, aes(x = DD, y = Tm6, color = Legend))+
  geom_line(data = TMSsm, aes(col = "chocolate4"))+
  geom_line(data =  TMSss, aes(col = "coral1"))+
  geom_line(data = TMSsn, aes(col = "darkolivegreen"))+
  scale_color_identity(name = "Sensor Location",
                       labels = c("Middle", "South", "North"),
                       guide = "legend")+
  ggtitle("Temp Levels 6 inches Below in Sapflow Site")+
  xlab("Day of Year")+
  ylab("TEMP (C)")

ggplot(sapflow, aes(x = DD, y = T2, color = Legend))+
  geom_line(data = TMSsm, aes(col = "chocolate4"))+
  geom_line(data =  TMSss, aes(col = "coral1"))+
  geom_line(data = TMSsn, aes(col = "darkolivegreen"))+
  scale_color_identity(name = "Sensor Location",
                       labels = c("Middle", "South", "North"),
                       guide = "legend")+
  ggtitle("Temp Levels 2 inches Above Surface in Sapflow Site")+
  xlab("Day of Year")+
  ylab("TEMP (C)")

ggplot(sapflow, aes(x = DD, y = T15, color = Legend))+
  geom_line(data = TMSsm, aes(col = "chocolate4"))+
  geom_line(data =  TMSss, aes(col = "coral1"))+
  geom_line(data = TMSsn, aes(col = "darkolivegreen"))+
  scale_color_identity(name = "Sensor Location",
                       labels = c("Middle", "South", "North"),
                       guide = "legend")+
  ggtitle("Temp Levels 15 Inches Above Surface in Sapflow Site")+
  xlab("Day of Year")+
  ylab("TEMP (C)")

#rogers glen info
ggplot(rogersglen, aes(x = DD, y = Tm6, color = Legend))+
  geom_line(data = TMSrgm, aes(col = "darkorchid"))+
  geom_line(data =  TMSrgs, aes(col = "sea green"))+
  geom_line(data = TMSrgn, aes(col = "aquamarine3"))+
  scale_color_identity(name = "Sensor Location",
                       labels = c("North", "Middle", "South"),
                       guide = "legend")+
  ggtitle("Temp Levels 6 inches Below Surface in Rogers Glen")+
  xlab("Day of Year")+
  ylab("TEMP (C)")

ggplot(rogersglen, aes(x = DD, y = T2, color = Legend))+
  geom_line(data = TMSrgm, aes(col = "darkorchid"))+
  geom_line(data =  TMSrgs, aes(col = "sea green"))+
  geom_line(data = TMSrgn, aes(col = "aquamarine3"))+
  scale_color_identity(name = "Sensor Location",
                       labels = c("North", "Middle", "South"),
                       guide = "legend")+
  ggtitle("Temp Levels 2 inches Above Surface in Rogers Glen")+
  xlab("Day of Year")+
  ylab("TEMP (C)")

ggplot(rogersglen, aes(x = DD, y = T15, color = Legend))+
  geom_line(data = TMSrgm, aes(col = "darkorchid"))+
  geom_line(data =  TMSrgs, aes(col = "sea green"))+
  geom_line(data = TMSrgn, aes(col = "aquamarine3"))+
  scale_color_identity(name = "Sensor Location",
                       labels = c("North", "Middle", "South"),
                       guide = "legend")+
  ggtitle("Temp Levels 15 inches Above Surface in Rogers Glen")+
  xlab("Day of Year")+
  ylab("TEMP (C)")

#reforestation info

ggplot(reforestation, aes(x = DD, y = T15, color = Legend))+
  geom_line(data = TMSrm, aes(col = "magenta2"))+
  geom_line(data =  TMSrs, aes(col = "mediumblue"))+
  geom_line(data = TMSrn, aes(col = "maroon4"))+
  scale_color_identity(name = "Sensor Location",
                       labels = c("Middle", "North", "South"),
                       guide = "legend")+
  ggtitle("Temp Levels 15 inches Above Surface in Reforestation Site")+
  xlab("Day of Year")+
  ylab("TEMP (C)")

ggplot(reforestation, aes(x = DD, y = T2, color = Legend))+
  geom_line(data = TMSrm, aes(col = "magenta2"))+
  geom_line(data =  TMSrs, aes(col = "mediumblue"))+
  geom_line(data = TMSrn, aes(col = "maroon4"))+
  scale_color_identity(name = "Sensor Location",
                       labels = c("Middle", "North", "South"),
                       guide = "legend")+
  ggtitle("Temp Levels 2 inches Above Surface in Reforestation Site")+
  xlab("Day of Year")+
  ylab("TEMP (C)")

ggplot(reforestation, aes(x = DD, y = Tm6, color = Legend))+
  geom_line(data = TMSrm, aes(col = "magenta2"))+
  geom_line(data =  TMSrs, aes(col = "mediumblue"))+
  geom_line(data = TMSrn, aes(col = "maroon4"))+
  scale_color_identity(name = "Sensor Location",
                       labels = c("Middle", "North", "South"),
                       guide = "legend")+
  ggtitle("Temp Levels 6 inches Below Surface in Reforestation Site")+
  xlab("Day of Year")+
  ylab("TEMP (C)")

#mowed info

ggplot(mowed, aes(x = DD, y = Tm6, color = Legend))+
  geom_line(data = TMSsm, aes(col = "chocolate4"))+
  geom_line(data =  TMSss, aes(col = "coral1"))+
  geom_line(data = TMSsn, aes(col = "darkolivegreen"))+
  scale_color_identity(name = "Sensor Location",
                       labels = c("Middle", "South", "North"),
                       guide = "legend")+
  ggtitle("Temp Levels 6 inches Below in Mowed Site")+
  xlab("Day of Year")+
  ylab("TEMP (C)")

ggplot(mowed, aes(x = DD, y = T2, color = Legend))+
  geom_line(data = TMSsm, aes(col = "chocolate4"))+
  geom_line(data =  TMSss, aes(col = "coral1"))+
  geom_line(data = TMSsn, aes(col = "darkolivegreen"))+
  scale_color_identity(name = "Sensor Location",
                       labels = c("Middle", "South", "North"),
                       guide = "legend")+
  ggtitle("Temp Levels 2 inches Above Surface in Mowed Site")+
  xlab("Day of Year")+
  ylab("TEMP (C)")

ggplot(mowed, aes(x = DD, y = T15, color = Legend))+
  geom_line(data = TMSsm, aes(col = "chocolate4"))+
  geom_line(data =  TMSss, aes(col = "coral1"))+
  geom_line(data = TMSsn, aes(col = "darkolivegreen"))+
  scale_color_identity(name = "Sensor Location",
                       labels = c("Middle", "South", "North"),
                       guide = "legend")+
  ggtitle("Temp Levels 15 Inches Above Surface in Mowed Site")+
  xlab("Day of Year")+
  ylab("TEMP (C)")

#agfield info

ggplot(agfield, aes(x = DD, y = Tm6, color = Legend))+
  geom_line(data = TMSsm, aes(col = "chocolate4"))+
  geom_line(data =  TMSss, aes(col = "coral1"))+
  geom_line(data = TMSsn, aes(col = "darkolivegreen"))+
  scale_color_identity(name = "Sensor Location",
                       labels = c("Middle", "South", "North"),
                       guide = "legend")+
  ggtitle("Temp Levels 6 inches Below in Field Site")+
  xlab("Day of Year")+
  ylab("TEMP (C)")

ggplot(agfield, aes(x = DD, y = T2, color = Legend))+
  geom_line(data = TMSsm, aes(col = "chocolate4"))+
  geom_line(data =  TMSss, aes(col = "coral1"))+
  geom_line(data = TMSsn, aes(col = "darkolivegreen"))+
  scale_color_identity(name = "Sensor Location",
                       labels = c("Middle", "South", "North"),
                       guide = "legend")+
  ggtitle("Temp Levels 2 inches Above Surface in Field Site")+
  xlab("Day of Year")+
  ylab("TEMP (C)")

ggplot(agfield, aes(x = DD, y = T15, color = Legend))+
  geom_line(data = TMSsm, aes(col = "chocolate4"))+
  geom_line(data =  TMSss, aes(col = "coral1"))+
  geom_line(data = TMSsn, aes(col = "darkolivegreen"))+
  scale_color_identity(name = "Sensor Location",
                       labels = c("Middle", "South", "North"),
                       guide = "legend")+
  ggtitle("Temp Levels 15 Inches Above Surface in Field Site")+
  xlab("Day of Year")+
  ylab("TEMP (C)")

#adding weather?

DirMeter <- ("/Users/emilybenson/Desktop/campus_weather/METER/weatherjuly22.csv")

meterTable <- read.csv(paste0(DirMeter), skip=3,header=FALSE)




colnames(meterTable) <- c("Date","SolRad","Precip","LightningAct","LightningDist","WindDir","WindSpeed",
                          "GustSpeed","AirTemp","VaporPr","AtmosPr","XLevel","YLevel","MaxPrecip",
                          "SensorTemp","VPD","BatPct","BatVolt","RefPr","LogTemp")


#set up day of year
dateForm <-  ymd_hms(meterTable$Date, tz="America/New_York")

meterTable$year <- year(dateForm) 
meterTable$doy <- yday(dateForm)
meterTable$hour <- hour(dateForm)
meterTable$minute <- minute(dateForm)
meterTable$time <- hour(dateForm)+(minute(dateForm)/60)
meterTable$DD <- meterTable$doy + (meterTable$time/24) 
meterTable$DY <- round(meterTable$year+((meterTable$DD-1)/ifelse(leap_year(meterTable$year),366,365)),6)



MeterMeta <- data.frame(name = c("Date","SolRad","Precip","LightningAct","LightningDist","WindDir","WindSpeed",
                                 "GustSpeed","AirTemp","VaporPr","AtmosPr","XLevel","YLevel","MaxPrecip",
                                 "SensorTemp","VPD","BatPct","BatVolt","RefPr","LogTemp"),
                        units = c("MM/DD/YYYY HH:MM",
                                  "W/m^2","mm","NA","km","degree","m/s","m/s","C",
                                  "kPa","kPa","degree","degree","mm/h","C","kPa","%","mV","kPa","C"))

meterTable2 <- meterTable[meterTable$doy >= 160 & meterTable$year == 2022,]

meterTable2 <- na.omit(meterTable2)

outDir <- "/Users/emilybenson/Desktop/figures buckthorn"


pt.cols <- c(rgb(0,114,178,155,maxColorValue=255), #sensors
             rgb(213,94,0,155,maxColorValue=255), #VPD
             rgb(0,158,115,155,maxColorValue=255)) #temperature

pt.cols2 <- c(rgb(0,114,178,50,maxColorValue=255), #sensors
              rgb(213,94,0,50,maxColorValue=255), #VPD
              rgb(0,158,115,50,maxColorValue=255)) #temperature

pt.cols3 <- c(rgb(0,114,178,maxColorValue=255), #sensors
              rgb(213,94,0,maxColorValue=255), #VPD
              rgb(0,158,115,maxColorValue=255)) #temperature

pt.cols4 <- c(rgb(0,114,178,190,maxColorValue=255), #sensors
              rgb(213,94,0,190,maxColorValue=255), #VPD
              rgb(0,158,115,190,maxColorValue=255)) #temperature

pt.cols5 <- c(rgb(0,114,178,190,maxColorValue=255), #sensors
              rgb(213,94,0,190,maxColorValue=255), #VPD
              rgb(0,158,115,190,maxColorValue=255)) #temperature

wd <- 16*2.54
hd <- 4*2.54

#point cex
pt.c <- 4
#line thickness
ln.w <- 3

#line thickness for line only
lln.w <- 4
#tick lwd
tlw <- 3
#axis tick label size
alc <- 2.5
#  axis label size
llc <- 2.5
#legend size
lg.c <- 2
#axis size
ax.c <- 2
#text size
tcx <- 2



##############################
#### daily T per unit leaf ----


png(paste0(outDir,"/rogersglen.png"), width = 20, height = 7, units = "in", res=300)
par(mai=c(1.5,3,0.5,0.5))
plot(c(0,0), c(0,0), ylim=c(0,0.5),
     xlim=c(190,270),
     axes=FALSE, xlab=" ",
     ylab= " ", xaxs = "i", yaxs="i")
points(soilAvg$DD[soilAvg$site == "reforestation"],
       soilAvg$SM[soilAvg$site == "reforestation"],
       pch=19, col=pt.cols[1],
       type="b", cex=pt.c, lwd=ln.w)
points(soilAvg$DD[soilAvg$site == "sapflow"],
       soilAvg$SM[soilAvg$site == "sapflow"],
       pch=19, col=pt.cols[1],
       type="b", cex=pt.c, lwd=ln.w)
points(soilAvg$DD[soilAvg$site == "rogersglen"],
       soilAvg$SM[soilAvg$site == "rogersglen"],
       pch=19, col=pt.cols[1],
       type="b", cex=pt.c, lwd=ln.w)
points(soilAvg$DD[soilAvg$site == "mowed"],
       soilAvg$SM[soilAvg$site == "mowed"],
       pch=19, col=pt.cols[1],
       type="b", cex=pt.c, lwd=ln.w)
points(soilAvg$DD[soilAvg$site == "agfield"],
       soilAvg$SM[soilAvg$site == "agfield"],
       pch=19, col=pt.cols[1],
       type="b", cex=pt.c, lwd=ln.w)

arrows(soilAvg$DD,
       soilAvg$SM-soilAvg$se,
       soilAvg$DD,
       soilAvg$SM+soilAvg$se,
       code=0, lwd=ln.w, 
       col=pt.cols[1])

points(meterTable2$DD,
       meterTable2$VPD,
       pch=19, col=pt.cols[2],
       type="b", cex=pt.c, lwd=ln.w)
arrows(meterTable2$DD,
       meterTable2$VPD-meterTable2$se,
       meterTable2$DD,
       meterTable2$VPD+meterTable2$se,
       code=0, lwd=ln.w, 
       col=pt.cols[2])

points(soilAll$DD,
       soilAll$T15,
       pch=19, col=pt.cols[3],
       type="b", cex=pt.c, lwd=ln.w)

arrows(soilAll$DD,
       soilAll$T15-soilAll$se,
       soilAll$DD,
       soilAll$mean+soilAll$se,
       code=0, lwd=ln.w, 
       col=pt.cols[3])

legend("topright",
       c("Rogers Glen Sensor",
         "VPD",
         "Temperature (C)"),
       col=pt.cols,
       pch=19, lwd=ln.w,
       cex=lg.c, pt.cex=pt.c, bty="n", horiz=TRUE)

axis(1, seq(190,270, by=10), rep(" ", length(seq(190,270, by=10))), cex.axis=ax.c, lwd.ticks=tlw)
axis(2, seq(0,0.5, by=0.1), rep(" ", length(seq(0,0.5, by=0.1))), las=2, cex.axis=ax.c, lwd.ticks=tlw)
mtext( seq(190,270, by=10), at= seq(190,270, by=10), side=1, line=2, cex=alc)
mtext( seq(0,0.5, by=0.1), at= seq(0,0.5, by=0.1), side=2, line=2, cex=alc, las=2)
mtext(expression(paste("Soil Moisture ")), side=2, line=9, cex=llc)
mtext(expression(paste("(L m"^"-2","day"^"-1",")")), side=2, line=6, cex=llc)
mtext("Day of year", side=1, line=4, cex=llc)
dev.off()








