require(dplyr, quietly = TRUE)
require(utils, quietly = TRUE)
require(LaF, quietly = TRUE)
require(tidyr, quietly = TRUE)
require(ggplot2, quietly = TRUE)
require(ggfortify, quietly = TRUE)
require(scales, quietly = TRUE)
source('~/RProjects/ghcn/HWplot.R')

# IV. FORMAT OF "ghcnd-stations.txt"
# 
# ------------------------------
#       Variable   Columns   Type
# ------------------------------
# ID            1-11   Character
# LATITUDE     13-20   Real
# LONGITUDE    22-30   Real
# ELEVATION    32-37   Real
# STATE        39-40   Character
# NAME         42-71   Character
# GSN FLAG     73-75   Character
# HCN/CRN FLAG 77-79   Character
# WMO ID       81-85   Character
# ------------------------------

#download.file("ftp://ftp.ncdc.noaa.gov/pub/data/ghcn/daily/ghcnd-stations.txt", destfile = "stations.txt")

stations_width<-c(12, 9, 10, 7, 3, 31)
stations_colname<-c("id", "latitude", "longitude", "elevation", "state", "name")

laf <- laf_open_fwf("stations.txt", column_widths = stations_width, 
                    column_types=rep("character",6),
                    column_names = stations_colname)

stations<-laf[,]

sp_stations<-filter(stations, grepl("SP00", substr(stations$id,1,4)))
sp_stations$url<-paste0("ftp://ftp.ncdc.noaa.gov/pub/data/ghcn/daily/all/", trimws(sp_stations$id), ".dly")

# ------------------------------
#       Variable   Columns   Type
# ------------------------------
# ID            1-11   Character
# YEAR         12-15   Integer
# MONTH        16-17   Integer
# ELEMENT      18-21   Character
# VALUE1       22-26   Integer
# MFLAG1       27-27   Character
# QFLAG1       28-28   Character
# SFLAG1       29-29   Character
# VALUE2       30-34   Integer
# MFLAG2       35-35   Character
# QFLAG2       36-36   Character
# SFLAG2       37-37   Character
# .           .          .
# .           .          .
# .           .          .
# VALUE31    262-266   Integer
# MFLAG31    267-267   Character
# QFLAG31    268-268   Character
# SFLAG31    269-269   Character
# ------------------------------

col_width<-c(11, 4, 2, 4, rep(c(5, 1, 1, 1), 31))

col_types<-c("character", "integer", "integer", "character", rep(c("integer", "character", "character", "character"),31))

col_names<-c("id", "year", "month", "element")
for(i in 1:31) col_names<-c(col_names, paste0("value", i), paste0("mflag", i), paste0("qflag", i), paste0("sflag", i))

for (i in 1:nrow(sp_stations)){
      
      #download.file(sp_stations$url[i], destfile = as.character(sp_stations$id[i]))
      
      laf <- laf_open_fwf(as.character(sp_stations$id[i]), column_widths = col_width, 
                          column_types=col_types,
                          column_names = col_names)
      
      if(i == 1){
            df<-laf[,]
      }else{
            df<-rbind(df, laf[,])
      }     
}

dff<-select(df, 1:4, starts_with("value"))
dffg<-gather(dff, key = day, value = value, 5:35)
dffg$value<-ifelse(dffg$value == -9999,NA,dffg$value)
dffg$day<-as.integer(substr(dffg$day,6,7))
dffg<-mutate(dffg, date=as.Date(paste(year,month,day), "%Y%m%d"))
dffg<-arrange(dffg, id, element, date)

dffg$id<-factor(dffg$id)
levels(dffg$id) = sp_stations$name
TData<-filter(dffg, element %in% c("TMAX", "TMIN"))
save(TData, file="TData.rda")

pru_gby<-group_by(dffg, id, element, month, year)
pru<-summarize(pru_gby, mval=mean(value, na.rm=TRUE ))
pru$month<-factor(pru$month)
levels(pru$month)<-month.name

pruTMAX<-filter(pru, element == "TMAX")
pruTMIN<-filter(pru, element == "TMIN")

pdf("MOnthly_output.pdf", pointsize=4)
p<-ggplot()
p<-p+geom_line(data=pruTMAX, aes(x=month, y=mval, group = interaction(year,element)), alpha=0.5, color="gray")
p<-p+geom_smooth(data=pruTMAX, aes(x=month, y=mval, group=1), size=1.5, se=FALSE, color="red")
p<-p+geom_line(data=pruTMIN, aes(x=month, y=mval, group = interaction(year,element)), alpha=0.5, color="gray")
p<-p+geom_smooth(data=pruTMIN, aes(x=month, y=mval, group=1), size=1.5, se=FALSE, color="blue")
p<-p+facet_wrap(~ id, ncol=4)
p<-p+ylab("Temp °C x 10")
p<-p+ggtitle("MONTHLY TMAX / TMIN")
p<-p+theme(axis.title.y=element_text(hjust = 0.5, size = 12), 
           axis.title.x=element_blank(), 
           plot.title = element_text(hjust = 0.5, size = 16),
           axis.text.x = element_text(size=12, angle = 45, hjust = 1)) 
suppressMessages(print(p))
dev.off()

pru<-filter(dffg, element == "TMAX", id == sp_stations$name[1], !is.na(value), date >= "2000-01-01")
pru<-select(pru, date, value)
pruts<-ts(pru$value, frequency = 365, start=c(as.numeric(format(pru$date[1], "%Y"), 1)))
prucomp<-decompose(pruts, type = "additive")

p<-autoplot(prucomp, ts.colour = "blue")
p<-p + xlab("Years")
p<-p+ylab("Temp °C x 10")
p<-p + ggtitle(paste("TMAX decomposition of station ", sp_stations$name[1]))
print(p)

pruHW<-HoltWinters(pruts)
hist(residuals(pruHW), breaks=20, freq=TRUE)
p<-HWplot(pruHW, n.ahead = 365, CI = 0.75)
p<-p + scale_x_date(breaks = date_breaks("month"), labels = date_format("%b-%y"),
                    limits=c(as.Date("2014-01-01", "%Y-%m-%d"), as.Date("2016-01-01", "%Y-%m-%d")))
print(p)
