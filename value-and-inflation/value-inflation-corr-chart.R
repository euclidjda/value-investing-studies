#!/usr/bin/env Rscript
# setwd("/Users/dtran24/dataDev/euclidean-studies/value-and-inflation")

# Load libraries
library(ggplot2)
library(stringr)
library(scales)
library(ggthemes)
library(PerformanceAnalytics)
library(RColorBrewer)
library(plyr)

# Define geometric mean
geomean = function(x, na.rm=TRUE){
  exp(sum(log(x[x > 0]), na.rm=na.rm) / length(x))
}


# The URL for the data
url.name     <- "http://mba.tuck.dartmouth.edu/pages/faculty/ken.french/ftp"
file.name    <- "F-F_Benchmark_Factors_Monthly.zip"
full.url     <- paste(url.name, file.name, sep="/")

# You can change the ending date of the analysis by changing these variables
# Beware, however, that the source datafile may only be updated annually
# and therefore the most recent monthly may not be available

end.year     <- 2017 # Change these to bring up-to-date
end.month    <- 4
window.width <- 5*12 # The rolling window width. 5 Years in this case

# Download the data and unzip it

temp.file <- tempfile()
download.file(full.url, temp.file)
file.list <- unzip(temp.file, list=TRUE)

# Parse the data
french.data   <- read.csv(unzip(temp.file,files=as.character(file.list[1,1])),
                          sep = "",
                          header=TRUE )
names(french.data)[[1]] <- "DATE"

# Now we want to remove all the data below the end date
ds.year     <- as.numeric(substr(french.data$DATE[[1]],1,4))
ds.month    <- as.numeric(substr(french.data$DATE[[1]],5,6))
num.rows    <- 12*(end.year-ds.year)+(end.month-ds.month)+1
french.data <- head(french.data,num.rows)
date.seq    <- as.Date(paste(french.data$DATE,"01",sep=""),"%Y%m%d")
french.data$DATE <- date.seq

# Transform the data so that the return cells are numeric decimal format
for (i in 2:ncol(french.data)) french.data[,i] <- as.numeric(str_trim(french.data[,i]))
for (i in 2:ncol(french.data)) french.data[,i] <- french.data[,i]/100

# Now we calculate the the HML sequence from the factor sequences
french.data$Hi.Lo <- french.data$HML

# Now create a time series of the HML data that we can pass off to apply.rolling 
# and other PerformanceAnalytics functions
ts.data <- data.frame(french.data$Hi.Lo)
row.names(ts.data) <- date.seq

hi.lo.monthly <- xts(ts.data$french.data.Hi.Lo,date.seq)

hi.lo.annual  <- aggregate(hi.lo.monthly+1,
                           as.integer(format(index(hi.lo.monthly),"%Y")), prod)-1

cpi.raw <- read.table("cpi_data.dat",
                      sep = " ",
                      header = TRUE,
                      na.strings="NULL",
                      colClasses=c(date="character"))

cpidate.seq    <- as.Date(paste(cpi.raw$date,"01",sep=""),"%Y%m%d")
cpi.data       <- xts(cpi.raw$cpi,cpidate.seq)
cpi.monthly    <- Return.calculate(cpi.data, method="compound")
cpi.monthly    <- cpi.monthly[2:nrow(cpi.monthly),]
cpi.annual     <- aggregate(cpi.monthly+1,
                            as.integer(format(index(cpi.monthly),"%Y")), prod)-1

data.annual    <- merge.zoo( cpi.annual, hi.lo.annual )
df.annual      <- data.frame(cpi=data.annual$cpi.annual,hilo=data.annual$hi.lo.annual)

df.annual$year <- rownames(df.annual)

# Now do the correlation calcs

result.all <- data.frame( size=c(), value=c() )

for ( window.size in 1:10 ) {
  
  num.years   <- nrow(df.annual)
  num.windows <- ceiling(num.years/window.size)
  
  samp <- df.annual
  samp$period <- gl(num.windows,window.size,length=num.years)
  
  result <- aggregate(samp[,c("cpi","hilo")]+1, by=list(samp$period), FUN=geomean)
  result <- result[,c("cpi","hilo")]-1
  result$year <- df.annual[seq(from=1,to=num.years,by=window.size),]$year
  
  new.row <- data.frame( size=c(window.size), value=c(cor(result$cpi,result$hilo)))
  result.all <- rbind( result.all, new.row )
  
}

result.all

result.all$size <- factor(result.all$size)

background <- data.frame( lower = c(0.0,0.5,0.8) , 
                          upper = c(0.5,0.8,1.0) ,
                          col = letters[1:3]     )

colors = c("#DD592D","#617994","#34BBA7")

pdf("value-inflation-corr-chart.pdf")

p <- ggplot(result.all)
p <- p + geom_rect(data = background ,
                   mapping = aes(
                     xmin = 1 ,
                     xmax = 10 ,
                     ymin = lower ,
                     ymax = upper ,
                     fill = col   ),
                   alpha = 0.3 )
p <- p + scale_fill_manual( values=colors )
p <- p + geom_point(aes(x=size,y=value,color=value),size=6)
p <- p + geom_smooth(aes(x=size,y=value,group=1),
                     method="lm",se=FALSE,color="grey",alpha=0.7)
p <- p + scale_x_discrete("Time Period (Horizon) in Years")
p <- p + scale_y_continuous("Correlation Between Inflation & Value Investing Performance\n",
                            limit=c(-0.0,1.0),breaks=c(0.0,0.5,0.8,1.0),expand=c(0,0))
p <- p + theme_bw()
p <- p + theme(legend.position="none",
               panel.border=element_blank() )+ggtitle("\n\n\n\n")
p <- p + ggtitle("Correlation Between CPI and Value Performance \n for Increasing Time Horizons\n")

p

dev.off()