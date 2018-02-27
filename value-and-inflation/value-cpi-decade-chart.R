#!/usr/bin/env Rscript

# Load libraries
library(ggplot2)
library(stringr)
library(scales)
library(ggthemes)
library(PerformanceAnalytics)
library(RColorBrewer)
library(plyr)
library(gdata)

# Define geometric mean
geomean = function(x, na.rm=TRUE){
  exp(sum(log(x[x > 0]), na.rm=na.rm) / length(x))
}

# For downloading French's value performance data #

# The URL for the data
url.name     <- "http://mba.tuck.dartmouth.edu/pages/faculty/ken.french/ftp"
file.name    <- "F-F_Benchmark_Factors_Monthly.zip"
full.url     <- paste(url.name, file.name, sep="/")

# You can change the ending date of the analysis by changing these variables
# Beware, however, that the source datafile may only be updated annually
# and therefore the most recent monthly may not be available
end.year     <- 2017 
end.month    <- 12

if (!file.exists(file.name)) {
                                        # Download the data and unzip it
    download.file(full.url, file.name)
}

file.list <- unzip(file.name, list=TRUE)

# Parse the data
french.data   <- read.csv(unzip(file.name,files=as.character(file.list[1,1])),
                          sep = "",
                          header=TRUE )
names(french.data)[[1]] <- "DATE"

# Now we want to remove all the data below the end date

# Transforming French's date data
ds.year     <- as.numeric(substr(french.data$DATE[[1]],1,4))
ds.month    <- as.numeric(substr(french.data$DATE[[1]],5,6))
num.rows    <- 12 * (end.year-ds.year)+(end.month-ds.month)+1
french.data <- head(french.data,num.rows)
date.seq    <- as.Date(paste(french.data$DATE,"01",sep=""),"%Y%m%d")
french.data$DATE <- date.seq

# Transform the data so that the return cells are numeric decimal format
for (i in 2:ncol(french.data)) french.data[,i] <- as.numeric(str_trim(french.data[,i]))
for (i in 2:ncol(french.data)) french.data[,i] <- french.data[,i]/100

# Now we calculate the the HML sequence from the factor sequences
french.data$Hi.Lo <- french.data$HML

# Converting monthly value data into annual value data #
ts.data <- data.frame(french.data$Hi.Lo)
row.names(ts.data) <- date.seq

hi.lo.monthly <- xts(ts.data$french.data.Hi.Lo,date.seq)

hi.lo.annual  <- aggregate(hi.lo.monthly+1,
                           as.integer(format(index(hi.lo.monthly),"%Y")), prod)-1

# Downloading Shiller spreadsheet for inflation data #

# The URL for the data
shiller.data.url <- 'http://www.econ.yale.edu/~shiller/data/ie_data.xls'

# Name of file to store data on local computer 
shiller.filename <- 'shillerdata.xls'

if (!file.exists(shiller.filename)) {
                                        # Download the data 
    download.file(shiller.data.url, shiller.filename, mode='wb')

}

# Parse the data 
shiller.data <- read.xls(shiller.filename, sheet='Data', skip=6, header=TRUE)

# Extract data needed
shiller.data <- shiller.data[, c("Date", "CPI")]
shiller.data <- head(shiller.data, nrow(shiller.data) - 2) ## Last row does not contain relevant data for inflation
shiller.data$CPI <- as.numeric(as.character(shiller.data$CPI))
# Will need to be further trimmed to align with timeline of French data 

# Turn shiller.data into annualized data #

# Selecting only the dates needed from Shiller data. French.data
# has less data points than shiller.data so we must choose proper
# dates to align the two data sets
shiller.data$Date <- str_replace_all(shiller.data$Date, '[[.]]', '')
shiller.data$Date <- as.Date(paste(shiller.data$Date, '01', sep=''), '%Y%m%d')
start.index <- which(shiller.data$Date == as.Date('1926-07-01'))[1] ## Start date of French's data
end.index <- which(shiller.data$Date == as.Date('2017-04-01'))[1] ## End date of French's data
shiller.data <- shiller.data[start.index : end.index, ]

# Converting monthly inflation data into annual inflation data #
cpi.data       <- xts(shiller.data$CPI, shiller.data$Date)
cpi.monthly    <- Return.calculate(cpi.data, method="compound")
cpi.monthly    <- cpi.monthly[2:nrow(cpi.monthly),]
cpi.annual     <- aggregate(cpi.monthly+1,
                            as.integer(format(index(cpi.monthly),"%Y")), prod)-1

# Create new data frame as zoo object
data.annual    <- merge.zoo( cpi.annual, hi.lo.annual )
df.annual      <- data.frame(cpi=data.annual$cpi.annual,hilo=data.annual$hi.lo.annual)

df.annual$year <- rownames(df.annual)

# Computing the data for the decade
cpi.decade     <- aggregate(cpi.annual+1,
                            as.integer(substr(index(cpi.annual),1,3)), geomean)-1
hi.lo.decade   <- aggregate(hi.lo.annual+1,
                            as.integer(substr(index(hi.lo.annual),1,3)), geomean)-1

data.decade    <- merge.zoo( cpi.decade, hi.lo.decade )

df.decade      <- data.frame(cpi=data.decade$cpi.decade,hilo=data.decade$hi.lo.decade)
df.decade$year <- rownames(df.decade)

lm_eqn = function(m) {

  l <- list(a = format(coef(m)[1], digits = 2),
      b = format(abs(coef(m)[2]), digits = 2),
      r2 = format(summary(m)$r.squared, digits = 3));

  if (coef(m)[2] >= 0)  {
    eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2,l)
  } else {
    eq <- substitute(italic(y) == a - b %.% italic(x)*","~~italic(r)^2~"="~r2,l)    
  }

  as.character(as.expression(eq));                 
}

# Creating the png canvas to draw graph on
png.filename <- "value-cpi-decade-chart.png"
png(png.filename, width=500, height=500)

p <- ggplot(df.decade,aes(x=cpi,y=hilo))
p <- p + geom_smooth(method="lm",se=FALSE,linetype = "dotted")
p <- p + geom_point(color="#DD592D",size=3)
p <- p + geom_text(aes(x = 0.05, y = -0.01, label = lm_eqn(lm(hilo ~ cpi, df.decade))),
                   parse = TRUE,size=4,color='#617994')

p <- p + geom_text(data=subset(df.decade, (year<195.5 | year==197) & year!=193),
                   aes(label=paste(year,"0s",sep=""),
                       vjust=-.8,hjust=0.8),size=4,color='#617994')
p <- p + geom_text(data=subset(df.decade, (year>195.5 | year==193) & year!=197),
                   aes(label=paste(year,"0s",sep=""),vjust=-.8,hjust=0.1),
                   size=4,color='#617994')

p <- p + scale_x_continuous("Annualized Change in CPI",
                            label=percent,
                            c(-0.04,-0.02,0,0.02,0.04,0.06,0.08))
p <- p + scale_y_continuous("Compound Annualized Value Factor",
                            label=percent,
                            breaks=c(-0.04,-0.02,0,0.02,0.04,0.06,0.08,0.10))+ggtitle("\n\n\n\n")
p <- p + ggtitle("The Relationship Between Inflation and Value Investing By Decade")
p <- p + theme(plot.title = element_text(hjust=0.5))

p

dev.off()

# Opening the png file 
system2('open', args = png.filename, wait = FALSE)
