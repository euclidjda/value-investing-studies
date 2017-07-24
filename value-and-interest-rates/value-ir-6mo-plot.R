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

# You can change the ending date of the analysis by changing these variables
# Beware, however, that the source datafile may only be updated annually
# and therefore the most recent monthly may not be available
end.year     <- 2017 
end.month    <- 5

# For downloading French's value performance data #

# The URL for the value data
value.data.url     <- "http://mba.tuck.dartmouth.edu/pages/faculty/ken.french/ftp/F-F_Benchmark_Factors_Monthly.zip"

# Download the data and unzip it
temp.file <- tempfile()
download.file(value.data.url, temp.file)
file.list <- unzip(temp.file, list=TRUE)

# Parse the data
value.data   <- read.csv(unzip(temp.file,files=as.character(file.list[1,1])),
                         sep = "",
                         header=TRUE )
names(value.data)[[1]] <- "DATE"

# Now we want to remove all the data below the end date

# Transforming French's date data
ds.year     <- as.numeric(substr(value.data$DATE[[1]],1,4))
ds.month    <- as.numeric(substr(value.data$DATE[[1]],5,6))
num.rows    <- 12 * (end.year-ds.year)+(end.month-ds.month)+1

ir.data.start.year <- 1959
ir.data.start.month <- 1

start.index <- 12 * (ir.data.start.year - ds.year) + (ir.data.start.month - ds.month) + 1
value.data <- value.data[start.index: num.rows, ]
date.seq    <- as.Date(paste(value.data$DATE,"01",sep=""),"%Y%m%d")
value.data$DATE <- date.seq

# Transform the data so that the return cells are numeric decimal format
for (i in 2:ncol(value.data)) value.data[,i] <- as.numeric(str_trim(value.data[,i]))
for (i in 2:ncol(value.data)) value.data[,i] <- value.data[,i]/100

# Now we calculate the the HML sequence from the factor sequences
value.data$Hi.Lo <- value.data$HML

# Converting monthly value peformance data into annual value performance data #
ts.data <- data.frame(value.data$Hi.Lo)
row.names(ts.data) <- date.seq

hi.lo.monthly <- xts(ts.data$value.data.Hi.Lo,date.seq)

hi.lo.annual  <- aggregate(hi.lo.monthly+1,
                           as.integer(format(index(hi.lo.monthly),"%Y")), prod)-1


# For reading in 6-month T-Bill data #
ir.data <- read.xls('tbill_6mo.xlsx',
                      header=TRUE)
ir.data <- head(ir.data, num.rows - start.index + 1)

# Convert into annualized ir return data 
ir.data <- cbind(ir.data, value.data$DATE)
ir.monthly <- xts(as.numeric(as.character(ir.data$tbill6mo))/100, ir.data$`value.data$DATE`)
ir.annual <- aggregate(ir.monthly,as.integer(format(index(ir.monthly), "%Y")), mean)

# Create new data frame as zoo object
data.annual    <- merge.zoo( ir.annual, hi.lo.annual )
df.annual      <- data.frame(ir=data.annual$ir.annual,hilo=data.annual$hi.lo.annual)
df.annual$year <- rownames(df.annual)

# Computing the data for the decade
ir.decade     <- aggregate(ir.annual+1,
                           as.integer(substr(index(ir.annual),1,3)), geomean) - 1
hi.lo.decade   <- aggregate(hi.lo.annual+1,
                            as.integer(substr(index(hi.lo.annual),1,3)), geomean) - 1

data.decade    <- merge.zoo( ir.decade, hi.lo.decade )

df.decade      <- data.frame(ir=data.decade$ir.decade,hilo=data.decade$hi.lo.decade)
df.decade$year <- rownames(df.decade)

# Creating the png canvas to draw graph on
pdf.filename <- "value-ir-6mo-plot.pdf"
pdf(pdf.filename)

p <- ggplot(df.decade,aes(x=ir,y=hilo))
p <- p + geom_point(color="#DD592D",size=3)
p <- p + geom_text(data=subset(df.decade, year<198.5),
                   aes(label=paste(year,"0s",sep=""),
                       vjust=-.8,hjust=0.8),size=4,color='#617994')
p <- p + geom_text(data=subset(df.decade, year>198.5),
                   aes(label=paste(year,"0s",sep=""),vjust=-.8,hjust=0.1),
                   size=4,color='#617994')
p <- p + scale_x_continuous("Annualized 6-Month T-Bill Returns",
                            label=percent,
                            c(-0.04,-0.02,0,0.02,0.04,0.06,0.08))
p <- p + scale_y_continuous("Compound Annualized Value Factor",
                            label=percent,
                            breaks=c(-0.04,-0.02,0,0.02,0.04,0.06,0.08,0.10))+ggtitle("\n\n\n\n")
p <- p + ggtitle("The Relationship Between 6-Month T-Bill Returns \n and Value Investing By Decade")
p <- p + theme(plot.title = element_text(hjust=0.5))

p

dev.off()

# Opening the png file 
system2('open', args = pdf.filename, wait = FALSE)
