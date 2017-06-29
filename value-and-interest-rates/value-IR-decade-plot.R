#!/usr/bin/env Rscript
# setwd("/Users/dtran24/dataDev/euclidean-studies/value-and-interest-rates")

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


### For downloading Shiller's interest rate data ###

# The URL for the data
shiller.data.url <- 'http://www.irrationalexuberance.com/shiller_downloads/ie_data.xls'

# Name of file to store data on local computer 
shiller.filename <- 'shillerdata.xls'

# Download the data 
download.file(shiller.data.url, shiller.filename, mode='wb')

# Parse the data 
shiller.data <- read.xls(shiller.filename, sheet=1, skip=5, header=TRUE)

# Extract data needed
shiller.data <- shiller.data[, c("Date", "Rate")]
shiller.data <- head(shiller.data, nrow(shiller.data) - 2) ## Last two rows do not contain relevant data 


### For downloading French's value performance data ### 

# The URL for the data
french.data.url     <- "http://mba.tuck.dartmouth.edu/pages/faculty/ken.french/ftp/F-F_Benchmark_Factors_Monthly.zip"

# Download the data and unzip it
temp.file <- tempfile()
download.file(french.data.url, temp.file)
file.list <- unzip(temp.file, list=TRUE)

# Parse the data 
french.data   <- read.csv(unzip(temp.file,files=as.character(file.list[1,1])),
                          sep = "",
                          header=TRUE )
names(french.data)[[1]] <- "DATE"


# You can change the ending date of the analysis by changing these variables
# Beware, however, that the source datafile may only be updated annually
# and therefore the most recent monthly may not be available

end.year     <- 2006 # Change these to bring up-to-date
end.month    <- 7
window.width <- 5*12 # The rolling window width. 5 Years in this case


###  Now we want to remove all the data below the end date ### 
# Transforming French's date data 
start.year     <- as.numeric(substr(french.data$DATE[[1]],1 ,4))
start.month    <- as.numeric(substr(french.data$DATE[[1]],5, 6))
num.rows    <- 12 * (end.year - start.year) + (end.month - start.month) + 1
french.data <- head(french.data, num.rows)
date.seq    <- as.Date(paste(french.data$DATE,"01",sep=""),"%Y%m%d")
french.data$DATE <- date.seq

# Selecting the relevant dates in shiller.data
shiller.data <- tail(shiller.data, nrow(french.data))

# Transform french.data so that the return cells are numeric decimal format
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
                           as.integer(format(index(hi.lo.monthly),"%Y")), prod) - 1


# Turn shiller.data into annual data #
shiller.data$Date <- str_replace_all(shiller.data$Date, '[[.]]', '')

shiller.data$Date <- as.Date(paste(shiller.data$Date, '01', sep=''), '%Y%m%d')
ir.data <- xts(shiller.data$Rate, shiller.data$Date)
ir.monthly <- Return.calculate(ir.data, method="compound") # ir = interest rate
ir.monthly <- ir.monthly[2 : nrow(ir.monthly), ]
ir.annual <- aggregate(ir.monthly + 1, 
                       as.integer(format(index(ir.monthly), "%Y")), prod) - 1

# Create new data frame as zoo object 
data.annual <- merge.zoo(ir.annual, hi.lo.annual)
df.annual <- data.frame(ir=data.annual$ir.annual, hilo=data.annual$hi.lo.annual)
df.annual$year <- rownames(df.annual)

# Computing the data for the decade 
ir.decade <- aggregate(ir.annual + 1,
                       as.integer(substr(index(ir.annual), 1, 3)), geomean) - 1
hi.lo.decade <- aggregate(hi.lo.annual+1,
                          as.integer(substr(index(hi.lo.annual),1, 3)), geomean) - 1

# Creating data frame to use for graphing 
data.decade <- merge.zoo(ir.decade, hi.lo.decade)
df.decade <- data.frame(ir=data.decade$ir.decade, hilo=data.decade$hi.lo.decade)
df.decade$year <- rownames(df.decade)

# Creating the pdf canvas to draw graph on 
pdf("value-IR-decade-plot.pdf")

# Creating the graph 
p <- ggplot(df.decade, aes(x=ir, y=hilo))
p <- p + geom_point(color='#DD592D', size=3)
p <- p + geom_text(data=subset(df.decade,year<195.5),
                   aes(label=paste(year,"0s",sep=""),
                       vjust=-.8,hjust=0.8),size=4,color='#617994')
p <- p + geom_text(data=subset(df.decade,year>195.5),
                   aes(label=paste(year,"0s",sep=""),vjust=-.8,hjust=0.1),
                   size=4,color='#617994')
p <- p + scale_x_continuous("Annualized Change in Long-Term Interest Rates",
                            label=percent,
                            c(-0.04,-0.02,0,0.02,0.04,0.06,0.08))
p <- p + scale_y_continuous("Compound Annualized Value Factor",
                            label=percent,
                            breaks=c(-0.04,-0.02,0,0.02,0.04,0.06,0.08,0.10)) + ggtitle("\n\n\n\n")
p <- p + ggtitle("The Relationship Between Long-Term Interest Rates and Value Investing By Decade")

p

dev.off()