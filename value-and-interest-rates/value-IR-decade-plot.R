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


# For downloading Shiller's interest rate data #

# The URL for the data
shiller.data.url <- 'www.econ.yale.edu/~shiller/data/ie_data.xls'

# Name of file to store data on local computer 
shiller.filename <- 'shillerdata.xls'

# Download the data 
download.file(shiller.data.url, shiller.filename, mode='wb')

# Parse the data 
shiller.data <- read.xls(shiller.filename, sheet='Data', skip=6, header=TRUE)

# Extract data needed
names(shiller.data)[names(shiller.data) == 'Rate.GS10'] <- 'Rate'
shiller.data <- shiller.data[, c("Date", "Rate")]
shiller.data <- head(shiller.data, nrow(shiller.data) - 2) ## Last two rows do not contain relevant data
# Will need to be further trimmed to align with timeline of French data 

# For downloading French's value performance data #

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

end.year     <- 2017 # Change these to bring up-to-date
end.month    <- 4


#  Now we want to remove all the data below the end date #

# Transforming French's date data
start.year     <- as.numeric(substr(french.data$DATE[[1]], 1, 4))
start.month    <- as.numeric(substr(french.data$DATE[[1]], 5, 6))
num.rows    <- 12 * (end.year - start.year) + (end.month - start.month) + 1
french.data <- head(french.data, num.rows)
date.seq    <- as.Date(paste(french.data$DATE,"01",sep=""),"%Y%m%d")
french.data$DATE <- date.seq

# Transform french.data so that the return cells are numeric decimal format
for (i in 2:ncol(french.data)) french.data[,i] <- as.numeric(str_trim(french.data[,i]))
for (i in 2:ncol(french.data)) french.data[,i] <- french.data[,i] / 100

# Now we calculate the the HML sequence from the factor sequences
french.data$Hi.Lo <- french.data$HML

# Converting French's HML data into annualized data #
ts.data <- data.frame(french.data$Hi.Lo)
row.names(ts.data) <- date.seq

hi.lo.monthly <- xts(ts.data$french.data.Hi.Lo,date.seq)

hi.lo.annual  <- aggregate(hi.lo.monthly+1,
                           as.integer(format(index(hi.lo.monthly),"%Y")), prod) - 1


# Turn shiller.data into annualized data #

# Selecting only the dates needed from Shiller data. French.data
# has less data points than shiller.data so we must choose proper 
# dates to align the two data sets
shiller.data$Date <- str_replace_all(shiller.data$Date, '[[.]]', '')
shiller.data$Date <- as.Date(paste(shiller.data$Date, '01', sep=''), '%Y%m%d')
start.index <- which(shiller.data$Date == as.Date('1926-07-01'))[1] ## Start date of French's data
end.index <- which(shiller.data$Date == as.Date('2017-04-01'))[1] ## End date of French's data
shiller.data <- shiller.data[start.index : end.index, ]

# Computing the annualized interest rate data

# ir = interest rate
ir.data <- xts(as.numeric(as.character(shiller.data$Rate)), shiller.data$Date) ## Type conversion
ir.monthly <- Return.calculate(ir.data, method="compound")
ir.monthly <- ir.monthly[2 : nrow(ir.monthly), ]
ir.annual <- aggregate(ir.monthly + 1,
                       as.integer(format(index(ir.monthly), "%Y")), prod) - 1

# Create new data frame as zoo object holding both ir and hilo data
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

# Creating the png canvas to draw graph on
png.filename <- "value-IR-decade-plot.png"
png(png.filename)

# Creating the graph
p <- ggplot(df.decade, aes(x=ir, y=hilo))
p <- p + geom_point(color='#DD592D', size=3)
p <- p + geom_text(data=subset(df.decade,year<195),
                   aes(label=paste(year,"0s",sep=""),
                       vjust=-.8,hjust=0.8),size=4,color='#617994')
p <- p + geom_text(data=subset(df.decade,year>=195),
                   aes(label=paste(year,"0s",sep=""),vjust=-.8,hjust=0.1),
                   size=4,color='#617994')
p <- p + scale_x_continuous("Annualized Change in Long-Term Interest Rates",
                            label=percent,
                            breaks=c(-0.10, -0.08, -0.06, -0.04,-0.02,0,0.02,0.04,0.06,0.08),
                            limits=c(-0.10, 0.08))
p <- p + scale_y_continuous("Compound Annualized Value Factor",
                            label=percent,
                            breaks=c(-0.04,-0.02,0,0.02,0.04,0.06,0.08,0.10, 0.12, 0.14)) + ggtitle("\n\n\n\n")
p <- p + ggtitle("The Relationship Between Long-Term Interest Rates \n and Value Investing By Decade")
p <- p + theme(plot.title = element_text(hjust=0.5))

p

# Opening the png file 
system2('open', args = png.filename, wait = FALSE)

dev.off()