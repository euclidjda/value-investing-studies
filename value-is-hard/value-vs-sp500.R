#!/usr/bin/env Rscript

# Load libraries
library(zoo)
library(xts)
library(PerformanceAnalytics)
library(ggplot2)
library(stringr)

# For downloading French's value performance data #

# The URL for the data
url.name     <- "http://mba.tuck.dartmouth.edu/pages/faculty/ken.french/ftp"
file.name    <- "Portfolios_Formed_on_BE-ME_TXT.zip"
full.url     <- paste(url.name, file.name, sep="/")

# Download the data and unzip it
temp.file <- tempfile()
download.file(full.url, temp.file)
file.list <- unzip(temp.file, list=TRUE)

# Parse the data
french.data   <- read.table(unzip(temp.file,files=as.character(file.list[1,1])),
                          fill=TRUE,
                          sep = "",
                          skip=24,
                          header=FALSE )

# Extracting data to create a new table 
dates.data <- french.data[, 1] ## Dates column
hi10.data <- french.data[, 20] ## Hi10 data column

# You can change the ending date of the analysis by changing these variables
# Beware, however, that the source datafile may only be updated annually
# and therefore the most recent monthly may not be available

end.year     <- 2015 ## End date used for results 
end.month    <- 9

start.year <- 1962 ## Start date used for results
start.month <- 1

french.start.year <- 1926 ## Start date in French data 
french.start.month <- 7

# Selecting necessary data from French data
num.rows <- 12 * (end.year-french.start.year)+(end.month-french.start.month)+1

## To begin at 1962-01, to reproduce results from data post
start.index <- 12 * (start.year - french.start.year) + (start.month - french.start.month) + 1
dates.data <- dates.data[start.index : num.rows]
hi10.data <- hi10.data[start.index : num.rows]
date.seq    <- as.Date(paste(dates.data,"01",sep=""),"%Y%m%d")

# Transform the hi10.data so that the return cells are numeric decimal format
for (i in 1:length(hi10.data)) hi10.data[i] <- as.numeric(str_trim(hi10.data[i]))
for (i in 1:length(hi10.data)) hi10.data[i] <- hi10.data[i]/100

# Load SP500 Return data 
raw <- read.table("1962data.txt",
                  sep = " ",
                  header = TRUE,
                  na.strings="NULL",
                  colClasses=c(DATE="character"))

# Create the time series
Hi10SP500.ts <- xts(hi10.data - raw$SP500, date.seq)
data.ts      <- xts( cbind(hi10.data, raw$SP500), date.seq )

pdf.filename <- "value-vs-sp500.pdf"
pdf(pdf.filename)

par(mfrow=c(2,1),mar=c(1,4,2,1), oma=c(2,1,2,1))

# Draw graph 
chart.CumReturns(data.ts,
                 main="Value Investing: Simulated Performance and Active Drawdowns",
                 cex.main=1.1,
                 sub="",
                 ylab="Growth of $1 Invested",
                 geometric=TRUE,
                 wealth.index=TRUE,
                 xlab="",
                 col=c('#34BBA7','#617994') )

legend(150,500,
       legend = c("Value Approach", "SP500 Total Return"),
       col=c('#34BBA7','#617994'),
       lty = c("solid", "solid"),
       lwd = c( 2, 2),
       xjust = .5, yjust = .5)

chart.Drawdown(Hi10SP500.ts,ylab="Drawdown Relative to SP500 TR",
               col=c('#DD592D'),yaxis=FALSE);

yLabels <- c(-.5, -.4, -.3, -.2, -.1, 0)
axis(2, at=yLabels, labels=sprintf(round(100*yLabels), fmt="%d%%") )

dev.off()

# Opening the png file 
system2('open', args = pdf.filename, wait = FALSE)