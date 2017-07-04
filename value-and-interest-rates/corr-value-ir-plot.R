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
end.year     <- 2017 # Change these to bring up-to-date
end.month    <- 4


###  Now we want to remove all the data below the end date ###
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

# Selecting only the dates needed from Shiller data
shiller.data$Date <- str_replace_all(shiller.data$Date, '[[.]]', '')
shiller.data$Date <- as.Date(paste(shiller.data$Date, '01', sep=''), '%Y%m%d')
start.index <- which(shiller.data$Date == as.Date('1926-07-01'))[1] ## Start date of French's data
end.index <- which(shiller.data$Date == as.Date('2017-04-01'))[1] ## End date of French's data
shiller.data <- shiller.data[start.index : end.index, ]

# Computing the annualized interest rate data
ir.data <- xts(as.numeric(as.character(shiller.data$Rate)), shiller.data$Date) # Type conversion
ir.monthly <- Return.calculate(ir.data, method="compound") # ir = interest rate
ir.monthly <- ir.monthly[2 : nrow(ir.monthly), ]
ir.annual <- aggregate(ir.monthly + 1,
                       as.integer(format(index(ir.monthly), "%Y")), prod) - 1

# Create new data frame as zoo object
data.annual <- merge.zoo(ir.annual, hi.lo.annual)
df.annual <- data.frame(ir=data.annual$ir.annual, hilo=data.annual$hi.lo.annual)
df.annual$year <- rownames(df.annual)


### Computing the correlations value performance vs. IR for different time periods ###
result.all <- data.frame(size=c(), values=c())
window.size <- 5 

num.years <- nrow(df.annual)
num.windows <- ceiling(num.years / window.size)

samp <- df.annual 
samp$period <- gl(num.windows, window.size, length=num.years)

for (window.size in 1:10) {
  num.years <- nrow(df.annual)
  num.windows <- ceiling(num.years / window.size)

  samp <- df.annual
  samp$period <- gl(num.windows, window.size, length=num.years)

  result <- aggregate(samp[, c('ir', 'hilo')] + 1, by=list(samp$period), FUN=geomean)
  result <- result[, c('ir', 'hilo')] - 1
  result$year <- df.annual[seq(from=1, to=num.years, by=window.size), ]$year

  new.row <- data.frame(size=c(window.size), value=c(cor(result$ir, result$hilo)))
  result.all <- rbind(result.all, new.row)
}

result.all$size <- factor(result.all$size)

# Preparing to graph the correlations
background <- data.frame( lower = c(-0.2,0.5,0.8) ,
                          upper = c(0.5,0.8,1.0) ,
                          col = letters[1:3]     )

colors = c("#DD592D","#617994","#34BBA7")

# Creating the png canvas to draw graph on
png.filename <- "corr-value-ir-plot.png"
png(png.filename)

# Graph the correlations
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
p <- p + scale_y_continuous("Correlation Between Long-Term Interest Rates & Value Investing Performance\n",
                            limit=c(-0.2,1.0),breaks=c(-0.2,0.0,0.5,0.8,1.0),expand=c(0,0))
p <- p + theme_bw()
p <- p + theme(legend.position="none",
               panel.border=element_blank() )+ggtitle("\n\n\n\n")
p <- p + ggtitle("Correlation Between Long-Term Interest Rates and Value Performance \n for Increasing Time Horizons")
p <- p + theme(plot.title = element_text(hjust=0.5))

p

# Opening the png file 
system2('open', args = png.filename, wait = FALSE)

dev.off()
