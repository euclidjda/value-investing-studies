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

# Computing the correlations value performance 
# vs. IR for different time periods #
result.all <- data.frame(size=c(), values=c())

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
pdf.filename <- "corr-value-ir-6mo.pdf"
pdf(pdf.filename)

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
p <- p + scale_y_continuous("Correlation Between 6-Month T-Bill Interest Rates & Value Investing Performance\n",
                            limit=c(-0.2,1.0),breaks=c(-0.2,0.0,0.5,0.8,1.0),expand=c(0,0))
p <- p + theme_bw()
p <- p + theme(legend.position="none",
               panel.border=element_blank() )+ggtitle("\n\n\n\n")
p <- p + ggtitle("Correlation Between 6-Month T-Bill Interest Rates and Value Performance \n for Increasing Time Horizons")
p <- p + theme(plot.title = element_text(hjust=0.5))

p

dev.off()

# Opening the png file 
system2('open', args = pdf.filename, wait = FALSE)