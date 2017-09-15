#!/usr/bin/env Rscript

# Load libraries
library(ggplot2)
library(stringr)
library(scales)
library(ggthemes)
library(PerformanceAnalytics)
library(RColorBrewer)


# The URL for the data
url.name     <- "http://mba.tuck.dartmouth.edu/pages/faculty/ken.french/ftp"
file.name    <- "25_Portfolios_5x5_CSV.zip"
full.url     <- paste(url.name, file.name, sep="/")

# You can change the ending date of the analysis by changing these variables
# Beware, however, that the source datafile may only be updated annually
# and therefore the most recent monthly may not be available

end.year     <- 2017
end.month    <- 5
window.width <- 5*12 # The rolling window width. 5 Years in this case

# Download the data and unzip it

temp.file <- tempfile()
download.file(full.url, temp.file)
file.list <- unzip(temp.file, list=TRUE)

print(temp.file)
print(as.character(file.list[1,1]))
stop("End of script.")

# Parse the data
french.data   <- read.csv(unzip(temp.file,
                                files=as.character(file.list[1,1])),
                          skip=19,
                          header=TRUE,
                          stringsAsFactors=FALSE)
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
# Column names are: DATE SMALL.LoBM ME1.BM2 SMALL.HiBM BIG.LoBM ME2.BM2 BIG.HiBM
fd <- french.data

french.data$Hi.Lo <-
  0.2*( fd$SMALL.HiBM + fd$ME2.BM5 + fd$ME3.BM5 + fd$ME4.BM5 + fd$BIG.HiBM ) -
  0.2*( fd$SMALL.LoBM + fd$ME2.BM1 + fd$ME3.BM1 + fd$ME4.BM1 + fd$BIG.LoBM )

# Now create a time series of the HML data that we can pass off to apply.rolling
# and other PerformanceAnalytics functions

ts.data <- data.frame(french.data$Hi.Lo)
row.names(ts.data) <- date.seq

# Now calculate the a time series of rolling 5-year annualized returns
z <- apply.rolling(ts.data, width=window.width, FUN = "Return.annualized")
french.data$Hi.Lo.5YrRolling <- z[[1]]

# Data sets and values used to plot the data
indexes <- seq(from=window.width,to=length(date.seq),by=7*12)
breaks  <- c(date.seq[indexes])
breaks  <- c(breaks,as.Date('2022-01-01'))
years   <- (indexes-window.width)/12+1931
years   <- c(years,2022)

dark.blue <- rev( brewer.pal(7,"Blues") )[2]
colors    <- c('#DD592D','#34BBA7')

# This is the data used to draw the backgrounds
background <- data.frame( lower = c(-.20,0.0) ,
                          upper = c(0.0, 0.25),
                          col = letters[1:2]  )

# Creating PDF for graph
pdf.filename <- "value-vs-growth.pdf"
pdf(pdf.filename)

# Plot the data
g <- ggplot()
g <- g + geom_line(data = french.data,
                   aes(x=DATE,y=Hi.Lo.5YrRolling,group=1),
                   colour=dark.blue)

# Labeling points
x.point.values <- c('1960-01-01', 
                    '1969-06-01',
                    '1980-06-01',
                    '1991-06-01',
                    '1999-06-01',
                    '2010-06-01')

y.point.values <- c(0.025,
                   -0.025,
                   0.025,
                   -0.025,
                   0.025,
                   -0.025)

time.period.labels <- c('1960-1966',
                       '1969-1977',
                       '1980-1985',
                       '1991-1996',
                       '1999-2005',
                       'Current: 2010-?')

g <- g + geom_point(color='#DD592D',
                    size=4,
                    aes(x=as.Date(x.point.values), y=0))

g <- g + geom_label(aes(label=time.period.labels, x=as.Date(x.point.values), y=y.point.values),
                    size=4,color='#617994')


# Creates label to describe significance of other labels 
g <- g + geom_label(aes(label='A Label Indicates Time Period of Value Recovery', 
                        x=as.Date('1983-06-01'), ## midpoint of dates on graph 
                        y=-0.15),
                    size=4,color='#617994')

# Put a horizontal line at zero for empaphsis
g <- g + geom_hline(aes(yintercept=0.0),color="black",linetype=2)

# Here we color the background
g <- g + geom_rect(data = background ,
                   mapping = aes(
                     xmin = as.Date('1945-01-01') ,
                     xmax = as.Date('2022-01-01') ,
                     ymin = lower ,
                     ymax = upper ,
                     fill = col   ) ,
                   alpha = .1 )
g <- g + scale_fill_manual( values=colors )
g <- g + scale_x_date("Year",breaks=breaks,
                      labels=years,
                      limits=as.Date(c('1945-01-01','2024-01-01')),
                      expand=c(0,1) )
g <- g + scale_y_continuous("Value vs. Growth (Annualized 5-Year Rolling Value Performance)",labels=percent,limits=c(-0.20,0.25),expand=c(0,0))

g <- g + theme_bw() + theme(legend.position="none",
                            axis.ticks = element_blank(),
                            panel.background=element_blank() ,
                            panel.border=element_blank() )

# Creating title and subtitle
g <- g + labs(title='Growth Has Outperformed Value Six Times Since 1945', 
              subtitle='Each Time Value Has Had A Significant Recovery')
# Centering title and subtitle
g <- g + theme(plot.title = element_text(hjust=0.5), 
               plot.subtitle = element_text(hjust=0.5))

g

dev.off()

# Opening the png file
system2('open', args = pdf.filename, wait = FALSE)
