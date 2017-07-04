# euclidean-studies
Euclidean Studies

In this README, we outline two relations through charts: 1) the performance of value investing with respect to inflation, and 2) the performance of value investing with respect to long-term interest rates. 

If you’re interested in reproducing the results on your own computer, there will be instructions on how to do so after the charts.  

# Value and Inflation 
(charts are inside “value-and-inflation” folder)

value-cpi-decade-chart.png
![alt text](/value-and-inflation/value-cpi-decade-chart.png)

value-inflation-corr-chart.png
![alt text](/value-and-inflation/value-inflation-corr-chart.png)


# Value and Long Term Interest Rates
(charts are inside “value-and-interest-rates” folder)

value-IR-decade-plot.png
![alt text](/value-and-interest-rates/value-IR-decade-plot.png)

corr-value-ir-plot.png
![alt text](/value-and-interest-rates/corr-value-ir-plot.png)


# Reproducing the Results

1) Cloning the Git Repository

To clone (read: copy) our repository, make sure you’re in a directory on your computer that you want to hold our files. Use the command 

git clone https://github.com/euclidjda/euclidean-studies.git

to copy our repository “euclidean-studies” into your current directory. 


2) Reproducing our Results

Choose a chart from our README to reproduce. Note that each chart is titled with its filename. Each chart can be found in the folder that is stated under the section header (i.e. “Value and Inflation”). 

To open the chart that you have chosen, navigate into the “euclidean-studies” directory, the directory that holds the chart you desire, and use the command:

Rscript <name-of-file>

ex: Rscript corr-value-ir-plot.R

After doing so, our chart should pop up. 