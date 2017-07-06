# Value Investing Studies
#### [by Euclidean Technologies, LLC](http://www.euclidean.com)

This repo is a collection of data anaysis studies that examine the performance and characteristics of value investing over long periods of time. Each directory is a study dedicated to a particular form of data analysis on value investing. The studies depend only on the code herein and publicly available stock market and economic data. We would like to thank [Kenneth R. French](http://mba.tuck.dartmouth.edu/pages/faculty/ken.french/) and [Robert Shiller](http://www.econ.yale.edu/~shiller/) for making this data available through their websites [here](http://mba.tuck.dartmouth.edu/pages/faculty/ken.french/data_library.html#Research) and [here](http://www.econ.yale.edu/~shiller/data.htm), respectively. 

The studies in this repo are:
- [Value and Inflation](https://github.com/euclidjda/value-investing-studies/tree/master/value-and-inflation): 
	- Characterizes the relationship of value stock performance with respect to inflation as measured by the consumer price index
- [Value and Interest Rates](https://github.com/euclidjda/value-investing-studies/tree/master/value-and-interest-rates):
	- Characterizes the relationship of value stock performance with respect to long-term interest rates 
- [Value is Hard](https://github.com/euclidjda/value-investing-studies/tree/master/value-is-hard)
- [Value vs. Growth](https://github.com/euclidjda/value-investing-studies/tree/master/value-vs-growth)


## Installing and Executing the Studies

### Cloning the Git Repository

To clone our repository, make sure you’re in a directory on your computer that you want to hold our files. Use the command 

```bash
$ git clone https://github.com/euclidjda/value-investing-studies.git
```

to copy our repository “value-investing-studies” into your current directory. 

### Installing Necessary R Dependecies 

Some R dependecies are required to run our scripts. Use the command 

```bash 
$ cd value-investing-studies
``` 

to navigate into your newly cloned directory. To install the necessary dependencies, use the command: 

```bash 
$ Rscript install_dependecies.R
``` 

After the script completes, you will have all the R dependecies required to run our scripts. 

### Executing the Code

Choose a chart from our README to reproduce.

Suppose we want to run the script “value-cpi-decade-chart.R”. To run this script, we find the directory the script is located in (in this case “value-and-inflation”) and use the command: 

```bash
$ cd value-and-inflation
```

to navigate into the directory. Lastly, to run the script, use the command: 

```bash 
$ Rscript value-cpi-decade-chart.R
```

After doing so, this chart should pop up:  

![alt text](/value-and-inflation/value-cpi-decade-chart.png)


## Contributors and Acknowledgement

This repository was developed and is maintained by [Euclidean Technologies, LLC](http://www.euclidean.com/). Contributors include [David Tran](http://github.com/dtran24) and [John Alberg](http://github.com/euclidjda). We would like to thank [Kenneth R. French](http://mba.tuck.dartmouth.edu/pages/faculty/ken.french/) and [Robert Shiller](http://www.econ.yale.edu/~shiller/) for making the stock market and economic data used by this repo available through their websites [here](http://mba.tuck.dartmouth.edu/pages/faculty/ken.french/data_library.html#Research) and [here](http://www.econ.yale.edu/~shiller/data.htm), respectively. 

## License 

This is experimental software. It is provided under the [MIT license][mit], so you can do with it whatever you wish except hold the authors responsible if it does something you don't like.

[mit]: http://www.opensource.org/licenses/mit-license.php
