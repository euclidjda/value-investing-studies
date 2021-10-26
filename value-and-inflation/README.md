# The Relationship Between Value Investing and Inflation

![alt text](/value-and-inflation/value-cpi-decade-chart.png)

For details on this chart and the data used to generate it, please [visit this post](https://www.euclidean.com/value-investing-as-a-hedge-against-inflation).

## Installing and Executing the Code

###  Requirements

You will need the programs git and Rscript installed on your computer.

### Cloning the Git Repository

To clone the repository, make sure you’re in a directory on your computer that you want to hold our files. Use the command 

```bash
$ git clone https://github.com/euclidjda/value-investing-studies.git
```

to copy the repository “value-investing-studies” into your current directory. 

### Installing Necessary R Dependecies 

Some R dependecies are required to run our scripts. Use the command: 

```bash 
$ cd value-investing-studies
``` 

to navigate into your newly cloned directory. To install the necessary dependencies, use the command: 

```bash 
$ Rscript install_dependecies.R
``` 

After the script completes, you will have all the R dependecies required to run any scripts within the value-investing-studies repo. 

### Executing the Code

Navigate to the directory where the script resides: 

```bash
$ cd value-and-inflation
```

and execute the script with the command: 

```bash 
$ Rscript value-cpi-decade-chart.R
```

## Contributors and Acknowledgement

This repository was developed and is maintained by [Euclidean Technologies, LLC](http://www.euclidean.com/). We would like to thank [Kenneth R. French](http://mba.tuck.dartmouth.edu/pages/faculty/ken.french/) and [Robert Shiller](http://www.econ.yale.edu/~shiller/) for making the stock market and economic data used by this repo available through their websites [here](http://mba.tuck.dartmouth.edu/pages/faculty/ken.french/data_library.html#Research) and [here](http://www.econ.yale.edu/~shiller/data.htm), respectively. The equity investment return data is copyrighted by Kenneth R. French.

In addition, some of the studies here were not originally conceived by [Euclidean](http://www.euclidean.com/). In particular, William Bernstein first [published](http://www.efficientfrontier.com/ef/701/value.htm) a version of the value-and-inflation study in 2001. Star Capital [published](http://www.starcapital.de/files/publikationen/Research_2016-09_Value_Premium_Fama_French.pdf) a version of the value-vs-growth analysis in 2016.

## License 

This is experimental software. It is provided under the [MIT license][mit], so you can do with it whatever you wish except hold the authors responsible if it does something you don't like.

[mit]: http://www.opensource.org/licenses/mit-license.php
