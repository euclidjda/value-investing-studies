# Value Investing Studies

This is a collection of data that examines the performance and characteristics of value investing over long periods of time. Each directory is dedicated to a particular form of data analysis pertaining to value investing and the directories are:
- [Value and Inflation](https://github.com/euclidjda/value-investing-studies/tree/master/value-and-inflation): 
	- Measures value stock performance with respect to inflation
- [Value and Interest Rates](https://github.com/euclidjda/value-investing-studies/tree/master/value-and-interest-rates)
	- Measures value stock performance with respect to interest rates 
- [Value is Hard](https://github.com/euclidjda/value-investing-studies/tree/master/value-is-hard)
- [Value vs. Growth](https://github.com/euclidjda/value-investing-studies/tree/master/value-vs-growth)


# Reproducing Our Results

1) Cloning the Git Repository

To clone our repository, make sure you’re in a directory on your computer that you want to hold our files. Use the command 

```bash
$ git clone https://github.com/euclidjda/value-investing-studies.git
```

to copy our repository “value-investing-studies” into your current directory. 


2) Reproducing our Results

Choose a chart from our README to reproduce.

Suppose we want to run the script “value-cpi-decade-chart.R”. To run this script, make sure you’re in the directory that is holding our cloned repository, and use the command: 

```bash
$ cd value-investing-studies/value-and-inflation
```

This will place you in the “value-investing-studies” directory that contains all of our files, and then in the “value-and-inflation” directory, which contains the script we want to run. Lastly, to run the script, use the command: 

```bash 
$ Rscript value-cpi-decade-chart.R
```


After doing so, this chart should pop up:  

![alt text](/value-and-inflation/value-cpi-decade-chart.png)


# Contributors and Acknowledgement

This repository was developed and is maintained by [Euclidean Technologies LLC](http://www.euclidean.com/). Contributors include David Tran and John Alberg. 


# License 

This is experimental software and NOT intended for treatment of any
kind. It is provided under the [MIT license][mit], so you can do with
it whatever you wish except hold me responsible if it does something
you don't like.

[mit]: http://www.opensource.org/licenses/mit-license.php