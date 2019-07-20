### <p align="center"> General Regression Neural Network </p>

#### Introduction

Compared with other types of neural networks, General Regression Neural Network (Specht, 1991) is advantageous in several aspects.

1. Being an universal approximation function, GRNN has only one tuning parameter to control the overall generalization
2. The network structure of GRNN is surprisingly simple, with only one hidden layer and the number of neurons equal to the number of training samples.
3. GRNN is always able to converge globally and won’t be trapped by local solutions.
4. The training of GRNN is a simple 1-pass, regardless of the sample size, and doesn’t require time-consuming iterations.
5. Since any projected value of GRNN is the weighted average of training samples, predictions are bounded by the observed range.

The grnn package (https://cran.r-project.org/web/packages/grnn/index.html), which has not been updated since 2013, is the only implementation of GRNN on CRAN and was designed elegantly with a parsimonious set of functions and lots of opportunities for potential improvements. 

The GRnnet project (https://github.com/statcompute/GRnnet) is my attempt to provide a R implementation of GRNN, with several enhancements.

1. While the training function **grnn.fit()** is very similar to learn() and smooth() in the grnn package. three functions were designed to provide GRNN projections. The **grnn.predone()** function generates one projected value based on an input vector. Both **grnn.predict()** and **grnn.parpred()** functions generate a vector of projected values based on an input matrix. The only difference is that **grnn.parpred()** runs in parallel and therefore can be 3 times faster than **grnn.predict()** on my 4-core workstation.
2. While tuning the only hyper-parameter is the key in GRNN training, there are two functions in the GRnnet project to search for the optimal parameter through the n-fold cross validation, including **grnn.cv_r2()** for numeric outcomes and **grnn.cv_auc()** for binary outcomes.
3. In **grnn.predone()** function, while the default projection is based on the Euclidean distance, there is an option to calculate the GRNN projection based on the Manhattan distance as well for the sake of computational simplicity (Specht, 1991).

#### Why Use GRNN?

In the banking industry, GRNN can be useful in several areas. First of all, it can be employed as the replacement of splines to approximate the term structure of interest rates. Secondly, like other neural networks, it can be used in Fraud Detection and Anti-Money Laundering given its flexibility. At last, in the credit risk modeling, it can also be used to develop performance benchmarks and rapid prototypes for scorecards or Expected Loss models due to the simplicity.

#### Package Dependencies
R version 3.6, base, stats, caret, parallel, MLmetrics

#### <a name="installation"></a>Installation

Download the [grnnet.R](https://github.com/statcompute/GRnnet/blob/master/code/grnnet.R) file and save it in your computer.

If you want to load specific functions (or a function) from the "grnnet.R" file, the import::from() should work. 
```r
import::from("grnnet.R", grnn.fit) 
```

Alternatively, if you just want to load all functions into the current environment, the source() is simple enough to get you started. 
```r
source("grnnet.R")
```


