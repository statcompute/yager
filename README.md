<p align="center">
  <img width="150" height="100" src="https://github.com/statcompute/GRnnet/blob/master/code/08.jpg">
</p>

### <p align="center"> Yet Another General Regression (YAG[E]R) </p>
### <p align="center">  Neural Networks </p>

#### Introduction

Compared with other types of neural networks, General Regression Neural Network (Specht, 1991) is advantageous in several aspects.

1. Being an universal approximation function, GRNN has only one tuning parameter to control the overall generalization
2. The network structure of GRNN is surprisingly simple, with only one hidden layer and the number of neurons equal to the number of training samples.
3. GRNN is always able to converge globally and won’t be trapped by local solutions.
4. The training of GRNN is a simple 1-pass, regardless of the sample size, and doesn’t require time-consuming iterations.
5. Since any projected value of GRNN is the weighted average of training samples, predictions are bounded by the observed range.

The grnn package (https://cran.r-project.org/web/packages/grnn/index.html), which has not been updated since 2013, is the only implementation of GRNN on CRAN and was designed elegantly with a parsimonious set of functions and lots of opportunities for potential improvements. 

The GRnnet project (https://github.com/statcompute/yagr) is my attempt to provide a R implementation of GRNN, with several enhancements.

1. While the training function **grnn.fit()** is very similar to learn() and smooth() in the grnn package. three functions were designed to provide GRNN projections. The **grnn.predone()** function generates one projected value based on an input vector. Both **grnn.predict()** and **grnn.parpred()** functions generate a vector of projected values based on an input matrix. The only difference is that **grnn.parpred()** runs in parallel and therefore can be 3 times faster than **grnn.predict()** on my 4-core workstation.
2. While tuning the only hyper-parameter is the key in GRNN training, there are two functions in the GRnnet project to search for the optimal parameter through the n-fold cross validation, including **grnn.cv_r2()** for numeric outcomes and **grnn.cv_auc()** for binary outcomes.
3. In **grnn.predone()** function, while the default projection is based on the Euclidean distance, there is an option to calculate the GRNN projection based on the Manhattan distance as well for the sake of computational simplicity (Specht, 1991).

#### Why Use GRNN?

In the banking industry, GRNN can be useful in several areas. First of all, it can be employed as the replacement of splines to approximate the term structure of interest rates. Secondly, like other neural networks, it can be used in Fraud Detection and Anti-Money Laundering given its flexibility. At last, in the credit risk modeling, it can also be used to develop performance benchmarks and rapid prototypes for scorecards or Expected Loss models due to the simplicity.

#### Package Dependencies
R version 3.6, base, stats, caret, parallel, MLmetrics

#### Installation

Download the [grnnet.R](https://github.com/statcompute/yagr/blob/master/code/grnnet.R) file and save it in your computer.

If you want to load specific functions (or a function) from the "grnnet.R" file, the import::from() should work. 
```r
import::from("grnnet.R", grnn.fit) 
```

Alternatively, if you just want to load all functions into the current environment, the source() is simple enough to get you started. 
```r
source("grnnet.R")
```

#### Example
It has been mentioned previously that GRNN is an ideal approach employed to develop performance benchmarks for a variety of risk models. People might wonder what the purpose of performance benchmarks is and why we would even need one at all. Sometimes, a model developer had to answer questions about how well the model would perform even before completing the model. Likewise, a model validator also wondered whether the model being validated has a reasonable performance given the data used and the effort spent. As a result, the performance benchmark, which could be built with the same data sample but an alternative methodology, is called for to address aforementioned questions.

While the performance benchmark can take various forms, including but not limited to business expectations, industry practices, or vendor products, a model-based approach should possess following characteristics:  

- Quick prototype with reasonable efforts
- Comparable baseline with acceptable outcomes
- Flexible framework without strict assumptions
- Practical application to broad domains

With both empirical and conceptual advantages, GRNN is able to accommendate each of abovementioned requirements and thus can be considered an approriate candidate that might potentially be employed to develop performance benchmarks for a wide variety of models. 

Below is an example illustrating how to use GRNN to develop a benchmark model for the logistic regression shown in https://github.com/statcompute/MonotonicBinning#example. 

```r
df <- readRDS("df.rds") 
source("mob.R")
source("grnnet.R")

# PRE-PROCESS THE DATA WITH MOB PACKAGE
bin_out <- batch_bin(df, 3)
bin_out$BinSum[order(-bin_out$BinSum$iv), ]
#            var nbin unique miss min  median     max      ks     iv
#   bureau_score   34    315  315 443   692.5     848 35.2651 0.8357
#   tot_rev_line   20   3617  477   0 10573.0  205395 26.8943 0.4442
#  age_oldest_tr   25    460  216   1   137.0     588 20.3646 0.2714
#      tot_derog    7     29  213   0     0.0      32 20.0442 0.2599
#            ltv   17    145    1   0   100.0     176 16.8807 0.1911
#       rev_util   12    101    0   0    30.0     100 16.9615 0.1635
#         tot_tr   15     67  213   0    16.0      77 17.3002 0.1425
#   tot_rev_debt    8   3880  477   0  3009.5   96260  8.8722 0.0847
#     tot_rev_tr    4     21  636   0     3.0      24  9.0779 0.0789
#     tot_income   17   1639    5   0  3400.0 8147167 10.3386 0.0775
#    tot_open_tr    7     26 1416   0     5.0      26  6.8695 0.0282

# PERFORMAN WOE TRANSFORMATIONS
df_woe <- batch_woe(df, bin_out$BinLst)

# PROCESS AND STANDARDIZE THE DATA WITH ZERO MEAN AND UNITY VARIANCE
Y <- df$bad
X <- scale(df_woe$df[, -1])
Reduce(rbind, Map(function(c) data.frame(var = colnames(X)[c], mean = mean(X[, c]), variance = var(X[, c])), seq(dim(X)[2])))
#                 var          mean variance
#1      woe.tot_derog  2.234331e-16        1
#2         woe.tot_tr -2.439238e-15        1
#3  woe.age_oldest_tr -2.502177e-15        1
#4    woe.tot_open_tr -2.088444e-16        1
#5     woe.tot_rev_tr -4.930136e-15        1
#6   woe.tot_rev_debt -2.174607e-16        1
#7   woe.tot_rev_line -8.589630e-16        1
#8       woe.rev_util -8.649849e-15        1
#9   woe.bureau_score  1.439904e-15        1
#10           woe.ltv  3.723332e-15        1
#11    woe.tot_income  5.559240e-16        1

# INITIATE A GRNN OBJECT 
net1 <- grnn.fit(x = X, y = Y)
# CROSS-VALIDATION TO CHOOSE THE OPTIONAL SMOOTH PARAMETER
S <- gen_sobol(min = 0.5, max = 1.5, n = 10, seed = 2019)
cv <- grnn.cv_auc(net = net1, sigmas = S, nfolds = 5)
# $test
#       sigma       auc
#1  1.4066449 0.7543912
#2  0.6205723 0.7303415
#3  1.0710133 0.7553075
#4  0.6764866 0.7378430
#5  1.1322939 0.7553664
#6  0.8402438 0.7507192
#7  1.3590402 0.7546164
#8  1.3031974 0.7548670
#9  0.7555905 0.7455457
#10 1.2174429 0.7552097
# $best
#     sigma       auc
#5 1.132294 0.7553664

# REFIT A GRNN WITH THE OPTIMAL PARAMETER VALUE
net2 <- grnn.fit(x = X, y = Y, sigma = cv$best$sigma)
net2.pred <- grnn.parpred(net2, X)

# BENCHMARK MODEL PERFORMANCE
MLmetrics::KS_Stat(y_pred = net2.pred, y_true = df$bad)
# 44.00242
MLmetrics::AUC(y_pred = net2.pred, y_true = df$bad)
# 0.7895033

# LOGISTIC REGRESSION PERFORMANCE
MLmetrics::KS_Stat(y_pred = fitted(mdl2), y_true = df$bad)
# 42.61731
MLmetrics::AUC(y_pred = fitted(mdl2), y_true = df$bad)
# 0.7751298

```

The function [grnn.margin()](https://github.com/statcompute/GRnnet/blob/master/code/grnn.margin.R) can also be employed to explore the marginal effect of each attribute in a GRNN. 

<p align="center">
  <img width="800" height="400" src="https://github.com/statcompute/GRnnet/blob/master/code/grnn_margin.PNG">
</p>



