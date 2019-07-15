### <p align="center"> General Regression Neural Network </p>

Compared with other types of neural networks, General Regression Neural Network (Specht, 1991) is advantageous in several aspects.

1. Being an universal approximation function, GRNN has only one tuning parameter to control the overall generalization
2. The network structure of GRNN is surprisingly simple, with only one hidden layer and the number of neurons equal to the number of training samples.
3. GRNN is always able to converge globally and won’t be trapped by local solutions.
4. The training of GRNN is a simple 1-pass, regardless of the sample size, and doesn’t require time-consuming iterations.
5. Since any projected value of GRNN is the weighted average of training samples, predictions are bounded by the observed range.

