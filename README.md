This project includes scripts for generating diagnostic plots to check assumptions for linear mixed effects models. 

**normality.R** can be used for any variables that you want to check normality for. It will generate a histogram, scatterplot and identify univariate outliers, and save a new dataframe with univariate outliers removed

**transformations.R** can be used for any variables that you want to transform. It will perform a squareroot, log10, log, and inverse transform on your data, save a new dataframe with the transformations, and output a document with histograms of transformed data. 
