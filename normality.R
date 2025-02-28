# load libraries
library(ggplot2)
library(lme4)
library(lmerTest)
library(car)
library(rlang)
library(officer)
library(flextable) 
library(ggeffects)
library(pastecs)
data("iris")

# Set directory where your data is and output can be
setwd("/Users/safiyyahnawaz/Documents/assumption-testing-lmems")
 
# read in with the sample data to test, or replace with your data
mydata <- read.csv('data/sampledata.csv')

# create a list that includes the names of the variables you are checking for normality
vars_lmems <- list("danceability","energy","loudness")

# Initialising output document that will output histograms
doc <- read_docx()

################################################################
# Checking normality ###########################################
################################################################
# initialising data frames to add stats descriptives back to, and again with outliers removed
vars_descs <- data.frame(matrix(nrow=20,ncol=1))
vars_zdescs <- data.frame(matrix(nrow=20,ncol=1))
vars_descs[1:20,1] <- c("nbr.val","nbr.null","nbr.na","min","max","range","sum","median","mean","SE.mean","CI.mean.0.95","var","std.dev","coef.var","skewness",
                        "skew.2SE","kurtosis","kurt.2SE","normtest.W","normtest.p")
vars_zdescs[1:20,1] <- c("nbr.val","nbr.null","nbr.na","min","max","range","sum","median","mean","SE.mean","CI.mean.0.95","var","std.dev","coef.var","skewness",
                        "skew.2SE","kurtosis","kurt.2SE","normtest.W","normtest.p")


doc <- body_add_par(doc, "Normality Tests", style = "heading 1")
vars <- vars_lmems

# loops through variables in data frame and generates histogram and scatterplots, descriptives
for (i in 1:length(vars)) {
  var <- vars[[i]]  # Extract variable name as string
   
  doc <- body_add_par(doc, paste0("Plots for ", var," variable"), style = "heading 2")
  
  # Save and add histograms 
  gg <- ggplot(mydata, aes(x=.data[[var]])) + geom_histogram(bins=30,color="white",fill="blue",alpha=0.7) + ggtitle(paste0("Histogram of ",var))
  doc <- body_add_gg(x=doc,value=gg,style="centered")
  
  # save and add scatterplots of outcomes against predictors
  gg2 <- ggplot(mydata, aes(x = memoryage, y = .data[[var]])) + geom_point() +geom_jitter() + geom_smooth(method = 'lm') + ggtitle(paste0("Scatter Plot: memoryage vs ", var)) 
  doc <- body_add_gg(x=doc, value =gg2, style="centered")
  
  # identifying univariate outliers
  z_var <- ((mydata[[var]] - mean(mydata[[var]])) / sd(mydata[[var]]))
  df <- paste0(var,"_outliersremoved")
  mydata_z <- cbind(mydata, z_var)
  numoutliers <- nrow(mydata_z[mydata_z$z_var > 3.29,]) + nrow(mydata_z[mydata_z$z_var< -3.29,]) 
  mydata_z <- mydata_z[mydata_z$z_var < 3.29,] #removing records where z > 3.29
  mydata_z <- mydata_z[mydata_z$z_var> -3.29,] #removing records where z < - 3.29
  doc <- body_add_par(doc,paste0("There are ", numoutliers, " outliers in this variable that were removed."))
  assign(df,mydata_z)

  # scatterplot with outliers removed
  gg3 <- ggplot(mydata_z, aes(x = memoryage, y = .data[[var]])) + geom_point() +geom_jitter() + geom_smooth(method = 'lm') + ggtitle(paste0("Scatter Plot: memoryage vs ", var, " , outliers removed"))
  doc <- body_add_gg(x=doc, value =gg3, style="centered")

  # Save the stats descriptions, including Shapiro-Wilks
  sw <- paste0(vars[[i]],"_sw")
  x <- data.frame(stat.desc(mydata[which(names(mydata)== vars[[i]])], norm=TRUE))
  x <- round(x, digits =2)
  vars_descs <- cbind(vars_descs,x)

  # save stats descs for outliers removed data
  sw_z <-paste0(vars[[i]],"z_sw")
  x <- data.frame(stat.desc(mydata_z$z_var, norm=TRUE))
  x <- round(x, digits =2)
  vars_zdescs <- cbind(vars_descs,x)
  
  doc <- body_add_break(doc, pos = 'after')

}

# Add descriptives summary as a table
flextable_vars <- flextable(vars_descs)
flextable_vars <- set_table_properties(flextable_vars, layout="autofit")
doc <- body_add_flextable(doc, flextable_vars)
doc <- body_add_break(doc, pos = 'after')

# Add descriptives summary for outliers removed as table
flextable_vars <- flextable(vars_zdescs)
flextable_vars <- set_table_properties(flextable_vars, layout="autofit")
doc <- body_add_flextable(doc, flextable_vars)

print(doc, target = "output/normality_outliers.docx")