##############################################################################################
#################################### TRANSFORMATIONS SCRIPT ##################################
##############################################################################################

# THIS SCRIPT IS FOR TRANSFORMING VARIABLES THAT ARE NON-NORMALLY DISTRIBUTED. IT WILL PERFORM:
#   * SQUARE ROOT TRANSFORM
#   * LOG10 TRANSFORM
#   * NATURAL LOG TRANSFORM
#   * INVERSE TRANSFORM 
# ON ANY OF THE VARIABLES YOU SPECIFY,  OUTPUT HISTOGRAMS AND SAVE A NEW DATAFRAME WITH THE
# TRANSFORMED DATA.



# load libraries
library(ggplot2)
library(lme4)
library(lmerTest)
library(car)
library(rtf)
library(rlang)
library(officer)
library(flextable) 
library(ggeffects)
library(pastecs)

# Set directory where your data is and output can be
setwd("/Users/safiyyahnawaz/Documents/assumption-testing-lmems")

# read in with the sample data to test, or replace with your data
data <- read.csv('data/sampledata.csv')
data <- data[!is.na(data$memoryage),]

# Update dependent variables list with name of outcome variables you want to try transformations on
# List can have as many variables as needed
variables <- list("memoryage")

transformvars <- function(mydata,vars,skew){
  # Initialising document Assumption Tests Document
  doc <- read_docx()
  
  # initialising data frames to add stats descriptives for transformed variables
  rownames <- c("nbr.val","nbr.null","nbr.na","min","max","range","sum","median","mean","SE.mean","CI.mean.0.95","var","std.dev","coef.var","skewness",
                "skew.2SE","kurtosis","kurt.2SE","normtest.W","normtest.p")
  
  desc_original <- data.frame(matrix(nrow=20,ncol=1)); desc_original[1:20,1] <- rownames
  desc_sqrt <- data.frame(matrix(nrow=20,ncol=1)); desc_sqrt[1:20,1] <- rownames
  desc_log10 <- data.frame(matrix(nrow=20,ncol=1));desc_log10[1:20,1] <- rownames
  desc_log <- data.frame(matrix(nrow=20,ncol=1)); desc_log[1:20,1] <- rownames
  desc_inv <- data.frame(matrix(nrow=20,ncol=1)); desc_inv[1:20,1] <- rownames
  
  for (i in 1:length(vars)){
    # Var
    var <- vars[[i]]
    
    # make transformed variables
    foo <- mydata
    name <- paste0(var,"_transformed")
    
    if(skew == "pos"){
      # For positive skew
      foo$sqrt <- sqrt(foo[[var]] + abs(min(foo[[var]])) + 1e-6)
      foo$log10 <-log10(foo[[var]] + abs(min(foo[[var]])) + 1e-6)
      foo$log <- log(foo[[var]] + abs(min(foo[[var]])) + 1e-6)
      foo$inv <- 1/(foo[[var]] + abs(min(foo[[var]])) + 1e-6)
    } else if(skew == "neg"){
      # For negative skew
      foo$sqrt <- sqrt(max(foo[[var]] + 1) - foo[[var]])
      foo$log10 <-log10(max(foo[[var]] + 1) - foo[[var]])
      foo$log <- log(max(foo[[var]] + 1) - foo[[var]])
      foo$inv <- 1/(max(foo[[var]] + 1) - foo[[var]])
    }
    
    # Original data for comparison
    doc <- body_add_par(doc, paste("Histogram of non-transformed ",var), style = "heading 2")
    
    gghist <- ggplot(foo, aes(x=.data[[var]])) + geom_histogram(bins=30,color="white",fill="blue",alpha=0.7) + ggtitle(paste0("Histogram of non-transformed ",var))
    doc <- body_add_gg(x=doc,value=gghist,style="centered")
    
    # Save the stats descriptions, including Shapiro-Wilks
    sw <- paste0(vars[[i]],"_sw")
    x <- data.frame(stat.desc(foo[which(names(foo)== vars[[i]])], norm=TRUE))
    x <- round(x, digits =2)
    desc_original <- cbind(desc_original,x)
    
    # Squareroot transform
    doc <- body_add_par(doc, paste("Square-root transform of ",var," (for moderate skew)"), style = "heading 2")
    
    # New hist of transformed data
    gghist <- ggplot(foo, aes(x=sqrt)) + geom_histogram(bins=30,color="white",fill="blue",alpha=0.7) + ggtitle(paste0("Histogram of sqrt transformed ",var))
    doc <- body_add_gg(x=doc,value=gghist,style="centered")
    
    # Save the stats descriptions, including Shapiro-Wilks
    sw <- paste0(vars[[i]],"_sw_sqrt")
    x <- data.frame(stat.desc(foo$sqrt, norm=TRUE))
    x <- round(x, digits =2)
    desc_sqrt <- cbind(desc_sqrt,x)
    
    # Log10 transform
    doc <- body_add_par(doc, paste("Log base 10 transform of ",var," (for high skew)"), style = "heading 2")
    
    # New hist of transformed data
    gghist <- ggplot(foo, aes(x=log10)) + geom_histogram(bins=30,color="white",fill="blue",alpha=0.7) + ggtitle(paste0("Histogram of log10 transformed ",var))
    doc <- body_add_gg(x=doc,value=gghist,style="centered")
    
    # Save the stats descriptions, including Shapiro-Wilks
    sw <- paste0(vars[[i]],"_sw_log10")
    x <- data.frame(stat.desc(foo$log10, norm=TRUE))
    x <- round(x, digits =2)
    desc_log10 <- cbind(desc_log10,x)
    
    # Natural log transform
    doc <- body_add_par(doc, paste("Natural log transform of ",var," (for higher skew)"), style = "heading 2")
    
    # New hist of transformed data
    gghist <- ggplot(foo, aes(x=log)) + geom_histogram(bins=30,color="white",fill="blue",alpha=0.7) + ggtitle(paste0("Histogram of log transformed ",var))
    doc <- body_add_gg(x=doc,value=gghist,style="centered")
    
    # Save the stats descriptions, including Shapiro-Wilks
    sw <- paste0(vars[[i]],"_sw_log")
    x <- data.frame(stat.desc(foo$log, norm=TRUE))
    x <- round(x, digits =2)
    desc_log <- cbind(desc_log,x)
    
    # Inverse transform
    doc <- body_add_par(doc, paste("Inverse transform of ",var," (for severe skew)"), style = "heading 2")
    
    # New hist of transformed data
    gghist <- ggplot(foo, aes(x=inv)) + geom_histogram(bins=30,color="white",fill="blue",alpha=0.7) + ggtitle(paste0("Histogram of inverse transformed ",var))
    doc <- body_add_gg(x=doc,value=gghist,style="centered")
    
    # Save the stats descriptions, including Shapiro-Wilks
    sw <- paste0(vars[[i]],"_sw_inv")
    x <- data.frame(stat.desc(foo$inv, norm=TRUE))
    x <- round(x, digits =2)
    desc_inv <- cbind(desc_inv,x)
    
    assign(name,foo)
    doc <- body_add_break(doc, pos = 'after')
  }
  
  # Adding descriptives summary for the variables
  doc <- body_add_par(doc, "Descriptives summary table for all variables (not transformed)", style = "heading 2")
  flextable_vars <- flextable(desc_original)
  flextable_vars <- set_table_properties(flextable_vars, layout="autofit")
  doc <- body_add_flextable(doc, flextable_vars)
  doc <- body_add_break(doc, pos = 'after')
  
  # Adding descriptives summary for the variables - sqrt
  doc <- body_add_par(doc, "Descriptives summary table for all variables - sqrt transform", style = "heading 2")
  flextable_vars <- flextable(desc_sqrt)
  flextable_vars <- set_table_properties(flextable_vars, layout="autofit")
  doc <- body_add_flextable(doc, flextable_vars)
  doc <- body_add_break(doc, pos = 'after')
  
  # Adding descriptives summary for the variables - log 10 
  doc <- body_add_par(doc, "Descriptives summary table for all variables - log10 transform", style = "heading 2")
  flextable_vars <- flextable(desc_log10)
  flextable_vars <- set_table_properties(flextable_vars, layout="autofit")
  doc <- body_add_flextable(doc, flextable_vars)
  doc <- body_add_break(doc, pos = 'after')
  
  # Adding descriptives summary for the variables - log
  doc <- body_add_par(doc, "Descriptives summary table for all variables - log transform", style = "heading 2")
  flextable_vars <- flextable(desc_log)
  flextable_vars <- set_table_properties(flextable_vars, layout="autofit")
  doc <- body_add_flextable(doc, flextable_vars)
  doc <- body_add_break(doc, pos = 'after')
  
  # Adding descriptives summary for the variables - inv
  doc <- body_add_par(doc, "Descriptives summary table for all variables - inverse transform", style = "heading 2")
  flextable_vars <- flextable(desc_inv)
  flextable_vars <- set_table_properties(flextable_vars, layout="autofit")
  doc <- body_add_flextable(doc, flextable_vars)
  doc <- body_add_break(doc, pos = 'after')
  
  print(doc, target = "output/transformations.docx")
}

transformvars(data,variables,"pos")
