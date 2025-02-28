##############################################################################################
#################################### NORMALITY TEST SCRIPT ###################################
##############################################################################################

# THIS SCRIPT IS FOR TESTING NORMALITY OF YOUR OUTCOME VARIABLES. THE NORMCHECK FUNCTION WILL:
#   1. OUTPUT A DOCUMENT IN 'OUTPUT' FOLDER CALLED 'NORMALITY_OUTLIERS' THAT INCLUDES
#       * A HISTOGRAM OF THE VARIABLE(S)
#       * A SCATTERPLOT OF THE VARIABLE(S) AGAINST A PREDICTOR
#       * THE NUMBER OF OUTLIERS IDENTIFIED IN THAT VARIABLE (Z-SCORE > |3.29|)
#       * A SCATTERPLOT WITH OUTLIERS REMOVED
#       * A SUMMARY TABLE THAT HAS DESCRIPTIVE STATS INCLUDING NORMALITY TESTS (SKEWNESS, KURTOSIS, SHAPIRO-WILKS)
#       * A SUMMARY TABLE OF THE SAME FOR VARIABLES WITH OUTLIERS REMOVED
#   2. SAVE A DATAFRAME THAT HAS UNIVARIATE OUTLIERS REMOVED FOR EACH OF YOUR SPECIFIED VARIABLES
#       TITLED 'VAR_OUTLIERSREMOVED', THAT YOU CAN USE FOR SUBSEQUENT ANALYSIS.

# ONLY LINES 27, 30, 34 AND 37 NEED TO BE UPDATED WITH YOUR WORKING DIRECTORY, DATASET, 
# DEPENDENT VARIABLE LIST, AND A PREDICTOR VARIABLE FOR THE SCRIPT TO RUN.

# load libraries (make sure packages are installed)
library(ggplot2)
library(car)
library(rlang)
library(officer)
library(flextable) 
library(ggeffects)
library(pastecs)

# Set directory where your data is and output can be
setwd("/Users/safiyyahnawaz/Documents/assumption-testing-lmems")

# read in with the sample data to test, or replace with your data
data <- read.csv('data/sampledata.csv')

# Update dependent variables list with name of outcome variables you want to check normality for. 
# List can have as many variables as needed
dvs <- list("valence", "loudness")

# update predictor variable with name of predictor of interest, this is only for visualising scatterplots of dvs against outcome
predictor <- "vivid"

normcheck <- function(mydata,vars,pred){
  # Initialising output document that will output histograms
  doc <- read_docx()
  
  # initialising data frames to add stats descriptives back to, and again with outliers removed
  vars_descs <- data.frame(matrix(nrow=20,ncol=1))
  vars_zdescs <- data.frame(matrix(nrow=20,ncol=1))
  vars_descs[1:20,1] <- c("nbr.val","nbr.null","nbr.na","min","max","range","sum","median","mean","SE.mean","CI.mean.0.95","var","std.dev","coef.var","skewness",
                          "skew.2SE","kurtosis","kurt.2SE","normtest.W","normtest.p")
  vars_zdescs[1:20,1] <- c("nbr.val","nbr.null","nbr.na","min","max","range","sum","median","mean","SE.mean","CI.mean.0.95","var","std.dev","coef.var","skewness",
                           "skew.2SE","kurtosis","kurt.2SE","normtest.W","normtest.p")
  
  
  doc <- body_add_par(doc, "Normality Tests", style = "heading 1")
  
  # loops through variables in data frame and generates histogram and scatterplots, descriptives
  for (i in 1:length(vars)) {
    var <- vars[[i]]  # Extract variable name as string
    
    doc <- body_add_par(doc, paste0("Plots for ", var," variable"), style = "heading 2")
    
    # Save and add histograms 
    gg <- ggplot(mydata, aes(x=.data[[var]])) + geom_histogram(bins=30,color="white",fill="blue",alpha=0.7) + ggtitle(paste0("Histogram of ",var))
    doc <- body_add_gg(x=doc,value=gg,style="centered")
    
    # save and add scatterplots of outcomes against predictors
    gg2 <- ggplot(mydata, aes(x = .data[[pred]], y = .data[[var]])) + geom_point() +geom_jitter() + geom_smooth(method = 'lm') + ggtitle(paste0("Scatter Plot: ",pred," vs ", var)) 
    doc <- body_add_gg(x=doc, value =gg2, style="centered")
    
    # identifying univariate outliers
    z_var <- ((mydata[[var]] - mean(mydata[[var]])) / sd(mydata[[var]]))
    df <- paste0(var,"_outliersremoved")
    mydata_z <- cbind(mydata, z_var)
    numoutliers <- nrow(mydata_z[mydata_z$z_var > 3.29,]) + nrow(mydata_z[mydata_z$z_var< -3.29,]) 
    mydata_z <- mydata_z[mydata_z$z_var < 3.29,] #removing records where z > 3.29
    mydata_z <- mydata_z[mydata_z$z_var> -3.29,] #removing records where z < - 3.29
    doc <- body_add_par(doc,paste0( numoutliers, " outliers were identified in the ", var, " variable."))
    assign(df,mydata_z)
    
    doc <- body_add_par(doc, paste0("Scatterplot of ", var," variable with outliers removed"), style = "heading 2")
    # scatterplot with outliers removed
    gg3 <- ggplot(mydata_z, aes(x = .data[[pred]], y = .data[[var]])) + geom_point() +geom_jitter() + geom_smooth(method = 'lm') + ggtitle(paste0("Scatter Plot: ",pred," vs ", var, " , outliers removed"))
    doc <- body_add_gg(x=doc, value =gg3, style="centered")
    
    # Save the stats descriptions, including Shapiro-Wilks
    sw <- paste0(vars[[i]],"_sw")
    x <- data.frame(stat.desc(mydata[which(names(mydata)== vars[[i]])], norm=TRUE))
    x <- round(x, digits =2)
    vars_descs <- cbind(vars_descs,x)
    
    # save stats descs for outliers removed data
    sw_z <-paste0(vars[[i]],"z_sw")
    x <- data.frame(stat.desc(mydata_z[which(names(mydata)== vars[[i]])], norm=TRUE))
    x <- round(x, digits =2)
    vars_zdescs <- cbind(vars_zdescs,x)
    
    doc <- body_add_break(doc, pos = 'after')
    
    print(paste0( numoutliers, " outliers were identified in the ", var, " variable."))
    print(paste0("A new data frame titled ",df, " has been saved."))
    
  }
  
  # Add descriptives summary as a table
  doc <- body_add_par(doc, "Descriptives summary table for all variables", style = "heading 2")
  flextable_vars <- flextable(vars_descs)
  flextable_vars <- set_table_properties(flextable_vars, layout="autofit")
  doc <- body_add_flextable(doc, flextable_vars)
  doc <- body_add_break(doc, pos = 'after')
  
  # Add descriptives summary for outliers removed as table
  doc <- body_add_par(doc, "Descriptives summary table for all variables with outliers removed", style = "heading 2")
  flextable_vars <- flextable(vars_zdescs)
  flextable_vars <- set_table_properties(flextable_vars, layout="autofit")
  doc <- body_add_flextable(doc, flextable_vars)
  
  print(doc, target = "output/normality_outliers.docx")
  
}

#running the function
normcheck(data, dvs, predictor)