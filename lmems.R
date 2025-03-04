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

# defining your model, you can add more here below, and be sure to update 'allmodels' with modelnames
model1 <- lmer(memenergy ~ danceability + acousticness + (1|pptnum) + (1|Title), data = data)
model2 <- lmer(social ~ danceability + acousticness + (1|pptnum) + (1|Title), data = data)

# update allmodels by adding to list 
allmodels <- list(model1,model2)

# function for running lmem assumption plots
lmemassumptions <- function(models){
  # Initialising document Assumption Tests Document
  doc <- read_docx()
  
  doc <- body_add_par(doc, "LMEM Assumption Checking", style = "heading 1")
  doc <- body_add_par(doc, "X.1 Model formula and summary output")
  doc <- body_add_par(doc, "X.1.1 Plot of observed versus predicted values. Look for a straight line pattern")
  doc <- body_add_par(doc, "X.1.2 Plot of fitted values against residuals. Inspect for homoscedasticity. Plot should not be funnel shaped")
  doc <- body_add_par(doc, "X.1.3 Q-Q Plot: data points should be aligned on red line. Indication of normality of residuals")
  doc <- body_add_par(doc, "X.1.4 ACF. Look for no significant autocorrelations")
  
  for (i in 1:length(models)){
    
    # Fit model
    model <- models[[i]]
    var <- model@call$formula[[2]]
    modeldata <- model@call$data
    
    doc <- body_add_par(doc, paste("Model ", i,":"), style = "heading 2")
    doc <- body_add_par(doc, paste0(model@call[2]))
    
    # Add model summary as a table
    summary_table <- as.data.frame(round(summary(model)$coefficients, 3))
    flextable_obj <- flextable(summary_table)
    doc <- body_add_flextable(doc, flextable_obj)
    
    # Add result in format for reporting
    result <- paste0("(Estimate = ", summary_table$Estimate[[2]], ", SE = ", summary_table$`Std. Error`[[2]], ", t(", summary_table$df[[2]],") = ", summary_table$`t value`[[2]],", p = " ,summary_table$`Pr(>|t|)`[[2]], ")" )
    doc <- body_add_par(doc, result)
    
    # Save and add diagnostic plots
    plots <- c("obspreds","homosced", "qq", "acf")
    plot_funcs <- list(
      function() { plot(fitted(model), data[[var]]); abline(0,1, col='red') }, # Scatterplot of observed vs predicted values
      function() { plot(fitted(model), resid(model)); abline(h=0, col='red') }, # Plot of residuals to assess homoscedasticity
      function() { qqnorm(resid(model)); qqline(resid(model), col='red') }, # Q-Q plot to assess normality of residuals
      function() { acf(resid(model)) } # ACF for independence of residuals
    )
    
    for (j in seq_along(plots)) {
      file_path <- paste0("/Users/safiyyahnawaz/Documents/assumption-testing-lmems/output/plots/model", i, "_", plots[j], ".png")
      png(file_path)
      plot_funcs[[j]]()
      dev.off()
      
      doc <- body_add_par(doc, paste("Plot:", plots[j]), style = "heading 3")
      doc <- body_add_img(doc, src = file_path, width = 5, height = 4)
      doc <- body_add_break(doc, pos = 'after')
      
    }
  }
  print(doc, target = "output/LMEM_assumptions.docx")
}

# Here is where you update
lmemassumptions(allmodels)
