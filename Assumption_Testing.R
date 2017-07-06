########################################################
#                    Assumption Testing                #
########################################################

### H1: Linear in parameters - true

### H2: Test for Multicollinearity
test_H2_multicollinearity <- function(explanatory_data, model) {
  # Normal Correlation Plot
  exp_cor <- cor(explanatory_data)
  corrplot(as.matrix(exp_cor), is.corr = FALSE, method = "color",
           order="hclust", type = "upper")
  
  # Variance Inflation Factors
  vif <-vif(model) 
  return(paste("VIF:",all(vif^2<4)))
}

### H3: Zero Conditional Mean:
#Strict Exogenity, Contemporaneous Exogeneity
test_H3_zeroConditionalMean <- function(explanatory_data, model) {
  # Error at time t uncorrelated with each explanatory variable in every time perio
  res <- model$residuals
  res_include_exp_data <- cbind(explanatory_data, res)
  # visual plotting of residuals against fitted values
  plot(model,1)
  
  
  # looking at the correlation
  res_cor <- cor(res_include_exp_data)
  corrplot(res_cor,is.corr = FALSE, method = "color",
            type = "upper")
  return ("Please check plot of fitted values against residuals")
}

# H4: Homoscedasticity
test_H4_homoscedasticity <- function (dependent_data, model, sig=0.05) {
  #Breusch-Pagan-Test: (if p<sig, reject H0 -> no homoscedasticity)
  bp_result <- bptest(model)
  return (paste("Breusch-Pagan-Test:",bp_result$p.value>sig))
}

### H5 No serial correlation
test_H5_noSerialCorrelation <- function (model, sig=0.05) {
  # Durbin Watson: if p < sig, reject H0 -> serial correlation
  DurbinWatson_NoSerialCorr <- dwtest(model)$p.value > sig
  return (paste("Durbin Watson Test:", DurbinWatson_NoSerialCorr))
}

### H6 Normal distribution of the errors
test_H6_NormalityOfErrors <- function (model, sig=0.1) {
  result<-list()
  # QQ-Plot
  qqPlot(model$residuals)
  result$QQPlot <- "Please check QQ-Plot!"
 
  #Shapiro Test
  normality <- shapiro.test(model$residuals)
  result$ShapiroTest<-(normality$p.value > sig)
  return (result)
} 

### Assumption Main
assumptions_test <- function (model){
  result<- list()
  result$H1_LinearParameters <- TRUE
  result$H2_multicollinearity <- test_H2_multicollinearity(juices[,2:24], model)
  result$H3_zeroConditionalMean <- test_H3_zeroConditionalMean(juices[,2:24],model)
  result$H4_homoscedasticity <- test_H4_homoscedasticity(juices[,2:24],model)
  result$H5_noSerialCorrelation <- test_H5_noSerialCorrelation(model)
  result$H6_errorNormality <- test_H6_NormalityOfErrors(model)
  return(result)
}