########################################################
#                    Assumption Testing                #
########################################################

### H1: Linear in parameters - true

### H2: Test for Multicollinearity
test_multicollinearity <- function(explanatory_data) {
  # Normal Correlation Plot
  exp_cor <- cor(explanatory_data)
  corrplot(as.matrix(exp_cor), is.corr = FALSE, method = "color",
           order="hclust", type = "upper")
  
  # Variance Inflation Factors
  vif <-vif(model) 
  return(all(vif^2<2))
}

### H3: Zero Conditional Mean:
#Strict Exogenity, Contemporaneous Exogeneity
test_zeroConditionalMean <- function(explanatory_data, model) {
  # Error at time t uncorrelated with each explanatory variable in every time perio
  res <- model$residuals
  res_include_exp_data <- cbind(explanatory_data, res)
  # visual plotting of residuals against fitted values
  plot(model,1)
  # looking at the correlation
  res_cor <- cor(res_include_exp_data)
  corrplot(res_cor,is.corr = FALSE, method = "color",
           order="hclust", type = "upper")
  return (sum(res_cor==1)==ncol(res_cor))
}

# H4: Homoscedasticity
test_homoscedasticity <- function (dependent_data, model, sig=0.05) {
  #Breusch-Pagan-Test: (if p<sig, reject H0 -> no homoscedasticity)
  bp_result <- bptest(model)
  return (bp_result$p.value>sig)
}

### H5 No serial correlation
test_serialCorrelation <- function (model, sig=0.05) {
  # Durbin Watson: if p < sig, reject H0 -> serial correlation
  DurbinWatson_NoSerialCorr <- dwtest(model)$p.value > sig
  return (DurbinWatson_NoSerialCorr)
}

# H6 Normal distribution of the errors
test_errorNormality <- function (model, sig=0.1) {
  # QQ-Plot
  qqPlot(model$residuals)
  
  res <- model$residuals
  normality <- shapiro.test(res)
  return (normality$p.value < sig)
} 

### Assumption Main
assumptions_test <- function (model){
  result<- list()
  result$H1 <- TRUE
  result$H2 <- test_multicollinearity(juices[,2:24])
  result$H3 <- test_zeroConditionalMean(juices[,2:24],model)
  result$H4 <- test_homoscedasticity(juices[,2:24],model)
  result$H5 <- test_serialCorrelation(model)
  result$H6 <- test_errorNormality(model)
  return(result)
}