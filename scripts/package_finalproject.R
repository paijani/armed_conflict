#For the final package, I intend to create a package that will help 
#choose an imputation model for each variable in the data set and use 
#that model to impute data on variable by variable basis. There is a 
#need for this package because the user is still responsible for specifying
#a conditional model for each missing variable. The input would be the list
#of values in each variable in the data set, and the output would be the 
#imputed columns. A challenge I expect in developing this package is that 
#I will encounter columns where more than one model can be used and then I
#would have to determine what the best imputation model is. 



#when you are doing multiple imputation analysis, its important to decide
#on the analysis model. That will be based on the scientific question of 
#interest. #Fully conditional specification (FCS) was proposed as a tool
#for handling a mixture of missing continuous and categorical variables. 
#Each missing variable will be imputed based on the appropriate model, 
#conditional on other observed variables.

#this function will do the imput and analyze step in one

#input -> data frame of y variable and x variables you want to include in
#your analysis plus the  m (number of imputations)
#output -> pooled effect estimate with variance measure

#example: out <- function(data= data, m = 10, method = norm)
#out = 45.2 (5.1)

#limitations- will still have to manually choose the imputation model

imputed_estimate <- function(data, m = 5, method = "logreg") {
  library(mice)
  imputed_data <- mice::mice(data, m = m, method = method)
  analysis_results <- mice::with(imputed_data, summary(pool(method)))
  mean_analysis <- mean(analysis_result)
  result <- mean_analysis$estimate[, c("mean", "sd")]
  return(result)
}