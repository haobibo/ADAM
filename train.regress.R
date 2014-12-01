

Predict = function(YName,method_=algorithm,nCV=N_CV){
  str_formula = paste(
    YName, '~',
    paste(XNames,collapse="+"),
    sep=' '
  )
  #print(str_formula)
  formula <- as.formula(str_formula)
  
  
  fitControl = trainControl(
    method = "cv"
    ,number = nCV
    ,returnResamp = "all"
    ,summaryFunction = function(data,lev = NULL, model = NULL){
      tmp = sum( (data$pred-data$obs)^2 ) / sum( (data$obs - mean(data$obs))^2  )
      
      out = c(
        sqrt( mean( (data$obs - data$pred)^2 ) ),    #RMSE
        mean( abs(data$obs - data$pred)   ),         #MAE
        abs( cor(data$pred,data$obs) ),              #PCC
        1 - tmp,                                                                      #R-Squared
        1 - tmp * (length(data$obs)-1) / (length(data$obs) - 1 - length(data$obs))   #Adj.R-Squared
      )
      names(out) <- c('RMSE','MAE','PCC','RSq','AdjRSq')
      return(out)
    }
  )
  
  lmFit = train(
    form = formula
    ,data = df
    ,method = method_
    ,trControl = fitControl
    ,preProc = c("center", "scale")
    #,maxit=2000 #only for rlm
    #,direction = "forward" #only for stepwise regression
  )
  r = lmFit$results
  if(nrow(r)>1)
    row.names(r) <- paste(YName, 1:nrow(r), sep='#' )
  else
    row.names(r) <- c(YName)
  return(r)
}