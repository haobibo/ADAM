#This function conduct stepwise features selection before Cross Validation
#In each round of cross validation, models are trained based on selected features.
#The stwpwise feature selection is only processed for one time: before Cross Validation.

Predict = function(YName,method_=algorithm,nCV=N_CV){
  str_formula = paste(
    YName, '~',
    paste(XNames,collapse="+"),
    sep=' '
  )
  model_lm   = lm(  as.formula(str_formula) ,  data=df)
  model_step = step(model_lm, direction=method_, trace=F) #both/forward/backward
  
  #Only use this line for accelerating debuging, no uses in other cases:
  #model_step = model_lm

  predictors = names(model_step$model)[-1]  #the first element is Y
  
  print(paste(length(predictors), 'variables are selected from', length(XNames), 'variables.' ))
  
  str_fm = paste(  YName, '~', paste(predictors, collapse="+"))
  fm <- as.formula(str_fm)

  n_row = nrow(df)
  allIndex = 1:n_row
  cvIndex = sample( rep(1:nCV,ceiling(n_row/nCV))[1:n_row], n_row)
  
  mat = matrix(NA,5,nCV)
  result = data.frame(mat)
  row.names(result) <- c('RMSE','MAE','PCC','RSquared','Adj.RSquared')

  #Notice: R^2 should be calculated on training data rather than test data, otherwise, negative R^2 may occur.
  Y.all.true      = df[, which(colnames(df)==YName) ]       #trueY of all data
  Y.all.predicted = predict(model_step, df)                 #predictedY of all data
  
  tmp = sum( (Y.all.predicted-Y.all.true)^2 ) / sum( (Y.all.true-mean(Y.all.true))^2  )
  R.Sq     = 1 - tmp                                                                            #R-Squared
  R.Sq.Adj = 1 - tmp * (length(Y.all.true)-1) / (length(Y.all.true) - 1 - length(predictors))   #Adj.R-Squared
  
  
  for(i in 1:nCV){
    testIndex = allIndex[cvIndex==i]                                    #test data samples
    model_predict = lm(formula=fm, data=df[-testIndex,])
    
    Y.test.true      = df[testIndex, which(colnames(df)==YName) ]       #trueY of test data
    Y.test.predicted = predict(model_predict, df[testIndex,])           #predictedY of test data
    Y.test.predicted[ Y.test.predicted<0   ] = 0
    Y.test.predicted[ Y.test.predicted>100 ] = 100
    
    result[1,i] = sqrt( mean( (Y.test.true - Y.test.predicted)^2 ) )    #RMSE
    result[2,i] = mean( abs(Y.test.true - Y.test.predicted)   )         #MAE
    result[3,i] = abs( cor(Y.test.true,Y.test.predicted) )              #PCC
    
    #Notice: R.Sq and R.Sq.Adj is based on all data, rather than train data or test data
    result[4,i] = R.Sq                                                  #R-Squared
    result[5,i] = R.Sq.Adj                                              #Adj.R-Squared
  }
  r = apply(result,1,median)
  r = t(r)
  row.names(r) <- c(YName)
  return ( r )
}