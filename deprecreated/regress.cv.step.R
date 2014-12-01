#This function process stepwise features selection in each round of Cross Validation.
#In each round of cross validation, this function firstly select features by using stepwise on training data,
#and the lm are trained on training data, after which, trained model are applied on test data.
#The stwpwise feature selection is only processed for nCV times: in each round of Cross Validation.

Predict = function(YName,method_=algorithm,nCV=N_CV){
  str_formula = paste(
    YName, '~',
    paste(XNames,collapse="+"),
    sep=' '
  )

  n_row = nrow(df)
  allIndex = 1:n_row
  cvIndex = sample( rep(1:nCV,ceiling(n_row/nCV))[1:n_row], n_row)
  
  mat = matrix(NA,5,nCV)
  result = data.frame(mat)
  row.names(result) <- c('RMSE','MAE','PCC','RSquared','Adj.RSquared')
  
  for(i in 1:nCV){
    testIndex = allIndex[cvIndex==i]                                     #test data samples

    model_lm   = lm(  as.formula(str_formula) ,  data=df[-testIndex,])   #on training samples
    model_step = step(model_lm, direction=method_, trace=F)              #both/forward/backward
    
    #model_step = model_lm  #Only use this line for accelerating debuging, no uses in other cases!!
    
    predictors = names(model_step$model)[-1]                            #the first element is Y
    print(paste(length(predictors), 'variables are selected from', length(XNames), 'variables in CV round', i, '.' ))
    
    Y.test.true      = df[testIndex, which(colnames(df)==YName) ]       #trueY of test data
    Y.test.predicted = predict(model_step, df[testIndex,])              #predictedY of test data
    Y.test.predicted[ Y.test.predicted<0   ] = 0
    Y.test.predicted[ Y.test.predicted>100 ] = 100

    result[1,i] = sqrt( mean( (Y.test.true - Y.test.predicted)^2 ) )    #RMSE
    result[2,i] = mean( abs(Y.test.true - Y.test.predicted)   )         #MAE
    result[3,i] = abs( cor(Y.test.true,Y.test.predicted) )              #PCC
    
    
    #Notice: R^2 should be calculated on training data rather than test data, otherwise, negative R^2 may occur.
    Y.train.true      = df[-testIndex, which(colnames(df)==YName) ]       #trueY of train data
    Y.train.predicted = predict(model_step, df[-testIndex,])              #predictedY of train data
    
    tmp = sum( (Y.train.predicted-Y.train.true)^2 ) / sum( (Y.train.true-mean(Y.train.true))^2  )
    result[4,i] = 1 - tmp                                                                                #R-Squared
    result[5,i] = 1 - tmp * (length(Y.train.true)-1) / (length(Y.train.true) - 1 - length(predictors))   #Adj.R-Squared
  }
  r = apply(result,1,median)
  r = t(r)
  row.names(r) <- c(YName)
  return ( r )
}