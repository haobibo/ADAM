Predict = function(X, Y, sampling, algorithm, nCV, ...){
  ctrl = trainControl(
    method = "cv"
    ,number = nCV
    ,returnResamp = "all"
    ,summaryFunction = iRegressSummary
  )
  
  x = as.matrix(X[sampling,])
  y = Y[sampling]
  
  set.seed(7)
  fit = train(
     x = x
    ,y = y
    ,method = algorithm
    ,trControl = ctrl
    ,...
  )
  
  r = fit$results
  return(r)
}