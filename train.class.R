Predict = function(X, Y, sampling, algorithm, nCV, ...){
  
  x = X[sampling,]
  y = Y[sampling]
  
  keep = y!=0             # First: select training data.
  X = x[keep,]            # Second: anandon the middle group, keep High and Low Score.
  Y = factor( y[keep] )

  ctrl = trainControl(
     method = "cv"
    ,number = nCV
    ,returnResamp = "all"
    ,classProbs = TRUE
    ,savePred = TRUE
    ,summaryFunction = iClassSummary
    ,allowParallel = accelerateInsideTraining
  )
  
  set.seed(7)
  fit = train(
     x = X
    ,y = Y
    ,method = algorithm
    ,trControl = ctrl
    ,...
  )
  
  r = fit$results
  return(r)
}