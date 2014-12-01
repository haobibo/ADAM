Predict = function(X,Y,algorithm, nCV, ...){
  
  keep = Y!=0             # First: select training data.
  X = X[keep,]            # Second: anandon the middle group, keep High and Low Score.
  Y = Y[keep]
  Y = factor( Y )

  ctrl = trainControl(
     method = "cv"
    ,number = nCV
    ,returnResamp = "all"
    ,classProbs = TRUE
    ,savePred = TRUE
    ,summaryFunction = iClassSummary  #defaultSummary
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