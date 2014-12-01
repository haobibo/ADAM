Predict = function(X,Y, algorithm, nCV, ...){
  
  training  = createDataPartition(Y, p=0.8)[[1]]   #train data samples
  testing   = (1:length(Y))[-training]             #test  data samples
  
  #### Testing Dataset
  XTest = X[testing,]
  YTest = factor(Y[testing])
  
  #### Training Dataset
  e = list(...)
  if(!is.null( e$GroupingY )){
    g = e$GroupingY
    if(g=='High~Low'){                #Train: High Score vs Low Score
      middleGrp = (1:length(Y))[Y==0] # Exclude midlle score part from training set
      training  = setdiff(training, middleGrp)

      YTest[YTest==0] = 'Negative'    # To testing dataset, set Y Values in middle group to Negative
      YTest = factor(YTest)
    }
  }
  XTrain  = X[training,]              #Select all the training part
  YTrain  = factor( Y[training] )
  
  #fit control: no cross validation, just train the tuned model.
  ctrl = trainControl(
     method = "cv"
    ,number = nCV
    ,returnResamp = "all"
    ,classProbs = TRUE
    ,savePred = TRUE
    ,summaryFunction =iClassSummary # iClassSummary2#, twoClassSummary, defaultSummary
    ,allowParallel = accelerateInsideTraining
  )
  
  set.seed(7)
  
  # train the model on training data
  fit = train(
     x = XTrain
    ,y = YTrain
    ,method = algorithm
    ,trControl = ctrl
    #,...
    ,preProc = c("center", "scale")
    ,metric = 'ROC'
    ,tuneLength = 10
  )
  
  pred = predict.train(fit, XTest) #, type = 'prob'
  tmp = list(pred=pred, obs=YTest)

  r = iClassSummary(tmp)
  r = data.frame( t(r) )
  return(r)
}