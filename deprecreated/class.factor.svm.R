
source('discrete.R')

require(e1071)
require(xtable)

Predict = function(YName, biClassify=T){
  Y = discrete.std( t(df[YName]) )
  if(biClassify){
    keep = (Y!=0)
    Y = Y[keep]
    X = df[keep,XBegin:XEnd]
  }else{
    X = df[,XBegin:XEnd]
  }

  
  model  = tune.svm(X, Y,  gamma = 2^(-15:0), cost = 2^(-2:4) )
  summary(model)
  plot (model, transform.x=log10, xlab=expression(log[10](gamma)), ylab="C")
  
  
  n.test.correct = 0
  n.train.total  = 0
  n.train.correct= 0
  
  n_row = nrow(X)
  allIndex = 1:n_row
  cvIndex = sample( rep(1:nCV,ceiling(n_row/nCV))[1:n_row], n_row)
  for(i in 1:nCV){
    testIndex = allIndex[cvIndex==i]                                    #test data samples

    X.train = X[-testIndex,]
    Y.train = Y[-testIndex]

    model  = svm(X.train, Y.train, type='C-classification')
    #model  = best.svm(X.train, Y.train, type='C-classification')
    
    Y.train.pred = predict(model, X.train)
    
    n.train.total  = n.train.total + length(Y.train.pred)
    n.train.correct= n.train.correct + length(Y.train.pred[ Y.train.pred == Y.train])
    
    
    X.test      = X[testIndex,]
    Y.test.true = Y[testIndex]                           #trueY of test data
    Y.test.pred = predict(model,X.test)                  #predictedY of test data
    
    n.test.correct = n.test.correct + length(Y.test.pred[ Y.test.pred == Y.test.true])
  }
  
  r = list(test_acc = n.test.correct/n_row, train_acc = n.train.correct/n.train.total)
  r = t(r)
  row.names(r) <- YName
  (r)
}

