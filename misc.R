cartesian = function(...){
  r = data.frame(t(expand.grid(         #Here goes each parameters
    ...
  )))
}

fork <- function(...){     #arg1 should be data, arg2 should be fork condition
  dots <- list(...)        #regulization: we regulate that the arg2 should have the name of function that will call
  fun  <- tail( names(dots), n=1 ) #last element of the list should be function name
  
  ###### this line of code will fork the procedure by cartesina product  #######
  cfg = cartesian(...)  #values will be named as arg2 sepcifed
  conditions = names(cfg)

  v = lapply(conditions, function(i){
    r = do.call(
      paste('f.',fun,sep=''),  #function name: f.ARG_2_NAME
      t(cfg[i])                #parameters pass to function
    ) #do.call
    (r)
  })
  
  last.level <- names(dots)[1]
  e          <- names( dots[[1]]  )     #t(cfg[last.level,])
  if(is.null(e) || class(e) %in% c('list','numeric','matrix','data.frame')){
    e = row.names(cfg[last.level,])  #row.name / names
    if (is.null(e)) e = names(cfg[last.level,])  #row.name / names
    e = rep(e, ncol(cfg) )
  }
  tmp <- t(cfg[fun,])
  names(v) <- paste(e,tmp,sep='|')
  (v)
}

dummy <- function(d){               ###################### dummy, returen data as they are
  r = lapply(d, function(i){i} )
  return(r)
}

standardize.normal <- function(d){  ###################### normalize data to N(0,1) by col
  r = lapply(d, function(i){100*(i-mean(i))/sd(i)} )
  (r)
}

standardize.scale <- function(d){   ###################### normalize data to [0,100]
  r = lapply(d, function(i){100*(i-min(i))/(max(i)-min(i))} )
  (r)
}

grouping.factor = function(d, to.factor=T){
  tmp = factor(d)
}

grouping.bifactor = function(d, to.factor=T){
  tmp = factor(unlist(d))
  levs = levels(tmp)
  if(length(levs)>2){
    print(levs)
    stop('bifactor groups y in no more thant 2 classes!')
  }
  levels(tmp)<-c('Negative', 'Positive')
  r = list(tmp)
  names(r) = names(d)
  return(r)
}

grouping.cutoff = function(d, cutOffScore=NULL, to.factor=T){
  f = Vectorize(  function(i,cut){
    if(i>=cut) ('Positive') else ('Negative')
  })

  r = lapply(d,  function(x){
    if(is.null(cutOffScore))  cutOffScore = mean(x) + sd(x)
    tmp = f(x,cutOffScore)
    if(to.factor) tmp = as.factor(tmp)
    (tmp)
  })
  
  return(r)
}

grouping.std = function(d, cutOffScore = NULL, to.factor=T){
  f = Vectorize(  function(i,cutHigh, cutLow){
    if(i>=cutHigh) ('Positive')
    else if(i<=cutLow) ('Negative')
    else (0)
  })
  
  r = lapply(d,function(x){
    if(is.null(cutOffScore)) cutOffScore = 1
    avg = mean(x)
    std = sd(x)
    cutOffHigh = avg + std * cutOffScore
    cutOffLow  = avg - std * cutOffScore
    tmp = f(x, cutOffHigh, cutOffLow)
    if(to.factor) tmp = as.factor(tmp)
    (tmp)
  })
  return (r)
}

discrete = function(grouping, x, cutOffScore=NULL, to.factor=T){
  if(grouping=='High~Rest'){
    return (discrete.cutoff(x, cutOffScore, to.factor))
  }else if(grouping=='High~Low'){
    return (discrete.std(x, cutOffScore, to.factor))
  }else{
	  stop('Unknown grouping method! ')
  }
}



#summary function
iClassSummary = function (data, lev = NULL, model = NULL, debug=F) 
{
  pred = data$pred  #data[, "pred"]
  obs  = data$obs   #data[, "obs"]
  
  
  if (!all(levels(pred) == levels(obs))) 
    stop("levels of observed and predicted data do not match")

  Pos = 'Positive'
  
  rocObject <- try(pROC::roc(obs, data[, Pos]), silent = TRUE)
  rocAUC <- if(class(rocObject)[1] == "try-error") NA else rocObject$auc
  
  precision = posPredValue(pred, obs, positive=Pos)
  recall    = sensitivity( pred, obs, positive=Pos)
  #specifity = specificity( pred, obs, positive=Pos)
  
  #cMat = confusionMatrix(pred, obs), positive=Pos)
  #pos  = cMat$postitive
  #if(is.null(pos) || pos != "Positive") stop('Positive class is not set properly!')
  
  # For binary classification: tab will be only one line
  # While for multi class classification, there will be multi lines in tab
  #tab  = cMat$byClass
  #row = ifelse( (nrow(tab)>1), tab['Class: Positive',], tab)
  #precision  = row['Pos Pred Value']
  #recall     = row['Sensitivity']
  
  
  f1 = 2 * precision * recall / (precision + recall)
  
  out <- c(rocAUC, precision, recall, f1)
  names(out) <- c("ROC", "Precision", "Recall","F1")
  return(out)
}
