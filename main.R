
twd = 'Z:/'
enableParallel = T

data.set = list(
   list(data_file = "mini-data.csv",                 XBegin = 12, YBegin = 2, YEnd   = 11)  #1
  ,list(data_file = "TimeSequence_all_features.csv", XBegin = 50, YBegin = 3, YEnd   = 24)  #2
  ,list(data_file = "data-suicide.csv",              XBegin = 6,  YBegin = 1, YEnd   = 5 )  #3
  ,list(data_file = "data-SWB.csv",                  XBegin = 13, YBegin = 2, YEnd   = 12)  #4
)[[3]]

predict_source = c(
   'train.regress.R'                                      #1
  ,'train.class.R'                                        #2
  ,'train.class.exp1.R' #Tr=H~R,DownSample;  Test:H~R     #3
  ,'train.class.exp2.R' #Tr=H~L,no sample ;  Test:H~R     #4
)[1]

demo_source = c('output.tex.R')

.preprocess = list(
   SelectX      = list('all')
  ,DiscreteX    = list('no', 'order', 'quantile','quantile.factor')[4:4]
  ,ReduceDimX   = list('no', 'PCA_0.8', 'SVD_2')[1:1]
  ,StandardizeY = list('no', 'scale')[2:2]
  ,GroupingY    = list('no', 'factor', 'bifactor', 'High~Rest', 'High~Low')[1:1]
  ,SamplingY    = list('no', 'downSample', 'upSample' )[1:1]
)

.run.model = list(
    algorithms   = list('lmStepAIC','lm','rlm', 'lasso', 'earth',
                       'J48','bayesglm', 'LogitBoost','svmRadial','rf')[c(10)] #4,5,9,
   ,nCVs         = list(5)# ,10)
   
   #-------------- Following arguments will be passed to caret train function directly
   #,preProc = c("center", "scale")
   ,metric = c('ROC','PCC')[2]
   ,tuneLength = 10
   #--------------
)

##################### Here ends the settings of parameters
require(caret)
require(pROC)
require(stringr)
require(doParallel)

data_file <<- data.set$data_file
df    = read.csv(data_file, head=T, stringsAsFactors=T)
DNames = colnames(df)

YBegin <<- data.set$YBegin
YEnd   <<- data.set$YEnd
XBegin <<- data.set$XBegin
XEnd   <<- length(df)

YNames <<- DNames[YBegin:YEnd]
XNames <<- DNames[XBegin:XEnd]

X = df[XNames]
Y = df[YNames]

##################### Here begins the main part of program
setwd(twd)

N_Cores = detectCores()

branches = lapply(.preprocess,function(i){return(length(i))})
branches = prod(unlist(branches))
accelerateInsideTraining = T  #branches < N_Cores

source(demo_source)
source(predict_source)
source('misc.R')
source('workflow.R')

if(enableParallel){
  cluster = makeCluster( N_Cores )
  registerDoParallel( cluster )
  clusterExport(cluster, varlist=ls(), envir = environment())
  clusterEvalQ(cluster, {
    require(caret)
    require(pROC)
    require(stringr)
    require(xtable)
  })
}

time = list()
time$preprocess = system.time({
  data   = do.call(preprocess, c(list(X=X,Y=Y), .preprocess ))
})[3]

time$runmodel = system.time({
  ENV = new.env()   #results will be stored in ENV
  result = do.call(run.model,  c(list(ENV=ENV,data=data), .run.model))
  #print(  ls(result)  )
})[3]

time$output = system.time({
  fname = sprintf('result[%s]%s.tex', predict_source, format(Sys.time(), "%Y%m%d_%H%M%S"))
  info   = do.call(output, list(result=result, output.file=fname))
})[3]

cat('Time consumed to run model:\n')
print(data.frame(time))

print(info)

############################################################END
if(enableParallel)stopCluster(cluster)  #close cluster