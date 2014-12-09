# author: Peter_Howe <haobibo@gmail.com>
# Notice: the code is under re-construction now, code cannot run.

f.selectX <- function(X, selectX){            ####### select X
  (X)
}

f.discreteX <- function(X, discreteX){        ####### discrete X
  if(discreteX=='no'){           ###################### no preprocess: just return X as it is
    return (X)
  }
  
  args   = unlist(  strsplit(discreteX, '_')  ) ## reduceDimX should follow reg of quantile_5 / order
  #print(args)
  
  discrete = args[1]     # should be one of [order, quantile]
  if(discrete == 'order'){           ###################### transform X to order
    r = transform.order(X)
    (r)
  }else if(discrete == 'quantile'){  ###################### discreete X to N groups
    if(length(args)>1){
      groups = as.integer( args[2] )        # quantile_5 quantile_7 ...
      r = transform.quantile( X , groups )
    }else{
      r = transform.quantile( X )
    }
    (as.data.frame(r))
  }else if(discrete == 'quantile.factor'){
    if(length(args)>1){
      groups = as.integer( args[2] )        # quantile_5 quantile_7 ...
      r = transform.quantile.factor( X , groups )
    }else{
      r = transform.quantile.factor(X)
    }
    (as.data.frame(r))
  }
  else{
    stop('Unknow data preprocess optinos!')
  }
}

f.reduceDimX <- function(X, reduceDimX){
  if(reduceDimX=='no'){           ###################### no preprocess: just return X as it is
    return (X)
  }
  
  args   = unlist(  strsplit(reduceDimX, '_')  ) ## reduceDimX should follow reg of PCA_0.8 / SVD_100
  #print(args)
  
  reduce = args[1]           #### should be one of [PCA, SVD]
  if(reduce == 'PCA'){       ###################### do PCA to X
    PCA.ratio = as.double( args[2] )       # PCA_0.8 PCA_0.9 ...
    
    dcor = cor(X)
    deig = eigen(dcor)$values
    thresold = sum(deig) * PCA.ratio
    
    idx = 0
    cumulate = 0
    while(cumulate<thresold){
      idx = idx + 1
      cumulate = cumulate + deig[idx]
    }
    pca = princomp(X, cor=T)
    r = data.frame(pca$score[,1:idx])
    (r)
    
  }else if(reduce == 'SVD'){  ###################### do SVD to X
    SVD_Dim = as.integer( args[2] )        # SVD_100 SVD_120 ...
    
    X0 = scale(X, center = T, scale = F)   # centerize each dimension
    X1 = svd(X0,  SVD_Dim)
    r = data.frame(X1$u)
    (r)
  }else{
    stop('Unknow data preprocess optinos!')
  }
} #f.reduceDimX

f.standardizeY <- function(Y, standardizeY){  ####### standardize Y
  s = standardizeY
  if(s=='no'){
    r = dummy(Y)
  }else if(s=='scale'){
    r = standardize.scale(Y)
  }else{
    stop('Unknow Standardize Method!')
  }
  #names(r) <- names(Y)
  (r)
}

f.groupingY <- function(Y, groupingY){        ####### grouping Y
  g = groupingY
  if(g=='no'){
    return(Y)
  }
  if(g=='factor'){
    r = grouping.factor(Y)
  }else if(g=='bifactor'){
    r = grouping.bifactor(Y)
  }else if(g=='High~Rest'){  
    r = grouping.cutoff(Y)
  }else if(g=='High~Low'){
    r = grouping.std(Y)
  }else{
    stop('Unknow Groupping Method!')
  }
  (r)
}

f.samplingY <- function(y, sampling){  ############# do sampling, return index
  r = lapply(y, function(col){
    v = 1:length(col)
    if( sampling=='no' ){
      return (v)
    }
    
    if(sampling=='downSample'){
      tmp = downSample(v, col, list=T)
    }else if(sampling=='upSample'){
      tmp = upSample(v ,col,list=T)
    }else{
      stop('Unknow sampling method!')
    }
    return(tmp$x)
  })
  
  return (list(Y=y, idx=r))
  #idx should not be data.frame since for different colums, sampling colud have different length
}

f.combine <- function(x, y){  ###################### combine X and Y together by cartesian product
  nCfg = cartesian(names(x), names(y))
  nDat = lapply(nCfg, function(i){paste( t(i), collapse = ' ; '  )})
  dat  = cartesian(X=x,Y=y)
  names(dat) <- nDat
  (dat)
}

############################################################################
`%op%` <- if(getDoParWorkers() > 1 && !accelerateInsideTraining)  `%dopar%` else  `%do%`  #preparation for for each


preprocess = function(X, Y, fdata = NULL, ...){
  e <- list(...)
  p = c()
  
  ##-------------- Pre-process of X ---------------##
  x  = list(X)
  if(!is.null(e$SelectX))      {x = fork(X = x  ,selectX       = e$SelectX     ); p = c(p,'SelectX')     }
  if(!is.null(e$DiscreteX))    {x = fork(X = x  ,discreteX     = e$DiscreteX   ); p = c(p,'DiscreteX')   }
  if(!is.null(e$ReduceDimX))   {x = fork(X = x  ,reduceDimX    = e$ReduceDimX  ); p = c(p,'ReduceDimX')  }

  ##-------------- Pre-process of Y ---------------##
  y  = list(Y)
  if(!is.null(e$StandardizeY)) {y = fork(Y = y  ,standardizeY = e$StandardizeY ); p = c(p,'StandardizeY')}
  if(!is.null(e$GroupingY))    {y = fork(Y = y  ,groupingY    = e$GroupingY    ); p = c(p,'GroupingY')   }
  if(!is.null(e$SamplingY))    {y = fork(Y = y  ,samplingY    = e$SamplingY    ); p = c(p,'SamplingY')   }
  
  ##----------- Combine Y and X together ----------##
  df = f.combine(x,y)
  
  env = new.env()
  assign('.param', p, envir = env)
  
  lapply(names(df), function(i){
    format = gsub("\\|"," %s:", i, perl = T)  # sep char '|' is defined in fork function.
    name   = do.call(sprintf, c(format, as.list(p)))
    
    v      = df[i]
    rnames = row.names(v)
    data   = t(v)
    names(data) = rnames
    
    #assign the variable to newly created env
    assign(name, list(X=data.frame(data$X),  Y=data.frame(data$Y$Y), idx = data$Y$idx), envir = env)
  })#lapply
  
  if(!is.null(fdata)) save(env, file=fdata)
  
  ## Now, pre-processed data are stored in environment env
  return(env)
}#function preprocess


run.model = function(ENV, data, algorithms, nCVs, output.file=NULL, ...){
  cat('Conditions:', length(ls(data)), '\n')
  
  cfgs   = cartesian(algorithm=algorithms, nCV=nCVs)
  rnames = row.names(cfgs)

  #foreach (cfg=cfgs) %:%
    #foreach(v=ls(data))  %op% {
  
  for (cfg in cfgs) {
    for(v in ls(data))  {
      
      h       = t(cfg)
      names(h)= rnames
      strCfg  = sprintf("Algorithm=%s CV=%d", h$algorithm, h$nCV)
      
      caption = gsub(":","=", v, perl = T)
      caption = paste(caption, strCfg, sep=' ; ')
      cat('Condition:',caption ,'\n')
      
      GroupingY = str_extract(caption, 'GroupingY=([^ ]+)')
      GroupingY = substr(GroupingY,11,nchar(GroupingY))
      
      d   = get(v, envir = data)
      XX  = d$X
      YY  = d$Y
      idx = d$idx
      
      YNames = names(YY)
      
      result = NULL
      result = lapply(YNames,  function(YName){
        sampleIdx = unlist( idx[YName] )
        
        r = do.call('Predict', c(list(X=XX, Y=YY[,YName], sampling=sampleIdx), h, list(GroupingY=GroupingY), list(...)) )
        result = if(is.null(result))  r  else  rbind(result,r)
        
        if(nrow(result)>1)
          row.names(result) <- paste(YName, 1:nrow(result), sep='#' )
        else
          row.names(result) <- c(YName)
        
        cat(sprintf('YDim[%s] with %d results.\n', YName, nrow(result)))
        
        return (result)
      }) #lapply
      
      tab = do.call(rbind, result)
      assign(caption, as.data.frame(tab), envir = ENV)

    }#inner foreach
  }#comment this line if you use foreach
  
  if(!is.null(output.file)) save(ENV, file=output.file)
  return(ENV)
}
