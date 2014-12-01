require(xtable)

output = function(result, output.file){
  vars = ls(result)

  for(var in vars){
    rtable = get(var ,envir = result)
    
    caption = gsub("_", "@", var, fixed = TRUE)
    rTable = xtable(rtable, caption = caption, display=rep('G',1+ncol(rtable)))
    
    write("%=========================================\n", append=T, file=output.file)
    print.xtable(rTable, append=T,
          table.placement = NULL, caption.placement = "top",
          file=output.file)
    write("\\clearpage\n%-----------------------------------------",append=T,file=output.file)
  }
  
  return(list(
    file = output.file,
    tables = length(vars)
  ))
}