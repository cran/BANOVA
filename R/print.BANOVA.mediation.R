print.BANOVA.mediation <- 
  function(x, ...){
    cat('Direct effect of', x$xvar,':\n')
    for (i in 1:length(x$dir_effects))
      print(noquote(x$dir_effects[[i]]))
    cat('Indirect effect of', x$xvar,':\n')
    for (i in 1:length(x$indir_effects))
      print(noquote(x$indir_effects[[i]]))
  }