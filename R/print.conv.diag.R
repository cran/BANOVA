print.conv.diag <-
function(x, ...){
  cat('\n')
  cat('Geweke Diag.\n')
  print(as.table(x$sol_geweke))
  cat('\n')
  cat("Heidelberger and Welch's Diag.\n")
  print(as.table(x$sol_heidel[,1:3]))
}
