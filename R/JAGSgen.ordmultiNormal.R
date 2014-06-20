JAGSgen.ordmultiNormal <-
function (X, Z, n.cut){
  num_l1_v <- ncol(X)
  num_l2_v <- ncol(Z)
  num_id <- nrow(Z)
  inits <- new.env() # store initial values for BUGS model
  monitorl1.parameters <- character()  # store monitors for level 1 parameters, which will be used in the computation of sum of squares
  monitorl2.parameters <- character()  # store monitors for level 2 parameters
  
  ### generate the code for BUGS model
  sModel <- paste("model{", sep="")
  ### level 1 likelihood 
  sModel <- paste(sModel,"
  for (i in 1:n){
    y[i] ~ dcat(P[i,])
    P[i,1] <- 1 - Q[i,1]
    for (i.cut in 2: n.cut) {
      P[i,i.cut] <- Q[i,i.cut-1] - Q[i,i.cut]	  
    }
    P[i,n.cut+1] <- Q[i,n.cut]
    for (i.cut in 1:n.cut){
      logit(Q[i,i.cut]) <- z[i,i.cut]
    ","z[i,i.cut] <-")
  for (i in 1:num_l1_v){
    if (i != num_l1_v)
      sModel <- paste(sModel,"beta",i,"[id[i]]","*","X[i,",i,"]+",sep="")
    else
      sModel <- paste(sModel,"beta",i,"[id[i]]","*","X[i,",i,"]",sep="")
    # generate inits for betas
    s <- paste("inits$","beta",i,"<-rep(",1/max(X[,i]), ",", num_id,")",sep="")
    s1 <- paste("inits$","sigma.beta",i,"<-runif(",1,")",sep="")
    eval(parse(text = s))
    eval(parse(text = s1))
    for (j in 1:num_id)
      monitorl1.parameters<-c(monitorl1.parameters, paste("beta",i,"[",j,"]",sep=""))
  }
  sModel <- paste(sModel,"-cutp[i.cut]*(1-equals(i.cut,1))
    }",sep="")
  sModel <- paste(sModel,"
  }",sep="")
  
  ### level 2 likelihood
  for (i in 1:num_l1_v){
    sModel <- paste(sModel,"
  for (i in 1:M){
    ","beta",i,"[i]~dnorm(mu.beta",i,"[i]",",tau.beta",i,")
    mu.beta",i,"[i]<- ",sep="")
    for (j in 1:num_l2_v){
      if (j != num_l2_v)
        sModel <- paste(sModel,"beta",i,'_',j,"*Z[i,",j,"]+",sep="")
      else
        sModel <- paste(sModel,"beta",i,'_',j,"*Z[i,",j,"]",sep="")
    }
    sModel <- paste(sModel,"
  }",sep="")
    sModel <- paste(sModel,"
  tau.beta",i,"<-pow(sigma.beta",i,",-2)
  sigma.beta",i,"~dgamma(1,1)",sep="")
    ### level 2 priors
    for (j in 1:num_l2_v){
      sModel <- paste(sModel,"
  beta",i,'_',j,"~dnorm(0,0.0001)",sep="")
      s<-paste("inits$","beta",i,'_',j,"<-",1/max(Z[,j]),sep="")
      eval(parse(text=s))
      monitorl2.parameters <- c(monitorl2.parameters,paste("beta",i,'_',j,sep=""))
    }
  }
  
  sModel <- paste(sModel,"
  for (i.cut in 1: n.cut) {
    cutp0[i.cut] ~ dunif(0,10)
  }
  cutp[1:n.cut] <- sort(cutp0)",sep="")

  sModel<- paste(sModel,"
}")
  inits$cutp0=c(0,sort(runif(n.cut-1,0,6)))
  monitor.cutp <- character()
  for (i in 2:n.cut)
    monitor.cutp <-c(monitor.cutp, paste('cutp[',i,']',sep=""))
  sol.inits <- dump.format(as.list(inits))
  results <- list(inits = sol.inits, monitorl1.parameters = monitorl1.parameters, 
                  monitorl2.parameters = monitorl2.parameters, monitor.cutp = monitor.cutp, sModel = sModel)
  return(results)
}
