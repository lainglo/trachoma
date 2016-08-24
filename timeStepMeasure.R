rm(list=ls(all=TRUE))


setwd("~/Documents/Trachoma/")

t0 <- 1
tend<- 500

#test

k = 0/12
params <- c("gamma.a" = 1/12, "gamma.c" = 1/12, "beta.c.from.a" = k, 
            "beta.c.from.c" = k, "beta.a.from.a" = k, "beta.a.from.c" = k)


#params <- c("gamma.a" = 1/12, "gamma.c" = 1/12, "beta.c.from.a" = 1/12, 
#            "beta.c.from.c" = 2/12, "beta.a.from.a" = 2/12, "beta.a.from.c" = 2/12)
state <- c("xa" = 0, "xc" = 0, "ya" = 100, "yc" = 100)

source("trachoma1step.R")
source("masstreatment.R")


trachoma <- function(t0, tend, params, state, verbose=FALSE) {
  #data frame for output:
  # xa(total susceptible adults), xc(total susceptible child), 
  #ya(inf adult), yc(inf child),
  # time
  output <- as.data.frame(as.list(state))
  
  #initial time
  output$time <- t0
  #current time, starts at initial time
  curtime <- t0
  #current state of system
  curstate <- state
  
  
  err.count <- 0
  #time based iteration
  while(curtime <= tend) {
    if (verbose) {
      cat("curtime="); cat(curtime); cat("\n")
    }
    
    #stepping forward one time step
    #output will be newtime, newstate
    one.step <- trachoma1step(curtime, params, curstate)
    if (one.step$err){
      
      err.count <- err.count +1
      
    }
    #current time updates to new time
    curtime <- one.step$newtime
    #current state to new state
    curstate <- one.step$newstate
    
    #append data structure with new info
    if(curtime <= tend) {
      newrow <- as.data.frame(as.list(curstate))
      newrow$time <- curtime
      output <- rbind(output, newrow)
    } else {
      newrow <- output[dim(output)[1],]
      newrow$time <- tend
      output <- rbind(output,newrow)
    }
  }
  show(err.count)
  output
}


  
  simData <- trachoma(t0, tend, params, state, verbose = TRUE)
#for the simulation, we need:
#initial state, pars(efficacy), coverage - for masstreatment
  initstate <- simData[dim(simData)[1],]

  trac.obj <- data.frame(time=c(0,3,6,9), 
                  coverage.adult=c(0.8,0.8,0.8,0.8), 
                coverage.child=c(0.8,0.8,0.8,0.8))
  paramas <- c("efficacy" = 0.9)

abs.time <- tend


#init.state <- initstate
entire.output <- initstate

for (ii in 1:length(trac.obj$time)){
  
  
  covg <- c("adults"=trac.obj$coverage.adult[1], "children"=trac.obj$coverage.child[1])
  trt.state <- masstreatment(initstate=entire.output[dim(entire.output)[1],], pars=paramas, coverage=covg)  
  show(trt.state)
  newtime <- abs.time + 3
  
  init.state <- trachoma(t0=abs.time,tend=newtime,params, trt.state)
  
  entire.output <- rbind(entire.output, init.state)
  abs.time <- newtime
}

entire.output

prev.adult <- entire.output$ya/(entire.output$ya+entire.output$xa)
plot(entire.output$time,prev.adult)
  


  
  

show("-------end------")
