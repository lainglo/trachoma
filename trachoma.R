rm(list=ls())

setwd("~/Documents/Trachoma")

t0 <- 1
tend<- 100

#test

k = 2/12
params <- c("gamma.a" = 1/12, "gamma.c" = 1/12, "beta.c.from.a" = k, 
            "beta.c.from.c" = k, "beta.a.from.a" = k, "beta.a.from.c" = k)


#params <- c("gamma.a" = 1/12, "gamma.c" = 1/12, "beta.c.from.a" = 1/12, 
#            "beta.c.from.c" = 2/12, "beta.a.from.a" = 2/12, "beta.a.from.c" = 2/12)
state <- c("xa" = 100, "xc" = 99, "ya" = 0, "yc" = 1)

source("trachoma1step.R")


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
  
  #time based iteration
  while(curtime <= tend) {
    if (verbose) {
      cat("curtime="); cat(curtime); cat("\n")
    }
    
    #stepping forward one time step
    #output will be newtime, newstate
    one.step <- trachoma1step(curtime, params, curstate)
    
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
  output
}

for (j in 1:1){
  
  
  simData <- trachoma(t0, tend, params, state, verbose = TRUE)
  
  pop <- sum(state)
  
  child.prev = simData$yc/(simData$xc+ simData$yc)
  adult.prev = simData$ya/(simData$xa+ simData$ya)
  
  timestep = simData$time
  
  plot(timestep, adult.prev, main="Simulation ")
  
  show(tail(simData))
  
  initstate <- simData[dim(simData)[1],]
  
}

show("-------end------")
