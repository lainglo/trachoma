trachoma1step <- function(t0, params, state, verbose=FALSE) {
  
  error <- FALSE
  #defining initial state parameters
  
  xa <- state[["xa"]]
  xc <- state[["xc"]]
  ya <- state[["ya"]]
  yc <- state[["yc"]]
  
  aprev <- ya/(ya+xa)
  cprev <- yc/(yc+xc)
  
  if (any(is.na(c(xa,xc,ya,yc)))) {
    stop("missing state")
  }
  if (xa < 0 || xc < 0 || ya < 0 || yc < 0) {
    stop("negative state")
  }
  if (aprev > 1 || cprev > 1) {
    step("illegal prevalence")
  }
  
  #defining initial rates
  
  rate.adult.rec <- ya*params[["gamma.a"]]
  rate.child.rec <- yc*params[["gamma.c"]]
  rate.child.inf <- xc*(params[["beta.c.from.a"]]*aprev + params[["beta.c.from.c"]]*cprev )
  rate.adult.inf <- xa*(params[["beta.a.from.c"]]*cprev + params[["beta.a.from.a"]]*aprev )
  
  #all rates and sum of rates
  
  all.rates <- c(adultrec = rate.adult.rec, childrec = rate.child.rec, adultinf = rate.adult.inf, 
                   childinf = rate.child.inf)
  if (any(all.rates < 0)) {
    stop("illegal rate")
  }
  tot.rate <- sum(all.rates)
  
  #time between events\
  try(etime <- rexp(1, tot.rate))
  
  if (is.na(etime)) {
    etime <- 0.1 #change later **
    error <- TRUE
    #stop("missing etime")
  }
  
  #choosing which event occurred/which state to move toward
  
  whatevent <- as.vector(rmultinom(1, 1, all.rates))
  names(whatevent) <- names(all.rates)
  newstate <- state
  
  #moving system forward to new state
  
  #adult recovers
  if(whatevent[["adultrec"]] == 1) {
    newstate[["xa"]] <- state[["xa"]] + 1
    newstate[["ya"]] <- state[["ya"]] - 1
    
    #child recovers
  } else if(whatevent[["childrec"]] == 1) {
    newstate[["xc"]] <- state[["xc"]] + 1
    newstate[["yc"]] <- state[["yc"]] - 1
    
    #adult is infected
  } else if(whatevent[["adultinf"]] == 1) {
    newstate[["xa"]] <- state[["xa"]] - 1
    newstate[["ya"]] <- state[["ya"]] + 1
    
    #child is infected
  } else if(whatevent[["childinf"]] == 1) {
    newstate[["xc"]] <- state[["xc"]] - 1
    newstate[["yc"]] <- state[["yc"]] + 1
  }
  
  list(newtime = t0 + etime,
       newstate = newstate, err = error)
}

#trachoma(t0, tend, params, state)
