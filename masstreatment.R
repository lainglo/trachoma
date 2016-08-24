masstreatment <- function(initstate,pars,coverage){
  
  xa <- initstate[["xa"]]
  ya <- initstate[["ya"]]
  xc <- initstate[["xc"]]
  yc <- initstate[["yc"]]
  
  ya.new <- rbinom(1,size=ya,prob=1-pars[["efficacy"]]*coverage[["adults"]])
  adult.cured <- ya - ya.new
  xa.new <- xa + adult.cured
  
  yc.new <- rbinom(1,size=yc,prob=1-pars[["efficacy"]]*coverage[["children"]])
  children.cured <- yc - yc.new
  xc.new <- xc + children.cured
  
  trt.info <- c(xa=xa.new, xc=xc.new, ya=ya.new, yc=yc.new)
    
}

#initstate <- c("xa"= 90,"xc"=90,"ya"=10,"yc"=10)
#pars <- c("efficacy" = 0.9)
#coverage <- c("adults"=0.8, "children"=0.8)

#show(masstreatment(initstate,pars,coverage))