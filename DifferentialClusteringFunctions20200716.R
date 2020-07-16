#############
# Functions to support the app for 
#   "Sample size for partially or differentially clustered designs with 
#    baseline measurements on independent subjects or subjects nested in clusters"
#    By Steven Teerenstra et al.


# J Kasza, 2020-07-16


var_design1 <- function(n1, k1, k0, sig1clus, sigind, r, rho1){
  
  #Function to calculate the variance for Design 1
  #n1: cluster size at FU in intervention
  #k1: number of clusters at FU in intervention
  #k0: number of clusters at FU in control
  #sig1clus: unconditional variance of the outcome of a subject in the intervention arm
  #r: test-retest reliability
  #sigind: total variance in subjects at baseline
  #rho1: ICC in the intervention arm at follow up

  myvar <- (1 + (n1-1)*rho1)*sig1clus/(k1*n1) + sigind/k0 - (r^2)*sigind*(1/k0 + 1/(k1*n1))    
 
   return(myvar)
  
}

var_design2 <- function(n1, k1, n0, k0, sig1clus, sig0clus, sigind, r, rho1, rho0){
  
  #Function to calculate the variance for Design 2
  #n1: cluster size at FU in intervention
  #n0: cluster size at FU in control
  #k1: number of clusters at FU in intervention
  #k0: number of clusters at FU in control
  #sig1clus: unconditional variance of the outcome of a subject in the intervention arm
  #sig0clus: unconditional variance of the outcome of a subject in the control arm
  #r: test-retest reliability
  #sigind: total variance in subjects at baseline
  #rho1: ICC in the intervention arm at follow up
  #rho0: ICC in the control arm at follow up
  
  myvar <- (1 + (n1-1)*rho1)*sig1clus/(k1*n1) +  (1 + (n0-1)*rho0)*sig0clus/(k0*n0) - (r^2)*sigind*(1/(k0*n0) + 1/(k1*n1))    
  
  return(myvar)
  
}

var_design3 <- function(n, k1,  k0, sig1clus, sig0clus, sigbase, r, rho1, rho0, rhobase){
  
  #Function to calculate the variance for Design 3
  #n: cluster sizes
  #k1: number of clusters at FU in intervention
  #k0: number of clusters at FU in control
  #sig1clus: unconditional variance of the outcome of a subject in the intervention arm
  #sig0clus: unconditional variance of the outcome of a subject in the control arm
  #sigbase:  unconditional variance of a subject at baseline
  #r: test-retest reliability
  #rho1: ICC in the intervention arm at follow up
  #rho0: ICC in the control arm at follow up
  #rhobase: ICC at baseline
  
  myvar <- (1 + (n-1)*rho1)*sig1clus/(k1*n) +  (1 + (n-1)*rho0)*sig0clus/(k0*n)   - (r^2)*sigbase*(1/(k0*n) + 1/(k1*n))*(1+(n-1)*rhobase)    
  
  return(myvar)
  
}


var_design4 <- function(n1, k1,  n0, k0, m, P,  sig1clus, sig0clus, sigbase, rho1, rho0, rhobase, rhoS){
  
  #Function to calculate the variance for Design 4
  #n1: cluster size at FU in intervention
  #n0: cluster size at FU in control
  #k1: number of clusters at FU in intervention
  #k0: number of clusters at FU in control
  #m: size of clusters at baseline
  #P: number of clusters at baseline
  #sig1clus: unconditional variance of the outcome of a subject in the intervention arm
  #sig0clus: unconditional variance of the outcome of a subject in the control arm
  #sigbase:  unconditional variance of a subject at baseline
  #rho1: ICC in the intervention arm at follow up
  #rho0: ICC in the control arm at follow up
  #rhobase: ICC at baseline
  #rhoS: correlation between two repeated measurements in the situation at baseline on a subject conditional on its cluster 
  
  pi1 <- n1/m
  pi0 <- n0/m
  mycoeff <- (rhoS^2)/((1+ (2*m*rhobase)/((1-rhobase)*(1/pi0 + 1/pi1)) )^2)
  
  myvar <- (1 + (n1-1)*rho1)*sig1clus/(k1*n1) +  (1 + (n0-1)*rho0)*sig0clus/(k0*n0)  - (mycoeff^2)*sigbase*((1-rhobase)*(1/(k0*n1) + 1/(k1*n0)) + rhobase*2/P )   
  
  return(myvar)
  
}

var_design5 <- function(n1, k1, n0, k0, sig1clus, sig0clus, sigbase, rho1, rho0, rhoS, rhobase){
  
  #Function to calculate the variance for Design 5
  #n1: cluster size at FU in intervention
  #n0: cluster size at FU in control
  #k1: number of clusters at FU in intervention
  #k0: number of clusters at FU in control
  #sig1clus: unconditional variance of the outcome of a subject in the intervention arm
  #sig0clus: unconditional variance of the outcome of a subject in the control arm
  #sigbase:  unconditional variance of a subject at baseline
  #rho1: ICC in the intervention arm at follow up
  #rho0: ICC in the control arm at follow up
  #rhobase: ICC at baseline
  #rhoS: correlation between two repeated measurements in the situation at baseline on a subject conditional on its cluster 
  
  
  myvar <- (1 + (n1-1)*rho1)*sig1clus/(k1*n1) +  (1 + (n0-1)*rho0)*sig0clus/(k0*n0)   - (rhoS^2)*sigbase*(1/(k0*n0) + 1/(k1*n1))*(1-rhobase)    
  
  return(myvar)
  
}
