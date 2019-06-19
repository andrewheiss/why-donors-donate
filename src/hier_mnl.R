hier_mnl = function (Data, Prior, Mcmc, Cont) {
  # This function implements an MCMC estimation algorithm for a hierarchical MNL with a multivariate 
  # normal distribution of heterogeneity.
  
  # Describe and Assign Function Arguments ----------------------------------
  # Data = list(y,X,Z,Beta,Gamma).
  y = Data$y                                              # List of choices.
  X = Data$X                                              # List of design matrices.
  Z = Data$Z                                              # Covariates for the upper-level model.
  if (Mcmc$sim_ind==1) {
    Beta = Data$Beta                                      # True values of Beta for parameter recovery.
    # Gamma = Data$Gamma                                    # True values of Gamma for parameter recovery.
    # Vbeta = Data$Vbeta                                    # True values of Vbeta for parameter recovery.
  }
  
  # Prior = list(gammabar,Agamma,nu,V).
  gammabar = Prior$gammabar                               # Means for normal prior on Gamma.
  Agamma = Prior$Agamma                                   # Precision matrix for normal prior on Gamma.
  nu = Prior$nu                                           # DF for IW prior on Vbeta.
  V = Prior$V                                             # Location for IW prior on Vbeta.
  
  # Mcmc = list(R,keep,step,sim_ind,cont_ind).
  R = Mcmc$R                                              # Number of iterations in the Markov chain.
  keep = Mcmc$keep                                        # Thinning parameter.
  step = Mcmc$step                                        # RW step (scaling factor) for the beta draws.
  sim_ind = Mcmc$sim_ind                                  # Indicates a simulation experiment.
  cont_ind = Mcmc$cont_ind                                # Indicates a run continuation.
  
  # Choice variables.
  nresp = length(y)                                       # Number of respondents.
  nvars = ncol(X[[1]])                                    # Number of attribute levels.
  nscns = length(y[[1]])                                  # Number of choice tasks.
  nalts = length(X[[1]][,1])/nscns                        # Number of alternatives in each choice task.
  ncovs = ncol(Z)                                         # Number of covariates.
  
  # Describe and Initialize Function Output ---------------------------------
  # Respondent-level parameter draws.
  betadraw = array(double(floor(R/keep)*nresp*nvars),dim=c(nresp,nvars,floor(R/keep)))
  
  # Aggregate-level parameter draws.
  Gammadraw = matrix(double(floor(R/keep)*nvars*ncovs),ncol=nvars*ncovs)
  Vbetadraw = matrix(double(floor(R/keep)*nvars*nvars),ncol=nvars*nvars)
  
  # Diagnostic draws and initial clock time.
  llikedraw = double(floor(R/keep))     # Log likelihood.
  acceptdraw = array(0,dim=c(R/keep))   # Beta acceptance rate.
  stepdraw = array(0,dim=c(R/keep))     # RW step adjusted during burn-in.
  itime = proc.time()[3]                # Initial clock time.
  
  # Initialize MCMC ---------------------------------------------------------
  cat("MCMC Iteration (estimated time to end in hours | step | beta accept | llike )",fill=TRUE)
  
  # Initialize values.
  if (cont_ind == 0) {
    step = Mcmc$step
    if (sim_ind==0) oldbetas = matrix(double(nresp*nvars),ncol=nvars)
    if (sim_ind==1) oldbetas = Beta
    oldGamma = matrix(double(nvars*ncovs),ncol=nvars)
    oldVbeta = diag(nvars)
  }
  
  # Initialize values and use the previous draws for continued runs.
  if (cont_ind == 1) {
    step = Cont$out_step
    oldbetas = Cont$out_oldbetas
    oldGamma = matrix(Cont$out_oldGamma,ncol=nvars)
    oldVbeta = Cont$out_oldVbeta
  }
  
  # Log-likelihood function for the MNL.
  ll_mnl <- function (beta, y, X) {
    nvars = ncol(X)        # Number of attribute levels.
    nscns = length(y)      # Number of choice tasks.
    nalts = nrow(X)/nscns  # Number of alternatives.
    
    # Compute Xbeta across all choice tasks.
    Xbeta = matrix(exp(X%*%beta),byrow=TRUE,ncol=nalts)
    
    # Numerator: Xbeta values associated with each choice.
    choices = cbind(c(1:nscns),y)
    numerator = Xbeta[choices]
    
    # Denominator: Xbeta values associated with each task.
    iota = c(rep(1,nalts))
    denominator = Xbeta%*%iota
    
    # Return the logit summed across choice tasks.
    return(sum(log(numerator) - log(denominator)))
  }
  
  # Run the MCMC ------------------------------------------------------------
  # The Markov chain will run for R iterations.
  for (r in 1:R) {
    loglike = 0   # Initialize log likelihood values for each iteration r.
    naccept = 0   # Initialize number of times the new beta draws are accepted.
    
    # Respondent-level loop.
    for (resp in 1:nresp) {
      # Draw beta (random walk).
      beta_old = oldbetas[resp,]
      if (r < (R/3)) beta_can = as.vector(rmvnorm(1,mean=beta_old,sigma=(step*oldVbeta)))
      if (r >= (R/3)) beta_can = as.vector(rmvnorm(1,mean=beta_old,sigma=(step*Vbeta_fixed)))
      
      # Log likelihood with old and candidate beta draws.
      log_old = ll_mnl(beta_old,y[[resp]],X[[resp]])
      log_can = ll_mnl(beta_can,y[[resp]],X[[resp]])
      
      # Log of the MVN distribution of heterogeneity.
      log_heter_old = dmvnorm(beta_old,mean=Z[resp,]%*%oldGamma,sigma=oldVbeta,log=TRUE)
      log_heter_can = dmvnorm(beta_can,mean=Z[resp,]%*%oldGamma,sigma=oldVbeta,log=TRUE)
      
      # Compare the old and candidate posteriors and compute alpha (second-stage prior and proposal densities cancel out.)
      diff = exp((log_can + log_heter_can) - (log_old + log_heter_old))
      if (diff == "NaN" || diff == Inf) {
        alpha = -1 # If the number doesn't exist, always reject.
      } else {
        alpha = min(1,diff)
      }
      unif = runif(1)
      if (unif < alpha) {
        oldbetas[resp,] = beta_can
        naccept = naccept + 1
        loglike = loglike + log_can
      } else {
        loglike = loglike + log_old
      }
    }
    
    # Draw Gamma and Vbeta (Gibbs step).
    out = rmultireg(oldbetas,Z,gammabar,Agamma,nu,V)
    oldGamma = out$B
    oldVbeta = out$Sigma
    
    # Fix Vbeta for post-burn-in beta proposal density.
    if (r == floor(R/3)) Vbeta_fixed = oldVbeta
    
    # Houskeeping and Output --------------------------------------------------
    # Modify the RW step sizes to constrain acceptance rates in batches of 100 during burn-in (R/3).
    if (r%%100 == 0 & cont_ind == 0) {
      if (r < (R/3)) {
        # Update step.
        if (naccept/nresp < .20) {
          step = step*0.95
        }
        if (naccept/nresp > .60) {
          step = step*1.05
        }
      }
    }
    
    # Print progress.
    if (r%%5 == 0) {
      ctime = proc.time()[3]
      timetoend = ((ctime - itime)/r)*(R - r)
      bacceptr=naccept/nresp
      cat(" ",r," (",round((timetoend/60)/60,2),"|",round(step,5),"|",round(bacceptr,2),"|",round(loglike,2),")",fill = TRUE)
    }
    
    # Print chart less often.
    if (r%%100 == 0) {
      par(mfrow=c(2,1))
      plot(llikedraw,type="l"); matplot(Gammadraw,type="l")
      # if (sim_ind==0) { plot(llikedraw,type="l"); matplot(Gammadraw,type="l") }
      # if (sim_ind==1) {
      #   plot(llikedraw,type="l")
      #   matplot(Gammadraw,type="l",col=c(1:nvars)); abline(h=Gamma,col=c(1:nvars))
      # }
    }
    
    # Save the posterior draws.
    mkeep = r/keep
    if (mkeep*keep == (floor(mkeep)*keep)) {
      betadraw[,,mkeep] = oldbetas
      Gammadraw[mkeep,] = as.vector(oldGamma)
      Vbetadraw[mkeep,] = as.vector(oldVbeta)
      llikedraw[mkeep] = loglike
      acceptdraw[mkeep] = naccept/nresp
      stepdraw[mkeep] = step
    }
    
    # Save out continuation files.
    if (r%%R == 0) {
      Cont = list(out_oldbetas = betadraw[,,R/keep],out_oldGamma = matrix(Gammadraw[R/keep,],byrow=TRUE,ncol=(nvars*ncovs)),
           out_oldVbeta = matrix(Vbetadraw[R/keep,],byrow=TRUE,ncol=nvars),out_step = step)
    }
  }
  
  # Print total run time.
  ctime = proc.time()[3]
  cat(" Total Time Elapsed (in Hours): ",round(((ctime - itime)/60)/60,2),fill = TRUE)
  
  # Output.
  return(list(betadraw=betadraw,Gammadraw=Gammadraw,Vbetadraw=Vbetadraw,
    llikedraw=llikedraw,acceptdraw=acceptdraw,stepdraw=stepdraw,Cont=Cont))
}