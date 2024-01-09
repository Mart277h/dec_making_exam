hier_ORL_sim <- function(payoff,nsubs,ntrials,mu_a_rew,mu_a_pun,
                         mu_K,mu_theta,mu_omega_f,mu_omega_p,
                         sigma_a_rew,sigma_a_pun,sigma_K,sigma_theta,
                         sigma_omega_f,sigma_omega_p) {
  
  # arrays to populate for simulation
  x <- array(NA,c(nsubs,ntrials[1]))
  X <- array(NA,c(nsubs,ntrials[1]))
  Ev <- array(NA,c(nsubs,ntrials[1],2))
  PS <- array(NA,c(nsubs,ntrials[1],2))
  I <- array(NA, c(nsubs,ntrials[1],2))
  
  
  
  for (s in 1:nsubs) {
    
    # free parameters - sampled from a normal distribution with group mean and sd
    a_rew <- rtruncnorm(1,0,,mu_a_rew,sigma_a_rew)
    a_pun <- rtruncnorm(1,0,,mu_a_pun,sigma_a_pun)
    K <- rtruncnorm(1,0,,mu_K,sigma_K)
    theta <- 1 #rtruncnorm(1,0,,mu_theta,sigma_theta)
    omega_f <- rtruncnorm(1,0,,mu_omega_f,sigma_omega_f) 
    omega_p <- rtruncnorm(1,0,,mu_omega_p,sigma_omega_p)
    
    # arrays to populate for simulation 
    Ev_updateinfo_cho <- array(NA,c(ntrials[s],2))
    
    Ev_updateinfo <- array(NA,c(ntrials[s],2))
    
    PS_prevcho <- array(NA,c(ntrials[s],2)) 
    PS_prev_nocho <- array(NA,c(ntrials[s],2)) 
    
    V <- array(NA,c(ntrials[s],2))
    
    exp_p <- array(NA,c(ntrials[s],2))
    p <- array(NA,c(ntrials[s],2))

    
    x[s,1] <- rcat(1,c(.5,.5)) 
    
    X[s,1] <- payoff[1, x[s,1]] # Payoff based on choice x of trial 1  
    
    Ev[s,1,] <- rep(0.5,2) 
    
    PS[s,1,] <- rep(0.5,2) 
    
    p[1,] <- rep(0.5,2)
    
    for (t in 2:ntrials[s]) {
      
      for (d in 1:2) {
        
        # -------- Updating expected values ------------------------
     
        #Updating when information given
        Ev_updateinfo_cho[t,d] <- ifelse(X[s,t-1]>=0,
                                         Ev[s,t-1,d] + a_rew*((X[s,t-1]-1) - Ev[s,t-1,d]), #Adding -1 to our reward such that our X=+2 goes to +1
                                         Ev[s,t-1,d] + a_pun*((X[s,t-1]+1) - Ev[s,t-1,d])  #Adding +1 to our punishment such that our X=-2 goes to -1
        )
        
        Ev_updateinfo[t,d] <- ifelse(d==x[s,t-1], Ev_updateinfo_cho[t,d], 1-Ev_updateinfo_cho[t,d]) #If the deck is chosen update like this
        
        Ev[s,t,d] <- ifelse(abs(X[s,t-1])==2, Ev_updateinfo[t,d], Ev[s,t-1,d])
        #------------------------------------------------------------------
        
        PS_prevcho[t,d] <- ifelse(abs(X[s,t-1])==2,# if you chose the deck on last round
                                  ifelse(X[s,t-1]>=0, PS[s,t-1,d]/(1+K), PS[s,t-1,d]), # and it had a reward, if the deck you chose had information and reward you perseverance for that deck would decay, if it had loss, you would expect it to flip to one for the non chosen deck. If you did not get reward you would still expect the value to be +1 since you chose it, so decay of perseverance but for the other deck you would not know. 
                                  PS[s,t-1,d])
        
        PS_prev_nocho[t,d] <- ifelse(abs(X[s,t-1])==2,
                                     ifelse(X[s,t-1]>=0, 1/(1+K), PS[s,t-1,d]/(1+K)), 
                                     PS[s,t-1,d])
        
        PS[s,t,d] <- ifelse(x[s,t-1]==d, PS_prevcho[t,d], PS_prev_nocho[t,d])
      }
        
      for (d in 1:2) {
        
        #I[t,d] <- ifelse(abs(payoff[t,d])==2, 1, 0) #I dont know what to upweight the info with, because this doesnt really work
        
        I[s,t,d] <- ifelse(abs(payoff[t,d])==2, ((1 - abs(Ev[s,t,1] - Ev[s,t,2]))/2), ((-1 - abs(Ev[s,t,1] - Ev[s,t,2]))/2)) 
        #Ive tried to implement what you suggested, but im not really sure that its what you meant
        
        #Or with PS also?
        #I[t,d] <- ifelse(abs(payoff[t,d])==2, (1 + (abs((PS[t,1] + Ev[t,1]))) - abs((PS[t,2] + Ev[t,2]))), 0.1)
        
        V[t,d] <- Ev[s,t,d] + PS[s,t,d]*omega_p + I[s,t,d]*omega_f
        
        exp_p[t,d] <- exp(theta*V[t,d])
        
      }
      
      # *** AH: flipping the decay whenever the info-choice changes...
      #PS[s,t,d] <- ifelse(abs(payoff[t,d])!=abs(payoff[t-1,d]), PS[s,t,] <- PS[s,t,c(2,1)], ) # is info-choice different from previous trial, then flip decay
      # :AH ***
      
      for (d in 1:2) {
        p[t,d] <- exp_p[t,d]/sum(exp_p[t,])
      }
      
      x[s,t] <- rcat(1, p[t,]) # Using rcat instead solved our problem with Error in X[s, t] <- payoff[t, x[s, t]] : replacement has length zero
      
      X[s,t] <- payoff[t,x[s,t]]
      
    }
  }
  
  result <- list(x=x,
                 X=X,
                 Ev=Ev)
  
  return(result)
  
}

