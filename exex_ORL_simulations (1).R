ORL_sim <- function(payoff,ntrials,a_rew,a_pun,K,theta,omega_p, omega_f) {
  
  # arrays to populate for simulation
  x <- array(NA,c(ntrials))
  X <- array(NA,c(ntrials))
  Ev <- array(NA,c(ntrials,2))
  PS <- array(NA,c(ntrials,2))
  I <- array(NA, c(ntrials,2))
    
    # arrays to populate for simulation 
  Ev_updateinfo_cho <- array(NA,c(ntrials,2))
  PS_prevcho <- array(NA,c(ntrials,2)) 
  PS_prev_nocho <- array(NA,c(ntrials,2))
    
  Ev_updateinfo <- array(NA,c(ntrials,2))
  
  V <- array(NA,c(ntrials,2))
    
  exp_p <- array(NA,c(ntrials,2))
  p <- array(NA,c(ntrials,2))
    
  x[1] <- rcat(1, 2) # Using rcat gives us deck 1 or 2 
    
  X[1] <- payoff[1, x[1]] # Payoff based on choice x of trial 1  
    
  Ev[1,] <- rep(0.5,2)
    
  PS[1,] <- rep(0.5,2) # Simulations right now shows that it might be too high for 1
  
  p[1,] <- rep(0.5,2)
    
  for (t in 2:ntrials) {
    
    for (d in 1:2) {
        
        # -------- Updating expected values ------------------------
        #Updating when information given
      Ev_updateinfo_cho[t,d] <- ifelse(X[t-1]>=0,
                                       Ev[t-1,d] + a_rew*((X[t-1]-1) - Ev[t-1,d]), #Adding -1 to our reward such that our X=+2 goes to +1
                                       Ev[t-1,d] + a_pun*((X[t-1]+1) - Ev[t-1,d])  #Adding +1 to our punishment such that our X=-2 goes to -1
        )
      
      Ev_updateinfo[t,d] <- ifelse(d==x[t-1], Ev_updateinfo_cho[t,d], 1-Ev_updateinfo_cho[t,d]) #If the deck is chosen update like this
        
      Ev[t,d] <- ifelse(abs(X[t-1])==2, Ev_updateinfo[t,d], Ev[t-1,d]) #If info is given (abs 2), update with info, else put EV from previous trial
        
        #------------------------------------------------------------------
        
      #------Andreas PS
      #PS[t,d] <- ifelse(x[t-1]==d, # did you choose this deck or not on last trial
       #                     ifelse(abs(X[t-1])==2, 1/(1+K), PS[t-1,d]/(1+K)), # if yes, did it have info or not
        #                    ifelse(abs(X[t-1])==2, PS[t-1,d]/(1+K), 1/(1+K))) # if no, did it have info or not (then, do the opposite to if you'd picked it)
      
      #-------Martines trial 1  
      #I was thinking something along this line, but i cannot really wrap my head around whether it does what i expect.
      #PS[t,d] <- ifelse(x[t-1]==d & abs(X[t-1])==2, # if you chose the deck and info
       #                 ifelse(X[t-1]>=0, PS[t-1,d]/(1+K), 1/(1+K)), # if the deck you chose had information and reward your perseverance for that deck would decay, if it had loss, you would expect it to flip to one for the non chosen deck. If you did not get reward you would still expect the value to be +1 since you chose it, so decay of perseverance but for the other deck you would not know. 
        #                ifelse(abs(X[t-1])==2, 1/(1+K), PS[t-1,d]/(1+K))) 
      
      #--------Martines trial 2 (Not finished, but cant tell if im headed the right direction)
      PS_prevcho[t,d] <- ifelse(abs(X[t-1])==2,# if you chose the deck on last round
                                ifelse(X[t-1]>=0, PS[t-1,d]/(1+K), PS[t-1,d]), # and it had a reward, if the deck you chose had information and reward you perseverance for that deck would decay, if it had loss, you would expect it to flip to one for the non chosen deck. If you did not get reward you would still expect the value to be +1 since you chose it, so decay of perseverance but for the other deck you would not know. 
                                      PS[t-1,d])
      
      PS_prev_nocho[t,d] <- ifelse(abs(X[t-1])==2,
                                   ifelse(X[t-1]>=0, 1/(1+K), PS[t-1,d]/(1+K)), 
                                   PS[t-1,d])
      
      PS[t,d] <- ifelse(x[t-1]==d, PS_prevcho[t,d], PS_prev_nocho[t,d])
      }
    
    for (d in 1:2) {

      #I[t,d] <- ifelse(abs(payoff[t,d])==2, 1, 0) #I dont know what to upweight the info with, because this doesnt really work
      
      I[t,d] <- ifelse(abs(payoff[t,d])==2, ((1 - abs(Ev[t,1] - Ev[t,2]))/2), ((-1 - abs(Ev[t,1] - Ev[t,2]))/2)) 
        #Ive tried to implement what you suggested, but im not really sure that its what you meant
      
        #Or with PS also?
      #I[t,d] <- ifelse(abs(payoff[t,d])==2, (1 + (abs((PS[t,1] + Ev[t,1]))) - abs((PS[t,2] + Ev[t,2]))), 0.1)
       
      V[t,d] <- Ev[t,d] + PS[t,d]*omega_p + I[t,d]*omega_f #I kinda think this is cool, because the weight then could be an indicator of how much you weight information? 
        
      exp_p[t,d] <- exp(theta*V[t,d])
        
    }
      
    for (d in 1:2) {
        p[t,d] <- exp_p[t,d]/sum(exp_p[t,])
      }
      
    x[t] <- rcat(1, p[t,]) 
      
    X[t] <- payoff[t,x[t]]
      
    }
  
  result <- list(x=x,
                 X=X,
                 Ev=Ev, 
                 PS=PS, 
                 p=p,
                 I=I
                 )
  
  return(result)

}

