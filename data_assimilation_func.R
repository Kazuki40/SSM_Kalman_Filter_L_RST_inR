#Data assimilation
#Modified model

#average
state_data_assimilation <- function(x_f_t, k_t, innovation_t) {
  #Renewal formula
  x_a_t <- x_f_t + k_t %*% innovation_t
  
  return(x_a_t)
}

#Distributed
state_distributed_data_assimilation <-
  function(sys_length, k_t, obs_H, P_f_t) {
    #unit matrix
    I <- diag(sys_length)
    
    #Renewal formula
    P_a_t <- (I - k_t %*% obs_H) %*% P_f_t
    
    return(P_a_t)
  }