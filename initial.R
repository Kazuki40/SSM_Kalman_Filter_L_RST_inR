#INITIAL VALUE

#state_matrix_x0
initial_state_matrix_value_x0 <- function(sys_length) {
  #make 0 vector
  ans <- matrix(data = 0,
                nrow = sys_length,
                ncol = 1)
  
  return(ans)
  
}

#state_matrix_error_p0
initial_state_matrix_dispersion_sd_p0 <-
  function(sys_length, initial_sd) {
    #make error matrix
    p0 <-  matrix(data = initial_sd,
                  nrow = sys_length,
                  ncol = 1)
    return(p0)
    
  }

#system_noise_q
system_noise_sd <- function(parameter, sys_length) {
  error_matrix <- matrix(data = 0.0,
                         nrow = sys_length,
                         ncol = 1)
  
  #change
  error_matrix[1, 1] <- exp(parameter[1])
  error_matrix[2, 1] <- exp(parameter[2])
  error_matrix[3, 1] <- exp(parameter[3])
  
  return(error_matrix)
  
}

#observation_error_r
observation_error_sd <- function(parameter, obs_length) {
  error_matrix <- matrix(data = 0.0,
                         nrow = obs_length,
                         ncol = 1)
  
  #change
  error_matrix[1, 1] <- exp(parameter[4])
  
  
  return(error_matrix)
  
}
