#FUNCTION

#time evolution about average value of system model(linear)
system_model_time_evolution_x_f <-
  function(sys_x_t_1, sys_H) {
    #system
    ans_x <- sys_H %*% sys_x_t_1
    
    return(ans_x)
    
  }

#observation model(innovation)
observation_model_innovation <-
  function(obs_y, sys_x_t_f, obs_design_H) {
    #observation
    del_x <- obs_y - obs_design_H %*% sys_x_t_f
    
    return(del_x)
    
  }

#error time evolution about system model noise
#LYAPUNOV EQUATION
system_model_noise_time_evolition_lyapunov <-
  function(sys_design_M, sys_P_a_t_1, sys_Q) {
    #lyapunov
    ans_P <-
      sys_design_M %*% sys_P_a_t_1 %*% t(sys_design_M) + sys_Q
    
    return(ans_P)
    
  }
#RICCATI EQUATION
system_model_noise_time_evolition_riccati <-
  function(sys_design_M,
           sys_P_f_t_1,
           sys_Q,
           obs_H_t_1,
           obs_R) {
    #riccati
    ans_P <-
      sys_design_M %*% (
        sys_P_f_t_1 - sys_P_f_t_1 %*% t(obs_H_t_1) %*% solve(obs_R + obs_H_t_1 %*%
                                                               sys_P_f_t_1 %*% t(obs_H_t_1)) %*% obs_H_t_1 %*% sys_P_f_t_1
      ) %*% t(sys_design_M) + sys_Q
    
    return(ans_P)
    
  }

#error kalman gaim
kalman_gaim_k <- function(sys_P_f_t, obs_H_t, obs_R) {
  kal_gain <-
    sys_P_f_t %*% t(obs_H_t) %*% solve(obs_R + obs_H_t %*% sys_P_f_t %*% t(obs_H_t))
  
  return(kal_gain)
  
}

#observation_error_update
observation_error_update_P_a <-
  function(sys_P_f, kal_K, obs_H, sys_length) {
    P_a <- (diag(sys_length) - kal_K %*% obs_H) %*% sys_P_f
    return(P_a)
  }


#var-cov from sd matrix
make_variance_covariance <- function(sd_matrix) {
  #sd to cov
  var_cov_matrix <- sd_matrix %*% t(sd_matrix)
  
  return(var_cov_matrix)
  
}

#var from sd matrix
diagonalzation_of_squares <- function(sd_matrix) {
  #sd to cov
  #(taikakuka(taikakuka(kyoubunsan gyouretu)))
  #Erase values other than diagonal components
  var_cov_matrix <- diag(diag(make_variance_covariance(sd_matrix)))
  
  return(var_cov_matrix)
  
}

#data assimilation function
data_assimilation <- function(x_f_t, k_t, obs_y, obs_H) {
  #update
  x_a_t <-
    x_f_t + k_t %*% observation_model_innovation(obs_y = obs_y,
                                                 sys_x_t_f = x_f_t,
                                                 obs_design_H = obs_H)
  
  return(x_a_f)
  
}

#likelihood
observation_likelihood <-
  function(innovation, obs_R) {
    #likelihood
    likelihood <-
      -(((innovation ^ 2) / obs_R) + log(2 * pi * obs_R)) / 2
    
    return(likelihood)
    
  }
