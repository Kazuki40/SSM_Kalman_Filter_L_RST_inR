#kalman filter

#optimization function
kf_function_parameter <- function(para) {
  #main function
  ans <- kf_function(para)
  
  #loglikelihood
  loglikelihood <- ans$loglikelihood
  
  return(loglikelihood)
  
}

#function
kf_function <- function(para) {
  #definition of the answer
  target_data_number <- 1 + estimate_number
  
  #make matrix
  #predict
  #Vertical = date
  #Horizontal = state
  ans_state_variable_x_f <-
    matrix(data = 0.0,
           ncol = system_state_vector_length,
           nrow =  target_data_number)
  
  #assimilation
  ans_state_variable_x_a <-
    matrix(data = 0.0,
           ncol = system_state_vector_length,
           nrow =  target_data_number)
  
  #predict
  ans_state_distributed_P_f <-
    array(
      data = 0,
      dim = c(
        system_state_vector_length,
        system_state_vector_length,
        target_data_number
      )
    )
  
  #assimilation
  ans_state_distributed_P_a <-
    array(
      data = 0,
      dim = c(
        system_state_vector_length,
        system_state_vector_length,
        target_data_number
      )
    )
  
  #likelihood
  loglikelihood <- 0
  
  #kalman gain (kalman ritoku)
  ans_kalman_gain <- matrix(data = 0.0,
                            ncol = system_state_vector_length,
                            nrow =  target_data_number)
  
  #innovation(kansokuchi sa)
  ans_innovation <- numeric(target_data_number)
  
  #Predictive variance
  ans_predictive_variance <- numeric(target_data_number)
  #initial variable
  
  #state
  #model
  state_variable_x_f_t <-
    initial_state_matrix_value_x0(sys_length = system_state_vector_length)
  state_variable_x_a_t <-
    initial_state_matrix_value_x0(sys_length = system_state_vector_length)
  
  #state error
  #model
  state_error_P_f_t <-
    diagonalzation_of_squares(
      sd_matrix = initial_state_matrix_dispersion_sd_p0(sys_length = system_state_vector_length, initial_sd = initial_state_standard_deviation)
    )
  state_error_P_a_t <-
    diagonalzation_of_squares(
      sd_matrix = initial_state_matrix_dispersion_sd_p0(sys_length = system_state_vector_length, initial_sd = initial_state_standard_deviation)
    )
  
  #system noise
  system_noise_Q <-
    diagonalzation_of_squares(sd_matrix = system_noise_sd(parameter = para, sys_length =
                                                            system_state_vector_length))
  
  #observation
  observation_error_R <-
    make_variance_covariance(sd_matrix = observation_error_sd(parameter = para, obs_length = observation_error_vector_length))
  
  #innovation
  innovation_x <- 0
  
  #kalman gain
  kalman_gain_now <-
    matrix(data = 0,
           nrow = system_state_vector_length,
           ncol = observation_error_vector_length)
  
  
  #structure
  
  #matrix
  #design
  state_matrix_M <-
    system_model_design_matrix_M(sys_length = system_state_vector_length)
  
  #design
  observate_matrix_H <-
    observation_model_design_matrix_H_t(sys_length = system_state_vector_length)
  
  #observation
  #data
  observate_y <- data_csv$y
  observate_y <- c(-9.999, observate_y)
  
  #initial
  ans_state_variable_x_f[1,] <- state_variable_x_f_t[, 1]
  ans_state_variable_x_a[1,] <- state_variable_x_a_t[, 1]
  ans_state_distributed_P_f[, , 1] <- state_error_P_f_t
  ans_state_distributed_P_a[, , 1] <- state_error_P_a_t
  ans_kalman_gain[1,] <- -9.999
  ans_innovation[1] <- -9.999
  loglikelihood <- 0
  ans_predictive_variance[1] <- -9.999
  
  #start
  for (target_loop_t in 2:target_data_number) {
    #Forecast distribution for the first period
    
    #state(E)
    #x_f_t update
    state_variable_x_f_t <-
      system_model_time_evolution_x_f(sys_x_t_1 = state_variable_x_a_t, sys_H =
                                        state_matrix_M)
    #error(V)
    #P_f_t update
    state_error_P_f_t <-
      system_model_noise_time_evolition_lyapunov(sys_design_M = state_matrix_M,
                                                 sys_P_a_t_1 = state_error_P_a_t,
                                                 sys_Q = system_noise_Q)
    #state_error_P_f_t <-
    #  system_model_noise_time_evolition_riccati(
    #    sys_design_M = state_matrix_M,
    #    sys_P_f_t_1 = state_error_P_f_t,
    #    sys_Q = system_noise_Q,
    #    obs_H_t_1 = observate_matrix_H,
    #    obs_R = observation_error_R
    #  )
    
    
    #One-term forecast likelihood
    #Innovation
    
    #state
    #observation model
    innovation_x <-
      observation_model_innovation(obs_y = observate_y[target_loop_t],
                                   sys_x_t_f = state_variable_x_f_t,
                                   obs_design_H = observate_matrix_H)
    
    #likelihood
    loglikelihood <- loglikelihood +
      observation_likelihood(innovation = innovation_x,
                             obs_R = observation_error_R)
    
    
    #K
    
    #kalman gain
    kalman_gain_now <-
      kalman_gaim_k(sys_P_f_t = state_error_P_f_t,
                    obs_H_t = observate_matrix_H,
                    obs_R = observation_error_R)
    
    #Data assimilation
    
    
    #state update
    state_variable_x_a_t <-
      state_data_assimilation(x_f_t = state_variable_x_f_t,
                              k_t = kalman_gain_now,
                              innovation_t = innovation_x)
    
    #Distribution update
    state_error_P_a_t <-
      state_distributed_data_assimilation(
        sys_length = system_state_vector_length,
        k_t = kalman_gain_now,
        obs_H = observate_matrix_H,
        P_f_t = state_error_P_f_t
      )
    
    
    #ans
    ans_state_variable_x_f[target_loop_t,] <-
      state_variable_x_f_t[, 1]
    ans_state_variable_x_a[target_loop_t,] <-
      state_variable_x_a_t[, 1]
    ans_state_distributed_P_f[, , target_loop_t] <-
      state_error_P_f_t
    ans_state_distributed_P_a[, , target_loop_t] <-
      state_error_P_a_t
    ans_kalman_gain[target_loop_t,] <- kalman_gain_now[, 1]
    ans_innovation[target_loop_t] <- innovation_x
    ans_predictive_variance[target_loop_t] <-
      (
        observation_error_R + observate_matrix_H %*% state_error_P_f_t %*% t(observate_matrix_H)
      )
    #loglikelihood
    
  }
  
  #output
  ans <-
    list(
      x_f = ans_state_variable_x_f,
      x_a = ans_state_variable_x_a,
      P_f = ans_state_distributed_P_f,
      P_a = ans_state_distributed_P_a,
      Kg = ans_kalman_gain,
      innovation = ans_innovation,
      loglikelihood = loglikelihood,
      predictive_variance = ans_predictive_variance
    )
  
  return(ans)
  
}