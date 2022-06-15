kalman_smooth_RTS <- function(x_a, x_f, P_a, P_f) {
  #definition of the answer
  target_data_number <- 1 + estimate_number
  
  
  #make matrix
  
  #smmoothing state xaT(now)
  smoothing_state_variable_x_aT <-
    matrix(data = 0,
           nrow = system_state_vector_length,
           ncol = 1)
  
  #substitute
  smoothing_state_variable_x_aT[, 1] <- x_a[target_data_number, ]
  
  #smoothing error PaT (now)
  smoothing_state_error_P_aT <- P_a[, , target_data_number]
  
  #smoothing state estimate x_a_T
  ans_smoothing_state_estimate_x_aT <- matrix(data = 0.0,
                                              ncol = system_state_vector_length,
                                              nrow =  target_data_number)
  
  #smoothing error variance Covariance matrix P_a_T
  ans_smoothing_error_variance_covaroance_P_a_T <- array(
    data = 0,
    dim = c(
      system_state_vector_length,
      system_state_vector_length,
      target_data_number
    )
  )
  
  
  #structure
  
  #design
  state_matrix_M <-
    system_model_design_matrix_M(sys_length = system_state_vector_length)
  
  
  #initial
  
  ans_smoothing_state_estimate_x_aT[target_data_number, ] <-
    x_a [target_data_number, ]
  ans_smoothing_error_variance_covaroance_P_a_T[, , target_data_number] <-
    P_a[, , target_data_number]
  
  #teigi
  x_a_now <- matrix(data = 0,
                    nrow = system_state_vector_length,
                    ncol = 1)
  x_f1_now <- matrix(data = 0,
                     nrow = system_state_vector_length,
                     ncol = 1)
  
  P_a_now <- matrix(data = 0,
                    nrow = system_state_vector_length,
                    ncol = system_state_vector_length)
  p_f1_now <- matrix(data = 0,
                     nrow = system_state_vector_length,
                     ncol = system_state_vector_length)
  
  #Start estimation
  
  #The beginning of the loop
  loop_brgin <- target_data_number - 1
  
  #start
  for (target_loop_t in loop_brgin:1) {
    #Substitute the value to use.
    #state_now_argument
    x_a_now[, 1] <- x_a[target_loop_t, ]#filter
    x_f1_now [, 1] <- x_f[target_loop_t + 1, ]#predit
    P_a_now <- P_a[, , target_loop_t]
    p_f1_now <- P_f[, , target_loop_t + 1]
    
    
    #s
    
    #smoother_gain
    smoother_gain_s <-
      RTS_smoother_smoother_gain_function(
        assimilation_error_Pat = P_a_now,
        system_design_matrix_Mt1 = state_matrix_M,
        prediction_error_Pft1 = p_f1_now
      )
    
    
    #update
    
    #state update
    #smoother_state_variavle_xaT(now)
    smoothing_state_variable_x_aT <-
      RTS_smoother_state_function(
        assimilation_x_a_t = x_a_now,
        smoother_gain =
          smoother_gain_s,
        smoother_x_t1T =
          smoothing_state_variable_x_aT,
        prediction_x_t1 = x_f1_now
      )
    
    #Update the covariance matrix
    #smoother_error_PaT(now)
    smoothing_state_error_P_aT <-
      RTS_smoother_covariance_matrix_function(
        assimilation_P_a_t = P_a_now,
        smoother_gain = smoother_gain_s,
        smoothing_P_t1T = smoothing_state_error_P_aT,
        prediction_P_f_t1 = p_f1_now
      )
    
    #ans
    ans_smoothing_state_estimate_x_aT[target_loop_t, ] <-
      smoothing_state_variable_x_aT[, 1]
    ans_smoothing_error_variance_covaroance_P_a_T[, , target_loop_t] <-
      smoothing_state_error_P_aT
    
  }
  
  #output
  ans <-
    list(x_a_T = ans_smoothing_state_estimate_x_aT,
         P_a_T = ans_smoothing_error_variance_covaroance_P_a_T)
  
  return(ans)
  
  
}