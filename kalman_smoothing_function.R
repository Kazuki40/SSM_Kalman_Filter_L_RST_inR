#RTS smoother state matrix update function
RTS_smoother_state_function <-
  function(assimilation_x_a_t,
           smoother_gain,
           smoother_x_t1T,
           prediction_x_t1) {
    #update
    #assimilation + S (smoother+1 - prediction+1)
    smoother_x_tT <-
      assimilation_x_a_t + smoother_gain %*% (smoother_x_t1T - prediction_x_t1)
    
    return(smoother_x_tT)
  }

#RTS smoother gain function
RTS_smoother_smoother_gain_function <-
  function(assimilation_error_Pat,
           system_design_matrix_Mt1,
           prediction_error_Pft1) {
    #Smoothing gain update formula
    smoother_gain_St <-
      assimilation_error_Pat %*% t(system_design_matrix_Mt1) %*% solve(prediction_error_Pft1)
    
    return(smoother_gain_St)
  }

#RTS smoother covariance matrix function
RTS_smoother_covariance_matrix_function <-
  function(assimilation_P_a_t,
           smoother_gain,
           smoothing_P_t1T,
           prediction_P_f_t1) {
    #update
    #assimilation + S (smoother+1 - prediction+1)S^T
    smoothing_P_a_tT <-
      assimilation_P_a_t + smoother_gain %*% (smoothing_P_t1T - prediction_P_f_t1) %*% t(smoother_gain)
    
    return(smoothing_P_a_tT)
  }