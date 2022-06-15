#STRUCTURE

#system_model_structure
system_model_design_matrix_M <- function(sys_length) {
  #definition_of_square_matrix
  sys_design_matrix <-
    matrix(data = 0,
           nrow = sys_length,
           ncol = sys_length)
  
  #change
  sys_design_matrix[1, 1] <- 1
  sys_design_matrix[2, 1] <- 1
  sys_design_matrix[2, 2] <- 1
  sys_design_matrix[3, 3] <- -1
  sys_design_matrix[3, 4] <- -1
  sys_design_matrix[3, 5] <- -1
  sys_design_matrix[3, 6] <- -1
  sys_design_matrix[3, 7] <- -1
  sys_design_matrix[3, 8] <- -1
  sys_design_matrix[4, 3] <- 1
  sys_design_matrix[5, 4] <- 1
  sys_design_matrix[6, 5] <- 1
  sys_design_matrix[7, 6] <- 1
  sys_design_matrix[8, 7] <- 1
  
  return(sys_design_matrix)
  
}

#system_noise_transformation_matrix_gamma
#gamma %*% Q %*% t(gamma) = Q
#omit

#observation_matrix_H
observation_model_design_matrix_H_t <-
  function(sys_length) {
    obs_design_matrix <- matrix(data = 0,
                                nrow = 1,
                                ncol = sys_length)
    
    #change
    obs_design_matrix[1, 2] <- 1
    obs_design_matrix[1, 3] <- 1
    
    return(obs_design_matrix)
    
  }
