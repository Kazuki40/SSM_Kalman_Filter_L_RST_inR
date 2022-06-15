#OPTIM

#estimate function
estimate_parameter_function <-
  function(estimate_para_func, para_0) {
    #maximization algorithm
    estimate_result <-
      optim(
        par = para_0,
        fn = estimate_para_func,
        gr = NULL,
        method = "Nelder-Mead",
        lower = -Inf,
        upper = Inf,
        control = list(
          fnscale = -1,
          maxit = 10 ^ 9,
          trace = T
        ),
        hessian = TRUE
      )
    #
    
    #convergence test
    if (estimate_result$convergence == 0) {
      print("T")
      print(estimate_result)
    } else{
      print("F")
    }
    
    #t-test
    #solve :inverse matrix
    #diag :diagonal component
    #sqrt : root
    t_valuse <-
      estimate_result$par / (sqrt(-diag(solve(
        estimate_result$hessian
      ))))
    
    #sd
    ans_sd <-
      exp(estimate_result$par) * sqrt(-diag(solve(estimate_result$hessian)))
    ans_sd2 <- sqrt(-diag(solve(estimate_result$hessian)))
    
    #Initial log-likelihood
    ln_likelihood_0 <- estimate_para_func(para_0)
    
    #Final log-likelihood
    ln_likelihood_last <- estimate_result$value
    
    #likelihood ratio
    likelihood_ratio <-
      ((ln_likelihood_0 - ln_likelihood_last) / ln_likelihood_0)
    
    #modified likelihood ratio
    modified_likelihood_ratio <-
      ((ln_likelihood_0 - (ln_likelihood_last - length(para_0))) / ln_likelihood_0)
    
    #summary
    answer <-
      list(
        estimate_result,
        t_valuse,
        ans_sd,
        ans_sd2,
        ln_likelihood_0,
        ln_likelihood_last,
        likelihood_ratio,
        modified_likelihood_ratio
      )
    
    #name
    names(answer) <-
      c("estimate",
        "t",
        "SD_exp",
        "SD",
        "LL_0",
        "LL_L",
        "L_ratio",
        "M_L_ratio")
    
    return (answer)
    
  }
