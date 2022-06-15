library(optimx)
library(doRNG)
library(GA)
library(compiler)

other_estimate_parameter_function <-
  cmpfun(function(estimate_func, para_0) {
    return(
      optimx(
        par = para_0,
        fn = estimate_func,
        gr = NULL,
        hess = NULL,
        lower = -Inf,
        upper = Inf,
        itnmax = NULL,
        hessian = FALSE,
        control = list(all.methods = TRUE, maximize = TRUE)
      )
    )
  })


ga_estimate_parameter_function <-
  cmpfun(function(estimate_func, para_0) {
    ans <-
      ga(
        type = "real-valued",
        fitness = estimate_func,
        lower = rep(-501, length(para_0)),
        upper = rep(501, length(para_0)),
        optim = TRUE,
        optimArgs = list(method = "Nelder-Mead", control = list(fnscale = -1, trace = 1)),
        parallel = T,
        seed = 0
      )
    plot(ans)
    return(ans)
  })
