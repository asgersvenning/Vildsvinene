require(fitdistrplus)
require(stats)

beta_trans <- function(x, 
                       start = list(shape1 = .5,
                                    shape2 = .5),
                       method = "mme",
                       threshold = c(.001, 1 - .001)) {
  x <- pmax(pmin(x, threshold[2]), threshold[1])
  fitted_dist <- fit_dist(x, "beta", start, method)
  dist_params <- as.list(fitted_dist$estimate)
  
  print("Fitted parameters:")
  print(dist_params)
  
  transform <- quantile_normalize("beta", dist_params, threshold)
  inverse   <- quantile_invert("beta", dist_params, threshold)
  
  scales::trans_new(
    "fitted_beta",
    transform,
    inverse,
    breaks = function(x, n = n) {
        x %>% 
          transform() %>%
          scales::breaks_extended(n)() %>% 
          inverse()
      }
  )
}

dist_trans <- function(x, 
                       dist,
                       start,
                       method = "mme",
                       threshold = c(0, Inf)) {
  type <- pmatch(dist, c("beta", "poisson", "exponential", "gamma", "log-normal", "negative binomial", "uniform", "normal", "gaussian"))
  if (is.na(type)) stop("Only 'beta', 'poisson', 'exponential', 'gamma', 'log-normal', 'negative binomial', 'uniform' and 'normal'/'gaussian' distributions are implemented.")
  
  type <- switch(type,
                 "1" = "beta",
                 "2" = "pois",
                 "3" = "exp",
                 "4" = "gamma",
                 "5" = "lnorm",
                 "6" = "nbinom",
                 "7" = "unif",
                 "8" = "norm",
                 "9" = "norm")
  
  x <- pmax(pmin(x, threshold[2]), threshold[1])
  fitted_dist <- fit_dist(x, type, start, method)
  dist_params <- as.list(fitted_dist$estimate)
  
  print("Fitted parameters:")
  print(dist_params)
  
  transform <- quantile_normalize(dist, dist_params, threshold)
  inverse   <- quantile_invert(dist, dist_params, threshold)
  
  scales::trans_new(
    paste0("fitted_", dist),
    transform,
    inverse,
    breaks = function(x, n = n) {
      x %>% 
        transform() %>%
        scales::breaks_extended(n)() %>% 
        inverse()
    }
  )
}

fit_dist <- function(x, dist, start, method = "mme") {
  print(dist)
  fitdistrplus::fitdist(data = x, 
                        dist = dist,
                        start = start,
                        method = method)
}

quantile_normalize <- function(dist, parameters, threshold) {
  dist <- pmatch(dist, c("beta", "poisson", "exponential", "gamma", "log-normal", "negative binomial", "uniform", "normal", "gaussian"))
  if (is.na(dist)) stop("Only 'beta', 'poisson', 'exponential', 'gamma', 'log-normal', 'negative binomial', 'uniform' and 'normal'/'gaussian' distributions are implemented.")
  
  pfun <- switch(dist,
                 "1" = "pbeta",
                 "2" = "ppois",
                 "3" = "pexp",
                 "4" = "pgamma",
                 "5" = "plnorm",
                 "6" = "pnbinom",
                 "7" = "punif",
                 "8" = "pnorm",
                 "9" = "pnorm")
  pfun <- match.fun(pfun)
  
  function(x) {
    x <- pmax(pmin(x, threshold[2]), threshold[1])
    q <- do.call(pfun, c(list(q = x), parameters))
    
    qnorm(q)
  }
}

quantile_invert <- function(dist, parameters, threshold) {
  dist <- pmatch(dist, c("beta", "poisson", "exponential", "gamma", "log-normal", "negative binomial", "uniform", "normal", "gaussian"))
  if (is.na(dist)) stop("Only 'beta', 'poisson', 'exponential', 'gamma', 'log-normal', 'negative binomial', 'uniform' and 'normal'/'gaussian' distributions are implemented.")
  
  qfun <- switch(dist,
                 "1" = "qbeta",
                 "2" = "qpois",
                 "3" = "qexp",
                 "4" = "qgamma",
                 "5" = "qlnorm",
                 "6" = "qnbinom",
                 "7" = "qunif",
                 "8" = "qnorm",
                 "9" = "qnorm")
  qfun <- match.fun(qfun)
  
  function(q) {
    p <- pnorm(q)
    
    x <- do.call(qfun, c(list(p = p), parameters))
    
    x[x == threshold[1]] <- 0
    x[x == threshold[2]] <- 1
    
    x
  }
}