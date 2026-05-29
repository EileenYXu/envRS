# function for bootstrapping glmnet with multiply imputed datasets
# each multiply imputed dataset has the same ppts, so to make sure bootstrap 
# resampling uses the same set of ppts across all multiply imputed datasets,
# so make sure the first column of x is ids and run boot() on unique(x[,1])

# data has to be an argument or boot() gets upset
# xmat = predictors in data.matrix form
# ymat = outcome in data.matrix form
# indices from boot()
# alpha = minimum alpha from cv.glmnet
# wt = weights. if NULL, set each weight to 1
# pf = list of penalty factors from cv.glmnet
# intercept = whether to remove intercept or not (default is FALSE)

miboot_glmnet <- function(data, indices, xmat, ymat, alpha, pf, wt = NULL,
                          intercept = TRUE){
  
  rows = xmat[,1] %in% indices
  
  if(is.null(wt)){
    wt = rep(1, nrow(x))
  }
  
  w = wt[rows]
  
  xdat = xmat[rows,-1]
  yvar = ymat[rows,]
  
  fit = glmnet::glmnet(x = xdat, y = yvar, alpha = alpha, penalty.factor = pf,
                      weights = w, intercept = intercept)
  s = min(fit$lambda)
  
  output = coef(fit, s = s) |> as.matrix()
  
  return(output)
}

# function to extract model fit from test data
# df = dataset passed from dflist |> map(\(df) ...)
# outcome = string naming the outcome to be predicted
# coefs = named vector of estimates, no intercept

test_fit <- function(df, outcome, coefs) {
  pred = df |> select(all_of(names(coefs))) |> data.matrix() 
  score = coefs %*% t(pred)
  newdf = data.frame(score = t(score), out = df[,outcome])
  fit = lm(out ~ score, data = newdf)
  return(fit)
}
