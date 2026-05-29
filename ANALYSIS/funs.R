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


# generic function for getting summary stats 
get_sum_stats = function(df, vars) {
  # make 3 stat columns for each variable - n_total (sample size excl missing), stat1, stat2. merge together for table.
  summarydf = data.frame()
  
  for (v in vars) {
    variable = df[,v]
    n_total = length(which(is.na(variable)==F))
    
    # stat1 and 2 are mean and sd for numeric vars. if var is a factor, make rows of stat1 be the counts for each level and stat2 the percents of total
    if (is.numeric(variable)==T) {
      
      stat1 = mean(variable, na.rm = T)
      stat2 = sd(variable, na.rm = T)
      descr = paste0(round(stat1, digits = 2), " (", round(stat2, digits = 2), ")")
      out = data.frame("Var" = v, "n_total" = n_total, 
                       "desc" = descr)
      
    } else if (is.factor(variable)) {
      levs = levels(variable)
      tab_var = table(variable)
      out = data.frame()
      
      for (lev in levs) {
        stat1 = tab_var[lev] |> as.numeric()
        stat2 = (stat1/n_total)*100
        descr = paste0(stat1, " (", round(stat2, digits = 2), "%)")
        outrow = data.frame("Var" = paste0(v, "_", lev), "n_total" = n_total,
                            "desc" = descr)
        out = rbind(out, outrow)
      }
    }
    summarydf = rbind(summarydf, out)
  }
  return(summarydf)
}
