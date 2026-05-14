# function for bootstrapping stacked elastic net from miselect

# pred = list of multiply imputed datasets used in cv.saenet()
# out = outcome used in cv.saenet()
# pf, wt, adwt  = penalties, weights and adaptive weights used in cv.saenet()
# a = alpha.min
# l = lambda.min
# indices come from boot() 
boot_saenet <- function(data, pred, out, pf, a, l, wt, adwt, indices) {
  boot_x = list()
  boot_y = list()
  boot_wt = wt[indices]
  
  for (i in 1:length(x)) {
    boot_x[[i]] = pred[[i]][indices,] |> as.matrix()
    boot_y[[i]] = out[[i]][indices] |> as.vector()
  }
  
  fit = saenet(x = boot_x, y = boot_y, pf = pf, alpha = a, lambda = l,
               weights = boot_wt, adWeight =  adwt)
  
  output = coef(fit, alpha = a, lambda = l)
  
  return(output)
}

