# generic function for summary stats 
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
