#I'm having trouble using Robert's function for LHS, so I'm going to write my own. I think we are trying to code things in a very different style, and this is making it hard for me to finish my part of the assignment. I know this is not ideal, but I'm finding collaborating solely digitially to be really challenging. 

#The input to this function is the already wrangled data frame with the correct minimum temperatures and precipitation numbers for each year 
yield_anomaly=function(climate, min_temp_coeff=-0.015,min_temp_coeff2=-0.0046,precip_coeff=-0.07, precip_coeff2=0.0043,intercept=0.28) {
  
  fun_clim = climate
  
  yield_results = data.frame(year = fun_clim$year, yield = NA) 
  
  
  for(i in 1:nrow(yield_results)) {
    
    yield_results$yield[i] = 
      min_temp_coeff*fun_clim$min_temp[i] + 
      min_temp_coeff2*fun_clim$min_temp[i]^2 + 
      precip*fun_clim$precip[i] + 
      precip_coeff2*clim$precip[i]^2 + 
      intercept
    
  }
  
  mean_yield = mean(yield_results$yield)
  
  return(mean_yield)
}