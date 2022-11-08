


streak_stats <- function(x, n, ID){
  
  
  # Maximum run of correct
  grp <- with(rle(x), rep(seq_along(values), lengths))
  max_length_correct_streak <- max(ave(x, grp, FUN = seq_along)*x)
  
  # Maximum run of incorrect
  y <- ifelse(x == 1,0,1)
  grp <- with(rle(y), rep(seq_along(values), lengths))
  max_length_incorrect_streak <- max(ave(y, grp, FUN = seq_along)*y)
  
  # average length of incorrect/correct responses
  z1 <- unlist(rle(x)[1])
  z2 <- unlist(rle(x)[2])
  z <- as.data.frame(cbind(z1,z2))
  ave_length_incorrect_streak <- mean(z[z2== 0, "z1"])
  ave_length_correct_streak <- mean(z[z2== 1, "z1"])
  
  

  
  # Total number of runs
  total_no_runs <- nrow(z)
  
  
  # Moving average for clusters of n = 30 ----
  ma <- function(x, n = n){stats::filter(x, rep(1 / n, n), sides = 2)}
  
  # Range of moving averages
  range_moving_averages <- range(ma(x = x, n = n), na.rm = T)[2] - range(ma(x = x, n = n), na.rm = T)[1]
  
  #the mean variation of the moving averages about the  average performance
  mean_var_moving_averages <- mean(abs(ma(x,n) - mean(x)), na.rm= T)
  
  average_correct <- mean(x, na.rm = T)
  
  x <- cbind(ID,average_correct, max_length_correct_streak, max_length_incorrect_streak, ave_length_correct_streak, ave_length_incorrect_streak, total_no_runs, range_moving_averages, mean_var_moving_averages)
  
  return(as.data.frame(x))
}





calc_streaks <- function(data, identifier, n, accuracy_var = "correct"){
  
  for(i in unique(data[,identifier])){
    
    
    x <- data[data[,identifier] == i, accuracy_var]
    
    look <- streak_stats(x, n= n, ID = i)
    
    
    if(unique(data[,identifier])[1] != i){streak_data <- rbind(streak_data, look)}
    if(unique(data[,identifier])[1] == i){streak_data <- look}
    
    
  }
  return(streak_data)
  
}
