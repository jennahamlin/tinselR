#function to create the tip list. list apply over the counter('n') and
#paste the values in the tip vector to the variable tips. The input in this 
#function is two reactive values (r_n_values and r_tip_vec); r_n_values is a 
#counter; while r_tip_vec is the actual tips the user has selected 
create_tip_list <- function(r_n_values, r_tip_vec) {
  tips <- c()
  if (r_n_values < 1) {
    #skip
  } else {
    tips <- lapply(1:r_n_values, function(i)
      r_tip_vec[[paste0("tips", i)]])
  }
  return(tips)
}