######################################
###### tipCheck server function ######
######################################

#get the number of columns of the meta data file. Here columns should be 1 or
#more after transformation of meta data. Tell user how many columns there are
#as the output here allows the user to use the add heatmap/matrix button. 
not_columns <- function(file) {
  col_n_file <- ncol(file)
  
  if (col_n_file < 1) {
    return("And looks like there is not a column for matrix plotting")
  } else {
    return(
      paste("And looks like the number of columns for matrix plotting is: ",
            col_n_file))
  }
}