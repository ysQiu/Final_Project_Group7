compre_cal <- function(pred, resp){
  my_array <- array(NA, dim = c(3, 3, 9))
  for(i in 1:9){
    v <- (0.1)*i
    my_array[,,i] <- conf_mat_free(pred, resp, value = v)
  }
  return(my_array)
}
