mae_formula = function(predicted_list, actual_list){

  if(length(predicted_list) == length(actual_list) &&
     !anyNA(predicted_list, actual_list) &&
     length(predicted_list) != 0 &&
     length(actual_list) != 0 &&
     is.list(predicted_list) &&
     is.list(actual_list)
  ) {
    length_of_list = length(predicted_list)
    sum = 0
    for(i in 1:length_of_list){
      ae = abs(actual_list[[i]] - predicted_list[[i]])
      sum = ae + ae
    }
    mae = sum/length_of_list
    return(mae)
  } else {
    stop("please ensure your inputs are correct")
  }
}
