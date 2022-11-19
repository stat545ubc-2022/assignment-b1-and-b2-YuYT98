#' Get the mean absolute error
#'
#' The function maeFormula is to calculate the mean absolute error or MAE based on the predicted results and their actual values you give. It returns MAE if all inputs have the same length, no NA/NULL values and the type of both lists is numeric, otherwise the function will not work and return an error message.
#'
#'
#' @param predicted_list the predicted results stored in a list without NA/NULL values. The reason why this parameter called predicted_list is that this is the list for predicted results.
#' @param actual_list the actual values stored in a list without NA/NULL values. The reason why this parameter called actual_list is that this is the list for actual values.
#'
#' @return mae  the computed mean absolute error result if all inputs are correct.
#'
#' @export
#'
#' @examples
#' predicted_list_1 = list(1,2,3)
#' actual_list_1 = list(4,5,6)
#' maeFormula(predicted_list_1, actual_list_1)

maeFormula = function(predicted_list, actual_list){

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
