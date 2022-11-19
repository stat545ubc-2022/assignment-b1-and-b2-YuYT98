predicted_list_1 = list(1,2,3)
actual_list_1 = list(4,5,6)
actual_list_2 = list(7,8,9)

test_that("Testing maeFormula function works well when inputs are correct", {
  expect_equal(maeFormula(predicted_list_1,actual_list_1),2)
})

test_that("Testing maeFormula function returns error when the number of inputs are incorrect", {
  expect_error(maeFormula(predicted_list_1,actual_list_1,actual_list_2))
})

test_that("Testing maeFormula function returns error when the type of inputs are incorrect", {
  expect_error(maeFormula(predicted_list_1,c(2,3)))
})

rm('predicted_list_1')
rm('actual_list_1')
rm('actual_list_2')
