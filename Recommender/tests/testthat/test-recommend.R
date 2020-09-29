context("recommend")

test_that("non numeric results in error", {

  expect_error(recommend(read_books,'book',type='item'), "subscript out of bounds")
})
