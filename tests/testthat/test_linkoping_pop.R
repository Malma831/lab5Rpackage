context("linkoping_pop")

test_that("Variables names are correct", {
  data<-linkoping_pop()

  expect_equal(colnames(data), c("Year","gender","value"))
  expect_equal(length(data),3)


})

test_that("Class is correct", {
  data<- linkoping_pop()

  expect_s3_class(data, "data.frame")
})

test_that("Wrong input throws an error.", {
  expect_error(linkoping_pop("100"))
  expect_error(linkoping_pop(100))
  expect_error(linkoping_pop(TRUE))
})

test_that("Variables types are correct", {
  data<- linkoping_pop()

  bool_list <- lapply(data, FUN = function(x){return(is.numeric(x) | is.factor(x))})

  expect_equal(do.call("all", bool_list), TRUE)
})


test_that("Dimensions of the data.fram are correct", {
  data<-linkoping_pop()

  expect_equal(nrow(data)>0 ,TRUE)
  expect_equal(ncol(data)>0 ,TRUE)


})

test_that("Assumptions about the gender variable are correct", {
  data<-linkoping_pop()

  expect_equal(all(levels(data$gender) %in% c("M", "K", "T")) ,TRUE)
  expect_equal(length(levels(data$gender))== 3 ,TRUE)

})
