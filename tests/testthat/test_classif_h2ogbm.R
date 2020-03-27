context("classif.h2ogbm")

test_that("autotest", {
  learner = LearnerClassifH2OGBM$new()
  expect_learner(learner)
  # Skip weights tasks since data set is too small
  result = run_autotest(learner, exclude = "(weights|weights_binary|weights_multiclass)")
  expect_true(result, info = result$error)
})
