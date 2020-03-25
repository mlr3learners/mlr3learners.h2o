context("classif.h2oglm")

test_that("autotest", {
  learner = LearnerClassifH2OGLM$new()
  expect_learner(learner)
  # seed must be implemented for feat_all
  # data set too small for weights_binary
  result = run_autotest(learner, exclude = "(weights|weights_binary|weights_multiclass|feat_all)")
  expect_true(result, info = result$error)
})
