context("classif.h2ogbm")

test_that("autotest", {
  learner = LearnerClassifH2OGBM$new()
  expect_learner(learner)
  # seed must be implemented for feat_all
  # data set too small for weights
  result = run_autotest(learner, exclude = "(weights|weights_binary|weights_multiclass|feat_all)")
  expect_true(result, info = result$error)
})
