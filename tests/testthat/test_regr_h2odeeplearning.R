context("regr.h2odeeplearning")

test_that("autotest", {
  learner = LearnerRegrH2ODeeplearning$new()
  learner$param_set$values$seed = 1
  expect_learner(learner)
  # Skip feat_all since seed seems not to work with deeplearning
  result = run_autotest(learner, exclude = "feat_all")
  expect_true(result, info = result$error)
})
