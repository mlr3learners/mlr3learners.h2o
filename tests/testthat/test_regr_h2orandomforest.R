context("regr.h2orandomforest")

test_that("autotest", {
  learner = LearnerRegrH2ORandomForest$new()
  learner$param_set$values$seed = 1
  expect_learner(learner)
  result = run_autotest(learner)
  expect_true(result, info = result$error)
})
