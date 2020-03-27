context("classif.h2oglm")

test_that("autotest", {
  learner = LearnerClassifH2OGLM$new()
  learner$param_set$values$seed = 1
  expect_learner(learner)
  result = run_autotest(learner, exclude = "missings_binary")
  expect_true(result, info = result$error)
})
