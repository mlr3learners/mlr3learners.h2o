context("regr.h2oglm")

test_that("autotest", {
  learner = LearnerRegrH2OGLM$new()
  expect_learner(learner)
  result = run_autotest(learner, exclude = "feat_all")
  expect_true(result, info = result$error)
})
