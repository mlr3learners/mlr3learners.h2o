context("regr.h2ogbm")

test_that("autotest", {
  learner = LearnerRegrH2OGBM$new()
  expect_learner(learner)
  result = run_autotest(learner)
  expect_true(result, info = result$error)
})
