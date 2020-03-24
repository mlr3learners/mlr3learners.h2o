context("regr.h2odeeplearning")

test_that("autotest", {
  learner = LearnerRegrH2ODeeplearning$new()
  expect_learner(learner)
  result = run_autotest(learner, exclude = "feat_all")
  expect_true(result, info = result$error)
})
