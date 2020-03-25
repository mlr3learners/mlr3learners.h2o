context("classif.h2ogbm")

test_that("autotest", {
  learner = LearnerClassifH2OGBM$new()
  # Data set too small for default min_rows
  learner$param_set$values$min_rows = 1
  expect_learner(learner)
  result = run_autotest(learner, exclude = "feat_all")
  expect_true(result, info = result$error)
})
