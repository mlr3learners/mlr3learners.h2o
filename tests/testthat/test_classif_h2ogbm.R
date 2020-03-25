context("classif.h2ogbm")

test_that("autotest", {
  learner = LearnerClassifH2OGBM$new()
  expect_learner(learner)
  result = run_autotest(learner, exclude = "feat_all")
  expect_true(result, info = result$error)
})
