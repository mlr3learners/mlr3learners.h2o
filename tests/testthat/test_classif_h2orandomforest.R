context("classif.h2orandomforest")

test_that("autotest", {
  learner = LearnerClassifH2ORandomForest$new()
  expect_learner(learner)
  result = run_autotest(learner, exclude = "feat_all")
  expect_true(result, info = result$error)
})
