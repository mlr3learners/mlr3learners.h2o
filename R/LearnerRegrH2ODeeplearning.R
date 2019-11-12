#' @title Regression H2O DeepLearning Learner
#'
#' @aliases mlr_learners_regr.h2odeeplearning
#' @format [R6::R6Class] inheriting from [mlr3::LearnerRegr].
#'
#' @description
#' A [mlr3::LearnerRegr] for a deeplearning regression implemented in [h2o::h2o.deeplearning()] in package \CRANpkg{h2o}.
#'
#' @export
LearnerRegrH2ODeeplearning = R6Class("LearnerRegrH2ODeeplearning", inherit = LearnerRegr,
  public = list(
    initialize = function() {
      ps = ParamSet$new(
        params = list(
          ParamLgl$new("ignore_const_cols", default = TRUE, tags = "train"),
          ParamLgl$new("score_each_iteration", default = FALSE, tags = "train"),
          # weights_column
          # offset_column
          ParamLgl$new("balance_classes", default = FALSE, tags = "train"),
          ParamDbl$new("class_sampling_factors", default = NULL, special_vals = list(NULL), tags = "train"),
          ParamDbl$new("max_after_balance_size", default = 5, tags = "train"),
          ParamInt$new("max_hit_ratio_k", default = 0, lower = 0, tags = "train"),
          # checkpoint
          # pretrained_autoencoder
          # overwrite_with_best_model
          ParamLgl$new("use_all_factor_level", default = TRUE, tags = "train"),
          ParamLgl$new("standardize", default = TRUE, tags = "train"),
          ParamFct$new("activation", levels = c("Rectifier", "Tanh", "TanhWithDropout", "RectifierWithDropout", "Maxout", "MaxoutWithDropout"), default = "Rectifier", tags = "train"),
          ParamUty$new("hidden", default = c(200L, 200L), tags = "train"),
          ParamDbl$new("epochs", default = 10L, lower = 1, tags = "train"),
          ParamInt$new("train_samples_per_iteration", default = -2, lower = -2, tags = "train"),
          ParamDbl$new("target_ratio_comm_to_comp", default = 0.05, tags = "train"),
          ParamInt$new("seed", default = -1, tags = "train"),
          ParamLgl$new("adaptive_rate", default = TRUE, tags = "train"),
          ParamDbl$new("rho", default = 0.99, lower = 0, tags = "train"),
          ParamDbl$new("epsilon", default = 1e-08, lower = 1e-10, upper = 1e-4, tags = "train"),
          ParamDbl$new("rate", default = 0.005, lower = 0, upper = 1, tags = "train"),
          ParamDbl$new("rate_annealing", default = 1e-06, lower = 0, tags = "train"),
          ParamDbl$new("rate_decay", default = 1, lower = 0, tags = "train"),
          ParamDbl$new("momentum_start", default = 0, tags = "train"),
          ParamDbl$new("momentum_ramp", default = 1e+06, tags = "train"),
          ParamDbl$new("momentum_stable", default = 0, tags = "train"),
          ParamLgl$new("nesterov_accelerated_gradient", default = TRUE, tags = "train"),
          ParamDbl$new("input_dropout_ratio", default = 0, tags = "train"),
          ParamDbl$new("hidden_dropout_ratios", default = 0.5, tags = "train"),
          ParamDbl$new("l1", default = 0, tags = "train"),
          ParamDbl$new("l2", default = 0, tags = "train"),
          ParamDbl$new("max_w2", default = Inf, tags = "train"),
          ParamFct$new("initial_weight_distribution", levels = c("UniformAdaptive", "Uniform", "Normal"), default = "UniformAdaptive", tags = "train"),
          ParamDbl$new("initial_weight_scale", default = 1, tags = "train"),
          # initial_weights
          # initial_bias
          ParamFct$new("loss", levels = c("Automatic", "CrossEntropy", "Quadratic", "Absolute", "Huber"), default = "Automatic", tags = "train"),
          ParamFct$new("distribution", levels = c("AUTO", "gaussian", "poisson", "gamma", "tweedie", "laplace",  "huber", "quantile"), default = "AUTO", tags = "train"),
          ParamDbl$new("quantile_alpha", default = 0.5, lower = 0, upper = 1, tags = "train"),
          ParamDbl$new("tweedie_power", default = 1.5, lower = 1, upper = 2, tags = "train"),
          ParamDbl$new("huber_alpha", default = 0.9, lower = 0, upper = 1, tags = "train"),
          ParamDbl$new("score_interval", default = 5, tags = "train"),
          ParamInt$new("score_training_samples", default = 10000, tags = "train"),
          ParamInt$new("score_validation_samples", default = 0, tags = "train"),
          ParamDbl$new("score_duty_cycle", default = 0.1, tags = "train"),
          # classification_stop
          ParamDbl$new("regression_stop", default = 1e+06, lower = -1, tags = "train"),
          ParamInt$new("stopping_rounds", default = 5L, lower = 0L, tags = "train"),
          ParamFct$new("stopping_metric", levels = c("AUTO", "logloss", "AUC", "lift_top_group", "misclassification",  "AUCPR", "mean_per_class_error", "custom", "custom_increasing"), tags = "train"),
          ParamDbl$new("stopping_tolerance", default = 0, lower = 0, tags = "train"),
          ParamDbl$new("max_runtime_secs", default = 0, lower = 0, tags = "train"),
          ParamFct$new("score_validation_sampling", levels = c("Uniform", "Stratified"), default = "Uniform", tags = "train"),
          ParamLgl$new("diagnostics", default = TRUE, tags = "train"),
          ParamLgl$new("fast_mode", default = TRUE, tags = "train"),
          ParamLgl$new("force_load_balance", default = TRUE, tags = "train"),
          ParamLgl$new("replicate_training_data", default = TRUE, tags = "train"),
          ParamLgl$new("single_node_mode", default = FALSE, tags = "train"),
          ParamLgl$new("shuffle_training_data", default = FALSE, tags = "train"),
          ParamFct$new("missing_values_handling", levels = c("MeanImputation", "Skip"), default = "MeanImputation", tags = "train"),
          ParamLgl$new("autoencoder", default = FALSE, tags = "train"),
          ParamLgl$new("sparse", default = FALSE, tags = "train"),
          ParamDbl$new("average_activation", default = 0, tags = "train"),
          ParamDbl$new("sparsity_beta", default = 0, tags = "train"),
          # max_categorical_features
          ParamLgl$new("reproducible", default = FALSE, tags = "train"),
          ParamLgl$new("export_weights_and_biases", default = FALSE, tags = "train"),
          ParamInt$new("mini_batch_size", default = 1, tags = "train"),
          ParamFct$new("categorical_encoding", levels = c("AUTO", "Enum", "OneHotInternal", "OneHotExplicit", "Binary", "Eigen", "LabelEncoder", "SortByResponse", "EnumLimited"), default = "AUTO", tags = "train"),
          ParamLgl$new("elastic_averaging", default = FALSE, tags = "train"),
          ParamDbl$new("elastic_averaging_moving_rate", default = 0.9, tags = "train"),
          ParamDbl$new("elastic_averaging_regularization", default = 0.001, tags = "train"),
          # export_checkpoints_dir
          ParamLgl$new("verbose", default = FALSE, tags = "train")
        )
      )
      ps$add_dep("class_sampling_factors", "balance_classes", CondEqual$new(TRUE))
      ps$add_dep("max_after_balance_size", "balance_classes", CondEqual$new(TRUE))
      ps$add_dep("elastic_averaging_moving_rate", "elastic_averaging", CondEqual$new(TRUE))
      ps$add_dep("elastic_averaging_regularization", "elastic_averaging", CondEqual$new(TRUE))
      ps$add_dep("quantile_alpha", "distribution", CondEqual$new("quantile"))
      ps$add_dep("tweedie_power", "distribution", CondEqual$new("tweedie"))
      ps$add_dep("huber_alpha", "distribution", CondEqual$new("huber"))

      super$initialize(
        id = "regr.h2odeeplearning",
        packages = "h2o",
        feature_types = c("integer", "numeric", "factor"),
        predict_types = "response",
        param_set = ps,
        properties = c("weights", "missings", "importance")
      )
    },

    train_internal = function(task) {
      pars = self$param_set$get_values(tags = "train")

      conn.up = tryCatch(h2o::h2o.getConnection(), error = function(err) return(FALSE))
      if (!inherits(conn.up, "H2OConnection")) {
        h2o::h2o.init()
      }

      y = task$target_names
      x = task$feature_names
      d = task$data()

      weights_column = NULL
      if ("weights" %in% task$properties) {
        d$.mlr_weights = task$weights[,weight,]
      }

      training_frame = h2o::as.h2o(d)
      invoke(h2o::h2o.deeplearning, y = y, x = x, training_frame = training_frame, weights_column = weights_column, .args = pars)
    },

    predict_internal = function(task) {
      newdata = h2o::as.h2o(task$data(cols = task$feature_names))

      p = invoke(predict, self$model, newdata = newdata, type = type)

      PredictionRegr$new(task = task, response = as.vector(p$predict))
    },

    importance = function() {
      imp = na.omit(as.data.frame(h2o::h2o.varimp(self$model)))
      res = imp$relative_importance
      names(res) = imp$variable
      res
    }
  )
)
