#' @title H2O Deep Learning Classification Learner
#'
#' @name mlr_learners_classif.h2odeeplearning
#'
#' @description
#' Feed-forward multilayer artificial neural network classification learner.
#' Class [h2o::h2o.deeplearning()] from package \CRANpkg{h2o}.
#'
#' @templateVar id classif.h2odeeplearning
#' @template section_dictionary_learner
#'
#' @export
#' @template seealso_learner
#' @template example
LearnerClassifH2ODeeplearning = R6Class("LearnerClassifH2ODeeplearning",
  inherit = LearnerClassif,
  public = list(

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function() {
      ps = ParamSet$new(
        params = list(
          # x
          # y
          # traning_frame
          # model_id
          # validation_frame
          # nfolds
          # keep_cross_validation_models
          # keep_cross_validation_predictions
          # keep_cross_validation_fold_assignment
          # fold_assignment
          # fold_column
          ParamLgl$new("ignore_const_cols", default = TRUE, tags = "train"),
          # score_each_iteration
          # weights_column - Implemented in .train
          # offset_column
          ParamLgl$new("balance_classes", default = FALSE, tags = "train"),
          ParamDbl$new("class_sampling_factors", special_vals = list(NULL),
            default = NULL, tags = "train"),
          ParamDbl$new("max_after_balance_size", default = 5, tags = "train"),
          ParamInt$new("max_hit_ratio_k", lower = 1L, special_vals = list(0L),
            default = 0L, tags = "train"),
          # checkpoint
          # pretrained_autoencoder
          ParamLgl$new("overwrite_with_best_model", default = TRUE,
            tags = "train"),
          ParamLgl$new("use_all_factor_level", default = TRUE, tags = "train"),
          ParamLgl$new("standardize", default = TRUE, tags = "train"),
          ParamFct$new("activation",
            levels = c("Rectifier", "Tanh", "TanhWithDropout",
              "RectifierWithDropout", "Maxout", "MaxoutWithDropout"),
            default = "Rectifier", tags = "train"),
          ParamUty$new("hidden", default = c(200L, 200L), tags = "train"),
          ParamInt$new("epochs", default = 10L, lower = 1L, tags = "train"),
          ParamInt$new("train_samples_per_iteration", lower = 1L,
            special_vals = list(-2L, -1L, 0L), default = -2L, tags = "train"),
          ParamDbl$new("target_ratio_comm_to_comp", default = 0.05,
            tags = "train"),
          # seed
          ParamLgl$new("adaptive_rate", default = TRUE, tags = "train"),
          ParamDbl$new("rho", default = 0.99, lower = 0, tags = "train"),
          ParamDbl$new("epsilon", default = 1e-08, lower = .Machine$double.eps,
            tags = "train"),
          ParamDbl$new("rate", default = 0.005, lower = 0, upper = 1,
            tags = "train"),
          ParamDbl$new("rate_annealing", default = 1e-06, lower = 0,
            tags = "train"),
          ParamDbl$new("rate_decay", default = 1, lower = 0, tags = "train"),
          ParamDbl$new("momentum_start", default = 0, tags = "train"),
          ParamDbl$new("momentum_ramp", default = 1e+06, tags = "train"),
          ParamDbl$new("momentum_stable", default = 0, tags = "train"),
          ParamLgl$new("nesterov_accelerated_gradient", default = TRUE,
            tags = "train"),
          ParamDbl$new("input_dropout_ratio", default = 0, tags = "train"),
          ParamDbl$new("hidden_dropout_ratios", default = 0.5, tags = "train"),
          ParamDbl$new("l1", default = 0, tags = "train"),
          ParamDbl$new("l2", default = 0, tags = "train"),
          ParamDbl$new("max_w2", default = 3.4028235e+38, tags = "train"),
          ParamFct$new("initial_weight_distribution",
            levels = c("UniformAdaptive", "Uniform", "Normal"),
            default = "UniformAdaptive", tags = "train"),
          ParamDbl$new("initial_weight_scale", default = 1, tags = "train"),
          ParamUty$new("initial_weights", default = NULL, tags = "train"),
          ParamUty$new("initial_bias", default = NULL, tags = "train"),
          ParamFct$new("loss",
            levels = c("Automatic", "CrossEntropy", "Quadratic", "Absolute",
              "Huber"), default = "Automatic", tags = "train"),
          # distribution - Set in .train
          # quantile_alpha - Only regression
          # tweedie_power - Only regression
          # huber_alpha - Only regression
          ParamInt$new("score_interval", default = 5, tags = "train"),
          ParamInt$new("score_training_samples", lower = 1L, default = 10000L,
            special_vals = list(0L), tags = "train"),
          ParamInt$new("score_validation_samples", lower = 1L,
            special_vals = list(0L), default = 0L, tags = "train"),
          ParamDbl$new("score_duty_cycle", default = 0.1, tags = "train"),
          ParamDbl$new("classification_stop", lower = 0,
            special_vals = list(-1), default = 0, tags = "train"),
          # regression_stop - Only regression
          ParamInt$new("stopping_rounds", lower = 1L, special_vals = list(0L),
            default = 5L, tags = "train"),
          ParamFct$new("stopping_metric",
            levels = c("AUTO", "logloss", "AUC", "lift_top_group",
              "misclassification", "AUCPR", "mean_per_class_error",
              "custom", "custom_increasing"), tags = "train"),
          # stopping_metric - Only regression "deviance", "MSE", "RMSE", "MAE",
          # "RMSLE"
          ParamDbl$new("stopping_tolerance", default = 0, lower = 0,
            tags = "train"),
          ParamInt$new("max_runtime_secs", lower = 1L, special_vals = list(0L),
            default = 0L, tags = "train"),
          ParamFct$new("score_validation_sampling",
            levels = c("Uniform", "Stratified"), default = "Uniform",
            tags = "train"),
          ParamLgl$new("diagnostics", default = TRUE, tags = "train"),
          ParamLgl$new("fast_mode", default = TRUE, tags = "train"),
          ParamLgl$new("force_load_balance", default = TRUE, tags = "train"),
          # variable_importance
          ParamLgl$new("replicate_training_data", default = TRUE,
            tags = "train"),
          ParamLgl$new("single_node_mode", default = FALSE, tags = "train"),
          ParamLgl$new("shuffle_training_data", default = FALSE,
            tags = "train"),
          ParamFct$new("missing_values_handling",
            levels = c("MeanImputation", "Skip"), default = "MeanImputation",
            tags = "train"),
          ParamLgl$new("quiet_mode", default = FALSE, tags = "train"),
          ParamLgl$new("autoencoder", default = FALSE, tags = "train"),
          ParamLgl$new("sparse", default = FALSE, tags = "train"),
          ParamDbl$new("average_activation", default = 0, tags = "train"),
          ParamDbl$new("sparsity_beta", default = 0, tags = "train"),
          ParamInt$new("max_categorical_features", lower = 1L,
            default = 2147483647L, tags = "train"),
          ParamLgl$new("reproducible", default = FALSE, tags = "train"),
          ParamLgl$new("export_weights_and_biases", default = FALSE,
            tags = "train"),
          ParamInt$new("mini_batch_size", lower = 1L, default = 1L,
            tags = "train"),
          ParamFct$new("categorical_encoding",
            levels = c("AUTO", "Enum", "OneHotInternal", "OneHotExplicit",
              "Binary", "Eigen", "LabelEncoder", "SortByResponse",
              "EnumLimited"), default = "AUTO", tags = "train"),
          ParamLgl$new("elastic_averaging", default = FALSE, tags = "train"),
          ParamDbl$new("elastic_averaging_moving_rate", default = 0.9,
            tags = "train"),
          ParamDbl$new("elastic_averaging_regularization", default = 0.001,
            tags = "train"),
          ParamUty$new("export_checkpoints_dir", default = NULL,
            tags = "train"),
          ParamLgl$new("verbose", default = FALSE, tags = "train")
        )
      )
      ps$add_dep("class_sampling_factors", "balance_classes",
        CondEqual$new(TRUE))
      ps$add_dep("max_after_balance_size", "balance_classes",
        CondEqual$new(TRUE))
      ps$add_dep("elastic_averaging_moving_rate", "elastic_averaging",
        CondEqual$new(TRUE))
      ps$add_dep("elastic_averaging_regularization", "elastic_averaging",
        CondEqual$new(TRUE))

      super$initialize(
        id = "classif.h2odeeplearning",
        packages = "h2o",
        feature_types = c("integer", "numeric", "factor"),
        predict_types = c("response", "prob"),
        param_set = ps,
        properties = c("weights", "twoclass", "multiclass", "missings")
      )
    }
  ),

  private = list(

    .train = function(task) {
      conn.up = tryCatch(h2o::h2o.getConnection(), error = function(err) {
        return(FALSE)
      })
      if (!inherits(conn.up, "H2OConnection")) {
        h2o::h2o.init()
      }

      pars = self$param_set$get_values(tags = "train")
      target = task$target_names
      feature = task$feature_names
      data = task$data()

      if ("twoclass" %in% task$properties) {
        pars$distribution = "bernoulli"
      } else {
        pars$distribution = "multinomial"
      }

      if ("weights" %in% task$properties) {
        data$.mlr_weights = task$weights$weight
        pars$weights_column = ".mlr_weights"
      }

      training_frame = h2o::as.h2o(data)
      invoke(h2o::h2o.deeplearning, y = target, x = feature,
        training_frame = training_frame, .args = pars)
    },

    .predict = function(task) {
      newdata = h2o::as.h2o(task$data(cols = task$feature_names))

      p = invoke(predict, self$model, newdata = newdata, type = type)

      if (self$predict_type == "response") {
        PredictionClassif$new(task = task, response = as.vector(p$predict))
      } else {
        lvls = names(p)[-1]
        p = as.matrix(p[, -1])
        colnames(p) = lvls

        PredictionClassif$new(task = task, prob = p)
      }
    }
  )
)
