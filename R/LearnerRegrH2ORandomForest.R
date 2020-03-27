#' @title Regression H2O Random Forest Learner
#'
#' @name mlr_learners_regr.h2orandomforest
#'
#' @description
#' Regression random forest learner.
#' Class [h2o::h2o.randomForest()] from package \CRANpkg{h2o}.
#'
#' @templateVar id regr.h2orandomforest
#' @template section_dictionary_learner
#'
#' @export
#' @template seealso_learner
#' @template example
LearnerRegrH2ORandomForest = R6Class("LearnerRegrRandomForest",
  inherit = LearnerRegr,
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
          # score_each_iteration
          # score_tree_interval
          # fold_assignment
          # fold_column
          ParamLgl$new("ignore_const_cols", default = TRUE, tags = "train"),
          # offset_column
          # weights_column - Implemented in .train
          ParamLgl$new("balance_classes", default = FALSE, tags = "train"),
          ParamUty$new("class_sampling_factors", default = NULL,
            tags = "train"),
          ParamDbl$new("max_after_balance_size", default = 5, tags = "train"),
          ParamInt$new("max_hit_ratio_k", lower = 0, default = 0,
            tags = "train"),
          ParamInt$new("ntrees", lower = 1L, default = 50L, tags = "train"),
          ParamInt$new("max_depth", lower = 1L, default = 20L, tags = "train"),
          ParamInt$new("min_rows", lower = 1L, default = 1L, tags = "train"),
          ParamInt$new("nbins", lower = 1L, default = 1024L, tags = "train"),
          ParamInt$new("nbins_top_level", lower = 1L, default = 1024L,
            tags = "train"),
          ParamInt$new("nbins_cats", lower = 1L, default = 1024L,
            tags = "train"),
          # r2_stopping - Deprecated
          ParamInt$new("stopping_rounds", default = 5L, lower = 0L,
            tags = "train"),
          ParamFct$new("stopping_metric",
            levels = c("AUTO", "logloss", "AUC", "lift_top_group",
              "misclassification", "AUCPR", "mean_per_class_error", "custom",
              "custom_increasing"), tags = "train"),
          ParamDbl$new("stopping_tolerance", default = 0, lower = 0,
            tags = "train"),
          ParamInt$new("max_runtime_secs", lower = 0L, default = 0L,
            tags = "train"),
          ParamInt$new("seed", default = -1, tags = "train"),
          ParamLgl$new("build_tree_one_node", default = FALSE,
            tags = "train"),
          ParamInt$new("mtries", lower = -1L, default = -1L,
            tags = "train"),
          ParamDbl$new("sample_rate", lower = 0, upper = 1, default = 0.632,
            tags = "train"),
          ParamUty$new("sample_rate_per_class", default = NULL, tags = "train"),
          ParamLgl$new("binomial_double_trees", default = FALSE,
            tags = "train"),
          ParamUty$new("checkpoint", default = NULL, tags = "train"),
          ParamDbl$new("col_sample_rate_change_per_level",
            lower = .Machine$double.neg.eps, upper = 2, default = 1,
            tags = "train"),
          ParamDbl$new("col_sample_rate_per_tree", lower = 0, upper = 1,
            default = 1, tags = "train"),
          ParamDbl$new("min_split_improvement", lower = 0, default = 1e-05,
            tags = "train"),
          ParamFct$new("histogram_type",
            levels = c("AUTO", "UniformAdaptive", "Random", "QuantilesGlobal",
              "RoundRobin"), default = "AUTO", tags = "train"),
          ParamFct$new("categorical_encoding", levels = c("AUTO", "Enum",
            "OneHotInternal", "OneHotExplicit", "Binary", "Eigen",
            "LabelEncoder", "SortByResponse", "EnumLimited"), default = "AUTO",
          tags = "train"),
          ParamLgl$new("calibrate_model", default = FALSE, tags = "train"),
          ParamUty$new("calibration_frame", default = NULL, tags = "train"),
          # distribution - Deprecated
          ParamUty$new("custom_metric_func", default = NULL, tags = "train"),
          ParamUty$new("export_checkpoints_dir", default = NULL,
            tags = "train"),
          ParamLgl$new("check_constant_response", default = TRUE,
            tags = "train"),
          ParamLgl$new("verbose", default = FALSE, tags = "train")
        )
      )
      ps$add_dep("class_sampling_factors", "balance_classes",
        CondEqual$new(TRUE))
      ps$add_dep("max_after_balance_size", "balance_classes",
        CondEqual$new(TRUE))

      super$initialize(
        id = "regr.h2orandomforest",
        packages = "h2o",
        feature_types = c("integer", "numeric", "factor"),
        predict_types = "response",
        param_set = ps,
        properties = c("weights", "missings"),
        man = "mlr3learners.h2o::mlr_learners_regr.h2orandomforest"
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

      if ("weights" %in% task$properties) {
        data$.mlr_weights = task$weights$weight
        pars$weights_column = ".mlr_weights"
      }

      training_frame = h2o::as.h2o(data)
      invoke(h2o::h2o.randomForest, y = target, x = feature,
        training_frame = training_frame, .args = pars)
    },

    .predict = function(task) {
      newdata = h2o::as.h2o(task$data(cols = task$feature_names))

      p = invoke(predict, self$model, newdata = newdata, type = type)

      PredictionRegr$new(task = task, response = as.vector(p$predict))
    }
  )
)
