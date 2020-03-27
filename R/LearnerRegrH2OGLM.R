#' @title Generalized Linear Model Regression Learner
#'
#' @name mlr_learners_regr.h2oglm
#'
#' @description
#' Generalized linear model regression learner
#' Class [h2o::h2o.glm()] from package \CRANpkg{h2o}.
#'
#' @templateVar id regr.h2oglm
#' @template section_dictionary_learner
#'
#' @export
#' @template seealso_learner
#' @template example
LearnerRegrH2OGLM = R6Class("LearnerRegrH2OGLM",
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
          ParamInt$new("seed", default = -1, tags = "train"),
          # keep_cross_validation_models
          # keep_cross_validation_predictions
          # keep_cross_validation_fold_assignment
          # fold_assignment
          # fold_column
          # random_columns
          ParamLgl$new("ignore_const_cols", default = TRUE, tags = "train"),
          # score_each_iteration
          # offset_column
          # weights_column - Implemented in .train
          ParamFct$new("family", levels = c("gaussian", "quasibinomial",
            "poisson", "gamma", "tweedie", "negativebinomial"), tags = "train"),
          # family - "binomial", "ordinal", "multinomial"
          ParamUty$new("rand_family", default = "[gaussian]", tags = "train"),
          ParamDbl$new("tweedie_variance_power", default = 0, tags = "train"),
          ParamDbl$new("tweedie_link_power", default = 1, tags = "train"),
          ParamDbl$new("theta", lower = .Machine$double.eps, default = 1e-10,
            tags = "train"),
          ParamFct$new("solver", levels = c("AUTO", "IRLSM", "L_BFGS",
            "COORDINATE_DESCENT_NAIVE", "COORDINATE_DESCENT"), default = "AUTO",
          tags = "train"),
          # solver - No implemented "GRADIENT_DESCENT_LH",
          # "GRADIENT_DESCENT_SQERR"
          ParamDbl$new("alpha", lower = 0, special_vals = list(NULL),
            default = NULL, tags = "train"),
          ParamDbl$new("lambda", lower = 0, special_vals = list(NULL),
            default = NULL, tags = "train"),
          ParamLgl$new("lambda_search", default = FALSE, tags = "train"),
          ParamLgl$new("early_stopping", default = TRUE, tags = "train"),
          ParamInt$new("nlambdas", default = -1, tags = "train"),
          ParamLgl$new("standardize", default = TRUE, tags = "train"),
          ParamFct$new("missing_values_handling",
            levels = c("MeanImputation", "Skip", "PlugValues"),
            default = "MeanImputation", tags = "train"),
          ParamUty$new("plug_values", default = NULL, tags = "train"),
          # compute_p_values
          ParamLgl$new("remove_collinear_columns", default = FALSE,
            tags = "train"),
          ParamLgl$new("intercept", default = TRUE, tags = "train"),
          ParamLgl$new("non_negative", default = FALSE, tags = "train"),
          ParamInt$new("max_iterations", lower = 1, special_vals = list(-1),
            default = -1, tags = "train"),
          ParamDbl$new("objective_epsilon", lower = 0, special_vals = list(-1),
            default = -1, tags = "train"),
          ParamDbl$new("beta_epsilon", lower = 0, default = 0.0001,
            tags = "train"),
          ParamDbl$new("gradient_epsilon", lower = 0, special_vals = list(-1),
            default = -1, tags = "train"),
          ParamFct$new("link", levels = c("family_default", "identity", "logit",
            "log", "inverse", "tweedie"), default = "family_default",
          tags = "train"),
          # link - Not implemented ologit
          ParamUty$new("rand_link", default = "[family_default]",
            tags = "train"),
          ParamUty$new("startval", default = NULL, tags = "train"),
          # calc_like
          ParamLgl$new("HGLM", default = FALSE, tags = "train"),
          ParamDbl$new("prior", lower = 0, special_vals = list(-1),
            default = -1, tags = "train"),
          ParamDbl$new("lambda_min_ratio", default = -1, tags = "train"),
          ParamUty$new("beta_constraints", default = NULL, tags = "train"),
          ParamInt$new("max_active_predictors", lower = 0,
            special_vals = list(-1), default = -1, tags = "train"),
          ParamUty$new("interactions", default = NULL, tags = "train"),
          ParamUty$new("interaction_pairs", default = NULL, tags = "train"),
          ParamDbl$new("obj_reg", lower = 1, special_vals = list(-1),
            default = -1, tags = "train"),
          ParamUty$new("export_checkpoints_dir", default = NULL,
            tags = "train"),
          # balance_classes
          # class_sampling_factors
          # max_after_balance_size
          # max_hit_ratio_k
          ParamInt$new("max_runtime_secs", lower = 0L, default = 0L,
            tags = "train"),
          ParamUty$new("custom_metric_func", default = NULL, tags = "train")
        )
      )
      ps$add_dep("plug_values", "missing_values_handling",
        CondEqual$new("MeanImputation"))
      ps$add_dep("beta_epsilon", "solver",
        CondEqual$new("IRLSM"))
      ps$add_dep("tweedie_variance_power", "family",
        CondEqual$new("tweedie"))
      ps$add_dep("tweedie_link_power", "family",
        CondEqual$new("tweedie"))
      ps$add_dep("theta", "family",
        CondEqual$new("negativebinomial"))
      ps$add_dep("beta_epsilon", "solver",
        CondEqual$new("IRLSM"))
      ps$add_dep("gradient_epsilon", "solver",
        CondEqual$new("L_BFGS"))

      super$initialize(
        id = "regr.h2oglm",
        packages = "h2o",
        feature_types = c("integer", "numeric", "factor"),
        predict_types = "response",
        param_set = ps,
        properties = c("weights", "missings"),
        man = "mlr3learners.h2o::mlr_learners_regr.h2oglm"
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
      invoke(h2o::h2o.glm, y = target, x = feature,
        training_frame = training_frame, .args = pars)
    },

    .predict = function(task) {
      newdata = h2o::as.h2o(task$data(cols = task$feature_names))

      p = invoke(predict, self$model, newdata = newdata, type = type)

      PredictionRegr$new(task = task, response = as.vector(p$predict))
    }
  )
)
