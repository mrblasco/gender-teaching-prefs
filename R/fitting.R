#' Fit and Save a Statistical Model
#'
#' This function fits a statistical model using a specified modeling function
#' (e.g., `lm`, `glm`, `lmer`), saves the fitted model to disk, and reloads it
#' on subsequent calls to avoid refitting.
#'
#' @param model_func The modeling function to use (e.g., `stats::lm`, `stats::glm`, `lme4::lmer`).
#' @param file Path to the `.rds` file where the fitted model should be saved or loaded from.
#' @param formula A model formula specifying the model to be fitted.
#' @param data A data frame containing the variables used in the model.
#' @param replace Logical, whether to replace existing file for the fitted model (default FALSE).
#' @param verbose Logical, whether to print messages (default TRUE).
#' @param strip Logical, whether to remove large objects from the fitted model to save memory (default TRUE).
#' @param ... Additional arguments passed to the modeling function.
#'
#' @return A fitted model object of the class returned by `model_func`.
#' @examples
#' fit_and_save(stats::lm, "lm_model.rds", mpg ~ wt + hp, mtcars)
#' fit_and_save(stats::glm, "glm_model.rds", vs ~ mpg + hp, mtcars, family = binomial)
#'
#' @export
fit_and_save <- function(model_func, file, formula, data, replace = FALSE, verbose = TRUE, strip = FALSE, ...) {
    log_msg <- function(...) {
        if (verbose) message(sprintf("[%s] %s", Sys.time(), ...))
    }

    if (!is.null(file) && file.exists(file) && !replace) {
        log_msg(sprintf("File found. Loading model from '%s'", file))
        start_load <- Sys.time()
        fit <- readRDS(file)
        load_time <- round(difftime(Sys.time(), start_load, units = "secs"), 2)
        log_msg(sprintf("Model loaded in %s seconds.", load_time))
        return(fit)
    }

    if (is.null(file)) log_msg("Fitting model (not saving to disk)...")
    else log_msg(sprintf("File %s not found or replacing. Fitting model...", file))

    start_time <- Sys.time()
    fit <- tryCatch({
        model_func(formula = formula, data = data, ...)
    }, error = function(e) {
        log_msg(sprintf("ERROR during model fitting: %s", e$message))
        stop(e)
    })
    fit_time <- round(difftime(Sys.time(), start_time, units = "secs"), 2)

    fit$call[[1]] <- substitute(model_func)
    fit$call$formula <- eval(formula)
    # TODO: modify family in call.
    #if ("family" %in% list(...)) fit$call$family <- eval(list(...)$family)

    if (strip) {
        if ("lm" %in% class(fit) || "glm" %in% class(fit)) {
            fit$model <- NULL
            fit$residuals <- NULL
            fit$fitted.values <- NULL
            environment(fit$terms) <- emptyenv()
        } else if ("lmerMod" %in% class(fit)) {
            fit@frame <- NULL
            environment(fit@call) <- emptyenv()
        }
    }

    if (!is.null(file)) {
        start_time <- Sys.time()
        tryCatch({
            saveRDS(fit, file)
        }, error = function(e) {
            log_msg(sprintf("ERROR during model saving: %s", e$message))
            stop(e)
        })
        save_time <- round(difftime(Sys.time(), start_time, units = "secs"), 2)
        log_msg(sprintf("Model fit in %s seconds and saved in %s seconds to '%s'", fit_time, save_time, file))
    } else {
        log_msg(sprintf("Model fit successfully in %s seconds.", fit_time))
    }
    return(fit)
}


#' Fit and Save a Generalized Linear Model (GLM)
#'
#' A convenience wrapper around [fit_and_save()] that uses `stats::glm`.
#'
#' @inheritParams fit_and_save
#' @examples
#' fit_and_save_glm("glm_model.rds", vs ~ mpg + hp, mtcars, family = binomial)
#'
#' @export
fit_and_save_glm <- function(file, formula, data, replace = FALSE, ...) {
    fit_and_save(model_func = stats::glm, file = file, formula = formula, data = data, replace = replace, verbose = TRUE, ...)
}


#' Fit and Save a Linear Model (LM)
#'
#' A convenience wrapper around [fit_and_save()] that uses `stats::lm`.
#'
#' @inheritParams fit_and_save
#' @examples
#' fit_and_save_lm("lm_model.rds", mpg ~ wt + hp, mtcars)
#'
#' @export
fit_and_save_lm <- function(file, formula, data, replace = FALSE, ...) {
    fit_and_save(model_func = stats::lm, file = file, formula = formula, data = data, replace = replace, verbose = TRUE, ...)
}


#' Fit and Save a Linear Mixed-Effects Model (LMM)
#'
#' A convenience wrapper around [fit_and_save()] that uses `lme4::lmer`.
#'
#' @inheritParams fit_and_save
#' @examples
#' \dontrun{
#'     library(lme4)
#'     fit_and_save_lmer("lmer_model.rds", Reaction ~ Days + (Days | Subject), sleepstudy)
#' }
#'
#' @export
fit_and_save_lmer <- function(file, formula, data, replace = FALSE, ...) {
    fit_and_save(model_func = lme4::lmer, file = file, formula = formula, data = data, replace = replace, ...)
}
