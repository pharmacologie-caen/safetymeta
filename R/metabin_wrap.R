#' Wrapper around meta::metabin
#'
#' `r lifecycle::badge("experimental")` Compute a meta analysis of binary events,
#' and allows for multiple subgroup analyses.
#' `event_colnames` is similar to `event.e` and `event.c`
#' in meta::metabin, and `n_colnames` is similar to `n.e` and `n.c`, but they use
#' character strings as inputs. The `*_colnames` args also cover more usecase to
#' prevent metabin failure, in particular when there are missing data.
#' `by_var` is different from `byvar` in metabin (by_var is a character string).
#' The same is true for `stud_lab`.
#'
#' @param data Data.frame containing study information
#' @param event_colnames List of paired experimental - control column names with event counts
#' @param n_colnames n counts, see Details.
#' @param by_var Character vector, subgroup variables
#' @param stud_lab Character string, id col for each study
#' @param ... Additional arguments to be passed to metabin (method, sm...).
#'
#' @return meta::metabin models, with very slight difference (subset and call parameters).
#' @export
#'
#' @examples
#'
#'
#' tnsc_data |>
#'   dplyr::filter(term_name == "myocardial_infarction") |>
#'   metabin_wrap(
#'     event_colnames = c("event_e", "event_c"),
#'     n_colnames = c("n_e", "n_c"),
#'     by_var = c("byvar1", "byvar2"),
#'     stud_lab = "study_id"
#'   )

metabin_wrap <- function(
    data,
    event_colnames,
    n_colnames = paste0("n_", event_colnames),
    by_var,
    stud_lab,
    ... ) {

  dots <- rlang::list2(...)

  # Get variables for metabin
  symb_create_from_colnames <-
    function(event_colnames) {
      # This function requires that groups names are provided in the following order :
      # experimental group THEN control group
      group_e <- event_colnames
      names(group_e) <- c("event.e", "event.c")

      group_n <- n_colnames
      names(group_n) <- c("n.e", "n.c")

      rlang::syms(c(group_e, group_n))
    }

  vars <-
      symb_create_from_colnames(event_colnames = event_colnames)

  studlab_s <-
      rlang::syms(stud_lab) |>
        rlang::set_names("studlab")

  # Build metabin expression with the variables
  core_metabin_sm <-
    function(bv) {

      byvar_var <- rlang::syms(bv)|>
        rlang::set_names("byvar")

      mod_expr <-
        rlang::expr(
          meta::metabin(
            !!!vars,
            !!!studlab_s,
            data = data,
            subset =
              !is.na(!!vars[["event.e"]]) &
              !is.na(!!vars[["event.c"]]) &
              !is.na(!!byvar_var),
            !!!byvar_var,
            !!!dots
          )
        )

      # Prevent error from absence of available studies

      if (nrow(data[!is.na(data[[vars[["event.e"]]]]) &
                    !is.na(data[[vars[["event.c"]]]]) &
                    !is.na(data[[bv]]),]) > 0) {

        data <- data[!is.na(data[[bv]]),]
        # needed for subgroups with missing values

        mod <- eval(mod_expr)

      } else {
        mod <- NULL
        warning(paste0(
          "there was no study to compare ",
          vars[["event.e"]],
          " to ",
          vars[["event.c"]],
          " for this outcome"
        ))
      }

      mod
    }

  by_var |>
    rlang::set_names() |>
    purrr::map(
      core_metabin_sm
    )

}
