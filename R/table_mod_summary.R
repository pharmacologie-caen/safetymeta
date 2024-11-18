#' Table a model summary estimates
#'
#' Shorthand to get results from one or more meta models as a data.frame
#'
#' `r lifecycle::badge("experimental")` The idea is to switch from a list of meta models, themselves lists, to an handy data.frame. At the moment, only works for metabin exports and metaprop "main" export. The function internally applies the core extraction to all items. A meta model does not incorporate much meta variables on what was summarized: the name of the list item is used and should be explicit enough to describe the whole setting (e.g. include adverse event name, type of data aggregation conservative/integrative, etc.)
#'
#' @param mod_list A named list of models, or a single meta model.
#' @param main_or_sbgp Do you want to extract the main summary or subgroups?
#' @param common_or_random Do you want common or random effects parameters?
#' @param prop_scaler Only used when working with metaproportion or metarates, will express the result as x events per `prop_scaler` patients
#'
#' @keywords meta-analysis data-extraction
#'
#' @returns A data.frame with the following columns:
#' \itemize{
#'  \item `analysis` The type of analysis (main outcome or subgroups)
#'  \item `mod_name` The name of the model
#'  \item `event`, `event.e`, `event.c`, `n`, `n.e`, `n.c`, `k`, `I2`, `Q`,
#'  `pval.Q`, `Q.b`, `pval.Q.b`:
#'  See `?meta::meta-object`. For metaprop, Q, and pval.Q are from Wald test.
#'  For metarate models, Q and pval.Q are from Wald test.
#'  \item `n_stud_i2` Formating of I2 and k.
#'  \item `sm (95%CI)` The summary effect and its 95% confidence interval ('sm' parameter)
#'  \item `method_sm_effects` The method, summary measure and effect type
#'  \item `p` The summary effect p-value
#'  \item `TE`, `lower`, `upper`, either .common or .random as in `?meta::meta-object`
#'  for main outcomes, and the same with .w for subgroups.
#'  \item `bylab` The subgroup label
#'  \item `bylevs` The subgroup levels
#'  }
#'
#' @export
#' @examples
#' library(meta)
#'
#' data <-
#'     data.frame(
#'       event = 4:1,
#'       n = c(5, 7, 8, 9),
#'       subgroup = c("a", "a", "b", "b")
#'     )
#'
#'  m1 <- metaprop(
#'     event = event,
#'     n = n,
#'     subgroup = subgroup,
#'     data = data
#'   )
#'
#' table_mod_summary(m1)
#'
#' # If you have a list of models, named them
#'  res_mod <-
#'    table_mod_summary(
#'      list(mod1 = m1)
#'    )
#

table_mod_summary <- function(
    mod_list,
    main_or_sbgp = c("main", "sbgp"),
    common_or_random = c("common", "random"),
    prop_scaler = 1000

){
  main_or_sbgp     <- match.arg(main_or_sbgp)
  common_or_random <- match.arg(common_or_random)

  # check that its a meta model, or a list of meta models

  is_single_model <-
    all(c("TE", "method", "sm") %in% names(mod_list))

  if(!is_single_model){
    list_checking <-
      purrr::map(mod_list, function(m_)
        all(c("TE", "method", "sm") %in% names(m_)) | is.null(m_)
      ) |>
      unlist()

    if(!all(list_checking)){
      stop(paste0("mod_list must be a unique model, or a list of models.",
                  " Nested lists in lists are no longer allowed."))
    }
  }

  # drop NULL elements if its not a non-listed model (say, if you find some key meta args in the names)

  if(!is_single_model){
    mod_list <-
      mod_list |>
      purrr::compact()
  }

  # transform a single model into a list

  if(is_single_model){
    mod_list <-
      list(mod = mod_list)
  }

  # check that all meta models are of one kind (bin, prop, rate)

  mod_class <-
    purrr::map(mod_list, function(m_)
      class(m_))

  ref_class <- mod_class[[1]]

  mod_class_unicity_checker <-
    mod_class |>
    purrr::map(
      function(mc)
        mc == ref_class
    ) |>
    unlist()

  if(!all(mod_class_unicity_checker)){
    stop(paste0("All models should be the same type (metabin, metarate OR metaprop)."))
  }




  # Effects type related variables
  TE.sym    <- rlang::sym(paste0("TE.",    common_or_random))
  lower.sym <- rlang::sym(paste0("lower.", common_or_random))
  upper.sym <- rlang::sym(paste0("upper.", common_or_random))
  pval.sym  <- rlang::sym(paste0("pval.",  common_or_random))

  TE.sym.w     <- rlang::sym(paste0("TE.",       common_or_random, ".w"))
  lower.sym.w  <- rlang::sym(paste0("lower.",    common_or_random, ".w"))
  upper.sym.w  <- rlang::sym(paste0("upper.",    common_or_random, ".w"))
  pval.Q.b.sym <- rlang::sym(paste0("pval.Q.b.", common_or_random))
  Q.b.sym      <- rlang::sym(paste0("Q.b.",      common_or_random))


  # extractors ---- ####

  extractor_metabin_main <- function(mod, mod_name){
    with(mod,
         eval(expr(
           data.frame(
             analysis = "main outcome",
             mod_name,
             event.e = sum(event.e),
             n.e = sum(n.e),
             event.c = sum(event.c),
             n.c = sum(n.c),
             n_stud_i2 = paste0(k, "/", cff(I2 * 100), "%"),
             sm_95 = cff(
               exp(!!TE.sym),
               exp(!!lower.sym),
               exp(!!upper.sym),
               dig = 2,
               method = "num_ci"
             ),
             method_sm_effects = paste0(method, " ", sm, " ", common_or_random),
             `p`          = nice_p(!!pval.sym),
             TE           = exp(!!TE.sym),
             lower        = exp(!!lower.sym),
             upper        = exp(!!upper.sym),
             !!pval.sym,
             k,
             I2,
             Q,
             pval.Q
           )
         ))) |>
      dplyr::rename(
        c("sm (95%CI)" = "sm_95",
          "n stud/I2" = "n_stud_i2"
        )
      )
  }


  # core function

  core_extractor <- function(mod,
                             mod_name) {



    if (any(grepl("metabin", class(mod)))) {
      res_main <-
        extractor_metabin_main(mod, mod_name)

      if(main_or_sbgp == "sbgp"){ # prevent error from extracting in the absence of an sbgp analysis
        res_sbgp <-

          with(mod,
               eval(expr(
                 data.frame(
                   analysis = "subgroups",
                   bylab,

                   mod_name,

                   bylevs,
                   event.e = event.e.w,
                   n.e = n.e.w,
                   event.c = event.c.w,
                   n.c = n.c.w,
                   n_stud_i2 = paste0(k.w, "/", cff(I2.w * 100), "%"),

                   sm_95 = cff(
                     exp(!!TE.sym.w),
                     exp(!!lower.sym.w),
                     exp(!!upper.sym.w),
                     dig = 2,
                     method = "num_ci"
                   ),
                   method_sm_effects = paste0(method, " ", sm, " ", common_or_random),
                   `p` = nice_p(!!pval.Q.b.sym),

                   TE    = exp(!!TE.sym.w),
                   lower = exp(!!lower.sym.w),
                   upper = exp(!!upper.sym.w),
                   !!pval.Q.b.sym,
                   !!Q.b.sym,
                   k.w,
                   I2.w
                 )
               ))) |>
          dplyr::rename(
            c("sm (95%CI)" = "sm_95",
              "n stud/I2" = "n_stud_i2"
            )
          )
      }

    } else {
      if (any(grepl("metaprop", class(mod)))) {

        if(mod$sm != "PLOGIT"){
          stop("metaprop table is only possible with sm='PLOGIT'")
        }

        res_main <- with(mod,
                         eval(rlang::expr(
                           data.frame(
                             analysis = "main outcome",
                             mod_name,
                             event = sum(event),
                             n = sum(n),
                             n_stud_i2 = paste0(k, "/", cff(I2 * 100), "%"),
                             prop_ci =
                               cff(
                                 # logit 2 probability is plogis in base R
                                 plogis(!!TE.sym) * prop_scaler,
                                 plogis(!!lower.sym) * prop_scaler,
                                 plogis(!!upper.sym) * prop_scaler,
                                 dig = 2,
                                 method = "num_ci"
                               ),
                             per = paste0(prop_scaler, " pts"),
                             method_sm_effects = paste0(method, " ", sm, " ", common_or_random),
                             TE    = plogis(!!TE.sym),
                             lower = plogis(!!lower.sym),
                             upper = plogis(!!upper.sym),
                             k,
                             I2,
                             Q["Wald"],
                             pval.Q[1]
                           )
                         ))) |>
          dplyr::rename(c("Proportion (95%CI)" = "prop_ci",
                          "n stud/I2" = "n_stud_i2"))

        if(main_or_sbgp == "sbgp")
          res_sbgp <- with(mod,
                           eval(rlang::expr(
                             data.frame(
                               analysis = "subgroups",
                               mod_name,
                               bylevs,
                               event = event.w,
                               n = n.w,
                               n_stud_i2 = paste0(k.w, "/", cff(I2.w * 100), "%"),
                               prop_ci =
                                 cff(
                                   # logit 2 probability is plogis in base R
                                   plogis(!!TE.sym.w) * prop_scaler,
                                   plogis(!!lower.sym.w) * prop_scaler,
                                   plogis(!!upper.sym.w) * prop_scaler,
                                   dig = 2,
                                   method = "num_ci"
                                 ),
                               per = paste0(prop_scaler, " pts"),

                               method_sm_effects = paste0(method, " ", sm, " ", common_or_random),
                               `p`   = nice_p(!!pval.Q.b.sym),
                               TE    = plogis(!!TE.sym.w),
                               lower = plogis(!!lower.sym.w),
                               upper = plogis(!!upper.sym.w),
                               !!pval.Q.b.sym,
                               !!Q.b.sym,
                               k.w,
                               I2.w
                             )
                           ))) |>
            dplyr::rename(c("Proportion (95%CI)" = "prop_ci",
                            "n stud/I2" = "n_stud_i2"))
      }
      if (any(grepl("metarate", class(mod)))) {

        na_event_time_checkers <-
          is.na(sum(mod$event)) |
          is.na(sum(mod$time))

        if(na_event_time_checkers){
          warning(
            paste0("NAs in event and/or time, ",
                   "event count will be false and time count will be NA.")
          )
        }

        if(main_or_sbgp == "main"){
          res_main <- with(mod,
                           eval(expr(
                             data.frame(
                               analysis = "main outcome",
                               mod_name,
                               event = sum(event),
                               time = sum(time),
                               n = sum(n),
                               n_stud_i2 = paste0(k, "/", cff(I2 * 100), "%"),
                               rate_ci =
                                 cff(
                                   exp(!!TE.sym) * prop_scaler,
                                   exp(!!lower.sym) * prop_scaler,
                                   exp(!!upper.sym) * prop_scaler,
                                   dig = 2,
                                   method = "num_ci"
                                 ),
                               per = paste0(prop_scaler, " person.time"),
                               method_sm_effects = paste0(method, " ", sm, " ", common_or_random),
                               TE    = exp(!!TE.sym),
                               lower = exp(!!lower.sym),
                               upper = exp(!!upper.sym),
                               k,
                               I2,
                               Q = Q[1], # first is Wald test in GLMM
                               pval.Q = pval.Q[1]
                             )
                           ))) |>
            dplyr::rename(c("Rate (95%CI)" = "rate_ci",
                            "n stud/I2" = "n_stud_i2"))
        } else {
          if(main_or_sbgp == "sbgp"){
            res_sbgp <- with(mod,
                             eval(expr(
                               data.frame(
                                 analysis = "subgroups",
                                 mod_name,
                                 bylevs,
                                 event = data |>
                                   dplyr::group_by(.subgroup) |>
                                   dplyr::summarise(event = sum(event, na.rm = TRUE)) |>
                                   # thats strange, event.w, n.w are NA...
                                   # I excluded is.na rows from the analysis, so should be working.
                                   dplyr::pull(),
                                 time = data |>
                                   dplyr::group_by(.subgroup) |>
                                   dplyr::summarise(time = sum(time, na.rm = TRUE)) |>
                                   dplyr::pull(),
                                 n = data |>
                                   dplyr::group_by(.subgroup) |>
                                   dplyr::summarise(n = sum(n, na.rm = TRUE)) |>
                                   dplyr::pull(),
                                 n_stud_i2 = paste0(k.w, "/", cff(I2.w * 100), "%"),
                                 rate_ci =
                                   cff(
                                     exp(!!TE.sym.w) * prop_scaler,
                                     exp(!!lower.sym.w) * prop_scaler,
                                     exp(!!upper.sym.w) * prop_scaler,
                                     dig = 2,
                                     method = "num_ci"
                                   ),
                                 per = paste0(prop_scaler, " person.time"),

                                 method_sm_effects = paste0(method, " ", sm, " ", common_or_random),
                                 `p`   = nice_p(!!pval.Q.b.sym),
                                 TE    = exp(!!TE.sym.w),
                                 lower = exp(!!lower.sym.w),
                                 upper = exp(!!upper.sym.w),
                                 !!pval.Q.b.sym,
                                 !!Q.b.sym,
                                 k.w,
                                 I2.w
                               )
                             ))) |>
              dplyr::rename(c("Rate (95%CI)" = "rate_ci",
                              "n stud/I2" = "n_stud_i2"))

          }
        }
      }
    }


    if (main_or_sbgp == "main") {
      res_main
    } else {
      res_sbgp
    }
  }


  # core_extractor(mod = mod, mod_name = names(mod_list)[106])


  res <-
    purrr::pmap_dfr(
      list(
        m_ = mod_list,
        m_n = names(mod_list)
      ),
      function(m_, m_n){
        core_extractor(
          mod = m_,
          mod_name = m_n
        )
      }
    )


  if(any(duplicated(res))){
    warning(paste0("There are duplicate results. ",
    "Did you intend to collect subgroups results, with main_or_sbgp = 'sbgp'?")
    )
  }

  res

}
