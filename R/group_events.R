#' Group events of a common term group
#'
#' Allows for prioritization of event data, aggregating multiples terms under a group term (see integrative and conservative methods).
#'
#' See \code{vignette("basic_workflow", package = "safetymeta")} for details on
#' "event" and "term" definition.
#' With this function, you can group multiple events corresponding to a term.
#' A t_groups list provides one or more `terms` as its name(s).
#' **Integrative** method means counts for each event of a `term`
#' will be summed up together, while **conservative** means only the event
#' with the highest count will be selected.
#' `add_zero` is used when an event is not reported for a study
#' (e.g., the event line is NOT in the dataset).
#'  The principle is that since all serious events must be reported,
#'  the absence of reporting is equal to reporting 0 events.
#'  This is not true when safety data is not available at all
#'  (i.e., when there is no data for such event in any available sources).
#'  Thus, these specific studies must be **excluded** prior to using `group_events`.
#' Some total counts are not homogeneous for a given study, depending on the reaction (notably found for all cause mortality reaction). The maximum count will be retained for no line study(ies), (when `add_zero` performs).
#' **`group_events` assumes events in `.data[["events"]]` are LOWER cased.**
#'
#' @param .data An event data.frame (see details)
#' @param t_groups Named list of terms. Names are terms, contents are event names (see description).
#' @param n_event  Character vector, column with number of events in study arms (e.g. placebo...).
#' @param n_arm    Optional, character vector, column with number of patients in study arms.
#' @param event_name Character string, column of events name.
#' @param study_id   Character string, column of main study ID
#' @param src        Character vector, column with source sorting \code{\link{sort_sources}}.
#' @param method "integrative" or "conservative" (see details).
#' @param add_zero Do you wish to count O events for study(ies) without any events of a term? (see description)
#'
#' @return Special `attributes` of the output data.table are
#' \itemize{
#'   \item `group_method` (conservative or integrative)
#'   \item `group_term`
#'   \item `event`
#'   \item `n`
#' } They are used in subsequent functions to create a single dataset when there are multiple arms.
#' @keywords meta-analysis
#' @importFrom purrr list_rbind
#' @export
#' @examples
#'
#'
#' # You are very much advised to check string case over t_groups
#' # and .data[["event_name"]] col. All should be lower cased +++
#'
#' source_order <- c("src1", "src2")
#'
#' # set src before using group_events
#' e_data <-
#'   e_data |>
#'   sort_sources(
#'     method = c("source_list"),
#'     source_name = "source",
#'     source_list_order = source_order
#'   )
#'
#' # The integrative method
#'  e_data |>
#'   group_events(
#'     t_groups = t_groups,
#'     n_event = "n_event_arm1",
#'     n_arm     = "n_arm1",
#'     event_name = "event_name",
#'     study_id   = "study_id",
#'     method = "integrative"
#'   )
#'
#' # Contrasting with the conservative method
#'
#'  e_data |>
#'   group_events(
#'     t_groups = t_groups,
#'     n_event = "n_event_arm1",
#'     n_arm     = "n_arm1",
#'     event_name = "event_name",
#'     study_id   = "study_id",
#'     method = "conservative"
#'   )

group_events <-
  function(.data,
           t_groups,
           n_event,
           n_arm = NULL,
           event_name,
           study_id,
           src = "src",
           method = c("conservative", "integrative"),
           add_zero = FALSE) {

    t_names <- tolower(names(t_groups))

    method <- match.arg(method)

    # c.1 check src column type ---- ####
    if(!(any(c("integer", "numeric") %in% class(.data[[src]])))){
      stop(paste0("Column ", src, " is not numeric or integer.",
                  " Was it created with sort_source()?"))
    }

    # c.2 check there are as many n_event as n_arm columns ---- ####

    if (!is.null(n_arm) && !(length(n_event) == length(n_arm))) {
      stop("n_event should be the same length as n_arm.")
    }


    # renamers ---- ####

    renamer_event <-
      c(n_event) |>
      rlang::set_names(
        ~ paste0("n___event_",
                 seq_along(.x))
      )

    renamer_arms <-
      if(!is.null(n_arm)){
        c(n_arm) |>
          rlang::set_names(
            ~ paste0("n___arm_",
                     seq_along(.x))
            )
      }

    renamer_other <-
      c(study_id,
        event_name,
        src) |>
      rlang::set_names(
        c("s___id",
          "e___name",
          "src")
      )

    # back renamers

    br_event <-
      paste0("n___event_",
             seq_along(n_event)
      ) |>
      rlang::set_names(
        n_event
      )

    br_arms <-
      if(!is.null(n_arm)){
        paste0("n___arm_",
               seq_along(n_arm)
        ) |>
          rlang::set_names(
            n_arm
          )
      }

    br_s___id <-
      "s___id" |>
      rlang::set_names(
        study_id
      )

    br_details <-
      paste0("details_n___event_",
             seq_along(n_event)
      ) |>
      rlang::set_names(
        ~ paste0("details_", n_event)
      )

    # d.1 renaming ---- ####

    .data <-
      .data |>
      dplyr::rename(dplyr::all_of(
        c(renamer_event,
          renamer_arms,
          renamer_other)
        ))

    # az.1 keeping track of n per arms, study and src (if add_zero TRUE) ---- ####

    if(add_zero == TRUE){
      n_arm_study_src <-
        .data |>
        dplyr::group_by(.data$s___id,
                        .data$src) |>
        dplyr::reframe(
          dplyr::across(
            dplyr::starts_with("n___arm_"),
            ~ unique(.x)))

      # and create zero mask before filtering out events
      zero_mask <-
        expand.grid(names(t_groups), unique(.data$s___id)) |>
        dplyr::rename("term_name" = "Var1",
                      "s___id"    = "Var2") |>
        dplyr::left_join(
          n_arm_study_src,
          by = c("s___id"),
          relationship = "many-to-many" # many term_name, many src
        )

    }

    # c.3 check n___arms are unique per s___id and src ---- ####

    d1 <-
      .data |>
      dplyr::group_by(.data$src, .data$s___id) |>
      dplyr::select(
        dplyr::all_of(c("src", "s___id")),
        dplyr::starts_with("n___arm_")
        ) |>
      dplyr::distinct()

    d2 <-
      .data |>
      dplyr::select(
        dplyr::all_of(c("src", "s___id"))) |>
      dplyr::distinct()

    if(nrow(d1) > nrow(d2)){
      stop("n_arm should be unique per src/study couple.")
    }

    # d.2 filtering out other events ---- ####
    .data <-
      .data |>
      # filter out all other events
      dplyr::filter(.data$e___name %in% unlist(t_groups))


    # d.3 identify events belonging to a common term ---- ####

    t_expr <-
      t_groups |>
      purrr::map(
        function(events_){
          rlang::quo(
            ifelse(
              .data$e___name %in% !!events_,
              1,
              0
            )
          )
        }
      )

    .data <-
      .data |>
      dplyr::mutate(!!!t_expr)

    # core grouping function ---- ####

    group_event_core <-
      function(one_term){ # a term name
        .data |>
          dplyr::group_by(.data$src, .data$s___id) |>
          dplyr::filter(
            dplyr::if_all(
              dplyr::all_of(
                one_term
              ),
              ~ .x == 1)
            ) |>
          dplyr::summarise(
            term_name = one_term,

            # details of events in terms
            dplyr::across(
              dplyr::starts_with("n___event_"),
              ~ paste0(paste0(.data$e___name,
                          " (",
                          .x,
                          ")"),
                   collapse = ", "),
              .names = "details_{.col}"
              ),

            # grouping of events in terms (according to method)
            if(method == "integrative"){
              dplyr::across(
                dplyr::starts_with("n___event_"),
                ~ sum(.x, na.rm = FALSE))
              },

            if(method == "conservative"){
              dplyr::across(
                dplyr::starts_with("n___event_"),
                ~ max(.x, na.rm = FALSE))
            },

            # keeping track of n per arm

            if(!is.null(n_arm)){
              dplyr::across(
                dplyr::starts_with("n___arm_"),
                ~ min(.x)
              )
            },

            .groups = "keep"
          )
      }

    # apply core to all terms, select source with highest priority ---- ####

    int <-
      t_groups |>
      names() |>
      purrr::map(
        group_event_core
      ) |>
      purrr::list_rbind()

    # c.4 integrative approach incompatibility ---- ####

    if(!is.null(n_arm) && method == "integrative"){
      incompatibility <-
        int |>
        dplyr::filter(
          .data$n___event_1 > .data$n___arm_1
          )

      if(nrow(incompatibility > 0)){
        print(incompatibility)
        stop("integrative method yielded inappropriate counts. Use conservative method.")
      }
    }

    # az.2 ---- add zeros to no event studies ---- ####

    if (add_zero == TRUE) {

      int <-
        zero_mask |>
        dplyr::left_join(
          int |>
            dplyr::select(- dplyr::starts_with("n___arm_")),
          by = c("term_name", "s___id", "src")
          ) |>
        dplyr::group_by(.data$s___id, .data$src) |>
        dplyr::mutate(
          if(!is.null(n_arm)){
            dplyr::across(
              dplyr::starts_with("n___arm_"),
              # min only works since it is assumed n is unique per src.
              ~ min(.x, na.rm = TRUE)
            )
          },
          dplyr::across(
            dplyr::starts_with("n___event_"),
            ~ ifelse(is.na(n___event_1),
                     0,
                     n___event_1)),
          src = ifelse(
            is.na(src),
            0,
            src
          )
        )
    }

    # ---- filtering according to src per term ---- ####

    int <-
      int  |>
      dplyr::ungroup() |>
      dplyr::group_by(.data$s___id, .data$term_name) |>

      # select source with highest priority (lower number)

      dplyr::filter(.data$src == min(.data$src))

    # ---- back renaming, column rearrangement ---- ####

    # interlace event and arm columns
    e_a_interlace <-
      as.vector(rbind(
      names(renamer_event),
      names(renamer_arms)
    ))

    res <-
      int |>
      dplyr::relocate(
        dplyr::all_of(
          c("src",
            "s___id",
            "term_name"
          )
        )
      ) |>
      dplyr::relocate(
        dplyr::all_of(e_a_interlace),
        .after = dplyr::all_of("term_name")
      ) |>
      dplyr::relocate(
        dplyr::starts_with("details_"),
        .after = dplyr::last_col()
      ) |>
      dplyr::rename(
        dplyr::all_of(
          c(br_event,
            br_arms,
            br_s___id,
            br_details)
        )) |>
      dplyr::ungroup()


    # keep some details within the data.frame attributes, to be re-used after performing joints

    attr(res, "group_method") <- method

    res

  }
