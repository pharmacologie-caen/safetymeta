#' Define source order
#'
#' Prioritize over multiple sources, according to some criteria
#'
#' When multiples sources report safety data for the same event,
#' they should be prioritized among each other, in order to retain the source
#' with the highest priority for each event.
#' \itemize{
#'   \item If source sorting has been decided use the `source_list` method.
#'   With this method, your dataset should have a column with the `source_name`,
#'   and you provide the order in the `source_list_order` argument.
#'   The first element of `source_list_order` is the highest priority
#'   source, the second element is the next highest priority, and so on.
#'   \item If you want to sort sources according to highest number of patients
#'   in a study_arm (or any other number), use method `max_n`, and provide
#'   `study_id` and `study_n` arguments.
#' }
#'
#'
#' @param .data A study- or event-level dataset @seealso [e_data]
#' @param method Character string, one of "source_list", "max_n". See description.
#' @param source_name Optional, character string, a column with sources
#' @param source_list_order Optional, character vector, order of sources.
#' @param study_id Optional, study identifier.
#' @param study_n Optional, character string, a column with numbers (of patients).
#'
#' @returns The original .data dataset, with an additional `src` numeric column.
#' @export
#'
#' @examples
#'
#' # Source order is already known, then use method "source_list"
#'
#'  source_order <- c("src1", "src2")
#'
#'  e_data |>
#'    sort_sources(
#'      method = c("source_list"),
#'      source_name = "source",
#'      source_list_order = source_order
#'    )
#'
#'  # Sources with higher total number in arm1 have highest priority
#'
#'    e_data_maxn <-
#'       e_data |>
#'         dplyr::select(study_id, n_arm1) |>
#'         dplyr::add_row(
#'           study_id = "NCT1",
#'           n_arm1   = 30
#'         )
#'
#'   e_data_maxn |>
#'   sort_sources(
#'     method = c("max_n"),
#'     study_n = "n_arm1",
#'     study_id = "study_id"
#'     )

sort_sources <-
  function(
    .data,
    method = c("source_list", "max_n"),
    source_name = NULL,
    source_list_order = NULL,
    study_id = NULL,
    study_n  = NULL
  ){

    method <- match.arg(method)

    if(method == "source_list"){

      # create mask with src column
      s_mask <-
        data.frame(
          v1 = source_list_order,
          src = seq_along(source_list_order)
        ) |>
        rlang::set_names(
          c(source_name, "src")
        )

      # join to .data
      res <-
        .data |>
        dplyr::left_join(
          s_mask,
          by = source_name
        )

    }

    if(method == "max_n"){

      renamer <-
        c(study_id, study_n) |>
        rlang::set_names(c("s___id",
                           "s___n" ))

      back_renamer <-
        c("s___id",
          "s___n" ) |>
        rlang::set_names(
          c(study_id, study_n)
        )

      # create mask with src column
      s_mask <-
        .data |>
        dplyr::rename(dplyr::all_of(renamer)) |>
        dplyr::group_by(.data$s___id) |>
        dplyr::reframe(s___n = unique(.data$s___n)) |>
        dplyr::group_by(.data$s___id) |>
        dplyr::mutate(src = order(- .data$s___n))

      # join to .data
      res <-
        .data |>
        dplyr::left_join(
          s_mask,
          by = c(back_renamer)
        )

    }
    return(res)
  }
