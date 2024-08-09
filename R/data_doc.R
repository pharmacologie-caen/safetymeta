#' Data of safety meta-analysis.
#'
#' Safety meta-analysis of aggregated datasets rely on 2 major compounds:
#' **study**-level data, and **event**-level data.
#' During the data management process, event-level data is grouped according to
#' safety terms, and becomes **term**-level data.
#' The study-level data begin with **`s_`**.
#' \itemize{
#'   \item `s_data` study-level data, including total number of subjects
#'   in each study arm, and other aggregated features (median age, median follow-up)
#' }
#' The event-level data begin with **`e_`**.
#' \itemize{
#'   \item `e_data_src*` event-level dataset from a specific source (src1, src2)
#'   \item `e_data`      event-level dataset, where all sources have been reconciled.
#' }
#' The term-level data begin with **`t_`**.
#' \itemize{
#'   \item `t_groups`    A named list. Names are the terms, contents are the
#'   events composing the term.
#'   \item `tnsc_data`   term-level dataset, also joined with some study
#'   characteristics (subgroups).
#' }
#'
#' @docType data
#'
#' @usage data(e_data)
#'
#' @format An object of class `data.frame`.
#'
#' @keywords datasets
#'
#' @references There is none
#'
#' @source None
#'
#' @examples
#' data(e_data)
#' e_data[e_data$study_id == "NCT1", ]

"e_data"

#' @rdname e_data

"e_data_src1"

#' @rdname e_data

"e_data_src2"

#' @rdname e_data

"t_groups"

#' @rdname e_data

"s_data"

#' @rdname e_data

"tnsc_data"
