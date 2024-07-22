test_that("methods source_list and max_n", {
  e_data

  source_order <- c("src1", "src2")

  ss_sl <-
    sort_sources(
      .data = e_data,
      method = c("source_list"),
      source_name = "source",
      source_list_order = source_order
    )

  expect_equal(
    dplyr::bind_cols(e_data, src = c(1, 1, 1, 1, 2, 2, 2)),
    ss_sl
  )

  e_data_maxn <-
    e_data |>
    dplyr::select(study_id, n_arm1) |>
    dplyr::add_row(
      study_id = "NCT1",
      n_arm1   = 30
    )

  ss_maxn <-
    sort_sources(
      .data = e_data_maxn,
      method = c("max_n"),
      study_n = "n_arm1",
      study_id = "study_id"
    )

  expect_equal(
    dplyr::bind_cols(e_data_maxn,
                     src = c(2, 2, 1, 1, 1, 1, 1, 1)),
    ss_maxn
  )
})
