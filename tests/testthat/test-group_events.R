test_that("standard grouping works", {

  source_order <- c("src1", "src2")

  t_data <-
    e_data |>
    sort_sources(
      method = c("source_list"),
      source_name = "source",
      source_list_order = source_order
    ) |>
    group_events(
      t_groups = t_groups,
      n_event = "n_event_arm1",
      n_arm     = "n_arm1",
      event_name = "event_name",
      study_id   = "study_id",
      method = "conservative"
  )

  # conservative
  true_t_data <-
    dplyr::tibble(
      src = c(1, 1, 1, 2),
      study_id = c("NCT1", "NCT2",
                   "NCT2", "NCT3"),
      term_name = c("myocardial_infarction", "myocardial_infarction",
                    "skin", "skin"),
      n_event_arm1 = c(2, 5, 10, 20),
      n_arm1  = c(20, 27, 27, 50),
      details_n_event_arm1 =
        c("myocardial infarction (1), acute coronary syndrome (2)",
          "myocardial infarction (5)",
          "eczema (10)",
          "pruritus (20)")
    )

  attr(true_t_data, "group_method") <- "conservative"

  expect_equal(
    t_data, true_t_data
  )

  # integrative

  t_data <-
    e_data |>
    sort_sources(
      method = c("source_list"),
      source_name = "source",
      source_list_order = source_order
    ) |>
    group_events(
      t_groups = t_groups,
      n_event = "n_event_arm1",
      n_arm     = "n_arm1",
      event_name = "event_name",
      study_id   = "study_id",
      method = "integrative"
    )

  true_t_data_int <-
    dplyr::tibble(
      src = c(1, 1, 1, 2),
      study_id = c("NCT1", "NCT2",
                   "NCT2", "NCT3"),
      term_name = c("myocardial_infarction", "myocardial_infarction",
                    "skin", "skin"),
      n_event_arm1 = c(3, 5, 10, 20),
      n_arm1  = c(20, 27, 27, 50),
      details_n_event_arm1 =
        c("myocardial infarction (1), acute coronary syndrome (2)",
          "myocardial infarction (5)",
          "eczema (10)",
          "pruritus (20)")
    )

  attr(true_t_data_int, "group_method") <- "integrative"

  expect_equal(
    t_data, true_t_data_int
  )
})

test_that("non numeric/integer src column produces an error", {
  expect_error(
    e_data |>
    dplyr::mutate(
      src = "src1"
    ) |>
    group_events(
      t_groups = t_groups,
      n_event = "n_event_arm1",
      n_arm     = "n_arm1",
      event_name = "event_name",
      study_id   = "study_id",
      method = "conservative"
    ),
    "src is not numeric or integer."
  )
})

test_that("non-unique n_arm per src/study couple produces an error", {
  source_order <- c("src1", "src2")

  e_data <-
    e_data |>
    dplyr::mutate(
      n_arm1 =
        ifelse(
          source == "src1" & study_id == "NCT1" &
            event_name == "acute coronary syndrome",
          21,
          n_arm1
    )) |>
    sort_sources(
      method = c("source_list"),
      source_name = "source",
      source_list_order = source_order
    )

  expect_error(
    e_data |>
    group_events(
      t_groups = t_groups,
      n_event   = "n_event_arm1",
      n_arm     = "n_arm1",
      event_name = "event_name",
      study_id   = "study_id",
      method = "conservative"
    ),
    "n_arm should be unique per src"
  )
})

test_that("term count exceeding n_arm with integrative method produces an error", {
  source_order <- c("src1", "src2")

  e_data <-
    e_data |>
    dplyr::mutate(
      n_event_arm1 =
        ifelse(
          source == "src1" & study_id == "NCT1" &
            event_name == "myocardial infarction",
          19,
          n_event_arm1
        )) |>
    sort_sources(
      method = c("source_list"),
      source_name = "source",
      source_list_order = source_order
    )

  expect_snapshot(expect_error(
    e_data |>
      group_events(
        t_groups = t_groups,
        n_event   = "n_event_arm1",
        n_arm     = "n_arm1",
        event_name = "event_name",
        study_id   = "study_id",
        method = "integrative"
      ),
    "integrative method yielded inappropriate counts. Use conservative method"
  ))
})

test_that("add_zero works", {
  source_order <- c("src1", "src2")

  t_data <-
    e_data |>
    sort_sources(
      method = c("source_list"),
      source_name = "source",
      source_list_order = source_order
    ) |>
    group_events(
      t_groups = t_groups,
      n_event = "n_event_arm1",
      n_arm     = "n_arm1",
      event_name = "event_name",
      study_id   = "study_id",
      method = "conservative",
      add_zero = TRUE
    )

  # conservative
  true_t_data <-
    dplyr::tibble(
      src = c(1, 1, 1, 1, 2, 2),
      study_id = c("NCT1", "NCT1",
                   "NCT2", "NCT2",
                   "NCT3", "NCT3"),
      term_name = rep(c("myocardial_infarction", "skin"), 3),
      n_event_arm1 = c(2, 0, 5, 10, 0, 20),
      n_arm1  = c(20, 20, 27, 27, 50, 50),
      details_n_event_arm1 =
        c("myocardial infarction (1), acute coronary syndrome (2)",
          NA_character_,
          "myocardial infarction (5)",
          "eczema (10)",
          NA_character_,
          "pruritus (20)")
    )

  attr(true_t_data, "group_method") <- "conservative"

  expect_equal(
    t_data, true_t_data
  )
})

test_that("no events matching t_group still produce a result",{
  e1 <-
    e_data |>
    dplyr::filter(event_name %in% c("eczema", "pruritus")) |>
    sort_sources(method = "source_list",
                 source_name = "source",
                 source_list_order = c("src1", "src2"))

  expect_warning(expect_warning(
    r1 <<- e1 |>
      group_events(
        t_groups = t_groups[1],
        n_event = "n_event_arm1",
        n_arm     = "n_arm1",
        event_name = "event_name",
        study_id   = "study_id"
      )
  ))

  r2 <-
    e1 |>
    group_events(
      t_groups = t_groups[2],
      n_event = "n_event_arm1",
      n_arm     = "n_arm1",
      event_name = "event_name",
      study_id   = "study_id"
    )

  expect_equal(
    names(r1),
    names(r2)
  )

  expect_equal(
    nrow(r1),
    0
  )

  # should procude a table with 0s
  expect_warning(
    r3 <<- e1 |>
      group_events(
        t_groups = t_groups[1],
        n_event = "n_event_arm1",
        n_arm     = "n_arm1",
        event_name = "event_name",
        study_id   = "study_id",
        add_zero = TRUE
      )
  )

  true_r3 <-
    dplyr::tibble(
      src = c(1, 2),
      study_id = c("NCT2", "NCT3"),
      term_name = "myocardial_infarction",
      n_event_arm1 = 0,
      n_arm1 = c(27, 50),
      details_n_event_arm1 = NA_character_
    )

  attr(true_r3, "group_method") <- "conservative"

  expect_equal(
    r3, true_r3
  )
})

test_that("n_arm should be same length as n_event", {
  e1 <-
    e_data |>
    dplyr::filter(event_name %in% c("eczema", "pruritus")) |>
    sort_sources(method = "source_list",
                 source_name = "source",
                 source_list_order = c("src1", "src2"))

  expect_error(
    e1 |>
      group_events(
        t_groups = t_groups[1],
        n_event = c("n_event_arm1", "n_not_even_in_data"),
        n_arm     = "n_arm1",
        event_name = "event_name",
        study_id   = "study_id"
      ),
    "n_event should be the same length as n_arm."
  )
})

test_that("a different source can be retained for each term, in the same study",{

  e_s <-
    data.frame(
      study_id = c("NCT1", "NCT1"),
      event_name = c("myocardial infarction",
                     "eczema"),
      src = c(1, 2),
      n_event_arm1 = c(2, 15),
      n_arm1 = c(20, 20)
    )

  t_s <-
    e_s |>
    group_events(
      t_groups = t_groups,
      n_event = "n_event_arm1",
      n_arm = "n_arm1",
      event_name = "event_name",
      study_id = "study_id"
    )

  true_t_s <-
    dplyr::tibble(
      src = c(1, 2),
      study_id = "NCT1",
      term_name = c("myocardial_infarction",
                    "skin"),
      n_event_arm1 = c(2, 15),
      n_arm1 = c(20, 20),
      details_n_event_arm1 =
        c("myocardial infarction (2)",
          "eczema (15)")
    )

  attr(true_t_s, "group_method") <- "conservative"

  expect_equal(t_s, true_t_s)
})

test_that("you can pass multiple event and arm columns, whatever the names", {
  source_order <- c("src1", "src2")

  t_data <-
    e_data |>
    dplyr::mutate(
      number_arm2 =
        c(10, 1, 8, 16, 13, 8, 10),
      number_patient_arm2 =
        c(30, 30, 31, 31,  31, 31, 49)
    ) |>
    sort_sources(
      method = c("source_list"),
      source_name = "source",
      source_list_order = source_order
    ) |>
    group_events(
      t_groups = t_groups,
      n_event = c("n_event_arm1", "number_arm2"),
      n_arm     = c("n_arm1", "number_patient_arm2"),
      event_name = "event_name",
      study_id   = "study_id",
      method = "conservative"
    )

  # conservative
  true_t_data <-
    dplyr::tibble(
      src = c(1, 1, 1, 2),
      study_id = c("NCT1", "NCT2",
                   "NCT2", "NCT3"),
      term_name = c("myocardial_infarction", "myocardial_infarction",
                    "skin", "skin"),
      n_event_arm1 = c(2, 5, 10, 20),
      n_arm1  = c(20, 27, 27, 50),
      number_arm2 = c(10, 8, 16, 10),
      number_patient_arm2 = c(30, 31, 31, 49),
      details_n_event_arm1 =
        c("myocardial infarction (1), acute coronary syndrome (2)",
          "myocardial infarction (5)",
          "eczema (10)",
          "pruritus (20)"),
      details_number_arm2 =
        c("myocardial infarction (10), acute coronary syndrome (1)",
          "myocardial infarction (8)",
          "eczema (16)",
          "pruritus (10)"
        )
    )

  attr(true_t_data, "group_method") <- "conservative"

  expect_equal(t_data, true_t_data)
  })

test_that("you can work without n_arms", {

  source_order <- c("src1", "src2")

  t_data <-
    e_data |>
    dplyr::select(-dplyr::all_of("n_arm1")) |>
    sort_sources(
      method = c("source_list"),
      source_name = "source",
      source_list_order = source_order
    ) |>
    group_events(
      t_groups = t_groups,
      n_event = c("n_event_arm1"),
      event_name = "event_name",
      study_id   = "study_id",
      method = "conservative"
    )

  # conservative
  true_t_data <-
    dplyr::tibble(
      src = c(1, 1, 1, 2),
      study_id = c("NCT1", "NCT2",
                   "NCT2", "NCT3"),
      term_name = c("myocardial_infarction", "myocardial_infarction",
                    "skin", "skin"),
      n_event_arm1 = c(2, 5, 10, 20),
      details_n_event_arm1 =
        c("myocardial infarction (1), acute coronary syndrome (2)",
          "myocardial infarction (5)",
          "eczema (10)",
          "pruritus (20)")

    )

  attr(true_t_data, "group_method") <- "conservative"

  expect_equal(t_data, true_t_data)
})

test_that("study_id cannot be missing", {
  source_order <- c("src1", "src2")

    expect_error({
      e_data |>
        sort_sources(
          method = c("source_list"),
          source_name = "source",
          source_list_order = source_order
        ) |>
        dplyr::mutate(
          stud_id_error =
            ifelse(
              study_id == "NCT1",
              NA_character_,
              study_id
            )
        ) |>
        group_events(
          t_groups = t_groups,
          n_event = "n_event_arm1",
          n_arm     = "n_arm1",
          event_name = "event_name",
          study_id   = "stud_id_error",
          method = "conservative"
        )},
      "missing values are not allowed in study_id (stud_id_error)",
      fixed = TRUE
    )
})
