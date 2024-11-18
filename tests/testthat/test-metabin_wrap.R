test_that("basic use",{
 # One subgroup

  m1 <-
   tnsc_data |>
   dplyr::filter(term_name == "myocardial_infarction") |>
   metabin_wrap(
     event_colnames = c("event_e", "event_c"),
     n_colnames = c("n_e", "n_c"),
     sub_group = c("byvar1"),
     stud_lab = "study_id"
   )

 m1_true <-
   meta::metabin(
     event.e = event_e,
     n.e     = n_e,
     event.c = event_c,
     n.c     = n_c,
     studlab = study_id,
     subgroup   = byvar1,
     data    = tnsc_data |>
       dplyr::filter(term_name == "myocardial_infarction")
   )

 expect_equal(
   m1$byvar1 |>
     purrr::discard_at(c("call", "subset", "data")),
   m1_true |>
     purrr::discard_at(c("call", "subset", "data"))
   )
 # the 3 last items almost identical, but call implies filtering in metabin_wrap

 # Two subgroups

 m2 <-
   tnsc_data |>
   dplyr::filter(term_name == "myocardial_infarction") |>
   metabin_wrap(
     event_colnames = c("event_e", "event_c"),
     n_colnames = c("n_e", "n_c"),
     sub_group = c("byvar1", "byvar2"),
     stud_lab = "study_id"
   )

 m2_true <-
   meta::metabin(
     event.e = event_e,
     n.e     = n_e,
     event.c = event_c,
     n.c     = n_c,
     studlab = study_id,
     subgroup   = byvar2,
     data    = tnsc_data |>
       dplyr::filter(term_name == "myocardial_infarction")
   )

 expect_equal(
   m2$byvar1 |>
     purrr::discard_at(c("call", "subset", "data")),
   m1_true |>
     purrr::discard_at(c("call", "subset", "data"))
 )

 expect_equal(
   m2$byvar2 |>
     purrr::discard_at(c("call", "subset", "data")),
   m2_true |>
     purrr::discard_at(c("call", "subset", "data"))
 )
})

test_that("no study warning", {

    expect_warning(
      m1 <-
        tnsc_data |>
        dplyr::filter(term_name == "inappropriate name") |>
        metabin_wrap(
          event_colnames = c("event_e", "event_c"),
          n_colnames = c("n_e", "n_c"),
          sub_group = c("byvar1"),
          stud_lab = "study_id"
          ),
      "there was no study to compare event_e to event_c for this outcome"
    )

  expect_equal(m1, list(byvar1 = NULL))
})

test_that("pass additional args to meta::metabin", {
  m1 <-
    tnsc_data |>
    dplyr::filter(term_name == "myocardial_infarction") |>
    metabin_wrap(
      event_colnames = c("event_e", "event_c"),
      n_colnames = c("n_e", "n_c"),
      sub_group = c("byvar1"),
      stud_lab = "study_id",
      method = "Peto",
      sm = "OR"
    )

  m1_true <-
    meta::metabin(
      event.e = event_e,
      n.e     = n_e,
      event.c = event_c,
      n.c     = n_c,
      studlab = study_id,
      subgroup   = byvar1,
      data    = tnsc_data |>
        dplyr::filter(term_name == "myocardial_infarction"),
      method  = "Peto",
      sm      = "OR"
    )

  expect_equal(
    m1$byvar1 |>
      purrr::discard_at(c("call", "subset", "data")),
    m1_true |>
      purrr::discard_at(c("call", "subset", "data"))
  )

})

test_that("works with missing data in byvar", {
  tnsc_data[1, "byvar1"] <- NA_character_

  m1 <-
    tnsc_data |>
    dplyr::filter(term_name == "myocardial_infarction") |>
    metabin_wrap(
      event_colnames = c("event_e", "event_c"),
      n_colnames = c("n_e", "n_c"),
      sub_group = c("byvar1"),
      stud_lab = "study_id"
    )

  m1_true <-
    meta::metabin(
      event.e = event_e,
      n.e     = n_e,
      event.c = event_c,
      n.c     = n_c,
      studlab = study_id,
      subgroup   = byvar1,
      data    = tnsc_data |>
        dplyr::filter(.data$term_name == "myocardial_infarction" &
                        !is.na(.data$byvar1))
    )

  expect_equal(
    m1$byvar1 |>
      purrr::discard_at(c("call", "subset", "data")),
    m1_true |>
      purrr::discard_at(c("call", "subset", "data"))
  )
})
