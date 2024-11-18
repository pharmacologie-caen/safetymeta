test_that("you can extract main and subgroup results from metarate", {

  data <-
    data.frame(
      event = 4:1,
      time = c(10, 20, 30, 40),
      n = c(5, 7, 8, 9),
      subgroup = c("a", "a", "b", "b")
    )

  m1 <- meta::metarate(
    event = event,
    time = time,
    # n = n,
    subgroup = subgroup,
    data = data
  )

  res_list <-
    table_mod_summary(
      mod_list = list(mod = m1),
      main_or_sbgp = "main",
      common_or_random = "random",
      prop_scaler = 100
    )

  res_true <-
    data.frame(
      `analysis` = "main outcome",
      `mod_name` = "mod",
      `event` = 10,
      `time` = 100,
      `n` = 0L,
      `n_stud_i2` = "4/64%",
      `sm_95` = "12.10 (4.05-36.18)",
      `per` = "100 person.time",
      `method_sm_effects` = "Inverse IRLN random",
      `TE` = 0.121024058783155,
      `lower` = 0.0404776874724543,
      `upper` = 0.361849298192145,
      `k` = 4L,
      `I2` = 0.640621406121013,
      `Q` = 8.34774260653429,
      `pval.Q` = 0.039345892002307
    ) |>
    dplyr::rename(c("Rate (95%CI)" = "sm_95", "n stud/I2" = "n_stud_i2"))

  res_unique <-
    table_mod_summary(
      m1,
      main_or_sbgp = "main",
      common_or_random = "random",
      prop_scaler = 100
    )

  res_sbgp <-
    table_mod_summary(
      mod_list = list(mod = m1),
      main_or_sbgp = "sbgp",
      common_or_random = "random",
      prop_scaler = 100
    )

  res_sbgp_unique <-
    table_mod_summary(
      mod_list = m1,
      main_or_sbgp = "sbgp",
      common_or_random = "random",
      prop_scaler = 100
    )

  res_list_common <-
    table_mod_summary(
      mod_list = list(mod = m1),
      main_or_sbgp = "main",
      common_or_random = "common",
      prop_scaler = 100
    )

  expect_equal(res_list, res_true)

  expect_equal(res_list,
               res_unique)
  expect_equal(res_sbgp,
               res_sbgp_unique)

  expect_equal(res_list[["event"]], sum(data[["event"]]))

  expect_equal(nrow(res_list), 1)
  expect_equal(nrow(res_sbgp), 2)

  expect_equal(nrow(res_list_common), 1)

  expect_false(res_list$`Rate (95%CI)` ==
                 res_list_common$`Rate (95%CI)`)

})

test_that("raise a warning if NAs in time in metarate", {
  d1 <-
    data.frame(
      e = c(115,
            60 ,
            8  ,
            2  ,
            21 ,
            0),
      t = c(3605.0417,
            5682.0000,
            301.0000,
            NA,
            635.9500,
            14.5475)
    )

  m1 <- meta::metarate(
    e, t, data = d1
  )

  res <-

  expect_warning(
    table_mod_summary(m1),
    "NAs in event and/or time"
  )

})

test_that("you can extract main and subgroup results from metabin", {

  data <-
    data.frame(
      event.e = 4:1,
      event.c = 2:5,

      n.e = c(5, 7, 8, 9),
      n.c = c(9, 10, 11, 12),
      subgroup = c("a", "a", "b", "b")
    )

  m1 <- meta::metabin(
    event.e = event.e,
    event.c = event.c,
    n.e = n.e,
    n.c = n.c,
    subgroup = subgroup,
    data = data
  )

  res_list <-
    table_mod_summary(
      mod_list = list(mod = m1),
      main_or_sbgp = "main",
      common_or_random = "random",
      prop_scaler = 100
    )

  res_unique <-
    table_mod_summary(
      m1,
      main_or_sbgp = "main",
      common_or_random = "random",
      prop_scaler = 100
    )

  res_sbgp <-
    table_mod_summary(
      mod_list = list(mod = m1),
      main_or_sbgp = "sbgp",
      common_or_random = "random",
      prop_scaler = 100
    )

  res_sbgp_unique <-
    table_mod_summary(
      mod_list = m1,
      main_or_sbgp = "sbgp",
      common_or_random = "random",
      prop_scaler = 100
    )

  res_list_common <-
    table_mod_summary(
      mod_list = list(mod = m1),
      main_or_sbgp = "main",
      common_or_random = "common",
      prop_scaler = 100
    )

  expect_equal(res_list,
               res_unique)
  expect_equal(res_sbgp,
               res_sbgp_unique)

  expect_equal(res_list[["event.e"]], sum(data[["event.e"]]))

  expect_equal(nrow(res_list), 1)
  expect_equal(nrow(res_sbgp), 2)

  expect_equal(nrow(res_list_common), 1)

  expect_false(res_list$`sm (95%CI)` ==
                 res_list_common$`sm (95%CI)`)

})

test_that("you can extract main and subgroup results from metaprop", {

  data <-
    data.frame(
      event = 4:1,
      n = c(5, 7, 8, 9),
      subgroup = c("a", "a", "b", "b")
    )

  m1 <- meta::metaprop(
    event = event,
    n = n,
    subgroup = subgroup,
    data = data
  )

  res_list <-
    table_mod_summary(
      mod_list = list(mod = m1),
      main_or_sbgp = "main",
      common_or_random = "random",
      prop_scaler = 100
    )

  res_unique <-
    table_mod_summary(
      m1,
      main_or_sbgp = "main",
      common_or_random = "random",
      prop_scaler = 100
    )

  res_sbgp <-
    table_mod_summary(
      mod_list = list(mod = m1),
      main_or_sbgp = "sbgp",
      common_or_random = "random",
      prop_scaler = 100
    )

  res_sbgp_unique <-
    table_mod_summary(
      mod_list = m1,
      main_or_sbgp = "sbgp",
      common_or_random = "random",
      prop_scaler = 100
    )

  res_list_common <-
    table_mod_summary(
      mod_list = list(mod = m1),
      main_or_sbgp = "main",
      common_or_random = "common",
      prop_scaler = 100
    )

  expect_equal(res_list,
               res_unique)
  expect_equal(res_sbgp,
               res_sbgp_unique)

  expect_equal(res_list[["event"]], sum(data[["event"]]))

  expect_equal(nrow(res_list), 1)
  expect_equal(nrow(res_sbgp), 2)

  expect_equal(nrow(res_list_common), 1)

  expect_false(res_list$`Proportion (95%CI)` ==
                 res_list_common$`Proportion (95%CI)`)

})

test_that("A NULL element in the model list doesnt crash the function", {

  data <-
    data.frame(
      event = 4:1,
      n = c(5, 7, 8, 9),
      subgroup = c("a", "a", "b", "b")
    )

  m1 <- meta::metaprop(
    event = event,
    n = n,
    subgroup = subgroup,
    data = data
  )

  res_mod <-
    table_mod_summary(
      list(mod1 = m1)
    )

  mod_list <- list(mod1 = m1, modnull = NULL)

  mod_list2 <- list(modnull = NULL, mod1 = m1) # even if its the first mod?

  res_mod2 <- table_mod_summary(
    mod_list
  )

  res_mod3 <- table_mod_summary(
    mod_list2
  )

  expect_equal(
    res_mod,
    res_mod2
  )
  expect_equal(
    res_mod,
    res_mod3
  )

})

test_that("a list in list of models doesn't work", {
  # this is to avoid handling NULL elements that far in the hierarchy
  data <-
    data.frame(
      event = 4:1,
      n = c(5, 7, 8, 9),
      subgroup = c("a", "a", "b", "b")
    )

  m1 <- meta::metaprop(
    event = event,
    n = n,
    subgroup = subgroup,
    data = data
  )

  list_in_a_list <-
    list(mod1_head =
           list(mod1 = m1),
         mod2_head =
           list(mod2 = NULL)
         )

  expect_error(
    table_mod_summary(
      list_in_a_list
      ),
    "Nested lists in lists are no longer allowed."
  )

})

test_that("you cannot mix metaprop and metabin models in the same list", {

  data <-
    data.frame(
      event.e = 4:1,
      event.c = 2:5,

      n.e = c(5, 7, 8, 9),
      n.c = c(9, 10, 11, 12),
      subgroup = c("a", "a", "b", "b")
    )

  m1 <- meta::metabin(
    event.e = event.e,
    event.c = event.c,
    n.e = n.e,
    n.c = n.c,
    subgroup = subgroup,
    data = data
  )

  m2 <- meta::metaprop(
    event = event.e,
    n = n.e,
    subgroup = subgroup,
    data = data
  )

  expect_error(
    table_mod_summary(
      list(m1, m2)
      ),
    "All models should be the same type"
  )

})

test_that("metaprop breaks if sm is not plogit",{
  data <-
    data.frame(
      event = 4:1,
      n = c(5, 7, 8, 9)
    )

  m1 <- meta::metaprop(
    event = event,
    n = n,
    data = data,
    sm = "PAS"
  )

  expect_error(
    table_mod_summary(
      mod_list = m1,
      main_or_sbgp = "sbgp",
      common_or_random = "random",
      prop_scaler = 100
    ),
    "metaprop table is only possible with sm='PLOGIT'"
  )

})

test_that("models with different subgroups and main_or_sbgp is main is warning",{
  data <-
    data.frame(
      event.e = 4:1,
      event.c = 2:5,

      n.e = c(5, 7, 8, 9),
      n.c = c(9, 10, 11, 12),
      subgroup = c("a", "a", "b", "b"),
      subgroup2 = c("a", "b", "b", "b")
    )

  m1 <- meta::metabin(
    event.e = event.e,
    event.c = event.c,
    n.e = n.e,
    n.c = n.c,
    subgroup = subgroup,
    data = data
  )

  m2 <- meta::metabin(
    event.e = event.e,
    event.c = event.c,
    n.e = n.e,
    n.c = n.c,
    subgroup = subgroup2,
    data = data
  )

  # also, the two models have the same name (quite unlikely scenario)
  expect_warning(
    table_mod_summary(
      mod_list = list(mod = m1,
                      mod = m2),
      main_or_sbgp = "main",
      common_or_random = "random",
      prop_scaler = 100
    ),
    "Did you intend to collect subgroups results, with main_or_sbgp = 'sbgp'?"
  )
})

test_that("you can extract common or random summaries", {
  m1 <-
    tnsc_data |>
    dplyr::filter(term_name == "myocardial_infarction") |>
    metabin_wrap(
      event_colnames = c("event_e", "event_c"),
      n_colnames = c("n_e", "n_c"),
      sub_group = c("byvar1"),
      stud_lab = "study_id"
    )

  m_rate <-
    meta::metarate(
      event = event_e,
      time = n_e,
      data = tnsc_data,
      subgroup = byvar1
    )

  m_prop <-
    meta::metaprop(
      event = event_e,
      n = n_e,
      data= tnsc_data,
      subgroup = byvar1
    )

  # names
  for (m_ in list(m_rate, m_prop, m1)) {
    for (an_ in c("sbgp", "main")) {
      res_common <-
        table_mod_summary(
          mod_list = m_,
          main_or_sbgp = an_,
          common_or_random = "common"
        )

      res_random <-
        table_mod_summary(
          mod_list = m_,
          main_or_sbgp = an_,
          common_or_random = "random"
        )

      expect_false(any(grepl("common", names(res_random))))

      expect_false(any(grepl("random", names(res_common))))
    }
  }

  # values
  # last model from for loop is metabin, main outcome
  expect_false(
    res_common$pval.common ==
      res_random$pval.random
  )

  expect_false(
    res_common$`sm (95%CI)` ==
      res_random$`sm (95%CI)`
  )
})

test_that("with metarate, return wald test line with method GLMM", {
  data <-
    data.frame(
      event = 4:1,
      time = c(10, 20, 30, 40),
      n = c(5, 7, 8, 9),
      subgroup = c("a", "a", "b", "b")
    )

  m1 <- meta::metarate(
    event = event,
    time = time,
    subgroup = subgroup,
    data = data,
    method = "GLMM"
  )

  res_list <-
    table_mod_summary(
      mod_list = list(mod = m1),
      main_or_sbgp = "main",
      common_or_random = "random",
      prop_scaler = 100
    )

  res_true <-
    data.frame(
      `analysis` = "main outcome",
      `mod_name` = "mod",
      `event` = 10,
      `time` = 100,
      `n` = 0L,
      `n_stud_i2` = "4/64%",
      `sm_95` = "10.03 (3.74-26.93)",
      `per` = "100 person.time",
      `method_sm_effects` = "GLMM IRLN random",
      `TE` = 0.100335303,
      `lower` = 0.037384984,
      `upper` = 0.269283869,
      `k` = 4L,
      `I2` = 0.640622871,
      `Q` = 8.34777662,
      `pval.Q` = 0.039345288
    ) |>
    dplyr::rename(c("Rate (95%CI)" = "sm_95", "n stud/I2" = "n_stud_i2"))

  expect_equal(res_list, res_true)
})
