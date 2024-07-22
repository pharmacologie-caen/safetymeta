## code to prepare `my_pkg_data` dataset goes here

s_data <-
  data.frame(
    study_id = c("NCT1",
                 "NCT2",
                 "NCT3"),
    n_arm1   = c(20, 27, 50),
    median_age = c(57.5, 70.1, 65.2),
    median_followup = c(24, 18, 30)
  )

e_data_src1 <-
  data.frame(
    source   = "src1",
    study_id = c("NCT1",
                 "NCT1",
                 "NCT2",
                 "NCT2"),
    event_name = c("myocardial infarction",
                   "acute coronary syndrome",
                   "myocardial infarction",
                   "eczema"),
    n_event_arm1 = c(1, 2, 5, 10),
    n_arm1     = c(20, 20, 27, 27)
  )

e_data_src2 <-
  data.frame(
    source       = "src2",
    study_id     = c("NCT2", "NCT2", "NCT3"),
    event_name   = c("eczema", "pruritus", "pruritus"),
    n_event_arm1 = c(9,   7, 20),
    n_arm1       = c(27, 27, 50)
  )

e_data <-
   data.frame(
     source   = c("src1",
                  "src1",
                  "src1",
                  "src1",
                  "src2",
                  "src2",
                  "src2"
                  ),
     study_id = c("NCT1",
                  "NCT1",
                  "NCT2",
                  "NCT2",
                  "NCT2",
                  "NCT2",
                  "NCT3"),
     event_name = c("myocardial infarction",
                    "acute coronary syndrome",
                    "myocardial infarction",
                    "eczema",
                    "eczema",
                    "pruritus",
                    "pruritus"),
     n_event_arm1 = c( 1,  2,  5, 10,  9,  7, 20),
     n_arm1       = c(20, 20, 27, 27, 27, 27, 50)
     )

t_groups <-
  list(myocardial_infarction =
         c("myocardial infarction",
           "sustained st-elevation myocardial infarction",
           "acute coronary syndrome"),
       skin = c("eczema", "pruritus"))


# exporting to .rda files

usethis::use_data(s_data           , compress = "xz", overwrite = TRUE)
usethis::use_data(e_data_src1      , compress = "xz", overwrite = TRUE)
usethis::use_data(e_data_src2      , compress = "xz", overwrite = TRUE)
usethis::use_data(e_data           , compress = "xz", overwrite = TRUE)
usethis::use_data(t_groups         , compress = "xz", overwrite = TRUE)
