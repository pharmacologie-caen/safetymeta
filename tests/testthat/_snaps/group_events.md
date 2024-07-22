# term count exceeding n_arm with integrative method produces an error

    Code
      expect_error(group_events(e_data, t_groups = t_groups, n_event = "n_event_arm1",
        n_arm = "n_arm1", event_name = "event_name", study_id = "study_id", method = "integrative"),
      "integrative method yielded inappropriate counts. Use conservative method")
    Output
      # A tibble: 1 x 6
      # Groups:   src, s___id [1]
          src s___id term_name             details_n___event_1   n___event_1 n___arm_1
        <int> <chr>  <chr>                 <chr>                       <dbl>     <dbl>
      1     1 NCT1   myocardial_infarction myocardial infarctio~          21        20

