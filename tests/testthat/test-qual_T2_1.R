## Test Setup
kri_workflow <- suppressMessages(MakeWorkflowList(
  strNames = "kri0001",
  strPath = GetYamlPathMetrics()
))[["kri0001"]]

outputs <- qual_step_outputs(kri_workflow)

## Test Code
testthat::test_that("Qual: Given raw participant-level data, a properly specified Workflow for a KRI creates summarized and flagged data (#68)", {
  test <- qual_run_workflow(kri_workflow, mapped_data)
  expected <- qual_double_program_counts(lData, strGroupCol = "invid")
  expected_rows <- length(stats::na.omit(unique(
    mapped_data$Mapped_SUBJ[[kri_workflow$steps[[2]]$params$strGroupCol]]
  )))

  # test output structure
  expect_true(all(outputs %in% names(test)))
  expect_true(is.vector(test$vThreshold))
  expect_true(all(purrr::map_lgl(
    test[grep("^Analysis_", outputs, value = TRUE)],
    is.data.frame
  )))
  expect_equal(nrow(test$Analysis_Flagged), expected_rows)
  expect_equal(nrow(test$Analysis_Summary), expected_rows)

  # test output content: group-level counts match double-programmed values
  transformed <- dplyr::arrange(test$Analysis_Transformed, GroupID)
  expect_identical(transformed$GroupID, expected$GroupID)
  expect_equal(transformed$Numerator, expected$Numerator)
  expect_equal(transformed$Denominator, expected$Denominator)
  expect_equal(transformed$Metric, expected$Metric)

  # test output content: flags match hard-coded threshold logic
  flags <- test$Analysis_Flagged %>%
    dplyr::mutate(
      hardcode_flag = qual_hardcode_flag_magnitude(Score, test$vThreshold) *
        sign(Score)
    ) %>%
    dplyr::left_join(
      test$Analysis_Summary %>% dplyr::select("GroupID", "Flag"),
      by = "GroupID"
    ) %>%
    dplyr::mutate(
      hardcode_summary_flag = dplyr::if_else(
        Denominator < kri_workflow$meta$AccrualThreshold,
        NA_real_,
        hardcode_flag
      )
    )

  expect_identical(flags$hardcode_flag, flags$Flag.x)
  expect_identical(flags$hardcode_summary_flag, flags$Flag.y)
})
