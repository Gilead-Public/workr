## Test Setup
kri_workflow <- suppressMessages(MakeWorkflowList(
  "kri0001_custom",
  strPath = GetYamlPathCustomMetrics()
))[["kri0001_custom"]]
outputs <- qual_step_outputs(kri_workflow)

## Test Code
testthat::test_that("Qual: Given appropriate metadata (i.e. vThresholds), flagged observations are properly marked in summary data (#68)", {
  test <- qual_run_workflow(kri_workflow, mapped_data)
  expect_true(all(outputs %in% names(test)))
  expect_true(is.vector(test[["vThreshold"]]))
  expect_true(all(purrr::map_lgl(
    test[grep("^Analysis_", outputs, value = TRUE)],
    is.data.frame
  )))
  expect_equal(nrow(test$Analysis_Flagged), nrow(test$Analysis_Summary))
  expect_identical(
    sort(test$Analysis_Flagged$GroupID),
    sort(test$Analysis_Summary$GroupID)
  )

  # the threshold vector comes from the workflow metadata
  expect_identical(
    test$vThreshold,
    as.numeric(strsplit(kri_workflow$meta$Threshold, ",")[[1]])
  )

  # flag magnitudes in the summary match hard-coded threshold logic for every
  # group that meets the accrual threshold
  flags <- test$Analysis_Summary %>%
    dplyr::filter(!is.na(Flag)) %>%
    dplyr::mutate(
      flagged_hardcode = qual_hardcode_flag_magnitude(Score, test$vThreshold)
    )

  expect_gt(nrow(flags), 0)
  expect_identical(abs(flags$Flag), flags$flagged_hardcode)
  expect_true(all(sign(flags$Flag[flags$Flag != 0]) == sign(flags$Score[flags$Flag != 0])))
})
