## Test Setup
workflow <- suppressMessages(MakeWorkflowList(
  strNames = I("ae_site"),
  strPath = GetYamlPathMetrics()
))[["ae_site"]]

## Test Code
testthat::test_that("Qual: Given raw data with missing values, the workflows carry the missing values through SQL steps and surface step warnings to the caller (#96, #68)", {
  mapped_data_missing_values <- suppressMessages(
    RunWorkflows(mappings_wf, lData_missing_values)
  )

  # missing values survive the SQL mapping unchanged
  expected_subj <- lData_missing_values$Raw_SUBJ %>%
    dplyr::filter(enrollyn == "Y") %>%
    dplyr::select(subjid, invid, country, timeonstudy)
  expect_equal(
    dplyr::arrange(mapped_data_missing_values$Mapped_SUBJ, subjid),
    dplyr::arrange(expected_subj, subjid),
    ignore_attr = TRUE
  )
  expect_true(anyNA(mapped_data_missing_values$Mapped_SUBJ$invid))

  # a warning raised inside a step reaches the caller
  expect_warning(
    test <- qual_run_workflow(workflow, mapped_data_missing_values),
    "missing group or exposure removed"
  )

  # results match counts recomputed with dplyr from the same raw data
  expected <- qual_double_program_counts(
    lData_missing_values,
    strGroupCol = "invid"
  )
  expect_identical(test$Group_Summary$GroupID, expected$GroupID)
  expect_equal(test$Group_Summary$Numerator, expected$Numerator)
  expect_equal(test$Group_Summary$Denominator, expected$Denominator)
})
