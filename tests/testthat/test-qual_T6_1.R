## Test Setup
metric_workflows <- suppressMessages(MakeWorkflowList(
  c(sprintf("kri%04d", 1:2), sprintf("cou%04d", 1:2)),
  strPath = GetYamlPathMetrics()
))

## Test Code
testthat::test_that("Qual: Given raw data and a list of mapping and metric workflows, RunWorkflows runs them in order and passes earlier results to later workflows (#96, #68)", {
  test <- suppressMessages(RunWorkflows(c(mappings_wf, metric_workflows), lData))

  # one result per workflow, named Type_ID, in run order
  expect_identical(
    names(test),
    c(
      paste0("Mapped_", names(mappings_wf)),
      paste0("Analysis_", names(metric_workflows))
    )
  )

  # mapping results are the mapped data frames, matching an independent
  # derivation from raw data
  expected_subj <- lData$Raw_SUBJ %>%
    dplyr::filter(enrollyn == "Y") %>%
    dplyr::select(subjid, invid, country, timeonstudy)
  expect_equal(
    dplyr::arrange(test$Mapped_SUBJ, subjid),
    dplyr::arrange(expected_subj, subjid),
    ignore_attr = TRUE
  )
  expect_equal(nrow(test$Mapped_AE), nrow(lData$Raw_AE))

  # metric results are the final step output of each workflow, identical to
  # running each workflow on its own against the mapped data
  purrr::iwalk(metric_workflows, function(workflow, id) {
    expect_identical(
      test[[paste0("Analysis_", id)]],
      suppressMessages(RunWorkflow(workflow, mapped_data))
    )
  })

  # serious adverse event metrics see only serious events
  sae <- qual_double_program_counts(lData, strGroupCol = "invid", bSeriousOnly = TRUE)
  kri0002 <- dplyr::arrange(test$Analysis_kri0002, GroupID)
  expect_identical(kri0002$GroupID, sae$GroupID)
  expect_equal(kri0002$Numerator, sae$Numerator)
})
