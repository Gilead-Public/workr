## Test Setup
metric_workflows <- suppressMessages(MakeWorkflowList(
  strPath = GetYamlPathMetrics()
))

## Test Code
testthat::test_that("Qual: Given a project directory of workflow phases, RunProject runs the phases in order and makes each phase's results available to later phases (#63, #67)", {
  test <- suppressMessages(RunProject(
    GetYamlPathQualWorkflows(),
    lData = lData,
    strPhases = c("1_mappings", "2_metrics")
  ))

  expect_identical(names(test), c("1_mappings", "2_metrics"))

  # the mapping phase reproduces the mapped data
  expect_identical(test[["1_mappings"]], mapped_data)

  # the metrics phase consumes the mapping phase's results and gives the same
  # results as running the metric workflows directly against mapped data;
  # inactive workflows are skipped
  expect_identical(
    test[["2_metrics"]],
    suppressMessages(RunWorkflows(metric_workflows, mapped_data))
  )
  expect_false("Analysis_kri0099" %in% names(test[["2_metrics"]]))
})
