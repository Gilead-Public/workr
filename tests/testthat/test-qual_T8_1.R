## Test Setup
metric_workflows <- suppressMessages(MakeWorkflowList(
  c("kri0001", "cou0001"),
  strPath = GetYamlPathMetrics()
))

## Test Code
testthat::test_that("Qual: Given registered LoadData and SaveData providers, workflows read their inputs from and write their results to the configured storage (#58)", {
  store <- withr::local_tempdir()
  saveRDS(mapped_data, file.path(store, "mapped_data.rds"))

  register_load_provider(
    "qual.load",
    function(lWorkflow, lConfig, lData) {
      c(lData, readRDS(file.path(lConfig$qual$store, "mapped_data.rds")))
    }
  )
  register_save_provider(
    "qual.save",
    function(lWorkflow, lConfig) {
      saveRDS(
        lWorkflow$lResult,
        file.path(lConfig$qual$store, paste0(lWorkflow$meta$ID, ".rds"))
      )
    }
  )
  lConfig <- list(
    LoadData = "qual.load",
    SaveData = "qual.save",
    qual = list(store = store)
  )

  # no data is passed in directly; the load hook supplies it
  test <- suppressMessages(RunWorkflows(metric_workflows, lConfig = lConfig))
  expected <- suppressMessages(RunWorkflows(metric_workflows, mapped_data))

  expect_identical(test, expected)

  # the save hook persisted each workflow's result
  purrr::iwalk(metric_workflows, function(workflow, id) {
    saved_path <- file.path(store, paste0(id, ".rds"))
    expect_true(file.exists(saved_path))
    expect_identical(readRDS(saved_path), test[[paste0("Analysis_", id)]])
  })
})
