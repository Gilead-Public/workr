# Test Setup -------------------------------------------------------
kri_workflows <- suppressMessages(MakeWorkflowList(
  c(sprintf("kri%04d", 1:2), sprintf("cou%04d", 1:2)),
  strPath = GetYamlPathMetrics()
))
kri_custom <- suppressMessages(MakeWorkflowList(
  c(sprintf("kri%04d_custom", 1:2), sprintf("cou%04d_custom", 1:2)),
  strPath = GetYamlPathCustomMetrics()
))

outputs <- purrr::map(kri_workflows, qual_step_outputs)
outputs_custom <- purrr::map(kri_custom, qual_step_outputs)

# verify each workflow's outputs are exported and analysis outputs are data
# frames
check_outputs <- function(test, outputs) {
  purrr::iwalk(test, ~ expect_true(all(outputs[[.y]] %in% names(.x))))
  expect_true(
    all(
      purrr::imap_lgl(test, function(kri, kri_name) {
        all(purrr::map_lgl(
          kri[grep("^Analysis_", outputs[[kri_name]], value = TRUE)],
          is.data.frame
        ))
      })
    )
  )
}

# verify vThreshold was converted to a numeric threshold vector of length 4
# matching the workflow metadata
check_thresholds <- function(test, workflows) {
  purrr::iwalk(test, function(kri, kri_name) {
    expect_true(is.vector(kri$vThreshold) && length(kri$vThreshold) == 4)
    expect_identical(
      kri$vThreshold,
      as.numeric(strsplit(workflows[[kri_name]]$meta$Threshold, ",")[[1]])
    )
  })
}

# verify each workflow's flagging step, whichever model it uses, received the
# parsed thresholds
check_flag_step <- function(test, workflows) {
  purrr::iwalk(test, function(kri, kri_name) {
    flag_step <- purrr::detect(
      workflows[[kri_name]]$steps,
      ~ .x$output == "Analysis_Flagged"
    )
    expect_identical(
      kri$Analysis_Flagged,
      get(flag_step$name)(kri$Analysis_Analyzed, threshold = kri$vThreshold)
    )
  })
}

## Test Code -------------------------------------------------------
testthat::test_that("Qual: Given appropriate metadata (i.e. vThresholds), each workflow's thresholds are parsed and passed to its flagging step (#96, #68)", {
  # default ---------------------------------
  test <- purrr::map(kri_workflows, ~ qual_run_workflow(.x, mapped_data))
  check_outputs(test, outputs)
  check_thresholds(test, kri_workflows)
  check_flag_step(test, kri_workflows)

  # custom ----------------------------------
  test_custom <- purrr::map(kri_custom, ~ qual_run_workflow(.x, mapped_data))
  check_outputs(test_custom, outputs_custom)
  check_thresholds(test_custom, kri_custom)
  check_flag_step(test_custom, kri_custom)
})
