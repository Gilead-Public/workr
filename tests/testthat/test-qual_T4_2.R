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

## Test Code -------------------------------------------------------
testthat::test_that("Qual: Given appropriate metadata (i.e. vThresholds), bounds are properly applied to generate flags (#68)", {
  # default ---------------------------------
  test <- purrr::map(kri_workflows, ~ qual_run_workflow(.x, mapped_data))
  check_outputs(test, outputs)
  check_thresholds(test, kri_workflows)

  # custom ----------------------------------
  test_custom <- purrr::map(kri_custom, ~ qual_run_workflow(.x, mapped_data))
  check_outputs(test_custom, outputs_custom)
  check_thresholds(test_custom, kri_custom)

  # verify vThreshold was properly applied to data to assign flags
  purrr::walk(c(test, test_custom), function(kri) {
    output <- kri$Analysis_Flagged %>%
      dplyr::mutate(
        hardcode_flag = qual_hardcode_flag_magnitude(Score, kri$vThreshold)
      )
    expect_identical(abs(output$Flag), output$hardcode_flag)
  })

  # custom thresholds are tighter than the defaults, so they flag at least as
  # many groups
  purrr::walk2(test, test_custom, function(default, custom) {
    expect_gte(
      sum(abs(custom$Analysis_Flagged$Flag)),
      sum(abs(default$Analysis_Flagged$Flag))
    )
  })
})
