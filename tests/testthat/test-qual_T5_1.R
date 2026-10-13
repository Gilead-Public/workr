# Test Setup -------------------------------------------------------
ae_workflow_custom <- suppressMessages(MakeWorkflowList(
  "kri0001_custom",
  strPath = GetYamlPathCustomMetrics()
))[["kri0001_custom"]]
ae_workflow_default <- suppressMessages(MakeWorkflowList(
  strNames = "kri0001",
  strPath = GetYamlPathMetrics()
))[["kri0001"]]

# define Data ------------------------------------------------------
test_custom <- qual_run_workflow(ae_workflow_custom, mapped_data)
test_default <- qual_run_workflow(ae_workflow_default, mapped_data)

hardcode_flag <- function(test, nAccrualThreshold) {
  test$Analysis_Flagged %>%
    dplyr::mutate(
      hardcode_flag = dplyr::if_else(
        Denominator < nAccrualThreshold,
        NA_real_,
        qual_hardcode_flag_magnitude(Score, test$vThreshold)
      )
    ) %>%
    dplyr::left_join(
      test$Analysis_Summary %>% dplyr::select(GroupID, SummaryFlag = Flag),
      by = "GroupID"
    )
}

hardcode_flag_custom <- hardcode_flag(
  test_custom,
  ae_workflow_custom$meta$AccrualThreshold
)
hardcode_flag_default <- hardcode_flag(
  test_default,
  ae_workflow_default$meta$AccrualThreshold
)

## define outputs --------------------------------------------------
outputs <- qual_step_outputs(ae_workflow_custom)

## Test Code -------------------------------------------------------
testthat::test_that("Qual: Given appropriate raw participant-level data, flag values can be correctly assigned to records that meet flagging criteria, including custom thresholding (#68)", {
  # Custom vThreshold
  expect_true(all(outputs %in% names(test_custom)))
  expect_true(is.vector(test_custom[["vThreshold"]]))
  expect_true(all(purrr::map_lgl(
    test_custom[outputs[outputs != "vThreshold"]],
    is.data.frame
  )))
  expect_equal(
    nrow(test_custom$Analysis_Flagged),
    nrow(test_custom$Analysis_Summary)
  )
  expect_identical(
    sort(test_custom$Analysis_Flagged$GroupID),
    sort(test_custom$Analysis_Summary$GroupID)
  )
  expect_identical(
    abs(hardcode_flag_custom$SummaryFlag),
    hardcode_flag_custom$hardcode_flag
  )

  # default vThreshold
  expect_true(all(outputs %in% names(test_default)))
  expect_true(is.vector(test_default[["vThreshold"]]))
  expect_true(all(purrr::map_lgl(
    test_default[outputs[outputs != "vThreshold"]],
    is.data.frame
  )))
  expect_equal(
    nrow(test_default$Analysis_Flagged),
    nrow(test_default$Analysis_Summary)
  )
  expect_identical(
    sort(test_default$Analysis_Flagged$GroupID),
    sort(test_default$Analysis_Summary$GroupID)
  )
  expect_identical(
    abs(hardcode_flag_default$SummaryFlag),
    hardcode_flag_default$hardcode_flag
  )

  # thresholds change the flags but not the underlying scores
  scores <- dplyr::inner_join(
    dplyr::select(test_default$Analysis_Analyzed, GroupID, Score),
    dplyr::select(test_custom$Analysis_Analyzed, GroupID, Score),
    by = "GroupID"
  )
  expect_equal(scores$Score.x, scores$Score.y)
  expect_false(identical(
    dplyr::arrange(test_default$Analysis_Flagged, GroupID)$Flag,
    dplyr::arrange(test_custom$Analysis_Flagged, GroupID)$Flag
  ))
})
