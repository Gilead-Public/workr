## Qualification step functions
##
## The qualification workflows in `qual_workflows/` need some step functions
## to run. They are deliberately small and domain-neutral: the qualification
## suite tests the workr runtime, not any analytics. (The KRI analytics, e.g.
## threshold parsing and flagging, belong to gsm.core and are qualified there.)
## Everything else in the workflows uses `workr::RunQuery` or base R.
##
## `RunStep()` resolves bare function names from the global environment, so
## each step function is assigned there at the bottom of this file.

# Sum event counts and exposure per group, and compute the event rate. Rows
# with a missing group or exposure are dropped with a warning; a missing count
# (a subject with no events) counts as zero.
qual_rate_by_group <- function(
  dfInput,
  strGroupCol,
  strCountCol,
  strExposureCol
) {
  df <- data.frame(
    GroupID = dfInput[[strGroupCol]],
    Count = dfInput[[strCountCol]],
    Exposure = dfInput[[strExposureCol]]
  )
  df$Count[is.na(df$Count)] <- 0

  incomplete <- is.na(df$GroupID) | is.na(df$Exposure)
  if (any(incomplete)) {
    warning(
      sum(incomplete),
      " row(s) with a missing group or exposure removed.",
      call. = FALSE
    )
  }
  df <- df[!incomplete, , drop = FALSE]

  out <- stats::aggregate(
    cbind(Numerator = Count, Denominator = Exposure) ~ GroupID,
    data = df,
    FUN = sum
  )
  out$Rate <- out$Numerator / out$Denominator
  out <- out[order(out$GroupID), , drop = FALSE]
  rownames(out) <- NULL
  out
}

# Mark groups whose total exposure is below a minimum.
qual_mark_low_exposure <- function(dfInput, nMinExposure) {
  dfInput$LowExposure <- dfInput$Denominator < nMinExposure
  dfInput
}

# Register step functions so RunStep() can resolve them by name ---------------
for (fn_name in c("qual_rate_by_group", "qual_mark_low_exposure")) {
  assign(fn_name, get(fn_name), envir = .GlobalEnv)
}
rm(fn_name)
