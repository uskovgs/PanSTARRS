## code to prepare `DATASET` dataset goes here

library(panstarrs)
df_meta <- expand.grid(
  table = c("mean", "stack", "detection", "forced_mean"),
  release = c("dr2", "dr1"),
  stringsAsFactors = FALSE
)
# Cmd + Shift + L
df_meta <- df_meta[order(df_meta$table), ]
df_meta
mean_dr2        <- ps1_metadata("mean", "dr2")
mean_dr1        <- ps1_metadata("mean", "dr1")
stack_dr2       <- ps1_metadata("stack", "dr2")
stack_dr1       <- ps1_metadata("stack", "dr1")
detection_dr2   <- ps1_metadata("detection", "dr2")
forced_mean_dr2 <- ps1_metadata("forced_mean", "dr2")


.meta <- list(
  mean_dr2 = mean_dr2,
  mean_dr1 = mean_dr1,
  stack_dr2 = stack_dr2,
  stack_dr1 = stack_dr1,
  detection_dr2 = detection_dr2,
  forced_mean_dr2 = forced_mean_dr2
)

.meta |> str(1)



usethis::use_data(.meta, internal = TRUE, overwrite = TRUE)

