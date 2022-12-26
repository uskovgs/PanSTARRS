## code to prepare `DATASET` dataset goes here

library(panstarrs)
df_meta <- expand.grid(table = c("mean", "stack", "detection", "forced_mean"),
            release = c("dr2", "dr1"))

df_meta <- df_meta[order(df_meta$table), ]
df_meta
mean_dr2        <- ps1_metadata(df_meta$table[1], df_meta$release[1])
mean_dr1        <- ps1_metadata(df_meta$table[2], df_meta$release[2])
stack_dr2       <- ps1_metadata(df_meta$table[3], df_meta$release[3])
stack_dr1       <- ps1_metadata(df_meta$table[4], df_meta$release[4])
detection_dr2   <- ps1_metadata(df_meta$table[5], df_meta$release[5])
forced_mean_dr2 <- ps1_metadata(df_meta$table[7], df_meta$release[7])


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

