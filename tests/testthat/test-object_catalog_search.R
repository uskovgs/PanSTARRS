
test_that("Is list `.meta` actual?", {
  expect_equal(ps1_metadata("mean", "dr2"), .meta[["mean_dr2"]])
  expect_equal(ps1_metadata("mean", "dr1"), .meta[["mean_dr1"]])
  expect_equal(ps1_metadata("stack", "dr2"), .meta[["stack_dr2"]])
  expect_equal(ps1_metadata("stack", "dr1"), .meta[["stack_dr1"]])
  expect_equal(ps1_metadata("detection", "dr2"), .meta[["detection_dr2"]])
  expect_equal(ps1_metadata("forced_mean", "dr2"), .meta[["forced_mean_dr2"]])
})
