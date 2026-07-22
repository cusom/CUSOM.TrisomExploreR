repo_root <- normalizePath(testthat::test_path("..", ".."), winslash = "/", mustWork = TRUE)
old_wd <- setwd(repo_root)
on.exit(setwd(old_wd), add = TRUE)

source(file.path("app", "logic", "app_resources", "catalog_services.R"), local = TRUE)
source(file.path("app", "logic", "app_resources", "app_configs.R"), local = TRUE)

test_that("resolver accepts directory-backed package artifacts", {
  resolver <- LocalPackageResolver$new("app/app_config/packages")
  fact_path <- resolver$resolve_fact_file("PAX_polyaRNA_P4C071320", "transcriptomics")

  expect_true(!is.null(fact_path) && nzchar(fact_path))
  expect_true(fact_path %in% c("facts", "facts/measurements.parquet"))

  expect_identical(
    resolver$resolve_precalculated_artifact("PAX_polyaRNA_P4C071320", "linear_model", "age"),
    "statistics"
  )
})

test_that("shared local artifact reader loads partitioned parquet datasets", {
  package_root <- file.path(repo_root, "app", "app_config", "packages", "PAX_polyaRNA_P4C071320")

  fact_data <- read_local_artifact(file.path(package_root, "facts"))
  stat_data <- read_local_artifact(file.path(package_root, "statistics"))
  age_stat_data <- read_local_artifact(
    file.path(package_root, "statistics"),
    feature_id = "age"
  )

  expect_false(is.null(fact_data))
  expect_gt(nrow(fact_data), 0)

  expect_false(is.null(stat_data))
  expect_gt(nrow(stat_data), 0)
  expect_true("feature" %in% names(stat_data))

  expect_false(is.null(age_stat_data))
  expect_gt(nrow(age_stat_data), 0)
  expect_true("feature" %in% names(age_stat_data))
  expect_true(all(tolower(as.character(age_stat_data$feature)) == "age"))
})