repo_root <- normalizePath(testthat::test_path("..", ".."), winslash = "/", mustWork = TRUE)
old_wd <- setwd(repo_root)
on.exit(setwd(old_wd), add = TRUE)

box::use(
  testthat[...],
)

app_main <- new.env(parent = globalenv())
source(file.path("app", "main.R"), local = app_main)

test_that("main module loads", {
  expect_true(is.function(app_main$ui))
  expect_true(is.function(app_main$server))
})
