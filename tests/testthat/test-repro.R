# The reproduction package: rebuilding the data from source and the launchers.

test_that("the rebuild script reproduces the shipped release from the Sep 15 files and leaves data/ alone", {
  skip_if_not(have_testdata, "Local IRCC test files not found in testdata/")
  tmp <- withr::local_tempdir()
  for (d in c("R", "data", "tools")) file.copy(file.path(PROJECT, d), tmp, recursive = TRUE)
  dir.create(file.path(tmp, "raw_profile"))
  file.copy(unname(LOCAL_CORE), file.path(tmp, "raw_profile"))
  before <- tools::md5sum(file.path(tmp, "data", c("baseline.rds", "cache.rds")))
  out <- withr::with_dir(tmp, system2(file.path(R.home("bin"), "Rscript"),
                                      c("tools/build_cache_and_profile.R", "--local"), stdout = TRUE, stderr = TRUE))
  expect_null(attr(out, "status"))
  expect_true(any(grepl("all checks passed", out)))
  expect_true(any(grepl("RESULT: the rebuilt data are identical to data/baseline.rds", out, fixed = TRUE)))
  expect_true(any(grepl("data/ was not changed", out, fixed = TRUE)))
  expect_equal(unname(tools::md5sum(file.path(tmp, "data", c("baseline.rds", "cache.rds")))), unname(before))
  expect_true(file.exists(file.path(tmp, "raw_profile", "rebuilt.rds")))
})

test_that("the rebuild script reports a different release instead of replacing the data", {
  skip_if_not(have_testdata, "Local IRCC test files not found in testdata/")
  tmp <- withr::local_tempdir()
  for (d in c("R", "data", "tools")) file.copy(file.path(PROJECT, d), tmp, recursive = TRUE)
  dir.create(file.path(tmp, "raw_profile"))
  # A changed release: one TFWP value in 2016 is raised by 100 (simulated, test only; a small revision that passes the baseline check).
  tf <- readLines(LOCAL_CORE[["TFWP"]], encoding = "UTF-8")
  i <- grep("^2016\t", tf)[1]; f <- strsplit(tf[i], "\t")[[1]]
  v <- suppressWarnings(as.numeric(f[length(f)])); if (is.na(v)) v <- 0
  f[length(f)] <- as.character(v + 100); tf[i] <- paste(f, collapse = "\t")
  writeLines(tf, file.path(tmp, "raw_profile", basename(LOCAL_CORE[["TFWP"]])), useBytes = TRUE)
  file.copy(LOCAL_CORE[["IMP"]], file.path(tmp, "raw_profile"))
  before <- tools::md5sum(file.path(tmp, "data", "baseline.rds"))
  out <- withr::with_dir(tmp, system2(file.path(R.home("bin"), "Rscript"),
                                      c("tools/build_cache_and_profile.R", "--local"), stdout = TRUE, stderr = TRUE))
  expect_true(any(grepl("different IRCC release than the shipped one", out, fixed = TRUE)))
  expect_true(any(grepl("Baseline check of settled months: passed", out, fixed = TRUE)))
  expect_equal(unname(tools::md5sum(file.path(tmp, "data", "baseline.rds"))), unname(before))
})

test_that("the launchers exist, use the bundled R and library, and never install packages", {
  bat <- readBin(file.path(PROJECT, "run_dashboard.bat"), "raw", 1e5)
  expect_true(all(c(as.raw(13), as.raw(10)) %in% bat))       # Windows line endings
  b <- readLines(file.path(PROJECT, "run_dashboard.bat"))
  expect_true(any(grepl("R-portable\\bin\\Rscript.exe", b, fixed = TRUE)))
  expect_true(any(grepl("--vanilla tools\\run_local.R %*", b, fixed = TRUE)))
  r <- readLines(file.path(PROJECT, "tools", "run_local.R"))
  code <- r[!grepl("^\\s*#", r)]
  expect_false(any(grepl("^[^\"]*install\\.packages\\(", code)))   # only inside the help message
  expect_true(any(grepl(".libPaths(lib)", code, fixed = TRUE)))
  expect_true(any(grepl('TW_DATA_SOURCE = "live"', code, fixed = TRUE)))
  expect_true(file.exists(file.path(PROJECT, "rebuild_data.bat")))
})
