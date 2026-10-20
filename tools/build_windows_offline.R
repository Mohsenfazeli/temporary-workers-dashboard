# =============================================================================
# tools/build_windows_offline.R
# Builds the two folders that make the Windows package run with nothing to
# install: R-portable/ (the official R 4.5.0 for Windows from CRAN, unpacked
# from its installer) and library/ (Windows binaries of every R package the
# dashboard needs, at the exact versions in renv.lock). They go into the zip
# only (deliverables/tools/build_1B.sh); they are never committed to git.
#
#   Rscript tools/build_windows_offline.R <output folder> [<download cache>]
#
# Needs internet and the 'innoextract' program (Linux/macOS; apt/brew install
# innoextract). Package binaries come from Posit Package Manager's dated CRAN
# snapshots, which keep Windows binaries of older versions (CRAN itself keeps
# only the newest). Each file's MD5 is checked against the repository index;
# the R installer against CRAN's published MD5.
# =============================================================================
args  <- commandArgs(trailingOnly = TRUE)
if (length(args) < 1) stop("Usage: Rscript tools/build_windows_offline.R <output folder> [<cache>]")
out   <- normalizePath(args[1], mustWork = FALSE)
cache <- normalizePath(if (length(args) > 1) args[2] else file.path(tempdir(), "win_cache"), mustWork = FALSE)
dir.create(out, recursive = TRUE, showWarnings = FALSE); dir.create(cache, recursive = TRUE, showWarnings = FALSE)
options(timeout = 600)
`%||%` <- function(x, y) if (is.null(x)) y else x

R_VERSION <- "4.5.0"
# Packages the app calls directly (app.R, R/, tools/run_local.R, tools/build_cache_and_profile.R).
ROOTS <- c("shiny", "htmltools", "bslib", "bsicons", "plotly", "DT", "dplyr", "tidyr",
           "readr", "lubridate", "tibble", "curl", "httr2", "jsonlite", "commonmark",
           "shinyWidgets", "shinycssloaders", "scales")
# Dated snapshots searched (in this order) for each package's exact version.
SNAPSHOTS <- c("2025-04-15", "2025-04-01", "2025-03-15", "2025-05-15", "2025-06-15",
               "2025-08-01", "2025-10-01", "2025-12-01", "2026-03-01", "2026-07-01", "2026-10-19")
ppm <- function(d) sprintf("https://packagemanager.posit.co/cran/%s/bin/windows/contrib/4.5", d)

# ---- 1. Exact versions and dependencies from renv.lock ----------------------
lock <- jsonlite::fromJSON("renv.lock", simplifyVector = FALSE)$Packages
deps_of <- function(p) {
  f <- unlist(lock[[p]][c("Depends", "Imports", "LinkingTo")])
  f <- trimws(sub("\\(.*$", "", unlist(strsplit(paste(f, collapse = ","), ","))))
  intersect(f, names(lock))
}
need <- character(); todo <- ROOTS
while (length(todo)) { p <- todo[1]; todo <- todo[-1]
  if (p %in% need) next
  if (!p %in% names(lock)) stop("Not in renv.lock: ", p)
  need <- c(need, p); todo <- c(todo, setdiff(deps_of(p), need)) }
base_pkgs <- c("base","compiler","datasets","grDevices","graphics","grid","methods","parallel",
               "splines","stats","stats4","tcltk","tools","utils")
need <- sort(setdiff(need, base_pkgs))
cat(length(need), "packages needed\n")

# ---- 2. Find and download the Windows binaries ------------------------------
idx <- list()
index_for <- function(d) {
  if (is.null(idx[[d]])) idx[[d]] <<- {
    f <- file.path(cache, paste0("PACKAGES_", d))
    if (!file.exists(f)) utils::download.file(paste0(ppm(d), "/PACKAGES"), f, quiet = TRUE)
    as.data.frame(read.dcf(f, fields = c("Package", "Version", "MD5sum", "License")))
  }
  idx[[d]]
}
lib <- file.path(out, "library"); unlink(lib, recursive = TRUE); dir.create(lib)
found <- do.call(rbind, lapply(need, function(p) {
  v <- lock[[p]]$Version
  for (d in SNAPSHOTS) {
    a <- index_for(d); r <- a[a$Package == p, ]
    if (nrow(r) && package_version(r$Version[1]) == package_version(v)) {
      z <- file.path(cache, sprintf("%s_%s.zip", p, r$Version[1]))
      if (!file.exists(z)) utils::download.file(sprintf("%s/%s_%s.zip", ppm(d), p, r$Version[1]), z,
                                                mode = "wb", quiet = TRUE)
      if (!is.na(r$MD5sum[1]) && unname(tools::md5sum(z)) != r$MD5sum[1]) stop("MD5 mismatch: ", z)
      utils::unzip(z, exdir = lib)
      return(data.frame(package = p, version = v, license = lock[[p]]$License %||% r$License[1],
                        snapshot = d))
    }
  }
  stop("No Windows binary of ", p, " ", v, " in the searched snapshots")
}))
cat("library/:", nrow(found), "packages unpacked\n")

# ---- 3. R for Windows, unpacked from the official installer ------------------
exe <- file.path(cache, sprintf("R-%s-win.exe", R_VERSION))
cran <- sprintf("https://cloud.r-project.org/bin/windows/base/old/%s/", R_VERSION)
if (!file.exists(exe)) utils::download.file(paste0(cran, basename(exe)), exe, mode = "wb", quiet = TRUE)
md5_txt <- readLines(paste0(cran, "md5sum.R-", R_VERSION, ".txt"), warn = FALSE)
if (!any(grepl(unname(tools::md5sum(exe)), md5_txt, fixed = TRUE))) stop("R installer MD5 does not match CRAN's")
tmp <- file.path(cache, "rx"); unlink(tmp, recursive = TRUE)
if (system2("innoextract", c("-s", "-d", shQuote(tmp), shQuote(exe))) != 0) stop("innoextract failed")
rp <- file.path(out, "R-portable"); unlink(rp, recursive = TRUE)
invisible(file.rename(file.path(tmp, "app"), rp))
# Leave out parts the dashboard never uses (PDF manuals, R's own test files and
# the Tcl/Tk runtime). Licence files (COPYING, doc/COPYRIGHTS, share/licenses) stay.
unlink(file.path(rp, c("doc/manual", "tests", "Tcl")), recursive = TRUE)
unlink(Sys.glob(file.path(rp, "library", "*", "tests")), recursive = TRUE)

# ---- 4. Notices ---------------------------------------------------------------
cur <- utils::available.packages(repos = "https://cloud.r-project.org", type = "source", filters = list())
is_cur <- found$version == cur[match(found$package, cur[, "Package"]), "Version"]
found$source <- ifelse(!is.na(is_cur) & is_cur,
  sprintf("https://cran.r-project.org/src/contrib/%s_%s.tar.gz", found$package, found$version),
  sprintf("https://cran.r-project.org/src/contrib/Archive/%s/%s_%s.tar.gz", found$package, found$package, found$version))
utils::write.csv(found, file.path(out, "library", "PACKAGES_BUNDLED.csv"), row.names = FALSE)
writeLines(c(
  "THIRD-PARTY SOFTWARE IN THIS PACKAGE",
  "",
  sprintf("R-portable/ is R %s for Windows, (c) The R Core Team and others, unpacked unchanged", R_VERSION),
  sprintf("from the official CRAN installer %s%s (MD5 %s),", cran, basename(exe), unname(tools::md5sum(exe))),
  "except that the PDF manuals, R's test files and the Tcl/Tk runtime were left out.",
  "R is free software under the GNU General Public License, version 2 or 3 (GPL-2 | GPL-3);",
  "it is redistributed here under those terms. The licence texts are in R-portable/COPYING,",
  "R-portable/doc/COPYRIGHTS and R-portable/share/licenses/. R comes with ABSOLUTELY NO WARRANTY.",
  sprintf("Complete source code: https://cran.r-project.org/src/base/R-4/R-%s.tar.gz", R_VERSION),
  "",
  sprintf("library/ holds %d R packages as Windows binaries (Posit Package Manager snapshots of CRAN),", nrow(found)),
  "each at the version in renv.lock and unchanged. Each package folder keeps its own DESCRIPTION",
  "(authors, licence) and any LICENSE file. Every licence is an open-source licence that allows",
  "redistribution (MIT, BSD, Apache-2.0, MPL-2.0, GPL-2 or later, GPL-3); the list, with licence",
  "and source-code link for each, is library/PACKAGES_BUNDLED.csv. The source code of every package",
  "is on CRAN at the link given (written offer not needed: GPL-3 section 6(d)).",
  "",
  "The dashboard's own code and the processed IRCC data are not covered by these notices;",
  "the IRCC data are published under the Open Government Licence - Canada."),
  file.path(out, "THIRD_PARTY_NOTICES.txt"))
cat("Done:", out, "\n")
