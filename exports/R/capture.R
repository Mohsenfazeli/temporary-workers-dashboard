# =============================================================================
# exports/R/capture.R: pictures of the dashboard's own visuals for the
# Downloads PDFs, taken at build time from a temporary local copy of the app
# (headless Chrome through shinytest2), so they always show the data the PDFs
# are built from. Filters and periods come from the data (q2_fig_ranges()),
# never from a hard-coded release.
#   capture_note_figures(): the Downloads note's figures (body and Appendix C)
#   capture_overview():     the Overview tab with numbered labels (How to use)
# Only the display is changed for a capture (sidebar hidden, plot height set,
# plotly toolbar hidden); the app code and data are not modified.
# =============================================================================

exports_app_copy <- function(project = ".") {
  app_dir <- file.path(tempdir(), paste0("exports_app_", as.integer(stats::runif(1, 1, 1e6))))
  dir.create(app_dir)
  for (f in c("app.R", "R", "www", "data")) file.copy(file.path(project, f), app_dir, recursive = TRUE)
  unlink(file.path(app_dir, "www", "exports"), recursive = TRUE)   # not needed for the pictures
  app_dir
}

exports_start_app <- function(app_dir, width, name) {
  Sys.unsetenv(c("TW_LOCAL_DIR", "TW_DATA_SOURCE", "GEMINI_API_KEY"))   # default data; no AI
  Sys.setenv(NOT_CRAN = "true")
  options(chromote.timeout = 180)
  shinytest2::AppDriver$new(app_dir, width = width, height = 900, load_timeout = 240000,
                            timeout = 60000, name = name, seed = 1)
}

capture_note_figures <- function(project, out_dir, bundle) {
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  P <- q2_periods(bundle); R <- q2_fig_ranges(P); F <- q2_fig_files
  first_year <- as.integer(format(P$first, "%Y")); first_m <- as.integer(format(P$first, "%m"))
  app <- exports_start_app(exports_app_copy(project), 980, "exports_q2")
  on.exit(try(app$stop(), silent = TRUE), add = TRUE)
  cs <- app$get_chromote_session()
  status <- gsub("\\s+", " ", app$get_text(".status-bar"))
  if (!grepl(bundle$meta$release_phrase, status, fixed = TRUE))
    stop("The local app does not show the expected data (", bundle$meta$release_phrase, "): ", status)
  W <- 980
  cs$Emulation$setDeviceMetricsOverride(width = W, height = 2600, deviceScaleFactor = 2, mobile = FALSE)
  Sys.sleep(2)
  app$run_js("var st=document.createElement('style'); st.innerHTML='.modebar{display:none !important;} html{scrollbar-width:none;} ::-webkit-scrollbar{display:none;}'; document.head.appendChild(st);")
  capture <- function(css_id, path) {
    r <- unlist(app$get_js(sprintf("(function(){var b=document.getElementById('%s').getBoundingClientRect();
           return [b.left + window.scrollX, b.top + window.scrollY, b.width, b.height];})()", css_id)))
    res <- cs$Page$captureScreenshot(format = "png", captureBeyondViewport = FALSE,
                                     clip = list(x = r[1], y = r[2], width = r[3], height = r[4], scale = 1))
    writeBin(jsonlite::base64_dec(res$data), path)
  }
  month_pos <- function(lab) { d <- as.Date(paste("01", lab), "%d %b %Y")
    (as.integer(format(d, "%Y")) - first_year) * 12 + as.integer(format(d, "%m")) - first_m }
  set_filters <- function(nav, geography, program = "Both", resolution, range, same = FALSE, measure = "share") {
    app$set_inputs(nav = nav, wait_ = FALSE)
    app$set_inputs(geography = geography, program = program, resolution = resolution, wait_ = FALSE)
    if (resolution == "Annual") app$set_inputs(same_months = same && R$same, wait_ = FALSE)
    app$run_js(sprintf("$('#range').data('ionRangeSlider').update({from: %d, to: %d});",
                       month_pos(range[1]), month_pos(range[2])))
    app$set_inputs(`composition-measure` = measure, wait_ = FALSE)
    app$wait_for_idle(3000, timeout = 120000); Sys.sleep(3)
    got <- unname(unlist(app$get_value(input = "range")))
    if (!identical(got, range) || !identical(app$get_value(input = "geography"), geography))
      stop("Filters not applied for ", nav, ": ", paste(got, collapse = " to "))
  }
  set_sidebar <- function(open) {
    if (open) app$run_js("var e=document.getElementById('hide-sb'); if(e) e.remove(); window.dispatchEvent(new Event('resize'));")
    else app$run_js("var st=document.createElement('style'); st.id='hide-sb';
        st.innerHTML='.bslib-sidebar-layout > .sidebar, .bslib-sidebar-layout > .collapse-toggle {display:none !important;} .bslib-sidebar-layout {display:block !important;}';
        document.head.appendChild(st); window.dispatchEvent(new Event('resize'));")
    Sys.sleep(3)
  }
  shot_card <- function(el_id, file, height = NULL, grow = FALSE) {
    set_sidebar(FALSE)
    app$run_js(sprintf("var p=document.getElementById('%s'); var c=p.closest('.card');
      document.querySelectorAll('#shot-target').forEach(function(e){e.removeAttribute('id');}); c.id='shot-target';
      %s %s window.scrollTo(0,0);", el_id,
      if (!grow) "" else "c.style.height='auto'; c.style.maxHeight='none'; c.style.flex='none';
        c.querySelectorAll('.card-body').forEach(function(b){b.style.overflow='visible'; b.style.maxHeight='none'; b.style.height='auto'; b.style.flex='none';});
        c.querySelectorAll('.dataTables_scrollBody, .html-widget, .datatables').forEach(function(b){b.style.maxHeight='none'; b.style.height='auto'; b.style.overflow='visible';});
        c.querySelectorAll('.card-body').forEach(function(b){b.style.paddingBottom='12px';});",
      if (is.null(height)) "" else sprintf("p.style.height='%dpx'; Plotly.relayout(p,{height:%d});", height, height)))
    Sys.sleep(3)
    capture("shot-target", file.path(out_dir, file))
    set_sidebar(TRUE)
  }
  set_filters("Overview", "Canada", resolution = "Annual", range = R$full, same = TRUE)
  shot_card("overview-headline_plot", F[["f1"]], 320)
  set_filters("Composition", "Canada", resolution = "Annual", range = R$y21, same = TRUE, measure = "number")
  shot_card("composition-share_imp_plot", F[["c3"]], 420)
  set_filters("Geography", "British Columbia", resolution = "Annual", range = R$full)
  shot_card("geography-contrib_plot", F[["f2"]], 335)
  set_filters("Overview", "Canada", resolution = "Annual", range = R$full, same = FALSE)
  shot_card("overview-headline_plot", F[["c1"]], 320)
  set_filters("Overview", "Canada", resolution = "Annual", range = R$last12)
  set_sidebar(FALSE)
  cs$Emulation$setDeviceMetricsOverride(width = 1300, height = 2600, deviceScaleFactor = 2, mobile = FALSE)
  app$run_js("window.dispatchEvent(new Event('resize'));"); Sys.sleep(3)
  app$run_js("window.scrollTo(0,0); document.querySelector('.kpi-row').id='kpi-target';"); Sys.sleep(1)
  capture("kpi-target", file.path(out_dir, F[["c2"]]))
  kpi_txt <- gsub("\\s+", " ", trimws(app$get_text(".kpi-row")))
  ytd_cur <- sum(bundle$data$value[bundle$data$geography == "Canada" & bundle$data$year == P$Y])
  if (!grepl(fmt_n(ytd_cur), kpi_txt, fixed = TRUE)) stop("Headline cards do not show ", fmt_n(ytd_cur), ": ", kpi_txt)
  cs$Emulation$setDeviceMetricsOverride(width = W, height = 2600, deviceScaleFactor = 2, mobile = FALSE)
  set_sidebar(TRUE)
  set_filters("Composition", "British Columbia", resolution = "Annual", range = R$y21, same = TRUE, measure = "number")
  shot_card("composition-share_imp_plot", F[["c4"]], 420)
  set_filters("Geography", "British Columbia", program = "TFWP", resolution = "Annual", range = R$y19)
  shot_card("geography-compare_plot", F[["c5"]], 280)
  set_filters("Composition", "British Columbia", resolution = "Annual", range = R$y21, same = TRUE, measure = "number")
  shot_card("composition-share_tfwp_plot", F[["c6"]], 360)
  set_filters("Composition", "British Columbia", resolution = "Monthly", range = R$y22, measure = "number")
  shot_card("composition-share_tfwp_plot", F[["c7"]], NULL)
  set_filters("Geography", "British Columbia", resolution = "Annual", range = R$ytd_prev)
  shot_card("geography-rank_table", F[["c8"]], NULL, grow = TRUE)
  logs <- app$get_logs(); errs <- logs[logs$level %in% "error", ]
  if (nrow(errs)) stop("The app logged errors while capturing figures.")
  missing <- F[!file.exists(file.path(out_dir, F))]
  if (length(missing)) stop("Figures not saved: ", paste(missing, collapse = ", "))
  invisible(file.path(out_dir, F))
}

#' Overview tab at 1600 x 900 (2x pixels), cropped and labelled 1-5 for the
#' how-to guide: 1 filters, 2 tabs, 3 headline cards, 4 chart tools, 5 notes.
capture_overview <- function(project, path) {
  app <- exports_start_app(exports_app_copy(project), 1600, "exports_overview")
  on.exit(try(app$stop(), silent = TRUE), add = TRUE)
  cs <- app$get_chromote_session()
  app$wait_for_idle(3000); Sys.sleep(3)
  cs$Emulation$setDeviceMetricsOverride(width = 1600, height = 900, deviceScaleFactor = 2, mobile = FALSE); Sys.sleep(3)
  pr <- unlist(app$get_js("(function(){var b=document.getElementById('overview-headline_plot').getBoundingClientRect(); return [b.right, b.top];})()"))
  cs$Input$dispatchMouseEvent(type = "mouseMoved", x = pr[1] - 140, y = pr[2] + 12); Sys.sleep(1.5)   # shows the chart tools, no tooltip
  pos <- app$get_js("(function(){
    function r(e){ if(!e) return null; var b=e.getBoundingClientRect(); return [b.left,b.top,b.right,b.bottom]; }
    function byText(sel, txt){ return Array.prototype.find.call(document.querySelectorAll(sel), function(e){ return e.textContent.trim().indexOf(txt) === 0; }); }
    var title = document.querySelector('.sidebar-title') || byText('.sidebar h2, .sidebar h3, .sidebar header', 'Filters (apply');
    var links = document.querySelectorAll('.navbar-nav .nav-link'); var last = links[links.length - 1];
    var card = document.querySelector('.kpi-row .bslib-value-box, .kpi-row > *');
    var cam = document.querySelector('#overview-headline_plot .modebar-btn');
    var plot = document.getElementById('overview-headline_plot');
    var sel = byText('.nav-link', 'For your selection');
    var range = document.createRange(); if (title) range.selectNodeContents(title);
    return {title: title ? r(range) : null, last: r(last), card: r(card), cam: r(cam), plot: r(plot), sel: r(sel)};
  })()")
  res <- cs$Page$captureScreenshot(format = "png")
  raw <- tempfile(fileext = ".png"); writeBin(jsonlite::base64_dec(res$data), raw)
  css_h <- 522 / 0.64                                    # crop height for the how-to guide
  img <- magick::image_read(raw)
  img <- magick::image_crop(img, magick::geometry_area(3200, round(css_h * 2), 0, 0))
  img <- magick::image_resize(img, "2000x")
  k <- 2000 / 1600                                       # css px -> output px
  num <- function(p) as.numeric(unlist(p))
  badges <- list()
  add <- function(n, x, y) badges[[length(badges) + 1]] <<- c(n, x * k, y * k)
  t <- num(pos$title); if (length(t) == 4) add(1, t[3] + 24, (t[2] + t[4]) / 2) else add(1, 275, 116)
  l <- num(pos$last); add(2, l[3] + 14, (l[2] + l[4]) / 2)
  cd <- num(pos$card); add(3, cd[1] + 20, cd[2] + 20)
  cm <- num(pos$cam); pl <- num(pos$plot)
  if (length(cm) == 4) add(4, cm[1] - 22, (cm[2] + cm[4]) / 2) else add(4, pl[3] - 60, pl[2] + 12)
  s <- num(pos$sel); add(5, s[3] + 16, (s[2] + s[4]) / 2)
  img <- magick::image_draw(img)
  for (b in badges) {
    graphics::symbols(b[2], b[3], circles = 21, inches = FALSE, add = TRUE, bg = "#FCBA19", fg = "#1A1A1A", lwd = 2.5)
    graphics::text(b[2], b[3], labels = b[1], col = "#1A1A1A", font = 2, cex = 2.2)
  }
  grDevices::dev.off()
  magick::image_write(magick::image_convert(img, "jpg"), path, quality = 88)
  invisible(path)
}
