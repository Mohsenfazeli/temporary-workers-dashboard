# Help assistant (R/assistant.R, R/mod_help.R): prompt building, input
# limits, fallbacks and the missing-key case. The API is always mocked.
skip_if_not(have_testdata, "Local IRCC test files not found in testdata/")
d <- BUNDLE$data; meta <- BUNDLE$meta; latest <- meta$latest_month

# A fake API: returns a canned httr2 response and records what was sent
fake_api <- function(status = 200, text = "IMP means International Mobility Program.", body = NULL) {
  calls <- new.env(); calls$reqs <- list()
  f <- function(req) {
    calls$reqs[[length(calls$reqs) + 1]] <- req
    b <- body %||% if (status == 200) list(candidates = list(list(content = list(parts = list(list(text = text))))))
                   else list(error = list(code = status, message = "mock error"))
    httr2::response(status_code = status, headers = list(`Content-Type` = "application/json"),
                    body = charToRaw(as.character(jsonlite::toJSON(b, auto_unbox = TRUE))))
  }
  attr(f, "calls") <- calls
  f
}
cfg <- function(...) utils::modifyList(list(has_key = TRUE, model = "m1", fallback = "m2", timeout = 5,
                                            thinking = "low", session_max = 3L, daily_max = 100L), list(...))

test_that("the system prompt holds the dashboard's own text, figures and rules", {
  sp <- assistant_system_prompt(d, meta)
  # rules
  expect_match(sp, "Answer only from the CONTEXT", fixed = TRUE)
  expect_match(sp, "Never invent", fixed = TRUE)
  expect_match(sp, "Politely decline", fixed = TRUE)
  expect_match(sp, "at most about 120 words", fixed = TRUE)
  # dashboard text: tabs, filters, glossary, About, policy markers, hidden counts
  for (t in c("Overview", "Trends", "Geography", "Composition", "About the data", "Reset filters",
              "Show only Jan\u2013Jul of each year", "International Mobility Program", "Labour Market Impact Assessment",
              "many hidden counts", "Privacy masking and rounding", "How to use", "POLICY MARKERS"))
    expect_match(sp, t, fixed = TRUE)
  for (i in seq_len(nrow(KEY_EVENTS))) expect_match(sp, paste0(i, ". ", KEY_EVENTS$label[i]), fixed = TRUE)
  for (g in GLOSSARY$term) expect_match(sp, paste0("- ", g, ": "), fixed = TRUE)
  # key figures come from the dashboard's own functions
  cards <- overview_cards(d, latest, "Canada", "Both")$cards
  expect_match(sp, cards[[1]]$value, fixed = TRUE)
  expect_match(sp, cards[[2]]$value, fixed = TRUE)
  ann <- summarise_series(filter_selection(d, min(d$month), latest, "Canada", c("TFWP", "IMP")), "Annual",
                          by = character(0), latest_month = latest)
  for (i in seq_len(nrow(ann))) expect_match(sp, paste0(ann$label[i], " | "), fixed = TRUE)
  expect_match(sp, fmt_n(ann$value[ann$label == "2025"]), fixed = TRUE)
  rk <- geo_ranking(d, min(d$month), latest, c("TFWP", "IMP"))
  expect_match(sp, sprintf("1 | %s | %s | %s", rk$geography[1], fmt_n(rk$value[1]), fmt_share(rk$share[1])), fixed = TRUE)
  for (h in release_highlights(d, latest)) expect_match(sp, h, fixed = TRUE)
  # no HTML tags left from the About page
  expect_no_match(sp, "<(p|li|a|div|strong)[ >]")
})

test_that("question checks: empty, too long, per-visit and daily limits", {
  expect_equal(check_question("", 0, 20), "empty")
  expect_equal(check_question("   ", 0, 20), "empty")
  expect_equal(check_question(strrep("a", ASSISTANT_MAX_CHARS + 1), 0, 20), "too_long")
  expect_null(check_question(strrep("a", ASSISTANT_MAX_CHARS), 0, 20))
  expect_equal(check_question("What is IMP?", 20, 20), "session")
  expect_equal(check_question("What is IMP?", 2, 20, daily_used = 300, daily_max = 300), "daily")
  expect_null(check_question("What is IMP?", 19, 20))
  expect_equal(ASSISTANT_MAX_CHARS, 500)
  expect_equal(assistant_config()$session_max, 20L)
  expect_equal(withr::with_envvar(c(TW_GEMINI_MODEL = NA), assistant_config()$model), "gemini-3.5-flash-lite")
  expect_equal(withr::with_envvar(c(TW_GEMINI_MODEL = "gemini-x"), assistant_config()$model), "gemini-x")
})

test_that("a good answer is returned; the key goes in a redacted header, never the URL", {
  api <- fake_api(200, "IMP means **International Mobility Program**.")
  withr::with_envvar(c(GEMINI_API_KEY = "test-key-123"), {
    r <- ask_assistant("What is IMP?", "SYSTEM", list(), cfg(), perform = api)
  })
  expect_true(r$ok); expect_equal(r$model, "m1")
  expect_match(r$text, "International Mobility Program", fixed = TRUE)
  req <- attr(api, "calls")$reqs[[1]]
  expect_match(req$url, "/v1beta/models/m1:generateContent$")
  expect_no_match(req$url, "test-key-123", fixed = TRUE)
  expect_no_match(paste(capture.output(print(req)), collapse = "\n"), "test-key-123", fixed = TRUE)
  expect_equal(req$body$data$systemInstruction$parts[[1]]$text, "SYSTEM")
  expect_equal(req$body$data$contents[[1]]$parts[[1]]$text, "What is IMP?")
  expect_equal(req$body$data$generationConfig$thinkingConfig$thinkingLevel, "low")
  expect_equal(req$options$timeout_ms, 5000)
  # Answers are shown as safe HTML: Markdown rendered, raw HTML escaped
  h <- as.character(answer_html("**Bold** <script>alert(1)</script>"))
  expect_match(h, "<strong>Bold</strong>", fixed = TRUE)
  expect_no_match(h, "<script>", fixed = TRUE)
})

test_that("follow-up questions send the last questions and answers", {
  api <- fake_api(200)
  ask_assistant("And TFWP?", "S", list(list(q = "What is IMP?", a = "IMP is ...")), cfg(), perform = api)
  roles <- vapply(attr(api, "calls")$reqs[[1]]$body$data$contents, `[[`, "", "role")
  expect_equal(roles, c("user", "model", "user"))
})

test_that("fallbacks: missing key, rate limit, busy server, other errors, timeouts, empty answers", {
  # Missing key: no call at all, friendly message
  api <- fake_api(200)
  r <- ask_assistant("What is IMP?", "S", list(), cfg(has_key = FALSE), perform = api)
  expect_false(r$ok); expect_equal(r$text, ASSISTANT_MESSAGES$no_key)
  expect_length(attr(api, "calls")$reqs, 0)
  # Rate limit on both models: the fallback is tried once, then the rate-limit message
  api <- fake_api(429)
  r <- suppressMessages(ask_assistant("q", "S", list(), cfg(), perform = api))
  expect_false(r$ok); expect_equal(r$text, ASSISTANT_MESSAGES$rate_limit)
  expect_length(attr(api, "calls")$reqs, 2)
  # No fallback model set: one call only
  api <- fake_api(429)
  r <- suppressMessages(ask_assistant("q", "S", list(), cfg(fallback = ""), perform = api))
  expect_length(attr(api, "calls")$reqs, 1)
  # First model over its limit, fallback answers
  n <- 0; flip <- function(req) { n <<- n + 1; (if (n == 1) fake_api(429) else fake_api(200, "ok"))(req) }
  r <- suppressMessages(ask_assistant("q", "S", list(), cfg(), perform = flip))
  expect_true(r$ok); expect_equal(r$model, "m2")
  # Busy server (503): the fallback model is tried once
  api <- fake_api(503)
  r <- suppressMessages(ask_assistant("q", "S", list(), cfg(), perform = api))
  expect_false(r$ok); expect_equal(r$text, ASSISTANT_MESSAGES$error)
  urls <- vapply(attr(api, "calls")$reqs, function(q) q$url, "")
  expect_match(urls[1], "/m1:"); expect_match(urls[2], "/m2:")
  # Bad request (400): no fallback, generic message
  api <- fake_api(400)
  r <- suppressMessages(ask_assistant("q", "S", list(), cfg(), perform = api))
  expect_equal(r$text, ASSISTANT_MESSAGES$error); expect_length(attr(api, "calls")$reqs, 1)
  # Timeout or network failure (an error while sending)
  r <- suppressMessages(ask_assistant("q", "S", list(), cfg(), perform = function(req) stop("Timeout was reached")))
  expect_false(r$ok); expect_equal(r$text, ASSISTANT_MESSAGES$error)
  # No text in the answer (e.g. blocked)
  api <- fake_api(200, body = list(candidates = list(list(finishReason = "SAFETY"))))
  r <- suppressMessages(ask_assistant("q", "S", list(), cfg(), perform = api))
  expect_equal(r$text, ASSISTANT_MESSAGES$blocked)
  # The log line names the problem only
  expect_message(ask_assistant("q", "S", list(), cfg(fallback = ""), perform = fake_api(429)),
                 "^\\[assistant\\] rate_limit, status 429, model m1\\s*$")
})

test_that("missing key: the app works without it and nothing is sent", {
  expect_false(withr::with_envvar(c(GEMINI_API_KEY = ""), assistant_config()$has_key))
  sel <- shiny::reactiveVal(list(start = min(d$month), end = latest, resolution = "Annual",
                                 geography = "Canada", program_choice = "Both", programs = c("TFWP", "IMP")))
  called <- FALSE
  shiny::testServer(function(input, output, session)
    help_server("h", sel, "", config = cfg(has_key = FALSE), ask = function(...) { called <<- TRUE; list(ok = TRUE, text = "x") }), {
    expect_match(paste(format(output$`h-status`), collapse = ""), "not available", fixed = TRUE)
    session$setInputs(`h-question` = "What is IMP?", `h-ask` = 1)
    expect_match(paste(format(output$`h-answer`), collapse = ""), "not available", fixed = TRUE)
  })
  expect_false(called)
})

test_that("Help tab: answers, the filters are sent, limits per visit", {
  sel <- shiny::reactiveVal(list(start = min(d$month), end = latest, resolution = "Annual",
                                 geography = "British Columbia", program_choice = "IMP", programs = "IMP"))
  sent <- character()
  mock <- function(q, sp, history, config) { sent <<- c(sent, q); list(ok = TRUE, text = "Use the **Geography** filter.") }
  shiny::testServer(function(input, output, session)
    help_server("h", sel, "SYSTEM", config = cfg(session_max = 2L), ask = mock), {
    expect_match(output$`h-left`, "2 of 2 questions left", fixed = TRUE)
    session$setInputs(`h-question` = "How do I see B.C. only?", `h-ask` = 1)
    a <- paste(format(output$`h-answer`), collapse = "")
    expect_match(a, "<strong>Geography</strong>", fixed = TRUE)
    expect_match(a, "You asked:", fixed = TRUE)
    expect_match(output$`h-left`, "1 of 2 questions left", fixed = TRUE)
    session$setInputs(`h-question` = strrep("x", 501), `h-ask` = 2)
    expect_match(paste(format(output$`h-answer`), collapse = ""), "under 500 characters", fixed = TRUE)
    session$setInputs(`h-question` = "", `h-ask` = 3)
    expect_match(paste(format(output$`h-answer`), collapse = ""), "type a question", fixed = TRUE)
    session$setInputs(`h-question` = "Second?", `h-ask` = 4)
    session$setInputs(`h-question` = "Third?", `h-ask` = 5)
    expect_match(paste(format(output$`h-answer`), collapse = ""), "limit of questions for this visit", fixed = TRUE)
  })
  expect_length(sent, 2)                                   # rejected questions are never sent
  expect_match(sent[1], "British Columbia, IMP", fixed = TRUE)
  expect_match(sent[1], "^How do I see B.C. only\\? \\(The visitor's current filters")
})

test_that("Help tab UI: note, 500-character limit, examples; no assistant text elsewhere", {
  ui <- as.character(htmltools::renderTags(help_ui("help"))$html)
  expect_match(ui, ASSISTANT_NOTE, fixed = TRUE)
  expect_match(ui, "AI answers can be wrong; check the charts and About the data.", fixed = TRUE)
  expect_match(ui, "setAttribute('maxlength', 500)", fixed = TRUE)
  for (e in HELP_EXAMPLES) expect_match(ui, e, fixed = TRUE)
  expect_no_match(ui, "GEMINI_API_KEY", fixed = TRUE)
})
