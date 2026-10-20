# =============================================================================
# R/mod_help.R: Help tab ("Ask about this dashboard", see R/assistant.R)
# =============================================================================
required_packages <- c("shiny", "bslib", "htmltools")
missing_packages  <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_packages) > 0) {
  install.packages(missing_packages, repos = "https://cloud.r-project.org")
}

HELP_EXAMPLES <- c("What does IMP mean?", "How do I see British Columbia only?",
                   "How many permits were issued from January to July 2026?")

help_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::card(
    class = "help-card", fill = FALSE,
    bslib::card_header(htmltools::tags$h2(class = "chart-title", "Ask about this dashboard")),
    bslib::card_body(fillable = FALSE,
      htmltools::tags$p(class = "help-intro",
        "Ask how to use the dashboard, what a term means or what the data show. ",
        "Answers use only this dashboard's text and figures."),
      shiny::uiOutput(ns("status")),
      shiny::textAreaInput(ns("question"), "Your question", width = "100%", rows = 2,
                           placeholder = "For example: What does IMP mean?"),
      htmltools::tags$script(htmltools::HTML(sprintf(
        "document.getElementById('%s').setAttribute('maxlength', %d);", ns("question"), ASSISTANT_MAX_CHARS))),
      htmltools::div(class = "d-flex flex-wrap gap-2 align-items-center mb-2",
        shiny::actionButton(ns("ask"), "Ask", icon = shiny::icon("comment-dots"), class = "btn-primary btn-sm"),
        htmltools::tags$span(class = "help-examples-label", "Try:"),
        lapply(seq_along(HELP_EXAMPLES), function(i)
          shiny::actionLink(ns(paste0("ex", i)), HELP_EXAMPLES[i], class = "help-example"))),
      htmltools::div(class = "help-answer", role = "status", `aria-live` = "polite",
        shiny::uiOutput(ns("answer"))),
      # While the server waits for the answer, say so (replaced by the answer)
      htmltools::tags$script(htmltools::HTML(sprintf(
        "$(document).on('click', '#%s', function() { if (document.getElementById('%s').value.trim()) $('#%s').html('<p class=\"help-wait\">Writing an answer\u2026</p>'); });",
        ns("ask"), ns("question"), ns("answer")))),
      htmltools::tags$p(class = "source-line help-note", ASSISTANT_NOTE,
        " Your question is sent to Google's Gemini service to write the answer; please do not include personal information."),
      shiny::textOutput(ns("left"), inline = TRUE) |> htmltools::tagAppendAttributes(class = "source-line")
    )
  )
}

#' @param sel the shared filter selection (reactive), sent with each question
#' @param system_prompt the instructions and context (built once at startup)
#' @param ask function(question, system_prompt, history, config) -> list(ok, text);
#'   tests pass a mock so they never call the real API
help_server <- function(id, sel, system_prompt, config = assistant_config(), ask = ask_assistant) {
  shiny::moduleServer(id, function(input, output, session) {
    n_asked <- shiny::reactiveVal(0L)
    answer  <- shiny::reactiveVal(NULL)
    history <- list()            # last two questions and answers, for follow-ups

    output$status <- shiny::renderUI({
      if (!config$has_key) htmltools::div(class = "alert alert-secondary py-2 px-3 small", ASSISTANT_MESSAGES$no_key)
    })
    output$left <- shiny::renderText({
      if (!config$has_key) return("")
      sprintf("%d of %d questions left for this visit.", max(config$session_max - n_asked(), 0L), config$session_max)
    })

    lapply(seq_along(HELP_EXAMPLES), function(i) shiny::observeEvent(input[[paste0("ex", i)]],
      shiny::updateTextAreaInput(session, "question", value = HELP_EXAMPLES[i])))

    shiny::observeEvent(input$ask, {
      q <- trimws(input$question %||% "")
      if (!config$has_key) { answer(list(ok = FALSE, q = q, n = input$ask, text = ASSISTANT_MESSAGES$no_key)); return() }
      problem <- check_question(q, n_asked(), config$session_max, daily_count(0), config$daily_max)
      if (!is.null(problem)) { answer(list(ok = FALSE, q = q, n = input$ask, text = ASSISTANT_MESSAGES[[problem]])); return() }
      n_asked(n_asked() + 1L); daily_count(1)
      s <- tryCatch(sel(), error = function(e) NULL)
      res <- ask(paste(q, selection_text(s)), system_prompt, history, config)
      if (isTRUE(res$ok)) history <<- utils::tail(c(history, list(list(q = q, a = res$text))), 2)
      answer(c(res, q = q, n = input$ask))
    })

    output$answer <- shiny::renderUI({
      a <- answer()
      if (is.null(a)) return(NULL)
      htmltools::tagList(
        if (nzchar(a$q %||% "")) htmltools::tags$p(class = "help-q", htmltools::tags$strong("You asked: "), a$q),
        if (isTRUE(a$ok)) htmltools::div(class = "help-a", answer_html(a$text))
        else htmltools::div(class = "alert alert-warning py-2 px-3 small mb-0", a$text))
    })
  })
}
