# =====================================================================
#  CV Builder: a guided academic CV maker for students
#  Ortiz Neuroscience Lab, Department of Psychology, NIU
#
#  HOW TO RUN
#    RStudio: open this file and click "Run App" (top right of the editor)
#    Console: shiny::runApp("path/to/folder/containing/app.R")
#
#  The first run installs any missing packages automatically.
#  Your work autosaves in your browser; use "Save my progress" to keep a
#  file you can load next semester.
# =====================================================================

pkgs <- c("shiny", "bslib", "officer", "flextable", "jsonlite")
need <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
if (length(need)) {
  message("Installing packages the CV Builder needs: ", paste(need, collapse = ", "))
  install.packages(need, repos = "https://cloud.r-project.org")
}
# Make sure dashes and bullets survive on Linux machines set to a non-UTF-8 locale
if (.Platform$OS.type == "unix" && !isTRUE(l10n_info()$`UTF-8`)) invisible(try(Sys.setlocale("LC_CTYPE", "C.UTF-8"), silent = TRUE))
suppressPackageStartupMessages({
  library(shiny); library(bslib); library(officer); library(flextable)
})

APP_VERSION <- "v1.0"
APP_YEAR <- "2026"
PAGE_MARGIN <- 0.5     # page margins in inches (all four sides, both layouts)
CONT_HEADINGS <- TRUE   # add "SECTION (CONT.)" headings when a section continues in a new column or page

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0 || (length(a) == 1 && is.na(a))) b else a
val <- function(e, k) {
  x <- e[[k]]
  if (is.null(x) || length(x) == 0 || is.na(x[1])) "" else trimws(as.character(x[1]))
}
esc <- function(x) htmltools::htmlEscape(x)
THIS_YEAR <- as.integer(format(Sys.Date(), "%Y"))
YEARS <- as.character(seq(THIS_YEAR + 5, 1970))
NOW <- THIS_YEAR + (as.integer(format(Sys.Date(), "%m")) - 1) / 12

# ---------------------------------------------------------------------
# 1. Section definitions: fields, tips, and which career stages use them
# ---------------------------------------------------------------------
fld <- function(name, label, type = "text", placeholder = "", required = FALSE,
                choices = NULL, help = NULL)
  list(name = name, label = label, type = type, placeholder = placeholder,
       required = required, choices = choices, help = help)
DATES <- fld("dates", "Dates", "dates")
BULLETS <- function(ph) fld("details", "Details: one bullet per line", "textarea", ph,
                            help = "Start each bullet with an action verb (performed, analyzed, trained, presented...).")

SECTIONS <- list(
  education = list(
    title = "Education", kind = "dated", stages = c("ug", "grad", "fac"),
    tip = "List your current degree first, even if you haven't finished it: just give the expected graduation month as the end date (it will say \"Expected\"). Include GPA only if it's 3.5 or higher. Honors theses, minors, and advisors go in the details.",
    fields = list(
      fld("title", "Degree", placeholder = "B.S. in Psychology, Minor in Biology", required = TRUE),
      fld("org", "Institution", placeholder = "Northern Illinois University", required = TRUE),
      fld("location", "City, State", placeholder = "DeKalb, IL"),
      DATES,
      BULLETS("GPA: 3.8/4.0\nHonors Thesis: Social buffering in prairie voles (Advisor: Dr. R. J. Ortiz)"))),
  research = list(
    title = "Research Experience", kind = "dated", stages = c("ug", "grad", "fac"),
    tip = "This is the heart of a research CV. Name the lab and the PI, then write 2-4 bullets about what YOU did: techniques, software, how many animals or samples. \"Performed c-Fos immunohistochemistry on 120 brain sections\" is much stronger than \"Helped in the lab.\"",
    fields = list(
      fld("title", "Your position", placeholder = "Undergraduate Research Assistant", required = TRUE),
      fld("org", "Lab and supervisor", placeholder = "Ortiz Neuroscience Lab (PI: Dr. Richard J. Ortiz)", required = TRUE),
      fld("location", "Institution, City", placeholder = "Northern Illinois University, DeKalb, IL"),
      DATES,
      BULLETS("Sectioned prairie vole brains on a vibratome and performed c-Fos immunohistochemistry\nRegistered sections to the Allen Brain Atlas using Fiji/ABBA"))),
  publications = list(
    title = "Publications", kind = "pub", stages = c("grad", "fac"),
    tip = "Formatted in APA 7th edition. Have a DOI? Paste it and click \"Fill in from DOI\" and the citation is filled in for you. Papers that aren't out yet still count: mark them \"In preparation\" or \"Submitted.\" Your name is bolded automatically.",
    fields = list(
      fld("doi", "DOI (optional)", "doi", placeholder = "10.1021/acsmeasuresciau.5c00028"),
      fld("authors", "Authors (APA style, in order)", placeholder = "Rivera, J. A., & Ortiz, R. J.", required = TRUE),
      fld("status", "Status", "select", choices = c("Published", "In press", "Submitted", "In preparation")),
      fld("year", "Year", "year"),
      fld("title", "Article title", placeholder = "Populational differences in prairie vole social behavior: A review", required = TRUE),
      fld("journal", "Journal", placeholder = "Hormones and Behavior"),
      fld("volume", "Volume", placeholder = "152"),
      fld("issue", "Issue", placeholder = "3"),
      fld("pages", "Pages or article number", placeholder = "105-118"))),
  presentations = list(
    title = "Presentations", kind = "pres", stages = c("ug", "grad", "fac"),
    tip = "Posters count! List every poster and talk, including campus events like NIU's Conference on Undergraduate Research and Engagement (CURE). If you won an award, add it in the Award box.",
    fields = list(
      fld("authors", "Authors (APA style, in order)", placeholder = "Martinez, G., Murillo, I., & Ortiz, R. J.", required = TRUE),
      fld("year", "Year", "year", required = TRUE),
      fld("month", "Month", "month"),
      fld("title", "Title", placeholder = "Brain-on-chip: Prairie vole neural calcium imaging", required = TRUE),
      fld("type", "Type", "select", choices = c("Poster presentation", "Oral presentation", "Invited talk", "Symposium talk")),
      fld("event", "Conference or event", placeholder = "Conference on Undergraduate Research and Engagement", required = TRUE),
      fld("location", "Location", placeholder = "Northern Illinois University, DeKalb, IL"),
      fld("award", "Award (optional)", placeholder = "Best Poster Award"))),
  grants = list(
    title = "Grants and Fellowships", kind = "grant", stages = c("grad", "fac"),
    tip = "Grad students and faculty: list unfunded applications too, because writing them is experience. Undergrads: summer fellowships (REU, SURF), travel awards, and research grants go here.",
    fields = list(
      fld("title", "Grant or fellowship name", placeholder = "Summer Program in Neuroscience, Excellence and Success (NIH R25)", required = TRUE),
      fld("org", "Funder or program", placeholder = "Marine Biological Laboratory"),
      fld("year", "Year", "year", required = TRUE),
      fld("status", "Outcome", "select", choices = c("Funded", "Not funded", "Pending")),
      fld("amount", "Amount (optional)", placeholder = "$5,000"),
      BULLETS(""))),
  awards = list(
    title = "Honors and Awards", kind = "award", stages = c("ug", "grad", "fac"),
    tip = "Dean's List, scholarships, poster awards, honor societies (like Psi Chi). Give the year and who awarded it.",
    fields = list(
      fld("title", "Award", placeholder = "Best Poster Award", required = TRUE),
      fld("org", "Awarded by", placeholder = "Psychology Undergraduate Research Conference, NIU"),
      fld("year", "Year", "year", required = TRUE),
      BULLETS(""))),
  teaching = list(
    title = "Teaching Experience", kind = "dated", stages = c("grad", "fac"),
    tip = "TA positions, guest lectures, tutoring, and Supplemental Instruction. Give the course number and say what you did: led labs, wrote quizzes, held review sessions.",
    fields = list(
      fld("title", "Role", placeholder = "Teaching Assistant", required = TRUE),
      fld("org", "Course", placeholder = "PSYC 305: Research Methods", required = TRUE),
      fld("location", "Institution", placeholder = "Northern Illinois University"),
      DATES,
      BULLETS("Led two weekly lab sections (~30 students each)\nWrote and graded quizzes"))),
  mentorship = list(
    title = "Mentoring", kind = "dated", stages = c("fac"),
    tip = "Students you have trained. Note their level and any outcomes, such as a co-authored poster or admission to a PhD program.",
    fields = list(
      fld("title", "Student name", placeholder = "Drew Unrath", required = TRUE),
      fld("org", "Level and program", placeholder = "Undergraduate, Psychology"),
      fld("location", "Outcome (optional)", placeholder = "Best Poster Award, 2025"),
      DATES,
      BULLETS(""))),
  employment = list(
    title = "Work Experience", kind = "dated", stages = c("ug", "grad"),
    tip = "Jobs outside research still show you are reliable. Keep bullets short and focus on skills that transfer: training others, handling data, managing schedules.",
    fields = list(
      fld("title", "Job title", placeholder = "Peer Tutor", required = TRUE),
      fld("org", "Employer", placeholder = "NIU Tutoring Center", required = TRUE),
      fld("location", "City, State", placeholder = "DeKalb, IL"),
      DATES,
      BULLETS("Tutored 10+ students per week in introductory psychology and statistics"))),
  service = list(
    title = "Service and Outreach", kind = "dated", stages = c("ug", "grad", "fac"),
    tip = "Outreach events, student clubs, committees, volunteering, and judging. Officer roles (President, Treasurer) are worth listing.",
    fields = list(
      fld("title", "Role", placeholder = "Treasurer", required = TRUE),
      fld("org", "Organization or event", placeholder = "Student Psychological Association", required = TRUE),
      fld("location", "Institution or city", placeholder = "Northern Illinois University"),
      DATES,
      BULLETS(""))),
  coursework = list(
    title = "Relevant Coursework", kind = "line", stages = c("ug"),
    tip = "Undergrads only: 4-8 upper-level courses related to what you're applying for. Drop this section once you have research experience to show.",
    fields = list(
      fld("title", "Course name", placeholder = "Research Methods in Psychology", required = TRUE),
      fld("org", "Course number (optional)", placeholder = "PSYC 305"),
      fld("year", "Year (optional)", "year"))),
  certifications = list(
    title = "Certifications and Training", kind = "cert", stages = c("ug", "grad"), add_label = "Add certification",
    tip = "Research trainings count: CITI human-subjects or animal-research courses, IACUC and animal-handling training, lab and chemical safety, biosafety, CPR, Mental Health First Aid. You can find completion dates on your CITI \"Records\" page.",
    fields = list(
      fld("title", "Certification or training", placeholder = "CITI Program: Social-Behavioral-Educational Researchers", required = TRUE),
      fld("org", "Provider", placeholder = "CITI Program"),
      fld("year", "Year completed", "year", required = TRUE),
      fld("expires", "Valid through (optional)", "year"))),
  skills = list(
    title = "Skills", kind = "skill", stages = c("ug", "grad", "fac"),
    tip = "Group skills into categories, for example Laboratory (IHC, perfusions, vibratome), Software (R, SPSS, Fiji/ImageJ), Languages (Spanish, fluent). Only list what you could do on day one.",
    fields = list(
      fld("title", "Category", placeholder = "Laboratory", required = TRUE),
      fld("items", "Skills, separated by commas", placeholder = "Immunohistochemistry, vibratome sectioning, brightfield microscopy", required = TRUE))),
  memberships = list(
    title = "Professional Memberships", kind = "dated", stages = c("ug", "grad", "fac"),
    tip = "Societies you belong to, such as Psi Chi, Society for Neuroscience, Chicago Society for Neuroscience, or APS. Student memberships are inexpensive and count.",
    fields = list(
      fld("title", "Organization", placeholder = "Society for Neuroscience", required = TRUE),
      fld("org", "Role (optional)", placeholder = "Student Member"),
      DATES)),
  references = list(
    title = "References", kind = "reference", stages = c("ug", "grad"),
    tip = "Usually three people who know your work, with your PI first. Always ask them before you list them! If you'd rather not list them, turn this section off on the Start tab.",
    fields = list(
      fld("title", "Name", placeholder = "Dr. Richard J. Ortiz", required = TRUE),
      fld("org", "Position", placeholder = "Assistant Professor of Psychology"),
      fld("location", "Institution", placeholder = "Northern Illinois University"),
      fld("email", "Email", placeholder = "rortiz@niu.edu"),
      fld("phone", "Phone (optional)")))
)
SEC_CHOICES <- setNames(names(SECTIONS), vapply(SECTIONS, `[[`, "", "title"))
SORTED_KINDS <- c("dated", "award", "grant", "pub", "pres", "cert")

# Student-made ("custom") sections. Each one is stored as list(id, title, kind),
# where kind is "dated" (title, place, dates, bullets) or "list" (one line per item).
custom_def <- function(cs) {
  if (identical(cs$kind, "list")) list(
    title = cs$title, kind = "simple", stages = character(), custom = TRUE,
    tip = "One line per item. Add a detail or a year if it helps; the arrows set the order.",
    fields = list(fld("title", "Item", placeholder = "Spanish (fluent)", required = TRUE),
                  fld("org", "Detail (optional)", placeholder = "Native speaker; medical interpreting certificate"),
                  fld("year", "Year (optional)", "year")))
  else list(
    title = cs$title, kind = "dated", stages = character(), custom = TRUE,
    tip = "Give each entry a title, where it happened, and the dates. Bullets are optional; start each one with an action verb.",
    fields = list(fld("title", "Title or role", placeholder = "Peer Mentor", required = TRUE),
                  fld("org", "Organization or place", placeholder = "NIU Psychology Department"),
                  fld("location", "City or institution (optional)", placeholder = "DeKalb, IL"),
                  DATES, BULLETS("")))
}
all_defs <- function(custom = list()) {
  d <- SECTIONS
  for (cs in custom %||% list()) d[[cs$id]] <- custom_def(cs)
  d
}
# Order on the CV: built-in sections, then custom sections, with References last
# st$order holds the student's own ordering (set with the arrows on the Add tab).
# Sections not in it yet (newly turned on or newly made) slot in before References.
section_order <- function(st) {
  ids <- c(intersect(setdiff(names(SECTIONS), "references"), st$sections),
           vapply(st$custom %||% list(), `[[`, "", "id"))
  if ("references" %in% st$sections) ids <- c(ids, "references")
  o <- intersect(unlist(st$order) %||% character(), ids)
  new <- setdiff(ids, o)
  if (!length(o)) return(ids)
  if (!length(new)) return(o)
  if (tail(o, 1) == "references") c(head(o, -1), setdiff(new, "references"), "references")
  else c(o, new)
}
stage_defaults <- function(stage) names(SECTIONS)[vapply(SECTIONS, function(s) stage %in% s$stages, TRUE)]

FONTS <- list(
  "Calibri (modern)"          = list(docx = "Calibri",         css = "Calibri, Carlito, 'Segoe UI', Arial, sans-serif"),
  "Times New Roman (classic)" = list(docx = "Times New Roman", css = "'Times New Roman', Tinos, 'Liberation Serif', Times, serif"),
  "Garamond (elegant)"        = list(docx = "Garamond",        css = "Garamond, 'EB Garamond', 'Times New Roman', serif"),
  "Arial (clean)"             = list(docx = "Arial",           css = "Arial, 'Liberation Sans', Helvetica, sans-serif"))
ACCENTS <- c("Black" = "#111111", "Navy" = "#1F3A5F", "Cardinal red" = "#A6192E", "Forest green" = "#2E5E3A")

# ---------------------------------------------------------------------
# 2. Formatting helpers (dates, names, APA)
# ---------------------------------------------------------------------
date_num <- function(m, y) {
  y <- suppressWarnings(as.numeric(y)); if (is.na(y)) return(NA_real_)
  mi <- match(m, month.abb); y + if (is.na(mi)) 0 else (mi - 1) / 12
}
is_current <- function(e) isTRUE(as.logical(e$current %||% FALSE))
date_label <- function(e, sec = "") {
  s <- trimws(paste(val(e, "start_month"), val(e, "start_year")))
  en <- trimws(paste(val(e, "end_month"), val(e, "end_year")))
  if (is_current(e)) en <- "Present"
  else if (sec == "education" && nzchar(en) &&
           isTRUE(date_num(val(e, "end_month"), val(e, "end_year")) > NOW)) return(paste("Expected", en))
  if (nzchar(s) && nzchar(en)) { if (s == en) s else paste0(s, " – ", en) }
  else if (nzchar(s)) s else en
}

R <- function(t, b = FALSE, i = FALSE) list(t = t, b = b, i = i)

name_last <- function(name) {
  n <- trimws(sub(",.*$", "", name %||% ""))
  n <- sub("^(Dr\\.?|Mr\\.?|Ms\\.?|Mrs\\.?|Mx\\.?)\\s+", "", n, ignore.case = TRUE)
  p <- strsplit(n, "\\s+")[[1]]
  if (!length(p) || !nzchar(n)) "" else p[length(p)]
}
# Split an author string into runs, bolding the student's own name
author_runs <- function(txt, last) {
  if (!nzchar(last) || !nzchar(txt)) return(list(R(txt)))
  rx <- paste0("\\b\\Q", last, "\\E(?:,?\\s+[A-Z]\\.?(?:\\s?-?[A-Z]\\.?){0,2})?")
  m <- gregexpr(rx, txt, perl = TRUE, ignore.case = TRUE)[[1]]
  if (m[1] == -1) return(list(R(txt)))
  out <- list(); pos <- 1
  for (k in seq_along(m)) {
    st <- m[k]; ln <- attr(m, "match.length")[k]
    if (st > pos) out[[length(out) + 1]] <- R(substr(txt, pos, st - 1))
    out[[length(out) + 1]] <- R(substr(txt, st, st + ln - 1), b = TRUE)
    pos <- st + ln
  }
  if (pos <= nchar(txt)) out[[length(out) + 1]] <- R(substr(txt, pos, nchar(txt)))
  out
}
end_punct <- function(t) if (grepl("[.?!]$", t)) t else paste0(t, ".")
clean_doi <- function(d) trimws(sub("^(https?://(dx\\.)?doi\\.org/|doi:\\s*)", "", trimws(d %||% ""), ignore.case = TRUE))
bullets_of <- function(e) {
  b <- trimws(strsplit(val(e, "details"), "\n", fixed = TRUE)[[1]])
  b <- sub("^[-*•]\\s*", "", b); b[nzchar(b)]
}

# APA initials, e.g. "Richard Joaquin" -> "R. J.", "Jean-Luc" -> "J.-L."
apa_initials <- function(given) {
  parts <- strsplit(trimws(gsub("\\.", " ", given)), "\\s+")[[1]]
  parts <- parts[nzchar(parts)]
  paste(vapply(parts, function(p) {
    paste0(paste0(substr(strsplit(p, "-")[[1]], 1, 1), "."), collapse = "-")
  }, ""), collapse = " ")
}
apa_author_list <- function(a) {
  n <- length(a); if (n == 0) return("")
  if (n == 1) return(a)
  if (n == 2) return(paste0(a[1], ", & ", a[2]))
  if (n <= 20) return(paste0(paste(a[-n], collapse = ", "), ", & ", a[n]))
  paste0(paste(a[1:19], collapse = ", "), ", ... ", a[n])
}

# Convert a Crossref "message" record into the publication form's fields
crossref_fields <- function(res) {
  au <- vapply(res$author %||% list(), function(a) {
    if (!is.null(a$family)) paste0(a$family, if (!is.null(a$given)) paste0(", ", apa_initials(a$given)) else "") else a$name %||% ""
  }, "")
  dp <- res$issued$`date-parts`[[1]][[1]] %||% res$published$`date-parts`[[1]][[1]] %||% ""
  strip <- function(x) trimws(gsub("\\s+", " ", gsub("<[^>]+>", "", unlist(x)[1] %||% "")))
  list(authors = apa_author_list(au[nzchar(au)]), title = strip(res$title), journal = strip(res$`container-title`),
       volume = as.character(res$volume %||% ""), issue = as.character(res$issue %||% ""),
       pages = as.character(res$page %||% res$`article-number` %||% ""), year = as.character(dp))
}

# ---------------------------------------------------------------------
# 3. Turn each entry into a format-neutral structure:
#    list(lines = list(runs...), right = "dates", bullets = chr)
#    The SAME structure feeds the preview, the PDF, and the Word file.
# ---------------------------------------------------------------------
render_entry <- function(e, sec, last, defs = SECTIONS) {
  kind <- defs[[sec]]$kind
  out <- list(lines = list(), right = "", bullets = character())
  switch(kind,
    dated = {
      out$lines[[1]] <- list(R(val(e, "title"), b = TRUE))
      l2 <- list()
      if (nzchar(val(e, "org"))) l2[[1]] <- R(val(e, "org"), i = TRUE)
      if (nzchar(val(e, "location"))) l2[[length(l2) + 1]] <- R(paste0(if (length(l2)) ", " else "", val(e, "location")))
      if (length(l2)) out$lines[[2]] <- l2
      out$right <- date_label(e, sec)
      out$bullets <- bullets_of(e)
    },
    award = , grant = {
      l1 <- list(R(val(e, "title"), b = TRUE))
      if (kind == "grant" && nzchar(val(e, "amount"))) l1[[2]] <- R(paste0(" (", val(e, "amount"), ")"))
      out$lines[[1]] <- l1
      l2 <- val(e, "org")
      if (kind == "grant" && nzchar(val(e, "status"))) l2 <- trimws(paste0(l2, if (nzchar(l2)) " " else "", "(", val(e, "status"), ")"))
      if (nzchar(l2)) out$lines[[2]] <- list(R(l2, i = TRUE))
      out$right <- val(e, "year")
      out$bullets <- bullets_of(e)
    },
    pub = {
      st <- val(e, "status"); yr <- val(e, "year")
      yrt <- if (st == "In press") "in press" else if (nzchar(yr)) yr else "n.d."
      runs <- c(author_runs(val(e, "authors"), last), list(R(paste0(" (", yrt, "). "))))
      ttl <- val(e, "title")
      if (st %in% c("Submitted", "In preparation")) {
        note <- if (st == "Submitted") "Manuscript submitted for publication" else "Manuscript in preparation"
        runs <- c(runs, list(R(paste0(sub("[.]$", "", ttl), " [", note, "]."))))
      } else {
        runs <- c(runs, list(R(paste0(end_punct(ttl), " "))))
        if (nzchar(val(e, "journal"))) {
          vol <- val(e, "volume")
          runs <- c(runs, list(R(paste0(val(e, "journal"), if (nzchar(vol)) ", " else ""), i = TRUE)))
          if (nzchar(vol)) runs <- c(runs, list(R(vol, i = TRUE)))
          if (nzchar(val(e, "issue"))) runs <- c(runs, list(R(paste0("(", val(e, "issue"), ")"))))
          if (nzchar(val(e, "pages"))) runs <- c(runs, list(R(paste0(", ", gsub("-", "–", val(e, "pages"))))))
          runs <- c(runs, list(R(". ")))
        }
        d <- clean_doi(val(e, "doi"))
        if (nzchar(d)) runs <- c(runs, list(R(paste0("https://doi.org/", d))))
      }
      out$lines[[1]] <- runs
    },
    pres = {
      yr <- val(e, "year"); mo <- val(e, "month")
      mo_full <- if (nzchar(mo)) month.name[match(mo, month.abb)] else ""
      runs <- c(author_runs(val(e, "authors"), last),
                list(R(paste0(" (", yr, if (nzchar(mo_full)) paste0(", ", mo_full) else "", "). ")),
                     R(sub("[.]$", "", val(e, "title")), i = TRUE),
                     R(paste0(" [", val(e, "type") %||% "Poster presentation", "]. ",
                              val(e, "event"),
                              if (nzchar(val(e, "location"))) paste0(", ", val(e, "location")) else "", "."))))
      if (nzchar(val(e, "award"))) runs <- c(runs, list(R(paste0(" (", val(e, "award"), ")"), b = TRUE)))
      out$lines[[1]] <- runs
    },
    cert = {
      out$lines[[1]] <- list(R(val(e, "title"), b = TRUE))
      l2 <- val(e, "org")
      if (nzchar(val(e, "expires"))) l2 <- paste0(l2, if (nzchar(l2)) "; " else "", "valid through ", val(e, "expires"))
      if (nzchar(l2)) out$lines[[2]] <- list(R(l2, i = TRUE))
      out$right <- val(e, "year")
    },
    simple = {
      out$lines[[1]] <- c(list(R(val(e, "title"))), if (nzchar(val(e, "org"))) list(R(paste0(" \u2013 ", val(e, "org")), i = TRUE)) else list())
      out$right <- val(e, "year")
    },
    line = {
      code <- val(e, "org")
      out$lines[[1]] <- c(if (nzchar(code)) list(R(paste0(code, ": "), b = TRUE)) else list(), list(R(val(e, "title"))))
      out$right <- val(e, "year")
    },
    skill = {
      out$lines[[1]] <- list(R(paste0(val(e, "title"), ": "), b = TRUE), R(val(e, "items")))
    },
    reference = {
      out$lines[[1]] <- list(R(val(e, "title"), b = TRUE))
      for (k in c("org", "location")) if (nzchar(val(e, k))) out$lines[[length(out$lines) + 1]] <- list(R(val(e, k)))
      ct <- c(val(e, "email"), val(e, "phone")); ct <- ct[nzchar(ct)]
      if (length(ct)) out$lines[[length(out$lines) + 1]] <- list(R(paste(ct, collapse = "  |  ")))
    })
  out
}

sort_entries <- function(ents, sec, defs = SECTIONS) {
  if (length(ents) < 2) return(ents)
  kind <- defs[[sec]]$kind
  if (!kind %in% SORTED_KINDS) return(ents)
  num <- function(x) ifelse(is.na(x), -1e5, x)
  if (kind == "dated") {
    endk <- vapply(ents, function(e) if (is_current(e)) 1e4 else num(date_num(val(e, "end_month"), val(e, "end_year"))), 0)
    stk <- vapply(ents, function(e) num(date_num(val(e, "start_month"), val(e, "start_year"))), 0)
    endk <- ifelse(endk == -1e5, stk, endk)
    return(ents[order(-endk, -stk)])
  }
  yr <- vapply(ents, function(e) num(suppressWarnings(as.numeric(val(e, "year")))), 0)
  if (kind == "pub") {
    rank <- match(vapply(ents, val, "", "status"), c("In preparation", "Submitted", "In press", "Published"))
    rank[is.na(rank)] <- 4
    return(ents[order(rank, -yr)])
  }
  if (kind == "pres") {
    mo <- vapply(ents, function(e) num(match(val(e, "month"), month.abb)), 0)
    return(ents[order(-yr, -mo)])
  }
  ents[order(-yr)]
}

# Assemble the whole CV (only sections that are turned on AND have entries)
build_cv <- function(st) {
  last <- name_last(st$profile$name)
  secs <- list()
  defs <- all_defs(st$custom)
  for (s in section_order(st)) {
    ents <- Filter(function(e) identical(e$section, s), st$entries)
    if (!length(ents)) next
    ents <- sort_entries(ents, s, defs)
    secs[[length(secs) + 1]] <- list(title = defs[[s]]$title,
                                      entries = lapply(ents, render_entry, sec = s, last = last, defs = defs))
  }
  secs
}
contact_items <- function(p) {
  x <- c(p$email, p$phone, p$website, if (nzchar(p$orcid %||% "")) paste("ORCID:", p$orcid))
  x[nzchar(x %||% "")]
}

# ---------------------------------------------------------------------
# 4. HTML output (live preview + PDF)
#    The CV is laid out as real 8.5 x 11 in pages by a small JavaScript
#    paginator (PAGINATE_JS). It fills one or two columns entry by entry,
#    moves an entry to the next column/page when it doesn't fit, and adds
#    "SECTION (CONT.)" headings there. The break points it finds are sent
#    back to R so the Word file breaks in the same places.
# ---------------------------------------------------------------------
cv_css <- function(style) {
  f <- FONTS[[style$font]] %||% FONTS[[1]]; a <- style$accent %||% "#111111"
  sprintf("
.cv-root .cv-src{display:none}
.cv-root .cv-scalebox{overflow:hidden}
.cv-root .cv-pages{width:8.5in;transform-origin:top left}
.cv-root .cv-page{position:relative;width:8.5in;height:11in;box-sizing:border-box;padding:%sin;background:#fff;
  color:#111;font-family:%s;font-size:10.5pt;line-height:1.3;margin:0 0 18px;box-shadow:0 2px 14px rgba(0,0,0,.18);overflow:hidden}
.cv-root[data-cols='2'] .cv-page{font-size:10pt}
.cv-root .cv-cols{display:flex;gap:.3in}
.cv-root .cv-col{flex:1;min-width:0;position:relative;overflow:hidden}
.cv-root .cv-name{font-size:21pt;font-weight:700;text-align:center;margin:0;line-height:1.15;color:%s}
.cv-root .cv-head{text-align:center;font-style:italic;font-size:11pt;margin-top:2pt}
.cv-root .cv-contact{text-align:center;font-size:9.5pt;margin-top:3pt;color:#333}
.cv-root .cv-header{padding-bottom:6pt}
.cv-root .cv-h{color:%s;border-bottom:1px solid %s;font-weight:700;text-transform:uppercase;
  letter-spacing:.05em;font-size:11pt;margin:12pt 0 4pt;padding-bottom:1pt}
.cv-root .cv-col>.cv-h:first-child{margin-top:0}
.cv-root[data-cols='2'] .cv-h{text-align:center}
.cv-root .cv-entry{display:flex;gap:10pt;padding:0 0 5pt}
.cv-root .cv-main{flex:1;min-width:0}
.cv-root .cv-right{white-space:nowrap;text-align:right}
.cv-root[data-cols='2'] .cv-right{font-size:9pt;font-style:italic}
.cv-root ul.cv-b{margin:1pt 0 0;padding-left:1.15em}
.cv-root ul.cv-b li{margin:0}
.cv-root .cv-empty{color:#888;text-align:center;margin-top:1in;font-family:system-ui,sans-serif}
", PAGE_MARGIN, f$css, a, a, a)
}
runs_html <- function(runs) paste0(vapply(runs, function(r) {
  t <- esc(r$t); if (isTRUE(r$b)) t <- paste0("<b>", t, "</b>"); if (isTRUE(r$i)) t <- paste0("<i>", t, "</i>"); t
}, ""), collapse = "")
entry_html <- function(en) {
  lines <- paste0("<div>", vapply(en$lines, runs_html, ""), "</div>", collapse = "")
  bl <- if (length(en$bullets)) paste0("<ul class='cv-b'>", paste0("<li>", esc(en$bullets), "</li>", collapse = ""), "</ul>") else ""
  sprintf("<div class='cv-entry'><div class='cv-main'>%s%s</div>%s</div>", lines, bl,
          if (nzchar(en$right)) sprintf("<div class='cv-right'>%s</div>", esc(en$right)) else "")
}
# The unpaginated "source": a header plus one block per section
cv_src_html <- function(st) {
  p <- st$profile; secs <- build_cv(st)
  h <- c(sprintf("<div class='cv-name'>%s</div>", esc(if (nzchar(p$name)) p$name else "Your Name")))
  if (nzchar(p$headline)) h <- c(h, sprintf("<div class='cv-head'>%s</div>", esc(p$headline)))
  if (nzchar(p$location)) h <- c(h, sprintf("<div class='cv-contact'>%s</div>", esc(p$location)))
  ci <- contact_items(p)
  if (length(ci)) h <- c(h, sprintf("<div class='cv-contact'>%s</div>", paste(esc(ci), collapse = " &nbsp;|&nbsp; ")))
  if (!length(secs)) h <- c(h, "<div class='cv-empty'>Your entries will appear here as you add them.</div>")
  body <- vapply(secs, function(s) sprintf("<div class='cv-sec'><div class='cv-h'>%s</div>%s</div>",
                                            esc(s$title), paste(vapply(s$entries, entry_html, ""), collapse = "")), "")
  paste0("<div class='cv-header'>", paste(h, collapse = ""), "</div>", paste(body, collapse = "\n"))
}
cv_cols <- function(style) if (identical(as.character(style$cols %||% "1"), "2")) 2L else 1L
cv_cont <- function(style) !identical(style$cont, FALSE)
cv_root_html <- function(st, report = FALSE, sig = "") sprintf(
  "<div class='cv-root' data-cols='%d' data-cont='%d' data-report='%d' data-sig='%s'><style>%s</style><div class='cv-src'>%s</div><div class='cv-scalebox'><div class='cv-pages'></div></div></div>",
  cv_cols(st$style), as.integer(cv_cont(st$style)), as.integer(report), sig, cv_css(st$style), cv_src_html(st))

PAGINATE_JS <- r"(
function cvPaginate(root){
  if(!root) return null;
  var src=root.querySelector('.cv-src'), out=root.querySelector('.cv-pages'); if(!src||!out) return null;
  var ncol=+root.getAttribute('data-cols')||1, cont=root.getAttribute('data-cont')==='1';
  out.style.transform=''; out.innerHTML='';
  var page, cols, ci, col, colH, breaks=[], npages=0;
  function newPage(first){
    page=document.createElement('div'); page.className='cv-page'; out.appendChild(page); npages++;
    if(first){ var h=src.querySelector('.cv-header'); if(h) page.appendChild(h.cloneNode(true)); }
    var wrap=document.createElement('div'); wrap.className='cv-cols'; page.appendChild(wrap);
    var padB=parseFloat(getComputedStyle(page).paddingBottom)||0;
    colH=Math.floor((page.clientHeight-padB-wrap.offsetTop)*0.97); // 3% slack so Word (different text engine) still fits
    cols=[]; for(var i=0;i<ncol;i++){ var c=document.createElement('div'); c.className='cv-col'; c.style.height=colH+'px'; wrap.appendChild(c); cols.push(c); }
    ci=0; col=cols[0];
  }
  function advance(){ ci++; if(ci>=ncol){ newPage(false); return 'page'; } col=cols[ci]; return 'col'; }
  function fits(el){ return el.offsetTop+el.offsetHeight<=colH+0.5; }
  newPage(true);
  var secs=src.querySelectorAll('.cv-sec');
  for(var si=0;si<secs.length;si++){
    var h=secs[si].querySelector('.cv-h'), ents=secs[si].querySelectorAll('.cv-entry');
    if(!ents.length) continue;
    // keep a heading together with its first entry
    for(var tries=0;;tries++){
      var empty=col.children.length===0, hc=h.cloneNode(true), e0=ents[0].cloneNode(true);
      col.appendChild(hc); col.appendChild(e0);
      if(fits(e0)||empty||tries>0) break;
      col.removeChild(hc); col.removeChild(e0);
      breaks.push({s:si,e:0,k:advance(),c:0});
    }
    for(var j=1;j<ents.length;j++){
      var ec=ents[j].cloneNode(true); col.appendChild(ec);
      if(fits(ec)) continue;
      col.removeChild(ec);
      breaks.push({s:si,e:j,k:advance(),c:cont?1:0});
      if(cont){ var hc2=h.cloneNode(true); hc2.textContent=h.textContent+' (cont.)'; col.appendChild(hc2); }
      col.appendChild(ec);
    }
  }
  return {breaks:breaks, pages:npages};
}
function cvScale(root){
  var box=root.querySelector('.cv-scalebox'), out=root.querySelector('.cv-pages'); if(!box||!out) return;
  var w=out.offsetWidth||816, s=Math.min(1,(box.clientWidth-2)/w);
  out.style.transform='scale('+s+')'; out.style.marginLeft=Math.max(0,(box.clientWidth-w*s)/2)+'px';
  box.style.height=Math.ceil(out.offsetHeight*s)+'px';
}
)"

cv_standalone <- function(st) paste0(
  "<!doctype html><html><head><meta charset='utf-8'><title>", esc(st$profile$name %||% "CV"), " CV</title>",
  "<style>@page{size:letter;margin:0}html,body{margin:0;background:#fff}",
  ".cv-root .cv-page{box-shadow:none!important;margin:0!important;break-after:page}",
  ".cv-root .cv-page:last-child{break-after:auto}</style>",
  "<script>", PAGINATE_JS, "</script></head><body>", cv_root_html(st),
  "<script>cvPaginate(document.querySelector('.cv-root'));</script></body></html>")

# ---------------------------------------------------------------------
# 5. Word output (officer + flextable)
#    `breaks` = the break points found by the paginator (data frame with
#    s = section, e = entry, c = add a (CONT.) heading; 0-based indices).
# ---------------------------------------------------------------------
make_docx <- function(st, path, breaks = NULL) {
  font <- (FONTS[[st$style$font]] %||% FONTS[[1]])$docx
  acc <- st$style$accent %||% "#111111"
  ncol <- cv_cols(st$style)
  fs <- if (ncol == 2) 10 else 10.5
  tp <- function(size = fs, b = FALSE, i = FALSE, color = "#111111")
    fp_text(font.size = size, bold = b, italic = i, color = color, font.family = font)
  center <- fp_par(text.align = "center", padding = 0)
  psize <- page_size(width = 8.5, height = 11, orient = "portrait")
  m_tb <- PAGE_MARGIN; m_lr <- PAGE_MARGIN
  pmar <- page_mar(top = m_tb, bottom = m_tb, left = m_lr, right = m_lr, header = .3, footer = .3, gutter = 0)
  brk <- function() if (ncol == 2) run_columnbreak() else run_pagebreak()
  p <- st$profile
  doc <- read_docx()
  col_w <- if (ncol == 2) (8.5 - 2 * m_lr - .3) / 2 else 8.5 - 2 * m_lr
  # The two-column part is the document's final section, so Word doesn't "balance" the last page
  doc <- body_set_default_section(doc, prop_section(page_size = psize, page_margins = pmar,
           type = if (ncol == 2) "continuous" else NULL,
           section_columns = if (ncol == 2) section_columns(widths = c(col_w, col_w), space = .3) else NULL))
  doc <- body_add_fpar(doc, fpar(ftext(if (nzchar(p$name)) p$name else "Your Name", tp(21, TRUE, color = acc)), fp_p = center))
  if (nzchar(p$headline)) doc <- body_add_fpar(doc, fpar(ftext(p$headline, tp(11, i = TRUE)), fp_p = center))
  if (nzchar(p$location)) doc <- body_add_fpar(doc, fpar(ftext(p$location, tp(9.5, color = "#333333")), fp_p = center))
  ci <- contact_items(p)
  if (length(ci)) doc <- body_add_fpar(doc, fpar(ftext(paste(ci, collapse = "   |   "), tp(9.5, color = "#333333")), fp_p = center))
  if (ncol == 2)   # header spans the page; everything after it flows in two columns
    doc <- body_end_block_section(doc, block_section(prop_section(page_size = psize, page_margins = pmar, type = "continuous")))
  right_w <- if (ncol == 2) 1.3 else 1.65
  heading <- function(doc, title, before_break = FALSE) {
    runs <- list(ftext(toupper(title), tp(11, TRUE, color = acc)))
    if (before_break) runs <- c(list(brk()), runs)
    doc <- body_add_fpar(doc, do.call(fpar, c(runs, list(fp_p = fp_par(
      text.align = if (ncol == 2) "center" else "left", padding.top = if (before_break) 0 else 10,
      padding.bottom = 2, border.bottom = fp_border(color = acc, width = .75), keep_with_next = TRUE)))))
  }
  entries_table <- function(doc, ents) {
    n <- length(ents)
    has_right <- any(vapply(ents, function(en) nzchar(en$right), TRUE))
    ft <- flextable(data.frame(a = character(n), b = character(n)))
    ft <- delete_part(ft, part = "header")
    for (k in seq_len(n)) {
      en <- ents[[k]]; ch <- list()
      for (li in seq_along(en$lines)) {
        if (li > 1) ch[[length(ch) + 1]] <- as_chunk("\n", props = tp())
        for (r in en$lines[[li]]) ch[[length(ch) + 1]] <- as_chunk(r$t, props = tp(b = isTRUE(r$b), i = isTRUE(r$i)))
      }
      for (bl in en$bullets) ch[[length(ch) + 1]] <- as_chunk(paste0("\n•  ", bl), props = tp())
      ft <- compose(ft, i = k, j = "a", value = as_paragraph(list_values = ch))
      ft <- compose(ft, i = k, j = "b", value = as_paragraph(as_chunk(en$right,
              props = tp(size = if (ncol == 2) 9 else fs, i = ncol == 2))))
    }
    ft <- border_remove(ft)
    ft <- width(ft, j = 1, width = if (has_right) col_w - right_w else col_w - .05)
    ft <- width(ft, j = 2, width = if (has_right) right_w else .05)
    ft <- align(ft, j = 2, align = "right", part = "body")
    ft <- valign(ft, valign = "top", part = "body")
    ft <- padding(ft, padding.left = 0, padding.right = 0, padding.top = 1.5, padding.bottom = 4, part = "body")
    ft <- set_table_properties(ft, layout = "fixed")
    ft <- set_table_properties(ft, layout = "fixed", opts_word = list(split = FALSE))   # never split an entry across pages
    body_add_flextable(doc, ft, align = "left")
  }
  br <- if (is.null(breaks) || !NROW(breaks)) data.frame(s = integer(), e = integer(), c = integer()) else breaks
  secs <- build_cv(st)
  for (si in seq_along(secs)) {
    s <- secs[[si]]; b <- br[br$s == si - 1, , drop = FALSE]
    doc <- heading(doc, s$title, before_break = any(b$e == 0))
    cuts <- sort(unique(b$e[b$e > 0]))
    starts <- c(1, cuts + 1); ends <- c(cuts, length(s$entries))
    for (k in seq_along(starts)) {
      if (k > 1) {
        if (isTRUE(b$c[b$e == cuts[k - 1]][1] == 1)) doc <- heading(doc, paste(s$title, "(cont.)"), before_break = TRUE)
        else doc <- body_add_fpar(doc, fpar(brk(), ftext("", tp(1))))
      }
      doc <- entries_table(doc, s$entries[starts[k]:ends[k]])
    }
  }
  print(doc, target = path)
  invisible(path)
}

# ---------------------------------------------------------------------
# 6. PDF output: Chrome/Edge (headless) -> LibreOffice -> browser print
# ---------------------------------------------------------------------
find_exe <- function(cands, which = character()) {
  cands <- c(Sys.getenv("CV_CHROME"), cands, unname(Sys.which(which)))
  cands <- cands[nzchar(cands)]
  hit <- cands[file.exists(cands) & !dir.exists(cands)]
  if (length(hit)) hit[1] else ""
}
find_browser <- function() {
  pf <- Sys.getenv(c("ProgramFiles", "ProgramFiles(x86)", "LOCALAPPDATA"))
  win <- if (.Platform$OS.type == "windows") c(
    file.path(pf, "Google/Chrome/Application/chrome.exe"),
    file.path(pf, "Microsoft/Edge/Application/msedge.exe"),
    file.path(pf, "BraveSoftware/Brave-Browser/Application/brave.exe")) else character()
  find_exe(c(win,
    "/Applications/Google Chrome.app/Contents/MacOS/Google Chrome",
    "/Applications/Microsoft Edge.app/Contents/MacOS/Microsoft Edge",
    "/Applications/Chromium.app/Contents/MacOS/Chromium",
    "/Applications/Brave Browser.app/Contents/MacOS/Brave Browser"),
    c("google-chrome", "google-chrome-stable", "chromium", "chromium-browser", "microsoft-edge", "brave-browser"))
}
find_soffice <- function() {
  pf <- Sys.getenv(c("ProgramFiles", "ProgramFiles(x86)"))
  win <- if (.Platform$OS.type == "windows") file.path(pf, "LibreOffice/program/soffice.exe") else character()
  find_exe(c(win, "/Applications/LibreOffice.app/Contents/MacOS/soffice"), c("soffice", "libreoffice"))
}
# Run an external program. system() + shQuote() quotes paths with spaces correctly on
# Windows, macOS and Linux. On Unix we drop R's LD_LIBRARY_PATH, which can break LibreOffice.
run_cmd <- function(exe, args, timeout = 120) {
  cmd <- paste(shQuote(exe), paste(args, collapse = " "))
  if (.Platform$OS.type == "unix") cmd <- paste("env -u LD_LIBRARY_PATH", cmd)
  try(suppressWarnings(system(cmd, ignore.stdout = TRUE, ignore.stderr = TRUE, timeout = timeout)), silent = TRUE)
}
html_to_pdf <- function(html_file, pdf_file) {
  br <- find_browser(); if (!nzchar(br)) return(FALSE)
  url <- paste0("file://", if (.Platform$OS.type == "windows") "/" else "", normalizePath(html_file, winslash = "/"))
  prof <- tempfile("cvchrome"); dir.create(prof)
  args <- c("--headless", "--disable-gpu", "--virtual-time-budget=5000", "--no-first-run", "--no-pdf-header-footer", "--print-to-pdf-no-header",
            paste0("--user-data-dir=", shQuote(prof)), paste0("--print-to-pdf=", shQuote(pdf_file)), shQuote(url))
  if (.Platform$OS.type == "unix" && identical(unname(Sys.info()["user"]), "root")) args <- c("--no-sandbox", args)
  run_cmd(br, args, timeout = 90)
  file.exists(pdf_file) && file.info(pdf_file)$size > 1000
}
docx_to_pdf <- function(docx_file, pdf_file) {
  so <- find_soffice(); if (!nzchar(so)) return(FALSE)
  docx_file <- normalizePath(docx_file)
  od <- tempfile("cvlo"); dir.create(od)
  run_cmd(so, c("--headless", "--convert-to", "pdf", "--outdir", shQuote(od), shQuote(docx_file)), timeout = 120)
  out <- file.path(od, sub("\\.docx$", ".pdf", basename(docx_file)))
  if (file.exists(out)) file.copy(out, pdf_file, overwrite = TRUE) else FALSE
}

# ---------------------------------------------------------------------
# 7. Checks: catch the mistakes that slip into real CVs
# ---------------------------------------------------------------------
TYPOS <- c(recieve = "receive", recieved = "received", reseach = "research", reserach = "research",
  experiance = "experience", expirience = "experience", managment = "management", liasson = "liaison",
  liason = "liaison", assistent = "assistant", asistant = "assistant", labratory = "laboratory",
  laboratoy = "laboratory", seperate = "separate", occured = "occurred", acheive = "achieve",
  acheived = "achieved", collegue = "colleague", comittee = "committee", commitee = "committee",
  accomodate = "accommodate", untill = "until", beleive = "believe", publically = "publicly",
  maintainance = "maintenance", neuroscence = "neuroscience", pyschology = "psychology",
  psycology = "psychology", phsyiology = "physiology", physiolgy = "physiology", univeristy = "university",
  universtiy = "university", conferance = "conference", presentaiton = "presentation", analyis = "analysis",
  anaylsis = "analysis", statisitcs = "statistics", statstics = "statistics", scholership = "scholarship",
  fellowshp = "fellowship", enviroment = "environment", independant = "independent", reccomend = "recommend",
  recomend = "recommend", sucessful = "successful", succesful = "successful", tutoral = "tutorial",
  volunter = "volunteer", graduatation = "graduation", microscopey = "microscopy", immunohistochemisty = "immunohistochemistry")

entry_label <- function(e) {
  t <- val(e, "title"); if (nchar(t) > 55) t <- paste0(substr(t, 1, 52), "...")
  if (nzchar(t)) sprintf("\"%s\"", t) else "An entry"
}
run_checks <- function(st) {
  out <- list()
  add <- function(level, sec, msg, id = NA) out[[length(out) + 1]] <<- list(level = level, sec = sec, msg = msg, id = id)
  p <- st$profile
  if (!nzchar(p$name)) add("error", "About you", "Add your name on the Start tab.")
  if (!nzchar(p$email)) add("warning", "About you", "Add an email address so people can contact you.")
  else if (!grepl("^[^@[:space:]]+@[^@[:space:]]+\\.[A-Za-z]{2,}$", p$email))
    add("error", "About you", sprintf("\"%s\" doesn't look like a valid email address.", p$email))
  else if (!grepl("\\.edu$", p$email, ignore.case = TRUE))
    add("tip", "About you", "On an academic CV, your university (.edu) email usually looks more professional.")
  last <- name_last(p$name)
  dois <- list()
  defs <- all_defs(st$custom)
  for (s in section_order(st)) {
    S <- defs[[s]]
    ents <- Filter(function(e) identical(e$section, s), st$entries)
    if (!length(ents)) { add("tip", S$title, "This section is turned on but empty, so it will be left off your CV."); next }
    has_dates <- any(vapply(S$fields, function(f) f$type == "dates", TRUE))
    for (e in ents) {
      lab <- entry_label(e)
      miss <- vapply(Filter(function(f) isTRUE(f$required), S$fields), function(f) if (nzchar(val(e, f$name))) "" else f$label, "")
      miss <- miss[nzchar(miss)]
      if (length(miss)) add("error", S$title, sprintf("%s is missing: %s.", lab, paste(miss, collapse = ", ")), e$id)
      if (has_dates) {
        sn <- date_num(val(e, "start_month"), val(e, "start_year"))
        en <- if (is_current(e)) NA else date_num(val(e, "end_month"), val(e, "end_year"))
        if (!is.na(sn) && !is.na(en) && en < sn) add("error", S$title, sprintf("%s ends before it starts. Check the dates.", lab), e$id)
        if (!is.na(sn) && sn > NOW + 0.01) add("warning", S$title, sprintf("%s has a start date in the future.", lab), e$id)
        if (!is.na(en) && en > NOW + 0.01 && s != "education")
          add("warning", S$title, sprintf("%s ends in the future. If it's ongoing, tick \"I'm currently doing this\" instead.", lab), e$id)
        if (is.na(sn) && is.na(en) && !is_current(e)) add("warning", S$title, sprintf("%s has no dates.", lab), e$id)
        if (s == "education" && is.na(en) && !is_current(e) && !is.na(sn))
          add("tip", S$title, sprintf("%s: add a graduation (or expected graduation) date.", lab), e$id)
      }
      if (S$kind %in% c("pub", "pres") && nzchar(last) && nzchar(val(e, "authors")) &&
          !grepl(tolower(last), tolower(val(e, "authors")), fixed = TRUE))
        add("warning", S$title, sprintf("%s: your last name (%s) isn't in the author list. Check the spelling.", lab, last), e$id)
      if (S$kind == "pub") {
        d <- tolower(clean_doi(val(e, "doi")))
        if (nzchar(d)) dois[[d]] <- c(dois[[d]], lab)
        if (val(e, "status") == "Published" && !nzchar(d)) add("tip", S$title, sprintf("%s: add the DOI so readers can find the paper.", lab), e$id)
        if (val(e, "status") %in% c("Published", "In press") && !nzchar(val(e, "journal")))
          add("warning", S$title, sprintf("%s: add the journal name.", lab), e$id)
        if (val(e, "status") == "Published" && !nzchar(val(e, "year"))) add("warning", S$title, sprintf("%s: add the publication year.", lab), e$id)
        tw <- strsplit(gsub("[^A-Za-z ]", " ", val(e, "title")), "\\s+")[[1]][-1]; tw <- tw[nchar(tw) > 3]
        if (length(tw) >= 4 && mean(grepl("^[A-Z][a-z]", tw)) > .6)
          add("tip", S$title, sprintf("%s: APA uses sentence case for article titles (only the first word, proper nouns, and the word after a colon are capitalized).", lab), e$id)
      }
      for (b in bullets_of(e)) {
        nw <- length(strsplit(b, "\\s+")[[1]])
        if (nw > 35) add("tip", S$title, sprintf("%s has a %d-word bullet. Aim for 1-2 lines.", lab, nw), e$id)
      }
      words <- tolower(unlist(strsplit(paste(vapply(e[setdiff(names(e), c("id", "section", "current"))], function(x) paste(x, collapse = " "), ""), collapse = " "), "[^A-Za-z]+")))
      bad <- unique(words[words %in% names(TYPOS)])
      for (w in bad) add("warning", S$title, sprintf("%s: possible typo \"%s\" (did you mean \"%s\"?).", lab, w, TYPOS[[w]]), e$id)
    }
    if (has_dates && length(ents) > 1) {
      key <- vapply(ents, function(e) {
        if (!nzchar(val(e, "start_year"))) return(NA_character_)
        paste(val(e, "start_month"), val(e, "start_year"), val(e, "end_month"), val(e, "end_year"), is_current(e))
      }, "")
      for (k in unique(key[!is.na(key) & duplicated(key)])) {
        ids <- which(key == k)
        add("warning", S$title, sprintf("%s have identical dates. Check for a copy-and-paste mistake.",
                                        paste(vapply(ents[ids], entry_label, ""), collapse = " and ")), ents[[ids[2]]]$id)
      }
    }
    ttl <- tolower(vapply(ents, function(e) paste(val(e, "title"), val(e, "org"), sep = "|"), "")); ttl[!nzchar(vapply(ents, val, "", "title"))] <- NA
    if (S$kind != "skill") for (t in unique(ttl[!is.na(ttl) & duplicated(ttl)]))
      add("warning", S$title, sprintf("\"%s\" appears more than once in this section.", ents[[which(ttl == t)[1]]]$title))
  }
  for (d in names(dois)) if (length(dois[[d]]) > 1)
    add("error", "Publications", sprintf("The DOI %s is used by more than one paper: %s.", d, paste(dois[[d]], collapse = " and ")))
  out
}

# ---------------------------------------------------------------------
# 8. Save / load
# ---------------------------------------------------------------------
state_json <- function(st) jsonlite::toJSON(c(list(app = "cv-builder", version = 1), st), auto_unbox = TRUE, pretty = TRUE, null = "null")
parse_state <- function(txt) {
  x <- jsonlite::fromJSON(txt, simplifyVector = FALSE)
  if (!identical(x$app, "cv-builder")) stop("This doesn't look like a CV Builder save file.")
  x$sections <- unlist(x$sections) %||% character()
  x$custom <- lapply(x$custom %||% list(), function(cs) lapply(cs, function(v) v %||% ""))
  x$order <- unlist(x$order) %||% character()
  x$entries <- lapply(x$entries %||% list(), function(e) lapply(e, function(v) v %||% ""))
  x
}

new_id <- function() paste0("e", paste(sample(c(letters, 0:9), 10, TRUE), collapse = ""))
mk <- function(section, ...) c(list(id = new_id(), section = section), list(...))
sample_ug <- function() list(
  profile = list(name = "Jordan A. Rivera", headline = "Undergraduate Researcher, Department of Psychology",
                 location = "Northern Illinois University, DeKalb, IL", email = "jrivera@example.edu",
                 phone = "", website = "", orcid = ""),
  stage = "ug",
  sections = stage_defaults("ug"),
  style = list(font = names(FONTS)[1], accent = unname(ACCENTS["Navy"]), cols = "1", cont = TRUE),
  custom = list(),
  entries = list(
    mk("education", title = "B.S. in Psychology, Minor in Biology", org = "Northern Illinois University", location = "DeKalb, IL",
       start_month = "Aug", start_year = "2023", end_month = "May", end_year = as.character(THIS_YEAR + 1), current = FALSE,
       details = "GPA: 3.8/4.0\nHonors Thesis: Alloparental behavior and c-Fos expression in prairie voles"),
    mk("research", title = "Undergraduate Research Assistant", org = "Ortiz Neuroscience Lab (PI: Dr. Richard J. Ortiz)",
       location = "Northern Illinois University, DeKalb, IL", start_month = "Jan", start_year = "2025", end_month = "", end_year = "", current = TRUE,
       details = "Sectioned prairie vole brains on a vibratome and performed c-Fos immunohistochemistry\nRegistered brain sections to the Allen Mouse Brain Atlas using Fiji/ABBA\nScored alloparental behavior videos for 30+ animals"),
    mk("presentations", authors = "Rivera, J. A., Nguyen, T. L., & Ortiz, R. J.", year = as.character(THIS_YEAR), month = "Apr",
       title = "Mapping c-Fos expression after alloparental behavior in prairie voles", type = "Poster presentation",
       event = "Conference on Undergraduate Research and Engagement", location = "Northern Illinois University, DeKalb, IL", award = ""),
    mk("awards", title = "Dean's List", org = "Northern Illinois University", year = "2024", details = "Fall 2023 through Spring 2025"),
    mk("employment", title = "Peer Tutor", org = "NIU Tutoring Center", location = "DeKalb, IL", start_month = "Aug", start_year = "2024",
       end_month = "", end_year = "", current = TRUE, details = "Tutor 10+ students per week in introductory psychology and statistics"),
    mk("service", title = "Treasurer", org = "Student Psychological Association", location = "Northern Illinois University",
       start_month = "Aug", start_year = "2025", end_month = "", end_year = "", current = TRUE, details = ""),
    mk("service", title = "Volunteer", org = "NIU STEMfest", location = "DeKalb, IL", start_month = "Oct", start_year = "2025",
       end_month = "", end_year = "", current = FALSE, details = "Ran a reaction-time neuroscience booth for K-12 visitors"),
    mk("coursework", title = "Research Methods in Psychology", org = "PSYC 305", year = "2025"),
    mk("coursework", title = "Statistics for Psychology", org = "", year = "2024"),
    mk("coursework", title = "Biological Psychology", org = "", year = "2024"),
    mk("certifications", title = "CITI Program: Social-Behavioral-Educational Researchers", org = "CITI Program",
       year = "2025", expires = "2028"),
    mk("certifications", title = "Working with Rodents in Research", org = "NIU Office of Research Compliance",
       year = "2025", expires = ""),
    mk("skills", title = "Laboratory", items = "Vibratome sectioning, immunohistochemistry (DAB, c-Fos), brightfield microscopy, rodent handling"),
    mk("skills", title = "Software", items = "R, SPSS, Fiji/ImageJ, Microsoft Excel"),
    mk("skills", title = "Languages", items = "Spanish (fluent)"),
    mk("memberships", title = "Psi Chi International Honor Society in Psychology", org = "Member",
       start_month = "", start_year = "2024", end_month = "", end_year = "", current = TRUE),
    mk("references", title = "Dr. Richard J. Ortiz", org = "Assistant Professor of Psychology",
       location = "Northern Illinois University", email = "rortiz@niu.edu", phone = "")))

# Example CVs (all people are fictional; emails use example.edu and DOIs use the
# 10.5555 test prefix so nothing points to a real person or paper)
yr <- function(k = 0) as.character(THIS_YEAR + k)
sample_state <- function(stage = "ug") switch(stage, grad = sample_grad(), fac = sample_fac(), sample_ug())

sample_grad <- function() list(
  profile = list(name = "Alex M. Chen", headline = "M.A. Student, Behavioral Neuroscience, Department of Psychology",
                 location = "Northern Illinois University, DeKalb, IL", email = "achen@example.edu",
                 phone = "", website = "", orcid = ""),
  stage = "grad",
  sections = setdiff(stage_defaults("grad"), "employment"),
  style = list(font = names(FONTS)[1], accent = unname(ACCENTS["Navy"]), cols = "1", cont = TRUE),
  custom = list(), order = character(),
  entries = list(
    mk("education", title = "M.A. in Psychology (Behavioral Neuroscience)", org = "Northern Illinois University", location = "DeKalb, IL",
       start_month = "Aug", start_year = yr(-1), end_month = "May", end_year = yr(1), current = FALSE,
       details = "Thesis: Oxytocin-evoked calcium signaling in prairie vole brain slices\nAdvisor: Dr. Richard J. Ortiz"),
    mk("education", title = "B.S. in Biological Sciences, Minor in Chemistry", org = "Northern Illinois University", location = "DeKalb, IL",
       start_month = "Aug", start_year = yr(-5), end_month = "May", end_year = yr(-1), current = FALSE,
       details = ""),
    mk("research", title = "Graduate Research Assistant", org = "Ortiz Neuroscience Lab (PI: Dr. Richard J. Ortiz)",
       location = "Northern Illinois University, DeKalb, IL", start_month = "Aug", start_year = yr(-1), end_month = "", end_year = "", current = TRUE,
       details = "Prepare acute prairie vole brain slices and record GCaMP calcium imaging in a 3D-printed perfusion chamber\nWrite R scripts to extract and analyze calcium traces from 40+ recordings\nTrain and supervise two undergraduate research assistants"),
    mk("research", title = "Undergraduate Research Assistant", org = "Behavioral Ecology Lab (PI: Dr. Avery Smith)",
       location = "Northern Illinois University, DeKalb, IL", start_month = "Jan", start_year = yr(-3), end_month = "May", end_year = yr(-1), current = FALSE,
       details = "Scored partner-preference tests and coded behavior videos\nCompleted an honors thesis on social buffering of stress"),
    mk("publications", authors = "Chen, A. M., Nguyen, T. L., & Ortiz, R. J.", status = "Submitted", year = yr(0),
       title = "Oxytocin-evoked calcium responses in prairie vole brain slices", journal = "", volume = "", issue = "", pages = "", doi = ""),
    mk("publications", authors = "Smith, A. B., Chen, A. M., & Garcia, R. P.", status = "Published", year = yr(-1),
       title = "Social buffering of stress responses in prairie voles: A meta-analysis", journal = "Example Journal of Behavioral Neuroscience",
       volume = "12", issue = "3", pages = "201-214", doi = "10.5555/example.2025.0123"),
    mk("publications", authors = "Chen, A. M., & Ortiz, R. J.", status = "In preparation", year = yr(0),
       title = "A low-cost workflow for analyzing calcium imaging data in R", journal = "", volume = "", issue = "", pages = "", doi = ""),
    mk("presentations", authors = "Chen, A. M., Nguyen, T. L., & Ortiz, R. J.", year = yr(-1), month = "Nov",
       title = "Oxytocin-evoked calcium signaling in prairie vole hypothalamic slices", type = "Poster presentation",
       event = "Society for Neuroscience Annual Meeting", location = "San Diego, CA", award = ""),
    mk("presentations", authors = "Chen, A. M., & Ortiz, R. J.", year = yr(0), month = "Mar",
       title = "Imaging social neuropeptide signaling on a chip", type = "Oral presentation",
       event = "Chicago Society for Neuroscience Annual Scientific Meeting", location = "Chicago, IL", award = "Best Graduate Student Talk"),
    mk("grants", title = "Graduate Student Research Grant", org = "NIU Graduate School", year = yr(0), status = "Funded", amount = "$1,000", details = ""),
    mk("grants", title = "NSF Graduate Research Fellowship Program", org = "National Science Foundation", year = yr(-1), status = "Not funded", amount = "",
       details = ""),
    mk("awards", title = "Outstanding Graduate Teaching Assistant", org = "Department of Psychology, Northern Illinois University", year = yr(0), details = ""),
    mk("teaching", title = "Teaching Assistant", org = "PSYC 305: Research Methods in Psychology", location = "Northern Illinois University",
       start_month = "Aug", start_year = yr(-1), end_month = "", end_year = "", current = TRUE,
       details = "Lead weekly lab sections of about 30 students\nGrade research proposals and hold weekly office hours"),
    mk("service", title = "Graduate Student Representative", org = "Psychology Graduate Student Association", location = "Northern Illinois University",
       start_month = "Aug", start_year = yr(0), end_month = "", end_year = "", current = TRUE, details = ""),
    mk("certifications", title = "CITI Program: Working with Rodents in Research", org = "CITI Program", year = yr(-1), expires = yr(2)),
    mk("certifications", title = "Laboratory and Chemical Safety", org = "NIU Environmental Health and Safety", year = yr(-1), expires = ""),
    mk("skills", title = "Laboratory", items = "Acute brain slice preparation, GCaMP calcium imaging, immunohistochemistry, vibratome sectioning"),
    mk("skills", title = "Data analysis", items = "R (tidyverse, lme4), Python, Fiji/ImageJ, GraphPad Prism"),
    mk("memberships", title = "Society for Neuroscience", org = "Student Member", start_month = "", start_year = yr(-2), end_month = "", end_year = "", current = TRUE),
    mk("references", title = "Dr. Richard J. Ortiz", org = "Assistant Professor of Psychology", location = "Northern Illinois University",
       email = "rortiz@niu.edu", phone = ""),
    mk("references", title = "Dr. Avery Smith", org = "Associate Professor of Biological Sciences", location = "Northern Illinois University",
       email = "asmith@example.edu", phone = "")))

sample_fac <- function() {
  inv <- "cus_invited"
  list(
  profile = list(name = "Morgan T. Ellis, Ph.D.", headline = "Assistant Professor, Department of Psychology",
                 location = "Lakeshore State University, Chicago, IL", email = "mellis@example.edu",
                 phone = "", website = "", orcid = ""),
  stage = "fac",
  sections = stage_defaults("fac"),
  style = list(font = names(FONTS)[1], accent = unname(ACCENTS["Cardinal red"]), cols = "2", cont = TRUE),
  custom = list(list(id = inv, title = "Invited Talks", kind = "dated")), order = character(),
  entries = list(
    mk("education", title = "Ph.D. in Neuroscience", org = "University of Wisconsin-Madison", location = "Madison, WI",
       start_month = "Aug", start_year = yr(-12), end_month = "May", end_year = yr(-6), current = FALSE,
       details = "Dissertation: Circadian regulation of social behavior in rodents"),
    mk("education", title = "B.S. in Psychology", org = "University of Iowa", location = "Iowa City, IA",
       start_month = "Aug", start_year = yr(-16), end_month = "May", end_year = yr(-12), current = FALSE, details = ""),
    mk("research", title = "Assistant Professor and Principal Investigator", org = "Social Neuroscience Lab, Department of Psychology",
       location = "Lakeshore State University", start_month = "Aug", start_year = yr(-2), end_month = "", end_year = "", current = TRUE,
       details = "Study how sleep and circadian disruption alter social brain networks"),
    mk("research", title = "Postdoctoral Research Fellow", org = "Department of Neuroscience", location = "University of Minnesota",
       start_month = "Jun", start_year = yr(-6), end_month = "Jul", end_year = yr(-2), current = FALSE,
       details = "Developed ex vivo imaging of the suprachiasmatic nucleus"),
    mk("publications", authors = "Park, J. H., Ellis, M. T., & Okafor, C. N.", status = "In preparation", year = yr(0),
       title = "Sleep loss and social approach in two vole species", journal = "", volume = "", issue = "", pages = "", doi = ""),
    mk("publications", authors = "Ellis, M. T., Rivera, D. A., & Brooks, L. K.", status = "Submitted", year = yr(0),
       title = "Circadian timing of oxytocin release in the hypothalamus", journal = "", volume = "", issue = "", pages = "", doi = ""),
    mk("publications", authors = "Ellis, M. T., & Brooks, L. K.", status = "Published", year = yr(-1),
       title = "Social isolation shifts circadian rhythms in prairie voles", journal = "Example Journal of Neuroendocrinology",
       volume = "37", issue = "2", pages = "e13012", doi = "10.5555/example.2025.0456"),
    mk("publications", authors = "Brooks, L. K., Ellis, M. T., Tan, W., & Kowalski, P. J.", status = "Published", year = yr(-3),
       title = "Ex vivo imaging of suprachiasmatic nucleus slices", journal = "Example Methods in Neuroscience",
       volume = "18", issue = "4", pages = "331-345", doi = "10.5555/example.2023.0789"),
    mk("publications", authors = "Ellis, M. T., Tan, W., & Kowalski, P. J.", status = "Published", year = yr(-5),
       title = "Light exposure at night disrupts pair bonding in voles", journal = "Example Behavioral Brain Research",
       volume = "402", issue = "", pages = "113-125", doi = "10.5555/example.2021.0321"),
    mk("presentations", authors = "Ellis, M. T., & Park, J. H.", year = yr(-1), month = "Nov",
       title = "Sleep, clocks, and social brain networks", type = "Symposium talk",
       event = "Society for Neuroscience Annual Meeting", location = "San Diego, CA", award = ""),
    mk("presentations", authors = "Park, J. H., & Ellis, M. T.", year = yr(0), month = "Mar",
       title = "Sleep loss reduces social approach in voles", type = "Poster presentation",
       event = "Chicago Society for Neuroscience Annual Scientific Meeting", location = "Chicago, IL", award = ""),
    mk("grants", title = "R15 Academic Research Enhancement Award", org = "National Institutes of Health (NIMH)", year = yr(0),
       status = "Pending", amount = "$450,000", details = ""),
    mk("grants", title = "Faculty Seed Grant", org = "Lakeshore State University", year = yr(-1), status = "Funded", amount = "$10,000", details = ""),
    mk("grants", title = "F32 Postdoctoral Fellowship", org = "National Institutes of Health", year = yr(-5), status = "Funded", amount = "", details = ""),
    mk("awards", title = "Early Career Teaching Award", org = "College of Arts and Sciences, Lakeshore State University", year = yr(0), details = ""),
    mk("teaching", title = "Instructor", org = "PSY 340: Behavioral Neuroscience", location = "Lakeshore State University",
       start_month = "Aug", start_year = yr(-2), end_month = "", end_year = "", current = TRUE, details = "About 80 students per semester"),
    mk("teaching", title = "Instructor", org = "PSY 590: Graduate Seminar in Sleep and Circadian Rhythms", location = "Lakeshore State University",
       start_month = "Jan", start_year = yr(-1), end_month = "May", end_year = yr(-1), current = FALSE, details = ""),
    mk("mentorship", title = "Jordan H. Park", org = "Ph.D. student, Psychology", location = "First-author poster, SfN",
       start_month = "Aug", start_year = yr(-2), end_month = "", end_year = "", current = TRUE, details = ""),
    mk("mentorship", title = "Chioma N. Okafor", org = "Undergraduate, Biology", location = "Now in an M.D./Ph.D. program",
       start_month = "Jan", start_year = yr(-2), end_month = "May", end_year = yr(-1), current = FALSE, details = ""),
    mk("service", title = "Ad Hoc Reviewer", org = "Example Journal of Neuroendocrinology; Example Behavioral Brain Research", location = "",
       start_month = "", start_year = yr(-4), end_month = "", end_year = "", current = TRUE, details = ""),
    mk("service", title = "Colloquium Committee Chair", org = "Department of Psychology", location = "Lakeshore State University",
       start_month = "Aug", start_year = yr(-1), end_month = "", end_year = "", current = TRUE, details = ""),
    mk("skills", title = "Techniques", items = "Ex vivo calcium imaging, circadian activity monitoring, stereotaxic surgery, IHC"),
    mk("skills", title = "Analysis", items = "R, MATLAB, Python, graph-theory network analysis"),
    mk("memberships", title = "Society for Neuroscience", org = "Member", start_month = "", start_year = yr(-14), end_month = "", end_year = "", current = TRUE),
    mk("memberships", title = "Society for Research on Biological Rhythms", org = "Member", start_month = "", start_year = yr(-8), end_month = "", end_year = "", current = TRUE),
    mk(inv, title = "Clocks and Connections: Sleep and the Social Brain", org = "Department of Psychology Colloquium", location = "Example University",
       start_month = "Feb", start_year = yr(0), end_month = "", end_year = "", current = FALSE, details = ""),
    mk(inv, title = "Imaging the Brain's Master Clock", org = "Neuroscience Seminar Series", location = "Example Medical School",
       start_month = "Oct", start_year = yr(-1), end_month = "", end_year = "", current = FALSE, details = "")))
}

# ---------------------------------------------------------------------
# 9. User interface
# ---------------------------------------------------------------------
JS <- r"(
Shiny.addCustomMessageHandler('clickDownload', function(id){ var a=document.getElementById(id); if(a) a.click(); });
Shiny.addCustomMessageHandler('printCV', function(html){
  var f=document.getElementById('cv-print-frame'); if(f) f.remove();
  f=document.createElement('iframe'); f.id='cv-print-frame';
  f.style.cssText='position:fixed;left:-10000px;top:0;width:8.5in;height:11in;border:0';
  document.body.appendChild(f);
  f.onload=function(){ setTimeout(function(){ f.contentWindow.focus(); f.contentWindow.print(); }, 300); };
  f.srcdoc=html;
});
Shiny.addCustomMessageHandler('autosave', function(s){ try{ localStorage.setItem('cvbuilder_v1', s); }catch(e){} });
$(document).on('shiny:connected', function(){
  var s=''; try{ s=localStorage.getItem('cvbuilder_v1')||''; }catch(e){}
  Shiny.setInputValue('autosaved', s, {priority:'event'});
});
function cvSet(name, v){ Shiny.setInputValue(name, v, {priority:'event'}); return false; }
function cvRun(id){
  var el=document.getElementById(id); if(!el) return;
  var root=el.querySelector('.cv-root'); if(!root) return;
  var r=cvPaginate(root); cvScale(root);
  if(r && root.getAttribute('data-report')==='1')
    Shiny.setInputValue('cv_layout', {sig:root.getAttribute('data-sig'), pages:r.pages, breaks:JSON.stringify(r.breaks)}, {priority:'event'});
}
// Drag-and-drop reordering of the section list (plain HTML5, no extra libraries)
(function(){
  var dragId=null;
  function items(){ return $('#sec_nav .sec-item'); }
  $(document).on('dragstart', '#sec_nav .sec-item', function(e){
    dragId=this.getAttribute('data-sec'); this.classList.add('dragging');
    var dt=e.originalEvent.dataTransfer; dt.effectAllowed='move'; dt.setData('text/plain', dragId);
  });
  $(document).on('dragend', '#sec_nav .sec-item', function(){
    this.classList.remove('dragging'); items().removeClass('drop-before drop-after'); dragId=null;
  });
  $(document).on('dragover', '#sec_nav .sec-item', function(e){
    if(!dragId) return; e.preventDefault(); e.originalEvent.dataTransfer.dropEffect='move';
    var r=this.getBoundingClientRect(), after=(e.originalEvent.clientY-r.top)>r.height/2;
    items().removeClass('drop-before drop-after');
    if(this.getAttribute('data-sec')!==dragId) this.classList.add(after?'drop-after':'drop-before');
  });
  $(document).on('drop', '#sec_nav .sec-item', function(e){
    e.preventDefault(); if(!dragId) return;
    var target=this.getAttribute('data-sec'), after=this.classList.contains('drop-after');
    var ids=items().map(function(){ return this.getAttribute('data-sec'); }).get().filter(function(x){ return x!==dragId; });
    var i=ids.indexOf(target); if(i<0) return;
    ids.splice(after?i+1:i, 0, dragId);
    items().removeClass('drop-before drop-after');
    Shiny.setInputValue('sec_order_new', ids, {priority:'event'});
  });
})();
function cvRescaleAll(){ document.querySelectorAll('.cv-root').forEach(function(r){ if(r.querySelector('.cv-page')) cvScale(r); }); }
$(document).on('shiny:value', function(e){ if(e.name==='preview_mini'||e.name==='preview_full') setTimeout(function(){ cvRun(e.name); }, 0); });
$(window).on('resize', cvRescaleAll);
$(document).on('shown.bs.tab', function(){ setTimeout(cvRescaleAll, 0); });
)"
CSS <- "
body{background:#f4f5f7}
.paper-wrap{background:#dfe3e8;padding:18px;border-radius:10px;overflow:auto}
.paper-wrap.mini{max-height:calc(100vh - 150px);position:sticky;top:12px}
.entry-card{background:#fff;border:1px solid #dee2e6;border-radius:8px;padding:10px 12px;margin-bottom:8px;
  display:flex;gap:10px;align-items:flex-start}
.entry-card .ec-main{flex:1;min-width:0;font-size:.93rem}
.entry-card .ec-right{color:#6c757d;font-size:.85rem;white-space:nowrap}
.entry-card .btn{padding:2px 8px}
.sec-tip{font-size:.92rem}
.check-item{display:flex;gap:10px;align-items:flex-start;padding:9px 12px;border-radius:8px;margin-bottom:7px;background:#fff;border:1px solid #e3e6ea}
.check-item .ci-msg{flex:1}
.lvl-error{border-left:5px solid #c0392b}.lvl-warning{border-left:5px solid #e0a106}.lvl-tip{border-left:5px solid #2f80c4}
.step-lead{color:#555;max-width:62rem}
.list-group-item.active .badge{background:#fff!important;color:#1F3A5F}
.modal-body .form-group{margin-bottom:.7rem}
.navbar-brand{font-weight:700}
.return-banner{display:flex;gap:16px;align-items:center;flex-wrap:wrap;background:#eef4fb;border:1px solid #c9daee;
  border-left:6px solid #1F3A5F;border-radius:10px;padding:14px 18px;margin:6px 0 18px}
.return-banner .rb-icon,.save-callout .sc-icon{font-size:1.6rem;color:#1F3A5F}
.return-banner .rb-text{flex:1;min-width:260px}
.return-banner .rb-input{min-width:300px}
.return-banner .rb-input .form-group{margin-bottom:0}
.return-banner .rb-input .shiny-input-container{margin-bottom:0}
.save-callout{display:flex;gap:16px;align-items:center;flex-wrap:wrap;background:#fff8e1;border:1px solid #f1dfa6;
  border-left:6px solid #e0a106;border-radius:10px;padding:14px 18px;margin-top:14px}
.save-callout .sc-icon{color:#b07d00}
.save-callout .sc-text{flex:1;min-width:280px}
.sec-item{cursor:grab;user-select:none}
.sec-item:active{cursor:grabbing}
.sec-grip{opacity:.4;font-size:.85rem;flex:none;padding:0 2px}
.sec-item:hover .sec-grip{opacity:.9}
.sec-item.dragging{opacity:.35}
.sec-item.drop-before{box-shadow:inset 0 3px 0 #e0a106}
.sec-item.drop-after{box-shadow:inset 0 -3px 0 #e0a106}
.app-footer{margin-top:40px;padding:18px 16px 22px;border-top:1px solid #dde1e6;background:#fff;
  text-align:center;font-size:.85rem;color:#5f6b7a;line-height:1.6}
.app-footer-small{font-size:.78rem;color:#7a8594}
.app-footer a{color:inherit}
.form-control::placeholder{color:#b0b6bd;font-style:italic;opacity:1}
"

ui <- page_navbar(
  title = "CV Builder", id = "tabs", fillable = FALSE,
  bg = "#1F3A5F", inverse = TRUE,
  theme = bs_theme(version = 5, primary = "#1F3A5F"),
  header = tags$head(tags$script(HTML(PAGINATE_JS)), tags$script(HTML(JS)), tags$style(HTML(CSS))),
  nav_panel("1 · Start", value = "start",
    div(class = "container-xl py-3",
      h3("Build your academic CV, step by step"),
      p(class = "step-lead", "Fill in your details here, then add your experience one section at a time. The app formats everything for you, checks it for common mistakes, and exports a Word or PDF file."),
      div(class = "return-banner",
        div(class = "rb-icon", icon("rotate")),
        div(class = "rb-text", tags$b("Coming back to update your CV?"),
            div("Upload the progress file (.json) you saved last time. All your entries come back, ready to edit, so you never have to start over.")),
        div(class = "rb-input", fileInput("load_file", NULL, accept = ".json", width = "100%",
                                          buttonLabel = "Upload progress file", placeholder = "No file chosen"))),
      layout_columns(col_widths = c(6, 6),
        card(card_header("About you"),
          textInput("p_name", "Full name *", placeholder = "Jordan A. Rivera", width = "100%"),
          textInput("p_headline", "Headline (optional)", placeholder = "Undergraduate Researcher, Department of Psychology", width = "100%"),
          textInput("p_location", "Institution, City", placeholder = "Northern Illinois University, DeKalb, IL", width = "100%"),
          layout_columns(col_widths = c(6, 6),
            textInput("p_email", "Email *", placeholder = "z1234567@students.niu.edu", width = "100%"),
            textInput("p_phone", "Phone (optional)", width = "100%")),
          layout_columns(col_widths = c(6, 6),
            textInput("p_website", "Website or LinkedIn (optional)", width = "100%"),
            textInput("p_orcid", "ORCID (optional)", placeholder = "0000-0002-1825-0097", width = "100%"))),
        card(card_header("Set up your CV"),
          radioButtons("stage", "I am a...", inline = TRUE,
                       choices = c("Undergraduate" = "ug", "Graduate student" = "grad", "Postdoc / Faculty" = "fac")),
          checkboxGroupInput("sections", "Sections to include (picking a stage above selects good defaults)",
                             choices = SEC_CHOICES, selected = stage_defaults("ug"), inline = TRUE),
          div(class = "form-text mt-n2 mb-3", "Need something that isn't listed? Click \"Add your own section\" on the next tab."),
          radioButtons("cols", "Layout", inline = TRUE,
                       choices = c("Standard (one column)" = "1", "Compact (two columns)" = "2")),
          div(class = "form-text mt-n2 mb-2", "One column is best for applications, because upload systems can scramble two-column files. Two columns fit more on a page, which suits a lab website or a printed copy."),
          layout_columns(col_widths = c(6, 6),
            selectInput("font", "Font", choices = names(FONTS), width = "100%", selectize = FALSE),
            selectInput("accent", "Heading color", choices = ACCENTS, selected = ACCENTS["Navy"], width = "100%", selectize = FALSE)))),
      layout_columns(col_widths = c(6, 6),
        card(card_header("First time here?"),
          p(class = "small text-muted mb-2", "Look at a finished example first, or clear everything to start fresh. The example matches the stage you picked under \"I am a...\" (undergraduate, graduate student, or postdoc/faculty)."),
          div(class = "d-flex gap-2 flex-wrap",
            actionButton("load_sample", "See a filled-in example", icon = icon("eye"), class = "btn-outline-primary"),
            actionButton("clear_all", "Start over", icon = icon("eraser"), class = "btn-outline-danger"))),
        div(class = "d-flex align-items-end justify-content-end h-100 pb-3",
          actionButton("go_add", "Next: add your experience →", class = "btn-primary btn-lg"))))),
  nav_panel("2 · Add your experience", value = "add",
    div(class = "container-fluid py-3",
      layout_columns(col_widths = c(3, 4, 5),
        div(h6(class = "text-muted text-uppercase small", "Sections"), uiOutput("sec_nav"),
            p(class = "small text-muted mt-2", tagList("To change the order of sections on your CV, drag a section by its ", icon("grip-vertical"), " handle. Turn sections on or off on the Start tab.")),
            actionLink("order_reset", "Reset to the standard order", class = "small")),
        div(uiOutput("sec_panel")),
        div(h6(class = "text-muted text-uppercase small", "Live preview"),
            div(class = "paper-wrap mini", uiOutput("preview_mini")))))),
  nav_panel(span("3 · Check ", uiOutput("issue_badge", inline = TRUE)), value = "check",
    div(class = "container-lg py-3",
      h3("Check your CV"),
      p(class = "step-lead", "These are the mistakes that most often slip into CVs: dates that don't add up, copy-and-paste duplicates, missing details, and typos. Click Fix to jump straight to the entry."),
      uiOutput("checks_ui"),
      div(class = "mt-3", actionButton("go_preview", "Next: preview and download →", class = "btn-primary")))),
  nav_panel("4 · Preview & download", value = "preview",
    div(class = "container-xl py-3",
      card(
        div(class = "d-flex flex-wrap gap-2 align-items-center",
          downloadButton("dl_docx", "Download Word (.docx)", class = "btn-primary"),
          actionButton("make_pdf", "Download PDF", icon = icon("file-pdf"), class = "btn-primary"),
          actionLink("print_cv", "or print / Save as PDF from your browser", class = "ms-1 small"),
          uiOutput("page_info", inline = TRUE)),
        uiOutput("export_warn"),
        div(class = "save-callout",
          div(class = "sc-icon", icon("floppy-disk")),
          div(class = "sc-text",
            tags$b("Want to update this CV next semester? Save your progress file too."),
            div("Word and PDF files can't be opened again in CV Builder, but your progress file (.json) can. Upload it on the Start tab and all your entries come back, ready to edit. Keep it somewhere safe, like OneDrive or Google Drive, next to your CV."),
            div(class = "small text-muted mt-1", "The app also autosaves, but only in this browser on this computer, and clearing your browser history erases it.")),
          div(class = "sc-btn", downloadButton("save_json2", "Save progress file", class = "btn-warning"))),
        div(style = "position:absolute;left:-9999px", downloadLink("dl_pdf", "pdf"))),
      div(class = "paper-wrap mt-3", uiOutput("preview_full")))),
  nav_spacer(),
  nav_item(downloadLink("save_json", label = tagList(icon("floppy-disk"), "Save progress file"), class = "nav-link")),
  footer = tags$footer(class = "app-footer",
    div(tags$b(paste("CV Builder", APP_VERSION)), " \u00b7 Ortiz Neuroscience Lab, Department of Psychology, Northern Illinois University \u00b7 ",
        APP_YEAR),
    div("Free for anyone to use, at no charge; it may not be sold without permission. Your CV is not stored on any server: it stays in your browser and in the files you download."),
    div(class = "app-footer-small", "Built to help students; this is not an official NIU service. Always have a mentor review your CV before you send it. ",
        "Questions or suggestions: ", tags$a(href = "mailto:rortiz@niu.edu", "rortiz@niu.edu")))
)

# ---------------------------------------------------------------------
# 10. Server
# ---------------------------------------------------------------------
server <- function(input, output, session) {
  rv <- reactiveValues(entries = list(), custom = list(), order = character(), cs_edit = NULL, ready = FALSE, skip_stage = FALSE, pdf = NULL,
                       edit_sec = NULL, edit_id = NULL, del_id = NULL, restore = NULL)
  cur_sec <- reactiveVal("education")

  profile <- reactive(list(
    name = trimws(input$p_name %||% ""), headline = trimws(input$p_headline %||% ""),
    location = trimws(input$p_location %||% ""), email = trimws(input$p_email %||% ""),
    phone = trimws(input$p_phone %||% ""), website = trimws(input$p_website %||% ""),
    orcid = trimws(input$p_orcid %||% "")))
  st <- reactive(list(profile = profile(), stage = input$stage, sections = input$sections %||% character(),
                      style = list(font = input$font, accent = input$accent, cols = input$cols %||% "1",
                                   cont = CONT_HEADINGS), entries = rv$entries,
                      custom = rv$custom, order = rv$order))
  defs <- reactive(all_defs(rv$custom))
  checks <- reactive(run_checks(st()))

  # ---- apply a loaded / sample / restored state ----
  apply_state <- function(x) {
    p <- x$profile %||% list()
    for (k in c("name", "headline", "location", "email", "phone", "website", "orcid"))
      updateTextInput(session, paste0("p_", k), value = p[[k]] %||% "")
    if (!identical(x$stage, input$stage)) rv$skip_stage <- TRUE
    updateRadioButtons(session, "stage", selected = x$stage %||% "ug")
    updateCheckboxGroupInput(session, "sections", selected = x$sections %||% character())
    updateSelectInput(session, "font", selected = x$style$font %||% names(FONTS)[1])
    updateSelectInput(session, "accent", selected = x$style$accent %||% ACCENTS[["Navy"]])
    updateRadioButtons(session, "cols", selected = as.character(x$style$cols %||% "1"))
    rv$entries <- x$entries %||% list()
    rv$custom <- x$custom %||% list()
    rv$order <- x$order %||% character()
    if (length(x$sections)) cur_sec(x$sections[[1]])
  }
  observeEvent(input$stage, {
    if (isTRUE(rv$skip_stage)) { rv$skip_stage <- FALSE; return() }
    updateCheckboxGroupInput(session, "sections", selected = stage_defaults(input$stage))
  }, ignoreInit = TRUE)

  # ---- autosave in the browser ----
  observeEvent(input$autosaved, {
    s <- input$autosaved
    x <- if (nzchar(s)) tryCatch(parse_state(s), error = function(e) NULL) else NULL
    if (!is.null(x) && (length(x$entries) || nzchar(x$profile$name %||% ""))) {
      rv$restore <- x
      showModal(modalDialog(title = "Welcome back!",
        p(sprintf("You have a CV in progress%s with %d entries. Pick up where you left off?",
                  if (nzchar(x$profile$name %||% "")) paste0(" for ", x$profile$name) else "", length(x$entries))),
        footer = tagList(actionButton("restore_no", "Start fresh"), actionButton("restore_yes", "Restore my CV", class = "btn-primary"))))
    } else rv$ready <- TRUE
  }, once = TRUE)
  observeEvent(input$restore_yes, { apply_state(rv$restore); removeModal(); rv$ready <- TRUE })
  observeEvent(input$restore_no, { removeModal(); rv$ready <- TRUE })
  autosave <- debounce(reactive(state_json(st())), 1200)
  observe({ js <- autosave(); if (isTRUE(rv$ready)) session$sendCustomMessage("autosave", as.character(js)) })

  # ---- start tab buttons ----
  observeEvent(input$load_sample, {
    stg <- input$stage %||% "ug"
    apply_state(sample_state(stg))
    who <- c(ug = "an undergraduate", grad = "a graduate student", fac = "a faculty member")[[stg]]
    showNotification(sprintf("Loaded an example CV for %s. Click \"Start over\" when you're ready to make your own.", who),
                     type = "message", duration = 7)
  })
  observeEvent(input$clear_all, showModal(modalDialog(title = "Start over?",
    "This clears every entry and your details. Save your progress first if you might want it back.",
    footer = tagList(modalButton("Cancel"), actionButton("clear_yes", "Yes, clear everything", class = "btn-danger")))))
  observeEvent(input$clear_yes, {
    apply_state(list(profile = list(), stage = "ug", sections = stage_defaults("ug"), style = list(), entries = list(), custom = list(), order = character()))
    removeModal()
  })
  observeEvent(input$load_file, {
    x <- tryCatch(parse_state(paste(readLines(input$load_file$datapath, warn = FALSE, encoding = "UTF-8"), collapse = "\n")),
                  error = function(e) { showNotification(conditionMessage(e), type = "error"); NULL })
    if (!is.null(x)) { apply_state(x); showNotification(sprintf("Loaded %d entries.", length(x$entries)), type = "message") }
  })
  observeEvent(input$go_add, nav_select("tabs", "add"))
  observeEvent(input$go_preview, nav_select("tabs", "preview"))

  # ---- section navigation ----
  observeEvent(input$cur_sec, cur_sec(input$cur_sec))
  active_sec <- reactive({
    en <- section_order(st())
    if (cur_sec() %in% en) cur_sec() else if (length(en)) en[1] else NULL
  })
  output$sec_nav <- renderUI({
    en <- section_order(st()); a <- active_sec(); D <- defs()
    cnt <- table(factor(vapply(rv$entries, `[[`, "", "section"), levels = names(D)))
    tagList(
      if (!length(en)) p(class = "text-muted", "No sections selected.")
      else div(class = "list-group", lapply(seq_along(en), function(k) { s <- en[k]; tags$a(href = "#",
        class = paste("sec-item list-group-item list-group-item-action d-flex align-items-center gap-2 py-2 ps-2", if (identical(s, a)) "active"),
        draggable = "true", `data-sec` = s, title = "Drag to move this section",
        onclick = sprintf("return cvSet('cur_sec','%s')", s),
        span(class = "sec-grip", icon("grip-vertical")),
        span(class = "flex-grow-1", D[[s]]$title, if (isTRUE(D[[s]]$custom)) span(class = "small opacity-75", " (yours)")),
        span(class = "badge rounded-pill bg-secondary", cnt[[s]])) })),
      actionButton("add_custom", "Add your own section", icon = icon("plus"), class = "btn-outline-primary btn-sm w-100 mt-2"))
  })
  output$sec_panel <- renderUI({
    s <- active_sec(); if (is.null(s)) return(NULL)
    D <- defs(); S <- D[[s]]; last <- name_last(profile()$name)
    ents <- sort_entries(Filter(function(e) identical(e$section, s), rv$entries), s, D)
    movable <- !S$kind %in% SORTED_KINDS
    cards <- lapply(seq_along(ents), function(k) {
      e <- ents[[k]]; r <- render_entry(e, s, last, D)
      div(class = "entry-card",
        div(class = "ec-main", HTML(runs_html(r$lines[[1]])),
            if (length(r$lines) > 1) div(class = "text-muted small", HTML(runs_html(r$lines[[2]])))),
        if (nzchar(r$right)) div(class = "ec-right", r$right),
        div(class = "btn-group btn-group-sm",
          if (movable) tags$button(class = "btn btn-outline-secondary", title = "Move up", disabled = if (k == 1) NA,
                                   onclick = sprintf("return cvSet('move',{id:'%s',dir:-1,n:Math.random()})", e$id), icon("arrow-up")),
          if (movable) tags$button(class = "btn btn-outline-secondary", title = "Move down", disabled = if (k == length(ents)) NA,
                                   onclick = sprintf("return cvSet('move',{id:'%s',dir:1,n:Math.random()})", e$id), icon("arrow-down")),
          tags$button(class = "btn btn-outline-primary", title = "Edit", onclick = sprintf("return cvSet('edit_id','%s')", e$id), icon("pen")),
          tags$button(class = "btn btn-outline-danger", title = "Delete", onclick = sprintf("return cvSet('del_id','%s')", e$id), icon("trash"))))
    })
    tagList(
      div(class = "d-flex justify-content-between align-items-center mb-2",
        h4(class = "m-0", S$title),
        actionButton("add_entry", S$add_label %||% (if (isTRUE(S$custom)) "Add entry" else paste("Add", tolower(sub("s$", "", S$title)))),
                     icon = icon("plus"), class = "btn-primary")),
      if (isTRUE(S$custom)) div(class = "d-flex gap-2 mb-2 small",
        actionLink("cs_rename", "Rename this section", icon = icon("pen")), span(class = "text-muted", "\u00b7"),
        actionLink("cs_remove", "Remove this section", icon = icon("trash"), class = "text-danger")),
      div(class = "alert alert-info sec-tip py-2", icon("lightbulb"), " ", S$tip),
      if (length(cards)) cards else p(class = "text-muted fst-italic", "Nothing here yet. Click Add to create your first entry."),
      if (movable && length(ents) > 1) p(class = "small text-muted", "Use the arrows to change the order.")
      else if (length(ents) > 1) p(class = "small text-muted", "Entries are sorted automatically, most recent first."))
  })

  # ---- section order ----
  observeEvent(input$sec_order_new, {
    o <- unlist(input$sec_order_new)
    if (setequal(o, section_order(st()))) rv$order <- o
  })
  observeEvent(input$order_reset, rv$order <- character())

  # ---- student-made sections ----
  custom_modal <- function(cs = NULL) {
    rv$cs_edit <- cs$id
    output$cs_err <- renderUI(NULL)
    showModal(modalDialog(title = if (is.null(cs)) "Add your own section" else "Rename section",
      p(class = "text-muted", "For anything that doesn't fit the built-in sections, such as Leadership, Languages, Volunteer Work, Media Coverage, Invited Talks, or Inventions. You can add as many as you need."),
      textInput("cs_title", "Section heading *", value = cs$title %||% "", placeholder = "e.g., Leadership", width = "100%"),
      if (is.null(cs)) radioButtons("cs_kind", "What kind of entries?", width = "100%", choices = c(
        "Dated entries: a title, where, dates, and bullets (like Research Experience)" = "dated",
        "Simple list: one line per item (like Languages)" = "list")),
      uiOutput("cs_err"),
      footer = tagList(modalButton("Cancel"), actionButton("cs_save", if (is.null(cs)) "Add section" else "Save", class = "btn-primary"))))
  }
  observeEvent(input$add_custom, custom_modal())
  observeEvent(input$cs_rename, {
    cs <- Find(function(x) identical(x$id, active_sec()), rv$custom); if (!is.null(cs)) custom_modal(cs)
  })
  observeEvent(input$cs_save, {
    t <- trimws(input$cs_title %||% "")
    others <- Filter(function(id) !identical(id, rv$cs_edit), names(defs()))
    taken <- tolower(vapply(defs()[others], `[[`, "", "title"))
    err <- if (!nzchar(t)) "Give the section a heading." else if (tolower(t) %in% taken) sprintf("There is already a section called \"%s\".", t)
    if (!is.null(err)) { output$cs_err <- renderUI(div(class = "alert alert-danger py-2", err)); return() }
    if (is.null(rv$cs_edit)) {
      id <- paste0("cus_", substr(new_id(), 2, 9))
      rv$custom[[length(rv$custom) + 1]] <- list(id = id, title = t, kind = input$cs_kind %||% "dated")
      cur_sec(id)
    } else {
      i <- match(rv$cs_edit, vapply(rv$custom, `[[`, "", "id")); rv$custom[[i]]$title <- t
    }
    removeModal()
  })
  observeEvent(input$cs_remove, {
    s <- active_sec(); n <- sum(vapply(rv$entries, function(e) identical(e$section, s), TRUE))
    showModal(modalDialog(title = "Remove this section?", size = "s",
      sprintf("\"%s\" and its %d entr%s will be removed.", defs()[[s]]$title, n, if (n == 1) "y" else "ies"),
      footer = tagList(modalButton("Cancel"), actionButton("cs_remove_yes", "Remove", class = "btn-danger"))))
  })
  observeEvent(input$cs_remove_yes, {
    s <- active_sec()
    rv$custom <- Filter(function(cs) !identical(cs$id, s), rv$custom)
    rv$entries <- Filter(function(e) !identical(e$section, s), rv$entries)
    removeModal()
  })

  # ---- add / edit modal ----
  field_ui <- function(f, e) {
    id <- paste0("f_", f$name); lab <- paste0(f$label, if (isTRUE(f$required)) " *" else "")
    v <- if (is.null(e)) "" else val(e, f$name)
    if (nzchar(f$placeholder)) f$placeholder <- if (f$type == "textarea") paste0("For example:\n", f$placeholder) else paste("e.g.,", f$placeholder)
    w <- switch(f$type,
      text = textInput(id, lab, value = v, placeholder = f$placeholder, width = "100%"),
      textarea = textAreaInput(id, lab, value = v, placeholder = f$placeholder, rows = 4, width = "100%", resize = "vertical"),
      year = selectInput(id, lab, choices = c("—" = "", YEARS), selected = v, selectize = FALSE, width = "100%"),
      month = selectInput(id, lab, choices = c("—" = "", month.abb), selected = v, selectize = FALSE, width = "100%"),
      select = selectInput(id, lab, choices = f$choices, selected = if (nzchar(v)) v else f$choices[1], selectize = FALSE, width = "100%"),
      doi = div(class = "d-flex gap-2 align-items-end",
                div(class = "flex-grow-1", textInput(id, lab, value = v, placeholder = f$placeholder, width = "100%")),
                div(class = "mb-3", actionButton("doi_fetch", "Fill in from DOI", icon = icon("wand-magic-sparkles"), class = "btn-outline-primary"))),
      dates = {
        g <- function(k) if (is.null(e)) "" else val(e, k)
        mo <- c("—" = "", month.abb); yr <- c("—" = "", YEARS)
        tagList(
          layout_columns(col_widths = c(3, 3, 3, 3),
            selectInput("f_start_month", "Start month", mo, g("start_month"), selectize = FALSE),
            selectInput("f_start_year", "Start year", yr, g("start_year"), selectize = FALSE),
            selectInput("f_end_month", "End month", mo, g("end_month"), selectize = FALSE),
            selectInput("f_end_year", "End year", yr, g("end_year"), selectize = FALSE)),
          checkboxInput("f_current", "I'm currently doing this (end date shows as \"Present\")", value = !is.null(e) && is_current(e)))
      })
    if (!is.null(f$help)) tagList(w, div(class = "form-text mt-n2 mb-2", f$help)) else w
  }
  open_modal <- function(s, e = NULL) {
    S <- defs()[[s]]; rv$edit_sec <- s; rv$edit_id <- if (is.null(e)) NULL else e$id
    output$modal_err <- renderUI(NULL)
    showModal(modalDialog(title = paste(if (is.null(e)) "Add to" else "Edit:", S$title), size = "l",
      div(class = "alert alert-light border small py-2", icon("lightbulb"), " ", S$tip),
      lapply(S$fields, field_ui, e = e), uiOutput("modal_err"),
      footer = tagList(modalButton("Cancel"), actionButton("save_entry", "Save", class = "btn-primary"))))
  }
  observeEvent(input$add_entry, open_modal(active_sec()))
  observeEvent(input$edit_id, {
    e <- Find(function(x) identical(x$id, input$edit_id), rv$entries)
    if (!is.null(e)) { cur_sec(e$section); open_modal(e$section, e) }
  })
  observeEvent(input$save_entry, {
    s <- rv$edit_sec; S <- defs()[[s]]
    e <- list(id = rv$edit_id %||% new_id(), section = s)
    for (f in S$fields) {
      if (f$type == "dates") {
        for (k in c("start_month", "start_year", "end_month", "end_year")) e[[k]] <- input[[paste0("f_", k)]] %||% ""
        e$current <- isTRUE(input$f_current)
        if (e$current) { e$end_month <- ""; e$end_year <- "" }
      } else e[[f$name]] <- trimws(input[[paste0("f_", f$name)]] %||% "")
    }
    miss <- vapply(Filter(function(f) isTRUE(f$required), S$fields), function(f) if (nzchar(val(e, f$name))) "" else f$label, "")
    miss <- miss[nzchar(miss)]
    if (length(miss)) {
      output$modal_err <- renderUI(div(class = "alert alert-danger py-2", "Please fill in: ", paste(miss, collapse = ", ")))
      return()
    }
    ids <- vapply(rv$entries, `[[`, "", "id")
    if (e$id %in% ids) rv$entries[[match(e$id, ids)]] <- e else rv$entries[[length(rv$entries) + 1]] <- e
    removeModal()
  })
  observeEvent(input$del_id, {
    rv$del_id <- input$del_id
    e <- Find(function(x) identical(x$id, input$del_id), rv$entries)
    showModal(modalDialog(title = "Delete this entry?", entry_label(e), size = "s",
      footer = tagList(modalButton("Cancel"), actionButton("del_yes", "Delete", class = "btn-danger"))))
  })
  observeEvent(input$del_yes, {
    rv$entries <- Filter(function(x) !identical(x$id, rv$del_id), rv$entries); removeModal()
  })
  observeEvent(input$move, {
    ids <- vapply(rv$entries, `[[`, "", "id"); i <- match(input$move$id, ids); if (is.na(i)) return()
    same <- which(vapply(rv$entries, `[[`, "", "section") == rv$entries[[i]]$section)
    pos <- match(i, same); j <- same[pos + input$move$dir]
    if (length(j) && !is.na(j)) { tmp <- rv$entries[[i]]; rv$entries[[i]] <- rv$entries[[j]]; rv$entries[[j]] <- tmp }
  })

  # ---- DOI lookup (Crossref) ----
  observeEvent(input$doi_fetch, {
    d <- clean_doi(input$f_doi)
    if (!nzchar(d)) { showNotification("Paste a DOI first, for example 10.1021/acsmeasuresciau.5c00028", type = "warning"); return() }
    res <- tryCatch(jsonlite::fromJSON(paste0("https://api.crossref.org/works/", utils::URLencode(d)), simplifyVector = FALSE)$message,
                    error = function(err) NULL)
    if (is.null(res)) { showNotification("Couldn't find that DOI (or no internet connection). You can still type the details in.", type = "error"); return() }
    x <- crossref_fields(res)
    for (k in c("authors", "title", "journal", "volume", "issue", "pages")) updateTextInput(session, paste0("f_", k), value = x[[k]])
    updateTextInput(session, "f_doi", value = d)
    if (nzchar(x$year)) updateSelectInput(session, "f_year", selected = x$year)
    updateSelectInput(session, "f_status", selected = "Published")
    showNotification("Filled in from Crossref. Double-check the details, and change the title to sentence case for APA.", type = "message", duration = 8)
  })

  # ---- preview ----
  # A fingerprint of the current CV, so we only reuse page breaks that match it
  sig <- reactive({ x <- utf8ToInt(paste(state_json(st()), collapse = ""))
                    sprintf("%.0f", sum(as.numeric(x) * (seq_along(x) %% 9973 + 1)) %% 2147483647) })
  preview_html <- reactive(HTML(cv_root_html(st(), report = TRUE, sig = sig())))
  layout_breaks <- reactive({
    L <- input$cv_layout
    if (is.null(L) || !identical(L$sig, sig())) return(NULL)
    b <- jsonlite::fromJSON(L$breaks)
    if (!length(b)) NULL else as.data.frame(b)
  })
  output$page_info <- renderUI({
    L <- input$cv_layout; if (is.null(L) || !identical(L$sig, sig())) return(NULL)
    span(class = "text-muted small ms-2", sprintf("%d page%s", L$pages, if (L$pages == 1) "" else "s"))
  })
  output$preview_mini <- renderUI(preview_html())
  output$preview_full <- renderUI(preview_html())

  # ---- checks ----
  output$issue_badge <- renderUI({
    n <- sum(vapply(checks(), function(x) x$level %in% c("error", "warning"), TRUE))
    if (n) span(class = "badge rounded-pill bg-warning text-dark", n) else span(class = "badge rounded-pill bg-success", icon("check"))
  })
  output$checks_ui <- renderUI({
    ch <- checks()
    if (!length(ch)) return(div(class = "alert alert-success", icon("circle-check"), " Everything looks good. Nice work!"))
    ord <- order(match(vapply(ch, `[[`, "", "level"), c("error", "warning", "tip")))
    lv <- list(error = list("Fix", "circle-xmark", "text-danger"), warning = list("Check", "triangle-exclamation", "text-warning"),
               tip = list("Tip", "lightbulb", "text-primary"))
    n <- table(factor(vapply(ch, `[[`, "", "level"), levels = c("error", "warning", "tip")))
    tagList(
      p(sprintf("%d to fix · %d to double-check · %d tips", n[["error"]], n[["warning"]], n[["tip"]])),
      lapply(ch[ord], function(x) {
        L <- lv[[x$level]]
        div(class = paste("check-item", paste0("lvl-", x$level)),
          span(class = L[[3]], icon(L[[2]])),
          div(class = "ci-msg", tags$b(paste0(x$sec, ":")), " ", x$msg),
          if (!is.na(x$id)) tags$button(class = "btn btn-sm btn-outline-primary", onclick = sprintf("return cvSet('edit_id','%s')", x$id), "Fix")
          else if (x$sec == "About you") tags$button(class = "btn btn-sm btn-outline-primary", onclick = "return cvSet('goto_start', Math.random())", "Fix"))
      }))
  })
  observeEvent(input$goto_start, nav_select("tabs", "start"))

  # ---- exports ----
  file_stub <- reactive({
    l <- gsub("[^A-Za-z-]", "", name_last(profile()$name)); paste0(if (nzchar(l)) l else "My", "_CV_", format(Sys.Date(), "%Y-%m"))
  })
  output$export_warn <- renderUI({
    n <- sum(vapply(checks(), function(x) x$level == "error", TRUE))
    if (n) div(class = "alert alert-warning py-2 mt-2 mb-0", icon("triangle-exclamation"),
               sprintf(" The Check tab found %d thing%s to fix before you send this out.", n, if (n > 1) "s" else ""))
  })
  remind_save <- function() showNotification(
    tagList(tags$b("Downloaded!"), " Also save your progress file so you can come back and update this CV later."),
    action = tags$a(href = "#", onclick = "document.getElementById('save_json2').click(); return false;",
                    icon("floppy-disk"), " Save progress file"),
    duration = 15, type = "warning")
  output$dl_docx <- downloadHandler(filename = function() paste0(file_stub(), ".docx"),
                                    content = function(file) { make_docx(st(), file, layout_breaks()); remind_save() })
  save_handler <- downloadHandler(filename = function() paste0(file_stub(), "_progress.json"),
                                  content = function(file) {
                                    writeLines(state_json(st()), file, useBytes = TRUE)
                                    showNotification("Progress file saved. Upload it on the Start tab any time to keep editing.", type = "message", duration = 8)
                                  })
  output$save_json <- save_handler
  output$save_json2 <- save_handler
  output$dl_pdf <- downloadHandler(filename = function() paste0(file_stub(), ".pdf"),
                                   content = function(file) file.copy(rv$pdf, file, overwrite = TRUE))
  outputOptions(output, "dl_pdf", suspendWhenHidden = FALSE)
  observeEvent(input$make_pdf, {
    s <- st(); html <- cv_standalone(s)
    ok <- withProgress(message = "Making your PDF...", value = .3, {
      hf <- tempfile(fileext = ".html"); writeLines(enc2utf8(html), hf, useBytes = TRUE)
      pf <- tempfile(fileext = ".pdf")
      ok <- html_to_pdf(hf, pf)
      if (!ok) { setProgress(.6, detail = "Trying LibreOffice"); df <- tempfile(fileext = ".docx"); make_docx(s, df, layout_breaks()); ok <- docx_to_pdf(df, pf) }
      if (ok) rv$pdf <- pf
      ok
    })
    if (ok) { session$sendCustomMessage("clickDownload", "dl_pdf"); remind_save() }
    else {
      session$sendCustomMessage("printCV", html)
      showNotification("Opening your browser's print window. Choose \"Save as PDF\" as the printer.", type = "message", duration = 8)
    }
  })
  observeEvent(input$print_cv, session$sendCustomMessage("printCV", cv_standalone(st())))
}

shinyApp(ui, server)
