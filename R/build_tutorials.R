# Builds the learnr tutorials in inst/tutorials/ of the package that the user
# works in. It builds two things for each tutorial:
# - the prerendered HTML that learnr serves when the reader runs run_tute()
# - a static, non-interactive pkgdown article in vignettes/articles/
#
# Ported from {manynet}'s data-raw/build_tutorial_articles.R (via {netrics})
# so that the stocnet packages share one article-generation transform.
# It replaces the earlier one-off data-raw/tute2article.py, which hard-coded
# absolute paths and so could not be run in CI.
#
# The articles are deliberately a *preview*, not a substitute for the
# interactive tutorial: code output and exercise solutions are withheld so
# that readers are motivated to install the package and run `run_tute()`.
#
# What the transform does to each source tutorial:
# - drops `runtime: shiny_prerendered` and switches to a plain html_document
# - drops `library(learnr)`, and sets `echo = TRUE, eval = FALSE` so the
#   demonstration code is shown but not run (no output is displayed).
#   Inline `r gloss()`/`r print_glossary()` calls still evaluate, since
#   `eval = FALSE` only applies to fenced chunks, so the glossary and the
#   hover-over term definitions still work.
# - for `exercise = TRUE` chunks with a real body (demonstrations), keeps
#   the code as an ordinary (non-evaluated) chunk
# - for blank exercise chunks (the reader was meant to type the answer),
#   drops the chunk entirely — the solution is never shown
# - drops `<label>-hint*`, `<label>-solution`, and `<label>-setup` scaffold
#   chunks (learnr-only)
# - drops learnr context chunks (those with a `context=` option, e.g. the
#   `context="server-start"` language-detection chunk), which are meaningless
#   outside the interactive runtime
# - replaces `question()`/`quiz()` chunks with a static callout pointing
#   back to the interactive tutorial (`run_tute()`)
# - keeps chunks labelled `explain-*` evaluated but hidden (`echo = FALSE,
#   eval = TRUE`): these draw the explanatory figures that belong to the
#   prose rather than to the exercises, so the preview shows them too

#' Building and editing the tutorials of a package
#'
#' @description
#'   `build_tutorials()` rebuilds every learnr tutorial in `inst/tutorials/`
#'   of the package that you work in, for example `{manynet}`, `{autograph}`,
#'   or `{netrics}`.
#'   For each tutorial it builds:
#'
#'   - the prerendered HTML file that learnr serves, next to the source `.Rmd`,
#'   and the folder of figures that this file links to, if the tutorial draws any
#'   - a static article in `vignettes/articles/` for the pkgdown website
#'
#'   The articles are a preview of the tutorial and not a replacement for it.
#'   The transform shows the demonstration code but does not run it, drops
#'   blank exercise chunks and their hints and solutions, and replaces each
#'   quiz with a callout that points back to `run_tute()`.
#'   The only chunks it runs are those with a label that starts with `explain-`.
#'   These chunks draw the figures that explain the text,
#'   so the article shows their output and hides their code.
#'
#'   `edit_tutorial()` opens in RStudio the learnr tutorial in
#'   `inst/tutorials/` of the package in the working directory
#'   whose name is nearest to `tute`.
#' @param path Path to the root of the package.
#'   By default this is the working directory.
#' @param tutorials A character vector that selects the tutorials to build.
#'   Each element matches the tutorial folder name or the file name,
#'   for example "netrics1" or "centrality".
#'   By default all the tutorials are built.
#' @param html Logical.
#'   By default `TRUE`, which rebuilds the prerendered HTML of each tutorial.
#'   Set this to `FALSE` to build the articles only.
#' @param articles Logical.
#'   By default `TRUE`, which rebuilds the static article of each tutorial.
#'   Set this to `FALSE` to build the HTML only.
#' @details
#'   The name of the article file comes from the article in
#'   `vignettes/articles/` that already holds the title of the tutorial.
#'   If there is no such article, the name comes from the title itself.
#'   For example the title "Making Network Data" gives
#'   `vignettes/articles/making-network-data.Rmd`.
#'
#'   A tutorial that draws figures outside its exercises, such as in the
#'   `explain-` chunks, links to them in a `<tutorial>_files/` folder next to
#'   the HTML file. The tutorial does not show these figures without the folder,
#'   so commit the folder together with the HTML file.
#'   Do not embed such figures in the HTML file as base64 data:
#'   learnr replaces an image with a video player
#'   where the data of the image contains the text "v/".
#' @param tute The name of a tutorial, or of its folder.
#'   A name that starts with "tut" is matched to the folder names,
#'   and any other name to the file names.
#' @return `build_tutorials()` returns a character vector of the paths of
#'   the files and figure folders that are built,
#'   returned invisibly.
#'
#'   `edit_tutorial()` returns the path to the tutorial, invisibly.
#' @examples
#' \dontrun{
#' build_tutorials()
#' build_tutorials(tutorials = "centrality", html = FALSE)
#' edit_tutorial("centrality")
#' }
#' @name tutorials
#' @importFrom usethis ui_done ui_info
#' @export
build_tutorials <- function(path = ".", tutorials = NULL,
                            html = TRUE, articles = TRUE) {
  tute_dir <- file.path(path, "inst", "tutorials")
  if (!dir.exists(tute_dir))
    stop("There is no inst/tutorials/ folder in ", normalizePath(path), ".")
  srcs <- list.files(tute_dir, pattern = "\\.Rmd$",
                     recursive = TRUE, full.names = TRUE)
  if (length(srcs) == 0) stop("There are no tutorials in ", tute_dir, ".")
  if (!is.null(tutorials)) srcs <- select_tutorials(srcs, tutorials)
  built <- character(0)
  for (src in srcs) {
    if (html) built <- c(built, build_tutorial_html(src))
    if (articles) built <- c(built, build_tutorial_article(src, path))
  }
  usethis::ui_done(paste0("Built ", length(built), " file(s) from ",
                          length(srcs), " tutorial(s)."))
  invisible(built)
}

select_tutorials <- function(srcs, tutorials) {
  folders <- basename(dirname(srcs))
  files <- sub("\\.Rmd$", "", basename(srcs))
  keep <- folders %in% tutorials | files %in% tutorials
  if (!any(keep))
    stop("None of the tutorials match ", paste(tutorials, collapse = ", "),
         ". The available tutorials are ",
         paste(paste0(folders, "/", files), collapse = ", "), ".")
  srcs[keep]
}

build_tutorial_html <- function(src) {
  if (!requireNamespace("rmarkdown", quietly = TRUE))
    stop("Please install the rmarkdown package to build the tutorial HTML.")
  usethis::ui_info(paste0("Rendering ", src))
  rmarkdown::shiny_prerendered_clean(src)
  out <- rmarkdown::render(src, quiet = TRUE)
  usethis::ui_done(paste0("Built ", out))
  # The render writes the figures drawn outside the exercises to this folder.
  # The HTML links to them by path, so the folder belongs with the HTML.
  figures <- sub("\\.html$", "_files", out)
  if (dir.exists(figures)) {
    usethis::ui_info(paste0("Built ", figures, "/ (",
                            length(list.files(figures, recursive = TRUE)),
                            " figure file(s)): commit it with the HTML"))
    out <- c(out, figures)
  }
  out
}

build_tutorial_article <- function(src, path) {
  out_path <- article_path(src, path)
  build_article(src, out_path)
  usethis::ui_done(paste0("Built ", out_path))
  out_path
}

# The article keeps the name it already has, so that the links of the
# pkgdown website do not break. A new tutorial gets a name from its title.
article_path <- function(src, path) {
  art_dir <- file.path(path, "vignettes", "articles")
  title <- rmd_title(readLines(src, warn = FALSE))
  existing <- list.files(art_dir, pattern = "\\.Rmd$", full.names = TRUE)
  for (art in existing) {
    if (identical(rmd_title(readLines(art, warn = FALSE)), title)) return(art)
  }
  file.path(art_dir, paste0(slugify(title), ".Rmd"))
}

rmd_title <- function(lines) {
  line <- grep("^title:", lines, value = TRUE)[1]
  if (is.na(line)) return(NA_character_)
  trimws(gsub('"', "", sub("^title:\\s*", "", line)))
}

slugify <- function(title) {
  slug <- tolower(title)
  slug <- gsub("[^a-z0-9]+", "-", slug)
  gsub("^-|-$", "", slug)
}

parse_rmd <- function(lines) {
  chunk_start_re <- "^```\\{r[ ]*([A-Za-z0-9_.-]*)[ ]*(,\\s*(.*))?\\}\\s*$"
  chunks <- list()
  segments <- list()
  i <- 1
  n <- length(lines)
  while (i <= n) {
    m <- regmatches(lines[i], regexec(chunk_start_re, lines[i]))[[1]]
    if (length(m) > 0) {
      label <- m[2]
      opts <- if (length(m) >= 4) m[4] else ""
      j <- i + 1
      while (j <= n && trimws(lines[j]) != "```") j <- j + 1
      body <- if (j > i + 1) lines[(i + 1):(j - 1)] else character(0)
      chunks[[length(chunks) + 1]] <- list(label = label, opts = opts, body = body)
      segments[[length(segments) + 1]] <- list(type = "chunk", idx = length(chunks))
      i <- j + 1
    } else {
      start <- i
      while (i <= n) {
        m2 <- regmatches(lines[i], regexec(chunk_start_re, lines[i]))[[1]]
        if (length(m2) > 0) break
        i <- i + 1
      }
      segments[[length(segments) + 1]] <- list(type = "prose", lines = lines[start:(i - 1)])
    }
  }
  list(chunks = chunks, segments = segments)
}

strip_exercise_opts <- function(opts) {
  parts <- if (nzchar(opts)) strsplit(opts, ",\\s*")[[1]] else character(0)
  parts <- parts[!grepl("^exercise(\\.setup)?\\s*=", parts)]
  paste(parts, collapse = ", ")
}

quiz_callout <- paste0(
  '::: {.callout}\n',
  '<span class="callout-label">**Try it yourself**:</span> ',
  "This section includes an interactive quiz in the live tutorial \u2014 ",
  "run `run_tute()` at the R console to try it.\n",
  ':::'
)

build_article <- function(src_path, out_path) {
  lines <- readLines(src_path, warn = FALSE)
  parsed <- parse_rmd(lines)
  chunks <- parsed$chunks
  labels <- vapply(chunks, function(x) x$label, character(1))

  is_quiz <- vapply(chunks, function(x) {
    grepl("\\b(question|quiz)\\s*\\(", paste(x$body, collapse = "\n"))
  }, logical(1))

  base_of_hint <- sub("-hint(-[0-9]+)?$", "", labels)
  is_hint <- grepl("-hint(-[0-9]+)?$", labels) & (base_of_hint %in% labels) & !is_quiz

  base_of_solution <- sub("-solution$", "", labels)
  is_solution_scaffold <- grepl("-solution$", labels) & (base_of_solution %in% labels) & !is_quiz

  is_setup_scaffold <- labels != "setup" & grepl("-setup$", labels) & !is_quiz

  # learnr runtime chunks (e.g. context="server-start") make no sense in a
  # static article, so drop them like the other scaffolding
  chunk_opts <- vapply(chunks, function(x) x$opts, character(1))
  is_context <- grepl("context\\s*=", chunk_opts) & !is_quiz

  is_scaffold <- is_hint | is_solution_scaffold | is_setup_scaffold | is_context

  # keep demonstration chunks (a real body); drop blank exercises entirely
  # so their solutions are never revealed in the static preview
  resolved_body <- vector("list", length(chunks))
  drop_primary <- logical(length(chunks))
  for (k in seq_along(chunks)) {
    if (is_quiz[k] || is_scaffold[k] || labels[k] == "setup") next
    body <- chunks[[k]]$body
    if (all(trimws(body) == "")) {
      drop_primary[k] <- TRUE
    } else {
      resolved_body[[k]] <- body
    }
  }

  render_chunk <- function(k) {
    if (is_scaffold[k]) return(NULL)
    label <- chunks[[k]]$label
    if (label == "setup") {
      body <- chunks[[k]]$body
      body <- body[!grepl("^\\s*library\\(learnr\\)\\s*$", body)]
      body <- sub("echo\\s*=\\s*FALSE", "echo = TRUE, eval = FALSE", body)
      return(c(paste0("```{r ", label,
                       if (nzchar(chunks[[k]]$opts)) paste0(", ", chunks[[k]]$opts) else "",
                       "}"), body, "```"))
    }
    if (is_quiz[k]) return(strsplit(quiz_callout, "\n")[[1]])
    if (drop_primary[k]) return(NULL)
    opts <- strip_exercise_opts(chunks[[k]]$opts)
    # explanatory figures are part of the prose, so they are drawn (and their
    # code hidden) even though the rest of the article is not evaluated
    if (grepl("^explain-", label)) {
      opts <- paste(c(if (nzchar(opts)) opts, "echo = FALSE, eval = TRUE"),
                    collapse = ", ")
    }
    header <- paste0("```{r ", label, if (nzchar(opts)) paste0(", ", opts) else "", "}")
    c(header, resolved_body[[k]], "```")
  }

  out <- character(0)
  last_was_quiz <- FALSE
  for (seg in parsed$segments) {
    if (seg$type == "prose") {
      if (last_was_quiz && all(trimws(seg$lines) == "")) next  # blank gap between quizzes
      out <- c(out, seg$lines)
      last_was_quiz <- FALSE
    } else {
      k <- seg$idx
      if (is_quiz[k] && last_was_quiz) next  # collapse consecutive quiz callouts
      rendered <- render_chunk(k)
      if (!is.null(rendered)) out <- c(out, rendered)
      last_was_quiz <- is_quiz[k]
    }
  }

  text <- paste(out, collapse = "\n")
  text <- gsub("\n{3,}", "\n\n\n", text)  # tidy up gaps left by dropped scaffold chunks

  # YAML: drop the learnr/shiny_prerendered output, use a plain html_document
  text <- sub(
    "output:\\n  learnr::tutorial:\\n    theme: flatly\\nruntime: shiny_prerendered",
    "output: rmarkdown::html_document",
    text
  )

  # banner pointing back to the interactive version, right before the
  # first section heading
  title <- rmd_title(lines)
  banner <- paste0(
    '::: {.callout}\n',
    '<span class="callout-label">**Static preview**:</span> ',
    "This is a static, read-only preview of the \"", title, "\" tutorial. ",
    "To keep it a preview, the code output and exercise solutions are not ",
    "shown here, and the quizzes are replaced with notes like this one. ",
    "Install the package and run `run_tute()` at the R console to work ",
    "through the tutorial interactively \u2014 running the code, seeing the ",
    "results, and getting hints, solutions, and quizzes.\n",
    ':::'
  )
  text <- sub("\\n(## )", paste0("\n\n", banner, "\n\n\\1"), text)

  dir.create(dirname(out_path), showWarnings = FALSE, recursive = TRUE)
  writeLines(text, out_path)
  invisible(text)
}

#' @rdname tutorials
#' @importFrom rstudioapi navigateToFile
#' @export
edit_tutorial <- function(tute){
  tutes <- list.files("inst/tutorials", recursive = TRUE)
  tutes <- tutes[grepl(".Rmd$", tutes)]
  if(grepl("^tut", tute)){
    opts <- vapply(strsplit(tutes,"/"), `[`, 1, FUN.VALUE=character(1))
  } else {
    opts <- vapply(strsplit(tutes,"/"), `[`, 2, FUN.VALUE=character(1))
  }
  opt <- tutes[which.min(utils::adist(tute, opts, ignore.case = TRUE,
                                      costs = list(ins=0, del=1, sub=1)))]
  pth <- paste0("inst/tutorials/", opt)
  if(rstudioapi::isAvailable())
    rstudioapi::navigateToFile(pth)
  invisible(pth)
}
