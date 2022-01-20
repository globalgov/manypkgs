#' Code lineage from agreement titles
#'
#' @param title A title column for agreements
#' @param database A database from the many packages ecosystem.
#' @return A list of lineages that combines agreement area
#' and agreement action.
#' @importFrom purrr map
#' @importFrom stringr str_squish
#' @examples
#' \dontrun{
#' code_lineage(title = sample(manyenviron::agreements$IEADB$Title, 30))
#' code_lineage(database = manyenviron::texts)
#' }
#' @export
code_lineage <- function(title = NULL, database = NULL) {
  if (is.null(title) & is.null(database)) {
    stop("Please declare a title column or a many database")
  }
  # Get title variable from database, if available
  if (is.null(title)) {
    title <- unname(unlist(purrr::map(database, "Title")))
    vars <- unlist(purrr::map(database, names))
    if (any("Text" == vars)) { # Find text variable in database, if available
      txt <- unname(unlist(purrr::map(database, "Text")))
      txt <- get_articles(txt, "preamble")
    }
  }
  # code entity and actions for titles
  entity <- code_entity(title)
  action <- code_actions(title)
  parties <- code_parties(title)
  # Get entity and actions from preamble if missing from title
  if (exists("txt")) {
    entity <- ifelse(is.na(entity), code_entity(txt), entity)
    action <- ifelse(is.na(action), code_actions(txt), action)
  }
  # Paste all together
  lineage <- ifelse(is.na(entity), paste0(parties, " - ", action), paste0(entity, " - ", action))
  lineage <- gsub("- NA|NULL", "", lineage)
  lineage <- trimws(gsub("^-", "", lineage))
  lineage
}

#' Code Agreement Entity
#'
#' @param title Treaty titles
#' @return The region of the agreement
#' @importFrom entity location_entity
#' @importFrom stringr str_squish
#' @examples
#' \donttest{
#' title <- sample(manyenviron::agreements$IEADB$Title, 30)
#' code_entity(title)
#' }
#' @export
code_entity <- function(title) {
  # MAke sure necessary model is available (adapted from entity package)
  outcome <- "openNLPmodels.en" %in% list.files(.libPaths())
  if (!outcome) {
    utils::install.packages(
      "http://datacube.wu.ac.at/src/contrib/openNLPmodels.en_1.5-1.tar.gz",
      repos=NULL,
      type="source")
  }
  # Code entity
  out <- entity::location_entity(title)
  # Remove states
  parties <- paste(countrynames$c, collapse = "|")
  out <- gsub(parties, "", out, ignore.case = TRUE)
  out <- gsub("^c|Britain|England", "", out)
  out <- gsub("[^[:alnum:]]", " ", out)
  out <- stringr::str_squish(out)
  out <- gsub("NULL", NA_character_, out)
  out <- ifelse(grepl("^$", out), NA_character_, out)
  out
}

#' Code Actions from agreement titles
#'
#' @param title Treaty titles
#' @return The action taken from agreement title
#' @importFrom dplyr case_when
#' @examples
#' \donttest{
#' title <- sample(manyenviron::agreements$IEADB$Title, 30)
#' code_actions(title)
#' }
#' @export
code_actions <- function(title) {
  actions <- dplyr::case_when(
    # For environmental treaties
    grepl("\\<biodiversity\\>|\\<species\\>|\\<habitat\\>|\\<ecosystems\\>|biological diversity|genetic resources|\\<biosphere\\>",
          title, ignore.case = T) ~ "biodiversity",
    grepl("\\<air\\>|atmos|\\<climate\\>|outer space|\\<ozone\\>|\\<emissions\\>|\\<coal\\>", title, ignore.case = T) ~ "climate change",
    grepl("\\<legal\\>|\\<enforcement\\>|\\<policy\\>|\\<planning\\>|\\<institution\\>|\\<dispute\\>|\\<court\\>|\\<tribunal\\>|\\<law\\>",
          title, ignore.case = T) ~ "management",
    grepl("\\<energy\\>|\\<nuclear\\>|\\<oil\\>|\\<mining\\>|\\<gas\\>|hydro|\\<power\\>", title, ignore.case = T) ~  "energy",
    grepl("agricultur|\\<food\\>|\\<livestock\\>|\\<crop\\>|\\<crops\\>|\\<irrigation\\>|\\<cattle\\>|\\<meat\\>|\\<farm\\>|\\<cultivate\\>|\\<poultry\\>",
          title, ignore.case = T) ~  "agriculture",
    grepl("\\<waste\\>|pollut|\\<noise\\>|\\<toxic\\>|\\<hazard\\>", title, ignore.case = T) ~  "waste",
    grepl("\\<culture\\>|scien|techno|\\<trade\\>|\\<research\\>|\\<exploration\\>|\\<navigation\\>|\\<data\\>|\\<information\\>",
          title, ignore.case = T) ~  "research",
    grepl("\\<weapon\\>|\\<military\\>|\\<proliferation\\>|nuclear material", title, ignore.case = T) ~  "military",
    grepl("\\<alliance\\>|\\<peace\\>|mutual defense|\\<defence\\>|\\<friendship\\>|\\<allied\\>|non-agression", title, ignore.case = T) ~  "alliance",
    grepl("\\<police\\>|\\<security\\>|\\<terrorism\\>", title, ignore.case = T) ~  "security",
    grepl("fish|\\<salmon\\>|\\<herring\\>|\\<tuna\\>|\\<aquaculture\\>|\\<mariculture\\>|\\<molluscs\\>", title,
          ignore.case = T) ~  "fishing",
    grepl("forest|\\<tree\\>", title, ignore.case = T) ~  "forestry", 
    grepl("financ|\\<fund\\>|\\<funding\\>|\\<loan\\>|\\<lease\\>|\\<debt\\>", title, ignore.case = T) ~  "finance",
    grepl("\\<trade\\>|\\<tariff\\>|\\<tax\\>|\\<exchange\\>|\\<business\\>", title, ignore.case = T) ~ "trade",
    grepl("economic union|economic community|free trade|common market|economic partnership|economic cooperation|economic zone", title, ignore.case = T) ~  "economic integration",
    grepl("\\<invest\\>|\\<BIT\\>|\\<BITs\\>|\\<Tips\\>", title, ignore.case = T) ~ "investment",
    grepl("human rights|refugee|\\<genocide\\>|\\<discrimination\\>|cultural rights|political rights|\\<torture\\>|\\<indigenous\\>|\\<migrants\\>|\\<disabilities\\>|\\<stateless\\>|geneva convention|\\<mines\\>", title, ignore.case = T) ~ "human rights",
    grepl("\\<health\\>|disease|\\<tobacco\\>|\\<asbestos\\>|\\<chemicals\\>|\\<mercury\\>|\\<nursing\\>|\\<cancer\\>|\\<radiation\\>|\\<accidents\\>", title, ignore.case = T) ~ "health",
    grepl("\\<boundary\\>|\\<territorial\\>|\\<delimitation\\>|\\<frontiers\\>|\\<border\\>|\\<limits\\>", title, ignore.case = T) ~ "delimitation",
    grepl("outer space|\\<moon\\>|\\<satellite\\>|space liability|space station", title, ignore.case = T) ~ "space")
  actions
}

#' Get links from treaty titles
#'
#' Treaties that modify, amend, or expand other treaties
#' usually specify so in the title.
#' We use the qIDs generated by `code_agreements()` and
#' `condense_agreements()` to return a data frame of treaty
#' links for a database.
#' @param database A Package database
#' @param dataset A Package dataset
#' @param treaty_type The type of treaties to be returned.
#' By default, all treaties are returned.
#' Other options include bilateral or multilateral treaties. 
#' @return A dataframe of agreements' qID and their linkages.
#' @importFrom purrr map map_chr
#' @examples
#' \dontrun{
#' get_links(database = manyenviron::agreements)
#' get_links(database = manyenviron::agreements, treaty_type = "multilateral")
#' get_links(dataset = manyenviron::agreements$IEADB, treaty_type = "bilateral")
#' samples <- lapply(manyenviron::agreements,
#' function(x) x[x$Beg > "1991-12-31" & x$Beg < "1993-01-01", ])
#' migraph::gglineage(get_links(samples))
#' }
#' @export
get_links <- function(database, dataset, treaty_type = "all") {
  # Get qID
  if (!missing(database)) {
    qID <- unname(unlist(purrr::map(database, "qID_ref")))
  }
  if (!missing(dataset)) {
    qID <- dataset$qID_ref
  }
  usethis::ui_done("qID column in dataset automatically found")
  # Filter by links
  qID <- grep(":", qID, value = TRUE)
  # Filter by treaty_type
  if (treaty_type == "bilateral") {
    qID <- grep("-", qID, value = TRUE)
  }
  if (treaty_type == "multilateral") {
    qID <- grep("-", qID, value = TRUE, invert = TRUE)
  } 
  # Split qID
  link <- purrr::map_chr(strsplit(qID, ":"), 2)
  agreement <- purrr::map_chr(strsplit(qID, ":"), 1)
  # Return dataset
  out <- data.frame(agreement, link)
  out
}
