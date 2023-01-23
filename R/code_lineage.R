#' Code lineage from agreement titles
#'
#' @param title A title column for agreements
#' @param database A database from the many packages ecosystem.
#' @return A list of lineages that combines agreement area
#' and agreement action.
#' @importFrom purrr map
#' @importFrom stringr str_squish
#' @importFrom stringi stri_trans_general
#' @examples
#' \dontrun{
#' code_lineage(title = sample(manyenviron::agreements$IEADB$Title, 30))
#' code_lineage(database = manyenviron::agreements)
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
      txt <- read_clauses(standardise_treaty_text(txt), "preamble")
    }
  }
  # code entity and actions for titles
  title <- stringi::stri_trans_general(title, id = "Latin-ASCII")
  entity <- code_entity(title)
  domain <- code_domain(title)
  parties <- code_states(title)
  # Get entity and actions from preamble if missing from title
  if (exists("txt")) {
    entity <- ifelse(is.na(entity), code_entity(txt), entity)
    domain <- ifelse(is.na(domain), code_domain(txt), domain)
  }
  # Paste all together
  lineage <- ifelse(is.na(entity), paste0(parties, " - ", domain), paste0(entity, " - ", domain))
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
  # Add a note about JavaScript
  usethis::ui_info("Please make sure JavaScript is installed (https://www.java.com/en/)")
  # Make sure necessary model is available (adapted from entity package)
  outcome <- "openNLPmodels.en" %in% list.files(.libPaths())
  if (!outcome) {
    utils::install.packages(
      "http://datacube.wu.ac.at/src/contrib/openNLPmodels.en_1.5-1.tar.gz",
      repos = NULL,
      type = "source")
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

#' Code domains from agreement titles
#'
#' @param title Treaty titles
#' @return The domain taken from agreement title
#' @importFrom dplyr case_when
#' @examples
#' \donttest{
#' title <- sample(manyenviron::agreements$IEADB$Title, 30)
#' code_domain(title)
#' }
#' @export
code_domain <- function(title) {
  domain <- dplyr::case_when(
    # For environmental treaties
    grepl("\\<biodiversity\\>|\\<species\\>|\\<habitat\\>|
          |\\<ecosystems\\>|biological diversity|environment|
          |genetic resources|\\<biosphere\\>|forest|\\<tree\\>|plant",
          title, ignore.case = T) ~  "environment",
    grepl("\\<air\\>|atmos|\\<climate\\>|outer space|rising temperature|
          |\\<ozone\\>|\\<emissions\\>|\\<coal\\>|global warming|
          |clean energy|renewable",
          title, ignore.case = T) ~ "climate change",
    grepl("\\<legal\\>|\\<enforcement\\>|\\<policy\\>|
          |\\<planning\\>|\\<institution\\>|\\<dispute\\>|
          |\\<court\\>|\\<tribunal\\>|\\<law\\>|settlement",
          title, ignore.case = T) ~ "law enforcement",
    grepl("\\<energy\\>|nuclear power|nuclear energy|\\<oil\\>|\\<mining\\>|
          |\\<gas\\>|hydro|\\<power\\>|generator|transmission lines",
          title, ignore.case = T) ~  "energy",
    grepl("agricultur|\\<food\\>|\\<livestock\\>|\\<crop\\>|
          |\\<crops\\>|\\<irrigation\\>|\\<cattle\\>|productivity|
          |\\<meat\\>|\\<farm\\>|\\<cultivate\\>|\\<poultry\\>|pesticide",
          title, ignore.case = T) ~  "agriculture",
    grepl("\\<waste\\>|pollut|\\<noise\\>|\\<toxic\\>|\\<hazard\\>|
          |chemical|\\<mercury\\>|residual|corrosive|substances",
          title, ignore.case = T) ~  "waste",
    grepl("\\<culture\\>|scien|techno|\\<trade\\>|\\<research\\>|
          |\\<knowledge\\>|\\<data\\>|\\<information\\>|survey",
          title, ignore.case = T) ~  "research",
    grepl("weapon|\\<military\\>|\\<proliferation\\>|
          |\\<police\\>|\\<security\\>|\\<terrorism\\>|
          |nuclear material|defense|mutual defense>|mass destruction",
          title, ignore.case = T) ~  "defense",
    grepl("alliance|peace|\\<friendship\\>|\\<allied\\>|
          |non-agression|non agression", title, ignore.case = T) ~  "peace",
    grepl("fish|\\<salmon\\>|\\<herring\\>|\\<tuna\\>|
          |\\<aquaculture\\>|\\<mariculture\\>|
          |\\<molluscs\\>|whaling", title, ignore.case = T) ~  "fishing",
    grepl("financ|\\<fund\\>|\\<funding\\>|\\<loan\\>|\\<lease\\>|
          |\\<debt\\>", title, ignore.case = T) ~  "finance",
    grepl("\\<trade\\>|\\<tariff\\>|\\<tax\\>|\\<exchange\\>|
          |\\<business\\>|economic union|economic community|free trade|
          |common market|economic partnership|
          |economic cooperation|economic zone|
          \\<invest\\>|\\<BIT\\>|\\<BITs\\>|\\<Tips\\>",
          title, ignore.case = T) ~ "trade",
    grepl("human rights|refugee|\\<genocide\\>|\\<discrimination\\>|
          |cultural rights|political rights|\\<torture\\>|\\<indigenous\\>|
          |\\<migrants\\>|\\<disabilities\\>|\\<stateless\\>|
          |geneva convention|\\<mines\\>|migration|peoples",
          title, ignore.case = T) ~ "human rights",
    grepl("\\<health\\>|disease|\\<tobacco\\>|\\<asbestos\\>|
          |\\<nursing\\>|\\<cancer\\>|\\<COVID\\>|hospital|poison|
          |\\<radiation\\>|\\<accidents\\>|medicine|medical|
          medication", title, ignore.case = T) ~ "health",
    grepl("boundar|\\<territorial\\>|delimit|
          |frontier|\\<border\\>|\\<limit\\>|\\<limits\\>",
          title, ignore.case = T) ~ "territorial boundaries",
    grepl("outer space|\\<moon\\>|\\<satellite\\>|space liability|
          |space station|mars", title, ignore.case = T) ~ "outer space")
  domain
}
