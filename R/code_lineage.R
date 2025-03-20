#' Code lineage from agreement titles
#'
#' @param title A title column for agreements
#' @param datacube A datacube from the many packages ecosystem.
#' @return A list of lineages that combines agreement area
#' and agreement action.
#' @importFrom purrr map
#' @importFrom stringr str_squish
#' @importFrom stringi stri_trans_general
#' @examples
#' \dontrun{
#' code_lineage(title = sample(manyenviron::agreements$IEADB$Title, 30))
#' code_lineage(datacube = manyenviron::agreements)
#' }
#' @export
code_lineage <- function(title = NULL, datacube = NULL) {
  if (is.null(title) & is.null(datacube)) {
    stop("Please declare a title column or a many datacube")
  }
  # Get title variable from datacube, if available
  if (is.null(title)) {
    title <- unname(unlist(purrr::map(datacube, "Title")))
    vars <- unlist(purrr::map(datacube, names))
    if (any("Text" == vars)) { # Find text variable in datacube, if available
      txt <- unname(unlist(purrr::map(datacube, "Text")))
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
  lineage <- ifelse(is.na(entity), paste0(parties, " - ", domain),
                    paste0(entity, " - ", domain))
  lineage <- gsub("- NA|NULL", "", lineage)
  lineage <- trimws(gsub("^-", "", lineage))
  lineage
}

#' Code Agreement Entity
#'
#' @param title Treaty titles
#' @return The region of the agreement
#' @importFrom stringr str_squish
#' @examples
#' \dontrun{
#' title <- sample(manyenviron::agreements$IEADB$Title, 30)
#' code_entity(title)
#' }
#' @export
code_entity <- function(title) {
  # Add a note about JavaScript
  usethis::ui_info("Please make sure JavaScript is installed.")
  # Download entity package
  pkgs <- NULL
  pkgs <- data.frame(utils::installed.packages())
  if (any(grepl("entity", pkgs$Package))) {
    remotes::install_github("trinker/entity")
    usethis::ui_info("Downloaded entity package.")
  }
  # Make sure necessary model is available (adapted from entity package)
  outcome <- "openNLPmodels.en" %in% list.files(.libPaths())
  if (!outcome) {
    utils::install.packages(
      "http://datacube.wu.ac.at/src/contrib/openNLPmodels.en_1.5-1.tar.gz",
      repos = NULL,
      type = "source")
  }
  suppressWarnings(requireNamespace("entity", quietly = TRUE))
  # Code entity
  out <- suppressWarnings(entity::location_entity(title))
  # Code entity (using spacy for better results)
  # # Add a note about python
  # usethis::ui_info("Please make sure spacyr, minicinda, python, and spacy are installed.
  #                   This can be done by running 'spacyr::spacy_install()'")
  # spacyr::spacy_initialize()
  # out <- spacyr::entity_extract(spacyr::spacy_parse(title, entity = TRUE),
  #                       type = "named")
  #   dplyr::filter()
  #   dplyr::group_by(doc_id) %>%
  #   dplyr::summarise(entity_type = paste(entity_type, collapse = ", "),
  #                    entity = paste(gsub("_", " ", entity), collapse = ", "))
  # title <- data.frame(title)
  # title$doc_id <- paste0("text", as.numeric(rownames(title)))
  # out <- dplyr::left_join(title, out, by = "doc_id")
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
#' @param type Issue-type of agreement, either environment or health
#' @return The domain taken from agreement title
#' @importFrom dplyr case_when
#' @examples
#' \dontrun{
#' title <- sample(manyenviron::agreements$IEADB$Title, 30)
#' code_domain(title)
#' }
#' @export
code_domain <- function(title, type = c("environment", "health")) {
  if(type == "environment") {
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
  } else{
    domain <- dplyr::case_when(
      # For health treaties
      # Labour (ILO), Human Rights (Rights, Refugees, Euthanasia, Ethical Standards, Tehran Declaration)
      # Protection (Prisoners, Juvenile Justice, Women, Disabilities, Minorities, Older Persons/Ageing, Social Charter, Social and Medical Assistance)
      # Mental Health (Caracas Declaration), Prevention (Alcohol/Drugs/Tobacco, Diseases)
      # Healthcare (Medical care, Data files), Pollution (Waste, pollution, transboundary), Climate change (Environment)
      grepl("\\<ILO\\>|\\<labour\\>",
            title, ignore.case = T) ~  "labour",
      grepl("right|\\<refugees\\>|\\<euthanasia\\>|\\<ethical\\>|
          |tehran declaration|genocide|nuremberg|teheran",
            title, ignore.case = T) ~ "human rights",
      grepl("\\<protection\\>|prisoner|\\<juvenile\\>|beijing|
          |\\<child\\>|\\<women\\>|\\<disabilities\\>|disappearance|
          |\\<minorities\\>|\\<ageing\\>|older persons|female|
          |social charter|\\<assistance\\>|armed|geneva|\\<mines\\>|
          |torture|humane|\\<oas\\>|discrimination|traffic|conduct",
            title, ignore.case = T) ~ "protection",
      grepl("\\<alcohol\\>|\\<drugs\\>|\\<narcotic\\>|\\<tobacco\\>",
            title, ignore.case = T) ~  "prevention",
      grepl("\\<mental\\>|caracas declaration|mental health|
          |\\<psychosocial\\>",
            title, ignore.case = T) ~  "mental health",
      grepl("\\<waste\\>|pollut|\\<noise\\>|\\<toxic\\>|\\<hazard\\>|nuclear|
          |chemical|\\<mercury\\>|residual|corrosive|substances|transboundary",
            title, ignore.case = T) ~  "pollution",
      grepl("\\<medical\\>|data files|world health organization|alma ata|
          |population|",
            title, ignore.case = T) ~  "healthcare",
      grepl("climate|\\<rio\\>|\\<habitat\\>|\\<environment\\>|agenda 21",
            title, ignore.case = T) ~  "climate change")
    domain
  }
}
