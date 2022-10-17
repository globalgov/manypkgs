# {{{dataset}}} Preparation Script

# <Please add more information about {{{dataset}}} here 
# if it is an original dataset.>

# This is a template for importing, cleaning, and exporting data
# ready for many packages universe.

# Stage one: Collecting data
{{{dataset}}} <- {{{import_type}}}("{{{path}}}")

# Stage two: Correcting data
# In this stage you will want to correct the variable names and
# formats of the '{{{dataset}}}' object until the object created
# below (in stage three) passes all the tests.
# We recommend that you avoid using one letter variable names to keep
# away from issues with ambiguous names down the road.
{{{dataset}}} <- as_tibble({{{dataset}}}) %>%
  manydata::transmutate(ID = {id_variable_name_here},
                        Beg = messydates::as_messydate({date_variable_name_here})) %>%
  dplyr::arrange(Beg)

# Remove duplicates and ensure NAs are coded correctly
{{{dataset}}} <- {{{dataset}}} %>%
  mutate(across(everything(), ~stringr::str_replace_all(., "^NA$", NA_character_))) %>% 
  mutate(Beg = messydates::as_messydate(Beg),
         Signature = messydates::as_messydate(Signature),
         Force = messydates::as_messydate(Force)) %>% 
  dplyr::distinct(.keep_all = TRUE)

# manypkgs includes several functions that should help with
# cleaning and standardising your data
# such as `standardise_titles()` and `standardise_texts()`.
# Please see the vignettes or website for more details.

# Stage three: Connecting data
# Next run the following line to make {{{dataset}}} available
# within the package.
# This function also does two additional things.
# First, it creates a set of tests for this object to ensure adherence
# to certain standards.
# You can hit Cmd-Shift-T (Mac) or Ctrl-Shift-T (Windows)
# to run these tests locally at any point.
# Any test failures should be pretty self-explanatory
# and may require you to return to stage two and further clean,
# standardise, or wrangle your data into the expected format.
# Second, it also creates a documentation file for you to fill in.
# Please note that the export_data() function requires a .bib file to be
# present in the data_raw folder of the package for citation purposes.
# Therefore, please make sure that you have permission to use the dataset
# that you're including in the package.
# To add a template of .bib file to the package,
# please run `manypkgs::add_bib("{{{database}}}", "{{{dataset}}}")`.
manypkgs::export_data({{{dataset}}}, database = "{{{database}}}",
                     URL = NULL)
