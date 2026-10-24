# Working with 'many' treaties

## International Treaties

Some of the ‘many’ packages include textual data on international
agreements governing certain issue-domain (e.g. `{manytrade}` and
`{manyenviron}`). While [manydata](https://www.manydata.ch/) facilitates
accessing ‘many’ data, `{manypks}` helps developers to standardise and
work with treaty data.

Using [manydata](https://www.manydata.ch/), we can download and check
which datacubes and datasets are available in the packages.

``` r

library(manydata)
manydata::get_packages("manytrade")
manydata::data_contrast("manytrade")
manydata::data_contrast("manytrade", "agreements")
```

## Assigning a ‘many’ ID

One of the central problems in assembled data packages is identifying
equivalences between observations across datasets.

To solve this issue we created the
[`code_agreements()`](https://globalgov.github.io/manypkgs/reference/code_agreements.md)
function which extracts key identifying information from the titles and
dates, rendering the same treatyID for observations that have the same
key identifying information. Please see the
[Agreements](https://globalgov.github.io/manypkgs/articles/agreements.html)
vignette for more information on how
[`code_agreements()`](https://globalgov.github.io/manypkgs/reference/code_agreements.md)
works and its performance vis-a-vis other matching options.

``` r

trade_agreements <- lapply(manytrade::agreements, function(x)
  x[x$Beg > "1995-01-01" & x$Beg < "2000-01-01", ])
trade_agreements <- lapply(trade_agreements, function(x)
  dplyr::select(x, Title, Beg))
trade_agreements <- purrr::map_df(trade_agreements, ~as.data.frame(.x),
                                  .id = "Dataset")
trade_agreements <- trade_agreements[complete.cases(trade_agreements), ]
trade_agreements$treatyID <- manypkgs::code_agreements(
  title = trade_agreements$Title, date = trade_agreements$Beg)
```

Different treatyIDs generated with
[`code_agreements()`](https://globalgov.github.io/manypkgs/reference/code_agreements.md)
for different datasets in a datacube might contain minor differences
that could mean these IDs actually refer to the same agreement. The
[`condense_agreements()`](https://globalgov.github.io/manypkgs/reference/condense_agreements.md)
function to works to match these inconsistencies within datacubes or
variables.

``` r

manyID <- manypkgs::condense_agreements(idvar = trade_agreements$treatyID)
trade_agreements <- dplyr::left_join(trade_agreements, manyID, by = "treatyID") %>%
  dplyr::mutate(compareID = ifelse(treatyID == manyID, "Equal", "Different"))
summary(as.factor(trade_agreements$compareID))
#> Different     Equal 
#>        13       872
```

## Standardising observations

Before analysing the text data contained in the datasets, standardising
observations to ensure that they have the same format (e.g. in terms of
spacing) can help make automated text analysis more convenient and
straightforward. The data in `{manytrade}` and `{manyenviron}` were
standardised using functions in
[manypkgs](https://github.com/globalgov/manypkgs) before being exported.
Treaty titles in the ‘many’ datasets were first standardised using
`standardise_titles()`. Then, `standardise_treaty_texts()` was used to
annotate the different segments of treaties (e.g. preamble, articles,
and annexes) and removes redundant paragraph, line, and website markers
as much as possible.

## Coding and analysing treaty texts

The ‘many’ packages contain datacubes that store the original texts of
treaties, where available digitally, listed in the `agreements`,
`memberships`, and `references` datacubes.
[manypkgs](https://github.com/globalgov/manypkgs) has several functions
to standardize and extract information from these texts. At its most
basic, `read_clauses()` extracts specified portions of treaties, such as
the ‘preamble’ or the ‘accession’ clause. To reduce the size of the
output produced, `read_clauses()` matches a limited number of words
only. This is currently set to a maximum of 20 words for accession
articles, since these clauses are usually shorter than this length.

``` r

trade_treaties <- manytrade::agreements$HUGGO %>%
  dplyr::filter(Beg > "2000-01-01" & Beg < "2006-01-01",
                !is.na(TreatyText), !grepl("NULL", TreatyText)) %>%
  dplyr::select(manyID, Title, Beg, TreatyText)
manypkgs::read_clauses(trade_treaties$TreatyText, article = "preamble")
# needs to improve...
manypkgs::read_clauses(trade_treaties$TreatyText, article = "accession")
manypkgs::read_clauses(trade_treaties$TreatyText, treaty_type = "agreements")
manypkgs::read_clauses(trade_treaties$TreatyText, match = "human rights")
```

### Code accession and termination clauses

With the standardized treaty text in hand, we can leverage functions in
[manypkgs](https://github.com/globalgov/manypkgs) to code accession and
termination rules for treaties. `code_accession_terms()` locates the
conditions and processes for accession to a treaty (depending on which
is specified in the argument). Accession conditions are categorised
either as ‘open’, where the treaty is open to any government, or
‘semi-open’, where nomination by an existing party to the treaty is
required. The function identifies the category by matching keywords
located in the clauses concerning accession (identified through
`read_clauses()`: ‘any government’, ‘all governments’, etc for ‘open’
and ‘nomination’ for ‘semi-open’.

``` r

manypkgs::code_accession_terms(trade_treaties$TreatyText) # All NAs?
manypkgs::code_term(trade_treaties$TreatyText)
```

## Retrieve ‘many’ additional information

[manydata](https://www.manydata.ch/) contains several functions to help
easily retrieve additional from ‘many’ treaties.

We can, for example, retrieve certain types of treaties with
[`retrieve_bilaterals()`](https://www.manydata.ch/reference/defunct.html)
and
[`retrieve_multilaterals()`](https://www.manydata.ch/reference/defunct.html).

``` r

manydata::retrieve_bilaterals(manytrade::memberships$DESTA_MEM, actor = "stateID")
manydata::retrieve_multilaterals(manytrade::memberships$GPTAD_MEM)
```

We can also retrieve treaty linkages with
[`retrieve_links()`](https://www.manydata.ch/reference/defunct.html).

``` r

manydata::retrieve_links(manytrade::agreements)
```

Or, even, retrieve treaty membership lists with
`retrieve_memberships()`.

``` r

manydata::retrieve_membership_list(manytrade::memberships$DESTA_MEM, "stateID")
```
