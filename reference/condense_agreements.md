# Condense similar treatyIDs

Different treatyIDs generated for different datasets might have minor
differences in terms of acronym or linkage. Some minor differences in
treatyIDs could mean different treatyIDs in different datasets actually
refer to the same agreement. The function finds these occurrences and
returns the first treatyID argument entered as a replacement.

## Usage

``` r
condense_agreements(datacube = NULL, idvar = NULL)
```

## Arguments

- datacube:

  A "many" package datacube

- idvar:

  Two or more treatyID variables

## Value

A dataframe of treatyID and treatyID references

## Examples

``` r
data1 <- data.frame(treatyID = c("CPV-PRT[FSD]_1980A",
"CPV-PRT[FSD]_1990P:FSD_1980A",
"TD06LJ_1981A", "RAMSA_1971A", "WIIEWH_1982P"))
data2 <- data.frame(treatyID = c("TD06LJ_1981A", "RAMSA_1971A",
"WIIEWH_1982P:RAMSA_1971A",
"PRTRPC_1976A", "PRTRPC_1983E1:PRTRPC_1976A"))
condense_agreements(idvar = c(data1$treatyID, data2$treatyID))
#> Joining with `by = join_by(match)`
#> Joining with `by = join_by(acronym, activity)`
#> # A tibble: 8 × 2
#>   treatyID                     manyID                      
#>   <chr>                        <chr>                       
#> 1 CPV-PRT[FSD]_1980A           CPV-PRT[FSD]_1980A          
#> 2 CPV-PRT[FSD]_1990P:FSD_1980A CPV-PRT[FSD]_1990P:FSD_1980A
#> 3 TD06LJ_1981A                 TD06LJ_1981A                
#> 4 RAMSA_1971A                  RAMSA_1971A                 
#> 5 WIIEWH_1982P                 WIIEWH_1982P:RAMSA_1971A    
#> 6 WIIEWH_1982P:RAMSA_1971A     WIIEWH_1982P:RAMSA_1971A    
#> 7 PRTRPC_1976A                 PRTRPC_1976A                
#> 8 PRTRPC_1983E1:PRTRPC_1976A   PRTRPC_1983E1:PRTRPC_1976A  
```
