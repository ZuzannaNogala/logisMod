
# projektROR

The goal of projektROR is to provide all necessary tools for data
analisys using logistic regression.

Our package supplies method for creating logistic regresion model. It
consists of all needed methods to help visualise dependencies between
variables. Let us compare two models with each other. Also with use of
AIC and BIC, for small data tables (a few variables), we can compare all
possible models. Our package also supplies method for K-Fold Cross
Validation. Using this package you can plot ROC curve and compute AUC,
too. One of its features is also possibility to compute and plot
predicted probabilities based on created model.

Package contains two exemplary data sets to get to know it better.

## Installation

You can install the development version of projektROR from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("ZuzannaNogala/projektROR", build_vignettes = TRUE)
```

## Data

First data set is `citrus`. This data set takes the color, weight, and
diameter of an “average” orange and grapefruit, which we can be used to
predict what type of fruit we have.

``` r
library(projektROR)
head(citrus)
#>      name diameter weight red green blue nameBin
#> 1: orange     2.96  86.76 172    85    2       0
#> 2: orange     3.91  88.05 166    78    3       0
#> 3: orange     4.42  95.17 156    81    2       0
#> 4: orange     4.47  95.60 163    81    4       0
#> 5: orange     4.48  95.76 161    72    9       0
#> 6: orange     4.59  95.86 142   100    2       0
```

Second one is a set of information about bank clients and status of
their loan payoff. Contains status of loan payoff from one month and
information such total income per year or education type, to use as
potential predictors in logistic regression.

``` r
head(creditData)
#>         id code_gender age flag_own_car flag_own_realty cnt_children
#> 1: 5008804           M  33            Y               Y            0
#> 2: 5008805           M  33            Y               Y            0
#> 3: 5008806           M  59            Y               Y            0
#> 4: 5008810           F  53            N               Y            0
#> 5: 5008811           F  53            N               Y            0
#> 6: 5008814           F  62            N               Y            0
#>    amt_income_total     name_income_type           name_education_type
#> 1:           427500              Working              Higher education
#> 2:           427500              Working              Higher education
#> 3:           112500              Working Secondary / secondary special
#> 4:           270000 Commercial associate Secondary / secondary special
#> 5:           270000 Commercial associate Secondary / secondary special
#> 6:           283500            Pensioner              Higher education
#>      name_family_status name_housing_type days_employed flag_mobil
#> 1:       Civil marriage  Rented apartment          4542          1
#> 2:       Civil marriage  Rented apartment          4542          1
#> 3:              Married House / apartment          1134          1
#> 4: Single / not married House / apartment          3051          1
#> 5: Single / not married House / apartment          3051          1
#> 6:            Separated House / apartment       -365243          1
#>    flag_work_phone flag_phone flag_email occupation_type cnt_fam_members status
#> 1:               1          0          0    lack of data               2      1
#> 2:               1          0          0    lack of data               2      1
#> 3:               0          0          0  Security staff               2      1
#> 4:               0          1          1     Sales staff               1      1
#> 5:               0          1          1     Sales staff               1      1
#> 6:               0          0          0    lack of data               1      0
```

## Model building

Making model in this package is using `stats::glm` function with
particular arguments, but it also gives the model another class, which
enables us to use specific methods.

``` r
logisMod(nameBin ~ diameter, citrus)
```

## More Details

If you want to learn more about this package and see how all of the
methods work look at our html vignette.

``` r
browseVignettes("projektROR")
```
