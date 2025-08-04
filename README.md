# GCuber

GCuber is an R package for computing forest **volume**, **biomass**, and **carbon** at the tree level using **species-specific allometric equations**. It includes functions for equation validation, bark thickness estimation, prediction interval calculation, and domain checks.

---

## Installation

You can install the development version of GCuber from [GitHub](https://github.com/Antonin-Caussin/GCuber) using:

```R
# Install devtools if necessary
install.packages("devtools")

# Install GCuber from GitHub
devtools::install_github("Antonin-Caussin/GCuber")
```

---

##  Main Features

### Core Functions

* `carbofor()`: Main wrapper function that integrates preprocessing, volume, biomass, and carbon calculations
* `calculate_volume()`: Estimates tree volume using species-specific allometric models
* `calculate_biomass()`: Computes aboveground biomass using either equations or volume × density
* `calculate_carbon()`: Converts total biomass to carbon stock (default ratio = 0.47)

### Preprocessing and Validation

* `as.carbofor_data()`: Converts user data into the standard format for GCuber workflows
* `validate_parameters()`: Ensures required columns and formats are present for volume and biomass calculations
* `detect_specimens_type()`: Automatically detects the type of specimen identifier (code, abbreviation, full name)
* `establish_species_correspondence()`: Matches species names between user input and equation database

### Diameter and Basal Area Handling

* `diameter_conversions()`: Converts between diameter and circumference, and between C130 and C150
* `convert_circumference()`: Harmonizes circumference measures using height-specific coefficients (IV, HV)
* `calculate_basal_areas()`: Computes basal area at 130 or 150 cm for each tree

### Advanced Volume & Biomass Components

* `calculate_bark_thickness()`: Estimates bark thickness and separates total volume into bark and wood components
* `calculate_prediction_interval()`: Computes relative width and reliability of prediction intervals around estimated volume

### Utilities

* `evaluate_expression()`: Safely evaluates mathematical expressions defined in equation metadata

---

##  Example

```R
library(GCuber)

trees <- data.frame(
  Species   = c("Hetre", "Chene pedoncule", "Epicea commun"),
  D130      = c(32, 40, 28),
  HTOT      = c(25, 30, 22),
  C130      = c(100.5, 125.7, 88.0),
  HDOM      = c(27, 31, 23)
)

result <- carbofor(
  x            = trees,
  volume_type  = "V22",
  carbon       = TRUE,
  bark         = TRUE,
  biomass_method = "equation",
  source       = "Dagnelie",
  specimens = Species
)

print(resultat[, c("Species", "D130", "V22", "Biomass_Total", "Carbon_Total")])

```

---

##  Documentation

* All exported functions are documented with `?function_name` in R
* Additional usage examples and workflows will be provided in vignettes soon

---

## ️ Package Structure

* `R/`: Main function implementations (volume, biomass, bark, etc.)
* `tests/`: Unit tests using `{testthat}`
* `data/`: Internal datasets (e.g., sample equations)
* `man/`: Auto-generated documentation files


---

##  Testing

GCuber includes a comprehensive test suite powered by `testthat`. To run all tests:

```R
devtools::test()
```

---

## Author

**Antonin Caussin**
Universite de Liege – Gembloux Agro-Bio Tech
Email: [antonin.caussin@gmail.com](mailto:antonin.caussin@gmail.com)

---

##  Bug Reports & Feature Requests

Please use the [GitHub Issues](https://github.com/Antonin-Caussin/GCuber/issues) page to report bugs, request features, or ask questions.

---

##  License

This project is licensed under the **MIT License** – see the [LICENSE](LICENSE) file for details.

---

## ️ Development Status

🚧 GCuber is under active development. Contributions, suggestions, and pull requests are warmly welcome!

---

##  Related Projects

* [carbofor() pipeline](#): Function that integrates volume, biomass, and carbon calculations in a single step 
* Shiny app (in development): visual interface for processing CSV tree data with dynamic translation : https://antonincaussin.shinyapps.io/GCubeR/
