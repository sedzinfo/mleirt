# mleirt

<!-- badges: start -->
<!-- badges: end -->

## Overview

**mleirt** is an R package that provides interactive Shiny applications for demonstrating Maximum Likelihood Estimation (MLE), Expected A Posteriori (EAP), and Maximum A Posteriori (MAP) estimation methods in Item Response Theory (IRT). The package offers visual and interactive tools for understanding parameter estimation in various IRT models, including the Rasch model, 2PL (Two-Parameter Logistic), and 3PL (Three-Parameter Logistic) models. Individual plots were originally developed by Metin Bulus and Wes Bonifay, whose work forms the foundation of the visualizations included here.

## Features

- **Interactive Shiny Interface**: Explore IRT concepts through dynamic visualizations
- **Multiple IRT Models**: 
  - Rasch Model (person location estimation)
  - 2PL Model (item discrimination and difficulty estimation)
  - 3PL Model (item discrimination, difficulty, and guessing parameter estimation)
- **Parameter Estimation Methods**:
  - Maximum Likelihood Estimation (MLE)
  - Expected A Posteriori (EAP)
  - Maximum A Posteriori (MAP)
- **Real-time Visualization**: Interactive 3D plots showing likelihood surfaces and parameter relationships
- **Educational Tool**: Ideal for teaching and learning IRT fundamentals

## Installation

You can install the development version of mleirt from GitHub:

```r
# Install devtools if you haven't already
install.packages("devtools")

# Install mleirt
devtools::install_github("sedzinfo/mleirt")
```

## Dependencies

The package requires:
- shiny
- shinydashboard
- htmltools

These will be automatically installed when you install mleirt.

## Usage

Launch the interactive Shiny application with a single command:

```r
library(mleirt)
mleirt()
```

This will open an interactive dashboard in your default web browser with multiple tabs:

### MLE Rasch Tab
Demonstrates person location estimation using MLE in the Rasch model. Features include:
- Adjust raw scores (0-5)
- Set starting values for the estimation
- Step through iterations to see how MLE converges
- Visualize likelihood functions

### MLE 2PL Tab
Explores item parameter estimation in the 2PL model with:
- Interactive discrimination and difficulty parameter controls
- 3D likelihood surface visualization
- Rotatable plots to examine the likelihood from different angles

### MLE 3PL Tab
Extends to the 3PL model, incorporating guessing parameters alongside discrimination and difficulty estimation.

## Examples

```r
# Launch the main application
library(mleirt)
mleirt()

# The application will open in your browser
# Navigate between tabs to explore different IRT models
# Adjust sliders to see how parameters affect the likelihood functions
```

## Educational Context

This package is particularly useful for:
- Students learning Item Response Theory
- Instructors teaching psychometric methods
- Researchers exploring IRT model behavior
- Anyone interested in understanding MLE, EAP, and MAP estimation visually

The implementations replicate examples from educational resources, including tables from de Ayala (2009).

## Credits

- **Package Development**: Dimitrios Zacharatos
- **Original Plot Development**: Metin Bulus & Wes Bonifay

## License

GPL (>= 2)

## References

- Bulus, M., & Bonifay, W. (2022). irtDemo R Package: Pedagogical Interactive Web Applications for Estimation, Scoring, and Multi Dimensionality in Item Response Theory. Anadolu University Journal of Education Faculty, 6(1), 92-108. https://doi.org/10.34056/aujef.913781
- de Ayala, R. J. (2009). *The Theory and Practice of Item Response Theory*. Guilford Press.

## Contact

For questions, issues, or contributions, please contact:
- Dimitrios Zacharatos: dzacharatos@yahoo.com
- GitHub Issues: https://github.com/sedzinfo/mleirt/issues

## Contributing

Contributions are welcome! Please feel free to submit a Pull Request.

