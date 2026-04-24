This repository contains all scripts associated with the following publication:

- J. Böhm and P. Schrag, ‘pam: An R Package for Fast and Efficient Processing of Pulse‐Amplitude Modulation Data’, Ecology and Evolution, vol. 16, no. 4, p. e73400, Apr. 2026, doi: [10.1002/ece3.73400](https://www.researchgate.net/publication/404020183_pam_An_R_Package_for_Fast_and_Efficient_Processing_of_Pulse-Amplitude_Modulation_Data).

### Abstract
Rapid light curves recorded via the pulse‐amplitude modulation (PAM) technique are widely used to characterize photosynthesis, enabling the determination of key photosynthetic parameters.
However, deriving these kinetic parameters from raw data requires fitting to regression models, a process traditionally involving laborious and error‐prone manual steps.
Our R package pam streamlines this process by automating regression analysis, enabling fast and reproducible processing of large datasets.
It provides the models of Vollenweider (1965), Platt et al. (1980), Eilers and Peeters (1988) and Walsby (1997).
To demonstrate the functionality of the package, we present a workflow including data reading, model fitting, data validation, and data export using a dataset of 20 files and compare the results with those obtained from the built‐in WALZ Solver and the Excel Solver add‐on.
The workflow successfully processed all data, produced reliable results, and executed in less than 20 s, making it several times faster than traditional approaches, such as using the Excel Solver add‐on.
This R package and its source code are freely available on GitHub at https://github.com/biotoolbox/pam and CRAN at https://cran.r‐project.org/web/packages/pam/index.html.
