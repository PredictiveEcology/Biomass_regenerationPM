# Biomass_regenerationPM (development version)

# Biomass_regenerationPM 0.3.0

This release updates the module for current versions of the SpaDES and LandR tools. It now reads and writes maps with the terra package instead of the retired raster package, and follows the LandR development branch.

Earlier fixes from 2023 reach the main branch: cohorts that die, survive or regenerate after a fire are grouped correctly before the forest is updated, and total biomass is recalculated afterwards. The module also gains automatic tests that run on every change.

Changes before 2021-11-18 are not included.
