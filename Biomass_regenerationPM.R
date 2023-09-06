# Everything in this file gets sourced during simInit, and all functions and objects
# are put into the simList. To use objects, use sim$xxx, and are thus globally available
# to all modules. Functions can be used without sim$ as they are namespaced, like functions
# in R packages. If exact location is required, functions will be: sim$<moduleName>$FunctionName
defineModule(sim, list(
  name = "Biomass_regenerationPM",
  description = paste("Post-disturbance biomass regeneration module for LandR. Simulates post-fire partial cohort mortality,",
                      "regeneration and serotiny as part of the same event - all occurring sequentially immediately after fire.",
                      "Mortality depends of fire severity, and follows mechanisms in LANDIS-II Dynamic Fire System v3.0.",
                      "Serotiny and regeneration algorithms taken from LANDIS-II Biomass Succession extension, v3.2.1"),
  keywords = c("biomass regeneration", "LandR", "disturbance", "mortality", "vegetation succession", "vegetation model"),
  authors = person("Ceres", "Barros", email = "cbarros@mail.ubc.ca", role = c("aut", "cre")),
  childModules = character(0),
  version = list(Biomass_regenerationPM = "0.2.0"),
  spatialExtent = raster::extent(rep(NA_real_, 4)),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year",
  citation = list("citation.bib"),
  documentation = list("README.txt", "Biomass_regenerationPM.Rmd"),
  reqdPkgs = list("crayon", "data.table", "raster", ## TODO: update package list!
                  "PredictiveEcology/LandR@LIM (>= 1.0.7.9026)",
                  "PredictiveEcology/pemisc@development"),
  parameters = rbind(
    defineParameter("calibrate", "logical", FALSE, desc = "Do calibration? Defaults to FALSE"),
    defineParameter("fireInitialTime", "numeric", NA,
                    desc = "The event time that the first fire disturbance event occurs"),
    defineParameter("fireTimestep", "numeric", NA,
                    desc = "The number of time units between successive fire events in a fire module"),
    defineParameter("initialB", "numeric", 10, 1, NA, desc = "initial biomass values of new age-1 cohorts"),
    defineParameter("LANDISPM", "logical", TRUE,
                    desc = "Use LANDIS-II version of partial fire-driven mortality? See LANDIS-II Dynamic Fire System v3.0"),
    defineParameter("successionTimestep", "numeric", 10L, NA, NA, "defines the simulation time step, default is 10 years")
  ),
  inputObjects = bindrows(
    expectsInput("cohortData", "data.table",
                 desc = "age cohort-biomass table hooked to pixel group map by pixelGroupIndex at
                 succession time step"),
    expectsInput("fireDamageTable", "data.table",
                 desc = paste("data.table defining upper age limit of cohorts killed by fire depending on the",
                 "species' fire tolerance values - 'species$firetolerance'. From LANDIS-II Dynamic Fire System v3.0 Manual")),
    expectsInput("fireCFBRas", "RasterLayer",
                 desc = "Raster of crown fraction burnt"),
    expectsInput("fireROSRas", "RasterLayer",
                 desc = "Raster of equilibrium rate of spread [m/min]"),
    expectsInput("fireRSORas", "RasterLayer",
                 desc = "Critical spread rate for crowning [m/min]"),
    expectsInput("inactivePixelIndex", "logical",
                 desc = "internal use. Keeps track of which pixels are inactive"),
    expectsInput("pixelGroupMap", "RasterLayer",
                 desc = "updated community map at each succession time step"),
    expectsInput("rstCurrentBurn", "RasterLayer",
                 desc = "Binary raster of fires, 1 meaning 'burned', 0 or NA is non-burned"),
    expectsInput("species", "data.table",
                 desc = "a table that has species traits such as longevity...",
                 sourceURL = "https://raw.githubusercontent.com/LANDIS-II-Foundation/Extensions-Succession/master/biomass-succession-archive/trunk/tests/v6.0-2.0/species.txt"),
    expectsInput("speciesEcoregion", "data.table",
                 desc = "table defining the maxANPP, maxB and SEP, which can change with both ecoregion and simulation time",
                 sourceURL = "https://raw.githubusercontent.com/LANDIS-II-Foundation/Extensions-Succession/master/biomass-succession-archive/trunk/tests/v6.0-2.0/biomass-succession-dynamic-inputs_test.txt"),
    expectsInput("sufficientLight", "data.frame",
                 desc = "table defining how the species with different shade tolerance respond to stand shadeness",
                 sourceURL = "https://raw.githubusercontent.com/LANDIS-II-Foundation/Extensions-Succession/master/biomass-succession-archive/trunk/tests/v6.0-2.0/biomass-succession_test.txt"),
    expectsInput("treedFirePixelTableSinceLastDisp", "data.table",
                 desc = "3 columns: pixelIndex, pixelGroup, and burnTime. Each row represents a forested pixel that was burned up to and including this year, since last dispersal event, with its corresponding pixelGroup and time it occurred")
  ),
  outputObjects = bindrows(
    createsOutput("cohortData", "data.table",
                  desc = paste("age cohort-biomass table hooked to pixel group map",
                               "by pixelGroupIndex at succession time step")),
    createsOutput("lastFireYear", "numeric",
                  desc = "Year of the most recent fire year"),
    createsOutput("pixelGroupMap", "RasterLayer",
                  desc = "updated community map at each succession time step"),
    createsOutput("postFireRegenSummary", "data.table",
                  desc = "summary table of species post-fire regeneration"),
    createsOutput("serotinyResproutSuccessPixels", "numeric",
                  desc = "Pixels that were successfully regenerated via serotiny or resprouting. This is a subset of treedBurnLoci"),
    createsOutput("severityBMap", "RasterLayer",
                  desc = "A map of fire severity, as in the amount of post-fire mortality (biomass loss)"),
    createsOutput("severityData", "data.table",
                  desc = "A data.table of pixel fire severity, as in the amount of post-fire mortality (biomass loss).
                  May also have severity class used to calculate mortality."),
    createsOutput("treedFirePixelTableSinceLastDisp", "data.table",
                  desc = "3 columns: pixelIndex, pixelGroup, and burnTime. Each row represents a forested pixel that was burned up to and including this year, since last dispersal event, with its corresponding pixelGroup and time it occurred")
  )
))

## event types
#   - type `init` is required for initialiazation

doEvent.Biomass_regenerationPM <- function(sim, eventTime, eventType) {
  switch(
    eventType,
    init = {
      # do stuff for this event
      sim <- Init(sim)

      ## schedule events
      sim <- scheduleEvent(sim, P(sim)$fireInitialTime,
                           "Biomass_regenerationPM", "fireDisturbance",
                           eventPriority = 3)
    },
    fireDisturbance = {
      if (!is.null(sim$rstCurrentBurn)) {
        sim <- FireDisturbance(sim)
      } else {
        message(crayon::red(paste0("The Biomass_regenerationPM module is expecting sim$rstCurrentBurn;\n",
                                   "Currently, it does not exist, so no regeneration will happen")))
      }
      sim <- scheduleEvent(sim, time(sim) + P(sim)$fireTimestep,
                           "Biomass_regenerationPM", "fireDisturbance",
                           eventPriority = 3)
    },
    warning(paste("Undefined event type: '", current(sim)[1, "eventType", with = FALSE],
                  "' in module '", current(sim)[1, "moduleName", with = FALSE], "'", sep = ""))
  )
  return(invisible(sim))
}

### template initialization
Init <- function(sim) {
  ## check parameters
  if (is.na(P(sim)$fireInitialTime))
    stop(paste("Please provide a value for `P(sim)$fireInitialTime`.",
               "It should match the first year of fire."))
  if (is.na(P(sim)$fireTimestep))
    stop(paste("Please provide a value for `P(sim)$fireTimestep`.",
               "It should match the fire time step (fire frequency)."))
  return(invisible(sim))
}

## Fire disturbance regeneration event
FireDisturbance <- function(sim, verbose = getOption("LandR.verbose", TRUE)) {

  ## as in B_core
  if (!suppliedElsewhere("columnsForPixelGroups", sim, where = "sim")) {
    columnsForPixelGroups <- LandR::columnsForPixelGroups
  } else {
    columnsForPixelGroups <- sim$columnsForPixelGroups
  }

  # the presence of valid fire can cause three processes:
  # 1. partially remove species cohorts from the pixels that have been affected.
  # 2. initiate the post-fire regeneration (serotiny and/or resprouting)
  # 3. change of cohortdata and pixelgroup map

  ## checks
  ## partial mortality needs the following objects
  if (any(!suppliedElsewhere("fireRSORas", sim),
          !suppliedElsewhere("fireROSRas", sim),
          !suppliedElsewhere("fireCFBRas", sim))) {
    message(crayon::red(paste0("Biomass_regenerationPM is missing one/several of the following rasters:\n",
                               "  fireRSORas, fireROSRas and fireCFBRas.\n",
                               "  DUMMY RASTERS will be used - if this is not intended, please \n",
                               "  use a fire module that provides them (e.g. fireSpread)")))
    vals <- getValues(sim$rstCurrentBurn)
    valsRSO <- valsROS <- valsCFB <- integer(0)
    valsRSO[!is.na(vals)] <- as.integer(round(runif(sum(!is.na(vals)), 0, 100)))
    valsROS[!is.na(vals)] <- as.integer(round(runif(sum(!is.na(vals)), 0, 100)))
    valsCFB[!is.na(vals)] <- runif(sum(!is.na(vals)), 0, 1)
    fireRSORas <- setValues(sim$rstCurrentBurn, valsRSO)
    fireROSRas <- setValues(sim$rstCurrentBurn, valsROS)
    fireCFBRas <- setValues(sim$rstCurrentBurn, valsCFB)
  } else {
    ## create copies, so that when dummies need to be used
    ## they are not detected in sim, but can still be updated using
    ## sim$rstCurrentBurn
    fireRSORas <- sim$fireRSORas
    fireROSRas <- sim$fireROSRas
    fireCFBRas <- sim$fireCFBRas
  }

  if (isTRUE(getOption("LandR.assertions", TRUE))) {
    if (!identical(NROW(sim$cohortData), NROW(unique(sim$cohortData, by = c("pixelGroup", "speciesCode", "age", "B"))))) {
      stop("sim$cohortData has duplicated rows, i.e., multiple rows with the same pixelGroup, speciesCode and age")
    }
  }

  ## make table where survivor, serotiny and resprouting cohorts will be stored
  postFirePixelCohortData <- sim$cohortData[0,]
  postFirePixelCohortData[, `:=`(pixelIndex = integer(),
                                 age = NULL, B = NULL, mortality = NULL,
                                 aNPPAct = NULL)]

  # In some cases sumB exists, but not always -- we want to remove it too here.
  if (isTRUE("sumB" %in% colnames(postFirePixelCohortData))) {
    set(postFirePixelCohortData, NULL, "sumB", NULL)
  }

  if (P(sim)$calibrate & is.null(sim$postFireRegenSummary)) {   # don't overwrite
    sim$postFireRegenSummary <- data.table(year = numeric(),
                                           regenMode = character(),
                                           species = character(),
                                           numberOfRegen = numeric())
  }

  ## extract burn pixel indices/groups and remove potentially inactive pixels
  burnedLoci <- which(getValues(sim$rstCurrentBurn) > 0)
  treedBurnLoci <- if (length(sim$inactivePixelIndex) > 0) {
    # These can burn other vegetation (grassland, wetland)
    burnedLoci[!(burnedLoci %in% sim$inactivePixelIndex)] # this is to prevent evaluating the pixels that are inactive
  } else {
    burnedLoci
  }

  treedFirePixelTableSinceLastDisp <- data.table(pixelIndex = as.integer(treedBurnLoci),
                                                 pixelGroup = as.integer(getValues(sim$pixelGroupMap)[treedBurnLoci]),
                                                 burnTime = time(sim))

  ## TODO: Ceres: I don't think we should be bring in the previously burnt pixelGroups at this point
  ##  solution (?) code was ciopy-paste to before the export to sim
  # ## update past pixelGroup number to match current ones.
  # sim$treedFirePixelTableSinceLastDisp[, pixelGroup := as.integer(getValues(sim$pixelGroupMap))[pixelIndex]]
  # # append previous year's
  # treedFirePixelTableSinceLastDisp <- rbindlist(list(sim$treedFirePixelTableSinceLastDisp,
  #                                                    treedFirePixelTableSinceLastDisp))

  ## make table spp/ecoregionGroup/age in burnt pixels
  burnedPixelCohortData <- sim$cohortData[pixelGroup %in% unique(treedFirePixelTableSinceLastDisp$pixelGroup)]
  # set(burnedPixelCohortData, NULL, c("B", "mortality", "aNPPAct"), NULL)
  # set(burnedPixelCohortData, ,c("sumB", "siteShade"), 0) # assume the fire burns all cohorts on site
  setkey(burnedPixelCohortData, speciesCode)

  ## DO MORTALITY ----------------------------
  ## TODO: MOVE SEVERITY ESTIMATES TO A SEVERITY MODULE (?)
  ## LANDIS-II Dynamic Fire System v3.0
  ## estimate fire severity from crown fraction burnt (CFB),
  ## equilibrium head fire rate of spread (ROS) and critical spread rate for crowning (RSO)

  ## select the pixels that have potential survivors and assess them
  burnedPixelTable <- treedFirePixelTableSinceLastDisp[pixelGroup %in% unique(burnedPixelCohortData$pixelGroup)]

  ## from now on the survivor process is assessed per pixel (to ensure compatibility with serotiny/resprout tables)
  burnedPixelCohortData <- burnedPixelTable[burnedPixelCohortData, allow.cartesian = TRUE,
                                            nomatch = 0, on = "pixelGroup"]

  severityData <- data.table(pixelIndex = 1:ncell(sim$pixelGroupMap),
                             pixelGroup = getValues(sim$pixelGroupMap),
                             burntPixels = getValues(sim$rstCurrentBurn),
                             RSO = getValues(fireRSORas),
                             ROS = getValues(fireROSRas),
                             CFB = getValues(fireCFBRas))
  severityData <- na.omit(severityData)

  severityData[CFB < 0.1 & ROS < (RSO + 0.458)/2, severity := 1]
  severityData[CFB < 0.1 & ROS >= (RSO + 0.458)/2, severity := 2]
  severityData[CFB >= 0.1 & CFB < 0.495, severity := 3]
  severityData[CFB >= 0.495 & CFB < 0.9, severity := 4]
  severityData[CFB >= 0.9, severity := 5]

  ## rm unnecessary cols
  severityData <- severityData[, .(pixelIndex, pixelGroup, severity)]

  ## add severity to survivor table.
  if (isTRUE(getOption("LandR.assertions", TRUE))) {
    if (!all(burnedPixelCohortData$pixelGroup %in% severityData$pixelGroup)) {
      warning("Some burnt pixels no fire behaviour indices or severity.\n",
              "Please debug Biomass_regenerationPM::fireDisturbance")
    }
  }

  burnedPixelCohortData <- severityData[burnedPixelCohortData,
                                        on = .(pixelGroup, pixelIndex),
                                        allow.cartesian = TRUE]

  ## DO MORTALITY -----------------------------
  ## Highest severity kills all cohorts
  burnedPixelCohortData[severity == 5, `:=` (B = 0, mortality = 0, aNPPAct = 0)]

  if (P(sim)$LANDISPM) {
    ## FIRE DAMAGE ---------------------------
    ## 1) Calculate site fire damage per cohort/species as severity - fire tolerance
    ## 2) lookup the fire damage values on the fire damage table to decide cohort ages to kill
    ## 3) for values that are not on the table, either the cohort/species is not affected or its totally killed

    ## add fire tolerance and longevity
    burnedPixelCohortData <- burnedPixelCohortData[sim$species[, .(speciesCode, longevity, firetolerance)],
                                                   on = "speciesCode", nomatch = 0]
    ## calculate dif between severity and tolerance
    burnedPixelCohortData[, severityToleranceDif := severity - firetolerance]
    assertFireToleranceDif(burnedPixelCohortData)

    ## find the % reduction in biomass:
    ## agesKilled w/ NAs come from observed severityToleranceDif having no matches in table,
    ## so they are beyond the range of values
    ## if the observed severityToleranceDif is higher than table values, then the fire damage is maximum
    ## if the observed severityToleranceDif is lower than table values, then there is no fire damage
    burnedPixelCohortData <- sim$fireDamageTable[burnedPixelCohortData, on = "severityToleranceDif",
                                                 nomatch = NA]

    if (isTRUE(getOption("LandR.assertions", TRUE))) {
      if (!all(is.na(burnedPixelCohortData[(severityToleranceDif > max(sim$fireDamageTable$severityToleranceDif) &
                                         severityToleranceDif < min(sim$fireDamageTable$severityToleranceDif)),
                                      agesKilled])))
        stop("The join of fireDamageTable and burnedPixelCohortData went wrong. agesKilled should be NA
             for site fire damage values outside the range of values in fireDamageTable")
    }

    ## add extreme values
    burnedPixelCohortData[severityToleranceDif > max(sim$fireDamageTable$severityToleranceDif),
                          agesKilled := 1.0]
    burnedPixelCohortData[severityToleranceDif < min(sim$fireDamageTable$severityToleranceDif),
                          agesKilled := 0.0]

    ## and kill cohorts below longevity * prop. - still not partial cohort mortality
    ## but partial stand mortality  -- a lot are being killed because longevities are so large now (disparity from landis)
    burnedPixelCohortData[age/longevity <= agesKilled,
                          `:=`(B = 0, mortality = 0, aNPPAct = 0)]

    ## remove unnecessary cols, but keep dead cohorts for serotiny/resprouting
    cols <- c("pixelGroup", "pixelIndex", "speciesCode",
              "ecoregionGroup", "age", "B", "mortality", "aNPPAct")
    burnedPixelCohortData <- burnedPixelCohortData[, ..cols]

    ## calculate amount of biomass lost per pixel and make severityMap
    ## add biomass-based severity to severityData
    severityData2 <- calcSeverityB(sim$cohortData, burnedPixelCohortData)
    cols <- c("pixelGroup", "pixelIndex")
    severityData <- severityData[severityData2, on = cols]

    ## make severity map
    severityBMap <- setValues(sim$rasterToMatch, rep(NA, ncell(sim$rasterToMatch)))
    severityBMap[severityData$pixelIndex] <- severityData$severityB
  } else {
    ## TODO MAYBE KEEP THE SAME SEVERITY NOTION, BUT THEN USE cfb TO DETERMINE AMOUNT OF BIOMASS
    ## REMOVED PER COHORT ON AN INVERSE AGE WEIGHTED AWAY
    ## USE SPECIES TRAITS TO WEIGHT BIOMASS REMOVAL WITHIN EACH COHORT
  }

  ## CALCULATE SIDE SHADE -----------------------------
  siteShade <- data.table(calcSiteShade(currentTime = round(time(sim)), burnedPixelCohortData,
                                        sim$speciesEcoregion, sim$minRelativeB))
  siteShade <- siteShade[, .(pixelGroup, siteShade)]
  burnedPixelCohortData <- siteShade[burnedPixelCohortData, on = "pixelGroup", nomatch = NA]
  burnedPixelCohortData[is.na(siteShade), siteShade := 0]
  rm(siteShade)

  ## clean burnedPixelCohortData from unnecessary columns
  # set(burnedPixelCohortData, NULL, c("B", "mortality", "aNPPAct"), NULL)
  set(burnedPixelCohortData, NULL, c("mortality", "aNPPAct"), NULL)
  # set(burnedPixelCohortData, ,c("sumB", "siteShade"), 0) # assume the fire burns all cohorts on site
  setkey(burnedPixelCohortData, speciesCode)

  ## DO SEROTINY -----------------------------
  ## assess potential serotiny reg: add sexual maturity to the table and compare w/ age
  ## as long as one cohort is sexually mature, serotiny is activated
  serotinyOutputs <- doSerotiny(burnedPixelCohortData = burnedPixelCohortData,
                                species = sim$species, currentTime = time(sim),
                                treedFirePixelTableSinceLastDisp = treedFirePixelTableSinceLastDisp,
                                sufficientLight = sim$sufficientLight,
                                speciesEcoregion = sim$speciesEcoregion,
                                calibrate = P(sim)$calibrate,
                                postFirePixelCohortData = postFirePixelCohortData,
                                postFireRegenSummary = sim$postFireRegenSummary)

  postFirePixelCohortData <- serotinyOutputs$postFirePixelCohortData
  serotinyPixel <- serotinyOutputs$serotinyPixel

  if (!is.null(serotinyOutputs$postFireRegserotinyOuputs))
    sim$postFireRegserotinyOuputs <- serotinyOutputs$postFireRegserotinyOuputs

  rm(serotinyOutputs)

  ## DO RESPROUTING --------------------------
  ## assess resprouting reproduction:
  ## basically same thing as serotiny
  resproutingOutputs <- doResprouting(serotinyPixel = serotinyPixel,
                                      treedFirePixelTableSinceLastDisp = treedFirePixelTableSinceLastDisp,
                                      burnedPixelCohortData = burnedPixelCohortData,
                                      postFirePixelCohortData = postFirePixelCohortData,
                                      currentTime = time(sim), species = sim$species,
                                      sufficientLight = sim$sufficientLight,
                                      calibrate = P(sim)$calibrate,
                                      postFireRegenSummary = sim$postFireRegenSummary)

  postFirePixelCohortData <- resproutingOutputs$postFirePixelCohortData
  sim$serotinyResproutSuccessPixels <- resproutingOutputs$serotinyResproutSuccessPixels
  if (!is.null(resproutingOutputs$postFireRegserotinyOuputs))
    sim$postFireRegserotinyOuputs <- resproutingOutputs$postFireRegserotinyOuputs

  rm(resproutingOutputs)

  ## ADD NEW COHORTS -----------------------------
  ## add new cohorts to pixels where serotiny/regeneration were activated
  if (NROW(postFirePixelCohortData)) {
    ## redo post-fire pixel groups by adding the maxPixelGroup to their ecoregioMap values
    if (!is.null(sim$serotinyResproutSuccessPixels)) {

      # Add new cohorts to BOTH the sim$cohortData and sim$pixelGroupMap
      ## reclassify pixel groups as burnt (0L)
      if (verbose > 0)
        message(blue("Post serotiny and resprouting"))

      ## add the survivors cohorts to the serotiny/reprouting ones
      cols <- c("pixelGroup", "pixelIndex", "speciesCode", "ecoregionGroup", "age", "B")
      postFirePixelCohortData <- rbind(postFirePixelCohortData, burnedPixelCohortData[B > 0, ..cols],
                                       use.names = TRUE, fill = TRUE)
      postFirePixelCohortData[is.na(type), type := "survivor"]

      ## set ages to 1 here, because updateCohortData will only so so if there isn't an age column
      postFirePixelCohortData[is.na(age), age := 1L]

      ## filter cohortData to only have unburnt pixels -- this is not sufficient!!!
      ## in PGs where cohorts die in one but not other pixels, these cohorts from other pixels are added back where they were supposed to be removed.
      # unburnedPCohortData <- addPixels2CohortData(copy(sim$cohortData), sim$pixelGroupMap)
      # unburnedPCohortData <- unburnedPCohortData[!pixelIndex %in% treedFirePixelTableSinceLastDisp$pixelIndex]
      # set(unburnedPCohortData, NULL, "pixelIndex", NULL)  ## collapse pixel groups again
      # unburnedPCohortData <- unburnedPCohortData[!duplicated(unburnedPCohortData)]

      ## redo PGs in all burnt pixels
      ## 1) we need to create a table of unburt pixels and burnt pixels with dead and surviving cohorts,
      ## but not new cohorts (serotiny/resprout) -- these are added by updateCohortData
      ## 2) then remove dead cohorts for updateCohortData and redo PG
      ## the PGs need to be done twice otherwise, once to account for cohorts that only died in some but not all pixels of a given
      ## pixelGroup, and the second time to ensure that pixels that became similar after the death of some cohorts can
      ## be grouped together.

      unburnedPCohortData <- addPixels2CohortData(copy(sim$cohortData), sim$pixelGroupMap)
      unburnedPCohortData <- unburnedPCohortData[!pixelIndex %in% treedFirePixelTableSinceLastDisp$pixelIndex]
      newPCohortData <- rbind(unburnedPCohortData, burnedPixelCohortData, fill = TRUE)

      cd <- newPCohortData[, c("pixelIndex", columnsForPixelGroups), with = FALSE]
      newPCohortData[, pixelGroup := generatePixelGroups(cd, maxPixelGroup = 0L, columns = columnsForPixelGroups)]
      pixelGroupMap <- sim$pixelGroupMap
      pixelGroupMap[newPCohortData$pixelIndex] <- newPCohortData$pixelGroup

      if (isTRUE(getOption("LandR.assertions", TRUE))) {
        test <- setdiff(which(!is.na(pixelGroupMap[])), newPCohortData$pixelIndex)
        if (any(pixelGroupMap[test] != 0)) {
          stop("Bug in Biomass_regenerationPM: not all pixels are in the joint burnt and unburnt pixelCohortData table")
        }
      }

      ## remove dead cohorts and re-do pixelGroups
      newPCohortData <- newPCohortData[B > 0]
      columnsForPG <- c("ecoregionGroup", "speciesCode", "age", "B")
      cd <- newPCohortData[, c("pixelIndex", columnsForPG), with = FALSE]
      newPCohortData[, pixelGroup := generatePixelGroups(cd, maxPixelGroup = 0L, columns = columnsForPG)]

      pixelGroupMap[newPCohortData$pixelIndex] <- newPCohortData$pixelGroup

      ## collapse to PGs
      tempCohortData <- copy(newPCohortData)
      set(tempCohortData, NULL, "pixelIndex", NULL)
      tempCohortData <- tempCohortData[!duplicated(tempCohortData[, .SD, .SDcols = columnsForPixelGroups])]

      outs <- updateCohortData(newPixelCohortData = copy(postFirePixelCohortData),
                               cohortData = copy(tempCohortData),
                               pixelGroupMap = pixelGroupMap,
                               currentTime = round(time(sim)),
                               speciesEcoregion = copy(sim$speciesEcoregion),
                               treedFirePixelTableSinceLastDisp = copy(treedFirePixelTableSinceLastDisp),
                               initialB = P(sim)$initialB,
                               successionTimestep = P(sim)$successionTimestep)

      assertPostFireDist(cohortDataOrig = tempCohortData, pixelGroupMapOrig = pixelGroupMap,
                         cohortDataNew = outs$cohortData, pixelGroupMapNew = outs$pixelGroupMap,
                         postFirePixelCohortData = postFirePixelCohortData,
                         burnedPixelCohortData, doAssertion = getOption("LandR.assertions", TRUE))

      sim$cohortData <- outs$cohortData
      sim$pixelGroupMap <- outs$pixelGroupMap
      sim$pixelGroupMap[] <- as.integer(sim$pixelGroupMap[])
      ##########################################################
      # rm missing cohorts (i.e., those pixelGroups that are gone due to the fire/treedFirePixelTableSinceLastDisp)
      ##########################################################
    }
  }


  ## export objects
  sim$severityBMap <- severityBMap
  sim$severityData <- severityData
  sim$lastFireYear <- time(sim)

  ## TODO: Ceres: moved this to here to avoid re-killing/serotiny/repsoruting pixelGroups that burned in the previous year.
  ## update past pixelGroup number to match current ones.
  sim$treedFirePixelTableSinceLastDisp[, pixelGroup := as.integer(getValues(sim$pixelGroupMap))[pixelIndex]]
  # append previous year's
  treedFirePixelTableSinceLastDisp <- rbindlist(list(sim$treedFirePixelTableSinceLastDisp,
                                                     treedFirePixelTableSinceLastDisp))

  sim$treedFirePixelTableSinceLastDisp <- treedFirePixelTableSinceLastDisp
  return(invisible(sim))
}

## ---------------------------------------------------------------------------
## INPUT OBJECTS

.inputObjects <- function(sim) {
  cacheTags <- c(currentModule(sim), "function:.inputObjects")
  dPath <- asPath(getOption("reproducible.destinationPath", dataPath(sim)), 1)
  message(currentModule(sim), ": using dataPath '", dPath, "'.")

  if (suppliedElsewhere(object = "scfmReturnInterval", sim = sim, where = "sim")) {
    if (P(sim)$fireTimestep != sim$scfmReturnInterval) {
      sim@params$Biomass_regenerationPM$fireTimestep <- sim$scfmReturnInterval
      message(paste0("Biomass_regenerationPM detected 'scfm' fire model. Setting fireTimestep to ",
                     sim$scfmReturnInterval, " to match it.")) ## TODO: don't hardcode module interdependencies!
    }
  } else {
    if (is.null(P(sim)$fireTimestep)) {
      P(sim)$fireTimestep <- 1L
      message("fireTimestep is 'NULL'. Setting to 1 unit of time")
    }
  }

  ## get LANDISII main input table where species and light requirements tables come from
  if (!suppliedElsewhere("sufficientLight", sim) |
      (!suppliedElsewhere("species", sim))) {
    mainInput <- prepInputsMainInput(url = NULL, dPath, cacheTags) ## uses default URL
  }

  ## read species txt and convert it to data table
  if (!suppliedElsewhere("species", sim)) {
    sim$species <- prepInputsSpecies(url = extractURL("species"), dPath, cacheTags)
  }

  ## make light requirements table
  if (!suppliedElsewhere("sufficientLight", sim)) {
    sufficientLight <- data.frame(mainInput)
    startRow <- which(sufficientLight$col1 == "SufficientLight")
    sufficientLight <- sufficientLight[(startRow + 1):(startRow + 5), 1:7]
    sufficientLight <- data.table(sufficientLight)
    sufficientLight <- sufficientLight[, lapply(.SD, function(x) as.numeric(x))]

    names(sufficientLight) <- c("speciesshadetolerance",
                                "X0", "X1", "X2", "X3", "X4", "X5")
    sim$sufficientLight <- data.frame(sufficientLight)
  }

  #  # input species ecoregion dynamics table
  if (!suppliedElsewhere("speciesEcoregion", sim)) {
    sim$speciesEcoregion <- prepInputsSpeciesEcoregion(url = extractURL("speciesEcoregion"),
                                                       dPath = dPath, cacheTags = cacheTags)
  }

  if (!suppliedElsewhere(sim$treedFirePixelTableSinceLastDisp)) {
    sim$treedFirePixelTableSinceLastDisp <- data.table(pixelIndex = integer(), pixelGroup = integer(),
                                                       burnTime = numeric())
  }

  if (!suppliedElsewhere(sim$fireDamageTable)) {
    sim$fireDamageTable <- data.table(agesKilled = c(0.2, 0.5, 0.85, 1.0), ## proportion of longevity below which there is cohort removal
                                      severityToleranceDif = c(-2, -1, 0, 1))   ## difference between severity and spp fire tolerance
  }

  return(invisible(sim))
}
