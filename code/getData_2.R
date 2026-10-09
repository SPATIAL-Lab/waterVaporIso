##
# DOWNLOAD DATA - MULTIPLE SITES
#
# Automated version of 01_getData.R: loops over a list of NEON sites and, for each one,
# downloads the eddy covariance bundle (DP4.00200.001) and relative humidity (DP1.00098.001),
# then saves the same iso / met / RH csv files that 01_getData.R does.
#
# Memory handling:
#   Each heavy step (download, iso stackEddy, met stackEddy, RH loadByProduct) runs in its own
#   fresh R process (via Rscript), so the large objects from one step are completely gone before
#   the next step starts. This replaces manually clearing the environment between stackEddy calls.
#   The filesToStack00200 folder is emptied before and after every site.
#
# Other automation:
#   - met barometer level is found automatically (the level ending in 5, e.g. 015 or 035)
#   - top and bottom levels are found automatically for iso and met
#   - sites/files that already exist are skipped (set overwrite <- TRUE to redo them)
#   - if one site or step fails, the error is logged and the script moves on to the next one
#   - progress is written to data/logs/getData_2_log.txt, and each step's full console output
#     is written to its own file in data/logs/
#   - a summary table of what succeeded/failed is printed at the end
#
# Run with the working directory set to the project root (open waterVaporIso.Rproj), then
# source this whole script. Start from a clean environment if you can.
#


#### settings ####
sites     <- c("SRER", "SCBI", "ORNL") #choose sites
release   <- "RELEASE-2026"
startdate <- "2021-01"
enddate   <- "2025-06"
overwrite <- FALSE #FALSE = skip any output csv that already exists

scriptPath <- "code/getData_2.R" #this script; re-run in separate R processes for each step
stackDir   <- "data/filesToStack00200"
logDir     <- "data/logs"
logFile    <- file.path(logDir, "getData_2_log.txt")

releaseTag <- tolower(sub("-", "", release)) #"RELEASE-2026" -> "release2026"

#rows in the eddy data that are calibration/validation gases, not air measurements
validationLevels <- c("co2Arch", "co2High", "co2Low", "co2Med", "co2Zero",
                      "h2oHigh", "h2oLow", "h2oMed")


#### helper functions ####

logMsg <- function(...) {
  msg <- paste0(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), " | ", paste0(...))
  message(msg)
  dir.create(logDir, showWarnings = FALSE, recursive = TRUE)
  cat(msg, "\n", file = logFile, append = TRUE, sep = "")
}

outFiles <- function(site) {
  c(iso = paste0("data/iso/iso_", site, "_", releaseTag, ".csv"),
    met = paste0("data/met/met_", site, "_", releaseTag, ".csv"),
    rh  = paste0("data/met/met_", site, "_", releaseTag, "_RH.csv"))
}

emptyStackDir <- function() {
  unlink(stackDir, recursive = TRUE)
  if (dir.exists(stackDir)) stop("could not empty ", stackDir)
}

getToken <- function() {
  if (file.exists("neon_token.R")) {
    e <- new.env()
    sys.source("neon_token.R", envir = e)
    if (exists("token", envir = e)) return(get("token", envir = e))
  }
  tok <- Sys.getenv("NEON_TOKEN")
  if (nzchar(tok)) return(tok)
  NA_character_ #download without a token (slower, rate limited)
}

#keep and rename columns; a column missing from the download is filled with NA and logged
#(instead of failing the whole site)
selectCols <- function(df, cols, newNames, site, step) {
  missing <- setdiff(cols, colnames(df))
  if (length(missing) > 0) {
    logMsg(site, " ", step, ": WARNING missing columns filled with NA: ",
           paste(missing, collapse = ", "))
    for (m in missing) df[[m]] <- NA
  }
  df <- df[, cols]
  colnames(df) <- newNames
  df
}

#measurement levels (e.g. "010", "020", "040") sorted from bottom to top
sortedLevels <- function(df) {
  lvls <- unique(df$verticalPosition)
  lvls <- lvls[grepl("^[0-9]+$", lvls)]
  lvls[order(as.numeric(lvls))]
}

#barometer level: the level ending in 5. If there is more than one (or none), use the level
#with the most non-NA pressure data
findBaroLevel <- function(df, site) {
  lvls <- sortedLevels(df)
  cand <- lvls[grepl("5$", lvls)]
  if (length(cand) == 1) return(cand)

  nPres <- sapply(lvls, function(l) sum(!is.na(df$presAtm[df$verticalPosition == l])))
  if (length(cand) > 1) nPres <- nPres[cand]
  if (all(nPres == 0)) stop("no barometer level found (no level ends in 5 and no pressure data)")
  baro <- names(nPres)[which.max(nPres)]
  logMsg(site, " met: WARNING barometer level ambiguous (levels ending in 5: ",
         ifelse(length(cand) == 0, "none", paste(cand, collapse = ", ")),
         "); using level with most pressure data: ", baro)
  baro
}


#### worker steps (each runs in its own R process) ####

stepDownload <- function(site) {
  neonUtilities::zipsByProduct(dpID = "DP4.00200.001",
                               package = "expanded", #expanded gets qm data (basic just has qf final)
                               release = release,
                               site = site,
                               startdate = startdate,
                               enddate = enddate,
                               savepath = "data/",
                               check.size = FALSE,
                               include.provisional = FALSE,
                               token = getToken())
  nFiles <- length(list.files(stackDir, recursive = TRUE))
  if (nFiles == 0) stop("download finished but ", stackDir, " is empty")
  logMsg(site, " download: ", nFiles, " files in ", stackDir)
}

stepIso <- function(site) {
  iso <- neonUtilities::stackEddy(filepath = stackDir,
                                  level = "dp01",
                                  avg = 30,
                                  var = c("dlta18OH2o", "dlta2HH2o", "rtioMoleWetH2o"))
  df <- iso[[site]]
  rm(iso); gc()

  #remove validation rows
  df <- subset(df, !verticalPosition %in% validationLevels)

  #save only the bottom and top measurement levels
  lvls <- sortedLevels(df)
  keep <- c(lvls[1], lvls[length(lvls)])
  df <- subset(df, verticalPosition %in% keep)

  df <- selectCols(df,
                   c("timeBgn", "timeEnd",
                     "verticalPosition",
                     "data.isoH2o.dlta18OH2o.mean",
                     "data.isoH2o.dlta2HH2o.mean",
                     "data.isoH2o.rtioMoleWetH2o.mean",
                     "qfqm.isoH2o.dlta18OH2o.qmRngPresCrdH2oFail",
                     "qfqm.isoH2o.dlta2HH2o.qmRngPresCrdH2oFail",
                     "qfqm.isoH2o.dlta18OH2o.qmFrt00MfmFail",
                     "qfqm.isoH2o.dlta2HH2o.qmFrt00MfmFail",
                     "qfqm.isoH2o.dlta18OH2o.qfFinl",
                     "qfqm.isoH2o.dlta2HH2o.qfFinl"),
                   c("timeBgn", "timeEnd",
                     "verticalPosition",
                     "dlta18OH2o",
                     "dlta2HH2o",
                     "rtioMoleWetH2o",
                     "qmRngPresCrdH2oFail.dlta18OH2o",
                     "qmRngPresCrdH2oFail.dlta2HH2o",
                     "qmFrt00MfmFail.dlta18OH2o",
                     "qmFrt00MfmFail.dlta2HH2o",
                     "qfFinl.dlta18OH2o",
                     "qfFinl.dlta2HH2o"),
                   site, "iso")

  dir.create("data/iso", showWarnings = FALSE, recursive = TRUE)
  write.csv(df, outFiles(site)[["iso"]], row.names = FALSE)
  logMsg(site, " iso: saved ", nrow(df), " rows, levels ", paste(keep, collapse = ", "),
         " -> ", outFiles(site)[["iso"]])
}

stepMet <- function(site) {
  met <- neonUtilities::stackEddy(filepath = stackDir,
                                  level = "dp01",
                                  avg = 30,
                                  var = c("presAtm",
                                          "presCor",
                                          "temp",
                                          "tempAir",
                                          "radiLwOut",
                                          "radiSwIn",
                                          "veloXaxsYaxsErth"))
  df <- met[[site]]
  rm(met); gc()

  #remove validation rows
  df <- subset(df, !verticalPosition %in% validationLevels)

  df <- selectCols(df,
                   c("timeBgn", "timeEnd",
                     "verticalPosition",
                     "data.presBaro.presAtm.mean", #atm pressure from the barometer
                     "data.tempAirLvl.temp.mean", #air temp on the ML
                     "data.tempAirTop.temp.mean", #air temp on the top (triple aspirated)
                     "data.radiNet.radiSwIn.mean", #incoming shortwave radiation
                     "data.radiNet.radiLwOut.mean", #outgoing longwave radiation
                     "data.soni.veloXaxsYaxsErth.mean", #wind speed from CSAT3D
                     "qfqm.presBaro.presAtm.qfFinl",
                     "qfqm.tempAirLvl.temp.qfFinl",
                     "qfqm.tempAirTop.temp.qfFinl",
                     "qfqm.radiNet.radiSwIn.qfFinl",
                     "qfqm.radiNet.radiLwOut.qfFinl",
                     "qfqm.soni.veloXaxsYaxsErth.qfFinl",
                     "qfqm.soni.veloXaxsYaxsErth.qmSoniSgnlPoorFail"),
                   c("timeBgn", "timeEnd",
                     "verticalPosition",
                     "presAtm",
                     "tempAirLvl",
                     "tempAirTop",
                     "radiSwIn",
                     "radiLwOut",
                     "veloXaxsYaxsErth",
                     "presAtm.qfFinl",
                     "tempAirLvl.qfFinl",
                     "tempAirTop.qfFinl",
                     "radiSwIn.qfFinl",
                     "radiLwOut.qfFinl",
                     "veloXaxsYaxsErth.qfFinl",
                     "veloXaxsYaxsErth.qmSoniSgnlPoorFail"),
                   site, "met")

  #save only the bottom, top, and barometer levels
  lvls <- sortedLevels(df)
  baro <- findBaroLevel(df, site)
  keep <- unique(c(lvls[1], baro, lvls[length(lvls)]))
  df <- subset(df, verticalPosition %in% keep)

  dir.create("data/met", showWarnings = FALSE, recursive = TRUE)
  write.csv(df, outFiles(site)[["met"]], row.names = FALSE)
  logMsg(site, " met: saved ", nrow(df), " rows, levels ", paste(keep, collapse = ", "),
         " (barometer = ", baro, ") -> ", outFiles(site)[["met"]])
}

stepRH <- function(site) {
  rh <- neonUtilities::loadByProduct(dpID = "DP1.00098.001",
                                     package = "expanded", #expanded gets qm data (basic just has qf final)
                                     release = release,
                                     timeIndex = 30,
                                     site = site,
                                     startdate = startdate,
                                     enddate = enddate,
                                     check.size = FALSE,
                                     include.provisional = FALSE,
                                     token = getToken())
  rh_dat <- rh[["RH_30min"]]
  rm(rh); gc()
  if (is.null(rh_dat)) stop("no RH_30min table returned")

  cols <- c("startDateTime",
            "horizontalPosition",
            "verticalPosition",
            "RHMean",
            "RHRangeFailQM",
            "RHPersistenceFailQM",
            "RHNullFailQM",
            "RHGapFailQM",
            "RHSpikeFailQM",
            "RHValidCalFailQM",
            "RHFinalQF",
            "RHFinalQFSciRvw",
            "tempRHMean",
            "tempRHRangeFailQM",
            "tempRHPersistenceFailQM",
            "tempRHNullFailQM",
            "tempRHGapFailQM",
            "tempRHSpikeFailQM",
            "tempRHValidCalFailQM",
            "tempRHFinalQF",
            "tempRHFinalQFSciRvw",
            "dewTempMean",
            "dewTempRangeFailQM",
            "dewTempPersistenceFailQM",
            "dewTempNullFailQM",
            "dewTempGapFailQM",
            "dewTempSpikeFailQM",
            "dewTempValidCalFailQM",
            "dewTempFinalQF",
            "dewTempFinalQFSciRvw",
            "RHSensorErrorFailQM")
  rh_dat <- selectCols(rh_dat, cols, cols, site, "rh")

  dir.create("data/met", showWarnings = FALSE, recursive = TRUE)
  write.csv(rh_dat, outFiles(site)[["rh"]], row.names = FALSE)
  logMsg(site, " rh: saved ", nrow(rh_dat), " rows -> ", outFiles(site)[["rh"]])
}


#### worker mode ####
#when this script is called as: Rscript code/getData_2.R --worker <step> <site>
#it runs just that one step and exits

args <- commandArgs(trailingOnly = TRUE)

if (length(args) >= 3 && args[1] == "--worker") {
  step <- args[2]
  site <- args[3]
  fn <- switch(step,
               download = stepDownload,
               iso = stepIso,
               met = stepMet,
               rh = stepRH,
               stop("unknown step: ", step))
  ok <- tryCatch({
    fn(site)
    TRUE
  }, error = function(e) {
    logMsg(site, " ", step, ": ERROR ", conditionMessage(e))
    FALSE
  })
  quit(save = "no", status = ifelse(ok, 0, 1))
}


#### main loop (runs when you source the script) ####

if (!file.exists(scriptPath)) {
  stop("can't find ", scriptPath, ". Set the working directory to the project root ",
       "(open waterVaporIso.Rproj) before running.")
}
if (!requireNamespace("neonUtilities", quietly = TRUE)) stop("install neonUtilities first")
if (is.na(getToken())) message("No NEON token found (neon_token.R or NEON_TOKEN); downloads may be slower.")

rscript <- file.path(R.home("bin"), "Rscript")

#run one step in a fresh R process; console output goes to data/logs/<site>_<step>.txt
runStep <- function(step, site) {
  stepLog <- file.path(logDir, paste0(site, "_", step, ".txt"))
  logMsg(site, " ", step, ": starting (output in ", stepLog, ")")
  status <- system2(rscript,
                    c(shQuote(scriptPath), "--worker", step, site),
                    stdout = stepLog, stderr = stepLog)
  if (status != 0) logMsg(site, " ", step, ": FAILED, see ", stepLog)
  status == 0
}

dir.create(logDir, showWarnings = FALSE, recursive = TRUE)
logMsg("==== getData_2 started: ", paste(sites, collapse = ", "), " | ", release, " | ",
       startdate, " to ", enddate, " ====")

runSummary <- data.frame()

for (site in sites) {
  logMsg("---- ", site, " ----")
  files <- outFiles(site)
  status <- c(iso = "skipped (exists)", met = "skipped (exists)", rh = "skipped (exists)")
  todo <- names(files)[overwrite | !file.exists(files)]

  #iso and met both come from the eddy covariance bundle
  eddyTodo <- intersect(c("iso", "met"), todo)
  if (length(eddyTodo) > 0) {
    emptyStackDir() #make sure there are no files left over from another site
    if (runStep("download", site)) {
      for (step in eddyTodo) status[step] <- ifelse(runStep(step, site), "ok", "FAILED")
    } else {
      status[eddyTodo] <- "FAILED (download)"
    }
    emptyStackDir() #free up disk space before the next site
    logMsg(site, ": emptied ", stackDir)
  }

  if ("rh" %in% todo) status["rh"] <- ifelse(runStep("rh", site), "ok", "FAILED")

  runSummary <- rbind(runSummary, data.frame(site = site, iso = status[["iso"]],
                                       met = status[["met"]], rh = status[["rh"]]))
}

logMsg("==== getData_2 finished ====")
print(runSummary, row.names = FALSE)
suppressWarnings(write.table(runSummary, logFile, append = TRUE, row.names = FALSE,
                             quote = FALSE, sep = "\t"))
