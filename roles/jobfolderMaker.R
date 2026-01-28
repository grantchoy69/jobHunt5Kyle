createJobFolder <- function(desc,
                            projectRootPath = "C:/Users/Grant Choy/Documents/jobHunt5Kyle",
                            rolesDirName = "roles",
                            baseConfigDirName = "baseConfig",
                            masterJsonFileName = "kyleResumeMaster.json",
                            createRproj = TRUE,
                            gitAdd = TRUE,
                            logFileName = "jobRoleLog.txt") {
  # Normalize project root
  projectRootPath <- normalizePath(projectRootPath, mustWork = TRUE)
  
  # Build key paths
  rolesDirPath <- file.path(projectRootPath, rolesDirName)
  baseConfigDirPath <- file.path(projectRootPath, baseConfigDirName)
  masterJsonPath <- file.path(baseConfigDirPath, masterJsonFileName)
  logFilePath <- file.path(projectRootPath, logFileName)
  
  if (!dir.exists(rolesDirPath)) {
    dir.create(rolesDirPath, recursive = TRUE)
  }
  
  if (!file.exists(masterJsonPath)) {
    stop(paste("Master JSON not found at:", masterJsonPath))
  }
  
  # Get today's date in YYYYMMDD format
  today <- format(Sys.Date(), "%Y%m%d")
  
  # Clean description for folder naming (remove non-alphanumeric)
  cleanDesc <- gsub("[^A-Za-z0-9]", "", desc)
  
  # Build folder name: description + date
  folderName <- paste(cleanDesc, today, sep = "_")
  
  # Full role folder path
  roleDirPath <- file.path(rolesDirPath, folderName)
  
  # Create role directory
  if (!dir.exists(roleDirPath)) {
    dir.create(roleDirPath, recursive = TRUE)
  }
  
  # Create blank jobDesc.txt
  jobDescPath <- file.path(roleDirPath, "jobDesc.txt")
  if (!file.exists(jobDescPath)) {
    file.create(jobDescPath)
  }
  
  # Copy master JSON into role folder as kyleResumeData.json
  roleJsonPath <- file.path(roleDirPath, "kyleResumeData.json")
  if (!file.exists(roleJsonPath)) {
    file.copy(from = masterJsonPath, to = roleJsonPath, overwrite = FALSE)
  }
  
  # Create minimal .Rproj file (optional)
  if (createRproj) {
    rprojPath <- file.path(roleDirPath, paste0(folderName, ".Rproj"))
    if (!file.exists(rprojPath)) {
      rprojLines <- c(
        "Version: 1.0",
        "",
        "RestoreWorkspace: Default",
        "SaveWorkspace: Default",
        "AlwaysSaveHistory: Default",
        "",
        "EnableCodeIndexing: Yes",
        "UseSpacesForTab: Yes",
        "NumSpacesForTab: 2",
        "Encoding: UTF-8"
      )
      writeLines(rprojLines, rprojPath)
    }
  }
  
  # Log entry in project root
  logEntry <- paste(
    format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
    "-",
    "Created role folder:",
    folderName,
    "| Desc:",
    desc,
    "| JSON:",
    roleJsonPath
  )
  write(logEntry, file = logFilePath, append = TRUE)
  
  # Git add new role folder (optional)
  if (gitAdd) {
    oldWdGit <- getwd()
    on.exit(setwd(oldWdGit), add = TRUE)
    
    setwd(projectRootPath)
    relRolePath <- file.path(rolesDirName, folderName)
    gitCmd <- "git"
    gitArgs <- c("add", shQuote(relRolePath))
    
    try({
      system2(command = gitCmd, args = gitArgs)
    }, silent = TRUE)
  }
  
  cat("Role folder created at:\n  ", roleDirPath, "\n")
  cat("Blank job description file:\n  ", jobDescPath, "\n")
  cat("Copied master JSON to:\n  ", roleJsonPath, "\n")
  
  return(roleDirPath)
}


setwd("C:/Users/Grant Choy/Documents/jobHunt5Kyle")

createJobFolder("Director of CRM, Email & SMS Marketing (Luxury eCommerce)")





