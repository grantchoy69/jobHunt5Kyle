library(jsonlite)

# 1. Load data -------------------------------------------------------------
setwd("~/jobHunt5Kyle")
resumeFilePath <- "kyleResumeData.txt"
resumeData <- fromJSON(resumeFilePath, simplifyVector = FALSE)

# 2. Helpers ---------------------------------------------------------------

collapseWithSep <- function(x, sep) {
  if (length(x) == 0) {
    return("")
  }
  return(paste(x, collapse = sep))
}

buildExperienceLines <- function(experienceList, bulletPrefix) {
  lines <- character(0)
  
  for (i in seq_along(experienceList)) {
    job <- experienceList[[i]]
    
    header <- paste(
      job$company, "—", job$title, "|",
      job$dates,
      if (nzchar(job$location)) paste("|", job$location) else "",
      sep = " "
    )
    
    header <- gsub("  ", " ", header)
    
    lines <- c(lines, header)
    
    if (!is.null(job$bullets)) {
      for (b in job$bullets) {
        lines <- c(lines, paste(bulletPrefix, b))
      }
    }
    
    lines <- c(lines, "")
  }
  
  return(lines)
}

# 3. Build ATS text version -----------------------------------------------

buildAtsResume <- function(data) {
  lines <- character(0)
  
  headerLine <- paste(
    data$name,
    data$location,
    data$email,
    data$linkedinUrl,
    sep = " | "
  )
  lines <- c(lines, headerLine, "")
  
  lines <- c(lines, "SUMMARY")
  lines <- c(lines, data$summary, "")
  
  lines <- c(lines, "CORE SKILLS")
  skillsLine <- collapseWithSep(data$coreSkills, " • ")
  lines <- c(lines, skillsLine, "")
  
  lines <- c(lines, "EXPERIENCE")
  experienceLines <- buildExperienceLines(data$experience, "•")
  lines <- c(lines, experienceLines)
  
  lines <- c(lines, "EDUCATION")
  for (i in seq_along(data$education)) {
    edu <- data$education[[i]]
    eduLine <- paste(
      edu$degree,
      "—",
      edu$school,
      "(",
      edu$years,
      ")"
    )
    lines <- c(lines, eduLine)
  }
  lines <- c(lines, "")
  
  if (!is.null(data$certifications) && length(data$certifications) > 0) {
    lines <- c(lines, "CERTIFICATIONS")
    for (c in data$certifications) {
      lines <- c(lines, paste("•", c))
    }
    lines <- c(lines, "")
  }
  
  if (!is.null(data$languages) && length(data$languages) > 0) {
    lines <- c(lines, "LANGUAGES")
    languageLine <- collapseWithSep(data$languages, ", ")
    lines <- c(lines, languageLine)
  }
  
  return(lines)
}

# 4. Build “pretty” markdown version --------------------------------------

buildPrettyResume <- function(data) {
  lines <- character(0)
  
  headerText <- paste(
    data$location,
    "·",
    data$email,
    "·",
    data$linkedinUrl
  )
  
  lines <- c(
    lines,
    paste0("# ", data$name),
    headerText,
    ""
  )
  
  lines <- c(lines, "## Summary")
  lines <- c(lines, data$summary, "")
  
  lines <- c(lines, "## Core Skills")
  skillsLine <- collapseWithSep(data$coreSkills, " · ")
  lines <- c(lines, skillsLine, "")
  
  lines <- c(lines, "## Experience")
  experienceLines <- buildExperienceLines(data$experience, "-")
  lines <- c(lines, experienceLines)
  
  lines <- c(lines, "## Education")
  for (i in seq_along(data$education)) {
    edu <- data$education[[i]]
    eduTitle <- edu$degree
    if (!nzchar(eduTitle)) {
      eduTitle <- edu$school
    }
    eduLine <- paste0("**", eduTitle, "** — ", edu$school, " (", edu$years, ")")
    lines <- c(lines, eduLine)
  }
  lines <- c(lines, "")
  
  if (!is.null(data$certifications) && length(data$certifications) > 0) {
    lines <- c(lines, "## Certifications")
    for (c in data$certifications) {
      lines <- c(lines, paste("- ", c))
    }
    lines <- c(lines, "")
  }
  
  if (!is.null(data$languages) && length(data$languages) > 0) {
    lines <- c(lines, "## Languages")
    languageLine <- collapseWithSep(data$languages, ", ")
    lines <- c(lines, languageLine)
  }
  
  return(lines)
}

# 5. Write outputs ---------------------------------------------------------

atsLines <- buildAtsResume(resumeData)
prettyLines <- buildPrettyResume(resumeData)

writeLines(atsLines, "kyleAtsResume.txt")
writeLines(prettyLines, "kylePrettyResume.md")
