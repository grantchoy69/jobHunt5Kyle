library(jsonlite)
library(officer)
library(flextable)  # For any potential tables, but not used here
library(rmarkdown)

# 1. Get JSON path from args

setwd("~/jobHunt5Kyle")

getResumeFilePath <- function() {
  args <- commandArgs(trailingOnly = TRUE)
  
  if (length(args) >= 1) {
    return(args[1])
  } else {
    return("kyleResumeData.json")
  }
}

resumeFilePath <- getResumeFilePath()

if (!file.exists(resumeFilePath)) {
  stop(paste("JSON file not found:", resumeFilePath, "\nCheck getwd() and arguments."))
}

# Normalize path and get directory for outputs
resumeFilePath <- normalizePath(resumeFilePath)
resumeDirPath <- dirname(resumeFilePath)

cat("Using resume JSON:", resumeFilePath, "\n")
cat("Output directory:", resumeDirPath, "\n\n")

# 2. Load data

resumeData <- fromJSON(resumeFilePath, simplifyVector = FALSE)

# 3. Helper functions

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

makeDocxFromLines <- function(lines, filePath) {
  doc <- read_docx()
  
  for (i in seq_along(lines)) {
    lineText <- lines[[i]]
    doc <- body_add_par(doc, lineText, style = "Normal")
  }
  
  print(doc, target = filePath)
  cat("Wrote DOCX:", filePath, "\n")
}

# Updated: Build pretty DOCX as 1-page with header (no sidebar), small fonts, narrow margins, no blank page
buildPrettyDocx <- function(data, outputPath) {
  doc <- read_docx()
  # Set narrow margins (0.5 inches = 1.27 cm)
  secPr <- prop_section(page_margins = page_mar(top = 0.3, bottom = 0.3, left = 0.3, right = 0.3, gutter = 0))
  blockSec <- block_section(secPr)
  doc <- body_add(doc, blockSec)
  
  # Clear initial blank content to avoid extra page
  doc <- cursor_begin(doc)
  doc <- body_remove(doc)
  
  # Font props (smaller 10pt body for 1-page fit, Arial like image, tighter spacing)
  headerFp <- fp_text(bold = TRUE, font.size = 18, font.family = "Arial")
  subheaderFp <- fp_text(font.size = 12, color = "#000080", font.family = "Arial")  # Gray like image
  sectionFp <- fp_text(bold = TRUE, font.size = 12, font.family = "Arial")
  bodyFp <- fp_text(font.size = 10, font.family = "Arial")  # Smaller for compactness
  
  # Add name (large bold)
  doc <- body_add_fpar(doc, fpar(ftext(data$name, headerFp)))
  
  # Add gray subheader (title/location)
  subHeaderText <- paste0("Senior Manager, CRM @ Hanna Andersson\n", data$location)
  doc <- body_add_fpar(doc, fpar(ftext(subHeaderText, subheaderFp)))
  
  # Add Contact as header section
  doc <- body_add_fpar(doc, fpar(ftext("Contact", sectionFp)))
  contactText <- paste0(data$email, " | www.linkedin.com/in/", data$linkedinUrl)
  doc <- body_add_fpar(doc, fpar(ftext(contactText, bodyFp)))
  
  # Add Education
  doc <- body_add_fpar(doc, fpar(ftext("Education", sectionFp)))
  for (i in seq_along(data$education)) {
    edu <- data$education[[i]]
    eduLine <- paste0(edu$degree, " — ", edu$school, " (", edu$years, ")")
    doc <- body_add_fpar(doc, fpar(ftext(eduLine, bodyFp)))
  }
  
  # Add Top Skills as horizontal list
  doc <- body_add_fpar(doc, fpar(ftext("Top Skills", sectionFp)))
  skillsText <- collapseWithSep(data$coreSkills, " | ")
  doc <- body_add_fpar(doc, fpar(ftext(skillsText, bodyFp)))
  
  # Add Languages
  # doc <- body_add_fpar(doc, fpar(ftext("Languages", sectionFp)))
  # langsText <- collapseWithSep(data$languages, ", ")
  # doc <- body_add_fpar(doc, fpar(ftext(langsText, bodyFp)))
  # 
  # Add Certifications if present
  
  # Add Summary
  doc <- body_add_fpar(doc, fpar(ftext("Summary", sectionFp)))
  doc <- body_add_fpar(doc, fpar(ftext(data$summary, bodyFp)))
  
  # Add Experience (condensed, group under companies if multiple roles)
  doc <- body_add_fpar(doc, fpar(ftext("Experience", sectionFp)))
  for (i in seq_along(data$experience)) {
    job <- data$experience[[i]]
    jobHeader <- paste0(job$company, " | ", job$title, " | ", job$dates, if (nzchar(job$location)) paste0(" (", job$location, ")") else "")
    doc <- body_add_fpar(doc, fpar(ftext(jobHeader, subheaderFp)))
    if (!is.null(job$bullets)) {
      for (b in job$bullets) {
        bulletText <- paste0("-", b)
        doc <- body_add_fpar(doc, fpar(ftext(bulletText, bodyFp)))
      }
    }
  }
  if (!is.null(data$certifications) && length(data$certifications) > 0) {
    doc <- body_add_fpar(doc, fpar(ftext("Certifications", sectionFp)))
    certsText <- collapseWithSep(data$certifications, "\n")
    doc <- body_add_fpar(doc, fpar(ftext(certsText, bodyFp)))
  }
  
  
  print(doc, target = outputPath)
  cat("Wrote pretty DOCX:", outputPath, "\n")
}

# 4. Build ATS text version
# 
# buildAtsResume <- function(data) {
#   lines <- character(0)
#   
#   headerLine <- paste(
#     data$name,
#     data$location,
#     data$email,
#     data$linkedinUrl,
#     sep = " | "
#   )
#   lines <- c(lines, headerLine)
#   
#   if (!is.null(data$targetRoleTitle) && nzchar(data$targetRoleTitle)) {
#     targetLine <- paste("Target Role:", data$targetRoleTitle)
#     lines <- c(lines, targetLine)
#   }
#   
#   lines <- c(lines, "")
#   
#   lines <- c(lines, "SUMMARY")
#   lines <- c(lines, data$summary, "")
#   
#   lines <- c(lines, "CORE SKILLS")
#   skillsLine <- collapseWithSep(data$coreSkills, " • ")
#   lines <- c(lines, skillsLine, "")
#   
#   lines <- c(lines, "EXPERIENCE")
#   experienceLines <- buildExperienceLines(data$experience, "•")
#   lines <- c(lines, experienceLines)
#   
#   lines <- c(lines, "EDUCATION")
#   for (i in seq_along(data$education)) {
#     edu <- data$education[[i]]
#     eduLine <- paste(
#       edu$degree,
#       "—",
#       edu$school,
#       "(",
#       edu$years,
#       ")"
#     )
#     lines <- c(lines, eduLine)
#   }
#   lines <- c(lines, "")
#   
#   if (!is.null(data$certifications) && length(data$certifications) > 0) {
#     lines <- c(lines, "CERTIFICATIONS")
#     for (c in data$certifications) {
#       lines <- c(lines, paste("•", c))
#     }
#     lines <- c(lines, "")
#   }
#   
#   if (!is.null(data$languages) && length(data$languages) > 0) {
#     lines <- c(lines, "LANGUAGES")
#     languageLine <- collapseWithSep(data$languages, ", ")
#     lines <- c(lines, languageLine)
#   }
#   
#   return(lines)
# }

# 5. Build pretty markdown version

# buildPrettyResume <- function(data) {
#   lines <- character(0)
#   
#   headerText <- paste(
#     data$location,
#     "·",
#     data$email,
#     "·",
#     data$linkedinUrl
#   )
#   
#   lines <- c(
#     lines,
#     paste0("# ", data$name),
#     headerText
#   )
#   
#   if (!is.null(data$targetRoleTitle) && nzchar(data$targetRoleTitle)) {
#     lines <- c(lines, paste0("### Target Role: ", data$targetRoleTitle))
#   }
#   
#   lines <- c(lines, "")
#   
#   lines <- c(lines, "## Education")
#   for (i in seq_along(data$education)) {
#     edu <- data$education[[i]]
#     eduTitle <- edu$degree
#     if (!nzchar(eduTitle)) {
#       eduTitle <- edu$school
#     }
#     eduLine <- paste0("**", eduTitle, "** — ", edu$school, " (", edu$years, ")")
#     lines <- c(lines, eduLine)
#   }
#   lines <- c(lines, "")
#   
#   lines <- c(lines, "## Summary")
#   lines <- c(lines, data$summary, "")
#   
#   lines <- c(lines, "## Core Skills")
#   skillsLine <- collapseWithSep(data$coreSkills, " · ")
#   lines <- c(lines, skillsLine, "")
#   
#   lines <- c(lines, "## Experience")
#   experienceLines <- buildExperienceLines(data$experience, "-")
#   lines <- c(lines, experienceLines)
#   
#   
#   
#   if (!is.null(data$certifications) && length(data$certifications) > 0) {
#     lines <- c(lines, "## Certifications")
#     for (c in data$certifications) {
#       lines <- c(lines, paste("- ", c))
#     }
#     lines <- c(lines, "")
#   }
  
  # if (!is.null(data$languages) && length(data$languages) > 0) {
  #   lines <- c(lines, "## Languages")
  #   languageLine <- collapseWithSep(data$languages, ", ")
  #   lines <- c(lines, languageLine)
  # }
#   
#   return(lines)
# }

# 6. Build content and write plain text/markdown

atsLines <- buildAtsResume(resumeData)
prettyLines <- buildPrettyResume(resumeData)

atsTxtPath <- file.path(resumeDirPath, "kyleAtsResume.txt")
prettyMdPath <- file.path(resumeDirPath, "kylePrettyResume.md")

writeLines(atsLines, atsTxtPath)
cat("Wrote ATS text:", atsTxtPath, "\n")

writeLines(prettyLines, prettyMdPath)
cat("Wrote pretty markdown:", prettyMdPath, "\n")

# 7. Generate ATS DOCX

atsDocxPath <- file.path(resumeDirPath, "kyleAtsResume.docx")
makeDocxFromLines(atsLines, atsDocxPath)

# 8. Generate pretty DOCX from officer

prettyDocxPath <- file.path(resumeDirPath, "kylePrettyResume.docx")
buildPrettyDocx(resumeData, prettyDocxPath)

cat("Done.\n")