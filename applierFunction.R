library(RSelenium)
library(wdman)
library(stringr)

# ---------- Inputs ----------
jobUrl <- "https://www.samsara.com/company/careers/roles/7340144?gh_jid=7340144&gh_src=a42fbb361"

# Put your real values here
applicant <- list(
  firstName = "Grant",
  lastName = "Choy",
  email = "GrantChoy@gmail.com",
  phone = "619-347-3646",
  resumePath = "C:/path/to/resume.pdf",        # local absolute path
  coverLetterPath = "C:/path/to/cover.pdf"     # optional
)

# ---------- Helpers ----------
safe <- function(expr) {
  try(expr, silent = TRUE)
}

waitFor <- function(remDr, using, value, timeoutSec = 25, pollSec = 0.5) {
  t0 <- Sys.time()
  while (as.numeric(difftime(Sys.time(), t0, units = "secs")) < timeoutSec) {
    elems <- try(remDr$findElements(using = using, value = value), silent = TRUE)
    if (!inherits(elems, "try-error") && length(elems) > 0) return(elems[[1]])
    Sys.sleep(pollSec)
  }
  return(NULL)
}

clickByText <- function(remDr, text) {
  # Works for links/buttons/divs with visible text
  xp <- paste0("//*[self::a or self::button or self::div or self::span][contains(normalize-space(.), '", text, "')]")
  el <- waitFor(remDr, "xpath", xp, 20)
  if (is.null(el)) stop(paste("Could not find clickable element with text:", text))
  el$clickElement()
}

switchToNewestWindow <- function(remDr) {
  handles <- remDr$getWindowHandles()[[1]]
  remDr$switchToWindow(handles[[length(handles)]])
}

switchIntoGreenhouseIframeIfPresent <- function(remDr) {
  # Look for likely iframe embed
  frames <- remDr$findElements("css selector", "iframe")
  if (length(frames) == 0) return(FALSE)
  
  for (i in seq_along(frames)) {
    safe(remDr$switchToFrame(frames[[i]]))
    # Greenhouse form often has these IDs
    found <- waitFor(remDr, "css selector", "form#application_form, input#first_name, input#last_name", 2)
    if (!is.null(found)) return(TRUE)
    safe(remDr$switchToFrame(NULL))
  }
  
  safe(remDr$switchToFrame(NULL))
  return(FALSE)
}

fillText <- function(remDr, css, text) {
  if (is.null(text) || nchar(text) == 0) return(invisible(NULL))
  el <- waitFor(remDr, "css selector", css, 15)
  if (is.null(el)) stop(paste("Missing input:", css))
  safe(el$clearElement())
  el$sendKeysToElement(list(text))
}

uploadFile <- function(remDr, css, filePath) {
  if (is.null(filePath) || nchar(filePath) == 0) return(invisible(NULL))
  fixed <- gsub("\\\\", "/", filePath)
  el <- waitFor(remDr, "css selector", css, 15)
  if (is.null(el)) stop(paste("Missing file input:", css))
  # Greenhouse often hides the input
  safe(remDr$executeScript("arguments[0].style.display='block'; arguments[0].style.opacity=1;", list(el)))
  el$sendKeysToElement(list(fixed))
}

# ---------- Start local ChromeDriver ----------
chromePort <- 9515L
cd <- wdman::chrome(port = chromePort)

remDr <- RSelenium::remoteDriver(
  remoteServerAddr = "localhost",
  port = chromePort,
  browserName = "chrome"
)

remDr$open()
on.exit({
  safe(remDr$close())
  safe(remDr$quit())
  safe(cd$stop())
}, add = TRUE)

# ---------- Navigate ----------
remDr$navigate(jobUrl)
Sys.sleep(2)

# ---------- Click Apply Now ----------
# Some sites have multiple "Apply Now" buttons. Try a couple patterns.
safe(clickByText(remDr, "Apply Now"))
Sys.sleep(2)

# If a new window opened, switch to it
handles <- remDr$getWindowHandles()[[1]]
if (length(handles) > 1) {
  switchToNewestWindow(remDr)
  Sys.sleep(1)
}

# If it's an iframe modal, switch into it
inFrame <- switchIntoGreenhouseIframeIfPresent(remDr)

# ---------- Confirm we're on the application form ----------
# If we didn't find it via iframe, we might already be on the page
formEl <- waitFor(remDr, "css selector", "form#application_form, input#first_name, input#email", 20)
if (is.null(formEl)) {
  # Last-ditch: try switching to default content and searching again
  safe(remDr$switchToFrame(NULL))
  formEl <- waitFor(remDr, "css selector", "form#application_form, input#first_name, input#email", 10)
}

if (is.null(formEl)) {
  currentUrl <- remDr$getCurrentUrl()[[1]]
  stop(paste("Could not locate Greenhouse form elements. Current URL:", currentUrl))
}

# ---------- Fill common Greenhouse fields ----------
# Greenhouse default ids/names are usually stable:
safe(fillText(remDr, "input#first_name, input[name='first_name']", applicant$firstName))
safe(fillText(remDr, "input#last_name, input[name='last_name']", applicant$lastName))
safe(fillText(remDr, "input#email, input[name='email']", applicant$email))
safe(fillText(remDr, "input#phone, input[name='phone']", applicant$phone))

# Resume upload
# Greenhouse often uses input#resume or input[name='resume']
safe(uploadFile(remDr, "input#resume, input[name='resume'], input[type='file'][name*='resume']", applicant$resumePath))

# Cover letter upload (optional)
safe(uploadFile(remDr, "input#cover_letter, input[name='cover_letter'], input[type='file'][name*='cover']", applicant$coverLetterPath))

cat("Filled what matched. Review the page and submit manually.\n")
