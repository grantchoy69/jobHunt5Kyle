import time
import traceback

from selenium import webdriver
from selenium.webdriver.chrome.options import Options
from selenium.webdriver.common.by import By
from selenium.webdriver.support.ui import WebDriverWait, Select
from selenium.webdriver.support import expected_conditions as EC

jobUrl = "https://www.samsara.com/company/careers/roles/7340144?gh_jid=7340144&gh_src=a42fbb361"

# Base applicant identity
applicant = {
    "firstName": "Kyle",
    "lastName": "Amundson",
    "email": "KyleAAmundson@gmail.com",
    "phone": "702-354-1934",
    "resumePath": r"C:\Users\Grant Choy\Documents\jobHunt5Kyle\pythonAppier\resume.docx",
    "coverLetterPath": r""  # optional, leave empty if not using
}

# US-only, consistent profile defaults for commonly asked application fields.
profileDefaults = {
    "city": "San Diego",
    "state": "CA",
    "zip": "92104",
    "address1": "3712 Grim Ave, Apt 4",
    "addressFull": "3712 Grim Ave, Apt 4, San Diego, CA 92104",
    "country": "United States",
    "linkedIn": "https://www.linkedin.com/in/kyleamundson/",
    "educationLevel": "Bachelor's",

    "authorizedToWork": "Yes",
    "requiresSponsorship": "No",
    "currentlyInUs": "Yes",

    "genderIdentity": "Woman",
    "raceEthnicity": "White",
    "disabilityStatus": "No, I don’t have a disability",
    "veteranStatus": "I am not a protected veteran"
}

# Exact question text -> answer mapping for Samsara (Greenhouse custom questions)
questionAnswerMap = {
    "What is your highest level of education?": profileDefaults["educationLevel"],
    "I confirm I reside in the US.": profileDefaults["currentlyInUs"],
    "Will you now or in the future require Samsara to commence (“sponsor”) an immigration case in order to employ you?": profileDefaults["requiresSponsorship"],
    "Are you currently legally authorized to work in the country in which this job is based": profileDefaults["authorizedToWork"],
    "LinkedIn Profile": profileDefaults["linkedIn"],
    "How do you identify? (gender identity)": profileDefaults["genderIdentity"],
    "How do you identify? (race/ethnicity)": profileDefaults["raceEthnicity"],
    "If you are based in the US, what is your veteran status?": profileDefaults["veteranStatus"],
    "Do you have a physical or mental disability, impairment, or condition that substantially limits major life activity?": profileDefaults["disabilityStatus"]
}

def waitCss(driver, css, timeout=20):
    return WebDriverWait(driver, timeout).until(
        EC.presence_of_element_located((By.CSS_SELECTOR, css))
    )

def clickByText(driver, text, timeout=20):
    xp = f"//*[self::a or self::button][contains(normalize-space(.), '{text}')]"
    el = WebDriverWait(driver, timeout).until(
        EC.element_to_be_clickable((By.XPATH, xp))
    )
    el.click()

def safeClick(driver, by, selector, timeout=20):
    """Click with scroll + clickability wait + JS fallback to avoid header/nav intercepts."""
    wait = WebDriverWait(driver, timeout)
    el = wait.until(EC.presence_of_element_located((by, selector)))

    driver.execute_script("arguments[0].scrollIntoView({block:'center'});", el)
    time.sleep(0.2)

    try:
        el = wait.until(EC.element_to_be_clickable((by, selector)))
        el.click()
        return True
    except Exception:
        driver.execute_script("arguments[0].click();", el)
        return True

def switchToNewestTab(driver):
    handles = driver.window_handles
    driver.switch_to.window(handles[-1])

def trySwitchIntoGreenhouseFrame(driver):
    frames = driver.find_elements(By.CSS_SELECTOR, "iframe")
    for fr in frames:
        driver.switch_to.frame(fr)
        found = driver.find_elements(By.CSS_SELECTOR, "form#application_form, input#first_name, input#email")
        if len(found) > 0:
            return True
        driver.switch_to.default_content()
    return False

def fillCss(driver, css, value):
    if value is None or str(value).strip() == "":
        return False
    el = waitCss(driver, css, timeout=20)
    driver.execute_script("arguments[0].scrollIntoView({block:'center'});", el)
    el.clear()
    el.send_keys(value)
    return True

def uploadCss(driver, css, filePath):
    if filePath is None or str(filePath).strip() == "":
        return False
    el = waitCss(driver, css, timeout=20)
    driver.execute_script("arguments[0].style.display='block'; arguments[0].style.opacity=1;", el)
    el.send_keys(filePath)
    return True

# ---------------------------
# Greenhouse "Questions" section handlers
# ---------------------------

def findQuestionBlockByText(driver, questionText, timeout=2):
    """Find the container for a Greenhouse question by matching question text."""
    # Use single quotes inside XPath to avoid breaking Python strings.
    # Also normalize whitespace so it matches what you see on the page.
    xp = (
        "//*[self::label or self::div or self::span or self::p]"
        f"[contains(normalize-space(.), '{questionText}')]"
        "/ancestor::*[contains(@class,'field') or contains(@class,'question') or contains(@class,'application-question')][1]"
    )
    try:
        el = WebDriverWait(driver, timeout).until(
            EC.presence_of_element_located((By.XPATH, xp))
        )
        return el
    except Exception:
        return None


def answerQuestionInBlock(driver, blockEl, answerText):
    """Attempt to answer via select, radios, or text inputs within the question block."""
    if blockEl is None:
        return False

    # 1) Native select dropdown
    try:
        selEl = blockEl.find_element(By.CSS_SELECTOR, "select")
        driver.execute_script("arguments[0].scrollIntoView({block:'center'});", selEl)
        Select(selEl).select_by_visible_text(answerText)
        return True
    except Exception:
        pass

    # 2) Radio/checkbox options
    try:
        optionLabels = blockEl.find_elements(By.XPATH, ".//label")
        for lab in optionLabels:
            txt = (lab.text or "").strip()
            if txt == answerText or (answerText in txt) or (txt in answerText):
                driver.execute_script("arguments[0].scrollIntoView({block:'center'});", lab)
                try:
                    lab.click()
                except Exception:
                    driver.execute_script("arguments[0].click();", lab)
                return True
    except Exception:
        pass

    # 3) Text / URL input or textarea
    try:
        inp = blockEl.find_element(By.CSS_SELECTOR, "input[type='text'], input[type='url'], input:not([type]), textarea")
        driver.execute_script("arguments[0].scrollIntoView({block:'center'});", inp)
        try:
            inp.clear()
        except Exception:
            pass
        inp.send_keys(answerText)
        return True
    except Exception:
        pass

    return False

def answerGreenhouseQuestions(driver, questionAnswerMap):
    """Try to answer each custom question on the page. Skips anything not found."""
    results = []
    for qText, ans in questionAnswerMap.items():
        tried = [qText]
        if len(qText) > 60:
            tried.append(qText[:60])

        answered = False
        for t in tried:
            block = findQuestionBlockByText(driver, t, timeout=2)
            if block is not None:
                answered = answerQuestionInBlock(driver, block, ans)
                if answered:
                    break

        results.append((qText, answered))
    return results

chromeOptions = Options()
chromeOptions.add_argument("--start-maximized")

driver = webdriver.Chrome(options=chromeOptions)
wait = WebDriverWait(driver, 25)

try:
    driver.get(jobUrl)
    time.sleep(2)

    # Click Apply button.
    try:
        safeClick(driver, By.CSS_SELECTOR, "button[data-id='grnhse_app']", timeout=15)
    except Exception:
        try:
            clickByText(driver, "Apply Now", timeout=15)
        except Exception:
            clickByText(driver, "Apply", timeout=15)

    time.sleep(2)

    # New tab?
    if len(driver.window_handles) > 1:
        switchToNewestTab(driver)
        time.sleep(1)

    # If embedded in iframe, switch into it
    trySwitchIntoGreenhouseFrame(driver)

    # Confirm we see greenhouse form elements
    try:
        wait.until(EC.presence_of_element_located((By.CSS_SELECTOR, "form#application_form, input#first_name, input#email")))
    except Exception:
        driver.switch_to.default_content()
        wait.until(EC.presence_of_element_located((By.CSS_SELECTOR, "form#application_form, input#first_name, input#email")))

    # Fill core Greenhouse fields
    fillCss(driver, "input#first_name, input[name='first_name']", applicant["firstName"])
    fillCss(driver, "input#last_name, input[name='last_name']", applicant["lastName"])
    fillCss(driver, "input#email, input[name='email']", applicant["email"])
    fillCss(driver, "input#phone, input[name='phone']", applicant["phone"])

    # Uploads
    uploadCss(driver, "input#resume, input[name='resume'], input[type='file'][name*='resume']", applicant["resumePath"])
    uploadCss(driver, "input#cover_letter, input[name='cover_letter'], input[type='file'][name*='cover']", applicant["coverLetterPath"])

    # Answer employer custom questions (Samsara-specific wording)
    qaResults = answerGreenhouseQuestions(driver, questionAnswerMap)
    for q, ok in qaResults:
        print(f"Question answered: {ok} | {q}")

    # Scroll to submit (but do not click)
    try:
        submitBtn = driver.find_element(By.CSS_SELECTOR, "input[type='submit'], button[type='submit']")
        driver.execute_script("arguments[0].scrollIntoView({block:'center'});", submitBtn)
    except Exception:
        pass

    print("Filled what matched. Review the page manually. I will NOT submit.")
    input("Press Enter after you review to close the browser...")

except Exception as e:
    print("\nAutomation error occurred. Leaving browser open for manual completion.\n")
    print("Error:", repr(e))
    print("\nTraceback:\n")
    traceback.print_exc()

    try:
        driver.save_screenshot("errorState.png")
        print("\nSaved screenshot: errorState.png\n")
    except Exception:
        pass

    input("Fix anything manually in the open browser, then press Enter to close it...")

finally:
    driver.quit()
