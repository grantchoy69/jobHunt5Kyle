from selenium import webdriver
from selenium.webdriver.chrome.options import Options

jobUrl = "https://www.samsara.com/company/careers/roles/7340144?gh_jid=7340144&gh_src=a42fbb361"

chromeOptions = Options()
chromeOptions.add_argument("--start-maximized")

driver = webdriver.Chrome(options=chromeOptions)  # Selenium Manager should handle driver
driver.get(jobUrl)

print("Title:", driver.title)

input("Press Enter to quit...")
driver.quit()
