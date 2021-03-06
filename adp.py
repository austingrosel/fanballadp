# docker run -d -p 4444:4444 --shm-size=8g selenium/standalone-chrome
# python adp.py RunAll --local-scheduler --date-interval 2020-02-08-2020-03-04

import time
import datetime
import pandas as pd
import luigi

from selenium import webdriver
from selenium.webdriver.common.desired_capabilities import DesiredCapabilities
from selenium.webdriver.chrome.options import Options

from luigi.contrib.s3 import S3Target, S3Client


class Streams(luigi.Task):
    date_scrape = luigi.DateParameter()

    def run(self):
        date_scrape = self.date_scrape

        driver = webdriver.Remote("http://127.0.0.1:4444/wd/hub", desired_capabilities=DesiredCapabilities.CHROME)
        driver.maximize_window()
        driver.get("https://bestball10s.shgn.com/adp/football")

        time.sleep(5)

        driver.find_element_by_xpath("//*[@id=\"num_teams\"]/option[text()='12']").click()

        from_date_input = driver.find_element_by_xpath("//*[@id=\"from_date\"]")
        from_date_input.clear()
        from_date_input.send_keys(date_scrape.strftime("%m/%d/%Y"))

        to_date_input = driver.find_element_by_xpath("//*[@id=\"to_date\"]")
        to_date_input.clear()
        to_date_input.send_keys(date_scrape.strftime("%m/%d/%Y"))

        btn = driver.find_element_by_xpath('//*[@id="adp_range"]/span/input[1]')
        btn.click()

        time.sleep(5)

        tbl = driver.find_element_by_xpath("//*[@id=\"adp\"]").get_attribute('outerHTML')

        try:
            df = pd.read_html(tbl)[0]
            df = df[['Rk', 'Player', 'Team', 'Position(s)', 'ADP', 'Min', 'Max', '# Picks']]
            df['ScrapeDate'] = date_scrape.strftime("%Y-%m-%d")
        except:
            print("***** Could not find a table for {}".format(date_scrape))
            df = pd.DataFrame()

        driver.close()

        with self.output().open('w') as fout:
            df.to_csv(fout, index=False)

    def output(self):
        client = S3Client()
        return S3Target('s3://luigiadp/adp/ADP_{}.csv'.format(self.date_scrape), client=client)


class RunAll(luigi.WrapperTask):
    date_interval = luigi.DateIntervalParameter()

    def requires(self):
        yield [Streams(date) for date in self.date_interval]


if __name__ == "__main__":
    luigi.run()
