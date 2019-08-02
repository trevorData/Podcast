import pandas as pd
import numpy as np
import os
import requests
from bs4 import BeautifulSoup

directory = str(os.environ['filepath']) + 'Projects/Podcasts/Pages/'

df = pd.DataFrame()

# loop through every saved file and save the needed information
for filename in os.listdir(directory):
    if filename.endswith(".html"):
        print(os.path.join(directory, filename))

        # Select file
        soup = BeautifulSoup(open(os.path.join(directory, filename), encoding='utf-8'), "html.parser")

        # Get Earnings Data
        # Transform text into a 2D list of data, then into a DataFrame
        script = soup.find_all('script')[9]
        str_data = str(script).split(';\n    ')[3].replace('var dailyGraph_patronSeriesData = [', '').replace(']]', ']')
        list_data = str_data[1: len(str_data)-1].split('],[')

        raw_data = [list(map(int, item.split(','))) for item in list_data]

        column = pd.DataFrame(raw_data)

        # Fix date formatting
        column.iloc[:, 0] = column.iloc[:, 0] / 1000
        column.iloc[:, 0] = pd.to_datetime(column.iloc[:, 0], unit='s')

        column = column.set_index(0)
        column = column[~column.index.duplicated()]
        column.index = column.index.rename('date')
        column = column.rename(columns={1: filename.replace('.html', '')})

        df = pd.concat([df, column], axis=1, sort=True)

today = pd.to_datetime('today').strftime('%Y-%m-%d')

df.to_csv('C:/Users/trevor.krause/Documents/Projects/Podcasts/data_{}.csv'.format(today), index=True)
