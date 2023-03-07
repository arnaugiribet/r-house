import os
import json
import csv
from collections import OrderedDict

path_to_outputs = '01-data_download/outputs/'
path_to_folders = [path_to_outputs + folder for folder in os.listdir(path_to_outputs) if 'output' in folder]

path_to_json = []
for path_to_folder in path_to_folders:
    ads_json = os.listdir(path_to_folder)
    for ad_json in ads_json:
        if ad_json.endswith('.json'):
            path_to_json.append(path_to_folder + '/' + ad_json)

# exportar totes les dades
## si fem sale
idealista_properties = open('01-data_download/01_output_idealista_properties.csv', 'w', newline='', encoding='utf-8')

writer_idealista_properties = csv.writer(idealista_properties, delimiter='|')

keys_title = ['propertyCode', 'thumbnail', 'externalReference', 'numPhotos', 'floor', 'price', 'propertyType',
              'operation', 'size', 'exterior', 'rooms', 'bathrooms', 'address', 'province', 'municipality',
              'district', 'country', 'neighborhood', 'latitude', 'longitude', 'showAddress', 'url', 'distance',
              'description', 'hasVideo', 'status', 'newDevelopment', 'hasLift', 'parkingSpace', 'priceByArea',
              'detailedType', 'suggestedTexts', 'hasPlan', 'has3DTour', 'has360', 'hasStaging', 'labels',
              'topNewDevelopment', 'superTopHighlight']

writer_idealista_properties.writerow(keys_title)

i=0
for file_json in path_to_json:
    f = open(file_json, encoding="utf-8")
    s = f.read().strip()
    l = eval(s)
    for dictionary in l['elementList']:
        keys = dictionary.keys()
        for key in keys_title:
            if key not in keys:
                dictionary[key] = None

        list_of_tuples = [(key, dictionary[key]) for key in keys_title]
        dictionary_ordered = OrderedDict(list_of_tuples)
        writer_idealista_properties.writerow(dictionary_ordered.values())
        i += 1

idealista_properties.close()
