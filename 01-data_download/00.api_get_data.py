import base64
import time
import requests

## oauth

key_secret = 'g5plrknzi6cs8lem7twp4es6b0p7oo44:lTKthq1Kuz07'
key_secret_bytes = key_secret.encode('ascii')
base64_bytes = base64.b64encode(key_secret_bytes)
base64_message = base64_bytes.decode('ascii')

# defining the api-endpoint for oauth
url_oauth = "https://api.idealista.com/oauth/token"

# data to be sent to api for oauth
headers_oauth = {'Authorization': 'Basic ' + base64_message,
                 'Content-Type': 'application/x-www-form-urlencoded'}
parameters_oauth = {'grant_type': 'client_credentials',
                    'scope': 'read'}

# sending post request and saving response as response object
oauth = requests.post(url=url_oauth, headers=headers_oauth, params=parameters_oauth)
token = oauth.json().get('access_token')


##search

# defining the api-endpoint for search

# locationId logic:
# 0-EU-ES-28 = Provincia de Madrid (Codigo de Provincia 28)
# 0-EU-ES-08 = Provincia de Barcelona (Codigo de Provincia 08)
# 0-EU-ES-17 = Provincia de Girona (Codigo de Provincia 17)
# 0-EU-ES-25 = Provincia de Lleida (Codigo de Provincia 25)
# 0-EU-ES-43 = Provincia de Tarragona (Codigo de Provincia 43)

# https://www.ine.es/daco/daco42/codmun/cod_ccaa_provincia.htm

def search(page_num):
    url_search = 'https://api.idealista.com/3.5/es/search'
    # data to be sent to api for oauth
    headers_search = {"Authorization": "Bearer " + token,
                      "Content-Type": "application/x-www-form-urlencoded"}

    parameters_search_coord = {'locale': 'ca',
                               'country': 'es',
                               'operation': 'sale',
                               'propertyType': 'homes',
                               'center': '41.398,2.158',
                               'distance': '5000',
                               'chalet': 'false',
                               'countryhouse': 'false',
                               'maxItems': '50',
                               'numPage': str(page_num)}

    parameters_search_location = {'locale': 'ca',
                                  'country': 'es',
                                  'operation': 'rent',
                                  'propertyType': 'homes',
                                  'locationId': '0-EU-ES-08',
                                  'flat': 'true',
                                  # 'maxPrice': '100000',
                                  'maxItems': '50',
                                  'numPage': str(page_num)}

    search_data = requests.post(url=url_search, headers=headers_search, params=parameters_search_location)
    data = search_data.json()

    return data


houses = []
for p in range(1, 150 + 1):
    print(p)
    houses.append(search(page_num=p))

for p in range(len(houses)):
    with open("01-data_download/output9_rent/Houses_Page" + str(p + 1) + ".json", "w", encoding='utf-8') as outfile:
        outfile.write(str(houses[p]))
