import networkx
import requests

url = 'https://www.ia.pw.edu.pl/~mkamola/dataset-big/4.txt'
r = requests.get(url, allow_redirects=True)
open('4.txt', 'wb').write(r.content)