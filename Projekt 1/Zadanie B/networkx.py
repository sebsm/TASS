import networkx as nx
import requests

url = 'https://www.ia.pw.edu.pl/~mkamola/dataset-big/4.txt'
r = requests.get(url, allow_redirects=True)
open('4.txt', 'wb').write(r.content)

# Polecenia do wykonania 
# -   zbadaj jaki jest rząd i rozmiar całej sieci: pierwotnej oraz po usunięciu pętli i duplikatów krawędzi (1);
# -   wyodrębnij największą składową spójną, zbadaj jej rząd i rozmiar (1);
# -   wyznacz aproksymacje średniej długości ścieżki, operując na próbie losowej 100, 1000 i 10 tys. par wierzchołków (2);
# -   wyznacz liczbę rdzeni o największym możliwym rzędzie, o drugim możliwie największym rzędzie o trzecim możliwie największym rzędzie; jakie to są rzędy? (3);
# -   wykreśl rozkład stopni wierzchołków (1);
# -   wyznacz wykładnik rozkładu potęgowego metodą regresji dla dopełnienia dystrybuanty rozkładu stopni, dla przedziałów rozlokowanych logarytmicznie (3);
# -   wyznacz wykres Hilla (3).

##### 1 #####
# -   zbadaj jaki jest rząd i rozmiar całej sieci: pierwotnej oraz po usunięciu pętli i duplikatów krawędzi (1);

G = nx.read_pajek('4.txt')