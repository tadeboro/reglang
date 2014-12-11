- [x] Naštudiraj delovanje avtomata
- [x] Napiši komentarje za delovanje avtomata
- [ ] Definiraj datatype za drevo
		- pomagajmo si s sintakso sml
			datatype tree = NIL | Node (int, leftTree, rightTree)
			datatype string = "" | znak @ string
			
- [ ] Popravi Kleene star za drevo
- [ ] Kriterij za sortiranje:
	- [ ] Sortiraj po številu vozlišč
	- [ ] V vozlišču rekurzivno določimo število vozlišč v levem in desnem poddrevesu: ([4,0], [3,1], [2,2], [1,3], [0,4])

	
Nova ideja nadaljevanja
- regex z capturing groups  (http://www.regular-expressions.info/named.html)
	- uporaba pomnilnika za shranjevanje vmesnih rezultatov (grup)

- [ ] dodaj nov konstruktor za regularni izraz, ki bo definiral novo grupo
- [ ] ustvari naš pomnilnik za grupe
- [ ] napolni pomnilnik
- [ ] poišči grupo v pomnilniku, ko jo zahtevamo
