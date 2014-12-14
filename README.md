Current project status:
[![Build Status](https://travis-ci.org/tadeboro/reglang.svg?branch=master)]
(https://travis-ci.org/tadeboro/reglang)


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
	
	- ne gre čez, ker lahko z lemo o napihovanju dokažemo, da ni konteksno neodvisna gramatika, torej se ne da narediti avtomat

	
Nova ideja nadaljevanja
- regex z capturing groups  (http://www.regular-expressions.info/named.html)
	- uporaba pomnilnika za shranjevanje vmesnih rezultatov (grup)

- [x] dodaj nov konstruktor za regularni izraz, ki bo definiral novo grupo
- [x] dodaj nov ukaz, ki bo kloniral grupo s podanim imenom
- [ ] runtime
	- [x] ustvari naš pomnilnik za grupe
	- [ ] napolni pomnilnik z vrednostmi
		- [x] prvi najbolj osnovni primer dela! (ena grupa, natanko določen znak za klic)
		- [ ] omogoči več grup
		- [ ] bolj pametna oznaka za klicanje grup (capturing)
	- [x] poišči grupo v pomnilniku, ko jo zahtevamo 
