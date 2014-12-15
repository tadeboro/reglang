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

	
Nova ideja nadaljevanja:
CAPTURING GROUPS
- regex z capturing groups  (http://www.regular-expressions.info/named.html)
	- uporaba pomnilnika za shranjevanje vmesnih rezultatov (grup)

Osnovna verzija	
- [x] dodaj nov konstruktor za regularni izraz, ki bo definiral novo grupo (neuporabno)
- [x] dodaj nov ukaz, ki bo kloniral grupo s podanim imenom (neuporabno)
- [x] runtime
	- [x] ustvari pomnilnik za grupe
	- [x] napolni pomnilnik z vrednostmi
		- [x] zapiši en znak v pomnilnik
		- [x] zapiši neomejeno število znakov pomnilnik
	- [x] preberi vrednost iz pomnilnika
	- [x] preberi vrednost iz pomnilnika in jo vstavi na ustrezno mesto, kjer se sklicujemo na grupo
	
	Kaj dela:
	- [x] (a)1 = (a)a
	- [x] (abc)def1 = (abc)defabc
	- [x] (a*)1 = (), (a)a, (aa)aa, (aaa)aaa

Napredna verzija
	Radi bi dodali še:
	- [ ] gnezdenje grup: (a(b))2 = (a(b))b
	- [ ] poseben znak za 'pastanje' grupe in ne Int številke (opcijsko)
	
	
	Gnezdene grupe (oz. pri nas oklepaji)
		- [ ] Glavni pomnilnik, ki shrani vse grupe (te naj bodo po vrsti v pomnilniku: 1, 2, 3, 4, 5, ...)
		- [ ] Pomožni pomnilnik, ki hrani stevilke grup, v katerih se trenutno nahajamo
			- [ ] Funkcija, ki preverja, če gremo v novo grupo
				- ko pridemo do oklepaja
			- [ ] Funkcija, ki oštevilči novo grupo
			- [ ] Funkcija, ki preverja konec grupe
				- ko pridemo do zaklepaja
			- [ ] Funkcija, ki zbriše številko grupe, ki se je končala, iz pomožnega pomnilnika
		- [ ] Funkcija, ki doda element vsem grupam v gl. pomnilniku, ki so zastopane v pomožnem pomnilniku 	
