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

- [x] OSNOVNA VERZIJA
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
		- [x] \(a\)1 = (a)a
		- [x] \(abc\)def1 = (abc)defabc
		- [x] \(a*\)1 = (), (a)a, (aa)aa, (aaa)aaa

NAPREDNA VERZIJA
- Radi bi dodali še
	- [x] gnezdenje grup: (a(b))2 = (a(b))b
	- [ ] poseben znak za 'pastanje' grupe in ne Int številke (opcijsko)
	
- [X] Gnezdene grupe (ne perfektno optimalno)
	- [x] Glavni pomnilnik, ki shrani vse grupe (te naj bodo po vrsti v pomnilniku: 1, 2, 3, 4, 5, ...)
	- [x] Pomožni pomnilnik, ki hrani stevilke grup, v katerih se trenutno nahajamo
		- [x] Funkcija, ki preverja, če gremo v novo grupo
			- ko pridemo do oklepaja
			- oštevilči novo grupo
		- [x] Funkcija, ki preverja konec grupe
			- ko pridemo do zaklepaja
		- [x] Funkcija, ki zbriše številko grupe, ki se je končala, iz pomožnega pomnilnika
		- [ ] Funkcija za dodajanje elementov v vse trenutne grupe
			- [x] NI OPTIMALNO 	
				- pomozni pomnilnik je oblike [5,2,1] in za enkrat bomo vzeli grupo in vanjo zapisali znak
				- O(n^2)
			- [x] OPTIMALNO 	
				- v enem prehodu dodamo v vse grupe določen znak
				- O(n)
	- [x] Funkcija, ki doda element vsem grupam v gl. pomnilniku, ki so zastopane v pomožnem pomnilniku 	
	
POPRAVKI
- GROUPS -> (Int, [String])
- vsi-cur

OPAZKE
- grupe ne pokvarijo urejenosti nizov po dolžini -vse nize z grupami lahko enolično preslikamo v nize brez grup

- a(b)1* -> KUL, ker '1' obravnava kot navaden znak, npr 'a'
- a(a(b)+)%2 -> 
	a(a(b)+)     |   a(a(b)+)%2
	aab				aab%b
	aabb			aabb%bb
	
- a(bb)(a|1) -> PADE, ker bi morali hkrati preverjat še dolžino nizov na obeh straneh alternacije

- (a | (b)) (c | \2) -> 2. ni nujno da se sklicuje na b v drugi grupi, ker možno, da ne obstaja, ampak
je naslednji oklep0aj
- back ref, če ga ni -> error

TABLA
- 