% Introductie Functioneel Programmeren (met Haskell)
% Han Joosten
% Juli 2015
# Welkom!

~~~~ {.haskell}
main = putStrLn "hallo!"
~~~~

* Mijn naam is Han Joosten (han.joosten@oridna.nl)

* Ik ben Analist bij RT&O

* Begonnen met Haskell ca. 2006

* Ik help mee bij ontwikkeling van Ampersand (Open source project)

    * [ampersand op github](https://github.com/AmpersandTarski)



# Achtergrond in (functioneel) programmeren

ik ben in aanraking gekomen met functionele talen op de universiteit

	* Twentel, Miranda, Lisp

Diverse specificatie talen:

	* Lotos, ASF+SDF, Typol, LaTeX

Na mijn afstuderen in 1990 heb ik geprogrammeerd in iteratieve talen

    * Pascal, C, Perl, Rexx, Cool:Gen (integrated Case tool)


* Mijn interesse voor Haskell werd gewekt rond 2006

* Haskell is een volwassen allround programmeertaal

* Haskell community is heel behulpzaam

# Waarom zou ik me druk maken over FP?

* Functioneel programmeren maakt dat je anders zal gaan denken over programmeren

    * Mainstream talen gaan over *state*

    * Functioneel programmeren gaat over *values*

* Of je nou overstapt op Haskell of niet, je wordt een ***betere programmeur*** in je eigen favoriete taal.


# Over deze presentatie

* Doel: Enthousiasmeren van jullie

* Veel is (met dank!) gepikt van Bryan O'Sullivan

![Real World Haskell](http://book.realworldhaskell.org/support/rwh-200.jpg) Bryan O'Sullivan is co-auteur van het boek [Real World Haskell](http://book.realworldhaskell.org/). Dit is een gratis online boek dat zeer goed bruikbaar is als leerboek als je verder wilt met Haskell.

* Sheets ook (nu al!) beschikbaar om zelf te doen:

    [http://hanjoosten.github.com/HaskellWorkshop](http://hanjoosten.github.com/HaskellWorkshop)


# Wat kan je verwachten 1

* Haskell is een behoorlijk uitgebreide taal

* Omdat er veel onbekend is voor nieuwkomers, kan je er van uit gaan dat je ver van je comfort-zone zal geraken

* Ik ga je *interessante* dingen leren, maar niet *alles*


# Wat kan je verwachten 2

* Dit is een *hands-on* workshop: je gaat zelf code schrijven!

* Na ongeveer een uur is er een korte pauze

* Schroom niet! Stel gerust vragen!


# Je gereedschap

* Je hebt het Haskell Platform al geïnstalleerd als het goed is.

    * [hackage.haskell.org/platform](http://hackage.haskell.org/platform/)

* We hebben nu een super gereedschapskist

    * De GHC compiler (`ghc`)

    * De GHCi interpreter (`ghci` of `winghci`)

    * De Cabal package manager (`cabal`)

    * Enkele handige libraries en tools


# Wat hebben we nog meer nodig?

* Een text editor

* Een terminal window

* Een 'speel'-directory

# Probleemstelling

Gegeven een website. We willen er data afhalen (web-scraping) en er andere belangrijke web pagina's mee vinden.

Er valt dan ook veel uit te zoeken!

1. Haskell leren

1. Een web pagina downloaden

1. De links van een pagina halen, zodat we meer pagina's kunnen vinden om te downloaden

1. Als we zo ver zijn, bepalen we de pagina's die relevant zijn

1. en ook nog een beetje snel?


# Aan de slag!

Maak in je 'speel'-directory een file `Hello.hs` met de volgende inhoud:

~~~~ {.haskell}
main = putStrLn "hello, world!"
~~~~

De extentie `.hs` is de standaard voor Haskell source files.

De naam van een bestand begint met een hoofdletter en iedereen gebruikt `CamelCase`.


# Vertalen

Dit commando zoekt naar `Hello.hs` in de huidige directory, en vertaalt het:

~~~~
ghc --make Hello
~~~~

Het gegenereerde programma heet `Hello` (`Hello.exe` op
Windows).

* De `--make` optie zorgt er voor dat GHC de afhankelijkheden met andere bestanden en packages automatisch afhandelt.


# Even controleren

Is het iedereen gelukt om hun `Hello` programma te genereren en uit te voeren?


# En nu wat handiger

Het is prettig om snelle, goede code te hebben.

maar als *ik* programmeer, dan:

* moet ik vaak nog veel uitzoeken.

* ik maak best veel fouten.

In deze omstandigheden werkt een echte compiler vertragend.

Er is daarom een interactieve interpreter, `ghci`.


# Aan de slag met GHCi

Simpel genoeg:

~~~~
ghci
~~~~

Er verschijnt wat opstart-tekst, gevolgd door de prompt:

~~~~
GHCi, version 7.0.3: http://www.haskell.org/ghc/  :? for help
Loading package ghc-prim ... linking ... done.
Loading package integer-gmp ... linking ... done.
Loading package base ... linking ... done.
Loading package ffi-1.0 ... linking ... done.
Prelude>
~~~~

Deze standaard prompt vertelt ons welke modules er beschikbaar zijn om mee te spelen.


# Spelen met GHCi

De `ghci` interpreter evalueert interactief expressies.

Probeer nu zelf:

~~~~ {.haskell}
2 + 2
~~~~

~~~~ {.haskell}
123456781234567812345678 * 87654321876543
~~~~

~~~~ {.haskell}
"foo" ++ "bar"
~~~~

(De `++` is de "append" operator.)


# Interpreter commando's

Alle interpreter commando's beginnen met een "`:`" teken.

Laten we ons programma in `ghci` laden:

~~~~
:load Hello.hs
~~~~

De `ghci` prompt verandert:

~~~~
Main>
~~~~


# Onze code uitvoeren met ghci

We hebben een functie met de naam `main` gedefinieerd. Voer het nu uit:

~~~~
main
~~~~

Is het gelukt?

En wat zou dit doen?

~~~~ {.haskell}
putStrLn "hoi mam!"
~~~~


# Enkele van de nuttigste commando's

Onthoud dat alle `ghci` commando's met een "`:`" beginnen.

* `:help` toont welke commando's we tot onze beschikking hebben.

* `:reload` laden van het bestand dat het laatst is ge`:load`.

* `:edit` opent de tekst editor met het bestand dat het laatst is ge`:load`. (echter *niet* automatische `:reload`.)

* `:type` geeft het type van een in Haskell gedefinieerd `ding`.

* `:quit` beëindigt `GHCi`.


# Enkele ghci efficiency tips

We kunnen commando's afkorten:

* `:e` is synoniem voor `:edit`

* `:r` is `:reload`

We hebben ook historie en command line editing.

* Op Unix, vergelijkbaar met `readline`.

* Op Windows, zelfde als `cmd.exe`.


# Gewenning aan de cyclus

Gebruik `:edit` of je tekst editor om de "hello" tekst aan te passen.

Gebruik `:reload` om je bestand opnieuw te laden.

Test je nieuwe definitie van `main`.

* Oefening: Gebruik de "pijl omhoog" toets om door je commando historie te gaan totdat je terug komt op de laatste keer dat je `main` typte.


# Lijsten en Strings

~~~~ {.haskell}
[1,2,3,4]
~~~~

~~~~ {.haskell}
['h','e','l','l','o']
~~~~

Dubbele haakjes zijn alleen maar een verkorte schrijfwijze voor de langere vorm:

~~~~ {.haskell}
"hello"
~~~~

Wat zou dit als resultaat opleveren?

~~~~ {.haskell}
"foo" == ['f','o','o']
~~~~


# Functies aanroepen: 1

We gebruiken spaties om de functie van zijn argumenten te scheiden:

~~~~ {.haskell}
head "foo"
~~~~

~~~~ {.haskell}
head [1,2,3]
~~~~

~~~~ {.haskell}
tail [1,2,3]
~~~~


# Functies aanroepen: 2

Bij meerdere argumenten, worden alle argumenten onderling gescheiden door spaties:

~~~~ {.haskell}
min 3 4
~~~~

Als een argument zelf een samengestelde expressie is, moeten er haken omheen:

~~~~ {.haskell}
compare (3+5) (2+7)
~~~~

~~~~ {.haskell}
max (min 3 4) 5
~~~~


# Zelf doen: 1

Gebruik `ghci` als een rekenmachine.

De `**` operator is voor machtsverheffen.

* Als ik 500 Euro op een bankrekening zet tegen 3% rente per jaar, hoeveel heb ik dan
  na 10 jaar?

# Zelf doen: 1

Gebruik `ghci` als een rekenmachine.

De `**` operator is voor machtsverheffen.

* Als ik 500 Euro op een bankrekening zet tegen 3% rente per jaar, hoeveel heb ik dan
  na 10 jaar?

~~~~
Prelude> 500 * 1.03 ** 10
671.9581896720612
Prelude>
~~~~

# Zelf doen: 2

De notatie `['a'..'z']` genereert een lijst van start tot eind, inclusief.

De `sum` functie sommeert alle elementen in een lijst.

* Wat is de som van de getallen tussen 9 en 250, inclusief, *minus* 2?

# Zelf doen: 2

De notatie `['a'..'z']` genereert een lijst van start tot eind, inclusief.

De `sum` functie sommeert alle elementen in een lijst.

* Wat is de som van de getallen tussen 9 en 250, inclusief, *minus* 2?

~~~~
Prelude> sum [9..250]
31339
Prelude> sum [9..250] -2
31337
Prelude>
~~~~

# Zelf doen: 3

De `show` functie toont een waarde als een String.  Probeer maar!

~~~~ {.haskell}
show (1 == 2)
~~~~

De `length` functie geeft ons het aantal elementen in een lijst.

~~~~ {.haskell}
length [1,2,3]
~~~~

* Uit hoeveel cijfers bestaat het product van alle getallen tussen 0xBE en
  0xEF, inclusief?

# Zelf doen: 3

De `show` functie toont een waarde als een String.  Probeer maar!

~~~~ {.haskell}
show (1 == 2)
~~~~

De `length` functie geeft ons het aantal elementen in een lijst.

~~~~ {.haskell}
length [1,2,3]
~~~~

* Uit hoeveel cijfers bestaat het product van alle getallen tussen 0xBE en
  0xEF, inclusief?

~~~~
Prelude> product [0xBE..0xEF]
332675816112263267856283533963851818076207506005655794001611738031848399818081502871648044328813710016512000000000000
Prelude> length (show (product [0xBE..0xEF]))
117
Prelude>
~~~~

# Zelf functies definiëren

Zelf functies definiëren is best eenvoudig.

Open je tekst editor, maak een nieuw bestand met de `.hs` extentie,
en begin te schrijven:

~~~~ {.haskell}
isOneven x  =  (rem x 2) == 1
~~~~

* We beginnen met de naam van de functie.

* Vervolgens komen de namen van de parameter(s) die we willen, gescheiden door spaties.

* Dan komt een enkel `=` teken, waarachter de *body* van de functie volgt.

Laad je bestand in `ghci` en probeer `isOneven` maar eens uit.


# Smaakmakers

Nu kunnen we heel eenvoudige functies maken, maar we missen belangrijke ingredienten voor het echte werk.

Dus, waar wachten we op!


# Conditionele uitvoering

Q: Hoe ziet de bekende `if` er uit in Haskell?

A: Bekend!

~~~~ {.haskell}
ggd a b = if b == 0
          then a
          else ggd b (rem a b)
~~~~

We hebben de volgende elementen:

* Een Boolean expressie

* `then` een expressie die het resultaat is als de Boolean `True` is

* `else` een expressie die het resultaat is als de Boolean `False` is


# Eindelijk! Een klein beetje over types

De twee mogelijke resultaat expressies van een `if` expressie moeten hetzelfde type hebben.

Als `then` een `String` oplevert, dan moet `else` dat ook doen!

Het volgende voorbeeld is natuurlijk onzin:

~~~~ {.haskell}
if True
then 3.14
else "wombat"
~~~~

Dit soort onzin-typering is in Haskell verboden.


# Wat nog meer over else?

In imperatieve talen kunnen we meestal het `else` deel na de `if` weglaten.

Maar niet in Haskell.

Waarom kan dit wel bij imperatieve talen, en niet in Haskell?


# Een bijna triviale oefening

Schrijf een functie met een string als parameter. Het resultaat is het argument met daarachter `", world"` geplakt, als dat
argument `"hello"` is, of alleen maar het onveranderde argument in andere gevallen.

* Oh ja, De "append"-functie luistert naar de naam `++`.

# Een bijna triviale oefening

Schrijf een functie met een string als parameter. Het resultaat is het argument met daarachter `", world"` geplakt, als dat
argument `"hello"` is, of alleen maar het onveranderde argument in andere gevallen.

* Oh ja, De "append"-functie luistert naar de naam `++`.

~~~~{.haskell}
hw1 str = if str == "hello"
          then str ++ ", world"
          else str
~~~~

of

~~~~{.haskell}
hw2 str = str ++ if str == "hello"
                 then ", world"
                 else ""
~~~~

# Lijsten in Haskell

We weten al hoe een lijst er in Haskell uitziet:

~~~~ {.haskell}
[1,2,3]
~~~~

En natuurlijk de verkorte schrijfwijze voor Strings:

~~~~ {.haskell}
"foo" == ['f','o','o']
~~~~

En misschien had je dit ook al geprobeerd:

~~~~ {.haskell}
[1..]
~~~~

Maar is dit alles wat er valt te weten?


# Lijst constructors

Stel dat we een lijst van de grond af aan willen opbouwen.

* We schrijven de *lege lijst* als `[]`.

* Gegeven een bestaande lijst, kunnen we een ander element aan
het *begin* van de lijst toevoegen met behulp van de `:` operator.


# Typ dit in ghci

Voeg een element toe aan een lege lijst:

~~~~ {.haskell}
1 : []
~~~~


# En nu verder vanaf lijsten met één element

Hoe breiden we zo'n lijst uit?

~~~~ {.haskell}
2 : (1 : [])
~~~~

Je hebt vaste wel geraden dat `[2,1]` een verkorte schrijfwijze is voor
`2:(1:[])`. En dat is inderdaad zo!

Wat is het resultaat van deze expressie?

~~~~ {.haskell}
5 : 8 : [] == [5,8]
~~~~


# Constructors

We refereren aan `[]` en `:` als *constructors*, omdat we ze gebruiken om lijsten mee te construreren.

Als je een lijst maakt, moet de Haskell runtime onthouden welke constructors je hebt gebruikt, en waar precies.

Dus de waarde `[5,8]` wordt gerepresenteerd als:

* Een `:` constructor, met `5` als zijn eerste argument, en als tweede ...

* Nog een `:` constructor, deze keer met `8` als zijn eerste argument, en nu als tweede ...

* Een `[]` constructor.


# Wat zagen we zojuist?

Afhankelijk van je achtergrond, dacht je waarschijnlijk iets als:

![Hee! Haskell lijsten lijken op enkelvoudig gelinkte lijsten](http://upload.wikimedia.org/wikipedia/commons/thumb/6/6d/Singly-linked-list.svg/408px-Singly-linked-list.svg.png)

![Hee! Dat lijkt op de `cons` cellen in Lisp!](http://upload.wikimedia.org/wikipedia/commons/thumb/1/1b/Cons-cells.svg/300px-Cons-cells.svg.png)


Inderdaad.


# Waarom zouden we ons druk maken om constructors?

Haskell moet natuurlijk onthouden hoe een lijst is opgebouwd.

Het laat *ons* ook een lijst inspecteren, zodat we kunnen nagaan welke contstructors er gebruikt zijn.

Hoe doen we dat?

Stel dat we van een string willen weten of die begint met een hoofdletter.  


# De case-expressie

De `case` expressie laat ons een structuur *inspecteren*.

~~~~ {.haskell}
import Data.Char

isCapitalized name
  = case name of
      []           -> False
      (first:rest) -> isUpper first
~~~~

* Tussen `case` en `of` staat de te inspecteren expressie.
* Als de structuur overeenkomt met de lege-lijst constructor `[]`, dan is
 de structuur `name` die we inspecteren leeg. Dus geen hoofdletter.

Als de gebruikte constructor de "aan de voorkant toevoegen" `:` operator is,
dan wordt het interessanter.

* Wat er ook maar als eerste argument gebruikt is voor de `:` constructor wordt gebonden
  aan de naam `first`.
* Het tweede argument van de `:` constructor (dus alles in de lijst na het eerste element) wordt gebonden aan de naam `rest`.
* De expressie volgend op `->` wordt geëvalueerd met deze waarden.

# Pattern matching

Wat de `case` expressie doet, noemen we *pattern matching*.

* Patronen worden gecontroleerd van boven naar beneden.

* Zodra een een patroon 'matcht', wordt het rechter deel (het deel achter de `->`) gebruikt
  als het resultaat van de hele `case` expressie.

* Als geen enkel patroon 'matcht', ontstaat een exceptie.


# Een voorbeeld

Laten we stap voor stap kijken wat er gebeurt als we deze expressie evalueren.

~~~~ {.haskell}
isCapitalized "Ann"
~~~~

# Een voorbeeld

~~~~ {.haskell}
isCapitalized name
  = case name of
      []           -> False
      (first:rest) -> isUpper first
~~~~

* isCapitalized ('A' : 'n' : 'n' : [] )

> * *matcht []  ? Nee.*
> * *matcht (first:rest)  ? Ja!*
> * first *krijgt als waarde* 'A'
> * rest *krijgt als waarde* 'n' : 'n' : []
> * isCapitalized ('A' : 'n' : 'n' : [] ) *wordt herschreven als* isUpper 'A'
> * isUpper 'A' *evalueert naar* True

# Whew! Een paar oefeningen!

Eindelijk! We kunnen nu iets ingewikkelder functies schrijven.

Nu je het begin van een lijst kan inspecteren, zou je een *hele* lijst recursief kunnen afhandelen.

Schrijf eerst een functie met de naam `myLength` die het aantal elementen in een lijst bepaalt.

Vervolgens schrijf je een functie met de naam `countCaps` die het aantal hoofdletters in een String bepaalt.

~~~~ {.haskell}
countCaps "Monkey Butter" == 2
~~~~

# Whew! Een paar oefeningen!

Eindelijk! We kunnen nu iets ingewikkelder functies schrijven.

Nu je het begin van een lijst kan inspecteren, zou je een *hele* lijst recursief kunnen afhandelen.

Schrijf eerst een functie met de naam `myLength` die het aantal elementen in een lijst bepaalt.

Vervolgens schrijf je een functie met de naam `countCaps` die het aantal hoofdletters in een String bepaalt.

~~~~ {.haskell}
countCaps "Monkey Butter" == 2
~~~~

~~~~ {.haskell}
myLength lijst =
   case lijst of
     []   -> 0
     e:es -> 1 + myLength es
~~~~

# Het tellen van Hoofdletters

Jôh, die countCaps functie was knap lastig. Of niet?

Hier is mijn definitie, gebruik makend van alleen de dingen die we tot nu toe hebben geleerd:

~~~~ {.haskell}
countCaps string =
  case string of
    []     -> 0
    (x:xs) -> if isUpper x
              then 1 + countCaps xs
              else countCaps xs
~~~~


# Huh.

Ik dacht dat Haskell zo eenvoudig leesbaar was!?


# Leesbaarheid 1: top-level pattern matching

~~~~ {.haskell}
countCaps []     = 0
countCaps (x:xs) =
    if isUpper x
    then 1 + countCaps xs
    else countCaps xs
~~~~

We kunnen een functie definieren als een serie vergelijkingen, die elk een `pattern match` bevatten.

Dit is een beter leesbare schrijfwijze voor `case`.


# Leesbaarheid 2: guards

~~~~ {.haskell}
countCaps []     = 0
countCaps (x:xs)
   | isUpper x    = 1 + countCaps xs
   | otherwise    = countCaps xs
~~~~

Achter elke `|` staat een *guard*.

* Als een patroon matcht, evalueren we iedere Boolean guard expressie van boven naar beneden.

* Zodra er een succes heeft, evalueren we de rechterkant als de `body` van de functie.

(Jazeker, patronen in een `case` kunnen ook guards hebben.)


# Vóór

Zoals in de originele versie, maar nu zonder gebruik van `case`:

~~~~ {.haskell}
countCaps xs =
  if null xs
  then 0
  else if isUpper (head xs)
       then 1 + countCaps (tail xs)
       else countCaps (tail xs)
~~~~

# Vóór en Na

Zoals in de originele versie, maar nu zonder gebruik van `case`:

~~~~ {.haskell}
countCaps xs =
  if null xs
  then 0
  else if isUpper (head xs)
       then 1 + countCaps (tail xs)
       else countCaps (tail xs)
~~~~

Zowel korter als beter leesbaar:

~~~~ {.haskell}
countCaps []     = 0
countCaps (x:xs)
   | isUpper x    = 1 + countCaps xs
   | otherwise    = countCaps xs
~~~~


# Een andere aanpak

Schrijf een nieuwe versie van `countCaps`:

* Schrijf een functie die door een lijst loopt, en die een nieuwe lijst genereert
  bestaande uit alleen de hoofdletters van de oorspronkelijke lijst.

~~~~ {.haskell}
~~~~

* Gebruik `length` om het aantal elementen te tellen.

Dit zou hetzelfde resultaat moeten opleveren als je eerste functie. Toch?

~~~~ {.haskell}
~~~~

# Een andere aanpak

Schrijf een nieuwe versie van `countCaps`:

* Schrijf een functie die door een lijst loopt, en die een nieuwe lijst genereert
  bestaande uit alleen de hoofdletters van de oorspronkelijke lijst.

~~~~ {.haskell}
allCaps [] = []
allCaps (x:xs) = if isUpper x
                 then x : allCaps xs
                 else allCaps xs
~~~~

* Gebruik `length` om het aantal elementen te tellen.

Dit zou hetzelfde resultaat moeten opleveren als je eerste functie. Toch?

~~~~ {.haskell}
countCaps2 string = length (allCaps string)
~~~~


# Wijziging van de specificatie

Stel dat we het aantal kleine letters in een string willen tellen.

Dit lijkt bijna hetzelfde als onze functie om hoofletters te tellen.

Wat kunnen we doen met deze observatie?

> * We zouden een **functie als parameter** willen gebruiken!


# Hogere orde functies

*Hogere orde functie*: een functie die een andere functie accepteert als argument.

~~~~ {.haskell}
filter pred [] = []
filter pred (x:xs)
  | pred x     = x : filter pred xs
  | otherwise  =     filter pred xs
~~~~

Hoe kunnen we dit gebruiken om `countLowerCase` te definieren?

~~~~ {.haskell}
~~~~

# Hogere orde functies

*Hogere orde functie*: een functie die een andere functie accepteert als argument.

~~~~ {.haskell}
filter pred [] = []
filter pred (x:xs)
  | pred x     = x : filter pred xs
  | otherwise  =     filter pred xs
~~~~

Hoe kunnen we dit gebruiken om `countLowerCase` te definieren?

~~~~ {.haskell}
countLowerCase string =
  length (filter isLower string)
~~~~


# Data in, data uit

Tot nu toe hebben we verschillende definities gezien zoals deze:

~~~~ {.haskell}
countLowerCase string =
  length (filter isLower string)
~~~~

We zien hierin een terugkerend pattroon:

* Een functie met één argument

* krijgt als input het resultaat van ...

* ... een andere functie met één argument


# Functie compositie

Haskell beperkt ons niet tot alleen maar alfanumerieke namen voor functies.

We kunnen een functie definiëren met de simpele naam "`.`", die we als een operator kunnen gebruiken:

~~~~ {.haskell}
(f . g) x = f (g x)
~~~~

Wat is hier het nut van?

~~~~ {.haskell}
countLowerCase = length . filter isLower
~~~~


# Compositie???

Enige toelichting is wel op z'n plek, om het duidelijker te maken.

We plaatsen de argumenten in de rechterkant van onze functie definitie:

~~~~ {.haskell}
(f . g) x = f (g x)
~~~~

Als eerste argument van "`.`" hadden we `length`, en `filter isLower`
als het tweede:

~~~~ {.haskell}
(length . filter isLower) x
  = length (filter isLower x)
~~~~


# Lokale variabelen

Binnen in een expressie kunnen we nieuwe variabelen introduceren door gebruik te maken van `let`.

~~~~ {.haskell}
let x = 2
    y = 4
in x + y
~~~~

* Lokale definities komen na `let`.

* De expressie waarin we ze gebruiken komen na `in`.


# White space

Haskell is gevoelig voor white space!

* Een top-level definitie start in de meest linkse kolom.

* Volgend op het begin van een definitie, als de volgende regel
  inspringt, wordt die regel behandeld als vervolg van die definitie.

* Gebruik nooit 'Tab' in je source files.


# White space en lokale variabelen

Als je lokale variabelen definieert, dan moeten ze allemaal beginnen in dezelfde kolom.

Dit is goed:

~~~~ {.haskell}
let x = 2
    y = 4
in x + y
~~~~

Maar dit niet:

~~~~ {.haskell}
let x = 2
      y = 4
in x + y
~~~~


# Oefenen met compositie

Schrijf een functie die een String accepteert en die een nieuwe String
oplevert, die alleen de woorden bevat die met een klinker beginnen.
Maak hierbij zoveel mogelijk gebruik van functie compositie.

* Voor je begint, is het verstandig om eerst met de `words` en `unwords` functies te spelen.

Voorbeeld:

~~~~ {.haskell}
klinkerWoorden "Ik denk, dus ik ben."
  == "Ik ik"
~~~~

~~~~ {.haskell}
~~~~

# Oefenen met compositie

Schrijf een functie die een String accepteert en die een nieuwe String
oplevert, die alleen de woorden bevat die met een klinker beginnen.
Maak hierbij zoveel mogelijk gebruik van functie compositie.

* Voor je begint, is het verstandig om eerst met de `words` en `unwords` functies te spelen.

Voorbeeld:

~~~~ {.haskell}
klinkerWoorden "Ik denk, dus ik ben."
  == "Ik ik"
~~~~

~~~~ {.haskell}
klinkerWoorden =
  let isKlinker c = toLower c `elem` "aeiou"
  in  unwords . filter (isKlinker . head) . words
~~~~

Doet dit je denken aan een Unix shell pipeline, maar dan van rechts naar links?


# Oh ja, hoe zat het met onze probleemstelling?

Gegeven een website. We willen er data afhalen (web-scraping) en er andere belangrijke web pagina's mee vinden.

Nu zijn we Haskell pro's, toch?

* Een web pagina downloaden


# Doen: Een web pagina downloaden!

Eigenlijk willen we gebruik maken van een library om een webpagina te kunnen downloaden.

Gelukkig bestaat er voor dit soort gevallen een hele handige centrale repository van open
source Haskell software:

* [http://hackage.haskell.org](http://hackage.haskell.org/)

* (In de volksmond: "Hackage")

Ga er nu heen!

Click op de
[Browse](http://hackage.haskell.org/packages)
link om door packages te bladeren

Helaas is het een overweldigende grote lijst, maar wie geduld heeft, kan er altijd nuttige dingen vinden.

Wie heeft geduld?


# Zoeken

Bladeren door *duizenden* libraries kost veel tijd/moeite. Gelukkig kan er ook worden gezocht.

Click op de
[Search](http://hackage.haskell.org/packages/search)
link om packages te doorzoeken.

zoek op keyword *http*

Je hebt nu nog wel heel veel packages om door te akkeren, maar het is beter dan de duizenden packages op de Packages pagina.

# Even de zoektocht 'helpen'.

We gebruiken vandaag de *http-conduit* library. Die ziet er goed uit, en is nog redelijk recent.
Laten we eens kijken of we daar wat mee kunnen.

De documentatie staat natuurlijk online:

* [http://hackage.haskell.org/package/http-conduit](http://hackage.haskell.org/package/http-conduit)

De startpagina voor een package kan intimiderend overkomen. Laat je niet gek maken. Kijk maar eens onderaan, bij de sectie "Modules".

Wat zie je?


# Een package installeren

We moeten `http-conduit` eerst installeren, voordat we er gebuik van kunnen maken.

Het installeren van het `http` package gaat met behulp van een eenvoudig commando:
(eerst doen we nog even een update van de lijst van packeges, want dat verandert met de dag)

~~~~
cabal update
cabal install http-conduit
~~~~

Dit commando bepaalt alle andere packages waar `http-conduit`
afhankelijk van is. Vervolgens worden al deze packages gedownload,
vertaald en geïnstalleerd.

Dit kan wel een paar minuten duren. De compiler output verschijnt op het scherm.


# Documentatie lezen: packages en modules

Nu we aan het wachten zijn tot het `http-conduit` package gereed is
voor gebruik, kunnen we vast uitzoeken hoe we het kunnen gebruiken.

Herinner je de link nog naar de API documentatie, onderin de webpage van de package?
Ga daar maar eens heen en bekijk de documentatie.

Een API pagina begint met een titel die er als volgt uit kan zien:

~~~~
Network.HTTP.Conduit
~~~~

Dit is de naam van een *module*.

Een module is een verzameling van samenhangende code.

Een *package* is een verzameling van samenhangende modules.


# Documentatie lezen: de rest

Na de gebruikelijke initiele bla bla, bestaat de documentatie van een module uit
type signatures en beschrijvingen.

Dit is een heel eenvoudige type signature:

~~~~
foo :: String
~~~~

Wat is dat nou weer?

De *naam* van het ding dat hier wordt gedefinieerd komt voor het `::` teken.

Het *type* ervan volgt na de `::`

Dit betekent dat "de waarde met de naam `foo` is van het type `String`".


# Type systeem van Haskell

Tot nu toe hebben we ons niet druk gemaakt over typering of type signatures.

Elke expressie en waarde in Haskell heeft één type.

Deze types kunnen vrijwel altijd automatisch worden *afgeleid* door
de compiler of interpreter.


# Veel voorkomende basis types

* `Bool`

* `Int`

* `Char`

* `Double`


# De signature van een functie

Hier is nog een type signature:

~~~~
words :: String -> [String]
~~~~

we zien een nieuw symbool, `->`. Die zegt "dit is een functie".

Het type na de laatste `->` is het return type van de functie.

Alles ertussen zijn de types van de argumenten.

Dus dit is een functie die een `String` als argument heeft. En het resultaat is een... wat?


# Lijst notatie

De notatie `[a]` betekent "een lijst van waarden, elk met een of ander type `a`".

Dus `[String]` betekent "een lijst van waarden, elk met type `String`".


# Type synoniemen

Wat is een `String` eigenlijk?

* Het is niks bijzonders, maar gewoon een *synoniem* voor `[Char]`, oftewel "een lijst van `Char`".

We kunnen zelf ook synoniemen definiëren.

~~~~ {.haskell}
type Euros = Int
~~~~

Een type synoniem kan handig zijn om de bedoeling van een bestaand type te documenteren.


# words

~~~~
words :: String -> [String]
~~~~

We zien nu dat deze functie een String als argument accepteert, en een lijst van Strings teruggeeft.

Als we dit zo zien, kan je dan raden wat `words` zou kunnen doen?


# Nog een signature

Wat kan je hierover vertellen?

~~~~
mystery :: [String] -> String
~~~~

Welk gedrag zou deze functie kunnen hebben?


# Documentatie uit de *echte* wereld

Hier is het allereerste signature uit `http-conduit`:

~~~~
simpleHttp :: MonadIO m => String -> m ByteString
~~~~

Dit is ingewikkelder! Hoe lezen we dit nu weer?

Tussen `'::'` en `'=>'` zien we *constraints* over het gebruik van
`simpleHttp` - Dit kunnen we nu wel even laten voor wat het is.

* *Belangrijk*: Meestal kan je zonder gevaar dingen negeren die je (nog) niet begrijpt.

De m laten we ook maar even voor wat het is.

Wat kunnen we nu zeggen over deze functie?

> * Inderdaad: Het is een functie die een String als parameter heeft, en die een ByteString teruggeeft.

# Speelkwartier met ghci!

Heeft iedereen `http-conduit` inmiddels geïnstalleerd?

Start `ghci`, en laten we eens wat spelen met de module:

~~~~
import Network.HTTP.Conduit
~~~~

Merk op dat de prompt verandert zodra we het hebben getypt:

~~~~
Prelude Network.HTTP.Conduit>
~~~~

We weten hierdoor dat de module geladen en beschikbaar is voor gebruik.


# Ophalen van een webpagina

Eindelijk - we gaan nu een webpagina ophalen!

~~~~
simpleHTTP "http://example.com/"
~~~~

Kreeg je een hoop HTML in je terminal window te zien? Yeah!


# Van binary naar tekst

We hebben nu een `ByteString`, die we om moeten zetten naar tekst, om er mee te kunnen manipuleren.

Voor het gemak nemen we even aan, dat alle web pagina's UTF-8 gecodeerd zijn.


# Pure code

Tot nu toe is alle code die we hebben geschreven "Puur".

* Het gedrag van al onze functies waren alleen afhankelijk van de input.

* Al onze data is onveranderlijk.

* Er is dus geen manier om een globale variabele of het gedrag
 van een functie aan te passen


# niet-pure code

En toch ... Op een of andere manier hebben we een webpagina gedownload!

* Webpagina's zijn duidelijk *niet* puur.

Hoe kan dit dan?

~~~~ {.haskell}
length (simpleHttp "http://x.org/")
~~~~

HELAAS.

Het type systeem van Haskell maakt onderscheid tussen code die puur moet zijn en code
dat zij-effecten mag hebben ("niet-pure" code).


# Wat nu?

Laten we eerst eens kijken naar een eenvoudiger voorbeeld dan `simpleHttp`.

Typ dit in `ghci`:

~~~~
:type readFile
~~~~

We krijgen nu het type van `readFile` te zien.


# IO

Het `:type` commando toont nu iets als dit:

~~~~ {.haskell}
readFile :: FilePath -> IO String
~~~~

Zie je het `IO` in het resulterend type?

Dat betekent "deze functie kan zij-effecten hebben".

niet-pure functies, met `IO` in het resulterend type, worden vaak ***acties*** genoemd.

* Dit helpt om ze te onderscheiden van pure functies.


# Mengen van *acties* met *pure code*

Het type-systeem 'weet' welke functies `IO` doen en zorgt ervoor dat we hier op een nette manier mee omgaan.

We kunnen echter op een natuurlijke manier pure code mengen met acties:

~~~~ {.haskell}
charCount fileName =
  do contents <- readFile fileName
     return (length contents)
~~~~


# "do" notatie

Cruciaal in wat we net zagen was het **do** keyword aan het begin van de functie definitie.

Het introduceert een serie van `IO` acties. Één per regel.


# Opvangen van het resultaat van acties

Om het resultaat van een `IO` actie op te vangen, gebruiken we `<-` in plaats van `=`.

~~~~ {.haskell}
contents <- readFile fileName
~~~~

Het resultaat (`contents`) is puur - Het heeft *geen* `IO` type.

Hiermee voeden we pure code met data uit acties (niet-pure code).


# De "return" actie

Dit is *niet* het `return` type zoals je gewend bent!

Het neemt een *pure* waarde (zonder `IO` in zijn type), en *wikkelt* het in het `IO` type.

Pure code kan niet-pure code nooit aanroepen. Het kan wel data opleveren aan de niet-pure wereld door gebruik te maken van `return`.


# Haskell programs en IO

Elk Haskell programma heeft als entry point een functie met als naam
`main`.

Het type van `main` is altijd:

~~~~ {.haskell}
main :: IO ()
~~~~

`()` heet "unit", en betekent zo ongeveer hetzelfde als `void`
in C or Java.

Dit betekent dat *alle* haskell programma's niet-puur zijn!


# Binary naar tekst

Herinner je nog dat we probeerden vals te spelen?

We hadden dit:

~~~~ {.haskell}
simpleHttp :: MonadIO m => String -> m ByteString
~~~~

In plaats hiervan hebben we iets nodig met als resultaat `IO String`.

Hoe moet dat er uit zien?

we zoeken een functie met het volgende type:

~~~~{.haskell}
eenOfAndereConversieFunctie :: ByteString -> String
~~~~

we maken gebruik van ***[Hayoo!](http://holumbus.fh-wedel.de/hayoo/hayoo.html)***

# UTF-8 conversie

Om de conversie te doen, gaan we gebruik maken van het package `utf8-string`, dat we dankzij Hayoo hebben gevonden.

~~~~
cabal install utf8-string
~~~~

Dit package bevat een module met de naam `Data.ByteString.Lazy.UTF8`.

~~~~ {.haskell}
import Data.ByteString.Lazy.UTF8
~~~~

Hierin is een functie gedefiniëerd met de naam `toString`:

~~~~ {.haskell}
toString :: ByteString -> String
~~~~


# UTF-8 conversie oefening

Gebruik `toString` om een actie te schrijven die een URL downloadt en het resultaat converteert naar een `String`.

Schrijf eerst het type op van de actie.

~~~~ {.haskell}
~~~~

* Haskell definities hebben gewoonlijk geen type signature nodig.

* Ondanks dit is het een goede gewoonte om ze als *documentatie* op te schrijven bij top-level
  definities.


# UTF-8 conversie oefening

Gebruik `toString` om een actie te schrijven die een URL downloadt en het resultaat converteert naar een `String`.

Schrijf eerst het type op van de actie.

~~~~ {.haskell}
download :: String -> IO String
~~~~

* Haskell definities hebben gewoonlijk geen type signature nodig.

* Ondanks dit is het een goede gewoonte om ze als *documentatie* op te schrijven bij top-level
  definities.

~~~~ {.haskell}
~~~~

# UTF-8 conversie oefening

Gebruik `toString` om een actie te schrijven die een URL downloadt en het resultaat converteert naar een `String`.

Schrijf eerst het type op van de actie.

~~~~ {.haskell}
download :: String -> IO String
~~~~

* Haskell definities hebben gewoonlijk geen type signature nodig.

* Ondanks dit is het een goede gewoonte om ze als *documentatie* op te schrijven bij top-level
  definities.

~~~~ {.haskell}
download :: String -> IO String
download url = do res <- simpleHttp url
                  return (toString res)
~~~~

# Downloaden en opslaan van een web pagina

Gebruik je `download` functie om een lokale kopie op te slaan van een webpagina.

~~~~ {.haskell}
saveAs :: String -> Int -> IO ()
~~~~

Laten we voor het gemak de lokale files opslaan met namen die nummers bevatten:

~~~~ {.haskell}
makeFileName :: Int -> FilePath
makeFileName k = "download-" ++ show k ++ ".html"
~~~~

Om een lokale kopie van een bestand op te slaan, heb je de actie `writeFile` nodig.

~~~~ {.haskell}
~~~~

# Downloaden en opslaan van een web pagina

Gebruik je `download` functie om een lokale kopie op te slaan van een webpagina.

~~~~ {.haskell}
saveAs :: String -> Int -> IO ()
~~~~

Laten we voor het gemak de lokale files opslaan met namen die nummers bevatten:

~~~~ {.haskell}
makeFileName :: Int -> FilePath
makeFileName k = "download-" ++ show k ++ ".html"
~~~~

Om een lokale kopie van een bestand op te slaan, heb je de actie `writeFile` nodig.

~~~~ {.haskell}
saveAs :: String -> Int -> IO ()
saveAs url k =
  do content <- download url
     writeFile (makeFileName k) content
~~~~

# Spitten in HTML

Twee waarheden:

* De meeste HTML in het wild is een zooitje.

* Zelfs het parsen van nette HTML is ingewikkeld.

Laten we voor de verandering maar weer eens een library gaan gebruiken!

~~~~
cabal install tagsoup
~~~~

De `tagsoup` package kan willekeurig rommelige HTML aan.

Het levert ons een lijst met events, vergelijkbaar met een SAX parser.


# Omgaan met problemen

Probeer dit:

~~~~ {.haskell}
head [1]
~~~~

Probeer nu dit:

~~~~ {.haskell}
head []
~~~~


# Oei

Als we een lege lijst aanleveren, levert de `head` functie een foutmelding.

Stel dat we een versie van `head` nodig hebben die *geen* foutmelding geeft.

~~~~ {.haskell}
safeHead :: [a] -> ????
~~~~

Wat zou de `????` moeten zijn?

Laten we wat proberen.

~~~~ {.haskell}
safeHead (x:xs) = Some x
safeHead []     = None
~~~~


# Some? None?

* We gebruiken een constructor met de naam `Some` om het idee te vangen: "We hebben een resultaat".

* De constructor `None` geeft aan: "We hebben hier geen resultaat".

Om deze constructors in het leven te roepen, definiëren we een nieuw type.

~~~~ {.haskell}
data Perhaps a = Some a
               | None
~~~~

Het `|` teken scheidt de constructors. We lezen het als volgt:

* Het `Perhaps` type kent twee constructors:

* `Some` gevolgd door één argument

* of `None` zonder argumenten


# Maybe

Eigenlijk heeft Haskell al een `Perhaps` type.

~~~~ {.haskell}
data Maybe a = Just a
             | Nothing
~~~~

De `a` is een *type parameter*, met als betekenis dat als we dit type opschrijven,
dat we dan een ander moeten toevoegen als parameter:

* `Maybe Int`

* `Maybe String`


# Gebruik maken van constructors

Als we een `Maybe Int` willen samenstellen door gebruik te maken van de `Just` constructor, dan
moeten we een `Int` meegeven.

~~~~ {.haskell}
Just 1  :: Maybe Int
Nothing :: Maybe Int
~~~~

Dit gaat niet werken, want de types komen niet overeen:

~~~~ {.haskell}
Just [1] :: Maybe String
~~~~


# Pattern matching met constructors

We kunnen pattern matchen met constructors van `Maybe` op dezelfde manier als dat we dat hebben gedaan voor lijsten.

~~~~ {.haskell}
case foo of
  Just x  -> x
  Nothing -> bar
~~~~


# Tags

Het `tagsoup` package definieert het volgende type:

~~~~ {.haskell}
data Tag = TagOpen String [Attribute]
         | TagClose String
         | TagText String
         | TagComment String
         | TagWarning String
         | TagPosition Row Column
~~~~

Wat denk je dat deze constructors kunnen betekenen?


# Pattern matching over een Tag

Stel dat we een predikaat willen schrijven die aangeeft of een `Tag` een opening tag is.

* Wat zou het type van deze functie zijn?

~~~~ {.haskell}
~~~~

* Hoe zou de body van de functie er uit zien?


# Pattern matching over een Tag

Stel dat we een predikaat willen schrijven die aangeeft of een `Tag` een opening tag is.

* Wat zou het type van deze functie zijn?

~~~~ {.haskell}
isOpenTag :: Tag -> Bool
~~~~

* Hoe zou de body van de functie er uit zien?

~~~~ {.haskell}
~~~~

# Om het even!

Onze eerste body zag er als volgt uit:

~~~~ {.haskell}
isOpenTag :: Tag -> Bool
isOpenTag (TagOpen x y)     = True
isOpenTag (TagClose x)      = False
isOpenTag (TagText x)       = False
isOpenTag (TagComment x)    = False
isOpenTag (TagWarning x)    = False
isOpenTag (TagPosition x y) = False
~~~~

Begrijpbaar, maar lelijk.

* We zijn maar in precies één constructor geïnteresseerd.

* We gebruiken de gedeclareerde variabelen `x` or `y` helemaal niet.


# Het wild card patroon

We kunnen met het **"`_`"** teken opschrijven dat het ons niet uitmaakt
wat een patroon of variabele precies is.

~~~~ {.haskell}
isOpenTag :: Tag -> Bool
isOpenTag (TagOpen _ _) = True
isOpenTag  _            = False
~~~~

Het wild card patroon matcht altijd.

* Aangezien we toch geen gebruik maken van `x` of `y`, kunnen we dat expliciet aangeven door gebruik te maken van `_`.

* Aangezien elke constructor behalve `TagOpen` ons niets interesseert, kunnen we ze allemaal laten matchen met `_`.


# Even een vraag tussendoor

Waarom schrijven we de functie niet op deze manier?

~~~~ {.haskell}
isOpenTag :: Tag -> Bool
isOpenTag  _            = False
isOpenTag (TagOpen _ _) = True
~~~~


# Extractie van de links van een webpagina

Stel dat we al een pagina geladen hebben.

* Bekijk de `tagsoup` documentatie in de module `Text.HTML.TagSoup` .

* Zoek een functie die een webpagina parsed in een serie tags.


# Die kunnen we mooi gebruiken!

~~~~ {.haskell}
processPage url = do
  page <- download url
  return (parseTags page)
~~~~


# Tags opschonen

De geparsede tags kunnen verschillende soorten tag namen hebben.

~~~~
<A HREF="...">
~~~~

~~~~
<a hrEF="...">
~~~~

* Zoek een `tagsoup` functie die tag namen en attributen omzet in kleine letters.


# Tags in normaalvorm

Laten we onze functie gebruiken om het resultaat van `parseTags` op te schonen.

~~~~ {.haskell}
processPage url = do
  page <- download url
  return
    (canonicalizeTags
      (parseTags page))
~~~~


# Extractie van links

We zijn alleen geïnteresseerd in open tags die links bevatten, dus `<a>` tags.

* Hoe zouden we het type functie schrijven die aangeeft of een `Tag` een open tag is met de juiste naam?

* Hoe zouden we die functie gebruiken om alleen de open tags over te houden van een lijst van geparsede tags?


# Poeh hee!

Deze cascade begint belachelijke vormen te krijgen.

~~~~ {.haskell}
processPage url = do
  page <- download url
  return
    (filter (isTagOpenName "a")
      (canonicalizeTags
        (parseTags page)))
~~~~

Twee observaties:

* Onze actie is nu voor het merendeel pure code.

* Het lijkt wel een pijplijn.


# Een herschrijf oefening

Verdeel de functie in pure en niet-pure delen.

Schrijf het pure gedeelte met gebruikmaking van functie compositie.

~~~~ {.haskell}
processPage url = do
  page <- download url
  return
    (filter (isTagOpenName "a")
      (canonicalizeTags
        (parseTags page)))
~~~~


# Mijn oplossing

~~~~ {.haskell}
processPage url = do
  page <- download url
  return (process page)

process =
    filter (isTagOpenName "a") .
    canonicalizeTags .
    parseTags
~~~~


# We willen nog meer kwijt

Laten we de `nofollow` links ook weglaten.

We willen het `"rel"` attribuut van een tag.

* Zoek een functie die een attribuut van een tag teruggeeft.


# Geen following

~~~~ {.haskell}
nofollow tag = fromAttrib "rel" tag == "nofollow"
~~~~

~~~ {.haskell}
process =
    filter (not . nofollow) .
    filter (isTagOpenName "a") .
    canonicalizeTags .
    parseTags
~~~~


# We hebben een lijst met \<a> tags

Hoe zouden we het `"href"` attribuute ophalen van elk element uit de lijst?


# Allen niet-lege \<a href\> tags

~~~~ {.haskell}
process =
    filter (not . null) .
    map (fromAttrib "href") .
    filter (not . nofollow) .
    filter (isTagOpenName "a") .
    canonicalizeTags .
    parseTags
~~~~


# URLs in normaalvorm

Links kunnen absoluut, relatief of onzinnige rommel zijn. We willen alleen maar absolute links die er geldig uitzien.

Om netjes een absolute link te maken moeten we de absolute URL kennen van de pagina waar we naar kijken.

~~~~ {.haskell}
canonicalizeLink :: String -> String -> Maybe String
~~~~


# Werken met URIs

Het package `Network.URI` bevat een paar functies die hier goed van pas komen.

~~~~ {.haskell}
parseURI :: String -> Maybe URI
parseURIReference :: String -> Maybe URI
uriToString id "" :: URI -> String
nonStrictRelativeTo :: URI -> URI -> Maybe URI
~~~~


# Een draak van een inspringer

Dit is niet meer om te lezen!

~~~~ {.haskell}
import Network.URI

canon :: String -> String -> Maybe String
canon referer path =
  case parseURI referer of
    Nothing -> Nothing
    Just r  ->
      case parseURIReference path of
        Nothing -> Nothing
        Just p  ->
          case nonStrictRelativeTo p r of
            Nothing -> Nothing
            Just u ->
             Just (uriToString id u "")
~~~~

Dat kan vast beter.


# Traplopen

Is het je opgevallen dat die functie een serie is van met `case` inspecties van
`Maybe` waarden?

Stel je voor dat we een functie zouden hebben die een gewone waarde als input heeft, en die een `Maybe` waarde teruggeeft.

~~~~ {.haskell}
a -> Maybe b
~~~~

En stel je voor dat we een begrijpbare syntax hebben om een anonieme functie te schrijven.

~~~~ {.haskell}
\a -> "hoi mam! " ++ a
~~~~

De `\` noemen we "lambda".


# Observatie

De inspectie met `case` is nogal breedsprakig. Stel dat we een functie zouden
hebben die de inspectie deed, en een andere functie zou aanroepen als de waarde
een `Just` was.

~~~~ {.haskell}
bind :: Maybe a -> (a -> Maybe b) -> Maybe b
bind  Nothing      _     = Nothing
bind (Just value) action = action value
~~~~


# Het gebruik van bind

Hoe kunnen we dit gebruiken?

~~~~ {.haskell}
canon1 referer path =
  parseURI referer                `bind`
   \r -> parseURIReference path   `bind`
    \p -> nonStrictRelativeTo p r `bind`
     \u -> Just (uriToString id u "")
~~~~

Als we een functienaam omsluiten in "`"-tekens, dan kunnen we de functie als infix operator gebruiken.


# Een andere layout van dezelfde code

~~~~ {.haskell}
canon referer path =
  parseURI referer         `bind` \r ->
  parseURIReference path   `bind` \p ->
  nonStrictRelativeTo p r  `bind` \u ->
  Just (uriToString id u "")
~~~~


# Een ingebouwde naam voor bind

De `>>=` operator is een generieke versie van onze `bind` functie.

~~~~ {.haskell}
canon referer path =
  parseURI referer >>= \r ->
  parseURIReference path >>= \p ->
  nonStrictRelativeTo p r >>= \u ->
  Just (uriToString id u "")
~~~~


# Gebruik maken van vereenvoudigde schrijfwijze

Hier is wat nettere syntax, die je vast bekend voorkomt.

~~~~ {.haskell}
canonicalize :: String -> String -> Maybe String

canonicalize referer path = do
  r <- parseURI referer
  p <- parseURIReference path
  u <- nonStrictRelativeTo p r
  return (uriToString id u "")
~~~~


# Bijna klaar

~~~~ {.haskell}
process url =
   map (canonicalize url) .
   filter (not . null) .
   map (fromAttrib "href") .
   filter (\t -> fromAttrib "rel" t /= "nofollow") .
   filter (isTagOpenName "a") .
   canonicalizeTags .
   parseTags
~~~~

Da's nou onhandig: wat is het type van deze functie?


# Van [Maybe a] naar [a]

Ga naar deze website:

* [haskell.org/hoogle](http://haskell.org/hoogle)

Typ dit in de search box:

~~~~ {.haskell}
[Maybe a] -> [a]
~~~~

Wat zien we als eerste resultaat?


# Klaar!

~~~~ {.haskell}
import Data.Maybe
import Network.URI

links url =
  catMaybes .
  map (canonicalize url) .
  filter (not . null) .
  map (fromAttrib "href") .
  filter (\t -> fromAttrib "rel" t /= "nofollow") .
  filter (isTagOpenName "a") .
  canonicalizeTags .
  parseTags
~~~~


# Van links naar spideren

Als we de links van een webpagina af kunnen halen, kunnen we eenvoudig een spider schrijven om die links te volgen.

Om het eenvoudig te houden, zetten we een limiet op het aantal pagina's dat we willen downloaden.

Welke informatie willen we genereren?

Wat willen we onderweg bijhouden?


# Wat we willen bijhouden

De status die we willen bijhouden:

* Het aantal pagina's dat we hebben gedownload

* Een verzameling pagina's waar we links naartoe hebben gezien, maar die we (nog) niet hebben gedownload

* Een verzameling pagina's met hun uitgaande links


# Bijhouden wat we gezien hebben

Voor een willekeurige pagina, willen we de pagina zelf bewaren en alle pagina's waar het naar verwijst.

Een mogelijkheid om deze twee te associeren is een *tuple*:

~~~~ {.haskell}
("http://x.org/", ["http://microsoft.com/"])
~~~~

Tuples zijn nuttig als we verschillende soorten data hebben zonder dat we
het gedoe willen van het definieren van een nieuw type.

Nu we het toch over een nieuw type hebben, zo definieer je er een:

~~~~ {.haskell}
data Link = Link String [String]

-- Let's define some accessors, too.
linkFrom (Link url _) = url
linkTo (Link _ links) = links
~~~~


# Dubbelingen voorkómen

We willen een URL niet twee keer bezoeken.

Hoe voorkomen we dat?

~~~~ {.haskell}
visited url = elem url . map linkTo
~~~~

Deze functie heeft een probleem - zie je welk probleem?


# Betere performance

We willen een structuur met een snelle opzoek operatie.

Wat zou je in jouw programmeertaal gebruiken?


# Maps and importing

In Haskell bestaan muteerbare hash tables, maar die gebruiken we niet.

in plaats hiervan gebruiken we liever *onveranderlijke* key-value maps.

We moeten de nodige trucks uithalen bij het importeren, want de `Data.Map`
module definieert veel namen die anders zouden overlappen met ingebouwde namen.

Dit betekent "importeer alleen de naam `Map` van de module `Data.Map`":

~~~~ {.haskell}
import Data.Map (Map)
~~~~

En dit betekent "importeer alles van `Data.Map`, maar elk van deze namen moet
voorafgegaan worden door `Map.`":

~~~~ {.haskell}
import qualified Data.Map as Map
~~~~


# Wat is het nut van een onveranderlijke data structuur?

Is iedereen bekend hoe je een key-value paar aan een hash tabel toevoegt?

En dat lijkt een fundamentele operatie.

Wat doen we met maps?

* Maak een *nieuwe* map die identiek is aan de map die we aanleveren, met het gevraagde element toegevoegd.

Hoe kan dat nou werken? Is het efficient?


# Een hand vol dollars

Hier is een verbazingwekkend handige ingebouwde operator:

~~~~ {.haskell}
f $ x = f x
~~~~

Waarom is dit nuttig? Omdat we hierdoor haakjes kunnen weglaten.

zonder $:

~~~~ {.haskell}
explode k = error ("failed on " ++ show k)
~~~~

met $:

~~~~ {.haskell}
explode k = error $ "failed on " ++ show k
~~~~


# Partiele toepassing

Dit is onhandig om op te schrijven:

~~~~ {.haskell}
increment k = 1 + k
~~~~

Bijna net zo erg:

~~~~ {.haskell}
\k -> 1 + k
~~~~

Veel handiger, en identiek:

~~~~ {.haskell}
(1+)
~~~~

Dit is valide:

~~~~ {.haskell}
increment = (1+)
~~~~

# Spideren, in al zijn glans

~~~~ {.haskell}
spider :: Int -> URL -> IO (Map URL [URL])
spider count url0 = go 0 Map.empty (Set.singleton url0)
  where
    go k seen queue0
        | k >= count = return seen
        | otherwise  =
      case Set.minView queue0 of
        Nothing -> return seen
        Just (url, queue) -> do
          page <- download url
          let ls       = links url page
              newSeen  = Map.insert url ls seen
              notSeen  = Set.fromList .
                         filter (`Map.notMember` newSeen) $ ls
              newQueue = queue `Set.union` notSeen
          go (k+1) newSeen newQueue
~~~~


# Stand van zaken?

We kunnen nu:

* Een webpagina downloaden

* De links ervan extraheren

* van daar uit verder `spideren`, zonder doublures

Wat blijft over?

* We zouden meerdere pagina's tegelijkertijd kunnen spideren

* Of we zouden kunnen berekenen welke pagina's "belangrijk" zijn


# Fin

In het hoogst onwaarschijnlijke geval dat we hier aankomen voordat we uit de tijd zijn gelopen,
gaam we verder in een we-zien-wel-wat-voor-avontuur sessie.

Bedankt dat je tot nu toe niet bent afgehaakt!

~~~~
git clone https://github.com/hanjoosten/HaskellWorkshop
~~~~


# Afronding
Allemaal hartelijk bedankt voor het lekker meedoen!




#### Hebben we de doelen gehaald?

> * Een idee over wat Functioneel Programmeren is
> * Enthousiasme gedeeld
> * Een leuke avond
> * Webcrawler

# Voor de liefhebber

Deze presentatie staat online:

* [http://hanjoosten.github.com/HaskellWorkshop](http://hanjoosten.github.com/HaskellWorkshop)

> Leuke Links:

> * [http://haskell.org]()  (alles over Haskell)
> * [http://www.haskell.org/haskellwiki/Haskell_in_industry]()(gebruik van Haskell in bedrijfsleven)
> * [http://corp.galois.com](http://corp.galois.com) is een innovatief bedrijf die heel veel met Haskell doet, en daarover veel vertelt.
> * [www.tryhaskell.org](http://www.tryhaskell.org) (online Haskell)
> * [www.projecteuler.net](http://www.projecteuler.net) (kijk hoever je zelf komt met Haskell)
