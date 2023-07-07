# Bachelorprojekt
Tak for din interesse i mit bachelorprojekt og dataene bag! Kort fortalt handler mit bachelorprojekt om hvilke karakteristika ved videregående uddannelser, der har den største betydning for valget af videregående uddannelse.<br><br>
Jeg har gjort al data bag opgaven tilgængeligt i dette repo. Du er velkommen til at dykke ned i dem og bruge dem til lige netop det, som du vurderer relevant. Da der har ligget et stort arbejde bag dette projekt, vil jeg dog bede om at du, hvis du bruger mine data, inkluderer en reference til denne side og til min bacheloropgave. Hertil skal lyde en stor tak til alle de gymnasielærere, der har hjulpet ifm. dataindsamlingen - uden jeres hjælp havde dette projekt ikke været muligt!

## Quicstart
Hvis du kun er interesseret i at læse min opgave, kan den downloades fra siden du kommer frem til ved at klikke på pdf-filen ovenfor.<br><br>
Hvis du er interesseret i at dykke ned i mine data eller analysen bag min opgave anbefaler jeg at downloade alle filerne. Disse kan nemt downloades som en zip ved at klikke på den grønne knap "Code" ovenfor. Herfra kan du vælge "Download ZIP"

## Filbeskrivelser
Her følger en kort beskrivelse af de enkelte filer i dette repository.

*Projektbeskrivelse.pdf*: En slags følgefil jeg anbefaler man starter med, hvis man er interesseret i selv at lege med dataene. Filen kan opdateres med øvrige oplysninger, hvis det skulle være relevant. I øjeblikket er der tale om en kodebog med variabelbeskrivelser for filen "Data.csv". Herudover indeholder filen en beskrivelse af datasættet, der bør give de relevante informationer for at arbejde med dataættet.<br><br>
*BA - Uddannelsesvalg med bankkontoen i baghovedet.pdf:* En pdf-fil med min opgave i sin helhed.<br><br>
*Data_Conjointly_RAW.csv*: En csv-fil med mine data som de så ud, da de blev downloadet fra conjointly (det anvendte survey-program). Bemærk at jeg ikke har lavet variabelbeskrivelser for denne fil.<br><br>
*Data.csv*: En csv-fil med mine data efter jeg har omkodet dataene fra conjointly. Generelt vil jeg også betegne dette som "rådata", da omkodningerne primært består i at omkode en række dummy-variable til kategoriske variable. Herunder er der også lavet lidt andre ændringer. Jeg vil generelt anbefale at man anvender dette datasæt, hvis man ønsker at dykke ned i dataene. Hvordan datasættet er konstrueret kan inspiceres i det tilhørende script.<br><br>
*Script til reproduktion.R*: Det komplette script jeg har brugt til både at lave omkodninger i conjointly-datasættet og gennemføre min analyse. Alle resultater der fremgår af opgaven kan reproduceres med dette script.<br>
