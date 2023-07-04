###############################################################################
#
# Script til BA-projekt: Uddannelsesvalg med bankkontoen i baghovedet?
#
# Forfatter: Christian Heide
#            Institut for Statskundskab
#            Aarhus Universitet
# Dato: 1. juni 2023
# Versioner: V2 - Opdateret til publikation på GitHub. Visse dele af koden er
#                 kommenteret ud for at kunne køre med datasættet der publiceres
#                 (4. juli 2023).
#
################################ SETUP ########################################

setwd("C:/Users/Christian Heide/OneDrive - Aarhus Universitet/Semestermapper/6. semester/Ulighed i uddannelse/BA-projekt/Analyse")

# Pakker jeg bruger
library('tidyverse')
library('labelled')
library('caret')
library('dplyr')
library('broom')
library('stargazer')
library('cregg')
library('cjpowR')
library('gridExtra')
library('cowplot')

# Får output med decimaler
options(scipen = 1000)


############################## DATA PREP ######################################

# Indlæser rå data fra conjointly-datasættet
raw_df <- read.csv("Datasæt/Conjointly_data_RAW.csv")

# Laver vektorer der klassificerer gymnasier pr. region !BEMÆRK!: Gymnasie-variabel
# er fjernet i publiceret datasæt, og derfor er kode hertil kommenteret ud

#gymnasier <- unique(raw_df$Q6_WHICH_HIGH_SCHOOL_DO_YOU_DID_YOU_GO_TO)
#gymnasier # her får jeg en liste af alle unikke inputs

#nordjylland <- c("HF&VUC nord", "Hhx hjørring", "Vesthimmerlands gymnasium",
                 #"Aalborg stx", "Aalborg katedralskole", "Aalborghus", "Støvring",
                 #"Aalborg Katedralskole", "aalborg katedraskole", "aalborg katedralskole")

#midtjylland <- c("Silkeborg gym", "HTX Århus", "Herningsholm", "HHX Herning",
                 #"AABC - Vejlby HHX", "Viby gym", "Det Kristne Gymnasium",
                 #"Silkeborg htx", "Herning Gymnasium", "horsens statsskole",
                 #"Teknisk Gymnasie Silkeborg", "Rødkilde gym.", "Rødkilde",
                 #"Risskov gymnasium", "Vejlefjordskolen", "Skanderborg",
                 #"Skanderborg Gymnasium", "Skanderborg Gymnasie", "skanderborg",
                 #"Skanderborg Gymnasium six", "skanderborg gymnasium",
                 #"Skanderborg gymnasie", "Skanderborg Gymnasiulm", "Skanderborg gymnasium",
                 #"Holstebro Gymnasium", "Viborg Gymnasium", "Viborg katedralskole",
                 #"Skanderborg stx", "Aarhus gymnasium", "Marselisborg", "Aarhus katedralskole",
                 #"Århus Statsgymnasium", "Marselisborg gymnasium", "HTX i Århus",
                 #"HTX Aarhus Gymnasium", "Herningsholm Erhvervsgymnasium HTX",
                 #"Viborg Katedralskole", "Skanderborg gymnasium STX", "Paderup gymnasium",
                 #"Risskov Gymnasium", "Paderup Gymnasium", "Egå Gymnasium",
                 #"Aarhus Gymnasium C (HTX)", "Det Kristne Gymnasium (Ringkøbing)",
                 #"Aarhus Gymnasium", "STX i Silkeborg", "Syddjurs Gymnasium / stx",
                 #"Skive", "Learnmark HTX Horsens", "Ringkjøbing Gymnasium",
                 #"Rosborg Gymnasie og HF", "Aarhus HF & VUC", "Aarhus", "Viby handelsgymnasium",
                 #"Holstebro gymnasium", "aarhus katedralskole", "silkeborg gymnasium",
                 #"risskov gymnasium", "Det kristnegymnasium", "Aarhus Katedralskole")

#syddanmark <- c("Nyborg", "Ribe katedralskole", "Odenste Tekniske Gymnasium (HTX)",
                #"Munkensdam Kolding", "Nyborg Gymnasium", "Hf FLOW",
                #"Esbjerg gymnasium", "Nyborg gymnasium", "Odense Katedralskole",
                #"Vuc syd", "Svendborg", "Svendborg Gymnasium", "Svendborg gymnasium STX",
                #"svendborg gymnasium", "Svendborg gym", "Svendborg gymnaisum",
                #"Svendborg gymnasium", "svendborg Gymnasium", "Svendborg gymnasie",
                #"Odense Tekniske Gymnasium", "Alssundgymnasiet Sønderborg",
                #"Midtfyns Gymnasium", "Midtfyns Gymnasiu", "Odense Tekniske Gymnasium - HTX",
                #"HF FLOW, Odense", "Svendborg Gymnasie", "Svendborg almen gymnasium",
                #"Oure Kostgymnasium", "Svendborg handelsgymnasium (hhx)", "svendborg gym",
                #"Svendborg STX", "Nyborg gym", "Varde handelsskole", "Tornbjerg Gymnasium",
                #"Tornbjerg Gymnasie", "Tornbjerg gymnasium", "tornbjerg gymnasium",
                #"tornbjerg", "Svendborg gymnasium.", "OTG", "Hansenberg Tekniske Gymnasium",
                #"Hf & vuc fyn", "Rybners HTX", "Grindsted gymnasium og HF",
                #"Aabenraa Statsskole", "faaborg gymnasium", "Tietgenskolen",
                #"Hansenberg Teknisk Gymnasium", "EUC SYD", "Vestfyns gymnasium",
                #"Rybners, Esbjerg", "Hansenberg", "Grindsted Gymnasium og Erhversskole",
                #"HTX Lillebælt", "Rybners HTX Esbjerg", "Mulernes legatskole",
                #"svendborg gymnasie", "tietgenskolen", "flow hf", "alssund gymnasie",
                #"midtfyens", "nyborg gymnasium", "Sønderborg Statsskole")

#sjælland <- c("Vordingborg Gymnasium og HF", "Vordingborg Gymnasie", "Slagelse gymnasium",
              #"Stenhus Gymnasium Holbæk", "Roskilde Handelsgymnasium", "Roskilde Katedralskole",
              #"Køge gymnasium", "Køge Gymnasium", "Gik på Køge gymnasium", "Køge gym",
              #"Køge gymnasie", "Kalundborg Gymnasium og HF", "næstved gymnasium",
              #"næstved gym", "Roskilde katedralskole", "Næstved Gymnasium")

#hovedstaden <- c("H.C. Ørsted Gymnasiet i Lyngby", "Tårnby gym", "Auregård",
                 #"KVUc", "HTX Vibenhus", "det frie gymnasium", "Det Frie",
                 #"Kbh syd hf og vuc", "Borupgaard gymnasium", "Virum Gymnasium",
                 #"NEXT Sukkertoppen", "KVUC", "Nørre Gymnasium", "Falko",
                 #"Sankt Annæ Gymnasium", "Frederiksborg Gymnasium og HF",
                 #"Sukkertoppen", "Nørre g", "Frederiksborg Gymnasie og Hf",
                 #"Falkonergården", "Frederiksborg gymnasium & HF", "Frederiksborg gymnasie",
                 #"Sankt annæ gymnasium", "Frederiksborg gymnsium og hf", "Rysensteen gymnasium",
                 #"Frederiksborg gymnasium", "Niels Brock, Det Internationale Gymnasium",
                 #"Sankt Annæ", "Rysensteen", "Christianshavn Gymnasium", "Frederiksborg gymnasium og HF",
                 #"Frederiksborg gymnasium og hf", "Falkonergårdens Gymnasium og HF",
                 #"Falkonergårdens Gymnasium", "Christianhavns gymnasie",
                 #"Christianshavns gymnasium", "Frederiksborg Gymnasium", "Rysensteen Gymnasium",
                 #"frederiksborg gymnasium og hf", "sukkertoppen", "Sukkertoppen Gymnasium",
                 #"Christianshavns Gymnasium", "Christianshavns gymnasie",
                 #"Niels Brock - Det internationale gymnasium", "rysensteen gymnasium",
                 #"Frederiksborg Gumnasium", "Sukkertoppen HTX NEXT", "HC ørsted Lyngby",
                 #"NEXT Vibenshus Gymnasium", "hcø Lyngby (htx)", "Et Nordsjællandsk",
                 #"HC Ørsted Gymnasium Lyngby", "Cph West ishøj", "Gladsaxe Gymnasium",
                 #"Gefion gymnasie", "Rødovre gymnasium", "nørre g", "hcø",
                 #"falkonergården", "virum gymmnasium")

# Tjekker om jeg har misset nogen gymnasier
#tjek <- setdiff(unique(raw_df$Q6_WHICH_HIGH_SCHOOL_DO_YOU_DID_YOU_GO_TO), c(nordjylland, midtjylland, syddanmark, sjælland, hovedstaden))
#print(tjek)
#rm(tjek)

# Opretter nyt midlertidigt dataframe til eksperiment-variable
eksperiment_df <- raw_df |>
  
  # Tilføjer eksperiment-variable til nyt dataframe - først dem der er "klar"
  # Livstidsindkomst, arberjdsløshed og længde laves til faktorvariable
  transmute(id = RESPONDENT_ID,
            choice_set = CHOICE_SET,
            profil = LABEL,
            valg = CHOICE_INDICATOR,
            
            livstidsindkomst = case_when(
              LIVSTIDSINDKOMST == 13 ~ "13 mio",
              LIVSTIDSINDKOMST == 17 ~ "17 mio",
              LIVSTIDSINDKOMST == 21 ~ "21 mio",
              LIVSTIDSINDKOMST == 25 ~ "25 mio"),
            
            livstidsindkomst = factor(livstidsindkomst,
                                      levels = c("13 mio", "17 mio", "21 mio",
                                                 "25 mio")),
            
            arbejdsløshed = case_when(
              ARBEJDSLOSHED == 5 ~ "5%",
              ARBEJDSLOSHED == 10 ~ "10%",
              ARBEJDSLOSHED == 15 ~ "15%",
              ARBEJDSLOSHED == 20 ~ "20%"),
            
            arbejdsløshed = factor(arbejdsløshed,
                                   levels = c("5%", "10%", "15%", "20%")),
            
            længde = case_when(
              LAENGDE == 2 ~ "2 år",
              LAENGDE == 3.5 ~ "3,5 år",
              LAENGDE == 4 ~ "4 år",
              LAENGDE == 5 ~ "5 år"),
            
            længde = factor(længde,
                            levels = c("2 år", "3,5 år", "4 år", "5 år")),
            
            # Tilføjer adgangskvotient som faktorvariabel
            adgangskvotient = factor(ADGANGSKVOTIENT,
                                    levels = c("Alle optaget", "Kun kvote 2",
                                               "6", "10")),
            
            # Omkoder åbenhed til binær, og tilføjer den som faktorvariabel
            åbenhed = ifelse(ABENHED != c("Mange jobs", "Mange jobs2"),
                           "Specifikke jobs", "Mange jobs"),
            åbenhed = factor(åbenhed,
                          levels = c("Specifikke jobs", "Mange jobs")),
            
            # Samme som ovenfor men med velfærdsjobs
            velfærdsarbejde = ifelse(JOBTYPE != c("Velfærdsjobs",
                                                  "Velfærdsjobs 2"),
                                     "Ikke mulighed for velfærdsjobs",
                                     "Mulighed for velfærdsjobs"),
            velfærdsarbejde = factor(velfærdsarbejde,
                                     levels = c("Ikke mulighed for velfærdsjobs",
                                                "Mulighed for velfærdsjobs")),
            
            # Samme som ovenfor men med placering
            placering = ifelse(PLACERING == c("store byer1", "store byer2"),
                               "I de fire store byer",
                               "Uden for de fire store byer"),
            placering = factor(placering,
                               levels = c("I de fire store byer",
                                          "Uden for de fire store byer")),
            
            # Laver en variabel, der indikerer om tærsklen er opfyldt i begge
            # opgaver
            target = case_when(
              
              # Der er lighed på indkomst
              profil == 1 & LIVSTIDSINDKOMST == lead(LIVSTIDSINDKOMST) ~ "Opfyldt/lighed",
              profil == 2 &  LIVSTIDSINDKOMST == lag(LIVSTIDSINDKOMST) ~ "Opfyldt/lighed",
              
              # Begge alternativer opfylder tærsklen
              profil == 1 & LIVSTIDSINDKOMST >= 21 & lead(LIVSTIDSINDKOMST) >= 21 ~ "Opfyldt/lighed",
              profil == 2 & LIVSTIDSINDKOMST >= 21 & lag(LIVSTIDSINDKOMST) >= 21 ~ "Opfyldt/lighed",
              
              # Alle andre alternativer er der forskel på tærskelopfyldelsen
              TRUE ~ "Ikke opfyldt"
            ),
            
            # Og laver også den til en faktor-variabel
            target = factor(target, levels = c("Ikke opfyldt", "Opfyldt/lighed")),
            
            # Laver target uden lighed til robusthed
            target_variation = case_when(
              
              # Lighed skal ekskluderes
              profil == 1 & LIVSTIDSINDKOMST == lead(LIVSTIDSINDKOMST) ~ NA,
              profil == 2 & LIVSTIDSINDKOMST == lag(LIVSTIDSINDKOMST) ~ NA,
              
              # Begge alternativer opfylder tærsklen
              profil == 1 & LIVSTIDSINDKOMST >= 21 & lead(LIVSTIDSINDKOMST) >= 21 ~ "Opfyldt",
              profil == 2 & LIVSTIDSINDKOMST >= 21 & lag(LIVSTIDSINDKOMST) >= 21 ~ "Opfyldt",
              
              # Alle andre alternativer er der forskel på tærskelopfyldelsen
              TRUE ~ "Ikke opfyldt"
            ),
            
            # Og laver til faktor-variabel
            target_variation = factor(target_variation,
                                      levels = c("Ikke opfyldt", "Opfyldt")),
            
            # Laver target variabel for arbejdsløshed til robusthedstests
            arbejdsløshed_target = case_when(
              
              # Der er lighed på arbejdsløshed
              profil == 1 & arbejdsløshed == lead(arbejdsløshed) ~ "Opfyldt/lighed",
              profil == 2 & arbejdsløshed == lag(arbejdsløshed) ~ "Opfyldt/lighed",
              
              # Tærskel er opfyldt i begge alternativer
              profil == 1 & arbejdsløshed %in% c("5%", "10%") & lead(arbejdsløshed) %in% c("5%", "10%") ~ "Opfyldt/lighed",
              profil == 2 & arbejdsløshed %in% c("5%", "10%") & lag(arbejdsløshed) %in% c("5%", "10%") ~ "Opfyldt/lighed",
              
              # Alle andre alternativer ikke opfyldt
              TRUE ~ "Ikke opfyldt"
            ),
            
            # Og laver til faktor
            arbejdsløshed_target = factor(arbejdsløshed_target,
                                      levels = c("Ikke opfyldt", "Opfyldt/lighed")),
            
            # Laver target variabel for åbenhed
            åbenhed_target = case_when(
              profil == 1 & åbenhed == lead(åbenhed) ~ "Lighed",
              profil == 2 & åbenhed == lag(åbenhed) ~ "Lighed",
              TRUE ~ "Variation"
            ),
            
            # Og laver til faktor
            åbenhed_target = factor(åbenhed_target,
                                    levels = c("Variation", "Lighed")))

# Opretter nyt midlertidigt data frame til kontrolvariable
kontrol_df <- raw_df |>
  
  # Tilføjer omkodningerne til de midlertidige df
  transmute(
    
    # Først tilføjer jeg identifikationsvariable
    id = RESPONDENT_ID,
    choice_set = CHOICE_SET,
    profil = LABEL,
    
    # Laver string variabel for klassetrin/sabbatår
    beskæftigelse = case_when(
      Q3_EDUCATION_O3_JEG_GAR_I_1_G == 1 ~ "1.g",
      Q3_EDUCATION_O2_JEG_GAR_I_2_G == 1 ~ "2.g",
      Q3_EDUCATION_O1_JEG_GAR_I_3_G == 1 ~ "3.g",
      Q3_EDUCATION_O5_JEG_GAR_I_1_HF == 1 ~ "1.hf",
      Q3_EDUCATION_O4_JEG_GAR_I_2_HF == 1 ~ "2.hf",
      Q3_EDUCATION_O2_JEG_GAR_I_2_HF == 1 ~ "2.hf",
      Q3_EDUCATION_O3_JEG_HAR_SABBATAR == 1 ~ "Sabbatår",
      Q3_EDUCATION_O6_JEG_HAR_SABBATAR == 1 ~ "Sabbatår",
      Q3_EDUCATION_O4_INGEN_AF_OVENSTAENDE == 1 ~ "Andet/uden for målgruppe",
      Q3_EDUCATION_O7_INGEN_AF_OVENSTAENDE == 1 ~ "Andet/uden for målgruppe"),
    
    # Omkoder beskæftigelses-variablen til at være faktor
    beskæftigelse = factor(beskæftigelse,
                           levels = c("1.g", "1.hf", "2.g", "2.hf", "3.g",
                                      "Sabbatår", 
                                      "Andet/uden for målgruppe")),
    
    # Omkoder og omdøber variablen for antal sabbatår så "NULL" bliver til 0
    # og konverterer tallene fra string til num
    sabbatår = ifelse(Q4_SABBATAR == "NULL", 0, as.numeric(Q4_SABBATAR)),
    
    # Laver en string variabel til at indikere køn
    køn = case_when(Q5_GENDER_O1_KVINDE == 1 ~ "Kvinde",
                    Q5_GENDER_O2_MAND == 1 ~ "Mand",
                    Q5_GENDER_O3_ANDET == 1 ~ NA),
    
    # Omkoder køn-variabel til at være faktorvariabel
    køn = factor(køn, levels = c("Kvinde", "Mand")),
    
    # Laver dummy, der indikerer om man er fra en af de fire store byer
    storby = case_when(
      Q7_SIZE_OF_CITY_O1_OVER_1_MIO_INDBYGGER_KOBENHAVN == 1 ~ "De fire store byer",
      Q7_SIZE_OF_CITY_O2_MELLEM_100_000_OG_1_MIO_INDBYGGERE_AARHUS_ODENSE_OG_AALBORG == 1 ~ "De fire store byer",
      Q7_SIZE_OF_CITY_O3_MELLEM_50_000_OG_100_000_INDBYGGERE == 1 ~ "Uden for de fire store byer",
      Q7_SIZE_OF_CITY_O4_MELLEM_20_000_OG_50_000_INDBYGGERE == 1 ~ "Uden for de fire store byer",
      Q7_SIZE_OF_CITY_O5_UNDER_20_000_INDBYGGERE == 1 ~ "Uden for de fire store byer"),
    
    # Og laver den om til en faktor-variabel
    storby = factor(storby,
                    levels = c("Uden for de fire store byer", "De fire store byer")),
    
    # Laver string-variabel for forældres uddannelse
    forældre_udd = case_when(
      Q8_PARENT_EDUCATION_O1_FOLKESKOLE == 1 ~ "Folkeskole",
      Q8_PARENT_EDUCATION_O2_GYMNASIE_STX_HHX_HTX_HF == 1 ~ "Gymnasial",
      Q8_PARENT_EDUCATION_O3_ERHVERVSUDDANNELSE == 1 ~ "Erhvervsuddannelse",
      Q8_PARENT_EDUCATION_O4_KORT_VIDEREGAENDE_UDDANNELSE_TYPISK_ERHVERVSAKADEMIUDDANNELSER == 1 ~ "KVU",
      Q8_PARENT_EDUCATION_O5_MELLEMLANG_VIDEREGAENDE_UDDANNELSE_TYPISK_PROFESSIONSBACHELORER == 1 ~ "MVU",
      Q8_PARENT_EDUCATION_O6_LANG_VIDEREGAENDE_UDDANNELSE_TYPISK_UNIVERSITETSUDDANNELSER == 1 ~ "LVU"),
    
    # Laver forældre_udd om til faktorvariabel
    forældre_udd = factor(forældre_udd,
                          levels = c("Folkeskole", "Erhvervsuddannelse",
                                     "Gymnasial", "KVU", "MVU", "LVU")),
    
    # Tilføjer karaktergennemsnit-variablen
    karaktergennemsnit = Q12_EDUCATION,
    
    # Laver variabel der indikerer om respondenten har/vil søge ind på uddannelse
    # i år
    studie_start = ifelse(Q13_HAVE_YOU_APPLIED_FOR_A_HIGHER_EDUCATION_YET_O1_JA == 1 | Q13_HAVE_YOU_APPLIED_FOR_A_HIGHER_EDUCATION_YET_O2_NEJ_MEN_JEG_PLANLAEGGER_AT_GORE_DET_I_AR ==1,
                          1, 0),
    
    # Tilføjer variabel der indikerer hvad respondenten vil starte på
    studie_valg = Q14_EDUCATION,
    # Og gør manglende værdier missing
    studie_valg = na_if(studie_valg, "NULL"),
    
    # Laver en string-variabel, der indikerer respondentens region vha. regions-vektorerne
    #region = ifelse(Q6_WHICH_HIGH_SCHOOL_DO_YOU_DID_YOU_GO_TO %in% hovedstaden, "Hovedstaden",
                    #ifelse(Q6_WHICH_HIGH_SCHOOL_DO_YOU_DID_YOU_GO_TO %in% midtjylland, "Midtjylland",
                           #ifelse(Q6_WHICH_HIGH_SCHOOL_DO_YOU_DID_YOU_GO_TO %in% nordjylland, "Nordjylland",
                                  #ifelse(Q6_WHICH_HIGH_SCHOOL_DO_YOU_DID_YOU_GO_TO %in% sjælland, "Sjælland",
                                         #ifelse(Q6_WHICH_HIGH_SCHOOL_DO_YOU_DID_YOU_GO_TO %in% syddanmark, "Syddanmark", NA))))),
    
    # Laver en dummy, der indikerer om et gennemsnit er over 9
    højt_snit = ifelse(karaktergennemsnit > 9, "Over 9",
                       "9 eller under"),
    
    # Og laver den om til en faktor-variabel
    højt_snit = factor(højt_snit,
                       levels = c("Over 9",
                                  "9 eller under")))

# Laver den endelige dataframe ved at slå de to midlertidige df's sammen
df <- merge(eksperiment_df, kontrol_df, by = c("id", "choice_set", "profil"))

# Fjerner de objekter jeg ikke skal bruge længere
rm(eksperiment_df, kontrol_df, raw_df, gymnasier, hovedstaden, midtjylland,
     nordjylland, sjælland, syddanmark)



######################### DESKRIPTIV STATISTIK #################################

# Laver først en række dummy-variable, der skal bruges til t-test
variabler <- c("forældre_udd", "køn")
dummy_vars <- dummyVars(as.formula(paste("~", paste(variabler, collapse = "+"))), data = df)

# Tilføjer dummies til data frame sammen med de to kontinuerte variable
deskriptiv_df <- data.frame(predict(dummy_vars, newdata = df)) |>
  mutate(storby = ifelse(df$storby == "De fire store byer", 1, 0),
         karaktergennemsnit = df$karaktergennemsnit)

# Laver vektor med alle de variable, der skal testes
variable <- names(deskriptiv_df)
# Laver vektor med variablenes populationsværdier
mus <- c(0.187, 0.376, 0.053, 0.064, 0.194, 0.127, 0.491, 0.509, 0.473, 7.7)

# Udfører selve t-testen
ttests <- map2(variable, mus, ~ {
  t_test <- t.test(deskriptiv_df[[.x]], mu = .y)
  # Angiver hvilke paramentre jeg gerne vil have med i ttests-data framet
  data.frame(
    variabel = .x,
    populationsværdi = .y,
    estimate = t_test$estimate,
    t.value = t_test$statistic,
    p.value = t_test$p.value,
    conf.low = t_test$conf.int[1],
    conf.high = t_test$conf.int[2])})
# Laver det om til et data frame jeg kan bruge til noget
ttests <- bind_rows(ttests)

# Fjerner midlertidige objekter
rm(deskriptiv_df, dummy_vars, mus, variabler, variable)

# Tilføjer en variabel, der viser forskellen mellem stikprøve og population
# og laver lidt andre ændringer der skal bruges i tabellen
ttests <- ttests |>
  mutate(
    forskel = round(estimate-populationsværdi, digits = 2),
    Variabel = variabel,
    Stikprøveandel = round(estimate, digits = 2),
    Populationsandel = round(populationsværdi, digits = 2),
    Forskel = ifelse(p.value < 0.001, paste0(forskel, "***"),
                     ifelse(p.value < 0.01, paste0(forskel, "**"),
                            ifelse(p.value < 0.05, paste0(forskel, "*"),
                                   forskel)))) |>
  select(Variabel, Stikprøveandel, Populationsandel, Forskel)

# Laver tabellen - mangler stadig lidt
stargazer(ttests, summary = FALSE, digits = 2, decimal.mark = ",",
          digits.extra = 2, rownames = FALSE,
          out = "Output/tab_deskriptiv.tex",
          notes = "*: p < 0,05; **: p < 0,01; **: p < 0,001")

############################ FORUDSÆTNINGER ####################################

# Laver først en liste over attributter, der skal bruges i det følgende
attributter <- c("livstidsindkomst", "arbejdsløshed", "længde", "adgangskvotient",
                 "placering", "åbenhed", "velfærdsarbejde")

# 1) FRAVÆR AF CRRYOVER-EFFECTS

# Liste til at gemme resultater
carryover <- list()

# Udfører F-test for hver attribut, og gemmer resultater
for (attribut in attributter) {
  carryover[[attribut]] <- cj_anova(df, formula = as.formula(paste("valg ~", attribut)),
                                    id = ~ id, by = ~ choice_set)
}

# Tester til sidst den samlede model, og gemmer også resultatet
carryover[["fuld_model"]] <- cj_anova(df, formula = as.formula(
  paste("valg ~", paste(attributter, collapse = " + "))), id = ~ id,
  by = ~ choice_set)


# 2) FRAVÆR AF PROFILE-ORDER EFFECTS
# Følger samme fremgangsmåde som ovenfor, men betinger i stedet på profil

# Liste til at gemme resultater
profile_order <- list()

# Udfører F-test for hver attribut, og gemmer resultater
for (attribut in attributter) {
  profile_order[[attribut]] <- cj_anova(df, formula = as.formula(
    paste("valg ~", attribut)),id = ~ id, by = ~ profil)
}

# Tester til sidst den samlede model, og gemmer også resultatet
profile_order[["fuld_model"]] <- cj_anova(df, formula = as.formula(
  paste("valg ~", paste(attributter, collapse = " + "))), id = ~ id,
  by = ~ profil)

# SAMLER FORUDSÆTNINGSTESTS

attributter <- c("livstidsindkomst", "arbejdsløshed", "længde", "adgangskvotient",
                 "placering", "åbenhed", "velfærdsarbejde", "fuld_model")

# Laver data frame til at samle resultaterne i
forudsætninger <- data.frame(attribut = character(),
                             carryover = numeric(),
                             profile_order = numeric(),
                             stringsAsFactors = FALSE)

# Tilføjer resultaterne til data frame
for (attribut in attributter) {
  f_carryover <- carryover[[attribut]][["Pr(>F)"]][[2]]
  f_profile_order <- profile_order[[attribut]][["Pr(>F)"]][[2]]
  forudsætninger <- rbind(forudsætninger, data.frame(
    attribut = attribut,
    carryover = f_carryover,
    profile_order = f_profile_order))
}

# Fjerner objekter der var midlertidige
rm(carryover, profile_order, attribut, attributter, f_carryover, f_profile_order)

# Laver tabel der eksporteres til LaTeX - kan laves bedre, hvis tid
stargazer(forudsætninger, summary = FALSE, dep.var.caption = c("Pr(>F)"),
          rownames = FALSE, digits = 2, title = "Forudsætninger",
          out = "Output/tab_forudsætninger.tex")


################################ POWER #########################################

# Vektor med mine to typer modeller
modeller <- c("Fuld", "Subgruppe")

# Laver data-frame til at samle resultater i
power <- data.frame(model = character(),
                    binær_power = numeric(),
                    binær_amce = numeric(),
                    multi_power = numeric(),
                    multi_amce = numeric(),
                    stringsAsFactors = FALSE)

# Kører et loop hvor der beregnes power og MDE's i begge typer modeller for både
# binære variable og variable med flere niveauer.
# Dette tilføjes herefter til min data frame
for (model in modeller) {
  if (model == "Fuld") {
    binær_power <- cjpowr_amce(amce = 0.046, n = nrow(df), levels = 2)$power
    binær_amce <- cjpowr_amce(amce = 0.046, n = nrow(df), levels = 2)$amce
    multi_power <- cjpowr_amce(amce = 0.065, n = nrow(df), levels = 4)$power
    multi_amce <- cjpowr_amce(amce = 0.065, n = nrow(df), levels = 4)$amce
    power <- rbind(power, data.frame(
      model = model,
      binær_power = binær_power,
      binær_amce = binær_amce,
      multi_power = multi_power,
      multi_amce = multi_amce))
  } else {
    binær_power <- cjpowr_amcie(delta3 = 0.092, n = nrow(df), levels1 = 2,
                                levels2 = 2)$power
    binær_amce <- cjpowr_amcie(delta3 = 0.092, n = nrow(df), levels1 = 2,
                               levels2 = 2)$delta3
    multi_power <- cjpowr_amcie(delta3 = 0.13, n = nrow(df), levels1 = 4,
                                levels2 = 2)$power
    multi_amce <- cjpowr_amcie(delta3 = 0.13, n = nrow(df), levels1 = 4,
                               levels2 = 2)$delta3
    power <- rbind(power, data.frame(
      model = model,
      binær_power = binær_power,
      binær_amce = binær_amce,
      multi_power = multi_power,
      multi_amce = multi_amce))
  }
}

# Fjerner midlertidige objekter
rm(binær_amce, binær_power, model, modeller, multi_amce, multi_power)

######################### ANALYSE I: HOVEDMODEL ################################

# Estimerer den fulde model og gemmer den i et objekt
fuld_model <- cj(df, valg ~ livstidsindkomst * arbejdsløshed + længde
                 + adgangskvotient + placering + åbenhed + velfærdsarbejde,
                 id = ~ id,
                 # Ændrer rækkefølgen for niveauer
                 level_order = "descending",
                 # Tilføjer andre labels til attributter
                 feature_labels = list(livstidsindkomst = "LIVSTIDSINDKOMST",
                                       arbejdsløshed = "ARBEJDSLØSHED",
                                       længde = "LÆNGDE",
                                       adgangskvotient = "ADGANGSKVOTIENT",
                                       placering = "PLACERING",
                                       åbenhed = "ÅBENHED",
                                       velfærdsarbejde = "VELFÆRDSARBEJDE"))

# Laver et plot med den fulde model og gemmer i objekt
plot(fuld_model, feature_headers = TRUE, header_fmt = "%s",
     xlim = c(-0.25, 0.42), xlab = "AMCE") +
  theme_bw() +
  # Ændrer farven for alle attributter til sort
  scale_color_manual(values = c("black", "black", "black", "black", "black",
                                "black", "black")) +
  # Fjerner forklarende box
  theme(legend.position = "none")

# Gemmer figuren - kan godt være der skal ændres lidt i størrelsen, når den skal
# ind i LaTeX
ggsave("Output/fuld_model.pdf", width = 6, height = 5)

###################### ANALYSE II: SUBGRUPPEANALYSE ############################

# 1) BOPÆL

# Estimerer først en model, der viser AMCE for de to grupper
sub_bopæl <- cj(df, valg ~ livstidsindkomst + arbejdsløshed + længde
                + adgangskvotient + placering + åbenhed + velfærdsarbejde,
                id = ~ id, by = ~ storby,
                # Ændrer rækkefølgen for niveauer
                level_order = "descending",
                # Tilføjer andre labels til attributter
                feature_labels = list(livstidsindkomst = "LIVSTIDSINDKOMST",
                                      arbejdsløshed = "ARBEJDSLØSHED",
                                      længde = "LÆNGDE",
                                      adgangskvotient = "ADGANGSKVOTIENT",
                                      placering = "PLACERING",
                                      åbenhed = "ÅBENHED",
                                      velfærdsarbejde = "VELFÆRDSARBEJDE"))

# Estimerer herefter model, der estimerer forskellene mellem de to grupper
# Det er forskel = storby - ikke_storby
diff_bopæl <- amce_diffs(df, valg ~ livstidsindkomst + arbejdsløshed + længde
                         + adgangskvotient + placering + åbenhed + velfærdsarbejde,
                         id = ~ id, by = ~ storby,
                         # Ændrer rækkefølgen for niveauer
                         level_order = "ascending",
                         # Tilføjer andre labels til attributter
                         feature_labels = list(livstidsindkomst = "LIVSTIDSINDKOMST",
                                               arbejdsløshed = "ARBEJDSLØSHED",
                                               længde = "LÆNGDE",
                                               adgangskvotient = "ADGANGSKVOTIENT",
                                               placering = "PLACERING",
                                               åbenhed = "ÅBENHED",
                                               velfærdsarbejde = "VELFÆRDSARBEJDE"))

# Laver et plot opdelt efter bopæl
p1 <- plot(sub_bopæl, "storby", feature_headers = TRUE, header_fmt = "%s",
     xlim = c(-0.30, 0.45), xlab = "AMCE") +
  theme_bw() +
  scale_color_manual(values = c("De fire store byer" = "black",
                                "Uden for de fire store byer" = "#999999")) +
  theme(legend.position = "bottom") +
  ggtitle("Bopæl") +
  theme(plot.title = element_text(hjust = 0.5, size = 10),
        legend.title = element_blank(),
        legend.direction = "vertical")
  
# Laver et plot med forskel imellem bopæl
p2 <- plot(diff_bopæl, feature_headers = TRUE, header_fmt = "%s",
     xlab = "", xlim = c(-0.23, 0.25)) +
  theme_bw() +
  scale_color_manual(values = c("black", "black", "black", "black", "black",
                                "black", "black")) +
  ggtitle("Forskel = 4 største byer - Uden for") +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5, size = 10))


# 2) KARAKTERGENNEMSNIT

# Estimerer først en model, der viser AMCE for de to grupper
sub_karakterer <- cj(df, valg ~ livstidsindkomst + arbejdsløshed + længde
                     + adgangskvotient + placering + åbenhed + velfærdsarbejde,
                     id = ~ id, by = ~ højt_snit,
                     # Ændrer rækkefølgen for niveauer
                     level_order = "descending",
                     # Tilføjer andre labels til attributter
                     feature_labels = list(livstidsindkomst = "LIVSTIDSINDKOMST",
                                           arbejdsløshed = "ARBEJDSLØSHED",
                                           længde = "LÆNGDE",
                                           adgangskvotient = "ADGANGSKVOTIENT",
                                           placering = "PLACERING",
                                           åbenhed = "ÅBENHED",
                                           velfærdsarbejde = "VELFÆRDSARBEJDE"))

# Estimerer herefter model, der estimerer forskellene mellem de to grupper
# Det er forskel = 9 eller under - over 9
diff_karakterer <- amce_diffs(df, valg ~ livstidsindkomst + arbejdsløshed + længde
                         + adgangskvotient + placering + åbenhed + velfærdsarbejde,
                         id = ~ id, by = ~ højt_snit,
                         # Ændrer rækkefølgen for niveauer
                         level_order = "ascending",
                         # Tilføjer andre labels til attributter
                         feature_labels = list(livstidsindkomst = "LIVSTIDSINDKOMST",
                                               arbejdsløshed = "ARBEJDSLØSHED",
                                               længde = "LÆNGDE",
                                               adgangskvotient = "ADGANGSKVOTIENT",
                                               placering = "PLACERING",
                                               åbenhed = "ÅBENHED",
                                               velfærdsarbejde = "VELFÆRDSARBEJDE"))

# Laver et plot opdelt efter karaktergennemsnit
p3 <- plot(sub_karakterer, "højt_snit", feature_headers = TRUE, header_fmt = "%s",
     xlim = c(-0.30, 0.5), xlab = "AMCE") +
  theme_bw() +
  scale_color_manual(values = c("Over 9" = "black",
                                "9 eller under" = "#999999")) +
  theme(legend.position = "bottom", axis.text.y = element_blank()) +
  ggtitle("Karaktergennemsnit") +
  theme(plot.title = element_text(hjust = 0.5, size = 10),
        legend.title = element_blank(),
        legend.direction = "vertical")

# Laver et plot med forskel imellem karaktergennemsnit
p4 <- plot(diff_karakterer, feature_headers = TRUE, header_fmt = "%s",
     xlab = "", xlim = c(-0.23, 0.25)) +
  theme_bw() +
  scale_color_manual(values = c("black", "black", "black", "black", "black",
                                "black", "black")) +
  ggtitle("Forskel = 9 eller under - Over 9") +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5, size = 10),
        axis.text.y = element_blank())


# LAVE GRAFIK

# Samler p1 og p3
grid.arrange(p1, p3, ncol = 7,
              layout_matrix = cbind(1, 1, 1, 1, 2, 2, 2))

# Eksporterer p1 og p3 som én figur
g <- arrangeGrob(p1, p3, ncol = 5,
                 layout_matrix = cbind(1, 1, 1, 2, 2))
ggsave(filename = "Output/subgrupper.pdf", g, width = 7, height = 6)

# Samler p2 og p4
grid.arrange(p2, p4, ncol = 5,
             layout_matrix = cbind(1, 1, 1, 1, 1, 2, 2, 2))

# Eksporterer p2 og p4 som én figur
g <- arrangeGrob(p2, p4, ncol = 5,
                 layout_matrix = cbind(1, 1, 1, 2, 2))
ggsave(filename = "Output/subgrupper_forskelle.pdf", g, width = 7, height = 6)

# Fjerner objekter der ikke skal bruges længere
rm(p1, p2, p3, p4, g)

###################### ANALYSE III: TARGET SETTING #############################

# Estimerer en model efter om tærsklen er opfyldt
sub_target <- cj(df, valg ~ livstidsindkomst + arbejdsløshed + længde
                     + adgangskvotient + placering + åbenhed + velfærdsarbejde,
                     id = ~ id, by = ~ target,
                     # Ændrer rækkefølgen for niveauer
                     level_order = "descending",
                     # Tilføjer andre labels til attributter
                     feature_labels = list(livstidsindkomst = "LIVSTIDSINDKOMST",
                                           arbejdsløshed = "ARBEJDSLØSHED",
                                           længde = "LÆNGDE",
                                           adgangskvotient = "ADGANGSKVOTIENT",
                                           placering = "PLACERING",
                                           åbenhed = "ÅBENHED",
                                           velfærdsarbejde = "VELFÆRDSARBEJDE"))

# Estimerer forskellene efter om target er opfyldt
diff_target <- amce_diffs(df, valg ~ livstidsindkomst + arbejdsløshed + længde
                          + adgangskvotient + placering + åbenhed + velfærdsarbejde,
                          id = ~ id, by = ~ target,
                          # Ændrer rækkefølgen for niveauer
                          level_order = "descending",
                          alpha = 0.1,
                          # Tilføjer andre labels til attributter
                          feature_labels = list(livstidsindkomst = "LIVSTIDSINDKOMST",
                                                arbejdsløshed = "ARBEJDSLØSHED",
                                                længde = "LÆNGDE",
                                                adgangskvotient = "ADGANGSKVOTIENT",
                                                placering = "PLACERING",
                                                åbenhed = "ÅBENHED",
                                                velfærdsarbejde = "VELFÆRDSARBEJDE"))

# Laver plot opdelt på target-status
plot(sub_target, "target", feature_headers = TRUE, header_fmt = "%s",
     xlim = c(-0.52, 0.5), xlab = "AMCE", legend_title = "Tærskel") +
  theme_bw() +
  scale_color_manual(values = c("Opfyldt/lighed" = "black",
                                "Ikke opfyldt" = "#999999")) +
  theme(legend.position = "bottom") +
  scale_x_continuous(breaks = c(-0.5, -0.3, -0.1, 0.1, 0.3, 0.5))

# Eksporterer figur som pdf
ggsave('Output/target_opdelt.pdf', width = 7, height = 6)

# Laver plot over forskel efter target-status
plot(diff_target, feature_headers = TRUE, header_fmt = "%s",
     xlab = "", xlim = c(-0.7, 0.2)) +
  theme_bw() +
  scale_color_manual(values = c("black", "black", "black", "black", "black",
                                "black", "black")) +
  ggtitle("Forskel = Opfyldt/lighed - Ikke opfyldt") +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5, size = 10)) +
  scale_x_continuous(breaks = c(-0.6, -0.4, -0.2, 0, 0.2))

ggsave('Output/target_forskel.pdf', width = 7, height = 6)

############################ ROBUSTHEDSTESTS ###################################

## Placebo-test med arbejdsløshed og åbenhed

# Arbejdsløshed
placebo_arbejdsløshed <- amce_diffs(df, valg ~ livstidsindkomst + arbejdsløshed
                                    + længde + adgangskvotient + placering + åbenhed
                                    + velfærdsarbejde, id = ~ id,
                                    by = ~ arbejdsløshed_target,
                       # Ændrer rækkefølgen for niveauer
                       level_order = "descending",
                       alpha = 0.1,
                       # Tilføjer andre labels til attributter
                       feature_labels = list(livstidsindkomst = "LIVSTIDSINDKOMST",
                                             arbejdsløshed = "ARBEJDSLØSHED",
                                             længde = "LÆNGDE",
                                             adgangskvotient = "ADGANGSKVOTIENT",
                                             placering = "PLACERING",
                                             åbenhed = "ÅBENHED",
                                             velfærdsarbejde = "VELFÆRDSARBEJDE"))

# Laver plot over placebo_arbejdsløshed
p1 <- plot(placebo_arbejdsløshed, feature_headers = TRUE, header_fmt = "%s",
     xlab = "") +
  theme_bw() +
  scale_color_manual(values = c("black", "black", "black", "black", "black",
                                "black", "black")) +
  ggtitle("Arbejdsløshed") +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5, size = 10))



# Åbenhed
placebo_åbenhed <- amce_diffs(df, valg ~ livstidsindkomst + arbejdsløshed
                                    + længde + adgangskvotient + placering + åbenhed
                                    + velfærdsarbejde, id = ~ id,
                                    by = ~ åbenhed_target,
                                    # Ændrer rækkefølgen for niveauer
                                    level_order = "descending",
                                    alpha = 0.1,
                                    # Tilføjer andre labels til attributter
                                    feature_labels = list(livstidsindkomst = "LIVSTIDSINDKOMST",
                                                          arbejdsløshed = "ARBEJDSLØSHED",
                                                          længde = "LÆNGDE",
                                                          adgangskvotient = "ADGANGSKVOTIENT",
                                                          placering = "PLACERING",
                                                          åbenhed = "ÅBENHED",
                                                          velfærdsarbejde = "VELFÆRDSARBEJDE"))

# Laver plot over placebo_åbenhed
p2 <- plot(placebo_åbenhed, feature_headers = TRUE, header_fmt = "%s",
     xlab = "") +
  theme_bw() +
  scale_color_manual(values = c("black", "black", "black", "black", "black",
                                "black", "black")) +
  ggtitle("Åbenhed") +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5, size = 10),
        axis.text.y = element_blank())


# Samler de to figurer og eksporterer dem
g <- arrangeGrob(p1, p2, ncol = 5,
                 layout_matrix = cbind(1, 1, 1, 2, 2),
                 top = "                               Forskel = Opfyldt/lighed - Ikke opfyldt/variation")
ggsave(filename = "Output/placebo_tests.pdf", g, width = 7, height = 6)

## Kør target-setting og ekskluder lighed på indkomst,
## for at udelukke leksiografiske præferencer

diff_target_variation <- amce_diffs(df, valg ~ livstidsindkomst + arbejdsløshed + længde
                                    + adgangskvotient + placering + åbenhed + velfærdsarbejde,
                                    id = ~ id, by = ~ target_variation,
                                    # Ændrer rækkefølgen for niveauer
                                    level_order = "descending",
                                    alpha = 0.1,
                                    # Tilføjer andre labels til attributter
                                    feature_labels = list(livstidsindkomst = "LIVSTIDSINDKOMST",
                                                          arbejdsløshed = "ARBEJDSLØSHED",
                                                          længde = "LÆNGDE",
                                                          adgangskvotient = "ADGANGSKVOTIENT",
                                                          placering = "PLACERING",
                                                          åbenhed = "ÅBENHED",
                                                          velfærdsarbejde = "VELFÆRDSARBEJDE"))

# Laver plot over forskel efter target-status og eksporterer den
plot(diff_target_variation, feature_headers = TRUE, header_fmt = "%s",
     xlab = "") +
  theme_bw() +
  scale_color_manual(values = c("black", "black", "black", "black", "black",
                                "black", "black")) +
  ggtitle("Forskel = Opfyldt - Ikke opfyldt") +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5, size = 10)) +
  geom_point(aes(x = -0.05, y = "10"), color = "#999999") +
  geom_point(aes(x = -0.13, y = "6"), color = "#999999") +
  geom_point(aes(x = -0.076, y = "Kun kvote 2"), color = "#999999") +
  geom_point(aes(x = -0.098, y = "10%"), color = "#999999") +
  geom_point(aes(x = -0.137, y = "15%"), color = "#999999") +
  geom_point(aes(x = -0.153, y = "20%"), color = "#999999") +
  geom_point(aes(x = -0.067, y = "3,5 år"), color = "#999999") +
  geom_point(aes(x = -0.155, y = "4 år"), color = "#999999") +
  geom_point(aes(x = -0.126, y = "5 år"), color = "#999999") +
  geom_point(aes(x = -0.056, y = "Uden for de fire store byer"), color = "#999999") +
  geom_point(aes(x = -0.009, y = "Mulighed for velfærdsjobs"), color = "#999999") +
  geom_point(aes(x = 0.05, y = "Mange jobs"), color = "#999999")

ggsave("Output/target_uden_lighed.pdf", width = 7, height = 6)

# Fjerner midlertidige objekter
rm(g, p1, p2)

##### //EOS\\
