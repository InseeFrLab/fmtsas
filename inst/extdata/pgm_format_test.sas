proc format ;

  /* Noms régions */
  value $reg
  "01" = "Guadeloupe"
  "02" = "Martinique"
  "03" = "Guyane"
  "04" = "La Réunion"
  "05" = "Mayotte"
  "11" = "Île-de-France"
  "24" = "Centre-Val de Loire"
  "27" = "Bourgogne-Franche-Comté"
  "28" = "Normandie"
  "32" = "Hauts-de-France"
  "44" = "Grand Est"
  "52" = "Pays de la Loire"
  "53" = "Bretagne"
  "75" = "Nouvelle Aquitaine"
  "76" = "Occitanie"
  "84" = "Auvergne-Rhône-Alpes"
  "93" = "Provence-Alpes-Côte d'Azur"
  "94" = "Corse" ;

  /* Secteur d'activité regroupé */
  value $a13_
  "B", "C", "D", "E" = "Industrie" 
  "F"                = "Construction"
  "G"                = "Commerce, réparation d'automobiles et de motocycles" 
  "H"                = "Transports et entreposage"
  "I"                = "Hébergement et restauration"
  "J"                = "Information et communication"
  "K", "L"           = "Activités financières, d'assurance et immobilières"
  "M"                = "Activités spécialisées, scientifiques et techniques"
  "N"                = "Activités de services administratifs et de soutien"
  "P"                = "Enseignement"
  "Q"                = "Santé humaine et action sociale"
  "R"                = "Arts, spectacles, activités récréatives"
  "S"                = "Autres activités de services"
  other              = "Hors champ" ;

run ;

PROC FORMAT;VALUE $ sexe  "1"="Homme"    "2"="Femme" ;RUN;

data t1 ;
  set t2 ;
  GEO2 = put(REG2016, $reg.) ;
  A13 = put(A21, $a13.) ;
run ;
