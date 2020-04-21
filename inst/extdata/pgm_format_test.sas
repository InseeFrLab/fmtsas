proc format ;

  /* Noms r�gions */
  value $reg
  "01" = "Guadeloupe"
  "02" = "Martinique"
  "03" = "Guyane"
  "04" = "La R�union"
  "05" = "Mayotte"
  "11" = "�le-de-France"
  "24" = "Centre-Val de Loire"
  "27" = "Bourgogne-Franche-Comt�"
  "28" = "Normandie"
  "32" = "Hauts-de-France"
  "44" = "Grand Est"
  "52" = "Pays de la Loire"
  "53" = "Bretagne"
  "75" = "Nouvelle Aquitaine"
  "76" = "Occitanie"
  "84" = "Auvergne-Rh�ne-Alpes"
  "93" = "Provence-Alpes-C�te d'Azur"
  "94" = "Corse" ;

  /* Secteur d'activit� regroup� */
  value $a13_
  "B", "C", "D", "E" = "Industrie" 
  "F"                = "Construction"
  "G"                = "Commerce, r�paration d'automobiles et de motocycles" 
  "H"                = "Transports et entreposage"
  "I"                = "H�bergement et restauration"
  "J"                = "Information et communication"
  "K", "L"           = "Activit�s financi�res, d'assurance et immobili�res"
  "M"                = "Activit�s sp�cialis�es, scientifiques et techniques"
  "N"                = "Activit�s de services administratifs et de soutien"
  "P"                = "Enseignement"
  "Q"                = "Sant� humaine et action sociale"
  "R"                = "Arts, spectacles, activit�s r�cr�atives"
  "S"                = "Autres activit�s de services"
  other              = "Hors champ" ;

run ;

PROC FORMAT;VALUE $ sexe  "1"="Homme"    "2"="Femme" ;RUN;
