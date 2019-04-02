## Laden von Daten aus Exceldateien oder MS Access-Datenbanken

# Nur ganz kurz...
library(kwb.db)

xls <- "testdata.xls"
mdb <- "testdata.mdb"

# Anzeige der Tabellen in einer Excel-Datei/MS Access-Datenbank
hsTables(xls)
hsTables(mdb)

hsGetTable(xls, "testdata_de$")
hsGetTable(xls, "testbereich")
hsGetTable(mdb, "testdata_de")
