library(ipumsr)
library(data.table)
library(janitor)

ddi_15 <- 
  read_ipums_ddi("data/raw/usa_00015.xml") #ddi is a file that will let us access additional info about our data
ddi_16 <- 
  read_ipums_ddi("data/raw/usa_00016.xml") #ddi is a file that will let us access additional info about our data
ddi_17 <- 
  read_ipums_ddi("data/raw/usa_00017.xml") #ddi is a file that will let us access additional info about our data
ddi_18 <- 
  read_ipums_ddi("data/raw/usa_00018.xml") #ddi is a file that will let us access additional info about our data

usa_11 <- 
  data.table( 
  clean_names( 
    read_ipums_micro( ddi_15 )
    )
  )
usa_11 <- usa_11[,indnaics:=NULL][,(names(usa_11)):=lapply(.SD, as.numeric)][year>2010,]

usa_12_15 <- 
  data.table( 
    clean_names(
      read_ipums_micro( ddi_16 )
    )
  )
usa_12_15 <- usa_12_15[,indnaics:=NULL][,(names(usa_12_15)):=lapply(.SD, as.numeric)]

usa_16_18 <- 
  data.table( 
    clean_names( 
      read_ipums_micro( ddi_17 )
    )
  )
usa_16_18 <- usa_16_18[,indnaics:=NULL][,(names(usa_16_18)):=lapply(.SD, as.numeric)]

usa_19 <- 
  data.table( 
    clean_names( 
      read_ipums_micro( ddi_18 )
    )
  )
usa_19 <- usa_19[,indnaics:=NULL][,(names(usa_19)):=lapply(.SD, as.numeric)]

usa <- rbind( usa_11 , 
              usa_12_15 , 
              usa_16_18 ,
              usa_19 )

rm(list=c("usa_11" ,"usa_12_15","usa_16_18","usa_19"))
saveRDS(usa, "data/export/usa_2011_2019.rds")
