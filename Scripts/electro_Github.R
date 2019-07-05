pacman::p_load(readr,corrplot, ggplot2,caret, lattice,
               plotly, reshape,arules, arulesViz, treemap, dplyr,plyr,imager,
               RColorBrewer,devtools, shiny,shinythemes)


#library------------------------------------------------------------------------
install.packages("arules")
install.packages("mlbench")
install.packages("C50")
install.packages("rpart.plot")
library(lattice)
library(ggplot2)
library(caret)
library(mlbench)
library(readr)
library(C50)
library(rpart)
library(rpart.plot)
library(class)
library(dplyr)
library(reshape2)
library(scales)
library(plotly)
library(corrplot)
library(Matrix)
library(caTools)
library(arules)
library(arulesViz)




#Read data ------------------------------------------------------------------------
orders <- read.csv2("C:/Users/KEYLA/Desktop/Rcsv/CSV_task4/orders_translated.csv")
lineItem <- read.csv2("C:/Users/KEYLA/Desktop/Rcsv/CSV_task4/lineitems.csv")
trans <- read.csv("C:/Users/KEYLA/Desktop/Rcsv/CSV_task4/trans.csv", header=FALSE)
trans[-1,]
trans <- trans[-1,]

#Read Transactions---------------------------------------------------------------------------
setwd("C:/Users/KEYLA/Desktop/Rcsv/CSV_task4") 
dataTrans <- read.transactions("trans.csv", format = "basket", sep=",", rm.duplicates=TRUE)
dataTrans[-1,]
dataTrans <- dataTrans[-1,]

summary(dataTrans)
is.na(dataTrans)
dim(dataTrans)



#NA ----------------------------------------------------------------------------------
summary(orders)
sum(is.na(orders))
orders <- na.omit(orders)
sum(is.na(lineItem))

#compare id order clomuns ------------------------------------------------------------------
orders$id_order %in% lineItem$id_order 
table(orders$id_order %in% lineItem$id_order) 
all (orders$id_order %in% lineItem$id_order )


## Deletion of all orders that are no repeated more that once ---------------------------------------------
orders_twoormore <- lineItem %>% group_by(id_order) %>% filter (n() >1)
length(unique(orders_twoormore$id_order))
summary(orders$state)

orders_completed <- orders %>% filter(state=="Completed")

#compare orders_complete with orders_twoormore, length 10453-------------------------
length(which(orders_completed$id_order %in% orders_twoormore$id_order))

match <- which (orders_twoormore$id_order %in% orders_completed$id_order)
orders_trans <- orders_twoormore [match,]
length(unique(orders_trans$id_order))

# Check if the total paid in the orders_translated data set-----------------------------
#correspond to the total unit price in lineitem.csv. 

total_paid_in_transaction<- orders_trans %>% group_by(id_order) %>% mutate(paid = product_quantity*unit_price) %>%
  dplyr :: summarise (total_paid = sum(paid), number_of_items = n())

comprobacion_precio <- full_join(orders ,total_paid_in_transaction, by= "id_order")

#Change names----------------------------------------------------------------------
names(comprobacion_precio)[4]<-"total_paid_orders_dataset"

names(comprobacion_precio)[5]<-"total_paid_lineitems_dataset"

# All NA values are removed.------------------------------------------------------

comprobacion_precio <- comprobacion_precio[complete.cases(comprobacion_precio), ]

# The price diference is calculated.
comprobacion_precio$price_diference <- as.numeric((comprobacion_precio$total_paid_orders_dataset - comprobacion_precio$total_paid_lineitems_dataset))

# The new column is rounded by 2 decimals.
comprobacion_precio [,7] <-round(comprobacion_precio[,7],2)


# Find the id order  for each transaction 
trans$id_order <- as.factor(total_paid_in_transaction$id_order) 


#ordena las columnas 
ncol(trans)
col_order <- c(14,1:13)
trans <- trans[,col_order]

trans$state <- as.factor(comprobacion_precio$state) 
col_order <- c(15,1:14)
trans <- trans[,col_order]

#PLOT ------------------------------------------------------------------------------

#plot with percentage of the each state purchase realized

ggplot(orders, aes(x= state,  group=1)) +
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") +
  geom_text(aes( label = scales::percent(..prop..),
                 y= ..prop.. ), stat= "count", vjust = -.5) +
  labs(y = "Percent", fill="state") +
  scale_y_continuous(labels=percent)


#Market basket analysis----------------------------------------------------------

#you can use each function to know your transactional data 
inspect(head(dataTrans))
length(dataTrans)
size(head(dataTrans))
LIST(head(dataTrans))
itemLabels(dataTrans)
tail(dataTrans)

#Item frequency
itemFrequency(dataTrans[,1:9])

itemFrequencyPlot(dataTrans, topN = 5)

inspect(dataTrans[1:10],
        setStart = "")

#You can use the itemFrequencyPlot() function to visualize the item frequencies within the transactions as a bar chart.
itemFrequencyPlot(dataTrans, horiz = TRUE, 
                  type = "absolute",topN = 20,col=brewer.pal(8,'Pastel2'),
                  main='Item Frequency Plot')

#the image() function, you can visualize all of the transactions within your dataset.
image(sample(dataTrans,100))
image(dataTrans)

#Apriori algorithm-----------------------------------------------------------------

rules <- apriori(dataTrans, parameter = list(supp = 0.001, conf = 0.5)) 

plot(rules)
inspect(rules)
summary(rules)

#Sorting rules, The "ordenar"sort() function will sort the rules in a decreasing order (higher to lower values.)
inspect(head(sort(rules, by = "confidence" ),5))

#Subset rules, To see a certain item's rules you can use the subset() function.
inspect(head(subset(rules, items %pin% "WDT0177"), 10))

#Check redundant rules and if there is, eliminate it. 
sum(is.redundant(rules))
inspect(rules[!is.redundant(rules)])

# With ruleExplorer() you can visualize better the difference plots and plays with variables which you want.
ruleExplorer(rules)

# 3rd epic ---------------------------------------------------------------------------
#Data Brands by product 
#READ DATA, Text file products with category 
setwd("/Users/KEYLA/Desktop")
data_txt <- read_table2("products_with_category.txt")
sum(is.na(data_txt))
head(data_txt)


nrow(data_txt)
nrow(trans)

#Check transactions of each categroy 
summary(as.factor(data_txt$manual_categories))

#Aggregate level for categories------------------------------------------------------
match_2 <- match(dataTrans@itemInfo$labels,data_txt$sku)

data_txt_2 <- data_txt[match_2,]
head(data_txt_2)

dataTrans@itemInfo$categories <- data_txt_2$manual_categories

#Aggregate, you can setting level to make predictions. 
dataTrans_2 <- aggregate(dataTrans,itemInfo(dataTrans)[["categories"]])

summary(dataTrans_2)

rules1 <- apriori(dataTrans_2, parameter = list(supp = 0.04, conf = 0.08,minlen=2)) 
inspect((rules1))


rules2 <- apriori(dataTrans_2, parameter=list (supp=0.00001,conf = 0.02, minlen=2),
                  appearance = list (lhs="laptop"))
rules.sub <- subset(rules1, subset = lhs %ain% c("laptop", "display"))
inspect(rules.sub)

ruleExplorer(rules2)
summary(rules2)


inspect(head(sort(rules1, by = "lift" ),10))
inspect(rules1[is.redundant(rules1)])


itemFrequency(dataTrans_2)
itemFrequencyPlot(dataTrans_2, horiz = FALSE, 
                  type = "absolute",topN = 10,col=brewer.pal(8,'Pastel2'),
                  main='Item Frequency')

#Type products  -----
brands_product <- read.csv2("C:/Users/KEYLA/Desktop/products.csv")


match_3 <- match(dataTrans@itemInfo$labels, brands_product$sku)
products <- brands_product [match_3,]
dataTrans@itemInfo$type_product <- products$name

#Using Substr() 
productsBrand <- substr(data_txt_2$sku, 1, 3)


productsBrand_2 <- data_txt_2
productsBrand_2$productsBrand <- productsBrand
productsBrand_2$productsBrand_full <- as.character(c("unknown_brand"))


productsBrand_2$productsBrand <- ifelse(is.na(productsBrand_2$productsBrand), 'TEMPORAL', productsBrand_2$productsBrand)

productsBrand_2$manual_categories <- as.character(productsBrand_2$manual_categories)

productsBrand_2$manual_categories <- ifelse(is.na(productsBrand_2$manual_categories), 'NO_DATA', productsBrand_2$manual_categories)




#Adding brand for each product --------------------------------------------------- 

for (row in 1:nrow(productsBrand_2)) {
  if(productsBrand_2[row,3]=="8MO"){productsBrand_2[row,4]<- c("8Mobility")
  } else if (productsBrand_2[row,3]=="ACM") {productsBrand_2[row,4]<- c("Acme")
  } else if (productsBrand_2[row,3]=="ADN") {productsBrand_2[row,4]<- c("Adonit")
  } else if (productsBrand_2[row,3]=="AII") {productsBrand_2[row,4]<- c("Aiino")
  } else if (productsBrand_2[row,3]=="AKI") {productsBrand_2[row,4]<- c("Akitio")
  } else if (productsBrand_2[row,3]=="ALL") {productsBrand_2[row,4]<- c("Allocacoc")
  } else if (productsBrand_2[row,3]=="AP2") {productsBrand_2[row,4]<- c("Apple_Second_Hand")
  } else if (productsBrand_2[row,3]=="APP") {productsBrand_2[row,4]<- c("Apple_Brand_New")
  } else if (productsBrand_2[row,3]=="BAN") {productsBrand_2[row,4]<- c("Apple_Panama")
  } else if (productsBrand_2[row,3]=="BEA") {productsBrand_2[row,4]<- c("BeatsX")
  } else if (productsBrand_2[row,3]=="BEL") {productsBrand_2[row,4]<- c("Belkin")
  } else if (productsBrand_2[row,3]=="BEZ") {productsBrand_2[row,4]<- c("Beez")
  } else if (productsBrand_2[row,3]=="BLL") {productsBrand_2[row,4]<- c("Blue_Lounge")
  } else if (productsBrand_2[row,3]=="BLM") {productsBrand_2[row,4]<- c("Blue")
  } else if (productsBrand_2[row,3]=="BNQ") {productsBrand_2[row,4]<- c("BenQ")
  } else if (productsBrand_2[row,3]=="BOD") {productsBrand_2[row,4]<- c("BodyGuardz")
  } else if (productsBrand_2[row,3]=="BOO") {productsBrand_2[row,4]<- c("Booq")
  } else if (productsBrand_2[row,3]=="BOS") {productsBrand_2[row,4]<- c("Bose")
  } else if (productsBrand_2[row,3]=="BTC") {productsBrand_2[row,4]<- c("Boostcase")
  } else if (productsBrand_2[row,3]=="CAI") {productsBrand_2[row,4]<- c("Casetify")
  } else if (productsBrand_2[row,3]=="CEL") {productsBrand_2[row,4]<- c("Celly")
  } else if (productsBrand_2[row,3]=="COG") {productsBrand_2[row,4]<- c("Cogito")
  } else if (productsBrand_2[row,3]=="CRU") {productsBrand_2[row,4]<- c("Crucial")
  } else if (productsBrand_2[row,3]=="CYB") {productsBrand_2[row,4]<- c("CyberPower")
  } else if (productsBrand_2[row,3]=="CYG") {productsBrand_2[row,4]<- c("Cygnett")
  } else if (productsBrand_2[row,3]=="DEV") {productsBrand_2[row,4]<- c("Devolo")
  } else if (productsBrand_2[row,3]=="DJI") {productsBrand_2[row,4]<- c("DJI")
  } else if (productsBrand_2[row,3]=="DLK") {productsBrand_2[row,4]<- c("D-Link")
  } else if (productsBrand_2[row,3]=="DLL") {productsBrand_2[row,4]<- c("Dell")
  } else if (productsBrand_2[row,3]=="DOD") {productsBrand_2[row,4]<- c("Dodocool")
  } else if (productsBrand_2[row,3]=="DRO") {productsBrand_2[row,4]<- c("Drobo")
  } else if (productsBrand_2[row,3]=="DVI") {productsBrand_2[row,4]<- c("Devia")
  } else if (productsBrand_2[row,3]=="EIZ") {productsBrand_2[row,4]<- c("Eizo")
  } else if (productsBrand_2[row,3]=="ELA") {productsBrand_2[row,4]<- c("Elago")
  } else if (productsBrand_2[row,3]=="ELE") {productsBrand_2[row,4]<- c("Elevation_Apple")
  } else if (productsBrand_2[row,3]=="ELG") {productsBrand_2[row,4]<- c("Elgato")
  } else if (productsBrand_2[row,3]=="EVU") {productsBrand_2[row,4]<- c("Evutec")
  } else if (productsBrand_2[row,3]=="FCM") {productsBrand_2[row,4]<- c("FCM")
  } else if (productsBrand_2[row,3]=="FIB") {productsBrand_2[row,4]<- c("Fibaro")
  } else if (productsBrand_2[row,3]=="FIF") {productsBrand_2[row,4]<- c("Fitbit")
  } else if (productsBrand_2[row,3]=="GAM") {productsBrand_2[row,4]<- c("Gamevice")
  } else if (productsBrand_2[row,3]=="GLY") {productsBrand_2[row,4]<- c("Glyph")
  } else if (productsBrand_2[row,3]=="GOP") {productsBrand_2[row,4]<- c("GoPro")
  } else if (productsBrand_2[row,3]=="GRT") {productsBrand_2[row,4]<- c("Griffin")
  } else if (productsBrand_2[row,3]=="GTE") {productsBrand_2[row,4]<- c("G_Technology")
  } else if (productsBrand_2[row,3]=="HAR") {productsBrand_2[row,4]<- c("Harman")
  } else if (productsBrand_2[row,3]=="HGD") {productsBrand_2[row,4]<- c("Henge_Docks")
  } else if (productsBrand_2[row,3]=="HGS") {productsBrand_2[row,4]<- c("HGST_Travelstar")
  } else if (productsBrand_2[row,3]=="HIC") {productsBrand_2[row,4]<- c("Hitcase")
  } else if (productsBrand_2[row,3]=="HOC") {productsBrand_2[row,4]<- c("	Nike_hoco_Apple")
  } else if (productsBrand_2[row,3]=="HOW") {productsBrand_2[row,4]<- c("Honeywell")
  } else if (productsBrand_2[row,3]=="HTE") {productsBrand_2[row,4]<- c("Hyper")
  } else if (productsBrand_2[row,3]=="IAM") {productsBrand_2[row,4]<- c("I_AM")
  } else if (productsBrand_2[row,3]=="ICA") {productsBrand_2[row,4]<- c("Incase")
  } else if (productsBrand_2[row,3]=="IFR") {productsBrand_2[row,4]<- c("iFrogz")
  } else if (productsBrand_2[row,3]=="IFX") {productsBrand_2[row,4]<- c("iFixit")
  } else if (productsBrand_2[row,3]=="IHE") {productsBrand_2[row,4]<- c("iHealth")
  } else if (productsBrand_2[row,3]=="IKM") {productsBrand_2[row,4]<- c("IK_Multimedia")
  } else if (productsBrand_2[row,3]=="IOT") {productsBrand_2[row,4]<- c("iOttie")
  } else if (productsBrand_2[row,3]=="JAB") {productsBrand_2[row,4]<- c("Jabra")
  } else if (productsBrand_2[row,3]=="JAW") {productsBrand_2[row,4]<- c("Jawbone")
  } else if (productsBrand_2[row,3]=="JBL") {productsBrand_2[row,4]<- c("JBL")
  } else if (productsBrand_2[row,3]=="JMO") {productsBrand_2[row,4]<- c("Just_Mobile")
  } else if (productsBrand_2[row,3]=="JOB") {productsBrand_2[row,4]<- c("Joby")
  } else if (productsBrand_2[row,3]=="JYB") {productsBrand_2[row,4]<- c("Jaybird")
  } else if (productsBrand_2[row,3]=="KAI") {productsBrand_2[row,4]<- c("Kaiser")
  } else if (productsBrand_2[row,3]=="KAN") {productsBrand_2[row,4]<- c("Kanex")
  } else if (productsBrand_2[row,3]=="KEN") {productsBrand_2[row,4]<- c("Kensington")
  } else if (productsBrand_2[row,3]=="KEU") {productsBrand_2[row,4]<- c("kenu")
  } else if (productsBrand_2[row,3]=="KIN") {productsBrand_2[row,4]<- c("Kingston")
  } else if (productsBrand_2[row,3]=="KOO") {productsBrand_2[row,4]<- c("Koogeek")
  } else if (productsBrand_2[row,3]=="KUA") {productsBrand_2[row,4]<- c("Kukaclip")
  } else if (productsBrand_2[row,3]=="LAC") {productsBrand_2[row,4]<- c("LaCie")
  } else if (productsBrand_2[row,3]=="LAN") {productsBrand_2[row,4]<- c("LandingZone")
  } else if (productsBrand_2[row,3]=="LEE") {productsBrand_2[row,4]<- c("Leef")
  } else if (productsBrand_2[row,3]=="LEX") {productsBrand_2[row,4]<- c("Lexar")
  } else if (productsBrand_2[row,3]=="LGE") {productsBrand_2[row,4]<- c("LG")
  } else if (productsBrand_2[row,3]=="LIF") {productsBrand_2[row,4]<- c("Lifeproof")
  } else if (productsBrand_2[row,3]=="LMP") {productsBrand_2[row,4]<- c("LMP")
  } else if (productsBrand_2[row,3]=="LOG") {productsBrand_2[row,4]<- c("Logitech")
  } else if (productsBrand_2[row,3]=="LUN") {productsBrand_2[row,4]<- c("Lunatik")
  } else if (productsBrand_2[row,3]=="MAC") {productsBrand_2[row,4]<- c("Macally")
  } else if (productsBrand_2[row,3]=="MAK") {productsBrand_2[row,4]<- c("Maclocks")
  } else if (productsBrand_2[row,3]=="MAT") {productsBrand_2[row,4]<- c("Matias")
  } else if (productsBrand_2[row,3]=="MBI") {productsBrand_2[row,4]<- c("Mobility")
  } else if (productsBrand_2[row,3]=="MDT") {productsBrand_2[row,4]<- c("Mediterrans")
  } else if (productsBrand_2[row,3]=="MIC") {productsBrand_2[row,4]<- c("Microsoft_MAC")
  } else if (productsBrand_2[row,3]=="MIN") {productsBrand_2[row,4]<- c("Minibatt")
  } else if (productsBrand_2[row,3]=="MMW") {productsBrand_2[row,4]<- c("My_MW")
  } else if (productsBrand_2[row,3]=="MOL") {productsBrand_2[row,4]<- c("Moleskine")
  } else if (productsBrand_2[row,3]=="MOP") {productsBrand_2[row,4]<- c("Mophie")
  } else if (productsBrand_2[row,3]=="MOS") {productsBrand_2[row,4]<- c("Moshi")
  } else if (productsBrand_2[row,3]=="MOX") {productsBrand_2[row,4]<- c("Moxie")
  } else if (productsBrand_2[row,3]=="MTF") {productsBrand_2[row,4]<- c("Mistify")
  } else if (productsBrand_2[row,3]=="MUJ") {productsBrand_2[row,4]<- c("Mujjo")
  } else if (productsBrand_2[row,3]=="MUV") {productsBrand_2[row,4]<- c("Muvit")
  } else if (productsBrand_2[row,3]=="MYF") {productsBrand_2[row,4]<- c("MyFox")
  } else if (productsBrand_2[row,3]=="NDA") {productsBrand_2[row,4]<- c("Nonda")
  } else if (productsBrand_2[row,3]=="NDE") {productsBrand_2[row,4]<- c("Ndevr")
  } else if (productsBrand_2[row,3]=="NEA") {productsBrand_2[row,4]<- c("Netatmo")
  } else if (productsBrand_2[row,3]=="NES") {productsBrand_2[row,4]<- c("Nest")
  } else if (productsBrand_2[row,3]=="NIE") {productsBrand_2[row,4]<- c("Ninebot")
  } else if (productsBrand_2[row,3]=="NIM") {productsBrand_2[row,4]<- c("Nimbus")
  } else if (productsBrand_2[row,3]=="NKI") {productsBrand_2[row,4]<- c("Nokia")
  } else if (productsBrand_2[row,3]=="NOD") {productsBrand_2[row,4]<- c("Nodon")
  } else if (productsBrand_2[row,3]=="NOK") {productsBrand_2[row,4]<- c("Noke")
  } else if (productsBrand_2[row,3]=="NOM") {productsBrand_2[row,4]<- c("Nomad")
  } else if (productsBrand_2[row,3]=="NTE") {productsBrand_2[row,4]<- c("NewerTech")
  } else if (productsBrand_2[row,3]=="OBL") {productsBrand_2[row,4]<- c("Oblumi")
  } else if (productsBrand_2[row,3]=="OLI") {productsBrand_2[row,4]<- c("Olixar")
  } else if (productsBrand_2[row,3]=="OLL") {productsBrand_2[row,4]<- c("Olloclip")
  } else if (productsBrand_2[row,3]=="OPU") {productsBrand_2[row,4]<- c("Opulus")
  } else if (productsBrand_2[row,3]=="OSM") {productsBrand_2[row,4]<- c("Osmo")
  } else if (productsBrand_2[row,3]=="OTR") {productsBrand_2[row,4]<- c("Startech")
  } else if (productsBrand_2[row,3]=="OTT") {productsBrand_2[row,4]<- c("OtterBox ")
  } else if (productsBrand_2[row,3]=="OWC") {productsBrand_2[row,4]<- c("OWC")
  } else if (productsBrand_2[row,3]=="PAC") {productsBrand_2[row,4]<- c("Packs")
  } else if (productsBrand_2[row,3]=="PAR") {productsBrand_2[row,4]<- c("Parrot")
  } else if (productsBrand_2[row,3]=="PEB") {productsBrand_2[row,4]<- c("Pebble")
  } else if (productsBrand_2[row,3]=="PHI") {productsBrand_2[row,4]<- c("Philips")
  } else if (productsBrand_2[row,3]=="PIE") {productsBrand_2[row,4]<- c("Pieza_Apple")
  } else if (productsBrand_2[row,3]=="PLA") {productsBrand_2[row,4]<- c("Plantronic")
  } else if (productsBrand_2[row,3]=="POL") {productsBrand_2[row,4]<- c("Polaroid")
  } else if (productsBrand_2[row,3]=="PQI") {productsBrand_2[row,4]<- c("Pqi")
  } else if (productsBrand_2[row,3]=="PRO") {productsBrand_2[row,4]<- c("Promise")
  } else if (productsBrand_2[row,3]=="PRY") {productsBrand_2[row,4]<- c("Prynt")
  } else if (productsBrand_2[row,3]=="PUR") {productsBrand_2[row,4]<- c("Puro")
  } else if (productsBrand_2[row,3]=="QAR") {productsBrand_2[row,4]<- c("Qardio")
  } else if (productsBrand_2[row,3]=="QDO") {productsBrand_2[row,4]<- c("QDos")
  } else if (productsBrand_2[row,3]=="QNA") {productsBrand_2[row,4]<- c("QNAP")
  } else if (productsBrand_2[row,3]=="RAI") {productsBrand_2[row,4]<- c("RainDesign")
  } else if (productsBrand_2[row,3]=="REP") {productsBrand_2[row,4]<- c("Reparacion_Apple")
  } else if (productsBrand_2[row,3]=="RET") {productsBrand_2[row,4]<- c("Retrak")
  } else if (productsBrand_2[row,3]=="RUT") {productsBrand_2[row,4]<- c("Runtastic")
  } else if (productsBrand_2[row,3]=="RYV") {productsBrand_2[row,4]<- c("Ryval")
  } else if (productsBrand_2[row,3]=="SAM") {productsBrand_2[row,4]<- c("Samsung")
  } else if (productsBrand_2[row,3]=="SAN") {productsBrand_2[row,4]<- c("Sandisk ")
  } else if (productsBrand_2[row,3]=="SAT") {productsBrand_2[row,4]<- c("Satechi")
  } else if (productsBrand_2[row,3]=="SDE") {productsBrand_2[row,4]<- c("SDesign")
  } else if (productsBrand_2[row,3]=="SEA") {productsBrand_2[row,4]<- c("Seagate")
  } else if (productsBrand_2[row,3]=="SEV") {productsBrand_2[row,4]<- c("Servicio_Apple")
  } else if (productsBrand_2[row,3]=="SHE") {productsBrand_2[row,4]<- c("SwitchEasy")
  } else if (productsBrand_2[row,3]=="SNA") {productsBrand_2[row,4]<- c("Sena")
  } else if (productsBrand_2[row,3]=="SNN") {productsBrand_2[row,4]<- c("Sonnet")
  } else if (productsBrand_2[row,3]=="SNS") {productsBrand_2[row,4]<- c("Sonos")
  } else if (productsBrand_2[row,3]=="SOF") {productsBrand_2[row,4]<- c("Parallels")
  } else if (productsBrand_2[row,3]=="SPE") {productsBrand_2[row,4]<- c("Speck")
  } else if (productsBrand_2[row,3]=="SPH") {productsBrand_2[row,4]<- c("Sphero")
  } else if (productsBrand_2[row,3]=="SSE") {productsBrand_2[row,4]<- c("Sen.se")
  } else if (productsBrand_2[row,3]=="STA") {productsBrand_2[row,4]<- c("StarTech")
  } else if (productsBrand_2[row,3]=="STC") {productsBrand_2[row,4]<- c("Stacked")
  } else if (productsBrand_2[row,3]=="STI") {productsBrand_2[row,4]<- c("Stil")
  } else if (productsBrand_2[row,3]=="STK") {productsBrand_2[row,4]<- c("Stikgo")
  } else if (productsBrand_2[row,3]=="STM") {productsBrand_2[row,4]<- c("STM")
  } else if (productsBrand_2[row,3]=="SXA") {productsBrand_2[row,4]<- c("SecurityXtra")
  } else if (productsBrand_2[row,3]=="SYN") {productsBrand_2[row,4]<- c("Synology")
  } else if (productsBrand_2[row,3]=="TAD") {productsBrand_2[row,4]<- c("Tado")
  } else if (productsBrand_2[row,3]=="TAM") {productsBrand_2[row,4]<- c("Tangram")
  } else if (productsBrand_2[row,3]=="TCH") {productsBrand_2[row,4]<- c("Tech21")
  } else if (productsBrand_2[row,3]=="THU") {productsBrand_2[row,4]<- c("Thule")
  } else if (productsBrand_2[row,3]=="TIG") {productsBrand_2[row,4]<- c("Tigra")
  } else if (productsBrand_2[row,3]=="TIL") {productsBrand_2[row,4]<- c("Tile")
  } else if (productsBrand_2[row,3]=="TOS") {productsBrand_2[row,4]<- c("Toshiba")
  } else if (productsBrand_2[row,3]=="TPL") {productsBrand_2[row,4]<- c("TP_Link")
  } else if (productsBrand_2[row,3]=="TRA") {productsBrand_2[row,4]<- c("Transcend")
  } else if (productsBrand_2[row,3]=="TRI") {productsBrand_2[row,4]<- c("Tribe")
  } else if (productsBrand_2[row,3]=="TRK") {productsBrand_2[row,4]<- c("TrackR")
  } else if (productsBrand_2[row,3]=="TRN") {productsBrand_2[row,4]<- c("Trunk")
  } else if (productsBrand_2[row,3]=="TUC") {productsBrand_2[row,4]<- c("Tucano")
  } else if (productsBrand_2[row,3]=="TWS") {productsBrand_2[row,4]<- c("Twelve_South")
  } else if (productsBrand_2[row,3]=="UAG") {productsBrand_2[row,4]<- c("Urban_Armor_Gear")
  } else if (productsBrand_2[row,3]=="WAC") {productsBrand_2[row,4]<- c("Wacom")
  } else if (productsBrand_2[row,3]=="WDT") {productsBrand_2[row,4]<- c("Western_Digital ")
  } else if (productsBrand_2[row,3]=="WHO") {productsBrand_2[row,4]<- c("Whoosh")
  } else if (productsBrand_2[row,3]=="WIK") {productsBrand_2[row,4]<- c("Wikango")
  } else if (productsBrand_2[row,3]=="WIT") {productsBrand_2[row,4]<- c("Withings")
  } else if (productsBrand_2[row,3]=="WOE") {productsBrand_2[row,4]<- c("Wowewa")
  } else if (productsBrand_2[row,3]=="XDO") {productsBrand_2[row,4]<- c("XDoria_Apple")
  } else if (productsBrand_2[row,3]=="XOO") {productsBrand_2[row,4]<- c("Xoopar ")
  } else if (productsBrand_2[row,3]=="XRI") {productsBrand_2[row,4]<- c("X-Rite")
  } else if (productsBrand_2[row,3]=="XTO") {productsBrand_2[row,4]<- c("Xtorm")
  } else if (productsBrand_2[row,3]=="ZAG") {productsBrand_2[row,4]<- c("Zagg ")
  } else if (productsBrand_2[row,3]=="ZEP") {productsBrand_2[row,4]<- c("Zeep")
  } else productsBrand_2[row,4]<- c("Unknown_Brand")
}



#Replace the items name by its category in the transaction data
match_4 <- match(dataTrans@itemInfo$labels, productsBrand_2$sku)
epic_3 <- productsBrand_2 [match_4,]
dataTrans@itemInfo$brands <- epic_3$productsBrand_full


rules_3 <- apriori(dataTrans_2, parameter = list(supp = 0.04, conf = 0.8,minlen=2)) 
inspect((rules_3))


































































#ggplotly(ggplot() + geom_bar(aes(x=trans@itemInfo[["brands_cat"]])))

#length(unique(state$sku)) para saber cuanto tengo(en numeros) sobre alguna variable 