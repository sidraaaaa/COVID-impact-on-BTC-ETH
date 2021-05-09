btc<-read.csv("BTC_USD_2021-01-01_2021-04-27-CoinDesk.csv")
eth<-read.csv("ETH_USD_2021-01-01_2021-04-27-CoinDesk.csv")
covid_19<-read.csv("data_Confirmed.csv")
library(tidyverse)
library(htmlwidgets)
library(jcolors)
library(RColorBrewer)
library(data.table)
library(ggplot2)
library(plotly)
library(gapminder)
library(rworldmap)
library(gridExtra)
library(dygraphs)
library(xts)
library(timelineR)
library(hrbrthemes)
library(viridis)

#COVID data- column 2-345 -> year 2020's data 
#Removing 2020 data
covid<-covid_19[ -c(2:346) ] #Data starting from 2021-01-01
co<-covid_19[ -c(2:346) ]
colnames(btc)
colnames(btc)[3]<-"btc_close"
colnames(btc)[4]<-"btc_open"
colnames(btc)[5]<-"btc_high"
colnames(btc)[6]<-"btc_low"

colnames(eth)[3]<-"eth_close"
colnames(eth)[4]<-"eth_open"
colnames(eth)[5]<-"eth_high"
colnames(eth)[6]<-"eth_low"

crypt<-btc%>%left_join(eth,by="Date")
colnames(crypt)[1]<-"btc"
colnames(crypt)[7]<-"eth"

covid$total_cases<-rowSums(covid[,c(2:115)]) #Total cases in each country-2021
covid$total_cases<-NULL
##joining country data based on country name
join_data<-joinCountryData2Map(covid, joinCode = "NAME", 
                               nameJoinColumn = "Country.Region",
                               , mapResolution="coarse",
                               nameCountryColumn = "Country")
#Mapping COVID cases all around the world
mapDevice()

theMap <- mapCountryData( join_data, 
          nameColumnToPlot=c("X20210424"),
          #colourPalette = "rainbow", 
          borderCol = "gray",
          mapTitle = "Total COVID-19 cases in 2021", 
          addLegend = FALSE )

##Adjusting Legends
do.call( addMapLegendBoxes, c(theMap,list(legendText=c('Antarctic','Africa','Oceania',
                    'Americas','South Asia','Eurasia'),x='bottom',cex=0.8,
                              title="Region",horiz=TRUE)))
do.call( addMapLegend, c( theMap
                          , legendLabels="all"
                          , legendWidth=0.5
                          , legendMar=5
                          , horizontal=TRUE
))

#Zoomed in COVID cases in South Asia
mapDevice()

theMap <- mapCountryData( join_data, 
          nameColumnToPlot=c("X20210424"), 
          mapTitle = "Total cases in asia in 2021", 
          mapRegion = "asia",
          colourPalette = "heat",
          addLegend=FALSE )
do.call( addMapLegendBoxes, c(theMap,list(legendText=
        c('Antarctic','Africa','Oceania','Americas',
          'South Asia','Eurasia'),x='bottom',cex=1,
           title="Region",horiz=TRUE)))
do.call( addMapLegend, c( theMap
                          , legendLabels="all"
                          , legendWidth=0.5
                          , legendMar=7
                          , horizontal=FALSE
))

#Zoomed in COVID cases in north america
mapDevice()

theMap <- mapCountryData( join_data, 
                          nameColumnToPlot=c("X20210424"), 
                          mapTitle = "Total cases in North America in 2021", 
                          mapRegion = "north america",
                          colourPalette = "heat",
                          addLegend=FALSE )
do.call( addMapLegendBoxes, c(theMap,list(legendText=
                                            c('Antarctic','Africa','Oceania','Americas',
                                              'South Asia','Eurasia'),x='bottom',cex=1,
                                          title="Region",horiz=TRUE)))
do.call( addMapLegend, c( theMap
                          , legendLabels="all"
                          , legendWidth=0.5
                          , legendMar=7
                          , horizontal=FALSE))
#Total cases in each day
cases_per_day <- as.data.frame(list(colSums(covid[2:115])))
setDT(cases_per_day, keep.rownames = TRUE)[]
colnames(cases_per_day)[1]<-"date"
colnames(cases_per_day)[2]<-"cases"



# colSums(covid[2:115])%>%pivot_longer(
#   cols=starts_with("X"),
#   names_prefix = "X",
#   names_to = "date",
#   values_to= "total_cases",
#   values_drop_na=TRUE)


###Refining crypto data
crypt<-crypt %>%
  dplyr::mutate(year = lubridate::year(Date), 
                month = lubridate::month(Date), 
                day = lubridate::day(Date))
                
                
###Refining and reformatting COVID data
names(covid)<-substring(names(covid),2,9)
#names(co)<-substring(names(co),2,9)
colnames(covid)[1]<-"country.R"
#Changing column data types and reformatting to Date
temp<-covid
colnames(covid)[2:115]=as.character(c(as.Date(parse_date_time(colnames(covid)[2:115],"ymd"))))


###
#df <- melt(covid ,  id.vars = 'country.R', variable.name = 'series')

#Tidy Pivot_longer for converting multiple date columns into one date column
cov<-covid %>% 
  pivot_longer(!country.R, names_to = "Date", values_to = "cases")
#Joining covid and crypto data based on date
cov_cryp<-cov%>%left_join(crypt,by="Date")

selec_data<-data.frame(cov_cryp$country.R,cov_cryp$Date,cov_cryp$cases,
           cov_cryp$btc_high,cov_cryp$eth_high)
colnames(selec_data)[1]<-"country.R"
colnames(selec_data)[2]<-"Date"
colnames(selec_data)[3]<-"cases"
colnames(selec_data)[4]<-"btc_high"
colnames(selec_data)[5]<-"eth_high"
selec_data<-selec_data %>%
  dplyr::mutate(year = lubridate::year(Date), 
                month = lubridate::month(Date), 
                day = lubridate::day(Date))

##BTC high All time for January 2021 only
jcolors('default')

p<-crypt %>%
  filter(month=="1") %>%
  ggplot( aes(day, btc_high)) +
  geom_point() +
  theme_bw()+
  theme(panel.background = element_blank(),
        legend.key = element_rect(fill = "grey15"),
        panel.border = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        legend.position = "bottom")+
        labs(y= "BTC High Price", x = "Day")
grid.arrange(p + scale_color_jcolors(palette = "pal6"),
             p + scale_color_jcolors(palette = "pal6"), ncol = 2)
plotly::ggplotly(p)

##BTC high during Jan-April 2021
my_plot=ggplot(crypt, aes(day, btc_high, colour = month)) + 
  geom_point()
my_plot +
  ggtitle("BTC High 2021")+
  xlab("Day") + ylab("BTC High Price")+ theme(
    panel.background = element_blank(),
    plot.title = element_text(hjust = 0.5))


##ETH high during Jan-April 2021
my_plot=ggplot(crypt, aes(day, eth_high, colour = month)) + 
  geom_point()
my_plot +
  ggtitle("ETH High 2021")+
  xlab("Day") + ylab("ETH High Price")+ theme(
  panel.background = element_blank(),
               plot.title = element_text(hjust = 0.5))

### BTC and Ethereum relation with each other
btc_eth=ggplot(crypt, aes(btc_high, eth_high, colour = Date)) + 
  geom_point()
btc_eth +
  ggtitle("BTC/ETH High 2021")+
  xlab("BTC High Price") + ylab("ETH High Price")+ theme(
    panel.background = element_blank(),
    plot.title = element_text(hjust = 0.5))


### Time series approach to model crypto data
# To make the convertion data-frame / xts format
# Then you can create the xts format:
don=xts( x=crypt[,-1], order.by=as.POSIXct(crypt$Date))
# Chart
p <- dygraph(don)
library(htmlwidgets)
 saveWidget(p, file=paste0( getwd(),
"dygraph_crypt.html"))


#COVID and BTC/ETH impact on each other

colnames(selec_data)[4]<-"BTC High Price"
colnames(selec_data)[5]<-"ETH High Price"

selec_data %>%
  ggplot( aes(x=day, y=cases, group=`ETH High Price` , color=`ETH High Price`)) +
  geom_line() +
  scale_color_viridis(discrete = FALSE) +
  ggtitle("Ethereum All Time High vs COVID-19 Cases January-April 2021") +
  theme_ipsum() +
  ylab("COVID cases")

selec_data %>%
  ggplot( aes(x=day, y=cases, group=`BTC High Price` , color=`BTC High Price`)) +
  geom_line() +
  scale_color_viridis(discrete = FALSE) +
  ggtitle("Bitcoin All Time High vs COVID-19 Cases January-April 2021") +
  theme_ipsum() +
  ylab("COVID cases")


#Using dygraph
# 
# selec_data%>%ggplot(aes(month,cases))+geom_line(color="blue")+
#   geom_line(aes(month,btc_high),color="black")+
#   geom_line(aes(month,eth_high),color="green")+xlab("Date")+
#   ylab("Rate-COVID and BTC/ETH high prices")+
#   title("COVID and Cryptocurrency 2021 - Month wise")


dy_total=xts( x=cov[,-1], 
              order.by=as.POSIXct(cov$Date))

dygraph(dy_total)
  

dy_total=xts( x=selec_data[,-1], 
              order.by=as.POSIXct(selec_data$Date))
dygraph(dy_total)

###Doing Linear Regression
reg<-lm(selec_data$cases~selec_data$btc_high+
          selec_data$eth_high+selec_data$day,
        data=selec_data)

summary(reg)
layout(matrix(c(1,2,3,4),2,2))

plot(reg)

# Chart
my_graph <- dygraph(dy_total)
library(htmlwidgets)
#graph of all values
saveWidget(my_graph, file=paste0( getwd(),
                           "dygraph_crypt.html"))
#Plot using ggplot
ggplot(selec_data, aes(cases, btc_high, colour = cases)) + 
  geom_point()+
  ggtitle("COVID CASES vs BTC/ETH High 2021")+
  xlab("Date") + ylab("ETH High Price")+ theme(
    panel.background = element_blank(),
    plot.title = element_text(hjust = 0.5))


