# Prework

Analizamos un Dataset de que contiene información de criptomonedas del periodo de 2013 - 2018.

Las criptomonedas que contiene son:

-Bitcoin
-Dash
-Lite
-Etherum
-Monero
-Nem
-Neo
-Numeraire
-Omisego
-Qtum
-Ripple
-Statis
-Waves

## Dataset

La base de datos se encuentra en: https://www.kaggle.com/sudalairajkumar/cryptocurrencypricehistory

Para efectos de manejar un solo archivo, juntamos las bases de datos con el siguiente código

```R
dash_price$CRIPTO<-("Dash")
ethereum_price$CRIPTO<-("Ethereum")
litecoin_price$CRIPTO<-("Lite")
bitcoin_price$CRIPTO<-("Bitcoin")
iota_price$CRIPTO<-("Iota")
monero_price$CRIPTO<-("Monero")
nem_price$CRIPTO<-("Nem")
neo_price$CRIPTO<-("Neo")
numeraire_price$CRIPTO<-("Numeraire")
omisego_price$CRIPTO<-("Omisego")
qtum_price$CRIPTO<-("Qtum")
ripple_price$CRIPTO<-("Ripple")
stratis_price$CRIPTO<-("Stratis")
waves_price$CRIPTO<-("Waves")
Datos<-rbind(dash_price,ethereum_price,litecoin_price,bitcoin_price,iota_price,
             monero_price,nem_price,numeraire_price,omisego_price,qtum_price,ripple_price,
             stratis_price)
write.csv(Datos,file="Base.csv")
```

Por último, configuramos el formato de la fecha con:

```R
base <- read.csv("bitcoin_price.csv")
base$Date <- as.Date(base$Date,format='%b %d, %Y')
```
