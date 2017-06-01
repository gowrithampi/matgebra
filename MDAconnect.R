library(RODBC)
myServer <- "mdaanalytics.database.windows.net"
myUser <- "amaazure"
myPassword <- "ama@1234"
myDatabase <- "MDAAMA"
myDriver <- "SQL Server" 
connectionString <- paste0(
    "Driver=", myDriver, 
    ";Server=", myServer, 
    ";Database=", myDatabase, 
    ";Uid=", myUser, 
    ";Pwd=", myPassword)
sqlQuery <- "select * from dbo.ereignis"
conn <- odbcDriverConnect(connectionString)
df <- sqlQuery(conn, sqlQuery)




#### Analysis of S204 #### 

#Select Machine S204
S204<-df[df$MASCH_NR=='S-204',]
#Percentage of Scrap Produced
pct<-S204$ZAEHLER3/S204$ZAEHLER2
ts<-auto.arima(pct[1:20])
#forecast the next session
fore<-forecast(ts,10)
x<-seq(1,30,1)
NAS<-rep(NA,20)
comp<-as.data.frame(cbind(x,c(NAS,fore$mean),pct[1:30])); 
colnames(comp)<-c("x","predicted","actuals")
S204_plot<-ggplot(comp, aes(x)) +                    # basic graphical object
  geom_line(aes(y=predicted), colour="red") +  # first layer
  geom_line(aes(y=actuals), colour="green") + 
  geom_line(aes(y=c(rep(NA,20),actuals[21:30])), colour="blue")

#### Analysis of S236 #### 
pct<-S204$ZAEHLER3/S204$ZAEHLER2
