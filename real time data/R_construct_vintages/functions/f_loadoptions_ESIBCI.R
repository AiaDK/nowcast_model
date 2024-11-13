#

f_loadoptions_ESIBCI <- function() {
  temp <- data.frame(
    names = c(
      # INDUSTRY
      "Confidence Indicator",
      "Production trend observed in recent months",
      "Assessment of order-book levels",
      "Assessment of export order-book levels",
      "Assessment of stocks of finished products",
      "Production expectations for the months ahead",
      "Selling price expectations for the months ahead",
      "Employment expectations for the months ahead",
      "Development of (export) orders over past 3 months",
      "Current level of capacity utilization",
      # RETAIL
      "Confidence Indicator",
      "Business activity (sales) development over the past 3 months",
      "Volume of stock currently hold",
      "Orders expectations over the next 3 months",
      "Business activity expectations over the next 3 months",
      "Employment expectations over the next 3 months",
      "Prices expectations over the next 3 months",
      # CONSTRUCTION
      "Confidence Indicator",
      "Building activity development over the past 3 months",
      "Evolution of your current overall order books",
      "Employment expectations over the next 3 months",
      "Prices expectations over the next 3 months",
      # SERVICES
      "Confidence Indicator",
      "Business situation development over the past 3 months",
      "Evolution of the demand over the past 3 months",
      "Expectation of the demand over the next 3 months",
      ##"Evolution of the employment over the past 3 months",
      "Expectations of the employment over the next 3 months",
      "Expectations of the prices over the next 3 months",
      # CONSUMER
      "Confidence Indicator", 
      "Financial situation over last 12 months",
      "Financial situation over next 12 months",
      "General economic situation over last 12 months",
      "General economic situation over next 12 months",
      "Price trends over last 12 months",
      "Price trends over next 12 months",
      "Unemployment expectations over next 12 months",
      "Major purchases at present",
      "Major purchases over next 12 months",
      ##"Savings at present",
      "Savings over next 12 months",
      "Statement on financial situation of household"
    ),
    groups = c(
      rep("ESI: industry", 10),
      rep("ESI: retail", 7),
      rep("ESI: building", 5),
      rep("ESI: services", 6), #7
      rep("ESI: consumer", 12) #13
    ),
    trafo = c(2, 2, 2, 2, 2, 2, 2, 2, 2, 1,
              2, 2, 2, 2, 2, 2, 2,
              2, 2, 2, 2, 2,
              2, 2, 2, 2, 2, 2, #2,
              2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2), #2
    type = c("m",	"m",	"m",	"m",	"m",	"m",	"m",	"m",	"q:A",	"q:A",
             "m",	"m",	"m",	"m",	"m",	"m",	"m",
             "m",	"m",	"m",	"m",	"m",
             "m",	"m",	"m",	"m",	"m",	"m", #"m",
             "m",	"m",	"m",	"m",	"m",	"m",	"m",	"m",	"m",	"m",	"m",	"m"), #"m"
    flag_usestartvals = c(1, 1, 1, 1, 1, 1, 1, 1, 0, 0,
                          1, 1, 1, 1, 1, 1, 1,
                          1, 1, 1, 1, 1,
                          0, 0, 0, 0, 0, 0,#0
                          1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1), #1
    flag_sa = c(0, 0, 0, 0, 0, 1, 1, 0, 0, 0,
                0, 0, 0, 0, 0, 0, 1,
                1, 1, 1, 1, 1,
                0, 0, 0, 1, 1, 1, #0
                0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), #0
    seriesnames = c(
        # INDUSTRY
        "INDU.DE.TOT.COF.BS.M",
        "INDU.DE.TOT.1.BS.M",
        "INDU.DE.TOT.2.BS.M",
        "INDU.DE.TOT.3.BS.M",
        "INDU.DE.TOT.4.BS.M",
        "INDU.DE.TOT.5.BS.M",
        "INDU.DE.TOT.6.BS.M",
        "INDU.DE.TOT.7.BS.M",
        "INDU.DE.TOT.11.BS.Q",
        "INDU.DE.TOT.13.QPS.Q",
        # RETAIL
        "RETA.DE.TOT.COF.BS.M",
        "RETA.DE.TOT.1.BS.M",
        "RETA.DE.TOT.2.BS.M",
        "RETA.DE.TOT.3.BS.M",
        "RETA.DE.TOT.4.BS.M",
        "RETA.DE.TOT.5.BS.M",
        "RETA.DE.TOT.6.BS.M",
        # CONSTRUCTION
        "BUIL.DE.TOT.COF.BS.M",
        "BUIL.DE.TOT.1.BS.M",
        "BUIL.DE.TOT.3.BS.M",
        "BUIL.DE.TOT.4.BS.M",
        "BUIL.DE.TOT.5.BS.M",
        # SERVICES
        "SERV.DE.TOT.COF.BS.M",
        "SERV.DE.TOT.1.BS.M",
        "SERV.DE.TOT.2.BS.M",
        "SERV.DE.TOT.3.BS.M",
        ##"SERV.DE.TOT.4.BS.M", #!
        "SERV.DE.TOT.5.BS.M",
        "SERV.DE.TOT.6.BS.M",
        # CONSUMER
        "CONS.DE.TOT.COF.BS.M", 
        "CONS.DE.TOT.1.BS.M",
        "CONS.DE.TOT.2.BS.M",
        "CONS.DE.TOT.3.BS.M",
        "CONS.DE.TOT.4.BS.M",
        "CONS.DE.TOT.5.BS.M",
        "CONS.DE.TOT.6.BS.M",
        "CONS.DE.TOT.7.BS.M",
        "CONS.DE.TOT.8.BS.M",
        "CONS.DE.TOT.9.BS.M",
        ##"CONS.DE.TOT.10.BS.M",#!
        "CONS.DE.TOT.11.BS.M",
        "CONS.DE.TOT.12.BS.M"))
  
  data <- list(
    names = as.character(temp$names),
    groups = as.character(temp$groups),
    trafo = as.numeric(temp$trafo),
    type = as.character(temp$type),
    flag_usestartvals = as.numeric(temp$flag_usestartvals),
    flag_sa = as.numeric(temp$flag_sa),
    seriesnames = as.character(temp$seriesnames))
  
  return(data)
}


      
