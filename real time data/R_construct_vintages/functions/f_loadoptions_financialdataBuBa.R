
f_loadoptions_financialdataBuBa <- function() {
  temp <- data.frame(
    names = c(
      "Umlaufrendite: insgesamt",
      "Zinsstrukturkurve: 0.5 Jahre RLZ",
      "Zinsstrukturkurve: 1 Jahre RLZ",
      "Zinsstrukturkurve: 10 Jahre RLZ",
      "DAX",
      "NEER (EWK 42)"
      ), 
    groups = c(rep("financial", 6)),
    trafo = c(2, 2, 2, 2, 3, 3),
    type = c(rep("m", 6)),
    flag_usestartvals = c(rep(1, 6)),
    flag_sa = c(rep(0, 6)),
    seriesnames = c(
      "BBSIS.M.I.UMR.RD.EUR.A.B.A.A.R.A.A._Z._Z.A",
      "BBSIS.M.I.ZST.ZI.EUR.S1311.B.A604.R005X.R.A.A._Z._Z.A",
      "BBSIS.M.I.ZST.ZI.EUR.S1311.B.A604.R01XX.R.A.A._Z._Z.A",
      "BBSIS.M.I.ZST.ZI.EUR.S1311.B.A604.R10XX.R.A.A._Z._Z.A",
      "BBQFS.M.DE.CORP.PRICE_DAX._X.0000",
      "BBEE1.M.I9.AAA.XZE022.A.AABAN.M00" 
      )) 
  
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