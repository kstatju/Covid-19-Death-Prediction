library(tidyr)
library(stringr)
path = "C:/Users/kanak/Downloads/"
dt = read.csv(file = paste(path, "selected indicator_wdi_sub_all.csv", sep = ""))
dt1 = dt[,-c(2, 3,4,6:11)]

dat_wide <- function(dt, columns){
  nam = names(dt[,columns])
  dt2 <- reshape(data = dt[,columns], timevar =  "Indicator.Code", 
                 idvar =  c("Country.Code"),
                 direction = "wide")
  nm = names(dt2)
  names(dt2) = str_replace(nm, paste(nam[length(nam)],'.', sep = ""), "")
  return(dt2)
}

impvar = c("Country.Code","NY.GDP.PCAP.CD",
           "NY.GNP.PCAP.CD", "SH.IMM.IDPT", "SH.IMM.HEPB",
           "SH.IMM.MEAS", "SH.TBS.INCD", "ST.INT.ARVL",
           "AG.LND.TOTL.K2", "TX.VAL.MRCH.WL.CD", "TX.VAL.MRCH.HI.ZS",
           "TM.VAL.MRCH.HI.ZS", "TG.VAL.TOTL.GD.ZS", 
           "NY.GSR.NFCY.CD", "SP.POP.0014.TO.ZS", "SP.POP.1564.TO.ZS",
           "SP.POP.65UP.TO.ZS", "EN.POP.DNST", "SP.POP.TOTL",
           "SP.RUR.TOTL.ZS", "BG.GSR.NFSV.GD.ZS", "SH.TBS.DTEC.ZS",
           "EG.ELC.ACCS.ZS",
           "SP.DYN.LE00.IN", "SM.POP.NETM", "SH.TBS.CURE.ZS",
           "SH.UHC.SRVS.CV.XD",
           "SH.DTH.COMM.ZS",
           "SH.XPD.CHEX.PC.CD", "SH.XPD.GHED.PC.CD", 
           "SH.XPD.PVTD.PC.CD", "SM.POP.TOTL.ZS")

dt2018 = dat_wide(dt = dt1, columns = c(1,2,21))[,impvar]
dt2017 = dat_wide(dt = dt1, columns = c(1,2,20))[,impvar]
dt2016 = dat_wide(dt = dt1, columns = c(1,2,19))[,impvar]
dt2015 = dat_wide(dt = dt1, columns = c(1,2,18))[,impvar]
dt2014 = dat_wide(dt = dt1, columns = c(1,2,17))[,impvar]
dt2013 = dat_wide(dt = dt1, columns = c(1,2,16))[,impvar]
dt2012 = dat_wide(dt = dt1, columns = c(1,2,15))[,impvar]
dt2011 = dat_wide(dt = dt1, columns = c(1,2,14))[,impvar]

final_dt = dt2018

# preplace missing by previous value
nam22 = names(final_dt)[-c(1)]

for (i in c(1:nrow(final_dt))){
  for (j in nam22){
    if (is.na(final_dt[i,j])){
      final_dt[i,j] = ifelse(!is.na(dt2017[which((final_dt[i,1]==dt2017[,1])),j]),
                             dt2017[which((final_dt[i,1]==dt2017[,1])),j],
                             ifelse(!is.na(dt2016[which((final_dt[i,1]==dt2016[,1])),j]),
                                    dt2016[which((final_dt[i,1]==dt2016[,1])),j],
                                    ifelse(!is.na(dt2015[which((final_dt[i,1]==dt2015[,1])),j]),
                                           dt2015[which((final_dt[i,1]==dt2015[,1])),j],
                                           ifelse(!is.na(dt2014[which((final_dt[i,1]==dt2014[,1])),j]),
                                                  dt2014[which((final_dt[i,1]==dt2014[,1])),j],
                                                  ifelse(!is.na(dt2013[which((final_dt[i,1]==dt2013[,1])),j]),
                                                         dt2013[which((final_dt[i,1]==dt2013[,1])),j],
                                                         ifelse(!is.na(dt2012[which((final_dt[i,1]==dt2012[,1])),j]),
                                                                dt2012[which((final_dt[i,1]==dt2012[,1])),j],
                                                                ifelse(!is.na(dt2011[which((final_dt[i,1]==dt2011[,1])),j]),
                                                                       dt2011[which((final_dt[i,1]==dt2011[,1])),j], NA)))))))
    }
  }
}



# Replace missing by minimun value
fac_var = names(final_dt)[-1]

for (i in c(1:length(fac_var))){
  final_dt[is.na(final_dt[,fac_var[i]]),fac_var[i]] = min(final_dt[,fac_var[i]], na.rm = TRUE)
}

colSums(is.na(final_dt))

write.csv(final_dt, paste(path, "final_dt.csv", sep = ""), row.names = FALSE)




################################################
