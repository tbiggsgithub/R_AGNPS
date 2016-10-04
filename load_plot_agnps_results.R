#  Read in AGNPS model runs and summarize event data
#  Requires 2 files:
  #  "AnnAGNPS_TBL_Gaging_Station_Data_Evt.txt"
  #  "event_ranges.txt", which defines the beginning and end dates of events to be summarized

indir.base = "G:/large_datasets/tijuana/los_laureles_canyon/AnnAGNPS_2014_06/"  # Directory that has AGNPS model directories
indir.model = "cc_LLC_revpar_llcrain04-16_reachesKris_1yr"  # Directory of the model name
setwd(paste0(indir.base,indir.model,"/original/"))  # Puts you in the directory of the model to run

# Read in output files for the outlet

fname = "AnnAGNPS_TBL_Gaging_Station_Data_Evt.txt"  # Output file to read in

#  Read in and format the file to read in as a table
x = readLines(fname)
x2 = x[29:(length(x)-2)] # get rid of header
write(x2,"x2foo.txt")  # write a temporary file
xin = read.table("x2foo.txt")

cols = c(1,5,10,11,43)
cnames = c("Date","Rain.mm","Qpk.cms","Qtot.Mg","SS.all.Mg")

xout = xin[,cols]
names(xout)= cnames
dates = as.Date(xout$Date,format="%m/%d/%Y")

events = read.table(paste0(indir.base,"event_ranges.txt"),header=TRUE)
events.start = as.Date(events[,1],format="%m/%d/%Y")
events.end = as.Date(events[,2],format="%m/%d/%Y")

outevent = data.frame(EventNo=NA,Start.Date=NA,Rain=NA,Qmm=NA,Qpk.cms=NA,SStons=NA)

# Summarize runoff, Qpk and sediment load by event.
for (i in 1:length(events.start)){
  sub = xout[(dates>=events.start[i]) & (dates<=events.end[i]),]
  Qtot = sum(sub$Qtot.Mg)  # runoff in Mg = m3
  Qtot.mm = Qtot/(11.61*1000)  #  m3/km2 x 1km2/1E6m2 x 1000mm/1m
  Qpk.cms = max(sub$Qpk.cms)
  SStot = sum(sub$SS.all.Mg)
  Rain = sum(sub$Rain.mm)
  outevent[i,] = c(i,events.start[i],Rain,Qtot.mm,Qpk.cms,SStot)
}

#  Plot rainfall vs runoff for all events.

#  Calculate annual total sediment load for all runs, all parameters, and calculate
#  Perturbation = 