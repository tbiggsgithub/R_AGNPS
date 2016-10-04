# Run AGNPS.exe from R, process data

#  Note: there cannot be any blank values in columns with numerical values, including parameters that will not be altered.
#  Blanks come up as NA values in numerical columns, which appears to override the na="" option in write.table.


indir.base = "G:/large_datasets/tijuana/los_laureles_canyon/AnnAGNPS_2014_06/"  # Directory that has AGNPS model directories
indir.model = "cc_LLC_revpar_llcrain04-16_reachesKris_1yr"  # Directory of the model name
wshed.area = 11.61  # Wshed area in km2
options(scipen=999)

setwd(paste0(indir.base,indir.model))  # Puts you in the directory of the model to run

input.files.wshed  = c("celldata.csv","egully.csv","rchdata.csv","rocurve.csv")

params.to.test = rbind(c("CN","general/rocurve.csv",0,100),  # 0,100 are the min and max possible values for the parameter
                      c("K_Factor","general/soildat.csv",0,1E10),
                      c("Saturated_Conductivity","general/soil_layers.csv",0,1E10),
                      c("Critical_Sheer_Stress","watershed/egully.csv",0,1E10),
                      c("Sheet_Flow_Mannings_n","watershed/celldata.csv",0.01,0.4),
                      c("Operation_Tillage_Depth","general/manoper.csv",0,2000))
#  C-factor (internal calculated)?  Erodibility (internally calculated--vary those internal parameters?)
  # Check erodibility against model prediction
  # Width function?  Default is Wells eq8

sfactor = 0.2  # Sensitivity factor

params.df = data.frame(Param=params.to.test[,1],dir.fname=as.character(params.to.test[,2]),min=as.numeric(params.to.test[,3]),max=as.numeric(params.to.test[,4]))
files.to.load = as.character(unique(params.df$dir.fname))

files.to.copy = c("annagnps_master.csv","AnnAGNPS_SIM_Ephemeral_Gully_Repair_Date.csv","AnnAGNPS.fil","AnnAGNPS.exe")

folders.to.copy = list.dirs(paste0(indir.base,indir.model,"/original/"),full.names=FALSE,recursive=FALSE)
folders.to.copy.fullpath = paste0(indir.base,indir.model,"/original/",folders.to.copy)
newruns.name = paste0("param",seq(1:length(params.df[,1])))  #  List of the folder names for new runs.

#  Create the directory for the run and copy all files needed for the AGNPS run into the new directory

#  LOOP STARTS HERE
#  Loops through all parameters in the list.
#  Creates new directories for each parameter, and both plus and minus 20% directories for each parameter. 
for (i in 6:length(params.df$Param)){
setwd(paste0(indir.base,indir.model))
param = as.character(params.df[i,"Param"])
file.to.load = as.character(params.df[i,"dir.fname"])

newrun.name = newruns.name[i]
dir.create(newrun.name)
setwd(newrun.name)

sfactor.name = sfactor*100
newdir.plus = paste0("plus",sfactor.name)
newdir.minus = paste0("minus",sfactor.name)

dir.create(newdir.plus)
dir.create(newdir.minus)

# Copy all base files from original model to the new model
file.copy(from=paste0(indir.base,indir.model,"/original/",files.to.copy),to=paste0(indir.base,indir.model,"/",newrun.name,"/",newdir.plus))
file.copy(from=paste0(indir.base,indir.model,"/original/",files.to.copy),to=paste0(indir.base,indir.model,"/",newrun.name,"/",newdir.minus))

#### Copy model folders and files to new folders
setwd(paste0(indir.base,indir.model,"/",newrun.name,"/",newdir.plus))
# Copy original model folders and files to the new model folder
for (k in 1:length(folders.to.copy)){
  dir.create(folders.to.copy[k])
  file.copy(from=list.files(folders.to.copy.fullpath[k],full.names=TRUE),to=folders.to.copy[k])
}
setwd(paste0(indir.base,indir.model,"/",newrun.name,"/",newdir.minus))
for (k in 1:length(folders.to.copy)){
  dir.create(folders.to.copy[k])
  file.copy(from=list.files(folders.to.copy.fullpath[k],full.names=TRUE),to=folders.to.copy[k])
}

x = read.csv(paste0(indir.base,indir.model,"/original/",as.character(params.df[i,"dir.fname"])),stringsAsFactors=FALSE)

#  R reads in "T" as "TRUE" and makes it a binary class.
  # Here, need to convert "TRUE" to "T".
x[,sapply(x,class) == "logical"] <-
  sapply(x[,sapply(x,class) == "logical"],
         function(i) substr(as.character(i),1,1))

file.colnames = names(x)
param.to.test = as.character(params.df[i,"Param"])  # Get list of parameters to test for the file.
index.cols.param = grep(param.to.test,file.colnames)

#  PLUS sfactor:
x.plus = x
x.plus[,index.cols.param] = x[,index.cols.param]*(1+sfactor)

#  If the new value is <min or >max for that parameter, set it to the min or max.
for (j in 1:length(index.cols.param)){
  x.plus[x.plus[,index.cols.param[j]]>params.df[i,"max"],index.cols.param[j]] = params.df[i,"max"]
}

#cols.with.data = which(!is.na(x.plus[1,]))
#x.plus[,cols.with.data] = format(x.plus[,cols.with.data],scientific=FALSE)
write.csv(x.plus,file=paste0(indir.base,indir.model,"/",newrun.name,"/",newdir.plus,"/",params.df$dir.fname[i]),row.names=FALSE,quote=FALSE,na="")

# MINUS sfactor
x.minus = x
x.minus[,index.cols.param] = x[,index.cols.param]*(1-sfactor)

#  If the new value is <min or >max for that parameter, set it to the min or max.
for (j in 1:length(index.cols.param)){
  x.minus[x.minus[,index.cols.param[j]]<params.df[i,"min"],index.cols.param[j]] = params.df[i,"min"]
}

# x.minus[,cols.with.data] = format(x.minus[,cols.with.data],scientific=FALSE)
write.csv(x.minus,file=paste0(indir.base,indir.model,"/",newrun.name,"/",newdir.minus,"/",params.df$dir.fname[i]),row.names=FALSE,quote=FALSE,na="")

##### RUN THE 2 MODELS FOR THE GIVEN PARAMETER
# Run the PLUS model
setwd(paste0(indir.base,indir.model,"/",newrun.name,"/",newdir.plus))
system("AnnAGNPS.exe")

+# Runs the MINUS model
setwd(paste0(indir.base,indir.model,"/",newrun.name,"/",newdir.minus))
system("AnnAGNPS.exe") 

#  TEST THE ORIGINAL RUN
#setwd(paste0(indir.base,indir.model,"/","original"))
#system("AnnAGNPS.exe")
}
