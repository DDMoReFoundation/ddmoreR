##################################################
# R functions linked to the WINBUGS connector
##################################################

#' @title prepareWinbugsDS
#' 
#' This function implements the required preliminary steps before running a model in WinBugs. 
#' In particular, the data set is automatically transformed from a CSV NONMEM format into the WinBUGS format through the function NMTRAN2BUGSdataconverter
#' Moreover, an additional file called pascal.properties is produced in order to provide the required information to the WinBUGS converter   
#'
#' @param updated submission object
#' @param fisServer FISServer instance.
#'
#' @return updated submission object
#' 
#'
#' @author Paolo Magni
#' @export


prepareWinbugsDs <- function(submission, fisServer=DDMORE.getServer(), ...) {
	model.pharmml <- as.PharmML(submission$parameters$modelFile)
	NMTRAN2BUGSdataconverter(model=model.pharmml)
	submission$parameters$extraInputFiles<-c(submission$parameters$extraInputFiles, c("data_BUGS.txt","pascal.properties"))
	if(is.null(submission$parameters$commandParameters)) {
		submission$parameters$commandParameters<-""
	}
	return(submission)
}


#' @title runWinBUGS
#' 
#' This function executes WinBUGS against specified MDL or PharmML encoded model file. 
#'
#' @param mdlfile MDL encoded model file 
#' @param subfolder of the current working directory where WinBUGS results will be stored, if not provided a WinBUGS_datetime directory will be used.
#' @param LoadCoda =FALSE (default) or TRUE if you want to preload in the R object the results of the analysis in term of CODA file instead of SO format.  
#' 
#' @return The results from executing the MDL file, in the form of an object of class \linkS4class{StandardOutputObject} or CODA objet.
#' 
#' @examples 
#' BUGS<-run.winbugs(mdlfile)
#' BUGSSO<-run.winbugs(mdlfile, subfolder="WINBUGSSO")
#' BUGSCoda<-run.winbugs(mdlfile, subfolder="WINBUGSCoda", LoadCoda=TRUE)
#'
#' @author Paolo Magni
#' @export


runWinBUGS <- function(mdlfile, subfolder=paste0("WinBUGS_",format(Sys.time(), "%Y%b%d%H%M%S")), LoadCoda=FALSE) {

postprocessWinbugsResSOCoda <- function( submission, fisServer=DDMORE.getServer(), ...) {
	tOutputIndex <- file.path(submission$parameters$importDirectory,"outputIndex.txt")
	tOutput <- file.path(submission$parameters$importDirectory,"output1.txt")
	tOutputSO <- file.path(submission$parameters$importDirectory,paste(c(basename(file_path_sans_ext(submission$parameters$modelFile)), ".SO.xml"), collapse=""))
	if (LoadCoda){
		submission$so <- read.coda(output.file=tOutput, index.file=tOutputIndex)
	}	
	else {
		submission$so <- LoadSOObject(tOutputSO)
	}	
	return(submission)
}


    ddmore:::execute(mdlfile, target="winbugs", importSO=FALSE,  preprocessSteps=list(prepareWinbugsDs), postprocessSteps=list(postprocessWinbugsResSOCoda), subfolder=subfolder)
}


######################################################################
# Script to convert NMTRAN dataset (CSV file) to BUGS format dataset
######################################################################

###### ver 17/7/16 ##################à 
# author Elisa Borella

NMTRAN2BUGSdataconverter <- function(model){	
	# Function to exclude values present in full_vector that are not present
	# in searched vector
	exclude_val<-function(full_vector,searched_vector){
		
		found=c()
		
		for(i in full_vector){  
			
			if(any(is.element(searched_vector,i))){
				searched_vector[(which(searched_vector==i))[1]]=NA
			}
			else{
				found=c(found,i)
			}
		}
		
		return(found)
	}
	
	# Load the libraries
	require(XML)
	require(plyr)
	require(tools)
	
	# Name of the PharmML file
	model <- model
	
	# Parse the XML file 
	doc <- xmlParse(model)
	
	# Convert the XML document into an R list
	xml_data <- xmlToList(doc)
	
	
	# Find the name of the datafile
	FILENAME <- xml_data$TrialDesign$ExternalDataSet$DataSet$ExternalFile$path
	
	# Extract the extension of the datafile
	EXT = file_ext(FILENAME)
	
	# Find all  the comment lines (starting with C in NMTRAN)
	conn=file(FILENAME,open="r")
	line=readLines(conn)
	iComments=grep("C", line, fixed = TRUE)
	close(conn)
	
	# Read only CSV file, for the other format give an error message
	if (EXT!="csv"){
		stop("Error: File format not supported. Only CSV files supported.")
	}else{
		
		# Read the datafile in a dataframe (keep the header, skip all the commented lines)
		dat <- read.csv(FILENAME, header = TRUE, skip=length(iComments)-1, na.strings=".")
	}  
	
	# Count the number of columns from XML
	dataPath<-xml_data$TrialDesign$ExternalDataSet$DataSet$Definition
	ncol <- length(dataPath) - length(xml_data$TrialDesign$ExternalDataSet$DataSet$Definition$IgnoreLine) #ignore the tag IgnoreLine
	
	# Store the column type, id, number and value type.
	colType <- vector()
	colId <- vector()
	colNumb <- vector()
	valueType <- vector()
	
	for (i in 1:ncol){
		#Save the NONMEM type of each column
		colType[i]<-dataPath[i]$Column[['columnType']] 
		#Save the user-defined name of each column 
		colId[i]<-dataPath[i]$Column[['columnId']]
		#Save the index of each column 
		colNumb[i]<-dataPath[i]$Column[['columnNum']]
		#Save the type of each column (int/real)
		valueType[i]<-dataPath[i]$Column[['valueType']]
	}
	
	# Change the user-defined name with the column Type 
	# except for undefined, covariate and regression types (keep the user name, otherwise multiple columns with same name)
	colnames(dat)<-colType
	
	# Treat regressor as covariate
	colType[colType=="reg"]="covariate" #regressor as covariate
	
	colnames(dat)[colType=="covariate"]<-colId[colType=="covariate"]
	colnames(dat)[colType=="undefined"]<-colId[colType=="undefined"]
	
	# Store the covariate names and their types (real/int)
	covnames<-colId[colType=="covariate"]
	covtype<-valueType[colType=="covariate"]
	
	########################## ID column ###############################################
	if(is.na(match('id',colType))) {
		warning("ID column is missing. Set to 1 by default.")
		dat$id=rep(1,nobs)
	} 
	
	########################## DV column ###############################################
	
	if(is.na(match('dv',colType))){ 
		warning("DV column is missing. Set to NA by default.")
		dat$dv=rep(NA,nobs)
	}
	
	########################## IDV column ###############################################
	
	if(is.na(match('idv',colType))){ 
		warning("IDV column is missing. Set to 0 by default.")
		dat$idv=rep(0, nobs)
	}
	
	########################## MDV column ###############################################
	# Fill thr DV column with NA when MDV=1
	
	if ('mdv' %in% colType){
		dat$dv[dat$mdv==1]=NA
	}
	
	# Store the names of the dataframe, the number of columns and the number of observations
	names <- colnames(dat)
	nobs <- nrow(dat)
	ncol <- ncol(dat)
	
	########################## DVID column ###############################################
	
	# Count the number of DV variables
	nDV <- length(xml_data$TrialDesign$ExternalDataSet$MultipleDVMapping$Piecewise)
	
	if ('dvid' %in% colType && nDV>0){
		
		# Count the number of unique IDs
		unique_id <- unique(dat$id)
		
		# Store the name of the DV
		dvId <- vector()
		
		# Store the number of the DV
		dvNumber <- vector()
		
		tot_dat <- list()
		
		for (i in 1:nDV){
			#dvId is the name of the i-th dependent variable
			dvId[i] <- xml_data$TrialDesign$ExternalDataSet$MultipleDVMapping$Piecewise[i]$Piece$SymbRef[['symbIdRef']]
			#dvNumber is the number used in the dvid column for the i-th dependent variable
			dvNumber[i] <- xml_data$TrialDesign$ExternalDataSet$MultipleDVMapping$Piecewise[i]$Piece$Condition$LogicBinop$Int
			#Create a list of dataframes, one for each dependent variable (called dat_dvId), with columns: id (subject id), dv and idv
			tot_dat[[paste('dat_',dvId[i], sep="")]] <- dat[dat$dvid==as.numeric(dvNumber[i]),]
		}
		
		# If a DVID is not used as DV set its value to 1 (for example for dose events)
		if (all(unique(dat$dvid)%in%dvNumber)==FALSE){
			notMatchedId=unique(dat$dvid)[!unique(dat$dvid)%in%dvNumber]
			dat$dvid[dat$dvid==notMatchedId]=1
		}
		
		# Find for each subject all the unique times for which the DVs are defined and store them in a list (tot_grid)
		N_subj <- length(unique(dat$id))
		tot_grid <- list()
		for (i in 1:N_subj){
			grid_subj <- vector()
			for (j in 1:nDV){
				grid_subj <- c(grid_subj, tot_dat[[j]]$idv[tot_dat[[j]]$id==unique_id[i]])
			}
			tot_grid[[i]] <- sort(unique(grid_subj))
		}
		
		# Find the maximum number of times for each subject (take the max among the different DVs) and store in a list n_time_subj
		n_time_subj <- list()
		for (i in 1:N_subj){
			maximum <- vector()
			for (k in 1:length(tot_grid[[i]])){
				n_time <- vector()
				for (j in 1:nDV){
					#Find how many times a time is present among all the DVs
					n_time <- c(n_time,length(which(tot_dat[[j]]$idv[tot_dat[[j]]$id==unique_id[i]]==tot_grid[[i]][k])))
				}
				#Take the maximum among the different DVs
				maximum[k] <- max(n_time)
				n_time_subj[[i]] <- maximum
			}
			#Repeat each time n_time_subj times
			tot_grid[[i]] <- rep(tot_grid[[i]],n_time_subj[[i]])
		}
		
		#Add to the dataframe for each DV (tot_dat) also the rows corresponding at the times where that DV is not defines but the 
		#others are (dataAdd) and store it in a new list (dataCollector)
		dataFinal<-list()
		number_rows<-matrix(0,nrow=nDV,ncol=N_subj)
		max_number_rows<-vector()
		for (j in 1:nDV){
			dataCollector<-list()
			for (i in 1:N_subj){
				# Compare the total number of samples for each subject to the samples for each subject for each DV
				missing_times<-exclude_val(tot_grid[[i]],tot_dat[[j]]$idv[tot_dat[[j]]$id==unique(dat$id)[i]])
				
				# If there are times used for a DV that are not used for another DV add rows with these missing times to the 
				# corresponding dataframes
				if(length(missing_times)!=0){
					dataAdd<-as.data.frame(matrix(data=NA,nrow=length(missing_times),ncol=ncol(tot_dat[[j]])))
					colnames(dataAdd)<-names
					
					#add the id, dvid, idv columns
					dataAdd$id <- unique_id[i]
					dataAdd$dvid <- as.numeric(dvNumber[j])
					dataAdd$idv <- missing_times
					
					dataFull <- rbind(tot_dat[[j]][tot_dat[[j]]$id==unique_id[i],],dataAdd)
					colnames(dataFull)=names
					
					arrange(dataFull,idv)
					dataFull <- dataFull[order(dataFull$idv),]
				}else{
					dataFull <- tot_dat[[j]][tot_dat[[j]]$id==unique_id[i],]
				}
				# Store the full dataset for each subject in a list
				dataCollector[[i]] <- dataFull
				# Count the number of rows of the full dataset
				number_rows[j,i] <-nrow(dataFull)
			}
			# Find the max number of rows for each subject 
			max_number_rows[j] <- max(number_rows)
			# Collect the full dataframes for each DV in a list (dataFinal)
			dataFinal[[j]] <- dataCollector
		}
		
		#Add to the dataframe for each DV (dataFinal) also the rows corresponding at the times where the DVs are not defined but 
		#for a certain subject but are defined for another subject (dataAdd2) and 
		#store it in a new list (dataFinal_df)
		dataFinal_df<-list()
		N_t<-apply(number_rows,2,max)
		#Find the maxminum number of rows for all the subjects
		max_max_number_rows<-max(max_number_rows)
		
		for(j in 1:nDV){
			for (i in 1:N_subj){
				#if not all the subject have the same number of rows add extra lines
				if((max_max_number_rows-number_rows[j,i])!=0){
					dataAdd2<-as.data.frame(matrix(data=0,nrow=max_max_number_rows-number_rows[j,i],ncol=ncol))
					colnames(dataAdd2)<-names
					
					#add the id column
					dataAdd2$id <- unique_id[i]
					
					#add the new rows
					dataFull2 <- rbind(dataFinal[[j]][[i]],dataAdd2)
					arrange(dataFull2,id)
					dataFull2 <- dataFull2[order(dataFull2$id),]
					
					dataFinal[[j]][[i]] <- dataFull2
				}
			}
			#transform the list of list in list of dataframes
			dataFinal_df[[j]] <- ldply(dataFinal[[j]],data.frame)
			
		}}else{#if there is only one DV
		nDV=1
		
		# Calculate number of subjects (N_subj)
		N_subj = length(unique(dat$id)) 
		
		# Calculate start vector with first indexes referred to each subject 
		start = (1:nobs)[!duplicated(dat$id)] 
		
		# Calculate end vector with last indexes referred to each subject
		end = c(start[-1]-1,nobs)
		
		# Calculate N_t vector with the number of observations for each subject
		
		N_t=end-start+1
		
		# Verify if idv and dv column are numeric, otherwise force to numeric (with introduction of NA)
		if (is.numeric(dat$idv)==FALSE){
			warning('The idv column is not numeric. NAs introduced by coercion.')
			dat$idv=as.numeric(dat$idv)
		}
		
		if (is.numeric(dat$dv)==FALSE){
			warning('The dv column is not numeric. NAs introduced by coercion.')
			dat$dv=as.numeric(dat$dv)
		}
		
		# Create a dataframe full of zeros beacuse there could be different measures for each subject
		dataFinal_df=list()
		dataFinal_df[[1]] = as.data.frame(matrix(0, N_subj*max(N_t), ncol))
		for (i in 1:N_subj){
			dataFinal_df[[1]][(1+(i-1)*max(N_t)):((i-1)*max(N_t)+N_t[i]),] = dat[start[i]:end[i],]
		}
		names(dataFinal_df[[1]])<-names(dat)
		
		#Save the times and the DV in two vectors (grid and y)
		grid <- dataFinal_df[[1]]$idv
		y <- dataFinal_df[[1]]$dv
		
	}
	
	########################## DOSE column ###############################################
	
	flag_dose <- FALSE
	
	if('dose' %in% names(dat)){
		dose <- dataFinal_df[[1]]$dose
		if(nDV>1){
			for (i in 2:nDV){
				#fill the missing values with the dose events of the other DVs
				dose[is.na(dose)] <- dataFinal_df[[i]]$dose[is.na(dose)]
			}
		}
		#all the values in the dose column must be not NA (set missing values to 0)
		dose[is.na(dose)] <- 0
	}else{
		warning('Column dose is missing. Default 0 values will be set.')
		flag_dose <- TRUE
		dose <- rep(0,nrow(dataFinal_df[[1]]))
	}
	
	########################## RATE column ###############################################
	
	if('rate' %in% colType) {
		rate <- dataFinal_df[[1]]$rate
		if(nDV>1){
			for (i in 2:(nDV)){
				#fill the missing values with the rates of the other DVs
				rate[is.na(rate)] <- dataFinal_df[[i]]$rate[is.na(rate)]
			}
		}
		if(flag_dose){
			warning('rate is specified without a dose. rate will be set to 0 by default.')
			rate <- rep(0,nrow(dataFinal_df[[1]]))
		}  
		rate[is.na(rate)]<-0
	}else{
		warning('rate column is missing. rate will be set to 0 by default.')
		rate <- rep(0,nrow(dataFinal_df[[1]]))
	}
	
	########################## EVID column ###############################################
	
#   if('evid' %in% names(dat)){
#     evid <- dataFinal_df[[1]]$evid
#     if(nDV>1){
#       for (i in 2:(nDV)){        
#         #fill the missing values with the evids of the other DVs
#         evid[is.na(evid)] <- dataFinal_df[[i]]$evid[is.na(evid)]
#       }
#     }
#     #all the values in the dose column must be not NA (set missing values to 0)
#     evid[is.na(evid)] <- 0
#     
#     # evid values of 3 and 4 are ignored (they are not supported by PKPDModelLibrary)
#     # evid = 3 defines a reset event.
#     # evid = 4 defines a reset and then dose event. 
#     if (length(which(evid==3))!=0) {
#       warning('evid value 3 is not supported. These rows will be ignored.')
#       evid[which(evid == 3)] <- NULL    
#     }
#     if (length(which(evid==4)!=0)) {
#       warning('evid value 4 is not supported. These rows will be ignored.')
#       evid[which(evid == 4)] <- NULL
#     }
#   }else{
#     #the evid column is missing but MDV and DOSE columns are present
#     if ('dose' %in% names(dat) & 'mdv' %in% names(dat)){
#           
#       if(nDV>1){
#         for (i in 2:nDV){
#           #set default values to 0
#           evid <- rep(0,nrow(dataFinal_df[[i]]))
#           #if dose>0 and mdv=1 set evid=1
#           evid[dose>0 & dataFinal_df[[i]]$mdv==1] <- 1
#           #if dose=0 and mdv=1 set evid=2
#           evid[dose==0 & dataFinal_df[[i]]$mdv==1] <- 2
#           #add the evid column to dataFinal_df
#           dataFinal_df[[i]]$evid <- evid
#         }
#       }
#       
#       #if there is only one DV
#       evid <- rep(0,nrow(dataFinal_df[[1]]))
#       evid[dataFinal_df[[1]]$dose>0 & dataFinal_df[[1]]$mdv==1] <-1
#       evid[dataFinal_df[[1]]$dose==0 & dataFinal_df[[1]]$mdv==1] <-2
#       dataFinal_df[[1]]$evid <- evid
#       
#       if(nDV>1){
#         for (i in 2:(nDV)){        
#           #fill the missing values with the evids of the other DVs
#           evid[is.na(evid)] <- dataFinal_df[[i]]$evid[is.na(evid)]
#         }
#       }
#       
#     }else{
#       warning('evid column is missing. Default values will be set.')
#       evid <- rep(0,nrow(dataFinal_df[[1]]))
#       evid[dose!=0] <- 1
#       dataFinal_df[[1]]$evid <- evid
#     }
#   }
	
# evid column
	if('evid' %in% names(dat)){
		evid=dataFinal_df[[1]]$evid
		if(nDV>1){
			for (i in 2:(nDV)){
				evid[is.na(evid)]=dataFinal_df[[i]]$evid[is.na(evid)]
			}
		}
		evid[is.na(evid)]=0
		# evid values of 3 and 4 are ignored (they are not supported by Pascal 2)
		# evid = 3 defines a reset event.
		# evid = 4 defines a reset and then dose event.
		
		if (length(which(evid==3))!=0) {
			warning('evid value 3 is not supported. These rows will be ignored.')
			evid[which(evid == 3)]=NULL    
		}
		if (length(which(evid==4)!=0)) {
			warning('evid value 4 is not supported. These rows will be ignored.')
			evid[which(evid == 4)]=NULL
		}
	}else{
		if ('dose' %in% names(dat) & 'mdv' %in% names(dat)){
			
			for (i in 1:nDV){
				evid=rep(0,nrow(dataFinal_df[[i]]))
				evid[dose>0 & dataFinal_df[[i]]$mdv==1]=1
				evid[dose==0 & dataFinal_df[[i]]$mdv==1]=2
				dataFinal_df[[i]]$evid=evid
			}
			
			evid[is.na(evid)]=0
			evid_dat=rep(0,nrow(dat))
			evid_dat[dat$dose>0 & dat$mdv==1]=1
			evid_dat[dat$dose==0 & dat$mdv==1]=2
			dat$evid=evid_dat
		}else{
			warning('evid column is missing. Default values will be set.')
			evid=rep(0,nrow(dataFinal_df[[1]]))
			evid[dose!=0]=1
			dataFinal_df[[1]]$evid=evid
			
			evid_dat=rep(0,nrow(dat))
			evid_dat[dat$dose!=0]=1
			dat$evid=evid_dat
		}
	}
	
	########################## II column ###############################################
	
	flag_ii <- FALSE
	if('ii'%in% colType) {
		ii <- dataFinal_df[[1]]$ii
		if(nDV>1){
			for (i in 2:(nDV)){
				#fill the missing values with the ii of the other DVs
				ii[is.na(ii)] <- dataFinal_df[[i]]$ii[is.na(ii)]
			}
		}
		if(flag_dose){
			warning('ii is specified without a dose. The specified ii will be ignored')
			ii <- rep(0,nrow(dataFinal_df[[1]]))
		}
	}else{
		flag_ii <- TRUE
		warning('ii column is missing. Default 0 values will be set.')
		ii <- rep(0,nrow(dataFinal_df[[1]]))
	}
	
	########################## CMT column ###############################################
	
	
	#Count the derivate variables and store their names
	count <- 0
	nameDer <- NULL
	for (i in 1:length(xml_data$ModelDefinition$StructuralModel)){
		if (length(xml_data$ModelDefinition$StructuralModel[i]$DerivativeVariable)!=0){
			count <- count+1
			nameDer <- c(nameDer,xml_data$ModelDefinition$StructuralModel[i]$DerivativeVariable$.attrs[["symbId"]])
		}
	}
	
	# If there aren't PK Macros and there are derivative variables
	if(is.null(xml_data$ModelDefinition$StructuralModel$PKmacros) && 'cmt' %in% colType && !is.null(nameDer)){
		cmt <- dataFinal_df[[1]]$cmt
		if(nDV>1){
			for (i in 2:(nDV)){
				#fill the missing values with the cmt of the other DVs
				cmt[which(dataFinal_df[[i]]$evid==1,TRUE)] <- dataFinal_df[[i]]$cmt[which(dataFinal_df[[i]]$evid==1,TRUE)]    
			}
		}
	}else if(is.null(xml_data$ModelDefinition$StructuralModel$PKmacros) && 'dose' %in% colType && !is.null(nameDer)){
		warning('cmt column is missing. cmt will be set to 1 by default.')
		cmt=rep(1,nrow(dataFinal_df[[1]])) 
	}else{
		warning('cmt column is missing. cmt will be set to 1 by default.')
		cmt=rep(1,nrow(dataFinal_df[[1]]))
	}
	
	
	# If there are PK Macros and adm column is present
	if(!is.null(xml_data$ModelDefinition$StructuralModel$PKmacros) && 'adm' %in% colType){
		cmt=dataFinal_df[[1]]$adm
		if(nDV>1){
			for (i in 2:(nDV)){
				#fill the missing values with the cmt of the other DVs
				cmt[which(dataFinal_df[[i]]$evid==1,TRUE)]=dataFinal_df[[i]]$adm[which(dataFinal_df[[i]]$evid==1,TRUE)]    
			}
		}
	}else if(!is.null(xml_data$ModelDefinition$StructuralModel$PKmacros) && 'dose' %in% colType){
		warning('cmt column is missing. cmt will be set to 1 by default.')
		cmt=rep(1,nrow(dataFinal_df[[1]]))
	}else{
		warning('cmt column is missing. cmt will be set to 1 by default.')
		cmt=rep(1,nrow(dataFinal_df[[1]]))
	}
	
	########################## ADDL column ###############################################
	if('addl' %in% colType) {
		addl <- dataFinal_df[[1]]$addl
		if(nDV>1){
			for (i in 2:(nDV)){
				#fill the missing values with the addl of the other DVs
				addl[is.na(addl)] <- dataFinal_df[[i]]$addl[is.na(addl)]
			}
		}
		if(flag_ii){
			warning('ii is not specified. addl will be set to 0 by default.')
			addl <- rep(0,nrow(dataFinal_df[[1]]))
			
		}
		if(flag_dose){
			warning('addl is specified without a dose. addl will be set to 0 by default.')
			addl <- rep(0,nrow(dataFinal_df[[1]]))
		}   
	}else{
		warning('addl column is missing. addl will be set to 0 by default.')
		addl <- rep(0,nrow(dataFinal_df[[1]]))
	}
	
	########################## SS column ###############################################
	if('ss' %in% colType) {
		ss <- dataFinal_df[[1]]$ss
		if(nDV>1){
			for (i in 2:(nDV)){
				#fill the missing values with the ss of the other DVs
				ss[is.na(ss)] <- dataFinal_df[[i]]$ss[is.na(ss)]
			}
		}
		if(flag_ii){
			warning('ii is not specified. ss will be set to 0 by default.')
			ss <- rep(0,nrow(dataFinal_df[[1]]))
		}
	}else{
		warning('ss column is missing. ss will be set to 0 by default.')
		ss <- rep(0,nrow(dataFinal_df[[1]]))
	}
	
	########################## OBSERVATION MODEL ###############################################
	#Count the observation variables and store their names
	len_om <- length(xml_data$ModelDefinition)
	nameOm <- vector()
	count <- 1
	for (i in 1:len_om){
		if(length(xml_data$ModelDefinition[i]$ObservationModel)!=0){
			nameOm[count] <- xml_data$ModelDefinition[i]$ObservationModel$ContinuousData$Standard$.attrs[["symbId"]]
			count <- count+1
		}
	}
	
	########################## WRITE THE BUGS DATA FILE ###############################################
	filename <- "data_BUGS.txt"
	#filename=paste(file_path_sans_ext(model),"_BUGS.txt", sep="")
	if (file.exists(filename)) file.remove(filename)
	
	str <- paste('# Data file WinBUGS automatically generated from', basename(model))
	
	str <- paste(str, paste('list(', 
					'N_subj = ', paste(as.character(N_subj), collapse=", "),
					', N_t = c(', paste(as.character(N_t), collapse=", "), ')', sep=""), sep="\n")
	
	if(nDV==1){
		str <- paste(str, paste(', grid = structure(.Data = c(', paste(as.character(as.vector(t(grid))), collapse=", "),
						'), .Dim = c(', as.character(N_subj), ',', as.character(max(N_t)), 
						')),', nameOm[1], ' = structure(.Data = c(', paste(as.character(as.vector(t(y))), collapse=", "),
						'), .Dim = c(', as.character(N_subj), ',', as.character(max(N_t)), '))', sep=""), sep="")
	}else{
		str <- paste(str, paste(', grid = structure(.Data = c(', paste(as.character(as.vector(t(dataFinal_df[[1]]$idv))), collapse=", "),
						'), .Dim = c(', as.character(N_subj), ',', as.character(max_max_number_rows), '))', sep=""), sep="")
		for (i in 1:nDV){
			str <- paste(str, paste(', ', dvId[i],' = structure(.Data = c(', paste(as.character(as.vector(t(dataFinal_df[[i]]$dv))), collapse=", "),
							'), .Dim = c(', as.character(N_subj), ',', as.character(max_max_number_rows), '))', sep=""), sep="")
		}
	}
	
	# If there are ODEs or PK Macros add the columns needed for PKPDModelLibrary
	if (!is.null(nameDer)||!is.null(xml_data$ModelDefinition$StructuralModel$PKmacros)){  
		str <- paste(str, paste(', rate = structure(.Data = c(', paste(as.character(rate), collapse=", "),
						'), .Dim = c(', as.character(N_subj), ',', as.character(max(N_t)),
						')), amt = structure(.Data = c(', paste(as.character(dose), collapse=", "),
						'), .Dim = c(', as.character(N_subj), ',', as.character(max(N_t)),
						')), ii = structure(.Data = c(', paste(as.character(ii), collapse=", "),
						'), .Dim = c(', as.character(N_subj), ',', as.character(max(N_t)),
						')), evid = structure(.Data = c(', paste(as.character(evid), collapse=", "),
						'), .Dim = c(', as.character(N_subj), ',', as.character(max(N_t)),
						')), cmt = structure(.Data = c(', paste(as.character(cmt), collapse=", "),
						'), .Dim = c(', as.character(N_subj), ',', as.character(max(N_t)),
						')), addl = structure(.Data = c(', paste(as.character(addl), collapse=", "),
						'), .Dim = c(', as.character(N_subj), ',', as.character(max(N_t)),
						')), ss = structure(.Data = c(', paste(as.character(ss), collapse=", "),
						'), .Dim = c(', as.character(N_subj), ',', as.character(max(N_t)),
						'))', sep=""), sep="")
	}
	
	ncovUsed=0
	filename2 <- "pascal.properties"
	#filename2=paste(file_path_sans_ext(model),".properties", sep="")
	
	if (file.exists(filename2)) file.remove(filename2)
	
	# Add the covariates to the datafile
	if ('dose' %in% colType & is.null(nameDer) & is.null(xml_data$ModelDefinition$StructuralModel$PKmacros)){
		N_t_cov <- matrix(0,1,N_subj)
		max_m <- rep(0,1)
		index_cov_used=which(names(dat)=='dose') 
		
		for (j in 1:(length(xml_data$TrialDesign$ExternalDataSet)-2)){
			if (!is.null(xml_data$TrialDesign$ExternalDataSet[j]$ColumnMapping$ColumnRef[['columnIdRef']])){
				if(xml_data$TrialDesign$ExternalDataSet[j]$ColumnMapping$ColumnRef[['columnIdRef']]==colId[index_cov_used]){
					doseName <- xml_data$TrialDesign$ExternalDataSet[j]$ColumnMapping$Piecewise$Piece$SymbRef[['symbIdRef']]
				}
			}
		}
		
		if(is.null(doseName)) doseName <- colId[index_cov_used]
		#if the dose column is missing but there are not derivative variables, use dose as covariate
		
		for (j in 1:N_subj){
			grid_cov_subj <- dat$idv[dat$id==unique(dat$id)[j]]
			N_t_cov[j] <- length(unique(grid_cov_subj[!is.na(dat[dat$id==unique(dat$id)[j],index_cov_used])]))        
		}
		max_m <- max(N_t_cov)
		
		# Create a dataframe full of zeros (different measures between subjects) with two columns (grid + cov)
		dataFinal_dfCov <- as.data.frame(matrix(0, N_subj*max_m, 2))
		for (j in 1:N_subj){
			dataFinal_dfCov[(1+(j-1)*max_m):((j-1)*max_m+N_t_cov[j]),1:2] <- cbind(dat$idv[which(dat$id==unique(dat$id)[j] & !is.na(dat[,index_cov_used]))],
					dat[which(dat$id==unique(dat$id)[j]&!is.na(dat[,index_cov_used])),index_cov_used])[!duplicated(dat$idv[which(dat$id==unique(dat$id)[j]&!is.na(dat[,index_cov_used]))]),]
		}
		
		str <- paste(str, paste(',', as.character(doseName), ' =  structure(.Data = c(', paste(as.character(as.vector(t(dataFinal_dfCov[,2]))), collapse=", "),
						'), .Dim = c(', as.character(N_subj), ',', as.character(max_m), 
						'))', sep=""), sep="")
	}
	
	# Find the number of used covariates
	if('covariate' %in% colType){
		for (j in 1:(length(xml_data$TrialDesign$ExternalDataSet)-2)){
			if (length(xml_data$TrialDesign$ExternalDataSet[j]$ColumnMapping$ColumnRef[['columnIdRef']])!=0){
				ncovUsed <- ncovUsed+xml_data$TrialDesign$ExternalDataSet[j]$ColumnMapping$ColumnRef[['columnIdRef']]%in%covnames
			}
		}
		
		#Find how many covariates are categorical and continuous
		index_cov_used = vector()
		for (i in 1:ncovUsed){
			# Find the index in dat corresponding to the covariate
			index_cov_used[i]=which(names(dat)==xml_data$ModelDefinition$CovariateModel[i]$Covariate$.attrs[['symbId']])  
		}
		NcovCAT <- sum(valueType[index_cov_used]%in%"int")
		NcovCONT <- ncovUsed - NcovCAT
		N_t_cov <- matrix(0,ncovUsed,N_subj)
		max_m <- rep(0,ncovUsed)
		
		#write the number of covariates only if there are ODEs or PKMacros
		if (!is.null(nameDer)||!is.null(xml_data$ModelDefinition$StructuralModel$PKmacros)){
			if("int"%in%covtype){
				str <- paste(str, paste(", n_cov_cat = ", as.character(NcovCAT),  sep=""), sep="")
				str <- paste(str, paste(", n_cov_cont = ", as.character(NcovCONT),  sep=""), sep="")
			}else{
				str <- paste(str, paste(", n_cov_cont = ", as.character(ncovUsed),  sep=""), sep="")
			}
		}
		
		for (i in 1:ncovUsed){
			# Find the index in dat corresponding to the covariate
			index_cov_used <- which(names(dat)==xml_data$ModelDefinition$CovariateModel[i]$Covariate$.attrs[['symbId']])
			for (j in 1:N_subj){
				grid_cov_subj <- dat$idv[dat$id==unique(dat$id)[j]]
				N_t_cov[i,j] <- length(unique(grid_cov_subj[!is.na(dat[dat$id==unique(dat$id)[j],index_cov_used])]))        
			}
			max_m[i] <- max(N_t_cov[i,])
			
			# Create a dataframe full of zeros (different measures between subjects) with two columns (grid + cov)
			dataFinal_dfCov <- as.data.frame(matrix(0, N_subj*max_m[i], 2))
			for (j in 1:N_subj){
				dataFinal_dfCov[(1+(j-1)*max_m[i]):((j-1)*max_m[i]+N_t_cov[i,j]),1:2] <- cbind(dat$idv[which(dat$id==unique(dat$id)[j] & !is.na(dat[,index_cov_used]))],
						dat[which(dat$id==unique(dat$id)[j]&!is.na(dat[,index_cov_used])),index_cov_used])[!duplicated(dat$idv[which(dat$id==unique(dat$id)[j]&!is.na(dat[,index_cov_used]))]),]
			}
			
			if (!is.null(nameDer)||!is.null(xml_data$ModelDefinition$StructuralModel$PKmacros)){
				str <- paste(str, paste(', max_m_', as.character(names(dat)[index_cov_used]), ' = ', as.character(max_m[i]), sep=""), sep="")            
			}
			
			str <- paste(str, paste(', N_t_', as.character(names(dat)[index_cov_used]), ' = c(', paste(as.character(N_t_cov[i,]), collapse=", "), ")", sep=""), sep="")         
			str <- paste(str, paste(', grid_',  as.character(names(dat)[index_cov_used]), ' =  structure(.Data = c(', paste(as.character(as.vector(t(dataFinal_dfCov[,1]))), collapse=", "),
							'), .Dim = c(', as.character(N_subj), ',', as.character(max_m[i]), 
							'))', sep=""), sep="")
			
			if (i==ncovUsed){
				str <- paste(str, paste(', ', as.character(names(dat)[index_cov_used]), ' =  structure(.Data = c(', paste(as.character(as.vector(t(dataFinal_dfCov[,2]))), collapse=", "),
								'), .Dim = c(', as.character(N_subj), ',', as.character(max_m[i]), 
								'))', sep=""), sep="")  
				
			}else{
				str <- paste(str, paste(', ', as.character(names(dat)[index_cov_used]), ' =  structure(.Data = c(', paste(as.character(as.vector(t(dataFinal_dfCov[,2]))), collapse=", "),
								'), .Dim = c(', as.character(N_subj), ',', as.character(max_m[i]), 
								'))', sep=""), sep="")  
				
			}
			
		}
		
		
		totCov=sum(2*max_m)
		write(paste("NUM1=",as.character(1+2*ncovUsed+totCov),sep=""),filename2,append=T)
	}else{
		totCov=0
		write("NUM1=0",filename2,append=T)
		
	}
	str <- paste(str, ')', sep="")
	write(str, filename)
	write(paste("NUM2=",as.character(max(N_t)+100),sep=""),filename2,append=T) 
}

#Two files must be returned: 
#data_BUGS.txt
#pascal.properties
