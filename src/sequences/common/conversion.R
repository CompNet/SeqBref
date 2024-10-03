#############################################################################################
# Set of functions used to convert the mandate table into Traminer sequences.
#
# 11/2021 Vincent Labatut
#############################################################################################




#############################################################################################
# Returns the duration, in days, of the intersection between the periods, or zero if they are
# exactly consecutive (i.e. one starts exactly one day after the other ends), or NA if they do
# not intersect at all.
#
# start1: start date of the first period.
# end1: end date of the first period.
# start2: start date of the second period.
# end2: end date of the second period.
#
# returns: duration of the intersection, zero if they are consecutive, or NA if there is no overlap.
#############################################################################################
date.intersect.val <- function(start1, end1, start2, end2)
{	#cat(format(start1),"--",format(end1)," vs ",format(start2),"--",format(end2),"\n",sep="")
	
	if(is.na(start1))
		start1 <- min(c(start1,end1,start2,end2), na.rm=TRUE)
	if(is.na(start2))
		start2 <- min(c(start1,end1,start2,end2), na.rm=TRUE)
	if(is.na(end1))
		end1 <- max(c(start1,end1,start2,end2), na.rm=TRUE)
	if(is.na(end2))
		end2 <- max(c(start1,end1,start2,end2), na.rm=TRUE)
	
	result <- min(end1,end2) - max(start1,start2) + 1
	if(result<0)
		result <- NA
	
	return(result)
}




#############################################################################################
# Merges the two specified periods, assuming they intersect. The order in which the periods are
# specified is not important.
#
# start1: start date of the first period.
# end1: end date of the first period.
# start2: start date of the second period.
# end2: end date of the second period.
#
# returns: period resulting fromt he merging, as a vector of two dates.
#############################################################################################
merge.overlapping.periods <- function(start1, end1, start2, end2)
{	result <- c(min(start1, start2), max(end1, end2))
	return(result)
}




#############################################################################################
# Splits two overlapping periods, so that the overlap consitutes a new period, that occurrs
# in addition to both others. Depending on the situation (see the function code), the result
# may involve 1 (complete overlap), 2 (original periods starting/ending on the same day) or 
# 3 (other cases) periods.
#
# start1: start date of the first period.
# end1: end date of the first period.
# start2: start date of the second period.
# end2: end date of the second period.
#
# returns: the three periods as a vector of dates. If a period disapears, its dates become NAs. 
#############################################################################################
split.overlapping.periods <- function(start1, end1, start2, end2)
{	#cat(format(start1),"--",format(end1)," vs ",format(start2),"--",format(end2),"\n",sep="")
	
	if(is.na(start1))
		start1 <- min(c(start1,end1,start2,end2), na.rm=TRUE)
	if(is.na(start2))
		start2 <- min(c(start1,end1,start2,end2), na.rm=TRUE)
	if(is.na(end1))
		end1 <- max(c(start1,end1,start2,end2), na.rm=TRUE)
	if(is.na(end2))
		end2 <- max(c(start1,end1,start2,end2), na.rm=TRUE)
	
	start1b <- end1b <- NA
	start2b <- end2b <- NA
	start3 <- end3 <- NA
	
	# handle each one of the many possible cases
	if(start1<start2)
	{	if(end1<end2)
		{	# 11111111111
			#         2222222222
			#-------------------
			# 11111111
			#            2222222
			#         333
			start3 <- start2
			end3 <- end1
			end1 <- start3 - 1
			start2 <- end3 + 1
			# test
			#split.overlapping.periods(as.Date("2022/01/01"), as.Date("2022/01/11"), as.Date("2022/01/09"), as.Date("2022/01/18"))
		}
		else if(end1==end2)
		{	# 111111111111111111
			#            2222222
			#-------------------
			# 11111111111
			#            3333333
			start3 <- start2
			end3 <- end2
			end1 <- start3 - 1
			start2 <- NA
			end2 <- NA
			# test
			#split.overlapping.periods(as.Date("2022/01/01"), as.Date("2022/01/18"), as.Date("2022/01/12"), as.Date("2022/01/18"))
		}
		else if(end1>end2)
		{	# 111111111111111111
			#       2222222
			#-------------------
			# 111111       11111
			#       3333333
			start3 <- start2
			end3 <- end2
			start1b <- end3 + 1
			end1b <- end1
			end1 <- start3 - 1
			start2 <- NA
			end2 <- NA
			# test
			#split.overlapping.periods(as.Date("2022/01/01"), as.Date("2022/01/18"), as.Date("2022/01/07"), as.Date("2022/01/13"))
		}
	}
	else if(start1==start2)
	{	if(end1<end2)
		{	# 11111111111
			# 222222222222222222
			#-------------------
			# 33333333333
			#            2222222
			start3 <- start1
			end3 <- end1
			start1 <- NA
			end1 <- NA
			start2 <- end3 + 1
			# test
			#split.overlapping.periods(as.Date("2022/01/01"), as.Date("2022/01/11"), as.Date("2022/01/01"), as.Date("2022/01/18"))
		}
		else if(end1==end2)
		{	# 111111111111111111
			# 222222222222222222
			#-------------------
			# 333333333333333333
			start3 <- start1
			end3 <- end1
			start1 <- NA
			end1 <- NA
			start2 <- NA
			end2 <- NA
			# test
			#split.overlapping.periods(as.Date("2022/01/01"), as.Date("2022/01/18"), as.Date("2022/01/01"), as.Date("2022/01/18"))
		}
		else if(end1>end2)
		{	# 111111111111111111
			# 22222222222
			#-------------------
			#            1111111
			# 33333333333
			start3 <- start1
			end3 <- end2
			start1 <- end3 + 1
			start2 <- NA
			end2 <- NA
			# test
			#split.overlapping.periods(as.Date("2022/01/01"), as.Date("2022/01/18"), as.Date("2022/01/01"), as.Date("2022/01/11"))
		}
	}
	else if(start1>start2)
	{	if(end1<end2)
		{	#         111
			# 222222222222222222
			#-------------------
			# 22222222   2222222
			#         333
			start3 <- start1
			end3 <- end1
			start1 <- NA
			end1 <- NA
			start2b <- end3 + 1
			end2b <- end2
			end2 <- start3 - 1
			# test
			#split.overlapping.periods(as.Date("2022/01/09"), as.Date("2022/01/11"), as.Date("2022/01/01"), as.Date("2022/01/18"))
		}
		else if(end1==end2)
		{	#            1111111
			# 222222222222222222
			#-------------------
			# 22222222222
			#            3333333
			start3 <- start1
			end3 <- end1
			start1 <- NA
			end1 <- NA
			end2 <- start3 - 1
			# test
			#split.overlapping.periods(as.Date("2022/01/12"), as.Date("2022/01/18"), as.Date("2022/01/01"), as.Date("2022/01/18"))
		}
		else if(end1>end2)
		{	#       111111111111
			# 2222222222222
			#-------------------
			#              11111
			# 222222
			#       3333333
			start3 <- start1
			end3 <- end2
			start1 <- end3 + 1
			end2 <- start3 - 1
			# test
			#split.overlapping.periods(as.Date("2022/01/07"), as.Date("2022/01/18"), as.Date("2022/01/01"), as.Date("2022/01/13"))
		}
	}
	
	result <- list()
	if(!is.na(start1)) 
		result[["1"]]  <- c(start1,  end1)
	if(!is.na(start1b)) 
		result[["1b"]] <- c(start1b, end1b)
	if(!is.na(start2)) 
		result[["2"]]  <- c(start2,  end2)
	if(!is.na(start2b)) 
		result[["2b"]] <- c(start2b, end2b)
	if(!is.na(start3)) 
		result[["3"]]  <- c(start3,  end3)
	
	return(result)
}




#############################################################################################
# Converts the mandate table into a Traminer-compatible data structure.
# 
# tab.mandates: mandate table.
# start.date: beginning of the period to process.
# end.date: end of the period to process.
# left: left alignment of the sequences ("DEL" = all aligned on their first state, NA = keep 
#	    the sequences as they are).
# colors: colors used when plotting the sequences.
#
# returns: a Traminer object.
#############################################################################################
convert.to.sequences <- function(tab.mandates, start.date=as.Date("2000/1/1"), end.date=Sys.Date(), left=NA, colors=CAT_COLORS_8)
{	cat("Converting data to sequences \n")
	
	COL_ID <- "Id"
	COL_MDT_START <- "DateDebutMandat"
	COL_MDT_END <- "DateFinMandat"
	COL_MDT_TYPE <- "TypeMandat"
	COL_SEQ <- "Sequence"
	
	# temporal resolution
	granularity <- "year"
	dates <- seq(start.date, end.date, granularity)
	semi.duration <- as.integer((dates[2]-dates[1])/2)
	empty.seq <- paste(rep(NA,length(dates)-1), collapse="-")
	
	# note on the granularity :
	# can be an integer = number of days
	# or a string among "day", "week", "month", "quarter", "year"
	# a number can be placed in the string, e.g. "2 months"
	
	# short versions of the mandate names
	short.names <- c()
	short.names["CONSEILLER DEPARTEMENTAL"] <- "CD"	# local
	short.names["CONSEILLER MUNICIPAL"] <- "CM"		# local
	short.names["CONSEILLER REGIONAL"] <- "CR"		# local
	short.names["DEPUTE"] <- "D"					# national
	short.names["REPRESENTANT EUROPEEN"] <- "RE"	# national
	short.names["SENATEUR"] <- "S"					# national
	short.names["CUMUL LOCAL"] <- "CuL"				# CM et (CD ou CR)
	short.names["CUMUL VERTICAL"] <- "CuV"			# local et national
	short.names["CUMUL RENFORCE"] <- "CuR"			# CM et local et national
	
	# at first, let us just use the mandate names
	mdt.order <- c("CD", "CM", "CR", "CuL", "D", "S", "RE", "CuV", "CuR")
	unique.ids <- sort(unique(tab.mandates[,COL_ID]))
	cat("  Processing each id separately \n")
	seqs <- t(sapply(1:length(unique.ids), function(r)
	{	id <- unique.ids[r]
		cat("    Processing id ",id, " (",r,"/",length(unique.ids),")\n",sep="")
		
		# retrieve the dates for the current id
		rows <- which(tab.mandates[,COL_ID]==id)
		tmp <- tab.mandates[rows,c(COL_MDT_TYPE,COL_MDT_START,COL_MDT_END)]
		#print(tmp)
		
		# convert mandate names to short form
		tmp[,COL_MDT_TYPE] <- short.names[tmp[,COL_MDT_TYPE]]
		
		# complete missing end dates
		no.end.date <- which(is.na(tmp[,COL_MDT_END]))
		tmp[no.end.date,COL_MDT_END] <- rep(end.date, length(no.end.date))
		
		# order by mandate dates and types
		mnd.types <- match(tmp[,COL_MDT_TYPE], mdt.order)
		idx <- order(tmp[,COL_MDT_START], mnd.types)
		mnd.types <- mnd.types[idx]
		tmp <- tmp[idx,]
		
		# test
		#tmp[3,COL_MDT_END] <- as.Date("2017-06-20")
		#tmp[5,COL_MDT_END] <- as.Date("2022-06-18")
#		print(tmp)
		
		# merge consecutive or overlapping mandates of same type
#		cat("......Merging consecutive or overlapping mandates of same type\n")
		if(nrow(tmp)>1)
		{	m.types <- sort(unique(tmp[,COL_MDT_TYPE]))
			for(m.type in m.types)
			{	ms <- which(tmp[,COL_MDT_TYPE]==m.type)
				if(length(ms)>1)
				{	i <- 1
					while(i<length(ms))
					{	
#						cat("........Processing row ",i,"/",length(ms),": ",tmp[ms[i],COL_MDT_TYPE],", ",format(tmp[ms[i],COL_MDT_START]),", ",format(tmp[ms[i],COL_MDT_END]),"\n",sep="")
						j <- i + 1
						while(j<=length(ms))
						{	
#							cat("..........Comparing with row ",j,"/",length(ms),": ",tmp[ms[j],COL_MDT_TYPE],", ",format(tmp[ms[j],COL_MDT_START]),", ",format(tmp[ms[j],COL_MDT_END]),"\n",sep="")
							overlap <- date.intersect.val(tmp[ms[i],COL_MDT_START], tmp[ms[i],COL_MDT_END], tmp[ms[j],COL_MDT_START], tmp[ms[j],COL_MDT_END])
							if(!is.na(overlap))
							{	dd <- merge.overlapping.periods(tmp[ms[i],COL_MDT_START], tmp[ms[i],COL_MDT_END], tmp[ms[j],COL_MDT_START], tmp[ms[j],COL_MDT_END])
								tmp[ms[i],COL_MDT_START] <- dd[1]
								tmp[ms[i],COL_MDT_END] <- dd[2]
								tmp <- tmp[-ms[j],]
								ms[j:length(ms)] <- ms[j:length(ms)] - 1
								ms <- ms[-j]
							}
							else
								j <- j + 1
						}
						i <- i + 1
					}
				}
			}
		}
#		print(tmp)
		
		# split periods where several mandate types overlap
#		cat("......Splitting periods where several mandates (of different types) overlap\n")
		offices <- list()
		for(i in 1:nrow(tmp))
			offices[[i]] <- tmp[i,COL_MDT_TYPE]
		if(nrow(tmp)>1)
		{	i <- 1
			while(i<nrow(tmp))
			{	
#				cat("........Processing row ",i,"/",nrow(tmp),": ",tmp[i,COL_MDT_TYPE],", ",format(tmp[i,COL_MDT_START]),", ",format(tmp[i,COL_MDT_END]),"\n",sep="")
				j <- i + 1
				goOn <- TRUE
				while(j<=nrow(tmp) && goOn)
				{	
#					cat("..........Comparing with row ",j,"/",nrow(tmp),": ",tmp[j,COL_MDT_TYPE],", ",format(tmp[j,COL_MDT_START]),", ",format(tmp[j,COL_MDT_END]),"\n",sep="")
					overlap <- date.intersect.val(tmp[i,COL_MDT_START], tmp[i,COL_MDT_END], tmp[j,COL_MDT_START], tmp[j,COL_MDT_END])
#					cat("............Overlap: ",overlap,"\n",sep="")
					if(!is.na(overlap) && overlap>0)
					{	# split the periods
						sp <- split.overlapping.periods(tmp[i,COL_MDT_START], tmp[i,COL_MDT_END], tmp[j,COL_MDT_START], tmp[j,COL_MDT_END])
#						cat("............Splitting: \n");print(sp)
						
						# keep the office types for latter
						o1 <- offices[[i]]
						o2 <- offices[[j]]
						o3 <- sort(union(o1,o2))
						
						# update first period
						if(is.null(sp[["1"]]))
						{	
#							cat("..............Removing the first row\n")
							tmp <- tmp[-i,, drop=FALSE]
							offices <- offices[-i]
							goOn <- FALSE
							j <- j - 1
						}
						else
						{	
#							cat("..............Updating the first row\n")
							tmp[i,COL_MDT_START] <- sp[["1"]][1]
							tmp[i,COL_MDT_END] <- sp[["1"]][2]
						}
						# update second period
						if(is.null(sp[["2"]]))
						{	
#							cat("..............Removing the second row\n")
							tmp <- tmp[-j,, drop=FALSE]
							offices <- offices[-j]
							j <- j - 1
						}
						else
						{	
#							cat("..............Updating the second row\n")
							tmp[j,COL_MDT_START] <- sp[["2"]][1]
							tmp[j,COL_MDT_END] <- sp[["2"]][2]
						}
						# possibly add new periods
						if(!is.null(sp[["1b"]]))
						{	
#							cat("..............Completing the first row\n")
							tmp <- rbind(tmp, rep(NA,ncol(tmp)))
							k <- nrow(tmp)
							tmp[k,COL_MDT_TYPE] <- paste0(o1,collapse=",")
							tmp[k,COL_MDT_START] <- sp[["1b"]][1]
							tmp[k,COL_MDT_END] <- sp[["1b"]][2]
							offices[[k]] <- o1
						}
						if(!is.null(sp[["2b"]]))
						{	
#							cat("..............Completing the second row\n")
							tmp <- rbind(tmp, rep(NA,ncol(tmp)))
							k <- nrow(tmp)
							tmp[k,COL_MDT_TYPE] <- paste0(o2,collapse=",")
							tmp[k,COL_MDT_START] <- sp[["2b"]][1]
							tmp[k,COL_MDT_END] <- sp[["2b"]][2]
							offices[[k]] <- o2
						}
						if(!is.null(sp[["3"]]))
						{	
#							cat("..............Adding a third row\n")
							tmp <- rbind(tmp, rep(NA,ncol(tmp)))
							k <- nrow(tmp)
							tmp[k,COL_MDT_TYPE] <- paste0(o3,collapse=",")
							tmp[k,COL_MDT_START] <- sp[["3"]][1]
							tmp[k,COL_MDT_END] <- sp[["3"]][2]
							offices[[k]] <- o3
						}
						
					}
					j <- j + 1
				}
				if(goOn)
					i <- i + 1
			}
		}
		tmp.idx <- order(tmp[,COL_MDT_START])
		tmp <- tmp[tmp.idx,]
		offices <- offices[tmp.idx]
#		print(tmp)
		
		# update mandate types to match multiple offices
		for(i in 1:length(offices))
		{	o <- offices[[i]]
			# only consider cases with several mandate types
			if(length(o)>1)
			{	# if two local mandates: local cumul
				if(all(sapply(o, function(o1) o1 %in% c("CM","CD","CR"))))
					tmp[i,COL_MDT_TYPE] <- "CuL"
				# if one local and one national mandates: vertical cumul
				else if(("CM" %in% o || "CD" %in% o || "CR" %in% o) && ("D" %in% o || "S" %in% o || "RE" %in% o) && length(o)==2)
					tmp[i,COL_MDT_TYPE] <- "CuV"
				# if 1-2 local and 1-2 national mandates, or if (S or D) and RE: reinforced cumul
				else if((("CM" %in% o || "CD" %in% o || "CR" %in% o) && ("D" %in% o || "S" %in% o || "RE" %in% o) && length(o)>2)
						|| (("D" %in% o || "S" %in% o) && "RE" %in% o && length(o)==2))
					tmp[i,COL_MDT_TYPE] <- "CuR"
				else 
				{	cat("WARNING: combination of mandates corresponding to no predefined combination: ",tmp[i,COL_MDT_TYPE],"\n")
					tmp[i,COL_MDT_TYPE] <- "CuR"
				}
			}
		}
#		print(offices)
#		print(tmp)
		
		# note : point médian entre deux dates (finalement pas utilisé)
		# as.Date((as.integer(dates[1])+as.integer(dates[2]))/2, origin=as.Date("1970-01-01"))		
		
		# build string representing sequence
		matches <- sapply(1:(length(dates)-1), function(d)
		{	# compute intersection durations
			inters <- sapply(1:nrow(tmp), function(i)
			{	date.intersect.val(start1=dates[d], end1=dates[d+1], 
						start2=tmp[i,COL_MDT_START], end2=tmp[i,COL_MDT_END])
			})
			# take the longest
			idx <- which(!is.na(inters) & inters>=semi.duration)
			if(length(idx)>0)
			{	idx <- idx[which.max(inters[idx])]
				str <- tmp[idx,COL_MDT_TYPE]
			}
			else
				str <- NA
		})
		
		res <- paste(matches, collapse="-")
		return(cbind(id, res))
	}))
	cat("  Processing of ids complete\n")
	colnames(seqs) <- c(COL_ID, COL_SEQ)
	
	# remove empty trajectories (all mandates out of the period)
	idx <- which(seqs[,COL_SEQ]==empty.seq)
	if(length(idx)>0)
	{	cat("  Removing",length(idx),"empty trajectories (no mandates, or all of them out of the specified period)\n")
		cat("     Concerned ids: ", paste0(unique.ids[idx],collapse=", "),"\n",sep="")
		seqs <- seqs[-idx,]
	}
#	print(seqs)
	
	# convert to dataframe
	seqs <- data.frame(
		seqs, 
		stringsAsFactors=FALSE,
		check.names=FALSE
	)
	print(seqs)
	
	# create the traminer object
	sd <- seqdef(
		data=seqs,									# data to process
		left=left,									# how to handle missing data at the beginning of the sequence (NA vs. "DEL") 
		gap=NA,										# how to handle missing data inside the sequence (same as above)
		right=NA,									# how to handle missing data at the end of the sequence (same as left) 
		var=COL_SEQ,								# name of the columns containing the formatted sequences
		id=seqs[,COL_ID],							# ids of the politicians
		alphabet=short.names,						# list of position codes
		labels=names(short.names),					# names of these positions
		cpal=colors[1:length(short.names)],			# colors of these positions
		missing.color="#AAAAAA",					# color of missing values
		cnames=format(dates[1:(length(dates)-1)])	# x-axis labels
	)
	
	return(sd)
}




#############################################################################################
# conversion of full mandate/function names to short versions
COMMON_SHORT_NAMES <- c(
	"CONSEILLER DEPARTEMENTAL"="CD",
	"CONSEILLER MUNICIPAL"="CM",
	"CONSEILLER REGIONAL"="CR",
	"DEPUTE"="D",
	"REPRESENTANT EUROPEEN"="RE",
	"SENATEUR"="S",
	"1er ADJOINT"="A",
	"ADJOINT"="A",
	"M"="M",
	"Ma"="Ma",
	"P"="P",
	"SUPPLEANT"="Sup",
	"VP"="VP",
	"VP AFFAIRES ECONOMIQUES"="VP"
)
COMMON_STATE_NAMES <- c(
	"A"="Adjoint(e) au maire",
	"CD"="Conseiller(e) departemental(e)",
	"CM"="Conseiller(e) municipal(e)",
	"CR"="Conseiller(e) regional(e)",
	"CuL"="Cumul local",
	"CuV"="Cumul vertical",
	"CuR"="Cumul renforce",
	"D"="Depute(e)",
	"M"="Maire",
	"Ma"="Maire d'arrondissement",
	"P"="President(e)",
	"L"="Local",
	"RE"="Representant(e) europeen(e)",
	"S"="Senateur/Senatrice",
	"Sup"="Suppleant(e)",
	"VP"="Vice-president(e)"
)
#############################################################################################
# mandates and functions that should be completely ignored during sequence extraction (short names)
NOFUN_IGNORED_MDT <- c("A", "M", "Ma", "P", "Sup", "VP")
# mandates that correspond to joint mandates with a specific name (all short names)  
NOFUN_JOINT_MDT <- c(
	# single mandates: nothing to do
	"CD"="CD", "CM"="CM", "CR"="CR", "D"="D", "RE"="RE", "S"="S",
	# exactly 2 local mandates
	"CD-CM"="CuL", "CD-CR"="CuL", "CM-CR"="CuL", 
	# exactly 3 local mandates
	"CD-CM-CR"="CuL",
	# exactly 1 local and 1 national mandates
	"CD-D"="CuV",  "CR-D"="CuV",  "CM-D"="CuV",
	"CD-RE"="CuV", "CR-RE"="CuV", "CM-RE"="CuV",
	"CD-S"="CuV",  "CR-S"="CuV",  "CM-S"="CuV",
	# 2 local and 1 national mandates
	"CD-CM-D"="CuR",  "CD-CR-D"="CuR",  "CM-CR-D"="CuR",
	"CD-CM-RE"="CuR", "CD-CR-RE"="CuR", "CM-CR-RE"="CuR",
	"CD-CM-S"="CuR",  "CD-CR-S"="CuR",  "CM-CR-S"="CuR",
	# 2 local and 2 national mandates
	"CD-CM-D-RE"="CuR", "CD-CR-D-RE"="CuR", "CM-CR-D-RE"="CuR", 
	"CD-CM-RE-S"="CuR", "CD-CR-RE-S"="CuR", "CM-CR-RE-S"="CuR", 
	# 3 local and 1 national mandates
	"CD-CM-CR-D"="CuR", "CD-CM-CR-RE"="CuR", "CD-CM-CR-S"="CuR",
	# 1 local and 2 national mandates
	"CD-D-RE"="CuR", "CR-D-RE"="CuR", "CM-D-RE"="CuR",
	"CD-RE-S"="CuR", "CR-RE-S"="CuR", "CM-RE-S"="CuR",		
	# RE and (S or D)
	"RE-S"="CuR", "D-RE"="CuR"
)
NOFUN_ORDER_MDT <- c("CD", "CM", "CR", "D", "RE", "S", "CuL", "CuV","CuR")
#############################################################################################
# mandates and functions that should be completely ignored during sequence extraction (short names)
LOCNAT_IGNORED_MDT <- c("A", "Ma", "P", "Sup", "VP", "RE")
# mandates that correspond to joint mandates with a specific name (all short names)  
LOCNAT_JOINT_MDT <- c(
	# at least mayor
	"CM-M"="M", "CD-CM-M"="M", "CM-CR-M"="M", "CD-CM-CR-M"="M",
	# purely local mandates
	"CD"="L", "CM"="L", "CR"="L", 
	"CD-CM"="L", "CD-CR"="L", "CM-CR"="L",
	"CD-CM-CR"="L", 
	# national assembly
	"D"="D", 
	"CD-D"="D", "CM-D"="D", "CM-D-M"="D", "CR-D"="D",
	"CD-CM-D"="D", "CD-CM-D-M"="D", "CD-CR-D"="D", "CM-CR-D"="D", "CM-CR-D-M"="D",
	"CD-CM-CR-D"="D", "CD-CM-CR-D-M"="D",
	# senate
	"S"="S", 
	"CD-S"="S", "CM-S"="S", "CM-M-S"="S", "CR-S"="S",
	"CD-CM-S"="S", "CD-CM-M-S"="S", "CD-CR-S"="S", "CM-CR-S"="S", "CM-CR-M-S"="S",
	"CD-CM-CR-S"="S", "CD-CM-CR-M-S"="S"
)
LOCNAT_ORDER_MDT <- c("M", "L", "D", "S")
#############################################################################################
# mandates and functions that should be completely ignored during sequence extraction (short names)
ONLYLOC_IGNORED_MDT <- c( "P", "Sup", "VP", "RE", "D", "S")
# mandates that correspond to joint mandates with a specific name (all short names)  
ONLYLOC_JOINT_MDT <- c(
	# only CM
	"CM"="CM",
	# at least mayor
	"CM-M"="M", "CD-CM-M"="M", "CM-CR-M"="M", "CD-CM-CR-M"="M",
	"A-CM-M"="M", "A-CD-CM-M"="M", "A-CM-CR-M"="M", "A-CD-CM-CR-M"="M",
	"Ma-CM-M"="M", "Ma-CD-CM-M"="M", "Ma-CM-CR-M"="M", "Ma-CD-CM-CR-M"="M",
	# at least assistant mayor
	"A-CM"="A", "A-CD-CM"="A", "A-CM-CR"="A", "A-CD-CM-CR"="A",
	"CM-Ma"="A", "CD-CM-Ma"="A", "CM-CR-Ma"="A", "CD-CM-CR-Ma"="A", "A-CR"="A",
	"A-CD"="A",
	# other purely local mandates
	"CD"="L", "CM"="L", "CR"="L",
	"CD-CM"="L", "CD-CR"="L", "CM-CR"="L", 
	"CD-CM-CR"="L"
)
ONLYLOC_ORDER_MDT <- c("CM", "A", "M", "L")
#############################################################################################




#############################################################################################
# Converts the mandate table into a Traminer-compatible data structure. This variant of the
# first function is able to handle functions (such as mayor) in addition to mandates.
# 
# tab.mandates: mandate table.
# start.date: beginning of the period to process.
# end.date: end of the period to process.
# left: left alignment of the sequences ("DEL" = all aligned on their first state, NA = keep 
#	    the sequences as they are).
# colors: whether to use true colors (TRUE) vs. shades of grey (FALSE).
# ignored.mandates: list of mandate types to ignore when extracting the sequences.
# joint.mandates: map describing how to handle **all** the possible types of joint mandates. 
# mandate.names: noms courts et longs des mandats, dans l'ordre voulu (utilise dans les graphiques).
#
# returns: a Traminer object.
#############################################################################################
convert.to.sequences.fun <- function(tab.mandates, start.date=as.Date("2000/1/1"), end.date=Sys.Date(), left=NA, colors=TRUE, ignored.mandates=c(), joint.mandates=c(), mandate.names=c())
{	cat("Converting data to sequences \n")
	
	# column names
	COL_ID <- "Id"
	COL_MDT_START <- "DateDebutMandat"
	COL_MDT_END <- "DateFinMandat"
	COL_MDT_TYPE <- "TypeMandat"
	COL_FUN_START <- "DateDebutFonction"
	COL_FUN_END <- "DateFinFonction"
	COL_FUN_TYPE <- "TypeFonction"
	COL_SEQ <- "Sequence"
	
	# colors
	if(colors)
		cols <- STATE_COLORS
	else
	{	cols <- get.gray.palette(length(mandate.names))
		names(cols) <- mandate.names
	}	
	
	# temporal resolution
	granularity <- "year"
	dates <- seq(start.date, end.date, granularity)
	semi.duration <- as.integer((dates[2]-dates[1])/2)
	empty.seq <- paste(rep(NA,length(dates)-1), collapse="-")
	
	# note on the granularity :
	# can be an integer = number of days
	# or a string among "day", "week", "month", "quarter", "year"
	# a number can be placed in the string, e.g. "2 months"
	
	# short version of the mandate/function names
	if(length(mandate.names)==0)
		mandate.names <- sort(unique(joint.mandates))
	
	# at first, let us just use the mandate names
	mdt.order <- c("M", "L", "D", "S")
	unique.ids <- sort(unique(tab.mandates[,COL_ID]))
	cat("  Processing each id separately \n")
	seqs <- t(sapply(1:length(unique.ids), function(r)
	{	id <- unique.ids[r]
		cat("    Processing id ",id, " (",r,"/",length(unique.ids),")\n",sep="")
		
		# retrieve the dates for the current id
		rows <- which(tab.mandates[,COL_ID]==id)
		tmp <- tab.mandates[rows,c(COL_MDT_TYPE,COL_MDT_START,COL_MDT_END)]
		#print(tmp)
		
		# consider functions as just other mandates
		tmp.fun <- tab.mandates[rows,c(COL_FUN_TYPE,COL_FUN_START,COL_FUN_END)]
		tmp.fun <- tmp.fun[!is.na(tmp.fun[,COL_FUN_TYPE]),,drop=FALSE]
		colnames(tmp.fun) <- c(COL_MDT_TYPE,COL_MDT_START,COL_MDT_END)
		tmp <- rbind(tmp, tmp.fun)
		
		# convert mandate types to short form
		tmp[,COL_MDT_TYPE] <- COMMON_SHORT_NAMES[tmp[,COL_MDT_TYPE]]
		
		# complete missing end dates
		no.end.date <- which(is.na(tmp[,COL_MDT_END]))
		tmp[no.end.date,COL_MDT_END] <- rep(end.date, length(no.end.date))
		
		# order by mandate dates and types
		mnd.types <- match(tmp[,COL_MDT_TYPE], mdt.order)
		idx <- order(tmp[,COL_MDT_START], mnd.types)
		mnd.types <- mnd.types[idx]
		tmp <- tmp[idx,]
		
		# remove ignored mandate types
		idx <- which(tmp[,COL_MDT_TYPE] %in% ignored.mandates)
		if(length(idx)>0)
			tmp <- tmp[-idx,,drop=FALSE]
		
		# test
		#tmp[3,COL_MDT_END] <- as.Date("2017-06-20")
		#tmp[5,COL_MDT_END] <- as.Date("2022-06-18")
#		print(tmp)
		
		# merge consecutive or overlapping mandates of same type
#		cat("......Merging consecutive or overlapping mandates of same type\n")
		if(nrow(tmp)>1)
		{	m.types <- sort(unique(tmp[,COL_MDT_TYPE]))
			for(m.type in m.types)
			{	ms <- which(tmp[,COL_MDT_TYPE]==m.type)
				if(length(ms)>1)
				{	i <- 1
					while(i<length(ms))
					{	
#						cat("........Processing row ",i,"/",length(ms),": ",tmp[ms[i],COL_MDT_TYPE],", ",format(tmp[ms[i],COL_MDT_START]),", ",format(tmp[ms[i],COL_MDT_END]),"\n",sep="")
						j <- i + 1
						while(j<=length(ms))
						{	
#							cat("..........Comparing with row ",j,"/",length(ms),": ",tmp[ms[j],COL_MDT_TYPE],", ",format(tmp[ms[j],COL_MDT_START]),", ",format(tmp[ms[j],COL_MDT_END]),"\n",sep="")
							overlap <- date.intersect.val(tmp[ms[i],COL_MDT_START], tmp[ms[i],COL_MDT_END], tmp[ms[j],COL_MDT_START], tmp[ms[j],COL_MDT_END])
							if(!is.na(overlap))
							{	dd <- merge.overlapping.periods(tmp[ms[i],COL_MDT_START], tmp[ms[i],COL_MDT_END], tmp[ms[j],COL_MDT_START], tmp[ms[j],COL_MDT_END])
								tmp[ms[i],COL_MDT_START] <- dd[1]
								tmp[ms[i],COL_MDT_END] <- dd[2]
								tmp <- tmp[-ms[j],]
								ms[j:length(ms)] <- ms[j:length(ms)] - 1
								ms <- ms[-j]
							}
							else
								j <- j + 1
						}
						i <- i + 1
					}
				}
			}
		}
#		print(tmp)
		
		if(nrow(tmp)>0)
		{
			# split periods where several mandate types overlap
#			cat("......Splitting periods where several mandates (of different types) overlap\n")
			offices <- list()
			for(i in 1:nrow(tmp))
				offices[[i]] <- tmp[i,COL_MDT_TYPE]
			if(nrow(tmp)>1)
			{	i <- 1
				while(i<nrow(tmp))
				{	
#					cat("........Processing row ",i,"/",nrow(tmp),": ",tmp[i,COL_MDT_TYPE],", ",format(tmp[i,COL_MDT_START]),", ",format(tmp[i,COL_MDT_END]),"\n",sep="")
					j <- i + 1
					goOn <- TRUE
					while(j<=nrow(tmp) && goOn)
					{	
#						cat("..........Comparing with row ",j,"/",nrow(tmp),": ",tmp[j,COL_MDT_TYPE],", ",format(tmp[j,COL_MDT_START]),", ",format(tmp[j,COL_MDT_END]),"\n",sep="")
						overlap <- date.intersect.val(tmp[i,COL_MDT_START], tmp[i,COL_MDT_END], tmp[j,COL_MDT_START], tmp[j,COL_MDT_END])
#						cat("............Overlap: ",overlap,"\n",sep="")
						if(!is.na(overlap) && overlap>0)
						{	# split the periods
							sp <- split.overlapping.periods(tmp[i,COL_MDT_START], tmp[i,COL_MDT_END], tmp[j,COL_MDT_START], tmp[j,COL_MDT_END])
#							cat("............Splitting: \n");print(sp)
							
							# keep the office types for latter
							o1 <- offices[[i]]
							o2 <- offices[[j]]
							o3 <- sort(union(o1,o2))
							
							# update first period
							if(is.null(sp[["1"]]))
							{	
#								cat("..............Removing the first row\n")
								tmp <- tmp[-i,, drop=FALSE]
								offices <- offices[-i]
								goOn <- FALSE
								j <- j - 1
							}
							else
							{	
#								cat("..............Updating the first row\n")
								tmp[i,COL_MDT_START] <- sp[["1"]][1]
								tmp[i,COL_MDT_END] <- sp[["1"]][2]
							}
							# update second period
							if(is.null(sp[["2"]]))
							{	
#								cat("..............Removing the second row\n")
								tmp <- tmp[-j,, drop=FALSE]
								offices <- offices[-j]
								j <- j - 1
							}
							else
							{	
#								cat("..............Updating the second row\n")
								tmp[j,COL_MDT_START] <- sp[["2"]][1]
								tmp[j,COL_MDT_END] <- sp[["2"]][2]
							}
							# possibly add new periods
							if(!is.null(sp[["1b"]]))
							{	
#								cat("..............Completing the first row\n")
								tmp <- rbind(tmp, rep(NA,ncol(tmp)))
								k <- nrow(tmp)
								tmp[k,COL_MDT_TYPE] <- paste0(o1,collapse=",")
								tmp[k,COL_MDT_START] <- sp[["1b"]][1]
								tmp[k,COL_MDT_END] <- sp[["1b"]][2]
								offices[[k]] <- o1
							}
							if(!is.null(sp[["2b"]]))
							{	
#								cat("..............Completing the second row\n")
								tmp <- rbind(tmp, rep(NA,ncol(tmp)))
								k <- nrow(tmp)
								tmp[k,COL_MDT_TYPE] <- paste0(o2,collapse=",")
								tmp[k,COL_MDT_START] <- sp[["2b"]][1]
								tmp[k,COL_MDT_END] <- sp[["2b"]][2]
								offices[[k]] <- o2
							}
							if(!is.null(sp[["3"]]))
							{	
#								cat("..............Adding a third row\n")
								tmp <- rbind(tmp, rep(NA,ncol(tmp)))
								k <- nrow(tmp)
								tmp[k,COL_MDT_TYPE] <- paste0(o3,collapse=",")
								tmp[k,COL_MDT_START] <- sp[["3"]][1]
								tmp[k,COL_MDT_END] <- sp[["3"]][2]
								offices[[k]] <- o3
							}
							
						}
						j <- j + 1
					}
					if(goOn)
						i <- i + 1
				}
			}
			tmp.idx <- order(tmp[,COL_MDT_START])
			tmp <- tmp[tmp.idx,]
			offices <- offices[tmp.idx]
#			print(tmp)
			
			# update mandate types to match multiple offices
			for(i in 1:length(offices))
			{	o <- offices[[i]]
				o.str <- paste(o,collapse="-")
				repl <- joint.mandates[o.str]
				if(is.na(repl))
				{	print(tmp)
					stop(paste0("ERROR: did not find the replacement string in joint.mandates for mandate(s) \"",paste(o,collapse=", "),"\""))
					#cat(paste0("ERROR: did not find the replacement string in joint.mandates for mandate(s) \"",paste(o,collapse=", "),"\"\n"))
				}
				else
					tmp[i,COL_MDT_TYPE] <- repl
			}
#			print(offices)
#			print(tmp)
		
			# note : point médian entre deux dates (finalement pas utilisé)
			# as.Date((as.integer(dates[1])+as.integer(dates[2]))/2, origin=as.Date("1970-01-01"))		
		
			# build string representing sequence
			matches <- sapply(1:(length(dates)-1), function(d)
					{	# compute intersection durations
						inters <- sapply(1:nrow(tmp), function(i)
								{	date.intersect.val(start1=dates[d], end1=dates[d+1], 
											start2=tmp[i,COL_MDT_START], end2=tmp[i,COL_MDT_END])
								})
						# take the longest
						idx <- which(!is.na(inters) & inters>=semi.duration)
						if(length(idx)>0)
						{	idx <- idx[which.max(inters[idx])]
							str <- tmp[idx,COL_MDT_TYPE]
						}
						else
							str <- NA
					})
			res <- paste(matches, collapse="-")
		}
		# empty trajectory
		else
			res <- empty.seq
		
		return(cbind(id, res))
	}))
	cat("  Processing of ids complete\n")
	colnames(seqs) <- c(COL_ID, COL_SEQ)
	
	# remove empty trajectories (all mandates out of the period)
	idx <- which(seqs[,COL_SEQ]==empty.seq)
	if(length(idx)>0)
	{	cat("  Removing",length(idx),"empty trajectories (no mandate/function, or all of them out of the specified period)\n")
		cat("     Concerned ids: ", paste0(unique.ids[idx],collapse=", "),"\n",sep="")
		seqs <- seqs[-idx,]
	}
#	print(seqs)
	
	# convert to dataframe
	seqs <- data.frame(
		seqs, 
		stringsAsFactors=FALSE,
		check.names=FALSE
	)
#	print(seqs)
	
	# create the traminer object
	mn <- COMMON_STATE_NAMES[mandate.names]
	sd <- seqdef(
		data=seqs,									# data to process
		left=left,									# how to handle missing data at the beginning of the sequence (NA vs. "DEL") 
		gap=NA,										# how to handle missing data inside the sequence (same as above)
		right=NA,									# how to handle missing data at the end of the sequence (same as left) 
		var=COL_SEQ,								# name of the columns containing the formatted sequences
		id=seqs[,COL_ID],							# ids of the politicians
		alphabet=names(mn),							# list of position codes
		labels=mn,									# names of these positions
		cpal=cols[mandate.names],					# colors of these positions
		missing.color=MISSING_COLOR,				# color of missing values
		cnames=format(dates[1:(length(dates)-1)])	# x-axis labels
	)
	
	return(sd)
}




#############################################################################################
# Counts the number of each state in each time step.
# 
# sd: sequence dataset.
#
# returns: table with the number of states.
#############################################################################################
count.states <- function(sd, file.name)
{	# compute the stats
	unique.vals <- alphabet(sd)
	m <- as.matrix(sd)
	tab <- apply(m, 2, function(vect) table(factor(vect, levels=unique.vals), useNA="always"))
	
	# add marginal sums
	tab <- cbind(as.table(t(tab)), colSums(tab))
	colnames(tab)[ncol(tab)] <- "Total"
	
	# record them as a CSV file
	write.table(
		x=tab,
		file=paste0(file.name,"_counts.csv"),
		quote=FALSE,
		sep=",",
		fileEncoding="UTF-8",
		row.names=TRUE,
		col.names=TRUE
	)
	
	##############
	
	# compute the frequencies
	ll <- seqstatd(
		sd,
		weighted=TRUE,
		with.missing=TRUE,
		norm=TRUE
	)
	
	# represent them as a table
	tab <- cbind(as.table(t(ll$Frequencies)), ll$ValidStates, ll$Entropy)
	colnames(tab)[ncol(tab)-1] <- "States"
	colnames(tab)[ncol(tab)] <- "Entropy"
	
	# record them as a CSV file
	write.table(
		x=tab,
		file=paste0(file.name,"_freqs.csv"),
		quote=FALSE,
		sep=",",
		fileEncoding="UTF-8",
		row.names=TRUE,
		col.names=TRUE
	)
}
