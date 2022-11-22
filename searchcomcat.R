# searches Comcat for basic hypocentral data using the csv format ( see http://earthquake.usgs.gov/earthquakes/feed/v1.0/csv.php although that documentation is missing the type parameter at the end )

# usage:
# save this file in searchcomcat.R (the name does not actually matter)
# these functions only use the basic level R installation and so no other packages need to be installed or loaded.
# in your R script or session do the following:
# source("searchcomcat.R")  (use the full path name if it is in another directory)
# comcatdata <- comcathypocsv()   (this retrieves all earthquakes for the past 30 days into a data frame called comcatdata)
# to apply different search criteria see the parameters listed in the function call below or at http://comcat.cr.usgs.gov/fdsnws/event/1/, e.g.
# comcatdata <- comcathypocsv(minmagnitude=5)   (this will return only M>=5 earthquakes)
# comcatdata <- comcathypocsv(alertlevel="red") (this will return only earthquakes with a PAGER level of red)

# this function returns a dataframe (comcatdata in the above examples) that has columns which are from the ComCat csv format
# and one additional column, named rtime, which is the time converted to a POSIXlt format which can be used for computations in R
# to explore the output of the frame try commands such as:
# colnames(comcatdata)  (will return the names of the columns)
# nrow(comcatdata)   (will return the number of events found by the search)

# this script lets you set many parameters but only uses the ones that are set to something other than NA
# it has few defaults beyond the ones applied by Comcat and it does very little parameter checking

# ordering is always done by ascending time to make it easier to deal with the need to search by multiple windows

# start, end, and updatedafter times should be given as R internal POSIXlt classes so that the script can work with them.  See the R function Sys.time in the base class
# numeric parameters (e.g. minmagnitude) should be given as numbers
# character string parameters (e.g. eventtype or alertlevel) should be given as strings

# time is assumed to be in UTC, the function does not check the time zone for the times that are given

# the defaults it has are:
#1: endtime defaults to 1 day in the future from system time.  This lets you search without worrying that time is passing as the program does things
#2: starttime defaults to defaultduration days in the past from system time, set below
#3: I put in the default eventtype=earthquake so that it does not return blasts unless you want them, set to NA if you don't want this applied

# parameter checking is limited to:
# if min or max radius is set in both km or degrees then the km version takes precedence
# it makes sure that starttime is before endtime

# if a parameter is not set in the call to the function then it is not used in the search and Comcat defaults are used
# see http://comcat.cr.usgs.gov/fdsnws/event/1/ for more info on parameters and their defaults

# FUNCTION STARTS HERE.  PARAMETERS ARE SHOWN WITH THEIR DEFAULTS AND DO NOT NEED TO BE GIVEN UNLESS YOU WANT TO CHANGE THEM.
# NA means Not Available and the parameter will not be used (except for the defaults on start and end times)

comcathypocsv <- function(starttime=NA,endtime=NA,updatedafter=NA, # these are the time window parameters
minlatitude=NA,maxlatitude=NA,minlongitude=NA,maxlongitude=NA, # these are the lat-lon box parameters
latitude=NA,longitude=NA,minradius=NA, minradiuskm=NA, maxradius=NA, maxradiuskm=NA, # these are the circle search parameters
mindepth=NA, maxdepth=NA, # depth search parameters
minmagnitude=NA, maxmagnitude=NA, # magnitude search parameters
catalog=NA, contributor=NA, # catalog and contributor specifications, omitting them gets the "preferred" solution
eventtype="earthquake", # if set to earthqukae will remove non-earthquakes from the search
reviewstatus=NA, # review status defaults to all or can be automatic or reviewed only
minmmi=NA, maxmmi=NA, # Minimum and Maximum values for Maximum Modified Mercalli Intensity reported by ShakeMap.
mincdi=NA, maxcdi=NA, # Minimum and Maximum values for Maximum Community Determined Intensity reported by DYFI.
minfelt=NA, # limit to events with this many DYFI responses
alertlevel=NA, # limit to events with a specific pager alert which can be "green", "yellow", "orange", or "red"
mingap=NA, maxgap=NA, # min and max azimuthal gaps
minsig=NA, maxsig=NA, # min and max significance (I have no idea what significance refers to in Comcat)
producttype=NA) # limits to events with specific product types available such as "moment-tensor", "focal-mechanism", "shakemap", "losspager", "dyfi".
{


maxeventspersearch <- 20000 # the code also searches for this but I am leaving the default in case that fails
defaultduration <- 30

# use orderby=time-asc

basic <- "http://comcat.cr.usgs.gov/fdsnws/event/1/"            # address of Comcat
basicdata <- paste(basic,"query?","orderby=time-asc&format=csv",sep="")     # basic request for data, always order by time, and use csv format
basiccount <- paste(basic,"count?","orderby=time-asc",sep="")   # basic request for count of data, always order by time

# get the maximum number of earthquakes allowed in a search by doing a event count on a small space-time window so it will be quick
# and using the gosjson format which returns this limit after the count
# note this search looks for M>=8 earthquakes in the last 10 minutes with hypocenters in a very small, rather inactive area
# thus it may fail if it ever returns an earthquake.
tenminutesago <- as.POSIXlt(Sys.time(), "GMT")  -(600) # this is 10 minutes in the past
tenminutesagostring <- strftime(tenminutesago,format="%Y-%m-%dT%H:%M:%S",tz="GMT") # convert time to a strong for comcat
getmaxcount <- paste(basic,"count?","orderby=time-asc&format=geojson&minlatitude=39.74881&maxlatitude=39.75012&minlongitude=-105.22078&maxlongitude=-105.21906&minmagnitude=8&starttime=",tenminutesagostring,sep="") # build the search URL to get the count
countstr <- readLines(getmaxcount,warn=FALSE)  # retrieve the count
countstr <- gsub("^.*wed.:","",countstr) # get rid of extraneous characters at the start of the line
countstr <- gsub("[,}]","",countstr) # get rid of extraneous characters at the end of the line
maxeventspersearch <- as.numeric(countstr) # convert the string to numeric

# build the request string for limits other than start and end time
requestlimits <- ""
if(!is.na(updatedafter)){
	updatedafterstr <- strftime(updatedafter,format="%Y-%m-%dT%H:%M:%S")
	requestlimits <- paste(requestlimits,"&updatedafter=",updatedafterstr,sep="")
}
if(!is.na(minlatitude))requestlimits <- paste(requestlimits,"&minlatitude=",minlatitude,sep="")
if(!is.na(maxlatitude))requestlimits <- paste(requestlimits,"&maxlatitude=",maxlatitude,sep="")
if(!is.na(minlongitude))requestlimits <- paste(requestlimits,"&minlongitude=",minlongitude,sep="")
if(!is.na(maxlongitude))requestlimits <- paste(requestlimits,"&maxlongitude=",maxlongitude,sep="")
if(!is.na(latitude))requestlimits <- paste(requestlimits,"&latitude=",latitude,sep="")
if(!is.na(longitude))requestlimits <- paste(requestlimits,"&longitude=",longitude,sep="")
if(!is.na(minradiuskm))requestlimits <- paste(requestlimits,"&minradiuskm=",minradiuskm,sep="")
if((!is.na(minradius)) & is.na(minradiuskm))requestlimits <- paste(requestlimits,"&minradius=",minradius,sep="")
if(!is.na(maxradiuskm))requestlimits <- paste(requestlimits,"&maxradiuskm=",maxradiuskm,sep="")
if((!is.na(maxradius)) & is.na(maxradiuskm))requestlimits <- paste(requestlimits,"&maxradius=",maxradius,sep="")
if(!is.na(mindepth))requestlimits <- paste(requestlimits,"&mindepth=",mindepth,sep="")
if(!is.na(maxdepth))requestlimits <- paste(requestlimits,"&maxdepth=",maxdepth,sep="")
if(!is.na(minmagnitude))requestlimits <- paste(requestlimits,"&minmagnitude=",minmagnitude,sep="")
if(!is.na(maxmagnitude))requestlimits <- paste(requestlimits,"&maxmagnitude=",maxmagnitude,sep="")
if(!is.na(catalog))requestlimits <- paste(requestlimits,"&catalog=",catalog,sep="")
if(!is.na(contributor))requestlimits <- paste(requestlimits,"&contributor=",contributor,sep="")
if(!is.na(eventtype))requestlimits <- paste(requestlimits,"&eventtype=",eventtype,sep="")
if(!is.na(reviewstatus))requestlimits <- paste(requestlimits,"&reviewstatus=",reviewstatus,sep="")
if(!is.na(minmmi))requestlimits <- paste(requestlimits,"&minmmi=",minmmi,sep="")
if(!is.na(maxmmi))requestlimits <- paste(requestlimits,"&maxmmi=",maxmmi,sep="")
if(!is.na(mincdi))requestlimits <- paste(requestlimits,"&mincdi=",mincdi,sep="")
if(!is.na(maxcdi))requestlimits <- paste(requestlimits,"&maxcdi=",maxcd,sep="")
if(!is.na(minfelt))requestlimits <- paste(requestlimits,"&minfelt=",minfelt,sep="")
if(!is.na(alertlevel))requestlimits <- paste(requestlimits,"&alertlevel=",alertlevel,sep="")
if(!is.na(mingap))requestlimits <- paste(requestlimits,"&mingap=",mingap,sep="")
if(!is.na(maxgap))requestlimits <- paste(requestlimits,"&maxgap=",maxgap,sep="")
if(!is.na(minsig))requestlimits <- paste(requestlimits,"&minsig=",minsig,sep="")
if(!is.na(maxsig))requestlimits <- paste(requestlimits,"&maxsig=",maxsig,sep="")
if(!is.na(producttype))requestlimits <- paste(requestlimits,"&producttype=",producttype,sep="")


# make sure we have start and end times, if not get defaults
if(is.na(starttime))starttime <- as.POSIXlt(Sys.time(), "GMT")  -(defaultduration*24*3600) # default is defaultduration days in the past
if(is.na(endtime))endtime <- as.POSIXlt(Sys.time(), "GMT") + (1*24*3600) # default is 1 day in the future

# make sure times are in order
if(endtime < starttime){
	junk <- endtime
	endtime <- starttime
	starttime <- junk
}

# put times into timepoints array that will get extended as needed to fit the maxeventspersearch limit
timepoints <- c(starttime,endtime)

# now build the request strings to do counts for the timespoints and requestlimits and run them to get the counts
# also do a request that will get the maximum allowed count for a data request
timepointstrings <- strftime(timepoints,format="%Y-%m-%dT%H:%M:%S",tz="GMT")
nwindows <- length(timepoints)-1
countrequests <- rep("",nwindows)
counts <- rep(0,nwindows)
i <- 1
countrequests[i] <- basiccount;
if(nchar(requestlimits)>0)countrequests[i] <- paste(countrequests[i],requestlimits,sep="")
countrequests[i] <- paste(countrequests[i],"&starttime=",timepointstrings[i],sep="")
countrequests[i] <- paste(countrequests[i],"&endtime=",timepointstrings[i+1],sep="")
counts[i] <- scan(countrequests[i],quiet=TRUE)

totalevents <- counts[i]

# now go into a loop where we split windows with counts > maxeventspersearch
while(max(counts)>maxeventspersearch){
	for(i in 1:nwindows){
		if(!is.na(counts[i]))if(counts[i]>maxeventspersearch){ # split the window from i to i+1, don't do test if counts is NA

			# timepoints gets a new value
			newtimepoint <- midtime(timepoints[i],timepoints[i+1])
			timepoints <- append(timepoints,newtimepoint,i)
			newtimepointstring <- strftime(newtimepoint,format="%Y-%m-%dT%H:%M:%S",tz="GMT")
			timepointstrings <- append(timepointstrings,newtimepointstring,i)

			# counts[i] should now be NA and the one after it should be NA
			counts[i] <- NA
			counts <- append(counts,NA,i)

			# similarly countrequests[i] and [i+1] should be NA
			countrequests[i] <- NA
			countrequests <- append(countrequests,NA,i)

			# build the count requests and get the new counts
			for(j in i:(i+1)){
				countrequests[j] <- basiccount;
				if(nchar(requestlimits)>0)countrequests[j] <- paste(countrequests[j],requestlimits,sep="")
				countrequests[j] <- paste(countrequests[j],"&starttime=",timepointstrings[j],sep="")
				countrequests[j] <- paste(countrequests[j],"&endtime=",timepointstrings[j+1],sep="")
				counts[j] <- scan(countrequests[j],quiet=TRUE)
			}

			# and now there is one more window
			nwindows <- nwindows+1

		} # end of the if loop if a window needs to be split
	} # end of for loop over the number of windows
} # end of while loop to develop the time windows with less than maxeventspersearch events


	# now get the data for each window

	# first develop the requests for each window
	datarequests <- rep("",nwindows)
	for(i in 1:nwindows){
		datarequests[i] <- basicdata;
		if(nchar(requestlimits)>0)datarequests[i] <- paste(datarequests[i],requestlimits,sep="")
		datarequests[i] <- paste(datarequests[i],"&starttime=",timepointstrings[i],sep="")
		datarequests[i] <- paste(datarequests[i],"&endtime=",timepointstrings[i+1],sep="")
	}

	# get the data for the first window
	data <- scan(datarequests[1],list(time="",latitude=0,longitude=0,depth=0,mag=0,magtype="",nst=0,gap=0,dmin=0,rms=0,net="",id="",updated="",place="",type=""),sep=",",quote="\"",skip=1,quiet=TRUE)

	# if there there are 2 or more windows, then get the data for later windows and append it
	if(nwindows>1)for(i in 2:nwindows){
		data2 <- scan(datarequests[i],list(time="",latitude=0,longitude=0,depth=0,mag=0,magtype="",nst=0,gap=0,dmin=0,rms=0,net="",id="",updated="",place="",type=""),sep=",",quote="\"",skip=1,quiet=TRUE)
		data <- mapply(c, data,data2,SIMPLIFY=FALSE) # mapply applies c (concatenate) to each item (years, mag, location...) in the lists  data and data2
	}

	# transform to a data frame
	dataframe <- data.frame(sapply(data,c))

	# remove non-unique events in case there are events on window boundaries
	dataframe <- unique(dataframe)

	# add POSIXlt format times to the data frame
	rtime <- as.POSIXlt(strptime(dataframe$time,"%Y-%m-%dT%H:%M:%OS"),tz="UTC")
	dataframe <- data.frame(dataframe,rtime=rtime)

	# return the data
	return(dataframe)

} # end of function


# midtime returns the midpoint time between two times
midtime <- function(early,late)
{
	mid <- early+(difftime(late,early)/2)
	return(mid)
}
