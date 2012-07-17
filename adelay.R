# stop chatter
sink("/dev/null")

# make sure tuneR is installed
is.installed <- function(mypkg) is.element(mypkg, installed.packages()[,1]) 
if(!is.installed("tuneR")) install.packages("tuneR")
require(tuneR, quietly=TRUE)

# Get file names from args
args = commandArgs(trailingOnly=TRUE)
if(length(args) != 2) stop("Usage: Rscript adelay.R <filename1.mp3> <filename2.mp3>\n")
oname = args[1]
dname = args[2]

# read 2 files
origwav = readMP3(oname)
delayedwav = readMP3(dname)

# Get samplingrate - assume delayed signal has same samplingrate
sr = origwav@samp.rate

# Extract signal - assume mono with signal in left
orig = origwav@left
delayed = delayedwav@left

# Calculate cross correlation, try for lags up to 1000ms
ccor = ccf(orig,delayed, plot=FALSE, lag.max=sr)
cor = ccor$acf[,,1]
lag = ccor$lag[,,1]
lagmax = lag[which.max(cor)]
delay = abs(lagmax/sr*1000)

# cat result
sink()
cat(paste(round(delay), "\n"))