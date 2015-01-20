# Copyright (c) 2012 Firmafon ApS
# 
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
# 
# The above copyright notice and this permission notice shall be included in all
# copies or substantial portions of the Software.
# 
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
# SOFTWARE.

# Calculates delay in ms between 2 audio signals, where one typically
# is a transmitted version of the other, in which case the result is an estimate
# of the delay of the transmission channel
# Algorithm http://dsp.stackexchange.com/questions/736/how-do-i-implement-cross-correlation-to-prove-two-audio-files-are-similar

# stop chatter
sink("/dev/null")

# make sure tuneR and stats are installed
#is.installed <- function(mypkg) is.element(mypkg, installed.packages()[,1]) 
#if(!is.installed("tuneR")) install.packages("tuneR")
#if(!is.installed("stats")) install.packages("stats")
suppressPackageStartupMessages(require(tuneR, quietly=TRUE, warn.conflicts=FALSE))
require(stats, quietly=TRUE, warn.conflicts=FALSE)

# Get file names from args
args = commandArgs(trailingOnly=TRUE)
scriptname = substring(commandArgs(trailingOnly=FALSE)[4], 8)
if(length(args) != 1) stop(paste("Usage: Rscript ", scriptname, " <filename.wav>\n", sep=""))
oname = args[1]

# read 2 files - use readMP3 to import .MP3 files instead

origwav = readMP3(oname)



# Get samplingrate - assume delayed signal has same samplingrate
sr = origwav@samp.rate

orig = origwav@right
delayed = origwav@left

threshold = max(c(abs(orig), abs(delayed)))/5

o_start = min(which(abs(orig)>threshold))
d_start = min(which(abs(delayed)>threshold))

delay = 1000 * (d_start - o_start)/sr

# output result
sink()
cat(paste(round(delay), "\n"))