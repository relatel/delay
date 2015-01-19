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

# stop chatter


# make sure tuneR and stats are installed
is.installed <- function(mypkg) is.element(mypkg, installed.packages()[,1]) 
if(!is.installed("tuneR")) install.packages("tuneR")
if(!is.installed("stats")) install.packages("stats")
require(tuneR, quietly=TRUE)
require(stats, quietly=TRUE)


# read 2 files - use readWave to import .WAV files instead
origwav = readMP3("file_1_trim.mp3")
delayedwav = readMP3("file_2_trim.mp3")

# Get samplingrate - assume delayed signal has same samplingrate
sr = origwav@samp.rate

# Extract signal - assume mono with signal in left
orig = origwav@left
delayed = delayedwav@left

# Zero pad, at least half. nextn() selects "factor rich" length
nlength = nextn(max(length(orig), length(delayed))*2)
orig = c(orig, rep(0, nlength-length(orig)))
delayed = c(delayed, rep(0, nlength-length(delayed)))

# Calculate cross correlation
ccor = fft(fft(orig)*Conj(fft(delayed)), inverse=TRUE)
lmax = abs(which.max(Re(ccor)) - length(orig))
delay = lmax/sr*1000

# output result
cat(paste(round(delay), "\n"))