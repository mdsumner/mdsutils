
From: Andrew Hoskins <ajhos@deakin.edu.au>
To: Michael Sumner <mdsumner@utas.edu.au>
In-Reply-To: <49A67BC8.4050807@utas.edu.au>
Mime-Version: 1.0 (Apple Message framework v930.3)
Subject: Re: A question regarding light levels
Date: Fri, 27 Feb 2009 11:46:44 +1100G'day Mike,

The formula I used was taken from a book "Solar radiation and daylight models" by Tariq Muneer.  The formula itself is part of the DuMortier-Perraudeau-Page model presented on page 112.  I've only used the part of the model for beam irradiance which seems to give me close enough to what I want once I've changed it's amplitude.  It's crude but is working so far, I'm sure inclusion of the rest of the model would make it much nicer.  See below for an example using your example.

Cheers,
Andrew.


library(tripEstimation)

## presume we want for times around 6am (GMT for my local area)
tm <- ISOdatetime(2009, 2, 23, 5, 0, 0, tz = "GMT") + 11 * 3600

times <- seq(tm, tm + 2 * 3600, by = 60)  ## 2 hours, in 1 min steps

sun <- solar(times)
elev <- elevation(147, -42, sun)

## compare that with the surrounding few days
tm0 <- ISOdatetime(2009, 2, 22, 4, 30, 0, tz = "GMT") + 11 * 3600

times0 <- seq(tm0, tm0 + 2 * 24 * 3600, by = 360)  ## 6 min steps

sun0 <- solar(times0)
SOLALT <- elevation(147, -42, sun0)


Kb <- rep(NA,length(SOLALT))

## estimate light for all solar altitudes below 50 but above 0

Kb[SOLALT >= 0 & SOLALT <= 50] <- 62.134-0.75885*SOLALT[SOLALT >= 0 & SOLALT <= 50] + 0.27749*SOLALT[SOLALT >= 0 & SOLALT <= 50]^2 - 0.012108*SOLALT[SOLALT >= 0 & SOLALT <= 50]^3 + 0.0002052*SOLALT[SOLALT >= 0 & SOLALT <= 50]^4 - 1.2278*10^-6*SOLALT[SOLALT >= 0 & SOLALT <= 50]^5

## estimate for altitudes between 50 and 60 degrees

Kb[SOLALT > 50 & SOLALT <= 60] <- 103 + 0.2*(SOLALT[SOLALT > 50 & SOLALT <= 60]-50)

## altitudes greater than 60

Kb[SOLALT > 60] <- 105


plot(times0, SOLALT,ylim=c(-40,110))
lines(times0,Kb,col="red")



