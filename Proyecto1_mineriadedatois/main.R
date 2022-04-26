# install.packages("devtools")
# install.packages("spotifyr")
library(devtools)
library(spotifyr)
library(tidyverse)
library(knitr)

Sys.setenv(SPOTIFY_CLIENT_ID = 'afe4982530de4f469b16a1d9fcba34f3')
Sys.setenv(SPOTIFY_CLIENT_SECRET = 'c7c96774d3a84feeab5c22519a8db4f1')

acces_token <- get_spotify_access_token()

song <- get_track_audio_features('2NHRuhyjjZo7WiQQgGP0Y1?si=9e4f7ab620d24f76') 

left_join(song, by="track_uri")
