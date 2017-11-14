Project authors: Amélie Meurer, Mathilde Lavacquery, Margaux Wehr, Clément Ponsonnet
All rights reserved

(To simplify running of code, download ‘base_trajet_total.csv’ and ‘temps_trajet.csv’ directly  to avoid having to merge all the databases from scratch)

The aim of the project was to find the optimal mid-way for two users in a city to meet using Djikstra algorithm. A visualisation tool depicting the mi-way and meeting point suggestions (café, bar, restaurant) was integrated into
 a Shiny App. This optimal mid-way was computed based on data taken from the public Paris metro station dataset (‘Open RATP’ website). The scope of the dataset was limited to metro and suburban train (RER), excluding buses. 

Datasets were taken from the following website:
https://data.ratp.fr/explore/dataset/offre-transport-de-la-ratp-format-gtfs/
Folder  “RATP_GTFS_FULL”

This folder included separate databases for metro stop, trips, routes and stop_times which we merged. 

After merging of different datasets, the global dataset was sorted by:
1. Direction of line (to or fro)
2.  branches (several branches per line)
3.  stop sequence (order of stations per line)

Some of the challenges encountered were loops for some lines and irregular stop sequences, which we corrected for to enable visualisation and computation of Djikstra algorithm. Visualisations were produced using ggplot and leaflet packages, with attribution of colours per line.

To prepare for the Djikstra algorithm, timings between stops were calculated using stop inputs and stop outputs. Commute travel time between stops (walking) were integrated to adjust for the timing computations.  The database was simplified to a 3-column dataframe containing stop input, stop output and time difference between input and output (‘weight’).

For example, the line RER B had 10 different branches, these were reduced to just 2 (KOCQ and SOIR) due to irregular stop sequences on the other branches (duplicate stop sequence for one stop, stops being skipped on the line)
Metro Line 7 had loops, which were separated as separate journeys.

The cafés, restaurants and bar recommendations were taken from Google API website (https://developers.google.com/places/web-service/)  and integrated to midway calculations through geographical coordinates.

The project was enriching from several angles :

1. Narrowing of the scoping of project (metro and RER, excluding for buses)
2. Arranging of database based on direction, branches, stop sequences 
3. Exclusion of outliers (loops,  stop sequences and variables (sorting the databases by key variables 
4. Visualisation using leaflet packages
5. Shiny App integration using user interface and server
