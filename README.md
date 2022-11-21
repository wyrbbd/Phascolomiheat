# Victorian bushfire hotspot application

The application is based on various data to determine whether a hotspot is an ongoing bushfire or a new bushfire, using a cluster analysis algorithm to differentiate the judgment. Research and attempts in this area are shown here, covering the determination of past hotspots and the visualization of the results. The current fire warning and historical fire review systems for people shown in the report could be developed as a new capability for Australia. For the accuracy of the clustering analysis and the accuracy of the fire judgments, the current hotspot dataset is calibrated to be consistent with the historical dataset based on geographic location, matching time, and confidence level. This application discusses the 2008-2022 fire season in Victoria, Australia, and provides guidance on how to use this application, including for new bushfire alerts. These fire data are now available in a consistent form going back to the 2008 historical record, but the data for the new year’s fire season is not traceable at this time.

# Application file structure

- app.R
This file contains the front end and back end of the application. All the drawing and table code is in this file.

- Data
The data folder, as shown in the name, contains all the data used in this application. This data file does not contain the rawest data, it is too large to upload to git.

- Datacleaning.rmd
This file contains the code to try and clean the raw data, but it is currently not working as there is no raw data.

- Clustering.r
This file contains code for an attempt at clustering analysis and an attempt at how to output its results.

- www
This file contains all of the historical hotspot data motion graphics, which are stored in advance in order to reduce the length of the code and increase the speed of the code.

- Create_gif.rmd
This file contains the code to run the historical fire season hotspots from 2008 to 2022.


# Data

  * [Digital Earth Australia Hotspots(72-hours)](https://hotspots.dea.ga.gov.au/data/recent-hotspots.json)
  * [historical hotspot data](https://ga-sentinel.s3-ap-southeast-2.amazonaws.com/historic/all-data-csv.zip)
 
*All the files can be found in this GitHub repo:https://github.com/wyrbbd/Phascolomiheat.*
