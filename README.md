Code and data for:
"Changes in filament microstructures during direct ink writing with yield stress fluid support" https://doi.org/10.1021/acsapm.0c00126
"Printing direction dependent microstructure in direct ink writing"  https://doi.org/10.1016/j.addma.2020.101192
"Corner accuracy in direct ink writing with support material" https://doi.org/10.1016/j.bprint.2020.e00086

Additional files, including the videos from examplevids, are at Mendeley Data: http://dx.doi.org/10.17632/b6ybb9ytb5.1

PIV code in MATLAB is adapted from OpenPIV circa 2018: http://www.openpiv.net/. The MATLAB version of OpenPIV will no longer be updated as of 2021.


32_16_3_2_combined
Raw output files from MATLAB modified PIV code, where each file is a video recording the printing of several prisms, and each row is a frame.

corrpoints
Averaged points for plotting correlations between dependent variables.

Examplevids
Example videos and timestamp lists indicating breaks between prisms and passes.

Mathematica
Mathematica code files for analyzing and plotting data.

MATLAB 
Code for getting flows and particle distributions from videos. Adapted from OpenPIV http://www.openpiv.net/

Tables
Tables of data


----------------------------

Videos were collected from underneath the print nozzle during direct ink writing of polymer matrix composites with acoustophoresis and yield stress fluid support. Particle image velocimetry and digital image analysis are used to characterize the flow field around the nozzle and particle distributions in the printed lines, via MATLAB. Data is analyzed and plotted using Mathematica.


In the folder '32_16_3_2_combined', csv files show frame-by-frame results of PIV and digital image analysis measurements which indicate key metrics of the flow field and particle distributions in the region near the nozzle. In the folder 'Examplevids', mp4 files show a subset of the raw videos that the csv files come from, and csv files show timestamps for the beginning of each pass of each prism. In the file names, 'gras' indicates that videos were collected with a PointGrey Grasshopper camera. 'D2' indicates layer-by-layer support, 'D3' indicates bath support, and 'deep3' indicates a deep bath. 'deep3' data are not used in any of the papers. 'u8XX' indicates a UDMA-based ink with 8 wt% fumed silica in the UDMA-silica base and XX wt% TEGDMA in the base-TEGDMA-particles-photoinitiators ink. Polygons were printed on 4 pre-designed slides containing a specific set of polygons in specified locations. 'v_Y' indicates the translation and flow speeds were Y mm/s. '######_######' is a time stamp. 'prism#' indicates the prism number on the given slide. As noted, slides contain a specified set of polygons in a pre-set order. 32 px is the size of the PIV interrogation region. 16 px is the spacing between regions. 3 is the number of sequential frames to use in each PIV measurement. 2 is a vestigial data collection mode; PIV and particle distribution measurements were collected separately and then combined into single files.

The folder 'MATLAB' contains the MATLAB '.m' functions and script used to measure flow fields and particle distributions and produce the tables in the folder '32_16_3_2_combined'. MATLAB files were written in MATLAB R2018b.

The folder 'Mathematica' contains '.wl' packages and '.nb' notebooks used to analyze the files in the folder '32_16_3_2_combined' and to produce the files in 'Tables'. Files also contain the code used to produce the figures  and tables in the three mentioned papers. Files were written in Wolfram Mathematica 11.1.

The folder 'Tables' contains several summaries of the data. The README in that folder contains more description of the tables.
