function tsfilename = findtsfilename(video)

% Finds the timestamp file name for a given video file.

% INPUTS
% video = video file name (mp4)

% OUTPUTS
% tsfilename = timestamp file name (csv)

% CALLED BY: pivvideorun, findnozzle2

% Last modified February 2019 by Leanne Friedrich

tsfilename = strrep(video, '.mp4', '_times.csv');
tsfilename = strrep(tsfilename, 'videos', 'times');
end