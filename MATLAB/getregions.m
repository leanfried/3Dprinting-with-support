function [w, vregions, hregions] = getregions()
% Gets a list of region coordinates in microns.

% OUTPUTS
% w = nozzle half-width in um
% vregions = vector regions
% hregions = histogram regions

% CALLED BY: pivvideorun, pivrow

% Last modified February 2019 by Leanne Friedrich

w = 0.0003/2; %w is the nozzle half-width
vregions = [-1, 1, -1, 1; %in nozzle
    -2, 2, 1, 4; %ahead of nozzle up to one outer nozzle width
    -2, 2, -4, -1; % immediately behind nozzle
    -5, -1, -20, -3; %swung out left, behind
    -1, 1, -20, -3; %behind nozzle, line 0
    1, 3, -20, -3; % behind nozzle, line 1
    3, 5, -20, -3;% behind nozzle, line 2
    -5, -1, -3, 3; %swung out left
    1, 3, -3, 3;%right of nozzle, line 1
    3, 5, -3, 3; % right of nozzle, line 2
    ]*w;
 hregions = [
% -1, 1, -20, -5; %behind nozzle, line 0
%   1, 3, -20, -5; % behind nozzle, line 1
%   3, 5, -20, -5;% behind nozzle, line 2
  -3, 7, -20, -5
  0, 7, 5, 20
]*w;
end