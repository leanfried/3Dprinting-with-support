function [edgelength, sides, cornerangle] = convertprismnumber(slide, prism)
% Several polygons are printed on each slide. 
% This function outputs the size, shape, and corner angle of the 
% requested prism number on the requested slide number.

% INPUTS
% slide = slide number 1-4
% prism = prism number

% OUTPUTS
% edgelength = length of polygon edge in mm
% sides = # of sides of the polygon
% cornerangle = corner angle in degree

% CALLED BY: pivvideorun

% Last modified February 2019 by Leanne Friedrich



switch slide
    case 1
        switch prism
            case 1
                edgelength = 6; sides = 5;
            case 2
                edgelength = 6; sides = 6;
            case 3
                edgelength = 8; sides = 6;
            case 4
                edgelength = 8; sides = 5;
            case 5
                edgelength = 10; sides = 5;
            case 6 
                edgelength = 12; sides = 5;
        end
    case 2
        switch prism
            case 1
                edgelength = 10; sides = 6;
            case 2
                edgelength = 12; sides = 6;
            case 3
                edgelength = 6; sides = 3;
            case 4
                edgelength = 8; sides = 3;
            case 5
                edgelength = 10; sides = 3;
            case 6 
                edgelength = 12; sides = 3;
        end
    case 3
        switch prism
            case 1
                edgelength = 10; sides = 8;
            case 2 
                edgelength = 10; sides = 4;
            case 3
                edgelength = 12; sides = 4;
            case 4
                edgelength = 6; sides = 4;
            case 5
                edgelength = 6; sides = 8;
        end
    case 4
        switch prism
            case 1
                edgelength = 12; sides = 8;
            case 2
                edgelength = 8; sides = 4;
            case 3
                edgelength = 8; sides = 8;
        end
end
cornerangle = (sides-2)*180/sides;
end