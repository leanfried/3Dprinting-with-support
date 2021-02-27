function phi = phiangle(prism, edgenum)
% Gets the angle phi, which is the angle between the printing 
% direction and the printer x-axis.

% INPUTS
% prism = prism number
% edgenum = edge number

% OUTPUTS
% phi = angle in degree

% CALLED BY: pivvideorun

% Last modified February 2019 by Leanne Friedrich

switch prism
    case 3
        angles = [150, 30, 270];
    case 4
        angles = [180, 90, 0, 270];
    case 5
        angles = [198, 126, 54, 342, 270];
    case 6
        angles = [210, 150, 90, 30, 330, 270];
    case 8
        angles = [225, 180, 135, 90, 45, 0, 315, 270];
end
phi = angles(edgenum);
end