function ymin = rotatevecsymin(xcorner, tc, pxrow, sgn)
%After a rotation, an empty triangle fills the bottom of the image. 
% If we try to collect particle distributions from inside that triangle,
% particle distributions will be skewed to one side since only one side 
% will contain part of the image. As such, we need to crop the bottom 
% of the image to avoid this skewing. This function finds the new bottom 
% of the image.

% INPUTS
% xcorner = point on the bottom edge where the corner of the old image hits
% tc = image rotation angle, adjusted
% pxrow = region [xmin xmax]
% sgn = -1 or 1, to adjust for different rotations

% OUTPUTS
% rstats = struct containing information about rotation

% CALLED BY: pivvideorun
% CALLS: rotatevecsymin

% Last modified February 2019 by Leanne Friedrich

pys = [0,0];
for i=1:2
    if sgn*(pxrow(i)-xcorner)>0
        pys(i) = abs(xcorner - pxrow(i))*tan(abs(pi/2 - tc));
    else
        pys(i) = abs(xcorner - pxrow(i))*tan(abs(tc));
    end
end
ymin = max(pys);
end