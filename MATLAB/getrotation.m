function rstats = getrotation(vel, pvi, imsize, hregions)
% Gets statistics about the rotated image

% INPUTS
% vel = print velocity in [x,y] coords
% pvi = struct containing information for PIV
% imsize = image size
% hregions = list of histogram detection regions

% OUTPUTS
% rstats = struct containing information about rotation

% CALLED BY: pivvideorun
% CALLS: rotatevecsymin

% Last modified February 2019 by Leanne Friedrich


theta = acos(vel(2)/norm(vel));
if vel(1)<0
    theta = -theta;
end

% vector rotation
vtheta = -theta;
nstats = pvi.nstats;
center = nstats.center;
center(2) = imsize(1) - center(2);
center = center*pvi.sclt;
rotationmatrix = [cos(vtheta), -sin(vtheta), 0; ...
    sin(vtheta), cos(vtheta), 0; 0,0,1];

% image rotation
c = nstats.center;
if abs(theta)>pi/2
    tc = sign(theta)*pi-theta;
else
    tc = theta;
end
h = imsize(1);
w = imsize(2);
cx = c(1);
cy = c(2);
if vel(1)<0
    if vel(2)<0
        A = [-cy, w-cx; -(w-cx), h-cy]; %--
        
    elseif vel(2)>0
        A = [-cy, cx; -(w-cx), cy]; %-+
    else
        A = [cy, 0; w-cx, 0]; %-0
    end
elseif vel(1)>0
    if vel(2)<0
        A = [h-cy, w-cx; cx, h-cy]; %+-
    elseif vel(2)>0
        A = [(h-cy), cx; cx, cy]; %++
    else
        A = [cy-h, 0; -cx, 0]; %+0
    end
else
    if vel(2)<0
        A = [0, cx-w; 0, cy-h]; %0-
    else
        A = [0, cx; 0, cy]; %-+
    end
end
cr = A*[sin(tc); cos(tc)];

rpx = hregions/pvi.sclt; % regions in pixels
rpx(:,1) = rpx(:,1) + cr(1);
rpx(:,2) = rpx(:,2) + cr(1);
rpx(:,3) = rpx(:,3) + cr(2);
rpx(:,4) = rpx(:,4) + cr(2);

im2size = [abs(w*sin(tc))+abs(h*cos(tc)),abs(w*cos(tc))+abs(h*sin(tc))]; 

if mod(theta, pi/2)~=0
    cornerptx = w*cos(tc);
    if (0<theta && theta<pi/2) || (-pi<theta && theta<-pi/2)
        cornerptx = im2size(2) - cornerptx;
        sgn = -1;
    else
        sgn = 1;
    end
    for i=1:size(rpx,1)-1
        rpx(i,3) = rotatevecsymin(cornerptx, tc, rpx(i, :), sgn);
    end
    rpx(end,4) = im2size(1)-rotatevecsymin(im2size(2)-cornerptx, tc, rpx(end,:), -sgn);
else
    for i=1:size(rpx,1)-1
        rpx(i,3) = 0;
    end
    rpx(end,4) = im2size(1);
end

rpx = round(rpx);

rstats = struct('center', center, 'rotationmatrix', rotationmatrix, ...
   'theta', theta, 'rpx', rpx, 'cr', cr);

end