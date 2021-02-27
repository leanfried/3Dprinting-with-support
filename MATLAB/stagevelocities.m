function stagespeeds = stagevelocities(n)
% Finds the velocities in x-y coordinates of each edge of the polygon.

% INPUTS
% n = number of sides

% OUTPUTS
% stagespeeds = list of stage speeds

% CALLED BY: pivvideorun

% Last modified February 2019 by Leanne Friedrich

radii = 1;
angles = (pi/n:2*pi/n:(2*pi+pi/n));
numpoints = [length(radii)*length(angles),1];
radii = reshape(radii, [length(radii), 1]);
points = [reshape((radii*cos(angles))', numpoints), reshape((radii*sin(angles))', numpoints)];
points = -fliplr(points);
stagespeeds = zeros(n,2);
% centerdirs = stagespeeds;
for i=1:n
    v = points(i+1,:) - points(i,:);
    stagespeeds(i, :) = v/norm(v);
%     mp = mean(points(i:i+1, :));
%     centerdirs(i,:) = [stagespeeds(i,2), -stagespeeds(i,1)];
%     if mp(1)<0 || (mp(1)==0 && mp(2)<0)
%         % center is to the right
%         if centerdirs(i,1)<0
%             centerdirs(i,:) = -centerdirs(i,:);
%         end
%     else
%         % center is to the left
%         if centerdirs(i,1)>0
%             centerdirs(i,:) = -centerdirs(i,:);
%         end
%     end
end

            
end