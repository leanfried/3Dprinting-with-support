function [hist, pos] = histpos(imr, rpx, i, pvi, pli, center, color)
% Crops the image to the region of interest and gets the intensity 
% histogram across the print path.

% INPUTS
% imr = rotated image
% rpx = coordinates of regions ahead and behind the nozzle in px
% i = region number
% pvi = struct containing video scaling information
% pli = struct containing plotting information
% center = center coordinates in px
% color = color to plot peaks in

% OUTPUTS
% hist = list of intensities
% pos = list of positions in m

% CALLED BY: findpeakshist


% crop image to region of interest
    %xmin, dx, ymin, dy are in px
   xmin = max(rpx(i,1),1);
   dx = rpx(i,2) - rpx(i,1);
   ymin = max(rpx(i,3),1);
   dy = min(rpx(i,4) - ymin, size(imr, 1));
   ymin = size(imr,1) - ymin - dy;
   if dy>0
       imcr = imcrop(imr, [xmin, ymin, dx, dy]);
       % get histogram of intensity
       hist = sum(imcr); 
       hist = hist-min(hist);

       % positions in the distribution in m
       pos = (1:size(hist,2)) - (center(1)-xmin);
       pos = pos.*pvi.sclt; 

       if pli.plot>0 && pli.plot<3
           xvecs = pos;
           if pli.plot==2
               yvecs = (hist/max(hist));
           else
               yvecs = (hist/max(hist) - 4)*10^-3;
           end
           if i==size(rpx,1)
                if pli.plot==2
                    colo = 'black';
                    yvecs = yvecs+1;
                else
                    colo = [0, 0.5, 0];
                    yvecs = yvecs+0.001;
                end
           else
               if pli.plot==2
                    colo = 'black';
               else
                    colo = 'red';
               end
           end
           plot(xvecs, yvecs, 'color', color);
       end
   else
       hist = [];
       pos = [];
   end

end