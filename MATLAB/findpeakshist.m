function vacross = findpeakshist(imr, rpx, i, pvi, pli, cr, hregions, linen, w, color)
% Finds particle distribution metrics in a frame

% INPUTS
% imr = rotated image
% rpx = coordinates of regions ahead and behind the nozzle
% i = region number
% pvi = struct containing video scaling information
% pli = struct containing plotting information
% cr = center coordinates in px
% hregions = list of histogram regions
% linen = number of peaks to look for
% w = nozzle half-width
% color = color to plot peaks in

% OUTPUTS
% vacross = a row of peak measurements

% CALLED BY: pivrow
% CALLS: histpos

% Last modified February 2019 by Leanne Friedrich

[hist, pos] = histpos(imr, rpx, i, pvi, pli, cr, color);
% on the last loop, take the whole region and find the peaks
vacross = zeros(9,1);
vind = 1;
mln = linen-(i-(size(rpx,1)-1));
if numel(hist)>1    
    [pks, locs1] = findpeaks(hist, 'SortStr','descend',...
                        'Npeaks', mln+2, ...
                        'MinPeakDistance',...
                        numel(hist)/((hregions(end, 2) - hregions(end,1))/(w)));
    % sort by peak value
    lcomb = sortrows([pks;locs1]',1,'descend');
    if size(lcomb,1)>= mln
        if size(lcomb,1)== mln
            locs = sort(locs1);
        elseif size(lcomb,1)> mln
            if lcomb(mln+1, 1)<0.6*lcomb(mln,1)
                % if the surplus peaks aren't very large, just take the
                % strongest linen peaks
                locs = sort(lcomb(1:mln, 2));
            else
                % if the surplus peaks are large, 
                % sort by position
                lcomb = sortrows([pks;locs1]',2);
                % if the first peak is too far to the left or smaller than the left
                % edge, throw it out
                if lcomb(1, 2)<0.00015/pvi.sclt || lcomb(1,1)<hist(1)
                    lcomb = lcomb(2:end, :);
                end
                critlist = (max(pks)./lcomb(:,1)).^3'.*(abs(pos(lcomb(:,2)))./max(abs(pos(lcomb(:,2)))));
                leftind = min(find(critlist==min(critlist)),size(lcomb,1)-(linen-1));
                if leftind<1 || leftind+linen-1>size(lcomb,1)
                    return 
                end
                locs = lcomb(leftind:leftind+(linen-1), 2)';
            end
        end
        % collect 2 extra peaks. the line 0 peak is the tallest one
        % closest to the center
        pwidth = w/pvi.sclt;
        for j=1:min(mln, numel(locs))
           left = locs(j) - pwidth;
           right = locs(j) + pwidth;
           left = max(min(round(left), length(hist)),1);
           right = max(min(round(right), length(hist)),1);
           htake = hist(left:right);
           postake = pos(left:right);
           [m,s] = mster(htake, postake);
           if pli.plot==1
               if i==1
                   range = [-4, -3]*10^-3;
               else
                   range = [-3, -2]*10^-3;
               end
               plot([m, m], range, 'color', color, 'LineWidth', 3);
           elseif pli.plot==2
               plot([m, m], [i-1,i],'--', 'color', color, 'LineWidth', 3);
           end
           [~, smax] = mster(ones(size(htake,1), size(htake,2)), postake);
            % smax is not actually the maximum possible standard deviation.
            % it is the standard deviation of a uniform distribution
%            smax = (postake(end) - postake(1))/2;
%            hmock = zeros(size(htake));
%             hmock(1) = 1;
%             hmock(end) = 1;
%             [~, smax] = mster(hmock, postake);
%            if s>smax
%                fprintf('s %d, smax %d\n', s*1000, smax*1000);
%            end
           vacross(vind:vind+2) = [m,s,s/smax/1000];
           vind = vind+3;
        end
    end
end
end