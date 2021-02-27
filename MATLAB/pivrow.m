function vacross = pivrow(piclist, pvi, pli, vel, linen, rstats, mode)
% Gets flow field with PIV and particle distribution peaks with digital 
% image analysis.

% INPUTS
% piclist = list of images to compare
% pvi is a struct containing variables used for PIV
% pli is a struct containing variables used for plotting
% vel is [u v] velocity of print path
% linen = pass number
% rstats is a struct containing information about rotation
% mode = 0 for all, 1 for just vecs, 2 for just means to collect data

% OUTPUTS
% vacross = a row of statistics about the flow field and particle
% distributions

% CALLED BY: pivvideorun
% CALLS: opmain, getregions, rotatevectors, findpeakshist

% Last modified February 2019 by Leanne Friedrich

[fr, a1] = opmain(piclist, pvi, pli, mode);
if pvi.sclt <=0 || pvi.dt <= 0
    error('Invalid scale or timestep');
end

if mode<2
    % convert filtered from px/frame to m/s
    fr = fr*pvi.sclt;
    fr(:,3:4) = fr(:,3:4)/pvi.dt;
end


%% select ROIs
%x left, x right, yleft, yright

 [w, vregions, hregions] = getregions();
% 
% %rotate
 [frotate, imr] = rotatevectors(vel, pvi, pli, fr, a1, rstats, mode);
% 
% %only include vectors that are at least 10% of the max length
if mode<2
    vcritx = max(abs(frotate(:,3)))*0.1;
    vcrity = max(abs(frotate(:,4)))*0.1;
end
nregions = size(vregions,1);

rpx = rstats.rpx;
vacross = zeros(1, nregions*3 + (size(rpx,1)-2)*3 + 9 + 6);
vind = 1;

%% vectors
% vectors are reported as average dx, average dy
for i = 1:nregions
    if mode<2
        if pli.plot>0 && pli.plot~=2
            drawbox(vregions(i,:), 'blue', pli.ax2);
        end
        vlog = boolean((frotate(:,1)>=vregions(i, 1)).*...
            (frotate(:,1)<=vregions(i, 2)).*...
            (frotate(:,2)>=vregions(i, 3)).* ...
            (frotate(:,2)<=vregions(i, 4)).*...
            ((abs(frotate(:,3))>vcritx) + (abs(frotate(:,4))>vcrity)));
        fselect = frotate(vlog, :);
        if size(fselect,1)<1
            for j = 1:2
                vacross(vind) = 0;
                vind = vind+1;
            end
        else
            for ind = 3:4
                vacross(vind) = median(fselect(:,ind));
                vind = vind+1;
            end
        end
        vacross(vind) = size(fselect,1)/1000;
    else
        vind = vind+2;
    end
    vind = vind+1;
end

%% intensities
if mode==0 || mode==2
    if pli.plot>0
        axes(pli.ax2);
        hold on;
        if pli.plot==2
            for dx = [0, 0.3, 0.6, 0.9]
                line([-0.15+dx, -0.15+dx]*10^-3, [0,2], 'Color', [0.5, 0.5, 0.5], 'Parent', pli.ax2);
            end
        end
    end
    cr = rstats.cr;
    %histograms of individual regions
    % for i=1:3
    %    if i<=linen
    %        [hist, pos] = histpos(imr, rpx, i, pvi,  pli, cr);    
    %        if numel(hist)>1
    %            [m,s] = mster(hist, pos);
    % %            [~, smax] = mster(ones(size(hist,1), size(hist,2)), pos);
    %             smax = (pos(end) - pos(1))/2;
    %             if pli.plot==1
    %                plot([m, m], [-4,-3]*10^-3, 'color', 'red', 'LineWidth', 3);
    %             elseif pli.plot==2
    %                plot([m, m], [0,1],'--', 'color', 'black', 'LineWidth', 3);
    %             end
    %        end
    %        vacross(vind:vind+2) = [m,s,s/smax/1000];
    %    end
    %    vind = vind+3;
    % end
    % histogram of whole region
    imr = imadjust(imtophat(imr,strel('disk',15))); %remove background
    if pli.plot==1
        color =[0, 0.5, 0];
    else
        color = 'black';
    end
    vacross(vind:vind+8) = findpeakshist(imr, rpx, size(rpx, 1)-1, pvi, pli, cr, hregions, linen, w, color);
    % findpeakshist(im3, rpx, size(rpx, 1)-1, pvi, pli, cr, hregions, linen, w, 'red');
    vind = vind+9;
    if linen>1
        fph = findpeakshist(imr, rpx, size(rpx, 1), pvi, pli, cr, hregions, linen, w, color);
        vacross(vind:vind+5) = fph(1:6);
    %     findpeakshist(im3, rpx, size(rpx, 1), pvi, pli, cr, hregions, linen, w, 'red');
    end


    if pli.plot>0
        hold off;
    end
end

% vacross is in m/s: convert to mm/s
vacross = vacross*1000;

