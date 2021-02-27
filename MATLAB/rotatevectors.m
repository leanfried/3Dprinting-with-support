function [frotate, im2] = rotatevectors(vel, pvi, pli, fr, im, rstats, mode)
% Rotates flow field so that the print path is vertical, 
% extending from the nozzle to the bottom of the image.

% INPUTS
% vel is [u v] velocity of print path
% pvi is a struct containing variables used for PIV
% pli is a struct containing variables used for plotting
% fr = flow field in [x y u v]
% im = image
% rstats is a struct containing information about rotation
% mode = 0 for all, 1 for just vecs, 2 for just means (plotting)

% OUTPUTS
% rstats = struct containing information about rotation

% CALLED BY: pivvideorun
% CALLS: rotatevecsymin

% Last modified February 2019 by Leanne Friedrich


% plotdiag = 1 to pop up a separate window to show mean probe region
plotdiag = 1;
rotationmatrix = rstats.rotationmatrix;
center = rstats.center;

%% rotate vectors
frotate = fr;
if mode<2
    for i=1:length(frotate)
        rx = rotationmatrix*[(fr(i, 1:2)-center)'; 1];
        frotate(i, 1:2) = rx(1:2);
        if fr(i, 3)==0
            frotate(i, 3:4) = fr(i, 3:4);
        else
            rv = rotationmatrix*[fr(i, 3:4)'; 1];
            frotate(i, 3:4) = rv(1:2);
        end
    end
    frotate(:, 2) = -frotate(:,2);
    frotate(:, 4) = -frotate(:,4);

    if pli.plot>0
        % draw nozzle and vectors on upper plot
        axes(pli.ax2);
        cla(pli.ax2);
        hold off;
        inner = 0.0001*[-3 3 -3 3]/2;
        outer = 2*inner;
        if pli.plot==1 || pli.plot==3
            drawnozzle(inner, outer, pli.ax2);
            hold on;
            quiverm(frotate,'color','black','AutoScaleFactor',2);
        else
            hold on;
        end
    end
end

%% rotate image
% i and j are in plotting coordinates, but images put 0,0 at the top, so
% image indexing has to be subtracted from the height of the image to match
% positions

if mode==0 || mode==2

    if pli.plot==1 || pli.plot==2
        ce = pvi.nstats.center;
        x = 1:size(im,2);
        wn = 0.00015/pvi.sclt;
        if round(abs(vel(1))*100)/100>0
            slope = vel(2)/vel(1);
            t = abs(atand(slope));
            if vel(1)>0
                dy = -wn/cosd(t); %++
            else
                dy = wn/cosd(t); %--
            end
            ycenter = size(im,1) - (ce(2) + slope*(x - ce(1)));
            axes(pli.ax1);
            hold on;
            for i=-1:2:5
                plot(x, ycenter - i*dy, 'color', 'red');
            end
            hold off;
        else
            if vel(2)<0
                i1 = -5; 
                i2 = 1;
            else
                i1 = -1;
                i2 = 5;
            end
            axes(pli.ax1);
            hold on;
            for i = i1:2:i2
                line([ce(1) + wn*i,ce(1) + wn*i], [1, size(im,1)], 'color', 'red');
            end
            hold off;
        end

    end

    % rotate image
    theta = rstats.theta;
    im2 = imrotate(im, theta*180/pi);


    if plotdiag>0
        cr = rstats.cr;
        fprintf('cx %d, cy %d\n', cr(1), cr(2));
        f1 = figure;
        f1.Position = [10, 100, 960, 600];
        ax3 = axes;
        imshow(im2);
        hold on;
        drawnozzlefromc(cr, pvi.sclt, ax3, im2);
        for i=1:size(rstats.rpx,1)
            pxrow = rstats.rpx(i,:);
            if pxrow(4)>pxrow(3) && plotdiag
                rectangle('Position', [pxrow(1), size(im2,1)-pxrow(4), pxrow(2)-pxrow(1), pxrow(4)-pxrow(3)], 'EdgeColor', 'green');
            end
        end
    end

    if plotdiag>0
        close(f1);
    end
else
    im2 = [];
end
end