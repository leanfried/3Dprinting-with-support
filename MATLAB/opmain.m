% -- Main loop --- %
% function [res, no_filt_res, filt_res] = opmain(pliclist,pvi,pli)
function [filt_res, a1] = opmain(pliclist,pvi,pli, mode)
% OPENPIV_MAIN_LOOP is the main PIV processing, post-processing and
% PIV.plotting subroutines calls
% file1 and file2 are file names
% pvi.ittWidth and pvi.ittHeight are interrogation window sizes in px
% pvi.ovlapHor and pvi.ovlapHor are overlap plixel sizes
% mode = 0 for all, 1 for just vecs, 2 for just means
% modified by Leanne Friedrich February 2019


% this modifies the original OpenPIV to only include parts of the image
% which are bright enough to contain particles. This is why it is very
% important not to fill holes for these videos
critintlow = 0.1;
critinthigh = 0.5;

% Prepare the results storage;
% row and cols are the positions of the regions to probe
a1 = parsefile(pliclist{1});
if mode==2
    filt_res = [];
else
    [verSize,horSize]= size(a1);
    rows = 1:pvi.ovlapVer:verSize - pvi.ittHeight + 1;
    cols =  1:pvi.ovlapHor:horSize - pvi.ittWidth + 1;
    numcols = length(rows);
    numrows = length(cols);
    res = zeros(numcols*numrows,5, length(pliclist)-1);

    for ii = 1:(length(pliclist)-1)
        file1 = pliclist{ii};
        file2 = pliclist{ii+1};
        [~,~,a1,b1,origin] = read_pair_of_images_rect(file1, file2,[0 0 0 0],...
            pvi.ittWidth,pvi.ittHeight,pvi.ovlapHor,pvi.ovlapVer);
        resind = 0;

        NfftWidth = 2*pvi.ittWidth;
        NfftHeight = 2*pvi.ittHeight;
        maxintensity = max(max(a1));

        for m = rows % vertically
            for k = cols % horizontally
                y = origin(2) + m + pvi.ittHeight/2-1;
                x = origin(1) + k + pvi.ittWidth/2-1;
                a2 = a1(m:m+pvi.ittHeight-1,k:k+pvi.ittWidth-1);
                if mean(mean(a2))>critintlow*maxintensity && max(max(a2))>critinthigh*maxintensity
                    b2 = b1(m:m+pvi.ittHeight-1,k:k+pvi.ittWidth-1);

                    % c is a NfftHeight x NfftWidth intensity matrix
                    c = cross_correlate_rect(a2,b2,NfftHeight,NfftWidth);

                    if any(c(:))
                        % peak1 and peak2 are signals, plixi and plixj are plixel indices
                        [peak1,peak2,plixi,plixj] = find_displacement_rect(c,pvi.s2ntype);

                        % refines the displacement down to sub-plixel resolution and
                        % determines signal to noise ratio
                        [peakVer,peakHor,s2n] = sub_pixel_velocity_rect(c,plixi,plixj,peak1,peak2,pvi.s2n1,pvi.ittWidth,pvi.ittHeight);

                        % Scale the pixel displacement to the velocity
                        u = (pvi.ittWidth-peakHor);
                        v = (pvi.ittHeight-peakVer);
                    else
                        % completely black, no correlation 
                        u = 0;
                        v = 0;
                        s2n = 0;
                    end
                else
                    u = 0;
                    v = 0;
                    s2n = 0;
                end
                resind = resind + 1;
                res(resind,:, ii) = [x y u v s2n];
            end
        end
    end

    res = mean(res,3);
    no_filt_res = res;

    % [res, filt_res] = openpiv_filter(res,numcols,numrows,pvi.out1);
    filt_res = openpiv_filter(res,numcols,numrows,pvi.out1);

    if pli.plot>0
        axes(pli.ax1);
        imshow(a1, 'Parent', pli.ax1);
        hold on
        drawnozzlefromc(pvi.nstats.center, pvi.sclt, pli.ax1, a1);
        if pli.plot==1 || pli.plot==3       
            quiverm(filt_res,'color','g','AutoScaleFactor',2);
        end
        hold off
        ind = (filt_res(:,3) ~= no_filt_res(:,3) | filt_res(:,4) ~= no_filt_res(:,4));
        if ~any(ind)
            quiverm(no_filt_res(ind,:),'color','r','AutoScaleFactor',1.25);
        end
    end
end
