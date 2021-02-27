function pivvideorun(datafolder, type, ink, v, slide, plot, isize, overlap, picchunk, mode, forceexist)
%  Runs through a set of frames in each video, where each video consists 
% of several polygons which have manually determined time stamps. Frames 
% are evenly collected from each edge of each pass of each polygon.

% INPUTS
% datafolder = data folder string
% type = support type 'D2', 'D3', or 'deep3'
% ink = ink composition
% v = print speed
% slide = slide number
% plot 0 to not plot anything, 1 for both, 2 for just histograms, 3 for
% just vectors
% isize is the interrogation window size
% overlap is the spacing between windows
% picchunk is how many frames to use
% mode = 0 for all, 1 for just vecs, 2 for just means to collect data
% forceexist = true to overwrite old csv files


% OUTPUTS
% exports csv files to data folder

% CALLED BY: pivrunscript
% CALLS: convertprismnumber, createPIVfigure, findnozzle2, findtsfilename,
% findvideofile, getregions, getrotation, initializePIVvariables, phiangle,
% pivrow, printfilename, sbvscale, slidestats, stagevelocities

% Last modified February 2019 by Leanne Friedrich

try
    video = findvideofile(datafolder, type, ink, v, slide);
catch
    return;
end

if picchunk<2
    picchunk = 2;
end
switch type
    case 'D2'
        typei = 2;
    case 'D3'
        typei = 3;
    case 'deep3'
        typei = 4;
end

pli = createPIVfigure(plot);
pvi = initializePIVvariables(isize, overlap);

% run from 30% to 70% of each line, spacing 10%
% t0s = 0.1;
dts = 0.01;
t0s = 0.45;
% dts = 1;
tfs = 1 - t0s;
timevec = t0s:dts:tfs;
% timevec = [0.1:0.02:0.2, 0.8:0.02:0.9];

%calibrate scale
tsfilename = findtsfilename(video);
if ~exist(tsfilename, 'file')
    error('Timestamp file does not exist.');
end
ts = csvread(tsfilename);
ts = ts(ts(:,3)>0,:);
vr = VideoReader(video);
pvi.dt = 1/vr.FrameRate;
imsize = [vr.Height, vr.Width];
[~, sides, ~] = slidestats(slide); % number of sides for each prism
numps = length(sides); % number of prisms
pvi.sclt = sbvscale(); % m per pixel
vabs = 0.001*v*pvi.dt/pvi.sclt; % pixels per frame
pvi.nstats = findnozzle2(video);

if pvi.sclt <=0 || pvi.dt <= 0
    error('Invalid scale or timestep');
end

[~, vregions, hregions] = getregions();
rowsize = size(vregions,1)*3 + (size(hregions,1)-2)*3 + 9 + 6 + 7 + 7;
resi = 1;
% loop through timestamp rows
firstts = 11;
for i=firstts:size(ts,1)
    % line
    if ts(i,2)==1 || i==firstts
        % start a new file for this prism
        % number of res rows * number of sides for this prism * number of
        % rows per side
        csvname = strrep(video, '.mp4', strcat('_prism', num2str(ts(i,1)),...
            '_', num2str(isize), '_', num2str(overlap),'_', num2str(picchunk), '_',...
            num2str(mode),'_vecs2.csv'));
        if exist(csvname, 'file') && ~forceexist
            % if file already exists, loop until the next polygon
            collectandexport = 0;
        else
            collectandexport = 1;
            res = zeros(sides(ts(i,1))*length(timevec), rowsize);
            resi = 1;
        end
    end
    if collectandexport
        if ts(i,2)<4
            prism = ts(i,1);
            numsides = sides(prism);
            ss = stagevelocities(numsides);
            vels = ss*vabs; %vector traveled in one dt
            cornertimes = ts(i,3):((ts(i+1,3)-ts(i,3))/numsides):ts(i+1,3);
            [edgelength, nsides, cornerangle] = convertprismnumber(slide, ts(i,1));
            el = edgelength - 2*(0.3*(3-ts(i,2)))*tan(pi/nsides);
            % edge
            for j=2:(size(cornertimes,2)-1)
                fprintf('\tPrism %d/%d, Line %d/3, Edge %d/%d\n', prism, numps, ts(i,2), j, numsides);
                pictimes = cornertimes(j) + (cornertimes(j+1) - cornertimes(j))*timevec;
                rstats = getrotation(vels(j,:), pvi, imsize, hregions);
                rp1 = rstats.rpx;
                cr = rstats.cr; % in px
                phi = phiangle(nsides, j);
                for k = 1:size(pictimes,2)
                    if plot && pli.c1.Value
                        waitfor(pli.c1, 'Value');
                    end
                    vr.CurrentTime = pictimes(k);
                    piclist = cell(picchunk,1);
                    for l = 1:picchunk
                        piclist{l} = readFrame(vr);
                    end
                    pt = pictimes(k); %time
                    ptime = pt - cornertimes(j); %time from corner
                    pdist = ptime*v; %distance from corner
                    miny = round(cr(2)-(pdist)*10^-3/pvi.sclt); % position of polygon corner
                    rp2 = rp1;
                    rp2(1,3) = max(rp1(1,3), miny);
                    rp2(1,4) = max(rp1(1,4), miny);                   
                    maxy = round(cr(2)+(el - (pdist))*10^-3/pvi.sclt);
                    rp2(2,3) = min(rp1(2,3), maxy);
                    rp2(2,4) = min(rp1(2,4), maxy);
                    rstats.rpx = rp2;
                    pr = pivrow(piclist, pvi, pli, vels(j, :), ts(i,2), rstats, mode);
                    res(resi, :) = [ink, v, typei, edgelength, nsides,...
                        cornerangle, phi, prism, ts(i,2), j, ...
                        pt, pt*v, ptime, pdist, pr];
                    % ink, v, type, prism, line, edge, time, distance, time
                    % from corner, distance from corner, vxvyN regions, mean
                    % stdev normalized stdev regions
                    resi = resi+1;
    %                 print(pli.f, strcat('C:\Users\Leanne\Desktop\figures\fig', num2str(resi), '.tiff'),'-dtiff');
                end
            end
        else
            % done with this prism
            % positive vx is to the right
            % positive vy is up
%             csvwrite(csvname, res);
            printfilename(csvname);
        end
    end
end

if plot>0
    close(pli.f);
end
clear;

fprintf('-------Finished-------\n');

end

