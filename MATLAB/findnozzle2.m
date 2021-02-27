function nstats = findnozzle2(video)

% Finds the nozzle in a frame by averaging a set of frames, rotating the
% averaged frame, and finding the position which aligns the original frame
% with the rotated frame

% INPUTS
% video = video file name

% OUTPUTS
% nstats = struct containing the coordinates of the nozzle center

% CALLED BY: pivvideorun
% CALLS: findtsfilename

% Last modified February 2019 by Leanne Friedrich


% bool: change this to 1 to pop up a figure that monitors the nozzle
% detection process
plot = 1; 

% get a frame before writing begins to determine the nozzle position
vread = VideoReader(video);

% find and read the timestamp file for this video
tsfilename = findtsfilename(video);
if ~exist(tsfilename, 'file')
    error('Timestamp file does not exist.');
end

% table: col1 = prism #, col2 = pass # (4 is the timestamp for the end of
% pass 3), col3 = timestamp in seconds
ts = csvread(tsfilename);

% cropped size
isize = [300, 300]; 
% crop position
dcrop = ([vread.Width, vread.Height] -isize)/2; 
% spacing between timestamps at which to collect frames    
flist = 0:0.3:1; 
% npics = number of timestamp pairs to step through
npics = 5;
% table for storing frame info    
imtable = zeros(isize(1)+1, isize(2)+1, npics*length(flist));  
% index for stepping through imtable
imi = 1;

%construct table of images
for i = 1:npics
    for f = flist % step through several times between those pairs
        vread.currentTime = f*ts(i,3) + (1-f)*ts(i+1,3); % get time
        im = readFrame(vread); % read frame
        im = imcrop(im, [dcrop isize]);     
        if size(im,3)>1
            im = rgb2gray(im);
        end
        im = imadjust(im);
        % store cropped and adjusted image in imtable
        imtable(:,:,imi) = im;
        imi = imi+1;
    end
end
im = mean(imtable(:,:,1:imi-1),3); % average all frames between the timestamps in the pair
% the overall effect here is to average several frames within a given
% pass around a polygon. This should blur out the actual particles and
% the print path, which change over the pass
% , but keep the nozzle, which remains constant

%plot images
if plot>0
    f = figure;
    f.Position = [10, 10, 600, 1500];
    subplot(3,1,1);
    im = 1-(im/max(max(im)));
    imshow(im);
end


imrot = imrotate(im, 90); %rotated image
if plot>0
    subplot(3,1,2);
    imshow(imrot);
    subplot(3,1,3);
end


%for a list of rotations, rotate the image, displace it,
%and take its product with its original image

% nozzle inner diagonal in px: note this is not the definition of w used throughout the
% rest of the code
w = round(sqrt(2)*0.0003/sbvscale());

% cxlist is a list of possible center coordinates to probe. we know the
% nozzle should be fully within the frame, so we only need to probe a few
% positions in the center of the image
margin = 1.5;
cxlist = round(margin*w+1):round(min(isize)-margin*w-1);

% table to store quality of alignment
clist = zeros(length(cxlist), length(cxlist));
for i = 1:length(cxlist)
    for j = 1:length(cxlist)
        cx = cxlist(i);
        cy = cxlist(j);
        
        %subsets of images of the size of the nozzle around the center
        %position5
        imt = im(cx-w:cx+w, cy-w:cy+w);
        rotimt = imrot((isize(2) - cy)-w:(isize(2) - cy)+w, cx-w:cx+w);
        
        % store the sum of the intensity difference in the ROIs
        imcombine = abs(imt-rotimt);
        clist(i,j) = sum(sum(imcombine))/sum(sum(imt));
    end
end

%find the smallest image intensity
[ci, cj] = find(clist==min(min(clist)));

%plot results
if plot>0
    cx = cxlist(ci);
    cy = cxlist(cj);
    imt = im(cx-w:cx+w, cy-w:cy+w);
    rotimt = imrot((isize(2) - cy)-w:(isize(2) - cy)+w, cx-w:cx+w);
    imcombine = abs(imt-rotimt);
    imshow(imcombine/max(max(imcombine)));
    subplot(3,1,1);
    hold on;
    scatter([cy], [cx]);
end
cx = cxlist(cj);
cy = cxlist(ci);

% get coordinates relative to the whole frame
center = [cx, cy] + dcrop;
center(2) = vread.Height - center(2);

nstats = struct('center', center);

% close the figure
if plot
    close(f);
end
