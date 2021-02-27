function video = findvideofile(datafolder, type, ink, v, slide)
% Finds video file using systematic file names

% INPUTS
% datafolder = string data folder
% type = support type 'D2' or 'D3'
% ink = ink composition (820, 830, etc)
% v = print speed (3,6,9,12)
% slide = slide #

% OUTPUTS
% video file name

% CALLED BY: pivvideorun
% CALLS: printfilename

% Last modified February 2019 by Leanne Friedrich


%     folder = strcat(datafolder, '\u', num2str(ink), '_v_', num2str(v),...
%         '_slide_', num2str(slide));
    folder = strcat(datafolder, '\videos');
    printfilename(folder);
    if ~isfolder(folder)
        fprintf('Folder does not exist.\n');
        error('Folder does not exist.');
    end
    videos = ls(strcat(datafolder, '\videos\*', type, '_u', num2str(ink),...
        '_slide_', num2str(slide), '_v_', num2str(v),...
         '*mp4'));
    if isempty(videos)
        fprintf('No videos of that type in folder\n');
        error('No videos of that type in folder');
    end
    video = videos(end, :);
    video = strcat(folder,'\', video);
    printfilename(video);
end
