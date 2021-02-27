function A = parsefile(file1)
% Imports a frame, converts to gray, and adjusts contrast.

% INPUTS
% file1 = an image filename or an image array

% OUTPUTS
% A = adjusted image array

% CALLED BY: opmain, read_pair_of_images_rect

% Last modified February 2019 by Leanne Friedrich

    if ischar(file1)
        if exist(file1, 2)
            A = imread(file1);
        else
            error('File does not exist');
        end
    elseif size(file1)>1
        A = file1;
    else
        error('First two inputs must be files or matrices');
    end
    if ndims(A) == 3
        A = rgb2gray(A);
    elseif ismatrix(A)
        A = double(A)/255;
    end
    A = imadjust(A);
end