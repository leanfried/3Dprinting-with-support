function [m, s] = mster(dist, pos)
% Mean and standard error of a distribution

% INPUTS
% dist = list of intensities
% pos = list of positions

% OUTPUTS
% m = mean
% s = standard error

% CALLED BY: findpeakshist

% Last modified February 2019 by Leanne Friedrich


s1 = size(dist);
if s1(2)>1
    if s1(1)>1
        error('First input must be vector');
    else
        dist = dist';
        s2 = size(pos);
        if s2(1)>1
            if s2(2)>1
                error('Second input must be vector');
            else
                pos = pos';
            end
        end
    end
end

% dist is n x 1, pos is 1 x n

if numel(dist)~=numel(pos)
    error('Inputs are wrong size');
end
dist = dist/sum(dist);
m = pos*dist;

s = sqrt((pos - m).^2*dist);

end