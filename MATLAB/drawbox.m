function drawbox(inner, color, parent)
% Draws a box on the open figure. 

% INPUTS
% inner = coordinates of square corners [x1 x2 y1 y2]
% color = line color
% parent = figure handle

% CALLED BY: drawnozzlefromc

% Last modified February 2019 by Leanne Friedrich

    line([inner(1), inner(2)], [inner(3), inner(3)], 'Color', color, 'Parent', parent);
    line([inner(1), inner(2)], [inner(4), inner(4)], 'Color', color, 'Parent', parent);
    line([inner(1), inner(1)], [inner(3), inner(4)], 'Color', color, 'Parent', parent);
    line([inner(2), inner(2)], [inner(3), inner(4)], 'Color', color, 'Parent', parent);
end