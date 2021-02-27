function drawnozzlefromc(c, scale, parent, im)
% Draws a box on the open figure. 

% INPUTS
% c = nozzle center coordinates [x y]
% scale = image scale in m/px
% parent = figure handle
% im = image array

% CALLED BY: opmain
% CALLS: drawbox

% Last modified February 2019 by Leanne Friedrich

    w = 0.00015/scale;
    c(2) = size(im,1) - c(2);
    inner = [c(1)-w, c(1)+w, c(2)-w, c(2)+w];
    outer = [c(1)-2*w, c(1)+2*w, c(2)-2*w, c(2)+2*w];
    drawbox(inner, 'red', parent);
    drawbox(outer, 'red', parent);
end
