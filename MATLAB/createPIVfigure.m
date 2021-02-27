function plotinputs = createPIVfigure(plot)
% Creates a MATLAB figure to track the PIV and particle distribution 
% detection process.

% INPUTS
% plot = 0 to not plot anything, 1 for both, 2 for just histograms, 3 for
% just vectors

% OUTPUTS
% plotinputs is a struct with information about the created figure

% CALLED BY: pivvideorun

% Last modified February 2019 by Leanne Friedrich

if plot>0
    f = figure;
    set(f,'color','w');
    set(0,'units','pixels') 
    Pix_SS = get(0,'screensize');
    hmin = 700;
    height = min([hmin, Pix_SS(4)-300]);
    sc = height/hmin;
    width = 1425*sc;
    f.Position = [10 100 width height];
    c1 = uicontrol(f, 'Style', 'togglebutton','String', 'Pause', 'Value', 0, 'Position', [100, hmin*0.95*sc, 100, 30]);
    uicontrol(f, 'Style', 'pushbutton', 'String', 'Quit', 'Callback', {@quitpvr, f}, 'Position', [400, hmin*0.95*sc, 100, 30]);
    ax1 = axes(f);
    ax1.Units = 'pixels';
    ax1.Position = [400*sc 50 960*sc 600*sc];
    
    ax2 = axes(f);
    ax2.Units = 'pixels'; 
    ax2.Position = [50 50 300*sc 600*sc]; 
    if plot==2
        axis(ax2, [-0.5, 1, 0, 2000]*0.001);
        ylabel('intensity');
        axis(ax2,'square');
    else
        axis(ax2, [-1, 1, -4, 1]*0.001);
        ylabel('y (m)');
        daspect([1 1 1]);
    end
    xlabel('x (m)');
else
    ax1 = 0;
    ax2 = 0;
    c1 = struct('Value', 0);
    f = 0;
end
plotinputs = struct('f', f,'plot', plot,'ax1',  ax1,'ax2', ax2, 'c1', c1);
end

function quitpvr(~,~, f)

close(f);
return

end