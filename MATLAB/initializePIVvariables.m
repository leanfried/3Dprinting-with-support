function pivinputs = initializePIVvariables(isize, overlap)
% Creates a struct to hold the input variables for PIV.

% INPUTS
% isize = interrogation region size
% overlap = spacing between interrogation regions

% OUTPUTS
% pivinputs = struct containing information for PIV

% CALLED BY: pivvideorun

% Last modified February 2019 by Leanne Friedrich
    if isize>0
        ittWidth = isize;
        ittHeight = isize;
    else
        ittWidth = 32; %interrogation window size
        ittHeight = 32;
    end
    if overlap>0
        ovlapHor = overlap;
        ovlapVer = overlap;
    else
        ovlapHor = 32; %overlap size
        ovlapVer = 32; 
    end
    s2ntype = 2; %S/N type
    s2n1 = 1; %S/N value
    out1 = 100; %outlier filter
    
    pivinputs = struct('ittWidth', ittWidth, 'ittHeight', ittHeight,...
        'ovlapHor', ovlapHor, 'ovlapVer', ovlapVer, 's2ntype', s2ntype,...
        's2n1', s2n1, 'out1', out1);
end