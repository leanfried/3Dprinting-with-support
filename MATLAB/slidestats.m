function [centers, sides, edges] = slidestats(index)
    switch index
        case 1
            centers = [ 20.1, 7.8; 35.1, 9; 54.5, 10.2; 54.8, 29.4; 34.8, 28.3; 12, 26.7];
            sides = [5, 6, 6, 5, 5, 5];
            edges = [6, 6, 8, 8, 10, 12];
        case 2
            centers = [24.2, 12.4; 50.4, 14.3;  51, 34.1; 38.9, 33; 24.5, 32.2; 8.9, 31];
            sides = [6,6,3,3,3,3];
            edges = [10, 12, 6, 8, 10, 12];
        case 3
            centers = [16.4, 22.8; 36, 8.4; 54.6, 10.1; 58.4, 33.4; 42.6, 28.4];
            sides = [8, 4, 4, 4, 8];
            edges = [10, 10, 12, 6, 6];
        case 4
            centers = [18.2, 20.9;  57.5, 7; 51.3, 26.3];
            sides = [8, 4, 8];
            edges = [12, 8, 8];
    end
end