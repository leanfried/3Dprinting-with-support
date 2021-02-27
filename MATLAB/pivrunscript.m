datafolder = 'FOLDERNAME\\FOLDERNAME';
isize = 32;
overlap = 16;
picchunk = 3;
for type = {'D2', 'D3', 'deep3'}
    for ink = [820, 825, 830, 835]
        for v=3:3:12
            for slide=1:4
%                 fprintf(strcat('type ', type{1}, ', ink %d, v %d, slide %d\n'),  ink, v, slide);
                 pivvideorun(datafolder, type{1}, ink, v, slide, 0, isize, overlap, picchunk, 2, 0);
            end
        end
    end
end