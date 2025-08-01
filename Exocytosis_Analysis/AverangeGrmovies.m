%%% To make image averange of the events
outXdim=21;
outYdim=21;
outZdim=41;

disp("Select ROIS.txt & videos.TIFF directory")
folder=uigetdir;

% get videos
cd(folder);
files=dir;
name={files.name};
movies=name(contains(name,".tif"));
EventRois=name(contains(name,".txt") & ~contains(name,"bg"));
bgRoi=name(contains(name,".txt") & contains(name,"bg"));

% first for loop to go cell by cell
for i=1:size(EventRois,2)
    if i==1
        count=0;
        zscoreimageProteinEvents=zeros(outYdim,outXdim,outZdim);
        zscoreimageGranulesEvents=zeros(outYdim,outXdim,outZdim);
        zscoreimageProteinFailures=zeros(outYdim,outXdim,outZdim);
        zscoreimageGranulesFailures=zeros(outYdim,outXdim,outZdim);
    end
    Events=readtable(strcat(folder,"\",EventRois{i}));
    bg=readtable(strcat(folder,"\",bgRoi{i}));
    %find events and failures in the ".txt"
    Failures=Events(Events.Slice==500,:);
    Events=Events(Events.Slice<500,:);
    Failures.Slice=Events.Slice;
    mcht=find(extractBefore(EventRois{i},".")==string(extractBefore(movies,".")));


%Open Tiff Movie
    t = Tiff(movies{mcht}, 'r');
    info = imfinfo(movies{mcht});
    numFrames = numel(info);

% Preallocate
    tiffArray = zeros(info(1).Height, info(1).Width, numFrames);

% Read frames
    for a = 1:numFrames
        t.setDirectory(a);
        tiffArray(:,:,a) = t.read();
    end
    t.close();
    tiffArray=tiffArray- bg.Mean(1);
% make Z-scores for ROIS in events
    for ii=1:size(Events,1)
        gr=zeros(outYdim,outXdim,outZdim);
        prtn=zeros(outYdim,outXdim,outZdim);
        X=Events.X(ii)-floor(outXdim/2):Events.X(ii)+floor(outXdim/2);
        Xgr=X+256;
        Y=Events.Y(ii)-floor(outYdim/2):Events.Y(ii)+floor(outYdim/2);
        Extime=Events.Slice(ii);
        time=Events.Slice(ii)-10:Events.Slice(ii)+30;

        time=time(time<500);
        if ~any(X<1 |X>256)&& ~any(Xgr<257|Xgr>512) && ~any(Y<1 | Y>256) && ~any(time<1 | time>numFrames)
            for a = 1:length(time)
                prtn(:,:,a) = (tiffArray(Y,...
                    X,...
                    time(a)));%-mean(tiffArray(Y,...
            %        X,...
            %        time(a)),[1,2]))./std(tiffArray(Y,...
            %        X,...
            %        time(a)),0,[1,2]);
                gr(:,:,a)= (tiffArray(Y,...
                    Xgr,...
                    time(a)));%-mean(tiffArray(Y,...
            %        Xgr,...
            %        time(a)),[1,2]))./std(tiffArray(Y,...
            %        Xgr,...
            %        time(a)),0,[1,2]);           
            end
            zscoreimageProteinEvents=zscoreimageProteinEvents+prtn;
            zscoreimageGranulesEvents=zscoreimageGranulesEvents+gr;
            count=count+1;
        end
    end

% make Z-scores for ROIS in failure
        for ii=1:size(Failures,1)
            gr=zeros(outYdim,outXdim,outZdim);
            prtn=zeros(outYdim,outXdim,outZdim);
            X=Failures.X(ii)-floor(outXdim/2):Failures.X(ii)+floor(outXdim/2);
            Xgr=X+256;
            Y=Failures.Y(ii)-floor(outYdim/2):Failures.Y(ii)+floor(outYdim/2);
            Extime=Failures.Slice(ii);
            time=Failures.Slice(ii)-10:Failures.Slice(ii)+30;
            %time=Failures.Slice(ii)-floor(outZdim/2):Failures.Slice(ii)+floor(outZdim/2);
            time=time(time<500);
            if  ~any(X<1 |X>256)&& ~any(Xgr<257|Xgr>512) && ~any(Y<1 | Y>256) && ~any(time<1 | time>numFrames)
                for a = 1:length(time)
                    prtn(:,:,a) = (tiffArray(Y,...
                        X,...
                        time(a)));%-mean(tiffArray(Y,...
                %        X,...
                %        time(a)),[1,2]))./std(tiffArray(Y,...
                %        X,...
                %        time(a)),0,[1,2]);
                    gr(:,:,a)= (tiffArray(Y,...
                        Xgr,...
                        time(a)));%-mean(tiffArray(Y,...
                %        Xgr,...
                %        time(a)),[1,2]))./std(tiffArray(Y,...
                %        Xgr,...
                %        time(a)),0,[1,2]);           
                end
                zscoreimageProteinFailures=zscoreimageProteinFailures+prtn;
                zscoreimageGranulesFailures=zscoreimageGranulesFailures+gr;
            end
        end
end

zscoreimageProteinEvents=zscoreimageProteinEvents/count;
zscoreimageGranulesEvents=zscoreimageGranulesEvents/count;
zscoreimageProteinFailures=zscoreimageProteinFailures/count;
zscoreimageGranulesFailures=zscoreimageGranulesFailures/count;

minMatrixGr= -7.2219;
%min([min(zscoreimageGranulesFailures,[],"all"),min(zscoreimageGranulesEvents,[],"all")])
%M13-1= 16.4905  ;%M13-2= 22.6730  ;%NT= -7.2219
maxMatrixGr= 256.7217;
%max([max(zscoreimageGranulesFailures,[],"all"),max(zscoreimageGranulesEvents,[],"all")])
%M13-1= 200.5166 ;%M13-2= 256.7217 ;%NT= 378.0454

normalizedGrEvents= ((zscoreimageGranulesEvents-minMatrixGr)/(maxMatrixGr-minMatrixGr)) *255;
normalizedGrFailures= ((zscoreimageGranulesFailures-minMatrixGr)/(maxMatrixGr-minMatrixGr)) *255;

minMatrixPrt= -7.2219;%
%min([min(zscoreimageProteinFailures,[],"all"),min(zscoreimageProteinEvents,[],"all")])
%M13-1= 16.4905  ;%M13-2= 22.6730 ;%NT= -7.2219
maxMatrixPrt= 45.7015;
% max([max(zscoreimageProteinFailures,[],"all"),max(zscoreimageProteinEvents,[],"all")])
%M13-1= 25.0433 ;%M13-2= 45.7015 ;%NT= -6.2386

normalizedPrtEvents= ((zscoreimageProteinEvents-minMatrixPrt)/(maxMatrixPrt-minMatrixPrt)) *255;
normalizedPrtFailures= ((zscoreimageProteinFailures-minMatrixPrt)/(maxMatrixPrt-minMatrixPrt)) *255;

normalizedGrEvents=uint16(normalizedGrEvents*(2^16-1)/255);
normalizedGrFailures=uint16(normalizedGrFailures*(2^16-1)/255);

normalizedPrtEvents=uint16(normalizedPrtEvents*(2^16-1)/255);
normalizedPrtFailures=uint16(normalizedPrtFailures*(2^16-1)/255);

% Step 3: Save as multi-frame TIFF
filename1 = 'Granule Events.tif';
filename2 = 'Granule Failure.tif';
filename3 = 'Protein Events.tif';
filename4 = 'Protein Failure.tif';
for k = 1:size(normalizedGrEvents, 3)
    if k == 1
        imwrite(normalizedGrEvents(:,:,k), filename1, 'tif', 'WriteMode', 'overwrite', 'Compression', 'none');
        imwrite(normalizedGrFailures(:,:,k), filename2, 'tif', 'WriteMode', 'overwrite', 'Compression', 'none');
        imwrite(normalizedPrtEvents(:,:,k), filename3, 'tif', 'WriteMode', 'overwrite', 'Compression', 'none');
        imwrite(normalizedPrtFailures(:,:,k), filename4, 'tif', 'WriteMode', 'overwrite', 'Compression', 'none');
    else
        imwrite(normalizedGrEvents(:,:,k), filename1, 'tif', 'WriteMode', 'append', 'Compression', 'none');
        imwrite(normalizedGrFailures(:,:,k), filename2,'tif', 'WriteMode', 'append', 'Compression', 'none');
        imwrite(normalizedPrtEvents(:,:,k), filename3, 'tif', 'WriteMode', 'append', 'Compression', 'none');
        imwrite(normalizedPrtFailures(:,:,k), filename4, 'tif', 'WriteMode', 'append', 'Compression', 'none');
    end
end