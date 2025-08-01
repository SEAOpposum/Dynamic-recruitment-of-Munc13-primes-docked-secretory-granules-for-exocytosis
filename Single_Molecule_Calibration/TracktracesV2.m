%%Load the tracks from TracKNTrace output manually
[file,path] = uigetfile(pwd, 'Select a file');%Select the .tiff video
info = imfinfo([path file]);
numberOfPages = length(info);
for k = 1 : numberOfPages
    % Read the kth image in this multipage tiff file.
    thisPage = imread( [path file], k);
    % Now process thisPage somehow...
end
%Exclude tracks at the border (image size 256x256 px)
outidx=find(trackingData(:,3)<10 | trackingData(:,3)>246); 
outidy=find(trackingData(:,4)<10 | trackingData(:,4)>246);

outid=cat(1,outidx,outidy);

if(~isempty(outid))
    numbers=unique(trackingData(outid,1));
    for ii=1:length(numbers)
        trackingData(trackingData(:,1)==numbers(ii),:)=[];
    end
    oldids=unique(trackingData(:,1));
    newids=1:length(oldids);
    for ii=1:length(oldids)
        trackingData(trackingData(:,1)==oldids(ii),1)=newids(ii);
    end
end% rectified tracks ID after exclusion


L= max(size(trackingData));
a= trackingData(L,1);
par= cell(a,1);


for ii=1 : a

p= find(trackingData(:,1)==ii); %this gives me the index for the particle id
track= length(p);
stack= trackingData(p,2);
stackplus= [max(stack)+1:max(stack)+5];


dfpar= zeros(track,6);
dfpar(:,1)= (flip(1:track)*-1)+1;
dfpar(:,6)= stack;%create array to store DF at the disappearance moment (5 frames before and 5 after)

for i=1 : track %Measure DF of the particle in the video before bleaching
    %first get the coordinates  
    xcor= round(trackingData(p(i),3));
    ycor= round(trackingData(p(i),4));
    ti= stack(i);
    
    % Read the kth image in this multipage tiff file.
    thisPage = imread([path file], ti);
    % Now process thisPage somehow...
    
    C =zeros(13,1);
    
    C(1,1)= thisPage(ycor, xcor);
    C(2,1)= thisPage(ycor,(xcor-1));
    C(3,1)= thisPage(ycor,xcor+1);
    C(4,1)= thisPage(ycor-1, xcor);
    C(5,1)= thisPage(ycor+1, xcor);
    C(6,1)= thisPage(ycor-1, xcor-1);
    C(7,1)= thisPage(ycor-1, xcor+1);
    C(8,1)= thisPage(ycor+1, xcor-1);
    C(9,1)= thisPage(ycor+1, xcor+1);
    C(10,1)= thisPage(ycor, xcor-2);
    C(11,1)= thisPage(ycor-2, xcor);
    C(12,1)= thisPage(ycor, xcor+2);
    C(13,1)= thisPage(ycor-2, xcor);
    Ca= sum(C)/13;
    
    A= zeros(12,1);
    A(1,1)= thisPage(ycor, xcor-3);
    A(2,1)= thisPage(ycor, xcor+3);
    A(3,1)= thisPage(ycor-3, xcor);
    A(4,1)= thisPage(ycor+3, xcor);
    A(5,1)= thisPage(ycor-1, xcor-2);
    A(6,1)= thisPage(ycor-2, xcor-1);
    A(7,1)= thisPage(ycor-2, xcor+1);
    A(8,1)= thisPage(ycor-1, xcor+2);
    A(9,1)= thisPage(ycor+1, xcor-2);
    A(10,1)= thisPage(ycor+2, xcor-1);
    A(11,1)= thisPage(ycor+2,xcor+1);
    A(12,1)= thisPage(ycor+1, xcor+2);
    Ac=sum(A)/12;
    
    dfpar(i,2)= Ca-Ac;
    dfpar(i,3)= sum(C)/13;
    dfpar(i,4)= xcor;
    dfpar(i,5)= ycor;
    
    end

       if (max(stackplus))<numberOfPages %measure DF after bleaching
        dfparplus= zeros(5,6);
        dfparplus(:,1)= [1:5];
        pplus= [max(p)+1:max(p)+5];
        
        for i=1 : 5
             %first get the coordinates
                xcor= round(trackingData(max(p),3));
                ycor= round(trackingData(max(p),4));
                ti= stackplus(i);
    
                 % Read the kth image in this multipage tiff file.
                thisPage = imread([path file], ti);
                % Now process thisPage somehow...
    
                 C =zeros(13,1);
    
                 C(1,1)= thisPage(ycor, xcor);
                 C(2,1)= thisPage(ycor,(xcor-1));
                 C(3,1)= thisPage(ycor,xcor+1);
                 C(4,1)= thisPage(ycor-1, xcor);
                C(5,1)= thisPage(ycor+1, xcor);
                C(6,1)= thisPage(ycor-1, xcor-1);
                C(7,1)= thisPage(ycor-1, xcor+1);
                C(8,1)= thisPage(ycor+1, xcor-1);
                C(9,1)= thisPage(ycor+1, xcor+1);
                C(10,1)= thisPage(ycor, xcor-2);
                C(11,1)= thisPage(ycor-2, xcor);
                C(12,1)= thisPage(ycor, xcor+2);
                C(13,1)= thisPage(ycor-2, xcor);
                Ca= sum(C)/13;
    
                A= zeros(12,1);
                A(1,1)= thisPage(ycor, xcor-3);
                A(2,1)= thisPage(ycor, xcor+3);
                A(3,1)= thisPage(ycor-3, xcor);
                A(4,1)= thisPage(ycor+3, xcor);
                A(5,1)= thisPage(ycor-1, xcor-2);
                A(6,1)= thisPage(ycor-2, xcor-1);
                A(7,1)= thisPage(ycor-2, xcor+1);
                A(8,1)= thisPage(ycor-1, xcor+2);
                A(9,1)= thisPage(ycor+1, xcor-2);
                A(10,1)= thisPage(ycor+2, xcor-1);
                A(11,1)= thisPage(ycor+2,xcor+1);
                A(12,1)= thisPage(ycor+1, xcor+2);
                Ac=sum(A)/12;
    
                dfparplus(i,2)= Ca-Ac;
                dfparplus(i,3)=sum(C)/13;
                dfparplus(i,4)= xcor;
                dfparplus(i,5)= ycor;
                dfparplus(i,6)= stackplus(i);
                
                
   
        end
        dfpar= cat(1,dfpar, dfparplus);
       else
           dfpar=[]; 
       end
    
 par{ii}= dfpar;

end
par= par(~any(cellfun('isempty',par),2),:);
achu= size(par);
a= achu(1);

figure
hold on
aes= par{round(a*rand(1)),1};
xaes= aes(:,1);
yaes= aes(:,2);
plot(xaes,yaes);
title(aes(1,1))

deltas= zeros(a,6);
hyper= zeros(a,8);
Av= zeros(a,10);
s=1;
wtf=0;
DeltaJump= zeros(a,1);
for i=1:a %measure the DF difference between before and after bleaching

    aes=par{i,1};
    
    if max(aes(:,1)) < 1
    else
        
         hyper(i,1)= max(aes([1:(length(aes(:,2))-5)],2));
         hyper(i,2)= std(aes([1:(length(aes(:,2))-5)],2));
         hyper(i,3)= mean(aes([1:(length(aes(:,2))-5)],2));
         hyper(i,4)= max(aes([1:(length(aes(:,2))-5)],3));
         hyper(i,5)= std(aes([1:(length(aes(:,2))-5)],3));
         hyper(i,6)= mean(aes([1:(length(aes(:,2))-5)],3));
         hyper(i,7)=aes(1,2);
         hyper(i,8)=aes(1,3);
     
         Av(i,1)= aes(find(aes(:,1)==-4),2);
         Av(i,2)= aes(find(aes(:,1)==-3),2);
         Av(i,3)= aes(find(aes(:,1)==-2),2);
         Av(i,4)= aes(find(aes(:,1)==-1),2);
         Av(i,5)= aes(find(aes(:,1)==0),2);
         Av(i,6)= aes(find(aes(:,1)==1),2);
         Av(i,7)= aes(find(aes(:,1)==2),2);
         Av(i,8)= aes(find(aes(:,1)==3),2);
         Av(i,9)= aes(find(aes(:,1)==4),2);
         Av(i,10)= aes(find(aes(:,1)==5),2);
         
        
         if ((Av(i,1)+Av(i,2)+Av(i,3)+Av(i,4)+Av(i,5))/5) > max(Av(i,6:10))
         DeltaJump(i)= (((Av(i,1)+Av(i,2)+Av(i,3)+Av(i,4)+Av(i,5))/5)-((Av(i,6)+Av(i,7)+Av(i,8))/3));
        
             
    for ii=1:(length(aes(:,1))-5)
    deltastep= aes(ii+1,2)- aes(ii,2);
    deltastepN= aes(ii,1);
    raw= aes(ii,2);
    
    deltas(s,1)= deltastepN;
    deltas(s,2)= raw;
    deltas(s,3)= deltastep;
    
    deltastepC= aes(ii+1,3)- aes(ii,3);
    deltastepNC= aes(ii,1);
    rawC= aes(ii,3);
    
    deltas(s,4)= deltastepNC;
    deltas(s,5)= rawC;
    deltas(s,6)= deltastepC;
    
    s= s+1;
     end
     else
             DeltaJump(i)= -100;
             wtf= wtf+1;
              Av(i,1)= -100;   
              dfpar=[];
              par{i,1}=dfpar;
 
         end
    end
end
DeltaJump=DeltaJump(DeltaJump~= -100);
Av(Av(:,1)==-100,:)=[];
par= par(~any(cellfun('isempty',par),2),:);
achu= size(par);
a= achu(1);

Average=[mean(Av(:,1)),mean(Av(:,2)),mean(Av(:,3)),mean(Av(:,4)),mean(Av(:,5)),mean(Av(:,6)),mean(Av(:,7)), mean(Av(:,8)),mean(Av(:,9)),mean(Av(:,10))];
TimeAver=[-4,-3,-2,-1,0,1,2,3,4,5];
figure(1)
hold on
plot(TimeAver,Average);
title("BleachAverage")
hold off
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% To make single molecule movie example
ScaryMovies= cell(a,10);
for i=1:a
   aes=par{i,1};
  
   if max(aes(:,1)) == 5 && min(aes(:,4))>= 21 && max(aes(:,4))<= 235 && min(aes(:,5))>= 21 && max(aes(:,5))<= 235
    index= find(aes(:,1)== -4);
    stack=length(aes(index:end,1));
    time= aes(index:end,6);
    for ii=1:stack
    ti= time(ii);
     %Read the kth image in this multipage tiff file.
    thisPage = imread([path file], ti);
    MiniMovie=thisPage([round((aes(ii,5)-20)):round((aes(ii,5)+20))],[(round(aes(ii,4)-20)):round((aes(ii,4)+20))]);
       ScaryMovies{i,ii}=MiniMovie;
           
    end
   end 
end
ScaryMovies= ScaryMovies(~any(cellfun('isempty',ScaryMovies),2),:);

achu= size(ScaryMovies);
AvMiniMovie= cell(achu(2),1);

for i=1:achu(2)
t1ScaryMovies= ScaryMovies{1,i};
for k = 2:achu(1)
    
  t2ScaryMovies= ScaryMovies{k,i};
  t1ScaryMovies = t1ScaryMovies + t2ScaryMovies;
  t1ScaryMovies = t1ScaryMovies/2; 
    
  
end
    AvMiniMovie{i}= t1ScaryMovies;

end

XX=cat(3, AvMiniMovie{1},AvMiniMovie{2},AvMiniMovie{3},AvMiniMovie{4},AvMiniMovie{5},AvMiniMovie{6},AvMiniMovie{7},AvMiniMovie{8},AvMiniMovie{9},AvMiniMovie{10}); 

u=size(XX);

%figure(2)
%tiledlayout('flow');
%for i=1:9
%nexttile
%surfc(XX(:,:,i))
%ax = gca;
%ax.ZLim(2) = 125;

%end
%t.Padding = 'compact';
%t.TileSpacing = 'compact';




imwrite(XX(:,:,1), 'MiniMovie.tiff'); %set your file name and video compression
for f = 3:size(XX,3)
    imwrite(XX(:,:,f),'MiniMovie.tiff','WriteMode','append');
end

figure(3)
histogram(DeltaJump,100);
[N,edges] = histcounts(DeltaJump,100);

HisData= cat(2,edges(2:101)', N');
writematrix(HisData,'HistogramDataBleach.xlsx');
writematrix(DeltaJump,'DF.xlsx');
%h = figure;
%axis tight manual % this ensures that getframe() returns a consistent size
%filename = 'testAnimated.gif';
%for n =1:1:u(3)
    % Draw plot for y = x.^n
%    surf(XX(:,:,n)) 
%    drawnow 
      % Capture the plot as an image 
%      frame = getframe(h); 
%      im = frame2im(frame); 
%      [imind,cm] = rgb2ind(im,256); 
      % Write to the GIF File 
%      if n == 1 
%          imwrite(imind,cm,filename,'gif', 'Loopcount',inf); 
%      else 
%          imwrite(imind,cm,filename,'gif','WriteMode','append'); 
%      end 
%  end



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%h=histogram(deltas(:,2));
%h=histogram(deltas(:,2));
%hm=histogram(maxi(:,1));
%%f = fit(x.',y.','gauss2')

%h;
%hmX=h.BinEdges;
%hmY=h.Values;
%%plot(hyper(:,1),hyper(:,2),'.');
%plot(hyper(:,4),hyper(:,5),'.');
%%plot(hyper(:,3),hyper(:,2),'.');
%%f= fit(hmX, hmY, 'gauss10')
%%h=histogram(hyper(:,2))
%h=histogram(hyper(:,7))

%%%% lets try to plot this thing
%%Mdl=fitlm(hyper(:,1), hyper(:,2));
%%resid= Mdl.Residuals.Raw(~isnan(Mdl.Residuals.Raw));

%%figure
%%subplot(2,1,1)
%%hold on
%%plotResiduals(Mdl,'fitted')
%%axis tight
%%plot([min(Mdl.fitted) max(Mdl.fitted)], [0 0], 'k-')
%%title('Residual Plot')
%%xlabel('$\hat y$', 'Interpreter', 'latex')
%%ylabel('Residuals')
%%axis tight
%%subplot(2,1,2)
%%autocorr(resid)

%%maxLag = floor(4*((length(hyper(:,1)))/100)^(2/9));
%%EstCov = hac(hyper(:,1),hyper(:,2),'bandwidth',maxLag+1,'display','full');