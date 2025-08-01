% to make the excell errant with matlab Just for one channel
%load Cells info and the BargTIRF Output as Events_info
%Events_info Structure: c:11 ;a:1011;a-c:2011;bg:3011; DF/S: 4011 left
% c:5011 ;a:6011;a-c:7011;bg:8011; DF/S: 9011 right
%Granules="Left";
%Protein="Right";
MovieLenght=500;
channels= 2;

IDEvents=Untitled.Properties.VariableNames;
Events_Info=table2array(Untitled);
Time=Events_Info(1,:);

if channels==1
    DF=nan(1000,size(Events_Info,2));

    for i=1:size(Events_Info,2)
        DF(501:601,i)=Events_Info((2010+Time(i))-50:(2010+Time(i))+50,i);% for DF left
        DF(701:801,i)=Events_Info((1010+Time(i))-50:(1010+Time(i))+50,i)-...
        Events_Info((3010+Time(i))-50:(3010+Time(i))+50,i);%for S left
    end

    Events_Info=cat(1,Events_Info,DF);
elseif channels==2
    DF=nan(2000,size(Events_Info,2));

    for i=1:size(Events_Info,2)
        DF(501:601,i)=Events_Info((2010+Time(i))-50:(2010+Time(i))+50,i);% for DF left
        DF(701:801,i)=Events_Info((1010+Time(i))-50:(1010+Time(i))+50,i)-...
        Events_Info((3010+Time(i))-50:(3010+Time(i))+50,i);%for S left

        DF(1501:1601,i)=Events_Info((7010+Time(i))-50:(7010+Time(i))+50,i);% for DF right
        DF(1701:1801,i)=Events_Info((6010+Time(i))-50:(6010+Time(i))+50,i)-...
        Events_Info((8010+Time(i))-50:(8010+Time(i))+50,i);%for S right
    end

    Events_Info=cat(1,Events_Info,DF);
end



Conditions=unique(extractAfter(CellsInfo.Image,5));
CellsData=struct();

ConditionsNames=strings(size(Conditions,1),1);
for i=1:size(Conditions,1)
    token1=[matlab.lang.makeValidName(replace(extractBefore(Conditions(i),".stk"), " ", "_"))];
    IndexCell=find(extractAfter(CellsInfo.Image,5)==Conditions(i));
    IDCell=extractBefore(CellsInfo.Image(IndexCell),5);

    for ii= 1:size(IDCell,1)
        ColumnIndex=find(extractBetween(IDEvents,"_","_")==IDCell(ii));
        if ii==1
            EventsCondition=Events_Info(:,ColumnIndex);
            EventsIDCondition=IDEvents(ColumnIndex);
        else
            EventsCondition=cat(2,EventsCondition,Events_Info(:,ColumnIndex));
            EventsIDCondition=cat(2,EventsIDCondition,IDEvents(ColumnIndex));
        end
    end
    CellsData.(token1).SingleEvents=EventsCondition;
    CellsData.(token1).IDSingleEvents=EventsIDCondition;
    ConditionsNames(i)=token1;
end

%  now you have the table with the event fluorescent for green & Red Channel DF

%to get the exocytosis table
for i=1:size(ConditionsNames,1)
    token1=[ConditionsNames(i)];
    IndexCell=find(extractAfter(CellsInfo.Image,5)==Conditions(i));
    IDCell=extractBefore(CellsInfo.Image(IndexCell),5);%Events and Cells ID
    CellsArea=CellsInfo.AreaReal(IndexCell); % Get cells area Real %Fiji already calculates that
    
    CumulativeExocytosis=zeros(MovieLenght,size(IndexCell,1));%make table

    for ii= 1:size(IDCell,1)
        ColumnIndex=find(extractBetween(IDEvents,"_","_")==IDCell(ii));
        Times=Events_Info(1,ColumnIndex);
        Times=sort(Times,"ascend");
        count=0;
        iii=1;
        while iii<MovieLenght
            if count<size(Times,2)
                if iii==Times(count+1)
                    count=count+1;
                else
                    iii=iii+1;
                end
            else
                iii=iii+1;
            end
            CumulativeExocytosis(iii,ii)=count;
        end
    end
    CumulativeExocytosis=CumulativeExocytosis./CellsArea';
    CellsData.(token1).Cummulative_Exo=CumulativeExocytosis;
    CellsInfoCondition=CellsInfo(IndexCell,:);
    CellsInfoCondition.TotalExo=CumulativeExocytosis(end,:)';
    CellsData.(token1).CellsInfoCondition=CellsInfoCondition;

end

for i=1:size(Conditions,1)
    Conditions(i)=strrep(Conditions(i),".","");
end

% first write events table
filename="Events_Data.xlsx";
for i=1:size(ConditionsNames,1)
    token1=ConditionsNames(i);
    TableTransfer=array2table(CellsData.(token1).SingleEvents);
    TableTransfer.Properties.VariableNames=CellsData.(token1).IDSingleEvents;
    writetable(TableTransfer,filename,"Sheet",extractBetween(Conditions(i)," ","Exo"));

end
filename="cumulativeExo.xlsx";
for i=1:size(ConditionsNames,1)
    token1=ConditionsNames(i);
    TableTransfer=array2table(CellsData.(token1).Cummulative_Exo);
    TableTransfer.Properties.VariableNames=CellsData.(token1).CellsInfoCondition.ROI';
    writetable(TableTransfer,filename,"Sheet",extractBetween(Conditions(i)," ","Exo"));
end

filename="CellsInfoOrganize.xlsx";
for i=1:size(ConditionsNames,1)
    token1=ConditionsNames(i);
    TableTransfer=CellsData.(token1).CellsInfoCondition;
    writetable(TableTransfer,filename,"Sheet",extractBetween(Conditions(i)," ","Exo"));
end



