
clear
close all

format

% Name of the data file containing the time series [ Time points| case incidence]
% % Get all Adjusted Incidences
datafilename1='Data-for-metrics.xlsx';
data=xlsread(datafilename1);
data_new=data(4:end,1:end);


adjusted_incidence=[];
j=0;
k=3; %start picking values from col 9, col counter
num_of_rows=52;
for i=1:num_of_rows
    for j=1:12 % read 8 columns separated by 3
        adjusted_incidence(i,j)=data_new(i,k);
        k=k+5;
    end
    k=3;
end

% % Get all True Incidences
true_incidence=[];
a=2; %col counter for true_incidence
for i=1:num_of_rows
      for j= 1:12
          true_incidence(i,j)=data_new(i,a);
          a=a+5;
      end
      a=2;
end

LL=0;
f=4;
for i=1:num_of_rows
    for j=1:12
        LL(i,j)=data_new(i,f);
        f=f+5;
    end
    f=4;
end 
   
UL=0;
u=5;
for i=1:num_of_rows
    for j=1:12
        UL(i,j)=data_new(i,u);
        u=u+5;
    end
    u=5;
end 



%%

MSE1=mean((adjusted_incidence(1:43,1)-true_incidence(1:43,4)).^2);

MAE1=mean(abs(adjusted_incidence(1:43,1)-true_incidence(1:43,4)));

%%%%%%%%%%%%%%%%%%%% PI Coverage

%100*(sum(data (i)<=UL(i)and data(i) >=LL (i))/ length (data)
ind1=[];
ind2=[];
ind3=[];
for  i=1:43 %row counter
    for j=1

        if true_incidence(i,4)<=UL(i,j)
            ind1(i,j)=1; %indicator1 for UL
        else
            ind1(i,j)=0;
        end
        if true_incidence(i,4)>=LL(i,j)
            ind2(i,j)=1; %indicator2 for LL
        else
            ind2(i,j)=0;
        end
        if ind1(i,j)&&ind2(i,j)
            ind3(i,j)=1;
        else
            ind3(i,j)=0;
        end
    end
end
%%
for j=1
    PI_Coverage(j)=100*sum(ind3(:,j))/i;
    end
   
 

