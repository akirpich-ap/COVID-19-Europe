clear;
fileList = dir('*.csv');
nFiles = length(fileList);
dists = [];
distsW = [];
distsOrig = [];
cophenCoeff = [];

nParam = nFiles%+3;
for i = 1:nParam
    if i <= nFiles
        fileList(i).name
        data = readtable(fileList(i).name);
        countryList = data.country;
        ids = data.country;
        l = length(ids);
        ids = [ids; (arrayfun(@num2str, (l+1):(2*l-1), 'UniformOutput', 0))'];
        matr = table2array(data(1:end,2:end));
    else
        load(['param' int2str(i) '.mat']);
    end
    
%     Y = mdscale(matr,3);
%     figure
%     scatter3(Y(:,1),Y(:,2),Y(:,3))
%     text(Y(:,1), Y(:,2),Y(:,3), cellstr(num2str((1:length(countryList))')));
    
    Z = linkage(squareform(matr),'average');
%     figure
%     dendrogram(Z,'Labels',ids(1:l));
    AM = treeAM(Z,'directed');
    G = digraph(AM);
%     figure
%     plot(G,'Layout','layered','NodeLabel',ids);

    d = distances(G);
    d = d(1:l,1:l);
    dt = d.';
    m  = tril(true(size(dt)));
    v  = dt(m).';
    dists =[dists (v(v>0))'];
    [c,d] = cophenet(Z,squareform(matr));
    distsW = [distsW d'];
    cophenCoeff = [cophenCoeff c];

    dt = matr.';
    m  = tril(true(size(dt)));
    v  = dt(m).';
    distsOrig =[distsOrig (v(v>0))'];
end

median(cophenCoeff)

corrMatr = corr(dists,'type','Pearson');
corrMatrW = corr(distsW,'type','Pearson');
corrMatrOrig = corr(distsOrig,'type','Pearson');



G = graph((corrMatrOrig >= 0.35) - eye(nParam,nParam));
figure
plot(G)
%% 
dataEpid = readcell('time_series_covid19_confirmed_global.csv');
dataPop = readtable('populations.xlsx');
incAll = [];
for i = 1:length(countryList)
    country = replace(countryList{i},'.',' ')
    idx = find(strcmp(dataEpid(:,2), country));
    for j = idx'
        if ismissing(dataEpid{j,1})
            idx = j;
            break;
        end
    end
    obsCases = dataEpid(idx,5:end);
    obsCases = [obsCases{:}];
    inc = diff(obsCases');
    ind = find(strcmp(dataPop.Var1,country));
    inc = 100000*inc/dataPop.Var2(ind);
    for k = 1:length(inc)
        if inc(k) < 0
            inc(k) = (inc(k-1)+inc(k+1))/2;
        end
    end
    incAll = [incAll inc];
end
times = dataEpid(1,6:end);
times = [times{:}];
incAll = incAll(1:765,:);

ind = 1:14:size(incAll,1);
incAll1 = zeros(length(ind)-1,size(incAll,2));
for i = 1:length(ind)-1
    incAll1(i,:) = sum(incAll(ind(i):(ind(i+1)-1),:),1);
end
incAll = incAll1;
 
matr = zeros(length(countryList),length(countryList));
for i = 1:length(countryList)
    for j = 1:length(countryList)
        if i == j
            continue;
        end
%         [~,~,matr(i,j)] = kstest2(incAll(:,i),incAll(:,j));
%         matr(i,j) = sum(abs(incAll(:,i)-incAll(:,j)));
%         matr(i,j) = sum(sum((incAll(:,i)-incAll(:,j)).^2));
%         matr(i,j) = 1-corr(incAll(:,i),incAll(:,j));
        [xcf,lags] = crosscorr(incAll(:,i),incAll(:,j));
        matr(i,j) = 1-max(xcf);
    end
end
save('param12.mat','matr');

%% 
time_test_begin = datetime('04-01-2020','InputFormat','MM-dd-yyyy');
time_test_end = datetime('02-24-2022','InputFormat','MM-dd-yyyy');
time_points = time_test_begin:caldays(30):time_test_end;
countryList = [countryList; 'Montenegro'; 'Serbia'];

metadataCont = readtable(['metadata_2022_05_01_updated_Europe_ui_extracted.tsv'],"FileType","text",'Delimiter', '\t');
metadataCont1 = readtable(['metadata_2022_05_01_updated_Asia_ui_extracted.tsv'],"FileType","text",'Delimiter', '\t');
ind_cnt = strcmp(metadataCont1.Country,'Armenia');
metadataCont1 = metadataCont1(ind_cnt,:);
metadataCont = [metadataCont; metadataCont1];
strainDistrib = cell(length(countryList),1);
nSeq = 0;
for cnt = 1:length(countryList) 
    strainDistrib{cnt} = zeros(6,length(time_points)-1);
    country = replace(countryList{cnt},'.',' ')
    if strcmp(country,'Czechia')
        country = 'Czech Republic';
    end
    ind_cnt = strcmp(metadataCont.Country,country);
    metadata = metadataCont(ind_cnt,:);
    nSeq = nSeq + sum(ind_cnt);
    for t = 2:length(time_points)
        ind = (metadata.CollectionDate > time_points(t-1))&(metadata.CollectionDate <= time_points(t));
        strains_t = metadata.categoricalVariants(ind);
        if ~isempty(strains_t)
            for j = 1:6
               strainDistrib{cnt}(j,t-1) = sum(strains_t == j-1); 
            end
            strainDistrib{cnt}(:,t-1) = strainDistrib{cnt}(:,t-1)/sum(strainDistrib{cnt}(:,t-1));
            [];
        end
    end
end

matr = zeros(length(countryList),length(countryList));
for i = 1:length(countryList)
    for j = 1:length(countryList)
        matr(i,j) = sum(sum((strainDistrib{i}-strainDistrib{j}).^2));
    end
end
save('param13.mat','matr');
writematrix(matr,'strains.csv')

%% 
dataMort = readcell('time_series_covid19_deaths_global.csv');
dataPop = readtable('populations.xlsx');
mortAll = [];
for i = 1:length(countryList)
    country = replace(countryList{i},'.',' ')
    idx = find(strcmp(dataMort(:,2), country));
    for j = idx'
        if ismissing(dataMort{j,1})
            idx = j;
            break;
        end
    end
    obsCases = dataMort(idx,5:end);
    obsCases = [obsCases{:}];
    mort = diff(obsCases');
    ind = find(strcmp(dataPop.Var1,country));
    mort = 100000*mort/dataPop.Var2(ind);
    for k = 1:length(mort)
        if mort(k) < 0
            mort(k) = (mort(k-1)+mort(k+1))/2;
        end
    end
    mortAll = [mortAll mort];
end
mortAll = mortAll(1:765,:);

ind = 1:14:size(mortAll,1);
mortAll1 = zeros(length(ind)-1,size(mortAll,2));
for i = 1:length(ind)-1
    mortAll1(i,:) = sum(mortAll(ind(i):(ind(i+1)-1),:),1);
end
mortAll = mortAll1;
 
matr = zeros(length(countryList),length(countryList));
for i = 1:length(countryList)
    for j = 1:length(countryList)
        if i == j
            continue;
        end
        [xcf,lags] = crosscorr(mortAll(:,i),mortAll(:,j));
        matr(i,j) = 1-max(xcf);
%         matr(i,j) = max(xcf);
    end
end
save('param14.mat','matr');
%% 
% seqF = fastaread('hiv_msa_earlier_seq.fasta');
% seq = char(seqF.Sequence);
% DM = squareform(pdist(seq,'hamming'));
% AM = (DM <= 0.015) - eye(size(DM,1),size(DM,2));
% G = graph(DM.*AM);
% plot(G)
%% 
% data = readmatrix('House.xlsx');
% histogram(data)
