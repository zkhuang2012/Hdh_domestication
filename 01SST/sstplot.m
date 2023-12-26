% sst plot
close all;clear;clc
sstfile = 'T20160012016366.L3m_YR_SST4_sst4_4km.nc';
step0 = 8; %in case of  out of memory 
x0 = 111;
x1 = 155;
y0 = 22;
y1 = 48;
colormin = 0;
colormax = 35;
nc = netcdf(sstfile,'r');
lon = nc{'lon'}(1:step0:end);
lat = nc{'lat'}(1:step0:end);
sst4 = nc{'sst4'}(1:step0:end,1:step0:end);
% palette = nc{'palette'}; 

scale_factor = nc{'sst4'}.scale_factor(:);
% sst4(sst4>100) = NaN;
sst4(sst4<-100) = NaN; % È¥µôfillvalue
[xx,yy]=meshgrid(lon,lat);
% h=fig('units','inches','width',7,'height',5.6);
m_proj('mercator',...
         'lon',[x0 x1],...
         'lat',[y0 y1]);
m_pcolor(xx,yy,sst4.*scale_factor)
shading interp
colormap(jet);
colorbar
set(gca,'Clim',[colormin,colormax]);
m_gshhs_i('patch',[0.6 0.6 0.6]);
% m_usercoast(fname,'patch',[0.6 0.6 0.6]);
hold on
% m_usercoast(fname,'linewidth',1,'color','k');
m_grid('box','fancy','xtick',5,'ytick',6,'tickdir','in',...
           'FontSize',11,'Fontname','Times New Roman','linestyle','none');
set(findobj('tag','m_grid_color'),'facecolor','white')
export_fig sst2016 -m6
% hold on
% wmask([1 1 1]);
% xlim([x0 x1])
% ylim([y0 y1])
close(nc);