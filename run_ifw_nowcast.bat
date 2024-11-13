@echo off
ECHO Set year and quarter of nowcast (also produces forecast for the next quarter)!
set /p YEAR="Year: "
set /p QUARTER="Quarter: "

set DIR_ROOT=C:\Users\Aickolpon\Desktop\IfW\nowcasting\R_final
set DIR_REALTIMEDATA=%DIR_ROOT%/Echtzeitdatensatz

SET /A switch_download_data = 0
CD "real time data\R_download_data\"
IF %switch_download_data%==1 (
	ECHO Downloading data...
	REM Rscript --vanilla download_bbk_rtd.R "%DIR_REALTIMEDATA%/raw data"
	REM Rscript --vanilla download_bbk_financial_data.R "%DIR_REALTIMEDATA%/raw data"
	Rscript --vanilla download_esi.R "%DIR_REALTIMEDATA%/raw data"
	REM Rscript --vanilla compile_rtd_gastgewerbe.R "%DIR_REALTIMEDATA%/raw data"
	REM Rscript --vanilla compile_rtd_lkw_maut_index.R "%DIR_REALTIMEDATA%/raw data"	
) ELSE (
	ECHO "##- Switch set to 0. Do not download data"
)

SET /A switch_construct_vintages = 1
CD ..\R_construct_vintages\
IF %switch_construct_vintages%==1 (
	ECHO Constructing real-time vintages...
	Rscript --vanilla construct_realtime_vintages.R %DIR_REALTIMEDATA% 
) ELSE (
	ECHO "##- Switch set to 0. Do not construct vintages."
)

CD ..\..\model\
SET /A switch_compute_nowcasts = 1
SET /A switch_estimate_models = 1
IF %switch_compute_nowcasts%==1 (
	ECHO Estimating models...
	Rscript --vanilla compute_nowcasts.R %DIR_ROOT% %YEAR% %QUARTER% %switch_estimate_models%
) ELSE (
	ECHO "##- Switch set to 0. Do not compute nowcasts."
)

SET /A switch_additional_plots = 1
IF %switch_additional_plots% == 1 (
	ECHO "##- Plotting monthly GDP and non-GDP forecasts"
	Rscript --vanilla plot_monthlyGDP.R %DIR_ROOT% %YEAR% %QUARTER%
	Rscript --vanilla plot_nonGDPforecasts.R %DIR_ROOT% %YEAR% %QUARTER%
) ELSE (
	ECHO "##- Switch set to 0. Do not produce additiona plots."
) 
cd ..
cmd /k
