Directory Structures

1. Main directores (NOAH-NLDAS): 

	- Readme.txt: this file
	- Makefile
        - makefile
	
2. Sub directories:
	- Noah_code: contain 4 Noah LSM progrmas
		- module_Noahlsm.F
		- module_Noahlsm_param_init.F
		- module_date_utilities.F
		- module_Noahlsm_utility.F

	- IO_code: 4 programs to read input data and output results

		- module_Noah_NC_output.F
		- module_Noahlsm_gridded_input.F
                - Noahlsm_driver.F:    main driver for the Noah LSM  

        - Run: Files needed to run Noah_beta
           	- Tables required by Noah
	         	- GENPARM.TBL
	         	- SOILPARM.TBL
	         	- VEGPARM.TBL

                - Noah_offline.namelist: namelist for model configuration input

	- Noah_data: 4 subdirectories
                - forcings: forcing data reprocessed from GLDAS data in NetCDF fromat

                - results: modeling results 
                   - /exp1
                       - /hrly: hourly outputs of runoff
                       - /hist: monthly-mean historical fiels
                       - /ini:  instantaneous outputs at the end of each year

                - static: required satic input data files (1 degree resolution)
                   - gvf.nc         : greenness vegetation cover fraction
                   - landmask.nc    : land-sea mask
                   - plotmask.nc    : number and location of each land point
                   - tbot.nc        : soil temperature at 8.0m deep
                   - lon_lat.nc     : lon/lat of each point
                   - soilcolor.nc   : soil color index (1-darker to 8-lighter)
                   - veg_soil.nc    : vegetation type index and soil texture index
                   - /rawdata       : raw data at higher or lower resolutions than 1 degree
                   - *.f            : fortran programs to produce the above data

How to run the offline Noah LSM:

 1. Compile the code by typing 'make' 
	1a: if it does not work, modify the makefile for your computer platform.

 2. Configure the Noah LSM by modifying 'Run/noah_offline.namelist' 

 3. Run the LSM executable './Noah' in the 'Run' directory

	- simulation results: in sub directory 'Noah_data/results'

