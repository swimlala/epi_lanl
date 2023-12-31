CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS     N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-11-20T21:21:25Z creation      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.2    Conventions       Argo-3.2 CF-1.6    featureType       trajectoryProfile      comment_dmqc_operator         ZPRIMARY | https://orcid.org/0000-0001-7324-3159 | Matthew Alkire, University of Washington        F   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    =    FORMAT_VERSION                 	long_name         File format version    
_FillValue                    =0   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    =4   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    =8   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    =H   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    =X   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    =h   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  �  =�   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  �  >@   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  �  ?    CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        ?�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    ?�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    ?�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                  `  ?�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    @8   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    @D   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                  `  @H   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                  `  @�   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                  `  A   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    Ah   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~       axis      T           At   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    A�   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~            A�   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           A�   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           A�   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    A�   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    A�   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    A�   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        D�   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    E    PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    E   PROFILE_CNDC_QC                	long_name         #Global quality flag of CNDC profile    conventions       Argo reference table 2a    
_FillValue                    E   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    E   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        I  E   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 H  �$   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     I  �l   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 H  �   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     I  ��   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     I D�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 H ��   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     I �8   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 H �L   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     I ��   CNDC         
      	   	long_name         Electrical conductivity    standard_name         !sea_water_electrical_conductivity      
_FillValue        G�O�   units         mhos/m     	valid_min                	valid_max         A     C_format      %12.5f     FORTRAN_format        F12.5      
resolution        8ѷ     I D�   CNDC_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 H ��   CNDC_ADJUSTED            
      	   	long_name         Electrical conductivity    standard_name         !sea_water_electrical_conductivity      
_FillValue        G�O�   units         mhos/m     	valid_min                	valid_max         A     C_format      %12.5f     FORTRAN_format        F12.5      
resolution        8ѷ     I �   CNDC_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 H �   CNDC_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         mhos/m     C_format      %12.5f     FORTRAN_format        F12.5      
resolution        8ѷ     I �`   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     I Dt   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 H ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     I ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 H ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     I �,   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  � D@   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                   E    SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                   Q    SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                   ]    SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  � i    HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                   i�   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                   i�   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                   i�   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                   i�   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  � i�   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                  , j�   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                   j�   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                  0 j�   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar        k    HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar        k   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�       k   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                  0 k$Argo profile    3.1 1.2 19500101000000  20181120212125  20200901153619  5901469 5901469 5901469 US ARGO PROJECT                                                 US ARGO PROJECT                                                 US ARGO PROJECT                                                 STEPHEN RISER                                                   STEPHEN RISER                                                   STEPHEN RISER                                                   PRES            TEMP            CNDC            PSAL            PRES            TEMP            CNDC            PSAL            PRES            TEMP            CNDC            PSAL               p   p   pAAA AOAOAO  2688                            2688                            2688                            2C  2B  2C  DAD APEX                            APEX                            APEX                            2730                            2730                            2730                            112607                          112607                          112607                          846 846 846 @�ڴz�,@�ڴz�,@�ڴz�,111 @�ڴ`�%f@�ڴ`�%f@�ڴ`�%f@5�z�H@5�z�H@5�z�H�cQ���o�cQ���o�cQ���o111 GPS     GPS     GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                 Near-surface sampling: discrete, unpumped [auxiliary STS]                                                                                                                                                                                                       Secondary sampling: discrete [high frequency 0.1 dbar data, for cross-calibration with the STS]                                                                                                                                                                          ACA ACA  CA ACA @9��@�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8ffB@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�>L��>L��>L��=���=���>���=���=���>L��=���=���=���=���>L��=���=���>L��=���    =���>L��=���>L��>���=���=���>L��=���    =���>L��=���=���=���=���=���=���=���=���=���=���=���=���>���=���=���=���=���>L��=���>L��?   >���=���=���=���>L��=���>L��=���=���=���=���=���=���>L��=���=���=���=���=���=���    >L��    =���>L��=���=���>���>���=���=���=���=���=���=���    =���=���>L��=���>L��=���=���=���=���=���=���=���=���=���>L��>L��=���=���=���>L��=���=���=���=���=���>L��>L��=���=���=���=���=���=���=���=���>L��=���    =���=���=���>L��>L��=���    =���=���=���=���=���>���>���=���=���=���=���>L��=���=���>���>���=���=���>L��>L��=���=���>L��>���?   >���=���>L��>L��>���>���=���    =���=���>L��=���=���>L��>L��=���=���=���>L��>���=���=���=���>L��>���?   ?   >���=���>L��=���=���>L��>���>���>L��=���>L��=���=���>L��>L��>L��=���=���>L��>L��=���=���>L��=���>L��>L��>���>L��=���>���>���>���>���>���>���>���>���>L��>���>���>���>���>���>���>���>���>���>���>���>���>���>���>���>L��>���>���>���>���>���?   >���>���>���>L��>L��>L��>���>���>���>���>���>���>���>���>���>���>���>���>���>���>���>���>L��>���>���>���>���>���>���>���>���>���>���>���>���>���>���>���>���>���>���>���>���>���>���>���>���>���>���>���>���>L��>���>���>���>���>L��>���>���>���>���>���>���>���>���>���>���>���>L��>���>���>���>���>���>���>���>���>���>���>���>���>���>���>���>���>L��>���>���>���>���>���>���>L��>���>���>���>L��>���>���>���>���>���>���>���>���>���>���>���>���>L��>���>���>���>L��>���>���>���>���>L��=���>���>���>���>���>���>���>���>���>���>���>���>���>���>���>���>���>���>���>���>���>���>���>���>���>���>���>���>���>���>���>���>���>���>���>���>���>���>L��>���>���>���>���>���>���>���>���>���>���>���>���>���>���>���>L��>���>���>���>���>���>���>���>���>���>L��>���>���>L��>L��>���>���>���>���>���>���>���>���>���>���>���>���>���>���>���>���>���>���>���>���>���>���>���>���>���>���>L��>���>���>���?   >���>���>���>���>���>���>���>���>���>L��>���>���>���?   ?   >���>���>���>���>���>���>���>���>���=���>���>L��>���>���>���?   ?��?��?333?L��?L��?�  ?�  ?�  ?���?���?�ff?�ff?�33?�  ?�  ?�  ?ٙ�?�ff?�33?�33?�33@   @ff@��@��@33@��@   @   @&ff@&ff@,��@9��@9��@Fff@L��@L��@Y��@fff@l��@s33@y��@�  @�ff@���@���@�  @�33@�ff@���@�  @�33@���@���@���@�33@�ff@���@�  @�33@�ff@ə�@�  @�33@�ff@ٙ�@���@�33@�ff@陚@���@�  @�33@�ff@���A   A��A33A��AffA  A33A��AffA  A��A33A��A  A��A��AffAffA!��A#33A&ffA(  A)��A+33A.ffA0  A1��A4��A6ffA8  A;33A<��A@  AA��AC33AD��AH  AI��AK33ANffAP  AQ��AT��AVffAX  AY��A\��A^ffA`  Aa��Ad��AfffAh  Ak33Al��AnffAp  As33At��AvffAy��A{33A|��A�  A���A���A�33A�  A���A�ffA�33A�  A���A�ffA�33A���A���A�33A�  A���A�ffA�33A�  A���A�ffA�33A���A���A�ffA�  A���A�ffA�33A�  A���A�ffA�33A���A���A�ffA�33A���A���A�ffA�  A���A���A�33A�  A���A�ffA�33A�  A���A�ffA�33A���A���A�ffA�33A���A���A�ffA�  A���A���A�33A�  A���A�ffA�33A���Aə�A�ffA�33A���A͙�A�33A�  A���A�ffA�33A�  Aՙ�A�ffA�  A���Aٙ�A�33A�  Aݙ�A�ffA�33A���AᙚA�33A�  A���A�ffA�33A���A陚A�ffA�  A���A�ffA�33A���A�AQ��A�  A���A�ffA�33A�  A���A�ffA�  A���A���A�33B   B ��B33B  BffB��B��B  B��B33B��BffB��B33B  BffB��B	��B
  B
ffB
��B33B  BffB��B33B��B  B��B33B��B  BffB��B33B  BffB��B33B��B  BffB��B��B  BffB��B33B��B  BffB33B��B  BffB��B33B��B  B��B33B��B  BffB��B33B��B   B ��B!33B!��B"  B"ffB"��B#33B#��B$ffB$��B%33B%��B&  B&ffB&��B'33B'��B(ffB(��B)33B)��B*  B*ffB*��B+33B,  B,ffB,��B-33B-��B.  B.ffB/33B/��B0  B0ffB0��B133B1��B2  B2��B333B3��B4  B4ffB4��B533B5��B6  B6ffB6��B733B7��B8  B8ffB8ffB8��B933B9��B:  B:ffB:��B:��B;33B;��B<  B<ffB<��B<��B=33B=��B>  B>  B>ffB>��B?33B?��B@  B@  B@ffB@��BA33BA��BB  BB  BBffBB��BC33BC��BD  BD  BDffBD��BE33BE33BE��BF  BFffBF��BG33BG33BG��BH  BHffBH��BH��BI33BI��BJ  BJffBJffBJ��BK33BK��BL  BL  BLffBL��BM33BM��BN  BN  BNffBN��BO33BO��BO��BP  BPffBP��BQ33BQ33BQ��BR  BRffBR��BS33BS33BS��BT  BTffBT��BT��BU33BU��BV  BVffBVffBV��BW33BW��BX  BXffBX��BX��BY33BY��BZ  BZffBZ��B[33B[��B\  B\  B\ffB\��B]33B]��B^  B^ffB^��B_33B_��B`  B`ffB`��Ba33Ba��Bb  Bb  BbffBb��Bc33Bc��Bd  BdffBd��Be33Be��Bf  BfffBf��Bg33Bg��Bh  BhffBh��Bi33Bi��Bj  BjffBj��Bk33Bk��Bl  BlffBl��Bm33Bm��Bn  BnffBn��Bo33Bp  BpffBp��Bq33Bq��Br  BrffBr��Bs33Bs��Bt  BtffBt��Bu33Bu��Bv  BvffBv��Bw33Bw��Bx  BxffBx��By33By��Bz  BzffBz��B{33B{��B|  B|��B}33B}��B~  B~ffB~��B33B��B�  B�33B�ffB���B���B�  B�33B�ffB���B���B�  B�ffB���B���B�  B�33B�ffB���B���B�  B�33B�ffB���B���B�  B�33B�ffB���B���B�  B�  B�ffB�ffB���B���B�  B�33B�ffB���B���B�  B�33B�ffB���B���B���B�  B�33B�ffB���B���B�  B�33B�ffB���B���B�  B�33B�ffB���B���B�  B�33B�ffB���B���B�  B�33B�ffB���B���B�  B�33B�ffB���B���B�  B�33B�ffB���B���B�  B�33B�ffB���B���B�  B�33B�ffB���B�  B�  B�ffB���B���B�  B�33B�ffB���B���B�  B�33B���B���B�  B�33B�ffB���B���B�  B�33B�ffB���B�  B�33B�ffB���B���B�  B�33B�ffB���B�  B�33B�ffB���B���B�  B�ffB���B���B�  B�33B�ffB���B�  B�33B�ffB���B���B�  B�ffB���B���B�  B�33B�ffB���B�  B�33B�ffB���B���B�33B�ffB���B���B�  B�33B���B���B�  B�33B���B���B�  B�33B�ffB���B�  B�33B�ffB���B���B�33B�ffB���B���B�  B�ffB���B���B�  B�ffB���B���B�  B�33B�ffB���B�  B�33B�ffB���B�  B�33B�ffB���B�  B�33B�ffB���B�  B�33B�ffB���B���B�33B�ffB���B���B�33B�ffB���B���B�33B�ffB���B���B�  B�ffB���B���B�  B�ffB���B���B�  B�ffB���B���B�  B�ffB���B���B�  B�33B���B���B�  B�33B�ffB���B�  B�33B�ffB���B���B�  B�33B�ffB���B���B�  B�33B�ffB���B���B�  B�33B�ffB���B���B�  B�33B�ffB���B���B�  B�33B�ffB���B���B�  B�33B�ffB���B���B�  B�33B�ffB���B���B���B�  B�33B�ffB���B���B�  B�33B�ffB���B���B�  B�33B�ffB���B���B�  B�33B�ffB�ffB���B���B�  B�33B�ffB�B���B�  B�33B�ffBÙ�B���B�  B�33B�ffB�ffBę�B���B�  B�33B�ffBř�B���B�  B�33B�ffBƙ�B���B�  B�33B�ffBǙ�BǙ�B���B�  B�33B�ffBș�B���B�  B�33B�ffBə�B���B�  B�33B�ffBʙ�B���B�  B�  B�33B�ffB˙�B���B�  B�33B�ffB̙�B���B�  B�33B�ffB͙�B���C�fC  C33CffC��C��C  C33CffC��C�3C�fC�CL�C� C�3C��C  C33CffC��C��C�fC�CL�C� C�3C��C  C33CL�C� C�3C��C  C33CL�C� C�3C��C  C�CL�C� C��C��C�fC�C33CffC� C��C��C�fC�C33CL�C� C��C�3C��C  C�C33CL�CffC��C�3C��C�fC  C�C33CL�CffC� C��C��C�3C��C�fC  C  C�C33C33CL�CL�CffCffC� C� C��C��C�3C�3C�3C�3C��C��C��C��C��C��C�fC��C�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�f@9��@Fff@L��@L��@Y��@fff@l��@s33@y��@�  @�ff@���@���@�  @�33@�ff@���@�  @�33@���@���@���@�33@�ff@���@�  @�33@�ff@ə�@�  @�33@�ff@ٙ�@���@�33@�ff@陚@���@�  @�33@�ff@���A   A��A33A��AffA  A33A��AffA  A��A33A��A  A��A��AffAffA!��A#33A&ffA(  A)��A+33A.ffA0  A1��A4��A6ffA8  A;33A<��A@  AA��AC33AD��AH  AI��AK33ANffAP  AQ��AT��AVffAX  AY��A\��A^ffA`  Aa��Ad��AfffAh  Ak33Al��AnffAp  As33At��AvffAy��A{33A|��A�  A���A���A�33A�  A���A�ffA�33A�  A���A�ffA�33A���A���A�33A�  A���A�ffA�33A�  A���A�ffA�33A���A���A�ffA�  A���A�ffA�33A�  A���A�ffA�33A���A���A�ffA�33A���A���A�ffA�  A���A���A�33A�  A���A�ffA�33A�  A���A�ffA�33A���A���A�ffA�33A���A���A�ffA�  A���A���A�33A�  A���A�ffA�33A���Aə�A�ffA�33A���A͙�A�33A�  A���A�ffA�33A�  Aՙ�A�ffA�  A���Aٙ�A�33A�  Aݙ�A�ffA�33A���AᙚA�33A�  A���A�ffA�33A���A陚A�ffA�  A���A�ffA�33A���A�AQ��A�  A���A�ffA�33A�  A���A�ffA�  A���A���A�33B   B ��B33B  BffB��B��B  B��B33B��BffB��B33B  BffB��B	��B
  B
ffB
��B33B  BffB��B33B��B  B��B33B��B  BffB��B33B  BffB��B33B��B  BffB��B��B  BffB��B33B��B  BffB33B��B  BffB��B33B��B  B��B33B��B  BffB��B33B��B   B ��B!33B!��B"  B"ffB"��B#33B#��B$ffB$��B%33B%��B&  B&ffB&��B'33B'��B(ffB(��B)33B)��B*  B*ffB*��B+33B,  B,ffB,��B-33B-��B.  B.ffB/33B/��B0  B0ffB0��B133B1��B2  B2��B333B3��B4  B4ffB4��B533B5��B6  B6ffB6��B733B7��B8  B8ffB8ffB8��B933B9��B:  B:ffB:��B:��B;33B;��B<  B<ffB<��B<��B=33B=��B>  B>  B>ffB>��B?33B?��B@  B@  B@ffB@��BA33BA��BB  BB  BBffBB��BC33BC��BD  BD  BDffBD��BE33BE33BE��BF  BFffBF��BG33BG33BG��BH  BHffBH��BH��BI33BI��BJ  BJffBJffBJ��BK33BK��BL  BL  BLffBL��BM33BM��BN  BN  BNffBN��BO33BO��BO��BP  BPffBP��BQ33BQ33BQ��BR  BRffBR��BS33BS33BS��BT  BTffBT��BT��BU33BU��BV  BVffBVffBV��BW33BW��BX  BXffBX��BX��BY33BY��BZ  BZffBZ��B[33B[��B\  B\  B\ffB\��B]33B]��B^  B^ffB^��B_33B_��B`  B`ffB`��Ba33Ba��Bb  Bb  BbffBb��Bc33Bc��Bd  BdffBd��Be33Be��Bf  BfffBf��Bg33Bg��Bh  BhffBh��Bi33Bi��Bj  BjffBj��Bk33Bk��Bl  BlffBl��Bm33Bm��Bn  BnffBn��Bo33Bp  BpffBp��Bq33Bq��Br  BrffBr��Bs33Bs��Bt  BtffBt��Bu33Bu��Bv  BvffBv��Bw33Bw��Bx  BxffBx��By33By��Bz  BzffBz��B{33B{��B|  B|��B}33B}��B~  B~ffB~��B33B��B�  B�33B�ffB���B���B�  B�33B�ffB���B���B�  B�ffB���B���B�  B�33B�ffB���B���B�  B�33B�ffB���B���B�  B�33B�ffB���B���B�  B�  B�ffB�ffB���B���B�  B�33B�ffB���B���B�  B�33B�ffB���B���B���B�  B�33B�ffB���B���B�  B�33B�ffB���B���B�  B�33B�ffB���B���B�  B�33B�ffB���B���B�  B�33B�ffB���B���B�  B�33B�ffB���B���B�  B�33B�ffB���B���B�  B�33B�ffB���B���B�  B�33B�ffB���B�  B�  B�ffB���B���B�  B�33B�ffB���B���B�  B�33B���B���B�  B�33B�ffB���B���B�  B�33B�ffB���B�  B�33B�ffB���B���B�  B�33B�ffB���B�  B�33B�ffB���B���B�  B�ffB���B���B�  B�33B�ffB���B�  B�33B�ffB���B���B�  B�ffB���B���B�  B�33B�ffB���B�  B�33B�ffB���B���B�33B�ffB���B���B�  B�33B���B���B�  B�33B���B���B�  B�33B�ffB���B�  B�33B�ffB���B���B�33B�ffB���B���B�  B�ffB���B���B�  B�ffB���B���B�  B�33B�ffB���B�  B�33B�ffB���B�  B�33B�ffB���B�  B�33B�ffB���B�  B�33B�ffB���B���B�33B�ffB���B���B�33B�ffB���B���B�33B�ffB���B���B�  B�ffB���B���B�  B�ffB���B���B�  B�ffB���B���B�  B�ffB���B���B�  B�33B���B���B�  B�33B�ffB���B�  B�33B�ffB���B���B�  B�33B�ffB���B���B�  B�33B�ffB���B���B�  B�33B�ffB���B���B�  B�33B�ffB���B���B�  B�33B�ffB���B���B�  B�33B�ffB���B���B�  B�33B�ffB���B���B���B�  B�33B�ffB���B���B�  B�33B�ffB���B���B�  B�33B�ffB���B���B�  B�33B�ffB�ffB���B���B�  B�33B�ffB�B���B�  B�33B�ffBÙ�B���B�  B�33B�ffB�ffBę�B���B�  B�33B�ffBř�B���B�  B�33B�ffBƙ�B���B�  B�33B�ffBǙ�BǙ�B���B�  B�33B�ffBș�B���B�  B�33B�ffBə�B���B�  B�33B�ffBʙ�B���B�  B�  B�33B�ffB˙�B���B�  B�33B�ffB̙�B���B�  B�33B�ffB͙�B���C�fC  C33CffC��C��C  C33CffC��C�3C�fC�CL�C� C�3C��C  C33CffC��C��C�fC�CL�C� C�3C��C  C33CL�C� C�3C��C  C33CL�C� C�3C��C  C�CL�C� C��C��C�fC�C33CffC� C��C��C�fC�C33CL�C� C��C�3C��C  C�C33CL�CffC��C�3C��C�fC  C�C33CL�CffC� C��C��C�3C��C�fC  C  C�C33C33CL�CL�CffCffC� C� C��C��C�3C�3C�3C�3C��C��C��C��C��C��C�fC��C�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111119999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999993333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999   @%@l(�@�{@�{A
=A;
=A[
=A{
=A��A��A��A��AͅA݅A�A��BBBBB&B.B7(�B>BFBNBVB^BfBnBvB~B�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB��{B�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHC��C��C��C��C	��C��C��C��C��C��C��C��C�>G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O���G���G���G��W
=�W
=�#���W
=�W
=��G��W
=�W
=�W
=�W
=��G��W
=�W
=��G��W
=���R�W
=��G��W
=��G��#���W
=�W
=��G��W
=���R�W
=��G��W
=�W
=�W
=�W
=�W
=�W
=�W
=�W
=�W
=�W
=�W
=�W
=�#���W
=�W
=�W
=�W
=��G��W
=��G�>B�\=�Q�W
=�W
=�W
=��G��W
=��G��W
=�W
=�W
=�W
=�W
=�W
=��G��W
=�W
=�W
=�W
=�W
=�W
=���R��G����R�W
=��G��W
=�W
=�#���#���W
=�W
=�W
=�W
=�W
=�W
=���R�W
=�W
=��G��W
=��G��W
=�W
=�W
=�W
=�W
=�W
=�W
=�W
=�W
=��G���G��W
=�W
=�W
=��G��W
=�W
=�W
=�W
=�W
=��G���G��W
=�W
=�W
=�W
=�W
=�W
=�W
=�W
=��G��W
=���R�W
=�W
=�W
=��G���G��W
=���R�W
=�W
=�W
=�W
=�W
=�#���#���W
=�W
=�W
=�W
=��G��W
=�W
=�#���#���W
=�W
=��G���G��W
=�W
=��G��#��>B�\�#���W
=��G���G�=�Q�#���W
=���R�W
=�W
=��G��W
=�W
=��G���G��W
=�W
=�W
=��G��#���W
=�W
=�W
=��G��#��>B�\>B�\�#���W
=��G��W
=�W
=��G��#���#����G��W
=��G��W
=�W
=��G���G���G��W
=�W
=��G���G��W
=�W
=��G��W
=��G���G��#����G��W
=�#���#���#���#��=�Q�#���#���#����G��#���#���#���#���#��=�Q�#���#���#���#���#���#��=�Q�#���#����G��#��=�Q�#���#���#��>B�\=�Q�#��=�Q��G���G���G��#���#���#���#���#��=�Q�#���#���#���#���#���#���#���#��=�Q�#����G�=�Q�=�Q�#���#���#���#��=�Q�#��=�Q�#��=�Q�#��=�Q�=�Q�#��=�Q�#���#���#���#���#���#���#��=�Q�#��=�Q�=�Q�#����G�=�Q�#���#��=�Q��G�=�Q�=�Q�#���#���#���#���#���#���#��=�Q�=�Q��G��#���#��=�Q�#���#���#���#���#���#���#���#���#��=�Q�=�Q�#���#����G��#���#���#���#���#���#����G�=�Q�#���#����G��#���#��=�Q�#���#���#���#���#���#���#���#���#����G�=�Q�=�Q�#����G��#���#��=�Q�=�Q��G��W
=�#���#���#���#��=�Q�#���#���#���#���#��=�Q�#���#���#���#���#��=�Q�=�Q�#��=�Q�#��=�Q�=�Q�#���#���#���#���#���#���#���#��=�Q�#���#���#���#���#����G��#���#���#���#��=�Q�=�Q�#���#���#���#���#��=�Q�=�Q�#��=�Q��G��#��=�Q�=�Q�#��=�Q�#���#���#��=�Q��G�=�Q�#����G���G�=�Q�#���#���#���#��=�Q�=�Q�#��=�Q�=�Q�#���#���#���#���#��=�Q�#���#���#���#���#��=�Q�=�Q�=�Q�#���#����G��#��=�Q�#��>B�\=�Q�#���#���#��=�Q�=�Q�#��=�Q�#����G�=�Q�=�Q�#��>B�\>B�\�#���#��=�Q�#��=�Q�#���#���#���#���W
=�#����G��#���#��=�Q�>B�\>�z�>�z�>Ǯ>��H>��H?0��?0��?0��?J=q?J=q?}p�?}p�?��?�Q�?�Q�?�Q�?��?��R?˅?˅?˅?�Q�?��?��?��?��R@@(�@(�@�\@�\@��@%@%@2�\@8��@8��@E@R�\@X��@_\)@e@l(�@x��@\*@��H@�{@�G�@�z�@��H@�{@�G�@��@��H@��H@�G�@�z�@��@�{@�G�@�z�@��@�{@�G�@�z�@Ϯ@��H@�G�@�z�@߮@��H@�{@�G�@�z�@�@�{@�G�@�z�@��Ap�A
=A=pA�
A	p�A
=A��A=pA�
A
=A��A�
Ap�Ap�A��A=pA!p�A#
=A$��A&=pA)p�A+
=A,��A/�
A1p�A3
=A6=pA7�
A;
=A<��A>=pA?�
AC
=AD��AF=pAIp�AK
=AL��AO�
AQp�AS
=AT��AW�
AYp�A[
=A\��A_�
Aap�Ac
=Af=pAg�
Aip�Ak
=An=pAo�
Aqp�At��Av=pAw�
A{
=A|��A~=qA��RA��A�Q�A��A��RA��A��A��A��RA�Q�A��A��RA��A�Q�A��A��RA��A��A��A��RA�Q�A��A��A��A�Q�A��A��RA��A��A��A��RA�Q�A��A��A��RA�Q�A��A��A��A�Q�A��A��RA��A�Q�A��A��RA��A��A��A��RA�Q�A��A��A��RA�Q�A��A��A��A�Q�A��A��RA��A�Q�A��AĸRA�Q�A��A��AȸRA�Q�A��A̸RAͅA�Q�A��AиRAхA��A��AՅA�Q�A��AظRAمA��A��AܸRA�Q�A��A�RA�A�Q�A��A�RA�Q�A��A��A�A�Q�A��A�RA�Q�A��AL��A�A�Q�A��A��RA��A��A��A��A�Q�A��A��RA��A��A��B B(�B�\B\)BB�\B��B\)B(�B�\B��BB(�B�\B\)BB	(�B	�\B	��B
B(�B�\B��B\)BB�\B��B\)BB(�B�\B��BB(�B�\B��B\)BB(�B�\B\)BB(�B�\B��B\)BB(�B��B\)BB(�B�\B��B\)BB�\B��B\)BB(�B�\B��B\)BB�\B��B \)B B!(�B!�\B!��B"\)B#(�B#�\B#��B$\)B$B%(�B%�\B%��B&\)B'(�B'�\B'��B(\)B(B)(�B)�\B)��B*B+(�B+�\B+��B,\)B,B-(�B-��B.\)B.B/(�B/�\B/��B0\)B0B1�\B1��B2\)B2B3(�B3�\B3��B4\)B4B5(�B5�\B5��B6\)B6B7(�B7(�B7�\B7��B8\)B8B9(�B9�\B9�\B9��B:\)B:B;(�B;�\B;�\B;��B<\)B<B<B=(�B=�\B=��B>\)B>B>B?(�B?�\B?��B@\)B@B@BA(�BA�\BA��BB\)BBBBBC(�BC�\BC��BC��BD\)BDBE(�BE�\BE��BE��BF\)BFBG(�BG�\BG�\BG��BH\)BHBI(�BI(�BI�\BI��BJ\)BJBJBK(�BK�\BK��BL\)BLBLBM(�BM�\BM��BN\)BN\)BNBO(�BO�\BO��BO��BP\)BPBQ(�BQ�\BQ��BQ��BR\)BRBS(�BS�\BS�\BS��BT\)BTBU(�BU(�BU�\BU��BV\)BVBW(�BW�\BW�\BW��BX\)BXBY(�BY�\BY��BZ\)BZBZB[(�B[�\B[��B\\)B\B](�B]�\B]��B^\)B^B_(�B_�\B_��B`\)B`B`Ba(�Ba�\Ba��Bb\)BbBc(�Bc�\Bc��Bd\)BdBe(�Be�\Be��Bf\)BfBg(�Bg�\Bg��Bh\)BhBi(�Bi�\Bi��Bj\)BjBk(�Bk�\Bk��Bl\)BlBm(�Bm�\Bm��BnBo(�Bo�\Bo��Bp\)BpBq(�Bq�\Bq��Br\)BrBs(�Bs�\Bs��Bt\)BtBu(�Bu�\Bu��Bv\)BvBw(�Bw�\Bw��Bx\)BxBy(�By�\By��Bz\)BzB{�\B{��B|\)B|B}(�B}�\B}��B~\)B~B(�B�[B��B�.B�aHB��{B�ǮB���B�.B�aHB�ǮB���B�.B�aHB��{B�ǮB���B�.B�aHB��{B�ǮB���B�.B�aHB��{B�ǮB���B�.B�aHB�aHB�ǮB�ǮB���B�.B�aHB��{B�ǮB���B�.B�aHB��{B�ǮB���B�.B�.B�aHB��{B�ǮB���B�.B�aHB��{B�ǮB���B�.B�aHB��{B�ǮB���B�.B�aHB��{B�ǮB���B�.B�aHB��{B�ǮB���B�.B�aHB��{B�ǮB���B�.B�aHB��{B�ǮB���B�.B�aHB��{B�ǮB���B�.B�aHB��{B�ǮB���B�aHB�aHB�ǮB���B�.B�aHB��{B�ǮB���B�.B�aHB��{B���B���B�aHB��{B�ǮB���B�.B�aHB��{B�ǮB���B�aHB��{B�ǮB���B�.B�aHB��{B�ǮB�.B�aHB��{B�ǮB���B�.B�aHB�ǮB���B�.B�aHB��{B�ǮB�.B�aHB��{B�ǮB���B�.B�aHB�ǮB���B�.B�aHB��{B�ǮB�.B�aHB��{B�ǮB���B�.B��{B�ǮB���B�.B�aHB��{B���B�.B�aHB��{B���B�.B�aHB��{B�ǮB�.B�aHB��{B�ǮB���B�.B��{B�ǮB���B�.B�aHB�ǮB���B�.B�aHB�ǮB���B�.B�aHB��{B�ǮB�.B�aHB��{B�ǮB�.B�aHB��{B�ǮB���B�aHB��{B�ǮB���B�aHB��{B�ǮB���B�.B��{B�ǮB���B�.B��{B�ǮB���B�.B��{B�ǮB���B�.B�aHB�ǮB���B�.B�aHB�ǮB���B�.B�aHB�ǮB���B�.B�aHB�ǮB���B�.B�aHB��{B���B�.B�aHB��{B�ǮB���B�aHB��{B�ǮB���B�.B�aHB��{B�ǮB���B�.B�aHB��{B�ǮB���B�.B�aHB��{B�ǮB���B�.B�aHB��{B�ǮB���B�.B�aHB��{B�ǮB���B�.B�aHB��{B�ǮB���B�.B�aHB��{B�ǮB���B���B�.B�aHB��{B�ǮB���B�.B�aHB��{B�ǮB���B�.B�aHB��{B�ǮB���B�.B�aHB��{B�ǮB�ǮB���B�.B�aHB��{B�ǮB���B�.B�aHB{B�ǮB���B�.B�aHBÔ{B�ǮB�ǮB���B�.B�aHBĔ{B�ǮB���B�.B�aHBŔ{B�ǮB���B�.B�aHBƔ{B�ǮB���B���B�.B�aHBǔ{B�ǮB���B�.B�aHBȔ{B�ǮB���B�.B�aHBɔ{B�ǮB���B�.B�aHB�aHBʔ{B�ǮB���B�.B�aHB˔{B�ǮB���B�.B�aHB̔{B�ǮB���B�.C�
C��C��C
CJ>C}qC��C��C
CJ>Cc�C�
C�>C�qC0�Cc�C}qC��C��C
CJ>C}qC�
C�>C�qC0�Cc�C}qC��C��C�qC0�Cc�C}qC��C��C�qC0�Cc�C}qC��C�>C�qC0�CJ>C}qC�
C�>C��C
C0�CJ>C}qC�
C�>C��C�qC0�CJ>Cc�C}qC��C�>C��C�qC
CJ>Cc�C}qC�
C��C�>C��C�qC
C0�CJ>CJ>Cc�C}qC�
C��C��C�>C��C��C�qC�qC
C
C0�C0�CJ>CJ>Cc�Cc�Cc�Cc�C}qC}qC}qC}qC}qC}qC�
C}qC�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
@%@2�\@8��@8��@E@R�\@X��@_\)@e@l(�@x��@\*@��H@�{@�G�@�z�@��H@�{@�G�@��@��H@��H@�G�@�z�@��@�{@�G�@�z�@��@�{@�G�@�z�@Ϯ@��H@�G�@�z�@߮@��H@�{@�G�@�z�@�@�{@�G�@�z�@��Ap�A
=A=pA�
A	p�A
=A��A=pA�
A
=A��A�
Ap�Ap�A��A=pA!p�A#
=A$��A&=pA)p�A+
=A,��A/�
A1p�A3
=A6=pA7�
A;
=A<��A>=pA?�
AC
=AD��AF=pAIp�AK
=AL��AO�
AQp�AS
=AT��AW�
AYp�A[
=A\��A_�
Aap�Ac
=Af=pAg�
Aip�Ak
=An=pAo�
Aqp�At��Av=pAw�
A{
=A|��A~=qA��RA��A�Q�A��A��RA��A��A��A��RA�Q�A��A��RA��A�Q�A��A��RA��A��A��A��RA�Q�A��A��A��A�Q�A��A��RA��A��A��A��RA�Q�A��A��A��RA�Q�A��A��A��A�Q�A��A��RA��A�Q�A��A��RA��A��A��A��RA�Q�A��A��A��RA�Q�A��A��A��A�Q�A��A��RA��A�Q�A��AĸRA�Q�A��A��AȸRA�Q�A��A̸RAͅA�Q�A��AиRAхA��A��AՅA�Q�A��AظRAمA��A��AܸRA�Q�A��A�RA�A�Q�A��A�RA�Q�A��A��A�A�Q�A��A�RA�Q�A��AL��A�A�Q�A��A��RA��A��A��A��A�Q�A��A��RA��A��A��B B(�B�\B\)BB�\B��B\)B(�B�\B��BB(�B�\B\)BB	(�B	�\B	��B
B(�B�\B��B\)BB�\B��B\)BB(�B�\B��BB(�B�\B��B\)BB(�B�\B\)BB(�B�\B��B\)BB(�B��B\)BB(�B�\B��B\)BB�\B��B\)BB(�B�\B��B\)BB�\B��B \)B B!(�B!�\B!��B"\)B#(�B#�\B#��B$\)B$B%(�B%�\B%��B&\)B'(�B'�\B'��B(\)B(B)(�B)�\B)��B*B+(�B+�\B+��B,\)B,B-(�B-��B.\)B.B/(�B/�\B/��B0\)B0B1�\B1��B2\)B2B3(�B3�\B3��B4\)B4B5(�B5�\B5��B6\)B6B7(�B7(�B7�\B7��B8\)B8B9(�B9�\B9�\B9��B:\)B:B;(�B;�\B;�\B;��B<\)B<B<B=(�B=�\B=��B>\)B>B>B?(�B?�\B?��B@\)B@B@BA(�BA�\BA��BB\)BBBBBC(�BC�\BC��BC��BD\)BDBE(�BE�\BE��BE��BF\)BFBG(�BG�\BG�\BG��BH\)BHBI(�BI(�BI�\BI��BJ\)BJBJBK(�BK�\BK��BL\)BLBLBM(�BM�\BM��BN\)BN\)BNBO(�BO�\BO��BO��BP\)BPBQ(�BQ�\BQ��BQ��BR\)BRBS(�BS�\BS�\BS��BT\)BTBU(�BU(�BU�\BU��BV\)BVBW(�BW�\BW�\BW��BX\)BXBY(�BY�\BY��BZ\)BZBZB[(�B[�\B[��B\\)B\B](�B]�\B]��B^\)B^B_(�B_�\B_��B`\)B`B`Ba(�Ba�\Ba��Bb\)BbBc(�Bc�\Bc��Bd\)BdBe(�Be�\Be��Bf\)BfBg(�Bg�\Bg��Bh\)BhBi(�Bi�\Bi��Bj\)BjBk(�Bk�\Bk��Bl\)BlBm(�Bm�\Bm��BnBo(�Bo�\Bo��Bp\)BpBq(�Bq�\Bq��Br\)BrBs(�Bs�\Bs��Bt\)BtBu(�Bu�\Bu��Bv\)BvBw(�Bw�\Bw��Bx\)BxBy(�By�\By��Bz\)BzB{�\B{��B|\)B|B}(�B}�\B}��B~\)B~B(�B�[B��B�.B�aHB��{B�ǮB���B�.B�aHB�ǮB���B�.B�aHB��{B�ǮB���B�.B�aHB��{B�ǮB���B�.B�aHB��{B�ǮB���B�.B�aHB�aHB�ǮB�ǮB���B�.B�aHB��{B�ǮB���B�.B�aHB��{B�ǮB���B�.B�.B�aHB��{B�ǮB���B�.B�aHB��{B�ǮB���B�.B�aHB��{B�ǮB���B�.B�aHB��{B�ǮB���B�.B�aHB��{B�ǮB���B�.B�aHB��{B�ǮB���B�.B�aHB��{B�ǮB���B�.B�aHB��{B�ǮB���B�.B�aHB��{B�ǮB���B�aHB�aHB�ǮB���B�.B�aHB��{B�ǮB���B�.B�aHB��{B���B���B�aHB��{B�ǮB���B�.B�aHB��{B�ǮB���B�aHB��{B�ǮB���B�.B�aHB��{B�ǮB�.B�aHB��{B�ǮB���B�.B�aHB�ǮB���B�.B�aHB��{B�ǮB�.B�aHB��{B�ǮB���B�.B�aHB�ǮB���B�.B�aHB��{B�ǮB�.B�aHB��{B�ǮB���B�.B��{B�ǮB���B�.B�aHB��{B���B�.B�aHB��{B���B�.B�aHB��{B�ǮB�.B�aHB��{B�ǮB���B�.B��{B�ǮB���B�.B�aHB�ǮB���B�.B�aHB�ǮB���B�.B�aHB��{B�ǮB�.B�aHB��{B�ǮB�.B�aHB��{B�ǮB���B�aHB��{B�ǮB���B�aHB��{B�ǮB���B�.B��{B�ǮB���B�.B��{B�ǮB���B�.B��{B�ǮB���B�.B�aHB�ǮB���B�.B�aHB�ǮB���B�.B�aHB�ǮB���B�.B�aHB�ǮB���B�.B�aHB��{B���B�.B�aHB��{B�ǮB���B�aHB��{B�ǮB���B�.B�aHB��{B�ǮB���B�.B�aHB��{B�ǮB���B�.B�aHB��{B�ǮB���B�.B�aHB��{B�ǮB���B�.B�aHB��{B�ǮB���B�.B�aHB��{B�ǮB���B�.B�aHB��{B�ǮB���B���B�.B�aHB��{B�ǮB���B�.B�aHB��{B�ǮB���B�.B�aHB��{B�ǮB���B�.B�aHB��{B�ǮB�ǮB���B�.B�aHB��{B�ǮB���B�.B�aHB{B�ǮB���B�.B�aHBÔ{B�ǮB�ǮB���B�.B�aHBĔ{B�ǮB���B�.B�aHBŔ{B�ǮB���B�.B�aHBƔ{B�ǮB���B���B�.B�aHBǔ{B�ǮB���B�.B�aHBȔ{B�ǮB���B�.B�aHBɔ{B�ǮB���B�.B�aHB�aHBʔ{B�ǮB���B�.B�aHB˔{B�ǮB���B�.B�aHB̔{B�ǮB���B�.C�
C��C��C
CJ>C}qC��C��C
CJ>Cc�C�
C�>C�qC0�Cc�C}qC��C��C
CJ>C}qC�
C�>C�qC0�Cc�C}qC��C��C�qC0�Cc�C}qC��C��C�qC0�Cc�C}qC��C�>C�qC0�CJ>C}qC�
C�>C��C
C0�CJ>C}qC�
C�>C��C�qC0�CJ>Cc�C}qC��C�>C��C�qC
CJ>Cc�C}qC�
C��C�>C��C�qC
C0�CJ>CJ>Cc�C}qC�
C��C��C�>C��C��C�qC�qC
C
C0�C0�CJ>CJ>Cc�Cc�Cc�Cc�C}qC}qC}qC}qC}qC}qC�
C}qC�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111119999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999993333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�ZA�XA�Q�A�E�A�C�A�G�A�C�A�=qA�-A���AǛ�A�;dA�7LA�%A��mA���A�ĜAƴ9AƅA�M�A��AőhA�?}A��A��yAľwAĕ�Aç�A�A��A�n�A�33A�5?A�VA�hsA��yA��!A���A�dZA�M�A�$�A��7A��A���A�-A���A��A��A�v�A�E�A���A��uA��A��A�ZA���A�VA�\)A���A�K�A��A�p�A���A���A���A�`BA��`A��DA�E�A���A�5?A���A��A�A�A�hsA�+A�ĜG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A��HA�^5A�n�A�bNA�?}A�9XA�Q�A�-A���A�;dAð!A���AżjA��A��!A��A�l�A���A��A���A�dZA�ĜA�VA�S�A���Aơ�A���A���A�hsA���A��;A���A��DA�hsA���A�=qA�33A�dZA�^5A�ZAǾwA��A�/A���A���A�%A��yAǛ�A�;dA��PA�bNA�dZA��A�Aƛ�A�z�A���AA��RA�"�A�7LA��wA��/A�  A�A�"�A�ƨA���A���A��9A��wA���A�=qA��
A��\A��HA�JA� �A�G�A�jA���A���A���A�7LA���A��+A�dZA��-AÉ7A�ZA�A�Q�A��A�1'A�5?A���A�%A�G�A�ffA��mA��-A�A�ffA�oA�x�A�K�AŋDA��jA���A��#A�+A���A��`A�ZA�?}A���A��A���A���A�|�A��A��^A�{A���A���A�7LA�x�A�~�Aã�A�ffA�AĲ-AÕ�A�(�A�\)A���A���A���A�`BAǟ�A�
=A��#A���A�ffA���A��A�hsA�l�Aǰ!A��
A�bNA�;dA�A�A�A��;A�r�A�v�A�hsAąAÏ\A�^5A�XA�\)A�?}A�1'A�hsA�ȴAøRA� �A���AƼjA�O�A���A��
A��A�`BAƑhA���A��A�z�AŴ9A�n�A�l�A�t�A�dZA��`A���A�K�A�`BA�  A�t�A�r�A�hsA��PA�S�A�x�A�hsA�ĜA�dZA�dZA�K�A�r�A�dZA�G�A�oA�I�A�t�A��A�oA�bNA�\)A�ffAǁAƟ�A�^5A�`BA�ffA�^5A�^5A�^5A�^5A�x�A�`BA�`BA�`BA�`BA�^5A�ffA�^5A�l�A�dZA�l�A�bNA�bNA�ffA�ffA�x�A�jA�p�AȃA�bNA�l�A�jA�p�A�p�A�dZA�dZA�jA�XA�hsA�l�A�VA�ffA�hsA�\)A�l�A�jA�hsA�dZA�hsA�t�A�x�A�jA�jA�ffA�n�A�hsA�ffA�p�A�l�A�n�A�dZA�hsA�t�A�hsA�ffA�jA�v�A�ffA�n�A�l�A�jA�n�A�p�A�p�A�ffA�bNA�bNA�jA�bNA�bNA�bNA�bNA�hsA�bNA�dZA�dZA�dZA�`BA�dZA�dZA�hsA�jA�dZA�bNA�dZA�hsA�^5A�^5A�dZA�bNA�jA�dZA�`BA�hsA�`BA�hsA�hsA�n�A�jA�ffA�l�A�p�A�ffA�jA�ffA�x�A�bNA�dZA�^5A�ZA�XA�XA�VA�\)A�ffA�ZA�^5A�\)A�ZA�\)A�r�A�l�A�n�A�jA�bNA�ffA�p�A�jA�hsA�l�A�l�A�l�A�`BA�ffA�dZA�dZA�^5A�\)A�^5A�^5A�x�A�^5A�M�A�?}A�`BA�^5A�^5A�l�A�dZA�`BA�bNA�\)A�ZA�XA�\)A�ZA�`BA�\)A�^5A�\)A�^5A�\)A�XA�XA�ZA�ffA�XA�^5A�XA�\)A�/A�l�A�dZA�^5A�hsA�\)A�bNA�^5A�ZA�ZA�9XA�Q�A�O�A�S�A�O�A�O�A�O�A�XA�XA�XA�VA�Q�A�S�A�VA�XA�S�A�S�A�O�A�Q�A�VA�XA�S�A�VA�ZA�^5A�VA�VA�Q�A�Q�Aǝ�A�VA�Q�A�O�A�VA�ZA�S�A�O�A�O�A�XA�O�A�O�A�O�A�S�A�\)A�VA�l�A�ffA�jA�ffA�ffA�jA�bNA�ZA�ZA�^5A�`BA�dZA�VA�hsA�l�A�\)A�`BA�bNA�ZA�XA�`BA�l�A�XA�^5A�`BA�dZA�\)A�\)A�bNA�\)A�`BA�`BA�ZA�bNA�bNA�\)A�bNA�ZA�XA�\)A�\)A�{A�VA�ZA�&�A�XA�VA�XA�VA�XA�XA�ZA�XA�hsA�S�A�O�A�\)A�ZA�Q�A�Q�A�\)A�O�A�VA�K�A�XA�VA�VA�\)A�XA�VA�^5A�Q�A�\)A�ZA�VA�ZA�ZA�ffA�I�A�S�A�XA�XA�M�A�M�A�ZA�G�A�E�A�S�A�O�A�VA�S�A�M�A�K�A�Q�A�Q�A�VA�VA�Q�A�XA�S�A�VA�Q�A�I�A�K�A�K�A�G�A�C�A�C�A�Q�A�I�A�M�A�I�A�E�A�E�A�K�A�?}A�A�A�C�A�C�A�;dA�?}A�?}A�9XA�;dA�9XA�33A�7LA�5?A�5?A�;dA�7LA�7LA�7LA�5?A�5?A�9XA�7LA�5?A�9XA�9XA�9XA�7LA�9XA�9XA�9XA�9XA�9XA�5?A�7LA�=qA�=qA�=qA�?}A�?}A�?}A�=qA�=qA�;dA�=qA�=qA�A�A�=qA�;dA�?}A�?}A�=qA�9XA�?}A�?}A�?}A�;dA�9XA�;dA�5?A�7LA�5?A�5?A�5?A�5?A�5?A�5?A�7LA�5?A�5?A�7LA�5?A�5?A�5?A�33A�33A�33A�33A�33A�33A�5?A�33A�-A�+A�+A�-A�+A�+A�+A�(�A� �A��A��A�{A�VA�VA�VA�
=A�A�A���A���A��A��A��A��A��A��mA��HA��;A��;A��#A��A��
A���AǾwAǲ-AǮAǗ�AǇ+A�p�A�ZA�Q�A�;dA�;dA�;dA�9XA�9XA�9XA�9XA�7LA�5?A�5?A�7LA�5?A�7LA�5?A�7LA�5?A�7LA�7LA�7LA�33A�7LA�5?A�7LA�5?A�7LA�5?A�33A�33A�33A�1'A�+A�&�A�"�A��A��A�{A��A�oA�1A���A���A��A��A��A��A��A��yA��A��yA��yA��mA��`A��TA��;A��;A��#A�C�A��
A��
A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A�ȴA�ȴA�ƨA�ƨA�AƾwAƼjAƼjAƺ^AƸRAƶFAƴ9Aƴ9AƲ-Aư!Aư!Aư!Aư!Aư!Aư!AƮAư!AƲ-Aư!Aư!Aư!AƮAƮAƮAƬAƬAƩ�AƩ�Aƥ�Aƣ�Aơ�Aƛ�AƓuAƇ+AƁA�v�A�r�A�n�A�l�A�jA�dZA�`BA�^5A�\)A�XA�VA�VA�S�A�Q�A�O�A�M�A�I�A�G�A�I�A�G�A�E�A�C�A�C�A�A�A�?}A�?}A�=qA�9XA�5?A�1'A�&�A�$�A�$�A�$�A� �A� �A��A��A�oA�VA�JA�1A�A�A�  A���A���A��A��`A��HA���AŲ-Aŏ\A�v�A�l�A�hsA�`BA�\)A�XA�S�A�O�A�O�A�M�A�I�A�E�A�E�A�C�A�A�A�A�A�C�A�A�A�A�A�A�A�=qA�;dA�;dA�7LA�7LA�5?A�33A�1'A�1'A�/A�/A�-A�-A�-A�+A�-A�-A�-A�-A�-A�"�A�"�A� �A��A�bA�VA�
=A�A���A���A���A���A���A���A���A���A���A���A���A��A��A��A��A��A��A��A��A��A��yA��mA��`A��TA��HA��/A��#A��#A��;A��/A���A��
A��
A���A���A���A���A���A�ƨA�ĜA�A���AļjAļjAļjAĺ^AĶFAĶFAĴ9AĴ9AĮAİ!AĮAħ�Aģ�Aģ�Aģ�Aģ�Aĝ�Aě�Aę�Aę�Aė�Aė�AēuAĕ�AēuAđhAď\AđhAď\AċDAċDAć+Ać+AąAāA�ZA�r�A�v�A�r�A�C�A�?}A�VA�Q�A�A�A�9XA�1'A�&�A�oA��A��yA�ĜAÛ�A�jA� �A��;A�A¶FA°!A§�A¡�A�A�APA7AA�~�A�v�A�?}A��A�A��A��#A���A��RA���A���A���A��A�r�A�S�A�=qA�(�A��A�VA�%A���A��A��yA��HA��#A��A���A���A���A�ƨA��wA��RA��A���A���A���A��uA�|�A�x�A�l�A�n�A�jA�ffA�ffA�`BA�`BA�\)A�^5A�\)A�\)A�ZA�XA�XA�ZA�XA�VA�XA�S�A�VA�Q�A�XA�Q�A�O�A�Q�A�S�A�O�A�O�A�O�A�M�A�7LA��A���A�jA�\)A�I�A�A�A�?}A�C�A�C�A�-A�/A�1'A�"�A�$�A�"�A��A� �A�$�A�(�A�(�A�+A�1'A�5?A�33A�5?A�5?A�5?A�33A�-A�/A�/A��A� �A��A�bA�%A�A���A��A��HA��#A���A��FA��!A���A��\A��A��A�~�A�x�A�r�A�p�A�l�A�ffA�`BA�^5A�\)A�ZA�XA�O�A�I�A�I�A�A�A�C�A�G�A�;dA�"�A�"�A�1A�  A���A��A��A��A��A��mA���A���A��RA��A���A��wA�ĜA���A�ȴA�ƨA�A��9A���A��hA��PA��\A��hA���A��A��FA��RA��RA��FA��RA��-A��!A��!A��!A��A��!A��A��A��A���A���A��A���A���A���A���A���A���A���A��hA��+A�|�A�t�A�p�A�ffA�hsA�ffA�dZA�^5A�^5A�^5A�^5A�`BA�ZA�VA�S�A�I�A�I�A�I�A�G�A�G�A�G�A�I�A�I�A�K�A�G�A�K�A�O�A�Q�A�O�A�O�A�O�A�O�A�Q�A�O�A�Q�A�K�A�I�A�G�A�A�A�33A�&�A��A�VA�
=A�1A�1A���A���A��A��A��A��A��A��A��HA���A���A���A���A�\)A�I�A�C�A�9XA�"�A�{A�oA�oA�oA�{A�{A�oA��A�&�A�(�A�(�A�(�A�1'A�/A�-A�-A�-A�+A��
A���A��+A�|�A�|�A��A���A���A��^A��A���A��A��/A�A��A�p�A�p�A�v�A�|�A��A��A�bNA�/A�oA�A�A�1A�
=A��A��A��A��A�1A�  A���A��A��TA��wA���A���A���A���A���A���A���A���A���A���A��PA��DA��A��A��A�v�A�XA�Q�A�=qA�"�A��A�oA�%A���A��A��#A��
A���A���A���A���A�ȴA���A�ĜA��wA��wA��^A��^A��9A��!A���A���A���A���A���A���A��hA��7A��A��A�v�A�bNA�Q�A�9XA�/A�&�A�"�A��A�JA�%A���A��A��mA��A���A��+A�|�A�p�A�hsA�ZA�Q�A�G�A�?}A�+A�oA��
A��-A���A��A�z�A�x�A�x�A�p�A�bNA�XA�K�A�?}A�9XA�9XA�5?A�5?A�5?A�5?A�33A�/A�$�A� �A��A��A��A��A�{A�oA�{A�oA�bA�VA�
=A�A���A��A��A��A��yA��mA��mA��TA��TA��TA��;A��#A��
A��^A�x�A�r�A�~�A��A��+A��+A��7A��A���A��A��A��yA��A�?}A�oA��A��/A�-A���A���A�1A��A�+A�G�A�C�A�=qA�;dA�=qA�Q�A�Q�A�Q�A�Q�A�Q�A�O�A�?}A�C�A�K�A�n�A�l�A�l�A�r�A��A��7A��hA��hA��DA�l�A�I�A�33A�"�A��A��A�{A�bA�JA�1A�%A�A�A�A�A�A�  A���A���A��A��HA��A���A���A�ĜA�A��RA���A��A�hsA�M�A�=qA�+A�$�A�(�A�-A� �A��A�VA�A�JA�1A�A�A���A���A���A���A�%A�oA�1A���A���A���A���A���A�A�A�
=A�{A��A�
=A�%A�
=A�{A��A� �A�(�A��A��A��A��A�ZA�ZA�\)A�XA�^5A�XA�XA�S�A�ZA�Q�A�XA�XA�VA�\)A�\)A�ZA�\)A�XA�ZA�XA�XA�^5A�S�A�VA�VA�K�A�M�A�Q�A�M�A�M�A�I�A�I�A�I�A�I�A�E�A�E�A�G�A�G�A�G�A�E�A�C�A�A�A�E�A�A�A�E�A�G�A�C�A�C�A�I�A�E�A�A�A�A�A�C�A�C�A�G�A�A�A�E�A�E�A�E�A�E�A�E�A�I�A�A�A�A�A�C�A�?}A�A�A�A�A�G�A�I�A�I�A�G�A�I�A�E�A�E�A�E�A�E�A�E�A�G�A�K�A�I�A�C�A�C�A�K�A�K�A�G�A�I�A�E�A�C�A�A�A�C�A�C�A�=qA�=qA�=qA�?}A�=qA�=qA�=qA�=qA�?}A�?}A�=qA�=qA�=qA�=qA�9XA�=qA�;dA�9XA�;dA�;dA�;dA�9XA�5?A�33A�5?A�5?A�33A�33A�33A�1'A�/A�-A�$�A��A��A��A��A�{A�
=A�VA�%A���A���A���A��A��A��A��A��yA��`A��`A��`A��HA��#A��#A���AǼjAǺ^Aǧ�AǓuAǇ+A�jA�XA�M�A�?}A�?}A�?}A�=qA�?}A�=qA�;dA�7LA�9XA�9XA�;dA�7LA�;dA�9XA�9XA�9XA�9XA�7LA�9XA�7LA�;dA�9XA�9XA�;dA�9XA�7LA�7LA�9XA�5?A�33A�-A�(�A�"�A��A��A��A��A�oA�%A���A���A��A���A��A��A��A��A��A��A��A��A��yA��yA��`A��`A��TA��;A��/A��/A��A���A���A���A���A���A��
A���A��
A���A���A���A���A���A���A���A���A���A���A���A���A�ƨA�ĜA���A���AƾwAƼjAƺ^Aƺ^AƸRAƶFAƴ9AƶFAƶFAƴ9Aƴ9AƶFAƴ9AƶFAƶFAƶFAƶFAƶFAƲ-AƲ-Aƴ9AƲ-Aư!Aư!AƮAƩ�AƩ�Aƥ�AƟ�Aƕ�AƉ7A�~�A�z�A�t�A�r�A�n�A�n�A�hsA�dZA�`BA�^5A�\)A�ZA�ZA�XA�VA�S�A�Q�A�O�A�M�A�K�A�K�A�I�A�I�A�G�A�E�A�E�A�C�A�A�A�?}A�9XA�1'A�+A�(�A�(�A�&�A�$�A�"�A��A��A��A�oA�bA�JA�1A�A�A���A���A��A��yA��#AžwAũ�AŃA�v�A�l�A�hsA�dZA�\)A�ZA�VA�S�A�S�A�O�A�I�A�I�A�G�A�E�A�E�A�G�A�C�A�E�A�E�A�C�A�A�A�?}A�=qA�;dA�9XA�9XA�7LA�5?A�33A�33A�1'A�1'A�1'A�1'A�/A�/A�/A�1'A�1'A�/A�(�A�$�A�"�A�"�A��A�oA�1A�A�A�A�  A�  A���A���A���A���A���A���A���A���A���A��A��A��A��A��A��A��A��mA��mA��`A��TA��HA��HA��/A��;A��HA��HA��HA��A��
A��
A��
A���A���A���A���A�ĜA�ƨA�A���A�A���AľwAĺ^AĸRAĸRAĶFAĲ-AĲ-Aİ!Aĩ�Aħ�Aħ�Aħ�Aħ�Aĥ�Aġ�Ağ�Ağ�Aĝ�Aě�Aě�Aě�Aė�Aė�Aė�Aė�Aĕ�AēuAĕ�AđhAď\Aď\Aď\Aď\Aĉ7Ać+AăA�|�A�z�A�z�A�p�A�\)A�Q�A�;dA�(�A�{A���A��`AöFAÏ\A�S�A�
=A���A�A¸RA°!A©�A¡�A¡�A�AhA\ADAA�p�A�33A�{A�  A��mA��A�ƨA��-A���A���A��hA�|�A�l�A�O�A�=qA�/A��A�oA�JA�%A���A��A��mA��TA��;A��/A���A���A���A���A��^A��-A��A���A���A��\A��A�z�A�r�A�n�A�l�A�jA�l�A�hsA�`BA�`BA�^5A�`BA�^5A�\)A�ZA�ZA�ZA�ZA�VA�VA�XA�VA�VA�VA�VA�S�A�VA�Q�A�Q�A�Q�A�O�A�I�A��A���A��\A�ffA�XA�M�A�E�A�C�A�E�A�E�A�1'A�7LA�&�A�&�A�$�A�$�A�"�A�"�A�"�A�(�A�-A�/A�33A�7LA�7LA�7LA�7LA�7LA�5?A�/A�1'A�+A�+A�"�A��A�JA�1A�%A�1A�A��A��HA�ĜA��jA���A���A���A��DA��A��A�|�A�x�A�r�A�n�A�ffA�`BA�`BA�\)A�^5A�ZA�VA�M�A�E�A�G�A�I�A�E�A�7LA�(�A��A�%A�1A���A��A��A��A��A��yA���A���A��wA��A��9A�ĜA�ȴA���A���A�ȴA�ĜA��^A��A���A���A��PA��PA���A��9A��^A��^A��RA��^A��jA��9A��9A��-A��-A��-A��9A��!A��A��A��A��A��A��A���A���A���A���A���A���A��uA��7A�|�A�v�A�l�A�hsA�hsA�ffA�bNA�`BA�`BA�bNA�bNA�^5A�XA�XA�S�A�K�A�K�A�K�A�I�A�I�A�K�A�M�A�K�A�K�A�I�A�Q�A�Q�A�Q�A�Q�A�Q�A�Q�A�Q�A�VA�S�A�O�A�O�A�M�A�K�A�E�A�;dA�+A��A�bA�JA�
=A�%A���A���A���A���A���A���A��A��A��`A��
A���A��RA��hA�dZA�O�A�C�A�33A��A��A��A��A�{A�{A�{A�{A�"�A�+A�(�A�+A�33A�33A�1'A�/A�1'A�/A�{A��wA���A��A�|�A��A��\A���A��9A���A���A���A��TA���A���A�v�A�r�A�t�A�z�A��A��+A�v�A�C�A� �A�JA�%A�1A�JA�bA��A��A��A�oA�%A�  A��A��A��mA�ĜA���A���A���A��A���A��A���A���A���A���A��\A��PA��+A��A��A�p�A�XA�O�A�7LA��A��A�{A�A���A��A��#A���A���A���A���A���A���A�ȴA�ĜA�A���A��jA��RA��FA��!A���A���A���A���A���A���A��uA��PA��+A��A�z�A�n�A�M�A�7LA�/A�+A�"�A��A�VA�A���A��A��A���A���A��PA�|�A�t�A�hsA�^5A�S�A�K�A�A�A�1'A�1A�A��A��uA��DA�x�A�x�A�r�A�l�A�jA�VA�G�A�;dA�=qA�9XA�7LA�7LA�9XA�5?A�33A�/A�&�A� �A��A��A��A��A��A��A��A��A�{A�oA�VA�JA�1A���A��A��A��A��A��A��yA��mA��`A��`A��`A�ĜA��A�r�A��A�~�A��+A��DA��\A��A���A���A��A��`A��A�E�A��A�  A��#A�`BA�%A���A�A��A�&�A�C�A�E�A�?}A�?}A�=qA�E�A�S�A�Q�A�Q�A�S�A�Q�A�G�A�?}A�I�A�n�A�l�A�l�A�n�A�|�A��A��uA���A���A��A�^5A�?}A�+A� �A��A��A�{A�bA�JA�
=A�1A�A�A�%A�A�A�A�  A���A���A��TA��A��
A���A�ƨA�ƨA���A��A��uA�|�A�ffA�M�A�A�A�5?A�+A�&�A�-A�/A�$�A��A��A��A�{A�oA�oA�oA��A�{A�oA�{A�{A��A��A�5?A�XA�`BA�r�A�z�A�A�A�9XA�/A�$�A�/A�=qA�;dA�+A�33A�A�A�E�A�?}A��mG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111119999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999993333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999   A�ZA�XA�Q�A�E�A�C�A�G�A�C�A�=qA�-A���AǛ�A�;dA�7LA�%A��mA���A�ĜAƴ9AƅA�M�A��AőhA�?}A��A��yAľwAĕ�Aç�A�A��A�n�A�33A�5?A�VA�hsA��yA��!A���A�dZA�M�A�$�A��7A��A���A�-A���A��A��A�v�A�E�A���A��uA��A��A�ZA���A�VA�\)A���A�K�A��A�p�A���A���A���A�`BA��`A��DA�E�A���A�5?A���A��A�A�A�hsA�+A�ĜG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A��HA�^5A�n�A�bNA�?}A�9XA�Q�A�-A���A�;dAð!A���AżjA��A��!A��A�l�A���A��A���A�dZA�ĜA�VA�S�A���Aơ�A���A���A�hsA���A��;A���A��DA�hsA���A�=qA�33A�dZA�^5A�ZAǾwA��A�/A���A���A�%A��yAǛ�A�;dA��PA�bNA�dZA��A�Aƛ�A�z�A���AA��RA�"�A�7LA��wA��/A�  A�A�"�A�ƨA���A���A��9A��wA���A�=qA��
A��\A��HA�JA� �A�G�A�jA���A���A���A�7LA���A��+A�dZA��-AÉ7A�ZA�A�Q�A��A�1'A�5?A���A�%A�G�A�ffA��mA��-A�A�ffA�oA�x�A�K�AŋDA��jA���A��#A�+A���A��`A�ZA�?}A���A��A���A���A�|�A��A��^A�{A���A���A�7LA�x�A�~�Aã�A�ffA�AĲ-AÕ�A�(�A�\)A���A���A���A�`BAǟ�A�
=A��#A���A�ffA���A��A�hsA�l�Aǰ!A��
A�bNA�;dA�A�A�A��;A�r�A�v�A�hsAąAÏ\A�^5A�XA�\)A�?}A�1'A�hsA�ȴAøRA� �A���AƼjA�O�A���A��
A��A�`BAƑhA���A��A�z�AŴ9A�n�A�l�A�t�A�dZA��`A���A�K�A�`BA�  A�t�A�r�A�hsA��PA�S�A�x�A�hsA�ĜA�dZA�dZA�K�A�r�A�dZA�G�A�oA�I�A�t�A��A�oA�bNA�\)A�ffAǁAƟ�A�^5A�`BA�ffA�^5A�^5A�^5A�^5A�x�A�`BA�`BA�`BA�`BA�^5A�ffA�^5A�l�A�dZA�l�A�bNA�bNA�ffA�ffA�x�A�jA�p�AȃA�bNA�l�A�jA�p�A�p�A�dZA�dZA�jA�XA�hsA�l�A�VA�ffA�hsA�\)A�l�A�jA�hsA�dZA�hsA�t�A�x�A�jA�jA�ffA�n�A�hsA�ffA�p�A�l�A�n�A�dZA�hsA�t�A�hsA�ffA�jA�v�A�ffA�n�A�l�A�jA�n�A�p�A�p�A�ffA�bNA�bNA�jA�bNA�bNA�bNA�bNA�hsA�bNA�dZA�dZA�dZA�`BA�dZA�dZA�hsA�jA�dZA�bNA�dZA�hsA�^5A�^5A�dZA�bNA�jA�dZA�`BA�hsA�`BA�hsA�hsA�n�A�jA�ffA�l�A�p�A�ffA�jA�ffA�x�A�bNA�dZA�^5A�ZA�XA�XA�VA�\)A�ffA�ZA�^5A�\)A�ZA�\)A�r�A�l�A�n�A�jA�bNA�ffA�p�A�jA�hsA�l�A�l�A�l�A�`BA�ffA�dZA�dZA�^5A�\)A�^5A�^5A�x�A�^5A�M�A�?}A�`BA�^5A�^5A�l�A�dZA�`BA�bNA�\)A�ZA�XA�\)A�ZA�`BA�\)A�^5A�\)A�^5A�\)A�XA�XA�ZA�ffA�XA�^5A�XA�\)A�/A�l�A�dZA�^5A�hsA�\)A�bNA�^5A�ZA�ZA�9XA�Q�A�O�A�S�A�O�A�O�A�O�A�XA�XA�XA�VA�Q�A�S�A�VA�XA�S�A�S�A�O�A�Q�A�VA�XA�S�A�VA�ZA�^5A�VA�VA�Q�A�Q�Aǝ�A�VA�Q�A�O�A�VA�ZA�S�A�O�A�O�A�XA�O�A�O�A�O�A�S�A�\)A�VA�l�A�ffA�jA�ffA�ffA�jA�bNA�ZA�ZA�^5A�`BA�dZA�VA�hsA�l�A�\)A�`BA�bNA�ZA�XA�`BA�l�A�XA�^5A�`BA�dZA�\)A�\)A�bNA�\)A�`BA�`BA�ZA�bNA�bNA�\)A�bNA�ZA�XA�\)A�\)A�{A�VA�ZA�&�A�XA�VA�XA�VA�XA�XA�ZA�XA�hsA�S�A�O�A�\)A�ZA�Q�A�Q�A�\)A�O�A�VA�K�A�XA�VA�VA�\)A�XA�VA�^5A�Q�A�\)A�ZA�VA�ZA�ZA�ffA�I�A�S�A�XA�XA�ZA�ZA�\)A�XA�^5A�XA�XA�S�A�ZA�Q�A�XA�XA�VA�\)A�\)A�ZA�\)A�XA�ZA�XA�XA�^5A�S�A�VA�VA�K�A�M�A�Q�A�M�A�M�A�I�A�I�A�I�A�I�A�E�A�E�A�G�A�G�A�G�A�E�A�C�A�A�A�E�A�A�A�E�A�G�A�C�A�C�A�I�A�E�A�A�A�A�A�C�A�C�A�G�A�A�A�E�A�E�A�E�A�E�A�E�A�I�A�A�A�A�A�C�A�?}A�A�A�A�A�G�A�I�A�I�A�G�A�I�A�E�A�E�A�E�A�E�A�E�A�G�A�K�A�I�A�C�A�C�A�K�A�K�A�G�A�I�A�E�A�C�A�A�A�C�A�C�A�=qA�=qA�=qA�?}A�=qA�=qA�=qA�=qA�?}A�?}A�=qA�=qA�=qA�=qA�9XA�=qA�;dA�9XA�;dA�;dA�;dA�9XA�5?A�33A�5?A�5?A�33A�33A�33A�1'A�/A�-A�$�A��A��A��A��A�{A�
=A�VA�%A���A���A���A��A��A��A��A��yA��`A��`A��`A��HA��#A��#A���AǼjAǺ^Aǧ�AǓuAǇ+A�jA�XA�M�A�?}A�?}A�?}A�=qA�?}A�=qA�;dA�7LA�9XA�9XA�;dA�7LA�;dA�9XA�9XA�9XA�9XA�7LA�9XA�7LA�;dA�9XA�9XA�;dA�9XA�7LA�7LA�9XA�5?A�33A�-A�(�A�"�A��A��A��A��A�oA�%A���A���A��A���A��A��A��A��A��A��A��A��A��yA��yA��`A��`A��TA��;A��/A��/A��A���A���A���A���A���A��
A���A��
A���A���A���A���A���A���A���A���A���A���A���A���A�ƨA�ĜA���A���AƾwAƼjAƺ^Aƺ^AƸRAƶFAƴ9AƶFAƶFAƴ9Aƴ9AƶFAƴ9AƶFAƶFAƶFAƶFAƶFAƲ-AƲ-Aƴ9AƲ-Aư!Aư!AƮAƩ�AƩ�Aƥ�AƟ�Aƕ�AƉ7A�~�A�z�A�t�A�r�A�n�A�n�A�hsA�dZA�`BA�^5A�\)A�ZA�ZA�XA�VA�S�A�Q�A�O�A�M�A�K�A�K�A�I�A�I�A�G�A�E�A�E�A�C�A�A�A�?}A�9XA�1'A�+A�(�A�(�A�&�A�$�A�"�A��A��A��A�oA�bA�JA�1A�A�A���A���A��A��yA��#AžwAũ�AŃA�v�A�l�A�hsA�dZA�\)A�ZA�VA�S�A�S�A�O�A�I�A�I�A�G�A�E�A�E�A�G�A�C�A�E�A�E�A�C�A�A�A�?}A�=qA�;dA�9XA�9XA�7LA�5?A�33A�33A�1'A�1'A�1'A�1'A�/A�/A�/A�1'A�1'A�/A�(�A�$�A�"�A�"�A��A�oA�1A�A�A�A�  A�  A���A���A���A���A���A���A���A���A���A��A��A��A��A��A��A��A��mA��mA��`A��TA��HA��HA��/A��;A��HA��HA��HA��A��
A��
A��
A���A���A���A���A�ĜA�ƨA�A���A�A���AľwAĺ^AĸRAĸRAĶFAĲ-AĲ-Aİ!Aĩ�Aħ�Aħ�Aħ�Aħ�Aĥ�Aġ�Ağ�Ağ�Aĝ�Aě�Aě�Aě�Aė�Aė�Aė�Aė�Aĕ�AēuAĕ�AđhAď\Aď\Aď\Aď\Aĉ7Ać+AăA�|�A�z�A�z�A�p�A�\)A�Q�A�;dA�(�A�{A���A��`AöFAÏ\A�S�A�
=A���A�A¸RA°!A©�A¡�A¡�A�AhA\ADAA�p�A�33A�{A�  A��mA��A�ƨA��-A���A���A��hA�|�A�l�A�O�A�=qA�/A��A�oA�JA�%A���A��A��mA��TA��;A��/A���A���A���A���A��^A��-A��A���A���A��\A��A�z�A�r�A�n�A�l�A�jA�l�A�hsA�`BA�`BA�^5A�`BA�^5A�\)A�ZA�ZA�ZA�ZA�VA�VA�XA�VA�VA�VA�VA�S�A�VA�Q�A�Q�A�Q�A�O�A�I�A��A���A��\A�ffA�XA�M�A�E�A�C�A�E�A�E�A�1'A�7LA�&�A�&�A�$�A�$�A�"�A�"�A�"�A�(�A�-A�/A�33A�7LA�7LA�7LA�7LA�7LA�5?A�/A�1'A�+A�+A�"�A��A�JA�1A�%A�1A�A��A��HA�ĜA��jA���A���A���A��DA��A��A�|�A�x�A�r�A�n�A�ffA�`BA�`BA�\)A�^5A�ZA�VA�M�A�E�A�G�A�I�A�E�A�7LA�(�A��A�%A�1A���A��A��A��A��A��yA���A���A��wA��A��9A�ĜA�ȴA���A���A�ȴA�ĜA��^A��A���A���A��PA��PA���A��9A��^A��^A��RA��^A��jA��9A��9A��-A��-A��-A��9A��!A��A��A��A��A��A��A���A���A���A���A���A���A��uA��7A�|�A�v�A�l�A�hsA�hsA�ffA�bNA�`BA�`BA�bNA�bNA�^5A�XA�XA�S�A�K�A�K�A�K�A�I�A�I�A�K�A�M�A�K�A�K�A�I�A�Q�A�Q�A�Q�A�Q�A�Q�A�Q�A�Q�A�VA�S�A�O�A�O�A�M�A�K�A�E�A�;dA�+A��A�bA�JA�
=A�%A���A���A���A���A���A���A��A��A��`A��
A���A��RA��hA�dZA�O�A�C�A�33A��A��A��A��A�{A�{A�{A�{A�"�A�+A�(�A�+A�33A�33A�1'A�/A�1'A�/A�{A��wA���A��A�|�A��A��\A���A��9A���A���A���A��TA���A���A�v�A�r�A�t�A�z�A��A��+A�v�A�C�A� �A�JA�%A�1A�JA�bA��A��A��A�oA�%A�  A��A��A��mA�ĜA���A���A���A��A���A��A���A���A���A���A��\A��PA��+A��A��A�p�A�XA�O�A�7LA��A��A�{A�A���A��A��#A���A���A���A���A���A���A�ȴA�ĜA�A���A��jA��RA��FA��!A���A���A���A���A���A���A��uA��PA��+A��A�z�A�n�A�M�A�7LA�/A�+A�"�A��A�VA�A���A��A��A���A���A��PA�|�A�t�A�hsA�^5A�S�A�K�A�A�A�1'A�1A�A��A��uA��DA�x�A�x�A�r�A�l�A�jA�VA�G�A�;dA�=qA�9XA�7LA�7LA�9XA�5?A�33A�/A�&�A� �A��A��A��A��A��A��A��A��A�{A�oA�VA�JA�1A���A��A��A��A��A��A��yA��mA��`A��`A��`A�ĜA��A�r�A��A�~�A��+A��DA��\A��A���A���A��A��`A��A�E�A��A�  A��#A�`BA�%A���A�A��A�&�A�C�A�E�A�?}A�?}A�=qA�E�A�S�A�Q�A�Q�A�S�A�Q�A�G�A�?}A�I�A�n�A�l�A�l�A�n�A�|�A��A��uA���A���A��A�^5A�?}A�+A� �A��A��A�{A�bA�JA�
=A�1A�A�A�%A�A�A�A�  A���A���A��TA��A��
A���A�ƨA�ƨA���A��A��uA�|�A�ffA�M�A�A�A�5?A�+A�&�A�-A�/A�$�A��A��A��A�{A�oA�oA�oA��A�{A�oA�{A�{A��A��A�5?A�XA�`BA�r�A�z�A�A�A�9XA�/A�$�A�/A�=qA�;dA�+A�33A�A�A�E�A�?}A��mA�ZA�ZA�\)A�XA�^5A�XA�XA�S�A�ZA�Q�A�XA�XA�VA�\)A�\)A�ZA�\)A�XA�ZA�XA�XA�^5A�S�A�VA�VA�K�A�M�A�Q�A�M�A�M�A�I�A�I�A�I�A�I�A�E�A�E�A�G�A�G�A�G�A�E�A�C�A�A�A�E�A�A�A�E�A�G�A�C�A�C�A�I�A�E�A�A�A�A�A�C�A�C�A�G�A�A�A�E�A�E�A�E�A�E�A�E�A�I�A�A�A�A�A�C�A�?}A�A�A�A�A�G�A�I�A�I�A�G�A�I�A�E�A�E�A�E�A�E�A�E�A�G�A�K�A�I�A�C�A�C�A�K�A�K�A�G�A�I�A�E�A�C�A�A�A�C�A�C�A�=qA�=qA�=qA�?}A�=qA�=qA�=qA�=qA�?}A�?}A�=qA�=qA�=qA�=qA�9XA�=qA�;dA�9XA�;dA�;dA�;dA�9XA�5?A�33A�5?A�5?A�33A�33A�33A�1'A�/A�-A�$�A��A��A��A��A�{A�
=A�VA�%A���A���A���A��A��A��A��A��yA��`A��`A��`A��HA��#A��#A���AǼjAǺ^Aǧ�AǓuAǇ+A�jA�XA�M�A�?}A�?}A�?}A�=qA�?}A�=qA�;dA�7LA�9XA�9XA�;dA�7LA�;dA�9XA�9XA�9XA�9XA�7LA�9XA�7LA�;dA�9XA�9XA�;dA�9XA�7LA�7LA�9XA�5?A�33A�-A�(�A�"�A��A��A��A��A�oA�%A���A���A��A���A��A��A��A��A��A��A��A��A��yA��yA��`A��`A��TA��;A��/A��/A��A���A���A���A���A���A��
A���A��
A���A���A���A���A���A���A���A���A���A���A���A���A�ƨA�ĜA���A���AƾwAƼjAƺ^Aƺ^AƸRAƶFAƴ9AƶFAƶFAƴ9Aƴ9AƶFAƴ9AƶFAƶFAƶFAƶFAƶFAƲ-AƲ-Aƴ9AƲ-Aư!Aư!AƮAƩ�AƩ�Aƥ�AƟ�Aƕ�AƉ7A�~�A�z�A�t�A�r�A�n�A�n�A�hsA�dZA�`BA�^5A�\)A�ZA�ZA�XA�VA�S�A�Q�A�O�A�M�A�K�A�K�A�I�A�I�A�G�A�E�A�E�A�C�A�A�A�?}A�9XA�1'A�+A�(�A�(�A�&�A�$�A�"�A��A��A��A�oA�bA�JA�1A�A�A���A���A��A��yA��#AžwAũ�AŃA�v�A�l�A�hsA�dZA�\)A�ZA�VA�S�A�S�A�O�A�I�A�I�A�G�A�E�A�E�A�G�A�C�A�E�A�E�A�C�A�A�A�?}A�=qA�;dA�9XA�9XA�7LA�5?A�33A�33A�1'A�1'A�1'A�1'A�/A�/A�/A�1'A�1'A�/A�(�A�$�A�"�A�"�A��A�oA�1A�A�A�A�  A�  A���A���A���A���A���A���A���A���A���A��A��A��A��A��A��A��A��mA��mA��`A��TA��HA��HA��/A��;A��HA��HA��HA��A��
A��
A��
A���A���A���A���A�ĜA�ƨA�A���A�A���AľwAĺ^AĸRAĸRAĶFAĲ-AĲ-Aİ!Aĩ�Aħ�Aħ�Aħ�Aħ�Aĥ�Aġ�Ağ�Ağ�Aĝ�Aě�Aě�Aě�Aė�Aė�Aė�Aė�Aĕ�AēuAĕ�AđhAď\Aď\Aď\Aď\Aĉ7Ać+AăA�|�A�z�A�z�A�p�A�\)A�Q�A�;dA�(�A�{A���A��`AöFAÏ\A�S�A�
=A���A�A¸RA°!A©�A¡�A¡�A�AhA\ADAA�p�A�33A�{A�  A��mA��A�ƨA��-A���A���A��hA�|�A�l�A�O�A�=qA�/A��A�oA�JA�%A���A��A��mA��TA��;A��/A���A���A���A���A��^A��-A��A���A���A��\A��A�z�A�r�A�n�A�l�A�jA�l�A�hsA�`BA�`BA�^5A�`BA�^5A�\)A�ZA�ZA�ZA�ZA�VA�VA�XA�VA�VA�VA�VA�S�A�VA�Q�A�Q�A�Q�A�O�A�I�A��A���A��\A�ffA�XA�M�A�E�A�C�A�E�A�E�A�1'A�7LA�&�A�&�A�$�A�$�A�"�A�"�A�"�A�(�A�-A�/A�33A�7LA�7LA�7LA�7LA�7LA�5?A�/A�1'A�+A�+A�"�A��A�JA�1A�%A�1A�A��A��HA�ĜA��jA���A���A���A��DA��A��A�|�A�x�A�r�A�n�A�ffA�`BA�`BA�\)A�^5A�ZA�VA�M�A�E�A�G�A�I�A�E�A�7LA�(�A��A�%A�1A���A��A��A��A��A��yA���A���A��wA��A��9A�ĜA�ȴA���A���A�ȴA�ĜA��^A��A���A���A��PA��PA���A��9A��^A��^A��RA��^A��jA��9A��9A��-A��-A��-A��9A��!A��A��A��A��A��A��A���A���A���A���A���A���A��uA��7A�|�A�v�A�l�A�hsA�hsA�ffA�bNA�`BA�`BA�bNA�bNA�^5A�XA�XA�S�A�K�A�K�A�K�A�I�A�I�A�K�A�M�A�K�A�K�A�I�A�Q�A�Q�A�Q�A�Q�A�Q�A�Q�A�Q�A�VA�S�A�O�A�O�A�M�A�K�A�E�A�;dA�+A��A�bA�JA�
=A�%A���A���A���A���A���A���A��A��A��`A��
A���A��RA��hA�dZA�O�A�C�A�33A��A��A��A��A�{A�{A�{A�{A�"�A�+A�(�A�+A�33A�33A�1'A�/A�1'A�/A�{A��wA���A��A�|�A��A��\A���A��9A���A���A���A��TA���A���A�v�A�r�A�t�A�z�A��A��+A�v�A�C�A� �A�JA�%A�1A�JA�bA��A��A��A�oA�%A�  A��A��A��mA�ĜA���A���A���A��A���A��A���A���A���A���A��\A��PA��+A��A��A�p�A�XA�O�A�7LA��A��A�{A�A���A��A��#A���A���A���A���A���A���A�ȴA�ĜA�A���A��jA��RA��FA��!A���A���A���A���A���A���A��uA��PA��+A��A�z�A�n�A�M�A�7LA�/A�+A�"�A��A�VA�A���A��A��A���A���A��PA�|�A�t�A�hsA�^5A�S�A�K�A�A�A�1'A�1A�A��A��uA��DA�x�A�x�A�r�A�l�A�jA�VA�G�A�;dA�=qA�9XA�7LA�7LA�9XA�5?A�33A�/A�&�A� �A��A��A��A��A��A��A��A��A�{A�oA�VA�JA�1A���A��A��A��A��A��A��yA��mA��`A��`A��`A�ĜA��A�r�A��A�~�A��+A��DA��\A��A���A���A��A��`A��A�E�A��A�  A��#A�`BA�%A���A�A��A�&�A�C�A�E�A�?}A�?}A�=qA�E�A�S�A�Q�A�Q�A�S�A�Q�A�G�A�?}A�I�A�n�A�l�A�l�A�n�A�|�A��A��uA���A���A��A�^5A�?}A�+A� �A��A��A�{A�bA�JA�
=A�1A�A�A�%A�A�A�A�  A���A���A��TA��A��
A���A�ƨA�ƨA���A��A��uA�|�A�ffA�M�A�A�A�5?A�+A�&�A�-A�/A�$�A��A��A��A�{A�oA�oA�oA��A�{A�oA�{A�{A��A��A�5?A�XA�`BA�r�A�z�A�A�A�9XA�/A�$�A�/A�=qA�;dA�+A�33A�A�A�E�A�?}A��mG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111119999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999993333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@�`�@�^�?���>4Y@�a�@�c�=���@��[@mp�>Y�@�uO=���>F�@�`k=�5?���?�W=�ͳ=�"�@Y�;@�$>D�`@�b�@�h4=�8�?��>�=�u�=��+?Ùp@�d�=g�X=��K=�!=�S�>~*�@�`�=�ں>���@u��@��=��.?�5i@�c�=�Yu=��)=���@V#�>�a=>T��@�e@�e@���>R??%��@�
�>ʄ8>0�"@3��>�e@*>��@_�>�>i��@�]:>�p�=�L�=ͳ}=��=��=��%@&>�{�>Z>��@�f�>3r@�sC@�f�@6Y�=㷕?	\�@��M=���=�i�>~�E=���?��?��?SZ�>�d�>�6=�f>.C�?ɧ�=�
�=�PH=�Z\>K+�=�	�>BH�@�do@@$�>�?�P	?���@wKs=��?@���=��s=�>u��@�g8?4K>"��@;�P@X��=�!B=��=��^=�>0��@�^�=�\S=��>#>^�>O�r@�iY@�j�?/�>$3@�iY=��>��H>vݭ?m��@�f'@�j�=�*�?��>/d@�i�?�"=�4D?�:@�iY@�k�=�f'?�q@�g�@B��=��?�N@�k<@�k<@�j�@�k�>`�@�j�@�j@�jj@�l7=��?�M+=��N>�@�jj>Zu�?��@�j�@=��@���>�N@�k�?taR@�l�?��=�*0?BN{@�k�@�l�@�k�@�l�@�l�>:�@�k�>t�
>E#�@�m�@�j@�l�??��>9��@�k�?(�V?���@�mH@�l�?�!-?4.4@�k�@�j�@X`-@��Z@�j@�j?��.@�m@�l�@�k'@�l�?��@�j�@�k{@�m�@�mH@�m�@�o@�m�@���@�l�@�k�@�k�@�o?@�n�@�mr@�l�@�s@�n�@�mH@�m�@�nY@�o�@�m�@�n�@�l�@�mH@�o?@�l�@�la@�j+@�mH@�p&@�l�@�k{@�jj@�k{@�k'@�k{@�i�@�j@�n�@�m�@�p�@�n/@�o�@�l�@�k{@�l�@�nY@�m�@�k�@�q�@�pz@�o�@�nY@�q�@�q@�n�@�n�@�l�@�oi@�p&@�o�@�m�@�oi@�l�@�o@�p&@�o@�n�@�mr@�q@�o?@�nY@�nY@�mH@�nY@�n@�l7@�l�@�q7@�o@�l�@�mH@�n�@�m�@�o?@�k�@�l7@�q�@�mH@�l�@�mH@�nY@�m�@�mH@�k�@�l7@�k�@�m�@�m@�k<@�la@�l7@�k{@�l7@�l�@�jj@�l�@�l�@�k�@�l�@�l7@�k�@�k�@�la@�m@�l7@�k'@�jj@�k�@�k'@�l�@�l�@�l7@�l7@�m�@�k'@�mr@�m�@�m@�la@�k�@�jj@�j�@�nY@�l�@�k{@�jj@�k�@�l7@�iY@�h�@�h�@�k�@�k<@�h^@�hI@�k'@�l�@�jj@�rq@�l�@�j�@�j@�l�@�l�@�k{@�j+@�k<@�h^@�n�@�o@�l�@�l�@�j�@�j@�j@�k�@�hI@�i�@�hI@�j@�i�@�gM@�i@�h�@�g�@�h�@�i@�h�@�f�@�f�@�g�@�g�@�g�@�g�@�hI@�g�@�hI@�g�@�g8@�f'@�ek@�ek@�e�@�i�@�i�@�f�@�h�@�h^@�hI@�i@�j@�f�@�iY@�g�@�h�@�i@�g8@�ek@�f�@�h^@�h^@�h
@�gM@�f<@W��@�i@�g8@�ek@�hI@�iY@�i@�g�@�k<@�j�@�h�@�h^@�j�@�i�@�l�@�k{@�k'@�l�@�l7@�jj@�k{@�jj@�i�@�k�@�k<@�k�@�j�@�j+@�k�@�k'@�i�@�g8@�g8@�k'@�j@�g8@�g8@�i@�g�@�f<@�h
@�h^@�h^@�g�@�g�@�f�@�hI@�mH@�l7@�j�@�j�@�f�@�hI@�f�@�f<@�i@�j+@_�@��@�dZ@�4D@�g�@�f'@�e@�e@�f�@�f'@�f�@�f<@�h^@�i�@�i@�f�@�e@�f�@�d�@�d�@�e@�ek@�f'@�g�@�g�@�g�@�e�@�f<@�g�@�e�@�f<@�e�@�f'@�e@�do@�d@�c^@�d�@�c�@�e@�d�@�d�@�d�@�e�@�c^@�c@�d�@�d�@�e@�do@�c^@�do@�e@�do@�do@�gb@�e�@�f�@�f<@�f<@�f�@�f<@�e@�d@�b�@�bc@�b�@�b�@�a�@�`�@�a=@�a=@�`�@�`-@�_�@�`-@�_@�_�@�_�@�_�@�_@�_@�_@�^t@�^�@�_@�^�@�_�@�`-@�_�@�^@�^t@�^@�_�@�_�@�^�@�_�@�`�@�`B@�`�@�`�@�`�@�`�@�`�@�`�@�`B@�`�@�aR@�bc@�b�@�b�@�c @�b�@�c @�b�@�c�@�c @�c�@�d0@�d�@�dE@�d�@�e�@�e�@�d�@�c�@�c @�c�@�eA@�c @�c5@�b@�b@�b@�b@�a�@�b@�bx@�b@�b@�b@�b@�b�@�b�@�b�@�c5@�b�@�b�@�c5@�c5@�b�@�b�@�b�@�bx@�b�@�b�@�b�@�bx@�bx@�b�@�b�@�b�@�dE@�d�@�e@�f'@�g#@�g#@�g#@�g�@�h�@�j@�j�@�m3@�n�@�p&@�p�@�q"@�q"@�q�@�r�@�sX@�s.@�s�@�t�@�t�@�t�@�uy@�u�@�v6@�v�@�wG@�w�@�v�@�u�@�t~@�r�@�r�@�r�@�s@�r�@�r�@�q�@�q�@�q�@�q�@�q�@�q�@�q7@�qv@�q�@�q@�q�@�q@�qL@�qL@�p�@�p�@�p�@�p�@�pe@�o�@�o*@�n�@�m�@�l�@�k�@�j@�h�@�f�@�e�@�d�@�d@�bN@�_p@�]�@�\>@�Z�@�Z�@�Z�@�Z@�Y�@�Y`@�X�@�X�@�X:@�W?@�W?@�VC@�U�@�Tv@�TAIG@�S;@�R�@�R~@�RT@�R*@�S@�R�@�S@�Se@�S�@�S�@�T"@�S�@�T�@�U�@�U�@�VC@�U�@�U2@�T�@�T�@�U�@�U2@�Tv@�S@�R�@�RT@�Q�@�Q�@�Q@�Q�@�P�@�P�@�P�@�QD@�Q�@�R @�Q�@�Q�@�Ri@�Q�@�R @�R?@�R @�QY@�QY@�P�@�P�@�O�@�O7@�N<@�Mj@�K�@�K
@�I=@�F_@�Cl@�?}@�=@�;:@�9�@�8�@�8	@�6�@�5�@�4�@�3�@�3	@�2�@�28@�0�@�0�@�/�@�/Z@�.I@�,�@�,�@�+�@�+A@�*�@�*E@�)�@�(�@�(c@�'g@�&�@�$�@�"�@� �@�6@�y@�@�X@��@��@�z@�Y@��@�f@�U@�@�R@�@��@�
�@��@�S@��@���@��0@��o@��@��:@���@��@��\@��@��`@���@���@��P@���@��@��@��?@��?@���@��?@���@���@���@��@��.@���@��r@��@��@��@��@��@��@���@��@��r@��@��@��r@��r@���@��r@��.@���@��?@���@��?@���@��P@��@��@��@��q@��@���@���@���@��q@��q@��q@��q@���@���@��@��@���@���@��>@���@��>@���@��>@��>@��@��>@���@���@��@��2@��2@��2@��!@��@��@��@��T@�� @��@���@�߹@��U@��D@�܇@���@���@��'@�ؙ@�׈@���@��$@���@�Ԫ@��[@�Ҟ@���@�ϖ@�Κ@��1@���@��d@��S@��2@���@��@���@�ƽ@�ŗ@��3@��e@���@��D@���@���@��@��R@��A@��A@���@��9@��@���@���@���@��?@���@���@��4@��x@���@���@���@���@�|�@�p�@�b�@�Vm@�?S@�&�@�@��@�@��@��@��@���@���@���@���@���@��&@��o@�ǹ@���@���@���@���@���@���@���@��@���@�{�@�s�@�h�@�`�@�Z�@�S;@�M�@�I�@�F�@�@�@�;d@�9�@�7@�4�@�1<@�-�@�+A@�%�@�#:@��@�*@��@�@�4@��@��@�	@��@��@�.@�@�!@� T@� �@���@��@��H@��v@��z@��j@��@��Y@��]@��M@���@��@���@���@��|@��o@��@��@��@��@���@��	@���@���@���@��y@��r@��@@��@��4@��@���@��P@���@��@���@���@���@���@���@���@��'@��z@���@��u@��:@���@��@��l@��}@��1@��V@���@��V@��1@��x@��)@���@���@��X@��'@���@���@��$@��:@���@��a@���@��/@��"@��I@���@���@�~�@�}@�z�@�xB@�u�@�t*@�r�@�q@�o�@�m@�k�@�i@�f<@�d�@�d�@�a�@�Z@�T�@�M�@�J�@�Ft@�CB@�B�@�A�@�@%@�=�@�6P@�0�@�+�@�&�@�&l@�-b@�33@�5T@�5T@�4D@�1�@�.
@�'�@�$_@��@��@��@�!�@�*E@�1'@�4Y@�4Y@�5?@�5�@�6z@�7a@�7v@�7"@�9@�9�@�9�@�9m@�9m@�9�@�9m@�9�@�9�@�9�@�8�@�82@�7�@�6e@�4�@�1�@�/�@�+�@�(�@�%�@�#�@�">@�!�@�!-@� 2@��@��@��@�!@�i@��@��@��@��@��@��@�]@��@��@�+@�z@��@�@�O@��@�&@�?@�@�~@��@��@��@��@�@�	@��@�o@�	�@�@� �@��H@���@���@��@��w@���@���@���@��@��M@���@���@���@��]@��@��@���@���@���@��+@��@���@���@���@���@��@��_@���@��x@��@���@���@��@@��e@��@��/@���@��k@���@�� @�p�@�f@�`k@�`k@�d�@�p�@�}@��'@���@��@��	@���@��c@���@�x�@�w�@�yS@�~�@��V@��<@�{�@�i/@�[�@�W@�V@�Wi@�Y�@�\�@�_�@�^�@�[�@�W�@�SP@�N'@�J�@�G0@�AJ@�9�@�6�@�<�@�GZ@�H@�GE@�GE@�F5@�GE@�G�@�E�@�D�@�D�@�B�@�A�@�=�@�5T@�.�@�)�@� �@�v@��@��@�@��@�	-@�@��@�P@� ?@���@��3@��z@��n@��b@���@���@��8@��,@���@��p@��@��@���@�݃@�܇@�پ@��R@��}@��d@��6@���@���@���@���@��
@���@��p@��e@���@���@�},@�uO@�iY@�[B@�M�@�Ft@�?�@�:�@�4@�,(@�&W@��@�r@�@���@���@��4@���@��e@��L@���@��D@��x@��@��K@���@��3@���@���@���@���@��@��@��f@��@���@���@��F@��t@���@���@���@���@��-@��1@��u@���@��*@��q@��e@���@���@���@��@��Q@��Y@��
@��M@��<@��@�}�@��i@���@���@��@��W@��t@��$@���@��^@�զ@�Ӯ@�ͳ@�ƨ@��Z@�� @�e�@�V�@�Bp@��@��@��@���@���@��@��@�@�@��@��@�!�@�(N@�(N@�(�@�(�@�(�@�'�@�&W@�+�@�;d@�A_@�A5@�B�@�G@�N�@�SP@�R @�O�@�HV@�=2@�2�@�,(@�)@�'R@�%p@�#�@�">@� G@��@��@�@�@��@�q@��@�+@��@�s@��@�y@��@��m@���@���@���@��.@�ݘ@��9@���@�˼@���@���@�˒@���@�̎@��1@�Ц@��A@��0@�ܱ@���@��@���@��@��@���@���@��C@�@��@�@�@�@� ?@��@��H@���@���@��;@��P@��;@���@��7@���@���@���@���@���@��m@� @�e@���@��o@��o@��@���@��)@���@��N@�� @���@���@��@��0@��@���@��o@��@��@��A@��V@��A@��V@��@��@��@��W@��@��@���@���@���@��W@��@��@��F@��@��t@���@���@��@��6@��t@��K@���@��6@��6@��6@��@���@���@��1@��@��@���@���@��B@��l@��@��l@���@��@���@��-@��l@���@��@��@��@��N@��@���@��@��@��@���@��@��@��N@��x@��@��o@��x@��>@��@���@��@���@��@���@��@��|@���@��@��@���@��B@��@��B@��B@��@��-@��B@���@��l@���@���@��)@��>@��g@��g@��|@��g@��>@��>@��g@��|@��|@��|@��|@���@���@���@��N@��@��t@��@��@��V@���@��@���@��@���@��<@��Y@���@��;@���@���@��7@��H@��C@���@���@���@� �@� ~@� ?@��@��@��@��@��@��@��@� ?@��v@��"@��a@��L@��v@��a@��z@���@��e@��z@���@���@���@���@���@���@���@���@���@���@��@��"@��@���@��"@���@���@���@���@���@���@���@��e@���@��@��@��@��k@��)@��@��%@��@��X@��@��@��G@���@���@���@��`@��@��&@�� @��@���AIG@���@��"@���@��&@��@@��@�޾@���@��P@��"@��@��@���@���@��	@��@���@���@��i@��@��]@��@��T@��@���@��L@���@�ߤ@���@��@���@��@@��@�ݭ@�ݭ@�ݭ@��@��j@���@��&@��;@�߹@��@��"@��@���@���@�߹@��7@�ߤ@�ߏ@��P@���@�ݭ@�ܱ@�۶@���@���@��J@�΅@��h@��\@��K@���@��?@���@��L@�@��j@���@��n@��@��H@��b@��#@��@��@��E@��@���@��
@���@��c@��@���@��=@���@���@���@���@���@���@��S@���@��C@��\@���@��@��@��v@���@��j@���@���@���@��V@��s@���@���@��}@��Q@��,@�y�@�w�@�u�@�t�@�t?@�s�@�r�@�rG@�rG@�q�@�q�@�q"@�p�@�p�@�p�@�pe@�p;@�p;@�p;@�p�@�p�@�o�@�oi@�o*@�n�@�n�@�n/@�n�@�nY@�n/@�n@�m�@�nY@�nY@�n/@�nY@�nY@�nn@�nn@�nY@�n/@�oi@�o�@�o�@�oi@�o�@�p�@�sm@�sC@�s.@�s.@�s.@�sm@�s.@�s.@�s.@�sC@�sC@�sm@�sC@�s�@�s�@�t�@�t?@�t*@�t~@�t�@�u�@�uy@�vK@�v6@�vu@�v@�t�@�t�@�ti@�t�@�u:@�t�@�t�@�s�@�r�@�s@�r�@�q"@�q"@�q7@�o@�m	@�l�@�k�@�k@�k'@�j�@�iD@�h@�g#@�f�@�f@�c�@�c�@�b9@�_�@�_1@�^�@�^ @�]�@�]%@�[�@�YK@�Z@�Y�@�X�@�X�@�X:@�W�@�W @�V�@�V�@�V@�U�@�T�@�T�@�S�@�S�@�S�@�S&@�R*@�Q@�O@�L�@�K�@�JM@�F_@�@@�=�@�6�@�2�@�-�@�%�@��@�0@�2@��@���@���@���@��T@��z@���@���@��@���@���@��@��1@���@���@�o�@�`�@�V�@�K�@�E9@�=2@�33@�+�@�'=@�#d@�&@�E@��@���@��@��@��@��T@���@��r@�؄@���@��>@�΅@��@���@���@��?@��@��8@��@��,@���@��@��P@���@��w@��N@��k@���@��@��@���@���@���@���@��6@��u@��y@���@��?@���@��?@���@���@���@���@��\@���@���@���@��G@���@��*@���@��@���@�x�@�SP@�7a@� 2@�.@��@��@��@��@��@�?@�u@�"S@�%F@�&@�&�@�)J@�*�@�+V@�/�@�6z@�:@�@y@�@�@�D�@�E�@�FJ@�G�@�I�@�P3@�P�@�Ov@�P	@�N'@�Jb@�E�@�DR@�C@�CW@�B@�;:@�5@�*�@�(�@�#%@�"�@�"�@��@�L@�z@��@�w@�@��@��@�
�@�
�@�	�@�	B@�p@�>@�@���@���@���@��]@���@��@��@���@���@���@��b@���@���@��@���@�̣@���@��@@���@���@��P@��;@�Κ@��%@���@���@�ı@��@��@���@��&@���@���@��j@��~@�ƽ@��?@�ȴ@��C@���@���@�ʬ@��}@��u@��@��1@��@��1@��@��B@��B@���@��@���@�ρ@�ί@��@��)@�Ȋ@�ć@���@���@���@���@��A@���@��5@���@��x@���@��J@���@���@��u@��:@��e@��;@��e@���@��*@���@���@��2@���@��@���@���@���@��x@��N@���@���@��[@���@��@��_@���@���@���@��@��m@���@���@���@��J@���@��u@��O@���@���@��`@���@��@���@���@���@�~(@�t�@�bN@�N{@�C�@�>�@�9C@�0@�,|@�,g@�,=@�,=@�,�@�-@�-M@�3�@�=2@�=@�=�@�A�@�BF@�A5@�@y@�@%@�?�@�<6@��@��z@���@��B@��@���@�	B@�E@� �@�F5@�G�@�C�@�<6@�)t@�b@��@��@��@��@�$ @��@��@��@���@��@���@��@��(@��	@���@���@��@��@��_@��F@���@��@��@@��;@��\@�ԕ@���@��@��H@��3@��e@��@��@��3@��*@��.@��H@��]@��b@���@��@��a@���@��9@��}@��:@���@��@��@��A@���@���@���@��A@���@���@���@��@���@��d@���@���@���@���@���@��
@�~�@��@�~g@�|�@�x-@�t?@�t�@�mH@�g#@�Y6@�LD@�G�@�DR@�A_@�<�@�5�@�1�@�.s@�)@�$�@�e@��@��<@���@���@��@��@��U@��=@��9@���@��s@��.@��@�s�@�lv@�bc@�a�@�_�@�[�@�[@�R @�H�@�B1@�A�@�A�@�?�@�@@�?�@�?@�>W@�;�@�8\@�6e@�4@�3�@�3r@�2�@�2a@�2#@�28@�2�@�1�@�1Q@�0�@�0�@�0U@�-�@�(N@�(x@�'g@�'g@�&�@�%�@�$�@�$�@�#�@�b@�W~@�8@�.�@�9@�6&@�;�@�A @�?@���@��7@��3@���@��4@�k�@�3�@�{@��@���@��&@���@��@���@���@���@��@��@��^@���@���@���@�˧@�˧@��S@�˒@���@��G@��H@��`@��@��e@��e@��@��@���@� @�&@��@��	@��J@��3@�ײ@��J@�Ц@��B@��u@�˼@��@�ȟ@�Ǥ@��e@��;@���@��2@���@��`@���@���@���@���@���@��@���@�� @���@���@���@���@��@��V@�w�@�r\@�k�@�h@�e�@�h�@�h4@�d�@�bc@�a�@�a=@�^_@�]@�\}@�\�@�^�@�_�@�b@�ff@�i@�n@�o~@�o~@�o�@�r@�rG@�s@�r\@�r�@�s�@�u%@�v6@�w�@�y�@�z�@�z%@�zN@�{t@�}@�xG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       3344334334344344444344334444443444443443344344434433344344444434434444444444343344434444444444444444443444434344434443444443444443344344443344434443344344433334333344443443434343444333334344333443443344333333433334333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222422222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@�`�@�^�G�O�G�O�@�a�@�c�G�O�@��Z@mp�G�O�@�uOG�O�G�O�@�`pG�O�G�O�G�O�G�O�G�O�@Y�BG�O�G�O�@�b�@�h7G�O�G�O�G�O�G�O�G�O�G�O�@�d�G�O�G�O�G�O�G�O�G�O�@�`�G�O�G�O�@u��@��G�O�G�O�@�c�G�O�G�O�G�O�@V#�G�O�G�O�@�e@�e@���G�O�G�O�@�
�G�O�G�O�G�O�G�O�G�O�G�O�@_�G�O�G�O�@�]9G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@�f�G�O�@�sD@�f�G�O�G�O�G�O�@��KG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@�doG�O�G�O�G�O�G�O�@wKrG�O�@���G�O�G�O�G�O�@�g9G�O�G�O�G�O�@X��G�O�G�O�G�O�G�O�G�O�@�^�G�O�G�O�G�O�G�O�G�O�@�iY@�j�G�O�G�O�@�i]G�O�G�O�G�O�G�O�@�f(@�j�G�O�G�O�G�O�@�i�G�O�G�O�G�O�@�iV@�k�G�O�G�O�@�g�G�O�G�O�G�O�@�k7@�k:@�j�@�k�G�O�@�j�@�j@�jl@�l:G�O�G�O�G�O�G�O�@�jkG�O�G�O�@�j�G�O�@���G�O�@�k�G�O�@�l�G�O�G�O�G�O�@�k�@�l�@�k�@�l�@�l�G�O�@�k�G�O�G�O�@�m�@�j@�l�G�O�G�O�@�k�G�O�G�O�@�mF@�l�G�O�G�O�@�k�@�j�@X`.@��^@�j@�jG�O�@�m@�l�@�k&@�l�G�O�@�j�@�k~@�m�@�mG@�m�@�o@�m�@���@�l�@�k�@�k�@�o?@�n�@�mu@�l�@�s@�n�@�mK@�m�@�nZ@�o�@�m�@�n�@�l�@�mG@�oB@�l�@�lb@�j)@�mH@�p"@�l�@�kz@�jn@�k}@�k"@�ky@�i�@�j@�n�@�m�@�p�@�n.@�o�@�l�@�k~@�l�@�nZ@�m�@�k�@�q�@�p}@�o�@�n^@�q�@�q@�n�@�n�@�l�@�oq@�p)@�o�@�m�@�oh@�l�@�o@�p*@�o@�n�@�mt@�q@�o>@�nZ@�nZ@�mF@�nZ@�n@�l=@�l�@�q7@�o@�l�@�mF@�n�@�m�@�o?@�k�@�l6@�q�@�mK@�l�@�mJ@�n]@�m�@�mG@�k�@�l<@�k�@�m�@�m@�k>@�lc@�l6@�k~@�l6@�l�@�jj@�l�@�l�@�k�@�l�@�l9@�k�@�k�@�le@�m@�l6@�k&@�ji@�k�@�k$@�l�@�l�@�l9@�l6@�m�@�k$@�mq@�m�@�m@�lc@�k�@�jj@�j�@�nZ@�l�@�ky@�jg@�k�@�l:@�iY@�h�@�h�@�k�@�k7@�h^@�hJ@�k&@�l�@�jh@�rp@�l�@�j�@�j@�l�@�l�@�k~@�j)@�k:@�hb@�n�@�o@�l�@�l�@�j�@�j@�j@�k�@�hH@�i�@�hR@�j@�i�@�gR@�i@�h�@�g�@�h�@�i@�h�@�f�@�f�@�g�@�g�@�g�@�g�@�hN@�g�@�hH@�g�@�g9@�f(@�ei@�ei@�e�@�i�@�i�@�f�@�h�@�h[@�hG@�i@�j@�f�@�iZ@�g�@�h�@�i@�g9@�ei@�f�@�h^@�h[@�h@�gN@�fB@W��@�i@�g7@�ei@�hI@�i]@�i@�g�@�k>@�j�@�h�@�hc@�j�@�i�@�l�@�k|@�k$@�l�@�l<@�jj@�k|@�jj@�i�@�k�@�k?@�k�@�j�@�j*@�k�@�k"@�i�@�g6@�g:@�k%@�j@�g<@�g:@�i@�g�@�f>@�h@�h^@�h\@�g�@�g�@�f�@�hL@�mI@�l9@�j�@�j�@�f�@�hN@�f�@�f=@�i@�j*@_�@��@�d`@�4B@�g�@�f&@�e@�e@�f�@�f)@�f�@�f@@�h\@�i�@�i@�f�@�e@�f�@�d�@�d�@�e@�ek@�f'@�g�@�g�@�g�@�e�@�f;@�g�@�e~@�f@@�e�@�f(@�e@�dr@�d@�c`@�d�@�c�@�e@�d�@���@��r@��r@��@��@��*@���@��P@��"@���@���@��@��6@��@���@��o@��@��@��C@��T@��C@��V@��@��@��@��V@��@��@���@���@���@��Z@��@��@��E@��@��v@���@���@��@��4@��v@��J@���@��6@��6@��6@��@���@���@��7@��@��@���@���@��C@��l@��@��j@���@��@���@��.@��l@���@��@��@��@��N@��@���@��@��@��@���@��@��@��Q@��}@��@��n@��|@��=@��@���@��@���@��@���@��@��{@���@��	@��@���@��@@��@��E@��G@��@��.@��>@���@��g@���@���@��*@��<@��d@��e@��|@��h@��?@��?@��j@��{@��x@��{@��~@���@���@���@��N@��@��r@��@��@��X@���@��@���@��@���@��:@��X@���@��:@���@���@��<@��F@��D@���@���@���@� �@� {@� A@��@��@��@��@��@��@��@� B@��v@��"@��e@��Q@��z@��b@��{@���@��f@��}@���@���@���@���@���@���@���@���@���@���@��@��"@��@���@��"@���@���@���@���@���@���@���@��b@���@��@��@��@��m@��,@��@��$@��@��V@��@��@��L@���@���@���@��^@��@��&@��@��@���G�O�@���@��"@���@��*@��>@���@�޼@���@��P@��!@��@��@���@���@��	@��@���@���@��g@��@��]@��@��O@��@���@��J@���@�ߧ@���@��~@���@��B@��@�ݰ@�ݰ@�ݱ@��@��j@���@��'@��=@�߶@��@��#@��@���@���@�߻@��6@�ߢ@�ߐ@��R@���@�ݭ@�ܶ@�۴@���@���@��J@�Ί@��k@��\@��M@���@��?@���@��L@�@��g@���@��n@��@��K@��d@��"@��@��@��C@��@���@��@���@��b@��@���@��>@���@���@���@���@���@���@��S@���@��E@��a@���@��@��@��v@���@��k@���@���@���@��Z@��w@���@���@��~@��N@��/@�y�@�w�@�u�@�t�@�tB@�s�@�r�@�rI@�rJ@�q�@�q�@�q@�p�@�p�@�p�@�pe@�p=@�p;@�p9@�p�@�p�@�o�@�of@�o.@�n�@�n�@�n.@�n�@�nZ@�n2@�n@�m�@�nZ@�n^@�n+@�n]@�n]@�nn@�nn@�nW@�n/@�oj@�o�@�o�@�ok@�o�@�p�@�sm@�s?@�s,@�s2@�s/@�sp@�s/@�s/@�s/@�sD@�sD@�sq@�sD@�s�@�s�@�t�@�t>@�t(@�t@�t�@�u�@�uz@�vH@�v9@�vv@�v@�t�@�t�@�tj@�t�@�u=@�t�@�t�@�s�@�r�@�s@�r�@�q@�q"@�q:@�o@�m@�l�@�k�@�k@�k&@�j�@�iE@�h@�g#@�f�@�f@�c�@�c�@�b=@�_�@�_4@�^�@�^"@�]�@�]#@�[�@�YN@�Z@�Y�@�X�@�X�@�X=@�W�@�V�@�V�@�V�@�V@�U�@�T�@�T�@�S�@�S�@�S�@�S$@�R+@�Q@�O@�L�@�K�@�JJ@�F^@�@@�=�@�6�@�2�@�-�@�%�@��@�3@�4@��@���@���@���@��S@��x@���@���@��@���@���@��@��6@���@���@�o�@�`�@�V�@�K�@�E:@�=2@�35@�+�@�'>@�#f@�$@�B@��@���@��@��@��@��R@���@��r@�؆@���@��>@�΅@��
@���@���@��A@��@��:@��@��-@���@��@��U@���@��y@��Q@��n@���@��@��@���@���@���@���@��;@��x@��~@���@��>@���@��A@���@���@���@���@��`@���@���@���@��J@���@��*@���@��@���@�x�@�SP@�7b@� 6@�.@��@��@��@��@��@�@@�v@�"S@�%F@�&@�&�@�)J@�*�@�+U@�/�@�6z@�:@�@v@�@�@�D�@�E�@�FM@�G�@�I�@�P4@�P�@�Ox@�P	@�N$@�Jc@�E�@�DX@�C@�CV@�B @�;>@�5@�*�@�(�@�#&@�"�@�"�@��@�M@�z@��@�w@�@��@��@�
�@�
�@�	�@�	>@�o@�<@�@���@���@���@��Z@���@��@��@���@���@���@��b@���@���@��@���@�̧@���@��D@���@���@��R@��:@�Λ@��#@���@���@�İ@���@��@���@��*@���@���@��i@��{@�ƹ@��@@�Ȳ@��A@���@���@�ʭ@��~@��t@��@��1@��@��0@��@��E@��>@�ϼ@��@���@�σ@�α@��@��*@�Ȋ@�ą@���@���@���@���@��B@���@��8@���@��{@���@��N@���@���@��t@��=@��i@��<@��f@���@��.@���@���@��7@���@��@���@���@���@��z@��M@���@���@��^@���@��@��e@���@���@���@��@��k@���@���@���@��J@���@��t@��S@���@���@��b@���@��@���@���@���@�~-@�t�@�bP@�N{@�C�@�>�@�9F@�0@�,}@�,g@�,B@�,=@�,�@�-@�-R@�3�@�=6@�=
@�=�@�A�@�BF@�A9@�@@�@'@�?�@�<7@��@��}@���@��G@��@���@�	D@�B@� �@�F;@�G�@�C�@�<4@�)w@�`@��@��@��@��@�$#@��@��@��@���@��@��@��@��'@��@���@���@��@��@��d@��C@���@��
@��B@��<@��]@�ԓ@���@��@��F@��9@��e@��@��@��6@��+@��,@��H@��Y@��a@���@��@��c@���@��=@��x@��9@���@��@��@��A@���@���@���@��A@���@���@���@��@���@��d@���@���@���@���@���@��@�~�@��@�~i@�|�@�x.@�t>@�t�@�mF@�g&@�Y4@�LD@�G�@�DV@�Ac@�<�@�5�@�1�@�.x@�)@�$�@�f@��@��;@���@���@��@��@��Y@��>@��9@���@��u@��3@��@�s�@�lt@�be@�a�@�_�@�[�@�[@�R@�H�@�B1@�A�@�A�@�?�@�@@�?�@�?@�>W@�;�@�8[@�6f@�4@�3�@�3s@�2�@�2`@�2"@�26@�2�@�1�@�1P@�0�@�0�@�0X@�-�@�(L@�(y@�'g@�'j@�&�@�%�@�$�@�$�@�#�@�b@�W@�8@�.�@�9@�6'@�;�@�A@�?@���@��8@��6@���@��5@�k�@�3�@�~@��@���@��'@���@��@���@���@���@��@��@��^@���@���@���@�˥@�˧@��R@�ˑ@���@��E@��H@��`@��@��e@��b@��@��@���@� @�$@��@��	@��O@��4@�ײ@��M@�Ч@��A@��x@�˻@��	@�Ȣ@�Ǥ@��f@��5@���@��3@���@��`@���@���@���@���@���@��@���@��@���@���@���@���@��@��V@�w�@�r]@�k�@�h!@�e�@�h�@�h2@�d�@�bd@�a�@�a>@�^d@�]@�\|@�\�@�^�@�_�@�b@�ff@�i@�n@�o~@�o~@�o�@�r@�rF@�s@�rZ@�r�@�s�@�u&@�v:@�w�@�y�@�z�@�z$@�zR@�{u@�} @�{@���@��r@��r@��@��@��*@���@��P@��"@���@���@��@��6@��@���@��o@��@��@��C@��T@��C@��V@��@��@��@��V@��@��@���@���@���@��Z@��@��@��E@��@��v@���@���@��@��4@��v@��J@���@��6@��6@��6@��@���@���@��7@��@��@���@���@��C@��l@��@��j@���@��@���@��.@��l@���@��@��@��@��N@��@���@��@��@��@���@��@��@��Q@��}@��@��n@��|@��=@��@���@��@���@��@���@��@��{@���@��	@��@���@��@@��@��E@��G@��@��.@��>@���@��g@���@���@��*@��<@��d@��e@��|@��h@��?@��?@��j@��{@��x@��{@��~@���@���@���@��N@��@��r@��@��@��X@���@��@���@��@���@��:@��X@���@��:@���@���@��<@��F@��D@���@���@���@� �@� {@� A@��@��@��@��@��@��@��@� B@��v@��"@��e@��Q@��z@��b@��{@���@��f@��}@���@���@���@���@���@���@���@���@���@���@��@��"@��@���@��"@���@���@���@���@���@���@���@��b@���@��@��@��@��m@��,@��@��$@��@��V@��@��@��L@���@���@���@��^@��@��&@��@��@���AIF@���@��"@���@��*@��>@���@�޼@���@��P@��!@��@��@���@���@��	@��@���@���@��g@��@��]@��@��O@��@���@��J@���@�ߧ@���@��~@���@��B@��@�ݰ@�ݰ@�ݱ@��@��j@���@��'@��=@�߶@��@��#@��@���@���@�߻@��6@�ߢ@�ߐ@��R@���@�ݭ@�ܶ@�۴@���@���@��J@�Ί@��k@��\@��M@���@��?@���@��L@�@��g@���@��n@��@��K@��d@��"@��@��@��C@��@���@��@���@��b@��@���@��>@���@���@���@���@���@���@��S@���@��E@��a@���@��@��@��v@���@��k@���@���@���@��Z@��w@���@���@��~@��N@��/@�y�@�w�@�u�@�t�@�tB@�s�@�r�@�rI@�rJ@�q�@�q�@�q@�p�@�p�@�p�@�pe@�p=@�p;@�p9@�p�@�p�@�o�@�of@�o.@�n�@�n�@�n.@�n�@�nZ@�n2@�n@�m�@�nZ@�n^@�n+@�n]@�n]@�nn@�nn@�nW@�n/@�oj@�o�@�o�@�ok@�o�@�p�@�sm@�s?@�s,@�s2@�s/@�sp@�s/@�s/@�s/@�sD@�sD@�sq@�sD@�s�@�s�@�t�@�t>@�t(@�t@�t�@�u�@�uz@�vH@�v9@�vv@�v@�t�@�t�@�tj@�t�@�u=@�t�@�t�@�s�@�r�@�s@�r�@�q@�q"@�q:@�o@�m@�l�@�k�@�k@�k&@�j�@�iE@�h@�g#@�f�@�f@�c�@�c�@�b=@�_�@�_4@�^�@�^"@�]�@�]#@�[�@�YN@�Z@�Y�@�X�@�X�@�X=@�W�@�V�@�V�@�V�@�V@�U�@�T�@�T�@�S�@�S�@�S�@�S$@�R+@�Q@�O@�L�@�K�@�JJ@�F^@�@@�=�@�6�@�2�@�-�@�%�@��@�3@�4@��@���@���@���@��S@��x@���@���@��@���@���@��@��6@���@���@�o�@�`�@�V�@�K�@�E:@�=2@�35@�+�@�'>@�#f@�$@�B@��@���@��@��@��@��R@���@��r@�؆@���@��>@�΅@��
@���@���@��A@��@��:@��@��-@���@��@��U@���@��y@��Q@��n@���@��@��@���@���@���@���@��;@��x@��~@���@��>@���@��A@���@���@���@���@��`@���@���@���@��J@���@��*@���@��@���@�x�@�SP@�7b@� 6@�.@��@��@��@��@��@�@@�v@�"S@�%F@�&@�&�@�)J@�*�@�+U@�/�@�6z@�:@�@v@�@�@�D�@�E�@�FM@�G�@�I�@�P4@�P�@�Ox@�P	@�N$@�Jc@�E�@�DX@�C@�CV@�B @�;>@�5@�*�@�(�@�#&@�"�@�"�@��@�M@�z@��@�w@�@��@��@�
�@�
�@�	�@�	>@�o@�<@�@���@���@���@��Z@���@��@��@���@���@���@��b@���@���@��@���@�̧@���@��D@���@���@��R@��:@�Λ@��#@���@���@�İ@���@��@���@��*@���@���@��i@��{@�ƹ@��@@�Ȳ@��A@���@���@�ʭ@��~@��t@��@��1@��@��0@��@��E@��>@�ϼ@��@���@�σ@�α@��@��*@�Ȋ@�ą@���@���@���@���@��B@���@��8@���@��{@���@��N@���@���@��t@��=@��i@��<@��f@���@��.@���@���@��7@���@��@���@���@���@��z@��M@���@���@��^@���@��@��e@���@���@���@��@��k@���@���@���@��J@���@��t@��S@���@���@��b@���@��@���@���@���@�~-@�t�@�bP@�N{@�C�@�>�@�9F@�0@�,}@�,g@�,B@�,=@�,�@�-@�-R@�3�@�=6@�=
@�=�@�A�@�BF@�A9@�@@�@'@�?�@�<7@��@��}@���@��G@��@���@�	D@�B@� �@�F;@�G�@�C�@�<4@�)w@�`@��@��@��@��@�$#@��@��@��@���@��@��@��@��'@��@���@���@��@��@��d@��C@���@��
@��B@��<@��]@�ԓ@���@��@��F@��9@��e@��@��@��6@��+@��,@��H@��Y@��a@���@��@��c@���@��=@��x@��9@���@��@��@��A@���@���@���@��A@���@���@���@��@���@��d@���@���@���@���@���@��@�~�@��@�~i@�|�@�x.@�t>@�t�@�mF@�g&@�Y4@�LD@�G�@�DV@�Ac@�<�@�5�@�1�@�.x@�)@�$�@�f@��@��;@���@���@��@��@��Y@��>@��9@���@��u@��3@��@�s�@�lt@�be@�a�@�_�@�[�@�[@�R@�H�@�B1@�A�@�A�@�?�@�@@�?�@�?@�>W@�;�@�8[@�6f@�4@�3�@�3s@�2�@�2`@�2"@�26@�2�@�1�@�1P@�0�@�0�@�0X@�-�@�(L@�(y@�'g@�'j@�&�@�%�@�$�@�$�@�#�@�b@�W@�8@�.�@�9@�6'@�;�@�A@�?@���@��8@��6@���@��5@�k�@�3�@�~@��@���@��'@���@��@���@���@���@��@��@��^@���@���@���@�˥@�˧@��R@�ˑ@���@��E@��H@��`@��@��e@��b@��@��@���@� @�$@��@��	@��O@��4@�ײ@��M@�Ч@��A@��x@�˻@��	@�Ȣ@�Ǥ@��f@��5@���@��3@���@��`@���@���@���@���@���@��@���@��@���@���@���@���@��@��V@�w�@�r]@�k�@�h!@�e�@�h�@�h2@�d�@�bd@�a�@�a>@�^d@�]@�\|@�\�@�^�@�_�@�b@�ff@�i@�n@�o~@�o~@�o�@�r@�rF@�s@�rZ@�r�@�s�@�u&@�v:@�w�@�y�@�z�@�z$@�zR@�{u@�} @�{G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       3344334334344344444344334444443444443443344344434433344344444434434444444444343344434444444444444444443444434344434443444443444443344344443344434443344344433334333344443443434343444333334344333443443344333333433334333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222422222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�9J�9KY9KY9K9H�9H�9IL9I�9JL9IO9J�9J�9K)9K�9L�9KW9K�9K�9L9L9L9L9K�9J�9K9H9H�9H�9He9G�9G�9H9G�9Gf9G49F�9F�9F�9F�9F:9FY9F�9Fk9F)9F[9F[9F[9F�9G�9G�9G)9F59F�9G�9G�9G�9H 9H39H9H�9H09H�9G�9H 9G�9G�9G�9HD9I�9I�9J9J:9I�9Ip9I,9I�9I�9I�9I�9K9KV9I�9H�9K�9K�9J�9K�9I�9I19I9H�9Ia9G�9H09G�9G�9H49H9H9G�9G�9G�9G�9H9H�9H�9H�9H�9H�9H�9H�9H�9H�9H�9H�9H�9H�9H�9H�9I9I9IP9I�9I�9J�9K�9L�9L9Lr9M9M`9NJ9N�9P�9SG9S�9T�9UQ9Ub9U�9Vl9W89Wi9Wi9W�9XX9X19X9YU9Y`9Y`9Z,9Z9YP9Y9X9U�9U�9U�9U�9U�9U�9T�9U9T�9T�9U
9U9U9UA9U9U9U-9UA9U?9UQ9Uu9U�9Uu9UQ9U�9U9U!9U9Tv9TE9R�9Q_9=�9N�9M�9M-9M9L"9H�9F�9E�9DO9D9C�9C�9C99B�9B�9B�9Bz9A�9A�9@�9@B9?�9{�9>�9>I9>&9=�9<�9<�9=*9=\9=�9>H9>=9>89>�9>�9?9@A9@�9@�9@�9?�9?E9?y9@�9@�9?�9>i9>'9=�9=_9<�9<�9<�9<�9<T9<T9<T9<�9<�9=_9=�9=�9=�9>79>J9>�9>�9>9=�9>Y9=�9=�9=�9=P9<Q9;�9:�99[96�9339029-�9,9+39*9)�9(r9'19&�9%�9%9$�9$�9#�9#C9#9"49!Z9 �9 �9 r9�9�9@9 9�9W9�9�9�9I9�9�9e99�9�9)9�9h99_9v969;9�9
h9�9�9�9�N9��9�9�y9�9�!9�y9��9�9�9�P9�Q9�9��9�b9�29� 9��9��9�9�9�9� 9��9�W9� 9��9�]9�\9�9�\9�*9�
9��9��9�*9�-9�9�,9�,9�:9�:9�'9�9�9�49�&9�9�$9�9�:9�9�9�9�9�<9�9�9�9�9�9�=9�9�9�}9�69��9��9�9�y9��9��9�9�w9�9�U9�x9�x9�9�69�9�k9�{9�|9�9��9��9�b9�d9�w9�9�9�9��9��9��9�f9�9�(9�c9��9܉9ڕ9ځ9�v9�~9�9��9�-9�9�`9�X9�O9��9ҁ9�9ѵ9�t9�9�t9�19�19Ϭ9�[9��9�{9��9��9��9�_9̘9˿9�9�c9ǎ9�H9�$9�9�9��9��9�T9��9��9�\9�[9}�9dU9P9H�9E`9BI9@09=�9=/99�97#96�94w92�9/v9C9d99��9�69��9��9�9�39�9��9�e9��9��9�B9��9��9��9��9�L9�(9��9��9�%9��9�l9��9��9�V9�9��9�s9�V9b9y*9s9r9m�9ln9k�9kO9ka9j�9g�9g�9g*9f�9f�9f9ej9e9ez9e9d�9c�9c�9c�9c�9c�9c�9b09c�9a�9a9a[9`'9^>9QR93z99
�9S999�9�9	[9.9	�9>9�9C9�9�9�9u9�9b9F9$a9$�9'�9(�9)9*9+�90�91>90f90�9/V9,T9(�9'}9&}9&�9%�9 39599M9�9�9�9�9�9)9!9�9 9�#9��9�|9�89�`9�(9��9��9�@9�9�`9�l9�o9��9�S9�C9�O9�@9Ԝ9�n9��9�.9�b9М9Ǣ9�9��9��9�,9�]9�J9�39�99�N9Č9�A9��9��9�,9�9��9�U9��9±9��99�w9ŷ9ĭ9�9�9ƴ9�F9Ƚ9��9��9��9��9ɻ9ɵ9�9ɘ9�U9��9�D9��9�q9�W9�9�9��9��9��9��9�
9��9�k9�G9��9��9�V9�v9�9�9�69�9�49��9�:9��9��9��9�9�K9��9��9��9�F9�"9��9��9��9�=9��9�9��9��9�,9��9��9��9��9�*9��9� 9��9��9�u9�?9��9�^9��9�9��9�-9��9�89ry9b�9Z19U�9Q�9J<9G^9GL9G/9G+9Go9G�9H	9M9T�9T�9U39Xm9X�9W�9Wd9W9V�9S�919 �9�9�99 C9+)92\9=�9[�9]9Z!9S�9D�90�9.�91R97�9=9@�9<{9*�9�9�9i9�9�9�9,9 V9 �9_99p9!9=9�9�9�G9��9 �9
�9
�9"99	�9�9 99�9�9#919i9�;9��9�29�9�9�s9�9�M9�F9��9�C9�9�t9�O9�A9��9�9�^9�9��9��9�s9��9Ɩ9�L9�$9�D9�,9��9��9�r9��9��9�9�<9�T9�)9��9�9�s9�9�H9��9~�9{�9w�9t?9k9T[9O�9I9DI9@�9:�97�94{91C9)�99�09�N9�W9��9؁9�9�99�M9Ҩ9�a9�9��9��9��9��9�9��9�99��9��9��9�E9�m9�9��9�>9�9��9��9�P9��9�39��9��9�l9�-9��9�9�C9�E9��9��9�N9��9��8�HU8�7l8�)8��u8��8�8�
�8��8�Q8�~E8��8���8�~"8�x�8�W�8��8��8��u8��S8�P:8��8���8��:8�	`8�8�>g8�@8�;�8�:�8�:�8�=�8�Wl8�Wo8�V�8�WL8�W�8�S�8�K�8�R/8��8�8�8�|)8���8���8��a8��8���8��"8���8�y�8�j�8�b8�_q8�]48�ZX8�W�8�T�8�R�8�Q8�P�8�PP8�Q�8�S�8�R�8�R/8�S!8�Q�8�L�8�?�8�5�8�4�8�-�8�'�8�&8�#8��8��8��98��8���8��l8���8��8��~8���8��"8��8���8���8���8��n8��O8��`8��*8���8���8��Y8��B8��~8���8���8���8��?8��8��G8��t8��g8���8��d8���8�Μ8��Y8��}8�Ռ8���8��*8���8��u8��nG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B	7B	7B
=B
=BJBJBJBPB{B1'B\)B|�B|�B� B�B�+B�PB�hB�{B��B��B��B�LBƨB�#B�HB�;B�B�B�HB�#B�B�mB\B�B�B�B+B/B1'B6FB2-B49BC�B]/Br�B�%B~�BffBB�BB�BD�BM�BffB{�Bu�B��B��B��B��B�+B� B��B�-B��B�LB�-B�^B�jB�RB��B��B��B��B�B�dB�^G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B��B\4A��?G7�B rBo ?!�A�e
A�:?JΚB�!?��?B��Bx?'�AK�AB�?�i?"RiA�?KAe�?}�pBcUB_�?$oz@�?>�>��?�A�*B�&>���>�2�>ҁ4?t?�w!Bn�?�@�A�__A���?P�A%e�B��>�HM>�O?"x�A��b@,�?���B`�B_�B�^?(}�@b�uBf�@
3�?b�jA��m?5�XA���?6��A�Hj?�\6?�F�Bq�?��K>ǻ�?��>���? ]B?�A}-�@
ل?)��@��B�U?H��A���B^�A��c?�(@>�B e>�m�>��?���>�cx@��8Aچ@��v?�1D?Ý�?(9?^�ZAƨ>�`�>�g�?^�?��?��?y��B^VA� �?>��A>r�@��1A�(?��B�>�tC?[d?���Be�@��?Q��A�;kA�S<>��N>��>��?0?c�}B$�>��:>�G?R�J?4MU?��sBb�B�t@qȴ?PӞBz�>��N?�3�?���@�^�BbIB��>�Y@^m?b.
Bcb@O�?�sA��B`zB�?! .A4�qBrPA��r?!�
@�s�B_�B^?BcqB�v?�4BgqBi%Bg�Bt�>�bA	GE? l�@"hMB~�?�.�@�(�BmA��A�@3?B�Bg�@���B��A9D?3�@��:Ba�Bc�B_�Bf�B�_?oQHBo�?�O?}}9BaFB^�Be.@�DV?n��B��@k�z@�/�BgUBg@��E@|,�Bf'BpEA�_=B_�A��B��@β�Bg�Bi�Bd�B��ABgiBg9Bf�Bi�Bi�Bk\Bj%B��Bh>Bg�Bg`Bj�Bk5Bf�Bh�BipBh�BdBhgBiBh�Bf�B`�Bd;Bb�B]2Bg�BcIBbBb�BeBf�Be�Bb:BjpBc�Bb{Bi�Bc�Bf�Bj�Bg<Be�BhBf�Bd
B`AB`VBe8Bc�BjtBfABhBg�Bf�Bg�Bd�Bh�BeBb�BhVBh�Be0BbBe�Bd�Bf�Bf�Bd�Bb�BfBhTBiBiBd�BiBh�BgBg�BiSBi�Bf�BgUBh�BiYBi BfBd�Bh�BgEBgjBgMBf�Bi�Bi�BfBgBc�Bg�Bh�Bc�BhBd�Bd
BbVBd;Bc�Bc�Bb:Be7Bd�Be�B^Bf�BfwBi�BjSBjBikBk�Bh�Bf4Bj�Bh�BiwBk�Bh�Ba�Bd�Bc,BdBf�Bc�B`@Be�BehBbsBavBb�Bg�Bb�BcBcBhyBh�Be"BeB]WBh�BmmA���Bh6BgiBf�Bc�Bf�Bg9Be6Bh�Bf�Bm7BlBj�Bh�Bh5Bf�Bg�Bh]Be�Bh�BgiBhQBcIBfBe�Bg�Be�Bw�B`,BcBc�B_|Be"Bc)Bd_BfXBf�Br�Bi�Bi�BhBh�Bg�Bg�Be!Bh�Bh�Bf�BjIBiBh4BhBj�Bg�Bk�Bi�Bh�BhBhBe�Be[Be"BhPBg�Bh�Bg�A���Bh�Bh�Bg�Bh=Bg�Bi�BjRBmTBi�BkBj�Bl�BjfBj.Bk3Bb%Be�Bc�Bc�Bd�BbBBd�Bi�BiYBh#Bf[BdjBk�Bc�B`�Bd�BcFBfBhIBftBcFB`,Bf�Bc BdBb�Be�Be�Bb�Bd�BdCBh�Bj:Be�Be�Bd�BcwBeBe�Bf�Bg�A��B��Bc BJSBf�BfCBdrBe6BfBe^Be9BeaB`�Bi�Bk(BdhBclBg�Bf<BbBBgMBe0Bi�BfqBg,Bg$BcBeBg/Ba�BgXBcBdBd�BbdBa�B\�Bh�BdBc�BcTBgNBg=BcBhMBh�Bd�BfSBd"BdJBe�Bg_Be�Bd�BcLBf BfBdXBe�Bd�Bf�BisBg�Bf�Bf�Bh"Bh_Bb�Be<Bb?BdtBfBe�Bb�Bf�BftBd�Bd�Bh"Bf�BfBhtBg�Bg�BjhBiBi�Bj�BhbBi�Bg�Bh@Bh�BjEBhXBhlBj7Bi3Bh�BiBi�BiWBiBi>Bh�Bh�BjnBjABh�BiBiBh�Bh?Bh|Bh�Bi�Bi�Bi�BjBh�BjBkBj_BjjBjBkBg�BhFBi�BicBj:BhVBj�Bi�Bj�BjABj~Bj�BjnBjfBi�BjLBj�BjBj�Bk3Bj�Bk�Bk�Bk�BkvBk[Bk]Bj(BkLBm�BnOBm�BmBn.BnBn(Bp6Bs�Bt�Bu�Bz�B|�B|�B}�B�B�xB��B��B��B�%B�xB��B��B��B�/B�:B��B�B��B��B��B��B��B��B�B�,B�BǣB϶BєBءBؙBؑBٚB�DB�cB�6B�BٶBٮB�B��B�^B�TBئB��B؍B��B�&BٶB׳B�*B�LB�7B�B�oB�pB�%B��B�B�BשBׅB��B��B�hB��B׾B�
B�UBےBܤB�`B��B�YBޓB�
BݐB�TB��BݩB�mB�?B� B�B�BB�@�B��B߼B��B�mB��B�B�fB�B��B�CB�B�B�SB�.B�B��B�LB��B�B�=B�4B�B�6B�B�]B��B�aB�B�FB�B��B�B�B�pB��B�6B�|B�%B�DB�B�+B�|B�|B�9B�B�YB�B�B�VB� B��B�B�@B�.B�=B��B�\B�xB�B��B�B��B��B�B�B�*B�?B�SB�pB��B�B�B�B�xB�>B�pB�B�B�B�B�GB�B�B�B�B�B�B�B�/B��B��B�B��B�)B�"B�RB�B��B�B��B�YB�<B��B�TB��B��B�!B�YB�CB�B�+B��B�B7B,B�B	�B
�BB�B�B^B�B5B�B�BB`B(B�B�B�B�B6B�B,B$B�B�BtB	B�B0BDB�B�B�B+B�BB�BrB�BfBzB �B$�B&�B(�B,B-�B.oB/B/�B0�B/rB08B0�B0'B0�B1OB3B3�B3�B3B4�B5[B5�B6B6gB7-B8TB8�B9BB9B:CB:�B:�B9B8�B;�B:�B:.B;B:�B9YB:<B8fB;B:�B:�B;#B;�B;B9�B9dB:GB9�B9TB8�B:.B8�B7�B8�B9�B95B8�B7}B8�B7�B8B7ZB7!B6�B7^B5:B4XB4mB3�B0�B.�B.�B-�B.�B.�B.�B.B;LB.MB+�B+�B;�B8B0)B/pB0zB0B-�B,�B-�B3�B* B+B/�B-xB3TB>�BDBE)BCxBCBB�B?�B;�B=�B;HB7<B4;B,iB2xB8ZB9	B9B9�B8B7�B:B9�B6B5�B3�B5�B7B94B7�B7�B6�B7KB5�B4�B6=B6)B4�B3�B0�B1UB-�B.�B-�B0B.�B,�B,�B*OB2DB1$B3�B2�B2B2�B1�B2cB2�B3RB1�B1�B1!B0�B0�B0gB.�B.�B.{B,]B-B+�B,sB'�B(nB(�B&B"�B!�B(BzB�B�B3B�B#4B%�B)$B.XB3�B4uB5B=UB?�B@�BH�BJBL�BQBRKBW�BVGBW�B[B\+B[�B\�B]�B^rBaBb�Be�BdqBcQBh*BfBeBBf�BfLBdBa\BaxBd�BbDBfkBm5BnLBu�Bw�Bw�BwjBwBv�BwjBvBu3BuBv"Bu�Bt�BtnBr�Bt�BtOBq�Bs�Br�Bm�Bk�Bp5Bi�Bq=BpBq�BtlBs8Bq�BpMBj�BpBk�Bn�Bs#Bz�Bw�BwnBsQBt�Bs=Bq$Bp�BsLBuXBv�Bw�Bx�B}�B{�B{�Bz�B{�B|�B|�B�B��B��B�iB��B��B�pB�^B�OB��B�JB�5B��B��B�B��B�B�B��B��B�OB�fB�B��B�>B�(B�)B�B�(B��B�B�FB��B��B�&B�.B�4B�@B�7B��B��B��B�`B��B��B��B�GB�B��B�~B�MB��B��B�1B��B��B��B��B�NB��B��B�QB��B��B��B�vB��B�lB�mB��B��B�B�B�OB�NB��B~�Bz�Bp{Bl#BQB��B��B��B��B��B�RB�UB��B�B�^B��B��B�2B�WB��B�B�fB�bB��B��B�Bt�B|fB�mB�mB�~B��B�~B�B�XB��B��B�fB�kB�-B�B��B�B��B��B�ZB��B�AB��B��B�bB�B�bB��B�0B��B�B��B�$B��B��B��B��B��B�kB�`B�=B�B�BB�pB�:B��BΠB�B�)B�eB�B��B�XBТB�kBԅB��B��B�=B�yB�iB׭B�>B�B�TBޚB��BކBޯB�PB�FB�rB��B۳B�B�B� B�^B֊BՍB�bB�[BӺB��B�5B�dB�BɮB�sB�nB�B��B�B��B��B�)B�wB��B�gB�IB��B�]B��B��B�dB�>B�.B��B�B��B��B��B�B��B�B��B�eB�,B�{B��B�OB�B�zB��B��B�	B�"B�B��B��B�B�B�6B��B�}B��B�B�B�1B�YB��B��B�iB�@B�TB��B��B�VB��B�<B��B��B��B��B��B�9B�yB�iB��B�B�BւB�wB��B�B�KB�ZB��BgB �B��B��B�#B�`BњB�)B��B�DB��BͿB�'B�5B��B�TB�:B�B�,B�TB�B�EB�8B�&B�hB�^B��B�B�B;BIB�B	mBB�BUBfB
�BfBqB B�B�BBBB�B�B�B�B�B�B�B�B7B�BB:B
�BB�BB�BB�B\B�
B��B��B�~B4B	/B"B�B�B�BoB!B$�B+�B4�B5B;�BD)BHyBP�BW�BZ�B\�BZBV�B[�BaBaB`B^kB]EBZ�BY)BV BQBOdBUCBW�BV?BRRBQxBMwBJBQ BR�BS)BTyB	&B	�B�B	�B�B"B�B	�BB
B	�B	.B
wB�B	�B	B�B	�B	�B
sB
WBBRB	�B	�B
RB
/B�B	�B�B
vB
�B
>B
BXB
�B	�B
'B	�B
B
�B�B
$BaB
 B	.B
�B%B	�BfBBB*B
�B�B
sBB�B�B�BB�B
[B�B�B�B9B_B�BBqB�B�BdBgBB�B�BxB�B�B�B6BBmB"BvB�BFB=B�B�B^B�BEB�B(B$B�B�B�B�B�BZB�B2B5B�B_B?B BBBB�B�BYB.B\BLBB$BBB1BTB!B�B�B�B�B!B%bB$�B(aB.�B2TB4gB6XB8B7�B8�B<B>{B>�B>�B@�BC�BCWBEsBPiBQ6BXJBaBe�Bo�Bv�ByCB|/B{�B|B|�B|B|�B|�B~9B}?B}AB|�B~"B|}B}fB}6B}B}1B}�B}+B~B|�B}XB}<B|:B}7B}qB}tB|�B}kB}�B~\B~nBi�B�&B��B��B�B��B��B�`B��B�lB�KB�qB�aB��B�B�B��B�OB�bB��B��B��B��B��	B�	B�jB�3B��B��B�iB�B�:B��B�oB�B�CB��B��B��B�8B�~B�mB��B��B�:B�lB��B�GB��B��B�8B��B� B�MB��B��B�UB��B��B��B�
B�B��B��B��B�^B��B��B�/B� B��B��B�[B��B�EB�B�cB��B��B�uB�4B�*B��B� B��B�:B��B�9B��B��B��B��B�LB�_B��B��B��B�uB��B��B��B��B�"B��B��B��B�B�}B�'B�tB��B�XB�*B��B��B��B�HB��B��B��B�B��B�gB�nB�ZB��B��B�[B�RB��B�NB��B�|B��B��B��B�B��B��B��B��B�:B�B�$B��B��B��B��B�sB��B��B�|B��B�B�<B��B�YB�XB��B�KB��B�HB��B�B��B�0B�B��B�
B�B��B��B��B��B��B��B�|B��B��B�vB�GB�iB��B�OBзB�gB�_B�+B�]B��BөB�mB�xBӬBԗB�gBՠB�lB�BםBׁB��B��B�UB�B�%B�	B�B�qB�6B�.B�4BߛB�XB�B�B�B��B�"B�B��B��B�B�GB�B�B��B�7B�vB�B�CB��B�B�B�^B�B��B�B� B�ZB�B�SB�$B�#B�zB��BޢB��B�BެB�^B�xB޿B�iB�`BއB��B݀BޮB��BޒBމB�B�|B�AB��B�MB�BݕB��B��B�|B��B�~B�vB��B��B�B��B��B�kB�<B�UB�B��B��B�B�!B�B�dB��B�B��B�B�B�B��B�B��B�kB��B�zB�}B��B�/B�B�B�QB�B�1B�&B�[B�B�lB�B�KB�6B�)BߊBߴB�xB�sB�IB�B�WB��B�<B޳B��B�kB܎B��BڨB��B��B� B�(BڋBڃBڪB�5B�6B�B�B١B�BِBڏB٭B��BٰB�0B�PB�GB�EB��B�OBוB��B�PB�rB�sB��BӗBʹBπB�~B�\B�>B�4B��BߐB�6B�UB�B�B�B�hB�B�GB��B��B�PB��B��BCB!B�B�B�BBB�BbB�BiB�B�BxB�BB�B�B)BBBHB\BpB eB B BCBrB�B�B&B�B�B3B B�BzB�B�B�B�B>B�B�B�BB.B�B�BUB,B.B�BBBBGB"BIB�BCBZBB|B�BpB.B�BB�B�B�BB?B�B B �B"�B$IB$�B%�B'�B'DB(�B)�B)�B)�B+xB*�B+�B,�B-CB-B-�B-�B.�B.tB.�B/�B/qB/�B/2B.�B/B/'B/bB/1B.�B/B/�B.�B.iB.�B-nB->B-]B-pB,�B,�B.jB.�B/dB0fB3}B3�B3�B4B3�B4iB3AB4B5jB6%B5tB5�B5�B6B7jB7�B6�B4�B4�B4�B5�B5�B5�B5�B7�B8'B7�B7|B7,B6B5B4�B2�B0�B/�B-�B-tB.�B.�B-�B-�B-�B.HB.�B.�B/*B/eB5WB5�B5�B6WB6�B6�B6�B5kB6
B<�B5fB1�B-tB,}B-�B3�B7�B:�B>�BQvBR�BWJBW7BW"BP�BP;BRHBW,BZ�B](B^�B]�BZ�BZ�BZ!BZ�B[�B\2B^�B`kB`�B`�B`�B_�B`�B`B`�BbB^�BbHBh�Bs	Bs�Bs\Bt�Bu�Bz�B}]B~�B�/B��B��B�oB�B��B�>B��B�EB�-B�@B�yB��B�wB�VB��B�AB��B��B�LB��B�rB� B�>B��B�B��B�!B�uB�B��B�YB��B��B�NB�BWB}�BB|B{Bz�Bw>BvBtyBt�BtZBq�Br"Bq>BpBoOBo�Bi�Bh�BgmBd�Be�BbcBb�Bb#BbSB_�Bc6BV;BO�BMzBI�BG]BF�BF�BE�BE�BEIBB3B@�B?�BA#B?�B@B>�B?�B?�B?,B?B?�B>�B?:B?	B?B>�B?NB?YB?�B?�B@B@�BA�BCBEGBCsBDeBD&BD�BD�BD�BD�BENBD�B�pB�LB��B�`B��B�_B�gB�B�TB��B��B��B��B�zB�[B��B��B�}B��B�YB~-By�Bx�B��B�B��B��B��B��B��B�B�B��B��B��B��B��B��B��B��B�&B�B�iB��B�RB�zB��B��B��B�]B��B��B��B��B��B��B��B�nB��B��B�B��B��B��B�\B��B�B��B�B��B��B��B��B�lB�>B��B�/B�3B��B�/B��B�LB��B�IB��B��B��B�NB��B��B�)B��B�(B��B�
B��B�$B�B��B�/B�jB�HB�2B�)B�#B��B�B��B��B��BÆB�cB�8B�B�fB¡B��B�_B�rB~G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111119999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999993344334334344344444344334444443444443443344344434433344344444434434444444444343344434444444444444444443444434344434443444443444443344344443344434443344344433334333344443443434343444333334344333443443344333333433334333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222422222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999   B	QB	QB
TB
VBdBdBfBiB�B1?B\EB}B}B�B�(B�HB�jB��B��B��B��B�B�fB��B�=B�cB�UB�B�B�cB�=B�+B�BvB�B�B�B+B/6B1BB6`B2JB4SBC�B]HBr�B�@BBf�BB�BB�BD�BM�Bf�B|Bu�B�B�B��B��B�GB�B��B�JB��B�iB�GB�}B��B�pB��B��B��B��B� B��B�xG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B��B\MG�O�G�O�B r%Bo<G�O�A�e7A�`G�O�B�9G�O�G�O�Bx*G�O�G�O�G�O�G�O�G�O�A�?pG�O�G�O�BcmB`G�O�G�O�G�O�G�O�G�O�G�O�B�>G�O�G�O�G�O�G�O�G�O�BoG�O�G�O�A�_�A��G�O�G�O�B��G�O�G�O�G�O�A���G�O�G�O�B`�B_�B�yG�O�G�O�Bf�G�O�G�O�G�O�G�O�G�O�G�O�A�H�G�O�G�O�BrG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B�lG�O�A���B_G�O�G�O�G�O�B {G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B^qG�O�G�O�G�O�G�O�A�NG�O�B�G�O�G�O�G�O�Be�G�O�G�O�G�O�A�SbG�O�G�O�G�O�G�O�G�O�B$�G�O�G�O�G�O�G�O�G�O�Bb�B�G�O�G�O�Bz�G�O�G�O�G�O�G�O�BbdB��G�O�G�O�G�O�BczG�O�G�O�G�O�B`�B�9G�O�G�O�BrlG�O�G�O�G�O�B_�B^XBc�B�G�O�Bg�Bi>Bg�Bt�G�O�G�O�G�O�G�O�B
G�O�G�O�BmG�O�A�@bG�O�Bg�G�O�B��G�O�G�O�G�O�Ba�Bc�B_�Bf�B�|G�O�Bo�G�O�G�O�Ba\B^�BeGG�O�G�O�B��G�O�G�O�BgmBg G�O�G�O�Bf@Bp`A�_^B`A��B��G�O�BhBjBd�B��G�O�Bg�BgTBf�Bi�BjBkxBj?B�BhWBg�Bg{Bj�BkPBf�BiBi�Bh�Bd9BhBi/Bh�Bf�B`�BdWBb�B]NBg�BcdBb!Bb�Be5BgBe�BbWBj�Bc�Bb�Bi�Bc�Bf�Bk
BgTBe�Bh*Bf�Bd&B`^B`qBeQBc�Bj�Bf[Bh*Bg�Bf�Bg�Bd�Bh�Be#BcBhrBh�BeIBb6Be�BeBf�Bf�Bd�Bb�Bf!BhmBi/Bi/BeBi/Bh�Bg2Bg�BinBi�Bf�BgmBh�BisBi:Bf)Bd�Bh�BgaBg�BggBf�BjBi�BfBg:Bc�Bg�Bh�Bc�Bh(Bd�Bd&BbpBdWBc�Bc�BbUBeNBd�Be�B^5Bf�Bf�Bi�BjkBj3Bi�Bk�Bh�BfNBj�Bh�Bi�Bk�Bh�BbBd�BcFBd)Bf�Bc�B`[BfBeBb�Ba�Bb�Bh Bb�Bc!Bc3Bh�Bh�Be;Be(B]pBiBm�A��BhPBg�Bf�Bc�BgBgTBeNBh�Bf�BmQBl7BkBh�BhPBf�Bg�BhvBe�Bh�Bg�BhgBcdBf�Be�BhBe�Bw�B`IBc'Bc�B_�Be;BcDBd{BfqBf�Br�Bi�BjBh&Bh�BhBh	Be>Bh�Bh�Bf�BjdBi5BhLBh1Bj�Bg�Bk�Bi�Bh�Bh1Bh&Be�BevBe;BhgBhBiBhA���BiBh�Bh	BhWBg�Bi�BjnBmnBi�Bk.Bj�Bl�Bj�BjIBkNBb>Be�BdBc�Bd�Bb]Bd�BjBiuBh?BfuBd�Bk�Bc�B`�Bd�BcbBf'BhcBf�BcbB`IBf�Bc<Bd$Bb�BfBe�Bb�Bd�Bd_Bh�BjUBe�Be�Bd�Bc�BeBe�Bf�Bg�A��(B��BcBJlBf�Bf[Bd�BeQBf'BezBeSBe}BaBjBkDBd�Bc�BhBfVBb\BggBeIBi�Bf�BgHBg=Bc)BeBgJBbBguBc'Bd&Bd�Bb~BbB\�BiBd!Bc�BcoB	@B	�B�B
B�B;B�B	�B+B
%B	�B	HB
�B�B	�B	B�B
	B	�B
�B
qB&BmB	�B	�B
hB
KB�B	�B	B
�B
�B
VB
*BoB
�B	�B
>B
B
&BBB
=ByB
B	GB
�B=B	�B~B^BABBB
�B'B�B�B�BB�B
tB�B�B�BUB{B�B*B�B�B�B}BB!BB�B�B�B�B�BPB%B�B>B�B�B_BYB�B
ByB�B^B�B@B?B�B�B�B�BBvB�BMBNBBxBVBB\B8B	B�BtBIBsBfB'B>B5B0BIBoB:B�B B�B�B!2B%|B$�B({B.�B2lB4�B6rB8�B7�B8�B<B>�B>�B>�B@�BC�BCnBE�BP�BQQBXdBa-Be�Bo�Bv�By_B|IB{�B|B|�B| B|�B|�B~UB}WB}\B|�B~<B|�B}�B}NB}-B}HB~B}CB~B|�B}rB}WB|TB}RB}�B}�B|�B}�B~B~tB~�Bj
B�AB��B��B�B��B��B�zB��B��B�eB��B�yB�B�3B�+B��B�fB�~B��B��B��B�G�O�B�&B��B�LB�B��B��B�)B�RB��B��B�3B�]B��B��B��B�RB��B��B��B��B�TB��B��B�bB��B�B�SB��B�B�gB��B��B�qB��B��B��B�"B�3B��B��B��B�sB��B��B�HB�B�B��B�sB��B�`B�B�~B��B��B��B�NB�AB��B�<B��B�TB�B�QB��B��B��B��B�eB�zB��B��B��B��B�B��B��B��B�9B�B�B��B�"B��B�AB��B��B�sB�CB��B�B��B�bB��B��B��B��B��B��B��B�qB��B�B�vB�iB��B�iB��B��B�B��B��B�2B��B��B��B��B�TB�+B�?B�B��B� B��B��B�B��B��B��B�*B�VB��B�sB�qB��B�eB��B�aB��B�B��B�JB�/B��B�%B�B��B��B��B��B�B��B��B�B��B��B�`BłB��B�hB��BсB�{B�FB�wB�B��BԈBԑB��BԳBԂBջBֆB� B׷BךB��B��B�nB�B�<B�$B�"BߌB�PB�FB�PBߴB�rB�B�*B�(B��B�;B�B��B��B��B�_B�B��B�B�SB�B�B�^B��B�B�6B�xB��B�B�+B�;B�wB�(B�mB�?B�<BߕB� B޼B��B�/B��B�zBߓB��BނB�yBޟB�BݘB��B��BޯBޢB�$BߕB�[B� B�gB�-BݭB��B�B�B��B�B�B��B��B��B��B��B��B�XB�oB�,B��B��B�B�>B�2B�}B��B�B��B�+B��B��B�B�B��B�B�B�B�B��B�IB�/B�B�kB�B�KB�AB�uB�5B�B��B�eB�PB�BBߤB��BߒBߎB�fB�$B�rB�B�TB��B�BۅBܪB��B��B��B��B�9B�ABڣBڟB��B�QB�TB�*B�'BٺB�,B٪BڨB��B�B��B�KB�jB�bB�_B�B�hBׯB�B�jB׌BۊB�BӳB��BϚBΖB�xB�ZB�LB�B߬B�OB�pB�2B�B�-B��B�B�aB��B��B�kB��B��B_B9B�B�B�B B�B�B~B�B�B�B�B�B�B;B�B�BGB\BbBxB�B |B 4B /B\B�B�BBBBB�BOB8BB�B�B�B�B�BXB�B�BB2BIB�BBmBEBGB�B(B,B)BbB=BdB�B^BrBB�B�B�BJB�B!B�B�B�BBWB�B B �B"�B$eB%B%�B'�B'^B)B)�B)�B)�B+�B*�B+�B,�B-^B-<B.B.B.�B.�B.�B/�B/�B/�B/OB.�B/B/CB/�B/MB.�B/6B0B/B.�B.�B-�B-YB-xB-�B-B,�B.�B.�B/~B0�B3�B3�B3�B41B4 B4�B3[B44B5�B6CB5�B5�B5�B6(B7�B7�B6�B5B4�B5B5�B6B5�B5�B7�B8BB7�B7�B7EB68B51B4�B3B0�B/�B-�B-�B.�B.�B-�B-�B-�B.aB.�B/B/GB/�B5sB6B5�B6sB6�B6�B6�B5�B6(B=B5�B1�B-�B,�B-�B3�B7�B:�B>�BQ�BR�BWgBWPBW?BQBPWBRcBWJBZ�B]AB^�B]�BZ�BZ�BZ<BZ�B[�B\NB^�B`�B`�B`�B`�B_�B`�B`&B`�Bb5B^�BbaBiBs'Bs�BsvBt�Bu�Bz�B}xB~�B�LB��B��B��B�B��B�ZB��B�cB�IB�XB��B��B��B�sB�B�]B�B�B�gB��B��B�B�]B��B�7B�B�:B��B�;B��B�sB��B��B�jB�3BtB~B2B|9B{1Bz�BwWBv#Bt�Bt�BtyBrBr>Bq[Bp0BokBo�Bi�Bh�Bg�Be Be�Bb|Bb�Bb=BbmB`BcQBVZBO�BM�BI�BGxBF�BGBFBFBEeBBNB@�B?�BA?B?�B@,B>�B?�B@B?IB?B?�B?B?SB?$B?B>�B?gB?qB?�B?�B@&BABA�BC2BEaBC�BD�BD@BE
BEBEBEBEkBD�B��B�jB��B�{B��B�|B��B�5B�rB��B�
B��B��B��B�wB��B�B��B��B�uB~JBy�Bx�B�B�6B��B��B��B�B��B�/B�:B� B��B��B�B��B��B��B��B�CB�7B��B��B�nB��B��B�B��B�{B��B��B��B��B��B��B��B��B��B��B�/B��B��B��B�wB��B�"B��B�.B��B��B��B��B��B�]B��B�NB�PB��B�JB��B�iB��B�fB��B��B��B�iB��B��B�DB��B�DB��B�%B�B�?B�;B��B�NBB�eB�NB�JB�?B��B�:B�B��B��BãB�B�TB�"BƄB»B�B�}B��B~6B	@B	�B�B
B�B;B�B	�B+B
%B	�B	HB
�B�B	�B	B�B
	B	�B
�B
qB&BmB	�B	�B
hB
KB�B	�B	B
�B
�B
VB
*BoB
�B	�B
>B
B
&BBB
=ByB
B	GB
�B=B	�B~B^BABBB
�B'B�B�B�BB�B
tB�B�B�BUB{B�B*B�B�B�B}BB!BB�B�B�B�B�BPB%B�B>B�B�B_BYB�B
ByB�B^B�B@B?B�B�B�B�BBvB�BMBNBBxBVBB\B8B	B�BtBIBsBfB'B>B5B0BIBoB:B�B B�B�B!2B%|B$�B({B.�B2lB4�B6rB8�B7�B8�B<B>�B>�B>�B@�BC�BCnBE�BP�BQQBXdBa-Be�Bo�Bv�By_B|IB{�B|B|�B| B|�B|�B~UB}WB}\B|�B~<B|�B}�B}NB}-B}HB~B}CB~B|�B}rB}WB|TB}RB}�B}�B|�B}�B~B~tB~�Bj
B�AB��B��B�B��B��B�zB��B��B�eB��B�yB�B�3B�+B��B�fB�~B��B��B��B�B��!B�&B��B�LB�B��B��B�)B�RB��B��B�3B�]B��B��B��B�RB��B��B��B��B�TB��B��B�bB��B�B�SB��B�B�gB��B��B�qB��B��B��B�"B�3B��B��B��B�sB��B��B�HB�B�B��B�sB��B�`B�B�~B��B��B��B�NB�AB��B�<B��B�TB�B�QB��B��B��B��B�eB�zB��B��B��B��B�B��B��B��B�9B�B�B��B�"B��B�AB��B��B�sB�CB��B�B��B�bB��B��B��B��B��B��B��B�qB��B�B�vB�iB��B�iB��B��B�B��B��B�2B��B��B��B��B�TB�+B�?B�B��B� B��B��B�B��B��B��B�*B�VB��B�sB�qB��B�eB��B�aB��B�B��B�JB�/B��B�%B�B��B��B��B��B�B��B��B�B��B��B�`BłB��B�hB��BсB�{B�FB�wB�B��BԈBԑB��BԳBԂBջBֆB� B׷BךB��B��B�nB�B�<B�$B�"BߌB�PB�FB�PBߴB�rB�B�*B�(B��B�;B�B��B��B��B�_B�B��B�B�SB�B�B�^B��B�B�6B�xB��B�B�+B�;B�wB�(B�mB�?B�<BߕB� B޼B��B�/B��B�zBߓB��BނB�yBޟB�BݘB��B��BޯBޢB�$BߕB�[B� B�gB�-BݭB��B�B�B��B�B�B��B��B��B��B��B��B�XB�oB�,B��B��B�B�>B�2B�}B��B�B��B�+B��B��B�B�B��B�B�B�B�B��B�IB�/B�B�kB�B�KB�AB�uB�5B�B��B�eB�PB�BBߤB��BߒBߎB�fB�$B�rB�B�TB��B�BۅBܪB��B��B��B��B�9B�ABڣBڟB��B�QB�TB�*B�'BٺB�,B٪BڨB��B�B��B�KB�jB�bB�_B�B�hBׯB�B�jB׌BۊB�BӳB��BϚBΖB�xB�ZB�LB�B߬B�OB�pB�2B�B�-B��B�B�aB��B��B�kB��B��B_B9B�B�B�B B�B�B~B�B�B�B�B�B�B;B�B�BGB\BbBxB�B |B 4B /B\B�B�BBBBB�BOB8BB�B�B�B�B�BXB�B�BB2BIB�BBmBEBGB�B(B,B)BbB=BdB�B^BrBB�B�B�BJB�B!B�B�B�BBWB�B B �B"�B$eB%B%�B'�B'^B)B)�B)�B)�B+�B*�B+�B,�B-^B-<B.B.B.�B.�B.�B/�B/�B/�B/OB.�B/B/CB/�B/MB.�B/6B0B/B.�B.�B-�B-YB-xB-�B-B,�B.�B.�B/~B0�B3�B3�B3�B41B4 B4�B3[B44B5�B6CB5�B5�B5�B6(B7�B7�B6�B5B4�B5B5�B6B5�B5�B7�B8BB7�B7�B7EB68B51B4�B3B0�B/�B-�B-�B.�B.�B-�B-�B-�B.aB.�B/B/GB/�B5sB6B5�B6sB6�B6�B6�B5�B6(B=B5�B1�B-�B,�B-�B3�B7�B:�B>�BQ�BR�BWgBWPBW?BQBPWBRcBWJBZ�B]AB^�B]�BZ�BZ�BZ<BZ�B[�B\NB^�B`�B`�B`�B`�B_�B`�B`&B`�Bb5B^�BbaBiBs'Bs�BsvBt�Bu�Bz�B}xB~�B�LB��B��B��B�B��B�ZB��B�cB�IB�XB��B��B��B�sB�B�]B�B�B�gB��B��B�B�]B��B�7B�B�:B��B�;B��B�sB��B��B�jB�3BtB~B2B|9B{1Bz�BwWBv#Bt�Bt�BtyBrBr>Bq[Bp0BokBo�Bi�Bh�Bg�Be Be�Bb|Bb�Bb=BbmB`BcQBVZBO�BM�BI�BGxBF�BGBFBFBEeBBNB@�B?�BA?B?�B@,B>�B?�B@B?IB?B?�B?B?SB?$B?B>�B?gB?qB?�B?�B@&BABA�BC2BEaBC�BD�BD@BE
BEBEBEBEkBD�B��B�jB��B�{B��B�|B��B�5B�rB��B�
B��B��B��B�wB��B�B��B��B�uB~JBy�Bx�B�B�6B��B��B��B�B��B�/B�:B� B��B��B�B��B��B��B��B�CB�7B��B��B�nB��B��B�B��B�{B��B��B��B��B��B��B��B��B��B��B�/B��B��B��B�wB��B�"B��B�.B��B��B��B��B��B�]B��B�NB�PB��B�JB��B�iB��B�fB��B��B��B�iB��B��B�DB��B�DB��B�%B�B�?B�;B��B�NBB�eB�NB�JB�?B��B�:B�B��B��BãB�B�TB�"BƄB»B�B�}B��B~6G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111119999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999993344334334344344444344334444443444443443344344434433344344444434434444444444343344434444444444444444443444434344434443444443444443344344443344434443344344433334333344443443434343444333334344333443443344333333433334333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222422222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            CNDC            PSAL            PRES            TEMP            CNDC            PSAL            PRES            TEMP            CNDC            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            CNDC_ADJUSTED = (sw_c3515 * sw_cndr(PSAL_ADJUSTED, TEMP, PRES_ADJUSTED)) / 10                                                                                                                                                                                   PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            CNDC_ADJUSTED = (sw_c3515 * sw_cndr(PSAL_ADJUSTED, TEMP, PRES_ADJUSTED)) / 10                                                                                                                                                                                   PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =0.31 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            dP =0.31 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            dP =0.31 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   none                                                                                                                                                                                                                                                            No significant salinity drift detected. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                                                                                                                                             Unpumped STS data; quality control flags assigned using automated protocols outlined in Argo Quality Control Manual. Additional quality control and/or inspection recommended prior to use                                                                      Unpumped STS data; quality control flags assigned using automated protocols outlined in Argo Quality Control Manual. Additional quality control and/or inspection recommended prior to use                                                                      Unpumped STS data; quality control flags assigned using automated protocols outlined in Argo Quality Control Manual. Additional quality control and/or inspection recommended prior to use                                                                      Unpumped STS data; quality control flags assigned using automated protocols outlined in Argo Quality Control Manual. Additional quality control and/or inspection recommended prior to use                                                                      High-frequecy sampling; data contain repeated/non-monotonic pressure readings for the purposes of cross-calibration with STS data; additional data processing is recommended prior to use                                                                       High-frequecy sampling; data contain repeated/non-monotonic pressure readings for the purposes of cross-calibration with STS data; additional data processing is recommended prior to use                                                                       High-frequecy sampling; data contain repeated/non-monotonic pressure readings for the purposes of cross-calibration with STS data; additional data processing is recommended prior to use                                                                       High-frequecy sampling; data contain repeated/non-monotonic pressure readings for the purposes of cross-calibration with STS data; additional data processing is recommended prior to use                                                                       202009011536192020090115361920200901153619202009011536192020090115361920200901153619202009011536192020090115361920200901153619202009011536192020090115361920200901153619AO  AO  AO  ARCAARCAARCAADJPADJPADJP                                                                                                                                                                                                            201811202121252018112021212520181120212125    IP  IP  IP                                                G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                AO  AO  AO  ARGQARGQARGQQCPLQCPLQCPL                                                                                                                                                                                                            201811202121252018112021212520181120212125  QCP$QCP$QCP$                                                G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1F03E           103E            703E            AO  AO  AO  ARGQARGQARGQQCPLQCPLQCPL                                                                                                                                                                                                            201811202121252018112021212520181120212125  QCF$QCF$QCF$                                                G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�0               0               0               UW  UW  UW  ARSQARSQARSQUWQCUWQCUWQC            WOD & nearby Argo as visual check                               WOD & nearby Argo as visual check                               WOD & nearby Argo as visual check                               202009011536192020090115361920200901153619  IP  IP  IP                                                  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                