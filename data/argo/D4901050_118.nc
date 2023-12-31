CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS      N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AMOL   source        
Argo float     history       d2012-09-18T08:17:42Z creation; 2014-11-09T13:24:49Z updated; 2014-11-20T15:35:00Z converted from 2.2   
references        (http://www.argodatamgt.org/Documentation   comment              user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       profile       @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    6�   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  6�   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7$   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7d   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    7�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     8   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8(   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8H   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   axis      T      units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�E�vQ�   
_FillValue        A.�~            8L   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8T   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�E�vQ�   
_FillValue        A.�~            8X   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   axis      Y      units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�             8`   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      axis      X      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�             8h   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8p   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8t   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8|   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9|   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    9�   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    9�   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    9�   PRES         
      	   	long_name         )Sea water pressure, equals 0 at sea-level      
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    standard_name         sea_water_pressure     C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���        9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    9�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    standard_name         sea_water_pressure     C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���        9�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    9�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���        9�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      standard_name         sea_water_temperature      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o        9�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    9�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      standard_name         sea_water_temperature      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o        9�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    9�   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o        9�   PSAL         
      	   	long_name         Practical salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     standard_name         sea_water_salinity     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o        9�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    9�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     standard_name         sea_water_salinity     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o        9�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    9�   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o        9�   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  9�   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    9�   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    <�   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ?�   SCIENTIFIC_CALIB_DATE               	             
_FillValue               	long_name         Date of calibration    conventions       YYYYMMDDHHMISS        ,  B�   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    C$   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    C(   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    C,   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    C0   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  C4   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    Ct   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    C�   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    C�   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         C�   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         C�   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        C�   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    C�Argo profile    3.1 1.2 19500101000000  4901050 US ARGO PROJECT                                                 GREGORY C. JOHNSON                                              PRES            TEMP            PSAL               vA   AO  20120918081742  20141120153500  2998_89218_118                  2C  D   APEX                            4360                            122707                          846 @�^g�#~1   @�^e�2 @*1���l��cl��
=q1   ARGOS   Primary sampling: discrete [discrete spot sampling by SBE-41 at reported pressures]                                                                                                                                                                                F   F   F   �D��4   G�O�4   G�O�AָR4   G�O�4   G�O�A�9X4   G�O�4   G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - (FILTERED_SURFACE_PRESSURE) for PADJ                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            PSAL_ADJ corrects Cnd. Thermal Mass (CTM), Johnson et al,2007,JAOT & effects of pressure adjustments                                                                                                                                                            PADJ FILTERED_SURFACE_PRESSURE =-12.5 dbar                                                                                                                                                                                                                      none                                                                                                                                                                                                                                                            CTM alpha = 0.0267 & tau = 18.6 s with error equal to the correction                                                                                                                                                                                            Pressures adjusted using filtered surface pressure when warranted.                                                                                                                                                                                              none                                                                                                                                                                                                                                                            Salinity corrected for pressure sensor calibration drift and cell thermal mass                                                                                                                                                                                  20140909172538              20140909172538  AO  ARGQ                                                                        20120918081742  QCP$                G�O�G�O�G�O�FFBFE           AO  ARGQ                                                                        20120918081742  QCF$                G�O�G�O�G�O�40              PM  ARSQPADJV1.1                                                                20140909172538  QC  PRES            �D���D��G�O�                PM  ARSQCTM V1.1                                                                20140909172538  QC  PSAL            �D���D��G�O�                PM  ARSQSIQCV2.0WOD2001 & Argo                                                  20141109132223  IP                  G�O�G�O�G�O�                