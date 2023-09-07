CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS      N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2017-05-25T19:24:00Z creation      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.03   Conventions       Argo-3.0 CF-1.6    featureType       trajectoryProfile         :   	DATA_TYPE                  	long_name         	Data type      
_FillValue                    /8   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    /H   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    /L   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    /P   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    /`   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    /p   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                  8  /�   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                 �  /�   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                 �  1x   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  �  38   CYCLE_NUMBER               	long_name         Float cycle number     conventions       <0..N, 0 : launch cycle (if exists), 1 : first complete cycle   
_FillValue         ��        4   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    44   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    4<   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                  �  4L   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    5,   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    5H   PLATFORM_TYPE                     	long_name         Type of float      
_FillValue                  �  5P   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                  p  60   FIRMWARE_VERSION                  	long_name         Instrument version     
_FillValue                  p  6�   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    7   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
_FillValue        A.�~       axis      T         8  7,   JULD_QC                	long_name         Quality on Date and Time   conventions       Argo reference table 2     
_FillValue                    7d   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
_FillValue        A.�~          8  7l   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y         8  7�   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X         8  7�   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                  8  8   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8T   CONFIG_MISSION_NUMBER                  	long_name         'Float's mission number for each profile    conventions       @0..N, 0 : launch mission (if exists), 1 : first complete mission   
_FillValue         ��        ?T   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    ?p   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    ?x   PRES         
      
   	long_name         SEA PRESSURE   standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  ?�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  �  B<   PRES_ADJUSTED            
      	   	long_name         SEA PRESSURE   standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  B�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  �  E�   PRES_ADJUSTED_ERROR          
         	long_name         SEA PRESSURE   
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  FX   TEMP         
      	   	long_name         $SEA TEMPERATURE IN SITU ITS-90 SCALE   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  I   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  �  K�   TEMP_ADJUSTED            
      	   	long_name         $SEA TEMPERATURE IN SITU ITS-90 SCALE   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  L�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  �  O<   TEMP_ADJUSTED_ERROR          
         	long_name         $SEA TEMPERATURE IN SITU ITS-90 SCALE   
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  O�   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  R�   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    S�   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    a�   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    o�   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    
_FillValue                  �  }�   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ~L   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ~h   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ~�   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ~�   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                 �  ~�   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                  d  �|   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                  p  ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �l   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                  p  ��Argo profile    3.0 1.2 19500101000000  20170525192400  20170525192400  2900830 2900830 2900830 2900830 2900830 2900830 2900830 NAVY, Argo equivalent                                           NAVY, Argo equivalent                                           NAVY, Argo equivalent                                           NAVY, Argo equivalent                                           NAVY, Argo equivalent                                           NAVY, Argo equivalent                                           NAVY, Argo equivalent                                           DR. CHARLIE HORTON                                              DR. CHARLIE HORTON                                              DR. CHARLIE HORTON                                              DR. CHARLIE HORTON                                              DR. CHARLIE HORTON                                              DR. CHARLIE HORTON                                              DR. CHARLIE HORTON                                              PRES            TEMP            PRES            TEMP            PRES            TEMP            PRES            TEMP            PRES            TEMP            PRES            TEMP            PRES            TEMP               2   2   2   2   2   2   2AAAAAAA AOAOAOAOAOAOAO  3015                            3015                            3015                            3015                            3015                            3015                            3015                            2B  2B  2B  2B  2B  2B  2B  AAAAAAA APEX                            APEX                            APEX                            APEX                            APEX                            APEX                            APEX                            3290            3290            3290            3290            3290            3290            3290            042607          042607          042607          042607          042607          042607          042607          846 846 846 846 846 846 846 @���R�P�@���R�P�@���R�P�@���R�P�@���R�P�@���R�P�@���R�P�1111111 @���i!@���i!@���i!@���i!@���i!@���i!@���i!@2g+I�@2g+I�@2g+I�@2g+I�@2g+I�@2g+I�@2g+I��d���"���d���"���d���"���d���"���d���"���d���"���d���"��1111111 ARGOS   ARGOS   ARGOS   ARGOS   ARGOS   ARGOS   ARGOS   Bounce sampling: discrete []                                                                                                                                                                                                                                    Bounce sampling: discrete []                                                                                                                                                                                                                                    Bounce sampling: discrete []                                                                                                                                                                                                                                    Bounce sampling: discrete []                                                                                                                                                                                                                                    Bounce sampling: discrete []                                                                                                                                                                                                                                    Bounce sampling: discrete []                                                                                                                                                                                                                                    Bounce sampling: discrete []                                                                                                                                                                                                                                                         AAAAAAA AAAAAAA B��B333BF  BZ��BnffB�33B�ffB���B���B�ffB�  B�33B�ffB�ffBۙ�B�  C�3C��G�O�G�O�G�O�G�O�G�O�G�O�G�O�BffB2  BF  BZ��Bn  B���B�33B�  B���B���B�33B�ffBǙ�B�ffB���B�33C�3C�3G�O�G�O�G�O�G�O�G�O�G�O�G�O�B��B2ffBF  BZ  BnffB�ffB�ffB���B�ffB�ffB�  B���BǙ�B�33B�ffB�ffC��C� G�O�G�O�G�O�G�O�G�O�G�O�G�O�B  B2��BFffBY��BnffB�ffB�ffB�  B�33B�  B�  B���BǙ�B�  B���BC� C��C�fG�O�G�O�G�O�G�O�G�O�G�O�B33B133BE33BX��Bl��B�33B�ffB�  B�ffB���B�33B���B���B�33B�  B�ffC�3C�3G�O�G�O�G�O�G�O�G�O�G�O�G�O�B33B1��BFffBY33Bn��B�  B���B���B�  B�ffB���B���B�ffB�  B���B�ffC�3C�3G�O�G�O�G�O�G�O�G�O�G�O�G�O�@���A33Ah  A�  A���A陚B
  B��B333BFffBZ  Bm��B�33B�  B���B�33B���B�ffB���BǙ�B�ffBۙ�B�  C��C�3111111111111111111       111111111111111111       111111111111111111       1111111111111111111      111111111111111111       111111111111111111       1111111111111111111111111 B  B2ffBE33BZ  Bm��B���B�  B�34B�fgB�  B���B���B�  B�  B�34BC� C��G�O�G�O�G�O�G�O�G�O�G�O�G�O�B��B133BE33BZ  Bm33B�34B���B���B�34B�fgB���B�  B�34B�  B�fgB���C� C� G�O�G�O�G�O�G�O�G�O�G�O�G�O�B  B1��BE33BY33Bm��B�  B�  B�34B�  B�  B���B�34B�34B���B�  B�  C��CL�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B33B2  BE��BX��Bm��B�  B�  B���B���B���B���B�34B�34BЙ�B�fgB�34CL�CfgC�3G�O�G�O�G�O�G�O�G�O�G�O�BffB0ffBDffBX  Bl  B���B�  B���B�  B�fgB���B�fgB�fgB���Bڙ�B�  C� C� G�O�G�O�G�O�G�O�G�O�G�O�G�O�BffB0��BE��BXffBn  B���B�fgB�34B���B�  B�34B�34B�  BЙ�B�fgB�  C� C� G�O�G�O�G�O�G�O�G�O�G�O�G�O�@�34A  Ad��A�ffA�33A�  B	33B��B2ffBE��BY33Bl��B���B���B�34B���B�34B�  B�34B�34B�  B�34BCfgC� 111111111111111111       111111111111111111       111111111111111111       1111111111111111111      111111111111111111       111111111111111111       1111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A���A���A��A��A��A��A��`A��HA��HA�5?A�Q�A��AăA���A�
=A�`BA���A��G�O�G�O�G�O�G�O�G�O�G�O�G�O�A���A��A��A��A��A��`A��`A��mA��/A�hsA�XA�7LA�;dA�$�A�A���A�bA�%G�O�G�O�G�O�G�O�G�O�G�O�G�O�A��A�{A�oA�VA�1A�  A���A���A��A��A��A� �A�|�A�ƨAōPA��A�S�A�?}G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�+A�&�A��A��A��A�oA�bA�JA�VA�JA���A��/A�dZA���Aũ�A�
=A�oA���A��
G�O�G�O�G�O�G�O�G�O�G�O�A�\)A�1'A�+A�"�A��A��A�{A�oA�bA�{A�VA���A��#A�XAĉ7A���A�-A��yG�O�G�O�G�O�G�O�G�O�G�O�G�O�A�G�A�1'A�1'A�-A�-A�+A�&�A�"�A� �A��A�"�A�bA�ƨA�jA���A�ZA�%A�~�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A՗�AՓuAգ�A՟�AՑhAՕ�Aե�A�-A�+A�+A�+A�(�A��A��A�VA�JAԶFA�ȴA�t�A�A�A���A���A�VA�^5A��;111111111111111111       111111111111111111       111111111111111111       1111111111111111111      111111111111111111       111111111111111111       1111111111111111111111111 A���A���A��A��A��A��A��`A��HA��HA�5?A�Q�A��AăA���A�
=A�`BA���A��G�O�G�O�G�O�G�O�G�O�G�O�G�O�A���A��A��A��A��A��`A��`A��mA��/A�hsA�XA�7LA�;dA�$�A�A���A�bA�%G�O�G�O�G�O�G�O�G�O�G�O�G�O�A��A�{A�oA�VA�1A�  A���A���A��A��A��A� �A�|�A�ƨAōPA��A�S�A�?}G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�+A�&�A��A��A��A�oA�bA�JA�VA�JA���A��/A�dZA���Aũ�A�
=A�oA���A��
G�O�G�O�G�O�G�O�G�O�G�O�A�\)A�1'A�+A�"�A��A��A�{A�oA�bA�{A�VA���A��#A�XAĉ7A���A�-A��yG�O�G�O�G�O�G�O�G�O�G�O�G�O�A�G�A�1'A�1'A�-A�-A�+A�&�A�"�A� �A��A�"�A�bA�ƨA�jA���A�ZA�%A�~�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A՗�AՓuAգ�A՟�AՑhAՕ�Aե�A�-A�+A�+A�+A�(�A��A��A�VA�JAԶFA�ȴA�t�A�A�A���A���A�VA�^5A��;111111111111111111       111111111111111111       111111111111111111       1111111111111111111      111111111111111111       111111111111111111       1111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PRES            TEMP            PRES            TEMP            PRES            TEMP            PRES            TEMP            PRES            TEMP            PRES            TEMP            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            surface_pressure=0.20 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      surface_pressure=0.20 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      surface_pressure=0.20 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      surface_pressure=0.20 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      surface_pressure=0.20 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      surface_pressure=0.20 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      surface_pressure=0.20 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                      Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                      Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                      Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                      Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                      Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                      Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                      20170525192400              20170525192400              20170525192400              20170525192400              20170525192400              20170525192400              20170525192400              AO  AO  AO  AO  AO  AO  AO  ARCAARCAARCAARCAARCAARCAARCAADJPADJPADJPADJPADJPADJPADJP                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            20170525192400201705251924002017052519240020170525192400201705251924002017052519240020170525192400    IP  IP  IP  IP  IP  IP  IP                                                                                                                G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                AO  AO  AO  AO  AO  AO  AO  ARGQARGQARGQARGQARGQARGQARGQQCPLQCPLQCPLQCPLQCPLQCPLQCPL                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            20170525192400201705251924002017052519240020170525192400201705251924002017052519240020170525192400  QCP$QCP$QCP$QCP$QCP$QCP$QCP$                                                                                                                G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B03E            303E            303E            303E            103E            103E            103E            AO  AO  AO  AO  AO  AO  AO  ARGQARGQARGQARGQARGQARGQARGQQCPLQCPLQCPLQCPLQCPLQCPLQCPL                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            20170525192400201705251924002017052519240020170525192400201705251924002017052519240020170525192400  QCF$QCF$QCF$QCF$QCF$QCF$QCF$                                                                                                                G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�0               0               0               0               0               0               0               