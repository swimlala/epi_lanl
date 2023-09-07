CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS      N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2017-05-25T19:24:03Z creation      
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
_FillValue                  p  ��Argo profile    3.0 1.2 19500101000000  20170525192403  20170525192403  2900830 2900830 2900830 2900830 2900830 2900830 2900830 NAVY, Argo equivalent                                           NAVY, Argo equivalent                                           NAVY, Argo equivalent                                           NAVY, Argo equivalent                                           NAVY, Argo equivalent                                           NAVY, Argo equivalent                                           NAVY, Argo equivalent                                           DR. CHARLIE HORTON                                              DR. CHARLIE HORTON                                              DR. CHARLIE HORTON                                              DR. CHARLIE HORTON                                              DR. CHARLIE HORTON                                              DR. CHARLIE HORTON                                              DR. CHARLIE HORTON                                              PRES            TEMP            PRES            TEMP            PRES            TEMP            PRES            TEMP            PRES            TEMP            PRES            TEMP            PRES            TEMP               >   >   >   >   >   >   >AAAAAAA AOAOAOAOAOAOAO  3015                            3015                            3015                            3015                            3015                            3015                            3015                            2B  2B  2B  2B  2B  2B  2B  AAAAAAA APEX                            APEX                            APEX                            APEX                            APEX                            APEX                            APEX                            3290            3290            3290            3290            3290            3290            3290            042607          042607          042607          042607          042607          042607          042607          846 846 846 846 846 846 846 @��<Mpf@��<Mpf@��<Mpf@��<Mpf@��<Mpf@��<Mpf@��<Mpf1111111 @��Ř(�@��Ř(�@��Ř(�@��Ř(�@��Ř(�@��Ř(�@��Ř(�@3x�t�j@3x�t�j@3x�t�j@3x�t�j@3x�t�j@3x�t�j@3x�t�j�d�I�^5?�d�I�^5?�d�I�^5?�d�I�^5?�d�I�^5?�d�I�^5?�d�I�^5?1111111 ARGOS   ARGOS   ARGOS   ARGOS   ARGOS   ARGOS   ARGOS   Bounce sampling: discrete []                                                                                                                                                                                                                                    Bounce sampling: discrete []                                                                                                                                                                                                                                    Bounce sampling: discrete []                                                                                                                                                                                                                                    Bounce sampling: discrete []                                                                                                                                                                                                                                    Bounce sampling: discrete []                                                                                                                                                                                                                                    Bounce sampling: discrete []                                                                                                                                                                                                                                    Bounce sampling: discrete []                                                                                                                                                                                                                                                         AAAAAAA AAAAAAA B33B1��BF  B[33Bn  B���B�33B�33B�33B�33B���B�ffB�ffBљ�Bۙ�B�ffC�3C� G�O�G�O�G�O�G�O�G�O�G�O�G�O�BffB2  BF��BZ��Bm��B���B���B�ffB�33B���B�ffB�ffB�ffB�  B���B�ffC��C��G�O�G�O�G�O�G�O�G�O�G�O�G�O�B��B2ffBF  BZffBn  B���B�33B�33B�ffB�33B�33B�33B���B�33B�ffBC� C��G�O�G�O�G�O�G�O�G�O�G�O�G�O�B��B333BG33BZ  Bn��B���B�33B�33B�ffB�ffB�ffB�  B�ffB�ffB�33BC�3C�3C��G�O�G�O�G�O�G�O�G�O�G�O�B  B333BF��BZffBn��B�ffB�ffB�ffB���B���B�ffB���B�33B�ffB�ffBC��C�3G�O�G�O�G�O�G�O�G�O�G�O�G�O�B  B1��BF  BY��BnffB�ffB�ffB�ffB�ffB�  B�ffB���BǙ�B���Bۙ�BC�3C�3G�O�G�O�G�O�G�O�G�O�G�O�G�O�@�ffA  Ai��A�  Ař�A홚B	��B��B2ffBFffBZ  Bm��B���B�  B�ffB�  B�33B�  B���B�ffB�33Bۙ�BC��C��111111111111111111       111111111111111111       111111111111111111       1111111111111111111      111111111111111111       111111111111111111       1111111111111111111111111 B  B0fgBD��BZ  Bl��B�33B���B���B���B���B�  B���B���B�  B�  B���CffC33G�O�G�O�G�O�G�O�G�O�G�O�G�O�B33B0��BE��BY��BlfgB�  B�33B���B���B�  B���B���B���B�ffB�33B���C� C� G�O�G�O�G�O�G�O�G�O�G�O�G�O�BfgB133BD��BY33Bl��B�  B���B���B���B���B���B���B�33BЙ�B���B�  C33CL�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B��B2  BF  BX��Bm��B�  B���B���B���B���B���B�ffB���B���Bڙ�B�  CffCffC� G�O�G�O�G�O�G�O�G�O�G�O�B��B2  BE��BY33Bm��B���B���B���B�  B�  B���B�33Bƙ�B���B���B�  C� CffG�O�G�O�G�O�G�O�G�O�G�O�G�O�B��B0fgBD��BXfgBm33B���B���B���B���B�ffB���B�  B�  B�33B�  B�  CffCffG�O�G�O�G�O�G�O�G�O�G�O�G�O�@���A33Ad��A���A�34A�34BfgB��B133BE33BX��BlfgB�  B�ffB���B�ffB���B�ffB�  B���BЙ�B�  B�  CL�CL�111111111111111111       111111111111111111       111111111111111111       1111111111111111111      111111111111111111       111111111111111111       1111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A��A�VA�VA�JA�JA�ȴA�bNA�?}A�{A�A�A�
=A�ĜA�A�`BA���A��A�n�A�-G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�;dA�9XA�  Aӟ�A�z�A�VA�O�A�?}A��A�XA��A��/Ả7A���A�-A��/A�A���G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�ZA�VA��mAӗ�A�hsA�ffA�Q�A�-A�  A��AҾwA�v�A���A�"�A�t�A�1A�C�A�=qG�O�G�O�G�O�G�O�G�O�G�O�G�O�A�|�AԃA�7LA�{A��A���Aӗ�A�x�A�?}A��A��`Aҟ�A�l�A��A��AɁA�C�A��A��^G�O�G�O�G�O�G�O�G�O�G�O�Aԗ�AԓuAԝ�Aԡ�Aԇ+A�z�A�S�A��A�A���A�t�A�+AЧ�A�JA�(�A�1'A�
=A��G�O�G�O�G�O�G�O�G�O�G�O�G�O�Aԛ�Aԥ�Aԣ�Aԟ�A��A�jA�+A���A��
A���A��mA�A�ƨAΓuA��A�VA���A��
G�O�G�O�G�O�G�O�G�O�G�O�G�O�AԑhAԛ�Aԣ�Aԩ�Aԡ�AԮA԰!Aԝ�Aԕ�AԴ9Aԕ�A�%A�bNA�  A��A�A���A���A�$�AΟ�A�5?A�-A��
A���A�?}111111111111111111       111111111111111111       111111111111111111       1111111111111111111      111111111111111111       111111111111111111       1111111111111111111111111 A��A�VA�VA�JA�JA�ȴA�bNA�?}A�{A�A�A�
=A�ĜA�A�`BA���A��A�n�A�-G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�;dA�9XA�  Aӟ�A�z�A�VA�O�A�?}A��A�XA��A��/Ả7A���A�-A��/A�A���G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�ZA�VA��mAӗ�A�hsA�ffA�Q�A�-A�  A��AҾwA�v�A���A�"�A�t�A�1A�C�A�=qG�O�G�O�G�O�G�O�G�O�G�O�G�O�A�|�AԃA�7LA�{A��A���Aӗ�A�x�A�?}A��A��`Aҟ�A�l�A��A��AɁA�C�A��A��^G�O�G�O�G�O�G�O�G�O�G�O�Aԗ�AԓuAԝ�Aԡ�Aԇ+A�z�A�S�A��A�A���A�t�A�+AЧ�A�JA�(�A�1'A�
=A��G�O�G�O�G�O�G�O�G�O�G�O�G�O�Aԛ�Aԥ�Aԣ�Aԟ�A��A�jA�+A���A��
A���A��mA�A�ƨAΓuA��A�VA���A��
G�O�G�O�G�O�G�O�G�O�G�O�G�O�AԑhAԛ�Aԣ�Aԩ�Aԡ�AԮA԰!Aԝ�Aԕ�AԴ9Aԕ�A�%A�bNA�  A��A�A���A���A�$�AΟ�A�5?A�-A��
A���A�?}111111111111111111       111111111111111111       111111111111111111       1111111111111111111      111111111111111111       111111111111111111       1111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PRES            TEMP            PRES            TEMP            PRES            TEMP            PRES            TEMP            PRES            TEMP            PRES            TEMP            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            surface_pressure=0.30 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      surface_pressure=0.30 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      surface_pressure=0.30 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      surface_pressure=0.30 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      surface_pressure=0.30 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      surface_pressure=0.30 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      surface_pressure=0.30 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                      Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                      Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                      Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                      Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                      Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                      Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                      20170525192403              20170525192403              20170525192403              20170525192403              20170525192403              20170525192403              20170525192403              AO  AO  AO  AO  AO  AO  AO  ARCAARCAARCAARCAARCAARCAARCAADJPADJPADJPADJPADJPADJPADJP                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            20170525192403201705251924032017052519240320170525192403201705251924032017052519240320170525192403    IP  IP  IP  IP  IP  IP  IP                                                                                                                G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                AO  AO  AO  AO  AO  AO  AO  ARGQARGQARGQARGQARGQARGQARGQQCPLQCPLQCPLQCPLQCPLQCPLQCPL                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            20170525192403201705251924032017052519240320170525192403201705251924032017052519240320170525192403  QCP$QCP$QCP$QCP$QCP$QCP$QCP$                                                                                                                G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B03E            303E            303E            303E            103E            103E            103E            AO  AO  AO  AO  AO  AO  AO  ARGQARGQARGQARGQARGQARGQARGQQCPLQCPLQCPLQCPLQCPLQCPLQCPL                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            20170525192403201705251924032017052519240320170525192403201705251924032017052519240320170525192403  QCF$QCF$QCF$QCF$QCF$QCF$QCF$                                                                                                                G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�0               0               0               0               0               0               0               