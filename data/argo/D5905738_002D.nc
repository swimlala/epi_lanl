CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  f   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-07-24T22:02:27Z creation      
references        (http://www.argodatamgt.org/Documentation   comment           user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    6�   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  �  6�   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  �  7p   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  `  7�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        8P   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    8X   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    8\   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                  @  8`   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    8�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    8�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                  @  8�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                  @  8�   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                  @  9,   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    9l   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�E�vQ�   
_FillValue        A.�~       axis      T           9t   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    9�   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�E�vQ�   
_FillValue        A.�~            9�   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           9�   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           9�   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    9�   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    9�   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    9�   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        ;�   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    ;�   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    ;�   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    ;�   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        0  ;�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  G   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     0  I�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  U   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     0  W�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     0  c   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  n8   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     0  q   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  |4   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     0      PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     0  �0   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �`   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     0  �,   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �\   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     0  �(   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  �  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �`   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �|   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                     ׄ   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         פ   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ׬   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ״   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                     ׼   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  �X   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �    SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �    SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �    	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  �       �  � Argo profile    3.1 1.2 19500101000000  20180724220227  20210722160147  5905738 5905738 US ARGO PROJECT                                                 US ARGO PROJECT                                                 GREGORY C. JOHNSON                                              GREGORY C. JOHNSON                                              PRES            TEMP            PSAL            PRES            TEMP            PSAL                  DD  AOAO7218                            7218                            2C  2C  DD  SOLO_D_MRV                      SOLO_D_MRV                      12001                           12001                           SBE603 01Dec16                  SBE603 01Dec16                  874 874 @�b���@�b���11  @�b��}G�@�b��}G�@6�;dZ�@6�;dZ��c�P%���c�P%��11  GPS     GPS     Primary sampling: averaged [1Hz sampling by SBE-41CP averaged in 2-dbar bins]                                                                                                                                                                                   Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 AB  AA  AA  >���?fff@   @@  @�33@�33@���@�  A��AffA$��AC33Ac33A~ffA�33A�33A�  A�  A�  A�  A�  B ffBffB  B  B ffB(ffB0ffB8ffB@ffBHffBPffBX  B`  Bh  BpffBxffB�ffB�ffB�ffB�ffB�33B�ffB�33B�33B�33B�  B�33B�33B�  B�  B�33B�33B�33B�  B���B�33B�ffB�33B�33B�33B�  B�  B�33B�33B�ffB�33B�  B�33C �C�C  C�fC  C
  C  C�fC  C  C  C�fC  C�C�C�C   C"  C#�fC&�C(�C*  C,�C.33C0�C1�fC4  C6�C8  C9�fC<  C>33C@�CB�CD  CE�fCG�fCJ  CL  CN  CP  CR�CT�CV  CX  CZ  C\  C^�C`  Cb�Cd�Cf  Ch  Ci�fCl  Cn  Co�fCr  Ct�Cv  Cx  Cz  C|  C}�fC�  C��C��C��3C�  C��C�  C�  C��C��C��C��C��C�  C��C��C��C�  C�  C��C�  C��C��C�  C��C��C��C�  C��C��C�  C��C�  C�  C�  C��C��C��C�  C�  C�  C�  C��3C��C�  C�  C��C�  C�  C��C��C��C��C��C�  C��3C�  C��C��C�  C��C��C�  C�  C��C��C�  C��C��C��C��C�  C�  C��C��C��C��C��C��C�  C�  C�  C��C��C��C�  C�  C�  C��C��C�  C��C�  C�  C��C��C�  C��C�  C��C��C�  C��C��C��3C�  C��C�  C�  C��C��C��C��C��C��C�  C�  C��C��C��C��C��C��fC��3C�  D ` D� D�fDٚDfD9�DffDy�D	��D
� D��D�3D  D33DY�D� DٚD3DL�Dy�D��D�3D�fD��D��D��D Y�D!33D"&fD$�D%  D&  D(,�D)S3D*� D+��D,� D.3D/L�D0� D1��D2��D4  D59�D6Y�D7s3D9��D:��D;��D<s3D>L�D?�D@�fDA� DCS3DD  DE�3DF� DHy�DI` DJS3DLL�DML�DNFfDOY�DQs3DRl�DSffDT` DVL�DW&fDX�DY� DZ��D\3D\�fD^33D_�3D`��DbFfDb��Dd` De��Dg@ Dg��Dis3Dj� DlFfDl�fDnL�Do� Dp�fDr33Dsy�Dt�3DvfDwS3Dx�3Dz3Dz��>���>���>���>���>���>���>���>���>���>���>���?   >���>���>���>���>���?��?��?333?L��?fff?���?���?�33?���?ٙ�@   @33@��@333@@  @L��@l��@y��@�ff@�  @���@���@�33@���@�ff@�33@�  @�  @���A��A33A��A��A   A(  A.ffA4��A<��AD��AI��AP  AX  A`  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111144441444114414114111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                          ?fff?�33@   @`  @�33@�33@���@�  A	��AffA,��AK33Ak33A�33A�33A�33A�  A�  A�  A�  A�  BffB
ffB  B  B"ffB*ffB2ffB:ffBBffBJffBRffBZ  Bb  Bj  BrffBzffB�ffB�ffB�ffB�ffB�33B�ffB�33B�33B�33B�  B�33B�33B�  B�  B�33B�33B�33B�  B���B�33B�ffB�33B�33B�33B�  B�  B�33B�33B�ffB�33B�  B�33C ��C��C� CffC� C
� C� CffC� C� C� CffC� C��C��C��C � C"� C$ffC&��C(��C*� C,��C.�3C0��C2ffC4� C6��C8� C:ffC<� C>�3C@��CB��CD� CFffCHffCJ� CL� CN� CP� CR��CT��CV� CX� CZ� C\� C^��C`� Cb��Cd��Cf� Ch� CjffCl� Cn� CpffCr� Ct��Cv� Cx� Cz� C|� C~ffC�@ C�L�C�L�C�33C�@ C�Y�C�@ C�@ C�L�C�L�C�L�C�L�C�Y�C�@ C�L�C�Y�C�L�C�@ C�@ C�L�C�@ C�L�C�Y�C�@ C�L�C�L�C�L�C�@ C�L�C�L�C�@ C�L�C�@ C�@ C�@ C�L�C�Y�C�L�C�@ C�@ C�@ C�@ C�33C�L�C�@ C�@ C�L�C�@ C�@ C�L�C�L�C�L�C�L�C�L�C�@ C�33C�@ C�L�C�L�C�@ C�L�C�Y�C�@ C�@ C�L�C�L�C�@ C�L�C�L�C�L�C�L�C�@ C�@ C�L�C�Y�C�Y�C�Y�C�L�C�L�C�@ C�@ C�@ C�L�C�Y�C�L�C�@ C�@ C�@ C�L�C�L�C�@ C�L�C�@ C�@ C�L�C�L�C�@ C�L�C�@ C�L�C�L�C�@ C�L�C�L�C�33C�@ C�L�C�@ C�@ C�L�C�L�C�L�C�L�C�L�C�L�C�@ C�@ C�L�C�L�C�L�C�Y�C�L�C�&fC�33C�@ D � D� D�fD��D&fDY�D�fD��D	��D
� DٚD3D  DS3Dy�D� D��D33Dl�D��D��D�3D�fD��D��D��D y�D!S3D"FfD$,�D%  D&  D(L�D)s3D*� D+��D-  D.33D/l�D0� D1ٚD3�D4@ D5Y�D6y�D7�3D9��D:��D;��D<�3D>l�D?9�D@�fDA� DCs3DD@ DE�3DF� DH��DI� DJs3DLl�DMl�DNffDOy�DQ�3DR��DS�fDT� DVl�DWFfDX,�DY� DZ��D\33D\�fD^S3D_�3Da�DbffDc�Dd� De��Dg` Dh�Di�3Dk  DlffDmfDnl�Do� DqfDrS3Ds��Dt�3Dv&fDws3Dx�3Dz33Dz��?L��G�O�G�O�G�O�G�O�?L��G�O�G�O�G�O�?L��?fffG�O�G�O�?L��G�O�?L��?fffG�O�?���?���?�ff?�33?���?ٙ�?�33@ff@��@   @333@9��@S33@`  @l��@�ff@���@�ff@�  @���@���@�33@���@�ff@�33@�  A   A��A��A33A��A!��A(  A0  A6ffA<��AD��AL��AQ��AX  A`  Ah  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111144441444114414114111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                          @ �@ %@ �@ {@ �@ "�@ (G@ /�@ 7L@ <�@ FQ@ SI@ `�@ l�@ z3@ ��@ �0@ ��@ �~@ �&@ ��@ �#@ ��@ ��@j@�@g@-@:�@H]@V@c�@p�@~K@��@�H@��@�F@��@є@�;@�4@��@�@*@"�@/�@>@K�@X�@ff@t�@�d@�@�@��@�@�W@�O@��@�@��@
=@�@&;@4�@A�@N�@\�@j@x&@�@�@�m@�@��@ȴ@�
@�@�Y@�Q@�@�@)�@7L@DD@Q�@^�@m�@{�@��@��@�5@�-@�w@��@�#@�@�@j@o@g@-@:@F�@T�@b�@p�@~K@��@�H@��@��@@�7@��@�4@�,@�@*@""@/�@<�@K@X�@e�@t@�d@�\@�@��@�R@�J@Ӡ@��@�@��@
=@B@%�@33@A�@O0@\�@j@x�@�@�u@��@��@��@�c@׹@�@�@^@�@�@)�@7L@DD@R�@`B@m:@{�@��@�0@��@�-@��@�|@�t@�@��@	j@	b@	g@	,`@	:@	H]@	UU@	b�@	qS@	~�@	��@	�H@	��@	��@	��@	�7@	ލ@	�4@	�,@
�@
�@
""@
/�@
>@
K�@
X�@
g@
t�@
�d@
�@
�@
��@
�@
�W@
��@
�@
�@
�E@
=@�@%�@3�@B8@O0@\)@i�@ww@��@�u@�m@��@��@�c@׹@�`@�Y@ �@�@�@)�@6�@D�@R�@^�@m:@{�@��@�0@��@�-@��@�|@�#@��@��@j@�@g@-@;d@H]@S�@bN@p�@�~@�7@�@b@0x@Q=@qS@��@�@�c@�m@$.@@�@a�@�@��@�J@�@1@(G@F�@e	@�d@��@є@�y@�@1�@K�@�@��@��@�L@�@/�@O�@p�@�h@��@Ӡ@�@�@6�@T�@s_@�h@�@�@^@�@Lu@bN@�@�A@խ@�@�@/�@bN@z�@��@��@�@ �@
@Wb@r@��@�A@��@�@�@:@O�@y�@��@��@�@��@"�@5�@\)@�@��@�w@�@�@3�@D�@k.@�\@�-@խ@�~@�@>@a�@�+@��@�@ G�O�G�O�G�O�G�O�@ G�O�G�O�G�O�@ @ �G�O�G�O�@ G�O�@ @ �G�O�@ @ �@ v@ %@ �@ 1@ 	�@ 
�@ �@ �@ �@ b@ @ {@ �@ B@ �@ �@ �@ !s@ $.@ &;@ (G@ *S@ -@ /�@ 33@ 5?@ 8�@ ;d@ >@ A�@ DD@ G�@ Ji@ M$@ P�@ S�@ V@ X�@ \)@ _�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�Aȏ\Aȕ�AȋDA�n�A�7LA���AǼjAǃA�O�A�7LA�$�A�VA���A��A��#A���A�ȴAƼjAƲ-AƝ�AƁA�S�AÍPA� �A�A��A´9A\A�`BA�=qA���A�(�A��yA���A�r�A�bNA�VA��TA���A�O�A��DA��A��A�A�ZA�?}A���A��+A��^A�K�A���A��mA��;A���A�$�A�^5A���A��-A�/A��\A�
=A�~�A��A�7LA�33A�$�A�"�A�A�M�A��A�1A��RA��7A�jA�bNA�JA���A��;A���A��hA�XA��9A��PA�(�A��`A���A�z�A�;dA��wA�ZA��A���A��-A��TA���A�=qA� �A�Q�A�G�A��HA�?}A�z�A� �A��A�ffA��A��DA�;dA�  A�ƨA��-A�ĜA�7LA��!A���A�jA���A���A��A���A�t�A�ffA�XA���A���A���A�E�A�$�A���A~5?AzȴAw\)Au��AsoApn�AnQ�Al1AkK�AkAjĜAjbAioAg�mAd�DAd�+AdI�Ac"�AbĜAb1'A^��A\�!A[�wAY�ATĜAOO�AM��AMK�ALI�AJ1'AHI�AGx�AF^5AE�hAD��ACG�AB��AB�A@VA>��A="�A<A�A;�
A:�RA9�hA9A8�A6�HA6=qA4�/A2�!A0�uA/\)A-ƨA,=qA+VA*A(�`A(bNA'�A&�9A$�`A#��A#dZA"n�A!x�A bNA�yA��A=qA\)AƨA�TA/Az�A�A��A��A�A%AȴAZA�AA�PA�A1AXA��A  A�uA��Az�AE�A��A�A��A��A�A��A33A
��A�A&�A��A�+Ar�A��A�A��A�uA�FA�/AQ�A��AC�Az�A�FA �R@�ƨ@��9@��-@�J@�~�@�
=@���@��;@@�-@�{@��T@���@ܴ9@�{@�%@���@���@���@��@��@őh@�"�@��`@�o@���@��T@�dZ@�33@�%@�v�@��P@�=q@��u@�b@�r�@���@�1'@��@��
@��R@���@��@���@��`@���@�I�@���@�ƨ@�@�l�@��@�G�@���@�=q@�~�@�x�@�-@�r�@�O�@�@�V@���@��h@���@��7@�1@��;@�o@���@�^5@��^@��-@�p�@�7L@�Q�@��F@���@�\)@�ȴ@�v�@�V@�{@�7L@��u@��@�"�@�-@�@�?}@�V@�r�@�  @���@��P@�;d@��@�E�@�J@��7@�hs@�?}@��@��;@�
=@��\@��T@�V@��/@��jAȗ�Aȉ7AȑhAȍPAȋDAȉ7Aȇ+AȋDAȍPAȏ\Aȇ+Aȏ\Aȕ�Aȗ�AȓuAȕ�Aȕ�Aȕ�AȓuAȓuAȓuAȗ�Aȗ�Aȗ�AȓuAȑhAȋDAȇ+Aȉ7Aȉ7A�v�A�l�A�ffA�Q�A�C�A�+A��A�%A��`A���AǼjAǧ�AǗ�AǁA�p�A�XA�E�A�;dA�1'A�-A�$�A� �A��A��A�oA�JA�
=A�%A�  A���G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                          Aȏ\Aȕ�AȋDA�n�A�7LA���AǼjAǃA�O�A�7LA�$�A�VA���A��A��#A���A�ȴAƼjAƲ-AƝ�AƁA�S�AÍPA� �A�A��A´9A\A�`BA�=qA���A�(�A��yA���A�r�A�bNA�VA��TA���A�O�A��DA��A��A�A�ZA�?}A���A��+A��^A�K�A���A��mA��;A���A�$�A�^5A���A��-A�/A��\A�
=A�~�A��A�7LA�33A�$�A�"�A�A�M�A��A�1A��RA��7A�jA�bNA�JA���A��;A���A��hA�XA��9A��PA�(�A��`A���A�z�A�;dA��wA�ZA��A���A��-A��TA���A�=qA� �A�Q�A�G�A��HA�?}A�z�A� �A��A�ffA��A��DA�;dA�  A�ƨA��-A�ĜA�7LA��!A���A�jA���A���A��A���A�t�A�ffA�XA���A���A���A�E�A�$�A���A~5?AzȴAw\)Au��AsoApn�AnQ�Al1AkK�AkAjĜAjbAioAg�mAd�DAd�+AdI�Ac"�AbĜAb1'A^��A\�!A[�wAY�ATĜAOO�AM��AMK�ALI�AJ1'AHI�AGx�AF^5AE�hAD��ACG�AB��AB�A@VA>��A="�A<A�A;�
A:�RA9�hA9A8�A6�HA6=qA4�/A2�!A0�uA/\)A-ƨA,=qA+VA*A(�`A(bNA'�A&�9A$�`A#��A#dZA"n�A!x�A bNA�yA��A=qA\)AƨA�TA/Az�A�A��A��A�A%AȴAZA�AA�PA�A1AXA��A  A�uA��Az�AE�A��A�A��A��A�A��A33A
��A�A&�A��A�+Ar�A��A�A��A�uA�FA�/AQ�A��AC�Az�A�FA �R@�ƨ@��9@��-@�J@�~�@�
=@���@��;@@�-@�{@��T@���@ܴ9@�{@�%@���@���@���@��@��@őh@�"�@��`@�o@���@��T@�dZ@�33@�%@�v�@��P@�=q@��u@�b@�r�@���@�1'@��@��
@��R@���@��@���@��`@���@�I�@���@�ƨ@�@�l�@��@�G�@���@�=q@�~�@�x�@�-@�r�@�O�@�@�V@���@��h@���@��7@�1@��;@�o@���@�^5@��^@��-@�p�@�7L@�Q�@��F@���@�\)@�ȴ@�v�@�V@�{@�7L@��u@��@�"�@�-@�@�?}@�V@�r�@�  @���@��P@�;d@��@�E�@�J@��7@�hs@�?}@��@��;@�
=@��\@��T@�V@��/@��jAȗ�Aȉ7AȑhAȍPAȋDAȉ7Aȇ+AȋDAȍPAȏ\Aȇ+Aȏ\Aȕ�Aȗ�AȓuAȕ�Aȕ�Aȕ�AȓuAȓuAȓuAȗ�Aȗ�Aȗ�AȓuAȑhAȋDAȇ+Aȉ7Aȉ7A�v�A�l�A�ffA�Q�A�C�A�+A��A�%A��`A���AǼjAǧ�AǗ�AǁA�p�A�XA�E�A�;dA�1'A�-A�$�A� �A��A��A�oA�JA�
=A�%A�  A���G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                          ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
�XB
�XB
�XB
�LB
�LB
�FB
�?B
�9B
�9B
�9B
�3B
�3B
�3B
�3B
�3B
�9B
�9B
�9B
�?B
�?B
�9B
��B
��BoB�B%�B0!B:^BK�BS�BS�BQ�BP�BiyB�+B�JB�uB��B��B�B�B�B'�B-B33BG�Bk�Bp�Bt�By�B|�B�%B�B�1B�JB�B�B�\B�uB�hB�JB�VB��B��B�B�3B�RB�XB�LB��B�B�B�B�-B�B��B�bB�B�%B� Bs�Bm�BaHB`BB^5BXBR�BK�BQ�B'�B�wB��B�hB��B�LBv�B�B
�)B
�)B
��B9XB5?B1'B.B.BR�Bt�B�+B�DB�JBy�Bm�Be`B_;B]/BYBK�B=qB�B	7B%BB
�B
��B
�LB
��B
��B
q�B
l�B
Q�B
7LB
�B

=B	��B	�BB	��B	��B	ǮB	B	��B	�qB	�B	��B	�+B	�DB	�PB	�7B	�hB	��B	�bB	�B	�B	q�B	XB	>wB	5?B	0!B	-B	$�B	�B	\B		7B	B��B��B��B�B�B�`B�5B�B�B��B��B��BȴBĜB�}B�XB�!B��B��B��B��B��B��B�PB�JB�7B�=B�+B�+B�1B�1B�1B�+B�B�B�B�Bz�Bn�Bl�BjBk�Bk�Bk�BgmBffBdZBcTBaHBW
BVBT�BP�BN�BM�BN�BbNBr�Bs�Bs�Bs�Bo�Bq�Bo�Bn�Bl�Bo�Bn�BcTBW
B[#B[#BaHBiyBl�Bo�Bo�Bn�Bn�Bn�Bm�Bo�Bp�Bq�Bp�Bo�BdZB`BBe`BiyBcTBS�BL�BR�BW
BT�BL�BD�B=qB>wB;dBB�BE�BE�BI�BM�BS�BW
BYB]/B^5BaHBdZBgmBl�Bs�Bv�B}�B�7B�VB�JB��B��B��B�9B�^BĜB��B��B��B�BB�ZB�NB�HB�HB�mB�NB�`B�B��B��B	\B	�B	%�B	8RB	>wB	B�B	K�B	M�B	Q�B	VB	n�B	r�B	x�B	}�B	�B	�=B	�DB	�VB	�\B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�'B	�-B	�FB	�XB	��B	B	ÖB	ƨB	ȴB	��B	��B	��B	��B	��B	��B	��B	�B	�B	�
B	�#B	�5B	�;B	�HB	�ZB	�ZB	�`B
�RB
�jB
�XB
�RB
�XB
�RB
�^B
�XB
�^B
�LB
�dB
�RB
�^B
�RB
�^B
�RB
�RB
�XB
�XB
�XB
�XB
�XB
�XB
�^B
�XB
�RB
�XB
�XB
�XB
�XB
�LB
�LB
�RB
�RB
�9B
�LB
�XB
�?B
�FB
�?B
�?B
�9B
�9B
�FB
�3B
�3B
�9B
�3B
�9B
�-B
�3B
�3B
�3B
�3B
�-B
�3B
�3B
�3B
�3B
�3G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                          B
�RB
�RB
�RB
�FB
�FB
�?B
�9B
�3B
�3B
�3B
�-B
�-B
�-B
�-B
�-B
�3B
�3B
�3B
�9B
�9B
�3B
�}B
��BhB�B$�B/B9XBJ�BR�BR�BP�BO�BhsB�%B�DB�oB��B��B��B�B�B&�B,B2-BF�BjBo�Bs�Bx�B{�B�B�B�+B�DB�B�B�VB�oB�bB�DB�PB��B��B�B�-B�LB�RB�FB��B�B�B�B�'B�B��B�\B�B�B~�Br�Bl�B`BB_;B]/BW
BQ�BJ�BP�B&�B�qB��B�bB��B�FBu�B{B
�#B
�#B
��B8RB49B0!B-B-BQ�Bs�B�%B�=B�DBx�Bl�BdZB^5B\)BXBJ�B<jB�B1BBB
�B
��B
�FB
��B
��B
p�B
k�B
P�B
6FB
�B
	7B	��B	�;B	��B	��B	ƨB	��B	��B	�jB	�B	��B	�%B	�=B	�JB	�1B	�bB	��B	�\B	�B	� B	p�B	W
B	=qB	49B	/B	,B	#�B	�B	VB	1B	B��B��B��B�B�B�ZB�/B�B�
B��B��B��BǮBÖB�wB�RB�B��B��B��B��B��B��B�JB�DB�1B�7B�%B�%B�+B�+B�+B�%B�B�B�B�By�Bm�Bk�BiyBjBjBjBffBe`BcTBbNB`BBVBT�BS�BO�BM�BL�BM�BaHBq�Br�Br�Br�Bn�Bp�Bn�Bm�Bk�Bn�Bm�BbNBVBZBZB`BBhsBk�Bn�Bn�Bm�Bm�Bm�Bl�Bn�Bo�Bp�Bo�Bn�BcTB_;BdZBhsBbNBR�BK�BQ�BVBS�BK�BC�B<jB=qB:^BA�BD�BD�BH�BL�BR�BW
BYB]/B^5BaHBdZBgmBl�Bs�Bv�B}�B�7B�VB�JB��B��B��B�9B�^BĜB��B��B��B�BB�ZB�NB�HB�HB�mB�NB�`B�B��B��B	\B	�B	%�B	8RB	>wB	B�B	K�B	M�B	Q�B	VB	n�B	r�B	x�B	}�B	�B	�=B	�DB	�VB	�\B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�'B	�-B	�FB	�XB	��B	B	ÖB	ƨB	ȴB	��B	��B	��B	��B	��B	��B	��B	�B	�B	�
B	�#B	�5B	�;B	�HB	�ZB	�ZB	�`B
�LB
�dB
�RB
�LB
�RB
�LB
�XB
�RB
�XB
�FB
�^B
�LB
�XB
�LB
�XB
�LB
�LB
�RB
�RB
�RB
�RB
�RB
�RB
�XB
�RB
�LB
�RB
�RB
�RB
�RB
�FB
�FB
�LB
�LB
�3B
�FB
�RB
�9B
�?B
�9B
�9B
�3B
�3B
�?B
�-B
�-B
�3B
�-B
�3B
�'B
�-B
�-B
�-B
�-B
�'B
�-B
�-B
�-B
�-B
�-G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                          <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�201807242202272021061413515420210614135154202106141746102021061417461020210614174610201807242202272021061413515420210614135154202106141746102021061417461020210614174610PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            new conductivity = original conductivity * (1 + delta*TEMP + CPcor_SBE*PRES) / (1 + delta*TEMP_ADJUSTED + CPcor_new*PRES_ADJUSTED)                                                                                                                              none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS                                                                                                                                                                                                                                       PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            new conductivity = original conductivity * (1 + delta*TEMP + CPcor_SBE*PRES) / (1 + delta*TEMP_ADJUSTED + CPcor_new*PRES_ADJUSTED)                                                                                                                              none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS                                                                                                                                                                                                                                       surface_pressure=-0.50 dbar                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            CPcor_new = -1.25e-07; CPcor_SBE = -9.57e-08; delta = 3.25e-06                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            OW r = 1 (+/-0), vertically averaged dS = -0.001 (+/-0)                                                                                                                                                                                                         surface_pressure=-0.50 dbar                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            CPcor_new = -1.25e-07; CPcor_SBE = -9.57e-08; delta = 3.25e-06                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            OW r = 1 (+/-0), vertically averaged dS = -0.001 (+/-0)                                                                                                                                                                                                         Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                      none                                                                                                                                                                                                                                                            New conductivity computed by using a different CPcor value from that provided by Sea-Bird.                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Significant salinity sensor drift detected. OW least squares fit adopted.                                                                                                                                                                                       Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                      none                                                                                                                                                                                                                                                            New conductivity computed by using a different CPcor value from that provided by Sea-Bird.                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Significant salinity sensor drift detected. OW least squares fit adopted.                                                                                                                                                                                       PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            AO  AO  ARCAARCAADJPADJP                                                                                                                                        2018072422022720180724220227  IP  IP                                G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2018072422022720180724220227QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�5F03E           703E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2018072422022720180724220227QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�0               0               PM  PM  ARSQARSQCOWGCOWGV1.1V1.1                                                                                                                                2021072216014720210722160147IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                