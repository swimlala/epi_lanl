CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS     N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-08-27T22:01:31Z creation      
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
resolution        =���   axis      Z        X  ;�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   L8   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     X  PP   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   `�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     X  d�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     X  u   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   �p   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     X  ��   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     X  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     X  �P   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     X  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   �   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     X  �0   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                   �   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                   �   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                       HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                      HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  �    HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                   �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                   �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar        �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar        �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�       �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  �   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �0   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �0   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    0   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  � 0      � 0Argo profile    3.1 1.2 19500101000000  20180827220131  20210722160152  5905738 5905738 US ARGO PROJECT                                                 US ARGO PROJECT                                                 GREGORY C. JOHNSON                                              GREGORY C. JOHNSON                                              PRES            TEMP            PSAL            PRES            TEMP            PSAL                  DD  AOAO7218                            7218                            2C  2C  DD  SOLO_D_MRV                      SOLO_D_MRV                      12001                           12001                           SBE603 01Dec16                  SBE603 01Dec16                  874 874 @�yM��@�yM��11  @�yM����@�yM����@6Di�4.�@6Di�4.��c����<`�c����<`11  GPS     GPS     Primary sampling: averaged [1Hz sampling by SBE-41CP averaged in 2-dbar bins]                                                                                                                                                                                   Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                              	   	AB  AA  AA  >���?�  @   @Fff@�33@�ff@�ff@�ffA   A  A$��AA��Aa��A�  A�  A�  A�33A���A���A�  A�  A�33B  B��B��B ffB(ffB0  B8  B@  BH��BPffBXffB`  Bh  BpffBxffB�33B�33B�  B�ffB�  B�  B�33B�ffB�33B�  B�33B�  B�  B�  B�  B�ffB�33B�33BǙ�B�  B�ffB���B�33B�ffB���B���B�  B�33B�33B�ffB�ffB���B���C��C��C33C33C
  CL�C33C�CffC33C�CL�CL�C�CL�C 33C"�C$33C&�C(�C*�C,�C.  C0�C2�C433C633C8L�C:  C<�C>L�C@  CB  CD�CE��CG��CI�fCL  CN�CP�CR�CT�CV33CX33CZ�C\�C^�C`  CbL�Cd33Cf  Ch  Ci��Cl  Cn33Cp33Cr  CtL�Cv�Cw�fCz�C|  C}�fC��C�  C��fC�  C��3C��fC��C��C�  C�  C��3C��C�&fC��fC��fC��C��C��C��C�  C��C�  C��C��C��C��C�  C�  C��C�ٚC��3C�  C��C�&fC��3C��3C�  C�  C��C��C��C��3C�  C��C��C��C��C�&fC�  C��C��C��fC��3C�  C��C��C��C�&fC��C��C��fC��fC��fC��fC��C��C��C�&fC��C��C��C��C��fC��3C��fC��fC��3C��3C��3C��3C��3C��3C��C��C��C�  C��3C�&fC��C��C�  C��3C�  C��C�  C��fC�  C��C��C��fC�  C��C��C�&fC�  C�ٚC��3C��3C�  C��C��C��C��C��C��C�  C��3C��3C��C�&fC��C��C��fC�  C�33DٚD9�D�fD
  D��D9�D��DFfD��D�fDy�D9�D!�3D$�3D's3D*@ D,��D/�3D2y�D5  D7��D:s3D=fD?��DB@ DDٚDGl�DI�3DL��DO33DQٚDTs3DW�DY�fD\s3D_3Da��Dd33DfٚDis3Dl�Dn��DqY�Ds��Dv��Dy9�D{Y�D}��D�&fD�ffD���D���D�3D�C3D�y�D���D�� D��D�VfD��3D��fD�  D�i�D�� D��3D�6fD�y�D�� D�fD�FfD�� D���D��3D�,�D�c3D���D��fD� D�C3D�vfD�� D��fD�#3D�\�D��fD��3D��D�c3D��fD��3D�I�D���D��fD�33D��fD���D�6fD�� D��fD�<�D���D��3D��D�i�Dư D��fD�9�D�vfD˩�D��3D���D�  D�FfD�i�D҆fDӜ�D԰ Dռ�D��fD��3D�� D�� D���D� D�  D�6fD�P D�c3D�fD� D��3D�  D�&fD�S3D�3D��D���D�6fD�y�D��3D�  D�FfD�fD��fD��D�I�D���D���D�fD�<�D���D��fD�  E 4�E ٚEx EfE��E\�E�fE� E>fE��Ex EfE��ENfE�E	�3E
 E
�fE>fE�3E�3EfE6fE� E� E� E� E�fE�3E&fE)�E�3E��EfE E��E ��E"9�E#FfE$Q�E%�3E&�E(y�E)�fE*� E,8 E-P E.` E0�E1�E2fE3��E4�3E5� E7S3E8T�E9� E:�3E<[3E?^fEBk3EE��EH�fEK� EN�fER�EU$�EX6fE[��E^h Ea� Ed�fEg�fEk�EnfEq\�Et�fEwc3Ez~fE}�fE�i�E�
fE�� E�3E���E�D�E���E�a�E��E�u�E� E�\�E��fE��3E�@ E��fE���E�6fE��fE���E��E�t�E�� E�#3E�_3E�� E��E�h E��3E�� E�S3E��f>���>���>���>���>���>���>���?   ?   ?   ?333?��?333?fff?�  ?���?���?���?�33?ٙ�?�33@ff@��@&ff@9��@L��@`  @s33@�33@�  @���@�33@�  @���@�ff@�  @���@���@���A��A	��A  AffAffA$��A,��A1��A;33AA��AI��AP  AVffA^ffAd��Al��At��A{33A���A���A���G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111414441144141111141111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 ?fff?�  @   @fff@�33@�ff@�ff@�ffA  A  A,��AI��Ai��A�  A�  A�  A�33A���A���A�  A�  B��B
  B��B��B"ffB*ffB2  B:  BB  BJ��BRffBZffBb  Bj  BrffBzffB�33B�33B�  B�ffB�  B�  B�33B�ffB�33B�  B�33B�  B�  B�  B�  B�ffB�33B�33Bș�B�  B�ffB���B�33B�ffB���B���B�  B�33B�33B�ffB�ffB���C ffCL�CL�C�3C�3C
� C��C�3C��C�fC�3C��C��C��C��C��C �3C"��C$�3C&��C(��C*��C,��C.� C0��C2��C4�3C6�3C8��C:� C<��C>��C@� CB� CD��CFL�CHL�CJffCL� CN��CP��CR��CT��CV�3CX�3CZ��C\��C^��C`� Cb��Cd�3Cf� Ch� CjL�Cl� Cn�3Cp�3Cr� Ct��Cv��CxffCz��C|� C~ffC�Y�C�@ C�&fC�@ C�33C�&fC�Y�C�Y�C�@ C�@ C�33C�Y�C�ffC�&fC�&fC�L�C�Y�C�L�C�L�C�@ C�L�C�@ C�L�C�L�C�L�C�L�C�@ C�@ C�L�C��C�33C�@ C�Y�C�ffC�33C�33C�@ C�@ C�L�C�Y�C�Y�C�33C�@ C�L�C�Y�C�Y�C�Y�C�ffC�@ C�L�C�L�C�&fC�33C�@ C�L�C�Y�C�Y�C�ffC�Y�C�Y�C�&fC�&fC�&fC�&fC�Y�C�Y�C�Y�C�ffC�Y�C�Y�C�Y�C�Y�C�&fC�33C�&fC�&fC�33C�33C�33C�33C�33C�33C�Y�C�Y�C�L�C�@ C�33C�ffC�Y�C�L�C�@ C�33C�@ C�Y�C�@ C�&fC�@ C�Y�C�L�C�&fC�@ C�Y�C�Y�C�ffC�@ C��C�33C�33C�@ C�L�C�L�C�Y�C�Y�C�L�C�L�C�@ C�33C�33C�L�C�ffC�Y�C�L�C�&fC�@ C�s3D��DY�D�fD
@ D��DY�D��DffD�D�fD��DY�D"3D$�3D'�3D*` D-�D/�3D2��D5@ D7��D:�3D=&fD?��DB` DD��DG��DJ3DL��DOS3DQ��DT�3DW9�DY�fD\�3D_33Da��DdS3Df��Di�3Dl,�Dn��Dqy�Dt�Dv��DyY�D{y�D}��D�6fD�vfD���D���D�3D�S3D���D���D�� D�,�D�ffD��3D��fD�0 D�y�D�� D�3D�FfD���D�� D�fD�VfD�� D���D�3D�<�D�s3D���D��fD�  D�S3D��fD�� D��fD�33D�l�D��fD��3D�,�D�s3D��fD�3D�Y�D���D��fD�C3D��fD���D�FfD�� D��fD�L�D���D��3D�)�D�y�D�� D�fD�I�DʆfD˹�D��3D��D�0 D�VfD�y�DҖfDӬ�D�� D���D��fD��3D�� D�  D��D�  D�0 D�FfD�` D�s3D�fD�� D��3D� D�6fD�c3D�3D���D�	�D�FfD퉚D��3D� D�VfD�fD��fD��D�Y�D���D���D�&fD�L�D���D��fD�0 E <�E �E� EfE��Ed�EfE� EFfE��E� EfE��EVfE�E	�3E
  E
�fEFfE�3E3E&fE>fE� E� E� E� E�fE�3E.fE1�E�3E��E&fE  E��E ��E"A�E#NfE$Y�E%�3E&�E(��E)�fE*� E,@ E-X E.h E0	�E1�E2&fE3��E4�3E5� E7[3E8\�E9� E:�3E<c3E?ffEBs3EE��EH�fEK� EN�fER�EU,�EX>fE[��E^p Ea� Ed�fEg�fEk�En&fEqd�Et�fEwk3Ez�fE}�fE�m�E�fE�� E�#3E���E�H�E���E�e�E��E�y�E� E�`�E��fE��3E�D E��fE���E�:fE��fE���E�!�E�x�E�� E�'3E�c3E�� E��E�l E��3E�  E�W3E��fG�O�?L��G�O�G�O�G�O�?L��?fffG�O�G�O�?�  G�O�?���?���?�33?�  ?���G�O�?ٙ�?�33@��@��@&ff@9��@Fff@Y��@l��@�  @���@�33@�  @���@�33@�  @ə�@�ff@�  @���@���A��A	��A��A  AffA&ffA,��A4��A9��AC33AI��AQ��AX  A^ffAfffAl��At��A|��A���A���A���A���G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111414441144141111141111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 @ �@ �@ �@ *@ �@ #�@ *S@ 1'@ 6�@ =q@ FQ@ R�@ `B@ m:@ z�@ ��@ ��@ ��@ �-@ �&@ ��@ ��@ �@ � @�@�@g@,`@:@G�@V�@c�@qS@~K@��@�H@��@��@�>@�7@�;@�@�,@�@�@"�@/�@>@K@X�@ff@t@�@�@��@�M@�R@�W@��@��@�L@��@	�@�@&;@3�@B8@O�@[z@i!@v@��@�#@��@�@��@��@׹@�m@�@ �@�@[@)�@8�@E�@R�@`�@m�@{�@�7@��@��@�-@��@�*@��@�(@��@@@�@,`@:�@FQ@S�@bN@p�@~�@��@�H@��@�F@��@��@ލ@�4@�,@�@�@""@/�@<@K@Z@g�@t@��@�@�U@�Y@�R@�J@��@�H@�@��@	�@�@&�@4�@@�@N�@[z@k.@y�@��@�h@�@�r@�k@�@�
@�`@�Y@ �@V@�@)�@6�@DD@R�@]�@l�@z�@��@�<@�(@��@�&@��@�#@�y@� @	�@	@	g@	-�@	;d@	I@	Wb@	b�@	qS@	~�@	��@	��@	�A@	��@	��@	є@	��@	��@	��@
v@
@
 �@
.l@
>�@
Lu@
Z@
hs@
uk@
�@
��@
�a@
�M@
��@
Ĝ@
�C@
��@
�@@
��@	�@6@$�@4�@B8@O0@\)@i!@y�@�|@�u@�m@�f@��@��@�
@�T@�Y@^@V@�@(�@7�@E�@S�@_�@k.@z3@��@�0@��@�-@��@�*@�#@��@��@�@b@g@.l@;d@H]@S�@b�@s_@�@5�@x&@��@^@F�@��@��@�@c�@��@��@FQ@�h@܀@(�@s_@��@	�@Q�@��@�T@)�@o�@�R@�Q@E�@��@є@�@bN@�M@�@:�@��@�o@o@Wb@��@�@-�@uk@�w@%@M�@��@ψ@o@V�@��@�/@
@`A@��@�@(G@i�@�f@�L@3�@x�@�&@v@K@�@��@ �@ _�@ �4@ �y@!,`@!o�@!�-@!�@"7L@"z3@"�@#  @#A�@#�@#��@$1@$K�@$��@$є@%*@%[z@%�@%�@&-@&v@&��@'�@'K�@'�#@'�/@(&�@(p�@(��@)�@)I�@)�@)խ@*[@*b�@*��@*�@+1'@+r�@+�-@+�@,/�@,n�@,��@,�y@-$�@-_�@-��@-є@.
�@.DD@.~K@.��@.�Y@/,`@/g�@/��@/ލ@0�@0\)@0�H@0�t@1B@1Yn@1�H@1�/@2 �@2dZ@2�M@2�@333@3x�@3�@4]@4F�@4��@4��@5�@5Z@5��@5��@6&�@6m:@6��@6��@7=q@7�@7ƨ@8J@8Q=@8�0@8��@9[@9_�@9�(@9�`@:(G@:j@:�@:�@;+�@;i!@;��@<&�@<�(@=�@=�c@>=q@>�~@?\)@?ψ@@>�@@��@AYn@A��@Bg@C	�@Ct@D�@D�@E7L@E��@F�@F��@G7L@G��@HT�@H�c@Iz�@I�Y@Jff@K�@K��@K�Q@L��@M @M��@N7�@N��@OK@O��@P]�@Q��@R�@Te	@U��@V�Y@X9X@Y��@Z��@\@,@]��@^�@`UU@a��@b�@dM$@e��@f�9@hX@i�P@j��@lQ=@m�$@n��@pQ�@q��@r�q@tH]@u��@v�M@x>�@y��@z�@{0x@{y�@{��@{�Y@|8�@|~�@|ě@}�@}Yn@}��@}�O@~�@~i!@~�T@~�@1'@~K@��@��@�#�@�7�G�O�@ G�O�G�O�G�O�@ @ �G�O�G�O�@ jG�O�@ @ �@ %@ �@ �G�O�@ 1@ 	�@ �@ �@ V@ b@ �@ �@ �@ �@ �@ �@ �@ !s@ "�@ %�@ '�@ *S@ ,`@ /@ 2�@ 5?@ 7L@ :�@ =q@ @,@ C�@ FQ@ I�@ K�@ O�@ R�@ V@ X�@ [z@ ^�@ a�@ e	@ hs@ k.@ n�@ qS@ t�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�I�A�O�A�S�A�VA�XA�`BA�dZA�ffA�dZA�ffA�bNA�`BA�;dAѬA�S�A�p�A�;dA�(�A��A�
=A��AζFAΙ�A�`BA�ȴAͩ�A�p�A�"�A�VA�  A��A���A��
AƃA�=qA��uA��RA���A�bA���A��;A�~�A�VA���A��A�(�A��9A�7LA�v�A���A�7LA�A�O�A�1A�A�9XA��A��`A�{A�A���A�I�A���A�oA���A�ĜA��uA���A��PA�{A�oA��A���A�G�A�~�A�5?A�M�A�hsA�JA�1'A��A���A�bA���A�C�A��/A��RA���A��HA�ZA���A���A��jA�$�A��mA�=qA�p�A�/A��A��-A��A��A~9XA|�\Ay��Aw��Av��AvI�At��At5?Asl�ApVAnAm��AmoAj��AhVAg�Ag7LAf�yAe��Ad~�Ab��Aa�#A`9XA^E�A]��A\��AZ��AY��AX  AWVAV5?AUC�AT�AQ�-AP1AM�AL��AK�AJ��AIhsAGx�AFffAC�TAA�mA@�A@r�A@VA?��A?dZA=��A< �A;?}A:�9A9hsA7�;A7VA6ffA6{A5�PA4��A4�A3hsA2�DA2bNA2=qA1��A1S�A0ffA/?}A.��A.�A.Q�A-�A,  A+XA*��A*ffA)A)/A(A�A&^5A$z�A"v�A!A��A��A��A33A�uA`BA5?A|�A�uAM�A�mA�Ar�A��A9XAC�A�A�A^5A��A��A�AZA��A
��A
~�A	A��AZAA�A1'A��A�A��At�A&�A�A��A�AoA�9A�A\)A 9XA {@��@��@�n�@�A�@�|�@���@�9X@���@�&�@��
@�\@���@��@�ȴ@�`B@�@�V@�F@��@���@�j@�C�@���@�M�@�$�@�J@ݺ^@���@�@ϕ�@�z�@��y@�hs@��9@�@��@�1@�`B@�?}@�O�@���@���@��D@��!@���@���@�V@�^5@�j@�^5@�X@�I�@�1@���@��
@���@�Z@�K�@��!@�5?@�  @���@�K�@���@�v�@��-@�z�@�
=@��@��+@��@�I�@�"�@��#@��@�r�@�1@|��@y�@xQ�@u�T@tI�@r~�@p�u@ol�@m�-@j��@g�w@ct�@ax�@_|�@]`B@Z�@Y&�@X  @T�@Sƨ@R^5@Nff@L�@LI�@J�@J��@I&�@H  @G|�@G�@F@D(�@Co@A��@?
=@>E�@=V@;t�@:��@9&�@7K�@6v�@5�T@4�D@3�F@3o@1��@0��@/�@.ff@-O�@,9X@+��@*��@)��@)%@(b@'\)@&5?@$�@"�H@!�@!%@K�@�h@z�@"�@�\@=q@7L@�u@ �@�R@�@�m@�!@^5@X@�@�w@K�@��@�h@O�@��@j@�@
��@
^5@	�@��@ �@;d@V@��@�@��@Z@9X@��@�@33@�H@�\@^5@�^@x�@�@ �u@ r�@ r�@ bN@ 1'@   ?��R?�V?��D?�1?��?�?��^?���?��y?���?��?��
?��`?��?�  ?��-?�I�?�~�?�u?�K�?�j?�F?�%?��;?ݲ-?�1?�?���?��?ش9?�1'?�+?�
=?և+?�?}?�Z?��
?�t�?�n�?ѩ�?�bN?�A�?��?�5??�5??�j?�1?�1?˅?��#?��?���?�$�?Õ�?�hs?��w?��?��?�1?�?�~�?�^5?��?���?�^5?���?��H?�"�?��?�ƨ?�(�?��D?�V?�O�?��h?��?���?��?���?�A�?�bN?��?���?�Ĝ?��`?��`?�%?�&�?�hs?��7?��7?���?���?���?���?��?�J?�-?�n�?�n�?°!A�O�A�O�A�I�A�I�A�I�A�G�A�G�A�G�A�E�A�I�A�E�A�VA�G�A�G�A�K�A�M�A�S�A�XA�ZA�XA�VA�O�A�S�A�XA�VA�VA�S�A�VA�XA�ZA�`BA�^5A�bNA�dZA�dZA�ffA�ffA�dZA�bNA�ffA�hsA�ffA�ffA�dZA�bNA�bNA�`BA�`BA�`BA�bNA�bNA�\)A�O�A�33A�JA��/AѸRAѡ�A�x�A��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 A�I�A�O�A�S�A�VA�XA�`BA�dZA�ffA�dZA�ffA�bNA�`BA�;dAѬA�S�A�p�A�;dA�(�A��A�
=A��AζFAΙ�A�`BA�ȴAͩ�A�p�A�"�A�VA�  A��A���A��
AƃA�=qA��uA��RA���A�bA���A��;A�~�A�VA���A��A�(�A��9A�7LA�v�A���A�7LA�A�O�A�1A�A�9XA��A��`A�{A�A���A�I�A���A�oA���A�ĜA��uA���A��PA�{A�oA��A���A�G�A�~�A�5?A�M�A�hsA�JA�1'A��A���A�bA���A�C�A��/A��RA���A��HA�ZA���A���A��jA�$�A��mA�=qA�p�A�/A��A��-A��A��A~9XA|�\Ay��Aw��Av��AvI�At��At5?Asl�ApVAnAm��AmoAj��AhVAg�Ag7LAf�yAe��Ad~�Ab��Aa�#A`9XA^E�A]��A\��AZ��AY��AX  AWVAV5?AUC�AT�AQ�-AP1AM�AL��AK�AJ��AIhsAGx�AFffAC�TAA�mA@�A@r�A@VA?��A?dZA=��A< �A;?}A:�9A9hsA7�;A7VA6ffA6{A5�PA4��A4�A3hsA2�DA2bNA2=qA1��A1S�A0ffA/?}A.��A.�A.Q�A-�A,  A+XA*��A*ffA)A)/A(A�A&^5A$z�A"v�A!A��A��A��A33A�uA`BA5?A|�A�uAM�A�mA�Ar�A��A9XAC�A�A�A^5A��A��A�AZA��A
��A
~�A	A��AZAA�A1'A��A�A��At�A&�A�A��A�AoA�9A�A\)A 9XA {@��@��@�n�@�A�@�|�@���@�9X@���@�&�@��
@�\@���@��@�ȴ@�`B@�@�V@�F@��@���@�j@�C�@���@�M�@�$�@�J@ݺ^@���@�@ϕ�@�z�@��y@�hs@��9@�@��@�1@�`B@�?}@�O�@���@���@��D@��!@���@���@�V@�^5@�j@�^5@�X@�I�@�1@���@��
@���@�Z@�K�@��!@�5?@�  @���@�K�@���@�v�@��-@�z�@�
=@��@��+@��@�I�@�"�@��#@��@�r�@�1@|��@y�@xQ�@u�T@tI�@r~�@p�u@ol�@m�-@j��@g�w@ct�@ax�@_|�@]`B@Z�@Y&�@X  @T�@Sƨ@R^5@Nff@L�@LI�@J�@J��@I&�@H  @G|�@G�@F@D(�@Co@A��@?
=@>E�@=V@;t�@:��@9&�@7K�@6v�@5�T@4�D@3�F@3o@1��@0��@/�@.ff@-O�@,9X@+��@*��@)��@)%@(b@'\)@&5?@$�@"�H@!�@!%@K�@�h@z�@"�@�\@=q@7L@�u@ �@�R@�@�m@�!@^5@X@�@�w@K�@��@�h@O�@��@j@�@
��@
^5@	�@��@ �@;d@V@��@�@��@Z@9X@��@�@33@�H@�\@^5@�^@x�@�@ �u@ r�@ r�@ bN@ 1'@   ?��R?�V?��D?�1?��?�?��^?���?��y?���?��?��
?��`?��?�  ?��-?�I�?�~�?�u?�K�?�j?�F?�%?��;?ݲ-?�1?�?���?��?ش9?�1'?�+?�
=?և+?�?}?�Z?��
?�t�?�n�?ѩ�?�bN?�A�?��?�5??�5??�j?�1?�1?˅?��#?��?���?�$�?Õ�?�hs?��w?��?��?�1?�?�~�?�^5?��?���?�^5?���?��H?�"�?��?�ƨ?�(�?��D?�V?�O�?��h?��?���?��?���?�A�?�bN?��?���?�Ĝ?��`?��`?�%?�&�?�hs?��7?��7?���?���?���?���?��?�J?�-?�n�?�n�?°!A�O�A�O�A�I�A�I�A�I�A�G�A�G�A�G�A�E�A�I�A�E�A�VA�G�A�G�A�K�A�M�A�S�A�XA�ZA�XA�VA�O�A�S�A�XA�VA�VA�S�A�VA�XA�ZA�`BA�^5A�bNA�dZA�dZA�ffA�ffA�dZA�bNA�ffA�hsA�ffA�ffA�dZA�bNA�bNA�`BA�`BA�`BA�bNA�bNA�\)A�O�A�33A�JA��/AѸRAѡ�A�x�A��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B�uB�oB�oB�oB�oB�uB�oB�oB�oB�oB�oB�{B��BŢBhB8RB?}BA�BF�BH�BO�BZB]/B_;BffBe`BaHB^5B]/B\)B[#B~�B��B�B\)BM�BM�BK�B[#BYBXBVB]/Bl�Bs�By�B|�B�B}�Bs�Bt�Bv�Bx�B|�B|�B�B�7B�bB��B��B��B��B��B��B��B��B�JB�PB�JB�%B}�Bu�Bp�Bq�Bk�BXB:^B,B�B{BVB��B�ZB�#B��B�uBaHB8RB�BB
�B
�HB
�B
��B
��B
�}B
�'B
��B
��B
�\B
�7B
|�B
q�B
e`B
P�B
N�B
E�B
@�B
7LB
0!B
#�B	��B	�B	�B	�`B	�B	��B	��B	��B	��B	ǮB	ŢB	�jB	�^B	�B	�B	��B	��B	��B	��B	�JB	�DB	�B	}�B	s�B	hsB	_;B	S�B	M�B	E�B	?}B	8RB	.B	&�B	�B	hB	JB	
=B	1B	B	  B��B�B�B�B�TB�;B�)B�B�
B��B��B��BȴBƨBŢBÖB��B�wB�LB�LB�LB�FB�?B�B�B��B��B��B��B��B�VB�Bs�Bs�BiyB\)BQ�BQ�BJ�BG�BC�BG�BD�BC�BA�BA�B<jB=qB;dB9XB;dB=qB=qB@�BB�BB�BA�B@�B<jB=qB;dB8RB8RB:^B9XB9XB;dB<jB=qBA�BB�BC�BC�BE�BI�BI�BH�BE�BH�BG�BC�BE�BA�BD�BA�B>wB=qB=qB<jB;dB9XB:^B:^B:^B8RB8RB:^B:^B:^B9XB:^B<jB;dB;dB;dB;dB:^BK�BO�BW
B\)BaHBffBq�By�B�%B�PB�hB��B�B�XBŢB�B�mB�B��B	B	VB	�B	#�B	+B	8RB	Q�B	`BB	gmB	�B	�\B	��B	�B	�B	�9B	��B	B	��B	��B	�5B	�fB	�B	�yB	�B	�B	�B	��B	��B	��B	��B	��B
B
1B

=B
PB
\B
bB
hB
oB
�B
�B
�B
�B
�B
 �B
"�B
$�B
'�B
)�B
,B
,B
.B
2-B
33B
33B
5?B
49B
6FB
7LB
7LB
8RB
9XB
<jB
<jB
=qB
A�B
@�B
B�B
B�B
D�B
E�B
G�B
H�B
I�B
J�B
J�B
K�B
L�B
N�B
O�B
O�B
Q�B
R�B
S�B
S�B
T�B
VB
W
B
VB
XB
YB
\)B
]/B
]/B
_;B
aHB
bNB
cTB
cTB
dZB
ffB
ffB
ffB
gmB
jB
k�B
l�B
l�B
m�B
n�B
o�B
o�B
q�B
q�B
q�B
r�B
r�B
t�B
t�B
t�B
t�B
w�B
x�B
x�B
y�B
z�B
z�B
{�B
|�B
|�B
|�B
}�B
}�B
~�B
~�B
~�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�%B
�%B
�1B
�1B
�7B
�=B
�7B
�=B
�PB
�PB
�PB
�VB
�bB
�hB
�oB
�oB
�{B
�{B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
�B
�B
�B
�B
�!B
�-B
�3B
�9B
�?B
�FB
�LB
�LB
�RB
�XB
�XB
�XB
�XB
�XB
�^B
�^B
�^B
�^B
�^B
�^B
�dB
�dB
�dB
�jB
�jB
�dB
�dB
�jB
�dB
�jB
�dB
�jB
�jB
�jB
�jB
�jB
�jB
�jB
�jB
�jB
�jB
�qB
�jB
�jB
�dB
�jB
�dB�uB�oB�uB�oB�oB�uB�{B�uB�uB��B�uB�hB�oB�uB�hB�{B�uB�oB�hB�oB�oB�uB�oB�hB�oB�oB�oB�uB�oB�oB�oB�uB�uB�oB�oB�oB�oB�oB�oB�oB�oB�oB�oB�oB�oB�oB�{B�{B�{B�{B�uB��B��B��B�B�^BBǮB��B�NG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 B�hB�bB�bB�bB�bB�hB�bB�bB�bB�bB�bB�oB��BÖB\B6FB=qB?}BD�BF�BM�BXB[#B]/BdZBcTB_;B\)B[#BZBYB|�B��B�BZBK�BK�BI�BYBW
BVBS�B[#BjBq�Bw�Bz�B~�B{�Bq�Br�Bt�Bv�Bz�Bz�B~�B�+B�VB��B��B��B��B��B��B��B�uB�=B�DB�=B�B{�Bs�Bn�Bo�BiyBVB8RB)�B�BoBJB��B�NB�B��B�hB_;B6FB�BB
�B
�;B
�
B
��B
ɺB
�qB
�B
��B
�uB
�PB
�+B
z�B
o�B
cTB
N�B
L�B
C�B
>wB
5?B
.B
!�B	��B	�B	�sB	�TB	��B	��B	��B	ɺB	ɺB	ŢB	ÖB	�^B	�RB	��B	�B	��B	��B	��B	�{B	�=B	�7B	�B	{�B	q�B	ffB	]/B	Q�B	K�B	C�B	=qB	6FB	,B	$�B	�B	\B	
=B	1B	%B	B��B��B�B�B�yB�HB�/B�B�B��B��B��B��BƨBĜBÖB��B�}B�jB�?B�?B�?B�9B�3B�B��B��B��B��B��B��B�JB�Bq�Bq�BgmBZBO�BO�BH�BE�BA�BE�BB�BA�B?}B?}B:^B;dB9XB7LB9XB;dB;dB>wB@�B@�B?}B?}B:^B<jB:^B7LB7LB9XB8RB8RB:^B;dB<jB@�BA�BB�BB�BD�BH�BH�BG�BD�BG�BF�BB�BD�B@�BC�B@�B=qB<jB<jB;dB:^B8RB9XB9XB9XB7LB7LB9XB9XB9XB8RB9XB;dB:^B:^B:^B:^B9XBJ�BN�BVB[#B`BBe`Bp�Bx�B�B�JB�bB��B�B�RBĜB�
B�fB�B��B	B	PB	�B	"�B	)�B	7LB	P�B	_;B	ffB	�B	�VB	��B	�B	�B	�3B	�}B	��B	��B	��B	�/B	�`B	�yB	�sB	�B	�B	�B	��B	��B	��B	��B	��B
B
+B
	7B
JB
VB
\B
bB
hB
{B
�B
�B
�B
�B
�B
!�B
#�B
&�B
(�B
+B
+B
-B
1'B
2-B
2-B
49B
33B
5?B
6FB
6FB
8RB
9XB
<jB
<jB
=qB
A�B
@�B
B�B
B�B
D�B
E�B
G�B
H�B
I�B
J�B
J�B
K�B
L�B
N�B
O�B
O�B
Q�B
R�B
S�B
S�B
T�B
VB
W
B
VB
XB
YB
\)B
]/B
]/B
_;B
aHB
bNB
cTB
cTB
dZB
ffB
ffB
ffB
gmB
jB
k�B
l�B
l�B
m�B
n�B
o�B
o�B
q�B
q�B
q�B
r�B
r�B
t�B
t�B
t�B
t�B
w�B
x�B
x�B
y�B
z�B
z�B
{�B
|�B
|�B
|�B
}�B
}�B
~�B
~�B
~�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�+B
�+B
�7B
�7B
�=B
�DB
�=B
�DB
�VB
�VB
�VB
�\B
�hB
�oB
�uB
�uB
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
�B
�B
�B
�B
�!B
�-B
�9B
�?B
�FB
�LB
�RB
�XB
�XB
�^B
�dB
�dB
�dB
�jB
�jB
�qB
�qB
�qB
�qB
�qB
�qB
�wB
�wB
�wB
�}B
�}B
�wB
�wB
�}B
�wB
�}B
�wB
�}B
�}B
�}B
�}B
�}B
�}B
�}B
�}B
�}B
�}B
��B
��B
��B
�}B
��B
�}B�hB�bB�hB�bB�bB�hB�oB�hB�hB�uB�hB�\B�bB�hB�\B�oB�hB�bB�\B�bB�bB�hB�bB�\B�bB�bB�bB�hB�bB�bB�bB�hB�hB�bB�bB�bB�bB�bB�bB�bB�bB�bB�bB�bB�bB�bB�oB�oB�oB�oB�hB�{B��B��B�B�RB��BŢB��B�BG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�201808272201312021061413523020210614135230202106141746352021061417463520210614174635201808272201312021061413523020210614135230202106141746352021061417463520210614174635PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            new conductivity = original conductivity * (1 + delta*TEMP + CPcor_SBE*PRES) / (1 + delta*TEMP_ADJUSTED + CPcor_new*PRES_ADJUSTED)                                                                                                                              none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS                                                                                                                                                                                                                                       PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            new conductivity = original conductivity * (1 + delta*TEMP + CPcor_SBE*PRES) / (1 + delta*TEMP_ADJUSTED + CPcor_new*PRES_ADJUSTED)                                                                                                                              none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS                                                                                                                                                                                                                                       surface_pressure=-0.50 dbar                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            CPcor_new = -1.25e-07; CPcor_SBE = -9.57e-08; delta = 3.25e-06                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            OW r = 1 (+/-0), vertically averaged dS = -0.002 (+/-0)                                                                                                                                                                                                         surface_pressure=-0.50 dbar                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            CPcor_new = -1.25e-07; CPcor_SBE = -9.57e-08; delta = 3.25e-06                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            OW r = 1 (+/-0), vertically averaged dS = -0.002 (+/-0)                                                                                                                                                                                                         Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                      none                                                                                                                                                                                                                                                            New conductivity computed by using a different CPcor value from that provided by Sea-Bird.                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Significant salinity sensor drift detected. OW least squares fit adopted.                                                                                                                                                                                       Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                      none                                                                                                                                                                                                                                                            New conductivity computed by using a different CPcor value from that provided by Sea-Bird.                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Significant salinity sensor drift detected. OW least squares fit adopted.                                                                                                                                                                                       PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            AO  AO  ARCAARCAADJPADJP                                                                                                                                        2018082722013120180827220131  IP  IP                                G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2018082722013120180827220131QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�5F03E           703E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2018082722013120180827220131QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�0               0               PM  PM  ARSQARSQCOWGCOWGV1.1V1.1                                                                                                                                2021072216015220210722160152IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                