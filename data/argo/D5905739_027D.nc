CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS     N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-08-16T07:02:51Z creation      
references        (http://www.argodatamgt.org/Documentation   comment           user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    6�   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  �  6�   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  �  7d   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  `  7�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        8D   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    8L   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    8P   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                  @  8T   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    8�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    8�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                  @  8�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                  @  8�   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                  @  9    WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    9`   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�E�vQ�   
_FillValue        A.�~       axis      T           9h   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    9x   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�E�vQ�   
_FillValue        A.�~            9|   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
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
resolution        =���   axis      Z        0  ;�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   L   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     0  P   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   `@   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     0  dL   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     0  t|   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   ��   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     0  ��   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     0  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     0  �$   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   �T   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     0  �`   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   ѐ   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     0  ՜   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                   4   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                   <   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                   D   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                   L   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  � T   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                   �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                   �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar           HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar            HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�       (   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    0   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �t   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �t   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �t   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  � 
tArgo profile    3.1 1.2 19500101000000  20180816070251  20210617131501  5905739 5905739 US ARGO PROJECT                                                 US ARGO PROJECT                                                 GREGORY C. JOHNSON                                              GREGORY C. JOHNSON                                              PRES            TEMP            PSAL            PRES            TEMP            PSAL                  DD  AOAO7219                            7219                            2C  2C  DD  SOLO_D_MRV                      SOLO_D_MRV                      12002                           12002                           SBE603 01Dec16                  SBE603 01Dec16                  874 874 @�w�]�Y�@�w�]�Y�11  @�w�UU\�@�w�UU\�@6�Ć�-�@6�Ć�-��c߈�~���c߈�~��11  GPS     GPS     Primary sampling: averaged [1Hz sampling by SBE-41CP averaged in 2-dbar bins]                                                                                                                                                                                   Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 AB  AA  AA  >���?fff@   @9��@�  @�33@�33@�  @���AffA!��A@  A`  A���A���A���A���A���A�  A�33A�  B   BffB33BffB   B(ffB0��B8ffB@  BHffBP��BX��B`ffBh  Bp  Bx  B�33B�  B�33B�ffB�  B���B�  B�33B�33B�33B�33B�33B�  B�ffB�ffB�ffB�ffB�ffB�33B�ffB�33B�33B�  B�  B�  B�  B���B���B���B�ffB�33B�33C   C�fC33C�C  C
33C  C��C�CL�C�C�fC�C�fC��C�C�fC!��C$  C&33C(�C)��C,  C.�C0L�C2ffC433C6  C8�C:33C<  C=��C@  CB33CDL�CF�CG��CJ�CL33CN�CO��CR  CT�CV33CXffCZffC\�C]�fC_�fCb33CdL�Cf�Cg�fCj�Cl33Cn  Co��Cr  Ct33Cv  Cw�fCz  C|33C~  C�fC��C��C��fC��C�  C��3C��C��C��3C��C��C�  C��C�  C�  C��C�&fC��C�  C��C�  C��fC��3C��C��C�&fC��C��fC��3C��3C��3C��3C�  C��3C��fC��fC��fC��3C��fC��fC��3C��3C��3C��3C��3C��3C�  C�  C��C��C��C�&fC�&fC��C�ٚC��fC��fC�  C�  C��C�&fC�  C��fC��fC��3C��C��C��C��fC��3C�  C��3C��fC��3C��C�  C��fC��C�&fC��C�  C��3C��fC��3C��C�&fC��C��3C��C��C��3C�  C��C��C��3C��3C�  C��C��C�33C��C��fC��3C��3C�  C�  C�  C��C��C�  C�  C�  C��3C��3C��3C��fC��fC�ٚC�ٚC��fC�ٚC��fC�ٚC��fC�ٚC��fC�33D �D ��D�Dl�D,�D�3D�D�3D33D�3D` D��D�3D,�D��D"l�D%fD'� D*,�D,�fD/3D1� D43D6�fD9  D;� D=��D@S3DB��DE9�DG��DJ&fDL��DO�DQ��DS��DVl�DX�3D[FfD]��D`&fDb��De  DgffDi��Dl33Dn��Dp�3Ds` Du��Dx33Dz��D|� D  D�� D���D�	�D�0 D�S3D�l�D���D�� D���D��fD�� D���D��3D��fD�fD��D�)�D�@ D�P D�Y�D�p D��fD�� D���D�ɚD��D�fD�  D�I�D�p D���D��fD�� D�  D�FfD�l�D��fD���D��fD�3D�#3D�@ D�Y�D�p D�� D��3D���D���D�ɚD��3D��3D�� D���D��D�fD�  D�  D�  D�&fD�#3D�,�D�6fD�6fD�@ D�L�D�P D�VfD�\�D�\�D�s3D̃3D̓3DΙ�Dϩ�DжfD�ɚD���D�� D�fD�  D�6fD�@ D�Y�D�i�D�y�D܌�DݦfD��fD�ٚD��3D�	�D�&fD�C3D�VfD�l�D�3D�3D驚D��D���D�� D��D���D��D�&fD�6fD�FfD�P D�S3D�\�D�ffD�s3D�y�D�vfD�� D���D��fD�� D�� E Y�E ��Ec3Es3E~fE�fEfE�E� E	��EfE	�E|�Ey�E� E��EnfE�fE��E` E^fE��E�3EA�E,�E� E�E S3E!9�E"��E#�3E%C3E&� E'vfE(� E*)�E+�fE,{3E-�fE/;3E0��E1�fE2� E4;3E5��E6q�E7��E93E:d�E;�fE>�3EA� EEfEH�EK�ENY�EQ� ET^fEW� EZ�fE]�3Ea3Ed3Eg;3Ej|�Emh Ep�fEs�fEv��Ey� E}3E�'3E��3E�8 E���E�_3E�� E�BfE�| E��3E�(�E�|�E���E�"f>���>���>L��>���>���>���>���>���>���>L��>���>���?   >���>���?   ?��?��?333?333?fff?�  ?���?�  ?ٙ�@   @��@&ff@333@S33@fff@�  @���@���@�33@�  @���@ə�@�ff@�ff@�33A��A33A��A��A!��A+33A333A;33AA��AL��AS33A[33Ad��AnffAvffA~ffA�ffA�33A�33G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111441444414111441141411111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                          ?fff?�33@   @Y��@�  @�33@�33@�  AffAffA)��AH  Ah  A���A���A���A���Ař�A�  A�33A�  B  B
ffB33BffB"  B*ffB2��B:ffBB  BJffBR��BZ��BbffBj  Br  Bz  B�33B�  B�33B�ffB�  B���B�  B�33B�33B�33B�33B�33B�  B�ffB�ffB�ffB�ffB�ffB�33B�ffB�33B�33B�  B�  B�  B�  B���B���B���B�ffB�33B�33C � CffC�3C��C� C
�3C� CL�C��C��C��CffC��CffCL�C��C ffC"L�C$� C&�3C(��C*L�C,� C.��C0��C2�fC4�3C6� C8��C:�3C<� C>L�C@� CB�3CD��CF��CHL�CJ��CL�3CN��CPL�CR� CT��CV�3CX�fCZ�fC\��C^ffC`ffCb�3Cd��Cf��ChffCj��Cl�3Cn� CpL�Cr� Ct�3Cv� CxffCz� C|�3C~� C�33C�L�C�L�C�&fC�L�C�@ C�33C�Y�C�L�C�33C�Y�C�L�C�@ C�Y�C�@ C�@ C�L�C�ffC�Y�C�@ C�L�C�@ C�&fC�33C�L�C�Y�C�ffC�L�C�&fC�33C�33C�33C�33C�@ C�33C�&fC�&fC�&fC�33C�&fC�&fC�33C�33C�33C�33C�33C�33C�@ C�@ C�L�C�L�C�Y�C�ffC�ffC�L�C��C�&fC�&fC�@ C�@ C�L�C�ffC�@ C�&fC�&fC�33C�L�C�Y�C�L�C�&fC�33C�@ C�33C�&fC�33C�L�C�@ C�&fC�L�C�ffC�Y�C�@ C�33C�&fC�33C�L�C�ffC�L�C�33C�Y�C�L�C�33C�@ C�Y�C�L�C�33C�33C�@ C�L�C�Y�C�s3C�L�C�&fC�33C�33C�@ C�@ C�@ C�L�C�L�C�@ C�@ C�@ C�33C�33C�33C�&fC�&fC��C��C�&fC��C�&fC��C�&fC��C�&fC�s3D 9�D ��D9�D��DL�D�3D9�D�3DS3D�3D� D�D�3DL�D��D"��D%&fD'� D*L�D,�fD/33D1� D433D6�fD9  D;� D>�D@s3DB��DEY�DG��DJFfDL��DO9�DQ��DT�DV��DX�3D[ffD]ٚD`FfDb��De  Dg�fDi��DlS3Dn��Dq3Ds� Du��DxS3Dz��D|� D  D�� D���D��D�@ D�c3D�|�D���D�� D�ɚD��fD�� D���D��3D�fD�fD�,�D�9�D�P D�` D�i�D�� D��fD�� D�ɚD�ٚD���D�fD�0 D�Y�D�� D���D��fD�  D�0 D�VfD�|�D��fD�ɚD��fD�3D�33D�P D�i�D�� D�� D��3D���D�ɚD�ٚD��3D��3D�  D��D��D�&fD�0 D�0 D�0 D�6fD�33D�<�D�FfD�FfD�P D�\�D�` D�ffD�l�D�l�D˃3D̓3D͓3DΩ�DϹ�D��fD�ٚD���D�  D�fD�0 D�FfD�P D�i�D�y�Dۉ�Dܜ�DݶfD��fD��D�3D��D�6fD�S3D�ffD�|�D�3D�3D鹚D���D���D�� D���D��D�)�D�6fD�FfD�VfD�` D�c3D�l�D�vfD��3D���D��fD�� D���D��fD�� D�� E a�E ��Ek3E{3E�fE�fEfE�E� E	��EfE�E��E��E  E��EvfE�fE��Eh EffE��E�3EI�E4�E� E��E [3E!A�E"��E#�3E%K3E&� E'~fE(� E*1�E+�fE,�3E-�fE/C3E0��E1�fE2� E4C3E5��E6y�E7��E93E:l�E;�fE>�3EA� EEfEH!�EK�ENa�EQ� ETffEW� EZ�fE]�3Ea3Ed3EgC3Ej��Emp Ep�fEs�fEv��Ey� E}3E�+3E��3E�< E���E�c3E�� E�FfE�� E��3E�,�E���E���E�&fG�O�G�O�?333G�O�G�O�G�O�G�O�?L��G�O�?333?L��?fffG�O�G�O�?fff?�  G�O�?���G�O�?���?�33?�  ?ٙ�@   @��@   @,��@Fff@S33@s33@�33@�  @���@���@�33@�  @���@ٙ�@�ff@�ffA��A	��A33A��A!��A)��A333A;33AC33AI��AT��A[33Ac33Al��AvffA~ffA�33A�ffA�33A�33G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111441444414111441141411111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                          @ �@ %@ �@ �@ O@ "�@ )�@ /�@ 5�@ <�@ D�@ Q�@ _�@ m�@ {�@ �7@ ��@ �5@ �~@ �w@ ��@ �t@ ��@ ��@@@g@-�@:�@G�@V@dZ@r@~�@��@��@�A@��@@��@�;@�@�~@�@*@"�@0x@>@K�@X�@g�@uk@�@��@�a@�Y@��@ƨ@�O@�H@��@��@
=@6@$�@2�@B8@O0@\�@i�@v�@�|@�u@�m@�r@��@�@׹@�@�@�Q@V@�@'�@7L@C�@P�@_�@n�@{�@�+@�0@��@��@��@�*@�t@��@� @j@�@�@-�@<@H]@S�@c�@r@~�@��@��@��@�F@�J@��@ލ@��@�~@1@�@"�@/@>@Lu@X�@e	@t@�@�\@�U@��@��@��@��@��@�@�9@
�@�@$�@4�@A�@M�@]�@j@ww@�|@��@�m@��@��@��@�
@�`@�Y@��@�@�@*S@8�@D�@P�@^�@l�@z3@��@�0@�(@�!@��@�o@��@�@�e@	�@	b@	
@	+�@	9X@	F�@	UU@	b�@	qS@	~�@	�P@	��@	�M@	��@	��@	��@	܀@	�@	�,@
�@
�@
""@
.l@
<@
Ji@
Yn@
g�@
t�@
�W@
��@
�@
��@
��@
�J@
�O@
�H@
�@
�E@J@B@%�@2�@?}@M�@\�@k�@x&@�p@�$@�@�f@��@��@׹@�@�@  @V@�@+�@7L@B�@Q=@^�@m:@z�@��@��@��@�~@�&@��@��@�m@�@@�@�@*S@8�@E�@S�@`�@oF@|?@��@�U@��@��@�J@�*@O�@�h@�\@�@a�@�M@��@4�@|�@��@�@SI@�H@�H@&�@j@��@�Y@5?@x&@��@  @B8@��@�W@	�@Lu@�@��@6@Z@�U@�;@ �@c�@��@��@*S@m:@��@�L@1�@s_@��@��@7�@y�@�@�Y@33@t@�9@�e@33@qS@�f@�(@%�@a�@��@Ӡ@�@D�@�@��@�@.l@i�@��@܀@ �@ SI@ �\@ �o@!v@!B�@!�@!��@!�9@":@"z3@"��@"�,@#:@#x�@#��@#� @$5@@$uk@$�-@$�@%,`@%hs@%��@%��@&�@&S�@&��@&�@' �@':�@'t@'�f@'�m@( @(X�@(�\@(��@(��@)3�@)l�@)�4@)��@*{@*M�@*�@*�@*�@++�@+g@+�@+׹@,@,M$@,�|@,�2@,��@-6�@-r@-�@-�y@.""@.^5@.�<@.�C@/�@/I@/�|@/�2@/�E@08�@0uk@0�-@0��@1(G@1c�@1��@1�@2�@2M�@2��@2�2@2��@38�@3r@3�@3�@4�@4V@4��@4�W@5 �@58�@5n�@5�A@5��@6B@6Q�@6��@6�>@6�9@74�@7��@8�@8�C@92�@9�z@:G�@:��@;T�@;��@<^5@<�@=m:@=�@>z2@?�@?�+@@(�@@��@A8�@A��@B> @B�z@C:@C��@Dg�@D�@E^5@E�L@F�@G{@Gs_@H�@H�I@I2�@I��@J+�@J��@KYn@K�w@LQ�@L�@Mx&@M׹@Nhs@N� @O�+@P6@Qg�@R�@T�@U`B@V��@X
�@Yff@Z�T@\  @]l�@^��@`�@aO�@b��@dJ@eK@f��@g�@iWb@j�m@k�@m[z@n�(@o�,@qH]@r��@s��@tFP@tww@t��@u
�@uR�@u�I@u��G�O�G�O�@ ^G�O�G�O�G�O�G�O�@ G�O�@ ^@ @ �G�O�G�O�@ �@ jG�O�@ G�O�@ �@ %@ �@ 1@ 
=@ �@ �@ @ �@ @ �@ �@ O@ 
@  �@ "�@ %�@ (G@ +@ -�@ 1'@ 3�@ 7L@ ;d@ >@ A�@ D�@ I@ Lu@ O�@ R�@ Wb@ Z@ ]�@ a�@ e�@ i!@ l�@ oF@ s_@ v�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�K�A�O�A�Q�A�VA�dZA�bNA�`BA�bNA�bNA�ffA�dZA�hsA�l�A�v�A�x�A�v�A�r�A�p�A�jA�l�A�bNA�l�A�bNA�dZA�n�A�n�A�l�A�ZA�I�A�?}A�;dA��A��;A�I�A���Aƙ�A��#A��A���A�$�A�x�A���A���A���A�9XA�A�E�A���A��^A���A�9XA���A�K�A���A�x�A�C�A���A�{A��9A�E�A��TA�ƨA���A�K�A��TA�K�A��^A�`BA��A�C�A���A�33A�A��
A�M�A�VA��#A�ƨA���A��RA�K�A���A�K�A���A��wA��/A���A�A���A���A�VA���A�$�A���A�"�A�\)A�A���A��9A�9XA��A��DA�JA�/A��A�(�A�A��-A��jA�?}A�%A�JA��A�{A�/A�=qA�=qA��#A��A��A~�+A}�A|jA{XAyO�AxbAv��AuS�As�^Ap�AlȴAk�hAi��Ah�\Ai33Ai%AghsAf-Ae��Ae�AdjAc�PAa�^A`�uA_��A_;dA]�A\�DA[%AZ~�AY��AY�AYC�AW��AWhsAUdZARE�APĜAO�AM��AK7LAI�wAH��AG�#AF5?AE\)AD�ACp�AA�hA?��A?XA>��A>JA<��A<�A;��A9��A8r�A6��A5\)A4��A4�A3�-A3\)A2�9A1�A/XA.ȴA-XA+33A*M�A*{A)?}A(�DA'�-A'�PA&�RA%%A#t�A#VA"��A"1A!l�A �HA ��A �\A  �AXA�A1'A/A��A��A1A��A�/A�;AM�A7LA33A;dA��AVAK�A1'A��A�;A��A&�A�jAVA �Al�A
r�A	��A	��AjA�A1A%A�DA�TA�`A��A �!A J@��R@�@��@��+@���@�33@���@�|�@�b@�9@�t�@��y@��@�1'@�{@�r�@�@��y@�?}@�"�@�^5@�n�@š�@�1@���@��\@��y@�r�@�hs@�r�@���@�A�@�-@���@�O�@��@�n�@�@�p�@�G�@�~�@��@��P@��P@���@�?}@���@��P@���@���@�(�@���@���@���@�/@�  @��@�@��@�|�@���@�@�V@��@�@�$�@�%@�b@���@�v�@�@��@�w@;d@}?}@|��@z��@y%@wl�@u��@tz�@so@q�^@p  @m�T@l1@j�@i��@hA�@f��@e/@c��@b=q@aX@`bN@_|�@^ff@]`B@\z�@Z^5@YX@X�@W\)@U�T@U�@So@R��@Q&�@P  @Mp�@K��@Jn�@HA�@F��@D�@C�
@B�H@@��@?;d@>5?@=��@<�D@:�\@9�^@8bN@6��@5��@4�D@3o@1�^@0 �@/�w@/l�@.5?@-��@,�@,�@+�@*�H@)��@(�u@(�@( �@&��@&�R@%O�@$�@#dZ@#C�@"n�@"M�@!�#@!�@ �@�w@
=@��@@/@�D@�
@�@X@K�@�+@@�h@�j@Z@t�@�\@��@bN@�@�y@�@�j@�@1@@
M�@	��@��@b@;d@ȴ@$�@��@�@/@�@t�@��@�?��;?��h?��^?�b?���?�z�?���?�Ĝ?�w?�V?�V?�"�?���?�1'?�l�?�?���?�M�?��`?�\)?ݑh?�(�?�"�?���?�X?ش9?��?�ȴ?�?}?�9X?�t�?���?ҏ\?�&�?�\)?���?�5??�V?���?�I�?�C�?�~�?��?��#?ǍP?�E�?�?}?ě�?�o?�M�?�hs?�bN?��?�O�?���?��?�(�?��m?��?�ƨ?���?�?�"�?��H?��H?�"�?�C�?���?�1?��D?�V?�O�?�p�?��h?��-?��?�{?�5?A�ZA�bNA�XA�S�A�O�A�M�A�G�A�E�A�G�A�G�A�C�A�C�A�A�A�A�A�C�A�A�A�C�A�A�A�C�A�E�A�\)A�^5A�ZA�ZA�ZA�M�A�K�A�O�A�Q�A�^5A�dZA�dZA�dZA�bNA�dZA�bNA�`BA�`BA�bNA�bNA�bNA�bNA�ffA�dZA�dZA�ffA�dZA�dZA�hsA�jA�hsA�jA�jA�n�A�n�A�t�A�v�A�v�A�z�A�x�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                          A�K�A�O�A�Q�A�VA�dZA�bNA�`BA�bNA�bNA�ffA�dZA�hsA�l�A�v�A�x�A�v�A�r�A�p�A�jA�l�A�bNA�l�A�bNA�dZA�n�A�n�A�l�A�ZA�I�A�?}A�;dA��A��;A�I�A���Aƙ�A��#A��A���A�$�A�x�A���A���A���A�9XA�A�E�A���A��^A���A�9XA���A�K�A���A�x�A�C�A���A�{A��9A�E�A��TA�ƨA���A�K�A��TA�K�A��^A�`BA��A�C�A���A�33A�A��
A�M�A�VA��#A�ƨA���A��RA�K�A���A�K�A���A��wA��/A���A�A���A���A�VA���A�$�A���A�"�A�\)A�A���A��9A�9XA��A��DA�JA�/A��A�(�A�A��-A��jA�?}A�%A�JA��A�{A�/A�=qA�=qA��#A��A��A~�+A}�A|jA{XAyO�AxbAv��AuS�As�^Ap�AlȴAk�hAi��Ah�\Ai33Ai%AghsAf-Ae��Ae�AdjAc�PAa�^A`�uA_��A_;dA]�A\�DA[%AZ~�AY��AY�AYC�AW��AWhsAUdZARE�APĜAO�AM��AK7LAI�wAH��AG�#AF5?AE\)AD�ACp�AA�hA?��A?XA>��A>JA<��A<�A;��A9��A8r�A6��A5\)A4��A4�A3�-A3\)A2�9A1�A/XA.ȴA-XA+33A*M�A*{A)?}A(�DA'�-A'�PA&�RA%%A#t�A#VA"��A"1A!l�A �HA ��A �\A  �AXA�A1'A/A��A��A1A��A�/A�;AM�A7LA33A;dA��AVAK�A1'A��A�;A��A&�A�jAVA �Al�A
r�A	��A	��AjA�A1A%A�DA�TA�`A��A �!A J@��R@�@��@��+@���@�33@���@�|�@�b@�9@�t�@��y@��@�1'@�{@�r�@�@��y@�?}@�"�@�^5@�n�@š�@�1@���@��\@��y@�r�@�hs@�r�@���@�A�@�-@���@�O�@��@�n�@�@�p�@�G�@�~�@��@��P@��P@���@�?}@���@��P@���@���@�(�@���@���@���@�/@�  @��@�@��@�|�@���@�@�V@��@�@�$�@�%@�b@���@�v�@�@��@�w@;d@}?}@|��@z��@y%@wl�@u��@tz�@so@q�^@p  @m�T@l1@j�@i��@hA�@f��@e/@c��@b=q@aX@`bN@_|�@^ff@]`B@\z�@Z^5@YX@X�@W\)@U�T@U�@So@R��@Q&�@P  @Mp�@K��@Jn�@HA�@F��@D�@C�
@B�H@@��@?;d@>5?@=��@<�D@:�\@9�^@8bN@6��@5��@4�D@3o@1�^@0 �@/�w@/l�@.5?@-��@,�@,�@+�@*�H@)��@(�u@(�@( �@&��@&�R@%O�@$�@#dZ@#C�@"n�@"M�@!�#@!�@ �@�w@
=@��@@/@�D@�
@�@X@K�@�+@@�h@�j@Z@t�@�\@��@bN@�@�y@�@�j@�@1@@
M�@	��@��@b@;d@ȴ@$�@��@�@/@�@t�@��@�?��;?��h?��^?�b?���?�z�?���?�Ĝ?�w?�V?�V?�"�?���?�1'?�l�?�?���?�M�?��`?�\)?ݑh?�(�?�"�?���?�X?ش9?��?�ȴ?�?}?�9X?�t�?���?ҏ\?�&�?�\)?���?�5??�V?���?�I�?�C�?�~�?��?��#?ǍP?�E�?�?}?ě�?�o?�M�?�hs?�bN?��?�O�?���?��?�(�?��m?��?�ƨ?���?�?�"�?��H?��H?�"�?�C�?���?�1?��D?�V?�O�?�p�?��h?��-?��?�{?�5?A�ZA�bNA�XA�S�A�O�A�M�A�G�A�E�A�G�A�G�A�C�A�C�A�A�A�A�A�C�A�A�A�C�A�A�A�C�A�E�A�\)A�^5A�ZA�ZA�ZA�M�A�K�A�O�A�Q�A�^5A�dZA�dZA�dZA�bNA�dZA�bNA�`BA�`BA�bNA�bNA�bNA�bNA�ffA�dZA�dZA�ffA�dZA�dZA�hsA�jA�hsA�jA�jA�n�A�n�A�t�A�v�A�v�A�z�A�x�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                          ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�BG�BH�BG�BH�BF�BF�BG�BH�BH�BH�BF�BG�BG�BH�BH�BH�BG�BG�BF�BF�BD�BE�BD�BE�BF�BE�BE�BC�BB�BB�BB�BA�BC�B@�B8RB@�BK�BP�BM�BVBW
B[#Bk�Bm�Bo�Bp�Bt�Bv�Bx�Bz�Bw�Bs�By�B{�B|�B{�B�B�7B�DB�JB�VB�\B�bB�hB�{B��B��B��B��B��B��B��B��B�oB�hB�bB�hB�JB�%B�%B�B� Bw�Bv�Bq�Bn�Bk�BbNBXBL�BH�BA�B.B �B{BB��B�B��B�}B�B��B�1By�Bm�B_;BK�B6FB)�B$�B&�B�B%B
�B
�!B
��B
�1B
n�B
hsB
[#B
H�B
@�B
:^B
0!B
"�B
�B
�B
%B	�B	�/B	��B	ȴB	�XB	�dB	�fB	�yB	�HB	�)B	�;B	�)B	�B	��B	ĜB	��B	�qB	�FB	�B	��B	��B	��B	�uB	�uB	�PB	�+B	�B	n�B	YB	P�B	G�B	9XB	.B	'�B	"�B	�B	�B	oB	\B	DB	B	B��B��B��B��B�B�B�5B��B�qB�B�B��B��B��B��B��B�uB�hB�DB�%B�B�B�%B�+B�1B�=B�%B�B� B|�B{�B~�B~�B~�B|�B|�By�Bu�Bl�BffBcTB\)BW
BT�BQ�BO�BJ�BC�BK�BL�BN�BN�BL�BH�BD�BB�BC�BC�BG�BI�BI�BM�BP�BS�BT�BS�BO�BP�BM�BN�BK�BI�BJ�BC�BB�B?}B?}B?}B<jB9XB;dB9XB8RB7LB6FB:^B:^B8RB8RB9XB8RB:^B9XB9XB49BF�BI�BQ�BVBW
B\)BdZBp�Bw�B�%B�JB��B��B�'B�qB��B�B�sB	%B	{B	�B	$�B	1'B	6FB	D�B	W
B	r�B	{�B	�B	�\B	��B	��B	��B	��B	�-B	�qB	ÖB	ƨB	��B	��B	�B	�B	�/B	�NB	�`B	�sB	�B	�B	�B	��B	��B	��B	��B	��B	��B
B
B
B
+B
+B

=B
JB
PB
\B
hB
{B
�B
�B
�B
�B
�B
�B
�B
 �B
!�B
"�B
#�B
%�B
&�B
&�B
(�B
)�B
)�B
,B
-B
.B
1'B
1'B
2-B
33B
6FB
7LB
8RB
:^B
:^B
<jB
<jB
=qB
>wB
@�B
A�B
A�B
C�B
D�B
E�B
E�B
E�B
F�B
I�B
J�B
J�B
L�B
L�B
M�B
N�B
N�B
O�B
O�B
P�B
R�B
R�B
T�B
S�B
T�B
W
B
VB
XB
YB
ZB
ZB
[#B
[#B
\)B
]/B
]/B
^5B
_;B
^5B
`BB
aHB
aHB
`BB
cTB
dZB
dZB
e`B
gmB
ffB
ffB
gmB
hsB
iyB
iyB
jB
jB
l�B
m�B
o�B
p�B
p�B
p�B
q�B
q�B
r�B
s�B
u�B
u�B
v�B
v�B
w�B
v�B
w�B
y�B
{�B
{�B
}�B
~�B
�B
�B
�B
�B
�B
�+B
�+B
�1B
�7B
�DB
�DB
�PB
�PB
�\B
�\B
�hB
�hB
�uB
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
�B
�B
�B
�B
�B
�'B
�-B
�3B
�3B
�3B
�?B
�?B
�?B
�?B
�FB
�LB
�LB
�RB
�RB
�RB
�XB
�XB
�^B
�^B
�XB
�XB
�XB
�^B
�XB
�XB
�XBG�BG�BF�BG�BH�BG�BH�BG�BG�BG�BH�BH�BF�BG�BG�BH�BH�BG�BH�BH�BH�BG�BH�BF�BF�BH�BH�BG�BI�BI�BF�BF�BG�BG�BF�BF�BG�BG�BH�BG�BG�BH�BH�BG�BG�BF�BF�BG�BG�BF�BG�BG�BG�BF�BG�BG�BH�BH�BH�BH�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                          BG�BH�BG�BH�BF�BF�BG�BH�BH�BH�BF�BG�BG�BH�BH�BH�BG�BG�BF�BF�BD}BE�BD~BE�BF�BE�BE�BC{BBuBBuBBvBAqBC~B@lB8;B@lBK�BP�BM�BU�BV�B[BkqBm~Bo�Bp�Bt�Bv�Bx�Bz�Bw�Bs�By�B{�B|�B{�B� B�+B�9B�?B�LB�SB�YB�`B�sB��B��B��B��B��B��B��B�~B�lB�fB�`B�gB�IB�%B�%B�B�Bw�Bv�Bq�Bn�Bk�BbSBXBL�BH�BA�B.B �B�B'B��B�B�B��B�B��B�<By�Bm�B_GBK�B6SB*
B$�B&�B�B4B
�B
�1B
��B
�BB
n�B
h�B
[5B
H�B
@�B
:qB
05B
"�B
�B
�B
;B	��B	�EB	�	B	��B	�pB	�|B	�B	�B	�bB	�CB	�VB	�DB	� B	��B	ĹB	��B	��B	�dB	�:B	��B	��B	��B	��B	��B	�rB	�MB	�<B	n�B	Y;B	Q	B	G�B	9}B	.9B	(B	"�B	�B	�B	�B	�B	mB	;B	6B�$B�B� B��B��B�B�bB�B��B�IB�1B�B�B��B��B��B��B��B�wB�XB�FB�FB�ZB�`B�gB�sB�\B�CB�8B}&B| B3B4B4B})B}*BzBv Bl�Bf�Bc�B\gBWIBU=BR,BPBKBC�BL	BMBOBOBMBH�BD�BB�BC�BC�BG�BJBJBNBQ.BTBBUHBTCBP*BQ1BNBO&BLBJBKBC�BB�B?�B?�B?�B<�B9�B;�B9�B8�B7�B6�B:�B:�B8�B8�B9�B8�B:�B9�B9�B4�BGBJBROBVjBWsB\�Bd�BqBxCB��B��B��B�iB��B��B�JB֐B�B	�B	B	PB	%xB	1�B	6�B	E?B	W�B	sZB	|�B	��B	�B	�BB	�pB	��B	��B	��B	�5B	�]B	�rB	͚B	ϨB	��B	��B	�B	�)B	�>B	�SB	�hB	�B	��B	��B	��B	��B	��B	��B	��B
B

B
B
/B
1B
FB
UB
]B
lB
{B
�B
�B
�B
�B
�B
�B
�B
�B
!�B
"�B
$ B
%B
'B
( B
(#B
*2B
+;B
+>B
-MB
.VB
/^B
2tB
2wB
3�B
4�B
7�B
8�B
9�B
;�B
;�B
=�B
=�B
>�B
?�B
A�B
B�B
B�B
EB
FB
GB
G B
G"B
H+B
K?B
LIB
LKB
NYB
N\B
OdB
PmB
PoB
QxB
QzB
R�B
T�B
T�B
V�B
U�B
V�B
X�B
W�B
Y�B
Z�B
[�B
[�B
\�B
\�B
]�B
^�B
^�B
_�B
aB
`B
bB
cB
cB
bB
e0B
f9B
f;B
gDB
iSB
hOB
hQB
i[B
jdB
klB
koB
lwB
lzB
n�B
o�B
q�B
r�B
r�B
r�B
s�B
s�B
t�B
u�B
w�B
w�B
x�B
x�B
y�B
x�B
y�B
|B
~B
~B
�0B
�;B
�TB
�YB
�mB
�rB
�B
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
�B
�B
�#B
�0B
�<B
�AB
�MB
�ZB
�fB
�mB
�~B
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
�B
�B
�B
�$B
�@B
�UB
�kB
�B
��B
��B
��B
��B
��B
�B
�B
�*B
�9B
�UB
�eB
�sB
��B
��B
��B
��B
��B
��B
��B
�B
�B
�+B
�;B
�8B
�:B
�=B
�GB
�DB
�GB
�JBG�BG�BF�BG�BH�BG�BH�BG�BG�BG�BH�BH�BF�BG�BG�BH�BH�BG�BH�BH�BH�BG�BH�BF�BF�BH�BH�BG�BI�BI�BF�BF�BG�BG�BF�BF�BG�BG�BH�BG�BG�BH�BH�BG�BG�BF�BF�BG�BG�BF�BG�BG�BG�BF�BG�BG�BH�BH�BH�BH�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                          <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�201808160702512021061413553320210614135533202106171312372021061713123720210617131237201808160702512021061413553320210614135533202106171312372021061713123720210617131237PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            new conductivity = original conductivity * (1 + delta*TEMP + CPcor_SBE*PRES) / (1 + delta*TEMP_ADJUSTED + CPcor_new*PRES_ADJUSTED)                                                                                                                              none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            new conductivity = original conductivity * (1 + delta*TEMP + CPcor_SBE*PRES) / (1 + delta*TEMP_ADJUSTED + CPcor_new*PRES_ADJUSTED)                                                                                                                              none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.50 dbar                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            CPcor_new = -1.25e-07; CPcor_SBE = -9.57e-08; delta = 3.25e-06                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.50 dbar                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            CPcor_new = -1.25e-07; CPcor_SBE = -9.57e-08; delta = 3.25e-06                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                      none                                                                                                                                                                                                                                                            New conductivity computed by using a different CPcor value from that provided by Sea-Bird.                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            No significant salinity drift detected.                                                                                                                                                                                                                         Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                      none                                                                                                                                                                                                                                                            New conductivity computed by using a different CPcor value from that provided by Sea-Bird.                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            No significant salinity drift detected.                                                                                                                                                                                                                         PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            AO  AO  ARCAARCAADJPADJP                                                                                                                                        2018081607025120180816070251  IP  IP                                G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2018081607025120180816070251QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�5F03E           703E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2018081607025120180816070251QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�0               0               PM  PM  ARSQARSQCOWGCOWGV1.1V1.1                                                                                                                                2021061713150120210617131501IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                