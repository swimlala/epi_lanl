CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS     N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-09-26T00:02:50Z creation      
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
resolution        =���   axis      Z        �  ;�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 @  L�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  Q   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 @  b   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  fP   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  wH   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 @  �@   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 @  �x   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 @  è   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 @  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �    HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                   �   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                   �   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                   �   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                   �   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  � �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                       HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                   <   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    D   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar        d   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar        l   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�       t   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    |   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  �   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                   �   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  � �      � �Argo profile    3.1 1.2 19500101000000  20180926000250  20210722160153  5905738 5905738 US ARGO PROJECT                                                 US ARGO PROJECT                                                 GREGORY C. JOHNSON                                              GREGORY C. JOHNSON                                              PRES            TEMP            PSAL            PRES            TEMP            PSAL                    DD  AOAO7218                            7218                            2C  2C  DD  SOLO_D_MRV                      SOLO_D_MRV                      12001                           12001                           SBE603 01Dec16                  SBE603 01Dec16                  874 874 @؁�D�hR@؁�D�hR11  @؁�DDQ0@؁�DDQ0@6 l�-�@6 l�-��c���٩�c���٩11  GPS     GPS     Primary sampling: averaged [1Hz sampling by SBE-41CP averaged in 2-dbar bins]                                                                                                                                                                                   Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 AB  AA  AA  ?fff@ff@@  @�  @�  @�  @�  A33A��A$��A>ffA`  A�  A�  A���A�  A���A���A�  A�  B   BffBffB  B ffB(��B/��B7��B@  BH��BP  BX  B`  Bh  Bp��BxffB�33B�33B�33B�33B�  B�33B�  B�  B�ffB�  B�  B�  B�  B�  B�33B�ffB�  B�33B�33B�33B���B�ffB�  B�  B���B���B�33B�  B���B�ffB�33B���C 33C�C  CL�CL�C
33C33C33C�C  C  C�fC33CL�C33C33C 33C"33C$  C&  C(  C*  C,  C.33C0L�C1�fC4  C6  C8�C:�C<�C>L�C?�fCB  CD  CF�CH�CJ33CL33CN�CP�CR�CT33CV�CX33CZ  C\  C]�fC`33Cb�Cd  CfL�Ch33Cj�Cl�Cn  Cp  Cq�fCt33Cv�Cw�fCz�C|  C}�fC��C�  C��3C��C�  C��fC��C�&fC��C��C�&fC��C��3C��C��C�  C�ٚC��3C�  C��C��C�&fC��C��3C��C��C��3C��C��C��fC�  C��C�&fC��C�  C��C�&fC��C��fC��3C�  C��C�&fC�  C��fC��C��C�  C��fC��3C�  C��C�&fC��C��fC��3C�  C�&fC��C��3C�  C��C��C�33C��C��3C��C�&fC��C��3C�  C��C�&fC��C�  C��C�&fC��C��3C��C��C�  C��fC�  C��C�33C��C��3C��C�&fC��C��3C��C�&fC��C�  C��C�  C�  C��C��C��3C��C��C��3C��C��C�  C��fC�  C��C�  C��3C�  C��C��C��3C��C��C��C�&fC�  C�ٚC�33C�@ C�ٚC��fC�33D �D l�D ��Ds3D��D�3D	  Dl�DٚDS3D��D` D�3D��D9�D�fD"��D%&fD'��D*ffD,��D/� D2fD4�fD7L�D9��D<��D?�DA�fDD9�DF� DIS3DK�3DN` DP�fDSS3DU�3DXL�DZ�fD]L�D_�fDbFfDdٚDgY�Di� DlY�Dn�3DqS3Ds��Dv@ Dx��D{&fD}9�D�3D��D�\�D��3D���D�6fD�s3D�� D���D�33D�|�D��fD���D�<�D�|�D�� D�  D�I�D��fD���D���D�  D�VfD��fD��3D��3D��D�33D�S3D�y�D���D��3D�� D��D�FfD�l�D���D���D�� D��3D��D�FfD�ffD�vfD��3D��3D�� D��D�	�D�&fD�FfD�ffD��3D��3D���D�ٚD��3D� D�9�D�c3Dƀ DǠ Dȼ�D�� D�  D��D�<�D�` D�|�DУ3D��fD��3D���D��D�<�D�ffD؆fDٳ3D���D�3D�0 D�\�Dߌ�D��D�� D�)�D�Y�D��D�� D���D�&fD�Y�D�fD� D��fD�  D�)�D�VfD�vfD�fD���D�ٚD���D�fD�)�D�C3D�C3D�Y�D�s3D���D���E VfE ٚE^fE�fEc3E�fEk3E�3Ek3E��Ek3E� E�E�3E	Q�E
�fE�3E( E3E��E�fE�fEP E� E�3E9�E;3E��E�fEL�EVfE��E�E!  E"��E#��E$� E&FfE'I�E(�3E)ٚE*��E,k3E-i�E.� E/��E1X E2��E3�fE5 E6y�E7i�E8�fE:.fE;��E<p E?~fEB�3EE�fEI  EL,�EOP ER[3EUp EXy�E[�E^�3Ea�fEeI�EhQ�EkD�En��Eq�fEt��Ew��E{3E~>fE�� E�:fE��3E�D�E���E�]�E��fE�P E���E��3E�4�E�vfE���E�3E�x�E���E��E�Y�E��fE�� E�` E�� E� E�D�E���E��3E�+3E��3E�ɚE�'3E�d�E�� E�3E�Y�E��fE���E�O3E�� E��3E�:fE�� E�� ?��?   ?   ?��?��?��?��?��?L��?333?L��?fff?�  ?���?���?�ff?�  ?�  ?�ff?�33@ff@33@   @,��@Fff@S33@l��@�  @���@�33@�  @���@�ff@�ff@�33@�  @���@���AffA��AffAffA$��A+33A4��A;33AA��AI��AQ��AX  A`  AfffAl��At��A{33A���A���A���A���A�  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111441444414111111141111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     ?�33@&ff@`  @�  @�  @�  @�  A33A��A,��AFffAh  A�  A�  A���A�  A���A���A�  A�  B  B
ffBffB  B"ffB*��B1��B9��BB  BJ��BR  BZ  Bb  Bj  Br��BzffB�33B�33B�33B�33B�  B�33B�  B�  B�ffB�  B�  B�  B�  B�  B�33B�ffB�  B�33B�33B�33B���B�ffB�  B�  B���B���B�33B�  B���B�ffB�33B���C �3C��C� C��C��C
�3C�3C�3C��C� C� CffC�3C��C�3C�3C �3C"�3C$� C&� C(� C*� C,� C.�3C0��C2ffC4� C6� C8��C:��C<��C>��C@ffCB� CD� CF��CH��CJ�3CL�3CN��CP��CR��CT�3CV��CX�3CZ� C\� C^ffC`�3Cb��Cd� Cf��Ch�3Cj��Cl��Cn� Cp� CrffCt�3Cv��CxffCz��C|� C~ffC�L�C�@ C�33C�L�C�@ C�&fC�L�C�ffC�Y�C�L�C�ffC�L�C�33C�L�C�Y�C�@ C��C�33C�@ C�L�C�L�C�ffC�L�C�33C�L�C�Y�C�33C�L�C�L�C�&fC�@ C�Y�C�ffC�L�C�@ C�Y�C�ffC�L�C�&fC�33C�@ C�L�C�ffC�@ C�&fC�L�C�Y�C�@ C�&fC�33C�@ C�L�C�ffC�L�C�&fC�33C�@ C�ffC�L�C�33C�@ C�Y�C�Y�C�s3C�Y�C�33C�L�C�ffC�L�C�33C�@ C�Y�C�ffC�Y�C�@ C�L�C�ffC�Y�C�33C�L�C�Y�C�@ C�&fC�@ C�L�C�s3C�Y�C�33C�L�C�ffC�L�C�33C�L�C�ffC�Y�C�@ C�Y�C�@ C�@ C�Y�C�L�C�33C�Y�C�L�C�33C�L�C�Y�C�@ C�&fC�@ C�Y�C�@ C�33C�@ C�Y�C�L�C�33C�L�C�Y�C�Y�C�ffC�@ C��C�s3C�� C��C�&fC�s3D 9�D ��D�D�3D�D�3D	  D��D��Ds3D��D� D3D��DY�D fD"��D%FfD'��D*�fD-�D/� D2&fD4�fD7l�D:�D<��D?9�DA�fDDY�DF� DIs3DK�3DN� DQfDSs3DU�3DXl�DZ�fD]l�D_�fDbffDd��Dgy�Dj  Dly�Dn�3Dqs3Ds��Dv` DxٚD{FfD}Y�D�3D�)�D�l�D��3D���D�FfD��3D�� D���D�C3D���D��fD��D�L�D���D�� D� D�Y�D��fD���D���D�0 D�ffD��fD��3D��3D��D�C3D�c3D���D���D��3D�  D�,�D�VfD�|�D���D���D�� D�3D�,�D�VfD�vfD��fD��3D��3D�� D���D��D�6fD�VfD�vfD��3D��3D���D��D�3D�  D�I�D�s3DƐ Dǰ D���D�� D� D�,�D�L�D�p Dό�Dг3D��fD��3D��D�)�D�L�D�vfDؖfD��3D���D�3D�@ D�l�Dߜ�D���D�  D�9�D�i�D��D�� D�	�D�6fD�i�D�fD�� D��fD� D�9�D�ffD�fD�fD�ɚD��D�	�D�&fD�9�D�S3D�S3D�i�D��3D���D���E ^fE �EffE�fEk3E�fEs3E�3Es3E��Es3E� E�E�3E	Y�E
�fE�3E0 E#3E��E�fE�fEX E� E�3EA�EC3E��E�fET�E^fE��E��E! E"��E#��E$� E&NfE'Q�E(�3E)�E*��E,s3E-q�E.� E/��E1` E2��E3�fE5  E6��E7q�E8�fE:6fE;��E<x E?�fEB�3EE�fEI EL4�EOX ERc3EUx EX��E[��E^�3Ea�fEeQ�EhY�EkL�En��Eq�fEt��Ew��E{3E~FfE�� E�>fE��3E�H�E���E�a�E��fE�T E���E��3E�8�E�zfE���E�3E�|�E���E��E�]�E��fE�  E�d E�� E� E�H�E���E��3E�/3E��3E�͚E�+3E�h�E�� E�3E�]�E��fE���E�S3E�� E��3E�>fE�� E�� G�O�G�O�?�  G�O�G�O�G�O�G�O�?���G�O�?���?�ff?�33?�  ?���?ٙ�?�ffG�O�@   @33@��@&ff@333@@  @L��@fff@s33@�ff@�  @���@�33@�  @���@�ff@�ff@�33@�  @���AffAffA��AffA&ffA,��A333A<��AC33AI��AQ��AY��A`  Ah  AnffAt��A|��A���A���A���A���A���A�  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111441444414111111141111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     @ %@ V@ {@ O@ ""@ (�@ /�@ 7�@ >@ FQ@ Q=@ _�@ m:@ z�@ �7@ �0@ ��@ �-@ �&@ ��@ �t@ ��@ �q@j@�@ @+�@9X@G�@V�@b�@p�@~K@��@��@��@��@�>@��@ލ@�@��@�@{@#�@/�@=q@K@X�@ff@t�@�@�\@��@�Y@�@�J@��@�H@��@��@	�@�@%�@2�@B8@O0@[z@k.@x&@�@��@�z@�r@�@��@׹@�@�Y@�Q@@[@*S@7�@E�@SI@_�@m:@z�@��@�0@�5@��@�w@��@�t@��@�q@@@
@,`@:@H]@V@dZ@r@~�@��@�H@��@��@��@�7@��@��@��@�@{@$.@1'@>@K�@X�@ff@s_@�@�@�U@�Y@�R@�J@�O@�H@�@@�E@
=@�@&;@5?@B8@O0@^5@j@v�@��@�$@�m@�@�@�c@׹@�`@�e@ �@�@�@*S@5�@D�@R�@^5@m:@|?@��@��@��@��@�2@�|@�@�m@��@	@	@	�@	+@	:�@	I@	UU@	a�@	o�@	~K@	��@	��@	��@	��@	��@	�7@	��@	�4@	�~@
�@
�@
#�@
2�@
>�@
Ji@
Yn@
hs@
t�@
�@
�\@
�a@
��@
��@
��@
�O@
�T@
�L@
��@
�@B@%�@1�@@�@O0@^�@k.@v�@��@��@�@�f@�k@�o@�h@�@�@  @�@�@)�@5�@E�@R�@^�@m�@|?@��@��@��@��@�&@�@�t@�y@�q@�@�@ @-�@<@G�@SI@e�@t@|?@��@�U@��@��@��@��@��@^5@�@�;@!s@e	@��@��@5?@|?@�J@V@V�@��@�@-@s_@�R@�E@D�@�P@��@�@bN@��@�@@33@y�@��@j@H]@��@��@o@V@��@ލ@"�@i!@�f@�Y@5�@y�@��@^@DD@��@�@�@FQ@��@ψ@*@[z@��@�`@(�@l�@�-@�~@;d@�@�J@	�@N�@��@�@ �@ ^�@ ��@ �H@!#�@!dZ@!��@!�`@"$�@"c�@"�@"��@#
@#\�@#�@#�/@$�@$[z@$�<@$�\@%{@%R�@%�@%є@&@&I@&��@&�>@'  @'<@'y�@'�F@'�@(1'@(m�@(�Y@(�m@)$/@)`A@)�@)܀@*�@*X�@*�0@*��@+@+N�@+�D@+ȴ@,�@,C�@,�d@,��@,�E@-9X@-v@-�9@-�@.1'@.qS@.��@.�@//�@/o�@/��@/�@033@0v@0��@0�~@1:@1|�@1�@1��@2>�@2~K@2�@2��@3<@3|?@3��@3� @45@@4r�@4�!@4��@5'�@5c�@5�H@5խ@6�@6M$@6��@6��@6��@72�@7i!@7��@7��@8o@8I@8�@8��@8��@9""@9�@9��@:��@;5�@;�a@<:@<��@=:�@=�[@><�@>�t@?z�@?�@@��@@�@A��@B%@B�!@C!s@C�o@D> @D�~@E`B@E�O@FG�@F�@G`B@H1@Hx&@H�(@I��@I�E@J�m@KJ@K��@LFQ@L�f@MDD@M�#@NA�@N��@Oo�@P@Pff@Q�9@S @T^�@U@W[@Xt@Y��@[@\\�@]׹@_ @`bN@a�O@cg@da�@e�C@g$�@hr�@i�^@k @lz2@m@o �@ps_@q�@s*@t]�@u��@v�@v>�@vv�@v�@w@wR�@w�C@wލ@x*@xg@x�a@x��@y(�@y~K@y��@z
=@zA�@zx&@z�@{%@{X@{�P@{�/@|�@|_�@|�f@|�@}1�@}g@}�9@~  @~2�@~|�@~��@G�O�G�O�@ jG�O�G�O�G�O�G�O�@ G�O�@ �@ v@ %@ �@ �@ 1@ �G�O�@ 
=@ J@ �@ V@ �@ @ o@ *@ �@ B@ O@ [@ g@ ""@ $�@ &�@ *S@ -@ /�@ 2�@ 5�@ 9X@ <@ @,@ C�@ FQ@ I@ M$@ O�@ R�@ V@ Yn@ \)@ _�@ bN@ e	@ hs@ k.@ n�@ r@ t�@ x&@ z�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A��/A��#A��mA��A��mA��A��A���A���A�A���A�1A�
=A���A���A���A�JA�VA�bA�VA�oA�{A��A��A��A���A��A��#A�9XA��A�^5Aħ�A�p�A�K�A�jA�dZA���A��A��A�x�A���A�hsA��#A��mA�33A��A��FA�ffA���A�O�A���A�?}A���A��uA�Q�A��A��FA���A�x�A��HA�l�A�1A�$�A��A��PA�
=A�x�A�(�A�oA��TA�dZA��A���A��A�  A��^A�;dA�jA�ĜA�&�A��A� �A���A��RA�r�A���A�33A�(�A��!A��+A��A��!A�v�A��A��jA��hA��A�A�hsA���A���A�XA�%A���A�&�A�A��uA��RA�7LA��HA���A��A�G�A�+A��DA�+A�ffA���A�5?A�`BA�K�A�33A��A���A��A�33A��DA��A�wA}��A{x�AxAuApn�An�DAm33Ah�uAdM�Ab{A_7LA]�FA]VA\�A\A�A[�mA[A[��A[�AZ��AY��AU�AO�AL��AK�AI�AG�AF{AD�jADA@��A>jA=?}A;�A:ĜA9�hA8��A7dZA3A1l�A0�DA0E�A0bA/��A/A-�FA,9XA*$�A)33A'�PA&��A&VA%�A$�\A$n�A$-A#�TA#�FA"�A��A"�A�`A\)A��AdZA^5AC�A�!A�A9XA�hA��A�TA�A�`AC�AbNA=qA�A��A\)A�!A�A
��A	C�Az�A��A��A�A�AhsA��AM�AAG�A �/A �@�o@��@��F@�dZ@�
=@�5?@��^@�Q�@���@�@��@�S�@�
=@�+@�J@��`@��@���@�v�@�V@�9X@��@���@�F@��@��@�V@�Z@� �@���@߮@�t�@�@�p�@�O�@܋D@�1'@��;@�
=@��;@��`@�b@�K�@��#@�K�@�x�@�V@��@�(�@�j@��H@�Ĝ@�|�@��F@���@�K�@�J@���@�&�@�dZ@�^5@���@�Q�@�Z@��P@��-@�hs@���@��9@�+@��\@�V@��@���@�@�I�@��
@�"�@��`@�A�@���@���@�ƨ@��y@�J@���@��@~�y@}p�@|I�@z��@w�P@vv�@t�D@q��@p��@n�y@l�@i�@gl�@e�h@d�j@c��@b�@a�7@_
=@\�j@[C�@Y�#@Y%@W
=@U�@U?}@SdZ@Q�#@Q%@O�@Nff@Mp�@KdZ@I��@F��@F{@Ep�@C��@A�^@@1'@?;d@>��@=�@;�
@:�H@9X@8bN@7K�@6�+@5�-@5`B@4��@4�@4Z@3��@3t�@3dZ@3o@2-@0��@0 �@/�@.ȴ@-/@,�@+�F@+@*n�@)�#@(�u@&ff@%�T@%?}@#S�@"M�@�@��@ff@@z�@�@�@�@��@�9@;d@�R@�+@�@�
@�H@��@X@�9@l�@5?@��@`B@��@��@Z@��@C�@
-@	hs@Ĝ@A�@|�@�y@�+@/@z�@�F@C�@33@@�\@�#@7L@ �9?���?�\)?��R?��?���?�$�?��
?�33?� �?��?�?�ƨ?�x�?�P?�?䛦?�M�?�hs?�5??�(�?�ƨ?�?���?�r�?�l�?ա�?Լj?ԛ�?�33?�o?���?У�?ϝ�?��?�5??͑h?͑h?̬?�j?�j?�dZ?��H?��?��#?ȓu?ǍP?�l�?�E�?�z�?��?�hs?��w?�5??�/?�I�?��?���?���?���?�=q?��#?���?��#?���?�^5?���?�^5?���?�C�?��?�1?��D?��?�O�?���?��?��?��?�5??�V?�V?�V?���?�v�?���?��R?��?���?��?�;d?�\)?�|�?���?��w?��w?��w?�  ?�A�?�A�?��?���?�Ĝ?��`?�%?�G�?�hs?��7?���?���?�JA��#A��/A��/A��HA��`A��HA��HA��/A��/A��#A��
A��
A��/A��TA��HA��
A��#A��
A���A��
A��
A��HA��HA��mA��mA��yA��A��A��A��`A��`A��A��A��A��;A��A���A��A���A�  A���A�A�A�A�  A���A���A���A���A�A�
=A�bA�VA�JA�
=A�%A�JA�A���A���G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     A��/A��#A��mA��A��mA��A��A���A���A�A���A�1A�
=A���A���A���A�JA�VA�bA�VA�oA�{A��A��A��A���A��A��#A�9XA��A�^5Aħ�A�p�A�K�A�jA�dZA���A��A��A�x�A���A�hsA��#A��mA�33A��A��FA�ffA���A�O�A���A�?}A���A��uA�Q�A��A��FA���A�x�A��HA�l�A�1A�$�A��A��PA�
=A�x�A�(�A�oA��TA�dZA��A���A��A�  A��^A�;dA�jA�ĜA�&�A��A� �A���A��RA�r�A���A�33A�(�A��!A��+A��A��!A�v�A��A��jA��hA��A�A�hsA���A���A�XA�%A���A�&�A�A��uA��RA�7LA��HA���A��A�G�A�+A��DA�+A�ffA���A�5?A�`BA�K�A�33A��A���A��A�33A��DA��A�wA}��A{x�AxAuApn�An�DAm33Ah�uAdM�Ab{A_7LA]�FA]VA\�A\A�A[�mA[A[��A[�AZ��AY��AU�AO�AL��AK�AI�AG�AF{AD�jADA@��A>jA=?}A;�A:ĜA9�hA8��A7dZA3A1l�A0�DA0E�A0bA/��A/A-�FA,9XA*$�A)33A'�PA&��A&VA%�A$�\A$n�A$-A#�TA#�FA"�A��A"�A�`A\)A��AdZA^5AC�A�!A�A9XA�hA��A�TA�A�`AC�AbNA=qA�A��A\)A�!A�A
��A	C�Az�A��A��A�A�AhsA��AM�AAG�A �/A �@�o@��@��F@�dZ@�
=@�5?@��^@�Q�@���@�@��@�S�@�
=@�+@�J@��`@��@���@�v�@�V@�9X@��@���@�F@��@��@�V@�Z@� �@���@߮@�t�@�@�p�@�O�@܋D@�1'@��;@�
=@��;@��`@�b@�K�@��#@�K�@�x�@�V@��@�(�@�j@��H@�Ĝ@�|�@��F@���@�K�@�J@���@�&�@�dZ@�^5@���@�Q�@�Z@��P@��-@�hs@���@��9@�+@��\@�V@��@���@�@�I�@��
@�"�@��`@�A�@���@���@�ƨ@��y@�J@���@��@~�y@}p�@|I�@z��@w�P@vv�@t�D@q��@p��@n�y@l�@i�@gl�@e�h@d�j@c��@b�@a�7@_
=@\�j@[C�@Y�#@Y%@W
=@U�@U?}@SdZ@Q�#@Q%@O�@Nff@Mp�@KdZ@I��@F��@F{@Ep�@C��@A�^@@1'@?;d@>��@=�@;�
@:�H@9X@8bN@7K�@6�+@5�-@5`B@4��@4�@4Z@3��@3t�@3dZ@3o@2-@0��@0 �@/�@.ȴ@-/@,�@+�F@+@*n�@)�#@(�u@&ff@%�T@%?}@#S�@"M�@�@��@ff@@z�@�@�@�@��@�9@;d@�R@�+@�@�
@�H@��@X@�9@l�@5?@��@`B@��@��@Z@��@C�@
-@	hs@Ĝ@A�@|�@�y@�+@/@z�@�F@C�@33@@�\@�#@7L@ �9?���?�\)?��R?��?���?�$�?��
?�33?� �?��?�?�ƨ?�x�?�P?�?䛦?�M�?�hs?�5??�(�?�ƨ?�?���?�r�?�l�?ա�?Լj?ԛ�?�33?�o?���?У�?ϝ�?��?�5??͑h?͑h?̬?�j?�j?�dZ?��H?��?��#?ȓu?ǍP?�l�?�E�?�z�?��?�hs?��w?�5??�/?�I�?��?���?���?���?�=q?��#?���?��#?���?�^5?���?�^5?���?�C�?��?�1?��D?��?�O�?���?��?��?��?�5??�V?�V?�V?���?�v�?���?��R?��?���?��?�;d?�\)?�|�?���?��w?��w?��w?�  ?�A�?�A�?��?���?�Ĝ?��`?�%?�G�?�hs?��7?���?���?�JA��#A��/A��/A��HA��`A��HA��HA��/A��/A��#A��
A��
A��/A��TA��HA��
A��#A��
A���A��
A��
A��HA��HA��mA��mA��yA��A��A��A��`A��`A��A��A��A��;A��A���A��A���A�  A���A�A�A�A�  A���A���A���A���A�A�
=A�bA�VA�JA�
=A�%A�JA�A���A���G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�BH�BH�BI�BG�BI�BG�BG�BH�BG�BG�BG�BG�BG�BG�BG�BG�BG�BG�BG�BG�BG�BG�BG�BG�BG�BF�BQ�BZBz�B�^B�B�)B�;B�B�B��BBBDBVB{B{B�B�B$�B%�B&�B)�B/B49B7LB=qB?}BA�BB�BG�BI�BJ�BL�BR�BXB[#B_;BhsBhsBffBhsBhsBffBe`Be`BaHB[#BZBW
BW
B^5B\)BP�BR�BVBe`BcTBcTBjB~�BjB9XB�B�B1BB��B�B�B�B�B��B�wB�LB�!B�B��B��B��B�Bv�BhsB_;BVBQ�BE�B�B�B�B
=B  B
�B
�B
�/B
�)B
�)B
�)B
��B
��B
��B
��B
�JB
s�B
iyB
M�B
5?B
�B	�yB	�B	ÖB	�{B	v�B	ffB	Q�B	I�B	E�B	@�B	=qB	;dB	:^B	9XB	9XB	49B	'�B	\B�B�ZB�B��BɺBĜB�wB�jB�-B�qB�FB�-B�B��B��B��B�bB�PB�=B�1B�B�B� B{�Bv�Br�Bn�BiyBiyBhsBdZBe`BdZBdZBdZBe`B^5B^5B\)BYBQ�BQ�BO�BK�BK�BK�BK�BJ�BI�BG�BI�BI�BH�BF�BE�BD�BC�BA�BB�B>wB>wB9XB;dB9XB7LB5?B33B49B49B5?B49B2-B33B49B49B49B6FB9XB:^B9XB;dB:^B;dB;dB2-B9XB6FB49B49B33B33B9XB:^B:^BA�BD�BD�BD�BE�BL�BW
B]/BaHBbNBbNBcTBdZBe`BiyBhsBiyBl�BjBm�B~�B�7B�{B��B�B�BB�fB�)B�B�BB��B	 �B	/B	1'B	49B	H�B	P�B	W
B	YB	e`B	r�B	w�B	}�B	�B	�%B	�\B	�oB	��B	��B	��B	�B	�'B	�dB	��B	�}B	ÖB	��B	��B	��B	�/B	�BB	�fB	�B	�B	�B	��B	��B	��B	��B	��B	��B
B
%B
+B

=B
DB
PB
hB
uB
�B
�B
�B
�B
�B
�B
�B
�B
"�B
$�B
$�B
%�B
)�B
+B
,B
.B
0!B
1'B
0!B
2-B
49B
6FB
7LB
:^B
9XB
;dB
=qB
>wB
@�B
A�B
A�B
C�B
D�B
F�B
H�B
H�B
I�B
J�B
J�B
K�B
L�B
L�B
L�B
M�B
M�B
N�B
M�B
O�B
O�B
P�B
P�B
R�B
S�B
T�B
VB
VB
VB
W
B
YB
\)B
\)B
\)B
^5B
^5B
`BB
`BB
aHB
aHB
cTB
bNB
cTB
dZB
ffB
gmB
hsB
hsB
gmB
iyB
jB
jB
l�B
m�B
m�B
o�B
q�B
r�B
q�B
q�B
r�B
r�B
s�B
s�B
u�B
v�B
u�B
w�B
w�B
y�B
y�B
{�B
|�B
}�B
}�B
|�B
}�B
~�B
~�B
� B
� B
�B
�B
�B
�B
�%B
�+B
�7B
�7B
�DB
�JB
�PB
�VB
�bB
�hB
�oB
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
�B
��B
�B
�B
�B
�B
�'B
�-B
�-B
�9B
�?B
�FB
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
�^B
�^B
�dB
�dB
�dB
�dB
�dB
�jB
�dB
�jB
�jB
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
�jB
�dB
�jB
�jB
�jB
�qB
�jB
�jB
�jB
�jB
�jB
�jB
�qB
�jB
�jB
�qB
�jB
�jB
�qB
�jBH�BG�BH�BH�BI�BH�BG�BH�BE�BG�BF�BH�BI�BH�BH�BJ�BF�BG�BH�BH�BH�BG�BH�BH�BH�BJ�BG�BH�BE�BG�BI�BJ�BF�BG�BG�BG�BG�BG�BH�BE�BH�BG�BG�BG�BG�BG�BG�BH�BH�BG�BG�BG�BF�BG�BG�BG�BH�BF�BG�BH�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     BF�BF�BG�BE�BG�BE�BE�BF�BE�BE�BE�BE�BE�BE�BE�BE�BE�BE�BE�BE�BE�BE�BE�BE�BE�BD�BO�BXBx�B�RB�
B�B�/B�B�B��B��BB	7BJBoBoB�B�B"�B#�B$�B'�B-B2-B5?B;dB=qB?}B@�BE�BG�BH�BJ�BP�BVBYB]/BffBffBdZBffBffBdZBcTBcTB_;BYBXBT�BT�B\)BZBN�BP�BS�BcTBaHBaHBhsB|�BhsB7LB�B{B%B  B��B�B�B�B�
B��B�jB�?B�B��B��B��B�uB� Bt�BffB]/BS�BO�BC�B�B�B�B1B
��B
�B
�B
�#B
�B
�B
�B
��B
��B
�wB
��B
�=B
q�B
gmB
K�B
33B
{B	�mB	�
B	��B	�oB	t�B	dZB	O�B	G�B	C�B	>wB	;dB	9XB	8RB	7LB	7LB	2-B	%�B	PB�B�NB�
B��BǮBB�jB�^B�!B�dB�9B�!B�B��B��B��B�VB�DB�1B�%B�B�B}�By�Bt�Bp�Bl�BgmBgmBffBbNBcTBbNBbNBbNBcTB\)B\)BZBW
BO�BO�BM�BI�BI�BI�BI�BH�BG�BE�BG�BG�BF�BD�BC�BB�BA�B?}B@�B<jB<jB7LB9XB7LB5?B33B1'B2-B2-B33B2-B0!B1'B2-B2-B2-B49B7LB8RB7LB9XB8RB9XB9XB0!B7LB49B2-B2-B1'B1'B7LB8RB8RB?}BB�BB�BB�BC�BJ�BT�B[#B_;B`BB`BBaHBbNBcTBgmBffBgmBjBhsBk�B|�B�+B�oB��B�
B�5B�ZB�B�
B�5B��B	�B	-B	0!B	33B	G�B	O�B	VB	XB	dZB	q�B	v�B	|�B	� B	�B	�VB	�hB	��B	��B	��B	�B	�!B	�^B	�}B	�wB	B	ɺB	��B	��B	�)B	�;B	�`B	�yB	�B	�B	��B	��B	��B	��B	��B	��B
B
B
%B
	7B

=B
JB
bB
oB
�B
�B
�B
�B
�B
�B
�B
�B
!�B
#�B
#�B
$�B
(�B
)�B
+B
-B
/B
0!B
/B
1'B
33B
5?B
6FB
9XB
8RB
:^B
<jB
=qB
?}B
@�B
@�B
B�B
C�B
E�B
G�B
G�B
H�B
I�B
I�B
J�B
K�B
K�B
K�B
L�B
L�B
M�B
M�B
O�B
O�B
P�B
P�B
R�B
S�B
T�B
VB
VB
VB
W
B
YB
\)B
\)B
\)B
^5B
^5B
`BB
`BB
aHB
aHB
cTB
bNB
cTB
dZB
ffB
gmB
hsB
hsB
gmB
iyB
jB
jB
l�B
m�B
m�B
o�B
q�B
r�B
q�B
q�B
r�B
r�B
s�B
s�B
u�B
v�B
u�B
w�B
w�B
y�B
y�B
{�B
|�B
}�B
}�B
|�B
}�B
~�B
~�B
� B
� B
�B
�B
�B
�B
�%B
�+B
�7B
�7B
�DB
�JB
�PB
�VB
�bB
�hB
�oB
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
�B
�B
�B
�B
�B
�B
�!B
�-B
�9B
�9B
�FB
�LB
�RB
�RB
�XB
�XB
�^B
�^B
�^B
�dB
�dB
�jB
�jB
�jB
�jB
�wB
�wB
�wB
�wB
�wB
�}B
�wB
�}B
�}B
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
�}B
�wB
�}B
�}B
�}B
��B
�}B
�}B
�}B
�}B
�}B
�}B
��B
�}B
�}B
��B
�}B
�}B
��B
�}BF�BE�BF�BF�BG�BF�BE�BF�BC�BE�BD�BF�BG�BF�BF�BH�BD�BE�BF�BF�BF�BE�BF�BF�BF�BH�BE�BF�BC�BE�BG�BH�BD�BE�BE�BE�BE�BE�BF�BC�BF�BE�BE�BE�BE�BE�BE�BF�BF�BE�BE�BE�BD�BE�BE�BE�BF�BD�BE�BF�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�201809260002502021061413523820210614135238202106141746432021061417464320210614174643201809260002502021061413523820210614135238202106141746432021061417464320210614174643PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            new conductivity = original conductivity * (1 + delta*TEMP + CPcor_SBE*PRES) / (1 + delta*TEMP_ADJUSTED + CPcor_new*PRES_ADJUSTED)                                                                                                                              none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS                                                                                                                                                                                                                                       PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            new conductivity = original conductivity * (1 + delta*TEMP + CPcor_SBE*PRES) / (1 + delta*TEMP_ADJUSTED + CPcor_new*PRES_ADJUSTED)                                                                                                                              none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS                                                                                                                                                                                                                                       surface_pressure=-0.50 dbar                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            CPcor_new = -1.25e-07; CPcor_SBE = -9.57e-08; delta = 3.25e-06                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            OW r = 0.9999 (+/-0), vertically averaged dS = -0.002 (+/-0)                                                                                                                                                                                                    surface_pressure=-0.50 dbar                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            CPcor_new = -1.25e-07; CPcor_SBE = -9.57e-08; delta = 3.25e-06                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            OW r = 0.9999 (+/-0), vertically averaged dS = -0.002 (+/-0)                                                                                                                                                                                                    Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                      none                                                                                                                                                                                                                                                            New conductivity computed by using a different CPcor value from that provided by Sea-Bird.                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Significant salinity sensor drift detected. OW least squares fit adopted.                                                                                                                                                                                       Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                      none                                                                                                                                                                                                                                                            New conductivity computed by using a different CPcor value from that provided by Sea-Bird.                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Significant salinity sensor drift detected. OW least squares fit adopted.                                                                                                                                                                                       PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            AO  AO  ARCAARCAADJPADJP                                                                                                                                        2018092600025020180926000250  IP  IP                                G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2018092600025020180926000250QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�5F03E           703E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2018092600025020180926000250QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�0               0               PM  PM  ARSQARSQCOWGCOWGV1.1V1.1                                                                                                                                2021072216015320210722160153IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                