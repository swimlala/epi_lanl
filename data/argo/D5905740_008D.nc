CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS     N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-07-24T22:03:00Z creation      
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
resolution        =���   axis      Z        h  ;�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   L<   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     h  PX   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   `�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     h  d�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     h  uD   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   ��   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     h  ��   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   �0   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     h  �L   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     h  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   �   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     h  �8   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   Ӡ   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     h  ׼   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                   �   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                   �   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                   �   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                   �   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  � �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                   ,   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                   H   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    P   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar        p   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar        x   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�       �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  �$   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  � �Argo profile    3.1 1.2 19500101000000  20180724220300  20210722161418  5905740 5905740 US ARGO PROJECT                                                 US ARGO PROJECT                                                 GREGORY C. JOHNSON                                              GREGORY C. JOHNSON                                              PRES            TEMP            PSAL            PRES            TEMP            PSAL                  DD  AOAO7220                            7220                            2C  2C  DD  SOLO_D_MRV                      SOLO_D_MRV                      12003                           12003                           SBE603 01Dec16                  SBE603 01Dec16                  874 874 @�k�u p�@�k�u p�11  @�k�l��@�k�l��@*h�s�@*h�s��cM[W>�6�cM[W>�611  GPS     GPS     Primary sampling: averaged [1Hz sampling by SBE-41CP averaged in 2-dbar bins]                                                                                                                                                                                   Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 AB  AA  AA  ?���@   @@  @�  @�  @�  @�  A   A  A#33A>ffA`  A���A���A�  A�33A���Aљ�AᙚA�B ffBffB  B  B��B'33B0  B8ffB@  BG��BO33BW��B`  Bh��Bp��BxffB�33B�ffB�ffB�33B�  B���B�  B�33B�ffB�  B���B�  B�33B�33B�33B�33B�33B�  B�  B�ffB�33B�33B�  B�33B�33B�33B�33B�33B�33B�  B���B�ffC 33C33C�C  C33C
  C�fC�C�C33CL�C  C�3C�C�C  C L�C"L�C$�C%�3C'��C)��C+��C-��C/��C1��C3��C5�fC7�fC:  C<  C>�C@33CBL�CD  CE��CG��CI��CK��CN�CP�CR�CTL�CVffCXL�CZL�C\L�C^L�C`L�Cb33Cd33Cf�Cg�fCj�ClL�Cn�Cp  Cq�fCt  Cv�Cx  Cy�fC|  C~33C�fC�  C�  C��C��C�  C��C��C��3C�  C��C��3C��C�&fC�  C��3C�  C��C��C�  C�  C��3C��fC�  C�33C�&fC��C��C��C��C��C�  C��3C�  C��C��C��C��C�  C��C��C��C��C�  C��C�  C��3C��3C�  C�  C��C��C��C��C�&fC�&fC�&fC��C��fC��fC��3C��3C�  C�  C�  C��C��C��C�&fC��C��fC��fC��3C�  C�  C��C��C��C��C�&fC�&fC�  C��fC��3C�  C��C��C�&fC��C��fC��3C�  C��C��C��C��fC�ٚC�  C��3C�  C��C��C��C�&fC�&fC��C�ٚC��fC��fC��fC��fC��fC��fC��fC��3C��3C�  C��3C��3C��3C�  C�  C�  C�  C�  C��3C�  C��3D   D s3D ��Dy�D�3D�fD��DY�D� D3Dl�D� D&fDy�D�fD  D � D"� D%9�D'�3D)��D,33D.s3D0�fD2��D5FfD7� D9�fD<fD>L�D@� DB��DES3DG� DJfDLS3DN�fDQfDSl�DU��DX33DZ��D\�3D_Y�Da�3Dd9�Df��Dh��Dkl�Dm�fDp` Dr�3Du@ Dw��Dz�D|fD~ffD�ffD���D��3D��fD�#3D�P D�p D��fD���D���D�  D��D�,�D�L�D�ffD�|�D��fD���D�� D���D��fD��fD��fD��3D�� D���D��3D��fD�� D���D���D���D���D��3D���D�� D���D���D��3D���D�� D��3D�� D�� D�� D�� D��fD���D�	�D�fD�)�D�6fD�FfD�Y�D�c3D�vfD��3D���D�� D��3D���D��fD��fDfDÖfDĐ Dŉ�Dƀ D�p D�` D�S3D�<�D�&fD�3D���D��D��3Dϼ�DЦfDѐ D�|�D�s3D�l�D�c3D�Y�D�L�D�FfD�I�D�L�D�I�D�FfD�FfD�I�D�L�D�S3D�L�D�I�D�P D�FfD�I�D�L�D�I�D�L�D�I�D�L�D�I�D�P D�S3D�\�D�\�D�` D�c3D�c3D�` D�ffD�l�D�s3D�l�D�p D�p D�i�D�Y�D�VfD�VfE $�E� E�E� El�Ea�E�3E	0 E
$�E��E��E� EH E�fE�fE�3EfEL�E�fEfEC3EvfE��E�E�ENfE � E!ٚE#( E$��E%�fE'&fE(	�E)ffE*�3E, E-T�E.��E/� E1�E2H E3vfE4�fE63E7)�E8�fE9��E:�3E<)�E?<�EB8 EEY�EH��EK� EN� EQ�3EU,�EXl�E[9�E^~fEa�3Ed�fEg��Ej� En�En�3EoX EoٚEp�fEqQ�Eq�fEr�3Er�3Es�fEt` Et� Eu|�Ev#3EvɚEwp Ex3Ex�fEyH Ey� ?�  ?L��?fff?�  ?fff?���?�  ?�  ?���?���?���?���?�  ?���?�  ?fff?���?�  ?���?�  ?�  ?���?���?���?���?�ff?�ff?�  ?���?�ff?�33@ff@��@&ff@333@@  @Y��@fff@s33@�33@���@�ff@�  @���@�ff@�ff@�33@�  @陚@���A33A33A33A33A!��A+33A1��A;33AC33AK33G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411414414414144141441441141111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   ?���@   @`  @�  @�  @�  @�  A  A  A+33AFffAh  A���A���A�  A�33A���Aՙ�A噚A���BffB
ffB  B  B!��B)33B2  B:ffBB  BI��BQ33BY��Bb  Bj��Br��BzffB�33B�ffB�ffB�33B�  B���B�  B�33B�ffB�  B���B�  B�33B�33B�33B�33B�33B�  B�  B�ffB�33B�33B�  B�33B�33B�33B�33B�33B�33B�  B���B�ffC �3C�3C��C� C�3C
� CffC��C��C�3C��C� C33C��C��C� C ��C"��C$��C&33C(L�C*L�C,L�C.L�C0L�C2L�C4L�C6ffC8ffC:� C<� C>��C@�3CB��CD� CFL�CHL�CJL�CLL�CN��CP��CR��CT��CV�fCX��CZ��C\��C^��C`��Cb�3Cd�3Cf��ChffCj��Cl��Cn��Cp� CrffCt� Cv��Cx� CzffC|� C~�3C�33C�@ C�@ C�L�C�Y�C�@ C�L�C�Y�C�33C�@ C�L�C�33C�Y�C�ffC�@ C�33C�@ C�Y�C�L�C�@ C�@ C�33C�&fC�@ C�s3C�ffC�Y�C�Y�C�L�C�L�C�L�C�@ C�33C�@ C�L�C�L�C�L�C�L�C�@ C�L�C�L�C�L�C�L�C�@ C�L�C�@ C�33C�33C�@ C�@ C�L�C�Y�C�Y�C�Y�C�ffC�ffC�ffC�L�C�&fC�&fC�33C�33C�@ C�@ C�@ C�L�C�Y�C�Y�C�ffC�Y�C�&fC�&fC�33C�@ C�@ C�L�C�Y�C�L�C�Y�C�ffC�ffC�@ C�&fC�33C�@ C�L�C�Y�C�ffC�L�C�&fC�33C�@ C�L�C�Y�C�L�C�&fC��C�@ C�33C�@ C�L�C�Y�C�Y�C�ffC�ffC�L�C��C�&fC�&fC�&fC�&fC�&fC�&fC�&fC�33C�33C�@ C�33C�33C�33C�@ C�@ C�@ C�@ C�@ C�33C�@ C�33D   D �3D�D��D3D�fD	�Dy�D� D33D��D� DFfD��D�fD@ D � D#  D%Y�D'�3D*�D,S3D.�3D0�fD3�D5ffD7� D9�fD<&fD>l�D@� DC�DEs3DG� DJ&fDLs3DN�fDQ&fDS��DU��DXS3DZ��D]3D_y�Da�3DdY�Df��Di�Dk��DnfDp� Dr�3Du` Dw��Dz,�D|&fD~�fD�vfD���D��3D�fD�33D�` D�� D��fD���D���D� D�,�D�<�D�\�D�vfD���D��fD���D�� D���D��fD��fD��fD��3D�� D���D��3D��fD�� D���D���D���D���D��3D���D�� D���D���D��3D���D�� D��3D�� D�� D�� D�� D��fD�	�D��D�&fD�9�D�FfD�VfD�i�D�s3D��fD��3D���D�� D��3D���D��fD��fD¦fDæfDĠ Dř�DƐ Dǀ D�p D�c3D�L�D�6fD�#3D�	�D���D��3D���DжfDѠ DҌ�DӃ3D�|�D�s3D�i�D�\�D�VfD�Y�D�\�D�Y�D�VfD�VfD�Y�D�\�D�c3D�\�D�Y�D�` D�VfD�Y�D�\�D�Y�D�\�D�Y�D�\�D�Y�D�` D�c3D�l�D�l�D�p D�s3D�s3D�p D�vfD�|�D��3D�|�D�� D�� D�y�D�i�D�ffD�ffE ,�E� E�E  Et�Ei�E�3E	8 E
,�E��E��E� EP E�fE�fE�3EfET�E�fEfEK3E~fE��E�E�EVfE � E!�E#0 E$��E%�fE'.fE(�E)nfE*�3E, E-\�E.��E/� E1�E2P E3~fE4�fE6#3E71�E8�fE9��E:�3E<1�E?D�EB@ EEa�EH��EK� EN� EQ�3EU4�EXt�E[A�E^�fEa�3Ed�fEg��Ej� En$�En�3Eo` Eo�Ep�fEqY�Eq�fEr�3Es3Es�fEth Et� Eu��Ev+3EvњEwx Ex3Ex�fEyP Ey� G�O�?�ff?�33G�O�?�33G�O�G�O�?�  G�O�G�O�?���G�O�?�  G�O�G�O�?�33G�O�?�  G�O�G�O�?�  G�O�G�O�?���?ٙ�G�O�?�ff@   @ff@33@��@&ff@9��@Fff@S33@`  @y��@�33@���@�33@���@�ff@�  @���@�ff@�ff@�33@�  @���A��A33A33A33A#33A)��A333A9��AC33AK33AS33G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411414414414144141441441141111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   @ �@ �@ {@ O@ ""@ (�@ /�@ 6�@ =q@ E�@ Q=@ _�@ m�@ |?@ ��@ ��@ ��@ ��@ ��@ �*@ �#@ ��@ ��@j@b@[@,`@:�@G�@T�@a�@o�@~K@�P@��@��@��@��@є@ލ@�@�~@�@*@#�@/�@<�@K@Yn@g@t�@�d@�@�@��@��@ƨ@�O@�H@�@�E@
�@�@&;@3�@@�@M�@]�@k.@x�@��@��@��@�@�@�@׹@�@�e@  @�@�@)�@6�@FQ@S�@`B@k.@y�@�+@��@�z@�!@��@�o@��@�m@��@j@�@ @.l@:@FQ@S�@a�@oF@~�@��@�H@�M@��@Ĝ@�C@��@�@�9@1@�@"�@/@>@M$@Yn@ff@s_@��@�@�@��@�R@�W@��@�H@��@�E@�@�@&;@4�@@,@N�@\�@i!@x�@�+@��@��@�@�@�@�
@�@�@��@�@
@+@7�@E�@R�@`B@m�@z�@��@�0@��@�-@��@�|@�t@��@�q@	@	�@	�@	-@	:@	F�@	T�@	b�@	p�@	~�@	�P@	��@	��@	��@	Ĝ@	�C@	ލ@	�(@	��@
%@
�@
""@
/�@
=q@
K�@
Z@
g�@
v@
�@
��@
��@
��@
�R@
��@
�O@
�@
�@
��@J@�@%�@1�@@,@N�@\�@k.@y�@��@�h@��@�@�k@��@׹@�T@�L@  @�@O@)�@7�@E�@S�@a�@m�@x�@�+@��@�y@�!@��@�o@�@�m@�@j@b@
@+�@:@G�@UU@b�@p�@}�@��@��@�A@��@��@ψ@܀@\�@�U@�/@�@^5@�a@��@g@^�@��@��@�@_�@��@��@ @^5@��@׹@6@V@��@��@V@Lu@��@�@J@K@��@�o@
�@K�@�P@�*@�@Q=@�h@��@�@X@��@��@�@`A@��@�@(�@k.@�@��@"�@dZ@��@�`@&�@g@�A@�@#�@bN@��@��@�@T�@�@�*@	�@E�@}�@��@�@@ &�@ ]�@ �#@ �@!  @!5�@!j@!��@!�
@"�@"C�@"z3@"�!@"�`@#�@#Q�@#��@#�w@#�q@$+�@$b�@$�H@$Ӡ@%�@%G�@%��@%�@%��@&1�@&k.@&��@&�;@'B@'S�@'��@'�W@( �@(8�@(p�@(��@(��@)�@)Lu@)�@)��@)��@*$/@*X�@*��@*�&@*�@+$�@+V�@+�7@+�^@+�@,g@,Q=@,�@,��@,�m@-�@-Q=@-��@-�^@-�@@.#�@.Z�@.�@.�@.��@/4�@/k�@/�(@/�#@0b@0FQ@0~K@0��@0�(@1!s@1Wb@1��@1Ĝ@1��@21�@2i�@2�@2��@3b@3G�@3~�@3��@3�@4#�@4[z@4�u@4ȴ@5  @56�@5k�@5�@5��@6B8@6��@7K@7�@8N�@8�@9V@9�L@:��@:��@;��@<$/@<��@=""@=��@>C�@>��@?+�@?��@@bN@@�@AdZ@A�l@Bk�@B�@Cr�@C�}@D��@EV@E�@F0x@F��@GQ=@G�-@HF�@H�h@Ii�@I�e@J~�@K�@K��@L@L�@Mb@M��@N&;@N�w@O.l@O�@PH]@Q�<@R��@T3�@U�<@V�@XK@Y��@Z�e@\Wb@]�7@^�@@`B8@a�T@b�@d6�@e�<@e�*@f�@fV@f��@f�q@g+�@gx�@g�@g�}@hDD@hww@h��@i�@iK�@i��@i�h@j�@j\)@j�mG�O�@ v@ %G�O�@ %G�O�G�O�@ �G�O�G�O�@ �G�O�@ �G�O�G�O�@ %G�O�@ �G�O�G�O�@ �G�O�G�O�@ �@ 1G�O�@ �@ 
=@ 
�@ J@ �@ V@ b@ �@ @ {@ 6@ �@ �@ �@ [@  @ ""@ $�@ &�@ *S@ -@ /�@ 1�@ 5?@ 7�@ ;d@ >�@ B8@ D�@ I@ K�@ O�@ SI@ V�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�1'A�=qA�?}A�?}A�G�A�E�A�I�A�K�A�K�A�\)A�\)A�bNA�r�A�jA�bNA�jA�n�A�jA�jA�ZA�ZA�VA�Q�A�Q�A�M�A�M�A�K�A�I�A�I�A�K�A�A�A�+A�%A��A�&�A˧�A���AƗ�A�33AþwA��A�
=A�`BA�9XA�5?A��A��A���A�v�A�ffA���A��+A�9XA���A���A�l�A�ĜA�{A�bA��7A���A�"�A���A�z�A��A��FA���A�t�A���A�ffA��A~�A{|�Ax��Av{ArJAmC�Af~�Aa;dA_�;A^E�AZ��AX�+AUx�ATJAT  AR�+AN$�AH��ADZABM�A@bA?��A?�TA?G�A?��A=�mA<-A:�9A9�mA9G�A9O�A8��A7�A6��A6M�A6�A5�hA5VA4ĜA49XA2�A2M�A1ƨA17LA1A0�jA/K�A.��A.^5A.�A-�mA-\)A+��A*�A*ZA)O�A(��A(-A'p�A&�\A%�^A%\)A$��A#�-A#?}A"~�A!�A!�A �A A�A�
A�A��A�`A�A��A��Av�AZA  A�PA33A�jA�A�TAK�A�/A^5A�AXAĜAM�AAt�A33A&�A+AA�A �A�A�-A�A�A�jA�\A�+Av�AI�AbA��A?}A%A�jAr�A�TA\)A?}A&�AoA��A�An�AbA��AA��Av�A�;Ax�A"�A
�/A
�+A
JA	�7A	?}A	VA��A^5A5?A{A�#A�hA;dA��A�A�^A�^A�^A�A|�AG�A�A�AM�AbA�
A��A��A�PA33A�jA�+Az�AI�A�mA��At�AO�A ��A bNA =qA {@��
@�;d@���@��\@�v�@�5?@�V@�bN@�9X@���@�+@��R@�@���@�p�@��@���@���@�;d@��R@��\@���@�7@땁@�C�@���@�p�@���@��@��y@��@���@ָR@�9X@�v�@�bN@�v�@�V@��@�^5@�+@ċD@�K�@�/@��@���@��@�ff@���@�1'@��@�J@�(�@�-@���@�M�@�Z@�;d@�v�@��j@�\)@��T@�
=@�v�@��@��@��@��;@�M�@� �@�|�@�{@�&�@�ƨ@��@��@��
@��!@��@��/@�S�@�n�@���@��j@�\)@��R@��#@���@�(�@�@}/@z�H@yx�@w�@v�R@t�j@sC�@p �@n@lj@k��@iG�@g�@fv�@ep�@cdZ@bM�@a%@^��@]p�@\�@Z��@Y��@X1'@W�@W;d@T�@TZ@R��@PbN@O+@M�-@L��@KC�@J�@H��@G|�@Fv�@E?}@DI�@B��@A7L@@1'@?;d@>ff@<�/@;��@:�@9��@97L@8b@7l�@6@4�D@3dZ@2M�@1�@0��@/�@.�@-`B@,I�@+C�@*J@(��@'��@&�y@&V@%�-@$�@$�@#��@"�@!&�@ Ĝ@�;@l�@�@@@��@1@�H@�#@ �@l�@�y@5?@@�D@9X@C�@�H@��@�#@X@�u@b@+@V@�T@�@��@�D@�m@�@	��@�9@��@O�@�@�\@�^@ �u?�\)?�{?�dZ?���?��9?�
=?���?��?�?���?��?�bN?���?�{?�O�?�j?�C�?�^5?�7L?�u?�l�?�K�?�E�?�?�`B?��?䛦?�9X?㕁?���?��?�&�?��?�  ?�;d?ޗ�?ݲ-?��?�(�?�dZ?ٙ�?ش9?֧�?ԛ�?�o?�&�?�|�?��?���?��m?ʟ�?ə�?���?ǍP?�?öF?��7?�A�?��;?�\)?�;d?���?��R?���?�V?�{?��?�p�?�p�?��?��?�I�?�(�?�ƨ?��?���?�ƨA�1'A�33A�33A�33A�/A�1'A�/A�-A�33A�-A�+A�+A�(�A�(�A�(�A�(�A�(�A�1'A�5?A�1'A�9XA�7LA�7LA�7LA�7LA�33A�/A�/A�7LA�?}A�?}A�?}A�A�A�?}A�A�A�A�A�=qA�?}A�=qA�?}A�?}A�I�A�G�A�E�A�C�A�G�A�G�A�I�A�I�A�K�A�I�A�K�A�K�A�XA�bNA�XA�XA�ZA�\)A�^5G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   A�1'A�=qA�?}A�?}A�G�A�E�A�I�A�K�A�K�A�\)A�\)A�bNA�r�A�jA�bNA�jA�n�A�jA�jA�ZA�ZA�VA�Q�A�Q�A�M�A�M�A�K�A�I�A�I�A�K�A�A�A�+A�%A��A�&�A˧�A���AƗ�A�33AþwA��A�
=A�`BA�9XA�5?A��A��A���A�v�A�ffA���A��+A�9XA���A���A�l�A�ĜA�{A�bA��7A���A�"�A���A�z�A��A��FA���A�t�A���A�ffA��A~�A{|�Ax��Av{ArJAmC�Af~�Aa;dA_�;A^E�AZ��AX�+AUx�ATJAT  AR�+AN$�AH��ADZABM�A@bA?��A?�TA?G�A?��A=�mA<-A:�9A9�mA9G�A9O�A8��A7�A6��A6M�A6�A5�hA5VA4ĜA49XA2�A2M�A1ƨA17LA1A0�jA/K�A.��A.^5A.�A-�mA-\)A+��A*�A*ZA)O�A(��A(-A'p�A&�\A%�^A%\)A$��A#�-A#?}A"~�A!�A!�A �A A�A�
A�A��A�`A�A��A��Av�AZA  A�PA33A�jA�A�TAK�A�/A^5A�AXAĜAM�AAt�A33A&�A+AA�A �A�A�-A�A�A�jA�\A�+Av�AI�AbA��A?}A%A�jAr�A�TA\)A?}A&�AoA��A�An�AbA��AA��Av�A�;Ax�A"�A
�/A
�+A
JA	�7A	?}A	VA��A^5A5?A{A�#A�hA;dA��A�A�^A�^A�^A�A|�AG�A�A�AM�AbA�
A��A��A�PA33A�jA�+Az�AI�A�mA��At�AO�A ��A bNA =qA {@��
@�;d@���@��\@�v�@�5?@�V@�bN@�9X@���@�+@��R@�@���@�p�@��@���@���@�;d@��R@��\@���@�7@땁@�C�@���@�p�@���@��@��y@��@���@ָR@�9X@�v�@�bN@�v�@�V@��@�^5@�+@ċD@�K�@�/@��@���@��@�ff@���@�1'@��@�J@�(�@�-@���@�M�@�Z@�;d@�v�@��j@�\)@��T@�
=@�v�@��@��@��@��;@�M�@� �@�|�@�{@�&�@�ƨ@��@��@��
@��!@��@��/@�S�@�n�@���@��j@�\)@��R@��#@���@�(�@�@}/@z�H@yx�@w�@v�R@t�j@sC�@p �@n@lj@k��@iG�@g�@fv�@ep�@cdZ@bM�@a%@^��@]p�@\�@Z��@Y��@X1'@W�@W;d@T�@TZ@R��@PbN@O+@M�-@L��@KC�@J�@H��@G|�@Fv�@E?}@DI�@B��@A7L@@1'@?;d@>ff@<�/@;��@:�@9��@97L@8b@7l�@6@4�D@3dZ@2M�@1�@0��@/�@.�@-`B@,I�@+C�@*J@(��@'��@&�y@&V@%�-@$�@$�@#��@"�@!&�@ Ĝ@�;@l�@�@@@��@1@�H@�#@ �@l�@�y@5?@@�D@9X@C�@�H@��@�#@X@�u@b@+@V@�T@�@��@�D@�m@�@	��@�9@��@O�@�@�\@�^@ �u?�\)?�{?�dZ?���?��9?�
=?���?��?�?���?��?�bN?���?�{?�O�?�j?�C�?�^5?�7L?�u?�l�?�K�?�E�?�?�`B?��?䛦?�9X?㕁?���?��?�&�?��?�  ?�;d?ޗ�?ݲ-?��?�(�?�dZ?ٙ�?ش9?֧�?ԛ�?�o?�&�?�|�?��?���?��m?ʟ�?ə�?���?ǍP?�?öF?��7?�A�?��;?�\)?�;d?���?��R?���?�V?�{?��?�p�?�p�?��?��?�I�?�(�?�ƨ?��?���?�ƨA�1'A�33A�33A�33A�/A�1'A�/A�-A�33A�-A�+A�+A�(�A�(�A�(�A�(�A�(�A�1'A�5?A�1'A�9XA�7LA�7LA�7LA�7LA�33A�/A�/A�7LA�?}A�?}A�?}A�A�A�?}A�A�A�A�A�=qA�?}A�=qA�?}A�?}A�I�A�G�A�E�A�C�A�G�A�G�A�I�A�I�A�K�A�I�A�K�A�K�A�XA�bNA�XA�XA�ZA�\)A�^5G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B	|�B	{�B	{�B	|�B	|�B	|�B	{�B	{�B	|�B	�B	� B	�B	�+B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�VB	W
B	C�B	~�B	ƨB
1B
33B
n�B
�PB
��B
�B
�qB
�B
��B
��B\BP�B@�B
��B\B,BM�BD�BN�B<jB33BC�B5?BVB
�mB
��B
/B	�#B	��B	�yB	�dB	��B	}�B	jB	R�B	C�B	+B	�B��B�B�ZB�/B��BȴB�dB�LB�#B	VB	�B	%B��B��B�B	%B	�B	 �B	(�B	S�B	gmB	t�B	�oB	�{B	��B	��B	�-B	ɺB	�B	�B	�5B	�B	��B	��B
B
�B
&�B
+B
2-B
33B
49B
@�B
B�B
E�B
E�B
F�B
I�B
J�B
J�B
L�B
L�B
M�B
L�B
L�B
M�B
Q�B
Q�B
P�B
T�B
VB
Q�B
R�B
Q�B
P�B
P�B
Q�B
W
B
ZB
W
B
T�B
R�B
P�B
O�B
Q�B
P�B
P�B
N�B
N�B
O�B
P�B
O�B
N�B
L�B
I�B
I�B
G�B
F�B
F�B
F�B
H�B
I�B
M�B
N�B
N�B
N�B
N�B
L�B
J�B
I�B
G�B
G�B
J�B
M�B
N�B
N�B
M�B
N�B
N�B
N�B
N�B
L�B
L�B
L�B
M�B
M�B
K�B
L�B
K�B
I�B
G�B
F�B
E�B
E�B
C�B
C�B
C�B
C�B
B�B
A�B
@�B
@�B
>wB
=qB
<jB
=qB
<jB
;dB
;dB
:^B
:^B
8RB
9XB
8RB
8RB
8RB
7LB
7LB
7LB
6FB
5?B
49B
33B
49B
7LB
8RB
7LB
5?B
49B
5?B
5?B
5?B
7LB
6FB
6FB
5?B
5?B
5?B
33B
33B
2-B
2-B
2-B
2-B
1'B
1'B
2-B
2-B
0!B
/B
.B
.B
.B
.B
-B
-B
.B
-B
,B
-B
#�B
!�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
{B
�B
oB
uB
uB
�B
{B
�B
uB
\B
\B
\B
bB
hB
bB
bB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 �B
 �B
"�B
#�B
%�B
$�B
%�B
'�B
)�B
+B
,B
,B
.B
.B
/B
2-B
2-B
33B
5?B
7LB
7LB
8RB
8RB
9XB
:^B
;dB
<jB
=qB
>wB
?}B
@�B
B�B
C�B
D�B
F�B
E�B
H�B
H�B
I�B
I�B
K�B
L�B
L�B
N�B
O�B
P�B
Q�B
R�B
R�B
Q�B
S�B
T�B
VB
VB
XB
YB
ZB
YB
ZB
\)B
\)B
\)B
]/B
^5B
^5B
`BB
aHB
aHB
aHB
bNB
bNB
cTB
cTB
e`B
e`B
ffB
e`B
gmB
gmB
hsB
iyB
jB
k�B
k�B
l�B
l�B
n�B
n�B
o�B
p�B
o�B
q�B
r�B
r�B
r�B
r�B
s�B
u�B
u�B
u�B
v�B
v�B
v�B
y�B
x�B
x�B
z�B
z�B
{�B
}�B
~�B
~�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�%B
�+B
�+B
�1B
�1B
�7B
�7B
�7B
�DB
�JB
�PB
�\B
�bB
�hB
�oB
�uB
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
��B
��B
��B
��B
��B
��B
��B
�B
�B
�B
�B
�B
�B
�B
�B
�!B
�!B
�!B
�'B
�-B
�-B
�3B
�9B
�?B
�?B
�FB
�FB
�FB
�FB
�LB
�RB
�RB
�RB
�RB
�RB
�RB
�RB
�RB
�XB
�XB
�XB
�XB
�XB
�XB
�XB
�^B
�^B
�XB	}�B	}�B	}�B	|�B	}�B	|�B	}�B	}�B	{�B	|�B	{�B	|�B	}�B	}�B	|�B	|�B	}�B	~�B	{�B	}�B	{�B	|�B	|�B	{�B	|�B	|�B	|�B	~�B	|�B	{�B	|�B	{�B	z�B	{�B	z�B	{�B	{�B	{�B	{�B	|�B	|�B	}�B	|�B	|�B	|�B	{�B	{�B	{�B	{�B	z�B	{�B	{�B	|�B	�B	�B	~�B	� B	�B	~�B	�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   B	|�B	{�B	{�B	|�B	|�B	|�B	{�B	{�B	|�B	��B	�B	��B	�
B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�AB	V�B	C�B	~�B	ƓB
B
3B
n�B
�=B
��B
��B
�_B
��B
��B
��BLBP�B@sB
��BMB+�BM�BD�BN�B<]B3&BC�B53BJB
�aB
��B
/B	�B	˻B	�nB	�YB	��B	}�B	jtB	R�B	C�B	*�B	�B��B�B�QB�'B��BȬB�]B�EB�B	PB	�B	 B��B��B�B	!B	�B	 �B	(�B	S�B	gkB	t�B	�nB	�{B	��B	��B	�/B	ɼB	�B	� B	�9B	�B	��B	��B
%B
�B
&�B
+
B
25B
3<B
4BB
@�B
B�B
E�B
E�B
F�B
I�B
J�B
J�B
L�B
L�B
M�B
L�B
L�B
M�B
Q�B
Q�B
P�B
UB
VB
R B
SB
RB
P�B
P�B
RB
W!B
Z5B
W"B
UB
SB
P�B
O�B
RB
QB
QB
N�B
N�B
O�B
QB
O�B
N�B
L�B
I�B
I�B
G�B
F�B
F�B
F�B
H�B
I�B
M�B
N�B
N�B
O B
O B
L�B
J�B
I�B
G�B
G�B
J�B
M�B
OB
OB
N B
OB
OB
OB
OB
L�B
L�B
L�B
NB
NB
K�B
M B
K�B
I�B
G�B
F�B
E�B
E�B
C�B
C�B
C�B
C�B
B�B
A�B
@�B
@�B
>�B
=�B
<�B
=�B
<�B
;�B
;�B
:�B
:�B
8�B
9�B
8�B
8�B
8�B
7�B
7�B
7�B
6�B
5�B
4�B
3zB
4�B
7�B
8�B
7�B
5�B
4�B
5�B
5�B
5�B
7�B
6�B
6�B
5�B
5�B
5�B
3�B
3�B
2~B
2B
2�B
2�B
1{B
1{B
2�B
2�B
0wB
/rB
.kB
.lB
.lB
.mB
-hB
-hB
.oB
-iB
,dB
-jB
$9B
"/B
 %B
"B
B
B
B
B
B
B
B
B
	B
 B
B
�B
B
B
B
B
B
B
�B
�B
�B
B
B
B
B
6B
?B
GB
JB
AB
>B
FB
IB
RB
[B
eB
yB
 �B
�B
 �B
!�B
!�B
#�B
$�B
&�B
%�B
&�B
(�B
*�B
+�B
,�B
,�B
/B
/B
0B
3/B
32B
4:B
6IB
8YB
8[B
9dB
9fB
:oB
;xB
<�B
=�B
>�B
?�B
@�B
A�B
C�B
D�B
E�B
G�B
F�B
I�B
I�B
J�B
J�B
MB
NB
NB
PB
Q(B
R0B
S:B
TBB
TDB
SAB
UOB
VXB
W`B
WcB
YqB
Z{B
[�B
Z�B
[�B
]�B
]�B
]�B
^�B
_�B
_�B
a�B
b�B
b�B
b�B
c�B
c�B
d�B
d�B
f�B
f�B
g�B
f�B
iB
i	B
jB
kB
l!B
m)B
m,B
n4B
n6B
pEB
pGB
qPB
rXB
qTB
scB
tkB
tmB
tpB
trB
uzB
w�B
w�B
w�B
x�B
x�B
x�B
{�B
z�B
z�B
|�B
|�B
}�B
�B
��B
��B
��B
��B
��B
��B
�B
�B
�B
�B
�B
� B
�"B
�%B
�-B
�5B
�8B
�@B
�CB
�KB
�MB
�RB
�dB
�qB
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
�
B
�B
�B
� B
�,B
�@B
�LB
�LB
�WB
�cB
�iB
�vB
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
�B
�	B
�B
�B
�(B
�-B
�4B
�9B
�KB
�LB
�aB
�|B
��B
��B
��B
��B
��B
��B
� B
�B
�*B
�9B
�PB
�_B
�nB
�}B
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
��B	}�B	}�B	}�B	|�B	}�B	|�B	}�B	}�B	{�B	|�B	{�B	|�B	}�B	}�B	|�B	|�B	}�B	~�B	{�B	}�B	{�B	|�B	|�B	{�B	|�B	|�B	|�B	~�B	|�B	{�B	|�B	{�B	z�B	{�B	z�B	{�B	{�B	{�B	{�B	|�B	|�B	}�B	|�B	|�B	|�B	{�B	{�B	{�B	{�B	z�B	{�B	{�B	|�B	��B	��B	~�B	�B	��B	~�B	��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�201807242203002021061413572520210614135725202107221611262021072216112620210722161126201807242203002021061413572520210614135725202107221611262021072216112620210722161126PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            new conductivity = original conductivity * (1 + delta*TEMP + CPcor_SBE*PRES) / (1 + delta*TEMP_ADJUSTED + CPcor_new*PRES_ADJUSTED)                                                                                                                              none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            new conductivity = original conductivity * (1 + delta*TEMP + CPcor_SBE*PRES) / (1 + delta*TEMP_ADJUSTED + CPcor_new*PRES_ADJUSTED)                                                                                                                              none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.50 dbar                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            CPcor_new = -1.25e-07; CPcor_SBE = -9.57e-08; delta = 3.25e-06                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.50 dbar                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            CPcor_new = -1.25e-07; CPcor_SBE = -9.57e-08; delta = 3.25e-06                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                      none                                                                                                                                                                                                                                                            New conductivity computed by using a different CPcor value from that provided by Sea-Bird.                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            No significant salinity drift detected.                                                                                                                                                                                                                         Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                      none                                                                                                                                                                                                                                                            New conductivity computed by using a different CPcor value from that provided by Sea-Bird.                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            No significant salinity drift detected.                                                                                                                                                                                                                         PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            AO  AO  ARCAARCAADJPADJP                                                                                                                                        2018072422030020180724220300  IP  IP                                G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2018072422030020180724220300QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�5F03E           703E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2018072422030020180724220300QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�40000           0               PM  PM  ARSQARSQCOWGCOWGV1.1V1.1                                                                                                                                2021072216141820210722161418IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                