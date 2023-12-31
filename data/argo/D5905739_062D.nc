CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS     N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2019-03-12T02:00:59Z creation      
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
resolution        =���   axis      Z        �  ;�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 ,  L|   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  P�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 ,  aP   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  e|   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  v$   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 ,  ��   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 ,  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �t   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 ,  �   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �H   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 ,  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                   ,   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                   4   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                   <   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                   D   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  � L   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                   �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                   �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar           HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar           HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�           HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    (   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �l   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �l   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                   l   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  � lArgo profile    3.1 1.2 19500101000000  20190312020059  20210617131516  5905739 5905739 US ARGO PROJECT                                                 US ARGO PROJECT                                                 GREGORY C. JOHNSON                                              GREGORY C. JOHNSON                                              PRES            TEMP            PSAL            PRES            TEMP            PSAL               >   >DD  AOAO7219                            7219                            2C  2C  DD  SOLO_D_MRV                      SOLO_D_MRV                      12002                           12002                           SBE603 01Dec16                  SBE603 01Dec16                  874 874 @ةu��@ةu��11  @ةO�p@ةO�p@6!����@6!�����cɒ���8�cɒ���811  GPS     GPS     Primary sampling: averaged [1Hz sampling by SBE-41CP averaged in 2-dbar bins]                                                                                                                                                                                   Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 AC  AA  AA  >L��?�  @ff@Fff@�33@�33@�33@�ffA   A��A(  AC33A`  A���A���A���A�  A�  A�33A�33A�ffA�ffB  BffBffB ��B'��B/��B8  B@  BH  BO��BXffB`��Bh  Bp  BxffB�33B�  B�  B�ffB�ffB�33B�  B�  B�33B�ffB�ffB�33B���B���B�  B�  B�  B�  BǙ�B���B�ffB�33B�  B�  B���B�  B���B�ffB�B�  B���B�  C   C33C�fC�fC  C
�CL�C  C  C33CL�CL�C  C�fC  C  C �C"33C$L�C&  C(  C*  C,  C.  C0  C2  C4  C5�fC7��C:33C<33C>33C@33CB�CD  CE�fCHL�CJ�CL  CNL�CP33CR�CS�fCU�fCX�CZ33C\33C^  C`�CbL�Cd33Ce�fCh�CjL�Cl33Cm�fCp�Cr33Ct�Cu��Cx  Cz33C|�C}�fC�  C��C��C��3C��C��C��3C��C��3C��3C��C�  C��3C��C�  C��3C��C��C�&fC��C�  C��C��C�  C��3C�  C��C�33C��C�ٚC��3C��C��C��C��C�ٚC�ٚC��3C��3C�  C��C��C��C��C��C�&fC�33C��C��3C��3C�  C��C�&fC�  C��fC�  C��C�&fC��C�  C��C��C�  C��3C��fC��C��C�  C�  C��3C��C��C��C��C��C��C��C��C�  C�  C�  C�  C��3C��C��C��3C��C��C�  C�  C��3C��C�  C��3C��3C��fC��C�  C��3C��C�  C��fC��C�&fC��C��3C��C�&fC��C��C�  C��fC��C�&fC��C��C�  C��3C��C�  C��3C��C��C��C���C��3C��C���Ds3D�D	�fD��D` D  D�fD9�D��D��D,�D!� D$��D'Y�D*&fD-fD/�fD2��D5� D8Y�D;FfD>9�DA�DD  DF��DI��DLffDO,�DQ� DTy�DW  DY��D\S3D^ٚDa` DcٚDf9�Dh� Dk�Dml�Do��Dr,�Dt�3Dw  DyffD{� D~�D�C3D���D��3D�#3D�vfD���D�,�D���D�� D�VfD���D�)�D���D��3D�` D���D�33D��fD�  D�` D�� D��D�s3D��3D� D�Y�D�� D�� D� D�FfD�vfD���D�ٚD� D�P D���D�� D�	�D�C3D�vfD��3D�ɚD��3D�#3D�L�D�vfD��fD��fD��3D���D�3D�#3D�FfD�c3DĆfDţ3D�ɚD��fD�#3D�Y�D˓3D�ɚD���D�0 D�ffDќ�D�ٚD�3D�I�D�|�Dש�D�� D��D�VfDܖfD�ɚD��fD�#3D�P D�y�D�3D�� D�  D�&fD�L�D�p D�3D빚D�� D�3D�&fD�I�D�ffD�fD��D�ٚD��D�I�D��fD���D���D��fD���D��D�C3E 8 E �3E` E�E� EfE��ED�E��Et�E	�E�fE6fE� Ed�E��E	� E
#3E
��E�3EfE;3E��E��E�E.fE��E��EK3EFfE� E��E� EI�E�fE�E!�E"c3E#��E$ٚE& E'NfE(��E)� E+�E,ffE-�fE.�3E03E1k3E2a�E3ٚE5Q�E6Q�E7� E8��E:A�E;;3E<�3E?�fEB� EF+3EH�fEL1�EOy�ER�fEUt�EX� E[��E^�3Eb EefEhi�EkA�EnS3Eq�fEt�3Ew��Ez�3E}��E��3E�, E���E�T�E��3E�l E�	�E�� E�3E�� E�A�E��fE�� E�$�E�o3E���E�&fE�o3E��3E��3E�FfE��fE���E�4�E�� E�� E�>fE���E��3E�"fE�� E��3E� E�Y�E��3E�fE�K3E�� E���E�,�>���>L��>L��>L��>L��=���=���>L��=���>���>L��=���=���>L��>L��>���>���>���>L��>L��>L��>L��>���>���>���>���?   ?333?��?L��?fff?���?���?�  ?�  ?�ff@ff@��@��@,��@@  @L��@`  @s33@�33@���@�ff@�33@�  @���@�33@�  @���@���@���A33A33A��A  A   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111444444141444141444444144111411111411111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           ?333?�  @&ff@fff@�33@�33@�33@�ffA  A��A0  AK33Ah  A���A���A���A�  A�  A�33A�33A�ffB33B
  BffBffB"��B)��B1��B:  BB  BJ  BQ��BZffBb��Bj  Br  BzffB�33B�  B�  B�ffB�ffB�33B�  B�  B�33B�ffB�ffB�33B���B���B�  B�  B�  B�  Bș�B���B�ffB�33B�  B�  B���B�  B���B�ffB�B�  B���B�  C � C�3CffCffC� C
��C��C� C� C�3C��C��C� CffC� C� C ��C"�3C$��C&� C(� C*� C,� C.� C0� C2� C4� C6ffC8L�C:�3C<�3C>�3C@�3CB��CD� CFffCH��CJ��CL� CN��CP�3CR��CTffCVffCX��CZ�3C\�3C^� C`��Cb��Cd�3CfffCh��Cj��Cl�3CnffCp��Cr�3Ct��CvL�Cx� Cz�3C|��C~ffC�@ C�Y�C�L�C�33C�Y�C�L�C�33C�L�C�33C�33C�L�C�@ C�33C�Y�C�@ C�33C�L�C�Y�C�ffC�Y�C�@ C�Y�C�Y�C�@ C�33C�@ C�Y�C�s3C�L�C��C�33C�L�C�L�C�Y�C�L�C��C��C�33C�33C�@ C�L�C�L�C�Y�C�Y�C�Y�C�ffC�s3C�L�C�33C�33C�@ C�L�C�ffC�@ C�&fC�@ C�Y�C�ffC�Y�C�@ C�Y�C�Y�C�@ C�33C�&fC�L�C�L�C�@ C�@ C�33C�L�C�L�C�L�C�Y�C�Y�C�Y�C�L�C�L�C�@ C�@ C�@ C�@ C�33C�L�C�L�C�33C�L�C�L�C�@ C�@ C�33C�L�C�@ C�33C�33C�&fC�L�C�@ C�33C�L�C�@ C�&fC�L�C�ffC�L�C�33C�L�C�ffC�Y�C�L�C�@ C�&fC�L�C�ffC�Y�C�L�C�@ C�33C�L�C�@ C�33C�Y�C�Y�C�L�C��C�33C�Y�C��D�3D9�D
fD��D� D  D�fDY�D�D��DL�D"  D$��D'y�D*FfD-&fD/�fD2��D5� D8y�D;ffD>Y�DA9�DD  DF��DI��DL�fDOL�DR  DT��DW@ DYٚD\s3D^��Da� Dc��DfY�Dh� Dk,�Dm��Do��DrL�Dt�3Dw  Dy�fD{� D~,�D�S3D���D��3D�33D��fD���D�<�D���D�  D�ffD���D�9�D���D�3D�p D���D�C3D��fD� D�p D�� D�)�D��3D��3D�  D�i�D�� D�� D�  D�VfD��fD���D��D�  D�` D���D�� D��D�S3D��fD��3D�ٚD�3D�33D�\�D��fD��fD��fD��3D���D�3D�33D�VfD�s3DĖfDų3D�ٚD�fD�33D�i�Dˣ3D�ٚD��D�@ D�vfDѬ�D��D�#3D�Y�D֌�D׹�D�� D�,�D�ffDܦfD�ٚD�fD�33D�` D≚D�3D�� D� D�6fD�\�D� D�3D�ɚD�� D�3D�6fD�Y�D�vfD�fD��D��D��D�Y�D��fD���D���D��fD�	�D�,�D�S3E @ E �3Eh E��E� EfE��EL�E��E|�E�E�fE>fE� El�E	�E	� E
+3E
��E�3EfEC3E��E�E�E6fE��E��ES3ENfE� E��E  EQ�E�fE�E!$�E"k3E#��E$�E& E'VfE(��E)� E+!�E,nfE-�fE.�3E03E1s3E2i�E3�E5Y�E6Y�E7� E8��E:I�E;C3E<�3E?�fEB� EF33EH�fEL9�EO��ER�fEU|�EX� E[��E^�3Eb EefEhq�EkI�En[3Eq�fEt�3Ex�Ez�3E}��E��3E�0 E�ŚE�X�E��3E�p E��E�� E�3E�� E�E�E��fE�� E�(�E�s3E���E�*fE�s3E��3E�3E�JfE��fE���E�8�E�� E�� E�BfE���E��3E�&fE�� E��3E�  E�]�E��3E�fE�O3E�� E���E�0�G�O�G�O�G�O�G�O�G�O�G�O�?��G�O�?��G�O�G�O�G�O�?��G�O�?333G�O�G�O�G�O�G�O�G�O�G�O�?333G�O�G�O�?L��?fff?�  G�O�?���?�ff?�33?���?ٙ�G�O�@   @33@&ff@,��@9��@L��@`  @l��@�  @���@�33@���@�ff@�33@�  @ə�@�33@�  @���@���A��A33A33A��A   A(  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111444444141444141444444144111411111411111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           @ ^@ �@ V@ *@ �@ "�@ )�@ 1'@ 6�@ >@ G�@ SI@ _�@ m�@ |?@ �7@ �0@ ��@ ��@ �w@ �o@ �@ �@ �q@@o@
@+�@:@G�@UU@bN@qS@�@��@��@��@��@@�7@�;@��@��@�@{@"�@1'@>�@K�@X@e�@t@��@�\@�@�M@��@�W@�O@�H@��@��@
=@6@&�@5?@@�@M�@\)@i�@x�@�p@�@�m@��@��@�c@�
@�@�e@@�@�@(�@6�@D�@SI@a�@m:@z�@��@�0@��@�~@�&@��@��@�@� @�@o@ @-@:@F�@Wb@c�@p�@�W@�P@�H@��@�9@�>@є@�;@�@��@�@�@!s@0x@?}@Lu@X@g@uk@�d@��@�@�@�@�J@Ӡ@�@�@��@�@�@$�@3�@@,@M�@\�@i�@v�@�|@��@��@��@�@�o@�h@�@�@^@�@�@(�@7�@F�@R�@]�@l�@{�@�7@��@��@�r@�@�@��@�@�q@	@	o@	 @	-�@	<@	Ji@	V@	bN@	o�@	~K@	��@	��@	�A@	��@	@	є@	��@	��@	�,@
1@
�@
""@
/@
<@
K�@
Yn@
ff@
t@
�@
�@
��@
�Y@
��@
�W@
��@
��@
�@
��@
=@�@%�@2�@A�@O0@[z@j@x&@�@��@��@��@��@ȴ@�[@�T@�@  @�@�@(�@5?@D�@S�@`B@l�@{�@��@��@��@�~@��@�|@܀@�y@�q@j@b@g@,`@9X@I@V�@c�@m�@}�@�P@��@ �@i!@��@^@K@��@�#@!s@k.@��@��@DD@�P@��@&;@t�@��@�@\)@��@��@Ji@��@�@4�@�@�|@B@b�@��@�Y@9X@�W@�J@
=@M�@��@�7@o@SI@�$@��@�@X�@�H@Ӡ@B@\�@�(@��@0x@x�@��@�@X@��@�L@<�@��@�\@"�@p�@�w@ 
�@ V�@ ��@ ��@!:@!��@!�|@"*@"\)@"�z@"�@#(�@#m:@#�r@#�L@$1�@$r�@$��@$�,@%<�@%��@%Ĝ@&�@&I@&�7@&�@'�@'H]@'��@'�W@(1@(E�@(~�@(�@(�q@)3�@)r@)��@)��@*)�@*hs@*��@*��@++@+m�@+�!@+�@,33@,uk@,��@,�9@->@-�W@-��@.@.DD@.��@.��@/@/P�@/��@/��@0@0P�@0�@0�7@1@1O�@1��@1��@2
�@2I�@2��@2ƨ@3�@3B�@3�@3�@3��@4<@4}�@4�2@5�@5B�@5z3@5��@5��@65�@6t�@6��@6�@733@7qS@7�@7�@@8.l@8oF@8�!@8��@90x@9o�@9��@9�Y@:1�@:r�@:�~@:�L@;0x@;��@<2�@<�r@=e	@=܀@>S�@>�@?v@?�@@�P@@�}@A��@A�q@B��@C�@C��@D7L@D��@EI@E��@FV@F�t@GbN@G�4@Hww@I �@I��@J	@J�@KB@K��@L�@L�k@M\�@M�@Ni�@Nխ@Ox&@O�@P|?@Q��@S@T�P@U�w@Wg@X��@Y��@[@\ww@]��@_ @`p�@a�w@c)�@d`B@e�r@gb@hn�@i�|@k�@lWb@m�9@o{@pn�@qƨ@s!s@ti�@u��@w{@x^�@y�~@{C@{Z�@{�@{�#@|�@|Yn@|��@|�@}2�@}o�@}��@~v@~> @~x&@~��@�@Z�@�u@�@��@�6�@�Q�@�y,@��u@���@���@���@� m@�D�@�Z�G�O�G�O�G�O�G�O�G�O�G�O�@  �G�O�@  �G�O�G�O�G�O�@  �G�O�@ ^G�O�G�O�G�O�G�O�G�O�G�O�@ ^G�O�G�O�@ @ �@ jG�O�@ @ v@ %@ �@ 1G�O�@ 
=@ J@ V@ @ b@ o@ {@ �@ �@ �@ �@ 
@  @ "�@ %�@ '�@ )�@ ,`@ /@ 2�@ 5?@ 7�@ ;d@ >@ @�@ DDG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A��HA��HA���A���A��HA���A�A�  A�A�1A�
=A�bA�VA�oA�{A��A��A��A��A��A��A��A��A�%A�A�A���A���A��A��A��A��A��mA��yA��`A��mA��mA��mA��`A��mA��mA��mA��mA��yA��yA��A��A��A��A��`A��
A���A�v�A�ffA�I�A���A�1A��+A�O�A�v�A���A�oA�G�A�?}A��A�A��A���A�E�A��^A�~�A�1'A��A��A��FA��TA�ZA���A�$�A���A�ȴA��mA���A��#A�{A��mA�oA��A�
=A�5?A�K�A��HA�Q�A�  A���A�%A�"�A���A�-A���A���A�jA�z�A��
A�dZA��uA��!A�/A�-A���A�oA�VA�hA~5?A|n�A{%AzbNAx(�At(�ArȴAq�Am�Ac�FA^=qA\bAX�/AWx�AVVAS��APbAM�^AK?}AI�TAG`BAE�ADbNAD  ACG�AB�/AB��AB9XAAt�A@ZA?��A?t�A>��A=�PA<A�A:VA8~�A7�PA7XA6��A6ZA5S�A4�9A4ffA45?A4(�A4�A3�A2�jA2^5A1�A1�A0v�A0�A/��A/dZA.�A.1A,��A,VA+33A*9XA)��A)�A)��A)\)A(��A&�A#ƨA!�A ��A 1A�RA1A��Al�AVA�yA�hA�A��AjA�wA�AG�AE�A�;A�TAVA�A��A
E�A
{A	�wA��A�A��A\)A7LA�AVA�/A��A�
A�AA $�@���@��^@�A�@�$�@�C�@���@�K�@�R@�A�@�Ĝ@��;@��@��@�1@��T@�1@��@�!@�\@�@�|�@�+@ް!@��@�(�@۝�@�C�@ڧ�@�7L@�Ĝ@�9X@�@պ^@�?}@���@ӕ�@�dZ@�S�@љ�@�\)@�ff@��@�\)@�hs@��@���@��@���@�1@��h@�M�@���@�v�@���@��@���@�K�@��-@���@�9X@�~�@�t�@�  @�9X@�`B@���@�Z@�M�@��@�ƨ@�^5@��j@��;@��;@��`@�@��@�1'@�\)@�@�M�@�V@�(�@~ff@{S�@yhs@w+@vȴ@t�@r��@qhs@o��@m�@k�
@ko@j��@j=q@hĜ@fff@b��@b-@`�`@_|�@_�@]�h@[�F@[t�@Y�@WK�@Sƨ@S"�@R-@Q7L@P�9@M�T@K�F@Jn�@I�@H1'@F{@Ep�@EO�@E�@D9X@CC�@A��@A��@@A�@>{@;C�@:�\@:^5@:�@9��@9��@7��@65?@5/@4�@3t�@2�\@1�#@0A�@/��@/�@/��@.�R@-p�@,��@+�
@)��@&E�@#��@#C�@"=q@!x�@!&�@ �u@ bN@  �@�@�@|�@
=@�@��@S�@�!@�@��@�w@V@`B@"�@��@�@ �@|�@+@+@ff@@�@j@�F@
��@	�#@	G�@	G�@�u@�;@E�@$�@$�@�h@�@�@�@��@~�@n�@��@�#@��@ Ĝ@ Q�@ A�@ 1'?��;?��;?��;?��;?���?���?��^?�ȴ?�`B?��
?�bN??�5??�?�dZ?�Q�?��y?�S�?�M�?�bN?�V?�(�?�dZ?��#?ش9?�ȴ?ԛ�?�-?�&�?У�?�v�?�/?��?�C�?ʟ�?ɺ^?���?�b?ǍP?�E�?�?�?���?�9X?�Z?�9X?��?\?� �?�;d?��?�5??��?���?��?�^5?�r�?��9?��9?���?�X?��^?�~�?��H?��H?�C�?���?��m?�I�?���?�O�?��?���?�\)?�  ?�bN?��?�Ĝ?�Ĝ?��`?�&�?�G�?�hs?��7?��7?���?�J?�J?�-?�M�?�M�?°!?°!?���?��?�33?�33?�o?�S�?�t�?Õ�?öF?���?��?��A��#A��;A��;A��`A��HA��TA��TA��TA��TA��HA��HA��HA��HA��HA��TA��TA��;A��TA��HA��HA��HA��HA��HA��HA��HA��HA��;A��/A��#A��A��#A��`A��mA��yA��yA���A���A���A���A���A���A���A��A��yA��;A��#A��A���A���A�A�A�  A�  A�  A�A�A�A�
=A�JA�
=G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           A��HA��HA���A���A��HA���A�A�  A�A�1A�
=A�bA�VA�oA�{A��A��A��A��A��A��A��A��A�%A�A�A���A���A��A��A��A��A��mA��yA��`A��mA��mA��mA��`A��mA��mA��mA��mA��yA��yA��A��A��A��A��`A��
A���A�v�A�ffA�I�A���A�1A��+A�O�A�v�A���A�oA�G�A�?}A��A�A��A���A�E�A��^A�~�A�1'A��A��A��FA��TA�ZA���A�$�A���A�ȴA��mA���A��#A�{A��mA�oA��A�
=A�5?A�K�A��HA�Q�A�  A���A�%A�"�A���A�-A���A���A�jA�z�A��
A�dZA��uA��!A�/A�-A���A�oA�VA�hA~5?A|n�A{%AzbNAx(�At(�ArȴAq�Am�Ac�FA^=qA\bAX�/AWx�AVVAS��APbAM�^AK?}AI�TAG`BAE�ADbNAD  ACG�AB�/AB��AB9XAAt�A@ZA?��A?t�A>��A=�PA<A�A:VA8~�A7�PA7XA6��A6ZA5S�A4�9A4ffA45?A4(�A4�A3�A2�jA2^5A1�A1�A0v�A0�A/��A/dZA.�A.1A,��A,VA+33A*9XA)��A)�A)��A)\)A(��A&�A#ƨA!�A ��A 1A�RA1A��Al�AVA�yA�hA�A��AjA�wA�AG�AE�A�;A�TAVA�A��A
E�A
{A	�wA��A�A��A\)A7LA�AVA�/A��A�
A�AA $�@���@��^@�A�@�$�@�C�@���@�K�@�R@�A�@�Ĝ@��;@��@��@�1@��T@�1@��@�!@�\@�@�|�@�+@ް!@��@�(�@۝�@�C�@ڧ�@�7L@�Ĝ@�9X@�@պ^@�?}@���@ӕ�@�dZ@�S�@љ�@�\)@�ff@��@�\)@�hs@��@���@��@���@�1@��h@�M�@���@�v�@���@��@���@�K�@��-@���@�9X@�~�@�t�@�  @�9X@�`B@���@�Z@�M�@��@�ƨ@�^5@��j@��;@��;@��`@�@��@�1'@�\)@�@�M�@�V@�(�@~ff@{S�@yhs@w+@vȴ@t�@r��@qhs@o��@m�@k�
@ko@j��@j=q@hĜ@fff@b��@b-@`�`@_|�@_�@]�h@[�F@[t�@Y�@WK�@Sƨ@S"�@R-@Q7L@P�9@M�T@K�F@Jn�@I�@H1'@F{@Ep�@EO�@E�@D9X@CC�@A��@A��@@A�@>{@;C�@:�\@:^5@:�@9��@9��@7��@65?@5/@4�@3t�@2�\@1�#@0A�@/��@/�@/��@.�R@-p�@,��@+�
@)��@&E�@#��@#C�@"=q@!x�@!&�@ �u@ bN@  �@�@�@|�@
=@�@��@S�@�!@�@��@�w@V@`B@"�@��@�@ �@|�@+@+@ff@@�@j@�F@
��@	�#@	G�@	G�@�u@�;@E�@$�@$�@�h@�@�@�@��@~�@n�@��@�#@��@ Ĝ@ Q�@ A�@ 1'?��;?��;?��;?��;?���?���?��^?�ȴ?�`B?��
?�bN??�5??�?�dZ?�Q�?��y?�S�?�M�?�bN?�V?�(�?�dZ?��#?ش9?�ȴ?ԛ�?�-?�&�?У�?�v�?�/?��?�C�?ʟ�?ɺ^?���?�b?ǍP?�E�?�?�?���?�9X?�Z?�9X?��?\?� �?�;d?��?�5??��?���?��?�^5?�r�?��9?��9?���?�X?��^?�~�?��H?��H?�C�?���?��m?�I�?���?�O�?��?���?�\)?�  ?�bN?��?�Ĝ?�Ĝ?��`?�&�?�G�?�hs?��7?��7?���?�J?�J?�-?�M�?�M�?°!?°!?���?��?�33?�33?�o?�S�?�t�?Õ�?öF?���?��?��A��#A��;A��;A��`A��HA��TA��TA��TA��TA��HA��HA��HA��HA��HA��TA��TA��;A��TA��HA��HA��HA��HA��HA��HA��HA��HA��;A��/A��#A��A��#A��`A��mA��yA��yA���A���A���A���A���A���A���A��A��yA��;A��#A��A���A���A�A�A�  A�  A�  A�A�A�A�
=A�JA�
=G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�Bs�Bs�Bs�Br�Bs�Bs�Br�Bs�Bs�Bs�Bs�Bs�Bs�Bs�Bs�Bt�Bt�Bt�Bt�Bs�Bt�Bt�Bt�Bz�B}�B~�B�B�B�+B�1B�7B�7B�=B�=B�DB�DB�DB�DB�JB�JB�JB�JB�PB�JB�PB�PB�PB�PB�PB�PB�PB�\B�oB��B��B��B��B�-BŢB��B�HBR�B_;BVB\)B[#BP�BP�BT�BR�BP�BO�BN�BL�BN�BL�BH�BB�B7LB#�B\B  B��B�sB�)B�B��B�DB~�Bk�BbNBaHB[#B\)BYBK�BE�B@�B7LB2-B�B�B+B
��B
�B
��B
�VB
s�B
`BB
T�B
G�B
8RB
.B
 �B
bB
1B	��B	�yB	ȴB	��B	�FB	�B	:^B	�B	B�B�BB�BĜB�^B�B��B��B��B�{B�oB�\B�JB�DB�=B�1B�+B�B�B� B}�B{�B}�Bw�Bw�Bv�Bt�Bs�Bo�Bn�Bm�Bm�Bk�Bk�Bk�BhsBjBk�BjBhsBhsBffBdZBcTBbNB`BB`BBaHB`BBcTBcTBbNBaHB_;B[#BS�BL�BO�BM�BM�BN�BM�BM�BL�BK�BJ�BK�BK�BJ�BI�BH�BH�BH�BF�BA�B>wBA�B=qB7LB9XB6FB49B2-B2-B6FB5?B49B49B33B49B49B49B33B5?B/B.B.B-B33B49B6FB9XB=qBA�BI�BJ�BM�BM�BN�BK�BL�BN�BN�BN�BM�BO�BO�BM�BP�BQ�BQ�BR�BS�BW
BW
BXBW
B\)B]/BcTBdZBgmBk�Bw�B~�B�B�+B�B�PB�bB��B�FB��B�B�)B��B	PB	�B	�B	+B	6FB	G�B	J�B	S�B	^5B	iyB	w�B	~�B	�bB	�bB	��B	��B	�B	�wB	�qB	��B	��B	ɺB	��B	�/B	�TB	�mB	�sB	�B	�B	�B	��B	��B	��B
B
B
B
+B

=B
JB
PB
VB
oB
uB
�B
�B
�B
�B
�B
!�B
 �B
!�B
#�B
#�B
$�B
'�B
&�B
)�B
+B
/B
.B
0!B
2-B
2-B
5?B
8RB
7LB
8RB
7LB
9XB
:^B
:^B
:^B
;dB
<jB
=qB
>wB
?}B
A�B
E�B
F�B
F�B
F�B
G�B
F�B
J�B
J�B
K�B
L�B
L�B
N�B
N�B
P�B
P�B
P�B
P�B
R�B
S�B
S�B
VB
W
B
[#B
]/B
]/B
_;B
^5B
_;B
_;B
`BB
`BB
`BB
_;B
`BB
aHB
dZB
e`B
e`B
dZB
ffB
gmB
iyB
jB
l�B
n�B
o�B
p�B
q�B
q�B
q�B
r�B
s�B
s�B
t�B
u�B
u�B
v�B
v�B
w�B
v�B
v�B
x�B
z�B
y�B
x�B
y�B
z�B
z�B
|�B
}�B
}�B
}�B
~�B
}�B
~�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�+B
�+B
�=B
�DB
�DB
�JB
�PB
�PB
�VB
�\B
�\B
�bB
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
�B
��B
�B
�B
�B
�B
�B
�B
�B
�!B
�!B
�'B
�-B
�9B
�9B
�?B
�FB
�RB
�RB
�XB
�RB
�XB
�^B
�XB
�XB
�XB
�XB
�^B
�^B
�^B
�^B
�dB
�dB
�^B
�^B
�dB
�jB
�dB
�^B
�dB
�dB
�^B
�dB
�dB
�dB
�dB
�dB
�dB
�dB
�dB
�dB
�dB
�dB
�dB
�dB
�dB
�dB
�dB
�jB
�dB
�dB
�dB
�dB
�dB
�dB
�jBs�Bs�Bs�Br�Bt�Bs�Bs�Bt�Bs�Br�Bs�Bs�Bs�Bs�Bs�Br�Bs�Bs�Bs�Bs�Bs�Bs�Bs�Bs�Bs�Bs�Br�Bs�Bs�Bt�Bt�Bs�Bs�Bs�Bs�Bt�Bs�Bs�Br�Bs�Br�Bq�Bs�Bt�Br�Bt�Bt�Bs�Bs�Br�Br�Br�Bs�Bs�Br�Bs�Bs�Br�Bs�Bs�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           Bs�Bs�Bs�Br�Bs�Bs�Br�Bs�Bs�Bs�Bs�Bs�Bs�Bs�Bs�Bt�Bt�Bt�Bt�Bs�Bt�Bt�Bt�Bz�B}�B~�B��B�B�B�B�B�B�#B�#B�+B�,B�,B�-B�3B�4B�5B�5B�<B�6B�=B�>B�>B�?B�?B�@B�AB�MB�aB�sB��B��B��B�!BŗB��B�>BR�B_2BU�B\!B[BP�BP�BT�BR�BP�BO�BN�BL�BN�BL�BH�BB�B7KB#�B[B  B��B�tB�*B�B��B�FB~�Bk�BbRBaLB[(B\.BYBK�BE�B@�B7TB25B�B�B5B
��B
�B
��B
�aB
s�B
`NB
U
B
G�B
8_B
.!B
 �B
pB
@B	�B	�B	��B	��B	�WB	�*B	:oB	�B	0B��B�TB�#BįB�qB�"B��B��B��B��B��B�rB�aB�[B�UB�IB�DB�8B�&B�B~B|B~Bw�Bw�Bv�Bt�Bs�Bo�Bn�Bm�Bm�Bk�Bk�Bk�Bh�Bj�Bk�Bj�Bh�Bh�Bf�Bd�Bc{BbvB`jB`kBaqB`lBc~BcBbyBatB_hB[PBT%BL�BPBNBNBO	BNBNBL�BK�BJ�BK�BK�BJ�BI�BH�BH�BH�BF�BA�B>�BA�B=�B7�B9�B6B4sB2gB2gB6�B5{B4uB4vB3pB4wB4wB4xB3rB5B/[B.UB.UB-PB3uB4|B6�B9�B=�BA�BI�BKBNBNBO BLBMBO"BO"BO#BNBP*BP*BNBQ1BR9BR:BS@BTGBWYBWZBX`BW[B\zB]�Bc�Bd�Bg�Bk�Bx$BOB�hB��B�}B��B��B�EB��B�SBڎBܝB�^B	�B	#B	,B	+�B	6�B	H8B	KNB	T�B	^�B	jB	xjB	�B	�B	�B	�mB	��B	��B	�)B	�&B	�;B	�DB	�yB	ӴB	��B	�B	�8B	�AB	�OB	�dB	�zB	��B	��B	��B
�B
�B
B
B
*B
:B
CB
LB
hB
qB
�B
�B
�B
�B
�B
"�B
!�B
"�B
$�B
$�B
%�B
)B
(B
+'B
,0B
0LB
/IB
1YB
3hB
3kB
6�B
9�B
8�B
9�B
8�B
:�B
;�B
;�B
;�B
<�B
=�B
>�B
?�B
@�B
B�B
GB
HB
HB
HB
I&B
H"B
L>B
LAB
MIB
NRB
NUB
PcB
PfB
RuB
RwB
RzB
R}B
T�B
U�B
U�B
W�B
X�B
\�B
^�B
^�B
`�B
_�B
`�B
`�B
b B
bB
bB
aB
bB
cB
f)B
g2B
g5B
f2B
h@B
iJB
kYB
lbB
nqB
p�B
q�B
r�B
s�B
s�B
s�B
t�B
u�B
u�B
v�B
w�B
w�B
x�B
x�B
y�B
x�B
x�B
z�B
|�B
{�B
z�B
{�B
}B
}	B
B
�"B
�%B
�'B
�0B
�-B
�6B
�EB
�NB
�KB
�SB
�VB
�YB
�bB
�eB
�rB
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
�B
�"B
�(B
�:B
�LB
�SB
�fB
�rB
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
�B
�B
�B
�0B
�/B
�<B
�GB
�MB
�\B
�kB
��B
��B
��B
��B
��B
��B
�B
�B
�,B
�BB
�]B
�mB
��B
��B
��B
��B
��B
��B
��B
��B
�B
�B
�!B
�1B
�FB
�UB
�^B
�mB
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
��Bs�Bs�Bs�Br�Bt�Bs�Bs�Bt�Bs�Br�Bs�Bs�Bs�Bs�Bs�Br�Bs�Bs�Bs�Bs�Bs�Bs�Bs�Bs�Bs�Bs�Br�Bs�Bs�Bt�Bt�Bs�Bs�Bs�Bs�Bt�Bs�Bs�Br�Bs�Br�Bq�Bs�Bt�Br�Bt�Bt�Bs�Bs�Br�Br�Br�Bs�Bs�Br�Bs�Bs�Br�Bs�Bs�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�201903120200592021061413560820210614135608202106171314172021061713141720210617131417201903120200592021061413560820210614135608202106171314172021061713141720210617131417PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            new conductivity = original conductivity * (1 + delta*TEMP + CPcor_SBE*PRES) / (1 + delta*TEMP_ADJUSTED + CPcor_new*PRES_ADJUSTED)                                                                                                                              none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            new conductivity = original conductivity * (1 + delta*TEMP + CPcor_SBE*PRES) / (1 + delta*TEMP_ADJUSTED + CPcor_new*PRES_ADJUSTED)                                                                                                                              none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.50 dbar                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            CPcor_new = -1.25e-07; CPcor_SBE = -9.57e-08; delta = 3.25e-06                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.50 dbar                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            CPcor_new = -1.25e-07; CPcor_SBE = -9.57e-08; delta = 3.25e-06                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                      none                                                                                                                                                                                                                                                            New conductivity computed by using a different CPcor value from that provided by Sea-Bird.                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            No significant salinity drift detected.                                                                                                                                                                                                                         Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                      none                                                                                                                                                                                                                                                            New conductivity computed by using a different CPcor value from that provided by Sea-Bird.                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            No significant salinity drift detected.                                                                                                                                                                                                                         PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            AO  AO  ARCAARCAADJPADJP                                                                                                                                        2019031202005920190312020059  IP  IP                                G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2019031202005920190312020059QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�5F03E           703E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2019031202005920190312020059QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�0               0               PM  PM  ARSQARSQCOWGCOWGV1.1V1.1                                                                                                                                2021061713151620210617131516IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                