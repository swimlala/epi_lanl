CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-12-24T03:00:55Z creation      
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
resolution        =���   axis      Z        �  ;�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  K�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  O�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  _�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  c�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  s�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �l   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �h   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �P   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �L   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �4   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �    PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                   L   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                   T   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                   \   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                   d   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  � l   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                   �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                   	   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    	   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar        	0   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar        	8   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�       	@   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    	H   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  � �Argo profile    3.1 1.2 19500101000000  20181224030055  20210617131513  5905739 5905739 US ARGO PROJECT                                                 US ARGO PROJECT                                                 GREGORY C. JOHNSON                                              GREGORY C. JOHNSON                                              PRES            TEMP            PSAL            PRES            TEMP            PSAL               7   7DD  AOAO7219                            7219                            2C  2C  DD  SOLO_D_MRV                      SOLO_D_MRV                      12002                           12002                           SBE603 01Dec16                  SBE603 01Dec16                  874 874 @ؙ`Ky�@ؙ`Ky�11  @ؙ`""20@ؙ`""20@6Xۋ�q@6Xۋ�q�c�4���c�4��11  GPS     GPS     Primary sampling: averaged [1Hz sampling by SBE-41CP averaged in 2-dbar bins]                                                                                                                                                                                   Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 AC  AA  AA  >���?���@ff@Fff@�ff@�33@���@�  A33A  A#33A@  A^ffA~ffA�  A�  A���A���A���A���A�33B ffBffB  B��B   B(ffB0  B8  B@ffBH  BP  BX  B`  Bg��BpffBxffB�  B�  B�  B�33B�33B�33B�33B�  B�  B�ffB�33B�  B�  B�ffB�ffB�33B�33B���B���B�  B���B�  B�33Bܙ�B���B䙚B�ffB�ffB�  B�ffB�ffB���B���C�CL�CL�C�C	��C  C33CL�C33C�fC�CL�C  C�fC�C L�C"�C#��C&�C(33C)�fC+��C-�fC033C2�C4  C633C833C9�fC<33C>�C?�fCB�CC�fCE�3CG�fCJ33CL�CM�fCP  CR33CT33CV  CW�3CY��C[�fC^  C`33Cb  Cc�3Ce��Cg�fCi�fCl  Cn  Cp  Cr  Ct  Cv�Cx  Cz�C|�C~33C��C��C��C��C�&fC�33C��C��fC��3C�  C��C��C��C��fC��3C��C��C��C��fC��C�&fC��C�  C��C��C�  C�&fC��C��C�  C��fC�  C��C�  C��fC�  C��C��C��3C�  C��C��C�&fC��C��3C��3C��C��C��C��3C�  C��C��C�&fC��C�&fC�33C��C��C�33C��C��fC��fC��fC��fC��fC��fC�ٚC�ٚC�  C�&fC�33C�&fC�&fC�&fC�&fC�&fC��C�ٚC��3C��3C��3C�  C�  C��C��C��C�&fC�33C��C��fC��3C�  C��C�&fC��C�  C��C�&fC��C�  C��C��C�&fC�&fC��C��3C�  C��C�&fC�&fC�&fC��C��3C�  C��C��C�&fC��C��fC��3C��3C�  DٚD9�D��D��D@ D��D�fD9�D��D� D@ D� DfD �fD"��D%� D(  D*y�D,��D/ffD1��D4L�D6��D9  D;��D>fD@y�DB�fDEY�DGٚDJS3DL�3DO  DQ�fDS� DV@ DX� D[  D]�3D`  Dby�Dd��DgS3Di��Dl9�Dn�3Dq  Dss3Du� DxY�Dz� D}�D��D� D�Y�D��fD��D�&fD�l�D�� D��3D��D�L�D�y�D��3D��3D��3D�3D�6fD�L�D�s3D��fD�� D��3D��fD���D���D�	�D��D�6fD�FfD�Y�D�y�D���D��3D��3D���D�fD�0 D�S3D��fD�� D���D�3D�0 D�S3D�y�D��3D��3D�� D��fD�  D�FfD�p D��3D�� D��D�	�D�,�D�P D�vfDÖfDĶfD�ٚD���D� D�#3D�<�D�Y�D�y�D͐ Dγ3D��fD�ٚD���D�fD�fD�,�D�<�D�L�D�VfD�i�D�|�Dی�Dܜ�DݦfD޳3D��3D��3D�� D��D���D�� D�� D��3D��3D���D��fD�� D��3D�ɚD�� D�3D�fD��D�fD�D� D�vfD�l�D�\�D�I�D�<�D�0 D�&fD�fD���D���D��fD���D��3E nfE �3Eh Ec3E�fEњE;3E��E��E
  Ea�ENfE� E E  E^fE�3EfEP E�fEvfE��E��E8 E�E��E` E 9�E!�fE"� E$!�E%q�E&��E(3E)Q�E*�3E+x E,�fE.\�E/��E0��E2	�E3<�E4l�E5�3E6��E8H E9c3E:vfE;�E>� EB#3EE�EHi�EKc3EN� EQ� ET�fEX#3EZ�E^K3EaVfEdS3Eg{3EjњEm�3EnP Eo.fEo��EpP Ep�3Eqp ErH Er� Esk3Es��Et�fEuX Eu�3Evp Ew@ Ew�f>L��>L��>���>L��>���>L��>���>���>���>���>L��>L��>���>L��=���>���>���=���>���>L��>���>L��>L��>���>���>���>���?   ?   ?   ?L��?fff?�  ?���?�ff?�  ?ٙ�@   @33@   @9��@Fff@Y��@s33@�ff@�  @���@���@�33@�ff@�33@�  @�  @���AffA��A33A33A!��A+33G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111414141441441441441414414411441111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                   ?L��?���@&ff@fff@�ff@�33@���@�  A33A  A+33AH  AfffA�33A�  A�  A���Ař�A���A���A�33BffB
ffB  B��B"  B*ffB2  B:  BBffBJ  BR  BZ  Bb  Bi��BrffBzffB�  B�  B�  B�33B�33B�33B�33B�  B�  B�ffB�33B�  B�  B�ffB�ffB�33B�33B���B���B�  B���B�  B�33Bݙ�B���B噚B�ffB�ffB�  B�ffB�ffB���C ffC��C��C��C��C
L�C� C�3C��C�3CffC��C��C� CffC��C ��C"��C$L�C&��C(�3C*ffC,L�C.ffC0�3C2��C4� C6�3C8�3C:ffC<�3C>��C@ffCB��CDffCF33CHffCJ�3CL��CNffCP� CR�3CT�3CV� CX33CZL�C\ffC^� C`�3Cb� Cd33CfL�ChffCjffCl� Cn� Cp� Cr� Ct� Cv��Cx� Cz��C|��C~�3C�Y�C�Y�C�Y�C�Y�C�ffC�s3C�L�C�&fC�33C�@ C�Y�C�Y�C�L�C�&fC�33C�L�C�Y�C�L�C�&fC�L�C�ffC�L�C�@ C�Y�C�L�C�@ C�ffC�Y�C�L�C�@ C�&fC�@ C�Y�C�@ C�&fC�@ C�Y�C�L�C�33C�@ C�L�C�Y�C�ffC�L�C�33C�33C�Y�C�Y�C�L�C�33C�@ C�L�C�Y�C�ffC�Y�C�ffC�s3C�L�C�L�C�s3C�L�C�&fC�&fC�&fC�&fC�&fC�&fC��C��C�@ C�ffC�s3C�ffC�ffC�ffC�ffC�ffC�L�C��C�33C�33C�33C�@ C�@ C�L�C�Y�C�Y�C�ffC�s3C�L�C�&fC�33C�@ C�L�C�ffC�Y�C�@ C�L�C�ffC�Y�C�@ C�L�C�Y�C�ffC�ffC�L�C�33C�@ C�Y�C�ffC�ffC�ffC�L�C�33C�@ C�L�C�Y�C�ffC�L�C�&fC�33C�33C�@ D��DY�D��D	�D` D��DfDY�D��D  D` D� D&fD �fD#�D%� D(  D*��D-�D/�fD1��D4l�D6ٚD9@ D;��D>&fD@��DCfDEy�DG��DJs3DL�3DO@ DQ�fDT  DV` DX� D[@ D]�3D`  Db��De�Dgs3Di��DlY�Dn�3Dq  Ds�3Dv  Dxy�D{  D},�D��D�  D�i�D��fD���D�6fD�|�D�� D��3D�,�D�\�D���D��3D��3D�3D�#3D�FfD�\�D��3D��fD�� D��3D��fD���D�	�D��D�,�D�FfD�VfD�i�D���D���D��3D��3D���D�fD�@ D�c3D��fD�� D���D�3D�@ D�c3D���D��3D��3D�� D�fD�0 D�VfD�� D��3D�� D���D��D�<�D�` DfDæfD��fD��D�	�D�  D�33D�L�D�i�D̉�D͠ D��3D��fD��D���D�fD�&fD�<�D�L�D�\�D�ffD�y�Dڌ�Dۜ�Dܬ�DݶfD��3D��3D��3D�� D���D���D�  D�  D�3D�3D���D��fD�� D��3D�ٚD�� D��3D�fD��D�fD�D� D�fD�|�D�l�D�Y�D�L�D�@ D�6fD�fD��D�	�D�fD���D��3E vfE �3Ep Ek3E�fEٚEC3E��E��E
 Ei�EVfE� E E EffE�3EfEX E�fE~fE��E�E@ E�E��Eh E A�E!�fE"� E$)�E%y�E&��E(3E)Y�E*�3E+� E,�fE.d�E/��E0��E2�E3D�E4t�E5�3E6��E8P E9k3E:~fE;�E>� EB+3EE�EHq�EKk3EN� EQ� ET�fEX+3EZ��E^S3Ea^fEd[3Eg�3EjٚEm�3EnX Eo6fEoɚEpX Ep�3Eqx ErP Er� Ess3Et�Et�fEu` Eu�3Evx EwH Ew�fG�O�?333G�O�?333G�O�?333G�O�G�O�?L��G�O�G�O�?333G�O�G�O�?��G�O�G�O�?��G�O�?333G�O�G�O�?333G�O�G�O�?L��?fffG�O�G�O�?�  ?�ff?�33?�  ?���?�ff@   @��@   @333@@  @Y��@fff@y��@���@�ff@�  @���@���@�33@�ff@�33@�  A   AffAffA��A33A#33A)��A333G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111414141441441441441414414411441111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                   @ @ �@ V@ *@ �@ "�@ (G@ /�@ 7�@ =q@ E�@ Q�@ ^�@ l�@ z�@ ��@ ��@ �5@ �-@ ��@ �@ �#@ ��@ ��@�@@g@,`@:@H]@UU@b�@p�@~K@�D@�H@��@��@@�7@ލ@�4@��@�@{@""@1'@>@K@X�@g�@uk@�d@�@�U@��@�R@�J@Ӡ@��@��@�Q@J@B@&�@33@>�@Lu@[z@i!@x&@�+@��@�@��@��@��@�@�@�@ �@�@O@(G@7L@FQ@R�@^5@m�@|?@��@��@�(@��@��@��@��@�y@�@�@�@
@-@9X@E�@T�@dZ@qS@}�@��@��@��@��@��@��@�/@�@��@�@o@ �@/@<�@K@X�@ff@t@��@�@�@�Y@�@�W@��@�@�L@��@J@�@&;@1�@@,@N�@]�@k.@x&@��@�@�@�r@�k@�@׹@�@�@  @@�@(�@8�@E�@R�@_�@k�@z�@��@�0@�y@�~@��@�|@��@�@�q@	�@	@	g@	+�@	9X@	I@	V�@	c�@	o�@	~K@	��@	��@	�M@	�F@	Ĝ@	��@	ލ@	�4@	��@
�@
@
 �@
.l@
<@
I�@
Wb@
dZ@
r@
��@
�h@
��@
��@
�^@
�@
խ@
�T@
�@
��@	�@6@$�@33@@�@O0@]�@k.@y�@��@�u@�@�f@��@�@�@�@�Y@ �@�@�@(�@7L@E�@S�@a�@m�@z3@��@��@��@��@�2@�|@��@�@�q@�@@g@+@9X@F�@UU@��@�@[z@��@�t@B@Yn@��@�h@�@X�@��@�#@g@bN@�A@�@/@r@��@� @;d@}�@�&@^@D�@��@�@�@Q=@��@խ@�@Yn@��@�t@�@_�@�y@�@(G@k.@��@��@2�@r�@��@��@:@}�@@��@B8@��@��@�@Z�@�a@�@(�@j@�f@�@@.l@m�@��@�4@)�@g�@�(@��@  @ \)@ ��@ є@!�@!FQ@!�W@!�@!� @"1'@"k�@"�M@"�@#g@#\�@#��@#�h@$�@$V@$��@$�
@%6@%V@%�0@%�O@&@&O0@&��@&�c@'1@'G�@'�|@'��@(@(DD@(��@(�2@(�Q@)=q@)|?@)��@)� @*5@@*r�@*�@*��@+$�@+a�@+�@+�t@,�@,SI@,��@,ȴ@-�@->�@-z3@-�9@-�@@.&�@.a�@.�U@.�\@/b@/I@/�d@/�j@/�q@0/�@0hs@0��@0�
@1�@1D�@1{�@1��@1�@2O@2O0@2��@2�R@2�4@3 @3T�@3��@3��@3�Y@4&�@4[z@4��@4�2@4�@5(�@5]�@5�P@5��@5��@6-�@6bN@6��@6�@7]@76�@7��@8C�@8�Z@9E�@9��@:FQ@:�H@;x&@;�/@<t@=
=@=p�@>%@>��@?(G@?��@@C�@@��@A+@A�-@B:@B�@CI�@D  @D\�@D��@Eww@F�@F��@G$/@G��@H> @H��@I(�@I��@Je	@J�4@Kr�@K�q@Ly�@L�9@M|?@M�9@N�m@OB@O��@P-@Qm:@R��@T*@U�d@V�W@X33@Y��@Z�/@\7�@]j@^�h@`$�@ak.@b��@d0x@en�@e�@f�@fK�@f��@f�W@gj@g_�@g�@g��@h�@hX@h�@h�y@i%�@i~K@i��G�O�@ ^G�O�@ ^G�O�@ ^G�O�G�O�@ G�O�G�O�@ ^G�O�G�O�@  �G�O�G�O�@  �G�O�@ ^G�O�G�O�@ ^G�O�G�O�@ @ �G�O�G�O�@ j@ v@ %@ �@ �@ �@ 
=@ �@ �@ �@ @ �@ *@ 6@ �@ �@ �@ !s@ $.@ &;@ *S@ -@ /�@ 33@ 5�@ 9X@ <@ >�@ B8@ D�@ IG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�
=A��A�{A���A���A���A�1A��A� �A� �A�$�A�(�A�+A�-A�-A�1'A�;dA�;dA�7LA�7LA�5?A�7LA�9XA�9XA�5?A�33A�1'A�33A�7LA�7LA�5?A�5?A�33A�1'A�/A�5?A�1'A�5?A�;dA�;dA�=qA�;dA�?}A�C�A�A�A�A�A�A�A�G�A�A�A�?}A�A�A�C�A�C�A�E�A�C�A�1'A�9XA�(�A��A��A���A�~�A��FA���A��DA�VA�$�A�ĜA���A�&�A���A�$�A�$�A���A��A��#A��A��A�A�A�A���A���A���A�O�A���A�"�A��A���A��A�JA��`A�ƨA�VA�M�A���A�VA�/A��HA�ȴA��A���A��TA�n�A�ZA�A�A��A��A�l�A���A�/A�A�A�~�A�A�=qA��9A�ffA�l�A�33A���A�\)A��A���A�|�A&�A}�;A}O�AyƨAv�As��AqAn��AmhsAiƨAfQ�Ab �A_S�A^�\A]t�AY�#AW��AU\)AT5?AS�AR��AP�`AO+AM�AK�AIAG�ADJAB�RAB  AA+A@��A?7LA=p�A<��A<�9A<��A<I�A;�^A;XA:=qA8�9A8�A8��A7�wA7+A6�/A4r�A2v�A//A-C�A,�+A*�!A(��A'�A%��A%��A$��A#�^A#�A"�A!|�A �+A�#AG�A�+A��A�A�A��A�#A��AhsAƨA��A��A��A�uA^5A�Av�A�Ap�AbNA��A��AO�A��A�A
�!A
9XA	�-A	
=Av�A�hA`BA�A-AA"�A n�@�S�@���@�E�@��@���@��-@�A�@��;@��F@�v�@�%@�C�@��@�?}@�j@�Q�@��@�9X@�K�@�`B@��y@��`@�t�@���@�A�@ޗ�@��@�%@�ȴ@��@�dZ@�5?@�^5@Ȭ@ă@���@�;d@��^@���@���@�"�@��;@��-@��m@�o@�ff@�@���@���@�x�@�9X@�V@��@�p�@�t�@��@��H@�=q@�G�@�M�@���@��9@��@��j@�A�@�~�@�/@���@���@��@��
@���@�\)@�@�O�@���@��j@�r�@�+@�ff@�E�@��@���@�@~{@}��@|�@z�@w|�@w
=@v{@tj@q��@p1'@oK�@l�/@j^5@i%@hr�@g+@f5?@c33@a�7@_�;@^@]/@\I�@Z�@W�@V�y@Vff@U/@S�@R-@Qhs@P �@N��@L��@L��@Kƨ@I��@H�9@G��@F@D�D@C33@@�`@?�;@>�R@=�h@<��@:��@8��@7;d@6E�@5�-@5V@4z�@3��@2�@0��@/�;@-�@,�@+�F@+C�@*�\@)��@(��@'��@&��@&ff@%@%V@$9X@#dZ@"~�@"M�@!�#@ ��@  �@ b@K�@�R@O�@z�@t�@�!@��@G�@��@1'@�@E�@��@��@9X@��@n�@J@7L@�9@��@��@��@��@�@�/@j@�F@��@@
=q@	��@	7L@�@��@l�@��@$�@�T@�@��@�/@��@I�@@ �`?�{?��?�=q?��9?�E�?�S�?��??�O�?�?�C�?�^?�b?��?䛦?�!?�Ĝ?߾w?޸R?܋D?ۅ?ٙ�?�Q�?��y?Ձ?�z�?�33?�-?�&�?�A�?���?�v�?͑h?�V?�(�?�?�^5?�x�?�X?��?�r�?�l�?�ȴ?�E�?��?öF?\?��7?���?��?�O�?�ƨ?��?�~�?���?���?��?�1'?�b?�r�?��9?���?���?���?���?���?���?�7L?�7L?�7L?�X?�x�?�x�?�x�?���?�x�?�x�A��A��yA��HA��TA��HA��TA��#A�A�
=A��A�{A��A��A�oA�A�VA��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A�{A��A��A��A��A�
=A���A���A���A���A���A���A���A�A�1A�1A�JA� �A� �A� �A� �A� �A� �A� �A�$�A�&�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                   A�
=A��A�{A���A���A���A�1A��A� �A� �A�$�A�(�A�+A�-A�-A�1'A�;dA�;dA�7LA�7LA�5?A�7LA�9XA�9XA�5?A�33A�1'A�33A�7LA�7LA�5?A�5?A�33A�1'A�/A�5?A�1'A�5?A�;dA�;dA�=qA�;dA�?}A�C�A�A�A�A�A�A�A�G�A�A�A�?}A�A�A�C�A�C�A�E�A�C�A�1'A�9XA�(�A��A��A���A�~�A��FA���A��DA�VA�$�A�ĜA���A�&�A���A�$�A�$�A���A��A��#A��A��A�A�A�A���A���A���A�O�A���A�"�A��A���A��A�JA��`A�ƨA�VA�M�A���A�VA�/A��HA�ȴA��A���A��TA�n�A�ZA�A�A��A��A�l�A���A�/A�A�A�~�A�A�=qA��9A�ffA�l�A�33A���A�\)A��A���A�|�A&�A}�;A}O�AyƨAv�As��AqAn��AmhsAiƨAfQ�Ab �A_S�A^�\A]t�AY�#AW��AU\)AT5?AS�AR��AP�`AO+AM�AK�AIAG�ADJAB�RAB  AA+A@��A?7LA=p�A<��A<�9A<��A<I�A;�^A;XA:=qA8�9A8�A8��A7�wA7+A6�/A4r�A2v�A//A-C�A,�+A*�!A(��A'�A%��A%��A$��A#�^A#�A"�A!|�A �+A�#AG�A�+A��A�A�A��A�#A��AhsAƨA��A��A��A�uA^5A�Av�A�Ap�AbNA��A��AO�A��A�A
�!A
9XA	�-A	
=Av�A�hA`BA�A-AA"�A n�@�S�@���@�E�@��@���@��-@�A�@��;@��F@�v�@�%@�C�@��@�?}@�j@�Q�@��@�9X@�K�@�`B@��y@��`@�t�@���@�A�@ޗ�@��@�%@�ȴ@��@�dZ@�5?@�^5@Ȭ@ă@���@�;d@��^@���@���@�"�@��;@��-@��m@�o@�ff@�@���@���@�x�@�9X@�V@��@�p�@�t�@��@��H@�=q@�G�@�M�@���@��9@��@��j@�A�@�~�@�/@���@���@��@��
@���@�\)@�@�O�@���@��j@�r�@�+@�ff@�E�@��@���@�@~{@}��@|�@z�@w|�@w
=@v{@tj@q��@p1'@oK�@l�/@j^5@i%@hr�@g+@f5?@c33@a�7@_�;@^@]/@\I�@Z�@W�@V�y@Vff@U/@S�@R-@Qhs@P �@N��@L��@L��@Kƨ@I��@H�9@G��@F@D�D@C33@@�`@?�;@>�R@=�h@<��@:��@8��@7;d@6E�@5�-@5V@4z�@3��@2�@0��@/�;@-�@,�@+�F@+C�@*�\@)��@(��@'��@&��@&ff@%@%V@$9X@#dZ@"~�@"M�@!�#@ ��@  �@ b@K�@�R@O�@z�@t�@�!@��@G�@��@1'@�@E�@��@��@9X@��@n�@J@7L@�9@��@��@��@��@�@�/@j@�F@��@@
=q@	��@	7L@�@��@l�@��@$�@�T@�@��@�/@��@I�@@ �`?�{?��?�=q?��9?�E�?�S�?��??�O�?�?�C�?�^?�b?��?䛦?�!?�Ĝ?߾w?޸R?܋D?ۅ?ٙ�?�Q�?��y?Ձ?�z�?�33?�-?�&�?�A�?���?�v�?͑h?�V?�(�?�?�^5?�x�?�X?��?�r�?�l�?�ȴ?�E�?��?öF?\?��7?���?��?�O�?�ƨ?��?�~�?���?���?��?�1'?�b?�r�?��9?���?���?���?���?���?���?�7L?�7L?�7L?�X?�x�?�x�?�x�?���?�x�?�x�A��A��yA��HA��TA��HA��TA��#A�A�
=A��A�{A��A��A�oA�A�VA��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A�{A��A��A��A��A�
=A���A���A���A���A���A���A���A�A�1A�1A�JA� �A� �A� �A� �A� �A� �A� �A�$�A�&�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B�7B�1B�1B�=B�7B�=B�7B�7B�1B�1B�1B�1B�1B�1B�1B�1B�1B�1B�1B�1B�1B�1B�1B�1B�1B�1B�1B�1B�1B�1B�7B�7B�1B�1B�1B�1B�1B�1B�1B�1B�7B�1B�1B�1B�7B�1B�7B�1B�1B�1B�1B�7B�7B�7B�1B�1B�1B�+B�%B�Bu�B�1B�B�=B�+B�B�%B�B�=B�B� B}�B|�By�Bu�By�Bw�Bt�Bq�Bn�BgmBcTB`BB\)BT�BS�BO�BC�B9XB2-B1'B.B#�B\BDBB��B��B�B�TB�
B�wB��B�VB�B|�Bq�B`BB[#BM�BB�B6FB.B
=B
�B
ɺB
�dB
�LB
�'B
��B
�1B
p�B
bNB
Q�B
K�B
B�B
#�B
B	��B	�mB	�HB	�B	��B	��B	�=B	z�B	r�B	k�B	Q�B	<jB	2-B	(�B	!�B	�B	oB	VB	B�B�`B�
BǮB��B�dB�FB�3B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�+B� B|�Bx�Bs�BhsBgmBcTB`BB]/B[#B[#BXBYBT�BT�BS�BR�BQ�BP�BQ�BN�BM�BL�BJ�BH�BG�BF�BF�BD�BD�BB�BB�BB�BB�B?}B?}B=qB>wB<jB?}B?}B=qB<jB<jB<jB:^B;dB:^B6FB;dB8RB<jB9XB9XB7LB6FB5?B5?B6FB5?B5?B33B49B33B5?B6FB6FB6FB6FB5?B7LB8RB7LB9XB<jB<jB<jB<jB>wB>wB<jBA�BB�BD�BD�BN�BXB_;BjBm�B�B�PB�uB��B��B�B�^BÖB��B�/B�B	B	bB	�B	!�B	6FB	C�B	H�B	M�B	XB	cTB	r�B	v�B	� B	�B	�+B	�1B	�{B	��B	��B	�B	�3B	�^B	�jB	ƨB	��B	��B	��B	��B	��B	�)B	�BB	�NB	�TB	�NB	�mB	�B	�B	�B	�B	��B
  B
B
B
%B
	7B
JB
JB
\B
\B
�B
�B
�B
�B
�B
!�B
$�B
%�B
'�B
)�B
,B
-B
.B
.B
.B
/B
1'B
0!B
2-B
49B
5?B
6FB
7LB
8RB
8RB
;dB
>wB
?}B
A�B
C�B
C�B
D�B
E�B
F�B
G�B
H�B
H�B
H�B
I�B
J�B
J�B
K�B
L�B
N�B
N�B
Q�B
Q�B
R�B
T�B
T�B
T�B
VB
W
B
XB
XB
ZB
ZB
[#B
\)B
^5B
]/B
]/B
^5B
_;B
_;B
`BB
aHB
bNB
dZB
dZB
e`B
ffB
gmB
ffB
gmB
hsB
iyB
iyB
iyB
k�B
l�B
m�B
n�B
n�B
o�B
o�B
o�B
p�B
r�B
r�B
s�B
s�B
t�B
s�B
u�B
u�B
u�B
v�B
w�B
x�B
y�B
z�B
z�B
z�B
{�B
{�B
{�B
{�B
|�B
~�B
� B
�B
�B
�B
�B
�1B
�7B
�=B
�DB
�PB
�PB
�VB
�VB
�bB
�bB
�hB
�uB
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
�B
�B
�B
�!B
�'B
�'B
�9B
�9B
�?B
�?B
�FB
�LB
�RB
�RB
�RB
�RB
�XB
�RB
�RB
�XB
�XB
�XB
�RB
�XB
�XB
�XB
�XB
�XB
�XB
�XB
�XB
�XB�7B�=B�=B�JB�=B�%B�PB�VB�1B�1B�1B�7B�7B�B�JB�=B�1B�1B�+B�1B�1B�1B�7B�1B�+B�7B�7B�1B�1B�1B�1B�1B�1B�1B�7B�7B�1B�1B�1B�+B�DB�7B�7B�=B�7B�7B�7B�DB�7B�1B�DB�1B�1B�1B�1B�1B�1B�1B�1B�1G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                   B�B�B�B�B�B�B�B�B�
B�
B�
B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�#B�B�B�B�%B� B�&B�!B�"B�"B�#B�)B�*B�+B�%B�&B�&B�!B�B�Bu�B�)B�B�5B�$B�B�B�B�7B�B�B}�B|�By�Bu�By�Bw�Bt�Bq�Bn�BgmBcUB`CB\+BUBS�BO�BC�B9]B22B1-B.B#�BcBLBB��B��B��B�^B�B��B�B�bB�B|�Bq�B`PB[1BM�BB�B6VB.$B
MB
��B
��B
�uB
�^B
�9B
��B
�DB
p�B
bbB
R B
K�B
B�B
#�B
5B	��B	�B	�_B	�.B	��B	�B	�UB	z�B	r�B	k�B	RB	<�B	2GB	)B	!�B	�B	�B	sB	6B��B�~B�(B��B��B��B�fB�TB�B�B�B�B�B�B�B��B��B��B�B�B�B�B��B�TB�)B}Bx�Bs�Bh�Bg�Bc�B`nB]\B[PB[QBX>BYFBU-BU.BT(BS#BRBQBRBOBNBMBJ�BH�BG�BF�BF�BD�BD�BB�BB�BB�BB�B?�B?�B=�B>�B<�B?�B?�B=�B<�B<�B<�B:�B;�B:�B6�B;�B8�B<�B9�B9�B7�B6�B5�B5�B6�B5�B5�B3zB4�B3{B5�B6�B6�B6�B6�B5�B7�B8�B7�B9�B<�B<�B<�B<�B>�B>�B<�BA�BB�BD�BD�BO1BXkB_�Bj�Bm�B�qB��B��B� B�AB��B��B�B�gBݭB�%B	�B	�B	*B	"XB	6�B	D)B	IIB	NkB	X�B	c�B	sQB	wmB	��B	��B	��B	��B	�-B	�mB	��B	��B	��B	�B	�-B	�nB	͖B	ϥB	͜B	ѷB	��B	� B	�B	�+B	�4B	�1B	�SB	�hB	��B	��B	��B	��B
 �B
B
B
&B

;B
QB
TB
hB
kB
�B
�B
�B
�B
�B
"�B
%�B
'B
)B
+&B
-4B
.=B
/EB
/HB
/KB
0TB
2cB
1_B
3nB
5|B
6�B
7�B
8�B
9�B
9�B
<�B
?�B
@�B
B�B
D�B
D�B
F B
G	B
HB
IB
J#B
J&B
J)B
K1B
L;B
L>B
MGB
NOB
P^B
PaB
SwB
SyB
T�B
V�B
V�B
V�B
W�B
X�B
Y�B
Y�B
[�B
[�B
\�B
]�B
_�B
^�B
^�B
_�B
`�B
`�B
bB
c
B
dB
f!B
f#B
g,B
h5B
i>B
h:B
iCB
jLB
kTB
kWB
kYB
mgB
npB
oxB
p�B
p�B
q�B
q�B
q�B
r�B
t�B
t�B
u�B
u�B
v�B
u�B
w�B
w�B
w�B
x�B
y�B
z�B
{�B
|�B
|�B
|�B
~B
~B
~B
~
B
B
�$B
�1B
�HB
�OB
�cB
�gB
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
�B
�B
�B
�$B
�2B
�BB
�QB
�[B
�aB
�nB
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
� B
�B
�B
� B
�&B
�+B
�>B
�RB
�nB
�}B
��B
��B
��B
��B
��B
�B
�B
�0B
�EB
�YB
�iB
�yB
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
��B�B�B�B� B�B��B�&B�,B�B�B�B�B�B��B� B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�	B�B�	B�	B�
B�
B�
B�
B�
B�
B�
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�201812240300552021061413560120210614135601202106171313572021061713135720210617131357201812240300552021061413560120210614135601202106171313572021061713135720210617131357PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            new conductivity = original conductivity * (1 + delta*TEMP + CPcor_SBE*PRES) / (1 + delta*TEMP_ADJUSTED + CPcor_new*PRES_ADJUSTED)                                                                                                                              none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            new conductivity = original conductivity * (1 + delta*TEMP + CPcor_SBE*PRES) / (1 + delta*TEMP_ADJUSTED + CPcor_new*PRES_ADJUSTED)                                                                                                                              none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.50 dbar                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            CPcor_new = -1.25e-07; CPcor_SBE = -9.57e-08; delta = 3.25e-06                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.50 dbar                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            CPcor_new = -1.25e-07; CPcor_SBE = -9.57e-08; delta = 3.25e-06                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                      none                                                                                                                                                                                                                                                            New conductivity computed by using a different CPcor value from that provided by Sea-Bird.                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            No significant salinity drift detected.                                                                                                                                                                                                                         Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                      none                                                                                                                                                                                                                                                            New conductivity computed by using a different CPcor value from that provided by Sea-Bird.                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            No significant salinity drift detected.                                                                                                                                                                                                                         PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            AO  AO  ARCAARCAADJPADJP                                                                                                                                        2018122403005520181224030055  IP  IP                                G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2018122403005520181224030055QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�5F03E           703E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2018122403005520181224030055QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�0               0               PM  PM  ARSQARSQCOWGCOWGV1.1V1.1                                                                                                                                2021061713151320210617131513IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                