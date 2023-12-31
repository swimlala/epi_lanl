CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS     N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-11-12T01:01:04Z creation      
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
_FillValue                 8  L�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  P�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 8  a�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  e�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  v�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 8  ��   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 8  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 8     PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 8  ׬   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                   $   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                   ,   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                   4   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                   <   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  � D   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                   �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                   �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar           HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar           HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�          HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                        SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  �   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �d   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �d   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                   d   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  � dArgo profile    3.1 1.2 19500101000000  20181112010104  20210617131508  5905739 5905739 US ARGO PROJECT                                                 US ARGO PROJECT                                                 GREGORY C. JOHNSON                                              GREGORY C. JOHNSON                                              PRES            TEMP            PSAL            PRES            TEMP            PSAL               -   -DD  AOAO7219                            7219                            2C  2C  DD  SOLO_D_MRV                      SOLO_D_MRV                      12002                           12002                           SBE603 01Dec16                  SBE603 01Dec16                  874 874 @؍���A�@؍���A�11  @؍�����@؍�����@6��JA@6��JA�c��m�c��m11  GPS     GPS     Primary sampling: averaged [1Hz sampling by SBE-41CP averaged in 2-dbar bins]                                                                                                                                                                                   Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                              	   	AB  AA  AA  >���?fff@   @@  @�  @�33@�  @���A   A  A$��AA��A`  A~ffA�33A�  A�  A�33A�  A���A���B ��BffB  B��B   B(��B0  B7��B?��BHffBPffBX��B`ffBhffBpffBx��B�ffB�33B�33B�  B�33B�ffB�ffB�33B���B�  B�  B�33B�  B���B���B���B���B�  B�  B�  Bϙ�B�  B�ffB�ffB���B�ffB�  B�  B���B���B�ffB�ffC   CL�C33C�C  C	��C�C  C�fC��C��C  CL�C33C33C�C   C"33C$33C&�C(�C*  C,  C.  C0  C2  C4  C5�fC833C:33C<33C>33C@�CB  CC��CF33CH�CI�fCL33CN  CO��CR  CT�CVL�CX�CY�fC\�C^�C_�fCa��Cd�CfL�Ch�Ci�3Ck��Cm��Co��Cq�3Ct�Cv�Cw�3Cy��C{�3C~�C�  C�  C�&fC�&fC��C�&fC�&fC�&fC�&fC�  C��fC��fC��3C��C��C��C��C�  C��fC��3C��C��C��C��fC��3C��C�  C��fC��C�  C��fC��C��C��C�&fC��C��C�  C��3C�&fC��C��C��3C��fC��C��C�  C��3C��3C��C��C�  C�  C��3C��C�  C�  C�  C��fC��C�  C�  C��C��C�  C��C�  C��fC�  C��3C��3C�  C��C�&fC��C��fC��3C��C��C�  C��fC��3C��C��C�&fC��C��3C�  C��C�&fC�&fC�&fC�&fC�33C��C��fC��3C��3C��3C��3C��3C�  C��C��C�&fC�&fC�&fC�33C��C�&fC��C��C��C��C��C��C��C��C�&fC�&fC�&fC��3C�ٚC��fC��3C��fC��3C��3D   D � D�D�3D�fD	FfDfD��D��D` D&fD� D� DY�D"�D$��D'�fD*&fD,� D/` D1��D4�fD7&fD9�3D<@ D>�3DAY�DC�3DF��DI&fDK��DNffDQfDS�fDV3DX� D[,�D]ٚD`s3Dc3De�3DhFfDjٚDml�Do��Dr�fDu�Dw�fDy�3D|  D~s3D�|�D�� D��fD�,�D�` D��fD���D�fD�33D�i�D��3D��3D��D�9�D�i�D��fD���D��D�9�D�i�D�� D�� D���D�)�D�` D���D���D��D�  D�S3D�� D���D�� D� D�<�D�l�D�� D�ɚD��fD�#3D�L�D�|�D��fD�� D��fD�#3D�L�D�s3D��3D���D�� D�	�D�,�D�S3D�|�D��3D�� D���D��D�<�D�Y�DȀ Dɩ�D�ٚD�  D�#3D�P D�s3DЖfDѶfD���D��fD�fD�6fD�S3D�p Dٌ�Dک�D��3D��3D�fD�)�D�I�D�l�D�3D� D�fD��fD���D� D�,�D�FfD�Y�D�y�D��DD��D�� D���D�3D��D�<�D�` D�� D���D�ɚD��fD���D��D�FfD�i�E C3E ٚEl�E�fE��E$�E�3EA�E��Ea�E� E~fE E&fE	C3E
�E�fE E��E�fE�3E33E6fE�fE��E+3E( E��E� E E� E� E� E �3E"\�E#�3E$� E&4�E'&fE(� E*  E*�3E,P E-� E/�E/��E1S3E2� E3�fE5H E6( E7vfE8��E:�E;i�E<�fE?��EB� EF EIfELI�EO&fERffEU��EXs3E[�E^њEb	�Ee@ Eh$�EkC3En� Eq��Et�fEw�fE{�E~�E���E�33E���E�H E���E�rfE�� E�vfE��E��3E�E�E���E��fE�  E�g3E�� E� E�e�E��3E�� E�X�E���E�  E�D E��fE���E�D�E���E�ŚE�"fE�x�E��fE��E�nfE��fE�	�E�G3E��fE��3>���>���?   >���?   ?   >���>���>���?   ?   >���>���>���>���?   >���?   ?��?��?L��?L��?fff?�  ?���?�33?���?�ff@ff@33@&ff@333@Fff@Y��@l��@�  @���@�ff@�  @�  @���@�33@�33@���@陚@�ffA   AffA��A��A33A!��A(  A0  A6ffA>ffAFffAL��AT��A\��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111414144441444441411414111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 ?fff?�33@   @`  @�  @�33@�  @���A  A  A,��AI��Ah  A�33A�33A�  A�  A�33A�  A���A���B��B
ffB  B��B"  B*��B2  B9��BA��BJffBRffBZ��BbffBjffBrffBz��B�ffB�33B�33B�  B�33B�ffB�ffB�33B���B�  B�  B�33B�  B���B���B���B���B�  B�  B�  BЙ�B�  B�ffB�ffB���B�ffB�  B�  B���B���B�ffB�ffC � C��C�3C��C� C
L�C��C� CffCL�CL�C� C��C�3C�3C��C � C"�3C$�3C&��C(��C*� C,� C.� C0� C2� C4� C6ffC8�3C:�3C<�3C>�3C@��CB� CDL�CF�3CH��CJffCL�3CN� CPL�CR� CT��CV��CX��CZffC\��C^��C`ffCbL�Cd��Cf��Ch��Cj33ClL�CnL�CpL�Cr33Ct��Cv��Cx33CzL�C|33C~��C�@ C�@ C�ffC�ffC�Y�C�ffC�ffC�ffC�ffC�@ C�&fC�&fC�33C�L�C�L�C�L�C�Y�C�@ C�&fC�33C�L�C�Y�C�L�C�&fC�33C�Y�C�@ C�&fC�L�C�@ C�&fC�L�C�L�C�L�C�ffC�Y�C�L�C�@ C�33C�ffC�L�C�L�C�33C�&fC�L�C�L�C�@ C�33C�33C�L�C�L�C�@ C�@ C�33C�Y�C�@ C�@ C�@ C�&fC�L�C�@ C�@ C�Y�C�L�C�@ C�L�C�@ C�&fC�@ C�33C�33C�@ C�Y�C�ffC�L�C�&fC�33C�L�C�Y�C�@ C�&fC�33C�L�C�Y�C�ffC�Y�C�33C�@ C�L�C�ffC�ffC�ffC�ffC�s3C�L�C�&fC�33C�33C�33C�33C�33C�@ C�L�C�Y�C�ffC�ffC�ffC�s3C�Y�C�ffC�Y�C�Y�C�L�C�L�C�L�C�L�C�Y�C�Y�C�ffC�ffC�ffC�33C��C�&fC�33C�&fC�33C�33D   D � D,�D�3D�fD	ffD&fD��D��D� DFfD  D� Dy�D",�D$��D'�fD*FfD,� D/� D2�D4�fD7FfD9�3D<` D>�3DAy�DD3DF��DIFfDK��DN�fDQ&fDS�fDV33DX� D[L�D]��D`�3Dc33De�3DhffDj��Dm��Dp�Dr�fDu,�Dw�fDz3D|  D~�3D���D�� D�fD�<�D�p D��fD���D�fD�C3D�y�D��3D��3D��D�I�D�y�D��fD���D��D�I�D�y�D�� D�� D��D�9�D�p D���D�ɚD���D�0 D�c3D�� D���D�� D�  D�L�D�|�D�� D�ٚD�fD�33D�\�D���D��fD�� D�fD�33D�\�D��3D��3D�ɚD�� D��D�<�D�c3D���D��3D�� D���D�)�D�L�D�i�DȐ Dɹ�D��D� D�33D�` Dσ3DЦfD��fD���D�fD�&fD�FfD�c3D؀ Dٜ�Dڹ�D��3D��3D�fD�9�D�Y�D�|�D�3D� D��fD��fD���D�  D�<�D�VfD�i�D쉚D��DD���D�� D���D�3D�,�D�L�D�p D�� D���D�ٚD��fD�	�D�,�D�VfD�y�E K3E �Et�EfE��E,�E�3EI�E��Ei�E� E�fE E.fE	K3E
�EfE E��E�fE�3E;3E>fE�fE��E33E0 E��E� E  E� E� E� E �3E"d�E#�3E$� E&<�E'.fE(� E* E*�3E,X E-� E/�E0�E1[3E2� E3�fE5P E60 E7~fE8��E:!�E;q�E<�fE?ɚEB� EF  EIfELQ�EO.fERnfEU��EX{3E[�E^ٚEb�EeH Eh,�EkK3En� Eq��Et�fEw�fE{$�E~�E���E�73E���E�L E���E�vfE�� E�zfE��E��3E�I�E���E��fE�$ E�k3E�� E�  E�i�E��3E�� E�\�E���E� E�H E��fE���E�H�E���E�ɚE�&fE�|�E��fE��E�rfE��fE��E�K3E��fE��3G�O�?L��G�O�?fffG�O�G�O�G�O�G�O�?fffG�O�G�O�G�O�G�O�G�O�?fffG�O�?fff?�  G�O�?���G�O�?�ff?�33?�  ?ٙ�?�33@ff@33@&ff@333@Fff@S33@fff@y��@�ff@�  @���@�ff@�  @�  @ə�@�33@�33@���@���A33A  AffA��A��A#33A)��A0  A8  A>ffAFffANffAT��A\��Ad��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111414144441444441411414111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 @ �@ %@ �@ {@ O@ "�@ (�@ /@ 6�@ =q@ FQ@ R�@ _�@ l�@ z3@ ��@ �0@ �(@ �~@ ��@ �|@ ��@ ��@ ��@�@@ @,`@9X@F�@V@c�@r@~�@��@�H@��@�F@�>@��@��@�4@��@1@*@!s@/�@=q@K�@X�@e�@s_@�@��@�@��@�R@Ĝ@Ӡ@�@�L@��@�@�@%�@2�@@,@O�@]�@i�@y�@�|@�u@�m@��@�k@�c@�[@�T@��@  @�@�@*S@7L@DD@SI@`�@m�@{�@��@�0@��@�~@�&@��@��@�y@� @�@o@g@,`@8�@I@V@bN@r@~K@��@��@��@��@�>@ψ@ލ@�4@�~@v@*@$.@0x@;d@I�@Wb@e	@r@�d@�@��@�M@�F@ƨ@Ӡ@�H@��@��@�@�@'�@5?@B�@N�@Z�@hs@v�@��@�u@�@�r@��@�@�[@�`@�@ �@J@�@*S@6�@B�@R�@_�@k�@{�@�7@��@��@��@��@��@��@�(@�q@	@	b@	[@	-@	:�@	G�@	T�@	bN@	qS@	~�@	��@	��@	��@	�F@	@	�7@	��@	�(@	��@
�@
{@
#�@
0x@
=q@
K�@
X�@
e	@
t@
�@
��@
�@
�@
�^@
ƨ@
�C@
��@
�@
��@
=@�@$�@3�@B8@P�@]�@i!@ww@��@��@�y@�!@��@�@׹@�T@�@�Q@�@�@(G@6�@D�@SI@a�@oF@|�@�D@��@��@��@��@�|@�#@��@�q@�@o@ �@.l@<@F�@SI@a�@o�@|�@�D@��@�A@��@��@�C@\�@��@�@;d@��@Ӡ@g@i�@��@�Q@I@�$@ލ@&;@m:@��@��@A�@�7@��@{@Z�@��@�@-�@t�@�@@K�@�@խ@O@`�@��@��@8�@�W@ƨ@�@SI@��@ލ@#�@g@�M@�H@$.@i!@��@�L@2�@t@�F@�~@;d@{�@��@ �@A�@�p@Ĝ@v@I@�D@�@ J@ M$@ �\@ �7@!b@!P�@!��@!�C@"@"S�@"�0@"׹@#�@#X@#��@#�t@$�@$[z@$�@$܀@%�@%\�@%�U@%�/@&�@&\)@&��@&�#@'�@'Yn@'��@'խ@({@(S�@(�@(��@)b@)O0@)��@)�@*J@*Ji@*�+@*��@+v@+FQ@+�@+�>@,j@,A�@,�@,�@,��@-7�@-uk@-��@-�@.,`@.i!@.��@.��@/g@/]�@/��@/�@06@0R�@0�\@0��@11@1C�@1��@1�w@1��@25@@2r�@2�f@2�(@3$�@3_�@3�U@3׹@4�@4Q=@4�\@4��@5J@5I�@5�@5�2@5�Q@6>�@6|�@6��@6��@78�@7v�@7��@7�e@81'@8m�@8��@8��@9%�@9bN@9�@:6@:��@;D�@;�@</�@<�#@=Ji@=��@>`�@>ψ@?o�@?܀@@�@@�@A��@A�,@B��@C6�@C�m@D=q@D�4@EFQ@E�@FN�@F�(@GQ=@G��@H��@H��@I�@JO@J�!@K*@K��@L:@Lȴ@MX�@M�R@NF�@Nխ@Og@O�q@P�p@Q��@S�@T�@U�J@W)�@XbN@Y�J@[)�@\Z@]��@_�@`qS@a�7@cJ@d`�@eӠ@g�@hX@i��@k$/@lg@m��@o�@p]�@q��@s*@toF@u��@w@xdZ@y��@{�@{Z@{�<@{�
@|�@|p�@|�@|��@}+�@}ff@}�k@}�q@~K@~�@~�^@
�@`B@��@�+@��@�3�@�L@�te@��T@���@�ލ@���@��@�C�G�O�@ G�O�@ �G�O�G�O�G�O�G�O�@ �G�O�G�O�G�O�G�O�G�O�@ �G�O�@ �@ jG�O�@ G�O�@ v@ %@ �@ 1@ 	�@ 
�@ J@ V@ �@ �@ @ *@ 6@ B@ O@ 
@  @ ""@ %�@ '�@ )�@ -@ /@ 1�@ 4�@ 6�@ 9X@ <@ >@ B8@ D�@ G�@ K@ M�@ Q=@ T�@ Wb@ Z�@ ^5G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�VA�VA�VA�XA�XA�XA�=qA�"�A��A�JA���AӍPA�^5A�Q�A�Q�A�Q�A�`BA�p�A�z�AӉ7Aӕ�Aӏ\A�t�A�jA�jA�hsA�K�A�&�A�bA�oA�(�A��#A��HA��TA��AϬA��yA��A�{A�=qA�/A��A��A���A�(�A��9A��;A���A���A�XA�bA��;A��-A�v�A� �A��A��A���A���A��#A�G�A�G�A���A�`BA�+A��RA�Q�A�ffA���A�ĜA�9XA�=qA���A��A�n�A�x�A��\A�&�A�1'A���A��A�(�A�hsA�%A�S�A��A���A�t�A��A���A�A�A��jA��hA�G�A�dZA�  A��TA���A��jA�(�A���A��/A�ffA�A���A�A�JA~I�A}�A|ZAyG�Av�yAu��AuG�At�!At(�Asx�Ar�yApv�AnI�Al��Ak�AiG�Af�!Ad�yAa�TA`  A]�#A]XA]VA\{AXjAT�DAS7LAPQ�AOoAM|�AL�jAK/AJ~�AH�/AHJAGx�AF��AE&�AC�mAB$�AA33A@M�A?A?�hA?&�A=�TA< �A;S�A:�A:=qA9ƨA8�HA8VA8(�A6��A6(�A5�TA5XA4�A3��A3��A3�A2�A2�HA2�9A2�A1�A0{A/�wA/oA.r�A.JA-G�A++A)�A(A�A'�^A&��A%K�A$��A$bNA#�;A#7LA!��A �!A��A�jA�#AdZA��Al�AVA��A?}A�yA�HA�wAK�A��A$�A~�A�-A�;A1A��A�hA
�jA
�\A	�wA	?}A��A�`A^5A%A�A��AJA/AQ�A�#A"�A ��A v�@�33@�r�@�Ĝ@�ƨ@���@�C�@�ȴ@�n�@��7@�7L@�o@�5?@��@��@�7L@��@���@��@柾@�^5@�@�j@�9@�r�@�1'@�|�@��y@��@��`@��@ߍP@އ+@ܛ�@�l�@�%@�=q@���@�V@�Ĝ@��@���@�b@��@��@�$�@���@��u@�-@��@��@�K�@�$�@�%@���@��@���@���@��@�9X@�I�@�l�@��R@�?}@��`@�l�@�E�@���@�Q�@��!@��-@��@�j@��@��P@�K�@�bN@�l�@�-@�-@�7L@�1'@�l�@�M�@�?}@�V@�z�@+@~ff@|�@z~�@xb@w
=@u��@u?}@s�F@r�@qx�@o+@l�@jM�@i%@g�w@d�@c��@a�@`A�@]p�@\I�@Z��@Y�^@V�y@UV@So@P�9@PA�@O�@Mp�@L1@J�!@H�9@Fff@C�m@B�@A��@AX@@�u@?�P@>@;�m@:n�@9�^@8�u@7�P@6$�@5@4z�@3S�@1��@1��@0�`@/+@.v�@-��@,��@+t�@*��@)��@)&�@(1'@'��@&��@%@$�j@#ƨ@"M�@ �u@ Q�@\)@�+@��@��@��@o@M�@x�@&�@�u@  @l�@��@@O�@j@�
@��@"�@-@hs@��@�;@�@�+@�@�j@z�@I�@33@
�\@	X@r�@b@�P@K�@�@��@�T@@��@/@Z@�@�@"�@^5@��@�?��w?�5??���?�7L?��?�z�?��?�n�?��?��?���?�I�?�=q?���?��?�?䛦?�J?�bN?�p�?�ƨ?��?�K�?�ff?ԛ�?���?�33?�J?щ7?�bN?�|�?��?�{?�V?�(�?�?��H?���?��?�r�?�r�?Ǯ?�ȴ?��?Õ�?°!?�Ĝ?��w?���?�{?�V?��D?�ƨ?�?��H?���?���?��#?���?�=q?�~�?�^5?�^5?���?��H?�?��?�(�?��?�O�?��?�v�?�;d?�|�?�bN?�bN?��?���?�Ĝ?�%?�%?�&�?�hs?�hs?���?���?�J?�J?�-?�M�?\?°!?���?�33?�33?�t�?�t�?öF?���?���?�9X?�Z?ě�A�VA�VA�XA�XA�VA�\)A�ZA�VA�XA�VA�Q�A�S�A�S�A�O�A�S�A�M�A�S�A�VA�VA�S�A�S�A�XA�VA�XA�XA�XA�VA�VA�VA�VA�VA�XA�ZA�XA�VA�XA�XA�XA�XA�VA�9XA�?}A�;dA��A�oA��A��A�"�A�$�A��A��#A���A�ȴAӺ^AӬAӗ�A�~�A�r�A�jA�bNG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 A�VA�VA�VA�XA�XA�XA�=qA�"�A��A�JA���AӍPA�^5A�Q�A�Q�A�Q�A�`BA�p�A�z�AӉ7Aӕ�Aӏ\A�t�A�jA�jA�hsA�K�A�&�A�bA�oA�(�A��#A��HA��TA��AϬA��yA��A�{A�=qA�/A��A��A���A�(�A��9A��;A���A���A�XA�bA��;A��-A�v�A� �A��A��A���A���A��#A�G�A�G�A���A�`BA�+A��RA�Q�A�ffA���A�ĜA�9XA�=qA���A��A�n�A�x�A��\A�&�A�1'A���A��A�(�A�hsA�%A�S�A��A���A�t�A��A���A�A�A��jA��hA�G�A�dZA�  A��TA���A��jA�(�A���A��/A�ffA�A���A�A�JA~I�A}�A|ZAyG�Av�yAu��AuG�At�!At(�Asx�Ar�yApv�AnI�Al��Ak�AiG�Af�!Ad�yAa�TA`  A]�#A]XA]VA\{AXjAT�DAS7LAPQ�AOoAM|�AL�jAK/AJ~�AH�/AHJAGx�AF��AE&�AC�mAB$�AA33A@M�A?A?�hA?&�A=�TA< �A;S�A:�A:=qA9ƨA8�HA8VA8(�A6��A6(�A5�TA5XA4�A3��A3��A3�A2�A2�HA2�9A2�A1�A0{A/�wA/oA.r�A.JA-G�A++A)�A(A�A'�^A&��A%K�A$��A$bNA#�;A#7LA!��A �!A��A�jA�#AdZA��Al�AVA��A?}A�yA�HA�wAK�A��A$�A~�A�-A�;A1A��A�hA
�jA
�\A	�wA	?}A��A�`A^5A%A�A��AJA/AQ�A�#A"�A ��A v�@�33@�r�@�Ĝ@�ƨ@���@�C�@�ȴ@�n�@��7@�7L@�o@�5?@��@��@�7L@��@���@��@柾@�^5@�@�j@�9@�r�@�1'@�|�@��y@��@��`@��@ߍP@އ+@ܛ�@�l�@�%@�=q@���@�V@�Ĝ@��@���@�b@��@��@�$�@���@��u@�-@��@��@�K�@�$�@�%@���@��@���@���@��@�9X@�I�@�l�@��R@�?}@��`@�l�@�E�@���@�Q�@��!@��-@��@�j@��@��P@�K�@�bN@�l�@�-@�-@�7L@�1'@�l�@�M�@�?}@�V@�z�@+@~ff@|�@z~�@xb@w
=@u��@u?}@s�F@r�@qx�@o+@l�@jM�@i%@g�w@d�@c��@a�@`A�@]p�@\I�@Z��@Y�^@V�y@UV@So@P�9@PA�@O�@Mp�@L1@J�!@H�9@Fff@C�m@B�@A��@AX@@�u@?�P@>@;�m@:n�@9�^@8�u@7�P@6$�@5@4z�@3S�@1��@1��@0�`@/+@.v�@-��@,��@+t�@*��@)��@)&�@(1'@'��@&��@%@$�j@#ƨ@"M�@ �u@ Q�@\)@�+@��@��@��@o@M�@x�@&�@�u@  @l�@��@@O�@j@�
@��@"�@-@hs@��@�;@�@�+@�@�j@z�@I�@33@
�\@	X@r�@b@�P@K�@�@��@�T@@��@/@Z@�@�@"�@^5@��@�?��w?�5??���?�7L?��?�z�?��?�n�?��?��?���?�I�?�=q?���?��?�?䛦?�J?�bN?�p�?�ƨ?��?�K�?�ff?ԛ�?���?�33?�J?щ7?�bN?�|�?��?�{?�V?�(�?�?��H?���?��?�r�?�r�?Ǯ?�ȴ?��?Õ�?°!?�Ĝ?��w?���?�{?�V?��D?�ƨ?�?��H?���?���?��#?���?�=q?�~�?�^5?�^5?���?��H?�?��?�(�?��?�O�?��?�v�?�;d?�|�?�bN?�bN?��?���?�Ĝ?�%?�%?�&�?�hs?�hs?���?���?�J?�J?�-?�M�?\?°!?���?�33?�33?�t�?�t�?öF?���?���?�9X?�Z?ě�A�VA�VA�XA�XA�VA�\)A�ZA�VA�XA�VA�Q�A�S�A�S�A�O�A�S�A�M�A�S�A�VA�VA�S�A�S�A�XA�VA�XA�XA�XA�VA�VA�VA�VA�VA�XA�ZA�XA�VA�XA�XA�XA�XA�VA�9XA�?}A�;dA��A�oA��A��A�"�A�$�A��A��#A���A�ȴAӺ^AӬAӗ�A�~�A�r�A�jA�bNG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B�\B�\B�\B�\B�\B�JB�hB�hB�uB�hB��B��B��B��B��B�B�3B�}B��B�fB��BBB
=BbB�B�B�BuB�B(�B-B33B7LB<jB>wBB�BH�BL�BL�Bq�Bq�B�7B�%B�7B��B��B�B�!B�B�B�B�B�B�B�?B�-B�B��B�hB�PB�Bx�BjBcTB\)BVBL�BH�BE�B<jB5?B+B$�B�BoBDBB��B�HB�}B�qB�B��B�DB}�BgmBR�BM�BH�BA�B;dB(�B�B
=B
�B
�mB
��B
ĜB
�wB
�B
��B
��B
��B
��B
�+B
}�B
u�B
p�B
e`B
R�B
E�B
>wB
9XB
33B
.B
)�B
"�B
{B
B	��B	�B	�NB	��B	ƨB	�XB	�3B	��B	��B	��B	��B	�1B	w�B	n�B	[#B	W
B	L�B	J�B	?}B	=qB	2-B	/B	.B	'�B	!�B	�B	uB	VB	
=B	1B	+B	B��B��B��B�B�B�B�yB�sB�fB�;B�BB�/B�B�B�B��B��B��B��B��B��BȴBȴBŢBB��B�wB�RB�9B�B��B��B��B��B��B��B�uB�hB�1B�B}�Bs�Br�Bo�Bl�BffBe`BbNBbNB`BBXB[#BYBXBR�BQ�BO�BK�BI�BH�BE�BF�BC�BA�BA�BA�B@�B?}B>wB>wB=qB;dB8RB:^B9XB<jB@�B>wB=qB9XB:^B@�B@�B@�B?}B?}B>wB>wB<jB=qB;dB9XB7LB33B33B1'B2-B49B5?B5?B:^B;dB<jB:^B:^B;dB;dB?}B?}B>wB@�B?}B=qBO�B\)B]/BffBt�B|�B�JB��B��B��B��BɺB��B	DB	�B	{B	)�B	5?B	8RB	K�B	YB	bNB	m�B	{�B	~�B	�DB	�VB	��B	��B	��B	�B	�3B	�RB	B	ǮB	��B	��B	��B	�B	�/B	�TB	�TB	�ZB	�B	�B	�B	�B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
+B
DB
JB
VB
hB
uB
�B
�B
�B
�B
�B
�B
�B
"�B
$�B
&�B
&�B
)�B
,B
/B
0!B
0!B
1'B
2-B
5?B
6FB
7LB
:^B
<jB
=qB
>wB
>wB
>wB
@�B
A�B
D�B
D�B
D�B
E�B
F�B
H�B
H�B
J�B
K�B
L�B
M�B
M�B
P�B
O�B
P�B
R�B
R�B
S�B
T�B
VB
W
B
W
B
XB
YB
[#B
[#B
\)B
^5B
^5B
_;B
`BB
`BB
bNB
cTB
dZB
e`B
e`B
e`B
gmB
gmB
gmB
hsB
iyB
iyB
k�B
l�B
k�B
l�B
m�B
n�B
o�B
p�B
p�B
q�B
r�B
s�B
r�B
s�B
t�B
t�B
v�B
x�B
w�B
x�B
y�B
x�B
y�B
y�B
z�B
y�B
z�B
z�B
{�B
|�B
|�B
}�B
~�B
~�B
�B
�B
�B
�B
�%B
�1B
�1B
�1B
�DB
�DB
�PB
�PB
�VB
�VB
�\B
�hB
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
�B
�B
�B
�B
�'B
�'B
�-B
�3B
�9B
�?B
�9B
�?B
�LB
�LB
�FB
�LB
�RB
�RB
�XB
�RB
�XB
�^B
�^B
�XB
�^B
�^B
�^B
�^B
�XB
�dB
�^B
�dB
�^B
�^B
�^B
�^B
�^B
�^B
�^B
�^B
�^B
�^B
�^B
�dB
�^B
�^B
�^B
�dB
�^B
�XB
�dB
�XB
�^B
�^B
�XB
�^B
�^B
�XB
�^B�\B�bB�\B�VB�bB�VB�VB�\B�PB�VB�\B�bB�\B�bB�\B�bB�\B�VB�bB�bB�bB�VB�bB�\B�\B�VB�\B�\B�VB�\B�\B�\B�\B�\B�\B�VB�\B�\B�VB�+B�bB�oB�\B�oB�uB�uB�uB�oB�VB�{B��B��B��B��B��B��B��B��B��B��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 B�5B�5B�5B�5B�6B�$B�BB�CB�PB�CB�vB��B��B��B��B��B�B�]B˧B�GB��B�B�B
 BFBwBlBgB[B�B(�B,�B3B75B<TB>aBBzBH�BL�BL�Bq�Bq�B�#B�B�$B�uB��B��B�B��B��B��B��B�B�B�2B� B��B��B�\B�DB��Bx�BjuBcJB\ BU�BL�BH�BE�B<cB59B*�B$�B�BkB@BB��B�FB�{B�pB�B��B�DB}�BgnBR�BM�BH�BA�B;gB(�B�B
AB
�B
�rB
��B
ĢB
�~B
�"B
��B
��B
��B
��B
�5B
}�B
u�B
p�B
ekB
R�B
E�B
>�B
9eB
3AB
."B
*B
"�B
�B
/B	��B	�B	�_B	�B	ƺB	�kB	�FB	��B	��B	��B	��B	�FB	w�B	n�B	[9B	W!B	L�B	J�B	?�B	=�B	2FB	/5B	..B	(B	!�B	�B	�B	sB	
ZB	OB	IB	8B�B��B��B��B�B��B�B�B�B�_B�fB�TB�CB�*B�+B�B�B�B�B�B�B��B��B��B»B��B��B��B�gB�IB�+B�%B��B��B��B��B��B��B�dB�EB~(Bs�Br�Bo�Bl�Bf�Be�Bb�Bb�B`zBXIB[\BYQBXJBS-BR'BPBLBI�BH�BE�BF�BC�BA�BA�BA�B@�B?�B>�B>�B=�B;�B8�B:�B9�B<�B@�B>�B=�B9�B:�B@�B@�B@�B?�B?�B>�B>�B<�B=�B;�B9�B7�B3�B3�B1vB2}B4�B5�B5�B:�B;�B<�B:�B:�B;�B;�B?�B?�B>�B@�B?�B=�BP=B\�B]�Bf�Bu&B}[B��B�?B�)B�QB�yB�:B�YB	�B	B	B	*�B	5�B	8�B	L`B	Y�B	b�B	n3B	|�B	�B	��B	�B	��B	��B	��B	��B	��B	�B	�SB	�uB	ΝB	ЬB	ҼB	��B	�B	�-B	�0B	�9B	�gB	��B	�B	�B	��B	��B	��B	��B	��B
 �B
B
B
B
B
)B
8B
TB
]B
lB
�B
�B
�B
�B
�B
�B
�B
 �B
 �B
$B
&B
(!B
($B
+:B
-HB
0^B
1gB
1jB
2sB
3{B
6�B
7�B
8�B
;�B
=�B
>�B
?�B
?�B
?�B
A�B
B�B
FB
FB
FB
GB
H#B
J2B
J4B
LDB
MMB
NVB
O^B
OaB
RvB
QsB
R{B
T�B
T�B
U�B
V�B
W�B
X�B
X�B
Y�B
Z�B
\�B
\�B
]�B
_�B
_�B
`�B
bB
bB
dB
eB
f'B
g0B
g2B
g5B
iEB
iGB
iJB
jSB
k[B
k^B
mmB
nuB
mrB
n{B
o�B
p�B
q�B
r�B
r�B
s�B
t�B
u�B
t�B
u�B
v�B
v�B
x�B
z�B
y�B
z�B
{�B
z�B
{�B
|B
}	B
|B
}B
}B
~B
$B
'B
�0B
�8B
�;B
�RB
�XB
�sB
�xB
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
�B
�#B
�.B
�;B
�EB
�RB
�fB
�kB
�xB
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
�B
�B
�"B
�/B
�<B
�QB
�kB
��B
��B
��B
��B
��B
��B
� B
�B
�+B
�5B
�KB
�fB
�uB
��B
��B
��B
��B
��B
��B
��B
�B
�B
�B
�0B
�@B
�NB
�]B
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
��B�5B�;B�5B�/B�;B�/B�/B�5B�)B�/B�5B�;B�5B�;B�5B�;B�5B�/B�;B�;B�;B�/B�;B�5B�5B�/B�5B�5B�/B�5B�5B�5B�5B�6B�6B�0B�6B�6B�0B�B�<B�IB�7B�JB�PB�PB�PB�JB�1B�VB�hB�oB�vB�|B�|B��B��B��B��B��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�201811120101042021061413555120210614135551202106171313272021061713132720210617131327201811120101042021061413555120210614135551202106171313272021061713132720210617131327PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            new conductivity = original conductivity * (1 + delta*TEMP + CPcor_SBE*PRES) / (1 + delta*TEMP_ADJUSTED + CPcor_new*PRES_ADJUSTED)                                                                                                                              none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            new conductivity = original conductivity * (1 + delta*TEMP + CPcor_SBE*PRES) / (1 + delta*TEMP_ADJUSTED + CPcor_new*PRES_ADJUSTED)                                                                                                                              none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.50 dbar                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            CPcor_new = -1.25e-07; CPcor_SBE = -9.57e-08; delta = 3.25e-06                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.50 dbar                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            CPcor_new = -1.25e-07; CPcor_SBE = -9.57e-08; delta = 3.25e-06                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                      none                                                                                                                                                                                                                                                            New conductivity computed by using a different CPcor value from that provided by Sea-Bird.                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            No significant salinity drift detected.                                                                                                                                                                                                                         Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                      none                                                                                                                                                                                                                                                            New conductivity computed by using a different CPcor value from that provided by Sea-Bird.                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            No significant salinity drift detected.                                                                                                                                                                                                                         PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            AO  AO  ARCAARCAADJPADJP                                                                                                                                        2018111201010420181112010104  IP  IP                                G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2018111201010420181112010104QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�5F03E           703E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2018111201010420181112010104QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�0               0               PM  PM  ARSQARSQCOWGCOWGV1.1V1.1                                                                                                                                2021061713150820210617131508IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                