CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  &   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2019-03-31T17:00:37Z creation      
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
resolution        =���   axis      Z        0  ;�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 L  M   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     0  QP   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 L  b�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     0  f�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     0  w�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 L  �,   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     0  �x   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 L  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     0  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     0  �$   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 L  �T   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     0  ɠ   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 L  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     0  �   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                   �   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                   �   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                   �   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                   �   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  � �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                   T   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                   p   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    x   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar        �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar        �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�       �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  �L   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                   �   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  � �Argo profile    3.1 1.2 19500101000000  20190331170037  20210617131516  5905739 5905739 US ARGO PROJECT                                                 US ARGO PROJECT                                                 GREGORY C. JOHNSON                                              GREGORY C. JOHNSON                                              PRES            TEMP            PSAL            PRES            TEMP            PSAL               @   @DD  AOAO7219                            7219                            2C  2C  DD  SOLO_D_MRV                      SOLO_D_MRV                      12002                           12002                           SBE603 01Dec16                  SBE603 01Dec16                  874 874 @خ�S�@خ�S�11  @خ �O�`@خ �O�`@6.g���@6.g����c�l�!-�c�l�!-11  GPS     GPS     Primary sampling: averaged [1Hz sampling by SBE-41CP averaged in 2-dbar bins]                                                                                                                                                                                   Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 AC  AA  AA  >���?fff@   @@  @�  @�  @�  @�33A��A��A(  AA��Aa��A���A���A���A���A�  A���A���A�  B   B  B  BffB   B(��B0��B8��B@  BHffBPffBX  B`ffBhffBp  BxffB�ffB�33B���B���B�  B�  B�  B�  B�33B�ffB�ffB�  B�  B�33B�ffB�  B�33B�ffB�  B�33B�ffBԙ�B�33Bۙ�B���B�  B�ffB�  BB�  B�ffB�33C   C  C�fC�fC��C
33C33C33C33C�C33C�fC  C  C  C�fC 33C"L�C$33C&33C(�C*�C,  C-�fC/��C2�C4  C5��C8�C:33C<�C=�fC@  CB33CDL�CF�CH  CJ�CLL�CN  CO��CR  CT�CVL�CX33CZ  C\L�C^�C_�fCb33Cd�Cf�Ch  Ci�fClL�Cn33Cp33Cr�Ct�Cv�Cx  Cz�C|  C~  C�fC��C��C��C�  C��3C��C��C��C��C��3C��C��C��3C��C�  C��3C��C��C��3C��C��C��3C��C��C�  C��fC��3C�  C��C��C��C��3C�  C��C�&fC��C��3C��3C�  C��C�&fC��C�  C��C�  C��fC��fC��fC��3C�  C��C��C�  C��3C��C��C��3C��C��C�  C��3C��3C��C��C��C�  C�  C��C��C�  C�&fC��C��C�&fC��C�  C�&fC��C��C�  C��3C��C�  C�ٚC��3C��C��C��C�  C��C�&fC��C�  C��C�  C��3C��C�  C��fC��3C��C��C��3C�  C��C��3C��fC��3C��C��C�&fC��C��3C��C��C��C��fC��3C��C�&fC�  C��fC�  D�fD  D��D
� DS3D  D�fDL�D��D��D�D� D"  D$��D'3D)s3D+��D.y�D0�3D3l�D5�3D8��D;  D=�fD@33DB� DEY�DG��DJ��DM33DO� DRS3DTٚDWs3DZ  D\�3D_  Da��DdFfDf� Dis3Dl3Dn�3DqL�Ds�3Dv�3Dy,�D{Y�D}�fD�@ D�� D��fD�6fD�|�D��3D�3D�C3D��3D���D�� D�,�D�\�D��3D��fD��3D�� D���D�  D�<�D�P D�p D���D�� D�� D��3D��fD���D�� D���D��D�&fD�@ D�c3D�|�D��3D�ɚD��3D�  D�I�D�l�D��fD��fD���D�,�D�\�D��3D�� D�3D�VfD��3D�ɚD�fD�C3D��fD���D� D�` Dĩ�D���D�,�D�` Dɓ3D��3D���D�)�D�\�Dϐ D�� D�� D���D�fD�33D�I�D�Y�D�ffD�ffD�p D�l�D�ffD�p D�l�D�ffD�` D�S3D�I�D�33D�#3D��D��fD��3D�� D��D驚D�fD뉚D�vfD�i�D�` D�VfD�FfD�<�D�9�D�<�D�<�D�<�D�@ D�9�D�9�D�I�D�` D�c3D�y�D��3D���D���E x E3E�3E)�E��ED�E�3E^fE��E� EnfE	�fE
� E��EnfE��E� E3E�E� E��E�3E�fE,�E��E��E>fE#3Et�E� E!0 E"!�E#� E%3E&fE'$�E(� E)�fE+T�E,^fE-\�E.� E0NfE1P E2ɚE3� E56fE6+3E7��E9fE9�fE;c3E<� E?��EB��EE� EI&fELH EO( ERl�EU� EX� E[�fE^ٚEa��Ee;3Eh�Ek` Enl�Eq��Et��Ew� E{3E~;3E�� E�$ E��3E�RfE���E�-�E�{3E�ŚE��E�W3E���E�3E�L E�� E���E�8 E���E���E�A�E��3E��3E�"fE�3E���E� E�T E���E��E�D E��fE��3E�L�E�� E���E�, E�3E��3E��E�jfE���E� E�c3E��3E���E�JfE��fE���E�BfE���E�՚E�8 E���E�ɚE��>���>���>L��>���>L��>���>���>L��>���>���=���>���>L��>L��>���>���>���>���>���>���>L��>���>���>L��>L��>���>���?   ?   ?   ?��?333?333?fff?���?�ff?���?�ff@ff@��@,��@@  @S33@fff@�  @���@�33@�33@���@�ff@�  @���@�ff@�33@�  A   A33A33A��A  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111441414411414411444441444111441141111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          ?L��?�33@   @`  @�  @�  @�  @�33A	��A��A0  AI��Ai��A���A���A���A���A�  A���A���A�  B  B
  B  BffB"  B*��B2��B:��BB  BJffBRffBZ  BbffBjffBr  BzffB�ffB�33B���B���B�  B�  B�  B�  B�33B�ffB�ffB�  B�  B�33B�ffB�  B�33B�ffB�  B�33B�ffBՙ�B�33Bܙ�B���B�  B�ffB�  B�B�  B�ffB�33C � C� CffCffCL�C
�3C�3C�3C�3C��C�3CffC� C� C� CffC �3C"��C$�3C&�3C(��C*��C,� C.ffC0L�C2��C4� C6L�C8��C:�3C<��C>ffC@� CB�3CD��CF��CH� CJ��CL��CN� CPL�CR� CT��CV��CX�3CZ� C\��C^��C`ffCb�3Cd��Cf��Ch� CjffCl��Cn�3Cp�3Cr��Ct��Cv��Cx� Cz��C|� C~� C�33C�Y�C�Y�C�L�C�@ C�33C�Y�C�Y�C�Y�C�L�C�33C�Y�C�L�C�33C�L�C�@ C�33C�Y�C�L�C�33C�L�C�L�C�33C�L�C�Y�C�@ C�&fC�33C�@ C�L�C�Y�C�L�C�33C�@ C�Y�C�ffC�L�C�33C�33C�@ C�Y�C�ffC�Y�C�@ C�L�C�@ C�&fC�&fC�&fC�33C�@ C�L�C�Y�C�@ C�33C�L�C�L�C�33C�Y�C�L�C�@ C�33C�33C�Y�C�Y�C�L�C�@ C�@ C�Y�C�Y�C�@ C�ffC�L�C�L�C�ffC�L�C�@ C�ffC�Y�C�L�C�@ C�33C�L�C�@ C��C�33C�L�C�Y�C�L�C�@ C�L�C�ffC�Y�C�@ C�L�C�@ C�33C�L�C�@ C�&fC�33C�Y�C�L�C�33C�@ C�L�C�33C�&fC�33C�L�C�Y�C�ffC�L�C�33C�L�C�Y�C�L�C�&fC�33C�L�C�ffC�@ C�&fC�@ D�fD@ D�D
� Ds3D  D�fDl�D�D��D9�D� D"@ D$��D'33D)�3D,�D.��D13D3��D63D8��D;@ D=�fD@S3DB� DEy�DH�DJ��DMS3DO� DRs3DT��DW�3DZ  D\�3D_@ Da��DdffDg  Di�3Dl33Dn�3Dql�Dt3Dv�3DyL�D{y�D~fD�P D�� D��fD�FfD���D��3D�3D�S3D��3D�ɚD�  D�<�D�l�D��3D��fD��3D�� D�	�D�0 D�L�D�` D�� D���D�� D�� D��3D��fD���D�  D��D��D�6fD�P D�s3D���D��3D�ٚD�3D�0 D�Y�D�|�D��fD��fD�	�D�<�D�l�D��3D�� D�#3D�ffD��3D�ٚD�fD�S3D��fD���D�  D�p DĹ�D���D�<�D�p Dɣ3D��3D��D�9�D�l�DϠ D�� D�� D�	�D�&fD�C3D�Y�D�i�D�vfD�vfDڀ D�|�D�vfD݀ D�|�D�vfD�p D�c3D�Y�D�C3D�33D��D�fD��3D�� D���D鹚D�fD뙚D�fD�y�D�p D�ffD�VfD�L�D�I�D�L�D�L�D�L�D�P D�I�D�I�D�Y�D�p D�s3D���D��3D���D���E � E3E�3E1�E��EL�E�3EffE��E� EvfE	�fE
� E��EvfE��E� E3E�E� E��E3E�fE4�E��E��EFfE+3E|�E� E!8 E")�E#� E%3E&fE',�E(� E)�fE+\�E,ffE-d�E.� E0VfE1X E2њE3� E5>fE633E7��E9fE9�fE;k3E<� E?��EB��EE� EI.fELP EO0 ERt�EU� EX� E[�fE^�Ea��EeC3Eh!�Ekh Ent�Eq��Et��Ew� E{3E~C3E�� E�( E��3E�VfE���E�1�E�3E�ɚE��E�[3E���E�3E�P E�� E���E�< E���E���E�E�E��3E��3E�&fE��3E���E� E�X E���E��E�H E��fE��3E�P�E�� E���E�0 E��3E��3E�!�E�nfE���E� E�g3E��3E��E�NfE��fE���E�FfE���E�ٚE�< E���E�͚E��G�O�G�O�?333G�O�?333G�O�G�O�?333?L��G�O�?��G�O�G�O�?333?L��G�O�G�O�G�O�G�O�G�O�?333G�O�G�O�G�O�?333?L��?fffG�O�G�O�?�  ?���G�O�?���?�33?���?�ff@ff@33@&ff@9��@L��@`  @s33@�33@�  @���@�33@�33@���@�ff@�  @���@�ff@�33A   A  A33A33A��A   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111441414411414411444441444111441141111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          @ @ %@ �@ {@ O@ ""@ (�@ 0x@ 7L@ >@ G�@ R�@ `B@ m�@ |?@ �7@ ��@ ��@ �-@ ��@ ��@ �t@ �@ ��@@@ @-�@;d@G�@V@c�@p�@~�@��@��@��@�F@�>@ψ@�/@�@�,@�@{@"�@1'@>�@K@X�@g@uk@��@�@�a@��@�@�W@խ@��@�@��@
=@B@%�@1�@@�@O�@\�@i�@ww@�p@�@�@�r@�@��@�h@�`@�@�Q@�@O@(�@5�@E�@S�@`�@n�@{�@�7@�0@�(@�!@��@��@�@��@� @@b@�@-�@<@H]@UU@c�@r�@~K@��@��@��@��@��@�7@��@�4@�~@1@*@"�@/�@<�@M$@Z@g�@t�@�d@�@�@�Y@�R@��@��@�@�L@�E@
=@6@&�@4�@B8@O0@[z@k.@x&@�p@�u@�m@�f@�@�@�[@�`@�@�Q@V@�@(�@5?@C�@Q�@`B@n�@{�@��@�0@�5@��@��@�@��@�@� @	v@	o@	�@	-@	:@	FQ@	S�@	a�@	o�@	~K@	��@	��@	�A@	�9@	�>@	��@	�/@	��@	��@
�@
�@
!s@
1'@
>�@
K�@
X�@
ff@
uk@
�@
�\@
�@
�Y@
�@
�@
�O@
�H@
��@
��@
�@�@$�@3�@@�@Lu@[z@j@x�@��@��@�@�!@�@�c@׹@�@�@ �@�@�@(G@7�@D�@Q=@_�@m�@z3@�+@��@��@��@�2@�|@��@��@� @@�@
@-@<@G�@S�@b�@�4@33@�@�c@@\)@��@��@4�@|?@��@�@K@�P@�C@@X@�U@��@#�@hs@�r@��@:�@�W@��@�@SI@��@�T@(�@oF@�9@�9@@�@�+@��@o@Yn@�m@�@.l@v@�@v@M$@�$@ψ@*@\)@��@��@4�@z3@��@@H]@��@��@@T�@��@�O@o@O0@��@�@ �@ C�@ ~K@ ��@ �~@!33@!m:@!��@!�@"�@"UU@"��@"ȴ@#�@#@�@#~�@#�@#��@$8�@$x&@$�R@$��@%5�@%uk@%�F@%��@&9X@&z3@&�j@'  @'D�@'��@'�|@(�@(SI@(��@(��@)!s@)ff@)�@)�e@*9X@*}�@*�&@+ �@+A�@+�p@+Ĝ@,%@,G�@,��@,��@-@->�@-{�@-��@-��@.*S@.`�@.��@.ψ@/�@/=q@/s_@/��@/��@0�@0FQ@0x&@0�Y@0�/@1@1A�@1t@1��@1�@2�@2?}@2r@2��@2�t@3@3B8@3v�@3��@3�@4�@4Q=@4��@4��@4�e@5.l@5i�@5�@5܀@6�@6T�@6�@6�7@7@7Lu@7�7@7Ĝ@8@8>�@8z2@8�E@9z�@:5�@:�f@;(�@;�z@<X@<�|@=@�@=�@>S�@>�,@?e	@?��@@^�@@�@A�W@B@B��@C�@C��@D/�@D��@E-@E�c@Fk.@F�/@GP�@G��@Hs_@I�@I�C@I��@J��@K9X@K�A@LH]@L��@MQ=@M��@NS�@N�@OX@O�@P�\@Q�o@S�@Tp�@U��@W(�@Xb�@Y�@[/�@\b�@]�9@_*@`hs@a�*@c�@dm:@e�^@g�@hYn@i�c@k @lx�@m��@o�@p^�@qě@s�@sZ@s�T@s��@tC@tX@t�-@t�@@u(�@u��@u��@u�Y@vH]@v~�@v��@w�@wC�@w��@w�@x�@xff@x��@x�`@y3�@yff@y��@y�Q@zH]@zww@z��@{�@{M�@{�@{��@|�@|V@|��@|��@}+�@}n�@}� @}��@~0x@~��@~�>@^@UU@�$@є@��G�O�G�O�@ ^G�O�@ ^G�O�G�O�@ ^@ G�O�@  �G�O�G�O�@ ^@ G�O�G�O�G�O�G�O�G�O�@ ^G�O�G�O�G�O�@ ^@ @ �G�O�G�O�@ j@ G�O�@ �@ %@ �@ �@ 
�@ J@ V@ b@ o@ {@ �@ �@ O@ [@ g@ "�@ $.@ &�@ (�@ +�@ -�@ 0x@ 33@ 6�@ 7�@ ;d@ >@ @�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A��A��A�"�A� �A��A��A��A�
=A�A�1A�JA���A�A��A���A���A�ȴA��FA���A���A���A���A���A���A���A���A���A���A��hA��hA��7A��A�l�A�bNA�^5A�^5A�`BA�bNA�\)A�A�A�33A�&�A��A�JA��wA�G�A���A�Q�A��mA��^A�bNA�I�A��
A��DA���A��/A�bNA�$�A��A��yA�\)A�&�A��A�"�A�M�A���A��jA�l�A�M�A��A��A��;A���A���A�VA�bA�z�A�;dA�|�A�  A�x�A�oA��RA�dZA���A���A��wA�`BA��A�;dA��!A�XA��A��-A�;dA���A�I�A���A�{A�ȴA���A��A��A�S�A��A��-A��HA�v�A��A~Q�A|�9AzĜAy�AyVAvQ�Ap��Al(�Aj�HAj-Ai��Agt�Ae�Ab�A`�A`9XA^��A\^5A[+AY�AX �AW�hAV��AUO�AShsAR�RARbAP�/AO+AL�AJ�HAJ(�AI��AH1'AF�AC�#AA�-A>ĜA>Q�A>�A=��A=��A=;dA<��A;��A:�A9��A4�yA3XA1��A0�A-��A,�A,�yA,r�A*�A(�9A&��A%��A$�/A$n�A$VA$�A#�;A#�PA"�`A"�+A"9XA!C�AhsA-A`BAȴAƨA$�A�yA�A�\A�A�-A�AhsA��AbNA�A`BA1A��AhsA�mA�A�9A1'A��A
�`A
-A	�A	t�A	
=A�DAr�AJAAbAƨAG�AbAA��A/A�AVAE�A33@��@�dZ@���@���@�x�@� �@��@�t�@��@�|�@�C�@�ȴ@�M�@�V@�t�@�$�@�9X@�V@�33@땁@�M�@�1@�ȴ@�$�@��T@���@�M�@�j@�@�E�@ݺ^@ܴ9@�Q�@��@Л�@͡�@˶F@˕�@��@�1@���@�J@��@��@�  @��H@���@�Ĝ@�K�@�Ĝ@�1@��R@��9@�Q�@�dZ@�=q@�/@��j@���@�S�@��@�S�@��!@�v�@�V@��P@��+@���@���@�-@��`@��F@�V@��m@��\@���@�?}@��`@�bN@�33@�@��H@�-@�G�@�&�@|1@{@x �@vv�@u`B@t�j@tz�@s"�@q��@pr�@n�@n5?@l��@k"�@j=q@d��@d�@cdZ@ct�@aG�@`1'@]`B@[��@Y�@U��@Sƨ@Q&�@P�`@PbN@O|�@N5?@M�@K�F@JM�@HA�@G|�@F@D�j@B�!@A��@A�@@A�@?�P@?+@=�-@;@9�^@9X@8��@81'@3�F@2��@1�@1�@/�@/
=@.��@.�+@.E�@-��@-�h@-p�@-?}@-?}@-O�@,�j@+�F@+@*��@)��@(��@(Ĝ@(�`@(r�@&��@&{@%�@$�@"n�@�P@O�@��@ƨ@S�@~�@�@|�@�@p�@�/@�j@j@M�@��@�@�9@�u@A�@1'@  @K�@�R@��@E�@�@j@��@	7L@�`@�9@r�@�@��@�y@{@`B@V@�@�/@��@9X@�
@C�@��@ A�?��h?��-?��?���?�n�?��?�|�?�5??ꟾ?���?��?�9X?�J?� �?��?��m?�ƨ?ٙ�?�ȴ?��?�Z?�9X?��
?�S�?���?�&�?Ѓ?�bN?�  ?��;?��?�v�?��?�(�?�1?�ƨ?�"�?�?�=q?��#?�Q�?�1'?�?���?�A�?���?��w?��w?��?��R?�/?��D?�I�?��m?�ƨ?���?���?���?���?���?��H?�?�C�?���?��m?��D?���?�/?�O�?�O�?�p�?�p�?��h?��h?��-?���?��?�5??�5??�5??�5??�v�?���?���?��R?���?��?�;d?�\)?�|�?���?��w?��;?�  ?�A�?� �?�bN?��?���?�Ĝ?�Ĝ?�%?�%?�G�?�G�?�G�?�hs?��7?���?���?�J?�-?�M�?\?\?°!?���A��A��A�{A�{A��A��A��A��A��A��A��A��A� �A��A��A��A��A��A��A��A��A��A��A��A��A��A� �A��A� �A��A��A��A��A��A��A� �A�$�A� �A� �A� �A� �A� �A�"�A��A��A��A��A��A��A��A��A�{A�A�JA�bA�%A���A�A�%A�VG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          A��A��A�"�A� �A��A��A��A�
=A�A�1A�JA���A�A��A���A���A�ȴA��FA���A���A���A���A���A���A���A���A���A���A��hA��hA��7A��A�l�A�bNA�^5A�^5A�`BA�bNA�\)A�A�A�33A�&�A��A�JA��wA�G�A���A�Q�A��mA��^A�bNA�I�A��
A��DA���A��/A�bNA�$�A��A��yA�\)A�&�A��A�"�A�M�A���A��jA�l�A�M�A��A��A��;A���A���A�VA�bA�z�A�;dA�|�A�  A�x�A�oA��RA�dZA���A���A��wA�`BA��A�;dA��!A�XA��A��-A�;dA���A�I�A���A�{A�ȴA���A��A��A�S�A��A��-A��HA�v�A��A~Q�A|�9AzĜAy�AyVAvQ�Ap��Al(�Aj�HAj-Ai��Agt�Ae�Ab�A`�A`9XA^��A\^5A[+AY�AX �AW�hAV��AUO�AShsAR�RARbAP�/AO+AL�AJ�HAJ(�AI��AH1'AF�AC�#AA�-A>ĜA>Q�A>�A=��A=��A=;dA<��A;��A:�A9��A4�yA3XA1��A0�A-��A,�A,�yA,r�A*�A(�9A&��A%��A$�/A$n�A$VA$�A#�;A#�PA"�`A"�+A"9XA!C�AhsA-A`BAȴAƨA$�A�yA�A�\A�A�-A�AhsA��AbNA�A`BA1A��AhsA�mA�A�9A1'A��A
�`A
-A	�A	t�A	
=A�DAr�AJAAbAƨAG�AbAA��A/A�AVAE�A33@��@�dZ@���@���@�x�@� �@��@�t�@��@�|�@�C�@�ȴ@�M�@�V@�t�@�$�@�9X@�V@�33@땁@�M�@�1@�ȴ@�$�@��T@���@�M�@�j@�@�E�@ݺ^@ܴ9@�Q�@��@Л�@͡�@˶F@˕�@��@�1@���@�J@��@��@�  @��H@���@�Ĝ@�K�@�Ĝ@�1@��R@��9@�Q�@�dZ@�=q@�/@��j@���@�S�@��@�S�@��!@�v�@�V@��P@��+@���@���@�-@��`@��F@�V@��m@��\@���@�?}@��`@�bN@�33@�@��H@�-@�G�@�&�@|1@{@x �@vv�@u`B@t�j@tz�@s"�@q��@pr�@n�@n5?@l��@k"�@j=q@d��@d�@cdZ@ct�@aG�@`1'@]`B@[��@Y�@U��@Sƨ@Q&�@P�`@PbN@O|�@N5?@M�@K�F@JM�@HA�@G|�@F@D�j@B�!@A��@A�@@A�@?�P@?+@=�-@;@9�^@9X@8��@81'@3�F@2��@1�@1�@/�@/
=@.��@.�+@.E�@-��@-�h@-p�@-?}@-?}@-O�@,�j@+�F@+@*��@)��@(��@(Ĝ@(�`@(r�@&��@&{@%�@$�@"n�@�P@O�@��@ƨ@S�@~�@�@|�@�@p�@�/@�j@j@M�@��@�@�9@�u@A�@1'@  @K�@�R@��@E�@�@j@��@	7L@�`@�9@r�@�@��@�y@{@`B@V@�@�/@��@9X@�
@C�@��@ A�?��h?��-?��?���?�n�?��?�|�?�5??ꟾ?���?��?�9X?�J?� �?��?��m?�ƨ?ٙ�?�ȴ?��?�Z?�9X?��
?�S�?���?�&�?Ѓ?�bN?�  ?��;?��?�v�?��?�(�?�1?�ƨ?�"�?�?�=q?��#?�Q�?�1'?�?���?�A�?���?��w?��w?��?��R?�/?��D?�I�?��m?�ƨ?���?���?���?���?���?��H?�?�C�?���?��m?��D?���?�/?�O�?�O�?�p�?�p�?��h?��h?��-?���?��?�5??�5??�5??�5??�v�?���?���?��R?���?��?�;d?�\)?�|�?���?��w?��;?�  ?�A�?� �?�bN?��?���?�Ĝ?�Ĝ?�%?�%?�G�?�G�?�G�?�hs?��7?���?���?�J?�-?�M�?\?\?°!?���A��A��A�{A�{A��A��A��A��A��A��A��A��A� �A��A��A��A��A��A��A��A��A��A��A��A��A��A� �A��A� �A��A��A��A��A��A��A� �A�$�A� �A� �A� �A� �A� �A�"�A��A��A��A��A��A��A��A��A�{A�A�JA�bA�%A���A�A�%A�VG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B��B��B��B��B��B��B��B��B�}B�}B�}B�}B��B�wB�}B�wB�wB�wB�wB�wB�}B�wB�wB�wB��B��B��B��B��BBBĜBȴBɺB��B��B��B��B��B�#B�)B�5B�HB�B��B%BVBoB�B'�BQ�BZBZBXBZBZBYBL�BA�B6FB,B�B�BVBB�B�fB�NB�BB�)B�B�B�
B��B��B��B�jB��B�=B�Bz�Bv�Bp�BiyBYB<jB1'B+B�B�BoBPB	7B  B
�B
�NB
�B
��B
��B
�3B
�B
�B
��B
�uB
�B
r�B
W
B
C�B
8RB
-B
�B
\B
B	��B	�NB	�B	��B	�DB	�B	}�B	l�B	XB	K�B	?}B	:^B	+B	!�B	�B	JB��B��B�B�yB��B��B��B�B�`B��BǮB��B��B��B�?B��B��B�VB�DB�7B�7B�B�B~�Bv�Bs�Bk�Be`BiyBdZBZBN�BQ�BN�BM�BJ�BL�BM�BM�BL�BM�BL�BK�BL�BK�BL�BL�BK�BH�BM�BK�BM�BM�BL�BM�BJ�BH�BH�BF�BE�BE�BD�BD�BF�BF�BG�BE�BF�BG�BK�BO�BP�BVB\)B\)BZB\)BbNBaHBaHB`BB_;B\)B`BB_;B^5B`BBbNBdZBhsBo�Bt�Bs�Bp�BhsBffBgmBffBhsBiyBjBk�Bl�Bm�Bp�Bs�Bv�Bw�Bw�Bx�Bp�BgmBt�B|�B{�B|�B�B�%B�1B�1B�JB�VB�JB�PB�PB�PB�VB�PB{�B�=B��BƨB�ZB��B�;B	B	B	  B	�B	)�B	K�B	]/B	k�B	q�B	�B	�=B	�\B	�\B	��B	��B	��B	��B	��B	�-B	�XB	ƨB	��B	��B	��B	�B	�
B	�)B	�NB	�TB	�mB	�B	��B	��B	��B	��B	��B	��B	��B
B
B
DB
DB

=B

=B
	7B
JB
PB
PB
PB
\B
bB
{B
�B
�B
�B
�B
�B
�B
�B
'�B
&�B
&�B
%�B
)�B
+B
/B
1'B
49B
7LB
8RB
;dB
;dB
;dB
=qB
A�B
A�B
D�B
E�B
F�B
F�B
G�B
H�B
H�B
G�B
G�B
E�B
F�B
F�B
E�B
G�B
H�B
G�B
H�B
H�B
N�B
M�B
N�B
O�B
O�B
Q�B
P�B
P�B
P�B
R�B
R�B
Q�B
R�B
S�B
R�B
S�B
T�B
W
B
VB
XB
W
B
XB
XB
XB
ZB
[#B
[#B
\)B
^5B
aHB
dZB
cTB
dZB
dZB
e`B
hsB
hsB
jB
jB
jB
iyB
jB
m�B
n�B
n�B
o�B
o�B
p�B
p�B
p�B
q�B
r�B
r�B
r�B
s�B
t�B
u�B
y�B
x�B
x�B
x�B
z�B
y�B
z�B
{�B
}�B
|�B
}�B
}�B
}�B
}�B
~�B
� B
�B
�B
�%B
�%B
�B
�DB
�PB
�JB
�VB
�\B
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
��B
��B
��B
�B
�B
�!B
�B
�B
�'B
�-B
�'B
�3B
�9B
�?B
�FB
�?B
�LB
�LB
�RB
�RB
�XB
�XB
�XB
�^B
�XB
�^B
�^B
�^B
�^B
�^B
�^B
�^B
�dB
�^B
�dB
�dB
�dB
�dB
�^B
�^B
�^B
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
�^B
�^B
�jB
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
�dB
�dB��B�}B�}B��B��B��B��B��B�}B��B��B��B��B�}B��B��B��B��B�}B�}B��B��B��B��B��B�}B�}B��B��B��B��B��B�}B�}B��B��B�}B�}B��B��B��B��B��B�}B��B��B��B��B��B��B��B�}B�}B��B��B�wB��B�}B�}B��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          B�XB�XB�XB�YB�YB�YB�ZB�ZB�TB�TB�UB�UB�\B�QB�WB�RB�RB�SB�TB�TB�[B�UB�VB�WB�cB�dB�eB�eB�lB�rB�sBāBșBɠBʧBͺB��B��B��B�B�B� B�3B�kB��BBCB]B�B'�BQ�BZBZBXBZBZBY	BL�BA|B69B+�B�B{BKBB�B�\B�EB�:B�!B�B�	B�B��B��BʽB�fB��B�:B�Bz�Bv�Bp�BixBYB<jB1(B+B�B�BrBSB	;B B
�B
�SB
�B
��B
��B
�:B
�#B
�B
��B
�~B
�B
r�B
WB
C�B
8]B
-B
�B
iB
,B	�B	�\B	�B	��B	�SB	�.B	~B	l�B	X B	K�B	?�B	:pB	+B	!�B	�B	^B�B��B��B�B�
B��B��B��B�xB��B��B��B��B��B�ZB��B��B�rB�`B�TB�TB�=B�*BBv�Bs�Bk�Be�Bi�Bd{BZ?BN�BRBN�BM�BJ�BL�BM�BM�BL�BM�BL�BK�BL�BK�BL�BL�BK�BH�BM�BK�BM�BN BL�BNBJ�BH�BH�BF�BE�BE�BD�BD�BF�BF�BG�BE�BF�BG�BK�BPBQBV;B\aB\aBZVB\bBb�Ba�Ba�B`}B_wB\eB`B_xB^sB`�Bb�Bd�Bh�Bo�Bt�Bs�Bp�Bh�Bf�Bg�Bf�Bh�Bi�Bj�Bk�Bl�Bm�Bp�Bs�BwBxBxBy Bp�Bg�BuB};B|4B}<B�ZB�tB��B��B��B��B��B��B��B��B��B��B|AB��B�B�B��B�UBߧB	�B	�B	 uB	 6B	*wB	LEB	]�B	l	B	r0B	��B	��B	��B	��B	�B	�VB	�rB	��B	��B	��B	��B	�RB	�oB	�xB	ҠB	ֻB	��B	��B	�B	�B	�3B	�TB	��B	��B	��B	��B	��B	��B	��B
�B
�B
+B
.B
*B
-B

*B
@B
IB
LB
OB
^B
gB
�B
�B
�B
�B
�B
�B
�B
�B
)B
(
B
(B
'	B
+%B
,.B
0IB
2XB
5lB
8�B
9�B
<�B
<�B
<�B
>�B
B�B
B�B
E�B
F�B
G�B
G�B
IB
JB
JB
IB
IB
GB
HB
HB
GB
IB
J&B
I#B
J,B
J/B
PWB
OTB
P]B
QfB
QiB
SyB
RuB
RxB
R{B
T�B
T�B
S�B
T�B
U�B
T�B
U�B
V�B
X�B
W�B
Y�B
X�B
Y�B
Y�B
Y�B
[�B
\�B
\�B
]�B
`B
cB
f*B
e'B
f/B
f1B
g9B
jOB
jQB
l_B
laB
lcB
k`B
lhB
o|B
p�B
p�B
q�B
q�B
r�B
r�B
r�B
s�B
t�B
t�B
t�B
u�B
v�B
w�B
{�B
z�B
z�B
z�B
|�B
{�B
|�B
~B
�B
B
�B
� B
�#B
�%B
�.B
�:B
�EB
�aB
�rB
�wB
�wB
��B
��B
��B
��B
��B
��B
��B
�
B
�B
�!B
�5B
�AB
�NB
�RB
�^B
�wB
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
��B
��B
��B
�
B
�B
�B
�*B
�(B
�;B
�<B
�VB
�~B
��B
��B
��B
��B
��B
��B
�B
�B
�,B
�BB
�KB
�fB
�vB
��B
��B
��B
��B
��B
��B
��B
�B
�B
�!B
�0B
�3B
�6B
�9B
�BB
�?B
�IB
�LB
�NB
�RB
�OB
�QB
�UB
�^B
�[B
�dB
�fB
�jB
�nB
�pB
�sB
�vB
�yB
�}B
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
��B�XB�RB�RB�^B�XB�^B�XB�XB�RB�XB�^B�XB�XB�RB�XB�XB�XB�^B�RB�RB�XB�^B�XB�XB�XB�RB�RB�XB�XB�^B�XB�XB�RB�RB�XB�XB�RB�RB�XB�XB�YB�YB�YB�SB�YB�YB�YB�YB�YB�YB�ZB�TB�TB�ZB�ZB�NB�ZB�TB�TB�[G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�201903311700372021061413561020210614135610202106171314212021061713142120210617131421201903311700372021061413561020210614135610202106171314212021061713142120210617131421PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            new conductivity = original conductivity * (1 + delta*TEMP + CPcor_SBE*PRES) / (1 + delta*TEMP_ADJUSTED + CPcor_new*PRES_ADJUSTED)                                                                                                                              none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            new conductivity = original conductivity * (1 + delta*TEMP + CPcor_SBE*PRES) / (1 + delta*TEMP_ADJUSTED + CPcor_new*PRES_ADJUSTED)                                                                                                                              none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.50 dbar                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            CPcor_new = -1.25e-07; CPcor_SBE = -9.57e-08; delta = 3.25e-06                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.50 dbar                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            CPcor_new = -1.25e-07; CPcor_SBE = -9.57e-08; delta = 3.25e-06                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                      none                                                                                                                                                                                                                                                            New conductivity computed by using a different CPcor value from that provided by Sea-Bird.                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            No significant salinity drift detected.                                                                                                                                                                                                                         Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                      none                                                                                                                                                                                                                                                            New conductivity computed by using a different CPcor value from that provided by Sea-Bird.                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            No significant salinity drift detected.                                                                                                                                                                                                                         PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            AO  AO  ARCAARCAADJPADJP                                                                                                                                        2019033117003720190331170037  IP  IP                                G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2019033117003720190331170037QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�5F03E           703E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2019033117003720190331170037QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�0               0               PM  PM  ARSQARSQCOWGCOWGV1.1V1.1                                                                                                                                2021061713151620210617131516IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                