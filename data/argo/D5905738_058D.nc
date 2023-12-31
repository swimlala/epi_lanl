CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS     N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2019-03-03T05:00:57Z creation      
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
_FillValue                 $  Lh   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  P�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 $  a   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  e8   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  u�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 $  �H   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �l   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 $  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 $  �(   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �L   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 $  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                   �   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                   �   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                   �   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                       HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  �    HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                   �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                   �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar        �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar        �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�       �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  �   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �(   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �(   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                   (   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  � (      � (Argo profile    3.1 1.2 19500101000000  20190303050057  20210722160158  5905738 5905738 US ARGO PROJECT                                                 US ARGO PROJECT                                                 GREGORY C. JOHNSON                                              GREGORY C. JOHNSON                                              PRES            TEMP            PSAL            PRES            TEMP            PSAL               :   :DD  AOAO7218                            7218                            2C  2C  DD  SOLO_D_MRV                      SOLO_D_MRV                      12001                           12001                           SBE603 01Dec16                  SBE603 01Dec16                  874 874 @ئ��ؕ�@ئ��ؕ�11  @ئ���P�@ئ���P�@5���?��@5���?���c�!-w1��c�!-w1�11  GPS     GPS     Primary sampling: averaged [1Hz sampling by SBE-41CP averaged in 2-dbar bins]                                                                                                                                                                                   Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 AC  AA  AA  >���?fff@ff@Fff@�33@�33@�  @�  A   A33A&ffAA��A`  A�  A�  A�33A�  A���A���AᙚA���B   BffBffB  B   B(ffB0ffB8  B@  BG��BO��BX  B`ffBh  Bp��Bx��B�  B���B�  B�  B�  B�  B���B���B�  B�  B�  B�33B�ffB�33B�33B�33B���B�  B�  B�33B�33B�33B���B���Bߙ�B�33B�  B뙚B���B�33B�  B���C 33CL�C�C�C�C
  C  C  C��C�fC�fC��C�C33C33CL�C��C"  C$33C&�C(L�C*  C,�C.33C/�fC2  C4�C6�C8L�C9��C<  C>�C@�CBL�CC�fCE�fCH33CJ33CL�CN  CO�fCR33CT33CV�CX  CY�fC\�C^L�C`33Cb  CdL�Cf�Ch  Cj�Cl  Cm��Cp�Cr  Cs��Cv�CxL�Cz33C|�C~�C�  C�  C��3C��C�  C��C��C�&fC��3C��fC��C��C��C��C��C�&fC�  C�  C��C��C��C��C�&fC��C��C��fC��fC��fC�  C�  C�  C��3C��3C�  C��3C�  C��3C�&fC��C��C��C�  C�&fC�&fC��C��C��C��C��C��3C�ٚC�  C��C��C��3C��C��C��fC��C��C�  C�  C��fC��C�&fC�33C�&fC��C�  C��3C��3C��C��C�  C��3C��3C��C�&fC��C��C�&fC��C��C�  C��3C��C�  C��3C�  C�  C�  C��3C��fC��3C��3C��C��C��C��C��C��C��C��C��C�  C��3C��C��C��3C��C��C��3C��C�  C��3C��C�&fC�&fC��C�  C��fC��C��C��3C�� C��3C�  C��D 3D y�D�D��D��D��D��D
�fDL�D�D�3DffD�D� DFfD� D"� D%3D'�fD*l�D-  D/� D2� D5l�D8@ D;  D=�3D@�fDC��DF��DI�fDL��DO� DRY�DU33DXfDZ�3D]�fD`l�Dc33De��Dh��DkFfDn  Dp��DsffDu�3Dxs3Dz��D}  D�fD�fD�I�D���D�ɚD�3D�FfD�y�D��3D��fD�,�D�l�D���D���D�0 D�i�D�� D��3D�<�D�|�D���D���D�@ D���D�ɚD�fD�VfD���D���D�9�D�� D��fD�fD�P D���D�� D�#3D�i�D���D��fD�)�D�p D���D��D�&fD�i�D�� D��D�0 D�l�D���D�� D��D�\�DÙ�D�� D��D�I�Dȉ�D�� D���D�)�D�c3DΣ3D��fD��D�S3DӐ D�ٚD�  D�Y�Dؙ�D�ٚD��D�\�Dݜ�D�� D�  D�c3D��D��fD�@ D��D�ɚD�fD�@ D�3D��3D���D�6fD�s3D�3D��3D�	�D�<�D�p D��fD��3D�3D�fD�@ D�ffD���D�� E k3E �fE��E$�E��EK3E��El�E�fE� E#3E��ED�E��Ed�E�fE	vfE
�E
�3E E�fE�fE�fE�fE��E�3E��EɚE~fE��E�3E��EfE� E�3E3EA�E p E!��E"� E$��E%� E&��E'�E)3E*� E+�3E-�E.fE/C3E0� E2	�E3$�E4�3E5��E6��E8x E9��E:�fE<3E?3EBffEE1�EH[3EKp EN��EQ� ET�fEW�3E[^fE^{3Ea��Ed� Eg��Ej�3En�Eq( Et( Ewx EzP E}x E�H E��fE�bfE��E���E�3E�� E�,�E���E�` E�� E���E��fE��fE�-�E��3E�\ E���E�W3E��3E��E�P�E��3E� �E�D E��fE��E�8 E�|�E�� E�/3E�w3E���>���>���>���>���>L��>���>���>L��>���>L��>L��>���>L��>���>���>���?   ?   ?   ?��?��?��?��?333?333?���?�  ?�ff?�  ?�ff@ff@33@,��@@  @S33@`  @s33@�33@�  @���@�33@�  @�ff@�  @���@���@�33@�33A   A  A  A��A33A!��A)��A0  A6ffA;33AA��AFffG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111444414414414141144144414141111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       ?L��?�33@&ff@fff@�33@�33@�  @�  A  A33A.ffAI��Ah  A�  A�  A�33A�  A���A���A噚A���B  B
ffBffB  B"  B*ffB2ffB:  BB  BI��BQ��BZ  BbffBj  Br��Bz��B�  B���B�  B�  B�  B�  B���B���B�  B�  B�  B�33B�ffB�33B�33B�33B���B�  B�  B�33B�33B�33B���B���B���B�33B�  B왚B���B�33B�  B���C �3C��C��C��C��C
� C� C� CL�CffCffCL�C��C�3C�3C��C L�C"� C$�3C&��C(��C*� C,��C.�3C0ffC2� C4��C6��C8��C:L�C<� C>��C@��CB��CDffCFffCH�3CJ�3CL��CN� CPffCR�3CT�3CV��CX� CZffC\��C^��C`�3Cb� Cd��Cf��Ch� Cj��Cl� CnL�Cp��Cr� CtL�Cv��Cx��Cz�3C|��C~��C�@ C�@ C�33C�L�C�@ C�L�C�L�C�ffC�33C�&fC�Y�C�Y�C�Y�C�Y�C�Y�C�ffC�@ C�@ C�L�C�L�C�Y�C�Y�C�ffC�Y�C�Y�C�&fC�&fC�&fC�@ C�@ C�@ C�33C�33C�@ C�33C�@ C�33C�ffC�Y�C�Y�C�L�C�@ C�ffC�ffC�L�C�L�C�L�C�Y�C�L�C�33C��C�@ C�Y�C�L�C�33C�Y�C�L�C�&fC�L�C�L�C�@ C�@ C�&fC�L�C�ffC�s3C�ffC�Y�C�@ C�33C�33C�L�C�L�C�@ C�33C�33C�L�C�ffC�Y�C�L�C�ffC�Y�C�L�C�@ C�33C�Y�C�@ C�33C�@ C�@ C�@ C�33C�&fC�33C�33C�Y�C�Y�C�Y�C�Y�C�Y�C�L�C�L�C�L�C�L�C�@ C�33C�L�C�L�C�33C�Y�C�L�C�33C�L�C�@ C�33C�L�C�ffC�ffC�L�C�@ C�&fC�Y�C�L�C�33C�  C�33C�@ C�Y�D 33D ��D,�D��D�D��D�D
�fDl�D9�D�3D�fD,�D� DffD   D"� D%33D'�fD*��D-@ D0  D2� D5��D8` D;  D>3DAfDC��DF��DI�fDL��DO� DRy�DUS3DX&fDZ�3D]�fD`��DcS3Df�Dh��DkffDn  DpٚDs�fDv3Dx�3D{�D}  D�fD�fD�Y�D���D�ٚD�3D�VfD���D��3D�fD�<�D�|�D���D���D�@ D�y�D�� D�3D�L�D���D���D��D�P D���D�ٚD�fD�ffD���D�	�D�I�D�� D��fD�fD�` D���D�� D�33D�y�D���D��fD�9�D�� D���D���D�6fD�y�D�� D���D�@ D�|�D���D�� D�)�D�l�Dé�D�� D��D�Y�Dș�D�� D��D�9�D�s3Dγ3D��fD�)�D�c3DӠ D��D�0 D�i�Dة�D��D�,�D�l�Dݬ�D�� D�0 D�s3D��D�fD�P D��D�ٚD�fD�P D�3D��3D�	�D�FfD��3D�3D��3D��D�L�D�� D��fD��3D�3D�&fD�P D�vfD���D�� E s3EfE��E,�E��ES3E��Et�EfE� E+3E��EL�E��El�E�fE	~fE
�E
�3E  E�fE�fE�fE�fE��E�3E��EњE�fE��E�3E��EfE� E�3E#3EI�E x E!��E"� E$��E%� E&��E'��E)3E*� E+�3E-	�E.&fE/K3E0� E2�E3,�E4�3E5��E6��E8� E9��E:�fE<3E?3EBnfEE9�EHc3EKx EN��EQ� ET�fEW�3E[ffE^�3Ea��Ed� Eg��Ej�3En$�Eq0 Et0 Ew� EzX E}� E�L E��fE�ffE��E���E�3E�� E�0�E���E�d E�� E���E��fE��fE�1�E��3E�` E���E�[3E��3E��E�T�E��3E��E�H E��fE���E�< E���E�� E�33E�{3E���G�O�G�O�G�O�G�O�?333G�O�G�O�?333G�O�G�O�?333G�O�?333G�O�?L��?fffG�O�G�O�?�  G�O�G�O�G�O�?���G�O�?���G�O�?�  ?�ff@   @33@&ff@333@L��@`  @s33@�  @���@�33@�  @���@�33@�  @�ff@�  @���@���@�33A��A  A  A  A��A#33A)��A1��A8  A>ffAC33AI��ANffG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111444414414414141144144414141111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       @ @ %@ V@ *@ �@ "�@ (�@ /�@ 6�@ >�@ F�@ R�@ _�@ m:@ z�@ ��@ �0@ ��@ �-@ ��@ �|@ �t@ ��@ �q@j@@g@-@:@G�@T�@bN@p�@~�@��@��@��@��@��@�7@��@�@�,@%@�@""@/�@=q@K�@Z@g@t�@�d@��@�@��@�@ƨ@�O@��@�@@�9@
�@�@$.@2�@A�@N�@[z@k.@y�@��@�u@�@�@��@�c@խ@�@�@��@V@�@*S@8�@B�@Q�@`�@m�@|�@��@��@�5@��@�&@�|@�#@�(@�e@j@�@g@.l@9X@F�@V�@dZ@qS@~K@�D@��@��@��@@ψ@ލ@�@��@�@�@"�@/�@>@K@Wb@g@t@�W@�@�@�@�@ƨ@Ӡ@�H@�@@�E@
=@�@&;@5?@@,@M$@]�@k.@x�@�|@�$@�y@�@��@�@׹@�@�@@@�@'�@5?@B�@Q�@_�@m:@z3@��@�0@�(@�~@�w@��@��@�y@�q@	j@	@	 �@	-@	:�@	H]@	V�@	c�@	o�@	|?@	��@	��@	��@	�9@	��@	��@	܀@	�4@	��@
�@
{@
 �@
0x@
?}@
M�@
Z�@
g�@
t@
�@
��@
��@
�Y@
�R@
�J@
��@
��@
��@
��@
�@�@&�@3�@@�@M�@]�@i�@v�@�@��@�m@�f@�^@ȴ@�[@�@�@^@@�@)�@7L@D�@R�@_�@l�@{�@�7@��@�5@�-@�w@�|@�t@�m@�q@v@@g@,`@8�@I@V@bN@m:@}�@��@��@�M@�9@��@��@�/@��@�@ƨ@o@^�@��@�@7�@~K@ƨ@�@UU@��@�`@-�@ww@@�@Z@�A@�Y@B�@�u@�@4�@��@��@""@o�@��@
�@Wb@��@�L@<@�|@ψ@�@b�@�f@�q@<@�W@�J@��@A�@�|@�o@b@S�@��@��@[@`A@�4@�m@+�@o�@�9@�,@<@��@ƨ@ �@ Q=@ ��@ ��@!�@!e	@!�M@!��@"4�@"|�@"Ĝ@#�@#N�@#�#@#�h@$�@$e	@$��@$�@%5@@%z3@%�@&@&G�@&��@&��@'o@'Wb@'�@'��@(%�@(i!@(��@(��@)1�@)v�@)�^@)��@*@,@*��@*�@+
=@+M�@+��@+��@,*@,V�@,��@,ލ@-""@-hs@-�@-��@.5@@.y�@.�w@/�@/F�@/��@/�7@0*@0[z@0��@0�@1/@1r�@1�F@1�,@2>@2�d@2Ĝ@31@3K�@3��@3�|@4�@4Q=@4��@4��@5*@5V@5��@5�7@6@6M�@6��@6��@7	�@7H]@7�+@7ƨ@8�@8B�@8�W@8�w@8��@9;d@9y�@9��@9�e@:1�@:l�@:��@:�T@;�@;X�@;�@<	�@<�@<�@=��@>�@>�u@?V@?ȴ@@D�@@��@A<@A�@Bww@B� @Cuk@C�@Dt@D��@Eww@F5@@F�~@G.l@G��@H#�@H�t@ISI@I��@JJi@J�W@K~K@K�q@LoF@M @M��@N�@N��@O+@O��@P;d@Q�@R�@T"�@U|?@V��@X�@Yt@Z�J@\�@]��@^��@`?}@a��@bխ@d�@e�<@f�@h,`@i�0@j��@l%�@mww@n�1@pZ@q�@r�<@t�@uww@v�>@x3�@y~K@z@|)�@}k�@~��@�u@���@�n�@��@���@��@� W@�@�=@�j(@���@���@���@���@�J@�*�@�Xh@�w @���G�O�G�O�G�O�G�O�@ ^G�O�G�O�@ ^G�O�G�O�@ ^G�O�@ ^G�O�@ @ �G�O�G�O�@ jG�O�G�O�G�O�@ G�O�@ �G�O�@ �@ �@ 
=@ J@ V@ �@ o@ {@ �@ �@ �@ �@ �@  �@ "�@ %�@ &�@ (�@ +�@ /@ 0x@ 3�@ 6�@ :@ =q@ ?}@ B8@ D�@ H]@ K@ M�@ O�@ R�@ T�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�"�A� �A� �A�$�A�33A�C�A�C�A�G�A�G�A�M�A�Q�A�ZA�ZA�ZA�\)A�\)A�bNA�dZA�bNA�bNA�bNA�bNA�bNA�dZA�bNA�dZA�bNA�`BA�dZA�\)A�\)A�&�A��A�bA���A��`A��-A���A���A��+A�v�A�bNA�7LA�$�A�"�A�(�A�33A�9XA�"�A�oA���A��HA��A�p�A�O�A�E�A�?}A�A�A���A�hsA���A�1'A��A�1'A��A��A�dZA��PA���A���A��yA��yA��`A��/A��9A��A� �A��A�|�A�ZA��FA��A�ffA���A��\A�bNA�1'A�5?A�A�A���A��hA���A�;dA�  A�VA�z�A���A��A�(�A��mA��A�A�ƨA��A��-A��
A�v�A�"�A�(�A���A��DA�ZA�ȴA�ffA��A�7LA���A��7A��jA�A�C�A��A�"�A�M�A�A~��A{�
AwƨAqG�ApAo�Aj��Ag`BAd^5AcAa�FA_�7A[7LAY�AX�yAU�7AQ��AP�AM�#AK�
AG�AE�#AE"�ADA@ĜA>��A>^5A=�A="�A<�\A;O�A:n�A7�A61A4�yA3XA2ZA1��A0(�A/`BA.-A,��A+��A)��A(�jA'�mA&bA$�!A$5?A#dZA!&�A�jA�uA|�A�A��A�7A��A/A  A-AM�A
=Ax�A7LA�A�HA^5A
z�A	7LA�jAx�A��A�AC�A�RAVA�At�A��A-A�
A��At�A�A ��A �D@�|�@�hs@��w@�+@��@�V@���@�  @�33@�M�@��@�@�%@��m@��@�G�@� �@�|�@�"�@�5?@�x�@�?}@���@��@�;d@�J@�X@�9@�A�@��
@��@��@��#@�Z@�o@�-@�{@�ff@��@��T@ܴ9@ۮ@�n�@��#@���@׍P@���@ӥ�@�dZ@�\)@���@�7L@���@���@�"�@ǍP@�r�@�+@�`B@���@��D@�ȴ@�ff@�ƨ@��h@���@���@���@�V@�Z@���@��9@��j@���@��-@��9@�bN@��@�M�@�p�@�/@��u@���@�@�`B@���@�"�@�@�Ĝ@�C�@���@�b@�dZ@�^5@��^@�`B@��`@���@���@�p�@�C�@�hs@�j@��@~$�@}V@|�/@{t�@z�@x��@v��@u?}@sƨ@r�!@q&�@o
=@n�R@m/@l(�@j-@i�@g
=@e��@cS�@b-@`b@]��@\�/@[C�@X1'@VV@TZ@S�
@Q�#@P �@NE�@L(�@Ix�@HĜ@G|�@F5?@D��@B�!@A�7@@1'@=��@<I�@:=q@8b@6��@5�-@4I�@2��@2=q@1�@0Q�@/�@/;d@-�@-�@+�F@*��@)hs@(�u@'|�@'
=@&ff@%��@$(�@#��@"�@"J@!x�@ ��@�;@��@��@�j@��@��@�\@��@Ĝ@�@�w@@��@(�@1@S�@�!@�@&�@�@�;@+@�+@�T@@V@�m@�F@t�@33@
�@	&�@Ĝ@b@��@�@5?@`B@`B@9X@33@C�@7L@  �@   ?���?�V?��?�/?��?�1'?���?���?�\?��?�;d?�?�?ꟾ?陚?�b?�
=?�$�?�?�t�?�7?�A�?�;d?�5??ܬ?�ƨ?�"�?��#?�Q�?׮?�K�?ա�?��?ӶF?�33?�n�?�&�?� �?Ͼw?�{?���?�/?�j?���?���?ǍP?Ƈ+?��/?��?���?�A�?���?�5??��h?�/?��?��?�I�?�(�?��m?���?��?�C�?�?�?���?�~�?���?���?��H?�"�?�C�?���?��m?�(�?��D?���?�O�?���?�5??�5??�V?��R?��R?��R?�V?�v�?�V?�V?�{?��-?��h?��h?��-?���?��?�{A��A� �A� �A�"�A� �A��A� �A�"�A�$�A�(�A�1'A�33A�$�A�-A� �A��A��A��A�$�A�+A�$�A�/A��A��A� �A��A��A��A� �A��A�(�A��A�"�A�"�A�"�A�&�A�(�A�33A�?}A�C�A�C�A�C�A�C�A�C�A�E�A�G�A�G�A�G�A�G�A�G�A�I�A�O�A�O�A�Q�A�O�A�VA�^5A�\)A�ZA�ZG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       A�"�A� �A� �A�$�A�33A�C�A�C�A�G�A�G�A�M�A�Q�A�ZA�ZA�ZA�\)A�\)A�bNA�dZA�bNA�bNA�bNA�bNA�bNA�dZA�bNA�dZA�bNA�`BA�dZA�\)A�\)A�&�A��A�bA���A��`A��-A���A���A��+A�v�A�bNA�7LA�$�A�"�A�(�A�33A�9XA�"�A�oA���A��HA��A�p�A�O�A�E�A�?}A�A�A���A�hsA���A�1'A��A�1'A��A��A�dZA��PA���A���A��yA��yA��`A��/A��9A��A� �A��A�|�A�ZA��FA��A�ffA���A��\A�bNA�1'A�5?A�A�A���A��hA���A�;dA�  A�VA�z�A���A��A�(�A��mA��A�A�ƨA��A��-A��
A�v�A�"�A�(�A���A��DA�ZA�ȴA�ffA��A�7LA���A��7A��jA�A�C�A��A�"�A�M�A�A~��A{�
AwƨAqG�ApAo�Aj��Ag`BAd^5AcAa�FA_�7A[7LAY�AX�yAU�7AQ��AP�AM�#AK�
AG�AE�#AE"�ADA@ĜA>��A>^5A=�A="�A<�\A;O�A:n�A7�A61A4�yA3XA2ZA1��A0(�A/`BA.-A,��A+��A)��A(�jA'�mA&bA$�!A$5?A#dZA!&�A�jA�uA|�A�A��A�7A��A/A  A-AM�A
=Ax�A7LA�A�HA^5A
z�A	7LA�jAx�A��A�AC�A�RAVA�At�A��A-A�
A��At�A�A ��A �D@�|�@�hs@��w@�+@��@�V@���@�  @�33@�M�@��@�@�%@��m@��@�G�@� �@�|�@�"�@�5?@�x�@�?}@���@��@�;d@�J@�X@�9@�A�@��
@��@��@��#@�Z@�o@�-@�{@�ff@��@��T@ܴ9@ۮ@�n�@��#@���@׍P@���@ӥ�@�dZ@�\)@���@�7L@���@���@�"�@ǍP@�r�@�+@�`B@���@��D@�ȴ@�ff@�ƨ@��h@���@���@���@�V@�Z@���@��9@��j@���@��-@��9@�bN@��@�M�@�p�@�/@��u@���@�@�`B@���@�"�@�@�Ĝ@�C�@���@�b@�dZ@�^5@��^@�`B@��`@���@���@�p�@�C�@�hs@�j@��@~$�@}V@|�/@{t�@z�@x��@v��@u?}@sƨ@r�!@q&�@o
=@n�R@m/@l(�@j-@i�@g
=@e��@cS�@b-@`b@]��@\�/@[C�@X1'@VV@TZ@S�
@Q�#@P �@NE�@L(�@Ix�@HĜ@G|�@F5?@D��@B�!@A�7@@1'@=��@<I�@:=q@8b@6��@5�-@4I�@2��@2=q@1�@0Q�@/�@/;d@-�@-�@+�F@*��@)hs@(�u@'|�@'
=@&ff@%��@$(�@#��@"�@"J@!x�@ ��@�;@��@��@�j@��@��@�\@��@Ĝ@�@�w@@��@(�@1@S�@�!@�@&�@�@�;@+@�+@�T@@V@�m@�F@t�@33@
�@	&�@Ĝ@b@��@�@5?@`B@`B@9X@33@C�@7L@  �@   ?���?�V?��?�/?��?�1'?���?���?�\?��?�;d?�?�?ꟾ?陚?�b?�
=?�$�?�?�t�?�7?�A�?�;d?�5??ܬ?�ƨ?�"�?��#?�Q�?׮?�K�?ա�?��?ӶF?�33?�n�?�&�?� �?Ͼw?�{?���?�/?�j?���?���?ǍP?Ƈ+?��/?��?���?�A�?���?�5??��h?�/?��?��?�I�?�(�?��m?���?��?�C�?�?�?���?�~�?���?���?��H?�"�?�C�?���?��m?�(�?��D?���?�O�?���?�5??�5??�V?��R?��R?��R?�V?�v�?�V?�V?�{?��-?��h?��h?��-?���?��?�{A��A� �A� �A�"�A� �A��A� �A�"�A�$�A�(�A�1'A�33A�$�A�-A� �A��A��A��A�$�A�+A�$�A�/A��A��A� �A��A��A��A� �A��A�(�A��A�"�A�"�A�"�A�&�A�(�A�33A�?}A�C�A�C�A�C�A�C�A�C�A�E�A�G�A�G�A�G�A�G�A�G�A�I�A�O�A�O�A�Q�A�O�A�VA�^5A�\)A�ZA�ZG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�BbNBcTBaHBcTBcTBbNBbNBcTBbNBcTBdZBcTBcTBdZBdZBdZBcTBcTBcTBcTBdZBdZBdZBgmBhsBffBgmBjBiyBk�Bl�B�%B�PB�\B��B��B�3B�RBɺB��B��B��B��B��B��B��B�B�B�fB�yB�B�B��BBDBPBbB�B1'B2-B(�B�B�B�5B�dB�RB�
B��BB �B;dBN�B[#BaHBgmBm�BiyBG�B;dB?}B=qB1'B)�B�B�B{B{B{B�B9XB<jB1'B-B)�B"�B�B
=B��B�;B|�Bu�B�B�B{�BC�B �B+B
��B
��B
ǮB
�BB
�mB
�B
�fB
�`B
�B
�B
�yB
ĜB
�!B
�{B
{�B
t�B
n�B
aHB
R�B
/B
#�B	��B	��B	��B	�9B	�JB	u�B	dZB	XB	M�B	;dB	 �B	�B	VB�B�TB��B��B�qB��B��B��B��B�oB�oB�bB�PB�JB�1B�B~�Bz�By�Bv�Bp�Bp�Bl�BhsBhsBaHBaHB[#BXBS�BQ�BL�BO�BN�BO�BI�BXBT�BS�BR�BP�BP�BN�BS�BYB^5B_;B^5B`BBaHBbNBhsBiyBm�Bv�Bx�Bv�By�By�Bw�By�Bw�Bu�Bu�Bu�By�B|�B}�B}�B~�B|�B� B�B�B�+B�7B�7B�=B�JB�\B�bB�uB�{B�uB��B��B��B��B��B��B��B��B��B��B��B�B�B�!B�!B�!B�B�B��B��B��B��B��B��B��B��B��B�B�!B�B�B�B�B��B��B��B��B��B��B��B��B�B��B�TB�mB�B��B��B	DB	,B	9XB	@�B	H�B	T�B	`BB	p�B	� B	�1B	��B	��B	��B	��B	��B	�B	�?B	�XB	�wB	ĜB	��B	��B	��B	�B	�)B	�/B	�TB	�mB	�B	�B	�B	��B	��B	��B	��B	��B
  B
B
B
	7B
DB
JB
JB
\B
bB
bB
bB
bB
{B
{B
�B
�B
�B
�B
�B
�B
�B
 �B
!�B
"�B
#�B
&�B
'�B
,B
.B
.B
0!B
0!B
1'B
49B
6FB
7LB
8RB
:^B
<jB
<jB
?}B
B�B
B�B
C�B
E�B
F�B
H�B
I�B
J�B
L�B
L�B
O�B
P�B
R�B
R�B
T�B
T�B
VB
W
B
XB
XB
YB
ZB
[#B
\)B
\)B
^5B
_;B
aHB
`BB
aHB
aHB
dZB
dZB
dZB
ffB
gmB
gmB
gmB
iyB
jB
jB
k�B
k�B
l�B
l�B
n�B
n�B
o�B
p�B
q�B
q�B
r�B
q�B
r�B
t�B
t�B
t�B
v�B
w�B
w�B
x�B
x�B
x�B
z�B
y�B
y�B
z�B
{�B
|�B
|�B
}�B
}�B
~�B
� B
�B
~�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�%B
�+B
�1B
�7B
�DB
�JB
�JB
�VB
�\B
�bB
�bB
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
�B
�B
�B
�!B
�'B
�-B
�3B
�?B
�FB
�FB
�FB
�RB
�RB
�XB
�XB
�^B
�^B
�^B
�jB
�dB
�qB
�qB
�qB
�qB
�wB
�wB
�}B
�}B
�}B
��B
��B
��B
��B
��B
��B
��B
B
B
B
��B
B
B
B
B
B
ÖB
ÖB
ÖB
ÖB
ÖB
ÖB
ÖB`BBbNBdZBbNBaHBbNBaHBaHBcTBbNBe`B_;BcTB_;BbNBbNBcTBdZBdZB`BBcTB`BBcTBdZBe`BdZBbNBdZBbNBbNB_;BcTBcTBbNBcTBcTBcTBdZBbNBbNBbNBbNBbNBcTBbNBcTBbNBbNBbNBcTBcTBcTBdZBdZBdZBe`BdZBbNBcTBcTG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       B_;B`BB^5B`BB`BB_;B_;B`BB_;B`BBaHB`BB`BBaHBaHBaHB`BB`BB`BB`BBaHBaHBaHBdZBe`BcTBdZBgmBffBhsBiyB�B�=B�JB�{B��B�!B�?BƨB��B��B��B��B��B��B��B��B�
B�TB�fB�B�B��BB1B
=BPB�B.B/B%�B�B�B�#B�RB�?B��B�BB�B8RBK�BXB^5BdZBjBffBD�B8RB<jB:^B.B&�B�B{BhBhBhB�B6FB9XB.B)�B&�B�B�B+B��B�)By�Br�B� B�Bx�B@�B�BB
��B
��B
ĜB
�/B
�ZB
�B
�TB
�NB
�
B
�mB
�fB
��B
�B
�hB
x�B
q�B
k�B
^5B
O�B
,B
 �B	�B	ɺB	�qB	�'B	�7B	r�B	aHB	T�B	J�B	8RB	�B	oB	DB�sB�BB��BǮB�^B��B��B��B��B�\B�\B�PB�=B�7B�B�B{�Bw�Bv�Bs�Bm�Bm�BiyBe`Be`B^5B^5BXBT�BP�BN�BI�BL�BK�BL�BF�BT�BQ�BP�BO�BM�BM�BK�BP�BVB[#B\)B[#B]/B^5B_;Be`BffBjBs�Bu�Bs�Bv�Bv�Bt�Bv�Bt�Br�Br�Br�Bv�By�Bz�Bz�B{�By�B|�B~�B�B�B�%B�%B�+B�7B�JB�PB�bB�hB�bB�uB��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�B��B��B��B��B��B��B��B��B��B��B��B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B�BB�ZB�yB��B��B	1B	(�B	7LB	>wB	F�B	R�B	^5B	n�B	}�B	�%B	��B	��B	��B	��B	��B	�B	�3B	�LB	�jB	B	ɺB	��B	��B	�B	�B	�#B	�HB	�`B	�yB	�B	�B	�B	��B	��B	��B	��B	��B	��B
B
+B
	7B

=B

=B
PB
VB
VB
VB
VB
oB
oB
�B
�B
�B
�B
�B
�B
�B
�B
�B
 �B
!�B
$�B
%�B
)�B
,B
,B
.B
.B
/B
2-B
49B
5?B
6FB
8RB
:^B
:^B
=qB
@�B
@�B
A�B
C�B
D�B
F�B
G�B
H�B
J�B
J�B
M�B
O�B
Q�B
Q�B
S�B
S�B
T�B
VB
W
B
W
B
XB
YB
ZB
[#B
[#B
]/B
^5B
`BB
_;B
`BB
`BB
cTB
cTB
cTB
e`B
ffB
ffB
ffB
hsB
iyB
iyB
jB
jB
k�B
k�B
m�B
m�B
n�B
o�B
p�B
p�B
q�B
p�B
q�B
s�B
s�B
s�B
u�B
v�B
v�B
w�B
w�B
w�B
y�B
x�B
x�B
y�B
z�B
{�B
{�B
|�B
|�B
}�B
~�B
� B
}�B
� B
�B
� B
�B
�B
�B
�B
�B
�B
�B
�B
�%B
�+B
�1B
�=B
�DB
�DB
�VB
�\B
�bB
�bB
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
�B
�B
�B
�'B
�-B
�3B
�9B
�FB
�LB
�LB
�LB
�XB
�XB
�^B
�^B
�dB
�dB
�dB
�qB
�qB
�}B
�}B
�}B
�}B
��B
��B
��B
��B
��B
B
B
B
B
B
ÖB
ÖB
ĜB
ŢB
ŢB
ĜB
ŢB
ŢB
ŢB
ŢB
ŢB
ƨB
ƨB
ƨB
ƨB
ƨB
ƨB
ƨB]/B_;BaHB_;B^5B_;B^5B^5B`BB_;BbNB\)B`BB\)B_;B_;B`BBaHBaHB]/B`BB]/B`BBaHBbNBaHB_;BaHB_;B_;B\)B`BB`BB_;B`BB`BB`BBaHB_;B_;B_;B_;B_;B`BB_;B`BB_;B_;B_;B`BB`BB`BBaHBaHBaHBbNBaHB_;B`BB`BG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�201903030500572021061413531720210614135317202106141747092021061417470920210614174709201903030500572021061413531720210614135317202106141747092021061417470920210614174709PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            new conductivity = original conductivity * (1 + delta*TEMP + CPcor_SBE*PRES) / (1 + delta*TEMP_ADJUSTED + CPcor_new*PRES_ADJUSTED)                                                                                                                              none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS                                                                                                                                                                                                                                       PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            new conductivity = original conductivity * (1 + delta*TEMP + CPcor_SBE*PRES) / (1 + delta*TEMP_ADJUSTED + CPcor_new*PRES_ADJUSTED)                                                                                                                              none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS                                                                                                                                                                                                                                       surface_pressure=-0.50 dbar                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            CPcor_new = -1.25e-07; CPcor_SBE = -9.57e-08; delta = 3.25e-06                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            OW r = 0.9999 (+/-0), vertically averaged dS = -0.003 (+/-0)                                                                                                                                                                                                    surface_pressure=-0.50 dbar                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            CPcor_new = -1.25e-07; CPcor_SBE = -9.57e-08; delta = 3.25e-06                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            OW r = 0.9999 (+/-0), vertically averaged dS = -0.003 (+/-0)                                                                                                                                                                                                    Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                      none                                                                                                                                                                                                                                                            New conductivity computed by using a different CPcor value from that provided by Sea-Bird.                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Significant salinity sensor drift detected. OW least squares fit adopted.                                                                                                                                                                                       Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                      none                                                                                                                                                                                                                                                            New conductivity computed by using a different CPcor value from that provided by Sea-Bird.                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Significant salinity sensor drift detected. OW least squares fit adopted.                                                                                                                                                                                       PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            AO  AO  ARCAARCAADJPADJP                                                                                                                                        2019030305005720190303050057  IP  IP                                G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2019030305005720190303050057QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�5F03E           703E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2019030305005720190303050057QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�0               0               PM  PM  ARSQARSQCOWGCOWGV1.1V1.1                                                                                                                                2021072216015820210722160158IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                