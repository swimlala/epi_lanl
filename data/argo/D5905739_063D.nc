CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS     N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2019-03-21T19:00:30Z creation      
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
_FillValue                 0  L�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  P�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 0  at   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  e�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  v\   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 0  �   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �D   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 0  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �,   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 0  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 0  ք   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ڴ   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                   �   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                   �   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                   �   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                   �   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  � �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                   t   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                   �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar        �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar        �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�       �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  �l   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                      	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  � Argo profile    3.1 1.2 19500101000000  20190321190030  20210617131516  5905739 5905739 US ARGO PROJECT                                                 US ARGO PROJECT                                                 GREGORY C. JOHNSON                                              GREGORY C. JOHNSON                                              PRES            TEMP            PSAL            PRES            TEMP            PSAL               ?   ?DD  AOAO7219                            7219                            2C  2C  DD  SOLO_D_MRV                      SOLO_D_MRV                      12002                           12002                           SBE603 01Dec16                  SBE603 01Dec16                  874 874 @ث�hR�@ث�hR�11  @ث�DDH`@ث�DDH`@6�O�;d@6�O�;d�c�'RT`��c�'RT`�11  GPS     GPS     Primary sampling: averaged [1Hz sampling by SBE-41CP averaged in 2-dbar bins]                                                                                                                                                                                   Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 AC  AA  AA  >���?�  @ff@Fff@�33@�33@�33@���@���AffA$��AD��Ac33A���A���A�  A�  A�  A���AᙚA���B   B  B��B  B ffB(  B0  B8��B@ffBH��BP��BX  B`ffBh��BpffBx  B�33B�33B�33B�ffB�  B�33B�ffB�  B�  B�  B�33B�  B�33B�  B�  B�ffB�ffB�  B�33B�ffB���B���B�ffB�ffBߙ�B���B�ffB왚B�  B���B���B���C   C  C�C�C�C
  C  C  CL�CL�C33C33C�C�C  C  C��C"33C$�C&�C(  C*  C,  C-�fC0�C2�C4  C6L�C833C:�C;�fC=��C@�CB33CD33CF  CH33CJL�CL33CN�CP  CQ��CS�fCV  CX33CZ  C[��C^�C`33Cb�Cd  Cf�Ch�Ci�fCl33Cn�Co�fCr33CtL�Cv33Cx  CzL�C|L�C~�C��C�  C�&fC�&fC��C��C��C��C��3C�&fC��C�  C��3C��fC��C�  C��fC��C�  C��fC�  C��C�  C��3C��C�&fC�&fC�  C�&fC��C�  C�  C��fC�  C��C�  C��3C�  C�&fC��C�  C��C�&fC��C��3C�  C��C�  C��fC��3C�  C��C�  C�ٚC��3C��3C�  C�  C��C�&fC��C��3C�  C��C��C�&fC�&fC��C��3C�  C��C��C��C�&fC�&fC�33C��C��3C�  C��C�&fC��C��3C�  C��C��C�&fC��C��3C��C�&fC��C��3C�  C��C�  C��fC�  C��C�&fC��C��3C�  C��C��C�  C��3C��C�  C��fC�  C��3C��fC�  C�&fC��C��3C��C�&fC�  C��3C��C�  C��fC��DFfD��D� DFfD��D�3D@ D��D�3DS3D�D �fD#� D&` D),�D+��D.� D1y�D49�D6��D9��D<` D?fDA� DD,�DF��DIS3DK�fDNY�DP�fDS9�DU�fDX&fDZ��D]fD_� Da��DdY�Df� Dis3Dk��Dn��Dq33Ds�3Dvl�Dx��D{,�D}� D�33D�� D��3D�S3D�� D�3D�p D���D�0 D��fD��D�L�D���D�fD�c3D��3D��D�i�D��3D���D�C3D�vfD�� D��fD�&fD�` D�� D��fD�3D�33D�c3D���D��3D���D��D�S3D���D���D��D��D�P D�� D�� D�3D�@ D�|�D�� D���D�0 D�c3D�� D�� D���D��D�<�D�i�DƖfDǶfD���D�  D�#3D�C3D�` D�s3Dϙ�DЩ�D��3D���D���D�3D�  D�6fD�VfD�s3Dړ3D۳3D�� D�� D�3D�0 D�I�D�i�D��D� D幚D��fD��3D��D�6fD�\�D�|�D��fD�� D�  D�,�D�Y�D�y�D��3D��fD��3D��D�FfD�ffD�i�D���D���D�� D���E �fE�E��E6fE��ENfE�3Ei�E��E~fE	�E� E E��EfE	)�E
+3E��E��E8 E<�E�fE�fE6fE1�E�fE� E8 E@ EњE��E�3E��E��E �fE"FfE#Y�E$l�E& E'fE(>fE)^fE+	�E,( E-H E.c3E03E16fE2FfE3Q�E4� E5�3E7^fE8X E9��E;!�E<3E?4�EB�fEE�fEH�fEK�3EN�3ER EUd�EX;3E[~fE^��Ea��Ed�3Eh Ej��EnD�EqS3Etq�Ew�3Ez�3E}�fE�_3E�fE���E��E�� E�6fE�� E�ffE�ݚE���E� �E�RfE���E�� E�G3E�� E��fE�?3E���E��fE�, E�{3E��fE�fE�X�E��fE� �E�I�E���E���E�A�E���E��3E�(�E���E�ٚE� E�rfE��fE�fE�a�E�� E���>���>���>L��>���>���>���>L��=���>���>���>���>���>L��>���>���>���>L��>L��>���>���>L��>L��>���=���>���=���>���>���>L��>���>L��>���>���>���?   ?   ?��?333?�  ?�  ?�ff?�  ?�ff?�33@��@   @,��@Fff@`  @s33@�33@�  @���@���@���@�ff@�33@�ff@�ffA   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111441444414444144441444141414414114141114111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             ?L��?�  @&ff@fff@�33@�33@�33@���AffAffA,��AL��Ak33A���A���A�  A�  A�  A���A噚A���B  B
  B��B  B"ffB*  B2  B:��BBffBJ��BR��BZ  BbffBj��BrffBz  B�33B�33B�33B�ffB�  B�33B�ffB�  B�  B�  B�33B�  B�33B�  B�  B�ffB�ffB�  B�33B�ffB���B���B�ffB�ffB���B���B�ffB홚B�  B���B���B���C � C� C��C��C��C
� C� C� C��C��C�3C�3C��C��C� C� C L�C"�3C$��C&��C(� C*� C,� C.ffC0��C2��C4� C6��C8�3C:��C<ffC>L�C@��CB�3CD�3CF� CH�3CJ��CL�3CN��CP� CRL�CTffCV� CX�3CZ� C\L�C^��C`�3Cb��Cd� Cf��Ch��CjffCl�3Cn��CpffCr�3Ct��Cv�3Cx� Cz��C|��C~��C�L�C�@ C�ffC�ffC�Y�C�L�C�Y�C�L�C�33C�ffC�Y�C�@ C�33C�&fC�L�C�@ C�&fC�L�C�@ C�&fC�@ C�Y�C�@ C�33C�L�C�ffC�ffC�@ C�ffC�Y�C�@ C�@ C�&fC�@ C�Y�C�@ C�33C�@ C�ffC�L�C�@ C�Y�C�ffC�L�C�33C�@ C�L�C�@ C�&fC�33C�@ C�Y�C�@ C��C�33C�33C�@ C�@ C�L�C�ffC�L�C�33C�@ C�L�C�Y�C�ffC�ffC�L�C�33C�@ C�L�C�L�C�Y�C�ffC�ffC�s3C�Y�C�33C�@ C�Y�C�ffC�L�C�33C�@ C�Y�C�Y�C�ffC�Y�C�33C�L�C�ffC�L�C�33C�@ C�Y�C�@ C�&fC�@ C�Y�C�ffC�L�C�33C�@ C�L�C�Y�C�@ C�33C�L�C�@ C�&fC�@ C�33C�&fC�@ C�ffC�L�C�33C�L�C�ffC�@ C�33C�L�C�@ C�&fC�L�DffD�D� DffD�D�3D` D�D�3Ds3D,�D �fD#� D&� D)L�D,�D.� D1��D4Y�D7�D9ٚD<� D?&fDA� DDL�DF��DIs3DLfDNy�DP�fDSY�DU�fDXFfDZ��D]&fD_� Db�Ddy�Dg  Di�3Dl�Dn��DqS3Ds�3Dv��Dy�D{L�D}� D�C3D�� D�3D�c3D�� D�#3D�� D���D�@ D��fD���D�\�D���D�fD�s3D��3D�,�D�y�D��3D��D�S3D��fD�� D��fD�6fD�p D�� D��fD�3D�C3D�s3D���D��3D���D�,�D�c3D���D���D���D�)�D�` D�� D�� D�3D�P D���D�� D��D�@ D�s3D�� D�� D���D�)�D�L�D�y�DƦfD��fD���D� D�33D�S3D�p D΃3Dϩ�Dй�D��3D���D���D�3D�0 D�FfD�ffDك3Dڣ3D��3D�� D�  D�#3D�@ D�Y�D�y�D��D� D�ɚD��fD�3D�)�D�FfD�l�D��D��fD�� D� D�<�D�i�D�D��3D��fD�3D�,�D�VfD�vfD�y�D���D���D�� D���E �fE�E��E>fE��EVfE�3Eq�E��E�fE�E� E  E��E&fE	1�E
33E��E��E@ ED�E�fE�fE>fE9�E�fE� E@ EH EٚE��E�3E��E��E �fE"NfE#a�E$t�E& E'&fE(FfE)ffE+�E,0 E-P E.k3E03E1>fE2NfE3Y�E4� E5�3E7ffE8` E9ɚE;)�E<3E?<�EB�fEE�fEH�fEK�3EN�3ER  EUl�EXC3E[�fE^��Ea��Ed�3Eh Ek�EnL�Eq[3Ety�Ew�3Ez�3E}�fE�c3E�fE���E��E�� E�:fE�� E�jfE��E���E��E�VfE���E�� E�K3E�� E��fE�C3E���E��fE�0 E�3E��fE�fE�\�E��fE��E�M�E���E���E�E�E���E��3E�,�E���E�ݚE� E�vfE��fE�fE�e�E�� E���G�O�G�O�?333G�O�G�O�G�O�G�O�?��G�O�G�O�G�O�G�O�?333G�O�G�O�G�O�G�O�?333G�O�G�O�G�O�?333G�O�?��G�O�?��G�O�G�O�?333G�O�?333?L��G�O�?fffG�O�?�  ?���?���G�O�?�  ?�ff@   @33@��@,��@@  @L��@fff@�  @���@�33@�  @���@���@���@�ff@�33@�ffA33A  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111441444414444144441444141414414114141114111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             @ @ �@ V@ *@ �@ "�@ )�@ /@ 5�@ <�@ FQ@ S�@ `�@ n�@ {�@ ��@ �0@ ��@ �-@ ��@ �|@ �t@ �@ �@j@�@�@,`@;d@H]@V�@dZ@p�@~�@�P@�H@�A@��@�>@��@�;@�@��@1@{@""@/�@>@K@Yn@ff@t@�@��@�@�Y@��@�J@��@�@�L@�9@	�@B@'�@33@@,@M�@[z@i�@ww@��@�u@�@�@��@�c@�@�@�@^@V@�@(�@6�@B�@SI@`B@m�@z�@��@�0@�(@�-@��@��@܀@�y@�q@�@�@g@-�@;d@G�@V�@e	@r@~�@��@�<@��@��@��@�7@܀@�4@��@�@{@"�@0x@<�@Lu@Yn@e�@uk@��@��@�@��@�^@ƨ@�O@�H@��@��@�@�@&�@3�@@,@P�@]�@i�@v�@��@�u@�m@��@�k@�c@խ@�@�@  @�@�@+@8�@DD@S�@`�@m:@z�@�+@�0@�5@�~@�w@��@܀@��@��@	�@	@	g@	+�@	:@	H]@	UU@	a�@	o�@	~K@	�P@	��@	�5@	�9@	��@	�7@	��@	�4@	�9@
�@
�@
""@
0x@
>�@
M$@
Z�@
g@
s_@
��@
�@
��@
�@
�^@
�@
�[@
�@
�@@
��@�@�@&;@2�@@�@O�@]�@k�@x�@�p@�u@�y@��@�@�c@�h@�@��@  @@[@)�@5�@DD@R�@`�@m:@z3@�7@�0@�y@�~@�w@�o@�t@�(@�q@�@�@ �@,`@9X@H]@UU@a�@qS@ �@I@��@�#@#�@k�@��@��@FQ@�h@��@&;@t@�&@�@X@��@�@@9X@�@ψ@�@`A@�A@��@4�@y�@��@�@D�@��@�@V@O�@�u@�
@B@[z@�m@�@+�@r@�^@@I@��@��@@Yn@��@�@:�@�@��@O@e�@�~@��@FQ@�@�/@&�@qS@�j@ %@ M$@ �u@ ��@!g@!`�@!��@!�@"*S@"m:@"�~@"�@#3�@#t�@#��@#�@$3�@$v�@$��@$��@%<@%}�@%��@%��@&@�@&�@&��@'
�@'N�@'�@'�
@(�@(\)@(��@(��@)�@)^�@)�@)�/@*[@*]�@*��@*��@+�@+V@+�u@+�7@,
�@,I�@,��@,��@,��@-5�@-qS@-�@-�y@.&�@.c�@.�@.ލ@/O@/X�@/��@/Ӡ@0�@0M$@0�D@0��@1@1>�@1{�@1�^@1� @25�@2s_@2��@2�Y@333@3s_@3��@3��@40x@4n�@4��@4�@@5-�@5k.@5�z@5��@6[@6\)@6��@6�[@7@7R�@7��@7�o@8%@8B8@8~�@8�^@8�@90x@9i�@9��@9܀@:�@:��@:�@;�I@<�@<�@=[@=��@>/@>ψ@?:�@?��@@N�@@�Y@Ab�@BV@B�W@B�@C��@D�@D��@E<�@E�-@F'�@F�
@GM�@Gȴ@HC�@H��@It@I��@Jg�@K @K�T@Lb@L�d@M,`@M��@N<�@N�A@OA�@O׹@P;d@Q��@S�@T[z@U��@V� @XI@Y��@[J@\B8@]��@^��@`S�@a��@c  @dB�@e�M@f� @hK�@i��@j�@l<�@m�C@n��@pB8@q�P@r�@t<@u��@v�e@x4�@y�I@z��@{'�@{k.@{�~@{�}@|@�@|�+@|�@}�@}Wa@}�0@}��@~�@~Z�@~��@~�'@&;@dZ@��@��@��@�:�@�`�@�~�@���@��@��@�C@�)�@�R�@�qS@���@���G�O�G�O�@ ^G�O�G�O�G�O�G�O�@  �G�O�G�O�G�O�G�O�@ ^G�O�G�O�G�O�G�O�@ ^G�O�G�O�G�O�@ ^G�O�@  �G�O�@  �G�O�G�O�@ ^G�O�@ ^@ G�O�@ �G�O�@ j@ @ �G�O�@ �@ �@ 
=@ J@ �@ @ @ o@ *@ �@ �@ �@ �@ !s@ $.@ (G@ *S@ -@ 1'@ 4�@ 6�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A��yA���A�A�%A�
=A�A�A�A�%A�%A�
=A�JA�VA�JA��^A��uA�|�A�hsA�^5A�VA�M�A�?}A�9XA�(�A��A��A��A�"�A�(�A�/A�;dA�v�A�O�A���A��A���A��\A�&�A�Q�A��FA�5?A�9XA��yA���A���A�A��FA��PA��A�|�A�hsA�K�A�O�A�O�A�K�A�"�A��A���A�A��-A��uA��A�x�A�VA��A���A���A�G�A���A�?}A��\A��A�?}A���A��A���A�ȴA��jA��A��A���A�=qA��yA�VA�oA��A�&�A�ZA���A�t�A��-A�ȴA���A��A�(�A��^A�;dA�n�A�hsA�v�A��yA�I�A�`BA��uA���A�$�A���A���A�  A�=qA�p�A�~�A��A�"�A��FA�l�A~��Av��Aq|�An�Am�AlVAk�mAi;dAfbAb��AXȴAR��AR1AP  ANz�AM��AL{AKoAI�AG�hAF�AF�uAE��AD�AD�AC��ACVAA\)A@ZA?�A?�A?"�A>��A=O�A8ĜA7�A5�A2�9A/�;A/�-A.�uA-��A-p�A-`BA,��A+��A+�FA+G�A*�A)"�A'�;A&�+A%�A#�
A"^5A!�A�
AdZA�\A��AZAoA�TAhsAĜA7LAM�A�#A+AbNA+AS�A��A��A��A�PA�PA�At�AK�A��A5?A�A
�A
��A
-AG�AA�A�FA�A�HA�!AA�A  A|�AVA
=A �A �9A ��@���@�^5@��@�$�@�x�@��`@�9X@�@�\)@��@��@�r�@���@���@���@�r�@��@�Ĝ@�x�@���@䛦@㝲@��@⟾@�n�@��T@��@��/@��/@�9@��;@߅@���@�/@�|�@ڟ�@٩�@�%@���@�Z@�ƨ@��@�dZ@�`B@̃@���@�t�@�@��T@�1@��@��P@���@�r�@��@�O�@�$�@�/@�X@�Q�@�l�@���@���@�"�@��@�$�@�ƨ@���@�-@�G�@���@���@���@���@��-@�;d@�ff@�1'@��!@���@��@�5?@�O�@�bN@�C�@���@��^@�`B@��@+@~E�@|Z@z�@x1'@vff@v$�@u�T@t��@t1@p�9@l�@k�@g��@fv�@e��@c"�@`1'@^ff@\�D@X�9@Vff@S�F@RM�@R�@R-@O|�@M�@L�@J�!@H�`@G�@GK�@E�@D�/@B��@A�@?�@?+@=�-@<��@;33@9�@9��@7K�@6��@6�y@6��@6@4�/@3"�@2�\@1�^@17L@1�@0�9@/+@-�@-/@+S�@*�!@)��@(A�@'�@&ȴ@%�h@$��@"M�@!hs@ b@��@�y@�h@�D@I�@�
@�!@�!@��@�7@x�@r�@�;@��@ff@5?@�-@�@��@��@dZ@�\@��@�@  @K�@�y@E�@�@(�@�F@�@
��@	hs@	X@	G�@	&�@��@��@�@@��@�/@�@t�@~�@M�@�@�#@��@�7@ ��?��?�I�?�dZ?�X?���?�`B?�S�?�-?�  ?�V?���?�1'?�`B?�`B?�z�?��?�bN?ޗ�?�V?�p�?ݑh?���?�=q?�1'?�+?֧�?ա�?�?ԛ�?�33?�o?�o?�G�?�Ĝ?� �?��;?��?Ͳ-?�p�?̬?�(�?��m?�?��H?���?�r�?�
=?Ƈ+?ě�?�%?�\)?�\)?�V?��h?��?�j?�C�?���?���?�=q?���?�~�?���?�C�?��H?�dZ?�ƨ?�(�?��?��?�O�?�{?��R?�\)?� �?���?�Ĝ?��`?�%?�&�?�G�?�hs?��7?���?��?��?�-?�-?�-?�M�?�M�?�n�?\?���?��?�o?�33?�S�?�t�?�t�?Õ�?��
?���?��?�Z?�z�?ě�?ļjA��yA��A��A��mA��yA��yA��A��A��A��A��A��A��mA��`A��TA��HA��TA��HA��TA��`A��TA��TA��mA��TA��mA��A��A��A��A���A��A��A��A��A��A���A���A���A���A���A���A�  A�A�  A�A�A�A�A�
=A�
=A�
=A�
=A�A�  A�A�A�A�  A�A�1G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             A��yA���A�A�%A�
=A�A�A�A�%A�%A�
=A�JA�VA�JA��^A��uA�|�A�hsA�^5A�VA�M�A�?}A�9XA�(�A��A��A��A�"�A�(�A�/A�;dA�v�A�O�A���A��A���A��\A�&�A�Q�A��FA�5?A�9XA��yA���A���A�A��FA��PA��A�|�A�hsA�K�A�O�A�O�A�K�A�"�A��A���A�A��-A��uA��A�x�A�VA��A���A���A�G�A���A�?}A��\A��A�?}A���A��A���A�ȴA��jA��A��A���A�=qA��yA�VA�oA��A�&�A�ZA���A�t�A��-A�ȴA���A��A�(�A��^A�;dA�n�A�hsA�v�A��yA�I�A�`BA��uA���A�$�A���A���A�  A�=qA�p�A�~�A��A�"�A��FA�l�A~��Av��Aq|�An�Am�AlVAk�mAi;dAfbAb��AXȴAR��AR1AP  ANz�AM��AL{AKoAI�AG�hAF�AF�uAE��AD�AD�AC��ACVAA\)A@ZA?�A?�A?"�A>��A=O�A8ĜA7�A5�A2�9A/�;A/�-A.�uA-��A-p�A-`BA,��A+��A+�FA+G�A*�A)"�A'�;A&�+A%�A#�
A"^5A!�A�
AdZA�\A��AZAoA�TAhsAĜA7LAM�A�#A+AbNA+AS�A��A��A��A�PA�PA�At�AK�A��A5?A�A
�A
��A
-AG�AA�A�FA�A�HA�!AA�A  A|�AVA
=A �A �9A ��@���@�^5@��@�$�@�x�@��`@�9X@�@�\)@��@��@�r�@���@���@���@�r�@��@�Ĝ@�x�@���@䛦@㝲@��@⟾@�n�@��T@��@��/@��/@�9@��;@߅@���@�/@�|�@ڟ�@٩�@�%@���@�Z@�ƨ@��@�dZ@�`B@̃@���@�t�@�@��T@�1@��@��P@���@�r�@��@�O�@�$�@�/@�X@�Q�@�l�@���@���@�"�@��@�$�@�ƨ@���@�-@�G�@���@���@���@���@��-@�;d@�ff@�1'@��!@���@��@�5?@�O�@�bN@�C�@���@��^@�`B@��@+@~E�@|Z@z�@x1'@vff@v$�@u�T@t��@t1@p�9@l�@k�@g��@fv�@e��@c"�@`1'@^ff@\�D@X�9@Vff@S�F@RM�@R�@R-@O|�@M�@L�@J�!@H�`@G�@GK�@E�@D�/@B��@A�@?�@?+@=�-@<��@;33@9�@9��@7K�@6��@6�y@6��@6@4�/@3"�@2�\@1�^@17L@1�@0�9@/+@-�@-/@+S�@*�!@)��@(A�@'�@&ȴ@%�h@$��@"M�@!hs@ b@��@�y@�h@�D@I�@�
@�!@�!@��@�7@x�@r�@�;@��@ff@5?@�-@�@��@��@dZ@�\@��@�@  @K�@�y@E�@�@(�@�F@�@
��@	hs@	X@	G�@	&�@��@��@�@@��@�/@�@t�@~�@M�@�@�#@��@�7@ ��?��?�I�?�dZ?�X?���?�`B?�S�?�-?�  ?�V?���?�1'?�`B?�`B?�z�?��?�bN?ޗ�?�V?�p�?ݑh?���?�=q?�1'?�+?֧�?ա�?�?ԛ�?�33?�o?�o?�G�?�Ĝ?� �?��;?��?Ͳ-?�p�?̬?�(�?��m?�?��H?���?�r�?�
=?Ƈ+?ě�?�%?�\)?�\)?�V?��h?��?�j?�C�?���?���?�=q?���?�~�?���?�C�?��H?�dZ?�ƨ?�(�?��?��?�O�?�{?��R?�\)?� �?���?�Ĝ?��`?�%?�&�?�G�?�hs?��7?���?��?��?�-?�-?�-?�M�?�M�?�n�?\?���?��?�o?�33?�S�?�t�?�t�?Õ�?��
?���?��?�Z?�z�?ě�?ļjA��yA��A��A��mA��yA��yA��A��A��A��A��A��A��mA��`A��TA��HA��TA��HA��TA��`A��TA��TA��mA��TA��mA��A��A��A��A���A��A��A��A��A��A���A���A���A���A���A���A�  A�A�  A�A�A�A�A�
=A�
=A�
=A�
=A�A�  A�A�A�A�  A�A�1G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�BA�B@�B@�B@�B?}B@�B@�B@�B@�B@�B@�B@�B@�B?}BB�BD�BG�BI�BJ�BM�BO�BQ�BR�BVBYB]/B`BBaHBbNBcTBiyB�uBȴB�`B�B�B��BB"�B�BhB�B�B�B#�B2-B5?B2-B5?B9XB6FB6FB<jBA�BE�BJ�BM�BO�BS�BVB_;BbNBe`BgmBk�Bo�Bp�Bo�BiyBffBbNB]/BW
BQ�BR�BR�BdZB_;B^5BYBT�BR�BO�BD�B:^B1'B-B#�B!�B�BbBB��B�fB�BB�B�qB�B��B�bB�7B|�Be`BYBL�BE�B=qB(�B�B
��B
�NB
��B
VB
?}B
/B
2-B
�B	�yB	ĜB	�!B	��B	��B	�oB	u�B	dZB	9XB��B�TB�
B��BȴBÖB�^B�LB�3B�3B�!B�B��B��B��B��B��B��B��B�{B�uB�hB�\B�Bw�Bt�Bp�B`BBcTB_;B\)B[#B[#BZBXBW
BW
BT�BR�BQ�BO�BQ�BQ�BO�BN�BM�BM�BK�BH�BH�BC�BF�BF�BD�BB�B@�BA�B?}B=qB=qB>wB=qB>wB<jB?}B=qB<jB<jB<jB;dB=qB=qB=qB;dB:^B7LB1'B.B+B+B+B)�B(�B(�B'�B%�B'�B(�B)�B(�B&�B$�B"�B#�B"�B"�B"�B#�B#�B#�B.B49B33B33B/B.B,B'�B+B(�B(�B,B0!B2-B33B6FB:^B<jB>wB?}BF�BJ�BK�BN�BP�BP�BS�BaHBl�Bp�Bt�Bw�B�hB�BÖB�ZB��B	{B	+B	1'B	7LB	;dB	K�B	gmB	m�B	k�B	r�B	n�B	l�B	t�B	t�B	�B	��B	��B	�!B	�RB	�}B	ǮB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�BB	�NB	�fB	�yB	�B	�B	��B	��B	��B	��B	��B
  B
B
B
1B
1B
1B

=B
JB
hB
�B
�B
�B
�B
�B
�B
"�B
#�B
%�B
+B
,B
0!B
0!B
0!B
0!B
33B
49B
6FB
7LB
8RB
9XB
9XB
;dB
<jB
>wB
@�B
@�B
A�B
B�B
C�B
F�B
G�B
F�B
I�B
I�B
H�B
I�B
K�B
L�B
M�B
M�B
N�B
N�B
O�B
O�B
Q�B
Q�B
S�B
VB
VB
W
B
XB
XB
ZB
[#B
\)B
_;B
`BB
aHB
aHB
aHB
cTB
cTB
dZB
dZB
e`B
e`B
e`B
gmB
gmB
hsB
hsB
iyB
iyB
iyB
jB
k�B
k�B
k�B
m�B
m�B
o�B
o�B
o�B
p�B
q�B
q�B
r�B
t�B
t�B
t�B
u�B
w�B
v�B
v�B
v�B
w�B
x�B
y�B
y�B
{�B
z�B
z�B
}�B
~�B
}�B
}�B
~�B
~�B
~�B
�B
�B
�B
�B
�+B
�%B
�+B
�1B
�1B
�JB
�JB
�VB
�VB
�bB
�hB
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
�B
�B
�B
�!B
�'B
�-B
�-B
�9B
�9B
�?B
�FB
�LB
�RB
�RB
�RB
�RB
�XB
�RB
�^B
�XB
�^B
�^B
�^B
�dB
�dB
�dB
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
�dB
�dB
�dB
�dB
�dB
�dB
�^B
�dB
�dB
�dB
�dB
�jB
�dB
�dB
�jB
�dB
�dB
�dB
�dB
�dB@�B@�BA�BA�BA�BA�BA�BB�BA�B@�BA�BA�B@�BA�BA�BA�BA�BB�BA�BA�BA�BA�B@�B@�BA�B?}B@�BA�B@�B?}BA�BB�B@�BA�B@�B@�B@�B@�B@�B@�B@�B@�B@�B@�B?}B@�B@�B@�B?}B?}B@�B?}B?}B@�B@�B@�B@�B@�B@�B?}G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             BA^B@YB@YB@YB?SB@ZB@ZB@ZB@[B@[B@[B@\B@]B?WBBjBDwBG�BI�BJ�BM�BO�BQ�BR�BU�BX�B]B`$Ba*Bb1Bc8Bi]B�ZBșB�FB�B��B��B�B"�BvBRB~BrByB#�B2B5,B2B5.B9GB66B66B<[BA{BE�BJ�BM�BO�BS�BU�B_1BbDBeWBgeBk}Bo�Bp�Bo�BitBfaBbJB]+BWBQ�BR�BR�BdXB_:B^5BYBT�BR�BO�BD�B:`B1*B-B#�B!�B�BgB%B��B�mB�IB�B�yB�B��B�kB�AB|�BekBY"BL�BE�B=~B)B�B
��B
�\B
��B
VB
?�B
/*B
2<B
�B	�B	ĬB	�1B	��B	��B	��B	u�B	dlB	9kB��B�fB�B��B��BêB�sB�aB�IB�IB�8B�B�B��B��B��B��B��B��B��B��B��B�yB�)Bw�Bt�Bp�B`aBcsB_ZB\IB[CB[DBZ?BX2BW-BW-BU"BSBRBPBRBRBPBO BM�BM�BK�BH�BH�BC�BF�BF�BD�BB�B@�BA�B?�B=�B=�B>�B=�B>�B<�B?�B=�B<�B<�B<�B;�B=�B=�B=�B;�B:�B7�B1^B.KB+:B+;B+;B*6B)0B)1B(+B&B(,B)3B*9B)4B''B%B#B$B#B#B#B$B$B$B.WB4}B3xB3xB/aB.ZB,OB(7B+JB)>B)?B,QB0kB2wB3~B6�B:�B<�B>�B?�BF�BKBLBO)BQ6BQ6BTJBa�Bl�Bp�BuBx$B��B�lB��B�B�*B	�B	+oB	1�B	7�B	;�B	L@B	g�B	nB	lB	s6B	o!B	mB	uLB	uOB	��B	�qB	��B	��B	��B	�#B	�WB	�B	ЎB	ҞB	њB	΋B	ҦB	ѢB	ϙB	͐B	ΙB	ӻB	��B	�B	�B	�:B	�QB	�lB	�B	��B	��B	��B	��B	��B
 �B
�B
B
	+B
	.B
	1B
AB
QB
rB
�B
�B
�B
�B
�B
�B
#�B
$�B
'
B
,,B
-5B
1QB
1TB
1WB
1ZB
4oB
5xB
7�B
8�B
9�B
:�B
:�B
<�B
=�B
?�B
A�B
A�B
B�B
C�B
D�B
HB
IB
HB
K)B
K,B
J)B
K2B
MBB
NKB
OTB
OWB
P`B
PbB
QkB
QnB
S~B
S�B
U�B
W�B
W�B
X�B
Y�B
Y�B
[�B
\�B
]�B
`�B
a�B
b�B
b�B
cB
eB
eB
fB
fB
g'B
g*B
g-B
i<B
i?B
jHB
jJB
kSB
kVB
kXB
laB
mjB
mmB
moB
o~B
o�B
q�B
q�B
q�B
r�B
s�B
s�B
t�B
v�B
v�B
v�B
w�B
y�B
x�B
x�B
x�B
y�B
z�B
{�B
{�B
~B
}
B
}B
�#B
�+B
�(B
�+B
�3B
�6B
�9B
�MB
�]B
�eB
�jB
�{B
�|B
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
�B
�(B
�9B
�8B
�MB
�RB
�cB
�qB
�|B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
�B
�B
�B
�!B
�'B
�.B
�3B
�;B
�XB
�tB
��B
��B
��B
��B
��B
��B
�B
�%B
�:B
�OB
�fB
�zB
��B
��B
��B
��B
��B
��B
��B
�B
�B
�B
�4B
�DB
�RB
�]B
�qB
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
��B@XB@XBA^BA^BA^BA^BA^BBdBA^B@XBA^BA^B@XBA^BA^BA^BA^BBdBA^BA^BA^BA^B@XB@XBA^B?RB@XBA^B@XB?RBA^BBdB@XBA^B@XB@XB@XB@XB@YB@YB@YB@YB@YB@YB?SB@YB@YB@YB?SB?SB@YB?TB?TB@ZB@ZB@ZB@ZB@ZB@[B?UG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�201903211900302021061413560920210614135609202106171314192021061713141920210617131419201903211900302021061413560920210614135609202106171314192021061713141920210617131419PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            new conductivity = original conductivity * (1 + delta*TEMP + CPcor_SBE*PRES) / (1 + delta*TEMP_ADJUSTED + CPcor_new*PRES_ADJUSTED)                                                                                                                              none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            new conductivity = original conductivity * (1 + delta*TEMP + CPcor_SBE*PRES) / (1 + delta*TEMP_ADJUSTED + CPcor_new*PRES_ADJUSTED)                                                                                                                              none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.50 dbar                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            CPcor_new = -1.25e-07; CPcor_SBE = -9.57e-08; delta = 3.25e-06                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.50 dbar                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            CPcor_new = -1.25e-07; CPcor_SBE = -9.57e-08; delta = 3.25e-06                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                      none                                                                                                                                                                                                                                                            New conductivity computed by using a different CPcor value from that provided by Sea-Bird.                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            No significant salinity drift detected.                                                                                                                                                                                                                         Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                      none                                                                                                                                                                                                                                                            New conductivity computed by using a different CPcor value from that provided by Sea-Bird.                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            No significant salinity drift detected.                                                                                                                                                                                                                         PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            AO  AO  ARCAARCAADJPADJP                                                                                                                                        2019032119003020190321190030  IP  IP                                G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2019032119003020190321190030QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�5F03E           703E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2019032119003020190321190030QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�0               0               PM  PM  ARSQARSQCOWGCOWGV1.1V1.1                                                                                                                                2021061713151620210617131516IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                