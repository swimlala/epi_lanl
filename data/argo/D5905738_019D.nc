CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-07-24T22:02:38Z creation      
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
resolution        :�o     �  sX   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �0   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �(   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �    TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �x   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �p   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                   �   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                   �   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                   �   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                   �   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  � �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                   P   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                   l   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    t   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar        �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar        �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�       �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  �H   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  � �      � �Argo profile    3.1 1.2 19500101000000  20180724220238  20210722160150  5905738 5905738 US ARGO PROJECT                                                 US ARGO PROJECT                                                 GREGORY C. JOHNSON                                              GREGORY C. JOHNSON                                              PRES            TEMP            PSAL            PRES            TEMP            PSAL                  DD  AOAO7218                            7218                            2C  2C  DD  SOLO_D_MRV                      SOLO_D_MRV                      12001                           12001                           SBE603 01Dec16                  SBE603 01Dec16                  874 874 @�q�`��z@�q�`��z11  @�q�UUh @�q�UUh @6��~��M@6��~��M�c��a���c��a��11  GPS     GPS     Primary sampling: averaged [1Hz sampling by SBE-41CP averaged in 2-dbar bins]                                                                                                                                                                                   Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                              	   	AB  AA  AA  >���?fff@   @Fff@�ff@�ff@�  @�  A��A33A#33AA��Aa��A~ffA�  A���A���A���Aљ�A���A���B   BffBffB  B ffB(��B0ffB8ffB@ffBH  BP  BX��B`ffBg��Bp  BxffB�33B�33B�33B�33B�33B�  B�33B�33B�33B�  B�33B�  B�ffB�  B���B�33B�33B�  B�  B�  B���B�33B�33B���B�33B�33B���B�33B�B�ffB�  B�ffC 33C�fC�CL�CL�C
  C��C��C  C33CL�CL�C33C  C33C�C��C"�C$  C%�fC(�C*�C+�fC.L�C0L�C233C4L�C6�C8�C:�C<33C>�C@�CB�CD  CF  CH  CJ  CL  CN  CP  CQ��CT33CV�CX  CZ  C[��C^33C`�Ca��Cc��Cf�Ch  Ci�fCl33Cn�Cp  CrL�Ct33Cv�Cw�fCy��C|�C~  C�fC�&fC��C�  C��C�&fC��C��C�&fC�&fC��C�  C��3C��C��C��3C��C�33C��C�  C��3C��fC��3C��C�  C��fC�  C��C��C��3C��C�&fC��C��3C�  C��C�&fC��C��fC��C��C�&fC��C��3C�  C��C��C�ٚC��3C�  C�  C��C��C��C�  C��fC��fC��3C��3C�  C�  C�  C��C��C��C�ٚC��fC��fC��C��C��fC��fC��3C��3C�  C��C��C�  C��fC�  C�  C��fC�  C��C�  C��3C��C��C�  C�  C�  C��C��C�  C��C�  C�  C�  C�  C�  C�  C��C��C��C��C��fC��3C��3C��3C��C�&fC��3C��3C��3C��fC�  C��3C�  C��3C��C�&fC��C��C��C��fC��fD�3D��D	,�D�3D�Dy�D��D  Ds3D�3D@ D��D!9�D#��D&l�D)�D+��D.S3D1fD3��D6` D9�D;��D>� DA&fDC�3DF` DH��DK��DN,�DP�fDS,�DU��DX3DZ� D\�3D_ffDa��Dd,�Df��Dh�fDk9�Dm��DoٚDr�Dt` Dv��Dy�D{` D}FfD��D��D�3D�9�D�p D��fD�� D� D�@ D�i�D���D��3D��3D�&fD�\�D��3D���D�  D�<�D�|�D���D�� D�,�D�\�D�� D��fD��3D�)�D�c3D�� D�ٚD�fD�VfD���D�� D�3D�I�D���D��fD�	�D�L�D���D�� D�	�D�C3D�� D�� D��fD�6fD�l�D��fD���D���D�0 D�c3D�D�ɚD���D�&fD�S3DȀ Dɠ D��3D���D�3D�C3D�\�D�s3DіfDҬ�D��3D���D��fD�	�D�#3D�C3D�c3D�|�D܌�Dݙ�Dެ�D߹�D���D�ٚD�� D���D���D�fD��D�&fD�6fD�C3D�VfD�l�D�fD� D�fD��D�� D��3D���D��D��D�0 D�I�D�\�D�p D�c3D�s3D�� D��3D��fE a�E ��Ey�E�fE��E�E��E3E�3E E�3E�fE)�E	0 E
�fE��EH ENfEٚE�fE�3Eq�Ep E�E�3ES3E��E�fE  E` E�3E�3E! E"t�E#` E$�fE&)�E'�fE(vfE)� E+<�E,.fE-��E/ E0	�E1{3E2t�E3�3E4� E6I�E7��E8�3E:fE;fE<�3E?ٚEB�fEE� EI;3ELNfEO{3ER� EU��EX�3E[��E^њEb Ee	�Eh8 Ekd�EnT�Eq� Et�3Ew�3E{�E~33E���E�< E���E�T E��3E�jfE��3E���E�H E���?   >���>���?��?��?��?333?��?333?333?333?L��?333?L��?L��?L��?L��?�  ?fff?���?���?���?�ff?�33?���?ٙ�@   @ff@��@&ff@9��@L��@`  @s33@�ff@�  @���@���@���@�33@�33@���@���@���A   A  A  A��A33A#33A)��A1��A8  A>ffAD��AK33AP  AX  A^ffAfffG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111441441414414144414141111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                 ?fff?�33@   @fff@�ff@�ff@�  @�  A	��A33A+33AI��Ai��A�33A�  A���A���Ař�Aՙ�A���A���B  B
ffBffB  B"ffB*��B2ffB:ffBBffBJ  BR  BZ��BbffBi��Br  BzffB�33B�33B�33B�33B�33B�  B�33B�33B�33B�  B�33B�  B�ffB�  B���B�33B�33B�  B�  B�  B���B�33B�33B���B�33B�33B���B�33B�B�ffB�  B�ffC �3CffC��C��C��C
� CL�CL�C� C�3C��C��C�3C� C�3C��C L�C"��C$� C&ffC(��C*��C,ffC.��C0��C2�3C4��C6��C8��C:��C<�3C>��C@��CB��CD� CF� CH� CJ� CL� CN� CP� CRL�CT�3CV��CX� CZ� C\L�C^�3C`��CbL�CdL�Cf��Ch� CjffCl�3Cn��Cp� Cr��Ct�3Cv��CxffCzL�C|��C~� C�33C�ffC�L�C�@ C�Y�C�ffC�Y�C�L�C�ffC�ffC�Y�C�@ C�33C�L�C�L�C�33C�L�C�s3C�Y�C�@ C�33C�&fC�33C�L�C�@ C�&fC�@ C�Y�C�L�C�33C�L�C�ffC�L�C�33C�@ C�Y�C�ffC�L�C�&fC�L�C�Y�C�ffC�L�C�33C�@ C�Y�C�L�C��C�33C�@ C�@ C�L�C�L�C�Y�C�@ C�&fC�&fC�33C�33C�@ C�@ C�@ C�L�C�Y�C�L�C��C�&fC�&fC�L�C�L�C�&fC�&fC�33C�33C�@ C�L�C�Y�C�@ C�&fC�@ C�@ C�&fC�@ C�Y�C�@ C�33C�L�C�L�C�@ C�@ C�@ C�Y�C�Y�C�@ C�L�C�@ C�@ C�@ C�@ C�@ C�@ C�L�C�L�C�Y�C�Y�C�&fC�33C�33C�33C�Y�C�ffC�33C�33C�33C�&fC�@ C�33C�@ C�33C�Y�C�ffC�Y�C�Y�C�Y�C�&fC��fD3D��D	L�D�3D,�D��D��D@ D�3D�3D` D��D!Y�D#��D&��D),�D+��D.s3D1&fD3��D6� D9,�D;��D>� DAFfDC�3DF� DI�DK��DNL�DP�fDSL�DU��DX33DZ� D]3D_�fDa��DdL�Df��DifDkY�Dm��Do��Dr9�Dt� DvٚDy9�D{� D}ffD��D���D�#3D�I�D�� D��fD�� D�  D�P D�y�D���D��3D�3D�6fD�l�D��3D���D� D�L�D���D�ɚD�  D�<�D�l�D�� D��fD�3D�9�D�s3D�� D��D�&fD�ffD���D�� D�#3D�Y�D���D��fD��D�\�D���D�� D��D�S3D�� D�� D�fD�FfD�|�D��fD���D�	�D�@ D�s3D©�D�ٚD�	�D�6fD�c3DȐ Dɰ D��3D���D�#3D�S3D�l�DЃ3DѦfDҼ�D��3D���D�fD��D�33D�S3D�s3Dی�Dܜ�Dݩ�D޼�D�ɚD���D��D�� D���D�	�D�fD�)�D�6fD�FfD�S3D�ffD�|�D�fD� D�fD���D�� D��3D�	�D��D�,�D�@ D�Y�D�l�D�� D�s3D��3D�� D��3D��fE i�E ��E��EfE��E�E��E3E�3E  E�3E�fE1�E	8 E
�fE��EP EVfE�E�fE�3Ey�Ex E��E�3E[3E��E�fE Eh E�3E�3E! E"|�E#h E$�fE&1�E'�fE(~fE)� E+D�E,6fE-��E/ E0�E1�3E2|�E3�3E4� E6Q�E7��E8�3E:&fE;&fE<�3E?�EB�fEE� EIC3ELVfEO�3ER� EU��EX�3E[��E^ٚEb  Ee�Eh@ Ekl�En\�Eq� Et�3Ew�3E{�E~;3E���E�@ E���E�X E��3E�nfE��3E���E�L E���G�O�G�O�?fffG�O�G�O�?���G�O�?���G�O�G�O�?���G�O�?���G�O�G�O�G�O�?�ffG�O�?�33G�O�?���?ٙ�?�ff?�33@ff@��@   @&ff@9��@Fff@Y��@l��@�  @���@�ff@�  @���@���@ə�@�33@�33@���@���A��A  A  A  A��A#33A+33A1��A9��A@  AFffAL��AS33AX  A`  AfffAnffG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111441441414414144414141111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                 @ �@ %@ �@ *@ �@ #�@ (�@ /�@ 7L@ >�@ E�@ R�@ `B@ l�@ z�@ ��@ ��@ �5@ ��@ ��@ �|@ �t@ ��@ �q@j@�@ @-@:�@H]@UU@b�@r@~�@�D@��@��@��@�>@��@ލ@�4@�,@�@*@"�@/�@>@K@Z@ff@s_@�d@�@�@��@�R@�J@�O@��@�@@�E@
�@6@&;@5?@B8@N�@]�@k.@v�@��@��@�z@�@�^@�@�
@�@�e@@@O@*S@7L@B�@R�@_�@l�@{�@�7@��@��@��@��@��@�#@��@�q@�@�@g@-@:@G�@UU@b�@p�@~K@��@�<@��@��@@�7@܀@��@��@v@@"�@/�@<�@Lu@Yn@ff@v@�@�@�U@�M@�@��@��@�T@�@��@�@�@&�@3�@B�@P�@]�@i�@v�@��@�u@��@��@�w@��@�
@�@��@�Q@V@O@'�@6�@E�@R�@^�@m�@|�@�7@��@��@��@�2@�|@�@��@� @	v@	�@	
@	,`@	;d@	H]@	SI@	bN@	p�@	~K@	��@	�H@	��@	��@	�2@	��@	�/@	��@	�,@
�@
{@
"�@
1'@
>@
I@
Wb@
e	@
t�@
�d@
��@
��@
��@
��@
��@
�O@
�@
��@
�9@
=@�@$.@33@B8@N�@[z@j@x&@�@��@�m@�r@�@�c@׹@�@�Y@  @�@O@(�@7L@D�@SI@`�@k�@z3@��@��@�5@��@�w@�@��@�@��@�@@
@-�@<@I@V�@dZ@oF@y�@@Z@��@�@&�@i!@��@�@'�@hs@��@��@2�@x�@��@1@O�@�<@��@*S@t@�@1@Q�@�H@�T@(�@o�@��@��@A�@�|@ȴ@J@N�@�h@�O@�@V�@��@׹@6@X@��@��@@Q=@�@�7@@B8@�W@��@��@@�@�@��@�@G�@�+@�@�@H]@��@�@V@Q=@��@�\@ �@ ^5@ �m@ �@!$�@!ff@!��@!��@"+@"m�@"�~@"�e@#7�@#|?@#�w@$j@$H]@$��@$��@%o@%Wb@%�U@%��@&%�@&hs@&�Y@&��@'33@'uk@'��@'��@(;d@(}�@(��@)  @)A�@)��@)Ĝ@*v@*E�@*��@*��@+j@+A�@+�@+��@, �@,<�@,x&@,�F@,�@--@-i!@-�4@-��@.�@.Yn@.��@.��@/�@/FQ@/�@/�^@/�@0.l@0ff@0��@0�@1o@1M$@1�|@1��@1��@24�@2o�@2�@2�@3 @3[z@3�0@3��@4J@4F�@4�@4��@4��@52�@5m:@5�@5�#@6{@6O0@6��@6ƨ@7@7> @7v�@7�!@7�(@8"�@8Z@8��@8�c@9]@9s_@:�@:��@;2�@;�@<G�@<��@=`B@=��@>B8@>��@?UU@?��@@dZ@@��@A�u@A�}@B�\@C%�@C��@D�@D��@EP�@E��@FM�@F�`@G}�@G��@Hww@I�@Iv�@J�@J�~@K�@K��@L$/@L��@M,`@Mƨ@Na�@N��@Oi!@O�[@Pr@Q�#@S%@T\)@U��@W+�@X�|@Y�
@[)�@\�@]�o@_�@`ww@a�@c{@doF@e�!@gO@h|?@i�F@k�@luk@m�r@o""@pg�@q��@s�@thr@u��@w;e@x�\@y��G�O�G�O�@ �G�O�G�O�@ G�O�@ G�O�G�O�@ �G�O�@ �G�O�G�O�G�O�@ vG�O�@ %G�O�@ �@ 1@ �@ 	�@ 
�@ �@ �@ V@ b@ �@ �@ �@ �@ �@ �@ �@ !s@ $�@ '�@ )�@ -@ /@ 2�@ 5?@ 6�@ :@ =q@ ?}@ B8@ E�@ H]@ K�@ N�@ Q=@ S�@ V�@ X�@ \)@ ^�@ bNG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�AϼjA���A��#A��
A���A��A��A��/A��`A��yA��yA��A��A��A��mA��A��A��A��A��A��A��A��A��A��A��A��/AϑhA�ƨA�dZA�+A�ȴA͋DA��
A�r�A�bA��AȅA��A�;dA�"�A�l�A��A���A��
A�oA�;dA��A���A��RA���A��hA� �A�A��
A�dZA��RA�-A�;dA���A��A��A�O�A��A�G�A��A�
=A���A�ĜA�l�A��A��`A�1A���A��mA��#A���A�&�A�ȴA��mA�=qA��A��A��A�C�A�A�A�33A��#A��A�A�A�5?A�&�A�|�A�K�A���A�7LA�"�A�&�A�^5A��A�;dA���A�1'A�1'A��\A�-A�I�A���A��DA��A�G�A�/A��`A��-A�O�A�ĜA��`A���A��A�^5A�S�A��!A�|�A�mA��AC�A~~�A}�A|-A{G�Az{Ax��AwO�At�/As�ArM�AqK�An=qAl(�Aj�Ag�;Af{AbffA`��A^�`A\�DA[��AY�AWoAUVAR��ARAQ"�AN �AK��AI��AI�AH��AHbAG�7AF��AD�`AC�
AB��A@�HA@I�A>(�A:�!A9�
A9��A9O�A9VA8�DA7��A7G�A6A�A4��A4  A3��A2{A0(�A.��A-?}A,A*�9A)��A(ȴA( �A'VA&jA$�!A#�hA"�RA!��A!�A v�A��AbNA��A%A~�A�A  A|�A�A��A�jA�A&�AȴA�9A�+A�-A��AQ�A��A|�A
��A
�A	��A	G�AƨAA�AK�An�A�A;dAĜAn�AE�AJAƨA ��A �9A VA  �@���@��^@���@� �@�dZ@�n�@���@��@�bN@�|�@�~�@��@@�ff@�Z@��#@�X@���@�\@���@���@�@� �@�1@�Q�@�5?@́@��@��;@�S�@�E�@��@���@��@�1@���@��F@��!@�%@���@�C�@���@�M�@���@��T@��@�hs@�bN@�A�@�;d@�v�@��
@�@��#@���@�r�@�1@���@��j@��!@�?}@��@��P@�M�@�`B@�Z@�|�@�@���@��`@���@���@�v�@��@�j@~��@|��@{�
@y�@xb@v�R@u/@st�@o�@m�h@jn�@gl�@fȴ@e�h@c�@b-@`�u@_�;@^E�@\z�@Z~�@Yhs@WK�@V{@Tz�@St�@R-@P�`@N��@M�@M`B@L9X@J��@I&�@Hb@F�R@E�-@D�D@B^5@A�@@r�@?\)@>�y@=`B@<��@;dZ@:�H@9��@8��@7�w@7
=@6$�@5�T@49X@3o@1hs@01'@/+@-O�@,9X@,�@+33@*=q@)��@(��@(  @'\)@&E�@%p�@%�@$�j@#�F@#@"J@ r�@�P@��@�h@�@(�@S�@�@��@�`@  @\)@E�@�@dZ@��@�@�7@��@�P@v�@$�@`B@�@��@��@��@
^5@	��@��@r�@1'@ȴ@ff@�@V@�D@9X@�@�!@n�@7L@ �`@ �`@ �@ 1'?���?���?�C�?��?�E�?��?��?�h?�dZ?��?�r�?�+?�ff?��T?�9X?�!?�hs?��?�O�?��m?��?ؓu?�+?�E�?���?�33?�-?��?��`?�  ?�\)?Η�?�p�?�O�?̬?�(�?���?�X?��?ȴ9?�1'?�l�?Ƈ+?��?öF?\?���?�G�?�bN?�  ?�;d?��?�5??�/?�I�?�ƨ?�?�^5?���?��^?�x�?�x�?���?�~�?���?��H?�"�?���?��m?�j?�j?�/?�{?��?��wAϺ^AϼjAϾwAϾwAϾwAϾwAϾwAϾwA�A�ȴA���A���A���A���A��/A��A��#A��A��#A��#A��#A��#A��A��A��#A��#A��/A��/A��A��
A���A���A��
A���A��
A���A��A��
A��
A��#A��#A��#A��;A��TA��mA��mA��yA��mA��mA��yA��A��A��mA��yA��A��A��A��A��A��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                 AϼjA���A��#A��
A���A��A��A��/A��`A��yA��yA��A��A��A��mA��A��A��A��A��A��A��A��A��A��A��A��/AϑhA�ƨA�dZA�+A�ȴA͋DA��
A�r�A�bA��AȅA��A�;dA�"�A�l�A��A���A��
A�oA�;dA��A���A��RA���A��hA� �A�A��
A�dZA��RA�-A�;dA���A��A��A�O�A��A�G�A��A�
=A���A�ĜA�l�A��A��`A�1A���A��mA��#A���A�&�A�ȴA��mA�=qA��A��A��A�C�A�A�A�33A��#A��A�A�A�5?A�&�A�|�A�K�A���A�7LA�"�A�&�A�^5A��A�;dA���A�1'A�1'A��\A�-A�I�A���A��DA��A�G�A�/A��`A��-A�O�A�ĜA��`A���A��A�^5A�S�A��!A�|�A�mA��AC�A~~�A}�A|-A{G�Az{Ax��AwO�At�/As�ArM�AqK�An=qAl(�Aj�Ag�;Af{AbffA`��A^�`A\�DA[��AY�AWoAUVAR��ARAQ"�AN �AK��AI��AI�AH��AHbAG�7AF��AD�`AC�
AB��A@�HA@I�A>(�A:�!A9�
A9��A9O�A9VA8�DA7��A7G�A6A�A4��A4  A3��A2{A0(�A.��A-?}A,A*�9A)��A(ȴA( �A'VA&jA$�!A#�hA"�RA!��A!�A v�A��AbNA��A%A~�A�A  A|�A�A��A�jA�A&�AȴA�9A�+A�-A��AQ�A��A|�A
��A
�A	��A	G�AƨAA�AK�An�A�A;dAĜAn�AE�AJAƨA ��A �9A VA  �@���@��^@���@� �@�dZ@�n�@���@��@�bN@�|�@�~�@��@@�ff@�Z@��#@�X@���@�\@���@���@�@� �@�1@�Q�@�5?@́@��@��;@�S�@�E�@��@���@��@�1@���@��F@��!@�%@���@�C�@���@�M�@���@��T@��@�hs@�bN@�A�@�;d@�v�@��
@�@��#@���@�r�@�1@���@��j@��!@�?}@��@��P@�M�@�`B@�Z@�|�@�@���@��`@���@���@�v�@��@�j@~��@|��@{�
@y�@xb@v�R@u/@st�@o�@m�h@jn�@gl�@fȴ@e�h@c�@b-@`�u@_�;@^E�@\z�@Z~�@Yhs@WK�@V{@Tz�@St�@R-@P�`@N��@M�@M`B@L9X@J��@I&�@Hb@F�R@E�-@D�D@B^5@A�@@r�@?\)@>�y@=`B@<��@;dZ@:�H@9��@8��@7�w@7
=@6$�@5�T@49X@3o@1hs@01'@/+@-O�@,9X@,�@+33@*=q@)��@(��@(  @'\)@&E�@%p�@%�@$�j@#�F@#@"J@ r�@�P@��@�h@�@(�@S�@�@��@�`@  @\)@E�@�@dZ@��@�@�7@��@�P@v�@$�@`B@�@��@��@��@
^5@	��@��@r�@1'@ȴ@ff@�@V@�D@9X@�@�!@n�@7L@ �`@ �`@ �@ 1'?���?���?�C�?��?�E�?��?��?�h?�dZ?��?�r�?�+?�ff?��T?�9X?�!?�hs?��?�O�?��m?��?ؓu?�+?�E�?���?�33?�-?��?��`?�  ?�\)?Η�?�p�?�O�?̬?�(�?���?�X?��?ȴ9?�1'?�l�?Ƈ+?��?öF?\?���?�G�?�bN?�  ?�;d?��?�5??�/?�I�?�ƨ?�?�^5?���?��^?�x�?�x�?���?�~�?���?��H?�"�?���?��m?�j?�j?�/?�{?��?��wAϺ^AϼjAϾwAϾwAϾwAϾwAϾwAϾwA�A�ȴA���A���A���A���A��/A��A��#A��A��#A��#A��#A��#A��A��A��#A��#A��/A��/A��A��
A���A���A��
A���A��
A���A��A��
A��
A��#A��#A��#A��;A��TA��mA��mA��yA��mA��mA��yA��A��A��mA��yA��A��A��A��A��A��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                 ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��BB"�Bn�BB!�B$�B,B.B33B9XB9XB7LB5?B1'B,B%�B<jBI�BT�Bn�Bq�Bw�Bw�Bw�Bz�B�%B�+B�JB�JB�VB�{B��B��B��B��B��B�B�B�B�3BB��B�dB�?B�3B�B�B��B��B��B��B��B��B�uB�bB�=B�=B�+B~�Bs�B\)BE�B<jB �B��B�B��B�dB�!B�B��B��B�=B�Bv�BaHBVBL�B@�B&�BbB
��B
�mB
��B
�B
��B
��B
�+B
u�B
aHB
K�B
VB
^5B
aHB
iyB
~�B
o�B
VB
?}B
A�B
B�B
@�B
<jB
;dB
J�B
E�B
;dB
8RB
%�B
�B
�B
JB
B	�B	�NB	��B	B	�3B	��B	��B	�1B	z�B	s�B	gmB	T�B	G�B	9XB	5?B	(�B	�B	DB	B	B��B��B��B�B�mB�NB�B��B��B�}B�^B�FB�9B�'B�B�B�B��B��B��B��B��B��B��B��B�uB�bB�PB�+B�B� B{�Bz�Bx�Bs�Bq�Bo�Bm�Bl�BiyBiyBffBe`B\)BR�BQ�BO�BL�BJ�BF�BE�BA�BB�B@�B@�B>wB@�BE�BF�BB�BF�BD�BB�B@�B?}B:^B7LB5?B2-B2-B33B33B2-B1'B.B/B/B/B/B+B.B.B/B-B.B.B.B1'B2-B1'B/B0!B1'B33B6FB6FB33B7LB49B49B5?B^5By�B��B�?B�^B�
B�B��B��B��B��B�`B�B�/B��B��B�B��B	VB	�B	"�B	)�B	9XB	D�B	Q�B	]/B	cTB	iyB	p�B	�B	��B	��B	��B	��B	�FB	�RB	�}B	ǮB	ȴB	��B	�B	�B	�#B	�;B	�;B	�fB	�yB	�B	�B	�B	��B	��B	��B	��B	��B
B
B
%B
1B
	7B
DB

=B
PB
oB
uB
�B
�B
�B
�B
�B
�B
�B
!�B
#�B
%�B
'�B
(�B
)�B
+B
,B
-B
/B
0!B
1'B
2-B
49B
49B
5?B
7LB
8RB
9XB
<jB
<jB
=qB
>wB
?}B
A�B
B�B
D�B
D�B
E�B
G�B
G�B
G�B
H�B
G�B
J�B
K�B
M�B
N�B
N�B
O�B
Q�B
P�B
Q�B
T�B
T�B
VB
W
B
W
B
XB
YB
YB
ZB
ZB
[#B
\)B
^5B
_;B
`BB
`BB
aHB
cTB
cTB
cTB
dZB
ffB
gmB
gmB
hsB
hsB
hsB
iyB
iyB
jB
k�B
m�B
n�B
m�B
n�B
o�B
p�B
q�B
p�B
r�B
r�B
s�B
t�B
t�B
v�B
v�B
w�B
w�B
x�B
y�B
y�B
z�B
z�B
|�B
|�B
|�B
|�B
~�B
� B
� B
�B
�B
�B
�+B
�1B
�=B
�DB
�JB
�PB
�VB
�VB
�\B
�hB
�oB
�oB
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
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
�B
�B
�B
�!B
�!B
�-B
�-B
�3B
�9B
�?B
�FB
�LB
�RB
�RB
�RB
�^B
�^B
�^B
�XB
�dB
�^B
�dB
�^B
�jB
�dB
�jB
�jB
�jB
�jB
�dB
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                 B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B  B �Bl�B��B�B"�B)�B,B1'B7LB7LB5?B33B/B)�B#�B:^BG�BR�Bl�Bo�Bu�Bu�Bu�Bx�B�B�B�=B�=B�JB�oB��B��B��B��B��B�B�B��B�'B��B�wB�XB�3B�'B�B�B��B��B��B��B��B�{B�hB�VB�1B�1B�B|�Bq�BZBC�B:^B�B��B��B��B�XB�B�B��B��B�1B� Bt�B_;BS�BJ�B>wB$�BVB
��B
�fB
��B
�B
��B
��B
�%B
t�B
`BB
J�B
T�B
]/B
`BB
hsB
}�B
n�B
T�B
>wB
@�B
A�B
?}B
;dB
:^B
I�B
D�B
:^B
7LB
$�B
�B
{B
DB
B	�B	�HB	��B	��B	�-B	��B	��B	�+B	y�B	r�B	ffB	S�B	F�B	8RB	49B	'�B	�B	
=B	B	  B��B��B��B�B�fB�HB��B��B��B�wB�XB�?B�3B�!B�B�B��B��B��B��B��B��B��B��B��B�oB�\B�JB�%B�B~�Bz�By�Bw�Br�Bp�Bn�Bl�Bk�BhsBhsBe`BdZB[#BQ�BP�BN�BK�BI�BE�BD�B@�BA�B?}B?}B=qB?}BD�BE�BA�BE�BC�BA�B?}B>wB9XB6FB49B1'B1'B2-B2-B1'B0!B-B.B.B.B.B)�B-B-B.B,B-B-B-B0!B1'B0!B.B/B0!B2-B5?B5?B2-B6FB33B33B49B]/Bx�B��B�9B�XB�B�B��B�B��B��B�ZB�B�)B��B��B�
B��B	PB	�B	!�B	(�B	8RB	C�B	P�B	\)B	bNB	hsB	o�B	�B	�{B	��B	��B	��B	�?B	�LB	�wB	ƨB	ǮB	��B	�
B	�B	�B	�5B	�5B	�`B	�sB	�B	�B	�B	�B	��B	��B	��B	��B
B
B
B
+B
1B

=B
	7B
PB
oB
uB
�B
�B
�B
�B
�B
�B
�B
!�B
#�B
%�B
'�B
(�B
)�B
+B
,B
-B
/B
0!B
1'B
2-B
49B
49B
5?B
7LB
8RB
9XB
<jB
<jB
=qB
>wB
?}B
A�B
B�B
D�B
D�B
E�B
G�B
G�B
G�B
H�B
G�B
J�B
K�B
M�B
N�B
N�B
O�B
Q�B
P�B
Q�B
T�B
T�B
VB
W
B
W
B
XB
YB
YB
ZB
ZB
[#B
\)B
^5B
_;B
`BB
`BB
aHB
cTB
cTB
cTB
dZB
ffB
gmB
gmB
hsB
hsB
hsB
iyB
iyB
jB
k�B
m�B
n�B
m�B
n�B
o�B
p�B
q�B
p�B
r�B
r�B
s�B
t�B
u�B
w�B
w�B
x�B
x�B
y�B
z�B
z�B
{�B
{�B
}�B
}�B
}�B
}�B
� B
�B
�B
�B
�B
�%B
�1B
�7B
�DB
�JB
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
��B
��B
��B
��B
��B
�B
�B
�B
�B
�!B
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
�XB
�^B
�dB
�dB
�qB
�qB
�qB
�jB
�wB
�qB
�wB
�qB
�}B
�wB
�}B
�}B
�}B
�}B
�wB
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
�B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
�B
��B
��B
��B
��B
��B
��B
��B
��B
��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                 <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�201807242202382021061413522220210614135222202106141746292021061417462920210614174629201807242202382021061413522220210614135222202106141746292021061417462920210614174629PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            new conductivity = original conductivity * (1 + delta*TEMP + CPcor_SBE*PRES) / (1 + delta*TEMP_ADJUSTED + CPcor_new*PRES_ADJUSTED)                                                                                                                              none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS                                                                                                                                                                                                                                       PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            new conductivity = original conductivity * (1 + delta*TEMP + CPcor_SBE*PRES) / (1 + delta*TEMP_ADJUSTED + CPcor_new*PRES_ADJUSTED)                                                                                                                              none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS                                                                                                                                                                                                                                       surface_pressure=-0.50 dbar                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            CPcor_new = -1.25e-07; CPcor_SBE = -9.57e-08; delta = 3.25e-06                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            OW r = 1 (+/-0), vertically averaged dS = -0.002 (+/-0)                                                                                                                                                                                                         surface_pressure=-0.50 dbar                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            CPcor_new = -1.25e-07; CPcor_SBE = -9.57e-08; delta = 3.25e-06                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            OW r = 1 (+/-0), vertically averaged dS = -0.002 (+/-0)                                                                                                                                                                                                         Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                      none                                                                                                                                                                                                                                                            New conductivity computed by using a different CPcor value from that provided by Sea-Bird.                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Significant salinity sensor drift detected. OW least squares fit adopted.                                                                                                                                                                                       Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                      none                                                                                                                                                                                                                                                            New conductivity computed by using a different CPcor value from that provided by Sea-Bird.                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Significant salinity sensor drift detected. OW least squares fit adopted.                                                                                                                                                                                       PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            AO  AO  ARCAARCAADJPADJP                                                                                                                                        2018072422023820180724220238  IP  IP                                G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2018072422023820180724220238QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�5F03E           703E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2018072422023820180724220238QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�0               0               PM  PM  ARSQARSQCOWGCOWGV1.1V1.1                                                                                                                                2021072216015020210722160150IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                