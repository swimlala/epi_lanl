CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2019-04-29T07:00:39Z creation      
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
resolution        =���   axis      Z        X  ;�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  K,   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     X  O   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ^\   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     X  b4   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     X  q�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     X  ��   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     X  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     X  �D   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     X  �t   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     X  ͤ   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                   d   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                   l   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                   t   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                   |   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  � �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                      HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                       HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    (   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar        H   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar        P   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�       X   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    `   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ݤ   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  � �Argo profile    3.1 1.2 19500101000000  20190429070039  20210617131518  5905739 5905739 US ARGO PROJECT                                                 US ARGO PROJECT                                                 GREGORY C. JOHNSON                                              GREGORY C. JOHNSON                                              PRES            TEMP            PSAL            PRES            TEMP            PSAL               C   CDD  AOAO7219                            7219                            2C  2C  DD  SOLO_D_MRV                      SOLO_D_MRV                      12002                           12002                           SBE603 01Dec16                  SBE603 01Dec16                  874 874 @صJT÷�@صJT÷�11  @صJ-��@صJ-��@5�P�ܜN@5�P�ܜN�c�G��X�c�G��X11  GPS     GPS     Primary sampling: averaged [1Hz sampling by SBE-41CP averaged in 2-dbar bins]                                                                                                                                                                                   Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 AC  AA  AA  >���?�  @ff@Fff@�33@�  @�  @�  A   A  A!��A@  Ac33A���A�  A���A���A�33A���A���A���B   B��B��B��B ffB(��B0��B8ffB@  BHffBPffBXffB`  Bg��BpffBx��B�33B�  B���B�  B�33B�  B���B�  B�33B�ffB�ffB�33B�  B�  B�  B�33B�33B�  B�  B̙�B���B�  B�  B�  B�  B㙚B�ffB�ffB�ffB���B�  B�ffC L�C33C�fC  C  C
�C33C  C��C��C�fC�fC�fC�fC�fC�C �C"  C$�C&�C(33C*33C,33C.33C0�C2�C433C633C833C:33C<L�C>33C@�CA�3CC��CE�fCG�fCJ  CL�CN33CP33CRL�CTL�CV�CW��CZ  C\�C^L�C`L�CbL�CdL�Ce�fCh33Cj�Ck�fCn�Cp�Cq�fCt�Cv  Cw�fCz33C|�C~  C��C��C�  C�&fC��C��C�  C��3C��C�  C��fC��3C��C��C��C��3C��C�  C��fC�  C��3C��fC�  C��C��C�  C��C��C��C�&fC�  C��fC�  C��C��C��C��3C�  C��C�&fC�&fC��C�  C�  C��C��C�  C��fC�  C��C�&fC��C��fC��3C�  C��C��C��C�ٚC��C��C��fC��fC��3C��3C��3C��3C�  C�  C�  C�  C�  C�  C��C��C��C��C�&fC�33C�&fC��C��C��C��C�  C��C��C��C��C��C�  C��3C��3C��C�&fC��C��C��C�  C�  C��3C��3C��3C��3C�  C��3C��3C��fC��C�33C�33C��C��C��C�  C�  C��3C��fC��fC��fC��fC��C�  DٚDFfD��D	&fD�fD  D�fD,�D� DL�D�fD9�D��D"  D$��D&��D)Y�D+��D.  D0Y�D2� D5fD7S3D9�3D;� D>,�D@�fDB� DE,�DGy�DI��DL  DNy�DP�fDSs3DUٚDXS3DZٚD]l�D_�3Db��De&fDg��Dj33Dl��DofDqs3Ds��Dv@ Dx��D{�D}  DY�D��fD���D�#3D�I�D�l�D���D���D�ɚD��fD�  D�  D�C3D�c3D���D��3D��3D�� D���D�#3D�FfD�p D���D�� D���D���D��D�33D�S3D�y�D���D���D��fD���D�3D�33D�\�D��3D���D�� D��3D�3D�,�D�C3D�` D�s3D��3D���D�� D��fD��3D��3D� D�)�D�@ D�S3D�c3D�s3D�� D��fD¦fDü�D�� D��fD���D�fD�&fD�@ D�S3D�ffD�s3D΀ Dό�DЙ�Dќ�DҠ Dө�DԹ�D���D��3D�� D��D�� D��fD�	�D� D�  D�&fD�0 D�33D�6fD�@ D�@ D�@ D�C3D�C3D�I�D�C3D�I�D�P D�L�D�C3D�9�D�)�D�fD�fD���D���D�ٚD��3D���D���D�y�D�` D�C3D�,�D���D���D��fD���D�y�D�Y�E ��E��E3Ed�E��EfE� E	A�E
�3E�fEI�E8 E��E�3EٚE#3Ei�E�3E#3E|�Ec3E�fE�fE;3E{3E��E �E"#3E#�fE$�3E&3E'A�E(l�E)��E+!�E,NfE-s3E.�3E/��E13E2��E3�3E4�fE6i�E7��E8�fE:3E;A�E<p E?� EB�3EE� EH� ELfEO)�ERL�EU��EX��E[��E^ɚEbf>L��>L��>L��>���>L��>���>L��>L��>���>���>L��>L��>L��>���>���>���>L��>���>���>���>���>���>���>���?   >���?��?��?fff?L��?�  ?���?�ff?�  ?ٙ�@   @ff@   @333@Fff@Y��@s33@�33@���@�ff@�  @���@���@�ff@�33@�  @���@�ffA33A��A��A33A!��A(  A1��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111441414414444144411444441414141111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                 ?L��?�  @&ff@fff@�33@�  @�  @�  A  A  A)��AH  Ak33A���A�  A���A���A�33A���A���A���B  B	��B��B��B"ffB*��B2��B:ffBB  BJffBRffBZffBb  Bi��BrffBz��B�33B�  B���B�  B�33B�  B���B�  B�33B�ffB�ffB�33B�  B�  B�  B�33B�33B�  B�  B͙�B���B�  B�  B�  B�  B䙚B�ffB�ffB�ffB���B�  B�ffC ��C�3CffC� C� C
��C�3C� CL�CL�CffCffCffCffCffC��C ��C"� C$��C&��C(�3C*�3C,�3C.�3C0��C2��C4�3C6�3C8�3C:�3C<��C>�3C@��CB33CDL�CFffCHffCJ� CL��CN�3CP�3CR��CT��CV��CXL�CZ� C\��C^��C`��Cb��Cd��CfffCh�3Cj��ClffCn��Cp��CrffCt��Cv� CxffCz�3C|��C~� C�Y�C�Y�C�@ C�ffC�Y�C�L�C�@ C�33C�L�C�@ C�&fC�33C�L�C�Y�C�L�C�33C�L�C�@ C�&fC�@ C�33C�&fC�@ C�Y�C�Y�C�@ C�Y�C�Y�C�L�C�ffC�@ C�&fC�@ C�L�C�Y�C�Y�C�33C�@ C�Y�C�ffC�ffC�L�C�@ C�@ C�L�C�Y�C�@ C�&fC�@ C�L�C�ffC�L�C�&fC�33C�@ C�Y�C�Y�C�L�C��C�L�C�L�C�&fC�&fC�33C�33C�33C�33C�@ C�@ C�@ C�@ C�@ C�@ C�L�C�Y�C�L�C�Y�C�ffC�s3C�ffC�Y�C�Y�C�L�C�L�C�@ C�L�C�Y�C�Y�C�Y�C�L�C�@ C�33C�33C�L�C�ffC�Y�C�L�C�L�C�@ C�@ C�33C�33C�33C�33C�@ C�33C�33C�&fC�L�C�s3C�s3C�Y�C�L�C�L�C�@ C�@ C�33C�&fC�&fC�&fC�&fC�L�C�@ D��DffDٚD	FfD�fD@ D�fDL�D� Dl�D�fDY�DٚD"@ D$��D'�D)y�D+ٚD.  D0y�D2� D5&fD7s3D9�3D<  D>L�D@�fDC  DEL�DG��DI��DL@ DN��DQfDS�3DU��DXs3DZ��D]��D`3Db��DeFfDg��DjS3Dl��Do&fDq�3Ds��Dv` Dx��D{,�D}  Dy�D��fD�	�D�33D�Y�D�|�D���D���D�ٚD��fD� D�0 D�S3D�s3D���D��3D��3D�� D��D�33D�VfD�� D���D�� D���D���D��D�C3D�c3D���D���D�ɚD��fD�	�D�#3D�C3D�l�D��3D���D�� D�3D�#3D�<�D�S3D�p D��3D��3D���D�� D��fD��3D�3D�  D�9�D�P D�c3D�s3D��3D�� D��fD¶fD���D�� D��fD��D�&fD�6fD�P D�c3D�vfD̓3Dΐ DϜ�DЩ�DѬ�DҰ Dӹ�D�ɚD���D��3D�� D���D�  D�fD��D�  D�0 D�6fD�@ D�C3D�FfD�P D�P D�P D�S3D�S3D�Y�D�S3D�Y�D�` D�\�D�S3D�I�D�9�D�&fD�fD�	�D���D��D��3D���D���D���D�p D�S3D�<�D�	�D���D��fD���D���D�i�E ��E��E#3El�E��EfE� E	I�E
�3E�fEQ�E@ E��E�3E�E+3Eq�E�3E+3E��Ek3E�fE�fEC3E�3E��E ��E"+3E#�fE$�3E&3E'I�E(t�E)��E+)�E,VfE-{3E.�3E0�E13E2��E3�3E4�fE6q�E7��E8�fE:3E;I�E<x E?� EB�3EE� EH� ELfEO1�ERT�EU��EX��E[��E^њEbfG�O�G�O�?333G�O�?333G�O�G�O�?333G�O�G�O�G�O�G�O�?333G�O�G�O�G�O�?333?L��G�O�G�O�G�O�G�O�G�O�?L��G�O�?fffG�O�?���G�O�?�ff?�  ?���?�ff@   @��@   @&ff@@  @S33@fff@y��@���@�33@���@�ff@�  @���@ə�@�ff@�33@�  @���A33A33A��A��A#33A)��A0  A9��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111441414414444144411444441414141111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                 @ @ �@ V@ *@ �@ ""@ (�@ /�@ 6�@ =q@ D�@ Q�@ `�@ m�@ z�@ ��@ ��@ �(@ �-@ ��@ �|@ �t@ �m@ �@�@�@ @-�@:�@G�@V@c�@qS@~K@�D@�H@��@��@@ψ@��@�4@�,@%@{@"�@1'@>�@K�@X�@ff@t@�d@�@�@��@�^@�J@Ӡ@�H@��@��@�@B@&�@4�@@,@N�@]�@k�@x�@�p@��@�m@��@�@�c@խ@�T@�@�Q@�@�@(G@7L@D�@Q�@`B@m�@|?@��@��@�5@�-@��@�*@��@�y@� @v@o@g@*S@8�@F�@T�@b�@qS@�@�P@��@�M@��@�2@�7@ލ@�@�9@�@�@!s@1'@>@Ji@Yn@g@s_@�d@�\@�U@�@�@��@��@�@��@��@�@�@%�@2�@A�@N�@Z�@i!@x&@�|@�u@��@��@��@�@�
@�@��@  @@�@(�@7�@E�@R�@a�@m:@y�@��@��@�5@��@�w@��@��@�(@��@	@	@	�@	-@	;d@	G�@	S�@	b�@	qS@	�W@	��@	�<@	��@	��@	��@	є@	ލ@	�y@	��@
�@
@
 �@
/@
<�@
Ji@
X@
ff@
t@
��@
�\@
�@
��@
�@
�W@
�O@
�@
��@
�Q@J@B@&�@3�@A�@N�@\�@k.@x�@�|@�u@�m@�f@�@�@�@�@�@ �@�@O@(G@5�@C�@Q=@_�@l�@z3@�+@��@��@�9@��@�|@�#@�@��@�@�@[@+@8�@H]@UU@��@�@^�@�@�`@(�@m�@��@�,@>�@�d@�J@	�@K@�P@�*@b@Q=@�\@ψ@@O0@��@�o@
=@I@�7@�c@1@F�@�|@��@%@H]@��@ψ@@X@�a@�T@*S@qS@�F@�9@<�@~�@�2@^@DD@�|@�W@��@<�@|?@�^@��@8�@v�@�9@�@.l@k.@�A@�@"�@`A@�@�#@�@UU@�@��@ @ N�@ �D@ �c@!%@!C�@!�@!��@!�E@"<@"z3@"��@"�@#1�@#m�@#�Y@#��@$)�@$hs@$�A@$�`@%"�@%^�@%�H@%�
@&�@&K�@&�+@&@&��@':�@'t�@'�~@'�@((�@(c�@(��@(׹@)@)Lu@)�|@)��@)��@*7�@*s_@*�r@*�y@+%�@+`A@+��@+�O@,�@,F�@,�W@,��@,��@-'�@-a�@-�U@-�O@.�@.FQ@.~K@.�F@.��@/(�@/b�@/��@/Ӡ@0
�@0B8@0z�@0�~@0�@1g@1V@1��@1�>@1�9@233@2i!@2��@2�C@3v@37�@3k.@3�@3��@4v@47L@4i!@4��@4�o@4��@5-@5^�@5��@5�@5�y@6�@6I�@6y�@6�#@7e	@7�L@8|�@9
�@9�T@9��@:�@;#�@;�9@<H]@<�@=B�@=�h@>:�@>�W@?R�@?�@@|?@A�@Ar@A�Q@B�C@C�@C�a@D&;@D�Z@E-�@E��@FZ@F��@G\�@G܀@H\�@I@I�p@J]@J��@K�@K�P@L-@L�@M+�@M�O@NS�@N��@O`�@O�`@Pff@QӠ@S@Tt@U��@W�@Xc�@Y�^@[�@\oF@]�k@_V@`s_G�O�G�O�@ ^G�O�@ ^G�O�G�O�@ ^G�O�G�O�G�O�G�O�@ ^G�O�G�O�G�O�@ ^@ G�O�G�O�G�O�G�O�G�O�@ G�O�@ �G�O�@ G�O�@ v@ �@ �@ �@ 
=@ �@ �@ V@ @ @ *@ 6@ �@ �@ 
@  @ ""@ $�@ '�@ *S@ -@ /�@ 2�@ 4�@ 7�@ <@ >@ B8@ D�@ G�@ K�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�A�A�I�A�K�A�M�A�M�A�S�A�S�A�K�A�7LA�5?A�;dA�;dA���A��A���A��A���A��A���A�%A���A�^5A�1'A�(�A��#A�+A�+A��mA��PA�{A��^A���A��7A�|�A�bNA�
=A��7A��A���A�v�A��;A�n�A�33A��^A�?}A��A�ƨA��DA�+A�ƨA�;dA�x�A��A�"�A��A��DA�p�A��A���A���A��A���A��uA�~�A�p�A�E�A�%A��-A�\)A�+A�
=A��A�$�A�jA��wA�ffA�$�A��DA�t�A�(�A��A�+A�?}A�&�A�33A�I�A��FA��A�&�A�S�A�VA�n�A�oA�bA�A�n�A���A�JA�K�A�=qA�dZA�oA��FA�dZA��wA�1'A��^A���A�n�A��A�?}A���A���A��PA�A�$�A��#A�JA�A�A�XA��uA�ȴA~ĜA}33A{��Az�Az�AyƨAt �Al�Aj�AjbAidZAh��Ag�;Ag?}AfVAe�hAd��Adr�Ab��AaC�A`ZA]��AY�^AWO�AT{AQVAOXANv�AL��AK33AH�yAF�RAEx�AC�;AAA?�TA?�7A>5?A=S�A<9XA:��A9�;A8r�A6~�A5G�A4Q�A4�A3�7A2�A2Q�A/A-�A,�\A+K�A*�A*5?A)��A)hsA(��A';dA&-A%�wA$^5A#?}A"�A"�/A"��A"I�A"  A!33A�AG�A��A�AoAr�A�A��A��AbNA�A�A��A"�A �A�hA�yA �A�^A
��A
�jA
�A	�;AffAM�A$�A�TA�A�AK�AC�A�AK�AE�A�^AM�A I�@���@�?}@��j@��@���@��@���@��`@�1'@�=q@�j@@�@��
@��H@�@�j@�A�@�F@�dZ@��@�n�@�7L@㝲@�R@◍@�\@�Z@�M�@��@�+@�S�@�t�@��@��R@�r�@�j@���@�ȴ@��h@�/@��@�C�@�G�@��@�9X@���@�&�@�r�@�  @�Ĝ@���@�hs@��@�
=@��@���@���@��@���@��@�E�@��#@��@���@�|�@�l�@�S�@���@���@��
@��@�$�@�9X@���@��y@��+@���@�%@�Q�@��\@���@�G�@��9@�@~V@{��@z~�@y&�@vV@sƨ@q7L@nv�@l�@k�F@kS�@iX@g��@fff@e@d9X@c"�@aG�@` �@_�P@^��@]�@]V@[��@Y�@W�P@W+@Vff@U��@U/@R�\@Q�^@Q%@O��@NE�@M��@LI�@Ix�@I&�@HA�@F��@E�T@DI�@A�7@?�w@>��@>E�@<��@:��@:J@9��@8��@8 �@6�y@3��@3o@1�@1X@/\)@.{@-�T@-�-@-�h@,��@+�m@*J@)x�@(��@'�@&��@$I�@#C�@"��@"n�@"J@!��@!X@ �`@��@�R@5?@�h@�@�D@o@^5@=q@G�@Q�@ff@@�@S�@��@  @;d@ȴ@�+@�@/@�D@33@
-@
-@	�#@	�@�@�;@;d@��@��@�T@p�@�@�/@z�@1@M�@G�@ �9@   ?��?�p�?�C�?��+?�t�?��;?�|�?�{?��m?ꟾ?�r�?�?��?�F?�t�?��?ޗ�?�1?�x�?���?�l�?��y?�?��/?��?�&�?�bN?��;?��?�{?�V?���?�(�?��m?˅?��#?���?�b?�
=?�
=?��y?��y?��y?Ƈ+?ļj?�9X?�M�?�%?�\)?���?�V?�{?�V?�v�?��-?��A�?}A�;dA�;dA�;dA�;dA�A�A�A�A�A�A�?}A�?}A�A�A�C�A�C�A�C�A�C�A�C�A�E�A�C�A�E�A�E�A�A�A�E�A�E�A�E�A�G�A�E�A�G�A�G�A�I�A�G�A�I�A�M�A�K�A�K�A�K�A�I�A�M�A�I�A�I�A�M�A�O�A�K�A�I�A�Q�A�S�A�S�A�Q�A�S�A�Q�A�Q�A�O�A�A�A�;dA�33A�5?A�5?A�5?A�5?A�E�A�?}G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                 A�A�A�I�A�K�A�M�A�M�A�S�A�S�A�K�A�7LA�5?A�;dA�;dA���A��A���A��A���A��A���A�%A���A�^5A�1'A�(�A��#A�+A�+A��mA��PA�{A��^A���A��7A�|�A�bNA�
=A��7A��A���A�v�A��;A�n�A�33A��^A�?}A��A�ƨA��DA�+A�ƨA�;dA�x�A��A�"�A��A��DA�p�A��A���A���A��A���A��uA�~�A�p�A�E�A�%A��-A�\)A�+A�
=A��A�$�A�jA��wA�ffA�$�A��DA�t�A�(�A��A�+A�?}A�&�A�33A�I�A��FA��A�&�A�S�A�VA�n�A�oA�bA�A�n�A���A�JA�K�A�=qA�dZA�oA��FA�dZA��wA�1'A��^A���A�n�A��A�?}A���A���A��PA�A�$�A��#A�JA�A�A�XA��uA�ȴA~ĜA}33A{��Az�Az�AyƨAt �Al�Aj�AjbAidZAh��Ag�;Ag?}AfVAe�hAd��Adr�Ab��AaC�A`ZA]��AY�^AWO�AT{AQVAOXANv�AL��AK33AH�yAF�RAEx�AC�;AAA?�TA?�7A>5?A=S�A<9XA:��A9�;A8r�A6~�A5G�A4Q�A4�A3�7A2�A2Q�A/A-�A,�\A+K�A*�A*5?A)��A)hsA(��A';dA&-A%�wA$^5A#?}A"�A"�/A"��A"I�A"  A!33A�AG�A��A�AoAr�A�A��A��AbNA�A�A��A"�A �A�hA�yA �A�^A
��A
�jA
�A	�;AffAM�A$�A�TA�A�AK�AC�A�AK�AE�A�^AM�A I�@���@�?}@��j@��@���@��@���@��`@�1'@�=q@�j@@�@��
@��H@�@�j@�A�@�F@�dZ@��@�n�@�7L@㝲@�R@◍@�\@�Z@�M�@��@�+@�S�@�t�@��@��R@�r�@�j@���@�ȴ@��h@�/@��@�C�@�G�@��@�9X@���@�&�@�r�@�  @�Ĝ@���@�hs@��@�
=@��@���@���@��@���@��@�E�@��#@��@���@�|�@�l�@�S�@���@���@��
@��@�$�@�9X@���@��y@��+@���@�%@�Q�@��\@���@�G�@��9@�@~V@{��@z~�@y&�@vV@sƨ@q7L@nv�@l�@k�F@kS�@iX@g��@fff@e@d9X@c"�@aG�@` �@_�P@^��@]�@]V@[��@Y�@W�P@W+@Vff@U��@U/@R�\@Q�^@Q%@O��@NE�@M��@LI�@Ix�@I&�@HA�@F��@E�T@DI�@A�7@?�w@>��@>E�@<��@:��@:J@9��@8��@8 �@6�y@3��@3o@1�@1X@/\)@.{@-�T@-�-@-�h@,��@+�m@*J@)x�@(��@'�@&��@$I�@#C�@"��@"n�@"J@!��@!X@ �`@��@�R@5?@�h@�@�D@o@^5@=q@G�@Q�@ff@@�@S�@��@  @;d@ȴ@�+@�@/@�D@33@
-@
-@	�#@	�@�@�;@;d@��@��@�T@p�@�@�/@z�@1@M�@G�@ �9@   ?��?�p�?�C�?��+?�t�?��;?�|�?�{?��m?ꟾ?�r�?�?��?�F?�t�?��?ޗ�?�1?�x�?���?�l�?��y?�?��/?��?�&�?�bN?��;?��?�{?�V?���?�(�?��m?˅?��#?���?�b?�
=?�
=?��y?��y?��y?Ƈ+?ļj?�9X?�M�?�%?�\)?���?�V?�{?�V?�v�?��-?��A�?}A�;dA�;dA�;dA�;dA�A�A�A�A�A�A�?}A�?}A�A�A�C�A�C�A�C�A�C�A�C�A�E�A�C�A�E�A�E�A�A�A�E�A�E�A�E�A�G�A�E�A�G�A�G�A�I�A�G�A�I�A�M�A�K�A�K�A�K�A�I�A�M�A�I�A�I�A�M�A�O�A�K�A�I�A�Q�A�S�A�S�A�Q�A�S�A�Q�A�Q�A�O�A�A�A�;dA�33A�5?A�5?A�5?A�5?A�E�A�?}G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                 ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B2-B1'B1'B2-B1'B1'B1'B1'B0!B1'B1'B0!B/B0!B,B0!B0!B8RB;dB=qBD�BG�BM�BP�BZBjB�=B�uB��B�jB��B��B��B��B��B�BB�B��B��BBVB�B�B%�B33B;dBA�BE�BI�BP�BJ�BG�BM�BS�BP�BO�BL�BL�BS�B^5Bz�B�+B�DB�JB�\B��B��B��B��B��B��B�B�B�B��B��B��B��B��B��B��B��B��B�{B�1BdZBK�BN�BZBv�B_;BG�B2-BuB��B�#B��BB�?B��B��B�Bl�BS�B>wB6FB0!B-B)�B!�B�B{BoB\B+B
��B
��B
��B
�B
p�B
gmB
W
B
C�B
7LB
+B
%�B
#�B
�B	�5B	�jB	��B	��B	��B	��B	�uB	�VB	�1B	�B	� B	}�B	o�B	ffB	]/B	E�B	,B	�B�B��BɺBȴBÖB�RB��B��B��B��B�\B�PB�=B�%B�B�=B�JB�DB�1B�%B�+B�7B�+B�Bn�B{�B{�Bv�Bt�Br�Bo�Bm�Bk�BhsBe`BbNBdZBaHB_;B^5B^5B]/B\)B\)B[#BYBYB[#BXBT�BQ�BM�BJ�BG�BI�BI�BH�BH�BL�BM�BJ�BK�BK�BI�BE�BG�BE�BD�BC�BK�BO�BQ�BR�BR�BR�BXBXBQ�BO�BYBZBO�BF�BE�BC�BE�BK�BYBe`BhsBhsBffBm�Bp�Bp�Bp�Bo�Bm�Bl�Bl�Bk�Bk�BjBk�Bm�Br�By�B|�B|�B|�B�JB�\B�1B|�By�B}�B��B��B�}B��B�B	%B	&�B	-B	1'B	I�B	R�B	aHB	cTB	e`B	m�B	x�B	�+B	��B	�'B	�RB	�}B	ĜB	��B	��B	��B	��B	�
B	�NB	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B
  B
B	��B
B
B
%B
1B
DB
DB
DB
DB
DB
PB
\B
bB
oB
{B
�B
�B
�B
�B
�B
"�B
"�B
!�B
#�B
%�B
%�B
&�B
(�B
)�B
,B
.B
/B
/B
.B
0!B
1'B
49B
5?B
5?B
7LB
7LB
7LB
;dB
<jB
;dB
=qB
>wB
?}B
@�B
B�B
B�B
C�B
E�B
F�B
G�B
I�B
J�B
J�B
K�B
M�B
M�B
M�B
N�B
M�B
K�B
K�B
N�B
N�B
N�B
O�B
R�B
T�B
S�B
T�B
T�B
VB
XB
ZB
ZB
[#B
\)B
^5B
_;B
`BB
`BB
`BB
aHB
aHB
aHB
bNB
aHB
cTB
bNB
bNB
cTB
dZB
e`B
ffB
e`B
gmB
gmB
jB
iyB
jB
m�B
p�B
p�B
q�B
q�B
q�B
r�B
s�B
t�B
u�B
w�B
v�B
v�B
w�B
x�B
x�B
y�B
z�B
y�B
z�B
{�B
|�B
|�B
|�B
}�B
� B
�B
�B
�B
�B
�B
�B
�7B
�DB
�JB
�DB
�DB
�PB
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
�B
�B
�B
�B
�!B
�!B
�-B
�-B
�3B
�3B
�?B
�?B1'B2-B1'B2-B33B2-B2-B2-B1'B2-B2-B1'B1'B2-B2-B1'B1'B1'B2-B1'B2-B1'B2-B1'B0!B2-B1'B1'B1'B2-B2-B1'B1'B1'B1'B1'B0!B1'B2-B2-B1'B0!B2-B1'B1'B1'B2-B1'B1'B2-B1'B1'B0!B0!B1'B1'B1'B1'B1'B/G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                 B2B0�B0�B2B0�B0�B0�B0�B/�B1 B1 B/�B.�B/�B+�B/�B/�B8/B;BB=OBD{BG�BM�BP�BY�BjaB�B�XB��B�NB̱B��B��B��B��B�)B�sB��B��B �B@BrB�B%�B3B;QBAwBE�BI�BP�BJ�BG�BM�BS�BP�BO�BL�BL�BS�B^*Bz�B�!B�;B�AB�TB��B��B��B��B��B��B�B�B�B��B��B��B��B��B��B��B��B��B�}B�4Bd]BK�BN�BZ"Bv�B_ABG�B24B|B��B�+B��BB�HB��B��B�#Bl�BTB>�B6RB0-B-B*
B!�B�B�BBlB<B
��B
��B
��B
�B
p�B
g�B
WB
C�B
7`B
+B
%�B
#�B
�B	�KB	��B	�B	��B	��B	��B	��B	�oB	�KB	�3B	�B	~B	o�B	f�B	]LB	E�B	,%B	�B�B�B��B��BõB�rB�B��B��B��B�~B�rB�`B�HB�0B�aB�oB�iB�WB�KB�RB�^B�SB�:Bn�B|B|Bv�Bt�Br�Bo�Bm�Bk�Bh�Be�Bb|Bd�BawB_kB^eB^eB]`B\[B\[B[VBYJBYKB[WBXEBU3BR"BN	BJ�BG�BI�BI�BH�BH�BMBNBJ�BLBLBI�BE�BG�BE�BD�BC�BLBPBR,BS3BS3BS4BXRBXSBR/BP#BY[BZbBP$BF�BE�BC�BE�BLBY_Be�Bh�Bh�Bf�Bm�Bp�Bp�Bp�Bo�Bm�Bl�Bl�Bk�Bk�Bj�Bk�Bm�BsBz-B}AB}AB}BB��B��B��B}OBz>B~ZB�B�IB��B�?B��B	�B	'dB	-�B	1�B	J=B	SxB	a�B	c�B	e�B	n"B	yhB	��B	�9B	��B	��B	�B	�@B	�hB	яB	ђB	ԧB	׼B	�B	�=B	�FB	�\B	�wB	��B	��B	��B	��B	��B	��B
 �B
�B	��B
�B
�B
	B
	B
-B
0B
3B
5B
8B
GB
VB
^B
nB
}B
�B
�B
�B
�B
 �B
#�B
#�B
"�B
$�B
&�B
'B
(
B
*B
+#B
-2B
/@B
0JB
0MB
/HB
1XB
2aB
5uB
6~B
6�B
8�B
8�B
8�B
<�B
=�B
<�B
>�B
?�B
@�B
A�B
C�B
C�B
D�B
G	B
HB
IB
K)B
L3B
L5B
M>B
OLB
OOB
ORB
PZB
OWB
MMB
MPB
PdB
PgB
PiB
QrB
T�B
V�B
U�B
V�B
V�B
W�B
Y�B
[�B
[�B
\�B
]�B
_�B
`�B
a�B
a�B
a�B
cB
c	B
cB
dB
cB
eB
dB
dB
e&B
f/B
g7B
h@B
g<B
iKB
iNB
lbB
k_B
lgB
o{B
r�B
r�B
s�B
s�B
s�B
t�B
u�B
v�B
w�B
y�B
x�B
x�B
y�B
z�B
z�B
{�B
|�B
{�B
|�B
}�B
B
	B
B
�B
�(B
�4B
�@B
�SB
�SB
�dB
�kB
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
�B
�B
�!B
�3B
�9B
�?B
�MB
�YB
�qB
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
�B
�B
�B
�B
�)B
�/B
�5B
�;B
�@B
�]B
�kB
��B
��B
��B
��B
��B
��B
�B
�B
�+B
�;B0�B2B0�B2B3	B2B2B2B0�B2B2B0�B0�B2B2B0�B0�B0�B2B0�B2B0�B2B0�B/�B2B0�B0�B0�B2B2B0�B0�B0�B0�B0�B/�B0�B2B2B0�B/�B2B0�B0�B0�B2B0�B0�B2B0�B1 B/�B/�B1 B1 B1 B1 B1 B.�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                 <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�201904290700392021061413561320210614135613202106171314272021061713142720210617131427201904290700392021061413561320210614135613202106171314272021061713142720210617131427PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            new conductivity = original conductivity * (1 + delta*TEMP + CPcor_SBE*PRES) / (1 + delta*TEMP_ADJUSTED + CPcor_new*PRES_ADJUSTED)                                                                                                                              none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            new conductivity = original conductivity * (1 + delta*TEMP + CPcor_SBE*PRES) / (1 + delta*TEMP_ADJUSTED + CPcor_new*PRES_ADJUSTED)                                                                                                                              none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.50 dbar                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            CPcor_new = -1.25e-07; CPcor_SBE = -9.57e-08; delta = 3.25e-06                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.50 dbar                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            CPcor_new = -1.25e-07; CPcor_SBE = -9.57e-08; delta = 3.25e-06                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                      none                                                                                                                                                                                                                                                            New conductivity computed by using a different CPcor value from that provided by Sea-Bird.                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            No significant salinity drift detected.                                                                                                                                                                                                                         Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                      none                                                                                                                                                                                                                                                            New conductivity computed by using a different CPcor value from that provided by Sea-Bird.                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            No significant salinity drift detected.                                                                                                                                                                                                                         PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            AO  AO  ARCAARCAADJPADJP                                                                                                                                        2019042907003920190429070039  IP  IP                                G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2019042907003920190429070039QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�5F03E           703E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2019042907003920190429070039QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�0               0               PM  PM  ARSQARSQCOWGCOWGV1.1V1.1                                                                                                                                2021061713151820210617131518IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                