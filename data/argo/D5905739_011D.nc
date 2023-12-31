CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS     N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-07-24T22:02:46Z creation      
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
resolution        =���   axis      Z        `  ;�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   L4   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     `  PL   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   `�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     `  d�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     `  u$   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   ��   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     `  ��   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     `  �   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     `  �t   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     `  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   �L   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     `  �d   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                   ,   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                   4   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                   <   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                   D   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  � L   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                   �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                   �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar           HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar           HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�           HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    (   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �l   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �l   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    l   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  � lArgo profile    3.1 1.2 19500101000000  20180724220246  20210617131454  5905739 5905739 US ARGO PROJECT                                                 US ARGO PROJECT                                                 GREGORY C. JOHNSON                                              GREGORY C. JOHNSON                                              PRES            TEMP            PSAL            PRES            TEMP            PSAL                  DD  AOAO7219                            7219                            2C  2C  DD  SOLO_D_MRV                      SOLO_D_MRV                      12002                           12002                           SBE603 01Dec16                  SBE603 01Dec16                  874 874 @�f49uJ�@�f49uJ�11  @�f4�� @�f4�� @6���6��@6���6���c�-�*EN�c�-�*EN11  GPS     GPS     Primary sampling: averaged [1Hz sampling by SBE-41CP averaged in 2-dbar bins]                                                                                                                                                                                   Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 AC  AA  AA  >���?�  @   @@  @�  @�33@�  @�  A   A  A!��A@  Ac33A���A�  A���A���A�  A�  A���A���B ffB  B  B  B   B(  B0ffB8ffB@ffBHffBPffBX  B`  Bh  BpffBx��B�  B���B�  B�ffB�33B�  B�  B�ffB�  B�  B�  B���B�33B�  B���B�  B�33B�33B�33B�ffB�33B�33B�  Bܙ�B�ffB�33B�33B�33B�  B���B���B���C 33C33C33C33C�C
�C�fC�fC��C�fC��C�C  C�fC33C�C �C"33C$�C&33C(�C*�C,  C-�fC/��C2�C3�fC5�fC833C:33C<  C>L�C@33CB�CD�CE�fCH33CJ  CK�fCN33CP�CQ�fCT33CV33CX�CZL�C\L�C^�C`�Cb  Cd  Ce�fChL�Cj33Cl�Cn  Co�fCr33Ct�Cu�fCx33Cz�C{�fC~�C�&fC��C�  C�&fC��C�  C��C��C�  C��C�  C��fC�  C��C�  C��fC�  C��C�  C��fC��3C�  C��C��C�  C��fC��3C��C�  C��3C��C�&fC��C�  C�&fC��C��fC��C��C��C��3C��C�  C��3C��C�&fC��C��C��C��C��3C�  C��C��C��3C��C�  C��fC�  C��C��C��3C��C��C��C��3C��C��C�&fC��C��3C��C�  C��3C��C�&fC�  C��fC�  C��C��C�  C��C��C��3C��C�  C��fC�  C��C��C��3C��C��C��C��3C��C��C��C��3C��C��C�33C��C�  C�&fC�  C��fC��C�&fC��C�  C�  C��fC�  C��3C��fC��C�  C��3C��C�  C��fC�  C��C�33D3D��D	� D33D  D��D` D3D� DL�D�3D!S3D#�fD&s3D)fD+��D.,�D0��D3�3D6FfD8��D;� D>� DA@ DD3DF�fDI� DLY�DO3DQ��DTl�DW3DY�3D\FfD^ٚDaffDc��DfL�Dh�fDk&fDm�3Do��DrffDt��Dw33Dy��D{�fD~�D�FfD��fD���D�	�D�FfD��3D�ٚD�#3D�p D��fD�  D�C3D���D��fD��D�` D���D��fD�33D�p D�� D��D�&fD�ffD���D��3D�  D�Y�D���D�ٚD�  D�i�D��3D��3D��D�VfD�� D��3D��3D�33D�p D���D���D�fD�L�D�� D��3D��D�  D�P D��fD���D�� D�  D�P DĀ DŰ D�� D��D�C3D�vfD˩�D��fD�	�D�33D�c3Dѓ3D��fD��3D��D�FfD�s3DئfD���D���D��D�<�D�\�D�y�D���D�fD��fD���D��D�,�D�<�D�Y�D�p D�fD뙚D� D��fD��fD��fD�  D�fD�)�D�9�D�I�D�Y�D�p D��3D�� D���D���D�� D���D�� D��fE x E�E� E3E��E4�E��ED�E� ET�E� Ei�E��E	�E	�E
,�EњE�3E3E+3E�3E�fE3E  E�3EٚE�3E�3E�fE�3EњEp E|�E! E"#3E#1�E$� E%�3E&��E(c3E)k3E*�3E+��E-q�E.t�E/��E0��E2q�E3i�E4�fE5��E7P E8��E9�fE;!�E<3E?T�EB�fEE�fEH��EK� EO ERfEU�EXy�E[d�E^�fEa� Ed�Eg��Ek<�En33EqD�Et{3Ew��Ez��E}�3E�zfE��E���E�5�E��3E�;3E�՚E�jfE�� E��3E� E��3E�#3E��3E�ɚE� E�l E���E�3E�NfE��fE���E�H E�� E���E�< E�zfE���E�, E�� E��f>L��>���>���>���>���>���>���>���>���>���>���>���>���>���>���>���>���>���>���>���>L��>���>���>���>���>���?   ?��?333?L��?fff?���?���?�  ?���?�33@ff@33@&ff@333@Fff@Y��@l��@�  @���@�ff@�33@�  @���@�33@�33@�33@���@���A33A��A33A��A#33A)��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111144414141444444144441444411111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                ?L��?�  @   @`  @�  @�33@�  @�  A  A  A)��AH  Ak33A���A�  A���A���A�  A�  A���A���BffB
  B  B  B"  B*  B2ffB:ffBBffBJffBRffBZ  Bb  Bj  BrffBz��B�  B���B�  B�ffB�33B�  B�  B�ffB�  B�  B�  B���B�33B�  B���B�  B�33B�33B�33B�ffB�33B�33B�  Bݙ�B�ffB�33B�33B�33B�  B���B���B���C �3C�3C�3C�3C��C
��CffCffCL�CffCL�C��C� CffC�3C��C ��C"�3C$��C&�3C(��C*��C,� C.ffC0L�C2��C4ffC6ffC8�3C:�3C<� C>��C@�3CB��CD��CFffCH�3CJ� CLffCN�3CP��CRffCT�3CV�3CX��CZ��C\��C^��C`��Cb� Cd� CfffCh��Cj�3Cl��Cn� CpffCr�3Ct��CvffCx�3Cz��C|ffC~��C�ffC�L�C�@ C�ffC�L�C�@ C�Y�C�Y�C�@ C�L�C�@ C�&fC�@ C�L�C�@ C�&fC�@ C�Y�C�@ C�&fC�33C�@ C�L�C�Y�C�@ C�&fC�33C�Y�C�@ C�33C�L�C�ffC�Y�C�@ C�ffC�L�C�&fC�L�C�Y�C�L�C�33C�L�C�@ C�33C�L�C�ffC�Y�C�L�C�Y�C�L�C�33C�@ C�Y�C�L�C�33C�L�C�@ C�&fC�@ C�Y�C�L�C�33C�L�C�Y�C�L�C�33C�L�C�Y�C�ffC�L�C�33C�L�C�@ C�33C�L�C�ffC�@ C�&fC�@ C�Y�C�Y�C�@ C�Y�C�L�C�33C�L�C�@ C�&fC�@ C�Y�C�L�C�33C�L�C�Y�C�L�C�33C�L�C�Y�C�L�C�33C�L�C�L�C�s3C�Y�C�@ C�ffC�@ C�&fC�L�C�ffC�Y�C�@ C�@ C�&fC�@ C�33C�&fC�L�C�@ C�33C�L�C�@ C�&fC�@ C�Y�C�s3D33DٚD	� DS3D  DٚD� D33D� Dl�D�3D!s3D$fD&�3D)&fD+��D.L�D0��D3�3D6ffD9�D;� D>� DA` DD33DGfDI� DLy�DO33DQٚDT��DW33DY�3D\ffD^��Da�fDd�Dfl�Dh�fDkFfDm�3Dp�Dr�fDt��DwS3Dy��D{�fD~9�D�VfD��fD���D��D�VfD��3D��D�33D�� D��fD� D�S3D���D��fD�)�D�p D���D�fD�C3D�� D�� D���D�6fD�vfD���D��3D�0 D�i�D���D��D�0 D�y�D��3D��3D�,�D�ffD�� D��3D�3D�C3D�� D���D���D�&fD�\�D�� D��3D���D�0 D�` D��fD�ɚD�  D�0 D�` DĐ D�� D�� D��D�S3DʆfD˹�D��fD��D�C3D�s3Dѣ3D��fD�3D�)�D�VfD׃3DضfD���D�	�D�,�D�L�D�l�D߉�Dਗ਼D��fD��fD��D��D�<�D�L�D�i�D� D�fD멚D�� D��fD��fD��fD� D�&fD�9�D�I�D�Y�D�i�D�� D��3D�� D���D���D�� D�ɚD�� D��fE � E	�E� E#3E��E<�E��EL�E� E\�E� Eq�E��E�E	$�E
4�EٚE�3E3E33E�3E�fE3E( E�3E�E�3E�3E�fE�3EٚEx E��E! E"+3E#9�E$� E%�3E&��E(k3E)s3E*�3E+��E-y�E.|�E/��E0��E2y�E3q�E4�fE5��E7X E8��E9�fE;)�E<3E?\�EB�fEE�fEH��EK� EO  ERfEU�EX��E[l�E^�fEa� Ed��Eg��EkD�En;3EqL�Et�3Ew��Ez��E}�3E�~fE��E���E�9�E��3E�?3E�ٚE�nfE�� E��3E� E��3E�'3E��3E�͚E� E�p E���E�3E�RfE��fE���E�L E�� E���E�@ E�~fE���E�0 E�� E��f?333G�O�G�O�G�O�?L��G�O�?L��G�O�?L��G�O�G�O�G�O�G�O�G�O�G�O�?L��G�O�G�O�G�O�G�O�?333G�O�G�O�G�O�G�O�?L��?�  ?���?���?�ff?�33?���?ٙ�@   @ff@��@&ff@333@Fff@S33@fff@y��@�ff@�  @���@�ff@�33@�  @ə�@�33@�33@�33@���A��A33A��A33A!��A+33A1��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111144414141444444144441444411111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                @ @ �@ �@ {@ O@ "�@ (�@ /�@ 6�@ =q@ D�@ Q�@ `�@ n�@ z�@ �7@ ��@ ��@ �~@ ��@ �|@ �#@ �@ ��@j@@�@-@:�@H]@V@c�@p�@~K@��@�H@��@��@��@�7@�;@�4@�,@�@�@""@/�@=q@Ji@Yn@ff@s_@��@�@��@�Y@��@ƨ@�O@�H@��@��@
�@�@&;@33@@,@M�@Z�@k.@x�@�|@�#@�@��@�@ȴ@խ@�@��@ �@�@�@*S@7L@D�@SI@`B@n�@{�@�7@�0@�(@�!@��@�@��@�y@� @j@@ @-@:�@F�@V�@b�@o�@�@��@��@��@�F@�>@�C@��@�4@��@�@{@!s@1�@>�@K�@X�@e�@uk@�d@��@�a@�Y@��@ƨ@խ@��@��@��@
�@�@&�@4�@@�@O0@\)@hs@ww@��@��@�@�@�@�c@խ@�@�Y@ �@@O@'�@5�@E�@Q�@^�@m�@|�@��@�0@��@�-@��@�|@��@��@�@	@	@	
@	-@	<@	I@	V@	dZ@	qS@	}�@	��@	��@	��@	�9@	�>@	�7@	܀@	�@	��@
�@
�@
"�@
1'@
>@
Ji@
Yn@
g�@
v@
�d@
��@
��@
��@
��@
ƨ@
խ@
�H@
�@
��@�@B@%�@4�@A�@M�@\�@i�@v@�@�$@�@�f@�k@��@׹@�@�@^@V@�@)�@7L@F�@SI@_�@oF@z�@�+@��@��@��@�&@��@�@�@�@@�@�@+�@:�@G�@S�@b�@r@�@�@^�@��@�e@@�@�D@Ӡ@[@ff@�@��@5?@{�@�2@�@M�@�$@��@'�@qS@�@�@Q�@�@�(@7L@��@�@�@^�@��@��@8�@~�@�J@
�@O�@��@��@*@Wb@��@�#@�@^5@��@׹@�@]�@��@�m@+@n�@��@�9@A�@��@�*@{@Yn@��@�@+@p�@��@��@ A�@ �@ �c@!J@!O�@!�#@!�@"�@"_�@"�z@"�m@#+@#p�@#��@#��@$>@$�@$��@%�@%H]@%�7@%�|@&@&S�@&��@&�h@'�@'\)@'��@'��@(""@(b�@(�4@(�@)(�@)i�@)��@)�@*,`@*m:@*�f@*�@+1'@+r�@+��@+�e@,3�@,t�@,��@,� @-7L@-v@-�F@-�q@.7�@.v�@.��@.�@/2�@/o�@/��@/�(@0&�@0dZ@0�(@0�/@1�@1T�@1�i@1��@21@2B�@2~K@2��@2�@3-�@3i�@3�4@3��@4�@4S�@4��@4�c@5@5=q@5r�@5��@5�@6 �@6\)@6��@6�7@7
�@7G�@7�@7��@7�9@85@@8oF@8��@8�T@9�@9Yn@9��@:
�@:�W@:�e@;��@< @<��@=�@=��@>C�@>�@?33@?�@@\�@@��@A�|@A��@Br@B��@C��@DJ@D�R@E-�@E�@FK@F�@G.l@G�h@HI@H�L@I^5@J �@JoF@K@K�W@L"�@L��@M+�@M�<@N6�@N�C@O<�@O׹@P;d@Q�z@S�@T^�@U��@V�@X\)@Y�(@Z��@\\�@]��@_%@`G�@a��@b�T@d^5@e��@f��@hO�@i�z@j�@l4�@m�y@n�q@p7L@q�	@r��@t@,@u�a@v��@xDD@y��@z�@|M�@}��@}�@~�@~UU@~�A@~ލ@2�@hr@�^@�@��@�E�@�_�@���@��s@��@��:@�{@�)�@ ^G�O�G�O�G�O�@ G�O�@ G�O�@ G�O�G�O�G�O�G�O�G�O�G�O�@ G�O�G�O�G�O�G�O�@ ^G�O�G�O�G�O�G�O�@ @ j@ @ �@ v@ %@ �@ 1@ 
=@ 
�@ �@ V@ �@ �@ @ *@ 6@ B@ O@ [@  @ "�@ %�@ '�@ )�@ -@ 0x@ 2�@ 5?@ 7�@ <@ >�@ A�@ E�@ H]G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�Q�A�ffA�p�A�v�Aǉ7Aǉ7AǍPAǋDAǍPAǍPAǋDAǇ+AǃA�hsA��A�jA�7LA��A��/AōPA�1AÛ�A£�A��A��A�G�A��A���A��^A��`A��A�-A��-A�-A�I�A��mA��#A�jA��FA�{A�|�A��A��A�n�A�~�A��A�dZA�A���A�M�A�ȴA��RA��-A�`BA��wA�  A�v�A�1'A���A�Q�A��jA� �A��A���A�^5A��TA�jA���A�ƨA�A�A��wA�dZA�jA��mA��^A��PA�A�K�A��RA�bNA�jA��A���A�JA��A�/A�{A�A��A��-A�(�A�S�A�ffA�A��RA��A��^A��/A�?}A���A�ZA�l�A�+A��A�~�A�=qA�r�A�/A�A���A��FA�Q�A�/A���A��DA��\A~��A~-A}��A}XA}7LA|�yA|9XA{XAz�/Az��Ay�Aw�mAs�PAq�mAp��Ao�Am�AlE�Aj~�Ai|�AhA�Ag�Ae��Ab�HA`��A`JA_t�A^z�A\�DA[dZAZ�AX�!AW�AU��AS�PARZAQ;dAOp�AM�TAJ�RAG;dAFE�AD�uACXAA��AA�7AAO�AA�A@{A?33A>{A="�A<n�A;��A:�jA9ƨA8��A7��A6�yA6-A4�HA4^5A3��A3x�A2�DA1�A0�A/ƨA.��A-&�A+�
A*�\A)�A(��A'ƨA'&�A&VA%��A%O�A%�A#7LA ��A bAVA�+A��AoA~�AoA=qA�A9XAG�A%A�A��At�A��A�^AQ�A�TAO�A=qA��A9XA|�A33A
v�A
bA	A=qA��AA�A/A�+A�wA33A��A�hA ��A ��A M�@�l�@���@���@��w@��@�|�@�E�@�J@��T@���@��^@�@�\@�@�(�@�x�@��@�
=@�$�@�hs@�C�@�  @�Ĝ@�9X@�M�@�1'@�-@�=q@��D@�r�@�S�@�~�@�ȴ@���@�hs@��@�%@��@��9@��9@�t�@�5?@��@�7L@� �@��@�G�@��-@���@�A�@�Ĝ@��9@�A�@�\)@��\@�^5@�%@��
@�v�@���@��@�V@��#@�?}@�(�@�\)@�v�@�p�@�Z@}��@{C�@y��@w��@vV@t�@sƨ@p�`@oK�@m�T@m�@j��@i7L@g\)@e/@c33@a��@`�9@_|�@^��@]`B@\�@[�
@Y�#@W\)@V{@T��@S��@R^5@P��@OK�@N5?@M`B@LZ@K��@J�@I��@H�@G+@E�@EV@Cƨ@B=q@A%@@�@?
=@=�-@<�j@;�F@:�\@97L@8bN@7�P@65?@5@4�@3dZ@2~�@1hs@0�@0  @.ff@-�@+�m@*�!@)&�@(A�@&��@%?}@$I�@"��@"-@ A�@�@��@/@��@��@�#@�`@ �@\)@@�j@ƨ@S�@n�@��@7L@��@A�@\)@V@{@@/@�@Z@�@@	�^@	%@r�@�@�@�y@�R@E�@��@��@��@��@z�@ƨ@@^5@��@X@ ��@ �9@ A�@  �?�{?�1?�=q?��?�?�9X?�33??�5??��?�?�r�?��?�z�?���?��?��?�5??�(�?�"�?��#?�r�?�Q�?�
=?ա�?��?���?��?щ7?�%?�|�?θR?�5??�O�?�ƨ?˥�?���?�=q?���?��?�ȴ?Ƨ�?�E�?��?���?\?�Ĝ?�  ?��;?�\)?�v�?��-?��?�1?���?�dZ?�C�?�"�?�?�~�?�~�?���?���?���?�"�?���?�1?��D?��?�p�?�{?���?��?���?� �?�Ĝ?�G�?��7?���?���?���?���?���?��7?���?���?��?��?���?��?���?��7?���?��?�JA�M�A�O�A�VA�S�A�Q�A�\)A�VA�VA�S�A�O�A�M�A�K�A�M�A�M�A�O�A�M�A�K�A�M�A�M�A�K�A�K�A�VA�VA�O�A�S�A�ZA�VA�ffA�dZA�bNA�dZA�ffA�jA�ffA�dZA�hsA�x�A�z�A�z�A�r�A�p�A�~�AǇ+Aǉ7AǋDAǋDAǇ+Aǉ7AǋDAǍPAǋDAǋDAǋDAǍPAǍPAǍPAǍPAǋDAǍPAǋDG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                A�Q�A�ffA�p�A�v�Aǉ7Aǉ7AǍPAǋDAǍPAǍPAǋDAǇ+AǃA�hsA��A�jA�7LA��A��/AōPA�1AÛ�A£�A��A��A�G�A��A���A��^A��`A��A�-A��-A�-A�I�A��mA��#A�jA��FA�{A�|�A��A��A�n�A�~�A��A�dZA�A���A�M�A�ȴA��RA��-A�`BA��wA�  A�v�A�1'A���A�Q�A��jA� �A��A���A�^5A��TA�jA���A�ƨA�A�A��wA�dZA�jA��mA��^A��PA�A�K�A��RA�bNA�jA��A���A�JA��A�/A�{A�A��A��-A�(�A�S�A�ffA�A��RA��A��^A��/A�?}A���A�ZA�l�A�+A��A�~�A�=qA�r�A�/A�A���A��FA�Q�A�/A���A��DA��\A~��A~-A}��A}XA}7LA|�yA|9XA{XAz�/Az��Ay�Aw�mAs�PAq�mAp��Ao�Am�AlE�Aj~�Ai|�AhA�Ag�Ae��Ab�HA`��A`JA_t�A^z�A\�DA[dZAZ�AX�!AW�AU��AS�PARZAQ;dAOp�AM�TAJ�RAG;dAFE�AD�uACXAA��AA�7AAO�AA�A@{A?33A>{A="�A<n�A;��A:�jA9ƨA8��A7��A6�yA6-A4�HA4^5A3��A3x�A2�DA1�A0�A/ƨA.��A-&�A+�
A*�\A)�A(��A'ƨA'&�A&VA%��A%O�A%�A#7LA ��A bAVA�+A��AoA~�AoA=qA�A9XAG�A%A�A��At�A��A�^AQ�A�TAO�A=qA��A9XA|�A33A
v�A
bA	A=qA��AA�A/A�+A�wA33A��A�hA ��A ��A M�@�l�@���@���@��w@��@�|�@�E�@�J@��T@���@��^@�@�\@�@�(�@�x�@��@�
=@�$�@�hs@�C�@�  @�Ĝ@�9X@�M�@�1'@�-@�=q@��D@�r�@�S�@�~�@�ȴ@���@�hs@��@�%@��@��9@��9@�t�@�5?@��@�7L@� �@��@�G�@��-@���@�A�@�Ĝ@��9@�A�@�\)@��\@�^5@�%@��
@�v�@���@��@�V@��#@�?}@�(�@�\)@�v�@�p�@�Z@}��@{C�@y��@w��@vV@t�@sƨ@p�`@oK�@m�T@m�@j��@i7L@g\)@e/@c33@a��@`�9@_|�@^��@]`B@\�@[�
@Y�#@W\)@V{@T��@S��@R^5@P��@OK�@N5?@M`B@LZ@K��@J�@I��@H�@G+@E�@EV@Cƨ@B=q@A%@@�@?
=@=�-@<�j@;�F@:�\@97L@8bN@7�P@65?@5@4�@3dZ@2~�@1hs@0�@0  @.ff@-�@+�m@*�!@)&�@(A�@&��@%?}@$I�@"��@"-@ A�@�@��@/@��@��@�#@�`@ �@\)@@�j@ƨ@S�@n�@��@7L@��@A�@\)@V@{@@/@�@Z@�@@	�^@	%@r�@�@�@�y@�R@E�@��@��@��@��@z�@ƨ@@^5@��@X@ ��@ �9@ A�@  �?�{?�1?�=q?��?�?�9X?�33??�5??��?�?�r�?��?�z�?���?��?��?�5??�(�?�"�?��#?�r�?�Q�?�
=?ա�?��?���?��?щ7?�%?�|�?θR?�5??�O�?�ƨ?˥�?���?�=q?���?��?�ȴ?Ƨ�?�E�?��?���?\?�Ĝ?�  ?��;?�\)?�v�?��-?��?�1?���?�dZ?�C�?�"�?�?�~�?�~�?���?���?���?�"�?���?�1?��D?��?�p�?�{?���?��?���?� �?�Ĝ?�G�?��7?���?���?���?���?���?��7?���?���?��?��?���?��?���?��7?���?��?�JA�M�A�O�A�VA�S�A�Q�A�\)A�VA�VA�S�A�O�A�M�A�K�A�M�A�M�A�O�A�M�A�K�A�M�A�M�A�K�A�K�A�VA�VA�O�A�S�A�ZA�VA�ffA�dZA�bNA�dZA�ffA�jA�ffA�dZA�hsA�x�A�z�A�z�A�r�A�p�A�~�AǇ+Aǉ7AǋDAǋDAǇ+Aǉ7AǋDAǍPAǋDAǋDAǋDAǍPAǍPAǍPAǍPAǋDAǍPAǋDG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
��B
�9B
�}B
ŢB
�)B
�B
�)B
�/B
�/B
�)B
�/B
�5B
�;B
�HB
�yBBbB�B$�B6FBT�B�3B�BVB�B/B49B7LB8RB.B=qB@�BC�B@�BF�BG�BE�BM�BZB]/Bp�B�+B��B�{B��B��B�B�B�9B�'B�3B�-B�'B�!B�B�B��B�B�B�B�B��B��B��B��B��B��B�{B�{B�B�B~�B{�Bt�Bo�Bk�BdZBdZBcTB^5BW
BW
BP�B=qB/B!�B+B��B��B�B�TBŢB�?B�B��B��B�B�Bu�Bn�Bm�BS�B49BB�B8RB�B
��B
��B
��B
�fB
��B
B
��B
�oB
hsB
F�B
<jB
8RB
5?B
49B
33B
1'B
(�B
#�B
 �B
�B
�B
VB
B	�B	�sB	�TB	��B	��B	ĜB	�dB	�'B	�B	��B	�B	x�B	x�B	q�B	k�B	iyB	e`B	\)B	M�B	F�B	?}B	6FB	7LB	1'B	#�B	�B��B�yB�`B�B��BǮBĜBB��B��B�}B�}B�qB�XB�LB�FB�3B�'B�B��B��B��B��B��B��B��B��B��B�uB�JB�7B�%B�B~�B}�B{�By�B|�B{�B�B�=Bx�Bo�BjBiyBe`BbNBaHBaHB]/B_;BYBW
BVBVBR�BQ�BQ�BK�BL�BK�BJ�BH�BD�BD�BC�B?}B?}B?}B=qB:^B<jB:^B8RB8RB6FB7LB5?B49B33B33B33B2-B2-B33B2-B49B1'B0!B1'B8RB9XBA�BH�BJ�BS�BS�BS�BN�BL�BI�BF�BK�B^5BL�BVBG�BL�BL�B^5BcTBjBr�B{�B�bB��B��B�B�3B�wB��B	\B	�B	$�B	+B	8RB	;dB	S�B	bNB	jB	{�B	~�B	�VB	��B	��B	��B	�B	�^B	�jB	ÖB	ȴB	��B	�B	�B	�5B	�NB	�NB	�mB	�B	�B	�B	�B	��B	��B	��B
  B
B
B
%B
DB
PB
VB
VB
uB
{B
�B
�B
�B
�B
�B
�B
�B
 �B
"�B
#�B
%�B
'�B
(�B
)�B
+B
,B
.B
/B
0!B
1'B
2-B
33B
49B
5?B
6FB
8RB
9XB
8RB
:^B
<jB
=qB
>wB
?}B
@�B
@�B
C�B
C�B
E�B
D�B
E�B
G�B
G�B
H�B
I�B
J�B
L�B
L�B
M�B
N�B
P�B
Q�B
R�B
S�B
T�B
W
B
XB
YB
ZB
ZB
]/B
]/B
]/B
_;B
aHB
aHB
bNB
dZB
e`B
e`B
gmB
hsB
iyB
iyB
iyB
k�B
k�B
k�B
l�B
l�B
l�B
m�B
n�B
n�B
o�B
o�B
p�B
p�B
s�B
s�B
s�B
t�B
u�B
u�B
u�B
v�B
u�B
v�B
w�B
x�B
x�B
y�B
z�B
z�B
{�B
{�B
{�B
|�B
|�B
|�B
~�B
�B
�B
�B
�B
�B
�+B
�7B
�7B
�7B
�DB
�VB
�bB
�bB
�oB
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
�B
�B
�B
�B
�B
�!B
�!B
�'B
�3B
�3B
�9B
�9B
�?B
�FB
�FB
�LB
�LB
�LB
�RB
�LB
�LB
�RB
�RB
�XB
�RB
�RB
�RB
�XB
�XB
�XB
�XB
�^B
�XB
�XB
�^B
�XB
�^B
�XB
�^B
�XB
�XB
�XB
�^B
�^B
�^B
�dB
�^B
�^B
�^B
�^B
��B
��B
�B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
�3B
�3B
�LB
�B
�^B
�?B
�9B
�'B
�FB
��B
��B
�wB
��B
ĜB
��B
�#B
�)B
�)B
�#B
�B
�B
�)B
�)B
�/B
�/B
�/B
�/B
�/B
�)B
�)B
�5B
�/B
�/G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                B
��B
�B
�UB
�zB
�B
��B
�B
�B
�	B
�B
�	B
�B
�B
�$B
�VB�B@BeB$�B6%BT�B�B��B7BcB.�B4B7/B86B-�B=VB@iBC|B@jBF�BG�BE�BM�BZB]Bp�B�B�xB�gB�yB��B��B�B�'B�B�#B�B�B�B�B��B��B��B��B��B��B��B��B��B��B��B��B�tB�tB�B� B~�B{�Bt�Bo�Bk�BdXBdXBcSB^4BW
BW
BP�B=rB/B!�B.B��B��B�B�YBŧB�EB�B��B��B�'B�Bu�Bn�Bm�BTB4CBB�B8]B�B
��B
��B
��B
�sB
� B
B
�B
�~B
h�B
F�B
<zB
8bB
5PB
4KB
3EB
1:B
)	B
#�B
 �B
�B
�B
lB
B	��B	�B	�lB	�B	��B	ĵB	�}B	�AB	�(B	��B	�4B	x�B	x�B	q�B	k�B	i�B	e~B	\GB	M�B	F�B	?�B	6fB	7mB	1HB	#�B	�B��B�B�B�3B�B��B��B´B��B��B��B��B��B��B�uB�pB�]B�RB�9B�"B�	B��B�B��B��B��B��B��B��B�{B�hB�WB�DB,B~'B|BzB}#B|B�UB�sByBo�Bj�Bi�Be�Bb�Ba�Ba�B]iB_vBYRBWFBV@BVABS/BR*BR+BLBMBLBKBH�BD�BD�BC�B?�B?�B?�B=�B:�B<�B:�B8�B8�B6�B7�B5�B4�B3|B3}B3}B2xB2yB3B2yB4�B1uB0oB1uB8�B9�BA�BIBKBTJBTJBTKBO,BM!BJBF�BLB^�BM)BVcBHBM1BM5B^�Bc�Bj�Bs#B|]B��B�"B�DB��B��B��B��B	�B	B	%rB	+�B	8�B	<B	T�B	b�B	k'B	|�B	�B	�B	�HB	��B	��B	��B	� B	�/B	�^B	�B	ѳB	��B	��B	�B	�'B	�*B	�LB	�aB	�}B	��B	��B	��B	��B	��B
 �B
B
B
$B
FB
UB
^B
aB
�B
�B
�B
�B
�B
�B
�B
�B
 �B
!�B
#�B
%B
'B
)%B
*.B
+7B
,@B
-IB
/XB
0bB
1kB
2tB
3}B
4�B
5�B
6�B
7�B
9�B
:�B
9�B
;�B
=�B
>�B
?�B
@�B
A�B
A�B
EB
EB
G#B
F B
G)B
I8B
I;B
JDB
KMB
LVB
NeB
NhB
OqB
PzB
R�B
S�B
T�B
U�B
V�B
X�B
Y�B
Z�B
[�B
[�B
^�B
^�B
^�B
aB
cB
cB
dB
f-B
g6B
g8B
iHB
jQB
kYB
k\B
k_B
mmB
mpB
mrB
n{B
n~B
n�B
o�B
p�B
p�B
q�B
q�B
r�B
r�B
u�B
u�B
u�B
v�B
w�B
w�B
w�B
x�B
w�B
x�B
y�B
z�B
z�B
|B
}B
}B
~B
~B
~B
)B
,B
.B
�@B
�QB
�\B
�jB
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
�
B
�B
� B
�+B
�9B
�>B
�RB
�WB
�\B
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
�B
�B
�B
�'B
�.B
�+B
�OB
�^B
�tB
��B
��B
��B
��B
��B
��B
�B
�B
�-B
�CB
�QB
�hB
�~B
��B
��B
��B
��B
��B
��B
��B
�B
�B
�)B
�2B
�AB
�QB
�fB
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
�wB
�}B
��B
�}B
��B
�}B
�qB
�wB
�wB
��B
�qB
�qB
��B
��B
��B
��B
��B
��B
�
B
�
B
�#B
��B
�6B
�B
�B
��B
�B
ʙB
˟B
�OB
�[B
�tB
��B
��B
�B
�B
��B
��B
��B
�B
�B
�B
�B
�B
�	B
�	B
�B
�B
�B
�	B
�	G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�201807242202462021061413551820210614135518202106171311532021061713115320210617131153201807242202462021061413551820210614135518202106171311532021061713115320210617131153PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            new conductivity = original conductivity * (1 + delta*TEMP + CPcor_SBE*PRES) / (1 + delta*TEMP_ADJUSTED + CPcor_new*PRES_ADJUSTED)                                                                                                                              none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            new conductivity = original conductivity * (1 + delta*TEMP + CPcor_SBE*PRES) / (1 + delta*TEMP_ADJUSTED + CPcor_new*PRES_ADJUSTED)                                                                                                                              none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.50 dbar                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            CPcor_new = -1.25e-07; CPcor_SBE = -9.57e-08; delta = 3.25e-06                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.50 dbar                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            CPcor_new = -1.25e-07; CPcor_SBE = -9.57e-08; delta = 3.25e-06                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                      none                                                                                                                                                                                                                                                            New conductivity computed by using a different CPcor value from that provided by Sea-Bird.                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            No significant salinity drift detected.                                                                                                                                                                                                                         Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                      none                                                                                                                                                                                                                                                            New conductivity computed by using a different CPcor value from that provided by Sea-Bird.                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            No significant salinity drift detected.                                                                                                                                                                                                                         PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            AO  AO  ARCAARCAADJPADJP                                                                                                                                        2018072422024620180724220246  IP  IP                                G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2018072422024620180724220246QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�5F03E           703E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2018072422024620180724220246QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�0               0               PM  PM  ARSQARSQCOWGCOWGV1.1V1.1                                                                                                                                2021061713145420210617131454IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                