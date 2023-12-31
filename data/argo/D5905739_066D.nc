CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS     N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2019-04-19T19:00:34Z creation      
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
_FillValue                 (  Lt   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  P�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 (  a<   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  ed   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  v   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 (  ��   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 (  �l   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �4   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 (  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 (  ՜   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                   �   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                   �   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                   �   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                   �   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  � �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                   l   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                   �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar        �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar        �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�       �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  �d   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                      	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  � Argo profile    3.1 1.2 19500101000000  20190419190034  20210617131517  5905739 5905739 US ARGO PROJECT                                                 US ARGO PROJECT                                                 GREGORY C. JOHNSON                                              GREGORY C. JOHNSON                                              PRES            TEMP            PSAL            PRES            TEMP            PSAL               B   BDD  AOAO7219                            7219                            2C  2C  DD  SOLO_D_MRV                      SOLO_D_MRV                      12002                           12002                           SBE603 01Dec16                  SBE603 01Dec16                  874 874 @ز�c-3`@ز�c-3`11  @ز�`� @ز�`� @5��]c�f@5��]c�f�c���/��c���/�11  GPS     GPS     Primary sampling: averaged [1Hz sampling by SBE-41CP averaged in 2-dbar bins]                                                                                                                                                                                   Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 AC  AA  AA  >���?�  @ff@@  @�  @���@�33@�  A��A��A$��AA��Ad��A���A�  A�33A�  A���A�  A�  A�33B ffB��B��BffB ��B(ffB0  B7��B@ffBHffBP  BX��B`ffBhffBp  Bx  B�33B�ffB�33B�  B�  B�33B�33B�  B�  B�  B���B�33B�33B�ffB�33B�  B�33B�33BǙ�B���B�  B�ffB�  Bۙ�B�  B�ffB虚B�ffB�  B�33B�ffB�33B���C�C33C  C��C	�fC�C33CL�C33C�fC�C33CffC33C  C 33C"  C#�fC&�C(  C)�fC,�C.L�C0L�C2�C4  C5��C7�fC:�C<L�C>�C?�fCB�CD33CF�CG��CJ�CLL�CN33CP  CR  CS��CV  CX33CZ  C[��C^33C`  Ca�fCd33Cf  Cg�fCj�Cl�Cn  Cp33Cr33Cs�fCv  Cx33Cz�C{��C~  C��C�  C��fC�  C��C�33C��C�  C��C�&fC��C��3C��C�  C��3C�  C��C��C��3C��C�&fC��C��3C�  C��C�&fC�  C��fC��3C�  C��C��C�&fC�  C�ٚC�  C�&fC�  C��C��C��C�33C�&fC��C�&fC��C��C��C��3C��fC��C�  C�  C�&fC�&fC�33C�  C�ٚC��fC��fC��fC��3C��3C�  C��C��C�  C��fC�  C��C��C��C��3C�  C��C�  C��fC�  C��C�33C��C�  C��fC�ٚC��fC��3C��C��C�33C��C��3C��C�&fC�  C��fC��3C�  C��C�  C��fC��3C��C��C�&fC�33C��C��3C��fC��3C�  C��C�&fC��C�&fC��C��fC��fC�ٚC�ٚC�  C�&fC�&fC��3C�&fC��C��C��C�&fD �D� D&fD	�3D�D� D  D�fD33D��D` D3D � D#Y�D&�D(�3D+Y�D.  D0�fD3@ D5��D8Y�D:�fD=ffD?��DBs3DD�fDGL�DI��DL,�DN�fDP� DS@ DU��DWٚDZ&fD\ffD^�3D`��Dc@ De�3Dg�fDj33Dl��Dn��Dq@ Ds�3Du�3DxY�Dz��D|��D�D�ɚD�fD�@ D�|�D���D�  D�<�D�|�D�ɚD�fD�FfD��3D��3D�  D�<�D�|�D�� D��3D�&fD�` D���D���D�� D�  D��D�33D�C3D�S3D�` D�c3D�p D�� D��fD�� D��fD�� D���D��3D���D���D���D��fD��3D�3D�3D�)�D�I�D�ffD��3D��3D��3D��fD�	�D�,�D�VfD�|�D�� D���D�� D���D��D�C3D�i�DȐ Dɰ D�� D�� D� D�0 D�P D�i�D�|�DҖfDӬ�D��3D���D�� D�3D�fD�#3D�,�D�33D�9�D�33D�9�D�33D�,�D�)�D��D�3D�	�D�	�D� D� D�fD�3D��fD�� D��3D�ٚD�� D��fD�fD��D� D�3D��D�� D��3D�vfD�l�D�ffD�\�D�@ D�6fD�33D�,�D�&fE fE �3E	�E��E��E��E�EfE��E�fE
�E!�E�3E�3E�El�E[3E��EfE�3EH E��E��E�E4�E�3E�3E)�E L�E!t�E#�E$;3E%q�E&� E'�fE)0 E*q�E+�fE,�E..fE/ffE0� E1ɚE3X E4� E5�fE6� E8,�E9;3E:��E;��E?fEBA�EEd�EH>fEK�fENs3EQ��ET��EW�E[ E^9�EaVfEdK3Eg��Eh0 Eh��EiQ�Ej,�Ej� EkFfEkњEl�3Em#3Em��En{3EofEo��EpK3Ep� Eq� Er�Er� Es` Et�Et�3EuL�Eu� Ev��Ew�Ew�3ExVfEyfEy�3Ez( Ez� E{��E|�E|њE}K3E}�f>���>���>���>L��>L��>���>L��>L��>L��>L��>���>L��>���>���>���>L��>���>L��>���>���>���>���>���>���>���?   ?��?333?fff?fff?���?���?�  ?���@   @33@��@333@@  @L��@fff@y��@���@�33@���@���@�ff@�33@���@ٙ�@�ff@�33A��A  AffA��A��A$��A+33A333G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111444414444141444141444144111141111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        ?L��?�  @&ff@`  @�  @���@�33@�  A	��A��A,��AI��Al��A���A�  A�33A�  A���A�  A�  A�33BffB
��B��BffB"��B*ffB2  B9��BBffBJffBR  BZ��BbffBjffBr  Bz  B�33B�ffB�33B�  B�  B�33B�33B�  B�  B�  B���B�33B�33B�ffB�33B�  B�33B�33Bș�B���B�  B�ffB�  Bܙ�B�  B�ffB陚B�ffB�  B�33B�ffB�33C ffC��C�3C� CL�C
ffC��C�3C��C�3CffC��C�3C�fC�3C� C �3C"� C$ffC&��C(� C*ffC,��C.��C0��C2��C4� C6L�C8ffC:��C<��C>��C@ffCB��CD�3CF��CHL�CJ��CL��CN�3CP� CR� CTL�CV� CX�3CZ� C\L�C^�3C`� CbffCd�3Cf� ChffCj��Cl��Cn� Cp�3Cr�3CtffCv� Cx�3Cz��C|L�C~� C�Y�C�@ C�&fC�@ C�Y�C�s3C�Y�C�@ C�Y�C�ffC�L�C�33C�L�C�@ C�33C�@ C�Y�C�L�C�33C�L�C�ffC�Y�C�33C�@ C�L�C�ffC�@ C�&fC�33C�@ C�L�C�Y�C�ffC�@ C��C�@ C�ffC�@ C�L�C�Y�C�L�C�s3C�ffC�Y�C�ffC�Y�C�L�C�L�C�33C�&fC�L�C�@ C�@ C�ffC�ffC�s3C�@ C��C�&fC�&fC�&fC�33C�33C�@ C�L�C�Y�C�@ C�&fC�@ C�L�C�Y�C�L�C�33C�@ C�Y�C�@ C�&fC�@ C�Y�C�s3C�Y�C�@ C�&fC��C�&fC�33C�L�C�Y�C�s3C�Y�C�33C�L�C�ffC�@ C�&fC�33C�@ C�L�C�@ C�&fC�33C�Y�C�Y�C�ffC�s3C�Y�C�33C�&fC�33C�@ C�L�C�ffC�Y�C�ffC�L�C�&fC�&fC��C��C�@ C�ffC�ffC�33C�ffC�Y�C�Y�C�Y�C�ffD ,�D� DFfD	�3D9�D� D@ D�fDS3DٚD� D33D � D#y�D&,�D(�3D+y�D.  D0�fD3` D5��D8y�D;fD=�fD@�DB�3DEfDGl�DIٚDLL�DN�fDQ  DS` DU��DW��DZFfD\�fD^�3Da�Dc` De�3DhfDjS3Dl��Do�Dq` Ds�3Dv3Dxy�Dz��D|��D9�D�ٚD�fD�P D���D���D� D�L�D���D�ٚD�fD�VfD��3D��3D� D�L�D���D�� D�3D�6fD�p D���D���D�� D� D�,�D�C3D�S3D�c3D�p D�s3D�� D�� D��fD�� D��fD�� D���D��3D���D���D���D��fD�3D�3D�#3D�9�D�Y�D�vfD��3D��3D��3D��fD��D�<�D�ffD���D�� D�ɚD�� D��D�,�D�S3D�y�DȠ D�� D�� D�  D�  D�@ D�` D�y�Dь�DҦfDӼ�D��3D���D�  D�3D�&fD�33D�<�D�C3D�I�D�C3D�I�D�C3D�<�D�9�D�,�D�#3D��D��D�  D�  D�fD�3D�fD�  D��3D��D�� D��fD��fD��D� D�3D���D�� D��3D��fD�|�D�vfD�l�D�P D�FfD�C3D�<�D�6fE fE �3E�E��E��E��E�EfE��E�fE
!�E)�E�3E�3E	�Et�Ec3E��EfE3EP E��E��E�E<�E�3E3E1�E T�E!|�E#�E$C3E%y�E&� E'�fE)8 E*y�E+�fE,��E.6fE/nfE0� E1њE3` E4� E5�fE6� E84�E9C3E:��E;��E?&fEBI�EEl�EHFfEK�fEN{3EQ��ET��EW�E[  E^A�Ea^fEdS3Eg��Eh8 EhɚEiY�Ej4�Ej� EkNfEkٚEl�3Em+3Em��En�3EofEo��EpS3Ep� Eq� Er$�Er� Esh Et�Et�3EuT�Eu� Ev��Ew�Ew�3Ex^fEy&fEy�3Ez0 Ez� E{��E|!�E|ٚE}S3E}�fG�O�G�O�G�O�G�O�?333G�O�G�O�G�O�G�O�?333G�O�?333G�O�G�O�G�O�?333G�O�?333G�O�G�O�G�O�?L��G�O�G�O�?fff?�  ?���?���G�O�?�33?���?ٙ�@   @ff@   @333@9��@S33@`  @l��@�33@���@���@�33@���@���@�ff@�33@���@陚@�ffA��A	��A  AffA��A$��A,��A333A;33G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111444414444141444141444144111141111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        @ @ �@ V@ {@ O@ !s@ )�@ /�@ 7L@ >@ FQ@ R�@ a�@ m�@ z�@ ��@ �0@ ��@ �~@ �&@ �@ �#@ �y@ � @@o@g@,`@9X@H]@V@b�@r@~�@��@��@�A@��@��@��@��@�@��@�@{@""@/�@<�@K�@Yn@g�@t�@��@�@��@�M@��@��@��@�H@�@��@�@�@&�@33@A�@O�@\�@i!@x&@�|@��@�@�f@�k@��@�@�@�@ �@@
@*S@6�@E�@Q�@^�@m�@z�@��@��@��@��@��@��@�@�m@�q@v@�@
@-@;d@H]@S�@c�@r�@�@��@��@��@��@��@�7@܀@��@�,@%@�@""@/@>@K�@X�@g�@uk@�@�\@�a@�Y@��@��@��@�H@�@��@�@�@&�@33@B8@P�@\�@i!@x&@�@�@�m@�r@�k@ȴ@׹@�@�@�Q@�@�@+@6�@B�@Q=@_�@m�@|?@��@�0@��@�~@�2@��@�#@�y@�q@	%@	@	 @	.l@	;d@	H]@	V@	bN@	oF@	~�@	��@	��@	�M@	��@	�J@	�7@	��@	�(@	��@
v@
�@
!s@
/�@
>@
Lu@
X�@
e	@
t@
�d@
��@
��@
��@
�R@
�W@
Ӡ@
��@
��@
��@�@B@%�@1�@>�@M$@[z@j@x�@��@�$@��@��@��@�c@խ@�@�Y@ �@�@�@(G@7�@E�@S�@bN@n�@z3@�+@��@��@�-@�2@�*@܀@��@�e@@@�@,`@<@I�@T�@e	@r@�@�P@��@��@(�@j@��@�@6�@z�@��@v@Ji@��@܀@%�@l�@�F@��@F�@�\@׹@�@dZ@��@�@3�@z3@��@ �@B8@�p@�W@�@G�@��@�W@%@D�@�d@�2@�Q@=q@|�@�j@�9@;d@|?@��@�9@<@}�@�@�@5�@y�@�@  @C�@��@��@b@T�@��@�;@#�@g@�Y@��@2�@v�@��@�E@ >�@ ��@ ��@!�@!@�@!~K@!�@!�q@"0x@"j@"��@"�#@#{@#N�@#�|@#�&@#� @$/�@$hs@$�@$��@%�@%M�@%�|@%��@%��@&3�@&oF@&��@&�y@'&;@'c�@'�@'�;@([@([z@(��@(��@)�@)S�@)��@)ψ@*�@*K�@*��@*�c@+�@+DD@+��@+�&@+��@,:@,v@,��@,��@-(G@-c�@-��@-�t@.*@.O�@.�7@.��@.��@/1�@/g@/�@/�O@0	�@0?}@0s_@0��@0܀@1@1K@1��@1�F@1�4@2 @2UU@2�7@2��@2�Y@3&�@3Z@3��@3@3�q@4+�@4b�@4��@4��@4�Q@54�@5i!@5��@5�*@6@69X@6n�@6�(@6�h@7V@7D�@7��@8 �@8Ĝ@92�@9�@:F�@:�4@;\�@;��@<dZ@=@=�@>�@>��@?/@?��@@�@@�M@A1�@A��@B8�@B�@Cg�@C��@De	@D�T@E�i@Fo@F��@G�@G��@H/�@H�@I@,@I�@JQ=@J�[@K\)@K�#@L�@Mj@M}�@M��@N��@O1@O�z@P*@Q�C@R��@T8�@Uo�@V��@X�@Yr�@Z��@\�@]z�@^��@`$�@ag�@b�O@c@cO1@c��@c�(@d%�@dbN@d��@d�@e-�@ej@e��@e��@f3�@f�|@f�&@g{@gM$@g�m@g�
@h"�@hV�@h�M@h�H@i3�@ik�@i�@i�@jJi@j�@j��@k
=@kZ�@k�@kލ@lo@lM�G�O�G�O�G�O�G�O�@ ^G�O�G�O�G�O�G�O�@ ^G�O�@ ^G�O�G�O�G�O�@ ^G�O�@ ^G�O�G�O�G�O�@ G�O�G�O�@ �@ j@ @ �G�O�@ %@ �@ 1@ 
=@ 
�@ �@ �@ b@ @ {@ �@ �@ �@ 
@ g@ !s@ $.@ &�@ )�@ +�@ .l@ 1'@ 3�@ 7L@ :@ <�@ ?}@ B�@ FQ@ I@ LuG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�^5A�\)A�O�A�ZA�bNA�jA�n�A�~�A�~�A��A��A�z�A�t�A�jA�S�A�1'A��A��A���A�x�A�M�A�M�A�K�A�9XA��A�A��A���A��jA���A���A��DA�t�A�9XA�33A��A�/A�JA���A�M�A��#A��RA��hA�33A���A�+A��mA���A�;dA��A�bNA�-A��/A�XA��A���A��A���A���A��uA�\)A��
A��9A�M�A�n�A��A�bNA��A���A��A�9XA��A�5?A���A�hsA��-A�7LA�|�A�E�A���A��wA���A�?}A�VA�ƨA��yA��`A�ȴA�|�A��A�t�A��A��!A�XA��\A��yA���A�K�A���A�+A���A�I�A��\A�jA�1A�A�z�A��wA�oA��A�jA~ �AzJAw?}Au��AtI�Aq"�AnM�AmC�Akx�Ah�!Ah$�Ag�hAf�DAdA�Ab��Aax�Aa%A`bA^jAZ �AT{AP��AO�AM�AM��ALA�AJ��AI�PAHVAG�AEƨAD�+AC�hAB��AA�#AAC�A?��A>��A<��A;VA97LA8�A7�hA6M�A3�;A1XA0-A/t�A/;dA-�-A-?}A+��A)�-A)?}A(Q�A%|�A#�#A"�uA!%A =qA�!AAXA�AE�A��A
=A^5A�#A��A��A��A��AQ�Ax�A�RA��A;dA&�A&�AG�AG�A?}AȴA
9XA�uA�A�;A��A�Al�A?}A��A$�A��A�!AƨA��AffA|�AK�A �yA z�A $�@�$�@�7L@�ƨ@�hs@�j@���@�?}@�t�@��H@���@�33@�J@��-@�7@�?}@�V@���@���@�Z@�R@�&�@�@��T@�@��@���@��@��@��@�O�@���@��
@�dZ@�^5@���@�bN@ΰ!@�X@ˍP@ʰ!@�5?@ȴ9@ǝ�@� �@�\)@�o@�ȴ@��@��
@�9X@��j@��;@��H@���@�5?@���@���@�|�@�5?@��T@��-@���@���@���@�;d@��@�Ĝ@��P@�~�@���@�  @�ȴ@� �@�ff@��@��@�ȴ@�M�@�%@�+@�33@�
=@���@�V@��@�V@��;@���@���@���@� �@�S�@�K�@�
=@��#@�Ĝ@���@�X@��j@���@�9X@{ƨ@{C�@y��@x�`@xbN@t�/@t�j@tZ@q�7@o\)@l��@l�@kC�@jM�@g�@fff@c�m@a�^@` �@^E�@]�@\�j@Z�H@ZJ@Y�@XQ�@V�y@Vv�@U�@S��@R=q@QX@P�9@OK�@Nv�@M�-@L(�@I�#@HA�@E�-@EV@C�m@C@B�\@@��@?�w@=��@<j@;��@:��@9��@9��@9��@9hs@8�u@7��@6{@3�m@3S�@2J@0��@/K�@,�@,I�@*=q@)�@)��@(�@';d@&�R@&v�@%��@#�
@#dZ@#C�@"�!@"J@!hs@l�@ff@@�@1@�@t�@��@X@  @�@��@|�@�+@�@(�@��@S�@@��@��@bN@�;@�;@��@
=@�+@@��@/@t�@
�\@
J@	&�@�u@1'@��@K�@ȴ@V@5?@�@�D@�!@J@ �u@ Q�?��?�/?��H?��#?���?�`B?�9X?�J?�|�?�5??�(�?���?�^?�r�?�?�z�?�9X?�G�?�  ?��?�"�?ٺ^?�b?�+?�E�?�?��/?�t�?�&�?�bN?���?Η�?�v�?��?�V?�(�?�ƨ?���?�"�?���?�x�?�
=?�?}?�9X?�-?��`?��?�\)?��?���?��-?�V?��?���?�?��H?���?���?�~�?���?�~�?�^5?�~�?�~�?���?���?�~�?�=q?���?���?���?��?��#?���?��?��?���?��?�^5?�=q?�~�?�~�?���?�?�"�?�"�?�C�?�dZ?�dZ?�dZ?�dZA�XA�O�A�K�A�C�A�VA�^5A�XA�O�A�ZA�\)A�bNA�`BA�`BA�`BA�^5A�`BA�hsA�hsA�jA�hsA�hsA�ffA�ffA�ffA�ffA�ffA�dZA�dZA�\)A�XA�ZA�VA�Q�A�O�A�O�A�O�A�O�A�Q�A�\)A�^5A�^5A�bNA�ffA�jA�hsA�jA�n�A�n�A�p�A�|�A�~�A�z�A�|�A��A��A��A��A��A��A�~�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        A�^5A�\)A�O�A�ZA�bNA�jA�n�A�~�A�~�A��A��A�z�A�t�A�jA�S�A�1'A��A��A���A�x�A�M�A�M�A�K�A�9XA��A�A��A���A��jA���A���A��DA�t�A�9XA�33A��A�/A�JA���A�M�A��#A��RA��hA�33A���A�+A��mA���A�;dA��A�bNA�-A��/A�XA��A���A��A���A���A��uA�\)A��
A��9A�M�A�n�A��A�bNA��A���A��A�9XA��A�5?A���A�hsA��-A�7LA�|�A�E�A���A��wA���A�?}A�VA�ƨA��yA��`A�ȴA�|�A��A�t�A��A��!A�XA��\A��yA���A�K�A���A�+A���A�I�A��\A�jA�1A�A�z�A��wA�oA��A�jA~ �AzJAw?}Au��AtI�Aq"�AnM�AmC�Akx�Ah�!Ah$�Ag�hAf�DAdA�Ab��Aax�Aa%A`bA^jAZ �AT{AP��AO�AM�AM��ALA�AJ��AI�PAHVAG�AEƨAD�+AC�hAB��AA�#AAC�A?��A>��A<��A;VA97LA8�A7�hA6M�A3�;A1XA0-A/t�A/;dA-�-A-?}A+��A)�-A)?}A(Q�A%|�A#�#A"�uA!%A =qA�!AAXA�AE�A��A
=A^5A�#A��A��A��A��AQ�Ax�A�RA��A;dA&�A&�AG�AG�A?}AȴA
9XA�uA�A�;A��A�Al�A?}A��A$�A��A�!AƨA��AffA|�AK�A �yA z�A $�@�$�@�7L@�ƨ@�hs@�j@���@�?}@�t�@��H@���@�33@�J@��-@�7@�?}@�V@���@���@�Z@�R@�&�@�@��T@�@��@���@��@��@��@�O�@���@��
@�dZ@�^5@���@�bN@ΰ!@�X@ˍP@ʰ!@�5?@ȴ9@ǝ�@� �@�\)@�o@�ȴ@��@��
@�9X@��j@��;@��H@���@�5?@���@���@�|�@�5?@��T@��-@���@���@���@�;d@��@�Ĝ@��P@�~�@���@�  @�ȴ@� �@�ff@��@��@�ȴ@�M�@�%@�+@�33@�
=@���@�V@��@�V@��;@���@���@���@� �@�S�@�K�@�
=@��#@�Ĝ@���@�X@��j@���@�9X@{ƨ@{C�@y��@x�`@xbN@t�/@t�j@tZ@q�7@o\)@l��@l�@kC�@jM�@g�@fff@c�m@a�^@` �@^E�@]�@\�j@Z�H@ZJ@Y�@XQ�@V�y@Vv�@U�@S��@R=q@QX@P�9@OK�@Nv�@M�-@L(�@I�#@HA�@E�-@EV@C�m@C@B�\@@��@?�w@=��@<j@;��@:��@9��@9��@9��@9hs@8�u@7��@6{@3�m@3S�@2J@0��@/K�@,�@,I�@*=q@)�@)��@(�@';d@&�R@&v�@%��@#�
@#dZ@#C�@"�!@"J@!hs@l�@ff@@�@1@�@t�@��@X@  @�@��@|�@�+@�@(�@��@S�@@��@��@bN@�;@�;@��@
=@�+@@��@/@t�@
�\@
J@	&�@�u@1'@��@K�@ȴ@V@5?@�@�D@�!@J@ �u@ Q�?��?�/?��H?��#?���?�`B?�9X?�J?�|�?�5??�(�?���?�^?�r�?�?�z�?�9X?�G�?�  ?��?�"�?ٺ^?�b?�+?�E�?�?��/?�t�?�&�?�bN?���?Η�?�v�?��?�V?�(�?�ƨ?���?�"�?���?�x�?�
=?�?}?�9X?�-?��`?��?�\)?��?���?��-?�V?��?���?�?��H?���?���?�~�?���?�~�?�^5?�~�?�~�?���?���?�~�?�=q?���?���?���?��?��#?���?��?��?���?��?�^5?�=q?�~�?�~�?���?�?�"�?�"�?�C�?�dZ?�dZ?�dZ?�dZA�XA�O�A�K�A�C�A�VA�^5A�XA�O�A�ZA�\)A�bNA�`BA�`BA�`BA�^5A�`BA�hsA�hsA�jA�hsA�hsA�ffA�ffA�ffA�ffA�ffA�dZA�dZA�\)A�XA�ZA�VA�Q�A�O�A�O�A�O�A�O�A�Q�A�\)A�^5A�^5A�bNA�ffA�jA�hsA�jA�n�A�n�A�p�A�|�A�~�A�z�A�|�A��A��A��A��A��A��A�~�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
z�B
z�B
z�B
z�B
z�B
y�B
z�B
y�B
z�B
z�B
y�B
{�B
}�B
� B
�B
�B
�B
�B
~�B
z�B
�B
�+B
�1B
�DB
�JB
�VB
�\B
�\B
�\B
�\B
�\B
�VB
�PB
�JB
�\B
��B
��B
�B
�LB
��B
�B  BhB�B'�B<jBM�B_;Bm�Bt�B�PB�B�qB��B�TB�B%B  B��B��B�B�ZB��B�/B�)B:^BE�BP�BP�BI�B.B�B�B7LB?}BC�B<jB�B�
B��BB�'B��B�B�B�JBz�Bm�BcTBP�B<jB7LB@�B=qB49B+B�BVB
��B
�`B
�HB
�B
��B
�9B
��B
�oB
�DB
y�B
q�B
dZB
F�B
B	�sB	��B	��B	B	��B	�hB	�B	l�B	]/B	ZB	R�B	I�B	?}B	6FB	+B	'�B	�B	
=B�BB��BŢB�jB�^B�?B�B�B�!B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�\B�1B� B{�Bz�Bv�Bv�Bu�Bo�Bp�Bl�BffBcTBaHB_;BYBZBR�BR�BN�BL�BJ�BI�BG�BF�BE�BD�BC�B@�B@�B=qB?}B;dB;dB=qB=qB>wBB�BD�BD�BB�BD�B>wB?}BB�BI�BI�BI�BI�BL�BN�BP�BR�BS�BS�BR�BT�BT�BS�BT�BR�BQ�BVBW
BZBZBZB[#B]/B\)BZB^5B_;B_;B^5B^5B]/B^5B]/B]/B]/B\)BW
BR�BO�BM�BI�BH�BJ�BI�BJ�BL�BN�BO�BP�BYB`BBcTBcTBaHB_;B_;BaHBe`Bl�Bo�Bo�Bp�B�7B��B�!B�qB��B�TB��B	PB	{B	!�B	C�B	M�B	N�B	Q�B	hsB	s�B	|�B	�JB	��B	��B	��B	�B	�LB	�^B	�}B	ÖB	ŢB	ƨB	��B	��B	��B	��B	�5B	�BB	�HB	�sB	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B
  B
B
%B
PB
\B
bB
bB
bB
oB
uB
�B
�B
�B
�B
�B
�B
�B
�B
!�B
!�B
"�B
#�B
'�B
)�B
,B
.B
.B
1'B
/B
1'B
2-B
2-B
33B
33B
33B
6FB
8RB
9XB
;dB
:^B
<jB
=qB
=qB
>wB
?}B
@�B
@�B
B�B
A�B
C�B
E�B
F�B
G�B
H�B
J�B
L�B
L�B
M�B
K�B
K�B
L�B
M�B
O�B
P�B
R�B
R�B
S�B
S�B
T�B
W
B
VB
VB
XB
XB
YB
ZB
\)B
\)B
]/B
]/B
^5B
^5B
]/B
^5B
^5B
^5B
aHB
aHB
bNB
bNB
dZB
dZB
dZB
dZB
ffB
hsB
hsB
hsB
hsB
iyB
l�B
l�B
l�B
m�B
m�B
n�B
o�B
o�B
p�B
q�B
o�B
r�B
q�B
r�B
s�B
s�B
v�B
v�B
v�B
x�B
x�B
x�B
y�B
z�B
z�B
z�B
z�B
{�B
|�B
�B
�B
�B
�B
�B
�%B
�+B
�+B
�+B
�=B
�DB
�JB
�\B
�\B
�bB
�hB
�oB
�uB
�uB
�{B
��B
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
�B
�B
�B
�B
�!B
�-B
�-B
�3B
�3B
�9B
�?B
�FB
�LB
�LB
�LB
�LB
�LB
�LB
�LB
�LB
�RB
�RB
�RB
�LB
�RB
�XB
�XB
�XB
�XB
�RB
�^B
�XB
�XB
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
�XB
�XB
�XB
�XB
�XB
�^B
y�B
{�B
{�B
{�B
|�B
z�B
{�B
|�B
y�B
x�B
x�B
y�B
z�B
y�B
y�B
z�B
y�B
y�B
y�B
y�B
y�B
y�B
y�B
y�B
z�B
z�B
y�B
z�B
y�B
z�B
z�B
z�B
y�B
z�B
z�B
z�B
z�B
z�B
z�B
y�B
{�B
y�B
y�B
y�B
z�B
y�B
y�B
z�B
{�B
y�B
y�B
y�B
z�B
z�B
y�B
z�B
z�B
y�B
y�B
y�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        B
z�B
z�B
z�B
z�B
z�B
y�B
z�B
y�B
z�B
z�B
y�B
{�B
}�B
�B
��B
��B
��B
��B
~�B
z�B
��B
�B
�B
�%B
�,B
�8B
�?B
�?B
�@B
�AB
�AB
�<B
�6B
�1B
�DB
�{B
��B
��B
�6B
��B
�B
��BTB�B'�B<XBM�B_*Bm�Bt�B�@B��B�bB��B�GB�BB��B��B��B�B�QB��B�'B�!B:WBE�BP�BP�BI�B.B�B�B7IB?zBC�B<iB�B�	B��BB�'B��B�B�	B�LBz�Bm�BcWBP�B<nB7PB@�B=vB4?B+B�B]B
��B
�hB
�PB
�B
��B
�CB
��B
�yB
�OB
y�B
q�B
dfB
F�B
B	�B	��B	��B	B	��B	�wB	�.B	l�B	]?B	Z.B	SB	I�B	?�B	6YB	+B	(B	�B	
RB�WB��BŷB��B�tB�UB�2B�&B�9B�&B�B��B��B��B��B��B��B��B��B�B�B��B��B��B�|B�QB� B|B{Bv�Bv�Bu�Bo�Bp�Bl�Bf�BcyBamB_aBY=BZDBSBSBOBL�BJ�BI�BG�BF�BE�BD�BC�B@�B@�B=�B?�B;�B;�B=�B=�B>�BB�BD�BD�BB�BD�B>�B?�BB�BI�BI�BI�BI�BMBOBQBS+BT2BT2BS-BU9BU:BT4BU;BS/BR*BVBBWIBZ\BZ]BZ]B[dB]pB\kBZ_B^xB_~B_B^zB^zB]uB^{B]vB]vB]wB\rBWSBS;BP)BNBJBH�BKBJBKBMBO'BP-BQ4BYfB`�Bc�Bc�Ba�B_�B_�Ba�Be�Bl�Bo�Bo�Bp�B��B��B��B��B�@B�B�6B	�B	�B	"AB	DB	NOB	OXB	RnB	h�B	t>B	}zB	��B	�,B	�fB	��B	��B	��B	��B	�!B	�=B	�KB	�TB	�vB	ϋB	њB	յB	��B	��B	�B	�5B	�DB	�MB	�cB	�}B	�B	��B	��B	��B	��B	��B
 �B
�B
B
9B
HB
PB
SB
VB
fB
oB
~B
�B
�B
�B
�B
�B
�B
�B
"�B
"�B
#�B
$�B
)B
+ B
-/B
/=B
/@B
2VB
0LB
2[B
3dB
3fB
4oB
4qB
4tB
7�B
9�B
:�B
<�B
;�B
=�B
>�B
>�B
?�B
@�B
A�B
A�B
C�B
B�B
D�B
GB
HB
IB
J%B
L4B
NCB
NFB
OOB
MEB
MHB
NQB
OZB
QhB
RqB
T�B
T�B
U�B
U�B
V�B
X�B
W�B
W�B
Y�B
Y�B
Z�B
[�B
]�B
]�B
^�B
^�B
_�B
_�B
^�B
_�B
_�B
_�B
cB
cB
dB
dB
f,B
f/B
f1B
f3B
hBB
jQB
jSB
jVB
jXB
k`B
nuB
nwB
nyB
o�B
o�B
p�B
q�B
q�B
r�B
s�B
q�B
t�B
s�B
t�B
u�B
u�B
x�B
x�B
x�B
z�B
z�B
z�B
{�B
} B
}B
}B
}B
~B
B
�=B
�BB
�PB
�TB
�iB
�zB
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
�B
�B
�B
�%B
�1B
�1B
�<B
�NB
�\B
�iB
�nB
�zB
��B
��B
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
�B
�B
�B
�2B
�1B
�NB
�cB
�~B
��B
��B
��B
��B
��B
��B
�B
�#B
�8B
�MB
�dB
�fB
�iB
�lB
�pB
�sB
�uB
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
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
y�B
{�B
{�B
{�B
|�B
z�B
{�B
|�B
y�B
x�B
x�B
y�B
z�B
y�B
y�B
z�B
y�B
y�B
y�B
y�B
y�B
y�B
y�B
y�B
z�B
z�B
y�B
z�B
y�B
z�B
z�B
z�B
y�B
z�B
z�B
z�B
z�B
z�B
z�B
y�B
{�B
y�B
y�B
y�B
z�B
y�B
y�B
z�B
{�B
y�B
y�B
y�B
z�B
z�B
y�B
z�B
z�B
y�B
y�B
y�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�201904191900342021061413561220210614135612202106171314252021061713142520210617131425201904191900342021061413561220210614135612202106171314252021061713142520210617131425PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            new conductivity = original conductivity * (1 + delta*TEMP + CPcor_SBE*PRES) / (1 + delta*TEMP_ADJUSTED + CPcor_new*PRES_ADJUSTED)                                                                                                                              none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            new conductivity = original conductivity * (1 + delta*TEMP + CPcor_SBE*PRES) / (1 + delta*TEMP_ADJUSTED + CPcor_new*PRES_ADJUSTED)                                                                                                                              none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.50 dbar                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            CPcor_new = -1.25e-07; CPcor_SBE = -9.57e-08; delta = 3.25e-06                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.50 dbar                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            CPcor_new = -1.25e-07; CPcor_SBE = -9.57e-08; delta = 3.25e-06                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                      none                                                                                                                                                                                                                                                            New conductivity computed by using a different CPcor value from that provided by Sea-Bird.                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            No significant salinity drift detected.                                                                                                                                                                                                                         Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                      none                                                                                                                                                                                                                                                            New conductivity computed by using a different CPcor value from that provided by Sea-Bird.                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            No significant salinity drift detected.                                                                                                                                                                                                                         PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            AO  AO  ARCAARCAADJPADJP                                                                                                                                        2019041919003420190419190034  IP  IP                                G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2019041919003420190419190034QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�5F03E           703E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2019041919003420190419190034QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�0               0               PM  PM  ARSQARSQCOWGCOWGV1.1V1.1                                                                                                                                2021061713151720210617131517IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                