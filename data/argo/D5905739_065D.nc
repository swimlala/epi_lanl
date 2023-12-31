CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2019-04-10T05:00:47Z creation      
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
resolution        =���     �  O|   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  _4   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  c$   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  r�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �<   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �,   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �D   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �4   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                   T   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                   \   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                   d   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                   l   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  � t   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                   �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                      HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                       HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar        8   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar        @   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�       H   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    P   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  � �Argo profile    3.1 1.2 19500101000000  20190410050047  20210617131517  5905739 5905739 US ARGO PROJECT                                                 US ARGO PROJECT                                                 GREGORY C. JOHNSON                                              GREGORY C. JOHNSON                                              PRES            TEMP            PSAL            PRES            TEMP            PSAL               A   ADD  AOAO7219                            7219                            2C  2C  DD  SOLO_D_MRV                      SOLO_D_MRV                      12002                           12002                           SBE603 01Dec16                  SBE603 01Dec16                  874 874 @ذpt�b#@ذpt�b#11  @ذpO�p@ذpO�p@6���b@6���b�c��Z�y�c��Z�y11  GPS     GPS     Primary sampling: averaged [1Hz sampling by SBE-41CP averaged in 2-dbar bins]                                                                                                                                                                                   Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 AC  AA  AA  >���?�  @   @@  @�ff@�33@���@�  A��A��A#33A@  Ac33A���A�  A�  A�  A�  A�33A���A�B ��BffBffBffB   B(ffB0��B8  B@ffBH��BPffBX  B`ffBh��Bp��Bx��B�33B�33B�33B�33B�ffB�33B���B�  B�ffB�33B���B���B�33B�ffB�ffB�ffB�ffB�ffB�33B�ffB�33B�33B���B�ffB�33B�  B���B���B�33B�33B�33B�33B���C��C��C33C33C	��C�fC  C�fCL�C33C33C33C33C33C33C L�C!�fC$  C&  C(�C*33C,�C.�C033C233C433C6L�C8  C9��C<33C>�C@  CB  CC�fCE�fCG�fCJ33CLL�CN33CP�CR  CTL�CV33CX�CZ  C[�fC^L�C`33Cb�Cd�Cf  ChL�CjL�Ck��Cm��Cp�Cr�Ct  Cu�fCw�fCz�C|  C}��C��C�  C�  C�  C�  C��C�  C�&fC��C�  C��3C��fC��C��C�  C�&fC�  C��3C��C�  C��fC��C�&fC��C��3C�  C��C�  C��fC��3C�  C��C�&fC�&fC��C��3C�  C��C�&fC�&fC��C��fC�  C��C��C�&fC�&fC��C��3C��3C��C��C�&fC��C��fC�  C��C��C�  C��fC��3C��C��C�  C��3C�  C�&fC��C��fC�  C��C��C��fC��C��C��C�  C��C��C��C�&fC�&fC�  C��3C��fC��C��3C��fC��C��C�  C��3C��3C��C�  C��3C��C�  C��fC�  C�&fC��C�  C�&fC��C��C��C��C�  C�  C��C�&fC��C��3C��3C�  C��C��C�&fC�&fC�33C��C��3C��3C�  C��C��C�33C�ٚD s3D �3Dy�D  D�3D	L�D��Ds3D3D�fD� D33D�3D�fD!� D$L�D'�D)�fD,s3D/&fD1� D4��D7,�D9�fD<ffD?�DA�fDD�fDG33DI�fDL` DO�DQ��DTL�DV� DY�fD\&fD^��Day�Dd  Df��DiL�Dk� Dnl�Dp��Dsy�Du�3DxffDz� D|�fDS3D�� D��D�Y�D��fD��3D� D�P D��3D��3D�fD�S3D���D���D�fD�C3D��3D���D��fD�6fD�vfD��3D��D�3D�FfD�i�D��3D��3D��fD�3D�6fD�` D�� D��3D��3D��fD�3D��D�,�D�I�D�c3D�� D�� D�� D�� D�3D�  D�@ D�ffD��3D�� D��fD�0 D�l�D���D��3D�#3D�` DĦfD��fD�)�D�l�Dɰ D��fD�9�D͐ D�� D��D�Y�DҐ D��fD� D�I�D׉�D��3D��3D��D�<�D�\�D�vfDߓ3D� D� D�� D���D�ٚD��D���D��fD�3D�3D� D�3D� D� D�	�D�3D���D�� D�ٚD���D�� D���D��3D���D�� D�� D��fD���D�� D��3D���D��fE ` E �3EffE� Ec3E�fE|�E3E��E3E� E� E�E	� E
�3E�3EfE33E�fE� E3E0 E�3E�3E E+3E�fE��E��E��E�fE�fE!S3E"T�E#� E$��E&I�E'@ E(� E)�3E+fE,q�E-\�E.�fE0&fE1��E2y�E3�3E5VfE6T�E7�fE8�fE:fE;{3E<` E?��EB�fEE�EI6fEL�EO@ ER��EU\�EX{3E[� E^� Ea�3Ed�fEha�Ekx EnS3Eq�3Et��Ew�fE{3E~0 E���E�$�=���>L��>L��>���>L��>L��>���>L��>L��>L��>L��>L��>���>���>���>L��>���>���>L��>L��>���>���?   >���?   ?   ?333?L��?L��?�  ?���?�ff?�33?ٙ�?�ff@ff@��@&ff@333@Fff@Y��@s33@�ff@�  @���@���@�33@�33@�33@���@���@���A��A��A��A33A!��A+33A1��A9��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141441444441444144414141411411111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                             ?L��?�  @   @`  @�ff@�33@���@�  A	��A��A+33AH  Ak33A���A�  A�  A�  A�  A�33A���A���B��B
ffBffBffB"  B*ffB2��B:  BBffBJ��BRffBZ  BbffBj��Br��Bz��B�33B�33B�33B�33B�ffB�33B���B�  B�ffB�33B���B���B�33B�ffB�ffB�ffB�ffB�ffB�33B�ffB�33B�33B���B�ffB�33B�  B���B���B�33B�33B�33B�33C ffCL�CL�C�3C�3C
L�CffC� CffC��C�3C�3C�3C�3C�3C�3C ��C"ffC$� C&� C(��C*�3C,��C.��C0�3C2�3C4�3C6��C8� C:L�C<�3C>��C@� CB� CDffCFffCHffCJ�3CL��CN�3CP��CR� CT��CV�3CX��CZ� C\ffC^��C`�3Cb��Cd��Cf� Ch��Cj��ClL�CnL�Cp��Cr��Ct� CvffCxffCz��C|� C~L�C�Y�C�@ C�@ C�@ C�@ C�L�C�@ C�ffC�Y�C�@ C�33C�&fC�Y�C�Y�C�@ C�ffC�@ C�33C�L�C�@ C�&fC�L�C�ffC�L�C�33C�@ C�L�C�@ C�&fC�33C�@ C�Y�C�ffC�ffC�L�C�33C�@ C�Y�C�ffC�ffC�L�C�&fC�@ C�L�C�Y�C�ffC�ffC�L�C�33C�33C�L�C�Y�C�ffC�L�C�&fC�@ C�L�C�Y�C�@ C�&fC�33C�L�C�Y�C�@ C�33C�@ C�ffC�L�C�&fC�@ C�Y�C�L�C�&fC�L�C�Y�C�L�C�@ C�Y�C�Y�C�L�C�ffC�ffC�@ C�33C�&fC�L�C�33C�&fC�L�C�L�C�@ C�33C�33C�L�C�@ C�33C�L�C�@ C�&fC�@ C�ffC�L�C�@ C�ffC�Y�C�L�C�Y�C�L�C�@ C�@ C�Y�C�ffC�L�C�33C�33C�@ C�Y�C�Y�C�ffC�ffC�s3C�L�C�33C�33C�@ C�L�C�Y�C�s3D �D �3D3D��D  D�3D	l�D�D�3D33D�fD� DS3D3D�fD!� D$l�D',�D)�fD,�3D/FfD2  D4��D7L�D9�fD<�fD?,�DA�fDD�fDGS3DI�fDL� DO,�DQ��DTl�DW  DY�fD\FfD^��Da��Dd@ DfٚDil�Dl  Dn��Dq�Ds��Dv3Dx�fD{  D}fDs3D�� D�)�D�i�D��fD��3D�  D�` D��3D��3D�&fD�c3D���D���D�fD�S3D��3D���D�fD�FfD��fD��3D���D�#3D�VfD�y�D��3D��3D��fD�#3D�FfD�p D�� D��3D��3D��fD�3D�)�D�<�D�Y�D�s3D�� D�� D�� D�� D�3D�0 D�P D�vfD��3D�� D�fD�@ D�|�D���D��3D�33D�p DĶfD��fD�9�D�|�D�� D�fD�I�D͠ D�� D�)�D�i�DҠ D��fD�  D�Y�Dי�D��3D�3D�)�D�L�D�l�DކfDߣ3D� D�� D�� D���D��D���D���D�fD�3D�3D�  D�#3D�  D�  D��D�3D�	�D�  D��D���D�� D���D��3D���D�� D�� D��fD���D�� D��3D���D��fE h E �3EnfE� Ek3E�fE��E3E��E3E� E� E�E	� E
�3E�3EfE;3E�fE� E3E8 E�3E3E E33E�fE��E��E��E�fE�fE![3E"\�E#� E$��E&Q�E'H E(� E)�3E+fE,y�E-d�E.�fE0.fE1��E2��E3�3E5^fE6\�E7�fE8�fE:fE;�3E<h E?��ECfEE�EI>fEL�EOH ER��EUd�EX�3E[� E^� Ea�3Ed�fEhi�Ek� En[3Eq�3Eu�Ew�fE{3E~8 E���E�(�?��G�O�?333G�O�G�O�?333G�O�G�O�G�O�G�O�G�O�?333G�O�G�O�G�O�?333G�O�G�O�G�O�?333G�O�?fffG�O�?fffG�O�?�  ?���G�O�?�ff?�  ?ٙ�?�ff?�33@��@33@&ff@9��@Fff@S33@fff@y��@���@�ff@�  @���@���@�33@�33@�33@���@���A��A��A��A��A#33A)��A333A9��AA��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141441444441444144414141411411111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                             @ @ �@ �@ {@ �@ "�@ (G@ /�@ 7L@ >@ E�@ Q�@ `�@ m�@ z�@ ��@ �0@ ��@ ��@ ��@ �*@ ��@ ��@ �q@@@g@-�@:@H]@V�@c�@p�@~�@�P@��@��@��@�>@��@ލ@��@��@%@{@#�@0x@<�@Ji@Yn@g�@uk@�@��@�a@�Y@��@ƨ@�O@��@�L@�E@
=@6@$�@3�@A�@O0@\�@i!@v@��@�#@��@��@�@�c@�[@�@�@^@@�@*S@7�@FQ@Q=@_�@m:@{�@��@��@��@��@��@�*@܀@�@�e@�@�@�@,`@9X@F�@T�@dZ@r�@�@��@��@�M@�F@�>@�7@�/@�@��@�@*@""@1�@?}@I�@Wb@g@t�@��@��@�U@�Y@�R@Ĝ@��@�H@��@��@
=@�@%�@5?@B8@N�@[z@hs@x�@�|@��@�y@�@�@�@�
@�T@�@@V@�@(�@7L@DD@P�@^�@m:@|?@��@�<@��@��@�&@�*@܀@�(@�q@	@	@	g@	-�@	<@	I�@	V@	bN@	o�@	~�@	�P@	��@	��@	��@	@	��@	�;@	�@	��@
%@
*@
#�@
/�@
<�@
K@
Z�@
g@
r�@
��@
��@
��@
�M@
�@
�W@
�O@
�H@
�L@
��@
�@�@'�@33@@,@M$@\�@i!@v@��@�u@�m@�f@�@�@�
@�@�@  @J@O@+@7L@DD@S�@`�@m�@|?@�7@�0@��@��@�2@�|@��@�m@��@�@o@ �@.l@<�@H]@T�@bN@p�@~�@�P@�U@�5@��@�2@ψ@��@a�@�5@��@1�@y�@�>@�@Wb@�y@�@:@�|@є@�@e	@��@�,@C�@��@��@�@`�@�Y@�q@?}@��@��@�@]�@�5@�@3�@{�@��@�@UU@�U@�@(�@n�@�9@�~@<@~�@@��@<@~K@�2@v@I@��@�7@{@Yn@��@�@&;@i!@�f@�L@3�@x&@�@��@ B8@ �|@ �@!J@!K�@!�P@!�o@"
�@"K�@"��@"�@#1@#G�@#�@#�>@$ �@$>�@${�@$��@$�@%.l@%j@%�A@%�@&""@&_�@&��@&�t@'�@'V�@'��@'�
@(B@(\)@(��@(�T@)&;@)j@)�@)�@*7�@*|�@*��@+�@+Lu@+�i@+�t@,�@,e	@,�M@,�@-1'@-t@-��@-�9@.>@.~�@.��@.��@/9X@/uk@/�-@/�@0%�@0_�@0��@0�C@1J@1C�@1|?@1��@1�4@2%�@2\�@2��@2�c@2��@33�@3hs@3�@3��@4�@46�@4i!@4�@4խ@5�@5DD@5uk@5�f@5�@6�@6T�@6�P@6��@6��@75�@7i�@7��@7�/@8�@8SI@8��@8�W@9j@9��@9��@:�R@;4�@;��@</@<�@=^�@=�
@>SI@>��@?�@?��@@t@@��@A��@B�@B��@C:�@C��@D$�@D��@EB�@E�@FS�@F�@G\)@G�,@H`�@H��@I�u@I��@J��@K(G@K��@L&;@L��@M^�@M�o@Ni!@Nψ@Oe�@O��@P_�@Q�^@S2�@TqS@U��@W@Xm:@Y��@[�@\]�@]��@_�@`Z@a��@c&;@dww@e�r@g�@h��@i��@k @lt@m��@oV@  �G�O�@ ^G�O�G�O�@ ^G�O�G�O�G�O�G�O�G�O�@ ^G�O�G�O�G�O�@ ^G�O�G�O�G�O�@ ^G�O�@ �G�O�@ �G�O�@ j@ �G�O�@ v@ �@ 1@ �@ 	�@ �@ J@ V@ b@ �@ @ *@ 6@ �@ �@ �@  �@ $.@ &;@ )�@ -@ /@ 2�@ 5?@ 8�@ <@ ?}@ B8@ D�@ I@ K�@ O0G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�n�A�t�A�r�A�r�A�r�A�r�A�r�A�r�A�v�A�v�A�z�A�|�A��A��A�~�A�~�A��A�~�A�~�A�z�A�+A��A���A�M�A�A��/A�I�A��A�ffA�9XA��-A���A���A��PA��A�`BA�Q�A�I�A�+A��A���A�/A��A��
A��jA��7A�C�A�/A�VA��A��yA��HA��
A��^A�v�A� �A��jA�x�A���A��A�&�A��yA��+A�VA�hsA�/A�{A�ƨA�XA�-A�/A��!A��yA��A�n�A���A�ffA�^5A��PA��#A�E�A���A��A��jA��A�x�A�r�A�=qA��#A�jA���A��A�I�A��A���A��/A���A���A�n�A��
A���A�z�A�1A��A���A���A���A��#A�O�A�v�A��!A��uA�|�A~ĜAyp�AwƨAtI�Aq�#Ao�Ao��An�Am�Al��AlffAjȴAi"�Ag��Ac�^A`(�A\ĜA[�AZ�AY�AU�AR�/AR�!AQdZAN�9AL�AK��AGS�AD��ADJAC�7ACVA@Q�A>�\A;\)A7
=A4�A4�9A3�
A0A.�A+�#A*��A(��A'7LA&�RA&=qA%�A%��A%hsA$�A$Q�A!��A�A\)AE�A;dA�9A��A��Ar�AQ�A��A  A�9A�A��A~�At�A��A
=A�A�;A
��A	�#A	|�A�RAM�A5?A  AS�AQ�Ap�A�jAA�A5?A  A�wA �\@��@�v�@���@�7L@�  @�t�@�l�@���@�ff@�-@�J@���@���@�^5@��
@�$�@�(�@�C�@��@��/@�33@�M�@��@��#@�Z@�t�@�
=@��@�S�@���@ۍP@�o@��y@ش9@�r�@�Z@��@��@ְ!@�^5@��@؛�@؛�@���@�33@��@��m@��H@��T@Ͳ-@���@ʗ�@�7L@ȋD@�1@ļj@��y@��j@���@���@�?}@���@��@�z�@�p�@���@���@���@�hs@���@�{@��j@�^5@���@��H@��D@��@��+@�Ĝ@�O�@��m@�S�@��@�A�@�t�@�5?@���@�z�@�C�@���@���@�bN@��
@�b@���@���@���@�X@���@�Z@��m@�7L@�@�A�@�dZ@��`@��F@��@���@���@��^@�;@\)@}�@|1@{o@x�@u�h@sS�@p��@n��@m��@lj@kƨ@h��@g;d@e?}@c��@c33@aX@a�@a7L@`��@^V@\(�@Z~�@Y�7@W��@U�h@R-@Q%@O;d@Lz�@J�H@J-@JJ@Hr�@G�P@Gl�@F@F@E�h@Cƨ@A��@?�@?�w@<Z@:�!@9��@7�;@7��@7�P@5��@4��@3o@2=q@17L@/�@/K�@.5?@,�/@+��@+33@)�@'|�@&�R@%p�@$(�@"�@ �@   @�;@|�@j@9X@9X@33@�@��@��@�h@�h@��@x�@7L@7L@�@�`@A�@K�@;d@��@{@V@�@��@z�@C�@
�\@��@��@��@�`@��@Q�@ �@�@��@ff@�T@`B@O�@j@ƨ@�H@~�@^5@�@J@%@   ?��?�O�?�~�?��u?�ff?��T?��?�-?�G�?�;d?�h?�^5?�X?�ff?�?��?���?��?�Ĝ?�A�?�  ?���?�V?�V?��?���?��?�l�?�E�?�9X?���?�G�?��`?�  ?�\)?�5??�{?�/?���?̬?̬?�V?��?̋D?�ƨ?˥�?���?ȴ9?���?�?���?��
?��?�;d?��?��h?�(�?�C�?��H?���?���?��H?���?�dZ?�dZ?�ƨ?���?�(�?�jA�hsA�l�A�jA�jA�jA�jA�jA�jA�jA�jA�l�A�n�A�l�A�n�A�jA�l�A�n�A�n�A�p�A�p�A�r�A�r�A�r�A�t�A�t�A�t�A�t�A�t�A�t�A�t�A�r�A�t�A�t�A�r�A�r�A�r�A�t�A�r�A�r�A�r�A�p�A�p�A�r�A�r�A�r�A�r�A�r�A�r�A�p�A�r�A�t�A�t�A�v�A�v�A�v�A�v�A�x�A�~�A�~�A�~�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                             A�n�A�t�A�r�A�r�A�r�A�r�A�r�A�r�A�v�A�v�A�z�A�|�A��A��A�~�A�~�A��A�~�A�~�A�z�A�+A��A���A�M�A�A��/A�I�A��A�ffA�9XA��-A���A���A��PA��A�`BA�Q�A�I�A�+A��A���A�/A��A��
A��jA��7A�C�A�/A�VA��A��yA��HA��
A��^A�v�A� �A��jA�x�A���A��A�&�A��yA��+A�VA�hsA�/A�{A�ƨA�XA�-A�/A��!A��yA��A�n�A���A�ffA�^5A��PA��#A�E�A���A��A��jA��A�x�A�r�A�=qA��#A�jA���A��A�I�A��A���A��/A���A���A�n�A��
A���A�z�A�1A��A���A���A���A��#A�O�A�v�A��!A��uA�|�A~ĜAyp�AwƨAtI�Aq�#Ao�Ao��An�Am�Al��AlffAjȴAi"�Ag��Ac�^A`(�A\ĜA[�AZ�AY�AU�AR�/AR�!AQdZAN�9AL�AK��AGS�AD��ADJAC�7ACVA@Q�A>�\A;\)A7
=A4�A4�9A3�
A0A.�A+�#A*��A(��A'7LA&�RA&=qA%�A%��A%hsA$�A$Q�A!��A�A\)AE�A;dA�9A��A��Ar�AQ�A��A  A�9A�A��A~�At�A��A
=A�A�;A
��A	�#A	|�A�RAM�A5?A  AS�AQ�Ap�A�jAA�A5?A  A�wA �\@��@�v�@���@�7L@�  @�t�@�l�@���@�ff@�-@�J@���@���@�^5@��
@�$�@�(�@�C�@��@��/@�33@�M�@��@��#@�Z@�t�@�
=@��@�S�@���@ۍP@�o@��y@ش9@�r�@�Z@��@��@ְ!@�^5@��@؛�@؛�@���@�33@��@��m@��H@��T@Ͳ-@���@ʗ�@�7L@ȋD@�1@ļj@��y@��j@���@���@�?}@���@��@�z�@�p�@���@���@���@�hs@���@�{@��j@�^5@���@��H@��D@��@��+@�Ĝ@�O�@��m@�S�@��@�A�@�t�@�5?@���@�z�@�C�@���@���@�bN@��
@�b@���@���@���@�X@���@�Z@��m@�7L@�@�A�@�dZ@��`@��F@��@���@���@��^@�;@\)@}�@|1@{o@x�@u�h@sS�@p��@n��@m��@lj@kƨ@h��@g;d@e?}@c��@c33@aX@a�@a7L@`��@^V@\(�@Z~�@Y�7@W��@U�h@R-@Q%@O;d@Lz�@J�H@J-@JJ@Hr�@G�P@Gl�@F@F@E�h@Cƨ@A��@?�@?�w@<Z@:�!@9��@7�;@7��@7�P@5��@4��@3o@2=q@17L@/�@/K�@.5?@,�/@+��@+33@)�@'|�@&�R@%p�@$(�@"�@ �@   @�;@|�@j@9X@9X@33@�@��@��@�h@�h@��@x�@7L@7L@�@�`@A�@K�@;d@��@{@V@�@��@z�@C�@
�\@��@��@��@�`@��@Q�@ �@�@��@ff@�T@`B@O�@j@ƨ@�H@~�@^5@�@J@%@   ?��?�O�?�~�?��u?�ff?��T?��?�-?�G�?�;d?�h?�^5?�X?�ff?�?��?���?��?�Ĝ?�A�?�  ?���?�V?�V?��?���?��?�l�?�E�?�9X?���?�G�?��`?�  ?�\)?�5??�{?�/?���?̬?̬?�V?��?̋D?�ƨ?˥�?���?ȴ9?���?�?���?��
?��?�;d?��?��h?�(�?�C�?��H?���?���?��H?���?�dZ?�dZ?�ƨ?���?�(�?�jA�hsA�l�A�jA�jA�jA�jA�jA�jA�jA�jA�l�A�n�A�l�A�n�A�jA�l�A�n�A�n�A�p�A�p�A�r�A�r�A�r�A�t�A�t�A�t�A�t�A�t�A�t�A�t�A�r�A�t�A�t�A�r�A�r�A�r�A�t�A�r�A�r�A�r�A�p�A�p�A�r�A�r�A�r�A�r�A�r�A�r�A�p�A�r�A�t�A�t�A�v�A�v�A�v�A�v�A�x�A�~�A�~�A�~�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                             ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
�#B
�B
�B
�B
�#B
�B
�#B
�#B
�#B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�#B
�BB;dB_;Be`BiyB� B��B��B��B�RB�RB�LB�FB�?B�FB�dB�}B�wB�^B�qB��B��B��B��B��B��B��B�B�B�/B�5B�TB�sB��BJB�B�B�B�B�B{B5?B<jB?}B=qB:^B6FB&�B1'B.B'�B�BuBJB  B�B��B�'B��B�hB�%B}�Bx�Bt�Br�Bq�Bm�BhsBbNBYBM�BD�BB�B:^B-B#�B#�B�B
=BB
��B
�B
�BB
��B
��B
�3B
��B
�%B
l�B
`BB
H�B
?}B
&�B
+B	��B	�sB	�
B	��B	��B	ĜB	�qB	�XB	�LB	��B	��B	�=B	w�B	T�B	>wB	7LB	33B	1'B	�B	�B	�B	hB	B	B��B��BɺBŢBĜB�wB�B��B�{B�B�B�B{�Br�Br�BjBhsBdZBbNB`BB]/B]/B[#B[#B[#BYBVBP�BM�BN�BK�BK�BJ�BI�BH�BH�BE�BD�BD�BC�B?}B@�B;dB=qB<jB<jB7LB9XB;dB9XB7LB8RB8RB8RB6FB49B;dBC�BE�BE�BG�BH�BF�BJ�BK�BM�BO�BN�BQ�BS�BS�BVBZB\)B\)B\)B[#BS�BE�BF�BB�BB�B@�B>wB>wB@�B?}BA�BH�BM�BN�BO�B\)BgmBk�Bl�Bp�BjBs�Bv�Bw�B}�B~�B�B��B��B��B��B��B��B��B�B�!B�RB�XB�RB�qB�qB�jBBBŢBĜB��B��B��B�B��B	B	B	B	B	PB	�B	9XB	J�B	ZB	cTB	o�B	t�B	�B	�B	�B	�DB	�bB	�uB	��B	��B	��B	�B	�-B	�3B	�LB	�}B	�}B	ŢB	��B	��B	�B	��B	��B	��B	�)B	�5B	�fB	�TB	�mB	�`B	�B	�B	�B	�B	��B	��B	��B	��B
B
B
B
%B
	7B
JB
VB
hB
uB
{B
�B
�B
�B
�B
�B
�B
�B
#�B
$�B
'�B
)�B
.B
1'B
2-B
2-B
5?B
7LB
8RB
9XB
;dB
;dB
<jB
=qB
<jB
>wB
@�B
A�B
C�B
C�B
E�B
G�B
I�B
I�B
H�B
L�B
M�B
N�B
P�B
Q�B
Q�B
R�B
S�B
S�B
T�B
T�B
VB
VB
VB
YB
YB
ZB
[#B
[#B
[#B
[#B
\)B
]/B
_;B
`BB
`BB
aHB
cTB
cTB
bNB
dZB
ffB
jB
jB
jB
jB
n�B
o�B
o�B
n�B
o�B
n�B
o�B
q�B
p�B
q�B
q�B
s�B
t�B
s�B
u�B
u�B
w�B
y�B
x�B
x�B
x�B
y�B
y�B
y�B
z�B
y�B
{�B
|�B
|�B
|�B
}�B
~�B
� B
� B
�B
�B
�B
�B
�B
�B
�%B
�+B
�1B
�=B
�7B
�JB
�JB
�JB
�VB
�\B
�oB
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
��B
��B
��B
��B
�B
�B
�B
�!B
�-B
�-B
�3B
�?B
�LB
�LB
�LB
�LB
�RB
�XB
�RB
�XB
�XB
�^B
�^B
�^B
�#B
�)B
�#B
�#B
�B
�#B
�#B
�#B
�#B
�#B
�#B
�B
�#B
�B
�#B
�#B
�)B
�#B
�B
�#B
�B
�B
�#B
�B
�B
�B
�B
�#B
�B
�B
�#B
�B
�B
�#B
�B
�B
�B
�#B
�B
�B
�B
�#B
�B
�#B
�B
�B
�#B
�B
�#B
�#B
�B
�#B
�B
�B
�B
�B
�#B
�B
�B
�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                             B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
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
�B�B;DB_BeABi[B�B��B��B��B�6B�7B�2B�,B�&B�-B�LB�fB�`B�HB�[BʬB;B̹B��B��B��B��B��B�B�B�&B�EB�eB��B=B�B�B�B�B�BqB55B<aB?tB=iB:WB6?B&�B1!B.B'�B�BqBFB��B�B��B�$B��B�fB�$B}�Bx�Bt�Br�Bq�Bm�BhvBbQBYBM�BD�BB�B:dB-B#�B#�B�B
EBB
��B
�B
�LB
�	B
��B
�>B
��B
�1B
l�B
`OB
H�B
?�B
&�B
9B	��B	�B	�B	�B	��B	ĭB	��B	�jB	�^B	��B	��B	�QB	w�B	UB	>�B	7aB	3HB	1=B	�B	�B	�B	�B	7B	*B��B�B��BżBĶB��B�#B��B��B�5B�.B�)B|Br�Br�Bj�Bh�BdyBbnB`bB]PB]PB[EB[FB[FBY;BV(BQ	BM�BN�BK�BK�BJ�BI�BH�BH�BE�BD�BD�BC�B?�B@�B;�B=�B<�B<�B7zB9�B;�B9�B7{B8�B8�B8�B6xB4kB;�BC�BE�BE�BG�BH�BF�BJ�BK�BN
BPBOBR%BT2BT2BV?BZXB\eB\eB\fB[aBT6BE�BF�BB�BB�B@�B>�B>�B@�B?�BA�BH�BNBOBP$B\nBg�Bk�Bl�Bp�Bj�Bs�BwBxB~>BEB�RB��B��B�B�B�>B�2B�9B�XB�rB��B��B��B��B��B��B��B��B��B��B�B�B�OB�oB�'B	B	|B	B	vB	�B	B	9�B	K7B	Z�B	c�B	pB	u?B	��B	��B	��B	��B	��B	�B	�QB	�yB	��B	��B	��B	��B	��B	�,B	�/B	�WB	̀B	͉B	��B	ҮB	ұB	ӺB	��B	�B	�8B	�)B	�EB	�;B	�]B	�B	��B	��B	��B	��B	��B	��B
 B
	B
B
"B

7B
MB
\B
qB
�B
�B
�B
�B
�B
�B
�B
�B
 �B
$�B
&B
)B
++B
/FB
2\B
3eB
3gB
6|B
8�B
9�B
:�B
<�B
<�B
=�B
>�B
=�B
?�B
A�B
B�B
D�B
D�B
GB
IB
K"B
K%B
J!B
N=B
OFB
POB
R]B
SgB
SjB
TsB
U|B
UB
V�B
V�B
W�B
W�B
W�B
Z�B
Z�B
[�B
\�B
\�B
\�B
\�B
]�B
^�B
`�B
a�B
a�B
cB
eB
eB
dB
f#B
h1B
lMB
lPB
lRB
lUB
ppB
qyB
q{B
pxB
q�B
p}B
q�B
s�B
r�B
s�B
s�B
u�B
v�B
u�B
w�B
w�B
y�B
{�B
z�B
z�B
z�B
{�B
{�B
{�B
|�B
{�B
}�B
B

B
B
�B
�B
�&B
�(B
�0B
�3B
�6B
�>B
�NB
�VB
�_B
�kB
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
�
B
�B
�B
�#B
�.B
�9B
�AB
�MB
�YB
�`B
�eB
�sB
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
�B
�
B
�B
�B
�"B
�)B
�4B
�JB
�aB
�oB
��B
��B
��B
��B
��B
��B
�B
�+B
�GB
�WB
�gB
�vB
��B
��B
��B
��B
��B
��B
��B
�B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                             <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�201904100500472021061413561120210614135611202106171314232021061713142320210617131423201904100500472021061413561120210614135611202106171314232021061713142320210617131423PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            new conductivity = original conductivity * (1 + delta*TEMP + CPcor_SBE*PRES) / (1 + delta*TEMP_ADJUSTED + CPcor_new*PRES_ADJUSTED)                                                                                                                              none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            new conductivity = original conductivity * (1 + delta*TEMP + CPcor_SBE*PRES) / (1 + delta*TEMP_ADJUSTED + CPcor_new*PRES_ADJUSTED)                                                                                                                              none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.50 dbar                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            CPcor_new = -1.25e-07; CPcor_SBE = -9.57e-08; delta = 3.25e-06                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.50 dbar                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            CPcor_new = -1.25e-07; CPcor_SBE = -9.57e-08; delta = 3.25e-06                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                      none                                                                                                                                                                                                                                                            New conductivity computed by using a different CPcor value from that provided by Sea-Bird.                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            No significant salinity drift detected.                                                                                                                                                                                                                         Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                      none                                                                                                                                                                                                                                                            New conductivity computed by using a different CPcor value from that provided by Sea-Bird.                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            No significant salinity drift detected.                                                                                                                                                                                                                         PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            AO  AO  ARCAARCAADJPADJP                                                                                                                                        2019041005004720190410050047  IP  IP                                G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2019041005004720190410050047QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�5F03E           703E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2019041005004720190410050047QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�0               0               PM  PM  ARSQARSQCOWGCOWGV1.1V1.1                                                                                                                                2021061713151720210617131517IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                