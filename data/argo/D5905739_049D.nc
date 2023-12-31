CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-11-30T23:00:40Z creation      
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
_FillValue                 �  _|   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  ct   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  sL   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �$   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   TEMP_ADJUSTED_ERROR          
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
_FillValue                 �  �l   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �d   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                   �   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                   �   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                   �   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                   �   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  � �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                   D   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                   `   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    h   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar        �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar        �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�       �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  �<   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  � �Argo profile    3.1 1.2 19500101000000  20181130230040  20210617131510  5905739 5905739 US ARGO PROJECT                                                 US ARGO PROJECT                                                 GREGORY C. JOHNSON                                              GREGORY C. JOHNSON                                              PRES            TEMP            PSAL            PRES            TEMP            PSAL               1   1DD  AOAO7219                            7219                            2C  2C  DD  SOLO_D_MRV                      SOLO_D_MRV                      12002                           12002                           SBE603 01Dec16                  SBE603 01Dec16                  874 874 @ؒ\�qt&@ؒ\�qt&11  @ؒ\�J�@ؒ\�J�@6���W�@6���W��c�6���c�6��11  GPS     GPS     Primary sampling: averaged [1Hz sampling by SBE-41CP averaged in 2-dbar bins]                                                                                                                                                                                   Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 AB  AA  AA  >���?���@ff@Fff@�33@�  @�  @�  A��A33A$��A>ffA`  A~ffA�  A���A���A���Aљ�AᙚA�  B   BffB33B��B ffB(  B/33B7��B@  BH  BP  BX  B`  Bh  BpffBw��B��B���B�33B�33B�33B�ffB�33B�33B�33B�33B�  B�  B�33B�33B�ffB�ffB�  B�  B�33B�  Bϙ�B�  B�ffB�33B���B�33B�  B뙚B���B�33B�ffB���C �C�fC  C�C�C
L�C  C��C�fC�C�C33CL�C�C��C�fC   C"�C$�C&33C(L�C*  C+��C-�fC0  C2�C4  C5��C8  C:33C<�C=�fC@33CB33CD  CF33CH33CJ  CL�CN  CP33CR33CT  CV  CX  CZ  C\  C]�fC_�fCa�fCc�fCf33Ch�Cj�Cl  Cn  Co�fCq��Ct  CvL�CxL�Cz33C|33C~  C�&fC��C��C�  C��3C��C��C��3C��C��3C��fC�  C��C��C�  C�  C��fC��C�  C��fC��C��C��C�  C��C��C��fC�  C��C��C��C��fC�  C�  C��C��C��C��C��C��C�  C��fC��3C��3C�  C�  C��C�  C��C��C��C��C�&fC�  C��3C��3C�  C�&fC��C��3C�  C��C��C�&fC�  C��fC��fC��3C��3C�  C�  C�  C�  C��C��C�&fC�  C��fC�  C��C�&fC�33C�  C�ٚC��fC�  C�  C��C��C��C�&fC�  C�ٚC��3C��3C��3C��3C�  C�  C�  C��C��C��C��C��C�&fC�&fC�&fC��C��C��C��C��C��C��C��C��C��C��C��C�&fC�&fC�  C�&fC�&fC�33C��C�&fD �D ` D  D� D
  D��D,�D�3Ds3D33D��D�fDy�D"FfD%3D'�3D*� D-S3D/�3D2� D4�3D7Y�D9��D;��D>&fD@s3DB�3DEfDGL�DI�fDL�DNy�DP� DS` DU� DX` DZ�fD]s3D`�Db��De�Dg�3Dj�Dl� DofDql�Ds� DvL�Dx�3D{�D}�D� D��3D��D�FfD�vfD��fD�� D�� D�fD�&fD�C3D�Y�D�s3D���D���D��3D�� D�  D��D�6fD�S3D�i�D�� D���D���D���D���D��D�C3D�ffD���D�� D�ٚD�fD�0 D�` D�� D���D�� D�#3D�S3D�� D��fD�� D�  D�P D��3D���D���D�#3D�VfD���D��3D��3D�	�D�9�D�c3D���D¶fD��3D�fD�  D�<�D�` D�y�Dʙ�Dˬ�D�� D��3D�� D��fD�3D�3D�#3D�)�D�0 D�@ D�L�D�S3D�VfD�\�D�\�D�` D�c3D�i�D�i�D�c3D�c3D�Y�D�S3D�S3D�L�D�I�D�L�D�P D�P D�P D�Y�D�i�D�vfD�3D��D��D� D��3D���D�� D�  D��D�6fD�P D�ffD�i�D�� D��3D��fD�� E h E �E{3E�E� E�E�fE��E�fEi�E{3E	�fE�E�E�3E��EfE�E� E��E E3E{3Eq�E�3EP EH E� E� E �E!~fE"l�E#�3E%H E&6fE'��E) E)�3E+VfE,��E-��E/�E0�3E1t�E2� E4K3E58 E6��E7� E9K3E:1�E;� E<ٚE?� EB�fEF[3EI9�EL\�EOx ER��EU�3EX� E\fE_9�Eb<�Ee9�Eh��Ek�3En�3Eq�3Et��Ew� E{33E~D�E���E�X E��fE�[3>���>���>���>L��>���=���>L��>���>���>L��>���>���>���>���>L��>���>���>���>���>���>���>���?   ?   ?333?333?fff?�  ?���?�33?�  ?�33@   @33@,��@@  @L��@Y��@s33@�33@���@�ff@�  @���@�ff@���@ə�@�33@�  @���@���A��A  A  AffA��A$��A+33A1��A;33G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111444141114144441144414141411111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                 ?L��?���@&ff@fff@�33@�  @�  @�  A	��A33A,��AFffAh  A�33A�  A���A���Ař�Aՙ�A噚A�  B  B
ffB33B��B"ffB*  B133B9��BB  BJ  BR  BZ  Bb  Bj  BrffBy��B���B���B�33B�33B�33B�ffB�33B�33B�33B�33B�  B�  B�33B�33B�ffB�ffB�  B�  B�33B�  BЙ�B�  B�ffB�33B���B�33B�  B왚B���B�33B�ffB���C ��CffC� C��C��C
��C� CL�CffC��C��C�3C��C��CL�CffC � C"��C$��C&�3C(��C*� C,L�C.ffC0� C2��C4� C6L�C8� C:�3C<��C>ffC@�3CB�3CD� CF�3CH�3CJ� CL��CN� CP�3CR�3CT� CV� CX� CZ� C\� C^ffC`ffCbffCdffCf�3Ch��Cj��Cl� Cn� CpffCrL�Ct� Cv��Cx��Cz�3C|�3C~� C�ffC�Y�C�L�C�@ C�33C�Y�C�L�C�33C�L�C�33C�&fC�@ C�Y�C�L�C�@ C�@ C�&fC�L�C�@ C�&fC�L�C�Y�C�Y�C�@ C�Y�C�L�C�&fC�@ C�L�C�Y�C�L�C�&fC�@ C�@ C�L�C�Y�C�Y�C�Y�C�Y�C�Y�C�@ C�&fC�33C�33C�@ C�@ C�L�C�@ C�L�C�L�C�Y�C�Y�C�ffC�@ C�33C�33C�@ C�ffC�L�C�33C�@ C�L�C�L�C�ffC�@ C�&fC�&fC�33C�33C�@ C�@ C�@ C�@ C�L�C�Y�C�ffC�@ C�&fC�@ C�L�C�ffC�s3C�@ C��C�&fC�@ C�@ C�L�C�L�C�L�C�ffC�@ C��C�33C�33C�33C�33C�@ C�@ C�@ C�L�C�Y�C�Y�C�L�C�Y�C�ffC�ffC�ffC�Y�C�Y�C�Y�C�Y�C�Y�C�L�C�L�C�L�C�L�C�L�C�Y�C�Y�C�ffC�ffC�@ C�ffC�ffC�s3C�Y�C�ffD ,�D � D@ D� D
  D��DL�D�3D�3DS3D�D�fD��D"ffD%33D'�3D*� D-s3D03D2� D53D7y�D9ٚD<�D>FfD@�3DB�3DE&fDGl�DI�fDL,�DN��DQ  DS� DV  DX� D[fD]�3D`,�Db��De9�Dg�3Dj,�Dl� Do&fDq��Dt  Dvl�Dx�3D{9�D},�D� D�3D�,�D�VfD��fD��fD�� D�� D�fD�6fD�S3D�i�D��3D���D���D��3D�� D� D�,�D�FfD�c3D�y�D�� D���D���D���D��D�,�D�S3D�vfD���D�� D��D�fD�@ D�p D�� D�ɚD�  D�33D�c3D�� D��fD�  D�0 D�` D��3D�ɚD���D�33D�ffD���D��3D��3D��D�I�D�s3D���D��fD��3D�fD�0 D�L�D�p Dɉ�Dʩ�D˼�D�� D��3D�� D�fD�3D�#3D�33D�9�D�@ D�P D�\�D�c3D�ffD�l�D�l�D�p D�s3D�y�D�y�D�s3D�s3D�i�D�c3D�c3D�\�D�Y�D�\�D�` D�` D�` D�i�D�y�D�fD�3D��D��D�� D��3D���D�  D� D�,�D�FfD�` D�vfD�y�D�� D��3D��fD�� E p E ��E�3E�E� E!�E�fE��E�fEq�E�3E	�fE�E!�E�3E��E&fE!�E� E��E E3E�3Ey�E�3EX EP E� E� E !�E!�fE"t�E#�3E%P E&>fE'��E) E)�3E+^fE,ɚE-��E/!�E0�3E1|�E2� E4S3E5@ E6��E7� E9S3E:9�E;� E<�E?� EB�fEFc3EIA�ELd�EO� ER��EU�3EX� E\fE_A�EbD�EeA�Eh��Ek�3En�3Er3Eu�Ex  E{;3E~L�E�ŚE�\ E��fE�_3G�O�G�O�G�O�?333G�O�?��?333?L��G�O�?333G�O�G�O�G�O�G�O�?333?L��G�O�G�O�G�O�?L��G�O�?L��G�O�?�  G�O�?���?�33?�  ?ٙ�?�33@   @��@   @333@L��@`  @l��@y��@���@�33@���@�ff@�  @���@�ff@���@ٙ�@�33@�  @���A��A	��A  A  AffA$��A,��A333A9��AC33G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111444141114144441144414141411111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                 @ @ �@ V@ *@ �@ ""@ (�@ /�@ 7L@ >�@ FQ@ Q=@ _�@ l�@ z�@ �7@ ��@ �5@ ��@ ��@ ��@ �t@ ��@ ��@�@�@�@+@9X@G�@UU@b�@p�@~K@��@�H@��@�9@��@��@ލ@�4@��@�@*@"�@0x@=q@K@Yn@g@uk@�@�\@�@�Y@�R@Ĝ@Ӡ@�@�@��@
�@�@$.@2�@A�@O�@^5@j@v�@�@�u@�@�!@��@�@�[@�`@�@^@�@�@'�@5�@DD@R�@`B@n�@|�@��@��@�(@�~@��@��@�@�@� @@b@ @-�@:@I@V�@b�@qS@~K@�P@��@�A@��@@�7@��@��@�~@%@�@#�@0x@>@K@X�@e�@r�@��@�h@�@�@��@��@խ@�@�@��@	�@B@&;@2�@A�@M�@Z�@i�@x�@��@��@�m@��@�k@�c@խ@�`@�@^@�@�@)�@5?@DD@R�@`�@m�@y�@��@�0@��@��@��@�*@��@�y@��@	@	b@	
@	,`@	:@	H]@	UU@	c�@	qS@	�@	�P@	��@	�A@	�9@	��@	�7@	��@	�4@	�~@
�@
*@
"�@
1�@
=q@
I�@
Wb@
e�@
s_@
��@
�\@
�@
��@
�@
�W@
խ@
�H@
�@
��@
�@�@(G@33@>�@M$@\)@i�@x&@��@�u@�y@�@��@ȴ@�[@�@�@  @�@O@)�@7�@E�@R�@`�@oF@|�@��@��@�5@��@��@�*@�#@��@�q@@�@ @-�@<@I�@UU@e	@r�@�@�P@��@��@�~@33@t@�R@��@E�@��@խ@ �@k.@��@�@O0@��@�@33@|�@Ĝ@
=@M$@��@ψ@�@H]@�+@Ĝ@@B8@�d@��@%@G�@��@�7@{@Yn@�@�@+�@o�@��@� @:@~�@��@j@E�@�+@ȴ@��@@�@�d@��@]@B8@�@�&@��@;d@x�@��@��@-@i!@��@��@�@\)@��@��@ �@ M$@ ��@ Ĝ@!�@!@,@!}�@!�@!��@"7�@"v�@"��@"�e@#4�@#t@#��@#��@$5@@$ww@$�@$��@%:@%|?@%�&@&  @&@�@&�d@&Ĝ@'%@'H]@'��@'�@(
�@(K�@(��@(�o@)
�@)I�@)��@)�@*1@*DD@*�@*�&@*�9@+8�@+s_@+�@+��@,""@,]�@,��@,��@-
�@-B�@-z�@-��@-�@@.&;@.]�@.��@.�@/j@/:�@/r�@/�M@/ލ@0*@0I�@0~�@0��@0��@1 �@1X@1�\@1��@1��@25@@2oF@2��@2��@3�@3T�@3�\@3�@4%@4@�@4z�@4��@4�@5/�@5k.@5�z@5��@6�@6SI@6�\@6�c@7@7>�@7y�@7��@7�@8+�@8�z@96@9ƨ@:;d@:�f@;V@;ƨ@<n�@<�#@=}�@=��@>��@>��@?�0@@]@@��@A
�@A��@BDD@B�@CK@C��@DO1@D�l@EM$@E�@F�@F��@G�+@H�@H�@I�@I��@J�@J��@KO�@K��@LQ�@L��@MQ�@M�@Nz�@O@OqS@Pj@P�u@Q��@S+�@T��@U�#@W1�@X�@Y�e@[-�@\~K@]�l@_> @`�+@a�|@c3�@d��@e�h@g>�@h��@i��@k-�@l|�@m�<@o:@p�h@q�G�O�G�O�G�O�@ ^G�O�@  �@ ^@ G�O�@ ^G�O�G�O�G�O�G�O�@ ^@ G�O�G�O�G�O�@ G�O�@ G�O�@ jG�O�@ �@ %@ �@ 1@ 	�@ 
=@ �@ �@ �@ o@ {@ �@ 6@ �@ �@ 
@  @ ""@ $.@ &�@ (G@ +@ -@ /�@ 2�@ 5?@ 7L@ :@ =q@ @,@ B�@ FQ@ I@ K�@ O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�oA�JA�JA��A��A� �A� �A� �A� �A�"�A� �A�"�A� �A�$�A�"�A�$�A�$�A�&�A�&�A�&�A�&�A�(�A�(�A�+A�+A�/A�-A�1'A�1'A�1'A�-A�5?A�9XA�=qA�=qA�=qA�A�A�C�A�E�A�E�A�?}A�`BA���A��HA�\)A���A���A�~�A���A�Q�A�%A�1'A�;dA��DA�
=A���A�O�A���A���A��^A�x�A�=qA�ȴA�bNA��TA�jA��yA�|�A� �A��#A�z�A���A�^5A�%A��-A�`BA��A���A�`BA���A��
A��A�|�A�A���A���A���A�|�A���A�p�A��A���A��A�M�A���A�l�A�JA�dZA�A��DA�r�A�O�A��yA�G�A��A�5?A�I�A�`BA��A�/A�1'A�~�A��!A�G�A�ZA��hA��A�\)A�~�A�I�A��/A�C�A��/A�x�A��
A�t�A���A��wA��A}G�A{�AxM�At�9AsoAr1Ap��An�DAlVAj-Ai/Ah��AhbNAhA�Ah=qAg��Ae?}Aa��A`ȴA`��A`��A`��A`��A`��A`�uA_�A_x�A^$�AY�AW�AV��AU�-ASK�ARAP�HAN��AKVAHA�AE�#AD�yAC�AB�A@ĜA>�A=�7A=|�A=�A<��A8A�A6A�A5XA4{A2VA.I�A-K�A+�mA)�A)�PA)&�A)
=A)�A)"�A(��A(�yA%�A$��A#C�A"ZA"-A"1A!�#A ȴAz�A��A5?AI�AVA�AAhsAK�AA�RAC�A�AA�/A��AjAJAx�A�+AM�A �Ap�A
ȴA
�DA	hsA	
=Az�AƨA�
A
=A�DA�mAȴA�A�;AoA 1@��@��@���@�o@�7L@��D@�K�@�{@��/@�\@�^5@�n�@���@���@�
=@�@�C�@�7@�+@�V@�5?@��T@���@���@۾w@�p�@�hs@�`B@˝�@Ǯ@��@�=q@�E�@��F@�C�@�+@��R@��F@�l�@�1'@�(�@�C�@��@�@�p�@� �@�
=@��\@�(�@�33@��@��R@���@���@�j@��F@�t�@�{@���@�z�@���@��y@��h@��j@�|�@�=q@��@���@���@��;@��!@���@���@���@��T@���@l�@
=@|�/@z��@x�u@w��@vv�@u@t��@q��@o��@m`B@l��@k33@hr�@e�@d�j@a��@`��@`  @]��@Z��@Y%@Wl�@V{@UV@R�\@Q��@O�P@M/@L(�@J�H@IX@H�9@G�w@G;d@E@B�H@Ahs@?+@=p�@<�/@;�
@;t�@9�@9�@8A�@6E�@4�j@3"�@1��@0r�@/+@.��@,�@+dZ@*��@)��@)G�@(��@(Q�@'|�@&��@&{@%�@$�@$��@#33@"�H@!�^@!�7@ �9@  �@�w@;d@�@{@��@�@(�@ƨ@@�\@�^@hs@|�@ȴ@5?@@�@j@S�@33@�H@��@�@7L@�@Ĝ@A�@;d@��@�y@��@V@E�@/@�@�
@
�@
J@	�7@	X@�;@K�@ȴ@{@��@�@��@9X@dZ@��@hs@ b?��?�?�X?�ff?��?��?��`?�R?��?��H?陚?��?��?��?���?�{?��?�7L?�r�?��y?֧�?�?�?��?ӕ�?Ұ!?ѩ�?�G�?��;?�\)?���?�O�?�I�?�dZ?�^5?ɺ^?�7L?�1'?�K�?�
=?Ƨ�?�?�Z?�o?���?���?�  ?� �?�\)?�V?�/?��D?�dZ?�C�?�^5?���?�x�?�x�?�7L?���?�X?���?�^5?�?��?�I�?��A��A�oA��A��A��A��A�{A�oA�{A�oA�oA�bA�{A�{A��A��A��A�{A�{A�
=A�1A�JA�
=A�JA�JA�A�{A�JA�bA�VA�VA�JA�JA�JA�oA��A��A��A��A��A��A� �A�"�A� �A� �A� �A� �A� �A� �A�"�A�"�A� �A� �A�"�A� �A� �A� �A� �A� �A� �G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                 A�oA�JA�JA��A��A� �A� �A� �A� �A�"�A� �A�"�A� �A�$�A�"�A�$�A�$�A�&�A�&�A�&�A�&�A�(�A�(�A�+A�+A�/A�-A�1'A�1'A�1'A�-A�5?A�9XA�=qA�=qA�=qA�A�A�C�A�E�A�E�A�?}A�`BA���A��HA�\)A���A���A�~�A���A�Q�A�%A�1'A�;dA��DA�
=A���A�O�A���A���A��^A�x�A�=qA�ȴA�bNA��TA�jA��yA�|�A� �A��#A�z�A���A�^5A�%A��-A�`BA��A���A�`BA���A��
A��A�|�A�A���A���A���A�|�A���A�p�A��A���A��A�M�A���A�l�A�JA�dZA�A��DA�r�A�O�A��yA�G�A��A�5?A�I�A�`BA��A�/A�1'A�~�A��!A�G�A�ZA��hA��A�\)A�~�A�I�A��/A�C�A��/A�x�A��
A�t�A���A��wA��A}G�A{�AxM�At�9AsoAr1Ap��An�DAlVAj-Ai/Ah��AhbNAhA�Ah=qAg��Ae?}Aa��A`ȴA`��A`��A`��A`��A`��A`�uA_�A_x�A^$�AY�AW�AV��AU�-ASK�ARAP�HAN��AKVAHA�AE�#AD�yAC�AB�A@ĜA>�A=�7A=|�A=�A<��A8A�A6A�A5XA4{A2VA.I�A-K�A+�mA)�A)�PA)&�A)
=A)�A)"�A(��A(�yA%�A$��A#C�A"ZA"-A"1A!�#A ȴAz�A��A5?AI�AVA�AAhsAK�AA�RAC�A�AA�/A��AjAJAx�A�+AM�A �Ap�A
ȴA
�DA	hsA	
=Az�AƨA�
A
=A�DA�mAȴA�A�;AoA 1@��@��@���@�o@�7L@��D@�K�@�{@��/@�\@�^5@�n�@���@���@�
=@�@�C�@�7@�+@�V@�5?@��T@���@���@۾w@�p�@�hs@�`B@˝�@Ǯ@��@�=q@�E�@��F@�C�@�+@��R@��F@�l�@�1'@�(�@�C�@��@�@�p�@� �@�
=@��\@�(�@�33@��@��R@���@���@�j@��F@�t�@�{@���@�z�@���@��y@��h@��j@�|�@�=q@��@���@���@��;@��!@���@���@���@��T@���@l�@
=@|�/@z��@x�u@w��@vv�@u@t��@q��@o��@m`B@l��@k33@hr�@e�@d�j@a��@`��@`  @]��@Z��@Y%@Wl�@V{@UV@R�\@Q��@O�P@M/@L(�@J�H@IX@H�9@G�w@G;d@E@B�H@Ahs@?+@=p�@<�/@;�
@;t�@9�@9�@8A�@6E�@4�j@3"�@1��@0r�@/+@.��@,�@+dZ@*��@)��@)G�@(��@(Q�@'|�@&��@&{@%�@$�@$��@#33@"�H@!�^@!�7@ �9@  �@�w@;d@�@{@��@�@(�@ƨ@@�\@�^@hs@|�@ȴ@5?@@�@j@S�@33@�H@��@�@7L@�@Ĝ@A�@;d@��@�y@��@V@E�@/@�@�
@
�@
J@	�7@	X@�;@K�@ȴ@{@��@�@��@9X@dZ@��@hs@ b?��?�?�X?�ff?��?��?��`?�R?��?��H?陚?��?��?��?���?�{?��?�7L?�r�?��y?֧�?�?�?��?ӕ�?Ұ!?ѩ�?�G�?��;?�\)?���?�O�?�I�?�dZ?�^5?ɺ^?�7L?�1'?�K�?�
=?Ƨ�?�?�Z?�o?���?���?�  ?� �?�\)?�V?�/?��D?�dZ?�C�?�^5?���?�x�?�x�?�7L?���?�X?���?�^5?�?��?�I�?��A��A�oA��A��A��A��A�{A�oA�{A�oA�oA�bA�{A�{A��A��A��A�{A�{A�
=A�1A�JA�
=A�JA�JA�A�{A�JA�bA�VA�VA�JA�JA�JA�oA��A��A��A��A��A��A� �A�"�A� �A� �A� �A� �A� �A� �A�"�A�"�A� �A� �A�"�A� �A� �A� �A� �A� �A� �G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                 ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B�+B�%B�%B�+B�%B�%B�%B�%B�%B�%B�%B�+B�%B�%B�%B�+B�%B�+B�%B�+B�+B�+B�+B�+B�+B�+B�+B�+B�%B�%B�+B�+B�+B�+B�%B�%B�%B�%B�%B�%B�%Bs�BVBs�Bp�Bl�Bk�BhsBs�Bs�Br�Bs�Bw�B~�B�B�B�=B�+B�+B�PB�hB�hB�7B�%B�DB�VB�oB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�!B�B��B��B��B��B��B�bBy�Bk�BjBbNBD�B.B#�B�B�B�B�B�BĜB��B�JB~�Br�Be`BS�B;dB.B{B
��B
�yB
�BB
�B
B
�9B
�B
��B
��B
�uB
�+B
x�B
_;B
P�B
@�B
-B
&�B
B	��B	�B	�NB	�B	B	�'B	��B	��B	��B	��B	��B	��B	��B	�B	t�B	n�B	m�B	l�B	k�B	jB	k�B	k�B	k�B	iyB	dZB	M�B	I�B	F�B	8RB	,B	�B	�B��B�ZB��BƨBĜB�dB�?B��B��B��B��B��B��B��B�hB�1B�Bt�BdZBaHBXBT�BXB`BBiyBn�Bv�B{�B�%B�%B�7B�B�B�B� B~�B{�Bq�BgmBaHBXBW
BO�BO�BN�BL�BK�BH�BG�BG�BC�BE�BC�BA�B@�B>wB=qB>wB=qB<jB=qB9XB:^B9XB7LB6FB49B6FB7LB8RB<jB<jB<jB>wBB�BE�BH�BH�BF�BH�BF�BB�BA�B=qB9XB<jB>wB<jB:^B:^B8RB>wBM�BO�BQ�BP�BP�BO�BO�BS�B`BBM�BQ�BVBx�Bo�B|�B�=B�hB��B�3B�3BȴB�sB	\B	!�B	33B	A�B	C�B	S�B	ZB	aHB	cTB	v�B	u�B	|�B	�B	w�B	�JB	��B	��B	��B	�3B	�XB	�qB	�}B	��B	ȴB	��B	��B	�B	�#B	�HB	�TB	�mB	�mB	�B	�B	�B	��B	��B	��B	��B	��B
B
B
+B
1B

=B
DB
JB
VB
oB
oB
{B
�B
�B
�B
�B
�B
�B
"�B
%�B
'�B
(�B
)�B
,B
/B
.B
0!B
2-B
33B
49B
5?B
6FB
7LB
8RB
:^B
=qB
@�B
@�B
A�B
B�B
D�B
D�B
E�B
G�B
G�B
G�B
I�B
J�B
L�B
N�B
O�B
P�B
R�B
S�B
S�B
T�B
T�B
VB
VB
W
B
XB
XB
ZB
ZB
ZB
\)B
[#B
]/B
\)B
^5B
^5B
_;B
_;B
`BB
aHB
aHB
bNB
bNB
cTB
dZB
dZB
e`B
e`B
gmB
hsB
iyB
iyB
iyB
iyB
l�B
k�B
l�B
l�B
l�B
m�B
n�B
n�B
n�B
p�B
q�B
p�B
q�B
p�B
q�B
r�B
s�B
t�B
u�B
v�B
w�B
w�B
y�B
x�B
z�B
z�B
{�B
{�B
{�B
|�B
}�B
}�B
� B
�B
�B
�B
�B
�%B
�1B
�=B
�=B
�JB
�PB
�\B
�\B
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
�B
�B
�B
�B
�B
�!B
�'B
�'B
�-B
�9B
�?B
�?B
�FB
�RB
�RB
�LB
�XB
�XB
�XB
�XB
�XB
�XB
�^B
�^B
�XB�+B�1B�+B�+B�%B�+B�B�+B�+B�%B�+B�+B�+B�%B�%B�+B�+B�%B�%B�1B�+B�+B�B�1B�B�+B�B�+B�%B�%B�%B�%B�+B�%B�1B�%B�%B�%B�%B�%B�+B�%B�B�+B�%B�%B�%B�%B�%B�%B�%B�%B�%B�%B�%B�+B�%B�%B�+B�+G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                 B�B��B��B�B��B��B��B��B��B��B��B�B� B�B�B�B�B�
B�B�B�B�B�B�B�B�B�B�B�
B�B�B�B�B�B�B�B�B�B�B�B�Bs�BU�Bs�Bp�BlxBksBhaBs�Bs�Br�Bs�Bw�B~�B��B�B�0B�B�B�EB�]B�^B�-B�B�;B�NB�hB��B��B��B��B�|B��B��B�~B��B��B��B��B��B��B��B��B�B�$B�B��B��B��B��B��B�hBy�Bk�Bj�BbVBD�B.B#�B�B�B�B�B�BĩB��B�XBBr�BeoBTB;tB.$B�B
��B
�B
�TB
�B
¢B
�LB
�B
��B
��B
��B
�AB
x�B
_QB
P�B
@�B
-&B
'B
*B	� B	�B	�hB	�B	©B	�BB	��B	��B	��B	��B	��B	��B	��B	�>B	t�B	n�B	m�B	l�B	k�B	j�B	k�B	k�B	k�B	i�B	dB	M�B	I�B	F�B	8xB	,/B	�B	�B�B�B�'B��B��B��B�iB�'B�B��B�	B�B��B��B��B�_B�;Bt�Bd�BaxBX@BU/BXAB`tBi�Bn�Bv�B|B�ZB�ZB�mB�BB�=B�=B�8B2B| Bq�Bg�Ba�BXJBWDBPBPBOBM	BLBH�BG�BG�BC�BE�BC�BA�B@�B>�B=�B>�B=�B<�B=�B9�B:�B9�B7�B6�B4�B6�B7�B8�B<�B<�B<�B>�BB�BE�BIBIBF�BIBF�BB�BA�B=�B9�B<�B>�B<�B:�B:�B8�B>�BN(BP4BRBBQ<BQ<BP7BP7BTUB`�BN5BRQBVlBy@BpB}_B��B��B�KB��B��B�8B��B	�B	"XB	3�B	BB	D,B	T�B	Z�B	a�B	c�B	wmB	vjB	}�B	��B	x~B	��B	�;B	�uB	��B	��B	�B	�5B	�DB	�SB	ɁB	ΣB	��B	��B	��B	�#B	�2B	�NB	�QB	�rB	��B	�B	��B	��B	��B	��B	��B
B
B
-B
	6B
DB
NB
VB
eB
�B
�B
�B
�B
�B
�B
�B
�B
�B
#�B
'B
)B
*'B
+0B
-?B
0TB
/PB
1`B
3nB
4wB
5�B
6�B
7�B
8�B
9�B
;�B
>�B
A�B
A�B
B�B
C�B
FB
FB
GB
IB
I B
I#B
K2B
L;B
NJB
PYB
QbB
RkB
TzB
U�B
U�B
V�B
V�B
W�B
W�B
X�B
Y�B
Y�B
[�B
[�B
[�B
]�B
\�B
^�B
]�B
_�B
_�B
`�B
`�B
bB
c
B
cB
dB
dB
eB
f(B
f*B
g3B
g5B
iDB
jMB
kUB
kWB
kZB
k\B
nqB
mmB
nuB
nxB
n{B
o�B
p�B
p�B
p�B
r�B
s�B
r�B
s�B
r�B
s�B
t�B
u�B
v�B
w�B
x�B
y�B
y�B
{�B
z�B
}B
}B
~B
~B
~B
B
�*B
�/B
�CB
�NB
�_B
�mB
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
�B
�B
�$B
�/B
�IB
�NB
�TB
�gB
�lB
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
� B
�B
�B
�B
�+B
�0B
�6B
�BB
�]B
�lB
��B
��B
��B
��B
��B
��B
��B
�B
�-B
�<B
�RB
�nB
�}B
��B
��B
��B
��B
��B
��B
��B
�B
�B
�B�B�B�B�B��B�B��B�B�B��B�B�B�B��B��B�B�B��B��B�B�B�B��B�B��B�B��B�B��B��B��B��B�B��B�	B��B��B��B��B��B�B��B��B�B��B��B��B��B��B��B��B��B��B��B��B�B��B��B�B�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                 <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�201811302300402021061413555520210614135555202106171313392021061713133920210617131339201811302300402021061413555520210614135555202106171313392021061713133920210617131339PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            new conductivity = original conductivity * (1 + delta*TEMP + CPcor_SBE*PRES) / (1 + delta*TEMP_ADJUSTED + CPcor_new*PRES_ADJUSTED)                                                                                                                              none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            new conductivity = original conductivity * (1 + delta*TEMP + CPcor_SBE*PRES) / (1 + delta*TEMP_ADJUSTED + CPcor_new*PRES_ADJUSTED)                                                                                                                              none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.50 dbar                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            CPcor_new = -1.25e-07; CPcor_SBE = -9.57e-08; delta = 3.25e-06                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.50 dbar                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            CPcor_new = -1.25e-07; CPcor_SBE = -9.57e-08; delta = 3.25e-06                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                      none                                                                                                                                                                                                                                                            New conductivity computed by using a different CPcor value from that provided by Sea-Bird.                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            No significant salinity drift detected.                                                                                                                                                                                                                         Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                      none                                                                                                                                                                                                                                                            New conductivity computed by using a different CPcor value from that provided by Sea-Bird.                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            No significant salinity drift detected.                                                                                                                                                                                                                         PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            AO  AO  ARCAARCAADJPADJP                                                                                                                                        2018113023004020181130230040  IP  IP                                G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2018113023004020181130230040QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�5F03E           703E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2018113023004020181130230040QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�0               0               PM  PM  ARSQARSQCOWGCOWGV1.1V1.1                                                                                                                                2021061713151020210617131510IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                