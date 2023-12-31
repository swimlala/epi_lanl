CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-07-24T22:02:48Z creation      
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
_FillValue                 �  _�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  c�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  s�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �x   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �t   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �d   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �T   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �P   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �@   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �<   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                   �   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                   �   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                   �   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                   �   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  � �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                   	4   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                   	P   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    	X   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar        	x   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar        	�   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�       	�   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    	�   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  �,   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  � �Argo profile    3.1 1.2 19500101000000  20180724220248  20210617131455  5905739 5905739 US ARGO PROJECT                                                 US ARGO PROJECT                                                 GREGORY C. JOHNSON                                              GREGORY C. JOHNSON                                              PRES            TEMP            PSAL            PRES            TEMP            PSAL                  DD  AOAO7219                            7219                            2C  2C  DD  SOLO_D_MRV                      SOLO_D_MRV                      12002                           12002                           SBE603 01Dec16                  SBE603 01Dec16                  874 874 @�g�y]B@�g�y]B11  @�g�q�7�@�g�q�7�@6�4C�k'@6�4C�k'�c�Lnm���c�Lnm��11  GPS     GPS     Primary sampling: averaged [1Hz sampling by SBE-41CP averaged in 2-dbar bins]                                                                                                                                                                                   Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 AB  AA  AA  >���?�  @ff@@  @�  @�33@�  @�  A��A  A#33A>ffA^ffA~ffA�  A�33A���A���Aљ�A�  A�33A�ffB  BffBffB ��B(ffB0  B7��B@ffBH��BPffBW��B`  BhffBp  Bx  B�33B�33B�33B�  B�33B�  B�  B���B�  B�  B�33B�33B�  B���B�  B�33B�33B�ffB���B˙�B�33Bԙ�B�ffB�33B�33B�  B�ffB�ffB�  B���B�ffB�33C �C  C  C��C�C
33C�C  C�C  C�fC�C�C�fC�C33C �C"�C$  C&  C'�fC)��C,�C.  C0  C233C433C5�fC833C:�C<  C>  C?��CB  CC�fCE��CH  CI�fCK�3CN  CP33CR�CT  CVL�CX33CZ  C\L�C^33C`  CbL�Cd�Cf  Ch�Cj  Ck�fCn�Co�fCq��Ct  Cv�Cx�Cy��C|  C}�fC��C�  C��C��3C��fC�  C��C�  C��fC�  C��C�&fC��C��fC��fC��3C�  C��C��3C��fC��3C��C�33C��C��3C��C�&fC��C��3C��C�  C�ٚC��3C��C�&fC��C��3C��C��C�  C��3C�  C��C��C��3C��C��C�&fC��C�  C��C��C��C��3C��C�  C��fC��3C��C�  C��3C��C�  C��3C��C�  C��fC��C�&fC��C�  C�&fC��C��C�&fC��C�  C��C��C��fC��C�&fC��C��3C��C�&fC��C�  C��C�  C��3C��C��3C��fC��C�  C��3C��C��C��3C��C��C��3C��C�  C��fC��C��C��3C��C�  C��3C��C�  C��3C��C��C��C�  C��3C��C�  C��3C��D��D  D�3D
� D&fD�3DS3D��DFfD�3DS3D�3D!S3D#�fD&y�D)3D+��D.@ D0� D3��D633D8� D;� D>�D@��DC@ DE�3DHs3DK  DM��DPS3DR� DUs3DXfDZ��D]L�D_��Dby�De&fDg��Djs3Dm,�Do��Dry�Du3Dw�3DzS3D|�fD&fD��fD�0 D�y�D��fD�fD�c3D��3D���D�33D�s3D��fD��3D�33D�s3D�� D��fD�#3D�S3D��fD���D���D��D�I�D��3D���D��D��D�I�D�|�D��fD���D��D�S3D���D���D�3D�<�D�vfD���D���D�C3D��3D��fD��D�S3D�� D��3D� D�P D���D�� D�3D�C3D D�� D��3D�0 D�i�DȦfD��fD� D�FfD�y�DΩ�D��fD�	�D�6fD�c3Dԉ�Dհ D��3D��fD��D�@ D�` D�y�DݖfDަfD߼�D��3D��fD��fD�	�D��D�6fD�I�D�` D�vfD� D�fD�ٚD���D��D�C3D�l�D�fD��3D���D�)�D�Y�D��fD��fD��fD�,�D�p D���D���E � E,�E�fEnfEfE� EQ�E�fE�fE+3EɚEffE3E��E	;3E	�3E
l�EfEњE��E!�E@ E` E3E E$�E�3E�3E$�E  E�fE�fEњE6fE � E!��E"�3E$X E%H E&� E(+3E)!�E*�fE+� E,њE.1�E/��E0�fE1� E3D�E4��E5|�E6��E8fE9^fE:� E;��E?1�EB�EEi�EHD�EK� EN��EQ��ET�3EX9�EZ�3E^( Ea9�Ed�3Eg�3Ej�fEm� Eq3Et�Ew Ezy�E}q�E�\ E��fE�\�E���E�}�E� �E���E�<�E�� E�U�E��E�h E�� E�ZfE��3E�  E�A�E�� E���>L��>���>���>���>L��>���>���>���>L��>���>L��>���>���>���>L��>���>���>���>���>���?   ?   ?   ?   ?333?fff?�  ?���?���?�ff?���?�ff@   @��@��@   @,��@@  @Y��@fff@s33@�ff@�  @�ff@�33@���@���@�33@�33@�  @���@���A��A33A33A33A#33A+33A1��A;33G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114414441411441141414441111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                  ?L��?�  @&ff@`  @�  @�33@�  @�  A	��A  A+33AFffAfffA�33A�  A�33A���Ař�Aՙ�A�  A�33B33B
  BffBffB"��B*ffB2  B9��BBffBJ��BRffBY��Bb  BjffBr  Bz  B�33B�33B�33B�  B�33B�  B�  B���B�  B�  B�33B�33B�  B���B�  B�33B�33B�ffB���B̙�B�33Bՙ�B�ffB�33B�33B�  B�ffB�ffB�  B���B�ffB�33C ��C� C� CL�C��C
�3C��C� C��C� CffC��C��CffC��C�3C ��C"��C$� C&� C(ffC*L�C,��C.� C0� C2�3C4�3C6ffC8�3C:��C<� C>� C@L�CB� CDffCFL�CH� CJffCL33CN� CP�3CR��CT� CV��CX�3CZ� C\��C^�3C`� Cb��Cd��Cf� Ch��Cj� ClffCn��CpffCrL�Ct� Cv��Cx��CzL�C|� C~ffC�&fC�@ C�L�C�33C�&fC�@ C�Y�C�@ C�&fC�@ C�L�C�ffC�L�C�&fC�&fC�33C�@ C�L�C�33C�&fC�33C�L�C�s3C�Y�C�33C�L�C�ffC�L�C�33C�L�C�@ C��C�33C�L�C�ffC�L�C�33C�L�C�Y�C�@ C�33C�@ C�Y�C�L�C�33C�L�C�Y�C�ffC�Y�C�@ C�L�C�Y�C�L�C�33C�L�C�@ C�&fC�33C�Y�C�@ C�33C�L�C�@ C�33C�L�C�@ C�&fC�L�C�ffC�Y�C�@ C�ffC�Y�C�L�C�ffC�Y�C�@ C�Y�C�L�C�&fC�L�C�ffC�L�C�33C�L�C�ffC�L�C�@ C�Y�C�@ C�33C�L�C�33C�&fC�Y�C�@ C�33C�L�C�L�C�33C�Y�C�L�C�33C�L�C�@ C�&fC�L�C�L�C�33C�L�C�@ C�33C�L�C�@ C�33C�Y�C�L�C�L�C�@ C�33C�L�C�@ C�33C�L�D��D@ D�3D
� DFfD�3Ds3D��DffD�3Ds3D�3D!s3D$fD&��D)33D+��D.` D1  D3��D6S3D9  D;� D>,�D@��DC` DE�3DH�3DK@ DMٚDPs3DS  DU�3DX&fDZ��D]l�D`�Db��DeFfDg��Dj�3DmL�Do��Dr��Du33Dw�3Dzs3D|�fDFfD��fD�@ D���D��fD�&fD�s3D��3D���D�C3D��3D��fD�3D�C3D��3D�� D��fD�33D�c3D��fD�ɚD���D�,�D�Y�D��3D�ɚD���D�)�D�Y�D���D��fD���D�,�D�c3D���D���D�3D�L�D��fD���D�	�D�S3D��3D��fD��D�c3D�� D��3D�  D�` D���D�� D�3D�S3D D�� D�3D�@ D�y�DȶfD��fD�  D�VfD͉�Dι�D��fD��D�FfD�s3Dԙ�D�� D��3D�fD�,�D�P D�p D܉�DݦfD޶fD���D��3D��fD�fD��D�,�D�FfD�Y�D�p D�fD� D��fD��D�	�D�,�D�S3D�|�D�fD��3D�	�D�9�D�i�D��fD��fD�fD�<�D�� D���D���E � E4�E�fEvfEfE� EY�E�fE�fE33EњEnfE3E��E	C3E	�3E
t�EfEٚE�E)�EH Eh E3E  E,�E�3E�3E,�E( E�fE�fEٚE>fE � E!��E"�3E$` E%P E&� E(33E))�E*�fE+� E,ٚE.9�E/��E0�fE1� E3L�E4��E5��E6��E8fE9ffE:� E;��E?9�EB$�EEq�EHL�EK� EN��EQɚET�3EXA�E[3E^0 EaA�Ed�3Eg�3Ej�fEm� Eq3Et�Ew Ez��E}y�E�` E��fE�`�E���E���E�$�E���E�@�E�� E�Y�E��E�l E�� E�^fE��3E� E�E�E�� E���?333?L��G�O�G�O�?333G�O�G�O�G�O�?333G�O�?333?L��G�O�G�O�?333?L��G�O�?L��G�O�?fffG�O�G�O�G�O�?�  ?���?�33?�  ?���?ٙ�?�ff@ff@33@   @,��@9��@@  @L��@`  @y��@�33@���@�ff@�  @�ff@�33@���@ə�@�33@�33@�  @���A��A��A33A33A#33A+33A333A9��AC33G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114414441411441141414441111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                  @ @ �@ V@ {@ O@ "�@ (�@ /�@ 7L@ =q@ E�@ Q=@ ^�@ l�@ z�@ ��@ ��@ �5@ ��@ �&@ �@ �@ �@ �q@@o@g@,`@9X@H]@V�@c�@o�@~K@��@��@�A@��@�>@��@��@�4@�,@�@�@""@/�@>@K�@X�@e�@t@�d@�@�a@��@��@ƨ@խ@�@�@�E@
=@B@&�@33@B�@O�@\�@j@ww@�@�h@�@�r@�k@�c@׹@�@�@ �@V@�@)�@7�@D�@R�@_�@m:@z3@�+@��@��@�~@��@�*@��@�y@�q@j@@[@,`@9X@FQ@UU@bN@n�@~K@�P@�H@�A@��@��@�7@��@��@�,@�@*@""@0x@=q@Ji@Yn@e�@r�@��@�@��@�M@�R@�J@�C@�H@�@��@�@�@&�@33@?}@N�@\�@k�@x&@��@�h@��@�@�k@ȴ@խ@�@�@�@@�@)�@8�@D�@Q=@`B@m:@x�@��@��@��@�-@�w@�|@��@�@�@	j@	o@	g@	+�@	:�@	I@	Wb@	dZ@	p�@	~�@	�P@	�H@	��@	��@	@	��@	�/@	��@	�,@
%@
*@
""@
/@
>@
K@
Wb@
g@
v@
�@
�\@
�@
�@
�@
�@
��@
�H@
�L@
�E@�@�@'�@3�@@,@O0@^5@j@ww@�|@��@��@��@�@�@�h@�@�@ �@V@�@*S@7L@C�@R�@_�@k�@{�@�7@��@��@�~@�w@�|@�t@�m@� @@�@�@+�@:�@G�@T�@c�@��@33@|�@��@V@Wb@��@�;@"�@hs@��@��@5?@{�@��@�@O�@�0@��@&�@oF@�R@  @E�@�P@Ӡ@�@a�@��@�@8�@~K@Ĝ@
�@Q=@��@�@(G@qS@��@@Lu@�$@�/@$.@k�@��@�@7L@�@��@J@SI@��@��@&;@l�@�-@�q@;d@~�@�>@�@K@�P@��@ �@ SI@ ��@ �\@!6@!Wb@!�H@!܀@"[@"^5@"�@"��@##�@#e�@#��@#��@$+�@$o�@$�-@$�@%7�@%}�@%�2@&�@&K�@&��@&�\@'�@'_�@'��@'�@(,`@(oF@(�~@(�q@):�@)~K@)@*@*G�@*��@*�*@+@+Q�@+�#@+խ@,�@,V�@,�<@,�h@-�@-Wb@-�0@-�O@.o@.Q=@.�\@.��@/�@/E�@/�@/�@/�q@01'@0k.@0��@0��@1�@1Wb@1��@1�*@2
=@2I@2�+@2Ĝ@3�@3A�@3�@3��@4 �@4B�@4��@4Ĝ@51@5Lu@5��@5�@6@6T�@6��@6�t@7[@7bN@7��@7��@8/�@8t�@8��@8��@9>�@9�d@9�J@:1@:K�@:�P@:�*@;�@;Q=@<*@<��@=�@=��@>�@>��@?/�@?�z@@I@@�F@AWb@A@B[z@B�@CV@C�@@D�@D�(@E�@F�@F�@G""@G��@H)�@H��@IX�@I�k@JR�@J�(@KQ=@K�@L|�@M�@MoF@M��@N�C@O6@O�m@P'�@Q�u@R�C@T:�@Ur�@V�H@X-@Y~�@Z��@\A�@]n�@^�c@`�@a�|@b�;@d+�@ep�@f��@h[@ii�@jލ@l"�@m��@n��@p�@qqS@r��@t)�@u��@v��@x/�@yul@z��@|{@}ff@}��@}�}@~K@~�@~��@V@ ^@ G�O�G�O�@ ^G�O�G�O�G�O�@ ^G�O�@ ^@ G�O�G�O�@ ^@ G�O�@ G�O�@ �G�O�G�O�G�O�@ j@ �@ %@ �@ �@ 1@ �@ 
�@ J@ �@ @ b@ @ o@ {@ 6@ �@ �@ �@ �@  @ "�@ $�@ '�@ )�@ -@ /�@ 2�@ 5?@ 8�@ ;d@ >�@ B8@ E�@ I@ K�@ O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A��A�bA�oA�VA�bA�VA�JA�JA�bA�bA�oA�{A�{A�bA�VA�1A�A�  A�  A�A��mA�ĜA�M�A�ffA��A���A�9XA��A²-A�
=A��A�^5A� �A�-A�K�A�$�A���A���A��HA�K�A�jA��hA�;dA��DA��A�jA��A�VA�|�A��yA���A��A��A�t�A���A�5?A��jA�&�A��A�$�A��A�+A�~�A��A���A�ZA� �A�A��;A���A��RA���A���A�jA�ĜA��A�A�ZA��A�1A�A�n�A��A���A�\)A�?}A�bNA�ƨA� �A��A�-A���A�`BA��A�A�M�A�JA��DA��A��yA��DA�VA���A�M�A�|�A�x�A��A�7LA���A��-A�"�A��^A� �A��mA�ZA�r�A�VA���A��A��A���A��A�7A~�DA}��A}�A{S�AxM�At�9ArM�Ao�7AjE�Ai|�Ah��Ag�Ae��AeoAc�PA`v�A^�DA^ZA]�A[��A[dZAX�jAW"�AU�AS��AQt�AO�
AN��AM��AL�AK/AHffAF��AD��AC��ACK�AB�RABbA@�+A>�A>-A<�A:ffA9�mA9�hA8�A7l�A6Q�A5A5�A4�yA5;dA4z�A2jA1��A1G�A0�DA.��A.ZA,��A+��A)�A(��A(��A'K�A&bA%��A%G�A$��A$�A#C�A!K�A �+A Q�A (�A   Ap�A��A�9AQ�A�A�PA��A�yA�DA^5A�wA
=A�!AdZA$�A�jAx�A�-AbNA�AE�A�A��A+A��AO�A
�RA
�A	%A��A�AoA�A�A{A+A�A��AK�A ff@���@��
@��#@�z�@��@�z�@�!@���@��T@�l�@�7@�1'@�!@�Ĝ@�b@�ȴ@�X@��;@�(�@��/@�\)@�dZ@�I�@���@�=q@�{@�ff@��@��u@�`B@���@�33@��@��@�V@�z�@��R@��!@�dZ@���@��`@��
@�33@���@�&�@��@�ff@�@��@�Ĝ@���@�Z@� �@���@��T@�7L@�z�@�S�@�ff@��#@�Ĝ@�b@�S�@�ȴ@��h@���@��@}p�@|�@|I�@z~�@wK�@v{@t��@s@p��@o\)@l��@l�@jM�@h��@fv�@e�-@dz�@b�\@_�w@]��@[��@ZM�@Y��@Xb@V�R@UO�@TZ@RJ@PĜ@OK�@M�@K�m@J�!@I��@G
=@E�@D��@C�@A�@Ahs@@��@?\)@>V@=�@;S�@9��@9%@8bN@7�;@5@4�@3o@2��@1%@/|�@.5?@,z�@+��@*M�@)x�@(�`@'|�@&�@&@$�@#t�@"=q@!X@ Q�@��@��@�-@��@1@��@��@��@7L@�@V@��@�D@�F@�H@��@�^@��@hs@��@l�@��@�y@��@�@Z@1@@	��@	7L@�9@�w@|�@�R@�T@@�@V@�@33@@��@-@��@X@ �`@ b?��;?��?��R?�O�?��m?�C�?�=q?��#?��9?��?�o?��?� �?�V?��H?��?�b?�E�?��?�\?��?��?��?�V?܋D?�I�?��?�7L?���?և+?Լj?��?�M�?�G�?У�?� �?�v�?��?�I�?˥�?�"�?�~�?�x�?���?�b?�l�?��y?�E�?��/?�o?��7?�%?��?�\)?�V?�V?���?�I�?��m?�C�?��H?�=q?�=q?�^5?���?��?���?���?�=q?�~�?�"�?��?��m?�j?��?�/?��?�v�?��R?�\)?�  ?��?���?��`?�Ĝ?�Ĝ?��`?�ĜA�"�A��A�$�A� �A�$�A�$�A��A��A��A��A�(�A�$�A�"�A�$�A�&�A�(�A��A� �A� �A�$�A��A�bA�bA�bA�oA�bA�VA�oA�bA�bA�bA�oA�{A�oA�{A�oA�oA�JA�JA�bA�VA�bA�oA�oA�VA�
=A�JA�JA�JA�
=A�JA�bA�bA�bA�VA�bA�oA�{A�oA�oG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                  A��A�bA�oA�VA�bA�VA�JA�JA�bA�bA�oA�{A�{A�bA�VA�1A�A�  A�  A�A��mA�ĜA�M�A�ffA��A���A�9XA��A²-A�
=A��A�^5A� �A�-A�K�A�$�A���A���A��HA�K�A�jA��hA�;dA��DA��A�jA��A�VA�|�A��yA���A��A��A�t�A���A�5?A��jA�&�A��A�$�A��A�+A�~�A��A���A�ZA� �A�A��;A���A��RA���A���A�jA�ĜA��A�A�ZA��A�1A�A�n�A��A���A�\)A�?}A�bNA�ƨA� �A��A�-A���A�`BA��A�A�M�A�JA��DA��A��yA��DA�VA���A�M�A�|�A�x�A��A�7LA���A��-A�"�A��^A� �A��mA�ZA�r�A�VA���A��A��A���A��A�7A~�DA}��A}�A{S�AxM�At�9ArM�Ao�7AjE�Ai|�Ah��Ag�Ae��AeoAc�PA`v�A^�DA^ZA]�A[��A[dZAX�jAW"�AU�AS��AQt�AO�
AN��AM��AL�AK/AHffAF��AD��AC��ACK�AB�RABbA@�+A>�A>-A<�A:ffA9�mA9�hA8�A7l�A6Q�A5A5�A4�yA5;dA4z�A2jA1��A1G�A0�DA.��A.ZA,��A+��A)�A(��A(��A'K�A&bA%��A%G�A$��A$�A#C�A!K�A �+A Q�A (�A   Ap�A��A�9AQ�A�A�PA��A�yA�DA^5A�wA
=A�!AdZA$�A�jAx�A�-AbNA�AE�A�A��A+A��AO�A
�RA
�A	%A��A�AoA�A�A{A+A�A��AK�A ff@���@��
@��#@�z�@��@�z�@�!@���@��T@�l�@�7@�1'@�!@�Ĝ@�b@�ȴ@�X@��;@�(�@��/@�\)@�dZ@�I�@���@�=q@�{@�ff@��@��u@�`B@���@�33@��@��@�V@�z�@��R@��!@�dZ@���@��`@��
@�33@���@�&�@��@�ff@�@��@�Ĝ@���@�Z@� �@���@��T@�7L@�z�@�S�@�ff@��#@�Ĝ@�b@�S�@�ȴ@��h@���@��@}p�@|�@|I�@z~�@wK�@v{@t��@s@p��@o\)@l��@l�@jM�@h��@fv�@e�-@dz�@b�\@_�w@]��@[��@ZM�@Y��@Xb@V�R@UO�@TZ@RJ@PĜ@OK�@M�@K�m@J�!@I��@G
=@E�@D��@C�@A�@Ahs@@��@?\)@>V@=�@;S�@9��@9%@8bN@7�;@5@4�@3o@2��@1%@/|�@.5?@,z�@+��@*M�@)x�@(�`@'|�@&�@&@$�@#t�@"=q@!X@ Q�@��@��@�-@��@1@��@��@��@7L@�@V@��@�D@�F@�H@��@�^@��@hs@��@l�@��@�y@��@�@Z@1@@	��@	7L@�9@�w@|�@�R@�T@@�@V@�@33@@��@-@��@X@ �`@ b?��;?��?��R?�O�?��m?�C�?�=q?��#?��9?��?�o?��?� �?�V?��H?��?�b?�E�?��?�\?��?��?��?�V?܋D?�I�?��?�7L?���?և+?Լj?��?�M�?�G�?У�?� �?�v�?��?�I�?˥�?�"�?�~�?�x�?���?�b?�l�?��y?�E�?��/?�o?��7?�%?��?�\)?�V?�V?���?�I�?��m?�C�?��H?�=q?�=q?�^5?���?��?���?���?�=q?�~�?�"�?��?��m?�j?��?�/?��?�v�?��R?�\)?�  ?��?���?��`?�Ĝ?�Ĝ?��`?�ĜA�"�A��A�$�A� �A�$�A�$�A��A��A��A��A�(�A�$�A�"�A�$�A�&�A�(�A��A� �A� �A�$�A��A�bA�bA�bA�oA�bA�VA�oA�bA�bA�bA�oA�{A�oA�{A�oA�oA�JA�JA�bA�VA�bA�oA�oA�VA�
=A�JA�JA�JA�
=A�JA�bA�bA�bA�VA�bA�oA�{A�oA�oG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
�}B
�}B
�}B
�}B
�}B
�}B
�wB
�}B
��B
�}B
�}B
�}B
�}B
�wB
�wB
�}B
�wB
�wB
�wB
�wB
��B
ƨB
�/B1BL�B��B�5B�mB�BhB!�B+B%�B,B8RBA�BJ�BJ�BF�BD�BB�BC�BA�BE�BE�BI�BG�BI�BN�BP�BYB\)BcTBgmBp�Bs�Bw�Bz�B{�B�B�%B�\B�{B��B��B��B��B��B��B��B��B��B��B��B��B��B�bB�DB�VB�Bx�Bk�BZBK�B33B �BhB	7B��B��B�B�mB�BÖB�wB�3B��B�oB�7Bz�BjBVBH�BA�B6FB,B�B  B
�B
�)B
�B
��B
ǮB
ÖB
�XB
��B
�\B
p�B
XB
T�B
R�B
O�B
F�B
@�B
<jB
8RB
%�B
\B	��B	�B	��B	�!B	��B	��B	�=B	� B	w�B	gmB	S�B	L�B	I�B	=qB	9XB	5?B	 �B	�B	bB	+B��B��B�B�B�`B�BB��B��BŢB��B�qB�RB�3B�B��B�B��B��B�FB�qB�qB�jB�FB�9B�^B��BȴBɺBĜB��B�}B�^B�XB�-B�B��B��B��B��B��B��B��B��B��B��B��B��B�{B�hB�bB�\B�PB�DB�7B�7B�1B�B~�B}�B|�Bz�Bw�Bv�Bs�Bo�BhsBcTB\)B[#BYB[#B^5B]/B\)B\)BYBYB[#BXB[#B[#BYBS�BN�BI�BB�B=qB:^B6FB33B.B,B'�B)�B(�B%�B&�B(�B$�B(�B'�B+B(�B)�B)�B(�B&�B)�B+B5?B9XB?}BJ�BL�BVB[#BbNBl�Bt�B� B�PB��B��B�B�wB��B�ZB��B	hB	�B	#�B	6FB	?}B	E�B	S�B	^5B	jB	�B	�=B	�bB	��B	�LB	�wB	ĜB	ȴB	��B	��B	�B	�#B	�HB	�ZB	�mB	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B
B
B
B
+B

=B
JB
\B
bB
oB
�B
�B
�B
�B
�B
�B
 �B
#�B
#�B
#�B
%�B
(�B
(�B
)�B
,B
.B
/B
2-B
2-B
49B
5?B
8RB
9XB
:^B
;dB
=qB
=qB
>wB
@�B
@�B
A�B
D�B
D�B
E�B
E�B
F�B
H�B
I�B
J�B
K�B
L�B
N�B
N�B
P�B
Q�B
R�B
S�B
T�B
VB
VB
W
B
XB
YB
[#B
\)B
]/B
]/B
^5B
_;B
`BB
aHB
aHB
cTB
cTB
dZB
ffB
gmB
gmB
hsB
iyB
jB
k�B
k�B
k�B
k�B
l�B
l�B
m�B
m�B
n�B
n�B
p�B
p�B
q�B
r�B
s�B
t�B
u�B
t�B
v�B
w�B
w�B
w�B
x�B
z�B
y�B
z�B
z�B
|�B
|�B
|�B
|�B
}�B
}�B
~�B
}�B
~�B
� B
�B
�B
�B
�B
�B
�%B
�1B
�+B
�=B
�DB
�PB
�VB
�VB
�\B
�oB
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
�B
�B
�B
�B
�B
�B
�'B
�'B
�-B
�3B
�9B
�9B
�?B
�FB
�?B
�LB
�LB
�LB
�RB
�RB
�XB
�RB
�XB
�XB
�^B
�^B
�XB
�RB
�XB
�^B
�^B
�XB
�^B
�^B
�XB
�^B
�^B
�^B
�dB
�}B
�}B
��B
��B
��B
�}B
�}B
�}B
��B
��B
�}B
�wB
��B
��B
��B
��B
�jB
�}B
�}B
��B
�wB
�}B
�}B
��B
�wB
��B
��B
�wB
�}B
�}B
�}B
�}B
�}B
�}B
�}B
�wB
�}B
�}B
�}B
�}B
�}B
�}B
�}B
�}B
�wB
�}B
�wB
�wB
�wB
�}B
�}B
��B
�}B
�wB
�}B
�}B
��B
�}B
�}B
�}G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                  B
�UB
�UB
�UB
�UB
�VB
�VB
�PB
�WB
�]B
�WB
�XB
�XB
�YB
�SB
�TB
�[B
�UB
�VB
�VB
�WB
�dB
ƉB
�BBL�B�pB�B�QB�BMB!�B*�B%�B+�B8:BAqBJ�BJ�BF�BD�BBzBC�BAuBE�BE�BI�BG�BI�BN�BP�BYB\BcEBg_Bp�Bs�Bw�Bz�B{�B�B�B�RB�rB�xB��B��B��B��B��B��B��B��B��B��B��B��B�aB�CB�UB�Bx�Bk�BZBK�B35B �BkB	;B��B��B�B�sB�#BÝB�~B�;B��B�wB�@Bz�Bj�BVBH�BA�B6QB,B�B B
�B
�7B
�B
��B
ǽB
æB
�hB
��B
�mB
p�B
X!B
UB
SB
O�B
F�B
@�B
<B
8gB
%�B
rB	��B	��B	��B	�8B	�B	��B	�UB	�B	w�B	g�B	TB	L�B	I�B	=�B	9tB	5\B	 �B	�B	�B	IB�B��B�B�B�B�cB�B��B��B��B��B�vB�WB�-B�B�(B�B�#B�mB��B��B��B�oB�cB��B��B��B��B��B��B��B��B��B�\B�=B�,B�B�B�B�(B�B�B��B��B��B��B��B��B��B��B��B��B�}B�pB�qB�kB�ZB5B~0B}*B{BxBwBs�Bo�Bh�Bc�B\iB[cBYXB[dB^wB]qB\lB\mBY[BY\B[hBXVB[iB[jBY^BT@BO!BJBB�B=�B:�B6�B3~B._B,TB(<B*IB)CB&1B'7B)EB%,B)FB(@B+SB)GB*NB*NB)IB'<B*PB+VB5�B9�B?�BK#BM2BVlB[�Bb�Bl�Bu/B�vB��B�'B�[B��B��B�B��B�cB	�B	FB	$nB	6�B	@B	FBB	T�B	^�B	k)B	��B	��B	�B	��B	�B	�4B	�\B	�wB	ΙB	��B	��B	��B	�B	�0B	�FB	�[B	�jB	�zB	�B	��B	��B	��B	��B	��B	��B
B
B
 B
/B
DB
TB
iB
rB
�B
�B
�B
�B
�B
�B
�B
!�B
%B
%B
%B
'B
*+B
*.B
+7B
-FB
/UB
0_B
3sB
3vB
5�B
6�B
9�B
:�B
;�B
<�B
>�B
>�B
?�B
A�B
A�B
B�B
FB
FB
GB
GB
H$B
J3B
K<B
LFB
MOB
NXB
PgB
PjB
RyB
S�B
T�B
U�B
V�B
W�B
W�B
X�B
Y�B
Z�B
\�B
]�B
^�B
^�B
_�B
`�B
bB
cB
cB
eB
e B
f(B
h7B
iAB
iCB
jLB
kTB
l]B
meB
mhB
mkB
mmB
nvB
nyB
o�B
o�B
p�B
p�B
r�B
r�B
s�B
t�B
u�B
v�B
w�B
v�B
x�B
y�B
y�B
y�B
z�B
}B
{�B
}B
}B
B
B
"B
%B
�.B
�1B
�:B
�7B
�?B
�HB
�QB
�TB
�]B
�`B
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
�B
�B
�B
�,B
�0B
�=B
�DB
�NB
�[B
�iB
�tB
��B
��B
��B
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
�B
�B
�B
�%B
�2B
�NB
�bB
�xB
��B
��B
��B
��B
��B
��B
�B
�B
�1B
�BB
�WB
�mB
�tB
��B
��B
��B
��B
��B
��B
��B
�B
�B
�-B
�=B
�FB
�OB
�eB
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
�UB
�UB
�[B
�[B
�[B
�UB
�UB
�UB
�[B
�[B
�UB
�OB
�[B
�[B
�[B
�[B
�BB
�UB
�UB
�[B
�OB
�UB
�UB
�[B
�OB
�[B
�[B
�OB
�UB
�UB
�UB
�UB
�UB
�UB
�UB
�OB
�UB
�UB
�VB
�VB
�VB
�VB
�VB
�VB
�PB
�VB
�PB
�PB
�QB
�WB
�WB
�]B
�WB
�QB
�WB
�WB
�^B
�XB
�XB
�XG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�201807242202482021061413552120210614135521202106171312022021061713120220210617131202201807242202482021061413552120210614135521202106171312022021061713120220210617131202PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            new conductivity = original conductivity * (1 + delta*TEMP + CPcor_SBE*PRES) / (1 + delta*TEMP_ADJUSTED + CPcor_new*PRES_ADJUSTED)                                                                                                                              none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            new conductivity = original conductivity * (1 + delta*TEMP + CPcor_SBE*PRES) / (1 + delta*TEMP_ADJUSTED + CPcor_new*PRES_ADJUSTED)                                                                                                                              none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.50 dbar                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            CPcor_new = -1.25e-07; CPcor_SBE = -9.57e-08; delta = 3.25e-06                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.50 dbar                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            CPcor_new = -1.25e-07; CPcor_SBE = -9.57e-08; delta = 3.25e-06                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                      none                                                                                                                                                                                                                                                            New conductivity computed by using a different CPcor value from that provided by Sea-Bird.                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            No significant salinity drift detected.                                                                                                                                                                                                                         Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                      none                                                                                                                                                                                                                                                            New conductivity computed by using a different CPcor value from that provided by Sea-Bird.                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            No significant salinity drift detected.                                                                                                                                                                                                                         PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            AO  AO  ARCAARCAADJPADJP                                                                                                                                        2018072422024820180724220248  IP  IP                                G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2018072422024820180724220248QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�5F03E           703E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2018072422024820180724220248QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�0               0               PM  PM  ARSQARSQCOWGCOWGV1.1V1.1                                                                                                                                2021061713145520210617131455IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                