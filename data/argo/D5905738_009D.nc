CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS     N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-07-24T22:02:31Z creation      
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
_FillValue                 0  L�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  P�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 0  a�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  e�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  v�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 0  �@   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �p   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 0  �0   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �`   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �    PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 0  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 0  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �    HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                   (   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                   0   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                   8   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                   @   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  � H   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                   �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                   �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar           HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar           HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�          HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    $   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �h   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �h   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                   h   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  � h      � hArgo profile    3.1 1.2 19500101000000  20180724220231  20210722160149  5905738 5905738 US ARGO PROJECT                                                 US ARGO PROJECT                                                 GREGORY C. JOHNSON                                              GREGORY C. JOHNSON                                              PRES            TEMP            PSAL            PRES            TEMP            PSAL               	   	DD  AOAO7218                            7218                            2C  2C  DD  SOLO_D_MRV                      SOLO_D_MRV                      12001                           12001                           SBE603 01Dec16                  SBE603 01Dec16                  874 874 @�e�05�@�e�05�11  @�e��f`@�e��f`@6ڀ�,'�@6ڀ�,'��c�<��
(�c�<��
(11  GPS     GPS     Primary sampling: averaged [1Hz sampling by SBE-41CP averaged in 2-dbar bins]                                                                                                                                                                                   Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 AB  AA  AA  >���?�  @   @@  @�  @�33@�  @���A   A  A#33A@  Aa��A���A�  A�33A���A���A�  A�33A�33B   BffB��B  B   B(ffB0ffB8  B@  BHffBP  BX  B`  BhffBp��BxffB�33B���B�33B�  B�  B�33B�  B�ffB�ffB�33B�  B�  B�33B�ffB�  B���B���B���BǙ�B�33B�33B�33B�33B�  B�  B���B���B���B���B���B���B���B���C�fC  C�fC�fC	�fC33C�C�CL�C33C�C  C�fC33C33C �C"�C#�fC&33C(�C*  C,L�C.�C0  C233C4�C6�C8�C:�C<�C>  C@  CA��CD33CF33CH33CJ33CL�CN�CP  CR  CS�fCU�fCW��CZ�C\  C]�fC`�Cb  Cc�fCf33Ch�Ci�fClL�CnL�Cp33Cr�Ct  Cu�fCw��Cz�C|  C}�fC�  C��3C��C��C��C�  C�  C�  C��fC��C��C��C��C�  C�  C��3C��3C��3C��C��C��C��C��3C��C��C��C�  C��3C��C�&fC�&fC�  C��C��C��3C��C�&fC��C��3C�  C��C�&fC��C��fC��3C��C��C�&fC��C��fC�  C��C�  C��fC�  C��C�&fC��C��3C��C�&fC��C�  C��C��3C��fC��3C�  C��C�  C�ٚC��fC�  C��C�  C��fC�  C��C�  C��3C�  C�&fC��C�  C��C�  C��fC��C�&fC��C��3C��C�  C��3C�  C��3C��fC�  C��C��C��3C��C�&fC��C�  C��C�  C��fC��3C��C�&fC�33C��C��3C�  C��C�  C��fC��3C��C��C��C�  C�&fC�&fC�&fC�&fC�ٚC��fD s3D �3Ds3D��D  D	�fD  D�fDy�D�D��Dl�D3D��D!` D$  D&��D)Y�D+�fD.y�D1�D3�fD6  D8��D;9�D=�fD@Y�DB�3DE�fDHFfDK  DM��DPffDS�DU�fDX�3D[L�D^fD`� Dcs3Df&fDh��Dkl�DnfDp�3Ds  Du�fDx&fDz��D|��D,�D�� D�	�D�FfD�y�D��3D��D�#3D�VfD���D��3D���D�6fD�s3D�� D���D�0 D�l�D��fD��fD�&fD�ffD���D�� D��D�VfD���D���D�	�D�FfD�� D��fD��fD�3D�FfD�|�D��3D��D�  D�P D�s3D���D��fD���D���D� D�,�D�C3D�S3D�ffD�y�D��3D��3D�� D�� D�ɚD�ٚD��fD���D�fD�3D�#3D�9�D�FfD�` D�vfD̐ DͦfD�ɚD��3D�fD��D�<�D�\�D�s3D֖fD��fD�� D�fD�@ D�i�Dݓ3D�� D��fD��D�L�D�y�D� D��fD�� D��D�L�D�s3D뙚D�fD�� D�3D�&fD�P D�y�D��D���D�� D���D�	�D�,�D�C3D�FfD�\�D�i�D�|�D��fE S3E �fEffE� Ex E�E� E E� E3E� E$�E��E9�E	A�E
� E��EH EFfE� E��E8 E.fE�fE��E�fEL�E� E��E E~fEi�E�3E!)�E"��E#t�E$��E& E'nfE(��E)��E*�3E,@ E-�3E.� E00 E1|�E2^fE3�3E5fE6` E7��E8��E9�3E;NfE<�fE?��EB��EF�EIfEL[3EOT�ERD�EU��EX��E[�fE^�fEa��EefEh0 Ek9�En�fEq~fEt��Ew� E{�E~fE���E�+3E�� E�VfE�� E�nfE�� E�y�E�3E��3E�@ E��fE�ɚE�0 E�u�E��fE�$ E�h E��3E�fE�P E��fE���E�5�E���E�՚E�4 E���E��fE�$ E�zfE�� E�fE�k3E�� >���>���?   >���?   >���>���>���>���?   ?   ?333?333?333?L��?fff?�  ?���?���?�33?�  ?ٙ�?�33@ff@33@,��@9��@S33@l��@y��@���@�ff@�33@�  @���@�ff@�33@���@陚@�33A   AffA��A��A��A   A$��A,��A1��A9��A@  AFffAL��AS33A[33A`  Ah  AnffAs33A{33G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111414144441414411114111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            ?fff?�  @   @`  @�  @�33@�  @���A  A  A+33AH  Ai��A���A�  A�33A���A���A�  A�33A�33B  B
ffB��B  B"  B*ffB2ffB:  BB  BJffBR  BZ  Bb  BjffBr��BzffB�33B���B�33B�  B�  B�33B�  B�ffB�ffB�33B�  B�  B�33B�ffB�  B���B���B���Bș�B�33B�33B�33B�33B�  B�  B���B���B���B���B���B���B���C ffCffC� CffCffC
ffC�3C��C��C��C�3C��C� CffC�3C�3C ��C"��C$ffC&�3C(��C*� C,��C.��C0� C2�3C4��C6��C8��C:��C<��C>� C@� CBL�CD�3CF�3CH�3CJ�3CL��CN��CP� CR� CTffCVffCXL�CZ��C\� C^ffC`��Cb� CdffCf�3Ch��CjffCl��Cn��Cp�3Cr��Ct� CvffCxL�Cz��C|� C~ffC�@ C�33C�Y�C�Y�C�L�C�@ C�@ C�@ C�&fC�L�C�L�C�L�C�L�C�@ C�@ C�33C�33C�33C�Y�C�Y�C�L�C�L�C�33C�Y�C�L�C�L�C�@ C�33C�Y�C�ffC�ffC�@ C�Y�C�L�C�33C�L�C�ffC�L�C�33C�@ C�L�C�ffC�L�C�&fC�33C�L�C�L�C�ffC�L�C�&fC�@ C�Y�C�@ C�&fC�@ C�L�C�ffC�L�C�33C�L�C�ffC�Y�C�@ C�Y�C�33C�&fC�33C�@ C�L�C�@ C��C�&fC�@ C�L�C�@ C�&fC�@ C�Y�C�@ C�33C�@ C�ffC�Y�C�@ C�L�C�@ C�&fC�L�C�ffC�L�C�33C�L�C�@ C�33C�@ C�33C�&fC�@ C�Y�C�L�C�33C�L�C�ffC�Y�C�@ C�L�C�@ C�&fC�33C�L�C�ffC�s3C�Y�C�33C�@ C�Y�C�@ C�&fC�33C�L�C�L�C�Y�C�@ C�ffC�ffC�ffC�ffC��D 3D �3D3D�3D�D  D	�fD@ D�fD��D9�DٚD��D33DٚD!� D$  D&��D)y�D,fD.��D1,�D3�fD6@ D8��D;Y�D=�fD@y�DC3DE�fDHffDK  DMٚDP�fDS9�DU�fDX�3D[l�D^&fD`� Dc�3DfFfDh��Dk��Dn&fDp�3Ds@ Du�fDxFfDz��D|ٚDL�D�� D��D�VfD���D��3D���D�33D�ffD���D��3D��D�FfD��3D�� D���D�@ D�|�D��fD��fD�6fD�vfD���D�� D�,�D�ffD���D���D��D�VfD�� D��fD��fD�#3D�VfD���D��3D���D�0 D�` D��3D���D��fD���D�	�D�  D�<�D�S3D�c3D�vfD���D��3D��3D�� D�� D�ٚD��D��fD�	�D�fD�#3D�33D�I�D�VfD�p DˆfD̠ DͶfD�ٚD��3D�fD�,�D�L�D�l�DՃ3D֦fD��fD�  D�&fD�P D�y�Dݣ3D�� D�fD�,�D�\�D㉚D� D��fD�  D�,�D�\�D�3D멚D��fD�� D�3D�6fD�` D�D��D�ɚD�� D���D��D�<�D�S3D�VfD�l�D�y�D���D��fE [3E �fEnfE� E� E	�E� E E� E#3E� E,�E��EA�E	I�E
� E��EP ENfE� E��E@ E6fE�fE��E�fET�E� E��E  E�fEq�E�3E!1�E"��E#|�E$��E&  E'vfE(ɚE)��E*�3E,H E-�3E.� E08 E1��E2ffE3�3E5fE6h E7��E8��E:3E;VfE<�fE?��EB��EF!�EIfELc3EO\�ERL�EU��EX��E[�fE^�fEb�EefEh8 EkA�En�fEq�fEt��Ew� E{	�E~fE���E�/3E�� E�ZfE�� E�rfE�� E�}�E�#3E��3E�D E��fE�͚E�4 E�y�E��fE�( E�l E��3E�fE�T E��fE���E�9�E���E�ٚE�8 E���E��fE�( E�~fE�� E�fE�o3E�� G�O�?L��G�O�?fffG�O�G�O�G�O�G�O�?L��G�O�?�  G�O�G�O�?���?�ff?�33?�  G�O�?ٙ�?�33@   @��@��@&ff@333@L��@Y��@s33@�ff@���@���@�ff@�33@�  @ə�@�ff@�33@���@���A��A  AffA��A��A!��A(  A,��A4��A9��AA��AH  ANffAT��A[33Ac33Ah  Ap  AvffA{33A���G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111414144441414411114111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            @ �@ �@ �@ {@ O@ "�@ (�@ /@ 6�@ =q@ E�@ Q�@ `B@ m�@ z�@ ��@ ��@ ��@ �~@ �w@ �@ �t@ ��@ �@j@@g@-@:@G�@V@b�@p�@~K@��@��@��@��@��@��@��@�@��@�@�@#�@0x@=q@K@Yn@g�@t@�@��@�U@�M@�@ƨ@�O@��@��@��@	�@6@$�@2�@@,@M�@[z@i!@v�@�@�@��@�f@�@�@׹@�@�@ �@�@�@*S@7�@D�@R�@^�@n�@{�@��@�<@��@�~@��@�|@�#@��@�q@@@�@+@;d@I@V�@dZ@qS@~�@��@��@��@�9@�2@��@��@��@��@�@�@#�@0x@<�@M$@Z�@g�@t�@��@��@��@�Y@�R@�J@Ӡ@��@�L@��@
�@�@%�@33@?}@O0@\�@j@x&@�@��@��@�f@�@��@�h@�`@�@�Q@@�@)�@6�@C�@SI@a�@oF@z�@��@��@�(@�-@�2@�|@��@�@�q@	v@	�@	[@	+�@	:�@	H]@	Wb@	c�@	oF@	~K@	�P@	��@	��@	��@	�>@	�C@	ލ@	��@	��@
�@
�@
""@
1'@
<�@
I�@
X@
ff@
t�@
��@
�P@
��@
��@
�@
��@
�C@
�H@
�L@
��@	�@�@'�@4�@@�@O0@\)@hs@x&@�+@�u@��@��@��@ȴ@�
@�@��@  @@�@(G@7L@FQ@SI@_�@m�@z�@�+@��@��@��@��@�*@��@�@� @j@�@
@-@:�@I@UU@e	@r�@�W@��@��@��@��@�2@��@�/@ff@�Y@�Y@:�@�p@�@�@]�@��@�@@6�@~K@�W@b@V@�U@�@)�@m:@��@�~@>@�p@�o@�@\�@�A@�@:�@�p@�|@�@dZ@��@�,@B�@��@��@�@c�@�M@��@3�@x&@�@�@7�@z�@��@]@B�@��@�@
�@Lu@��@��@�@V�@�H@��@!s@ff@��@��@ 1'@ uk@ ��@ ��@!@�@!�p@!�W@"	�@"M�@"�i@"��@#�@#Z@#��@#�#@$�@$^�@$�@$�T@%%�@%ff@%��@%�T@& @&^�@&��@&�
@'�@'O0@'�7@'��@'��@(:�@(t�@(�@(�@) �@)Z�@)�#@)��@*1@*A�@*{�@*��@*�L@+,`@+g�@+��@+�;@,[@,Yn@,��@,��@-b@-M�@-�7@-�W@.1@.G�@.�|@.��@/v@/D�@/�@/�W@0%@0F�@0�+@0��@1�@1DD@1�p@1�J@2@2B�@2�@2�&@2�E@3;d@3z�@3�^@3�~@45@@4p�@4�f@4�(@5(G@5c�@5��@5�\@6�@6Ji@6�|@6��@6��@75�@7p�@7��@7�`@8�@8X�@8��@8��@9j@9<@9t�@:g@:�@;6�@;��@<G�@<�9@=X�@=Ĝ@>b�@>�@?e�@?�9@@bN@@�9@A�0@A��@B��@C2�@C��@D-�@D�>@EZ�@E��@FM$@F��@Go�@H �@H^5@H��@I~K@J�@J�@K,`@K�^@L�@L�@M<�@M�7@Ne	@N�@OZ@O��@Pz2@Q�@S@T��@U�J@W1'@Xv@Y��@[+@\r�@]�w@_�@`j@a�w@c@d\�@eψ@g	�@hj@i��@k�@le�@m��@o�@pm:@q�@s�@tk�@u��@w�@xl�@y�J@{�@{S�@{�P@{�@| @|Z�@|��@|��@}(G@}|�@}��@~�@~A�@~x�@~��@^@Q�@�a@�C@�\@�49@�L�@�s_@���@���G�O�@ G�O�@ �G�O�G�O�G�O�G�O�@ G�O�@ jG�O�G�O�@ �@ v@ %@ �G�O�@ 1@ 	�@ 
=@ �@ �@ V@ �@ o@ �@ �@ B@ �@ [@  @ "�@ %�@ '�@ *S@ -@ /@ 1�@ 3�@ 6�@ 9X@ <@ >@ A�@ DD@ FQ@ I�@ K�@ O0@ Q�@ T�@ Wb@ Z@ ]�@ _�@ b�@ e�@ g�@ k.G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A��A��`A��HA��HA��;A��/A��TA��A��A��A��A��yA��yA��A��yA��yA��A��yA��`A��TA��HA��HA��HA�S�A�t�A�E�A��A��A��A���A��A��A���A�ffA��A��A�A�ĜA��A��mA�r�A���A�x�A�9XA���A�Q�A�A���A���A�v�A�"�A���A�ZA��HA���A�l�A�;dA��A��A�bA���A�JA�v�A��A�  A��A��A��TA���A�`BA���A�hsA�-A�%A���A��A���A�JA�;dA��`A�E�A�$�A�1'A��A�=qA��TA���A��PA�$�A���A��A��A�Q�A��HA���A��A�?}A��A�\)A��A�9XA��A���A��wA�Q�A�I�A�E�A�O�A�O�A��A���A�  A�33A���A�%A��A�A}�TA}K�A|ZAz��Ay��Ay�Au�Ap�yAo/Am�AlZAkp�Aj�`AjA�Ah�AeS�AdVAc�Ab�DAaƨAahsAal�A`r�A^-A]VA\1AZ�jAY�AWhsAV9XAT��AR�AP��AO��AN�uAM��AL�AK
=AI��AHjAG�
AG�AF=qADACAA�TA@�\A@{A>��A<��A;�A;�A:�HA:��A:{A9dZA8��A8n�A7"�A4bA3
=A21'A1VA0(�A.jA-��A-33A,��A,�A,�!A+�A+�7A*�9A)�A(ZA'\)A%�A%`BA$�RA$1'A#�;A#dZA"�DA!%A��A�A�yA��A�AE�A�A
=AjA��AZA&�A=qAG�A$�AXAr�A&�A
1A	/A1A��AO�A�A��A{A�A�jA�DA$�AhsAĜA~�A��A ��A b@�?}@��@�E�@�-@�@���@�I�@��@��;@�@�
=@���@�\@�J@�V@띲@�R@�ff@�5?@�1@�h@�u@�I�@�A�@� �@��@ᙚ@���@��T@�(�@�b@���@�V@�@���@�;d@�&�@��7@��;@��+@�bN@���@��9@��@���@�5?@���@��R@��-@��j@���@�I�@�  @�r�@��@�1'@���@�5?@�b@�t�@�ff@�O�@�r�@��@�V@�%@�(�@�t�@�ȴ@�@���@�9X@��F@�"�@�
=@�E�@��-@��@~�@}p�@{dZ@yhs@wK�@t��@r��@o�w@n��@mp�@k�
@j�@gK�@e`B@c33@bn�@a%@`  @^��@\��@[��@Z^5@X��@W��@V�y@U�@U�@T��@So@QX@O|�@M��@L9X@K��@J��@I��@H�u@G��@F�y@E@Dz�@A�#@?�@>ff@<�@:n�@9��@7��@7+@5�@5p�@4��@3�
@2n�@1��@1G�@/�;@.�R@-�@+��@+C�@*=q@)�@(1'@'|�@'�@&�R@%�T@$�@$�@#"�@"M�@!X@!%@�w@l�@V@?}@�@��@X@bN@;d@ff@��@��@9X@��@"�@=q@�#@G�@��@�w@��@E�@��@p�@�@�@�F@@
~�@	G�@��@Q�@�w@ȴ@5?@��@p�@V@Z@��@33@�\@^5@�@&�@�@ Ĝ@  �?�O�?�1?��?�b?�`B?��?�hs??�h?�D?��#?�K�?��?�9X?���?�Ĝ?޸R?���?ۥ�?ٙ�?�r�?�K�?�ff?�?��?ӶF?���?��?�bN?�|�?�5??��?�ƨ?�C�?�"�?��?�x�?�Q�?Ǯ?�+?�$�?š�?��?�t�?��7?�  ?�;d?��-?��?�(�?��m?�?�~�?�=q?���?���?��#?��#?��#?��?�=q?�~�?���?�?��?���?�1?�j?��?�p�?��?�v�?��?���?�A�?�bN?��?���?���?�Ĝ?��`?��`?�%?�%?�%?�&�?�G�?�G�?�&�?�&�?��`?�Ĝ?�Ĝ?��`?��?��?���?��`?��`A��#A��HA��#A��/A��A��#A��
A���A���A���A��
A��TA��mA��mA��TA��`A��`A��`A��`A��`A��`A��TA��HA��HA��HA��TA��TA��;A��TA��HA��#A��#A��;A��;A��;A��mA��A��A��A��A��A��A��yA��A��A��A��A��yA��yA��yA��yA��A��yA��yA��yA��A��A��yA��A��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            A��A��`A��HA��HA��;A��/A��TA��A��A��A��A��yA��yA��A��yA��yA��A��yA��`A��TA��HA��HA��HA�S�A�t�A�E�A��A��A��A���A��A��A���A�ffA��A��A�A�ĜA��A��mA�r�A���A�x�A�9XA���A�Q�A�A���A���A�v�A�"�A���A�ZA��HA���A�l�A�;dA��A��A�bA���A�JA�v�A��A�  A��A��A��TA���A�`BA���A�hsA�-A�%A���A��A���A�JA�;dA��`A�E�A�$�A�1'A��A�=qA��TA���A��PA�$�A���A��A��A�Q�A��HA���A��A�?}A��A�\)A��A�9XA��A���A��wA�Q�A�I�A�E�A�O�A�O�A��A���A�  A�33A���A�%A��A�A}�TA}K�A|ZAz��Ay��Ay�Au�Ap�yAo/Am�AlZAkp�Aj�`AjA�Ah�AeS�AdVAc�Ab�DAaƨAahsAal�A`r�A^-A]VA\1AZ�jAY�AWhsAV9XAT��AR�AP��AO��AN�uAM��AL�AK
=AI��AHjAG�
AG�AF=qADACAA�TA@�\A@{A>��A<��A;�A;�A:�HA:��A:{A9dZA8��A8n�A7"�A4bA3
=A21'A1VA0(�A.jA-��A-33A,��A,�A,�!A+�A+�7A*�9A)�A(ZA'\)A%�A%`BA$�RA$1'A#�;A#dZA"�DA!%A��A�A�yA��A�AE�A�A
=AjA��AZA&�A=qAG�A$�AXAr�A&�A
1A	/A1A��AO�A�A��A{A�A�jA�DA$�AhsAĜA~�A��A ��A b@�?}@��@�E�@�-@�@���@�I�@��@��;@�@�
=@���@�\@�J@�V@띲@�R@�ff@�5?@�1@�h@�u@�I�@�A�@� �@��@ᙚ@���@��T@�(�@�b@���@�V@�@���@�;d@�&�@��7@��;@��+@�bN@���@��9@��@���@�5?@���@��R@��-@��j@���@�I�@�  @�r�@��@�1'@���@�5?@�b@�t�@�ff@�O�@�r�@��@�V@�%@�(�@�t�@�ȴ@�@���@�9X@��F@�"�@�
=@�E�@��-@��@~�@}p�@{dZ@yhs@wK�@t��@r��@o�w@n��@mp�@k�
@j�@gK�@e`B@c33@bn�@a%@`  @^��@\��@[��@Z^5@X��@W��@V�y@U�@U�@T��@So@QX@O|�@M��@L9X@K��@J��@I��@H�u@G��@F�y@E@Dz�@A�#@?�@>ff@<�@:n�@9��@7��@7+@5�@5p�@4��@3�
@2n�@1��@1G�@/�;@.�R@-�@+��@+C�@*=q@)�@(1'@'|�@'�@&�R@%�T@$�@$�@#"�@"M�@!X@!%@�w@l�@V@?}@�@��@X@bN@;d@ff@��@��@9X@��@"�@=q@�#@G�@��@�w@��@E�@��@p�@�@�@�F@@
~�@	G�@��@Q�@�w@ȴ@5?@��@p�@V@Z@��@33@�\@^5@�@&�@�@ Ĝ@  �?�O�?�1?��?�b?�`B?��?�hs??�h?�D?��#?�K�?��?�9X?���?�Ĝ?޸R?���?ۥ�?ٙ�?�r�?�K�?�ff?�?��?ӶF?���?��?�bN?�|�?�5??��?�ƨ?�C�?�"�?��?�x�?�Q�?Ǯ?�+?�$�?š�?��?�t�?��7?�  ?�;d?��-?��?�(�?��m?�?�~�?�=q?���?���?��#?��#?��#?��?�=q?�~�?���?�?��?���?�1?�j?��?�p�?��?�v�?��?���?�A�?�bN?��?���?���?�Ĝ?��`?��`?�%?�%?�%?�&�?�G�?�G�?�&�?�&�?��`?�Ĝ?�Ĝ?��`?��?��?���?��`?��`A��#A��HA��#A��/A��A��#A��
A���A���A���A��
A��TA��mA��mA��TA��`A��`A��`A��`A��`A��`A��TA��HA��HA��HA��TA��TA��;A��TA��HA��#A��#A��;A��;A��;A��mA��A��A��A��A��A��A��yA��A��A��A��A��yA��yA��yA��yA��A��yA��yA��yA��A��A��yA��A��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�BuBoBoBuBoBuBuBoBoBoBoBoBoBoBuBuBuBuB{B{B�B�B�Bl�B��B��B+B�B�B&�B-B7LB;dB8RB=qBF�BE�BM�B]/BdZBiyBm�BjBhsBffBhsBiyBjBn�Bz�B~�B{�B~�B~�B� B�B�B�7B�7B�=B�VB��B�{B�{B��B��B��B��B��B��B��B��B��B��B�uB�VB�B{�Bt�Bo�Bl�Bn�Bo�BjBhsBE�B0!B&�B�B%B��B�B�yB�B��B�!B��B��Bs�BaHBH�B1'B�B1BB  B
��B
�mB
��B
ŢB
�wB
�3B
��B
�B
q�B
]/B
G�B
:^B
2-B
$�B
{B

=B
B	�HB	��B	�wB	�XB	�!B	��B	��B	��B	�=B	s�B	m�B	e`B	_;B	YB	YB	[#B	ZB	W
B	P�B	K�B	J�B	D�B	B�B	=qB	8RB	/B	)�B	#�B	�B	�B	�B	{B	VB	bB	bB	VB	B��B�B�yB�NB�/B��B�}B�XB�LB�9B�9B�!B�B�B�B��B��B��B��B��B��B�hB�VB�\B�VB�VB�\B�VB�\B�JB�%B�7B�B~�B�B� B}�B|�B{�B{�Bv�Bn�B`BB^5B^5B[#B\)B]/B[#BN�BI�BG�BF�BE�BA�BB�B>wB=qB9XB8RB6FB6FB49B49B2-B2-B.B0!B0!B0!B0!B33B5?B6FB49B5?B33B2-B0!B0!B.B.B)�B%�B&�B'�B%�B%�B$�B#�B$�B#�B$�B%�B$�B$�B �B%�B$�B$�B$�B#�B$�B%�B$�B+B6FB=qBN�BQ�BZB^5BgmBm�B�%B�=B�{B��B�BB��B�#B�HB�B��B	1B	 �B	+B	D�B	jB	}�B	�B	�PB	��B	��B	��B	�'B	��B	ĜB	ɺB	��B	��B	�
B	�)B	�BB	�NB	�TB	�mB	�yB	�B	�B	�B	�B	�B	��B	��B	��B	��B
  B
B
%B
1B
JB
JB
VB
bB
uB
�B
�B
�B
�B
�B
�B
 �B
"�B
#�B
$�B
&�B
'�B
'�B
(�B
)�B
,B
-B
.B
1'B
33B
49B
33B
5?B
6FB
7LB
7LB
8RB
9XB
:^B
>wB
?}B
A�B
C�B
E�B
D�B
G�B
G�B
I�B
H�B
I�B
J�B
L�B
L�B
M�B
M�B
O�B
P�B
R�B
Q�B
S�B
T�B
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
\)B
]/B
]/B
^5B
^5B
`BB
aHB
aHB
cTB
dZB
e`B
e`B
ffB
gmB
gmB
hsB
iyB
iyB
jB
k�B
k�B
k�B
n�B
n�B
n�B
p�B
o�B
p�B
p�B
p�B
r�B
r�B
t�B
u�B
u�B
u�B
v�B
w�B
w�B
x�B
x�B
z�B
z�B
{�B
{�B
{�B
|�B
~�B
}�B
~�B
� B
� B
�B
�B
�B
�+B
�1B
�7B
�=B
�JB
�PB
�VB
�bB
�hB
�hB
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
�B
�B
�B
�B
�'B
�'B
�3B
�3B
�?B
�?B
�FB
�FB
�FB
�LB
�LB
�RB
�RB
�XB
�XB
�XB
�XB
�XB
�^B
�^B
�dB
�^B
�dB
�^B
�^B
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
�jB
�dB
�jB
�dB
�jB
�jB
�jB
�qB
�jB
�jB
�jB
�jB
�qB
�qB
�qBhBuBuBuBuBoBuB{BuBoBuBuBoBoBoBhBuBuBoBoBoBoBoBoBuBoBoB{BoBoBuB{BuBuB{BoBoBoBoBoBhBoBoBhBhBhBoBoBoBoBoBoBoBoBuBoBoBuBoBoG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            BoBhBhBoBhBoBoBhBhBhBhBhBhBhBoBoBoBoBuBuB{B{B�Bk�B��B��B%B�B�B%�B,B6FB:^B7LB<jBE�BD�BL�B\)BcTBhsBl�BiyBgmBe`BgmBhsBiyBm�By�B}�Bz�B}�B}�B~�B�B�B�1B�1B�7B�PB��B�uB�uB��B��B��B��B��B��B��B��B�{B��B�oB�PB�Bz�Bs�Bn�Bk�Bm�Bn�BiyBgmBD�B/B%�B�BB��B�B�sB��B�}B�B��B�{Br�B`BBG�B0!B�B+BB
��B
��B
�fB
��B
ĜB
�qB
�-B
��B
�B
p�B
\)B
F�B
9XB
1'B
#�B
uB
	7B
B	�BB	��B	�qB	�RB	�B	��B	��B	��B	�7B	r�B	l�B	dZB	^5B	XB	XB	ZB	YB	VB	O�B	J�B	I�B	C�B	A�B	<jB	7LB	.B	(�B	"�B	�B	�B	�B	uB	PB	\B	\B	PB	  B�B�B�sB�HB�)B��B�wB�RB�FB�3B�3B�B�B�B�B��B��B��B��B��B��B�bB�PB�VB�PB�PB�VB�PB�VB�DB�B�1B�B}�B� B~�B|�B{�Bz�Bz�Bu�Bm�B_;B]/B]/BZB[#B\)BZBM�BH�BF�BE�BD�B@�BA�B=qB<jB8RB7LB5?B5?B33B33B1'B1'B-B/B/B/B/B2-B49B5?B33B49B2-B1'B/B/B-B-B(�B$�B%�B&�B$�B$�B#�B"�B#�B"�B#�B$�B#�B#�B�B$�B#�B#�B#�B"�B#�B$�B#�B)�B5?B<jBM�BP�BYB]/BffBl�B�B�7B�uB��B�B��B��B�B�BB�B��B	+B	�B	)�B	C�B	iyB	|�B	�B	�PB	��B	��B	��B	�'B	��B	ĜB	ɺB	��B	��B	�
B	�)B	�BB	�NB	�TB	�mB	�yB	�B	�B	�B	�B	�B	��B	��B	��B	��B
  B
B
%B
1B
JB
JB
VB
bB
uB
�B
�B
�B
�B
�B
�B
 �B
"�B
#�B
$�B
&�B
'�B
'�B
(�B
)�B
,B
-B
.B
1'B
33B
49B
33B
5?B
6FB
7LB
7LB
8RB
9XB
:^B
>wB
?}B
A�B
C�B
E�B
D�B
G�B
G�B
I�B
H�B
I�B
J�B
L�B
L�B
M�B
M�B
O�B
P�B
R�B
Q�B
S�B
T�B
T�B
VB
W
B
W
B
XB
ZB
ZB
\)B
]/B
^5B
^5B
_;B
_;B
aHB
bNB
bNB
dZB
e`B
ffB
ffB
gmB
hsB
hsB
iyB
jB
jB
k�B
l�B
l�B
l�B
o�B
o�B
o�B
q�B
p�B
q�B
q�B
q�B
s�B
s�B
u�B
v�B
v�B
v�B
w�B
x�B
x�B
y�B
y�B
{�B
{�B
|�B
|�B
|�B
}�B
� B
~�B
� B
�B
�B
�B
�B
�%B
�1B
�7B
�=B
�DB
�PB
�VB
�\B
�hB
�oB
�oB
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
�B
�B
�B
�B
�B
�'B
�'B
�3B
�3B
�?B
�?B
�RB
�RB
�XB
�XB
�XB
�^B
�^B
�dB
�dB
�jB
�jB
�jB
�jB
�jB
�qB
�qB
�wB
�wB
�}B
�wB
�wB
�wB
�}B
�}B
�}B
�}B
�}B
�}B
�}B
�}B
�}B
�}B
�}B
��B
�}B
��B
�}B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��BbBoBoBoBoBhBoBuBoBhBoBoBhBhBhBbBoBoBhBhBhBhBhBhBoBhBhBuBhBhBoBuBoBoBuBhBhBhBhBhBbBhBhBbBbBbBhBhBhBhBhBhBhBhBoBhBhBoBhBhG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�201807242202312021061413520720210614135207202106141746182021061417461820210614174618201807242202312021061413520720210614135207202106141746182021061417461820210614174618PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            new conductivity = original conductivity * (1 + delta*TEMP + CPcor_SBE*PRES) / (1 + delta*TEMP_ADJUSTED + CPcor_new*PRES_ADJUSTED)                                                                                                                              none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS                                                                                                                                                                                                                                       PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            new conductivity = original conductivity * (1 + delta*TEMP + CPcor_SBE*PRES) / (1 + delta*TEMP_ADJUSTED + CPcor_new*PRES_ADJUSTED)                                                                                                                              none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS                                                                                                                                                                                                                                       surface_pressure=-0.50 dbar                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            CPcor_new = -1.25e-07; CPcor_SBE = -9.57e-08; delta = 3.25e-06                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            OW r = 1 (+/-0), vertically averaged dS = -0.001 (+/-0)                                                                                                                                                                                                         surface_pressure=-0.50 dbar                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            CPcor_new = -1.25e-07; CPcor_SBE = -9.57e-08; delta = 3.25e-06                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            OW r = 1 (+/-0), vertically averaged dS = -0.001 (+/-0)                                                                                                                                                                                                         Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                      none                                                                                                                                                                                                                                                            New conductivity computed by using a different CPcor value from that provided by Sea-Bird.                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Significant salinity sensor drift detected. OW least squares fit adopted.                                                                                                                                                                                       Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                      none                                                                                                                                                                                                                                                            New conductivity computed by using a different CPcor value from that provided by Sea-Bird.                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Significant salinity sensor drift detected. OW least squares fit adopted.                                                                                                                                                                                       PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            AO  AO  ARCAARCAADJPADJP                                                                                                                                        2018072422023120180724220231  IP  IP                                G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2018072422023120180724220231QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�5F03E           703E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2018072422023120180724220231QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�0               0               PM  PM  ARSQARSQCOWGCOWGV1.1V1.1                                                                                                                                2021072216014920210722160149IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                