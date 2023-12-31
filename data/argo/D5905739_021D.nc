CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS     N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-07-24T22:02:53Z creation      
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
_FillValue                  � lArgo profile    3.1 1.2 19500101000000  20180724220253  20210617131458  5905739 5905739 US ARGO PROJECT                                                 US ARGO PROJECT                                                 GREGORY C. JOHNSON                                              GREGORY C. JOHNSON                                              PRES            TEMP            PSAL            PRES            TEMP            PSAL                  DD  AOAO7219                            7219                            2C  2C  DD  SOLO_D_MRV                      SOLO_D_MRV                      12002                           12002                           SBE603 01Dec16                  SBE603 01Dec16                  874 874 @�pCrX��@�pCrX��11  @�pCl�@�pCl�@6�1;�.^@6�1;�.^�c������c�����11  GPS     GPS     Primary sampling: averaged [1Hz sampling by SBE-41CP averaged in 2-dbar bins]                                                                                                                                                                                   Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 AB  AA  AA  >���?�  ?�33@@  @�  @���@���@���A��A��A$��AA��Ac33A���A�ffA���A���A�33A�  A���A���B ��B  BffB��B ffB(  B0  B8ffB?��BH  BPffBX  B`  Bg��BpffBx��B�ffB�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�ffB�33B�  B�33B�  B���BǙ�B�  B�33B�33B�ffB�33B�33B�33B�ffB�33B�33B�33B�33B�  C   C  C�fC�fC33C
33C�C�C�C  C  C�fC��C  C  C��C   C"33C$�C%�fC(�C*  C+��C.�C0L�C2�C3�fC6  C8L�C:�C;�fC>�C@L�CB�CC�fCF�CH  CI��CL  CN  CO��CR  CT33CV�CX  CZ33C\�C^  C`33Cb  Cc��Cf  ChL�Cj�Cl  Cn33Cp33Cr  CtL�Cv�Cw�fCz33C|  C}��C��C�  C��3C��C�  C��3C��C��C�  C��C��C�  C�  C�  C�  C��3C��C��C�  C�&fC�&fC��C��C��C��C�  C�  C�  C�  C��3C��C��C��C��C�  C�  C��3C��3C��fC��C�  C��3C��C�&fC��C��C�  C��3C��C��C�  C�  C��fC��C��C��fC��C��C��3C��C�  C��3C��C�&fC��C�  C��C��C��3C��C��C��3C�  C��C��C��fC�  C��C��C�&fC�  C��3C��C��C�&fC��C��3C�  C��C��C�&fC�&fC��C��fC��3C��3C�  C��3C��3C��3C��3C��3C��3C��3C��3C�  C�  C��C��C�&fC��C��fC��3C�  C��C��C��C�&fC�  C��fC��fC��3C��fC���D� D&fD� D
�fD` D33D�3D�3Dl�D33D  D �3D#ffD&�D(��D+` D.fD0� D3@ D5�3D8��D;9�D=�3D@s3DC  DE��DH9�DJٚDM` DO�3DR� DU3DW��DZ@ D\��D_ffDa�3Dd�3Dg33Di�fDl��Do@ Dq�3Dt�fDwL�Dz  D|33D~�3D���D�  D�l�D���D� D�Y�D���D��D�6fD�vfD��fD�� D�,�D�c3D��3D�� D� D�C3D�s3D��3D���D�  D�<�D�l�D��fD�ٚD�	�D�<�D�p D���D���D��D�S3D�� D�� D�  D�,�D�c3D��3D��fD���D�0 D�` D���D��fD���D�0 D�c3D���D�ɚD���D�0 D�c3D���D�ɚD���D�)�D�\�Dǐ D�� D���D� D�<�D�` D΃3DϬ�D�� D���D��D�9�D�S3D�l�D�|�DؖfD٬�D���D��3D���D�fD�,�D�FfD�VfD�p D�3D�fD�3D�fD��3D��fD��D��D��fD�fD� D��D�#3D�&fD�)�D�33D�9�D�@ D�<�D�9�D�9�D�6fD�9�D�)�D�&fD�,�D�,�D�,�E  E ��E!�E��E1�E��E<�E� ES3E�3Ed�E�3EfE8 E	��E+3E` E�3E� E�3E$�ET�E��EA�El�E�3E��E��Ep E|�E�3EfE fE!�fE"��E$�E%	�E&x E'� E(��E*( E+�fE,nfE-��E/( E0�3E1i�E2��E4�E5x E6њE7�fE9fE:ffE;��E>��EAɚED��EH&fEK�EN1�EQA�ET�3EW��EZ� E^�Ea+3Ed<�EgL�Ejc3Em� Ep� Es�3Ev� Ey�3E}3E�'3E���E�K3E�� E�L E��fE�p�E� E�d�E���E�3E�@�E��fE���E�6fE��fE��E�fE�vfE�͚E�$�E�^fE�� E� E�[3E���E��3E�Q�E��fE��fE�@ E�~f>���>���>���>L��>���>���>���>���>���>���>���>���?   ?   ?   ?333?L��?fff?���?���?�33?���?�ff@   @33@,��@@  @Y��@l��@�ff@�33@���@���@�ff@�33@�33@���@陚@���A��A��A��A33A$��A,��A4��A<��AD��AL��AVffA`  Ah  Ap  AvffA�  A�  A�  A�  A�  A�  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111444144441441441111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                ?fff?�  @��@`  @�  @���@���@���A	��A��A,��AI��Ak33A���A�ffA���A���A�33A�  A���A���B��B
  BffB��B"ffB*  B2  B:ffBA��BJ  BRffBZ  Bb  Bi��BrffBz��B�ffB�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�ffB�33B�  B�33B�  B���Bș�B�  B�33B�33B�ffB�33B�33B�33B�ffB�33B�33B�33B�33B�  C � C� CffCffC�3C
�3C��C��C��C� C� CffCL�C� C� CL�C � C"�3C$��C&ffC(��C*� C,L�C.��C0��C2��C4ffC6� C8��C:��C<ffC>��C@��CB��CDffCF��CH� CJL�CL� CN� CPL�CR� CT�3CV��CX� CZ�3C\��C^� C`�3Cb� CdL�Cf� Ch��Cj��Cl� Cn�3Cp�3Cr� Ct��Cv��CxffCz�3C|� C~L�C�Y�C�@ C�33C�L�C�@ C�33C�Y�C�L�C�@ C�Y�C�L�C�@ C�@ C�@ C�@ C�33C�Y�C�L�C�@ C�ffC�ffC�Y�C�L�C�L�C�L�C�@ C�@ C�@ C�@ C�33C�Y�C�Y�C�L�C�L�C�@ C�@ C�33C�33C�&fC�L�C�@ C�33C�L�C�ffC�Y�C�L�C�@ C�33C�Y�C�Y�C�@ C�@ C�&fC�Y�C�L�C�&fC�L�C�L�C�33C�L�C�@ C�33C�L�C�ffC�Y�C�@ C�Y�C�L�C�33C�L�C�L�C�33C�@ C�Y�C�L�C�&fC�@ C�L�C�Y�C�ffC�@ C�33C�L�C�Y�C�ffC�L�C�33C�@ C�Y�C�Y�C�ffC�ffC�L�C�&fC�33C�33C�@ C�33C�33C�33C�33C�33C�33C�33C�33C�@ C�@ C�L�C�Y�C�ffC�L�C�&fC�33C�@ C�L�C�L�C�Y�C�ffC�@ C�&fC�&fC�33C�&fC��D� DFfD  D
�fD� DS3D3D�3D��DS3D  D �3D#�fD&9�D(ٚD+� D.&fD0� D3` D63D8��D;Y�D=�3D@�3DC@ DE��DHY�DJ��DM� DP3DR� DU33DW��DZ` D\��D_�fDb3Dd�3DgS3DjfDl��Do` Dr3Dt�fDwl�Dz  D|S3D3D���D�0 D�|�D���D�  D�i�D���D���D�FfD��fD��fD�  D�<�D�s3D��3D�� D�  D�S3D��3D��3D���D� D�L�D�|�D��fD��D��D�L�D�� D���D���D�)�D�c3D�� D�� D� D�<�D�s3D��3D��fD�	�D�@ D�p D���D��fD��D�@ D�s3D���D�ٚD��D�@ D�s3D���D�ٚD�	�D�9�D�l�DǠ D�� D���D�  D�L�D�p DΓ3Dϼ�D�� D���D�)�D�I�D�c3D�|�D׌�DئfDټ�D���D��3D��D�&fD�<�D�VfD�ffD� D�3D�fD�3D��fD��3D��fD���D���D�fD�fD�  D�)�D�33D�6fD�9�D�C3D�I�D�P D�L�D�I�D�I�D�FfD�I�D�9�D�6fD�<�D�<�D�<�E   E ��E)�E��E9�E��ED�E� E[3E�3El�E�3EfE@ E	��E33Eh E�3E� E�3E,�E\�E��EI�Et�E�3E��E��Ex E��E�3EfE &fE!�fE"��E$�E%�E&� E'� E(��E*0 E+�fE,vfE-��E/0 E0�3E1q�E2��E4$�E5� E6ٚE7�fE9fE:nfE;��E>ɚEAњED��EH.fEK�EN9�EQI�ET�3EW��EZ� E^	�Ea33EdD�EgT�Ejk3Em� Ep� Es�3Ev� Ez3E}3E�+3E���E�O3E�� E�P E��fE�t�E� E�h�E���E�3E�D�E��fE���E�:fE��fE��E�"fE�zfE�њE�(�E�bfE�� E� E�_3E���E�3E�U�E��fE��fE�D E��fG�O�G�O�G�O�?333G�O�G�O�G�O�G�O�?L��G�O�G�O�?fffG�O�G�O�?�  ?���?�ff?�33?���?ٙ�?�33@ff@33@   @333@L��@`  @y��@�ff@�ff@�33@���@���@�ff@�33@�33@���@���AffA��A��A��A#33A,��A4��A<��AD��AL��AT��A^ffAh  Ap  Ax  A~ffA�  A�  A�  A�  A�  A�  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111444144441441441111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                @ �@ �@ �@ {@ O@ !s@ (G@ /@ 7L@ >@ FQ@ R�@ `�@ n�@ |�@ ��@ ��@ �(@ �~@ ��@ �|@ ��@ �@ �q@�@�@�@,`@:�@F�@UU@c�@p�@~K@�D@�H@��@�F@@�7@��@�@��@�@{@""@/�@=q@K@Z@g@t@�d@�\@�U@�M@�R@ƨ@�O@�@�@�E@
�@B@&;@3�@A�@O0@\)@i�@ww@�p@�@��@�r@�k@�@׹@�@�Y@�Q@J@O@(�@5?@DD@SI@`B@l�@{�@��@��@��@��@��@�@�t@�(@�q@�@�@ �@-@9X@H]@UU@a�@p�@~K@��@��@��@��@@є@ލ@�@��@�@@""@1�@>@K@Z@g�@t@��@�@�U@�@�R@Ĝ@��@�H@�@@�E@
=@6@&�@3�@@�@O�@\�@i�@ww@�@��@��@�r@�k@�c@�@�@�@ �@V@�@(�@6�@DD@Q�@^�@n�@|?@�7@��@��@�~@�w@�@�@��@��@	�@	�@	 �@	-�@	:�@	G�@	T�@	dZ@	r@	~K@	��@	�<@	��@	��@	�2@	��@	ލ@	��@	��@
�@
�@
"�@
1�@
>�@
K@
Z@
g@
s_@
�d@
�@
�U@
��@
��@
ƨ@
�C@
�H@
�@
��@J@�@$�@3�@B8@P�@\�@i!@ww@�|@�$@�y@�!@�k@�@�[@�@�Y@�Q@�@�@(G@5�@C�@Q=@^�@m:@z�@�7@��@��@�-@��@�@�t@��@�q@�@@�@+@8�@F�@S�@`B@�@3�@~K@�@{@a�@��@��@B8@��@�t@$.@m�@��@�Q@G�@�@�
@�@hs@��@�~@?}@�+@�7@�@[z@�(@�@.l@t@�^@^@G�@�P@�O@�@a�@�M@�@<�@�@��@�@`�@��@�@1�@z3@@	�@Q=@��@��@$�@k�@��@� @;d@~K@��@@H]@��@��@ V@ O0@ �@ ψ@!@!T�@!��@!�h@"�@"Z�@"�U@"��@#
@#bN@#��@#��@$,`@$m:@$�~@$�@%3�@%t�@%�F@%��@&:@&z�@&�^@&��@'@,@'��@'�>@(v@(FQ@(��@(�c@)
�@)M$@)��@)��@*�@*Q=@*��@*Ӡ@+�@+Q�@+�@+�7@,V@,M�@,��@,ȴ@-�@-FQ@-�d@-�w@-�~@.4�@.o�@.�f@.��@/$�@/`�@/�U@/�h@0o@0N�@0�7@0��@0�E@17�@1qS@1�@1�@2[@2V�@2��@2�c@3@3:�@3r@3�M@3��@4�@4Q�@4��@4��@4�e@5*S@5a�@5��@5��@6�@69X@6o�@6�A@6��@7�@7R�@7��@7Ĝ@7��@8:@8uk@8�r@8�(@9&�@9�(@:�@:܀@;`�@;�@<g�@<�@=k.@=�@>oF@>��@?��@@.l@@�@A'�@A�@BQ�@BĜ@C7�@C��@DQ=@D�@E`B@F �@Fj@G�@G�m@Hv@H��@I/@I�@J'�@J��@KO�@K�-@LFQ@L�@Mm:@N �@NbN@N�@O��@P*@Qc�@R��@S��@Ue�@V�@W��@YH]@Z�@\ �@]_�@^�@`o@aa�@b�!@d]@eX�@f�A@ho@iUU@j��@k��@m[z@n��@p	�@qN�@r��@t@uH]@v�m@v�@w)�@wz2@w��@w��@x3�@x�W@x�o@y�@yFP@y�h@y��@z&;@zWa@z�m@z�@{/@{ul@{�@|^@|FP@|��@|��@}G�O�G�O�G�O�@ ^G�O�G�O�G�O�G�O�@ G�O�G�O�@ �G�O�G�O�@ j@ �@ v@ %@ �@ 1@ 	�@ 
�@ J@ �@ �@ o@ {@ 6@ B@ �@ g@  �@ $.@ &�@ )�@ -@ /@ 1�@ 5�@ 8�@ <@ ?}@ B8@ FQ@ I�@ M$@ P�@ S�@ Wb@ [z@ _�@ b�@ ff@ i!@ m:@ p�@ t@ ww@ z�@ ~KG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�"�A�"�A��A��A��A� �A�"�A�$�A�$�A�$�A�"�A��A��A� �A��A�VA�bA��A��Aϴ9A�oA�ƨA�7LA��/A˸RA�?}A�5?A��/A�ĜA�hsA��A���A���A�bNA��A�9XA�/A��7A�1'A�(�A���A�K�A�=qA���A�/A�jA�p�A��A��\A��A���A��+A�n�A��A�I�A�;dA�"�A���A��;A� �A��A���A���A�M�A�+A��FA���A�I�A�-A�oA���A��
A��!A�dZA��TA�G�A���A�?}A��A�%A�?}A��HA�jA��A�9XA���A�?}A�&�A���A���A��/A�x�A�  A�I�A���A� �A�dZA�x�A���A�  A��
A�K�A��A��A���A�C�A�n�A�1A�;dA�-A�z�A�33A�ȴA��A�A��7A���A�ĜA���A��\A�z�A}�AwVAt�HAt5?Aq��AodZAnJAm\)Ak�AhjAex�Aa/A_��A^ �AY��AUƨAS�AQdZAN�\AL1AKXAJAG�AG+AFA�AD9XABZAA�TA@��A@A@1A?��A>�uA=t�A<^5A;dZA:I�A9�hA9�A8�uA7hsA6A5��A5dZA5"�A4�!A4^5A3�A3/A2��A1�;A/�TA.�A.^5A-|�A,��A+��A*�A*-A)��A)p�A)7LA)oA(�/A(�uA'\)A$�A#;dA"ffA!�wA!&�A �!A  �AC�AI�A��A�jA��AC�A�/A=qA�FA�AXA�DA+A�DA�;A��A�
AQ�A?}A�uA��A�DA�A�jA�A��A	A	�A�uA��A=qAl�AĜA�^Av�AJA/A ��A {@�E�@��9@���@��y@�
=@���@���@��-@�7L@�r�@��;@��@�-@�%@�|�@��@��@�1@��@�hs@�|�@���@�v�@�ff@�dZ@ϝ�@�@ԓu@�t�@�-@�~�@���@�9X@���@�hs@���@��y@���@���@��@��@��@�x�@��@�v�@���@��@�ff@�O�@�7L@��y@�n�@��/@��+@�V@��\@��
@��H@�/@�dZ@�5?@�7L@��@�|�@�=q@�p�@�bN@�1@}V@x��@x��@vv�@up�@x��@s��@r��@tI�@q�#@o�;@o�;@n�@m@l�D@k��@j�@j�\@i&�@g�@g+@d�@cS�@ax�@`��@`  @^v�@\�@Z~�@X �@V��@U�T@S��@Q��@P�9@O�w@Nff@MO�@K�m@J��@J-@H �@G�@F�y@EO�@C��@B�@A�#@@�`@@  @?�@=?}@;C�@:M�@81'@7��@7\)@5@49X@3��@2��@1�#@0��@/�P@.V@-�@,1@+33@*��@)��@)7L@'�;@&��@&@%p�@$9X@"�H@!x�@ Q�@;d@��@�h@�@(�@��@^5@�#@&�@�;@K�@�@�y@@�@1@o@�#@hs@��@�;@�P@@��@�j@�F@
�@
�\@	�#@	X@��@A�@�@|�@��@�T@`B@�@1@�F@�@��@�@�7@ ��@ ��?�\)?���?�ƨ?��H?�b?�$�?��?�!?�&�?�  ?�v�?���?�C�?���?�P?��T?�Z?㕁?�-?��?ߝ�?���?�O�?ܬ?�ƨ?�=q?���?�ȴ?�z�?��?�S�?�-?�%?��`?θR?�{?�O�?̬?�1?�"�?���?�~�?��?�7L?���?Ǯ?�ff?���?��?�o?���?��?���?��h?��?��?�ƨ?��?�?�~�?�=q?�=q?�~�?���?�^5?���?��H?�"�?���?�1?��D?�/?��-?�v�?���?��R?��?���?��?�;d?�|�?���?�|�?���?��;?�  ?� �?� �?�bN?��?��?��`?��`?�&�?�&�?�Ĝ?�%?��`A��A��A�"�A� �A�"�A�"�A�"�A�"�A�$�A�"�A�$�A�$�A�$�A� �A�"�A�&�A�$�A�"�A� �A� �A� �A��A��A� �A� �A� �A��A��A��A��A��A� �A�"�A�"�A� �A�"�A�$�A�$�A�$�A�$�A�$�A�$�A�$�A�"�A��A��A��A��A��A��A��A��A��A� �A� �A� �A� �A� �A��A�oG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                A�"�A�"�A��A��A��A� �A�"�A�$�A�$�A�$�A�"�A��A��A� �A��A�VA�bA��A��Aϴ9A�oA�ƨA�7LA��/A˸RA�?}A�5?A��/A�ĜA�hsA��A���A���A�bNA��A�9XA�/A��7A�1'A�(�A���A�K�A�=qA���A�/A�jA�p�A��A��\A��A���A��+A�n�A��A�I�A�;dA�"�A���A��;A� �A��A���A���A�M�A�+A��FA���A�I�A�-A�oA���A��
A��!A�dZA��TA�G�A���A�?}A��A�%A�?}A��HA�jA��A�9XA���A�?}A�&�A���A���A��/A�x�A�  A�I�A���A� �A�dZA�x�A���A�  A��
A�K�A��A��A���A�C�A�n�A�1A�;dA�-A�z�A�33A�ȴA��A�A��7A���A�ĜA���A��\A�z�A}�AwVAt�HAt5?Aq��AodZAnJAm\)Ak�AhjAex�Aa/A_��A^ �AY��AUƨAS�AQdZAN�\AL1AKXAJAG�AG+AFA�AD9XABZAA�TA@��A@A@1A?��A>�uA=t�A<^5A;dZA:I�A9�hA9�A8�uA7hsA6A5��A5dZA5"�A4�!A4^5A3�A3/A2��A1�;A/�TA.�A.^5A-|�A,��A+��A*�A*-A)��A)p�A)7LA)oA(�/A(�uA'\)A$�A#;dA"ffA!�wA!&�A �!A  �AC�AI�A��A�jA��AC�A�/A=qA�FA�AXA�DA+A�DA�;A��A�
AQ�A?}A�uA��A�DA�A�jA�A��A	A	�A�uA��A=qAl�AĜA�^Av�AJA/A ��A {@�E�@��9@���@��y@�
=@���@���@��-@�7L@�r�@��;@��@�-@�%@�|�@��@��@�1@��@�hs@�|�@���@�v�@�ff@�dZ@ϝ�@�@ԓu@�t�@�-@�~�@���@�9X@���@�hs@���@��y@���@���@��@��@��@�x�@��@�v�@���@��@�ff@�O�@�7L@��y@�n�@��/@��+@�V@��\@��
@��H@�/@�dZ@�5?@�7L@��@�|�@�=q@�p�@�bN@�1@}V@x��@x��@vv�@up�@x��@s��@r��@tI�@q�#@o�;@o�;@n�@m@l�D@k��@j�@j�\@i&�@g�@g+@d�@cS�@ax�@`��@`  @^v�@\�@Z~�@X �@V��@U�T@S��@Q��@P�9@O�w@Nff@MO�@K�m@J��@J-@H �@G�@F�y@EO�@C��@B�@A�#@@�`@@  @?�@=?}@;C�@:M�@81'@7��@7\)@5@49X@3��@2��@1�#@0��@/�P@.V@-�@,1@+33@*��@)��@)7L@'�;@&��@&@%p�@$9X@"�H@!x�@ Q�@;d@��@�h@�@(�@��@^5@�#@&�@�;@K�@�@�y@@�@1@o@�#@hs@��@�;@�P@@��@�j@�F@
�@
�\@	�#@	X@��@A�@�@|�@��@�T@`B@�@1@�F@�@��@�@�7@ ��@ ��?�\)?���?�ƨ?��H?�b?�$�?��?�!?�&�?�  ?�v�?���?�C�?���?�P?��T?�Z?㕁?�-?��?ߝ�?���?�O�?ܬ?�ƨ?�=q?���?�ȴ?�z�?��?�S�?�-?�%?��`?θR?�{?�O�?̬?�1?�"�?���?�~�?��?�7L?���?Ǯ?�ff?���?��?�o?���?��?���?��h?��?��?�ƨ?��?�?�~�?�=q?�=q?�~�?���?�^5?���?��H?�"�?���?�1?��D?�/?��-?�v�?���?��R?��?���?��?�;d?�|�?���?�|�?���?��;?�  ?� �?� �?�bN?��?��?��`?��`?�&�?�&�?�Ĝ?�%?��`A��A��A�"�A� �A�"�A�"�A�"�A�"�A�$�A�"�A�$�A�$�A�$�A� �A�"�A�&�A�$�A�"�A� �A� �A� �A��A��A� �A� �A� �A��A��A��A��A��A� �A�"�A�"�A� �A�"�A�$�A�$�A�$�A�$�A�$�A�$�A�$�A�"�A��A��A��A��A��A��A��A��A��A� �A� �A� �A� �A� �A��A�oG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
�ZB
�ZB
�ZB
�ZB
�ZB
�ZB
�ZB
�ZB
�ZB
�ZB
�TB
�TB
�TB
�ZB
�ZB
�TB
�TB
�NB
�TB
�yB�'B)�B,B2-B33B+B�BoB �B6FB33B7LBI�BJ�BK�BO�BcTBiyB�B�{B��B�uBjBS�BR�BYB`BBe`Bm�Bt�Bt�Bw�Bz�Bq�Bu�B~�B�B�7B�\B��B��B��B�{B�hB�\B�1B�B�B~�B}�B{�Bz�Bx�Bv�Br�Bn�BhsBe`BbNBW
BT�BQ�BL�BH�B=qB'�B&�B$�B �BbB  B�B�fBȴB�jB�'B��B�\Bw�BaHBS�BK�B>wB7LB"�BJB
�B
�HB
�
B
�}B
�XB
�FB
�B
��B
�VB
�B
k�B
]/B
[#B
XB
9XB
\B	�BB	��B	�wB	��B	�7B	}�B	u�B	dZB	N�B	33B	�B	{B		7B�B�BȴB�}B�B��B��B��B�{B��B�{B�bB�uB�uB�\B�oB�bB�\B�\B�JB�PB�\B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�{B�\B�hB�hB�oB�\B�PB�VB�\B�VB�PB�PB�PB�JB�=B�B{�Bz�Bv�Bt�Bq�Bo�BiyBiyBgmBe`BcTBbNB_;B]/B]/B]/B_;B_;B\)B]/B[#BVBT�BR�BP�BL�BL�BI�BF�BE�BB�BA�B;dB<jB<jB9XB9XB6FB6FB5?B33B2-B1'B1'B6FB6FB2-B2-B-B.B0!B1'B2-B2-B1'B1'B1'B2-B2-B1'B33B5?B6FB9XB;dB:^B=qB=qB>wB>wBL�BYB�hB�qB��B�1B�7B�JB�oB��B��B��B�LB��B��B�B�)B�BB��B��B	
=B	B	{B	'�B	2-B	5?B	C�B	J�B	W
B	ZB	e`B	s�B	�B	�JB	�JB	�PB	�oB	��B	��B	�B	�-B	�?B	�^B	��B	ǮB	ɺB	��B	�B	�B	�mB	�ZB	�sB	��B	��B	��B
  B	��B
B
+B
	7B
PB
PB
hB
oB
oB
�B
�B
�B
�B
�B
 �B
 �B
!�B
#�B
$�B
'�B
,B
-B
.B
.B
0!B
0!B
2-B
33B
33B
5?B
5?B
6FB
8RB
9XB
:^B
;dB
<jB
<jB
=qB
?}B
@�B
A�B
C�B
C�B
C�B
D�B
F�B
F�B
G�B
H�B
H�B
J�B
K�B
L�B
N�B
O�B
Q�B
Q�B
Q�B
S�B
T�B
T�B
T�B
VB
W
B
YB
[#B
]/B
\)B
]/B
^5B
^5B
_;B
aHB
`BB
bNB
cTB
dZB
dZB
cTB
e`B
ffB
gmB
hsB
iyB
iyB
jB
jB
l�B
n�B
n�B
n�B
p�B
p�B
p�B
q�B
r�B
s�B
t�B
u�B
t�B
u�B
w�B
w�B
w�B
x�B
x�B
y�B
z�B
{�B
{�B
{�B
|�B
~�B
~�B
� B
� B
�B
�B
�B
�%B
�+B
�1B
�1B
�=B
�DB
�PB
�PB
�VB
�\B
�bB
�bB
�oB
�uB
�uB
�{B
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
�B
�B
�B
�!B
�'B
�-B
�-B
�9B
�9B
�?B
�FB
�LB
�FB
�LB
�FB
�RB
�RB
�XB
�RB
�XB
�XB
�XB
�XB
�XB
�XB
�XB
�XB
�XB
�XB
�XB
�XB
�XB
�RB
�XB
�XB
�XB
�XB
�XB
�XB
�RB
�XB
�^B
�XB
�^B
�XB
�XB
�^B
�XB
�^B
�ZB
�`B
�ZB
�ZB
�`B
�ZB
�ZB
�ZB
�ZB
�`B
�ZB
�`B
�ZB
�`B
�ZB
�TB
�`B
�ZB
�`B
�ZB
�TB
�ZB
�ZB
�TB
�ZB
�ZB
�ZB
�ZB
�ZB
�ZB
�ZB
�ZB
�TB
�TB
�ZB
�TB
�ZB
�ZB
�ZB
�ZB
�ZB
�ZB
�ZB
�TB
�TB
�TB
�ZB
�TB
�TB
�NB
�ZB
�TB
�ZB
�ZB
�ZB
�TB
�ZB
�TB
�ZB
�TG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                B
�2B
�2B
�3B
�3B
�3B
�4B
�4B
�4B
�5B
�5B
�/B
�0B
�0B
�7B
�8B
�2B
�3B
�-B
�4B
�ZB�B)�B+�B2B3B*�B�BSB �B6+B3B72BI�BJ�BK�BO�Bc<BibB��B�eB��B�`BjjBS�BR�BYB`/BeNBmBt�Bt�Bw�Bz�Bq�Bu�B~�B��B�+B�PB��B��B��B�qB�_B�SB�)B�B��B~�B}�B{�Bz�Bx�Bv�Br�Bn�BhqBe_BbMBW
BT�BQ�BL�BH�B=sB'�B&�B$�B �BgB B�B�lBȻB�qB�.B��B�dBw�BaQBTBK�B>�B7WB"�BVB
�B
�TB
�B
��B
�fB
�TB
�*B
��B
�fB
�)B
k�B
]@B
[4B
X"B
9jB
oB	�UB	��B	��B	��B	�KB	~	B	u�B	dpB	N�B	3IB	�B	�B		OB�B�(B��B��B�B�B��B��B��B��B��B�~B��B��B�zB��B��B�{B�|B�jB�qB�}B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�|B��B��B��B�~B�B�B�zB�nB�JB|B{Bv�Bt�Bq�Bo�Bi�Bi�Bg�Be�Bc�Bb�B_rB]gB]gB]hB_uB_uB\dB]jB[_BV@BU;BS/BQ#BMBMBI�BF�BE�BB�BA�B;�B<�B<�B9�B9�B6�B6�B5�B3xB2sB1mB1nB6�B6�B2uB2vB-WB.^B0kB1rB2xB2yB1sB1tB1tB2{B2{B1vB3�B5�B6�B9�B;�B:�B=�B=�B>�B>�BM'BYsB��B��B�B��B��B��B��B�-B�IB�kB��B�BB�cBڥBܴB��B�MB�{B	
�B	�B	B	(�B	2�B	5�B	D?B	KmB	W�B	Z�B	fB	toB	��B	�	B	�B	�B	�7B	�wB	��B	��B	�B	�B	�9B	�gB	ȏB	ʟB	κB	��B	�B	�^B	�NB	�jB	��B	��B	��B
B
 B
B
8B

GB
cB
fB
�B
�B
�B
�B
�B
�B
�B
�B
!�B
!�B
# B
%B
&B
).B
-IB
.QB
/ZB
/]B
1mB
1pB
3B
4�B
4�B
6�B
6�B
7�B
9�B
:�B
;�B
<�B
=�B
=�B
>�B
@�B
A�B
CB
EB
EB
EB
F%B
H4B
H6B
I?B
JHB
JKB
L[B
MdB
NlB
P{B
Q�B
S�B
S�B
S�B
U�B
V�B
V�B
V�B
W�B
X�B
Z�B
\�B
^�B
]�B
^�B
`B
`B
aB
cB
bB
d(B
e1B
f9B
f<B
e8B
gGB
hOB
iYB
jaB
kjB
klB
luB
lwB
n�B
p�B
p�B
p�B
r�B
r�B
r�B
s�B
t�B
u�B
v�B
w�B
v�B
w�B
y�B
y�B
y�B
z�B
z�B
|B
}B
~B
~B
~B
$B
�3B
�5B
�AB
�FB
�aB
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
�B
�B
�#B
�*B
�5B
�=B
�HB
�UB
�eB
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
�B
�
B
�B
�$B
�$B
�@B
�UB
�iB
��B
��B
��B
��B
��B
��B
�B
�B
�1B
�@B
�UB
�kB
��B
��B
��B
��B
��B
��B
��B
��B
�B
�B
�%B
�5B
�DB
�SB
�WB
�YB
�]B
�_B
�cB
�eB
�iB
�fB
�oB
�qB
�uB
�xB
�{B
�~B
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
�2B
�8B
�2B
�2B
�8B
�2B
�2B
�2B
�2B
�8B
�2B
�8B
�2B
�8B
�2B
�,B
�8B
�2B
�9B
�3B
�-B
�3B
�3B
�-B
�3B
�3B
�3B
�3B
�3B
�3B
�4B
�4B
�.B
�.B
�4B
�.B
�4B
�4B
�5B
�5B
�5B
�5B
�5B
�/B
�/B
�0B
�6B
�0B
�0B
�*B
�6B
�1B
�7B
�7B
�7B
�1B
�7B
�1B
�8B
�2G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�201807242202532021061413552720210614135527202106171312202021061713122020210617131220201807242202532021061413552720210614135527202106171312202021061713122020210617131220PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            new conductivity = original conductivity * (1 + delta*TEMP + CPcor_SBE*PRES) / (1 + delta*TEMP_ADJUSTED + CPcor_new*PRES_ADJUSTED)                                                                                                                              none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            new conductivity = original conductivity * (1 + delta*TEMP + CPcor_SBE*PRES) / (1 + delta*TEMP_ADJUSTED + CPcor_new*PRES_ADJUSTED)                                                                                                                              none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.50 dbar                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            CPcor_new = -1.25e-07; CPcor_SBE = -9.57e-08; delta = 3.25e-06                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.50 dbar                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            CPcor_new = -1.25e-07; CPcor_SBE = -9.57e-08; delta = 3.25e-06                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                      none                                                                                                                                                                                                                                                            New conductivity computed by using a different CPcor value from that provided by Sea-Bird.                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            No significant salinity drift detected.                                                                                                                                                                                                                         Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                      none                                                                                                                                                                                                                                                            New conductivity computed by using a different CPcor value from that provided by Sea-Bird.                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            No significant salinity drift detected.                                                                                                                                                                                                                         PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            AO  AO  ARCAARCAADJPADJP                                                                                                                                        2018072422025320180724220253  IP  IP                                G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2018072422025320180724220253QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�5F03E           703E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2018072422025320180724220253QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�0               0               PM  PM  ARSQARSQCOWGCOWGV1.1V1.1                                                                                                                                2021061713145820210617131458IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                