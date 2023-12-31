CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS     N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-10T07:01:04Z creation      
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
_FillValue                 8  L�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  P�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 8  a�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  f   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  v�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 8  ��   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 8  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �    PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �    PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 8  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 8  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �0   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                   x   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                   �   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                   �   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                   �   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  � �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                      HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                   4   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    <   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar        \   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar        d   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�       l   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    t   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  �   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                   �   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  � �      � �Argo profile    3.1 1.2 19500101000000  20181010070104  20210722160153  5905738 5905738 US ARGO PROJECT                                                 US ARGO PROJECT                                                 GREGORY C. JOHNSON                                              GREGORY C. JOHNSON                                              PRES            TEMP            PSAL            PRES            TEMP            PSAL               #   #DD  AOAO7218                            7218                            2C  2C  DD  SOLO_D_MRV                      SOLO_D_MRV                      12001                           12001                           SBE603 01Dec16                  SBE603 01Dec16                  874 874 @؅d���@؅d���11  @؅d�J�@؅d�J�@6)��r�@6)��r��c�f�3]%�c�f�3]%11  GPS     GPS     Primary sampling: averaged [1Hz sampling by SBE-41CP averaged in 2-dbar bins]                                                                                                                                                                                   Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 AB  AA  AA  ?   ?L��@   @@  @�33@�33@�33@���A��A33A$��AA��Ac33A�  A�33A�  A���A���A�  A�33A�ffB   B��BffBffB   B(  B0  B8ffBA33BH��BPffBX  B`ffBh��Bp  BxffB�33B�  B���B���B���B�  B�33B�33B�  B�  B���B�33B�33B�  B�  B���B�  B�33B�33B�33B�  B�ffB�ffB�  B�ffB�33B癚B���B�  B�33B�ffB���C �C��C�fC  C33C
L�C33C�fC�CL�C33C�CL�C�C�fC33C 33C"�C$L�C&33C(  C*33C,33C.�C0�C1�fC4�C6�C8  C:  C;��C>�C@L�CB�CD  CF33CH33CJ  CL�CN  CO��CR  CT33CV  CW��CY�fC\33C^L�C`�Ca�fCd  Cf33ChL�Cj�Ck�fCn  Cp33Cr33Ct�Cu�fCx�CzL�C|�C}�fC�  C��C��C�  C��fC��3C�  C��C��C��C�  C��fC�  C��3C��fC��C�&fC��C��C�  C��fC�  C��C�&fC��C��3C��C�  C��fC�  C�  C��fC��C��C��C��3C�  C�&fC��C��3C�  C��fC��C�  C��fC��C��C�  C��3C��C�  C��fC��C�  C��3C��C�  C��fC�  C��fC��fC��3C�  C�  C��C��C��C��fC��3C��3C��3C��3C�  C��C��C�&fC��C��fC��3C�  C��C��C�  C��fC��3C��3C��3C��C��C��C�&fC��C�  C��C��C�  C��fC��3C�  C��C�&fC�&fC��C��3C��C��C��C��fC��C��C��C�  C��C��C��fC��C�  C��3C��3C��3C��C��C�  C��DfD��DffD  D� DffD��D�3D�D� D9�D��D"S3D$��D'��D*33D,��D/ٚD2��D5��D8l�D;,�D=��D@��DCy�DF9�DH� DK�fDN9�DP��DS@ DU��DX9�DZ��D]fD_` Da� Dd�Df� Dh� Dk9�Dm�fDo�fDr@ Dt�3Dv��DyS3D{S3D}�fD��D�L�D�� D��3D��fD�fD�FfD�s3D��fD��fD�	�D�<�D�l�D���D�� D���D�)�D�Y�D���D��fD��fD�3D�I�D��fD���D���D��D�S3D��3D��3D�� D�	�D�6fD�` D���D��3D�ٚD�fD�6fD�i�D���D��fD�3D�P D�� D��fD��fD�)�D�` D���D�ٚD� D�L�D���D��fD�3D�@ D�vfDũ�D��D�#3D�VfDʆfD˶fD�� D�#3D�I�DЀ Dѣ3D���D���D��D�9�D�VfD�s3Dِ Dڬ�D��fD�� D��3D�  D� D�#3D�)�D�9�D�C3D�VfD�ffD�s3D�3D�3D��D멚D� D���D��3D��3D�� D��3D���D��fD�� D��D�� D��3D���D�	�D�  D� D�  D�,�D�<�E &fE �3E0 E� E33E��E@ E�fEP E� E` E��EfE	� E
��E��E@ EL�E� E� E�fEh Eh E��E9�E+3E��E�E33E|�E��E��E �E"1�E#t�E$��E&3E'^fE(�3E)��E+ E,��E-~fE.��E/� E1h E2��E3�fE5K3E6C3E7��E8� E:&fE;fE<��E?�fEB�3EE��EH��ELS3EOS3ERP EUd�EX� E[��E^��Ea�3Ed�EhFfEk33End�Eq� EtٚEw�fE{fE}�fE��3E�!�E��fE�X�E��fE�\�E��3E�73E���E���E�@ E���E��3E��E�e�E���E�3E�\�E���E�	�E�P E��fE���E�@ E�� E���E�#3E�� E��fE��E�zfE�� E� E�RfE�� E�fE�C3E���E��fE�2fE��fE��fE�5�E��fE��3E��?   ?��?   ?   ?��?   ?��?��?��?��?333?333?333?L��?fff?���?���?�33?�  ?ٙ�?�33@ff@��@&ff@9��@Fff@`  @l��@�33@���@�ff@�33@���@���@�ff@�33@���@陚@�33A��A  A  AffAffA&ffA+33A333A;33AA��AI��AP  AX  A^ffAh  AnffAt��A{33A�ffA���A���G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111144141444144111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                ?�  ?�ff@   @`  @�33@�33@�33@���A	��A33A,��AI��Ak33A�  A�33A�  A���A���A�  A�33A�ffB  B
��BffBffB"  B*  B2  B:ffBC33BJ��BRffBZ  BbffBj��Br  BzffB�33B�  B���B���B���B�  B�33B�33B�  B�  B���B�33B�33B�  B�  B���B�  B�33B�33B�33B�  B�ffB�ffB�  B�ffB�33B虚B���B�  B�33B�ffB���C ��CL�CffC� C�3C
��C�3CffC��C��C�3C��C��C��CffC�3C �3C"��C$��C&�3C(� C*�3C,�3C.��C0��C2ffC4��C6��C8� C:� C<L�C>��C@��CB��CD� CF�3CH�3CJ� CL��CN� CPL�CR� CT�3CV� CXL�CZffC\�3C^��C`��CbffCd� Cf�3Ch��Cj��ClffCn� Cp�3Cr�3Ct��CvffCx��Cz��C|��C~ffC�@ C�L�C�Y�C�@ C�&fC�33C�@ C�L�C�Y�C�Y�C�@ C�&fC�@ C�33C�&fC�L�C�ffC�Y�C�L�C�@ C�&fC�@ C�L�C�ffC�L�C�33C�L�C�@ C�&fC�@ C�@ C�&fC�L�C�Y�C�L�C�33C�@ C�ffC�L�C�33C�@ C�&fC�L�C�@ C�&fC�L�C�Y�C�@ C�33C�Y�C�@ C�&fC�L�C�@ C�33C�L�C�@ C�&fC�@ C�&fC�&fC�33C�@ C�@ C�Y�C�Y�C�L�C�&fC�33C�33C�33C�33C�@ C�L�C�Y�C�ffC�L�C�&fC�33C�@ C�L�C�L�C�@ C�&fC�33C�33C�33C�L�C�L�C�Y�C�ffC�L�C�@ C�L�C�Y�C�@ C�&fC�33C�@ C�L�C�ffC�ffC�Y�C�33C�L�C�Y�C�L�C�&fC�L�C�Y�C�Y�C�@ C�Y�C�L�C�&fC�L�C�@ C�33C�33C�33C�Y�C�L�C�@ C�Y�D&fD��D�fD@ D� D�fD�D�3D9�D� DY�D��D"s3D%�D'��D*S3D-�D/��D2ٚD5��D8��D;L�D>�D@ٚDC��DFY�DI  DK�fDNY�DPٚDS` DUٚDXY�DZ��D]&fD_� Da� Dd9�Df� Di  DkY�Dm�fDpfDr` Dt�3Dw�Dys3D{s3D}�fD�,�D�\�D�� D��3D��fD�&fD�VfD��3D��fD��fD��D�L�D�|�D���D�� D��D�9�D�i�D���D��fD��fD�#3D�Y�D��fD���D���D�,�D�c3D��3D��3D�� D��D�FfD�p D���D��3D��D�fD�FfD�y�D���D��fD�#3D�` D�� D��fD�fD�9�D�p D���D��D�  D�\�D���D��fD�3D�P DĆfDŹ�D���D�33D�ffDʖfD��fD�  D�33D�Y�DА Dѳ3D���D�	�D�,�D�I�D�ffD؃3D٠ Dڼ�D��fD�� D�3D� D�  D�33D�9�D�I�D�S3D�ffD�vfD�3D�3D�3D��D빚D�� D���D��3D��3D�� D��3D���D��fD�� D���D�  D�3D��D��D� D�  D�0 D�<�D�L�E .fE �3E8 E� E;3E��EH E�fEX E� Eh E�EfE	� E
��E��EH ET�E� E� E�fEp Ep E��EA�E33E��E�E;3E��E��E��E �E"9�E#|�E$ɚE&3E'ffE(�3E)��E+  E,��E-�fE.��E/� E1p E2��E3�fE5S3E6K3E7��E8� E:.fE;&fE<��E?�fEB�3EE��EH��EL[3EO[3ERX EUl�EX� E[��E^��Ea�3Ed��EhNfEk;3Enl�Eq� Et�Ew�fE{fE}�fE��3E�%�E��fE�\�E��fE�`�E��3E�;3E���E���E�D E���E��3E�!�E�i�E���E�3E�`�E���E��E�T E��fE� �E�D E�� E���E�'3E�� E��fE�!�E�~fE�� E� E�VfE�� E�
fE�G3E���E��fE�6fE��fE��fE�9�E��fE��3E��?�  G�O�G�O�?�  G�O�?�  G�O�G�O�G�O�?���G�O�G�O�?���?�ff?�33?���?ٙ�?�33@   @��@��@&ff@9��@Fff@Y��@fff@�  @�ff@�33@���@�ff@�33@���@���@�ff@�33@���@���A��A	��A  A  AffA&ffA.ffA333A;33AC33AI��AQ��AX  A`  AfffAp  AvffA|��A���A�ffA���A���G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111144141444144111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                @ j@ v@ �@ {@ �@ "�@ )�@ /@ 7L@ >�@ FQ@ R�@ `�@ m:@ z3@ ��@ ��@ ��@ �~@ �w@ �o@ �t@ �y@ �q@@@�@,`@:�@I�@V�@c�@p�@~�@�P@��@��@��@@ψ@�/@��@�,@�@*@""@/�@<�@K�@Yn@ff@t@�@�\@��@�Y@�@��@��@�@��@��@
�@�@$�@33@A�@O�@^5@j@v@�p@��@��@�!@�@ȴ@׹@�@�@ �@�@�@(G@7�@E�@R�@a�@n�@z�@��@��@��@�-@�w@�|@�#@�@��@@�@ �@-@:@I@V�@b�@qS@~K@��@��@��@��@�2@ψ@�;@�@��@%@{@#�@1�@>@Ji@X�@g�@uk@�d@��@��@��@�@�J@Ӡ@��@�L@��@�@6@%�@3�@B8@O�@\)@hs@ww@�p@�h@�@�!@�@�@�
@�T@�Y@ �@�@�@(G@7L@DD@P�@_�@m:@y�@�7@��@��@��@�&@��@�#@�m@��@	@	�@	�@	+@	:�@	I@	UU@	bN@	r@	~K@	��@	�H@	�A@	�9@	�>@	�7@	܀@	�@	��@
v@
�@
""@
/�@
>�@
Lu@
Yn@
e	@
s_@
�@
��@
�U@
��@
�@
�W@
խ@
��@
�@
��@
=@�@&;@33@?}@M�@[z@i!@x&@��@�$@�y@��@��@�@�h@�@��@�Q@�@�@+@8�@E�@Q=@`B@n�@{�@�+@��@�5@��@�&@�*@�#@�@�q@j@b@
@+�@;d@H]@UU@dZ@��@B8@��@�
@�@g@�f@�e@9X@~K@�J@�@P�@��@�;@'�@s_@��@b@^�@�@� @B8@��@��@$�@m:@��@�Q@C�@��@�@b@SI@�u@Ӡ@{@T�@�0@�
@6@V@��@�
@�@X@�<@��@�@T�@��@�
@�@Z@��@��@�@]�@�a@��@!s@bN@�(@�@$�@e	@��@�@ &�@ g�@ ��@ �(@!-�@!o�@!��@!�@"3�@"t�@"��@"��@#5@@#uk@#��@#�@$3�@$r�@$��@$�@%5@@%v@%��@%�E@&@�@&�@&�W@'1@'I�@'��@'��@(@(UU@(��@(܀@) @)c�@)�A@)�y@*+@*oF@*�-@*�@+4�@+uk@+�R@+��@,8�@,z�@,�@,�~@-8�@-v�@-��@-�L@.-@.i�@.��@.�@/�@/Yn@/��@/��@0�@0?}@0y�@0�-@0��@1&�@1`A@1�H@1�O@2�@2FQ@2~K@2��@2�@3&;@3\)@3�u@3�@4�@4=q@4v@4�@4�`@5
@5Wb@5��@5��@6  @69X@6s_@6�f@6�@7�@7UU@7�P@7ƨ@8  @89X@8t@8�@8�@9��@:	�@:��@;'�@;��@<DD@<��@=_�@=�7@>@,@>�@?Q�@?�(@@��@@��@A�d@B*@B�4@C1�@C��@D�@D��@E3�@E��@FK�@F�h@Gi!@G��@He	@I  @I��@J%@J��@K@K�~@LM$@L��@MZ@M��@Nc�@N�7@Ol�@O�[@PqS@Qψ@S&�@Tv@U��@W-�@Xuk@Y��@[J@\�@]�o@_�@`dZ@a��@c�@dZ@e��@g
=@hx&@i��@k�@lX@m�9@o�@phr@q�@sv@t\�@u�f@u�@vR�@v�u@vӠ@wo@wQ=@w��@w�+@x)�@xe�@x�@x��@y4�@yp�@y��@z@z=q@z��@z�@z�Q@{Q�@{�+@{��@|$.@|X�@|�A@|܀@}(�@}v@}��@}�q@~B�@~v@~�1@I@SI@��@�@��@ jG�O�G�O�@ jG�O�@ jG�O�G�O�G�O�@ G�O�G�O�@ �@ v@ %@ �@ 1@ 	�@ 
=@ �@ �@ V@ b@ �@ �@ *@ �@ B@ �@ 
@  @ "�@ $�@ (G@ *S@ -@ /@ 1�@ 3�@ 7L@ :@ =q@ @,@ C�@ F�@ I@ Lu@ O�@ R�@ V@ X�@ \)@ ^�@ b�@ e�@ hs@ k.@ oF@ r@ ukG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A��mA��mA��A��A���A���A�JA�
=A���A���A�JA��A�{A���A���A֏\A�XA�7LAլA�7LA�-A���A���AԁA�I�A�(�A��A�z�A�ZA��TAΏ\A� �A��A�dZA�p�A��HA��A��wA��\A��A�A��A��PA���A�JA���A��DA��A�M�A�9XA���A�
=A��A�ZA�(�A�A��^A���A�\)A�ȴA���A�v�A�1'A���A��A���A��A�&�A��9A��A��TA��RA�7LA���A�7LA��A���A�A�`BA�bA�ĜA�K�A���A��+A�ĜA��A���A���A���A�bNA���A���A�hsA�A�^5A��A��jA��\A�S�A��A���A�Q�A��TA�Q�A��jA�XA���A��#A���A�I�A�l�A�^5A��-A���A���A�;dA��+A���A�ĜA���A�x�A���A�hsA�bA��RA���A�ffA��yA���A��hA���A�oA��A�A~�A|��A{��A{�Ay�wAx�AvQ�AtJAr  ApM�An�RAnbAm"�AkAjz�AidZAhM�AfVAd1'A`��A_
=A^1A]
=A\�yA\��A\  AZ �AX�RAW\)AT~�AS��AR�AQ�-AP��AN��ALE�AJ��AJ�!AJA�AI�FAH�AD��AB�\ABE�AA�^A?�A=x�A;A;oA:9XA8(�A7�A5��A4�9A3
=A2M�A2bA0��A/�A,�yA*��A(�A%��A$ZA#hsA"�jA"��A��A\)A%A��A&�AffA��A~�A=qAA&�A~�A�
A��A��A �A�mAhsA~�A��AhsA�A�RAƨAt�A\)A�yAE�A�wAl�A
ȴAȴA^5A �A�A@���@�J@���@��@��-@��`@�I�@�
=@��@���@�(�@�@�P@�"�@�v�@��@���@�hs@�|�@�~�@���@ҏ\@���@��T@���@�p�@�p�@�ƨ@�5?@�V@�v�@��@�z�@�v�@�  @��@��@�ȴ@�9X@���@�
=@�$�@�?}@�G�@�|�@��\@���@�ƨ@�E�@�z�@��@�K�@���@�{@���@��@��
@�;d@�/@��
@�5?@��@�  @�"�@��T@�X@��@�|�@�"�@��R@�ff@�&�@�Z@~�@z�!@xĜ@u�@sdZ@sC�@p�`@p1'@n�y@k�
@j^5@iX@f��@e/@c�m@b^5@a��@`�9@_|�@[@X�9@W�P@W�@T��@S��@R=q@P1'@Nȴ@M��@LZ@Kt�@Jn�@I��@H�9@G�@Fff@E/@D��@D1@C��@A7L@@�@?+@=�T@=`B@<Z@:��@:^5@8�9@7�w@6�R@5p�@4��@3C�@2�\@/�;@.�y@.��@.$�@-�@,I�@*��@)x�@(�9@'�@&E�@%��@$�@#@"��@"^5@"J@!%@ r�@�P@+@��@�@�/@��@�#@�7@7L@�@��@�h@O�@Z@1@M�@��@hs@�9@��@��@@/@��@9X@S�@
�!@
=q@	��@	G�@�@Q�@�@V@@@��@9X@1@��@@^5@x�@G�@ ��?��R?�O�?���?��P?��?�?���?��?�O�?�?陚?�b?��?��
?�J?�  ?��?ܬ?�?���?׮?և+?ա�?�Z?ӶF?��?�%?�bN?Ͼw?��?Ͳ-?�p�?̬?�1?�ƨ?�"�?�"�?ʟ�?��?ə�?���?ȓu?�1'?Ǯ?Ł?�t�?��?��?��w?�v�?��?�V?���?��D?�(�?���?���?�C�?���?�~�?�^5?�=q?�^5?���?��H?�dZ?��m?�1?�I�?��?�p�?�{?�5??�5??�V?�v�?��R?��?��?��?��?��?�;d?�\)?�|�?���?��w?��;?�  ?� �?�A�?�bN?���?��?���?�Ĝ?��`?�&�?�&�?�hs?��7?�hs?���?���?��?�J?�M�?�M�?�M�?�n�?\A��mA��A��A��TA��mA��HA��`A��HA��HA��#A��HA��TA��mA��A��A��A��A��mA��A��A��A��A��A��A��A��A��A���A��A��A���A���A�A�1A�VA�oA�
=A�A���A���A���A���A���A�A�JA��A��A��A��A��A��A��A�{A�oA�JA�%A���A��A��mA��#G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                A��mA��mA��A��A���A���A�JA�
=A���A���A�JA��A�{A���A���A֏\A�XA�7LAլA�7LA�-A���A���AԁA�I�A�(�A��A�z�A�ZA��TAΏ\A� �A��A�dZA�p�A��HA��A��wA��\A��A�A��A��PA���A�JA���A��DA��A�M�A�9XA���A�
=A��A�ZA�(�A�A��^A���A�\)A�ȴA���A�v�A�1'A���A��A���A��A�&�A��9A��A��TA��RA�7LA���A�7LA��A���A�A�`BA�bA�ĜA�K�A���A��+A�ĜA��A���A���A���A�bNA���A���A�hsA�A�^5A��A��jA��\A�S�A��A���A�Q�A��TA�Q�A��jA�XA���A��#A���A�I�A�l�A�^5A��-A���A���A�;dA��+A���A�ĜA���A�x�A���A�hsA�bA��RA���A�ffA��yA���A��hA���A�oA��A�A~�A|��A{��A{�Ay�wAx�AvQ�AtJAr  ApM�An�RAnbAm"�AkAjz�AidZAhM�AfVAd1'A`��A_
=A^1A]
=A\�yA\��A\  AZ �AX�RAW\)AT~�AS��AR�AQ�-AP��AN��ALE�AJ��AJ�!AJA�AI�FAH�AD��AB�\ABE�AA�^A?�A=x�A;A;oA:9XA8(�A7�A5��A4�9A3
=A2M�A2bA0��A/�A,�yA*��A(�A%��A$ZA#hsA"�jA"��A��A\)A%A��A&�AffA��A~�A=qAA&�A~�A�
A��A��A �A�mAhsA~�A��AhsA�A�RAƨAt�A\)A�yAE�A�wAl�A
ȴAȴA^5A �A�A@���@�J@���@��@��-@��`@�I�@�
=@��@���@�(�@�@�P@�"�@�v�@��@���@�hs@�|�@�~�@���@ҏ\@���@��T@���@�p�@�p�@�ƨ@�5?@�V@�v�@��@�z�@�v�@�  @��@��@�ȴ@�9X@���@�
=@�$�@�?}@�G�@�|�@��\@���@�ƨ@�E�@�z�@��@�K�@���@�{@���@��@��
@�;d@�/@��
@�5?@��@�  @�"�@��T@�X@��@�|�@�"�@��R@�ff@�&�@�Z@~�@z�!@xĜ@u�@sdZ@sC�@p�`@p1'@n�y@k�
@j^5@iX@f��@e/@c�m@b^5@a��@`�9@_|�@[@X�9@W�P@W�@T��@S��@R=q@P1'@Nȴ@M��@LZ@Kt�@Jn�@I��@H�9@G�@Fff@E/@D��@D1@C��@A7L@@�@?+@=�T@=`B@<Z@:��@:^5@8�9@7�w@6�R@5p�@4��@3C�@2�\@/�;@.�y@.��@.$�@-�@,I�@*��@)x�@(�9@'�@&E�@%��@$�@#@"��@"^5@"J@!%@ r�@�P@+@��@�@�/@��@�#@�7@7L@�@��@�h@O�@Z@1@M�@��@hs@�9@��@��@@/@��@9X@S�@
�!@
=q@	��@	G�@�@Q�@�@V@@@��@9X@1@��@@^5@x�@G�@ ��?��R?�O�?���?��P?��?�?���?��?�O�?�?陚?�b?��?��
?�J?�  ?��?ܬ?�?���?׮?և+?ա�?�Z?ӶF?��?�%?�bN?Ͼw?��?Ͳ-?�p�?̬?�1?�ƨ?�"�?�"�?ʟ�?��?ə�?���?ȓu?�1'?Ǯ?Ł?�t�?��?��?��w?�v�?��?�V?���?��D?�(�?���?���?�C�?���?�~�?�^5?�=q?�^5?���?��H?�dZ?��m?�1?�I�?��?�p�?�{?�5??�5??�V?�v�?��R?��?��?��?��?��?�;d?�\)?�|�?���?��w?��;?�  ?� �?�A�?�bN?���?��?���?�Ĝ?��`?�&�?�&�?�hs?��7?�hs?���?���?��?�J?�M�?�M�?�M�?�n�?\A��mA��A��A��TA��mA��HA��`A��HA��HA��#A��HA��TA��mA��A��A��A��A��mA��A��A��A��A��A��A��A��A��A���A��A��A���A���A�A�1A�VA�oA�
=A�A���A���A���A���A���A�A�JA��A��A��A��A��A��A��A�{A�oA�JA�%A���A��A��mA��#G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B�B�B�B�B�B�B �B�B�B�B!�B'�B+B,B.B6FB>wBD�B[#BjBk�BhsBjBo�Bs�Bv�Bv�Br�Br�Bu�B� B��B��B�qBǮBɺB�mB��B+B	7B+BBVB�B�B�B�B�B#�B/B2-B6FB>wBF�BG�BH�BJ�BK�BM�BT�BT�BT�BXB[#B^5BffBgmBe`BffBcTBcTB`BB`BB\)BZBYBVBQ�BM�BL�BJ�BI�BM�BR�BQ�BN�B8RB.B)�B%�B"�B!�B�B{BVBB��B��B��B�yB�;B�B��B��BB�7Bs�Be`B_;BW
BG�B$�B#�BuBJB	7BJB{B{BuB
=BB
��B
��B
�BB
�
B
ŢB
ÖB
�}B
�RB
�B
��B
��B
�%B
|�B
p�B
jB
bNB
ZB
Q�B
@�B
0!B
#�B
�B
\B

=B
B	��B	�B	�B	�ZB	�
B	ǮB	�LB	�'B	��B	��B	��B	��B	��B	�PB	�B	w�B	iyB	cTB	ZB	Q�B	M�B	?}B	2-B	-B	,B	(�B	%�B	{B��B�B�B�`B�#B��BƨBĜB�jB�LB�-B�B��B��B��B��B��B��B�1B�1B�Bx�Bv�Bt�Br�Bk�BcTBaHB]/BZBYBT�BS�BR�BR�BO�BN�BM�BL�BF�BF�BF�BD�BA�BB�B@�B>wB>wB<jB;dB<jB;dB;dB:^B;dB<jB:^B:^B;dB@�B;dB7LB7LB49B1'B1'B0!B/B.B-B)�B1'B0!B/B/B/B0!B/B.B/B6FB7LB@�BC�BH�B]/Bs�B{�B�VB��B�!B�mB��B��B	�B	33B	;dB	G�B	I�B	S�B	hsB	k�B	p�B	u�B	z�B	~�B	�JB	�\B	��B	��B	��B	�B	�B	�RB	�^B	��B	��B	��B	��B	��B	��B	�B	�/B	�HB	�fB	�mB	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B
B
%B

=B
DB
DB
\B
bB
oB
�B
�B
�B
�B
�B
�B
!�B
!�B
 �B
#�B
'�B
(�B
)�B
)�B
,B
+B
.B
0!B
2-B
2-B
49B
5?B
5?B
6FB
8RB
9XB
9XB
;dB
;dB
<jB
<jB
@�B
@�B
A�B
C�B
C�B
D�B
F�B
F�B
H�B
H�B
J�B
K�B
K�B
M�B
M�B
Q�B
Q�B
Q�B
Q�B
R�B
T�B
VB
XB
XB
ZB
[#B
[#B
\)B
^5B
^5B
^5B
_;B
_;B
`BB
`BB
aHB
aHB
bNB
bNB
cTB
ffB
ffB
e`B
gmB
iyB
jB
jB
l�B
k�B
n�B
m�B
n�B
n�B
o�B
q�B
r�B
s�B
s�B
t�B
u�B
u�B
v�B
v�B
w�B
w�B
w�B
x�B
z�B
z�B
z�B
{�B
|�B
}�B
|�B
~�B
~�B
� B
~�B
�B
�B
�B
�B
�%B
�1B
�=B
�DB
�VB
�PB
�VB
�bB
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
��B
��B
�B
�B
�B
�B
�!B
�'B
�-B
�3B
�9B
�9B
�?B
�?B
�FB
�FB
�LB
�RB
�XB
�XB
�XB
�^B
�^B
�^B
�dB
�^B
�dB
�jB
�dB
�dB
�dB
�dB
�jB
�jB
�jB
�dB
�dB
�jB
�jB
�jB
�jB
�jB
�jB
�jB
�jB
�jB
�jB
�jB
�jB
�jB
�jB
�dB
�jB
�jB
�jB
�jB
�jB
�jB
�dB
�jB
�jB
�jB
�jB
�jB
�jB
�jB
�jB
�jB
�jB
�jB�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B �B �B�B�B�B�B�B�B�B�B"�B$�B"�B&�B(�B)�B(�B)�B+B+B,B,B,B,B-B.G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                B�B�B�B�B�B�B�B�B�B�B�B%�B(�B)�B,B49B<jBB�BYBhsBiyBffBhsBm�Bq�Bt�Bt�Bp�Bp�Bs�B}�B��B��B�dBŢBǮB�`B��BB+BBBJB{B�B�B�B�B!�B-B0!B49B<jBD�BE�BF�BH�BI�BK�BR�BR�BR�BVBYB\)BdZBe`BcTBdZBaHBaHB^5B^5BZBXBW
BS�BO�BK�BJ�BH�BG�BK�BP�BO�BL�B6FB,B'�B#�B �B�B�BoBJBB��B��B�B�mB�/B�B��BɺB��B�+Bq�BcTB]/BT�BE�B"�B!�BhB
=B+B
=BoBoBhB1B
��B
��B
��B
�5B
��B
ÖB
��B
�qB
�FB
�B
��B
�uB
�B
z�B
n�B
hsB
`BB
XB
O�B
>wB
.B
!�B
{B
PB
1B
  B	��B	�B	�yB	�NB	��B	ŢB	�?B	�B	��B	��B	��B	��B	�uB	�DB	�B	u�B	gmB	aHB	XB	O�B	K�B	=qB	0!B	+B	)�B	&�B	#�B	oB��B�B�yB�TB�B��BĜBB�^B�?B�!B�B��B��B��B��B��B��B�%B�%B�Bv�Bt�Br�Bp�BiyBaHB_;B[#BXBW
BR�BQ�BP�BP�BM�BL�BK�BJ�BD�BD�BD�BB�B?}B@�B>wB<jB<jB:^B9XB:^B9XB9XB8RB9XB:^B8RB8RB9XB>wB9XB5?B5?B2-B/B/B.B-B,B+B'�B/B.B-B-B-B.B-B,B-B49B5?B>wBA�BF�B[#Bq�By�B�JB��B�B�`B�B��B	�B	1'B	9XB	E�B	G�B	Q�B	ffB	iyB	n�B	s�B	y�B	}�B	�DB	�VB	��B	��B	��B	�B	�B	�LB	�XB	�}B	�}B	�}B	ɺB	��B	��B	��B	�)B	�BB	�`B	�fB	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B
B
B
	7B

=B

=B
VB
\B
hB
{B
�B
�B
�B
�B
�B
 �B
 �B
�B
"�B
&�B
'�B
(�B
(�B
+B
)�B
-B
/B
1'B
1'B
33B
49B
49B
5?B
7LB
8RB
8RB
:^B
:^B
;dB
;dB
?}B
?}B
@�B
B�B
B�B
C�B
E�B
E�B
G�B
G�B
I�B
J�B
J�B
L�B
L�B
P�B
P�B
P�B
P�B
Q�B
S�B
VB
XB
XB
ZB
[#B
[#B
\)B
^5B
^5B
^5B
_;B
_;B
`BB
`BB
aHB
aHB
bNB
bNB
cTB
ffB
ffB
e`B
gmB
iyB
jB
jB
l�B
k�B
n�B
m�B
n�B
n�B
o�B
q�B
r�B
s�B
s�B
t�B
u�B
u�B
v�B
v�B
w�B
w�B
w�B
x�B
z�B
z�B
z�B
{�B
|�B
}�B
|�B
~�B
~�B
� B
~�B
�B
�B
�B
�B
�%B
�1B
�=B
�DB
�VB
�PB
�VB
�bB
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
��B
��B
�B
�B
�B
�B
�'B
�-B
�3B
�9B
�FB
�FB
�LB
�LB
�RB
�RB
�XB
�^B
�dB
�dB
�dB
�jB
�jB
�jB
�qB
�jB
�qB
�}B
�wB
�wB
�wB
�wB
�}B
�}B
�}B
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
�}B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B �B"�B �B$�B&�B'�B&�B'�B(�B(�B)�B)�B)�B)�B+B,G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�201810100701042021061413524320210614135243202106141746462021061417464620210614174646201810100701042021061413524320210614135243202106141746462021061417464620210614174646PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            new conductivity = original conductivity * (1 + delta*TEMP + CPcor_SBE*PRES) / (1 + delta*TEMP_ADJUSTED + CPcor_new*PRES_ADJUSTED)                                                                                                                              none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS                                                                                                                                                                                                                                       PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            new conductivity = original conductivity * (1 + delta*TEMP + CPcor_SBE*PRES) / (1 + delta*TEMP_ADJUSTED + CPcor_new*PRES_ADJUSTED)                                                                                                                              none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS                                                                                                                                                                                                                                       surface_pressure=-0.50 dbar                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            CPcor_new = -1.25e-07; CPcor_SBE = -9.57e-08; delta = 3.25e-06                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            OW r = 0.9999 (+/-0), vertically averaged dS = -0.002 (+/-0)                                                                                                                                                                                                    surface_pressure=-0.50 dbar                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            CPcor_new = -1.25e-07; CPcor_SBE = -9.57e-08; delta = 3.25e-06                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            OW r = 0.9999 (+/-0), vertically averaged dS = -0.002 (+/-0)                                                                                                                                                                                                    Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                      none                                                                                                                                                                                                                                                            New conductivity computed by using a different CPcor value from that provided by Sea-Bird.                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Significant salinity sensor drift detected. OW least squares fit adopted.                                                                                                                                                                                       Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                      none                                                                                                                                                                                                                                                            New conductivity computed by using a different CPcor value from that provided by Sea-Bird.                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Significant salinity sensor drift detected. OW least squares fit adopted.                                                                                                                                                                                       PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            AO  AO  ARCAARCAADJPADJP                                                                                                                                        2018101007010420181010070104  IP  IP                                G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2018101007010420181010070104QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�5F03E           703E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2018101007010420181010070104QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�0               0               PM  PM  ARSQARSQCOWGCOWGV1.1V1.1                                                                                                                                2021072216015320210722160153IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                