CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  
   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-07-24T22:02:47Z creation      
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
resolution        =���   axis      Z        P  ;�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   L$   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     P  P8   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   `�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     P  d�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     P  t�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   �<   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     P  �P   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     P  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     P  �   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   �T   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     P  �h   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   Ҹ   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     P  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                   �   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                   �   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                   �   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                   �   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  � �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                   $   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                   @   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    H   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar        h   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar        p   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�       x   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  �   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  � �Argo profile    3.1 1.2 19500101000000  20180724220247  20210617131454  5905739 5905739 US ARGO PROJECT                                                 US ARGO PROJECT                                                 GREGORY C. JOHNSON                                              GREGORY C. JOHNSON                                              PRES            TEMP            PSAL            PRES            TEMP            PSAL                  DD  AOAO7219                            7219                            2C  2C  DD  SOLO_D_MRV                      SOLO_D_MRV                      12002                           12002                           SBE603 01Dec16                  SBE603 01Dec16                  874 874 @�f�9uK�@�f�9uK�11  @�f�8�@@�f�8�@@6��hr�@6��hr��c�P	,�l�c�P	,�l11  GPS     GPS     Primary sampling: averaged [1Hz sampling by SBE-41CP averaged in 2-dbar bins]                                                                                                                                                                                   Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 AC  AA  AA  >���?fff@   @@  @�33@�33@�33@�33@���A��A&ffA@  A`  A���A�  A�  A���A���Aљ�A�  A�ffA�33B  B��B��B��B(ffB133B933B@ffBHffBP  BX  B`  Bh  Bo��Bx  B�  B�  B�  B�33B�  B�33B�33B�  B�  B�33B�33B�33B�  B�ffB�  B���B�  B�  B�  B�ffBϙ�B���B�ffB�33B�  B�  B���B���B�  B�  B�  B�33C �C�C33C33C33C
L�C33CL�C�fC  C  C�C�C�C�C  C�fC!�fC$33C&�C(  C*33C,  C-�fC0�C2�C3�fC6  C8L�C:�C;��C>�C@33CB33CC�fCF33CH33CJ  CL33CN  CO�fCR�CT33CV�CW��CZ  C\�C^  C_��Ca�fCd�Ce�fCg�fCj�Cl�Cm��Cp�Cr  Ct  Cv  Cw��Cz�C|  C}�fC��C��C�  C�  C��3C��C��C��C��C�  C��3C��3C��3C��3C��C��C��C��C��C��C�  C��3C��fC�  C�  C��fC��3C��C�&fC��C��3C�  C��C��C��C�&fC�33C��C��fC��fC��3C��3C�  C�  C�  C�  C�  C��3C��C��C�  C�&fC��C��C�  C�  C�  C��3C��3C��3C��3C�  C�  C��C��C�  C��fC�  C��C�&fC��C��fC�  C��C�  C��fC��3C��C��3C��fC�  C��3C��fC�  C��C��C��3C��C��C�  C�&fC��C��C��C��3C��C�  C��fC��C�&fC��C�  C��C��C�  C��C�  C��fC�  C�  C��fC�  C��C�&fC��C��fC��3C��C��C��3C�  C�&fC�  C�ٚC�  D@ D� Ds3D
��D�fD3D�fD33D�fD  D��DfD!� D$�D&�fD)fD+��D.&fD0� D3Y�D5�3D8�fD;&fD=�fD@` DB��DE�fDHL�DKfDM��DPl�DS�DU��DX�fD[L�D^�D`�3Dc��DfY�Di�Dk�3Dnl�Dq33DtfDv� Dyy�D{�fD~�3D�� D��D�|�D�� D�@ D���D���D�FfD��fD���D�<�D��3D���D� D�L�D��fD���D��D�  D�P D�� D��fD�� D�� D�  D�C3D�p D�� D���D�� D� D�@ D�|�D��fD�� D�0 D�l�D���D��3D�9�D��3D�ٚD�#3D�|�D��fD��D�l�D��fD��D�i�D��3D�fD�VfDÜ�D��3D�&fD�ffDȣ3D���D�  D�Y�D͉�DζfD��3D�	�D�0 D�S3D�s3DՐ D֩�D��fD��fD��fD���D��D� D� D��D��D��D�#3D��D��D��D�fD�3D�3D�	�D�fD�	�D�  D���D��fD��3D��3D��fD�  D�fD�	�D�3D�&fD�9�D�FfD�S3D�i�D�i�D��fD�� D���D��3E y�E	�E�3E( E� EH E��Ed�E��E��E3E�3E1�E��E�3E
� E� E�3E� E3E� E��E3E1�EVfE3E$�ED�Ec3E�E3E0 E� E �3E!� E#��E$��E%�3E'<�E(H E)Q�E*�3E+�E-nfE.nfE/� E0�E2Y�E3Y�E4� E68 E70 E8��E9��E:� E<K3E?vfEB�fEE~fEH�fEL3EN��ER	�EUfEXvfE[� E^�3Ea�fEd�3Eg�3Ek3En,�Eq^fEt�3Ew��Ez��E}�3E�m�E��E��fE�3E���E�O3E���E�M�E�� E�~fE��E��fE�)�E�o3E��3E�fE�\�E��fE��E�g3E�� E�fE�BfE���E�� E�33E��3E��3E�&f>L��>L��>���>L��>���>���>���>���>���>L��>���>���>���>���?   ?   ?   ?   >���?��?��?��?333?L��?L��?fff?���?���?���?�33?ٙ�?�ff?�33@ff@��@&ff@,��@@  @S33@`  @s33@�33@�  @���@�33@���@�ff@�ff@�  @���@陚@�ffA��A33A��A  A   A&ffA.ffA4��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111414114444141414444144114111411111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                              ?L��?�33@   @`  @�33@�33@�33@�33AffA��A.ffAH  Ah  A���A�  A�  A���A���Aՙ�A�  A�ffB��B
  B��B��B!��B*ffB333B;33BBffBJffBR  BZ  Bb  Bj  Bq��Bz  B�  B�  B�  B�33B�  B�33B�33B�  B�  B�33B�33B�33B�  B�ffB�  B���B�  B�  B�  B�ffBЙ�B���B�ffB�33B�  B�  B���B���B�  B�  B�  B�33C ��C��C�3C�3C�3C
��C�3C��CffC� C� C��C��C��C��C� C ffC"ffC$�3C&��C(� C*�3C,� C.ffC0��C2��C4ffC6� C8��C:��C<L�C>��C@�3CB�3CDffCF�3CH�3CJ� CL�3CN� CPffCR��CT�3CV��CXL�CZ� C\��C^� C`L�CbffCd��CfffChffCj��Cl��CnL�Cp��Cr� Ct� Cv� CxL�Cz��C|� C~ffC�Y�C�L�C�@ C�@ C�33C�Y�C�Y�C�L�C�L�C�@ C�33C�33C�33C�33C�Y�C�Y�C�Y�C�Y�C�L�C�L�C�@ C�33C�&fC�@ C�@ C�&fC�33C�L�C�ffC�L�C�33C�@ C�L�C�Y�C�Y�C�ffC�s3C�L�C�&fC�&fC�33C�33C�@ C�@ C�@ C�@ C�@ C�33C�L�C�L�C�@ C�ffC�Y�C�L�C�@ C�@ C�@ C�33C�33C�33C�33C�@ C�@ C�L�C�Y�C�@ C�&fC�@ C�L�C�ffC�L�C�&fC�@ C�Y�C�@ C�&fC�33C�L�C�33C�&fC�@ C�33C�&fC�@ C�Y�C�L�C�33C�Y�C�Y�C�@ C�ffC�Y�C�L�C�L�C�33C�L�C�@ C�&fC�L�C�ffC�Y�C�@ C�Y�C�L�C�@ C�Y�C�@ C�&fC�@ C�@ C�&fC�@ C�L�C�ffC�L�C�&fC�33C�Y�C�L�C�33C�@ C�ffC�@ C��C�@ D` D  D�3D�D�fD33D�fDS3D�fD@ D��D&fD!� D$,�D&�fD)&fD+��D.FfD0� D3y�D63D8�fD;FfD=�fD@� DC�DE�fDHl�DK&fDMٚDP��DS9�DU��DX�fD[l�D^9�D`�3Dc��Dfy�Di,�Dk�3Dn��DqS3Dt&fDv� Dy��D{�fD~�3D�� D�,�D���D�� D�P D���D�	�D�VfD��fD���D�L�D��3D���D�  D�\�D��fD�ɚD���D�0 D�` D�� D��fD�� D�  D�0 D�S3D�� D�� D�ɚD�� D�  D�P D���D��fD�  D�@ D�|�D���D�3D�I�D��3D��D�33D���D��fD�,�D�|�D��fD�,�D�y�D��3D�fD�ffDì�D��3D�6fD�vfDȳ3D���D�0 D�i�D͙�D��fD��3D��D�@ D�c3Dԃ3Dՠ Dֹ�D��fD��fD��fD�	�D��D�  D�  D�)�D�)�D�)�D�33D�)�D�,�D�)�D�&fD�#3D�#3D��D�fD��D� D�	�D�fD�3D�3D�fD� D�fD��D�#3D�6fD�I�D�VfD�c3D�y�D�y�D��fD�� D���D��3E ��E�E�3E0 E� EP E��El�E��E��E3E�3E9�E��E�3E
� E� E�3E  E#3E� E��E3E9�E^fE3E,�EL�Ek3E�E#3E8 E� E �3E!� E#��E$��E%�3E'D�E(P E)Y�E*�3E+�E-vfE.vfE/� E0�E2a�E3a�E4� E6@ E78 E8��E:�E:� E<S3E?~fEB�fEE�fEH�fEL3EN��ER�EUfEX~fE[� E^�3Ea�fEd�3Eh3Ek3En4�EqffEt�3Ew��Ez��E}�3E�q�E��E��fE�3E���E�S3E���E�Q�E�� E��fE��E��fE�-�E�s3E��3E�fE�`�E��fE��E�k3E�� E�
fE�FfE���E�� E�73E��3E��3E�*fG�O�?333G�O�?333?L��G�O�G�O�G�O�G�O�?333G�O�?L��G�O�?fffG�O�G�O�G�O�G�O�?fffG�O�G�O�?���?���G�O�?�ff?�33?���G�O�?ٙ�?�33@��@33@��@&ff@9��@Fff@L��@`  @s33@�  @���@�33@�  @���@�33@���@�ff@�ff@�  @���@���A33A	��A33A��A   A(  A.ffA6ffA<��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111414114444141414444144114111411111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                              @ @ %@ �@ {@ �@ "�@ )�@ 0x@ 5�@ >@ F�@ Q�@ _�@ n�@ z�@ ��@ ��@ ��@ ��@ �&@ �o@ ��@ �@ �@�@b@g@.l@<@H]@V@b�@p�@~K@��@��@�A@��@@�7@ލ@�@��@�@{@""@0x@>@K�@X�@g�@t@�@�\@�@��@��@Ĝ@��@�@�@��@
=@6@$�@33@@�@N�@\�@j@x&@�|@�#@��@�!@�@�o@�[@�@�Y@ �@V@�@)�@6�@C�@Q=@`�@m�@z�@��@�0@�(@�-@��@�@�t@�(@�q@@�@ @-�@9X@I@V�@b�@r@~K@�D@�H@��@��@�2@�7@ލ@�@��@%@*@!s@/@>@K�@Wb@g@t@��@�\@��@�Y@�R@�J@��@��@��@��@	�@B@&�@3�@A�@N�@[z@i!@v�@�p@�$@��@�r@�@�@׹@�@�@��@�@O@'�@5�@D�@S�@`B@l�@z�@�7@��@�5@��@��@�|@�@�@�@	�@	@	�@	,`@	:@	G�@	T�@	c�@	qS@	~K@	��@	��@	��@	��@	@	�7@	�/@	��@	�~@
%@
{@
""@
0x@
>�@
K@
Wb@
ff@
t�@
��@
�@
��@
��@
��@
��@
�C@
��@
�@
��@�@�@$�@1�@@�@O�@\�@i!@x�@�|@��@�y@�r@�k@�@�[@�`@�Y@��@V@[@*S@6�@E�@R�@_�@n�@z�@�+@�0@��@�!@�&@�|@܀@��@�e@�@o@g@+�@:@I�@UU@`�@p�@  @G�@��@��@�@^5@��@�(@0x@p�@��@�q@:@�@�>@�@M�@�u@�t@!s@hs@��@�q@>@�@�@*@]�@��@�@;d@�p@�*@�@dZ@��@�9@F�@�@��@$.@n�@�^@�@Q�@�U@�#@'�@t@��@�@X�@��@�@8�@�@�W@b@X@��@�@(�@l�@�r@��@ 1�@ t@ ��@ ��@!4�@!t@!�~@!�Y@"0x@"p�@"�@"�@#,`@#m:@#�@#�@$4�@$ww@$��@$�Q@%C�@%�7@%��@&�@&^5@&��@&�@@'4�@'}�@'�J@(@(X@(�@(�`@)-�@)uk@)�@* �@*E�@*��@*�|@+b@+UU@+�<@+�@,B@,Yn@,�<@,�
@-*@-R�@-�\@-�o@.1@.B8@.|?@.��@.�@/(�@/_�@/�<@/��@0v@0>@0r�@0��@0��@1�@1K�@1�d@1��@1��@2$/@2X�@2��@2��@2��@30x@3g�@3�m@3�h@4�@4H]@4�@4��@4� @50x@5k�@5�z@5�;@6O@6X@6�u@6��@7V@7Lu@7��@7��@8j@8?}@8|�@8�^@8�q@94�@9r@9��@9�@:k.@;""@;�@<B@<��@=@=�c@>FQ@>��@?:�@?��@@r@@�(@Ae	@A�;@B�i@C1@C~K@D/�@D�4@E�@Eƨ@F:�@F�r@GZ�@G��@H> @H�@IYn@I�Q@Jl�@K�@Kx&@L�@L��@M""@M�&@N(�@N�2@OZ�@O�&@PV�@Q��@Sb@TC�@U��@W@X?}@Y��@Z��@\[z@]��@_]@`C�@a�4@b��@dI@e�@f��@hSI@i��@j�`@lO�@m��@n��@p8�@q��@r�@tQ=@u�I@v�<@x=q@y�<@z�M@|FP@}�$@}ψ@~(G@~a�@~�I@~��@)�@}�@��@��@�T@�C>@�i�@��@��S@���@���G�O�@ ^G�O�@ ^@ G�O�G�O�G�O�G�O�@ ^G�O�@ G�O�@ �G�O�G�O�G�O�G�O�@ �G�O�G�O�@ @ �G�O�@ v@ %@ �G�O�@ 1@ 	�@ �@ J@ �@ V@ b@ �@ o@ {@ �@ �@ �@ �@ �@  �@ "�@ $�@ &�@ *S@ ,`@ /@ 1�@ 4�@ 7L@ ;d@ >@ @�@ DD@ F�@ Ji@ M$G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�oA�oA�1A�1A�1A�1A�JA�JA�%A�%A�%A�  A�  A���A�A�A���A���A���AǗ�A��A��
Aď\Að!A���A�dZA���A��-A���A���A���A��A�t�A�ffA�S�A�5?A�ĜA��A�/A�7LA��A���A��/A��/A�%A���A�A�A���A��A���A�Q�A���A�7LA��jA�~�A�VA� �A���A���A���A�jA��A�t�A�r�A��uA��FA��A�33A��A�ƨA�;dA��HA���A�|�A�M�A��A���A�XA��mA���A�I�A�?}A��^A���A�z�A��A�\)A�^5A�7LA�M�A�jA��A�^5A���A��A���A��\A���A�;dA��
A�;dA� �A�VA�7LA��!A�XA�1A�{A�n�A�bA���A���A��A�\)A���A}�^A{��AxȴAs�-Ap�\An9XAjȴAh=qAfv�AeS�Ad�+Ac�
Aa�-A`1A]oAX�AX1'AWG�AVA�AU�
AUƨAUK�AT�RATZAT9XAS`BAR��AQ��AP�+AOS�AN��AM�hAL�HAL��ALZAJ��AIoAGx�AGhsAGt�AGhsAGS�AF �AC�AAoA@r�A?hsA>$�A="�A<�DA;|�A9l�A89XA7
=A61'A5��A5G�A4z�A4jA4M�A3�;A3+A1
=A/l�A.�\A-G�A,�\A+?}A*  A(=qA&{A$  A"$�A �`A�TA%A��A��AƨA�DAC�AoA^5A�AdZA�AjA��A�A&�A~�AdZA  A`BA�uAbA��A�!AK�A��Al�A	�hAffA�mA�A�AJA;dAVA�mA\)A�\A(�AƨA v�A 1@��A  �@��@���@��;@���@��+@�9X@��+@�  @���@�!@�v�@��y@�  @�C�@�^@�?}@�I�@�l�@�K�@�ȴ@噚@��@�l�@�;d@�ff@�=q@�@͙�@�`B@�J@�dZ@��@�J@�I�@��@��@�%@�`B@���@�$�@��-@�Z@���@���@�p�@�J@��!@��/@��H@���@�Ĝ@�^5@���@�b@�@��7@�G�@��/@��@���@�O�@� �@���@�x�@� �@�n�@���@��@�Q�@�;d@��+@���@���@~��@}`B@{t�@z�@x�@v��@v5?@t��@r-@p �@m�@lz�@kS�@j=q@i7L@g��@f��@e�-@ct�@ax�@`1'@^ȴ@\I�@Z��@W�w@V$�@T(�@R�H@QG�@O�@N��@MO�@J��@I&�@G�P@F�R@F$�@DZ@Ct�@BM�@A&�@@1'@?
=@>E�@=O�@<��@;dZ@9�7@7�P@65?@4�/@3��@2�\@1G�@0A�@/\)@.$�@-O�@,j@+"�@)��@)&�@(Q�@'��@&��@&@%�@$Z@#ƨ@"�!@"J@!X@�@�+@@�/@z�@�m@o@�@ �@�@
=@��@�T@`B@I�@��@33@�\@��@Ĝ@bN@�;@�R@�T@�@�@9X@�m@�F@33@
�H@	x�@�9@��@��@ȴ@��@$�@�@p�@��@��@�!@^5@�^@%@ b@   ?�|�?��?��?�(�?�X?�E�?���?�33?�-?�Ĝ?���?��?�dZ?���?�u?�K�?��?�9X?�33?�&�?ߝ�?�{?�I�?ۅ?��?���?�Q�?�l�?�E�?�?}?��?�o?�M�?�%?�bN?��?�V?͑h?��?�ƨ?���?���?���?���?�Q�?���?�$�?ļj?���?�S�?�-?�&�?�A�?��?�v�?�5??�p�?��?�ƨ?��?�C�?�?��H?��H?��H?�dZ?�dZ?��?���?�1?�j?��?�O�?��-?�{?��?�\)?�  ?���?�G�?�hs?�hs?��7?��7?��7?��7?��7?��7?�%?�&�?�G�?��7?��7?���?��?�JA�bA�bA�oA�oA�bA�bA�bA�oA�oA�oA�oA��A�{A�bA�bA�bA�oA�bA�{A�{A�{A�bA�{A�oA�oA�bA�bA�bA�VA�bA�
=A�
=A�
=A�%A�%A�%A�1A�
=A�1A�1A�1A�1A�1A�1A�1A�1A�
=A�JA�JA�VA�1A�%A�%A�A�%A�%A�%A�%A�A�  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                              A�oA�oA�1A�1A�1A�1A�JA�JA�%A�%A�%A�  A�  A���A�A�A���A���A���AǗ�A��A��
Aď\Að!A���A�dZA���A��-A���A���A���A��A�t�A�ffA�S�A�5?A�ĜA��A�/A�7LA��A���A��/A��/A�%A���A�A�A���A��A���A�Q�A���A�7LA��jA�~�A�VA� �A���A���A���A�jA��A�t�A�r�A��uA��FA��A�33A��A�ƨA�;dA��HA���A�|�A�M�A��A���A�XA��mA���A�I�A�?}A��^A���A�z�A��A�\)A�^5A�7LA�M�A�jA��A�^5A���A��A���A��\A���A�;dA��
A�;dA� �A�VA�7LA��!A�XA�1A�{A�n�A�bA���A���A��A�\)A���A}�^A{��AxȴAs�-Ap�\An9XAjȴAh=qAfv�AeS�Ad�+Ac�
Aa�-A`1A]oAX�AX1'AWG�AVA�AU�
AUƨAUK�AT�RATZAT9XAS`BAR��AQ��AP�+AOS�AN��AM�hAL�HAL��ALZAJ��AIoAGx�AGhsAGt�AGhsAGS�AF �AC�AAoA@r�A?hsA>$�A="�A<�DA;|�A9l�A89XA7
=A61'A5��A5G�A4z�A4jA4M�A3�;A3+A1
=A/l�A.�\A-G�A,�\A+?}A*  A(=qA&{A$  A"$�A �`A�TA%A��A��AƨA�DAC�AoA^5A�AdZA�AjA��A�A&�A~�AdZA  A`BA�uAbA��A�!AK�A��Al�A	�hAffA�mA�A�AJA;dAVA�mA\)A�\A(�AƨA v�A 1@��A  �@��@���@��;@���@��+@�9X@��+@�  @���@�!@�v�@��y@�  @�C�@�^@�?}@�I�@�l�@�K�@�ȴ@噚@��@�l�@�;d@�ff@�=q@�@͙�@�`B@�J@�dZ@��@�J@�I�@��@��@�%@�`B@���@�$�@��-@�Z@���@���@�p�@�J@��!@��/@��H@���@�Ĝ@�^5@���@�b@�@��7@�G�@��/@��@���@�O�@� �@���@�x�@� �@�n�@���@��@�Q�@�;d@��+@���@���@~��@}`B@{t�@z�@x�@v��@v5?@t��@r-@p �@m�@lz�@kS�@j=q@i7L@g��@f��@e�-@ct�@ax�@`1'@^ȴ@\I�@Z��@W�w@V$�@T(�@R�H@QG�@O�@N��@MO�@J��@I&�@G�P@F�R@F$�@DZ@Ct�@BM�@A&�@@1'@?
=@>E�@=O�@<��@;dZ@9�7@7�P@65?@4�/@3��@2�\@1G�@0A�@/\)@.$�@-O�@,j@+"�@)��@)&�@(Q�@'��@&��@&@%�@$Z@#ƨ@"�!@"J@!X@�@�+@@�/@z�@�m@o@�@ �@�@
=@��@�T@`B@I�@��@33@�\@��@Ĝ@bN@�;@�R@�T@�@�@9X@�m@�F@33@
�H@	x�@�9@��@��@ȴ@��@$�@�@p�@��@��@�!@^5@�^@%@ b@   ?�|�?��?��?�(�?�X?�E�?���?�33?�-?�Ĝ?���?��?�dZ?���?�u?�K�?��?�9X?�33?�&�?ߝ�?�{?�I�?ۅ?��?���?�Q�?�l�?�E�?�?}?��?�o?�M�?�%?�bN?��?�V?͑h?��?�ƨ?���?���?���?���?�Q�?���?�$�?ļj?���?�S�?�-?�&�?�A�?��?�v�?�5??�p�?��?�ƨ?��?�C�?�?��H?��H?��H?�dZ?�dZ?��?���?�1?�j?��?�O�?��-?�{?��?�\)?�  ?���?�G�?�hs?�hs?��7?��7?��7?��7?��7?��7?�%?�&�?�G�?��7?��7?���?��?�JA�bA�bA�oA�oA�bA�bA�bA�oA�oA�oA�oA��A�{A�bA�bA�bA�oA�bA�{A�{A�{A�bA�{A�oA�oA�bA�bA�bA�VA�bA�
=A�
=A�
=A�%A�%A�%A�1A�
=A�1A�1A�1A�1A�1A�1A�1A�1A�
=A�JA�JA�VA�1A�%A�%A�A�%A�%A�%A�%A�A�  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                              ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�BVBVBVBPBPBPBVBVBPBPBPBVBVBVBPBVB\B\BuB�B,BZB�DB�B�B�B\B�B�B�B�B�B�B�B�B�B�B�B\BuB,B8RB)�B;dBffBk�B|�B{�By�Bx�B� B{�Br�Bq�Bv�Bv�Bu�Bw�Bw�B{�B|�B�+B�hB�{B�uB�JB�7B�1B�+B�B�B}�Bx�Bt�Bt�Be`BS�BN�B?}B�BbBB��BB��B�B�sB�/BĜB�-B��B�{B�PB� Bl�BO�B/B �B�BPBB
��B
�B
�B
��B
�wB
�B
��B
�\B
�%B
s�B
dZB
YB
N�B
>wB
%�B
oB	��B	��B	B	�B	��B	�B	z�B	s�B	m�B	e`B	YB	I�B	8RB	!�B	�B	�B	�B	�B	6FB	C�B	F�B	J�B	J�B	H�B	J�B	K�B	H�B	J�B	I�B	G�B	G�B	E�B	C�B	6FB	0!B	,B	)�B	+B	+B	)�B	$�B	hB	JB		7B	B	B	  B��B��B��B�B�fB�ZB�NB�;B�5B�/B�)B�B��B��B��B�^B�?B�!B��B��B��B�+Bx�Bq�Bp�Bo�BiyBk�Bo�Br�Br�Bn�Bp�Bm�BjBiyBiyBhsBcTBhsB\)BYBS�BR�BO�BN�BL�B]/B_;BYBT�BI�B@�B;dB:^B9XB8RB2-B5?B0!B1'B/B.B2-B5?B0!B/B7LB9XB7LB33B/B/B-B)�B(�B(�B)�B+B,B%�B+B'�B)�B)�B(�B)�B)�B)�B)�B,B-B-B-B8RB6FB=qBD�BN�B^5BdZBk�Br�By�B�B��B��B��B�RB��B��B�/B�NB��B	�B	%B	8RB	A�B	<jB	D�B	bNB	m�B	�+B	�PB	��B	��B	��B	�B	�XB	�qB	B	��B	��B	�B	�5B	�BB	�NB	�fB	�B	�B	�B	�B	��B	��B	��B	��B
B
B
B
%B
+B
DB
VB
hB
hB
uB
{B
�B
�B
�B
�B
�B
�B
�B
#�B
#�B
&�B
'�B
+B
-B
-B
.B
0!B
1'B
5?B
6FB
7LB
8RB
9XB
;dB
;dB
<jB
=qB
>wB
?}B
@�B
A�B
B�B
C�B
E�B
E�B
G�B
H�B
I�B
J�B
L�B
M�B
M�B
N�B
O�B
P�B
Q�B
S�B
T�B
VB
VB
W
B
XB
XB
YB
YB
ZB
ZB
\)B
]/B
_;B
^5B
`BB
_;B
aHB
aHB
cTB
e`B
e`B
e`B
ffB
gmB
gmB
hsB
iyB
jB
iyB
jB
k�B
k�B
k�B
m�B
m�B
n�B
o�B
o�B
p�B
p�B
q�B
q�B
s�B
s�B
u�B
v�B
u�B
v�B
v�B
v�B
x�B
x�B
y�B
{�B
{�B
{�B
}�B
}�B
}�B
~�B
~�B
� B
� B
�B
�B
�B
�%B
�%B
�1B
�7B
�=B
�DB
�JB
�PB
�VB
�bB
�bB
�bB
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
��B
��B
�B
�B
�B
�B
�B
�!B
�'B
�'B
�-B
�-B
�9B
�?B
�?B
�?B
�FB
�FB
�LB
�FB
�LB
�RB
�RB
�RB
�XB
�RB
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
�^B
�^B
�XB
�^B
�dB
�^B
�^B
�^B
�^B
�dB
�^B
�^BVBVBVBVB\BVB\BVBVB\BVBPBVB\BVB\B\B\BPBVBPB\BVB\BVBVB\BVBVBVBVBPBVBVBPBPBPBPBVBVBPBVBPBPBPBVBVBPBVBVBVBPBPBVBPBPBPBPBPBVG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                              B-B-B.B(B(B)B/B/B*B*B*B1B1B2B-B3B:B:BTB�B+�BY�B�%B��B��B�B?B�B�B�B�B�B�B�B�B�BjBjBFB_B+�B8=B)�B;PBfSBkrB|�B{�By�Bx�B�B{�Br�Bq�Bv�Bv�Bu�Bw�Bw�B{�B|�B�!B�_B�rB�mB�BB�0B�*B�%B�B�B}�Bx�Bt�Bt�Be]BS�BN�B?|B�BaBB��BB��B�B�uB�2BğB�1B��B��B�UB�Bl�BO�B/"B �B�BXB"B
��B
�B
�B
��B
��B
�B
��B
�hB
�2B
s�B
dhB
Y%B
N�B
>�B
%�B
B	��B	��B	B	�,B	��B	�*B	z�B	s�B	m�B	esB	Y+B	I�B	8fB	!�B	�B	�B	�B	�B	6]B	C�B	F�B	J�B	J�B	H�B	J�B	K�B	H�B	J�B	I�B	G�B	G�B	E�B	C�B	6eB	0AB	,(B	*B	+#B	+$B	*B	% B	�B	nB		[B	DB	7B	 &B�B��B��B��B�B�B�wB�eB�_B�ZB�TB�IB�*B��B��B��B�mB�PB�+B�B��B�[ByBq�Bp�Bo�Bi�Bk�Bo�Br�Br�Bn�Bp�Bm�Bj�Bi�Bi�Bh�Bc�Bh�B\cBYQBT3BS-BPBOBM
B]lB_yBYUBU=BI�B@�B;�B:�B9�B8�B2oB5�B0dB1kB/_B.YB2rB5�B0gB/bB7�B9�B7�B3|B/eB/eB-YB*GB)AB)BB*HB+OB,VB&1B+PB(?B*LB*LB)GB*MB*NB*NB*OB,[B-bB-bB-cB8�B6�B=�BD�BO?B^�Bd�Bk�Bs!BzOB��B� B�(B�{B��B�QB�yBݹB��B�YB	4B	�B	8�B	B&B	=
B	E?B	b�B	n;B	��B	�B	�5B	�]B	�xB	��B	�B	�5B	�VB	͘B	��B	��B	�	B	�B	�)B	�DB	�`B	�vB	��B	�B	��B	��B	��B	��B
B
B
B
*B
3B
OB
dB
yB
|B
�B
�B
�B
�B
�B
�B
�B
�B
 �B
%B
%B
(B
))B
,>B
.MB
.OB
/XB
1hB
2qB
6�B
7�B
8�B
9�B
:�B
<�B
<�B
=�B
>�B
?�B
@�B
A�B
B�B
DB
EB
GB
G B
I/B
J8B
KAB
LKB
NZB
OdB
OgB
PpB
QyB
R�B
S�B
U�B
V�B
W�B
W�B
X�B
Y�B
Y�B
Z�B
Z�B
[�B
[�B
]�B
^�B
aB
_�B
bB
aB
cB
cB
e(B
g7B
g9B
g;B
hDB
iMB
iPB
jXB
k`B
liB
keB
lmB
mvB
mxB
m{B
o�B
o�B
p�B
q�B
q�B
r�B
r�B
s�B
s�B
u�B
u�B
w�B
x�B
w�B
x�B
x�B
x�B
z�B
z�B
|B
~B
~B
~B
�(B
�+B
�-B
�6B
�9B
�BB
�DB
�VB
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
��B
�B
�B
�'B
�,B
�@B
�EB
�PB
�eB
�cB
�oB
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
��B
�B
�B
�	B
�B
�&B
�-B
�IB
�_B
�sB
��B
��B
��B
��B
��B
��B
�B
�B
�'B
�CB
�XB
�gB
�wB
��B
��B
��B
��B
��B
��B
��B
�B
�B
�"B
�2B
�GB
�VB
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
��B-B-B-B-B3B-B3B-B-B3B-B'B-B3B-B3B3B3B'B-B'B3B-B3B-B-B4B.B.B.B.B(B.B.B(B(B(B(B.B.B(B.B)B)B)B/B/B)B/B/B/B*B*B0B*B*B*B*B*B1G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                              <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�201807242202472021061413551920210614135519202106171311562021061713115620210617131156201807242202472021061413551920210614135519202106171311562021061713115620210617131156PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            new conductivity = original conductivity * (1 + delta*TEMP + CPcor_SBE*PRES) / (1 + delta*TEMP_ADJUSTED + CPcor_new*PRES_ADJUSTED)                                                                                                                              none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            new conductivity = original conductivity * (1 + delta*TEMP + CPcor_SBE*PRES) / (1 + delta*TEMP_ADJUSTED + CPcor_new*PRES_ADJUSTED)                                                                                                                              none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.50 dbar                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            CPcor_new = -1.25e-07; CPcor_SBE = -9.57e-08; delta = 3.25e-06                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.50 dbar                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            CPcor_new = -1.25e-07; CPcor_SBE = -9.57e-08; delta = 3.25e-06                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                      none                                                                                                                                                                                                                                                            New conductivity computed by using a different CPcor value from that provided by Sea-Bird.                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            No significant salinity drift detected.                                                                                                                                                                                                                         Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                      none                                                                                                                                                                                                                                                            New conductivity computed by using a different CPcor value from that provided by Sea-Bird.                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            No significant salinity drift detected.                                                                                                                                                                                                                         PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            AO  AO  ARCAARCAADJPADJP                                                                                                                                        2018072422024720180724220247  IP  IP                                G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2018072422024720180724220247QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�5F03E           703E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2018072422024720180724220247QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�0               0               PM  PM  ARSQARSQCOWGCOWGV1.1V1.1                                                                                                                                2021061713145420210617131454IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                