CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS     N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-07-24T22:03:00Z creation      
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
_FillValue                  � Argo profile    3.1 1.2 19500101000000  20180724220300  20210722161419  5905740 5905740 US ARGO PROJECT                                                 US ARGO PROJECT                                                 GREGORY C. JOHNSON                                              GREGORY C. JOHNSON                                              PRES            TEMP            PSAL            PRES            TEMP            PSAL               	   	DD  AOAO7220                            7220                            2C  2C  DD  SOLO_D_MRV                      SOLO_D_MRV                      12003                           12003                           SBE603 01Dec16                  SBE603 01Dec16                  874 874 @�lM�DK@�lM�DK11  @�lM��@�lM��@*b#ᆘ5@*b#ᆘ5�cM�y����cM�y���11  GPS     GPS     Primary sampling: averaged [1Hz sampling by SBE-41CP averaged in 2-dbar bins]                                                                                                                                                                                   Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 AB  AA  AA  ?�  @   @@  @�  @�  @�33@�33A��A��A!��A>ffA`  A���A�  A�33A�  A���A���A�  A�  B ffBffB  BffB ��B(��B0  B7��B?33BH  BP��BX��B_��Bg33Bp  BxffB��B���B�  B�33B�  B�33B�ffB�ffB�ffB�ffB�33B�33B���B���B�  B���B�33B�33B�  B�33B���B�33B�33B�ffB���B䙚B�  B뙚B�33B���B�33B�  B���C��C33C33C��C	�fC  C  C33C  C�CffC�C��C  C33C��C"  C$33C&33C(L�C*  C,33C.33C033C2�C4�C6  C7�fC:33C<  C=�fC@  CB�CD33CF  CH�CJ�CK�3CN  CP�CQ��CS�fCV  CX33CZL�C\�C]�fC`  Cb�Cd�CfL�Ch�Ci��Ck�fCn�Cp33Cr  Cs��Cu�fCx33Cz  C{��C~  C��C��C��fC�  C�  C��C�  C��fC�  C��C�&fC��C��3C��C��C�  C��fC��3C�  C��C�&fC��C��3C�  C��C�&fC��C��3C�  C��C�&fC��C�&fC��C��fC��3C�  C��C��C��C��C�&fC�&fC��C��fC��3C�  C�  C��C��C��C��C��C��C�ٚC��fC��3C��3C��3C�  C�  C��C��C�&fC��C��fC�  C��C��3C��fC�  C��C�  C��3C��C�&fC��C�  C��C�&fC�&fC�  C��C�&fC��C��3C�  C�&fC��C��fC�  C��C�  C��fC�  C��3C��C��C��C��C�&fC�  C��fC��3C�  C�  C��C��C��C�&fC�33C��C��fC��3C��3C��3C�  C��3C��fC��3C��3C��3C��3C�  C��C��C��C��D 3D ��D3D�3Dy�D��D��D�D�3D@ D�3Dl�D�3D�fD Y�D"��D%�fD(` D+3D-ٚD0�3D3Y�D6fD8��D;�3D>@ D@�fDCl�DFfDH��DK,�DM�fDPFfDR�fDUffDW��DZ� D]3D_�3Db@ Dd��Dg,�Di��DlfDnY�Dp��Dr��DuS3Dw��Dz3D|  D~�fD��fD�� D�3D�VfD���D�� D�&fD�p D���D�fD�S3D��3D�ٚD��D�Y�D���D���D�	�D�@ D�l�D���D�ɚD��fD�	�D�)�D�I�D�c3D��3D��fD���D��fD���D�3D��D�6fD�L�D�y�D���D��3D�� D��D�S3D��3D���D��fD�33D�vfD���D��3D�0 D�p D���D���D�@ D��fD�� D�fD�VfDƌ�D�ɚD�fD�FfD�y�D̶fD��D�  D�P Dу3DҰ D�� D�&fD�L�D׀ Dذ D��3D�fD�,�D�P D�l�Dߓ3Dਗ਼D��fD�� D��fD�	�D�&fD�9�D�S3D�` D�y�D�fD�fD���D�3D��D��fD��3D��D��fD� D�#3D�@ D�Y�D�p D�y�D�� D���D���D���E �E ��E!�E�3EFfE�fEk3E  E�3E+3E�3E[3E� E�3E� E	�fE$�ET�E�3E��E�fE	�E�fE� E��E E� E��EɚEP EL�E�fE� E 6fE!��E"��E#�fE%K3E&,�E'� E(� E* E+` E,��E-��E/�E0S3E1��E2� E3�fE533E6p E7��E8�fE:4�E;��E<�3E?�fEC3EFI�EI�fELI�EO� ER� EU�EY�E\NfE_` Ebi�Eed�Eh��Ek�3En��Eq�Eu&fEx E{Q�E~NfE��fE�bfE���E�h E�  E���E�<�E��3E�JfE�� E�c3E�� E�}�E� E�� E��3E�9�E��3E���E�fE�t�E��fE�&fE�a�E�� E�3E�NfE�� E���E�T�E���E�� E�@ ?L��?L��?L��?L��?L��?L��?fff?L��?fff?�  ?fff?fff?�  ?fff?�  ?�  ?���?�  ?�ff?���?�ff?�33?�33?�  ?���?���?�ff?�33@ff@33@   @,��@9��@Fff@S33@fff@y��@�ff@���@�ff@�  @���@�ff@�33@�  @���@陚@�33A��A  A��A��A#33A)��A1��A9��AC33AI��AQ��AY��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111444441411441414141411411411111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        ?�  @   @`  @�  @�  @�33@�33A	��A��A)��AFffAh  A���A�  A�33A�  A���A���A�  A�  BffB
ffB  BffB"��B*��B2  B9��BA33BJ  BR��BZ��Ba��Bi33Br  BzffB���B���B�  B�33B�  B�33B�ffB�ffB�ffB�ffB�33B�33B���B���B�  B���B�33B�33B�  B�33B���B�33B�33B�ffBᙚB噚B�  B왚B�33B���B�33B�  C ffCL�C�3C�3CL�C
ffC� C� C�3C� C��C�fC��CL�C� C�3C L�C"� C$�3C&�3C(��C*� C,�3C.�3C0�3C2��C4��C6� C8ffC:�3C<� C>ffC@� CB��CD�3CF� CH��CJ��CL33CN� CP��CRL�CTffCV� CX�3CZ��C\��C^ffC`� Cb��Cd��Cf��Ch��CjL�ClffCn��Cp�3Cr� CtL�CvffCx�3Cz� C|L�C~� C�Y�C�L�C�&fC�@ C�@ C�L�C�@ C�&fC�@ C�Y�C�ffC�L�C�33C�L�C�Y�C�@ C�&fC�33C�@ C�Y�C�ffC�L�C�33C�@ C�Y�C�ffC�L�C�33C�@ C�L�C�ffC�Y�C�ffC�L�C�&fC�33C�@ C�L�C�L�C�Y�C�Y�C�ffC�ffC�Y�C�&fC�33C�@ C�@ C�L�C�L�C�L�C�Y�C�Y�C�L�C��C�&fC�33C�33C�33C�@ C�@ C�L�C�Y�C�ffC�L�C�&fC�@ C�L�C�33C�&fC�@ C�Y�C�@ C�33C�L�C�ffC�Y�C�@ C�L�C�ffC�ffC�@ C�L�C�ffC�L�C�33C�@ C�ffC�L�C�&fC�@ C�Y�C�@ C�&fC�@ C�33C�L�C�L�C�L�C�Y�C�ffC�@ C�&fC�33C�@ C�@ C�L�C�Y�C�Y�C�ffC�s3C�Y�C�&fC�33C�33C�33C�@ C�33C�&fC�33C�33C�33C�33C�@ C�L�C�L�C�Y�C�Y�D 33D ��D33D�3D��D	�D��D9�D�3D` D�3D��D3D�fD y�D#�D%�fD(� D+33D-��D0�3D3y�D6&fD8��D;�3D>` DAfDC��DF&fDH��DKL�DM�fDPffDR�fDU�fDX�DZ� D]33D_�3Db` DdٚDgL�Di��Dl&fDny�Dp��Ds�Dus3Dw��Dz33D|@ D~�fD��fD�� D�#3D�ffD���D�� D�6fD�� D���D�fD�c3D��3D��D�)�D�i�D���D���D��D�P D�|�D���D�ٚD��fD��D�9�D�Y�D�s3D��3D��fD�ɚD��fD���D�3D�)�D�FfD�\�D���D���D��3D�  D�,�D�c3D��3D�ɚD�fD�C3D��fD�ɚD�3D�@ D�� D�ɚD�	�D�P D��fD�� D�&fD�ffDƜ�D�ٚD�fD�VfDˉ�D��fD���D�0 D�` Dѓ3D�� D�  D�6fD�\�Dא D�� D��3D�fD�<�D�` D�|�Dߣ3D๚D��fD�� D�fD��D�6fD�I�D�c3D�p DꉚD�fD�fD���D��3D���D��fD��3D���D�fD�  D�33D�P D�i�D�� D���D�� D���D���D���E 	�E ��E)�E�3ENfE�fEs3E E�3E33E�3Ec3E� E�3E� E	�fE,�E\�E�3E��E�fE�E�fE� E�E E� EɚEњEX ET�E�fE� E >fE!��E"��E#�fE%S3E&4�E'� E(� E*  E+h E,��E-��E/!�E0[3E1��E2� E3�fE5;3E6x E7��E8�fE:<�E;��E<�3E?�fEC3EFQ�EI�fELQ�EO� ER� EU��EY$�E\VfE_h Ebq�Eel�Eh��Ek�3En��Eq�Eu.fEx E{Y�E~VfE��fE�ffE���E�l E�$ E���E�@�E��3E�NfE�� E�g3E�� E���E� E�� E��3E�=�E��3E���E�"fE�x�E��fE�*fE�e�E�� E�3E�RfE�� E��E�X�E���E�� E�D G�O�G�O�G�O�G�O�G�O�?�ffG�O�?�ff?�33G�O�G�O�?�33G�O�?�33G�O�?�  G�O�?�  G�O�?ٙ�?�ffG�O�?�33@   G�O�@ff@33@��@&ff@333@@  @L��@Y��@fff@s33@�33@���@�ff@���@�ff@�  @���@�ff@�33@�  @���@���A��A	��A  A��A!��A+33A1��A9��AA��AK33AQ��AY��Aa��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111444441411441414141411411411111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        @ �@ �@ {@ O@ ""@ )�@ 0x@ 7L@ >@ D�@ Q=@ _�@ m�@ z�@ ��@ �0@ ��@ �-@ �&@ ��@ �#@ ��@ ��@@o@ @,`@9X@FQ@UU@dZ@r@}�@��@��@��@�9@�2@�7@ލ@�@��@1@�@#�@1'@>@K�@X@e�@t@�@�@��@��@�@�J@�O@��@�L@��@J@�@$.@3�@B�@O0@\)@i!@v@�|@�#@�@�f@��@�c@�h@�@�@�@V@�@(�@7�@B�@Q�@`�@n�@|�@��@��@�5@��@��@�|@�t@�m@� @j@b@�@-@;d@G�@V@c�@n�@~K@��@�<@��@��@��@�C@ލ@��@�,@�@*@$.@0x@<@Ji@Yn@g�@t@�W@��@�a@��@��@��@��@��@�@��@
=@�@%�@1�@@�@O�@^5@j@v�@��@�$@�m@��@�@�c@�h@�@�@�Q@�@�@+@7L@C�@Q�@`B@oF@|?@��@��@�y@��@�&@�|@�#@�y@� @	v@	@	 @	+@	9X@	G�@	UU@	c�@	qS@	~�@	�P@	��@	��@	��@	�2@	ψ@	�/@	��@	�,@
�@
*@
#�@
1�@
>@
I�@
X�@
g@
s_@
�W@
�\@
�a@
��@
��@
ƨ@
խ@
�@
��@
�E@J@�@%�@3�@B�@O0@[z@i�@y�@��@�h@�m@�r@��@�@�
@�@�@ �@V@�@+@6�@B�@Q=@_�@m:@{�@��@��@��@�9@��@�o@��@�m@�@j@b@[@+�@9X@F�@T�@b�@qS@~�@�P@��@�M@�F@Ĝ@�C@X@�U@�@(G@oF@��@�9@B8@�+@��@�@bN@�Y@��@?}@�D@խ@!s@j@�F@@K@�u@�h@g@dZ@�@�@7L@{�@�>@	�@N�@��@܀@""@e�@��@��@-@l�@��@��@+@k.@��@�@)�@n�@�~@�q@;d@�@��@�@Q�@��@�;@&;@j@�!@�e@8�@|�@��@ @ DD@ �p@ Ĝ@!v@!B8@!�W@!��@!�9@"7L@"t�@"��@"�@#*S@#e�@#�@#܀@$B@$T�@$��@$��@%�@%Q�@%�@%�O@&*@&Wb@&��@&ލ@'#�@'hs@'�Y@'��@(33@(y�@(��@)j@)I@)�\@)��@*B@*[z@*�@*�@+&�@+hs@+�@+�@,/�@,p�@,�-@,�Y@-6�@-x�@-��@-�,@.:@.{�@.��@.�~@/6�@/s_@/�-@/�@0*S@0ff@0��@0܀@1B@1S�@1�@1�c@2v@2>�@2x�@2��@2�4@3$�@3]�@3��@3�C@4�@4G�@4�d@4�&@4�9@56�@5oF@5��@5�@6!s@6^�@6��@6�#@7�@7V�@7��@7��@8o@8Q�@8��@8є@9o@9SI@9��@9��@:X�@:�/@;^5@;��@<`�@<�T@=bN@=��@>�T@?�@?�i@@�@@�R@A-@A��@BDD@B�!@CQ=@C��@D[z@D�e@EZ�@E�@F�|@F�@Gww@H�@H��@I�@I�M@J1�@J��@K;d@K��@LDD@Lȴ@MO�@M�
@N`B@N�l@Or�@P �@P�P@Q��@S;d@T�I@U��@W)�@X�\@Y�@[H]@\�z@]�Q@_N�@`�I@a��@cB�@d��@e�@g3�@h��@iӠ@k:�@l�@m��@oB�@p�7@q�
@sN�@t�$@u�q@w(G@x�h@y�H@{5�@|ww@}��@33@�=q@�`�@���@��S@���@��l@�J@�2�@�X@�qS@���@��@��[@���@�!@�FQ@�`B@��@���G�O�G�O�G�O�G�O�G�O�@ vG�O�@ v@ %G�O�G�O�@ %G�O�@ %G�O�@ �G�O�@ �G�O�@ 1@ �G�O�@ 	�@ 
=G�O�@ 
�@ J@ �@ V@ �@ @ o@ �@ *@ �@ �@ �@ �@ 
@  @ ""@ $.@ &�@ )�@ ,`@ /@ 1�@ 3�@ 7L@ :@ >@ A�@ E�@ H]@ K�@ O0@ SI@ V@ Yn@ \�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�AہA�r�A�v�AۍPAۏ\AۑhAۙ�Aۛ�Aۛ�Aۛ�Aۡ�Aۡ�Aۧ�Aۧ�A۩�AۓuAۑhA�p�A�K�A�1A���A�ȴA�ĜAڼjAڶFAڮAڥ�Aڣ�Aڥ�Aڣ�Aڥ�Aڥ�Aڣ�Aڟ�Aڝ�Aڝ�Aڛ�Aڛ�Aڕ�AڑhA�~�A�O�A�jA��A�`BA�1'A�x�A��A��A���A�z�A�{A���A�VA�"�A�t�A��wA��hA���A���A�VA�ĜA�`BA���A�+A��;A��^A��A�~�A�n�A�O�A�{A�XA���A��+A�;dAz1'Ax1Au�hApI�Al�uAg�Ab^5A]33AZZAVv�AT�DAS�AQ��AP�jAP�+AL��AJ�AIoAFv�AC�A@�A?dZA?G�A?C�A?�A<bNA;;dA8�yA5�mA4A4bA4ffA4�!A4^5A4�DA4z�A3�TA1�-A1�A0��A0n�A0bA/�A/|�A.�A.ĜA.E�A-x�A,�/A,$�A+��A+/A*=qA)�-A)XA(��A(VA(=qA(1'A'�A'�A&�+A%ƨA%XA$��A$n�A#��A#
=A"�\A!��A!\)A!VA ��A ffA��A�A$�AS�A��A��A��At�A��AĜA�A=qA�A33A��AffAE�A��A�yA�uAM�A{A�;A�A�A��A�DA-A�Ax�A;dA��A�A��A�/A��A�DA=qA(�A�Al�A��A�!A~�A��A�PA\)A&�A��A��A�\Av�AZAE�A  A�-Ax�A/A
��A	�TA	�-A	�7A	dZA	A��A �A�;A��AoA��AI�A�#A��Ax�A;dA
=A�AZA�A�TA��A�`An�AE�A(�A�A+A ��A j@���@��@�^5@�$�@��T@�hs@��/@��
@�@���@��@��@���@�Ĝ@��;@�|�@�"�@���@�-@��@�K�@�-@���@�j@�l�@�`B@�O�@�@���@ޟ�@�b@ّh@���@�?}@�J@�;d@̋D@�~�@��;@�&�@��H@��#@�|�@���@�ƨ@���@�z�@��#@���@�"�@�E�@���@�K�@�O�@�1@���@��@�K�@�$�@�9X@�@���@��#@��y@��@�(�@��H@�&�@��w@�V@�%@���@���@���@�  @��@���@��w@���@�X@�1@��@�$�@���@�(�@}��@}/@{C�@zJ@x1'@vv�@t��@q�^@p �@m�-@k��@j�@h �@g�@e/@ct�@b~�@`bN@_;d@^@[�@Y&�@Vff@U@T�D@S@Q��@O�w@NE�@L�@Kƨ@J-@H�@GK�@FE�@E`B@Co@A��@@Q�@>ȴ@=p�@<z�@;t�@:�@:�@9��@8 �@7+@5��@4�D@3t�@17L@/�;@.E�@-�@+"�@*^5@)hs@(bN@'�P@&V@%�h@$�@"�!@!�#@!�@ b@K�@V@p�@��@��@��@��@��@  @�@��@�D@�
@@�\@�@��@�@�P@�R@�T@V@�@�@S�@
^5@	X@��@r�@�@|�@
=@��@��@��@�@/@�@Z@��@��@@��@�^@G�@ bN?���?�\)?��h?��?�^5?�x�?�l�?�ff?��j?��
?�-?��?��?�{?��-?�I�?�ƨ?ꟾ?���?�x�?�1'?��?��T?��?�z�?�t�?���?���?��?߾w?�V?ݑh?�V?�j?�C�?�=q?��?�Q�?�K�?�ff?ա�?�t�?�J?�Ĝ?�;d?͑h?��?�dZ?��?ə�?ȴ9?ǍP?Ƨ�?��?��?�G�?��;?���?��?���?�ƨ?��H?���?���?�Q�?�b?�
=?���?��T?���?���?�9X?��F?��?��?�-?��?��?�J?�-?�M�?�n�?�n�?��\?�n�?�n�?��\?��\?�n�?��\?��!?��!?���?���?�oAۉ7AۍPA�|�AۑhAۑhAۍPAۋDAۏ\AۍPAۋDAۉ7A�z�A�v�A�|�A�v�A�t�A�v�A�v�A�v�A�t�A�t�A�v�A�r�A�t�A�t�A�r�A�n�A�p�A�n�A�r�A�t�A�t�A�t�A�x�A�z�AۃAۍPAۓuAۑhAۏ\Aۏ\AۍPAۏ\Aۏ\AۓuAۗ�Aۛ�Aۛ�A۝�Aۛ�Aۛ�Aۛ�Aۛ�Aۛ�A۝�Aۣ�Aۡ�Aۥ�Aۧ�Aۥ�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        AہA�r�A�v�AۍPAۏ\AۑhAۙ�Aۛ�Aۛ�Aۛ�Aۡ�Aۡ�Aۧ�Aۧ�A۩�AۓuAۑhA�p�A�K�A�1A���A�ȴA�ĜAڼjAڶFAڮAڥ�Aڣ�Aڥ�Aڣ�Aڥ�Aڥ�Aڣ�Aڟ�Aڝ�Aڝ�Aڛ�Aڛ�Aڕ�AڑhA�~�A�O�A�jA��A�`BA�1'A�x�A��A��A���A�z�A�{A���A�VA�"�A�t�A��wA��hA���A���A�VA�ĜA�`BA���A�+A��;A��^A��A�~�A�n�A�O�A�{A�XA���A��+A�;dAz1'Ax1Au�hApI�Al�uAg�Ab^5A]33AZZAVv�AT�DAS�AQ��AP�jAP�+AL��AJ�AIoAFv�AC�A@�A?dZA?G�A?C�A?�A<bNA;;dA8�yA5�mA4A4bA4ffA4�!A4^5A4�DA4z�A3�TA1�-A1�A0��A0n�A0bA/�A/|�A.�A.ĜA.E�A-x�A,�/A,$�A+��A+/A*=qA)�-A)XA(��A(VA(=qA(1'A'�A'�A&�+A%ƨA%XA$��A$n�A#��A#
=A"�\A!��A!\)A!VA ��A ffA��A�A$�AS�A��A��A��At�A��AĜA�A=qA�A33A��AffAE�A��A�yA�uAM�A{A�;A�A�A��A�DA-A�Ax�A;dA��A�A��A�/A��A�DA=qA(�A�Al�A��A�!A~�A��A�PA\)A&�A��A��A�\Av�AZAE�A  A�-Ax�A/A
��A	�TA	�-A	�7A	dZA	A��A �A�;A��AoA��AI�A�#A��Ax�A;dA
=A�AZA�A�TA��A�`An�AE�A(�A�A+A ��A j@���@��@�^5@�$�@��T@�hs@��/@��
@�@���@��@��@���@�Ĝ@��;@�|�@�"�@���@�-@��@�K�@�-@���@�j@�l�@�`B@�O�@�@���@ޟ�@�b@ّh@���@�?}@�J@�;d@̋D@�~�@��;@�&�@��H@��#@�|�@���@�ƨ@���@�z�@��#@���@�"�@�E�@���@�K�@�O�@�1@���@��@�K�@�$�@�9X@�@���@��#@��y@��@�(�@��H@�&�@��w@�V@�%@���@���@���@�  @��@���@��w@���@�X@�1@��@�$�@���@�(�@}��@}/@{C�@zJ@x1'@vv�@t��@q�^@p �@m�-@k��@j�@h �@g�@e/@ct�@b~�@`bN@_;d@^@[�@Y&�@Vff@U@T�D@S@Q��@O�w@NE�@L�@Kƨ@J-@H�@GK�@FE�@E`B@Co@A��@@Q�@>ȴ@=p�@<z�@;t�@:�@:�@9��@8 �@7+@5��@4�D@3t�@17L@/�;@.E�@-�@+"�@*^5@)hs@(bN@'�P@&V@%�h@$�@"�!@!�#@!�@ b@K�@V@p�@��@��@��@��@��@  @�@��@�D@�
@@�\@�@��@�@�P@�R@�T@V@�@�@S�@
^5@	X@��@r�@�@|�@
=@��@��@��@�@/@�@Z@��@��@@��@�^@G�@ bN?���?�\)?��h?��?�^5?�x�?�l�?�ff?��j?��
?�-?��?��?�{?��-?�I�?�ƨ?ꟾ?���?�x�?�1'?��?��T?��?�z�?�t�?���?���?��?߾w?�V?ݑh?�V?�j?�C�?�=q?��?�Q�?�K�?�ff?ա�?�t�?�J?�Ĝ?�;d?͑h?��?�dZ?��?ə�?ȴ9?ǍP?Ƨ�?��?��?�G�?��;?���?��?���?�ƨ?��H?���?���?�Q�?�b?�
=?���?��T?���?���?�9X?��F?��?��?�-?��?��?�J?�-?�M�?�n�?�n�?��\?�n�?�n�?��\?��\?�n�?��\?��!?��!?���?���?�oAۉ7AۍPA�|�AۑhAۑhAۍPAۋDAۏ\AۍPAۋDAۉ7A�z�A�v�A�|�A�v�A�t�A�v�A�v�A�v�A�t�A�t�A�v�A�r�A�t�A�t�A�r�A�n�A�p�A�n�A�r�A�t�A�t�A�t�A�x�A�z�AۃAۍPAۓuAۑhAۏ\Aۏ\AۍPAۏ\Aۏ\AۓuAۗ�Aۛ�Aۛ�A۝�Aۛ�Aۛ�Aۛ�Aۛ�Aۛ�A۝�Aۣ�Aۡ�Aۥ�Aۧ�Aۥ�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�'B	�9B	�XB	�qB	�}B	��B	��B	�}B	�wB	�}B	�}B	�}B	�}B	��B	��B	��B	��B	��B	��B	��B	��B	�}B	��B	�qB	�bB	&�B	�1B	�TB
�B
M�B
�bB
��B
�LB
�B
�B
��B
��B0!B�BVB
�B
�B
��B
��B{B49B;dB0!B�B1B
��B
ffB
%�B	��B	�;B	�qB	�{B	gmB	G�B	B�B	2-B	,B	oB�B�`BÖBBŢBÖB�}B��BǮB��B��B	hB	JB	B��B��B	B	hB	�B	49B	s�B	��B	��B	��B	�uB	�uB	��B	��B	ȴB	��B	�HB	�B	��B
B
VB
�B
!�B
'�B
,B
7LB
A�B
C�B
C�B
C�B
F�B
I�B
J�B
I�B
K�B
M�B
L�B
O�B
P�B
O�B
O�B
O�B
R�B
R�B
R�B
Q�B
Q�B
S�B
P�B
P�B
O�B
P�B
R�B
Q�B
R�B
R�B
P�B
R�B
Q�B
S�B
T�B
R�B
T�B
T�B
VB
YB
XB
VB
VB
R�B
P�B
O�B
N�B
K�B
J�B
L�B
L�B
J�B
K�B
M�B
L�B
L�B
L�B
L�B
M�B
N�B
M�B
I�B
G�B
G�B
C�B
H�B
K�B
J�B
K�B
J�B
H�B
F�B
F�B
F�B
G�B
H�B
H�B
G�B
G�B
G�B
G�B
G�B
H�B
H�B
G�B
H�B
H�B
H�B
D�B
D�B
C�B
B�B
B�B
?}B
>wB
=qB
=qB
;dB
:^B
:^B
8RB
8RB
8RB
8RB
7LB
7LB
5?B
49B
5?B
49B
33B
1'B
33B
6FB
7LB
6FB
49B
49B
49B
49B
2-B
2-B
2-B
2-B
1'B
1'B
/B
0!B
0!B
0!B
0!B
.B
.B
-B
,B
,B
+B
)�B
)�B
&�B
$�B
$�B
$�B
 �B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
{B
{B
�B
{B
uB
oB
uB
bB
VB
bB
\B
bB
bB
{B
�B
�B
�B
�B
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
 �B
!�B
 �B
#�B
$�B
%�B
'�B
'�B
)�B
+B
-B
,B
.B
1'B
1'B
2-B
49B
6FB
7LB
8RB
9XB
:^B
<jB
;dB
>wB
>wB
?}B
@�B
@�B
B�B
C�B
E�B
G�B
G�B
H�B
I�B
K�B
K�B
K�B
N�B
N�B
N�B
Q�B
R�B
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
ZB
[#B
\)B
]/B
]/B
_;B
_;B
`BB
aHB
bNB
bNB
bNB
dZB
dZB
dZB
e`B
ffB
gmB
hsB
iyB
iyB
k�B
k�B
m�B
m�B
o�B
o�B
p�B
q�B
q�B
s�B
s�B
t�B
t�B
u�B
u�B
v�B
v�B
x�B
y�B
y�B
z�B
{�B
{�B
}�B
}�B
� B
�B
�B
�B
�B
�B
�B
�B
�B
�%B
�+B
�+B
�7B
�7B
�7B
�7B
�7B
�=B
�=B
�=B
�JB
�=B
�JB
�PB
�PB
�PB
�VB
�VB
�VB
�VB
�bB
�bB
�bB
�hB
�oB
�hB
�uB
�{B
�{B
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
��B
��B
��B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�!B
�'B
�-B
�-B
�'B
�3B
�9B
�3B
�9B
�FB
�?B
�FB
�LB
�LB
�RB
�XB
�XB
�XB
�dB
�dB
�jB
�jB
�wB
�qB
�}B
�}B
��B
��B
��B
��B
B
B
ÖB
ÖB
ŢB
ĜB
ĜB
ĜB
ĜB
ĜB
ĜB
ĜB
ĜB
ĜB
ĜB
ĜB
ŢB
ĜB
ĜB
ŢB
ĜB
ŢB
ŢB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	�B	�B	�B	�B	�B	�B	��B	�B	�B	�B	��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�	B	�B	�;B	�TB	�aB	�hB	�hB	�cB	�]B	�dB	�dB	�eB	�fB	�lB	�mB	�sB	�nB	�oB	�oB	�pB	�pB	�kB	�rB	�`B	�RB	&�B	� B	�CB
�B
M�B
�RB
��B
�=B
�B
��B
��B
��B0B�BJB
�B
�B
��B
��BpB4/B;ZB0BxB(B
��B
f]B
%�B	��B	�2B	�iB	�sB	geB	G�B	B�B	2%B	,B	hB�B�YBÐBBŜBÐB�xB��BǪB��B��B	eB	GB	B��B��B	B	gB	�B	49B	s�B	��B	��B	��B	�xB	�xB	��B	��B	ȹB	��B	�NB	�B	��B
!B
^B
�B
!�B
'�B
,B
7WB
A�B
C�B
C�B
C�B
F�B
I�B
J�B
I�B
K�B
M�B
L�B
O�B
P�B
O�B
O�B
O�B
SB
SB
SB
RB
RB
TB
P�B
P�B
O�B
P�B
SB
RB
SB
SB
QB
SB
R	B
TB
UB
SB
UB
UB
V$B
Y8B
X1B
V&B
V'B
SB
Q	B
PB
N�B
K�B
J�B
L�B
L�B
J�B
K�B
M�B
L�B
L�B
L�B
L�B
M�B
OB
N B
I�B
G�B
G�B
C�B
H�B
K�B
J�B
K�B
J�B
H�B
F�B
F�B
F�B
G�B
H�B
H�B
G�B
G�B
G�B
G�B
G�B
H�B
H�B
G�B
H�B
H�B
H�B
D�B
D�B
C�B
B�B
B�B
?�B
>�B
=�B
=�B
;�B
:�B
:�B
8�B
8�B
8�B
8�B
7�B
7�B
5�B
4�B
5�B
4�B
3|B
1qB
3}B
6�B
7�B
6�B
4�B
4�B
4�B
4�B
2|B
2|B
2}B
2}B
1xB
1xB
/mB
0sB
0tB
0uB
0uB
.iB
.iB
-dB
,^B
,_B
+ZB
*TB
*UB
'BB
%7B
%7B
%8B
! B
 B
B
B
B
 B
�B
 B
�B
�B
B
�B
�B
B
B
 B
�B
B
�B
�B
�B
�B
B
B
"B
7B
GB
JB
FB
1B
4B
CB
LB
UB
RB
bB
qB
�B
 �B
!�B
"�B
!�B
$�B
%�B
&�B
(�B
(�B
*�B
+�B
-�B
,�B
/	B
2B
2"B
3+B
5:B
7JB
8SB
9\B
:eB
;nB
=}B
<zB
?�B
?�B
@�B
A�B
A�B
C�B
D�B
F�B
H�B
H�B
I�B
J�B
MB
MB
MB
PB
PB
P B
S6B
T>B
VMB
VPB
WXB
XaB
XdB
YmB
ZwB
[B
[�B
\�B
]�B
^�B
^�B
`�B
`�B
a�B
b�B
c�B
c�B
c�B
e�B
e�B
e�B
f�B
g�B
iB
jB
kB
kB
m)B
m,B
o;B
o=B
qMB
qPB
rYB
sbB
seB
utB
uwB
vB
v�B
w�B
w�B
x�B
x�B
z�B
{�B
{�B
|�B
}�B
}�B
�B
�B
��B
��B
��B
�B
�B
�B
�
B
�B
�B
�%B
�-B
�0B
�>B
�AB
�DB
�FB
�IB
�QB
�TB
�WB
�fB
�\B
�lB
�uB
�wB
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
��B
��B
��B
��B
��B
��B
�B
�B
�B
�*B
�8B
�IB
�TB
�\B
�`B
�nB
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
��B
�B
�B
�B
�B
�0B
�0B
�6B
�BB
�HB
�TB
�[B
�qB
��B
��B
��B
��B
��B
��B
��B
�B
�B
�5B
�=B
�SB
�iB
�xB
��B
��B
��B
��B
��B
��B
�B
�B
�,B
�5B
�RB
�aB
�vB
ńB
ŔB
ƩB
ǿB
��B
��B
��B
�B
�B
�B
�B
�B
�B
�B
�B
�"B
�$B
�(B
�+B
�3B
�1B
�4B
�=B
�:B
�CB
�FB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�201807242203002021061413572720210614135727202107221611282021072216112820210722161128201807242203002021061413572720210614135727202107221611282021072216112820210722161128PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            new conductivity = original conductivity * (1 + delta*TEMP + CPcor_SBE*PRES) / (1 + delta*TEMP_ADJUSTED + CPcor_new*PRES_ADJUSTED)                                                                                                                              none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            new conductivity = original conductivity * (1 + delta*TEMP + CPcor_SBE*PRES) / (1 + delta*TEMP_ADJUSTED + CPcor_new*PRES_ADJUSTED)                                                                                                                              none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.50 dbar                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            CPcor_new = -1.25e-07; CPcor_SBE = -9.57e-08; delta = 3.25e-06                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.50 dbar                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            CPcor_new = -1.25e-07; CPcor_SBE = -9.57e-08; delta = 3.25e-06                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                      none                                                                                                                                                                                                                                                            New conductivity computed by using a different CPcor value from that provided by Sea-Bird.                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            No significant salinity drift detected.                                                                                                                                                                                                                         Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                      none                                                                                                                                                                                                                                                            New conductivity computed by using a different CPcor value from that provided by Sea-Bird.                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            No significant salinity drift detected.                                                                                                                                                                                                                         PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            AO  AO  ARCAARCAADJPADJP                                                                                                                                        2018072422030020180724220300  IP  IP                                G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2018072422030020180724220300QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�5F03E           703E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2018072422030020180724220300QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�0               0               PM  PM  ARSQARSQCOWGCOWGV1.1V1.1                                                                                                                                2021072216141920210722161419IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                