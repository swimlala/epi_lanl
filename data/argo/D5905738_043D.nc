CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS      N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-11-17T18:00:30Z creation      
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
resolution        =���   axis      Z           ;�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 @  L�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���        Q    PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 @  b    PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���        f`   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o        w`   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 @  �`   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o        ��   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 @  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o        ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o        ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 @  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o        �    PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 @  �    PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o        �`   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                   �   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                   �   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                   �   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                   �   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  � �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                   h   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                   �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar        �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar        �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�       �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  �`   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                      	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �       � Argo profile    3.1 1.2 19500101000000  20181117180030  20210722160155  5905738 5905738 US ARGO PROJECT                                                 US ARGO PROJECT                                                 GREGORY C. JOHNSON                                              GREGORY C. JOHNSON                                              PRES            TEMP            PSAL            PRES            TEMP            PSAL               +   +DD  AOAO7218                            7218                            2C  2C  DD  SOLO_D_MRV                      SOLO_D_MRV                      12001                           12001                           SBE603 01Dec16                  SBE603 01Dec16                  874 874 @؏\Y�Y@؏\Y�Y11  @؏[�`@؏[�`@5�&����@5�&�����c��Kr��c��Kr�11  GPS     GPS     Primary sampling: averaged [1Hz sampling by SBE-41CP averaged in 2-dbar bins]                                                                                                                                                                                   Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 AB  AA  AA  ?   ?L��@   @@  @�  @���@���@�  A   A  A&ffAA��Aa��A���A���A���A���A�  A�  A���A���B33B	33B��B��B ��B(��B0ffB8  B@ffBH��BPffBW��B`ffBhffBpffBx��B�  B�  B���B���B�  B�33B�33B�33B�33B�  B�  B�  B�33B�33B�33B�  B�33B�ffB���B���B���Bә�B�  B�33B�33B�  B���B뙚B�ffB�ffB�  B���C L�C�C33C�C  C	�fC33C�C  C  C  C33C33C�C�C  C   C!�fC$33C&  C'�fC*�C,�C.  C0L�C233C4�C6�C7�fC:�C<L�C>33C@�CB  CC��CF  CHL�CJ�CK�fCN�CPL�CR�CS�fCV  CXL�CZ�C[�fC^�C`  Ca��Cd�Cf  Cg�fCjL�Cl33Cm�fCp33Cr�Ct  Cv33Cx�Cz  C|33C~33C�  C�  C��3C��C��C�  C�  C��3C��3C��fC��C��C�  C��3C��3C��C��C��3C��C��3C��3C�  C��C��C�  C��C��C�  C��C�&fC�  C��fC��3C�  C��C��C��C�&fC�  C��fC��3C�  C��C�&fC��C��fC��3C�  C��C��C��C��C�&fC�&fC��C��fC�  C��C�  C��3C�  C��C�  C��fC��C��C��C��3C�  C��C��C��fC�  C��C�  C��fC��C�  C��3C��C��C��C��fC�  C��C��C��3C�  C�&fC��C��fC�  C��C�  C��fC�  C��C��C�&fC�  C��fC��fC��3C��3C�  C��C��C�  C�  C��C��fC��fC�ٚC�ٚC��C�&fC�33C�&fC�&fC�&fC�&fC��C��3C��C��C�  C��3C��fD �D ��DfDy�D&fD� D33D� D` DfD��DffD�D��D � D#&fD%��D(33D*��D-L�D/�3D2L�D4�3D7Y�D9ٚD<` D>� DAffDC�3DFy�DI�DK�fDN33DP� DS� DV&fDX�fD[ffD^fD`�3Dc  De��Dh9�Dj� DmFfDo�3Dr,�Dt� Dw�DyffD{ffD}�3D�  D�FfD�p D���D��fD��3D�fD�@ D�c3D��fD�� D��fD�  D�33D�\�D���D���D�� D��3D�3D�9�D�` D���D���D��D�  D�Y�D��fD��3D��fD�fD�C3D�c3D���D��fD��3D���D���D�#3D�L�D�i�D��3D�� D��3D�� D��fD�� D��3D�3D��D��D�)�D�33D�6fD�<�D�FfD�FfD�P D�` D�s3D�|�DČ�DŜ�DƬ�DǼ�D�� D�� D�� D�fD�#3D�33D�@ D�L�D�\�D�l�D�|�DԌ�DՖfD֣3Dש�Dذ Dٹ�D�� D��fD��3D��3D�� D���D�	�D��D�,�D�<�D�S3D�ffD�y�D艚D陚D�fD빚D�ɚD��3D�ٚD�� D��3D���D�	�D�fD��D�)�D�,�D�33D�6fD�<�D�)�D�,�D�)�D�)�D�33E 3E �fE�E��EfE�E�3EX E�fE�fE�fE
33E�3E��E4�EfE{3EٚE0 E E` E� E�fE9�E��E��E3Ea�E�fE ��E!� E#$�E$x E%��E'4�E(3E)q�E*�3E,+3E- E.h E/��E1+3E23E3t�E4�fE6 E7a�E8� E9�fE;fE<I�E?q�EB�3EE��EH� EK� ENٚERfEU	�EX� E[�fE^��Ea��Ed�3EhfEk( En4�EqfEt� Ew�3Ez{3E}�fE�a�E�� E��E�d�E�� E��E�@�E���E�� E�9�E��3E�� E�73E�y�E��fE� E�s3E��fE� E�e�E�� E�  E�<�E���E�� E�33E���E�� E��E�vfE�� E�3E�e�E���E���E�P�E��fE���E�K3E���?   ?   ?��?��?��?��?333?��?333?333?333?333?L��?L��?L��?L��?���?���?�ff?�  ?ٙ�?�33@ff@��@&ff@9��@Fff@Y��@s33@y��@���@�33@���@���@�33@���@ə�@�33@�33@���@���A33A33A  AffA   A&ffA.ffA6ffA<��AD��AL��AS33A[33Ac33Ai��Ap  Ax  A�  A�33G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111414441414441444141111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    ?�  ?�ff@   @`  @�  @���@���@�  A  A  A.ffAI��Ai��A���A���A���A���A�  A�  A���A���B33B33B��B��B"��B*��B2ffB:  BBffBJ��BRffBY��BbffBjffBrffBz��B�  B�  B���B���B�  B�33B�33B�33B�33B�  B�  B�  B�33B�33B�33B�  B�33B�ffB���B���B���Bԙ�B�  B�33B�33B�  B���B왚B�ffB�ffB�  B���C ��C��C�3C��C� C
ffC�3C��C� C� C� C�3C�3C��C��C� C � C"ffC$�3C&� C(ffC*��C,��C.� C0��C2�3C4��C6��C8ffC:��C<��C>�3C@��CB� CDL�CF� CH��CJ��CLffCN��CP��CR��CTffCV� CX��CZ��C\ffC^��C`� CbL�Cd��Cf� ChffCj��Cl�3CnffCp�3Cr��Ct� Cv�3Cx��Cz� C|�3C~�3C�@ C�@ C�33C�Y�C�L�C�@ C�@ C�33C�33C�&fC�L�C�L�C�@ C�33C�33C�Y�C�L�C�33C�L�C�33C�33C�@ C�Y�C�Y�C�@ C�Y�C�L�C�@ C�L�C�ffC�@ C�&fC�33C�@ C�L�C�Y�C�Y�C�ffC�@ C�&fC�33C�@ C�L�C�ffC�L�C�&fC�33C�@ C�L�C�Y�C�Y�C�Y�C�ffC�ffC�Y�C�&fC�@ C�L�C�@ C�33C�@ C�Y�C�@ C�&fC�L�C�Y�C�L�C�33C�@ C�Y�C�L�C�&fC�@ C�Y�C�@ C�&fC�L�C�@ C�33C�L�C�Y�C�L�C�&fC�@ C�Y�C�L�C�33C�@ C�ffC�L�C�&fC�@ C�Y�C�@ C�&fC�@ C�L�C�Y�C�ffC�@ C�&fC�&fC�33C�33C�@ C�L�C�Y�C�@ C�@ C�L�C�&fC�&fC��C��C�L�C�ffC�s3C�ffC�ffC�ffC�ffC�Y�C�33C�Y�C�Y�C�@ C�33C�&fD 9�D ��D&fD��DFfD� DS3D� D� D&fDٚD�fD9�D��D � D#FfD%ٚD(S3D*ٚD-l�D/�3D2l�D4�3D7y�D9��D<� D?  DA�fDD3DF��DI9�DK�fDNS3DQ  DS� DVFfDX�fD[�fD^&fD`�3Dc@ De��DhY�Dj� DmffDo�3DrL�Dt� Dw,�Dy�fD{�fD}�3D�0 D�VfD�� D���D��fD�3D�&fD�P D�s3D��fD�� D��fD� D�C3D�l�D���D���D�� D�3D�#3D�I�D�p D���D�ɚD���D�0 D�i�D��fD��3D��fD�&fD�S3D�s3D���D��fD��3D���D��D�33D�\�D�y�D��3D�� D��3D�� D��fD�� D�3D�3D��D�)�D�9�D�C3D�FfD�L�D�VfD�VfD�` D�p D3DÌ�DĜ�DŬ�DƼ�D���D�� D�� D�  D�fD�33D�C3D�P D�\�D�l�D�|�Dӌ�DԜ�DզfDֳ3D׹�D�� D�ɚD�� D��fD��3D��3D�  D��D��D�)�D�<�D�L�D�c3D�vfD牚D虚D驚D�fD�ɚD�ٚD��3D��D�� D�3D�	�D��D�&fD�,�D�9�D�<�D�C3D�FfD�L�D�9�D�<�D�9�D�9�D�C3E #3E �fE$�E��EfE�E3E` E�fE�fE�fE
;3E�3E��E<�E&fE�3E�E8 E  Eh E� E�fEA�E��E��E#3Ei�E�fE ��E!� E#,�E$� E%��E'<�E(#3E)y�E*�3E,33E- E.p E/��E133E2#3E3|�E4�fE6  E7i�E8� E9�fE;fE<Q�E?y�EB�3EE��EH� EK� EN�ERfEU�EX� E[�fE^��Ea��Ed�3EhfEk0 En<�Eq&fEt� Ew�3Ez�3E}�fE�e�E�� E��E�h�E�� E�	�E�D�E���E�� E�=�E��3E�� E�;3E�}�E��fE� E�w3E��fE� E�i�E�� E� E�@�E���E�� E�73E���E�� E� �E�zfE�� E�3E�i�E���E� �E�T�E��fE���E�O3E���G�O�?�  G�O�G�O�G�O�?���G�O�?���G�O�G�O�G�O�?���G�O�G�O�G�O�?�ffG�O�?���?�ff@   @��@��@&ff@9��@Fff@Y��@fff@y��@���@���@���@�33@���@���@�33@���@ٙ�@�33@�33@���A��A33A33A  AffA(  A.ffA6ffA>ffAD��AL��AT��A[33Ac33Ak33Aq��Ax  A�  A�  A�33G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111414441414441444141111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    @ j@ v@ �@ {@ O@ !s@ (G@ /�@ 6�@ =q@ F�@ R�@ `B@ n�@ {�@ �7@ ��@ ��@ �~@ ��@ �|@ ܀@ �(@ � @�@o@ @-@:@H]@V�@c�@o�@~�@��@�H@��@��@@ψ@�/@�@��@�@*@"�@/�@=q@K@Yn@g@t�@��@�@�a@��@��@�J@�C@�H@�@�E@
=@6@$.@4�@B8@N�@^5@k�@x&@�|@�u@�m@�f@�@�@�
@�@�Y@^@@�@)�@6�@DD@Q=@`�@m:@z3@�7@��@��@��@��@�|@�#@�m@�q@v@o@g@,`@8�@G�@Wb@c�@o�@~�@��@�H@��@��@Ĝ@��@�/@�4@�,@v@*@""@/@?}@Lu@X@g�@t�@��@��@��@��@��@�W@Ӡ@�H@�@@��@
�@�@%�@2�@@,@M$@\�@j@ww@�p@�@��@��@�@�@�[@�@�Y@^@@O@*S@7L@DD@R�@a�@m:@y�@��@�0@��@��@��@��@�t@�@�@	j@	�@	 �@	-@	8�@	F�@	UU@	c�@	r@	�@	�P@	��@	�M@	�F@	�2@	�7@	ލ@	�@	�~@
�@
�@
""@
.l@
>@
Lu@
Yn@
e�@
t@
�@
�@
��@
��@
��@
��@
�C@
��@
��@
��@
�@B@&;@1�@@�@O�@\�@i!@ww@�+@�u@�@�@�@�c@խ@�@�@^@�@O@'�@5?@C�@Q=@_�@m�@|?@��@�0@��@�!@��@��@�h@��@��@%@@ �@.l@<@I@T�@dZ@r@~K@�D@�<@��@�F@�>@ψ@O0@��@�@�@ff@��@�~@A�@�D@��@�@g@�f@��@5�@|?@�2@�@I�@��@��@�@\)@�@�@+�@s_@�@��@G�@�\@׹@g@g@��@�e@:@�@�J@
=@O0@�h@��@�@Z@�H@��@@UU@�#@Ӡ@�@SI@�u@є@@O0@�P@��@�@K@��@�@J@Ji@��@ƨ@ @ B�@ ��@ �2@!@!B�@!�@!�@"1@"H]@"��@"��@#
�@#H]@#��@#��@$ �@$<�@$z3@$�@$�~@%5@@%qS@%�@%��@&""@&]�@&�0@&��@'
�@'C�@'|�@'��@'�@(&�@(^�@(��@(�*@)�@)@�@){�@)�9@)�@@*(G@*bN@*�U@*�
@+@+K@+�|@+�>@+�E@,6�@,o�@,��@,�@-
@-X@-��@-�@.@.:@.r�@.��@.�@/�@/V@/�\@/ȴ@0@0<@0v�@0��@0�4@1&�@1a�@1��@1խ@2@2I�@2��@2�j@2�e@3,`@3g@3�@3�@4o@4Ji@4��@4�@4�@5*S@5bN@5��@5�@6@68�@6qS@6��@6��@7�@7I�@7}�@7�@8O�@8�@9v�@9�[@:i!@:� @;�|@<�@<��@=b@=�4@>:�@>��@?/�@?��@@G�@@��@A`B@A�4@By�@C1@C�u@D!s@D�W@E�@E��@F,`@F�2@GWb@G��@HK�@H�;@Iuk@I�
@Ji�@J��@K��@K��@L�i@M!s@M�~@N> @N��@OJi@O��@PV@Q��@S�@TO1@U�@V�@XA�@Y�(@Z�`@\_�@]�f@^�q@`H]@a�a@c�@dUU@e�z@f��@hX�@i��@j�;@lM�@m�P@m�H@n�@nj@n�m@n�@o&;@ot�@o��@o��@pJi@p��@p��@q�@qZ�@q��@q��@r�@rb�@r�r@r�@s33@sg@s��@tj@t9X@t��@t�7@u �@uM$@u��@u�@vC@vg@v�I@v��@w+@wqS@w��@w�EG�O�@ jG�O�G�O�G�O�@ G�O�@ G�O�G�O�G�O�@ �G�O�G�O�G�O�@ vG�O�@ �@ �@ 
=@ �@ �@ V@ b@ �@ �@ *@ 6@ �@ �@ [@ g@ !s@ $.@ &;@ (G@ +@ -@ 0x@ 2�@ 5?@ 7�@ ;d@ =q@ @,@ DD@ F�@ Ji@ M�@ P�@ S�@ Wb@ Z@ ]�@ `�@ c�@ ff@ i�@ m:@ o�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A��A��A���A���A��A��;A֟�A�5?A��A��A��A���AոRAգ�AՏ\A�v�A�O�A��A�VAӉ7A�$�A�A���A���A��A���A�ȴAҧ�AґhA�|�A�hsA��A�ffA�O�A�33A�l�A�ffA�hsA��/A���A�7LA�33A�G�A��wA�VA���A�?}A���A�/A��jA��A�K�A��!A��A��TA�hsA�A�A��A��A���A��A�+A��A��A�x�A���A��A�n�A��`A���A���A���A�^5A�z�A�n�A���A�ZA�$�A��9A�VA���A�9XA���A�|�A�M�A�~�A��A��\A�G�A��DA�t�A��`A���A��A��7A�-A���A��A�%A���A�O�A��A��A�v�A�I�A���A�%A���A��DA��A��A�A�A��A��A}hsA{�
A{;dAzM�Ax-Av�AuƨAt��Aq��Ao�-AnZAm\)Al1'AkhsAhVAf �AdM�Ab  A`�!A^1'A[��AZ��AY�wAXjAW?}AVZAU��AS�PAQ�
AQ"�AP�uAO+AM�AL�AKl�AJ=qAHbNAFĜAF-AEO�ADjAC�PABQ�AA�A?�A??}A>��A=��A<�HA<bNA<A;��A;�A:�\A8Q�A5�A4E�A3G�A2A0 �A/dZA.ffA-A-33A,-A++A)�TA(jA&z�A%?}A$ �A!33A(�A�AVA�
A7LA�uAjA-A��A�A�;A��A|�A��A�A�9AO�AbNA�TAp�A��A��A��A�A�7A�A�A�A
�/A
bNA	��A1'A�AA\)A�-A�A��A��A�An�A^5A9XAV@�&�@�1@�o@��y@���@��@���@���@�Z@�{@��@�Q�@�S�@���@�$�@�7@��`@� �@�t�@�o@�V@���@��@��`@��#@�Z@�\)@�^5@�G�@ܴ9@�1'@�K�@ٲ-@ו�@Չ7@��
@�v�@��`@���@�(�@��w@���@�
=@��#@�X@��-@��@��T@���@��@�x�@���@�Z@��@���@�S�@��+@�Z@�^5@�?}@�  @�|�@�v�@� �@��P@�;d@�^5@��-@���@�I�@���@���@��h@��u@���@�;d@���@��7@��`@�A�@��!@���@�%@�(�@��P@�V@���@�1@}/@{��@y�#@w
=@u`B@sC�@pr�@n��@m�@kdZ@h��@e��@cS�@`1'@_�@_K�@^5?@]�@["�@X�`@W�w@W;d@V�R@Up�@S33@R�@P��@Nv�@Nv�@NV@N$�@M/@J��@I&�@H�@Fv�@D�/@C�
@Ax�@?�;@<��@<(�@;�
@;S�@9x�@7|�@7K�@6�+@5`B@3ƨ@2-@1��@01'@0  @/�P@/|�@.��@,�@+t�@)�#@)��@(��@(bN@'\)@&v�@%��@$�j@#��@"��@!��@!�7@!�@ Ĝ@ 1'@��@ȴ@$�@�@`B@?}@/@��@9X@@�^@��@�w@K�@�@`B@�@C�@�^@��@bN@��@l�@��@��@�@z�@�@33@
^5@	�@	X@bN@�;@��@;d@�y@��@ff@�-@�@9X@�@1@@M�?��?��D?���?�$�?�?�-?�w?��?�I�?��H?�^?�u?�
=?�`B?�F?�M�?�A�?���?�{?�/?ܬ?�"�?���?ؓu?�K�?�$�?�`B?�Z?��?��?�M�?Ѓ?�\)?�5??�5??�O�?̋D?̋D?�I�?�"�?ɺ^?���?Ǯ?��y?�ff?�$�?�o?�hs?��w?��?��h?���?��?�(�?�1?�I�?�(�?�"�?�~�?�^5?���?��H?�?�?�"�?�C�?�C�?�C�?�dZ?���?���?�ƨ?�ƨ?�1?�1?�(�?�j?�j?�j?��D?��D?���?���?���?�V?�V?�/?�O�?�p�?��h?���?��?�{?�{?�5??�5??���?���?��R?��?���?���?��?�\)?�|�?��wA��A���A���A���A��A��A��A��A��A��A��A��A��A��A��A���A��A���A���A���A���A���A���A���A���A���A���A���A���A��A��A��A��;A���A���A֛�A�r�A�C�A�5?A�(�A��A��A���A��mA��HA��/A��A���A���A���A�ȴA�ĜA���AռjAոRAմ9Aհ!AլAէ�Aա�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    A��A��A���A���A��A��;A֟�A�5?A��A��A��A���AոRAգ�AՏ\A�v�A�O�A��A�VAӉ7A�$�A�A���A���A��A���A�ȴAҧ�AґhA�|�A�hsA��A�ffA�O�A�33A�l�A�ffA�hsA��/A���A�7LA�33A�G�A��wA�VA���A�?}A���A�/A��jA��A�K�A��!A��A��TA�hsA�A�A��A��A���A��A�+A��A��A�x�A���A��A�n�A��`A���A���A���A�^5A�z�A�n�A���A�ZA�$�A��9A�VA���A�9XA���A�|�A�M�A�~�A��A��\A�G�A��DA�t�A��`A���A��A��7A�-A���A��A�%A���A�O�A��A��A�v�A�I�A���A�%A���A��DA��A��A�A�A��A��A}hsA{�
A{;dAzM�Ax-Av�AuƨAt��Aq��Ao�-AnZAm\)Al1'AkhsAhVAf �AdM�Ab  A`�!A^1'A[��AZ��AY�wAXjAW?}AVZAU��AS�PAQ�
AQ"�AP�uAO+AM�AL�AKl�AJ=qAHbNAFĜAF-AEO�ADjAC�PABQ�AA�A?�A??}A>��A=��A<�HA<bNA<A;��A;�A:�\A8Q�A5�A4E�A3G�A2A0 �A/dZA.ffA-A-33A,-A++A)�TA(jA&z�A%?}A$ �A!33A(�A�AVA�
A7LA�uAjA-A��A�A�;A��A|�A��A�A�9AO�AbNA�TAp�A��A��A��A�A�7A�A�A�A
�/A
bNA	��A1'A�AA\)A�-A�A��A��A�An�A^5A9XAV@�&�@�1@�o@��y@���@��@���@���@�Z@�{@��@�Q�@�S�@���@�$�@�7@��`@� �@�t�@�o@�V@���@��@��`@��#@�Z@�\)@�^5@�G�@ܴ9@�1'@�K�@ٲ-@ו�@Չ7@��
@�v�@��`@���@�(�@��w@���@�
=@��#@�X@��-@��@��T@���@��@�x�@���@�Z@��@���@�S�@��+@�Z@�^5@�?}@�  @�|�@�v�@� �@��P@�;d@�^5@��-@���@�I�@���@���@��h@��u@���@�;d@���@��7@��`@�A�@��!@���@�%@�(�@��P@�V@���@�1@}/@{��@y�#@w
=@u`B@sC�@pr�@n��@m�@kdZ@h��@e��@cS�@`1'@_�@_K�@^5?@]�@["�@X�`@W�w@W;d@V�R@Up�@S33@R�@P��@Nv�@Nv�@NV@N$�@M/@J��@I&�@H�@Fv�@D�/@C�
@Ax�@?�;@<��@<(�@;�
@;S�@9x�@7|�@7K�@6�+@5`B@3ƨ@2-@1��@01'@0  @/�P@/|�@.��@,�@+t�@)�#@)��@(��@(bN@'\)@&v�@%��@$�j@#��@"��@!��@!�7@!�@ Ĝ@ 1'@��@ȴ@$�@�@`B@?}@/@��@9X@@�^@��@�w@K�@�@`B@�@C�@�^@��@bN@��@l�@��@��@�@z�@�@33@
^5@	�@	X@bN@�;@��@;d@�y@��@ff@�-@�@9X@�@1@@M�?��?��D?���?�$�?�?�-?�w?��?�I�?��H?�^?�u?�
=?�`B?�F?�M�?�A�?���?�{?�/?ܬ?�"�?���?ؓu?�K�?�$�?�`B?�Z?��?��?�M�?Ѓ?�\)?�5??�5??�O�?̋D?̋D?�I�?�"�?ɺ^?���?Ǯ?��y?�ff?�$�?�o?�hs?��w?��?��h?���?��?�(�?�1?�I�?�(�?�"�?�~�?�^5?���?��H?�?�?�"�?�C�?�C�?�C�?�dZ?���?���?�ƨ?�ƨ?�1?�1?�(�?�j?�j?�j?��D?��D?���?���?���?�V?�V?�/?�O�?�p�?��h?���?��?�{?�{?�5??�5??���?���?��R?��?���?���?��?�\)?�|�?��wA��A���A���A���A��A��A��A��A��A��A��A��A��A��A��A���A��A���A���A���A���A���A���A���A���A���A���A���A���A��A��A��A��;A���A���A֛�A�r�A�C�A�5?A�(�A��A��A���A��mA��HA��/A��A���A���A���A�ȴA�ĜA���AռjAոRAմ9Aհ!AլAէ�Aա�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
�B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
�B
�B
�/B
�HB
�fB+B+Bu�B�B�dB��B��B�yB�B�B�B��B��B��B��BBoB$�B)�BR�BS�B_;B_;Bz�Bv�Bv�Bu�B{�B�B� B�B� B|�B|�Bz�B�B�B~�B|�B{�By�Bs�Bo�Bl�BjBgmBbNBe`BhsBdZBcTBgmBaHB_;B^5B]/BXB\)BP�BJ�BE�BA�B;dB1'B�BbB1B%B  B��B�B�)BɺB�qB�!B��B�+Bs�Bk�BK�B+B�B1BB
��B
�B
�B
�`B
ȴB
�B
��B
p�B
cTB
E�B
:^B
1'B
,B
 �B
	7B
�B
0!B
'�B
{B
bB
+B	��B	�yB	�B	��B	ȴB	�}B	�^B	��B	��B	�+B	x�B	n�B	`BB	ZB	W
B	P�B	K�B	G�B	B�B	<jB	/B	'�B	#�B	�B	{B	PB	B��B��B�B�`B�NB�/B�#B��B��B��BƨBB��B�XB�RB�FB�?B�3B�!B�B��B��B��B��B�DB�%B~�Bx�Bv�Bt�Br�Bn�Bl�Be`B`BBe`BgmB]/BP�BO�BN�BJ�BL�BP�BP�BQ�BXBaHB^5B\)B]/B^5B^5B[#BR�BQ�BN�BP�BJ�BF�BE�BE�BC�BB�BD�BC�BB�BD�BC�BB�BA�B>wB:^B:^B:^B9XB8RB7LB6FB6FB6FB49B8RB7LB8RB7LB7LB49B49B6FB5?B6FB8RB6FB7LB6FB6FB6FB6FB5?B6FB5?B5?B6FB5?B33B7LB7LB7LB33B7LB7LB6FB9XB9XB<jB<jBI�BZB_;B`BBhsBy�B�B�=B��B�?B�B��B	%B	JB	oB	�B	/B	9XB	D�B	S�B	XB	n�B	iyB	n�B	o�B	�B	�B	�JB	��B	��B	��B	�B	�B	�FB	�^B	��B	ɺB	��B	��B	��B	��B	�B	�)B	�;B	�NB	�sB	�B	�B	�B	�B	��B	��B	��B
B
B
B
1B

=B
VB
bB
hB
�B
�B
�B
 �B
#�B
$�B
"�B
#�B
#�B
$�B
%�B
'�B
(�B
)�B
+B
-B
0!B
0!B
1'B
49B
49B
33B
33B
5?B
8RB
9XB
:^B
=qB
>wB
>wB
@�B
B�B
E�B
E�B
E�B
E�B
H�B
I�B
H�B
J�B
J�B
M�B
N�B
N�B
P�B
P�B
P�B
P�B
R�B
VB
VB
YB
XB
YB
YB
ZB
[#B
[#B
\)B
]/B
^5B
_;B
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
bNB
cTB
cTB
e`B
e`B
ffB
gmB
ffB
hsB
iyB
jB
jB
l�B
l�B
m�B
n�B
n�B
o�B
p�B
q�B
q�B
r�B
r�B
s�B
s�B
t�B
v�B
v�B
v�B
w�B
w�B
x�B
x�B
y�B
z�B
{�B
z�B
{�B
|�B
|�B
�B
�B
�B
�%B
�1B
�1B
�DB
�JB
�PB
�VB
�\B
�\B
�hB
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
�B
�B
�B
�'B
�-B
�3B
�3B
�9B
�?B
�?B
�FB
�?B
�LB
�RB
�LB
�RB
�RB
�XB
�XB
�^B
�^B
�dB
�dB
�dB
�dB
�dB
�^B
�dB
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
�jB
�jB
�dB
�dB
�dB
�jB
�dB
�dB
�dB
�dB
�dB
�jB
�dB
�jB
�dB
�dB
�dB
�jB
�jB
�dB
�dB
�dB
�B
��B
��B
��B
��B
�B
��B
��B
��B
��B
��B
��B
�B
�B
��B
��B
��B
��B
��B
��B
��B
�B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
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
�B
�
B
�B
�B
�B
�#B
�)B
�)B
�/G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
�B
�B
�5B
�TBB'�Br�B��B�RB�wB��B�fB�B�B�B�B��B��B��B  B\B!�B&�BO�BP�B\)B\)Bw�Bs�Bs�Br�Bx�B}�B|�B~�B|�By�By�Bw�B}�B�B{�By�Bx�Bv�Bp�Bl�BiyBgmBdZB_;BbNBe`BaHB`BBdZB^5B]/B[#B[#BVBZBN�BH�BC�B?}B9XB/B�BVB%BB��B��B�B�BǮB�dB�B��B�Bq�BiyBI�B(�B�B%B
��B
��B
�B
�B
�TB
ƨB
�B
��B
n�B
aHB
C�B
8RB
/B
)�B
�B
+B
�B
.B
%�B
oB
VB
B	��B	�mB	�
B	��B	ƨB	�qB	�RB	��B	�uB	�B	v�B	l�B	^5B	XB	T�B	N�B	I�B	E�B	@�B	:^B	-B	%�B	!�B	�B	oB	DB	B��B�B�yB�TB�BB�#B�B��B��BɺBĜB��B�wB�LB�FB�9B�3B�'B�B��B��B��B�{B�uB�7B�B|�Bv�Bt�Br�Bp�Bl�BjBcTB^5BcTBe`B[#BN�BM�BL�BH�BJ�BN�BN�BO�BVB_;B\)BZB[#B\)B\)BYBP�BO�BL�BN�BH�BD�BC�BC�BA�B@�BB�BA�B@�BB�BA�B@�B?}B<jB8RB8RB8RB7LB6FB5?B49B49B49B2-B6FB5?B6FB5?B5?B2-B2-B49B33B49B6FB49B5?B49B49B49B49B33B49B33B33B49B33B1'B5?B5?B5?B1'B5?B5?B49B7LB7LB:^B:^BG�BXB]/B^5BffBw�B~�B�1B��B�3B�B��B	B	
=B	bB	�B	-B	7LB	B�B	Q�B	VB	l�B	gmB	l�B	m�B	~�B	�B	�=B	�uB	��B	��B	��B	�B	�9B	�RB	�wB	ǮB	ȴB	��B	��B	��B	�B	�B	�/B	�BB	�fB	�B	�B	�B	�B	��B	��B	��B
  B
  B
B
+B
	7B
PB
\B
bB
{B
�B
�B
�B
"�B
#�B
!�B
"�B
"�B
#�B
$�B
&�B
'�B
(�B
)�B
,B
/B
/B
0!B
33B
33B
2-B
2-B
49B
7LB
8RB
9XB
<jB
=qB
=qB
?}B
A�B
D�B
D�B
D�B
D�B
G�B
H�B
G�B
I�B
I�B
L�B
M�B
M�B
O�B
O�B
O�B
O�B
Q�B
T�B
T�B
XB
W
B
XB
XB
YB
ZB
ZB
[#B
\)B
]/B
^5B
]/B
]/B
^5B
^5B
_;B
`BB
`BB
aHB
aHB
bNB
aHB
bNB
bNB
dZB
dZB
e`B
ffB
e`B
gmB
hsB
iyB
iyB
k�B
k�B
l�B
m�B
n�B
o�B
p�B
q�B
q�B
r�B
r�B
s�B
s�B
t�B
v�B
v�B
v�B
w�B
w�B
x�B
x�B
y�B
z�B
{�B
z�B
{�B
|�B
|�B
�B
�B
�B
�%B
�1B
�1B
�DB
�JB
�PB
�VB
�\B
�\B
�hB
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
�B
�B
�B
�B
�B
�!B
�-B
�3B
�9B
�9B
�?B
�FB
�FB
�LB
�FB
�XB
�^B
�XB
�^B
�^B
�dB
�dB
�jB
�jB
�qB
�qB
�qB
�qB
�qB
�jB
�qB
�jB
�qB
�qB
�qB
�qB
�qB
�qB
�qB
�qB
�qB
�qB
�wB
�wB
�qB
�qB
�qB
�wB
�qB
�qB
�qB
�qB
�qB
�wB
�qB
�wB
�qB
�qB
�qB
�wB
�wB
�qB
�wB
�wB
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
ȴB
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
�B
�
B
�B
�B
�B
�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�201811171800302021061413525520210614135255202106141746542021061417465420210614174654201811171800302021061413525520210614135255202106141746542021061417465420210614174654PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            new conductivity = original conductivity * (1 + delta*TEMP + CPcor_SBE*PRES) / (1 + delta*TEMP_ADJUSTED + CPcor_new*PRES_ADJUSTED)                                                                                                                              none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS                                                                                                                                                                                                                                       PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            new conductivity = original conductivity * (1 + delta*TEMP + CPcor_SBE*PRES) / (1 + delta*TEMP_ADJUSTED + CPcor_new*PRES_ADJUSTED)                                                                                                                              none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS                                                                                                                                                                                                                                       surface_pressure=-0.50 dbar                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            CPcor_new = -1.25e-07; CPcor_SBE = -9.57e-08; delta = 3.25e-06                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            OW r = 0.9999 (+/-0), vertically averaged dS = -0.002 (+/-0)                                                                                                                                                                                                    surface_pressure=-0.50 dbar                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            CPcor_new = -1.25e-07; CPcor_SBE = -9.57e-08; delta = 3.25e-06                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            OW r = 0.9999 (+/-0), vertically averaged dS = -0.002 (+/-0)                                                                                                                                                                                                    Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                      none                                                                                                                                                                                                                                                            New conductivity computed by using a different CPcor value from that provided by Sea-Bird.                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Significant salinity sensor drift detected. OW least squares fit adopted.                                                                                                                                                                                       Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                      none                                                                                                                                                                                                                                                            New conductivity computed by using a different CPcor value from that provided by Sea-Bird.                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Significant salinity sensor drift detected. OW least squares fit adopted.                                                                                                                                                                                       PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            AO  AO  ARCAARCAADJPADJP                                                                                                                                        2018111718003020181117180030  IP  IP                                G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2018111718003020181117180030QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�5F03E           703E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2018111718003020181117180030QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�0               0               PM  PM  ARSQARSQCOWGCOWGV1.1V1.1                                                                                                                                2021072216015520210722160155IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                