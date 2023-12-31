CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS     N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-07-24T22:02:29Z creation      
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
_FillValue                    L`   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  P�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    a    PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  e    TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  u�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    �    TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �@   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �`   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �    PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    Ԁ   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ؠ   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                   �   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                   �   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                   �   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                   �   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  � �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                   (   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                   D   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    L   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar        l   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar        t   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�       |   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  �    SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                   �   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  � �      � �Argo profile    3.1 1.2 19500101000000  20180724220229  20210722160148  5905738 5905738 US ARGO PROJECT                                                 US ARGO PROJECT                                                 GREGORY C. JOHNSON                                              GREGORY C. JOHNSON                                              PRES            TEMP            PSAL            PRES            TEMP            PSAL                  DD  AOAO7218                            7218                            2C  2C  DD  SOLO_D_MRV                      SOLO_D_MRV                      12001                           12001                           SBE603 01Dec16                  SBE603 01Dec16                  874 874 @�d����@�d����11  @�d��\P@�d��\P@6�I�^5?@6�I�^5?�cŮ}Vl��cŮ}Vl�11  GPS     GPS     Primary sampling: averaged [1Hz sampling by SBE-41CP averaged in 2-dbar bins]                                                                                                                                                                                   Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 AB  AA  AA  >���?fff@   @@  @�  @�  @�33@���A��A33A#33A@  A^ffA~ffA�  A�  A�33A�33A�  A�  A�  B ffBffB  B  B   B(ffB0ffB8  B@  BG��BPffBXffB`  BhffBpffBxffB�33B�33B�33B�33B���B�  B�33B�33B�33B�ffB�33B�33B�  B���B�33B�ffB�ffB�  BǙ�B�  B���B���B�33B�  B���B䙚B�33B���B�B�33B���B�33C 33C�C�fC�C�C	�fC�C  C  C33CL�C�C  C�CL�C�C�fC!�fC#�fC%�fC(  C*  C,�C.�C0�C233C4�C633C8L�C:  C;��C>  C@33CB  CC��CE�fCH  CJ33CL  CM��CO�fCR�CTL�CVffCX33CY�fC\�C^  C_��Cb  CdL�Cf33Ch  Cj33Cl�Cm�fCp�CrL�Ct33Cv  CxL�Cz33C|�C~  C�fC��C��C�  C��C�  C��3C��C�  C��3C��C��C�  C��fC�  C��C�&fC��C��3C�  C��C��C��C�&fC�33C�33C�33C��C��fC��fC��C�&fC�&fC��C��C�  C�  C��3C�  C�  C��C��C��C��C��C��C��C��3C��3C��fC�ٚC��fC�ٚC�ٚC�ٚC��fC��fC�ٚC��fC��fC��3C��C��C�&fC�&fC��C�  C�  C��C��C�  C��C��C��3C��C��C��C��3C��C��C�  C��fC��3C�  C�  C��3C��fC��3C��C��C�&fC��C��3C��3C�  C��C��C�&fC�&fC�  C��C��C��3C�  C�  C��3C�  C�  C��C��C��C��C�  C��C��C��C��C��C��C��C��C�  C�ٚC��fC��3C��3C�  C�  C�� D�D��D
ffD  D�3D��D9�D�fD� D3D�fD"33D$� D'L�D)��D,� D/fD1� D4&fD6�3D9L�D;ٚD>Y�D@�3DCS3DEٚDH` DJ�3DML�DO��DRFfDT�fDW33DY�3D\,�D^��DafDc� De�fDhL�Dj� Dm  Do� Dr3Dt�fDw  Dy� D{��D~�D�L�D�y�D��fD�� D�0 D�p D���D�� D�,�D�ffD���D��fD�3D�L�D�|�D���D���D�fD�33D�ffD��fD�ɚD��fD�#3D�P D�vfD���D��fD��fD��D�6fD�ffD��3D���D�� D�3D�#3D�L�D�l�D�� D���D���D���D�3D�0 D�P D�l�D�� D��3D�ɚD��D�fD�)�D�P D�s3D���D��fD�� D��D�C3D�p DǦfD�ٚD�3D�FfD̃3Dͼ�D��fD�)�D�Y�D҃3Dө�D�ɚD�� D� D�6fD�VfD�s3Dۉ�Dܠ Dݰ D޹�D��3D���D�ٚD�� D��fD��3D��D��D�� D���D���D�ٚD��3D��3D�� D�� D��3D���D�ɚD��fD�� D�� D���D��3D��fD�� D�� D��3D�� D�� D�� D���D���E X E �3E^fE�3Ed�E�3Es3E� E��E#3E0 E	8 E
�fE��E9�E.fE��EfE�3E;3E� E��E��E4�Ep E�3E�E&fEh E�fE ��E"NfE#�fE%  E%� E'H E(�fE*�E*�E,VfE-�fE.��E0fE1t�E2d�E3�3E5#3E6y�E7[3E8��E9��E;<�E<|�E?��EB�fEE�EHٚEL9�EOS3ERh EUq�EX~fE[�3E^��Ea�fEe0 Eh( Ek� En�fEq{3Et��Ew�3Ez�fE~�E��fE�  E���E�VfE���E�VfE���E�vfE�3E�RfE���E� E�D�E�� E�� E�5�E���E��3E�"fE�}�E��3E�*fE�c3E���E��E�c3E���E��>���>���?   ?��?333?��?333?333?333?L��?L��?fff?fff?���?���?���?�ff?�  ?ٙ�?�33@ff@33@&ff@333@L��@`  @s33@�ff@�33@�  @���@���@�ff@�33@�33@�33A��A  A33A33A!��A+33A333A<��AC33AK33AS33AY��Ac33Ak33As33A{33A���A���A���A�ffA���A���A�ffA���G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411141441414141111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    ?fff?�33@   @`  @�  @�  @�33@���A	��A33A+33AH  AfffA�33A�  A�  A�33A�33A�  A�  A�  BffB
ffB  B  B"  B*ffB2ffB:  BB  BI��BRffBZffBb  BjffBrffBzffB�33B�33B�33B�33B���B�  B�33B�33B�33B�ffB�33B�33B�  B���B�33B�ffB�ffB�  Bș�B�  B���B���B�33B�  B���B噚B�33B���B�B�33B���B�33C �3C��CffC��C��C
ffC��C� C� C�3C��C��C� C��C��C��C ffC"ffC$ffC&ffC(� C*� C,��C.��C0��C2�3C4��C6�3C8��C:� C<L�C>� C@�3CB� CDL�CFffCH� CJ�3CL� CNL�CPffCR��CT��CV�fCX�3CZffC\��C^� C`L�Cb� Cd��Cf�3Ch� Cj�3Cl��CnffCp��Cr��Ct�3Cv� Cx��Cz�3C|��C~� C�33C�Y�C�L�C�@ C�L�C�@ C�33C�L�C�@ C�33C�L�C�Y�C�@ C�&fC�@ C�L�C�ffC�L�C�33C�@ C�L�C�L�C�Y�C�ffC�s3C�s3C�s3C�Y�C�&fC�&fC�L�C�ffC�ffC�L�C�L�C�@ C�@ C�33C�@ C�@ C�L�C�L�C�Y�C�Y�C�Y�C�Y�C�L�C�33C�33C�&fC��C�&fC��C��C��C�&fC�&fC��C�&fC�&fC�33C�L�C�Y�C�ffC�ffC�Y�C�@ C�@ C�Y�C�L�C�@ C�Y�C�L�C�33C�L�C�Y�C�L�C�33C�L�C�Y�C�@ C�&fC�33C�@ C�@ C�33C�&fC�33C�L�C�Y�C�ffC�L�C�33C�33C�@ C�L�C�Y�C�ffC�ffC�@ C�L�C�Y�C�33C�@ C�@ C�33C�@ C�@ C�L�C�L�C�L�C�L�C�@ C�L�C�L�C�L�C�L�C�Y�C�Y�C�Y�C�Y�C�@ C��C�&fC�33C�33C�@ C�@ D   D9�DٚD
�fD@ D�3D��DY�DfD� D33D�fD"S3D$� D'l�D*�D,� D/&fD1� D4FfD6�3D9l�D;��D>y�D@�3DCs3DE��DH� DJ�3DMl�DO��DRffDT�fDWS3DY�3D\L�D^��Da&fDc� DffDhl�Dj� Dm@ Do� Dr33Dt�fDw  Dy� D{��D~9�D�\�D���D��fD�  D�@ D�� D���D�  D�<�D�vfD���D��fD�#3D�\�D���D���D���D�fD�C3D�vfD��fD�ٚD�fD�33D�` D��fD���D��fD��fD��D�FfD�vfD��3D�ɚD�� D�3D�33D�\�D�|�D�� D���D���D���D�#3D�@ D�` D�|�D�� D��3D�ٚD���D�fD�9�D�` D��3D���D��fD�  D�)�D�S3Dƀ DǶfD��D�#3D�VfD̓3D���D�fD�9�D�i�Dғ3Dӹ�D�ٚD�  D�  D�FfD�ffDڃ3Dۙ�Dܰ D�� D�ɚD��3D���D��D�� D��fD��3D���D���D�� D���D���D��D��3D��3D�� D�� D��3D���D�ٚD��fD�� D�� D�ɚD��3D��fD�� D�� D��3D�� D�� D�� D���D���E ` E �3EffE�3El�E�3E{3E� E��E+3E8 E	@ E
�fEɚEA�E6fE��EfE�3EC3E� EɚE�E<�Ex E�3E�E.fEp E�fE!�E"VfE#�fE% E%� E'P E(�fE*	�E*��E,^fE-�fE.��E0fE1|�E2l�E3�3E5+3E6��E7c3E8��E:�E;D�E<��E?��EB�fEE�EH�ELA�EO[3ERp EUy�EX�fE[�3E^ɚEa�fEe8 Eh0 Ek� En�fEq�3Et��Ew�3Ez�fE~�E��fE�$ E�ŚE�ZfE���E�ZfE��E�zfE�3E�VfE���E� E�H�E�� E�  E�9�E���E��3E�&fE���E��3E�.fE�g3E���E��E�g3E���E���G�O�?fff?�  ?���G�O�?���G�O�G�O�?���G�O�?�ffG�O�?�33G�O�?���?ٙ�?�ff@   @��@��@&ff@333@Fff@S33@l��@�  @���@�ff@�33@�  @���@���@�ff@�33@�33A��A	��A  A33A#33A)��A333A;33AD��AK33AS33A[33Aa��Ak33As33A{33A���A���A���A���A�ffA���A���A�ffA���G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411141441414141111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    @ �@ %@ �@ {@ O@ ""@ )�@ /@ 7L@ >�@ E�@ Q�@ ^�@ l�@ z�@ ��@ ��@ �(@ �~@ �&@ ��@ �#@ ��@ ��@j@@g@-@:@G�@T�@c�@qS@~K@��@�H@��@��@�>@��@ލ@��@�,@�@*@"�@1'@>@K�@X�@e�@t�@�@��@�@�M@�R@�J@��@��@��@��@J@�@$�@5?@A�@M�@\�@k.@x&@�p@�u@�@�f@�k@�c@�
@�@�e@ �@�@�@+@7L@C�@Q=@^�@l�@z�@��@��@��@�-@��@�|@��@�(@��@@@ @,`@8�@F�@UU@dZ@p�@|�@�D@�H@�M@��@��@ψ@ލ@�@��@�@�@#�@/�@>�@K�@X@g@v@�@�\@�@�@�@��@��@�@�@��@
�@�@$�@3�@@�@M�@\�@k.@ww@��@��@�@�!@�k@ȴ@�
@�`@�@^@�@
@+�@9X@E�@P�@^5@m�@|�@��@��@��@�~@�&@�@�t@�@�q@	@	o@	 @	-�@	;d@	H]@	T�@	bN@	oF@	|?@	��@	��@	�5@	��@	�2@	��@	��@	�(@	��@
%@
*@
#�@
1�@
?}@
Lu@
X�@
ff@
uk@
�d@
�\@
�a@
�Y@
��@
ƨ@
��@
��@
�@@
�E@�@�@$.@2�@@�@N�@[z@hs@v�@��@�$@�y@��@�@ȴ@�
@�`@�@@�@O@)�@7�@C�@Q�@_�@l�@z�@��@��@��@�-@��@��@�#@��@�q@@o@ @-�@;d@G�@SI@a�@o�@}�@��@��@��@2�@z3@�>@�@Wb@��@��@3�@z�@�2@�@M$@��@�h@ @ff@�Y@�Y@7L@|�@��@	�@M�@�h@խ@�@_�@�y@�@*S@m�@�-@�e@8�@|?@�w@ �@DD@��@�W@
=@K@�\@�C@*@X�@�@�\@�@^�@�@�@%�@i�@�@�@6�@z3@�@�Q@B8@��@ȴ@	�@Ji@�D@��@ 
�@ Lu@ �P@ ��@!@!O0@!�\@!�*@"J@"Lu@"��@"ȴ@#1@#I@#�7@#�@$�@$D�@$�d@$��@$�Q@%=q@%z3@%��@%�@&3�@&p�@&�@&��@'%�@'c�@'�z@'��@(�@(Z�@(��@(׹@)6@)V�@)�0@)խ@**@*UU@*��@*�@+�@+]�@+�@+�@,&�@,hs@,�M@,��@-'�@-e	@-��@-�H@. @.]�@.�H@.խ@/@/K@/��@/�j@/�@0.l@0ff@0�a@0�O@1J@1B�@1ww@1�f@1�@2�@2O0@2��@2��@2�Y@3)�@3^�@3��@3��@4  @46�@4k�@4�@4�h@5@5G�@5{�@5�~@5�@6�@6S�@6�D@6@6��@72�@7k.@7�z@7��@8�@8��@8�E@9��@:O@:��@;2�@;�@<A�@<��@=H]@=��@>B8@>є@?\)@?�`@@j@@��@Aww@A��@B�d@C�@C�0@D!s@D��@E@,@E��@Fff@F�c@G_�@G�@H�7@H�@I��@J!s@J��@K	@K��@L[@L��@MI@M�#@N;d@N�o@OYn@O�T@Pk�@Q�C@S$�@TqS@U�-@W"�@Xuk@Y��@[�@\^�@]��@_
�@`[z@a�c@c�@d��@eȴ@g1@he	@i��@k%@lhs@m�1@o
=@pn�@q�@s�@tWa@u��@w@xe�@x�<@x�@y33@yg@y��@zj@z4�@z�@z�@z��@{Lv@{��@{��@|b@|Z@|��@|��@}3�@}dZG�O�@ �@ j@ G�O�@ G�O�G�O�@ �G�O�@ vG�O�@ %G�O�@ �@ 1@ �@ 
=@ �@ �@ V@ �@ �@ @ �@ �@ �@ �@ g@ ""@ $�@ (G@ *S@ -@ 0x@ 3�@ 7L@ :@ >�@ B8@ D�@ I@ Lu@ P�@ SI@ V�@ Z@ \�@ `�@ dZ@ g�@ k.@ m�@ r@ uk@ y�@ |?@ �@ ��@ �|G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�ZA�\)A�p�A�n�A�p�A�ffA�ffA�ffA�dZA�ffA�p�A�l�A�jA�l�A�v�A�^5A�5?AǮA�dZA�n�A�\)A�;dA�-A�1A���A�t�A��PA���A�O�A���A�33A�~�A���A��jA��uA�+A�ZA��A�VA�G�A�A���A�\)A�JA��HA���A�l�A�G�A��
A��\A�x�A�%A�^5A�(�A��A�$�A�A�v�A�A�A�C�A�`BA�hsA�S�A�~�A�G�A�M�A��
A�Q�A��+A�=qA���A�"�A�O�A��DA�VA��9A��A��uA���A�v�A�=qA�$�A�dZA��A���A��hA�VA��TA�l�A���A�Q�A��A���A��yA�bA�oA���A�O�A���A��PA�|�A���A�hsA��HA�/A�\)A�$�A�r�A��RA��+A�K�A���A�"�A���A�ĜA���A��DA��A�l�A���A�ȴA�I�Ax�A}/A{�Awx�As�7Ar�An^5AmAl��Ak�;Aj �AhĜAf��Ad��Ab�DA_�PA]x�A]G�A]S�A]x�A]��A\~�AZ�jAX9XAU�AT��ASS�AR�+AQ��AQoAP  AN��AMG�AK7LAJ{AIVAG;dAFJAE7LAD  ABbNA@n�A=�A;��A:-A8A�A6��A5�hA4��A3�^A2��A21'A1�A/��A/+A.E�A-�PA+��A+�hA+p�A+%A*  A(��A%�A$(�A#A"bA!;dA ��Ar�A��AI�A�A�A�A9XA-AƨAVA��AVA��AdZA�A�A�jAz�AE�A�
A�!AJA�AƨA��A�A�A�A?}A�;AS�Av�A|�A�`AffAA��A33A
��A
��A
A�A
JA��Al�A��A�A�mA$�A�A"�A �D@�33@���@�E�@���@�~�@���@���@���@�ƨ@�hs@�S�@�h@�b@�t�@��@�r�@�  @��@���@�l�@�=q@��H@��/@�Q�@��@���@��!@��@���@�M�@��m@���@�V@��m@�I�@�J@�^5@��!@�x�@��@�ff@��@�1'@�v�@���@��`@�A�@��P@�\)@��R@��@���@�+@��T@���@�(�@�ff@��-@���@���@���@�I�@�
=@�X@�A�@l�@}�h@|(�@z��@y&�@x��@w
=@u�T@s��@qG�@q%@oK�@n�y@m��@l�@kƨ@j��@ihs@h�u@fȴ@c�F@`1'@]��@[��@Z~�@Y%@W+@U�@U�@T�j@S��@RJ@P��@OK�@Nv�@MV@L1@K33@J�@I�^@IG�@H�u@Fȴ@E��@EV@DI�@A�@A�@@Q�@>v�@=O�@<�j@;ƨ@:n�@9��@9%@7��@6@5��@3��@2�\@1�@1��@17L@0b@/�@-�-@-?}@,1@*�@*n�@)�7@(r�@(b@'�w@&�@%p�@#��@#t�@"J@!X@�;@
=@@?}@(�@dZ@�@hs@%@�;@�@$�@@�h@�@��@S�@o@n�@��@�@�@�@+@v�@�T@�/@j@��@S�@	��@	�@A�@+@ȴ@V@�-@p�@��@��@��@o@~�@J@��@�?��?��?�"�?�=q?�b?�ȴ?�9X?�33?���?��;?���?�?�?�ff?�9X?�33?�M�?� �?��?���?��m?���?ٙ�?ؓu?�K�?�?���?��?�J?�G�?Ѓ?Ͼw?�|�?�;d?�V?�O�?��?�(�?�ƨ?��H?ʟ�?ɺ^?�b?�l�?Ƨ�?ļj?���?���?�bN?��w?���?���?�p�?��?�1?���?��?�C�?�"�?���?���?�~�?�~�?���?���?�?�dZ?�ƨ?�I�?��?�V?�p�?�{?���?���?��?�;d?�\)?�\)?���?��w?��w?�  ?�  ?�  ?�A�?�A�?��?���?���?�Ĝ?�%?��`?�%A�\)A�S�A�\)A�Q�A�S�A�VA�S�A�VA�^5A�^5A�ZA�\)A�\)A�bNA�bNA�^5A�l�A�l�A�n�A�p�A�r�A�l�A�l�A�jA�n�A�p�A�p�A�n�A�ffA�dZA�ffA�ffA�dZA�ffA�ffA�dZA�dZA�bNA�ffA�l�A�r�A�r�A�n�A�n�A�l�A�hsA�jA�hsA�jA�l�A�l�A�n�A�l�A�hsA�p�A�x�A�z�A�r�A�r�A�bNG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    A�ZA�\)A�p�A�n�A�p�A�ffA�ffA�ffA�dZA�ffA�p�A�l�A�jA�l�A�v�A�^5A�5?AǮA�dZA�n�A�\)A�;dA�-A�1A���A�t�A��PA���A�O�A���A�33A�~�A���A��jA��uA�+A�ZA��A�VA�G�A�A���A�\)A�JA��HA���A�l�A�G�A��
A��\A�x�A�%A�^5A�(�A��A�$�A�A�v�A�A�A�C�A�`BA�hsA�S�A�~�A�G�A�M�A��
A�Q�A��+A�=qA���A�"�A�O�A��DA�VA��9A��A��uA���A�v�A�=qA�$�A�dZA��A���A��hA�VA��TA�l�A���A�Q�A��A���A��yA�bA�oA���A�O�A���A��PA�|�A���A�hsA��HA�/A�\)A�$�A�r�A��RA��+A�K�A���A�"�A���A�ĜA���A��DA��A�l�A���A�ȴA�I�Ax�A}/A{�Awx�As�7Ar�An^5AmAl��Ak�;Aj �AhĜAf��Ad��Ab�DA_�PA]x�A]G�A]S�A]x�A]��A\~�AZ�jAX9XAU�AT��ASS�AR�+AQ��AQoAP  AN��AMG�AK7LAJ{AIVAG;dAFJAE7LAD  ABbNA@n�A=�A;��A:-A8A�A6��A5�hA4��A3�^A2��A21'A1�A/��A/+A.E�A-�PA+��A+�hA+p�A+%A*  A(��A%�A$(�A#A"bA!;dA ��Ar�A��AI�A�A�A�A9XA-AƨAVA��AVA��AdZA�A�A�jAz�AE�A�
A�!AJA�AƨA��A�A�A�A?}A�;AS�Av�A|�A�`AffAA��A33A
��A
��A
A�A
JA��Al�A��A�A�mA$�A�A"�A �D@�33@���@�E�@���@�~�@���@���@���@�ƨ@�hs@�S�@�h@�b@�t�@��@�r�@�  @��@���@�l�@�=q@��H@��/@�Q�@��@���@��!@��@���@�M�@��m@���@�V@��m@�I�@�J@�^5@��!@�x�@��@�ff@��@�1'@�v�@���@��`@�A�@��P@�\)@��R@��@���@�+@��T@���@�(�@�ff@��-@���@���@���@�I�@�
=@�X@�A�@l�@}�h@|(�@z��@y&�@x��@w
=@u�T@s��@qG�@q%@oK�@n�y@m��@l�@kƨ@j��@ihs@h�u@fȴ@c�F@`1'@]��@[��@Z~�@Y%@W+@U�@U�@T�j@S��@RJ@P��@OK�@Nv�@MV@L1@K33@J�@I�^@IG�@H�u@Fȴ@E��@EV@DI�@A�@A�@@Q�@>v�@=O�@<�j@;ƨ@:n�@9��@9%@7��@6@5��@3��@2�\@1�@1��@17L@0b@/�@-�-@-?}@,1@*�@*n�@)�7@(r�@(b@'�w@&�@%p�@#��@#t�@"J@!X@�;@
=@@?}@(�@dZ@�@hs@%@�;@�@$�@@�h@�@��@S�@o@n�@��@�@�@�@+@v�@�T@�/@j@��@S�@	��@	�@A�@+@ȴ@V@�-@p�@��@��@��@o@~�@J@��@�?��?��?�"�?�=q?�b?�ȴ?�9X?�33?���?��;?���?�?�?�ff?�9X?�33?�M�?� �?��?���?��m?���?ٙ�?ؓu?�K�?�?���?��?�J?�G�?Ѓ?Ͼw?�|�?�;d?�V?�O�?��?�(�?�ƨ?��H?ʟ�?ɺ^?�b?�l�?Ƨ�?ļj?���?���?�bN?��w?���?���?�p�?��?�1?���?��?�C�?�"�?���?���?�~�?�~�?���?���?�?�dZ?�ƨ?�I�?��?�V?�p�?�{?���?���?��?�;d?�\)?�\)?���?��w?��w?�  ?�  ?�  ?�A�?�A�?��?���?���?�Ĝ?�%?��`?�%A�\)A�S�A�\)A�Q�A�S�A�VA�S�A�VA�^5A�^5A�ZA�\)A�\)A�bNA�bNA�^5A�l�A�l�A�n�A�p�A�r�A�l�A�l�A�jA�n�A�p�A�p�A�n�A�ffA�dZA�ffA�ffA�dZA�ffA�ffA�dZA�dZA�bNA�ffA�l�A�r�A�r�A�n�A�n�A�l�A�hsA�jA�hsA�jA�l�A�l�A�n�A�l�A�hsA�p�A�x�A�z�A�r�A�r�A�bNG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
B
��B
��B
��B
��B
��B
��B
��B
��B
B
��B
��B
��B
��B
��B
ÖB
ĜB
��B
�BB
�BBB+BoB�B=qBXBv�B��B��B�^B��B�mBB!�BI�BbNB�B�bB��B��B��B��B�B�'B�!B�!B�'B�9B�RB�RB�XB�jB�jB�9B�3B�-B�!B�B��B�BgmBYBL�Be`BdZBaHBW
BF�B#�B&�B7LB?}BM�BL�B=qB49B:^BM�BO�BR�BR�BB�B.B$�B�BuBB�B�BB��B��B�FB�B�jB��BaHBB�BA�BA�B>wB'�B.B)�B+B$�BDB
��B
�)B
��B
�^B
��B
��B
��B
��B
�{B
�oB
�1B
�B
x�B
s�B
jB
bNB
R�B
@�B
 �B
+B	�B	�
B	��B	��B	B	�3B	��B	�{B	� B	r�B	]/B	VB	VB	VB	[#B	iyB	^5B	k�B	_;B	bNB	[#B	S�B	O�B	M�B	F�B	@�B	;dB	-B	$�B	�B	hB	1B	B	  B��B�B�mB�/B�B��B��BĜB�wB�jB�RB�FB�!B�B��B��B��B��B��B��B��B��B��B�PB�%B�B�Bz�Bz�Bw�Br�Br�Bo�Bn�Bm�Bl�Bm�Bm�Bk�Bk�BjBgmBhsBffBiyBjBk�Bm�Bm�Bn�Bo�Bq�Br�Bs�Bs�Bt�Bt�Bt�Bt�Bu�Bu�Bo�Bk�BiyBjBjBhsBdZBaHB_;B[#BZBN�BF�B?}B9XB5?B2-B33B1'B/B5?B=qBF�BG�BE�BG�BE�BC�B=qB:^B8RB8RB8RB8RB9XB:^B:^B:^B>wB:^BB�BF�BQ�B\)B]/BgmBo�B�\B��B��B��B��B�RB�
B�B�/B�5B��B	
=B	(�B	D�B	S�B	e`B	v�B	�B	�JB	�{B	��B	��B	��B	�!B	�?B	�LB	�qB	ÖB	ŢB	��B	��B	�B	�5B	�fB	�mB	�B	�B	��B	��B	��B	��B	��B
  B
  B
B
B
+B
DB

=B
PB
PB
VB
hB
bB
oB
uB
{B
�B
�B
�B
 �B
"�B
#�B
$�B
'�B
(�B
(�B
'�B
+B
-B
-B
0!B
1'B
1'B
2-B
33B
33B
5?B
49B
6FB
8RB
8RB
9XB
:^B
=qB
=qB
>wB
@�B
@�B
A�B
B�B
D�B
C�B
E�B
G�B
G�B
G�B
I�B
K�B
K�B
L�B
L�B
M�B
N�B
O�B
P�B
Q�B
R�B
S�B
S�B
VB
VB
W
B
W
B
YB
ZB
ZB
\)B
\)B
^5B
^5B
_;B
`BB
aHB
bNB
bNB
e`B
dZB
e`B
e`B
ffB
ffB
ffB
gmB
hsB
hsB
hsB
jB
jB
k�B
l�B
l�B
m�B
n�B
o�B
p�B
p�B
q�B
q�B
t�B
t�B
t�B
v�B
v�B
w�B
w�B
w�B
x�B
y�B
y�B
z�B
{�B
{�B
{�B
|�B
~�B
� B
�B
�B
�B
�B
�B
�%B
�1B
�7B
�=B
�JB
�VB
�\B
�bB
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
��B
��B
��B
�B
�B
�B
�B
�!B
�'B
�-B
�-B
�9B
�9B
�?B
�?B
�?B
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
�XB
�^B
�^B
�^B
�^B
�^B
�^B
�dB
�dB
�^B
�^B
�^B
�^B
�^B
�dB
�dB
�dB
�dB
�^B
�^B
�dB
�dB
�^B
�dB
�dB
B
��B
B
��B
��B
ÖB
B
��B
��B
��B
��B
��B
�jB
��B
�}B
B
B
��B
��B
��B
��B
��B
��B
B
��B
��B
��B
��B
��B
B
��B
��B
��B
��B
��B
��B
��B
��B
B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
�}B
��B
B
ÖB
��B
�;B
�BBB%BhB�B<jBW
Bu�B��B��B�XB��B�fBB �BH�BaHB�B�\B��B��B��B��B�B�!B�B�B�!B�3B�LB�LB�RB�dB�dB�3B�-B�'B�B�B��B�BffBXBK�BdZBcTB`BBVBE�B"�B%�B6FB>wBL�BK�B<jB33B9XBL�BN�BQ�BQ�BA�B-B#�B�BoBB�B�;B��B��B�?B�B�dB��B`BBA�B@�B@�B=qB&�B-B(�B)�B#�B
=B
��B
�#B
��B
�XB
��B
��B
��B
��B
�uB
�hB
�+B
� B
w�B
r�B
iyB
aHB
Q�B
?}B
�B
%B	�B	�B	��B	ɺB	��B	�-B	��B	�uB	~�B	q�B	\)B	T�B	T�B	T�B	ZB	hsB	]/B	jB	^5B	aHB	ZB	R�B	N�B	L�B	E�B	?}B	:^B	,B	#�B	�B	bB	+B	B��B��B�B�fB�)B�B��BɺBÖB�qB�dB�LB�?B�B�B��B��B��B��B��B��B��B��B�{B�JB�B�B� By�By�Bv�Bq�Bq�Bn�Bm�Bl�Bk�Bl�Bl�BjBjBiyBffBgmBe`BhsBiyBjBl�Bl�Bm�Bn�Bp�Bq�Br�Br�Bs�Bs�Bs�Bs�Bt�Bt�Bn�BjBhsBiyBiyBgmBcTB`BB^5BZBYBM�BE�B>wB8RB49B1'B2-B0!B.B49B<jBE�BF�BD�BF�BD�BB�B<jB9XB7LB7LB7LB7LB8RB9XB9XB9XB=qB9XBA�BE�BP�B[#B\)BffBn�B�VB��B��B��B��B�LB�B��B�)B�/B��B	
=B	(�B	D�B	S�B	e`B	v�B	�B	�JB	�{B	��B	��B	��B	�!B	�?B	�LB	�qB	ÖB	ŢB	��B	��B	�B	�5B	�fB	�mB	�B	�B	��B	��B	��B	��B	��B
  B
  B
B
B
+B
DB

=B
PB
PB
VB
hB
bB
oB
uB
{B
�B
�B
�B
 �B
"�B
#�B
$�B
'�B
(�B
(�B
'�B
+B
-B
-B
0!B
1'B
1'B
2-B
33B
33B
5?B
49B
6FB
8RB
8RB
9XB
:^B
=qB
=qB
>wB
@�B
@�B
A�B
B�B
D�B
C�B
E�B
G�B
G�B
G�B
I�B
K�B
K�B
L�B
L�B
M�B
O�B
P�B
Q�B
R�B
S�B
T�B
T�B
W
B
W
B
XB
XB
ZB
[#B
[#B
]/B
]/B
_;B
_;B
`BB
aHB
bNB
cTB
cTB
ffB
e`B
ffB
ffB
gmB
gmB
gmB
hsB
iyB
iyB
iyB
k�B
k�B
l�B
m�B
m�B
n�B
o�B
p�B
q�B
q�B
r�B
r�B
u�B
u�B
u�B
w�B
w�B
x�B
x�B
x�B
y�B
z�B
z�B
{�B
|�B
|�B
|�B
}�B
� B
�B
�B
�B
�B
�B
�%B
�+B
�7B
�=B
�DB
�PB
�\B
�bB
�hB
�uB
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
�B
�!B
�'B
�3B
�9B
�?B
�?B
�LB
�LB
�RB
�RB
�RB
�XB
�^B
�^B
�dB
�dB
�jB
�jB
�jB
�jB
�qB
�qB
�wB
�wB
�wB
�wB
�wB
�wB
�}B
�}B
�wB
�wB
�wB
�wB
�wB
�}B
�}B
�}B
�}B
�wB
�wB
�}B
�}B
�wB
�}B
�}B
��B
��B
��B
��B
�}B
B
��B
��B
��B
��B
�}B
��B
�dB
�}B
�wB
��B
��B
��B
��B
��B
�}B
�}B
��B
��B
��B
��B
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
��B
��B
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
�}B
�}B
�}B
��B
��B
�}B
��B
��B
��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�201807242202292021061413520220210614135202202106141746152021061417461520210614174615201807242202292021061413520220210614135202202106141746152021061417461520210614174615PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            new conductivity = original conductivity * (1 + delta*TEMP + CPcor_SBE*PRES) / (1 + delta*TEMP_ADJUSTED + CPcor_new*PRES_ADJUSTED)                                                                                                                              none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS                                                                                                                                                                                                                                       PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            new conductivity = original conductivity * (1 + delta*TEMP + CPcor_SBE*PRES) / (1 + delta*TEMP_ADJUSTED + CPcor_new*PRES_ADJUSTED)                                                                                                                              none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS                                                                                                                                                                                                                                       surface_pressure=-0.50 dbar                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            CPcor_new = -1.25e-07; CPcor_SBE = -9.57e-08; delta = 3.25e-06                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            OW r = 1 (+/-0), vertically averaged dS = -0.001 (+/-0)                                                                                                                                                                                                         surface_pressure=-0.50 dbar                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            CPcor_new = -1.25e-07; CPcor_SBE = -9.57e-08; delta = 3.25e-06                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            OW r = 1 (+/-0), vertically averaged dS = -0.001 (+/-0)                                                                                                                                                                                                         Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                      none                                                                                                                                                                                                                                                            New conductivity computed by using a different CPcor value from that provided by Sea-Bird.                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Significant salinity sensor drift detected. OW least squares fit adopted.                                                                                                                                                                                       Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                      none                                                                                                                                                                                                                                                            New conductivity computed by using a different CPcor value from that provided by Sea-Bird.                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Significant salinity sensor drift detected. OW least squares fit adopted.                                                                                                                                                                                       PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            AO  AO  ARCAARCAADJPADJP                                                                                                                                        2018072422022920180724220229  IP  IP                                G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2018072422022920180724220229QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�5F03E           703E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2018072422022920180724220229QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�0               0               PM  PM  ARSQARSQCOWGCOWGV1.1V1.1                                                                                                                                2021072216014820210722160148IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                