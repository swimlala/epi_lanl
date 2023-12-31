CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS     N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-07-24T22:02:49Z creation      
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
resolution        =���   axis      Z        @  ;�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   L   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     @  P$   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   `d   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     @  dt   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     @  t�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   ��   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     @  �   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   �D   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     @  �T   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     @  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     @  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   �$   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     @  �4   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                   �   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                   �   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                   �   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                   �   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  � �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                   |   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                   �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar        �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar        �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�       �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  �t   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  � Argo profile    3.1 1.2 19500101000000  20180724220249  20210617131456  5905739 5905739 US ARGO PROJECT                                                 US ARGO PROJECT                                                 GREGORY C. JOHNSON                                              GREGORY C. JOHNSON                                              PRES            TEMP            PSAL            PRES            TEMP            PSAL                  DD  AOAO7219                            7219                            2C  2C  DD  SOLO_D_MRV                      SOLO_D_MRV                      12002                           12002                           SBE603 01Dec16                  SBE603 01Dec16                  874 874 @�j'�Mr@�j'�Mr11  @�j'���`@�j'���`@6��M�@6��M��c��TɅ��c��TɅ�11  GPS     GPS     Primary sampling: averaged [1Hz sampling by SBE-41CP averaged in 2-dbar bins]                                                                                                                                                                                   Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 AC  AA  AA  >���?fff@   @Fff@�33@�  @�33@�  A   A  A&ffAA��Ac33A���A�  A�33A�33A���A���A�33A�33A�33BffBffB  B ffB(  B0ffB8ffB?��BH  BPffBW��B`  Bh��Bp  Bw��B�  B�33B�ffB�ffB�33B�33B�33B�33B�33B�  B���B���B�  B�33B�  B�  B�  B�33B�  B�33B�  B�  B�  Bܙ�B�ffB�33B�  B�  B���B�B�33B�33C   C�fC��C�C  C	��C�CL�C33C�C�C�fC�C�C  CL�C �C!�fC$�C%�fC'��C)�fC,�C.33C0  C1�3C3�fC6�C7�fC9��C;�fC>  C@�CB33CD  CE�3CG��CI�fCL�CN33CP�CQ�fCT�CV33CX  CY��C[�fC^�C`33Cb  Cc�fCf33Ch�Cj  Ck�fCm��Cp  Cr33Ct  Cu��Cx  Cz  C{�fC~L�C��C��C�  C�  C�&fC��C��C�  C��3C��C��C�  C�  C��3C��C��C�  C�&fC��C��C�  C��3C�  C��3C��C��C�  C�&fC��C�  C��C��C��3C��C�&fC��C��3C�  C��C�&fC��C��3C��C�&fC��C�  C��C�&fC�  C��3C�  C��C��C�  C��fC��fC��fC�  C��C��C�  C��fC��3C�  C��C�&fC��C��fC��3C�  C��C��C�&fC��C��fC��3C��C��C��C��fC�  C��C��C��C��C��C��fC��3C�  C��C��C��C�&fC��C��fC�  C��C��C��C��C�  C�ٚC��3C��3C��C��C�  C��fC�  C��C��C�&fC��C��fC��3C��C��C��C��C��C�  C�ٚC��3DFfDٚD�fD
9�DfD� D� D@ D�3D�3D33D�fD"S3D$�fD'9�D)��D,3D.� D0��D3� D5��D8�fD;3D=�fD@fDB�3DE,�DG��DJ` DL��DO��DR�DT��DW&fDY��D\,�D^��Da  Dc�3DffDhs3DjٚDmFfDo� DrfDts3Dv�fDy9�D{33D}� D�3D�@ D�y�D�� D���D� D�6fD�` D��fD��fD��fD��fD�3D�#3D�FfD�i�D��3D���D���D��3D�	�D�9�D�i�D�� D���D���D�&fD�` D��3D���D��D�\�D�� D���D�  D�\�D���D�� D�fD�<�D�l�D��3D��3D���D�&fD�I�D�i�D�� D���D���D��fD�	�D�#3D�9�D�P D�i�D�|�D DÙ�DĬ�DŶfD��fD��fD���D��fD���D�	�D�fD�  D�,�D�6fD�<�D�FfD�S3D�c3D�l�Dր Dא DؖfDٜ�Dڠ Dۣ3Dܹ�D�� D��3D�ٚD�� D��D���D��fD���D�3D� D�3D�  D�0 D�@ D�S3D�ffD�y�DD� D�3D��3D��fD���D���D�	�D��D�0 D�@ D�6fD�I�D�ffD�s3D��3E H E � EX E� Eh E� Et�E�3E$�E0 E>fE	�fE
�3Ep E{3E��E�E3E�3E�3EfE!�E��E��E3E E{3EvfE� E VfE!I�E"� E$fE%�E&VfE'�fE)3E)�fE+S3E,��E.	�E.�fE0FfE1�fE2��E49�E5��E6p E7� E9fE:^fE;�3E>�3EB3ED� EG�fEKD�EN+3EQ� ET��EW��EZ�3E]��E`� EdVfEg<�Ejd�Emx Ep��Es�3Ev��Ez  E}3E� E��3E�H�E��3E�c3E��E��fE�fE���E�fE�| E��fE�3E�rfE��fE��E�^fE��fE��3E�BfE��fE�� E�E�E���E��3>L��>���>���>���>���>���>���=���>���>L��>���>���>���>���>���>���>���>���>���>���>L��>L��>���>���>���?333?��?333?�  ?���?�ff?���?�ff@ff@��@,��@L��@`  @y��@�ff@�33@�  @���@���@ə�@�ff@�ff@�ffA33A33A��AffA&ffA0  A9��A@  AK33AT��A\��Ah  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111144444414114444144144144141111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                            ?L��?�33@   @fff@�33@�  @�33@�  A  A  A.ffAI��Ak33A���A�  A�33A�33A���A���A�33A�33B��B
ffBffB  B"ffB*  B2ffB:ffBA��BJ  BRffBY��Bb  Bj��Br  By��B�  B�33B�ffB�ffB�33B�33B�33B�33B�33B�  B���B���B�  B�33B�  B�  B�  B�33B�  B�33B�  B�  B�  Bݙ�B�ffB�33B�  B�  B���B���B�33B�33C � CffCL�C��C� C
L�C��C��C�3C��C��CffC��C��C� C��C ��C"ffC$��C&ffC(L�C*ffC,��C.�3C0� C233C4ffC6��C8ffC:L�C<ffC>� C@��CB�3CD� CF33CHL�CJffCL��CN�3CP��CRffCT��CV�3CX� CZL�C\ffC^��C`�3Cb� CdffCf�3Ch��Cj� ClffCnL�Cp� Cr�3Ct� CvL�Cx� Cz� C|ffC~��C�Y�C�L�C�@ C�@ C�ffC�Y�C�L�C�@ C�33C�Y�C�L�C�@ C�@ C�33C�Y�C�L�C�@ C�ffC�Y�C�L�C�@ C�33C�@ C�33C�Y�C�Y�C�@ C�ffC�Y�C�@ C�Y�C�L�C�33C�L�C�ffC�Y�C�33C�@ C�Y�C�ffC�L�C�33C�L�C�ffC�L�C�@ C�L�C�ffC�@ C�33C�@ C�Y�C�Y�C�@ C�&fC�&fC�&fC�@ C�L�C�Y�C�@ C�&fC�33C�@ C�Y�C�ffC�Y�C�&fC�33C�@ C�L�C�Y�C�ffC�Y�C�&fC�33C�Y�C�Y�C�L�C�&fC�@ C�L�C�Y�C�Y�C�Y�C�L�C�&fC�33C�@ C�L�C�Y�C�Y�C�ffC�Y�C�&fC�@ C�L�C�L�C�Y�C�Y�C�@ C��C�33C�33C�L�C�Y�C�@ C�&fC�@ C�L�C�Y�C�ffC�L�C�&fC�33C�L�C�L�C�Y�C�Y�C�Y�C�@ C��C�33DffD��D�fD
Y�D&fD� D� D` D3D�3DS3D�fD"s3D$�fD'Y�D)��D,33D.� D1�D3� D6�D8�fD;33D=�fD@&fDB�3DEL�DG��DJ� DM�DO��DR9�DT��DWFfDY��D\L�D^��Da@ Dc�3Df&fDh�3Dj��DmffDo� Dr&fDt�3Dv�fDyY�D{S3D}� D�3D�P D���D�� D���D�  D�FfD�p D��fD��fD��fD��fD�3D�33D�VfD�y�D��3D���D���D��3D��D�I�D�y�D�� D�ɚD���D�6fD�p D��3D���D�)�D�l�D�� D���D�0 D�l�D���D�� D�fD�L�D�|�D��3D��3D��D�6fD�Y�D�y�D�� D���D���D��fD��D�33D�I�D�` D�y�D���D  Dé�Dļ�D��fD��fD��fD���D��fD�	�D��D�&fD�0 D�<�D�FfD�L�D�VfD�c3D�s3D�|�D֐ Dנ DئfD٬�Dڰ D۳3D�ɚD�� D��3D��D�� D���D���D�fD�	�D�3D�  D�#3D�0 D�@ D�P D�c3D�vfDDD� D��3D��3D��fD���D��D��D�,�D�@ D�P D�FfD�Y�D�vfD��3D��3E P E � E` E� Ep E� E|�E�3E,�E8 EFfE	�fE
�3Ex E�3E��E!�E#3E�3E�3E&fE)�E��E��E3E E�3E~fE� E ^fE!Q�E"� E$fE%	�E&^fE'�fE)3E)�fE+[3E,��E.�E.�fE0NfE1�fE2��E4A�E5��E6x E7� E9fE:ffE;�3E>�3EB3ED� EG�fEKL�EN33EQ� ET��EW��EZ�3E]��Ea  Ed^fEgD�Ejl�Em� EpɚEs�3Ev��Ez E}3E�  E��3E�L�E��3E�g3E��E��fE�fE���E�"fE�� E��fE�3E�vfE��fE��E�bfE��fE��3E�FfE��fE�� E�I�E���E��3?333G�O�G�O�G�O�G�O�G�O�G�O�?��G�O�?333?L��G�O�G�O�G�O�G�O�?L��G�O�G�O�?L��G�O�G�O�?333G�O�G�O�?fffG�O�?���?���?�  ?���?�ff@ff@33@&ff@9��@L��@l��@�  @���@�ff@�33@�  @���@���@ٙ�@�ff@�ffA33A33A33A��A&ffA.ffA8  AA��AH  AS33A\��Ad��Ap  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111144444414114444144144144141111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                            @ @ %@ �@ *@ �@ ""@ )�@ /�@ 6�@ =q@ F�@ R�@ `�@ m�@ z�@ ��@ ��@ ��@ �-@ �w@ �@ ��@ ��@ �q@j@�@�@-@:�@F�@UU@c�@o�@~K@�P@��@��@��@�>@є@�;@�4@��@�@*@"�@/�@<�@Ji@X�@g@t@��@�\@��@��@�@��@Ӡ@�H@��@��@
�@�@%�@2�@?}@O0@\�@i�@v�@��@�u@�m@��@�k@�o@�h@�`@�@�Q@V@�@(�@8�@D�@Q=@`B@l�@y�@��@��@�5@�~@�@�@�#@�m@�e@�@@g@-�@:@E�@S�@bN@qS@�@��@��@��@�F@@��@�/@�4@��@�@�@#�@0x@=q@Ji@Wb@ff@uk@��@��@�@��@��@�@��@��@��@��@J@B@&;@33@@,@O�@\�@i�@ww@�p@�$@�@�@��@��@׹@�@�@  @�@�@*S@6�@FQ@SI@_�@n�@{�@��@��@��@��@�w@��@��@�(@�q@	�@	�@	 �@	-@	:@	H]@	Wb@	b�@	o�@	~K@	�P@	��@	�A@	��@	�2@	��@	��@	�4@	��@
�@
@
!s@
/�@
>�@
M$@
Z@
e	@
s_@
��@
�@
�a@
��@
��@
Ĝ@
��@
�@
�L@
�E@�@�@&;@4�@B8@O�@\�@hs@v�@�@�u@��@�r@��@��@խ@�@�@ �@@�@(�@4�@C�@Q=@`B@n�@z�@�+@�0@��@��@�2@�|@�@�m@�q@@o@ @-�@:@E�@T�@�`@+�@t�@�w@
�@UU@�m@�@5?@|�@Ĝ@
�@P�@�u@�\@6@Z�@�@��@%�@i!@��@�e@7L@{�@�2@1@O�@�0@�/@#�@i!@�r@�@7�@|?@��@j@FQ@�7@�o@�@O0@�\@��@@R�@��@�o@�@O0@��@խ@�@X@��@�h@�@V�@�#@є@@K�@�7@�W@v@A�@|�@�@��@ 8�@ y�@ �^@ �,@!8�@!z3@!�@"  @"D�@"��@"�o@#b@#UU@#��@#��@$!s@$e	@$�A@$�y@%+�@%l�@%��@%�@&/@&n�@&��@&�(@'(�@'e�@'�(@'�;@([@(Yn@(��@(�7@)J@)F�@)��@)�^@)�@*-�@*g�@*��@*��@+o@+M$@+�+@+��@+�,@,2�@,k.@,�(@,��@-*@-O0@-��@-@-��@.4�@.l�@.��@.�#@/�@/N�@/�7@/�2@/�,@01�@0i!@0��@0�@1�@1K@1�d@1��@1��@2/�@2j@2�4@2��@3�@3UU@3�@3�@4�@4@,@4z3@4��@4�@@5(�@5b�@5��@5�C@6@6H]@6�d@6��@6��@7/�@7i�@7��@7��@8�@8�P@9<@9�@:!s@:�@;>�@;�@<]�@<�C@={�@=�y@>��@>��@?��@@V@@�!@A[@A�@B%�@B��@C/@C��@Di!@D��@Ei�@F�@Fg@F�}@G�C@H @H�p@IB@I��@JA�@J�(@K5�@K�J@LS�@L�`@Mv@M�
@Nff@N�@O�p@P@QV�@R�W@T�@UM�@V�^@W� @Yp�@Z�^@[��@]g�@^��@_��@al�@b�M@d@eQ�@f�@g��@iZ�@j��@k�@mQ�@n��@p�@q^�@r�f@s� @uZ�@v�@w��@yFP@y�0@y�@z�@zhr@z��@z��@{1�@{|�@{�f@{�e@|<@|�@|є@}�@}<�@ ^G�O�G�O�G�O�G�O�G�O�G�O�@  �G�O�@ ^@ G�O�G�O�G�O�G�O�@ G�O�G�O�@ G�O�G�O�@ ^G�O�G�O�@ �G�O�@ @ �@ �@ �@ �@ 
�@ J@ V@ b@ o@ �@ �@ �@ �@ g@ ""@ $�@ (G@ +@ -�@ 1'@ 4�@ 7�@ ;d@ ?}@ C�@ F�@ K@ O0@ Q�@ V�@ Z�@ ^5@ b�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A��A� �A�+A�+A�+A�/A�/A�33A�33A�7LA�=qA�A�A�=qA�9XA�7LA�7LA�9XA�=qA�A�A�E�A�E�A�G�A�E�A�A��;A�O�AƼjAƛ�A�(�A�%A�(�A�A���AüjAå�A�v�A�C�A�+A�bA�~�A�v�A�r�A��wA�Q�A���A���A�n�A��;A���A�r�A�jA���A��A��9A�jA�"�A�\)A�%A��A�bNA�C�A�&�A�v�A��A�(�A�oA���A�VA��RA�E�A��A�ȴA��
A�$�A���A�^5A��`A�`BA���A��A�z�A�C�A���A�bNA���A��A���A�^5A�O�A��TA��A��\A�"�A�1A���A��A�9XA��+A��\A���A�I�A���A�|�A��DA���A���A���A�x�A�-A�?}A��+A��A�/A��A���A��/A�I�A��jA�\)A�A��!A�XA��`A��-A���A��jA�=qA���A�p�A�JA�$�A~�A{�#Az�Ay��AxbNAw&�AuƨAt(�Ap�AoK�An�Am/AkXAi�PAgp�AfjAd�HAc�AbffA_O�A]S�A\(�A[�7A[K�AZn�AYAX�9AV��AT�`AQ�TAO��AN�DAL�AJ�AIx�AH(�AG��AF��AFn�AC�#AB1AAp�A@Q�A?�PA>~�A=�TA=A=p�A=%A;��A:�`A9��A8z�A7�TA6�HA5�A3�A1A0ȴA/?}A.A,�jA+p�A*�/A)��A(��A'A&9XA%��A$�A"�A"�A!hsA ^5A�A7LAbAE�A�`A�TA�HA=qA��A��A%AA�A33AƨA��A�A��A�A��A�jAjAƨA	�7A��A`BA{A��A33A�Ar�A(�A�AA��A�AE�A r�@��@�K�@���@�@�S�@��@�w@�C�@��@�+@���@�z�@�@���@١�@��@���@ύP@�(�@Ɂ@���@��h@�o@��T@�`B@���@�b@�`B@���@�v�@�K�@���@���@�G�@�+@�x�@��F@�n�@��T@�V@��!@�E�@�p�@�Z@�\)@��#@��D@���@��h@�`B@�A�@��@��h@�Ĝ@��
@�o@���@�V@��@��;@���@�E�@��@�z�@�1'@~V@|�@{��@z�@x��@v�y@t�j@sS�@p�`@n�R@l�@j�!@i&�@g��@e�-@d�@cS�@a��@_
=@^@^@\Z@[o@XA�@Vv�@T��@T1@P�9@O�P@M�T@L��@Kƨ@J�\@H�`@F�@Fv�@F{@Ep�@Cƨ@B�@A�@@bN@?��@>�@=O�@<�j@:��@9x�@8Q�@7;d@5��@5O�@49X@333@3C�@2~�@1�7@0�`@.�@.$�@-`B@+ƨ@*�@*~�@*-@(bN@'�w@'
=@&�+@%�@$��@#��@"��@!x�@ bN@�@��@�h@��@(�@t�@~�@�@G�@�`@ �@
=@��@E�@�@�j@9X@�@"�@�\@G�@�@A�@K�@�@{@O�@Z@�
@o@
n�@
J@	x�@��@�@�@K�@�R@p�@�@�@(�@��@^5@-@hs@�@ Q�?��?�dZ?��?�l�?���?�o?��?�|�?�1?�=q?��?�K�?�$�?���?���?�o?�&�?�A�?���?ޗ�?ܬ?�"�?��#?�1'?�K�?֧�?Ձ?�z�?ӕ�?��?�-?�&�?�A�?Ͼw?�\)?�v�?���?͑h?��?̋D?�1?�ƨ?�dZ?�~�?���?�
=?�$�?��?�33?���?�bN?��?�v�?��?��-?���?�j?�1?�ƨ?��?�ƨ?�ƨ?�ƨ?��?���?�ƨ?�ƨ?�I�?��?�V?�p�?�{?���?�;d?��;?�  ?� �?�A�?�bN?��?���?���?�Ĝ?�Ĝ?��`?�%?�&�?�&�?�G�?�G�A� �A� �A� �A��A��A�$�A�(�A�(�A�$�A�"�A�$�A�"�A� �A� �A� �A��A��A��A��A��A�{A�{A�{A��A��A��A��A�"�A�$�A�"�A�&�A�&�A�(�A�+A�-A�-A�+A�+A�+A�+A�-A�/A�/A�/A�/A�1'A�33A�33A�33A�5?A�7LA�;dA�=qA�A�A�?}A�?}A�C�A�C�A�?}A�;dG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                            A��A� �A�+A�+A�+A�/A�/A�33A�33A�7LA�=qA�A�A�=qA�9XA�7LA�7LA�9XA�=qA�A�A�E�A�E�A�G�A�E�A�A��;A�O�AƼjAƛ�A�(�A�%A�(�A�A���AüjAå�A�v�A�C�A�+A�bA�~�A�v�A�r�A��wA�Q�A���A���A�n�A��;A���A�r�A�jA���A��A��9A�jA�"�A�\)A�%A��A�bNA�C�A�&�A�v�A��A�(�A�oA���A�VA��RA�E�A��A�ȴA��
A�$�A���A�^5A��`A�`BA���A��A�z�A�C�A���A�bNA���A��A���A�^5A�O�A��TA��A��\A�"�A�1A���A��A�9XA��+A��\A���A�I�A���A�|�A��DA���A���A���A�x�A�-A�?}A��+A��A�/A��A���A��/A�I�A��jA�\)A�A��!A�XA��`A��-A���A��jA�=qA���A�p�A�JA�$�A~�A{�#Az�Ay��AxbNAw&�AuƨAt(�Ap�AoK�An�Am/AkXAi�PAgp�AfjAd�HAc�AbffA_O�A]S�A\(�A[�7A[K�AZn�AYAX�9AV��AT�`AQ�TAO��AN�DAL�AJ�AIx�AH(�AG��AF��AFn�AC�#AB1AAp�A@Q�A?�PA>~�A=�TA=A=p�A=%A;��A:�`A9��A8z�A7�TA6�HA5�A3�A1A0ȴA/?}A.A,�jA+p�A*�/A)��A(��A'A&9XA%��A$�A"�A"�A!hsA ^5A�A7LAbAE�A�`A�TA�HA=qA��A��A%AA�A33AƨA��A�A��A�A��A�jAjAƨA	�7A��A`BA{A��A33A�Ar�A(�A�AA��A�AE�A r�@��@�K�@���@�@�S�@��@�w@�C�@��@�+@���@�z�@�@���@١�@��@���@ύP@�(�@Ɂ@���@��h@�o@��T@�`B@���@�b@�`B@���@�v�@�K�@���@���@�G�@�+@�x�@��F@�n�@��T@�V@��!@�E�@�p�@�Z@�\)@��#@��D@���@��h@�`B@�A�@��@��h@�Ĝ@��
@�o@���@�V@��@��;@���@�E�@��@�z�@�1'@~V@|�@{��@z�@x��@v�y@t�j@sS�@p�`@n�R@l�@j�!@i&�@g��@e�-@d�@cS�@a��@_
=@^@^@\Z@[o@XA�@Vv�@T��@T1@P�9@O�P@M�T@L��@Kƨ@J�\@H�`@F�@Fv�@F{@Ep�@Cƨ@B�@A�@@bN@?��@>�@=O�@<�j@:��@9x�@8Q�@7;d@5��@5O�@49X@333@3C�@2~�@1�7@0�`@.�@.$�@-`B@+ƨ@*�@*~�@*-@(bN@'�w@'
=@&�+@%�@$��@#��@"��@!x�@ bN@�@��@�h@��@(�@t�@~�@�@G�@�`@ �@
=@��@E�@�@�j@9X@�@"�@�\@G�@�@A�@K�@�@{@O�@Z@�
@o@
n�@
J@	x�@��@�@�@K�@�R@p�@�@�@(�@��@^5@-@hs@�@ Q�?��?�dZ?��?�l�?���?�o?��?�|�?�1?�=q?��?�K�?�$�?���?���?�o?�&�?�A�?���?ޗ�?ܬ?�"�?��#?�1'?�K�?֧�?Ձ?�z�?ӕ�?��?�-?�&�?�A�?Ͼw?�\)?�v�?���?͑h?��?̋D?�1?�ƨ?�dZ?�~�?���?�
=?�$�?��?�33?���?�bN?��?�v�?��?��-?���?�j?�1?�ƨ?��?�ƨ?�ƨ?�ƨ?��?���?�ƨ?�ƨ?�I�?��?�V?�p�?�{?���?�;d?��;?�  ?� �?�A�?�bN?��?���?���?�Ĝ?�Ĝ?��`?�%?�&�?�&�?�G�?�G�A� �A� �A� �A��A��A�$�A�(�A�(�A�$�A�"�A�$�A�"�A� �A� �A� �A��A��A��A��A��A�{A�{A�{A��A��A��A��A�"�A�$�A�"�A�&�A�&�A�(�A�+A�-A�-A�+A�+A�+A�+A�-A�/A�/A�/A�/A�1'A�33A�33A�33A�5?A�7LA�;dA�=qA�A�A�?}A�?}A�C�A�C�A�?}A�;dG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                            ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B�B�B �B�B �B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B �B0!B<jBK�BM�B^5BaHB|�BǮB�B1'B49B>wBB�B7LB8RB6FB5?B.B33B6FB>wBH�BT�B\)BdZBhsBo�Bv�Bv�Bx�Bz�B�+B�B�B�B�DB�JB�VB�oB�bB��B��B��B��B��B��B��B��B�bB�1B�%B�%B�B~�Bw�Bn�BjBgmBdZB]/BVBP�BM�BJ�BI�BA�B7LB33B2-B:^B7LB49B#�B"�BhBDB�B�NB��B��B�9B��B�=B�Bz�Bm�BaHBI�B<jB'�B�B
��B
��B
�B
�B
�B
�fB
�NB
�/B
�)B
��B
ĜB
�dB
��B
��B
�%B
v�B
gmB
R�B
J�B
B�B
8RB
0!B
"�B
�B
%B	��B	��B	�B	�NB	�B	��B	ȴB	�jB	�RB	��B	��B	�VB	�+B	�B	�B	z�B	w�B	m�B	e`B	VB	F�B	<jB	33B	$�B	�B	oB	JB	1B	B	B�B�B�mB�TB�/B�
B��B��B��B��BɺBĜBB�dB�XB�-B�!B��B��B��B��B�oB�hB�JB�=B�B}�Bx�Bp�Bm�BiyBhsBdZBcTBaHBaHB_;B[#B[#B[#BYB\)BYB[#B[#B[#B[#BYB[#BYB\)B\)B\)B[#BZBXBW
B\)B]/BYBT�BP�BN�BQ�BN�BM�BK�BK�BL�BXBffB[#BS�BR�BJ�BC�B<jB9XB5?B8RB5?B6FB49B5?B49B5?BD�BA�BG�BS�B`BB�1B�bB��B��B�B�!B�-B�RBŢB��BÖBɺB�sB��B	B		7B	"�B	49B	>wB	33B	Q�B	YB	iyB	s�B	� B	�B	�=B	�VB	��B	��B	�?B	�RB	�}B	ȴB	��B	��B	�
B	�/B	�NB	�NB	�fB	�sB	�B	�B	�B	�B	��B	��B	��B	��B	��B
  B
B
B
1B
PB
PB
hB
uB
{B
�B
�B
�B
�B
�B
�B
�B
�B
!�B
#�B
%�B
&�B
&�B
(�B
)�B
+B
,B
.B
/B
2-B
49B
49B
49B
6FB
8RB
8RB
9XB
;dB
;dB
<jB
>wB
?}B
?}B
@�B
B�B
D�B
E�B
D�B
F�B
G�B
J�B
K�B
L�B
L�B
N�B
O�B
O�B
P�B
Q�B
R�B
R�B
R�B
R�B
S�B
VB
VB
XB
ZB
[#B
\)B
]/B
]/B
]/B
`BB
`BB
aHB
aHB
cTB
cTB
cTB
bNB
dZB
e`B
ffB
gmB
hsB
hsB
hsB
iyB
iyB
jB
k�B
k�B
m�B
m�B
n�B
o�B
p�B
q�B
r�B
r�B
r�B
s�B
t�B
u�B
v�B
v�B
v�B
v�B
v�B
x�B
x�B
y�B
z�B
{�B
{�B
{�B
|�B
}�B
� B
�B
�B
�B
�%B
�%B
�1B
�1B
�DB
�JB
�PB
�VB
�\B
�bB
�hB
�oB
�uB
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
�B
�B
�B
�!B
�!B
�'B
�-B
�3B
�9B
�?B
�?B
�?B
�?B
�FB
�FB
�LB
�LB
�LB
�RB
�RB
�XB
�XB
�XB
�RB
�XB
�XB
�XB
�XB
�^B
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
�^B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B �B�B�B�B �B�B�B�B�B �B �B�B�B�B�B�B �B �B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                            B�B�B �B�B �B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B �B0B<MBK�BM�B^Ba-B|�BǔB�B1B4!B>`BBxB76B8<B61B5*B. B3B63B>dBH�BT�B\BdIBhcBo�Bv�Bv�Bx�Bz�B�B�B�B�B�9B�@B�LB�fB�YB�B��B��B��B��B��B��B��B�^B�.B�"B�#B�B~�Bw�Bn�BjBgnBd\B]1BVBP�BM�BJ�BI�BA�B7RB3:B24B:fB7TB4BB#�B"�BrBOB�B�YB��B��B�FB��B�JB�Bz�Bm�BaWBI�B<zB(B�B
��B
��B
�B
�B
�B
�zB
�cB
�DB
�?B
��B
ĲB
�{B
��B
��B
�=B
v�B
g�B
SB
J�B
B�B
8mB
0<B
"�B
�B
AB	�B	��B	�B	�lB	�/B	��B	��B	��B	�sB	�B	��B	�xB	�MB	�BB	�)B	{B	w�B	m�B	e�B	V*B	F�B	<�B	3ZB	%B	�B	�B	sB	ZB	<B	0B�B�B�B�B�[B�7B� B�B�B�B��B��B��B��B��B�_B�TB�B�B��B��B��B��B��B�sB�IB~+ByBp�Bm�Bi�Bh�Bd�Bc�Ba�Ba�B_wB[_B[`B[`BYUB\gBYVB[bB[cB[cB[dBYXB[eBYYB\lB\lB\mB[gBZbBXUBWPB\oB]vBY^BUFBQ-BO!BR5BO"BNBLBLBMBX\Bf�B[pBTFBS@BKBC�B<�B9�B5�B8�B5�B6�B4�B5�B4�B5�BD�BA�BHBTZB`�B��B��B�B�=B�wB��B��B��B�"BՁB�B�BB��B�uB	�B		�B	#hB	4�B	?B	3�B	R�B	Y�B	j"B	tbB	��B	��B	��B	�B	�nB	��B	�B	�B	�EB	�~B	ˎB	ϩB	��B	�B	�'B	�*B	�EB	�TB	�cB	�lB	�B	�B	��B	��B	��B	��B	��B
 �B
B
B
	7B
XB
[B
vB
�B
�B
�B
�B
�B
�B
�B
�B
�B
 �B
"�B
%B
'B
(B
( B
*0B
+9B
,BB
-KB
/ZB
0dB
3yB
5�B
5�B
5�B
7�B
9�B
9�B
:�B
<�B
<�B
=�B
?�B
@�B
@�B
A�B
DB
FB
GB
FB
H(B
I1B
LGB
MOB
NXB
NZB
PiB
QqB
QtB
R}B
S�B
T�B
T�B
T�B
T�B
U�B
W�B
W�B
Y�B
[�B
\�B
]�B
^�B
^�B
^�B
bB
bB
cB
cB
eB
e B
e#B
dB
f.B
g6B
h?B
iHB
jQB
jSB
jVB
k^B
kaB
liB
mrB
mtB
o�B
o�B
p�B
q�B
r�B
s�B
t�B
t�B
t�B
u�B
v�B
w�B
x�B
x�B
x�B
x�B
x�B
z�B
z�B
{�B
}B
~B
~B
~B
B
�)B
�<B
�MB
�SB
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
��B
�B
�B
�B
�$B
�/B
�<B
�IB
�ZB
�gB
�nB
�rB
�B
��B
��B
��B
��B
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
�B
�B
�*B
�?B
�VB
�jB
�B
��B
��B
��B
��B
��B
� B
�B
�*B
�@B
�UB
�dB
�sB
��B
��B
��B
��B
��B
��B
��B
�B
�B
�&B
�4B
�>B
�SB
�bB
�qB
�uB
�B
�{B
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
��B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B �B�B�B�B �B�B�B�B�B �B �B�B�B�B�B�B �B �B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                            <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�201807242202492021061413552320210614135523202106171312072021061713120720210617131207201807242202492021061413552320210614135523202106171312072021061713120720210617131207PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            new conductivity = original conductivity * (1 + delta*TEMP + CPcor_SBE*PRES) / (1 + delta*TEMP_ADJUSTED + CPcor_new*PRES_ADJUSTED)                                                                                                                              none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            new conductivity = original conductivity * (1 + delta*TEMP + CPcor_SBE*PRES) / (1 + delta*TEMP_ADJUSTED + CPcor_new*PRES_ADJUSTED)                                                                                                                              none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.50 dbar                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            CPcor_new = -1.25e-07; CPcor_SBE = -9.57e-08; delta = 3.25e-06                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.50 dbar                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            CPcor_new = -1.25e-07; CPcor_SBE = -9.57e-08; delta = 3.25e-06                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                      none                                                                                                                                                                                                                                                            New conductivity computed by using a different CPcor value from that provided by Sea-Bird.                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            No significant salinity drift detected.                                                                                                                                                                                                                         Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                      none                                                                                                                                                                                                                                                            New conductivity computed by using a different CPcor value from that provided by Sea-Bird.                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            No significant salinity drift detected.                                                                                                                                                                                                                         PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            AO  AO  ARCAARCAADJPADJP                                                                                                                                        2018072422024920180724220249  IP  IP                                G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2018072422024920180724220249QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�5F03E           703E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2018072422024920180724220249QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�0               0               PM  PM  ARSQARSQCOWGCOWGV1.1V1.1                                                                                                                                2021061713145620210617131456IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                