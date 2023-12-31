CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS     N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-08-13T17:04:00Z creation      
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
resolution        =���   axis      Z        (  ;�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   K�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     (  P   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   `0   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     (  d<   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     (  td   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   ��   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     (  ��   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     (  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     (  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   �   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     (  �(   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   �P   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     (  �\   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                   
�   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                   
�   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                   
�   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                      HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  �    HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                   �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                   �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar        �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar        �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�       �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  �   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �,   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �,   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �,   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  � 
,Argo profile    3.1 1.2 19500101000000  20180813170400  20210617131500  5905739 5905739 US ARGO PROJECT                                                 US ARGO PROJECT                                                 GREGORY C. JOHNSON                                              GREGORY C. JOHNSON                                              PRES            TEMP            PSAL            PRES            TEMP            PSAL                  DD  AOAO7219                            7219                            2C  2C  DD  SOLO_D_MRV                      SOLO_D_MRV                      12002                           12002                           SBE603 01Dec16                  SBE603 01Dec16                  874 874 @�vXs|�@�vXs|�11  @�vXq�(�@�vXq�(�@6�vݬ��@6�vݬ���c�M����c�M���11  GPS     GPS     Primary sampling: averaged [1Hz sampling by SBE-41CP averaged in 2-dbar bins]                                                                                                                                                                                   Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 AB  AA  AA  >���?fff?�33@Fff@�  @�  @�  @�33@���A  A#33A>ffA`  A���A�  A�  A�33A���Aљ�A���A�  B   B��B��B  B ��B(  B/��B8  B@��BHffBPffBX  B_��Bh  Bp  Bx  B�33B�33B�  B�  B���B�  B�33B�33B�33B�ffB�33B�33B�33B�  B���B���B�33B�33BǙ�B˙�B�  Bԙ�B�ffB�  B���B�33B���B�33B�33B�33B�  B�  C 33C�C�fC33C�C
  C  C�fC�fC�fC�C33C�C�C  C  C�fC"L�C$�C&�C'�fC)�3C,  C.33C0  C1��C4  C633C8ffC:�C;�fC>  C@�CB�CD�CF�CHL�CJ�CK��CN  CP33CR�CS��CU�fCX  CZL�C\�C]�fC`�CbL�Cd33Ce�fCh�Cj33Cl�Cm�fCp�Cr  Cs�fCv�Cx33Cz�C{�fC~33C��C��C��C��3C��C��C��C��C�  C�  C��fC�  C��C��C��3C��C��C��C��fC��3C��3C��C��C��C��fC��3C��3C��C��C��C��C��C��C��C��C��C��C��C��C��C�  C��3C��3C�  C��C�&fC��C��C��C��C��C�&fC�&fC��C��fC�  C��C��C�  C�  C��C�&fC��C��fC��3C��C��3C��fC��C��C��C�  C�&fC��C��C��C�  C�&fC��C�  C��3C��fC��C��C��C�  C�  C��C��C��C�&fC��C�  C��C�&fC��C�  C��C��C��3C��C�&fC��C��fC��3C��3C�  C��3C��3C��3C��fC��C�&fC�&fC�&fC�&fC�&fC�&fC��C��C�  C�  C��3C��fC��fC��C��C�  C��3D s3D �fD9�Dy�D	� DL�D� DY�D  D�3DY�D�D�fD!��D$@ D&��D)�fD,@ D.�fD1ffD3�3D6s3D8��D;` D=��D@9�DB��DD��DGS3DI� DL,�DN�3DQ33DS� DVFfDX�fD[FfD]��D`L�Db�fDeL�Dg� Dj9�Dl� Do3Dqs3Ds��Dv  Dx�fDz�fD|ٚD33D���D��fD�  D�I�D�p D�� D���D���D�&fD�S3D�� D���D�ٚD�	�D�9�D�i�D���D��3D�fD�C3D�� D��fD��D��D�FfD�y�D���D��3D� D�@ D�l�D�� D��3D�� D��D�I�D�y�D�� D��fD��D� D�0 D�S3D�y�D���D�� D��D� D�<�D�c3D���D���D���D��D�FfD�vfD©�D�ٚD��D�<�D�l�DȠ D���D���D�0 D�` DΓ3D�ɚD���D�  D�@ D�i�DՐ DֶfD���D���D�  D�I�D�l�D݉�Dެ�D��fD���D�� D�3D��D�0 D�@ D�VfD�ffD�|�D� D��D쩚D��fD�� D�� D��fD�� D���D�  D��D�fD�#3D�9�D�FfD�VfD�P D�c3D�s3D��3D��3E S3E ��Ed�E� E{3EfE� E�E�3E$�E6fE9�E>fE	��E
��E33E�3E��E�fE��ET�E�3E�fE E|�El�E�fE0 E�fEl�E��E ,�E!�3E"~fE#� E%A�E&)�E'�fE(�fE*@ E+,�E,� E.�E.�fE0[3E1��E2� E4 E5{3E6� E7� E9  E:q�E;� E>� EA��ED��EH9�EK;3END�EQA�ET� EW��EZ� E]� Ea4�EdX Eg#3Ej�3Em� Ep� Es� Ev��Ez�E} E�!�E���E�L�E���E�K3E��3E�6fE��3E��fE�'3E�� E�ɚE��E�jfE�� E� �>���?   >���>���?   >���>���>���?   >���?   >���>���?   ?   ?��?   ?333?333?fff?fff?���?���?�  ?���?�ff@   @33@&ff@9��@L��@`  @l��@�  @���@�ff@�  @���@�ff@�  @���@�ff@�33@���@�ffA��A33A  AffA��A#33A+33A333A9��AA��AH  AQ��AX  A`  AfffG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141141414144141414141411111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                           ?fff?�33@��@fff@�  @�  @�  @�33AffA  A+33AFffAh  A���A�  A�  A�33A���Aՙ�A���A�  B  B	��B��B  B"��B*  B1��B:  BB��BJffBRffBZ  Ba��Bj  Br  Bz  B�33B�33B�  B�  B���B�  B�33B�33B�33B�ffB�33B�33B�33B�  B���B���B�33B�33Bș�B̙�B�  Bՙ�B�ffB�  BᙚB�33B���B�33B�33B�33B�  B�  C �3C��CffC�3C��C
� C� CffCffCffC��C�3C��C��C� C� C ffC"��C$��C&��C(ffC*33C,� C.�3C0� C2L�C4� C6�3C8�fC:��C<ffC>� C@��CB��CD��CF��CH��CJ��CLL�CN� CP�3CR��CTL�CVffCX� CZ��C\��C^ffC`��Cb��Cd�3CfffCh��Cj�3Cl��CnffCp��Cr� CtffCv��Cx�3Cz��C|ffC~�3C�Y�C�L�C�L�C�33C�Y�C�Y�C�Y�C�Y�C�@ C�@ C�&fC�@ C�Y�C�L�C�33C�L�C�Y�C�L�C�&fC�33C�33C�L�C�Y�C�L�C�&fC�33C�33C�L�C�L�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�L�C�L�C�@ C�33C�33C�@ C�Y�C�ffC�Y�C�Y�C�Y�C�Y�C�Y�C�ffC�ffC�L�C�&fC�@ C�L�C�Y�C�@ C�@ C�L�C�ffC�L�C�&fC�33C�L�C�33C�&fC�L�C�Y�C�Y�C�@ C�ffC�Y�C�L�C�L�C�@ C�ffC�L�C�@ C�33C�&fC�L�C�L�C�L�C�@ C�@ C�Y�C�Y�C�L�C�ffC�Y�C�@ C�Y�C�ffC�Y�C�@ C�Y�C�L�C�33C�L�C�ffC�L�C�&fC�33C�33C�@ C�33C�33C�33C�&fC�L�C�ffC�ffC�ffC�ffC�ffC�ffC�Y�C�Y�C�@ C�@ C�33C�&fC�&fC�Y�C�L�C�@ D �D �3DfDY�D��D
  Dl�D� Dy�D  D�3Dy�D9�D�fD!��D$` D'�D)�fD,` D/fD1�fD43D6�3D9�D;� D=��D@Y�DB��DE�DGs3DI� DLL�DN�3DQS3DS� DVffDX�fD[ffD]��D`l�Db�fDel�Dg� DjY�Dl� Do33Dq�3Ds��Dv@ Dx�fD{fD|��DS3D���D�fD�0 D�Y�D�� D�� D���D�	�D�6fD�c3D�� D���D��D��D�I�D�y�D���D��3D�fD�S3D�� D��fD���D�,�D�VfD���D���D��3D�  D�P D�|�D�� D��3D�  D�)�D�Y�D���D�� D��fD���D�  D�@ D�c3D���D���D�� D���D�  D�L�D�s3D���D���D���D�)�D�VfD��fD¹�D��D��D�L�D�|�DȰ D���D�	�D�@ D�p DΣ3D�ٚD���D�0 D�P D�y�Dՠ D��fD���D��D�0 D�Y�D�|�Dݙ�D޼�D��fD���D�  D�3D�,�D�@ D�P D�ffD�vfD��D� D��D칚D��fD�� D�� D��fD�� D���D� D��D�&fD�33D�I�D�VfD�ffD�` D�s3D��3D��3D��3E [3E ��El�E� E�3EfE� E�E�3E,�E>fEA�EFfE	ɚE
��E;3E�3E��EfE��E\�E�3E�fE  E��Et�E�fE8 E�fEt�E��E 4�E!�3E"�fE#� E%I�E&1�E'�fE(�fE*H E+4�E,� E.	�E.�fE0c3E1��E2� E4  E5�3E6� E7� E9( E:y�E;� E>� EA��ED��EHA�EKC3ENL�EQI�ET� EW��EZ� E]� Ea<�Ed` Eg+3Ej�3Em� Ep� Es� Ev��Ez�E} E�%�E���E�P�E���E�O3E��3E�:fE��3E��fE�+3E�� E�͚E��E�nfE�� E��?fffG�O�?L��?fffG�O�?L��G�O�?fffG�O�?fffG�O�G�O�?fffG�O�?�  G�O�?�  G�O�?���G�O�?�33G�O�?���@   @ff@33@   @333@Fff@Y��@l��@�  @�ff@�  @���@�ff@�  @���@�ff@�  @���@�ff@�33@���A33A	��A33A  AffA$��A+33A333A;33AA��AI��AP  AY��A`  Ah  AnffG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141141414144141414141411111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                           @ �@ %@ �@ *@ O@ ""@ (�@ 0x@ 5�@ =q@ E�@ Q=@ _�@ m�@ z�@ ��@ ��@ ��@ ��@ ��@ ��@ �t@ �m@ �@j@o@�@+�@:@I@V@c�@p�@}�@��@��@�A@��@�>@�7@��@��@�,@�@*@"�@1'@>@K�@Yn@ff@s_@�@�@��@�M@��@��@խ@�@��@��@
�@6@&;@3�@A�@N�@\)@k.@x&@�p@�#@�@�@��@ȴ@�[@�@�@^@V@�@(�@6�@C�@S�@`B@m�@z3@�|@�0@�5@�~@��@��@��@��@�q@�@@g@-@:�@H]@Wb@c�@oF@~K@�P@�H@��@�9@@�C@ލ@��@��@�@�@!s@0x@>�@K�@X@g@t@�@�@�a@�Y@��@�W@��@��@�@��@�@B@&�@4�@@�@N�@Z�@i�@x�@��@�@�@�r@�k@�@�[@�@�@^@V@�@(G@5�@D�@R�@`�@n�@|?@��@��@�5@��@��@�*@�#@��@��@	�@	b@	�@	-�@	<@	I@	V�@	dZ@	r@	�@	��@	��@	��@	��@	@	��@	�;@	�@	�,@
�@
�@
"�@
.l@
<�@
K�@
X@
e	@
t�@
�@
��@
�@
��@
��@
ƨ@
�O@
�H@
��@
�E@
=@6@$.@3�@A�@O0@\)@i�@x�@�|@�u@�y@�r@��@��@�@�@�Y@^@V@�@)�@8�@D�@P�@^�@l�@z�@��@��@�(@�!@��@��@܀@�(@��@v@@ @-�@:@G�@T�@a�@oF@�@��@��@��@��@��@5�@s_@��@� @:@�@�c@�@[z@��@�@;d@�@ψ@�@_�@��@�4@1�@v@��@��@>�@�@��@�@B�@�@�W@J@P�@�0@�#@g@c�@��@��@0x@uk@�R@��@=q@�W@�2@^@@�@�d@�>@�~@8�@z3@��@�,@8�@ww@�R@�~@8�@x�@�@�,@8�@y�@�^@�9@<@}�@��@ ]@ D�@ ��@ ��@!J@!M�@!�P@!��@"b@"R�@"��@"Ӡ@#�@#UU@#�u@#Ӡ@$@$S�@$��@$Ӡ@%o@%P�@%�\@%��@&
�@&I�@&�+@&��@'v@'DD@'�p@'�>@(�@(C�@(�p@(Ĝ@)�@)E�@)�+@)�@*	�@*Ji@*�D@*��@+�@+M$@+�\@+�7@,�@,S�@,�@,Ӡ@-@-P�@-�\@-�*@.�@.Ji@.��@.�@/%@/B�@/�@/�@/�~@033@0m�@0��@0�@1�@1Z@1�#@1ψ@2
=@2C�@2|�@2�F@2��@3(�@3`�@3��@3��@4�@4F�@4�@4�@4�e@5-�@5g�@5�@5׹@6�@6K�@6��@6��@6�9@75@@7p�@7�@7�l@8""@8Z�@8�#@8��@9C�@9�-@:!s@:ƨ@;1�@;є@<n�@<խ@=o�@=խ@>oF@?1@?o�@@
=@@�z@A�@A��@B6�@Bȴ@C+@C�2@DWb@D��@ET�@E��@F�d@F�`@Gz2@H�@H��@I�@I��@J> @J��@K>�@K�@L=q@L�
@Mn�@N�@Ni�@N��@O��@P�@QUU@R�!@S�9@Um�@V�F@X@YH]@Z��@\�@]N�@^�~@`�@am:@b�a@d@eUU@f�R@h�@iZ�@j�!@k��@mV�@n�	@p
�@qV@r��@s�@t<@t��@t�C@u	�@u\)@u�$@u�+@vZ@vm:@v��@ �G�O�@ @ �G�O�@ G�O�@ �G�O�@ �G�O�G�O�@ �G�O�@ jG�O�@ jG�O�@ �G�O�@ %G�O�@ �@ 
=@ 
�@ J@ �@ �@ �@ �@ �@ �@ B@ O@ [@  @ ""@ $.@ &�@ (�@ +�@ -�@ 0x@ 2�@ 4�@ 7L@ ;d@ =q@ @,@ B�@ E�@ I@ Lu@ O0@ R�@ UU@ Yn@ \)@ _�@ bNG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�r�A�n�A�t�AԃAԍPAԍPAԋDAԋDAԏ\Aԏ\AԓuAԏ\AԑhAԋDAԉ7A�t�A�^5A��/A�+A�ĜA���A���AѸRA��A�^5A�JA�ƨA̓uA�{A���A�jA�E�A���Aʗ�A�v�A�t�A¥�A��;A���A�O�A��hA��A��!A�x�A�p�A���A�1'A��;A���A��+A�t�A�XA��
A��A��9A�=qA��!A�dZA���A�l�A��!A��A��A��mA�ȴA�n�A��^A�/A��!A���A���A��FA��/A�jA�+A���A�ȴA��jA�`BA��mA�C�A��A���A��yA�jA�JA�jA���A�K�A�5?A�A��A�-A�v�A���A���A��+A���A��+A�hsA��FA��7A�33A��;A��A�E�A�A��\A�I�A��wA�v�A��/A���A�C�A���A�1A~��A|�RA{�AzM�Ax��AwdZAu�FAs�ApM�AoXAo�Am��Al  Aj  Ah�uAg?}AfI�AehsAd9XAb��Ab�A`bNA_��A_\)A^M�A]7LA[�mAY��AX�`AXJAW\)AU�AUt�AU�ATVAT�ATAS|�ARM�AQ�AO��ANE�ALȴAK�#AI��AH�AG`BAF1AD�RACdZAAƨA@�/A??}A>�A=C�A;�A:�A:-A7��A6�jA4A1�A0��A0bNA.��A.I�A.�A-\)A+VA)XA((�A'��A'\)A&��A&�+A%��A$I�A#+A!�A ��A (�AM�A��A�HA��A?}A��AI�A�A�A/A-AoAȴA�-AVA(�A?}AQ�A�mA�^A��A�A��A�yA$�AS�A��A\)A
A	;dAr�AE�A�At�A��AbNA�7A��A��A��AffA�#A �HA ĜA =q@��R@� �@�1'@�o@���@���@�D@�C�@��y@�ff@�x�@��@���@�r�@� �@�ƨ@�\)@�
=@��^@��@�b@���@��D@�r�@�~�@�
=@�7L@���@��F@��@�&�@��H@�j@�|�@�(�@��@�%@��R@���@���@�O�@�G�@���@�;d@�O�@���@�E�@��@��^@�5?@���@���@�-@���@���@�E�@�hs@�1@���@�@��/@� �@��w@��y@��-@��j@��@��!@��h@��j@� �@���@��@�v�@���@��9@��@~@|9X@{��@x��@x1'@v5?@u?}@s��@q�7@o\)@n{@l�j@k"�@hA�@fE�@c��@a��@_��@^E�@]O�@\��@[�
@X��@W�w@V��@TZ@S33@Q��@P�@N{@M?}@KdZ@J~�@I&�@G��@F��@FE�@D1@A�@?;d@=@<�/@;ƨ@:��@9�@8�9@7�P@7;d@5�@5V@3�m@2��@0bN@/�P@.V@,�@,9X@+�m@*�@*J@)�@(��@(bN@'��@&�R@%�@%V@$�@#��@"��@"n�@!�@!�7@ ��@ Q�@l�@�+@��@�m@�F@�@�@7L@bN@+@$�@O�@V@1@S�@@^5@�@hs@1'@�@�R@�T@�@��@�@�
@
^5@	�#@	��@	�^@	7L@r�@A�@�w@|�@�+@�T@�@��@Z@@^5@�@ ��?�|�?���?�7L?��?���?��?��?�h?�ƨ?��#?�?�+?�?㕁?�&�?�A�?߾w?�{?�/?�C�?�^5?�r�?�l�?�$�?�?Լj?ӕ�?�o?�hs?�&�?У�?�  ?�;d?�v�?�V?�p�?�/?���?�1?�?�^5?ȴ9?�ff?�z�?��
?�M�?���?��;?��R?�{?��D?�I�?��m?�dZ?���?���?���?���?���?���?��H?�C�?��?��?��m?�j?���?�V?�/?�O�?�O�?��h?��-?���?��?�{?�V?�VA�p�A�p�A�n�A�r�A�r�A�r�A�r�A�r�A�p�A�x�A�r�A�t�A�r�A�r�A�p�A�n�A�n�A�p�A�l�A�l�A�n�A�n�A�n�A�r�A�t�A�t�A�v�A�t�A�|�AԃAԇ+Aԇ+AԍPAԍPAԏ\AԍPAԍPAԍPAԋDAԋDAԉ7AԋDAԋDAԉ7AԍPAԏ\Aԏ\Aԏ\Aԏ\AԑhAԓuAԓuAԏ\AԍPAԏ\AԍPAԍPAԏ\AԑhAԑhG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                           A�r�A�n�A�t�AԃAԍPAԍPAԋDAԋDAԏ\Aԏ\AԓuAԏ\AԑhAԋDAԉ7A�t�A�^5A��/A�+A�ĜA���A���AѸRA��A�^5A�JA�ƨA̓uA�{A���A�jA�E�A���Aʗ�A�v�A�t�A¥�A��;A���A�O�A��hA��A��!A�x�A�p�A���A�1'A��;A���A��+A�t�A�XA��
A��A��9A�=qA��!A�dZA���A�l�A��!A��A��A��mA�ȴA�n�A��^A�/A��!A���A���A��FA��/A�jA�+A���A�ȴA��jA�`BA��mA�C�A��A���A��yA�jA�JA�jA���A�K�A�5?A�A��A�-A�v�A���A���A��+A���A��+A�hsA��FA��7A�33A��;A��A�E�A�A��\A�I�A��wA�v�A��/A���A�C�A���A�1A~��A|�RA{�AzM�Ax��AwdZAu�FAs�ApM�AoXAo�Am��Al  Aj  Ah�uAg?}AfI�AehsAd9XAb��Ab�A`bNA_��A_\)A^M�A]7LA[�mAY��AX�`AXJAW\)AU�AUt�AU�ATVAT�ATAS|�ARM�AQ�AO��ANE�ALȴAK�#AI��AH�AG`BAF1AD�RACdZAAƨA@�/A??}A>�A=C�A;�A:�A:-A7��A6�jA4A1�A0��A0bNA.��A.I�A.�A-\)A+VA)XA((�A'��A'\)A&��A&�+A%��A$I�A#+A!�A ��A (�AM�A��A�HA��A?}A��AI�A�A�A/A-AoAȴA�-AVA(�A?}AQ�A�mA�^A��A�A��A�yA$�AS�A��A\)A
A	;dAr�AE�A�At�A��AbNA�7A��A��A��AffA�#A �HA ĜA =q@��R@� �@�1'@�o@���@���@�D@�C�@��y@�ff@�x�@��@���@�r�@� �@�ƨ@�\)@�
=@��^@��@�b@���@��D@�r�@�~�@�
=@�7L@���@��F@��@�&�@��H@�j@�|�@�(�@��@�%@��R@���@���@�O�@�G�@���@�;d@�O�@���@�E�@��@��^@�5?@���@���@�-@���@���@�E�@�hs@�1@���@�@��/@� �@��w@��y@��-@��j@��@��!@��h@��j@� �@���@��@�v�@���@��9@��@~@|9X@{��@x��@x1'@v5?@u?}@s��@q�7@o\)@n{@l�j@k"�@hA�@fE�@c��@a��@_��@^E�@]O�@\��@[�
@X��@W�w@V��@TZ@S33@Q��@P�@N{@M?}@KdZ@J~�@I&�@G��@F��@FE�@D1@A�@?;d@=@<�/@;ƨ@:��@9�@8�9@7�P@7;d@5�@5V@3�m@2��@0bN@/�P@.V@,�@,9X@+�m@*�@*J@)�@(��@(bN@'��@&�R@%�@%V@$�@#��@"��@"n�@!�@!�7@ ��@ Q�@l�@�+@��@�m@�F@�@�@7L@bN@+@$�@O�@V@1@S�@@^5@�@hs@1'@�@�R@�T@�@��@�@�
@
^5@	�#@	��@	�^@	7L@r�@A�@�w@|�@�+@�T@�@��@Z@@^5@�@ ��?�|�?���?�7L?��?���?��?��?�h?�ƨ?��#?�?�+?�?㕁?�&�?�A�?߾w?�{?�/?�C�?�^5?�r�?�l�?�$�?�?Լj?ӕ�?�o?�hs?�&�?У�?�  ?�;d?�v�?�V?�p�?�/?���?�1?�?�^5?ȴ9?�ff?�z�?��
?�M�?���?��;?��R?�{?��D?�I�?��m?�dZ?���?���?���?���?���?���?��H?�C�?��?��?��m?�j?���?�V?�/?�O�?�O�?��h?��-?���?��?�{?�V?�VA�p�A�p�A�n�A�r�A�r�A�r�A�r�A�r�A�p�A�x�A�r�A�t�A�r�A�r�A�p�A�n�A�n�A�p�A�l�A�l�A�n�A�n�A�n�A�r�A�t�A�t�A�v�A�t�A�|�AԃAԇ+Aԇ+AԍPAԍPAԏ\AԍPAԍPAԍPAԋDAԋDAԉ7AԋDAԋDAԉ7AԍPAԏ\Aԏ\Aԏ\Aԏ\AԑhAԓuAԓuAԏ\AԍPAԏ\AԍPAԍPAԏ\AԑhAԑhG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                           ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�BcTBcTBbNBcTBbNBbNBbNBbNBbNBbNBbNBcTBcTBdZBffBk�Bo�B�DB�'BƨB��BPB$�BgmBT�BQ�BN�BI�BC�B?}BC�BC�B@�B<jBC�B=qBdZBn�Bs�Bw�By�B~�B�B�+B�%B�B~�B}�B� B�B� B~�B�B�1B�JB�VB�oB��B��B��B�uB�1B�+B�DB��B��B�=B�\B�1Bz�BjBW
BN�BG�B5?B1'B0!B0!B+B(�B9XB:^B;dB;dB=qB<jB<jB8RB7LB5?B0!B�B��B�fB��B�}B�jB��Bt�Be`BK�B7LB �BPBB
��B
�B
�B
�`B
�B
��B
�9B
�B
��B
��B
�JB
x�B
o�B
iyB
\)B
VB
J�B
@�B
2-B
�B
�B
{B

=B
B	��B	�B	�fB	�;B	�B	��B	ǮB	�}B	�LB	�?B	�'B	��B	��B	��B	�{B	�PB	�1B	�B	z�B	x�B	u�B	p�B	o�B	n�B	jB	bNB	\)B	S�B	J�B	D�B	>wB	1'B	-B	#�B	�B	�B	\B		7B	B��B��B�B�`B�TB�/B��B��B��B�jB�9B�-B�B��B��B��B��B��B�oB�bB�PB�JB�=B�%B�B}�By�Bw�Bs�Br�Bo�BiyBgmBdZBcTBaHBaHB`BB_;B[#B[#BXBS�BQ�BO�BK�BJ�BI�BG�BE�BG�BE�BD�BD�BB�BA�B:^B;dB8RB7LB6FB5?B33B33B2-B2-B1'B2-B0!B1'B/B0!B/B,B.B+B-B,B+B)�B)�B+B)�B)�B+B+B+B+B,B,B,B-B.B0!B0!B0!B?}BE�B^5BS�BjBW
BaHBl�Br�Bx�B�B�oB��B�'BÖB�fB�B	B	uB	bB	�B	33B	2-B	Q�B	YB	aHB	u�B	~�B	�hB	��B	��B	�B	�LB	�qB	��B	ŢB	��B	��B	��B	�B	�)B	�;B	�HB	�ZB	�sB	�B	�B	�B	�B	��B	��B	��B	��B	��B
  B
B
B
B
B
%B
1B

=B
JB
\B
bB
oB
oB
{B
�B
�B
�B
�B
�B
�B
 �B
!�B
#�B
&�B
%�B
'�B
(�B
)�B
,B
,B
/B
0!B
33B
49B
5?B
6FB
8RB
7LB
:^B
<jB
>wB
>wB
?}B
@�B
A�B
B�B
C�B
D�B
E�B
G�B
G�B
H�B
I�B
K�B
J�B
L�B
N�B
N�B
M�B
O�B
P�B
Q�B
R�B
R�B
S�B
R�B
S�B
T�B
VB
VB
VB
XB
XB
\)B
[#B
\)B
]/B
\)B
^5B
_;B
`BB
aHB
bNB
dZB
e`B
gmB
gmB
hsB
hsB
iyB
iyB
jB
jB
jB
k�B
k�B
k�B
m�B
n�B
n�B
n�B
p�B
o�B
q�B
q�B
q�B
s�B
t�B
t�B
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
|�B
}�B
~�B
�B
�B
�B
�%B
�+B
�7B
�=B
�DB
�PB
�PB
�\B
�bB
�oB
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
�B
�B
�!B
�!B
�'B
�-B
�3B
�9B
�9B
�?B
�FB
�?B
�FB
�FB
�LB
�LB
�LB
�RB
�RB
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
�^BdZBcTBcTBdZBdZBbNBdZBcTBcTBcTBe`BdZBcTBcTBcTBcTBdZBdZBdZBdZBcTBcTBdZBbNBbNBbNBbNBcTBe`BbNBbNBcTBbNBbNBbNBbNBcTBbNBbNBbNBcTBbNBbNBbNBbNBbNBbNBbNBbNBbNBaHBbNBbNBdZBbNBcTBdZBcTBcTBcTG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                           Bc-Bc-Bb'Bc.Bb(Bb(Bb(Bb)Bb)Bb)Bb*Bc0Bc1Bd7BfDBkdBo}B�$B�BƉB��B2B$�BgPBT�BQ�BN�BI�BC{B?cBC}BC}B@kB<RBCB=ZBdCBn�Bs�Bw�By�B~�B�B�B�B��B~�B}�B�B��B�B~�B��B�#B�<B�IB�bB�{B��B��B�jB�'B�!B�;B�xB�B�6B�UB�*Bz�BjyBWBN�BG�B5;B1$B0B0B+ B(�B9XB:^B;eB;eB=sB<lB<mB8UB7PB5CB0&B�B��B�lB��B��B�rB��Bt�BeiBK�B7UB �BZBB
��B
�B
�B
�lB
�$B
��B
�GB
�B
��B
��B
�ZB
x�B
o�B
i�B
\:B
VB
J�B
@�B
2@B
�B
�B
�B

RB
B	��B	��B	�}B	�RB	�/B	��B	��B	��B	�fB	�YB	�BB	�B	��B	��B	��B	�mB	�OB	�0B	{ B	x�B	u�B	p�B	o�B	n�B	j�B	bqB	\LB	TB	J�B	D�B	>�B	1LB	-4B	#�B	�B	�B	�B		_B	/B��B��B�B�B�B�ZB�B��B��B��B�fB�[B�6B�+B�%B�B��B��B��B��B��B�}B�pB�YB�@B~)BzBxBs�Br�Bo�Bi�Bg�Bd�Bc�Ba�Ba�B`}B_vB[_B[_BXMBT5BR*BPBLBK BI�BG�BE�BG�BE�BD�BD�BB�BA�B:�B;�B8�B7�B6�B5�B3{B3{B2vB2vB1qB2wB0lB1rB/gB0mB/hB,UB.bB+PB-]B,WB+RB*LB*MB+SB*NB*NB+UB+UB+VB+WB,]B,^B,^B-eB.kB0yB0yB0zB?�BFB^�BT]Bj�BWtBa�Bl�Bs#ByKB��B��B�=B��B�B��B�6B	�B	B	�B	JB	3�B	2�B	R�B	Y�B	a�B	vlB	�B	�B	�jB	��B	��B	�B	�/B	�JB	�fB	̎B	ѯB	��B	��B	��B	�B	� B	�5B	�QB	�`B	�oB	��B	��B	��B	��B	��B	��B	��B
 �B
B
B
!B
$B
,B
	;B
JB
ZB
oB
wB
�B
�B
�B
�B
�B
�B
�B
�B
 �B
!�B
# B
%B
($B
' B
)0B
*9B
+BB
-QB
-SB
0iB
1rB
4�B
5�B
6�B
7�B
9�B
8�B
;�B
=�B
?�B
?�B
@�B
A�B
B�B
DB
EB
FB
G"B
I1B
I3B
J<B
KEB
MUB
LRB
NaB
PpB
PrB
OoB
Q~B
R�B
S�B
T�B
T�B
U�B
T�B
U�B
V�B
W�B
W�B
W�B
Y�B
Y�B
]�B
\�B
]�B
^�B
]�B
`	B
aB
bB
c$B
d,B
f;B
gCB
iSB
iVB
j^B
jaB
kiB
klB
ltB
lwB
lyB
m�B
m�B
m�B
o�B
p�B
p�B
p�B
r�B
q�B
s�B
s�B
s�B
u�B
v�B
v�B
v�B
w�B
w�B
w�B
x�B
y�B
zB
{
B
{B
}B
}#B
5B
�BB
�MB
�fB
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
�B
�B
�B
�/B
�<B
�HB
�SB
�`B
�fB
�xB
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
��B
��B
��B
�B
�B
�B
�8B
�UB
�oB
��B
��B
��B
��B
��B
��B
�B
�B
�1B
�@B
�TB
�lB
�sB
��B
��B
��B
��B
��B
��B
��B
�B
�B
�%B
�4B
�7B
�;B
�>B
�AB
�DB
�GB
�JB
�MB
�QB
�YBd3Bc-Bc-Bd3Bd3Bb'Bd3Bc-Bc-Bc-Be9Bd3Bc-Bc-Bc-Bc-Bd3Bd3Bd3Bd3Bc-Bc-Bd3Bb'Bb'Bb'Bb'Bc-Be9Bb'Bb(Bc.Bb(Bb(Bb(Bb(Bc.Bb(Bb(Bb(Bc/Bb)Bb)Bb)Bb)Bb)Bb)Bb)Bb)Bb*Ba$Bb*Bb*Bd6Bb*Bc0Bd7Bc1Bc1Bc1G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                           <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�201808131704002021061413553220210614135532202106171312342021061713123420210617131234201808131704002021061413553220210614135532202106171312342021061713123420210617131234PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            new conductivity = original conductivity * (1 + delta*TEMP + CPcor_SBE*PRES) / (1 + delta*TEMP_ADJUSTED + CPcor_new*PRES_ADJUSTED)                                                                                                                              none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            new conductivity = original conductivity * (1 + delta*TEMP + CPcor_SBE*PRES) / (1 + delta*TEMP_ADJUSTED + CPcor_new*PRES_ADJUSTED)                                                                                                                              none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.50 dbar                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            CPcor_new = -1.25e-07; CPcor_SBE = -9.57e-08; delta = 3.25e-06                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.50 dbar                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            CPcor_new = -1.25e-07; CPcor_SBE = -9.57e-08; delta = 3.25e-06                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                      none                                                                                                                                                                                                                                                            New conductivity computed by using a different CPcor value from that provided by Sea-Bird.                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            No significant salinity drift detected.                                                                                                                                                                                                                         Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                      none                                                                                                                                                                                                                                                            New conductivity computed by using a different CPcor value from that provided by Sea-Bird.                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            No significant salinity drift detected.                                                                                                                                                                                                                         PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            AO  AO  ARCAARCAADJPADJP                                                                                                                                        2018081317040020180813170400  IP  IP                                G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2018081317040020180813170400QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�5F03E           703E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2018081317040020180813170400QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�0               0               PM  PM  ARSQARSQCOWGCOWGV1.1V1.1                                                                                                                                2021061713150020210617131500IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                