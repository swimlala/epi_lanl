CDF       
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS   s   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JAMSTEC    source        
Argo float     history       b2009-09-09T15:56:35Z creation;2015-03-10T02:10:42Z update;2015-06-07T15:15:25Z conversion to V3.1;     
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      
_FillValue               conventions       Argo reference table 1          6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    6�   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  6�   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  78   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7x   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    7�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     8   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8<   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8\   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
_FillValue        A.�~       standard_name         time   
resolution        >�����h�   axis      T           8`   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8h   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
_FillValue        A.�~       
resolution        >�����h�        8l   LATITUDE               	long_name         &Latitude of the station, best estimate     units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        standard_name         latitude   axis      Y           8t   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        standard_name         	longitude      axis      X           8|   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8�   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8�   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    8�   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    8�   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    8�   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8�   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9�   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   standard_name         sea_water_pressure     axis      Z        �  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  t  ;l   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   standard_name         sea_water_pressure       �  ;�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  t  =�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  >    TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o   standard_name         sea_water_temperature        �  ?�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  t  A�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o   standard_name         sea_water_temperature        �  B,   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  t  C�   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  Dl   PSAL         
      	   	long_name         Practical salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o   standard_name         sea_water_salinity       �  F8   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  t  H   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o   standard_name         sea_water_salinity       �  Hx   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  t  JD   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  J�   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  L�   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   M   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   V   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   _   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  h   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    h�   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    h�   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    h�   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    h�   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  h�   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    h�   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    h�   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    h�   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         i   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         i   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        i   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    iArgo profile    3.1 1.2 19500101000000  4900898 J-ARGO                                                          JAMSTEC                                                         PRES            TEMP            PSAL               tA   JA  20090909155635  20150613142511  A9_60144_116                    2C  D   APEX                            2414                            061305                          846 @�JRفS�1   @�JS{r�\@Cϝ�-V�d"��`B1   ARGOS   A   A   A   Primary sampling: discrete [spot sampling CTD data]                                                                                                                                                                                                                @�  A	��AfffA���A���A�ffB	��BffB0��BE��BY33Bm33B�33B���B�33B���B���B�  B�  B�  B�  B�  B�ffB�33B�33CL�C33CffC  CL�C33C�C$�C)  C-�fC3  C8  C=  CB�CGffCQ33C[�Ce��Co� Cy��C�� C���C�� C��3C�s3C�� C�� C���C�s3C���C���C�� C�� C�Cǳ3C�� CѦfCֳ3CۦfC�fC�� C�fC��C��3C��fD��D� D�3DٚDٚD�3D� D$�3D)�fD.ٚD3��D8� D=�3DB�fDG��DL��DQ��DV�3D[�fD`�3De�3Dj�3Do��Dt��DyٚD�)�D�i�D�� D��D�  D�p D�� D���D�#3D�ffD�� D�� D�fD�p Dڠ D��fD�  D�Y�D�D� 1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @�  A��A^ffA���A���A�ffB��BffB.��BC��BW33Bk33B~ffB���B�33B���B���B�  B�  B�  B�  B�  B�ffB�33B�33C ��C�3C
�fC� C��C�3C��C#��C(� C-ffC2� C7� C<� CA��CF�fCP�3CZ��Ce�Co  Cy�C�� C�L�C�@ C�s3C�33C�@ C�@ C�L�C�33C�L�C�Y�C�� C�� C�Y�C�s3C̀ C�ffC�s3C�ffC�ffC� C�ffC�L�C�s3C�ffD��D� D�3D��D��D�3D� D$�3D)�fD.��D3��D8� D=�3DB�fDG��DL��DQ��DV�3D[�fD`�3De�3Dj�3Do��Dt��Dy��D��D�Y�D�� D�ٚD� D�` D�� D���D�3D�VfD�� D�� D�fD�` Dڐ D��fD� D�I�D�D�  1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�t�A�v�A�XA�O�A�/A��/A��;ArM�Ad�!AVJAJ�DAFv�AC�AB��A@=qA=A<�/A;K�A:��A8ĜA6�!A5C�A3�A4Q�A3A2bNA1O�A0ȴA0�!A/�#A.��A-��A,��A,A�A+��A+VA*v�A)O�A(�DA'%A$bNA"9XAƨA��Av�A�AQ�AVA�PA�
A��A�A��A\)A�Ar�A��@�S�@�9X@��@�D@���@◍@�-@��@֗�@���@˶F@��/@�O�@��;@���@��D@��;@�1@�-@�@���@�Q�@���@��@��/@�5?@�l�@���@}�T@y�7@u�@q�@l�D@h�9@e�T@bJ@^E�@Z�\@Qhs@IX@A�#@:�H@5@1��@-�@)�7@%�@!�^@�-@J@�R@33@|�@�j@	��@�+@dZ@ 1'1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�t�A�v�A�XA�O�A�/A��/A��;ArM�Ad�!AVJAJ�DAFv�AC�AB��A@=qA=A<�/A;K�A:��A8ĜA6�!A5C�A3�A4Q�A3A2bNA1O�A0ȴA0�!A/�#A.��A-��A,��A,A�A+��A+VA*v�A)O�A(�DA'%A$bNA"9XAƨA��Av�A�AQ�AVA�PA�
A��A�A��A\)A�Ar�A��@�S�@�9X@��@�D@���@◍@�-@��@֗�@���@˶F@��/@�O�@��;@���@��D@��;@�1@�-@�@���@�Q�@���@��@��/@�5?@�l�@���@}�T@y�7@u�@q�@l�D@h�9@e�T@bJ@^E�@Z�\@Qhs@IX@A�#@:�H@5@1��@-�@)�7@%�@!�^@�-@J@�R@33@|�@�j@	��@�+@dZ@ 1'1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oBQ�BQ�BP�BP�BR�B�uB/B�RB�B(�B��B�B�yB��B��B��B��B��B	7B%B��B%BDB.B?}BL�BZBffBq�B|�B�1B�PB�uB��B��B��B��B�uB�\B�=B|�Bw�BjB\)BN�B?}BA�BC�BC�BB�B@�B?}B6FB6FB33B+B �BoB%B��B�B�B�fB�;B�/B�5B��B��BɺBɺB��B�B�B��B�B)�B7LBE�BR�BgmBu�B�7B��B��B�B�wBɺB�B�HB�B��B	+B	oB	�B	+B	E�B	]/B	u�B	�PB	��B	�B	�jB	��B	�5B	�B	��B
B
bB
�B
(�B
2-B
;dB
E�B
N�B
X1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 BQ�BQ�BP�BP�BYB��B=qB�}B��B/BB�B�B��B��B��B��B��B
=B+B��B+BDB.B@�BM�BZBffBq�B}�B�7B�PB�uB��B��B��B��B�uB�bB�DB}�Bx�Bk�B]/BO�B?}BA�BC�BD�BB�BA�B@�B6FB7LB49B,B!�BuB+B��B��B�B�mB�BB�/B�;B��B��BɺB��B��B�#B�B��B�B)�B7LBE�BR�BgmBu�B�7B��B��B�B�wBɺB�B�HB�B��B	+B	oB	�B	+B	E�B	]/B	u�B	�PB	��B	�B	�jB	��B	�5B	�B	��B
B
bB
�B
(�B
2-B
;dB
E�B
N�B
X1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 <#�
<#�
<#�
<#�
<#�
<�o <t!<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ = PRES-SP(NextCycle), where SP is SURFACE PRESSURE from next cycle                                                                                                                                                                                     TEMP_ADJUSTED = TEMP                                                                                                                                                                                                                                            PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,PRES_ADJ,elapsed_time,alpha,tau),elapsed_time=P/mean_rise_rate,P=dbar since the start of the profile for each samples                                                                                                       None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            SP(NextCycle)=0.5(dbar)                                                                                                                                                                                                                                         None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE; PRES_ADJ_ERR : Manufacture sensor accuracy                                                                                                                                                                 TEMP_ADJ_ERR : SBE sensor accuracy                                                                                                                                                                                                                              Salinity Recalculation using PRES_ADJ; PSAL_ADJ_ERR : max(sum of RecalS & CTM errors, 0.01(PSS-78))                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         200909221357262009092213572620090922135726200909221659332009092216593320090922165933201010040000002010100400000020101004000000  JA  ARFMdecpA9_b                                                                20090909155634  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.5                                                                 20090909155635  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.0                                                                 20090909155636  IP  PRES            G�O�G�O�G�O�                JA  ARCArsal2.1a                                                                20090909155636  IP  PSAL            G�O�G�O�G�O�                JA  ARGQrqcppo_b                                                                20090909155636  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19b                                                                20090909155637  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19b                                                                20090909155637  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8b                                                                20090909155637  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8b                                                                20090909155637  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16b                                                                20090909155637  QCP$                G�O�G�O�G�O�           10000JA  ARUP                                                                        20090909160228                      G�O�G�O�G�O�                JA  ARFMdecpA9_b                                                                20090913065450  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.5                                                                 20090913065718  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.0                                                                 20090913065718  IP  PRES            G�O�G�O�G�O�                JA  ARCArsal2.1a                                                                20090913065719  IP  PSAL            G�O�G�O�G�O�                JA  ARGQrqcppo_b                                                                20090913065719  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19b                                                                20090913065720  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19b                                                                20090913065720  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8b                                                                20090913065720  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8b                                                                20090913065720  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16b                                                                20090913065720  QCP$                G�O�G�O�G�O�           10000JA  ARUP                                                                        20090913070053                      G�O�G�O�G�O�                JM  ARCAJMQC1.0                                                                 20090922135726  CV  PRES            G�O�G�O�G�O�                JM  ARCAJMQC1.0                                                                 20090922135726  IP  PSAL            G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20090922165933  CV  PSAL            G�O�G�O�G�O�                JM  ARSQOW  1.1 CTD2010V1                                                       20101004000000  IP  PSAL            G�O�G�O�G�O�                JA  RFMTcnvd2.1                                                                 20101014014526  IP                  G�O�G�O�G�O�                JA  ARDU                                                                        20101015004920                      G�O�G�O�G�O�                JM  RENCREJM1.1c                                                                20150209092147  ED  SCIENTIFIC_CALIBG�O�G�O�G�O�                JA  ARDU                                                                        20150310021042                      G�O�G�O�G�O�                JM      JMFC1.0                                                                 20150607151517                      G�O�G�O�G�O�                JA  ARDU                                                                        20150613142511                      G�O�G�O�G�O�                