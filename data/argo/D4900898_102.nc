CDF   &   
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS   s   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JAMSTEC    source        
Argo float     history       b2009-04-22T09:57:55Z creation;2015-03-10T02:10:41Z update;2015-06-07T15:12:04Z conversion to V3.1;     
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
_FillValue                    iArgo profile    3.1 1.2 19500101000000  4900898 J-ARGO                                                          JAMSTEC                                                         PRES            TEMP            PSAL               fA   JA  20090422095755  20150613142512  A9_60144_102                    2C  D   APEX                            2414                            061305                          846 @�'NA
��1   @�'N�|ƶ@C�x����d!�$�/1   ARGOS   A   A   A   Primary sampling: discrete [spot sampling CTD data]                                                                                                                                                                                                                @�ffA��AfffA�ffA���A�  B  B��B/��BDffBXffBm33B�  B���B���B�33B�  B�ffB�  B���B���B�ffB�  B���B�ffC33CL�C� C33C  C�fC  C$�C)33C.ffC3�C7�fC<�3CBL�CG  CQL�C[33Ce� Co�Cy�C��fC��3C�� C��fC�� C�� C��fC�s3C���C�ffC�� C�ffC�ffC�ffCǀ C̳3Cѳ3Cֳ3C�� C�3C�fC�3C��C��fC��3D�fD�3D� D� D��DٚD�fD$�fD)�fD.�fD3�3D8�3D=� DB�3DG��DLٚDQٚDV� D[ٚD`�3De� Dj�3Do� Dt� Dy� D�  D�i�D��3D���D�  D�ffD���D��D�#3D�c3D��3D��D��D�i�Dڬ�D��3D�&fD�\�D�D�&f1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @�33A  A\��A���A�  A�33B��B33B-33BB  BV  Bj��B��B���B���B�  B���B�33B���Bř�Bϙ�B�33B���B홚B�33C ��C�3C
�fC��CffCL�CffC#� C(��C-��C2� C7L�C<�CA�3CFffCP�3CZ��Cd�fCn� Cx� C�Y�C�ffC�s3C�Y�C�s3C�s3C�Y�C�&fC�@ C��C�33C��C��C��C�33C�ffC�ffC�ffC�s3C�ffC�Y�C�ffC�@ C�Y�C�ffD� D��D��D��D�fD�3D� D$� D)� D.� D3��D8��D=��DB��DG�fDL�3DQ�3DV��D[�3D`��De��Dj��Do��Dt��Dy��D��D�VfD�� D�ٚD��D�S3D���D��fD� D�P D�� D��fD�	�D�VfDڙ�D�� D�3D�I�D�fD�31111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��AGx�AG�AGp�AGt�AG�AG�hAG"�AG%AGVAF�`AF9XAC�FA;�mA:  A8�A7�FA7�#A7"�A7`BA5�A6=qA4JA4VA4�uA4v�A3C�A2��A1�#A0E�A.��A.��A-A+�TA*�/A)dZA(�yA(�A'��A'/A&�+A#��A {A33AhsAbNA�HA��AQ�A�A�
AVAA�A
��A�A7LA^5A�A �R@�J@�;d@��#@�&�@�@��#@� �@�%@Η�@��@�r�@��@�C�@�n�@���@��H@�$�@�{@�Z@��w@���@���@�`B@���@���@�@���@~�+@y�7@tZ@pb@lz�@i��@e�h@a�^@_�@[C�@Q�^@Hr�@B�@<9X@6�+@1�^@-��@(�@#C�@��@t�@�y@C�@�P@j@	hs@��@z�@�?��R1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 AGx�AG�AGp�AGt�AG�AG�hAG"�AG%AGVAF�`AF9XAC�FA;�mA:  A8�A7�FA7�#A7"�A7`BA5�A6=qA4JA4VA4�uA4v�A3C�A2��A1�#A0E�A.��A.��A-A+�TA*�/A)dZA(�yA(�A'��A'/A&�+A#��A {A33AhsAbNA�HA��AQ�A�A�
AVAA�A
��A�A7LA^5A�A �R@�J@�;d@��#@�&�@�@��#@� �@�%@Η�@��@�r�@��@�C�@�n�@���@��H@�$�@�{@�Z@��w@���@���@�`B@���@���@�@���@~�+@y�7@tZ@pb@lz�@i��@e�h@a�^@_�@[C�@Q�^@Hr�@B�@<9X@6�+@1�^@-��@(�@#C�@��@t�@�y@C�@�P@j@	hs@��@z�@�?��R1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB�B�B�B�B�B�B�B�B�B�B�fB�B�LB�9B�-B�'B�^B�B��BDB"�BG�BaHB~�B�VB�{B��B��B�oB�VB�VB�DB�B{�Bs�Bx�B{�Bv�By�B}�Bo�B_;BffBXBK�BJ�BH�BF�BD�BF�B<jB5?B/B+B)�B!�B�BuBVB	7B��B�B�B�ZB�HB�;B�)B�)B�)B�5B�NB�ZB��B1BuB �B/B?}BS�BaHBl�Bz�B�B�oB��B�LBƨB�B�ZB�B��B	+B	uB	�B	'�B	D�B	`BB	q�B	�1B	��B	�B	�jB	��B	�TB	�B
  B
PB
�B
&�B
1'B
9XB
A�B
H�B
O�B
X1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B�B�B�B�B�B�B�B�B�B�B�mB�)B�RB�?B�3B�'B�^B�B��BDB#�BG�BaHB~�B�\B�{B��B��B�uB�VB�\B�JB�B|�Bs�Bx�B|�Bv�By�B~�Bp�B_;BgmBYBL�BJ�BI�BG�BD�BG�B=qB5?B0!B,B+B!�B�BuBVB
=B��B��B�B�`B�HB�BB�/B�/B�/B�5B�NB�ZB��B1BuB!�B/B?}BS�BaHBl�Bz�B�B�oB��B�LBƨB�B�ZB�B��B	+B	uB	�B	'�B	D�B	`BB	q�B	�1B	��B	�B	�jB	��B	�TB	�B
  B
PB
�B
&�B
1'B
9XB
A�B
H�B
O�B
X1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ = PRES-SP(NextCycle), where SP is SURFACE PRESSURE from next cycle                                                                                                                                                                                     TEMP_ADJUSTED = TEMP                                                                                                                                                                                                                                            PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,PRES_ADJ,elapsed_time,alpha,tau),elapsed_time=P/mean_rise_rate,P=dbar since the start of the profile for each samples                                                                                                       None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            SP(NextCycle)=0.6(dbar)                                                                                                                                                                                                                                         None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE; PRES_ADJ_ERR : Manufacture sensor accuracy                                                                                                                                                                 TEMP_ADJ_ERR : SBE sensor accuracy                                                                                                                                                                                                                              Salinity Recalculation using PRES_ADJ; PSAL_ADJ_ERR : max(sum of RecalS & CTM errors, 0.01(PSS-78))                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         200905051403132009050514031320090505140313200905051414342009050514143420090505141434201010040000002010100400000020101004000000  JA  ARFMdecpA9_b                                                                20090422095754  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.5                                                                 20090422095755  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.0                                                                 20090422095755  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_b                                                                20090422095756  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19b                                                                20090422095757  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19b                                                                20090422095757  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8b                                                                20090422095757  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8b                                                                20090422095757  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16b                                                                20090422095757  QCP$                G�O�G�O�G�O�           10000JA  ARUP                                                                        20090422100509                      G�O�G�O�G�O�                JA  ARFMdecpA9_b                                                                20090426065444  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.5                                                                 20090426065710  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.0                                                                 20090426065710  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_b                                                                20090426065711  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19b                                                                20090426065712  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19b                                                                20090426065712  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8b                                                                20090426065712  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8b                                                                20090426065712  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16b                                                                20090426065712  QCP$                G�O�G�O�G�O�           10000JA  ARUP                                                                        20090426070109                      G�O�G�O�G�O�                JA  ARCArsal2.1a                                                                20090821003841  IP  PSAL            G�O�G�O�G�O�                JA  ARGQrqcppo_b                                                                20090821003841  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19b                                                                20090821003842  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19b                                                                20090821003842  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8b                                                                20090821003842  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8b                                                                20090821003842  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16b                                                                20090821003843  QCP$                G�O�G�O�G�O�           10000JA  ARUP                                                                        20090821011150                      G�O�G�O�G�O�                JM  ARCAJMQC1.0                                                                 20090505140313  CV  PRES            G�O�G�O�G�O�                JM  ARCAJMQC1.0                                                                 20090505140313  IP  PSAL            G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20090505141434  CV  PSAL            G�O�G�O�G�O�                JM  ARSQOW  1.1 CTD2010V1                                                       20101004000000  IP  PSAL            G�O�G�O�G�O�                JA  RFMTcnvd2.1                                                                 20101014014528  IP                  G�O�G�O�G�O�                JA  ARDU                                                                        20101015004923                      G�O�G�O�G�O�                JM  RENCREJM1.1c                                                                20150209092127  ED  SCIENTIFIC_CALIBG�O�G�O�G�O�                JA  ARDU                                                                        20150310021041                      G�O�G�O�G�O�                JM      JMFC1.0                                                                 20150607151156                      G�O�G�O�G�O�                JA  ARDU                                                                        20150613142512                      G�O�G�O�G�O�                