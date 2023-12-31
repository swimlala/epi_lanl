CDF   $   
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS   s   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JAMSTEC    source        
Argo float     history       b2009-10-02T00:55:06Z creation;2015-03-10T02:35:31Z update;2015-06-07T17:18:25Z conversion to V3.1;     
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
_FillValue                    iArgo profile    3.1 1.2 19500101000000  4900902 J-ARGO                                                          JAMSTEC                                                         PRES            TEMP            PSAL               vA   JA  20091002005506  20150616074511  A9_60146_118                    2C  D   APEX                            2416                            061305                          846 @�O�+l�1   @�O�%�}�@<�;dZ��dƧ1   ARGOS   A   A   A   Primary sampling: discrete [spot sampling CTD data]                                                                                                                                                                                                                @���AffAfffA�  A�  A�ffB	33B��B2��BFffBZ  Bm33B�33B�ffB�ffB���B���B�33B���B�33B�ffB�  B���B���B���CL�C33C  C  C�CffC33C$33C)L�C.33C3L�C8L�C=� CB�CG��CQL�C[� CeL�Co33CyL�C��3C��fC���C��fC�� C���C��3C���C���C���C���C�s3C�� C¦fC�Y�C̦fCр C�s3Cۙ�C�3C噚C�fCC��3C�� D� D��D� D� D�3D�fD� D$��D)� D.��D3�3D8�fD=��DB�3DG�3DL�3DQ��DV�fD[��D`��DeٚDj�3Do�fDtٚDy�fD��D�s3D���D�� D��D�i�D��3D���D�  D�p D���D��3D�&fD�ffDڠ D�� D�  D�Y�D��D�` 1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @�ffA33A[33A�ffA�ffA���BffB��B0  BC��BW33BjffB��B�  B�  B�ffB�ffB���B�ffB���B�  Bؙ�B�ffB�ffB�33C ��C� C
L�CL�CffC�3C� C#� C(��C-� C2��C7��C<��CAffCF�fCP��CZ��Cd��Cn� Cx��C�Y�C�L�C�33C�L�C�ffC�@ C�Y�C�@ C�@ C�33C�33C��C�&fC�L�C�  C�L�C�&fC��C�@ C�Y�C�@ C�L�C�@ C�Y�C�ffD�3D� D�3D�3D�fD��D�3D$� D)�3D.� D3�fD8��D=��DB�fDG�fDL�fDQ� DV��D[� D`��De��Dj�fDo��Dt��Dy��D�fD�\�D��fD�ɚD�fD�S3D���D��fD�	�D�Y�D��fD���D� D�P Dډ�D�ɚD�	�D�C3D�fD�I�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A��A��A��A��A��A��A��A�{A���A�r�A�l�A�dZA��;A���A�A�E�A���A�E�A���A���A�(�A�v�A��TA��A�VA��A�t�A���A���A
=A|  Az��Ax��Aw��As7LAp��Am�
AmoAg�Ae�#A_��AY��AW�FAVz�APQ�AM�^AK��AH�AGƨAF�`AD��A@�\A<��A8�jA5\)A3�;A/�;A.�jA-`BA't�A&1A%p�A"�uA��A�-A�A"�A�9A	x�A�@��-@�@�^@�~�@�@�(�@��@�1'@�&�@�33@��@��^@��P@�ƨ@��!@�v�@���@K�@y%@u?}@r�@o�P@nV@kt�@h �@]��@Y7L@T(�@M@E�@@�@:^5@3��@/�@*�!@'l�@ �u@Z@�R@x�@��@	�@bN@v�@~�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A��A��A��A��A��A��A��A�{A���A�r�A�l�A�dZA��;A���A�A�E�A���A�E�A���A���A�(�A�v�A��TA��A�VA��A�t�A���A���A
=A|  Az��Ax��Aw��As7LAp��Am�
AmoAg�Ae�#A_��AY��AW�FAVz�APQ�AM�^AK��AH�AGƨAF�`AD��A@�\A<��A8�jA5\)A3�;A/�;A.�jA-`BA't�A&1A%p�A"�uA��A�-A�A"�A�9A	x�A�@��-@�@�^@�~�@�@�(�@��@�1'@�&�@�33@��@��^@��P@�ƨ@��!@�v�@���@K�@y%@u?}@r�@o�P@nV@kt�@h �@]��@Y7L@T(�@M@E�@@�@:^5@3��@/�@*�!@'l�@ �u@Z@�R@x�@��@	�@bN@v�@~�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB��B��B��B��B��B��B��B��B�B��B�uBffB0!BB�NB�3B��BdZB[#B=qB,B�BB
�B
�3B
�VB
�1B
|�B
e`B
J�B
=qB
8RB
/B
(�B
�B
VB	��B	��B	�TB	�#B	ȴB	�FB	�B	��B	�oB	�1B	~�B	u�B	q�B	l�B	cTB	P�B	?}B	)�B	�B	\B��B��B�B�#B��B��B��B�B��B�=Bu�BjBXBL�B49B,B�B�B�B�B�B#�B?}BC�BL�BffB�+B��B��B�XB��B�B	%B	�B	"�B	<jB	G�B	VB	iyB	��B	�FB	ǮB	�BB	��B

=B
�B
(�B
33B
;dB
A�B
L�B
R�B
[#B
cTB
hsB
o�B
r�B
t�B
z�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B��B��B��B��B��B��B��B��B�yB�#B��Bk�B49B+B�mB�LB�BffB^5B?}B.B�BB
�/B
�FB
�\B
�7B
~�B
gmB
K�B
>wB
9XB
0!B
+B
�B
\B	��B	��B	�ZB	�/B	ɺB	�LB	�B	��B	�uB	�7B	� B	u�B	q�B	m�B	dZB	Q�B	@�B	+B	�B	bB��B��B��B�#B��B��B��B�B��B�JBv�Bk�BYBM�B5?B-B�B�B�B�B�B$�B?}BC�BM�BgmB�+B��B��B�XB��B�B	%B	�B	"�B	<jB	G�B	VB	iyB	��B	�FB	ǮB	�BB	��B

=B
�B
(�B
33B
;dB
A�B
L�B
R�B
[#B
cTB
hsB
o�B
r�B
t�B
z�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<u<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ = PRES-SP(NextCycle), where SP is SURFACE PRESSURE from next cycle                                                                                                                                                                                     TEMP_ADJUSTED = TEMP                                                                                                                                                                                                                                            PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,PRES_ADJ,elapsed_time,alpha,tau),elapsed_time=P/mean_rise_rate,P=dbar since the start of the profile for each samples                                                                                                       None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            SP(NextCycle)=0.7(dbar)                                                                                                                                                                                                                                         None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE; PRES_ADJ_ERR : Manufacture sensor accuracy                                                                                                                                                                 TEMP_ADJ_ERR : SBE sensor accuracy                                                                                                                                                                                                                              Salinity Recalculation using PRES_ADJ; PSAL_ADJ_ERR : max(sum of RecalS & CTM errors, 0.01(PSS-78))                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         200910150138042009101501380420091015013804201010150409062010101504090620101015040906201010150000002010101500000020101015000000  JA  ARFMdecpA9_b                                                                20091002005505  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.5                                                                 20091002005506  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.0                                                                 20091002005506  IP  PRES            G�O�G�O�G�O�                JA  ARCArsal2.1a                                                                20091002005507  IP  PSAL            G�O�G�O�G�O�                JA  ARGQrqcppo_b                                                                20091002005507  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19b                                                                20091002005508  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19b                                                                20091002005508  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8b                                                                20091002005508  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8b                                                                20091002005508  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16b                                                                20091002005508  QCP$                G�O�G�O�G�O�           10000JA  ARUP                                                                        20091002010241                      G�O�G�O�G�O�                JA  ARFMdecpA9_b                                                                20091005185452  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.5                                                                 20091005185701  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.0                                                                 20091005185701  IP  PRES            G�O�G�O�G�O�                JA  ARCArsal2.1a                                                                20091005185701  IP  PSAL            G�O�G�O�G�O�                JA  ARGQrqcppo_b                                                                20091005185702  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19b                                                                20091005185703  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19b                                                                20091005185703  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8b                                                                20091005185703  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8b                                                                20091005185703  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16b                                                                20091005185703  QCP$                G�O�G�O�G�O�           10000JA  ARUP                                                                        20091005185954                      G�O�G�O�G�O�                JM  ARGQJMQC1.0                                                                 20091005104257  CV  DAT$            G�O�G�O�F��                JM  ARGQJMQC1.0                                                                 20091005104257  CV  DAT$            G�O�G�O�F��                JM  ARGQJMQC1.0                                                                 20091005104257  CV  LAT$            G�O�G�O�A��m                JM  ARGQJMQC1.0                                                                 20091005104257  CV  LON$            G�O�G�O�� g�                JM  ARCAJMQC1.0                                                                 20091015013804  CV  PRES            G�O�G�O�G�O�                JM  ARCAJMQC1.0                                                                 20091015013804  IP  PSAL            G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20101015040906  CV  PSAL            G�O�G�O�G�O�                JM  ARSQOW  1.1 CTD2010V1                                                       20101015000000  IP  PSAL            G�O�G�O�G�O�                JA  RFMTcnvd2.1                                                                 20101104043249  IP                  G�O�G�O�G�O�                JA  ARDU                                                                        20101104043315                      G�O�G�O�G�O�                JM  RENCREJM1.1c                                                                20150209092723  ED  SCIENTIFIC_CALIBG�O�G�O�G�O�                JA  ARDU                                                                        20150310023531                      G�O�G�O�G�O�                JM      JMFC1.0                                                                 20150607171816                      G�O�G�O�G�O�                JA  ARDU                                                                        20150616074511                      G�O�G�O�G�O�                