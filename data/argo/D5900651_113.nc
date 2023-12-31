CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PARAM       N_PROF        N_CALIB       N_LEVELS   s   	N_HISTORY             	   title         Argo float vertical profile    institution       JAMSTEC    source        
Argo float     history       b2007-10-17T18:49:40Z creation;2009-03-18T07:29:19Z update;2015-06-09T20:30:29Z conversion to V3.1;     
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      
_FillValue               conventions       Argo reference table 1          6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER       	            	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    6�   PROJECT_NAME      	            	long_name         Name of the project    
_FillValue                  @  6�   PI_NAME       	            	long_name         "Name of the principal investigator     
_FillValue                  @  78   STATION_PARAMETERS        	               	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7x   CYCLE_NUMBER      	         	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION         	         	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE       	            	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    7�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7�   DC_REFERENCE      	            	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR      	            	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE         	         	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE         	            	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO       	            	long_name         Serial number of the float     
_FillValue                     8   FIRMWARE_VERSION      	            	long_name         Instrument firmware version    
_FillValue                     8<   WMO_INST_TYPE         	            	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8\   JULD      	         	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
_FillValue        A.�~       standard_name         time   
resolution        >�����h�   axis      T           8`   JULD_QC       	         	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8h   JULD_LOCATION         	         	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
_FillValue        A.�~       
resolution        >�����h�        8l   LATITUDE      	         	long_name         &Latitude of the station, best estimate     units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        standard_name         latitude   axis      Y           8t   	LONGITUDE         	         	long_name         'Longitude of the station, best estimate    units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        standard_name         	longitude      axis      X           8|   POSITION_QC       	         	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8�   POSITIONING_SYSTEM        	            	long_name         Positioning system     
_FillValue                    8�   PROFILE_PRES_QC       	         	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    8�   PROFILE_TEMP_QC       	         	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    8�   PROFILE_PSAL_QC       	         	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    8�   VERTICAL_SAMPLING_SCHEME      	            	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8�   CONFIG_MISSION_NUMBER         	         	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9�   PRES      	         
   	long_name         )Sea water pressure, equals 0 at sea-level      
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   standard_name         sea_water_pressure     axis      Z        �  9�   PRES_QC       	            	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  t  ;l   PRES_ADJUSTED         	         	   	long_name         )Sea water pressure, equals 0 at sea-level      
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   standard_name         sea_water_pressure       �  ;�   PRES_ADJUSTED_QC      	            	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  t  =�   PRES_ADJUSTED_ERROR       	            	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  >    TEMP      	         	   	long_name         $Sea temperature in-situ ITS-90 scale   
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o   standard_name         sea_water_temperature        �  ?�   TEMP_QC       	            	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  t  A�   TEMP_ADJUSTED         	         	   	long_name         $Sea temperature in-situ ITS-90 scale   
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o   standard_name         sea_water_temperature        �  B,   TEMP_ADJUSTED_QC      	            	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  t  C�   TEMP_ADJUSTED_ERROR       	            	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  Dl   PSAL      	         	   	long_name         Practical salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o   standard_name         sea_water_salinity       �  F8   PSAL_QC       	            	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  t  H   PSAL_ADJUSTED         	         	   	long_name         Practical salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o   standard_name         sea_water_salinity       �  Hx   PSAL_ADJUSTED_QC      	            	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  t  JD   PSAL_ADJUSTED_ERROR       	            	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  J�   	PARAMETER         	   
               	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  L�   SCIENTIFIC_CALIB_EQUATION         	   
               	long_name         'Calibration equation for this parameter    
_FillValue                 	   M   SCIENTIFIC_CALIB_COEFFICIENT      	   
               	long_name         *Calibration coefficients for this equation     
_FillValue                 	   V   SCIENTIFIC_CALIB_COMMENT      	   
               	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   _   SCIENTIFIC_CALIB_DATE         	   
                	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  h   HISTORY_INSTITUTION          	            	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    h�   HISTORY_STEP         	            	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    h�   HISTORY_SOFTWARE         	            	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    h�   HISTORY_SOFTWARE_RELEASE         	            	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    h�   HISTORY_REFERENCE            	            	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  h�   HISTORY_DATE         	             	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    h�   HISTORY_ACTION           	            	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    h�   HISTORY_PARAMETER            	            	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    h�   HISTORY_START_PRES           	         	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         i   HISTORY_STOP_PRES            	         	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         i   HISTORY_PREVIOUS_VALUE           	         	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        i   HISTORY_QCTEST           	            	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    iArgo profile    3.1 1.2 19500101000000  5900651 J-ARGO                                                          JAMSTEC                                                         PRES            TEMP            PSAL               qA   JA  20071017184940  20150621180536  A5_23694_113                    2C  D   APEX                            1561                            013004                          846 @ԝ�s�1   @ԝ����@4�Z�1�b�^5?|�1   ARGOS   A   A   A   Primary sampling: discrete [spot sampling CTD data]                                                                                                                                                                                                                @���A  Ai��A���A�33A陚BffB  B133BFffBX��Bl��B���B�  B���B���B�  B�33B�  B�ffB�33B�  B�ffB�ffB�33CL�CffCL�CffC� CL�C� C$ffC)L�C.33C3L�C8  C=�3CBffCG��CQL�C[33Ce33CoL�CyL�C��fC�� C���C���C�� C��fC��fC���C��fC��3C���C��3C���C�s3CǙ�C̳3C�� Cֳ3C۳3C���C�3C��CC� C�� D� D��D��D�3D�3D��D�fD$�fD)�3D.ٚD3��D8�fD=�fDB��DG� DL�fDQ��DV�3D[��D`�fDe� Dj��Do�fDt� DyٚD�#3D�ffD���D��fD�&fD�p D���D��3D�,�D�ffD��3D���D�,�D�p Dڠ D��D�)�D�S3D�fD�6f1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @�ffA��AfffA�  A���A�  B��B33B0ffBE��BX  Bl  B�ffB���B�33B�ffB���B���B���B�  B���Bڙ�B�  B�  B���C�C33C�C33CL�C�CL�C$33C)�C.  C3�C7��C=� CB33CGffCQ�C[  Ce  Co�Cy�C���C�ffC�� C�s3C��fC���C���C�� C���C���C�s3C���C�s3C�Y�Cǀ C̙�CѦfC֙�Cۙ�C�� C噚C�s3C� C�ffC�ffD�3D� D� D�fD�fD� D��D$ٚD)�fD.��D3� D8��D=��DB� DG�3DL��DQ� DV�fD[� D`��De�3Dj��Do��Dt�3Dy��D��D�` D��fD�� D�  D�i�D��fD���D�&fD�` D���D��fD�&fD�i�Dڙ�D��3D�#3D�L�D� D�0 1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�bA�(�A�/A�=qA̬A���A���A� �A�K�A�E�A�-A��A�ZA��
A��A�&�A��PA�A�hsA��#A�A�ZA�A�A�7LA�oA���A��PA�p�A�ffA��`A��A���A�-A��FA�A�A��A�A�
=A�K�A���A�
=AsoAhA�AaVAW��AO�hAKt�AF�jA?��A>�!A7C�A.jA( �A#`BAG�A�-Ar�A�HA�A+A+A�+@�b@�1'@�V@��@ܼj@�?}@�G�@�@�b@���@¸R@�K�@�X@���@��u@��;@�X@�n�@�ƨ@�I�@��\@�{@�b@�@�;d@� �@���@�(�@��@��@�+@�Ĝ@��!@���@{@r~�@g�w@^ȴ@W;d@O��@H �@AG�@;ƨ@5�h@0 �@)�#@"~�@(�@ff@t�@�9@�j@
�H1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�bA�(�A�/A�=qA̬A���A���A� �A�K�A�E�A�-A��A�ZA��
A��A�&�A��PA�A�hsA��#A�A�ZA�A�A�7LA�oA���A��PA�p�A�ffA��`A��A���A�-A��FA�A�A��A�A�
=A�K�A���A�
=AsoAhA�AaVAW��AO�hAKt�AF�jA?��A>�!A7C�A.jA( �A#`BAG�A�-Ar�A�HA�A+A+A�+@�b@�1'@�V@��@ܼj@�?}@�G�@�@�b@���@¸R@�K�@�X@���@��u@��;@�X@�n�@�ƨ@�I�@��\@�{@�b@�@�;d@� �@���@�(�@��@��@�+@�Ĝ@��!@���@{@r~�@g�w@^ȴ@W;d@O��@H �@AG�@;ƨ@5�h@0 �@)�#@"~�@(�@ff@t�@�9@�j@
�H1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB�B{B{B'�B��B��B�BB'�BA�BA�B@�B,B�?BŢB�TB�`B�HB��B��B��B��B��BƨB�FB�-B��B�Bk�BVB@�B$�BB��B�B�qB�\B<jB\B
p�B	��B	�PB	C�B	jB	5?B	�B	VB��B�
B	VB�yB�FB��B��B�\B�Bx�Bq�BiyBaHBW
BG�BB�BF�BP�BM�BM�BYBx�B|�B��B�B�ZB�)B��B��B	<jB	_;B	v�B	�B	�oB	��B	��B	�XB	B	��B	�)B	�fB	�B	��B	��B	��B	��B
B
	7B
uB
�B
%�B
0!B
7LB
>wB
E�B
K�B
P�B
VB
[#B
`BB
e`B
k�B
q�B
w�B
z�B
}�B
�B
�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B\BVBVB"�B�uBȴB��B��B!�B;dB;dB:^B1'B�-B��B�/B�BB�/BɺBƨB��B��B��B��B�!B�B��B|�BgmBP�B<jB �B��B�B�yB�^B�JB8RBVB
n�B	�B	�=B	?}B	ffB	1'B	�B		7B�B��B	
=B�`B�-B��B�oB�=B~�Bs�Bl�BdZB\)BQ�BC�B=qBA�BK�BH�BH�BR�Br�Bw�B�\B��B�;B�
B��B�B	6FB	YB	p�B	z�B	�JB	�bB	��B	�3B	�jB	��B	�B	�BB	�sB	�B	�B	��B	��B	��B
B
PB
�B
�B
)�B
1'B
8RB
?}B
E�B
J�B
O�B
T�B
ZB
_;B
e`B
k�B
q�B
t�B
w�B
{�B
}�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<[��<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - SP, where SP is SURFACE PRESSURE (minus 5 dbar for Apf-6,7,8) from next cycle.                                                                                                                                                           none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = RecalS = PSAL(PRES_ADJUSTED,TEMP,Conductivity)                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = celltm_sbe41(RecalS,TEMP,PRES_ADJUSTED,elapsed_time,alpha,tau),elapsed_time=P/mean_rise_rate,P= dbar since the start of the profile for each samples.                                                                                           none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADUSTED=PSAL(corrected by SSP&CTM)+deltaS, where deltaS is calculated by WJO                                                                                                                                                                               SP(NextCycle) = 0.2 dbar                                                                                                                                                                                                                                        none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            r=0.9998(+-0.0001), deepest deltaS=-0.006(+-0.006)                                                                                                                                                                                                              Pressures adjusted by using reported SURFACE PRESSURE. The quoted error is max [2.4, size of pressure adjustment] in dbar.                                                                                                                                      The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   Salinity Recalculation using PRES_ADJUSTED. PSAL_ADJ_ERR : max(combines 1x WJO uncertainty & CTM adjustment , SBE sensor accuracy)                                                                                                                              none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            WJO(2003) salinity adjustment is adopted with SeHyD1.0; large scale 8/4, small scale 4/2; Use interpolate_float_valuesnov2003.m, map_data_grid.m, map_data_grid_t.m; Use T levels <= 4c; Run Const:18;                                                          200710301933072007103019330720071030193307200710301946172007103019461720071030194617200809300000002008093000000020080930000000  JA  ARFMdecpA5_a                                                                20071017184937  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.4                                                                 20071017184940  IP                  G�O�G�O�G�O�                JA  ARCArsal2.1                                                                 20071017184940  IP  PSAL            G�O�G�O�G�O�                JA  ARGQrqcppo_a                                                                20071017184941  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19b                                                                20071017184945  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19b                                                                20071017184945  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.7c                                                                20071017184945  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.7c                                                                20071017184945  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16b                                                                20071017184945  QCP$                G�O�G�O�G�O�           10000JA  ARUP                                                                        20071017190424                      G�O�G�O�G�O�                JA  ARFMdecpA5_a                                                                20071021154302  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.4                                                                 20071021154307  IP                  G�O�G�O�G�O�                JA  ARCArsal2.1                                                                 20071021154307  IP  PSAL            G�O�G�O�G�O�                JA  ARGQrqcppo_a                                                                20071021154308  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19b                                                                20071021154311  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19b                                                                20071021154311  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.7c                                                                20071021154312  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.7c                                                                20071021154312  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16b                                                                20071021154312  QCP$                G�O�G�O�G�O�           10000JA  ARUP                                                                        20071021191032                      G�O�G�O�G�O�                JM  ARCAJMQC1.0                                                                 20071030193307  CV  PRES            G�O�G�O�G�O�                JM  ARCAJMQC1.0                                                                 20071030193307  IP  PSAL            G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20071030194617  CV  PSAL            G�O�G�O�G�O�                JM  ARSQWJO 2.0 SeHyD1.0                                                        20080930000000  CV  PSAL            G�O�G�O�G�O�                JA  RFMTcnvd2.1                                                                 20081008001607  IP                  G�O�G�O�G�O�                JA  ARDU                                                                        20081008025319                      G�O�G�O�G�O�                JM  AREVjmrvp1.2                                                                20090312120709  IP  PRES            G�O�G�O�G�O�                JA  RFMTcnvd2.1                                                                 20090318072358  IP                  G�O�G�O�G�O�                JA  ARDU                                                                        20090318072919                      G�O�G�O�G�O�                JM      JMFC1.0                                                                 20150609203018                      G�O�G�O�G�O�                JA  ARDU                                                                        20150621180536                      G�O�G�O�G�O�                