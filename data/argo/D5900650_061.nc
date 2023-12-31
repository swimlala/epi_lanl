CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PARAM       N_PROF        N_CALIB       N_LEVELS   s   	N_HISTORY             	   title         Argo float vertical profile    institution       JAMSTEC    source        
Argo float     history       b2006-05-15T04:49:58Z creation;2009-03-18T07:31:30Z update;2015-06-09T20:05:04Z conversion to V3.1;     
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
_FillValue                    iArgo profile    3.1 1.2 19500101000000  5900650 J-ARGO                                                          JAMSTEC                                                         PRES            TEMP            PSAL               =A   JA  20060515044958  20150614054518  A5_23632_061                    2C  D   APEX                            1557                            013004                          846 @�\�1   @�{��@4�S����c]��l�D1   ARGOS   A   A   A   Primary sampling: discrete [spot sampling CTD data]                                                                                                                                                                                                                @���A33Ad��A�ffA�ffA陚B	��BffB2ffBF��BZffBlffB�  B�  B�  B���B���B���B���B���B�ffBڙ�B���B�  B���C ��C�3C�CffC33CffCffC$ffC)ffC.33C333C8ffC=L�CBffCG� CQL�C[� Ce� Co33Cy33C�� C���C��3C���C��3C�� C��fC�� C��fC�� C�� C�� C���C¦fCǙ�C̙�C�� C�� CۦfC���C噚CꙚC�s3C��fC�� D�3D� D�3D�3D�3D��D� D$�fD)�fD.��D3�3D8� D=�fDB� DGٚDL�fDQ� DV� D[�fD`�3De��Dj� Do�3Dt� Dy��D�&fD�i�D���D��3D�#3D�l�D���D���D�)�D�i�D��fD�� D�  D�p Dڣ3D�� D��D�i�D�fD��31111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @�ffA��Ac33A���A���A���B	33B  B2  BFffBZ  Bl  B��B���B���B���B���B���B���Bƙ�B�33B�ffB㙚B���B���C �3C��C  CL�C�CL�CL�C$L�C)L�C.�C3�C8L�C=33CBL�CGffCQ33C[ffCeffCo�Cy�C�s3C�� C��fC���C��fC�s3C���C��3C���C��3C��3C�s3C���C�Cǌ�Č�Cѳ3Cֳ3Cۙ�C�� C��C��C�ffC���C�s3D��D��D��D��D��D�3D��D$� D)� D.�fD3��D8��D=� DB��DG�3DL� DQ��DV��D[� D`��De�fDjٚDo��DtٚDy�fD�#3D�ffD���D�� D�  D�i�D��fD��D�&fD�ffD��3D���D��D�l�Dڠ D���D��D�ffD�3D�� 1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�A�VA���Aũ�A�x�A�+AčPA�;dA��A�
=A��A�$�A�&�A��yA���A�E�A�p�A���A��A�^5A�S�A�x�A��-A��mA��RA��A���A��A�+A���A��PA�33A�E�A���A��hA�E�A�5?A���A��A��#A�ffA�r�Ay�AtȴAjz�Ai�mAcS�ASXAMS�AC�FA>��A8�uA2�`A-�wA&��A#��A �A�wA��AAC�A�wA
��A�A|�@�%@�1'@�^@�-@柾@֗�@��@���@��7@�`B@���@��R@��y@�S�@�I�@�~�@�V@�ƨ@��T@���@���@�;d@��D@�O�@��@�@�x�@�|�@��w@��@�z�@wl�@p��@hĜ@bJ@Z��@R�@KdZ@E�@>@7l�@0bN@*��@#��@dZ@�y@��@  @�@�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�A�VA���Aũ�A�x�A�+AčPA�;dA��A�
=A��A�$�A�&�A��yA���A�E�A�p�A���A��A�^5A�S�A�x�A��-A��mA��RA��A���A��A�+A���A��PA�33A�E�A���A��hA�E�A�5?A���A��A��#A�ffA�r�Ay�AtȴAjz�Ai�mAcS�ASXAMS�AC�FA>��A8�uA2�`A-�wA&��A#��A �A�wA��AAC�A�wA
��A�A|�@�%@�1'@�^@�-@柾@֗�@��@���@��7@�`B@���@��R@��y@�S�@�I�@�~�@�V@�ƨ@��T@���@���@�;d@��D@�O�@��@�@�x�@�|�@��w@��@�z�@wl�@p��@hĜ@bJ@Z��@R�@KdZ@E�@>@7l�@0bN@*��@#��@dZ@�y@��@  @�@�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB	��B	��B
B
%B
+B
�B
"�B
&�B
%�B
)�B
A�B
�}BhB<jBu�B�!B�
B�?BɺB�B�ZB��BbB?}B^5B|�B�=B�+Bw�B�{B� B\)B?}B��Be`B�B
ĜB
��B
� BB
�hB
6FB	v�B	W
B	:^B	{�B	�B��B�#B��B��Bu�B{�B�B�JB�VB�+B�DB��B��B��B��B��B|�B��B|�B� B��B��B��BhsBR�BffB�VB��B�9B��B��B	49B	O�B	x�B	�1B	��B	��B	�B	�RB	��B	��B	�#B	�TB	�fB	�B	��B	��B	��B
+B
uB
�B
&�B
0!B
5?B
=qB
B�B
H�B
N�B
S�B
ZB
_;B
e`B
l�B
o�B
q�B
u�B
x�B
|�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B	��B	��B
B
%B
+B
�B
"�B
&�B
%�B
+B
E�B
BoB<jBu�B�'B�B�RB��B�#B�`B��BhB?}B_;B|�B�DB�1Bw�B��B�B^5BB�B��BhsB�B
ƨB
��B
~�B+B
�{B
;dB	w�B	ZB	:^B	}�B	�+B	B�/B��B��Bw�B|�B�%B�PB�bB�1B�JB��B��B��B��B��B|�B��B}�B� B��B��B��BiyBS�BffB�\B��B�9B��B��B	49B	O�B	x�B	�1B	��B	��B	�B	�RB	��B	��B	�#B	�TB	�fB	�B	��B	��B	��B
+B
uB
�B
&�B
0!B
5?B
=qB
B�B
H�B
N�B
S�B
ZB
_;B
e`B
l�B
o�B
q�B
u�B
x�B
|�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - SP, where SP is SURFACE PRESSURE (minus 5 dbar for Apf-6,7,8) from next cycle.                                                                                                                                                           none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = RecalS = PSAL(PRES_ADJUSTED,TEMP,Conductivity)                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = celltm_sbe41(RecalS,TEMP,PRES_ADJUSTED,elapsed_time,alpha,tau),elapsed_time=P/mean_rise_rate,P= dbar since the start of the profile for each samples.                                                                                           none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            SP(NextCycle) = 0.1 dbar                                                                                                                                                                                                                                        none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using reported SURFACE PRESSURE. The quoted error is max [2.4, size of pressure adjustment] in dbar.                                                                                                                                      The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   Salinity Recalculation using PRES_ADJ; PSAL_ADJ_ERR : max(sum of RecalS & CTM errors, 0.01(PSS-78))                                                                                                                                                             none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         200706281404362007062814043620070628140436200706290309412007062903094120070629030941200707020000002007070200000020070702000000  JA  ARFMfmtp2.2                                                                 20060515044958  IP                  G�O�G�O�G�O�                JA  ARCAsspa2.0                                                                 20060515044958  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcp2.4                                                                 20060515044959  QCP$                G�O�G�O�G�O�           3F6BCJA  ARUP                                                                        20060515045747                      G�O�G�O�G�O�                JA  ARFMfmtp2.2                                                                 20060519034232  IP                  G�O�G�O�G�O�                JA  ARCAsspa2.0                                                                 20060519034232  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcp2.4                                                                 20060519034232  QCP$                G�O�G�O�G�O�           3F6BCJA  ARUP                                                                        20060519035054                      G�O�G�O�G�O�                JA  ARUP                                                                        20060602051137                      G�O�G�O�G�O�                JA  ARGQrqcp2.5                                                                 20060627031711  QCP$                G�O�G�O�G�O�           1FB7CJA  ARGQaqcp2.5                                                                 20060627031711  QCP$                G�O�G�O�G�O�           1FB40JA  ARUP                                                                        20060627035412                      G�O�G�O�G�O�                JM  ARCAJMQC1.0                                                                 20070628140436  CV  PRES            G�O�G�O�G�O�                JM  ARCAJMQC1.0                                                                 20070628140436  CV  PSAL            G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20070629030941  CV  PSAL            G�O�G�O�G�O�                JM  ARSQWJO 2.0 SeHyD1.0                                                        20070702000000  IP  PSAL            G�O�G�O�G�O�                JA  RFMTcnvd2.1                                                                 20071001072803  IP                  G�O�G�O�G�O�                JA  ARDU                                                                        20071001091733                      G�O�G�O�G�O�                JM  AREVjmrvp1.2                                                                20090312120642  IP  PRES            G�O�G�O�G�O�                JA  RFMTcnvd2.1                                                                 20090318072641  IP                  G�O�G�O�G�O�                JA  ARDU                                                                        20090318073130                      G�O�G�O�G�O�                JM      JMFC1.0                                                                 20150609200459                      G�O�G�O�G�O�                JA  ARDU                                                                        20150614054518                      G�O�G�O�G�O�                