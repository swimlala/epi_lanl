CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PARAM       N_PROF        N_CALIB       N_LEVELS   s   	N_HISTORY             	   title         Argo float vertical profile    institution       JAMSTEC    source        
Argo float     history       b2007-12-08T10:50:54Z creation;2009-03-18T07:28:15Z update;2015-06-09T21:20:04Z conversion to V3.1;     
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
_FillValue                    iArgo profile    3.1 1.2 19500101000000  5900654 J-ARGO                                                          JAMSTEC                                                         PRES            TEMP            PSAL               vA   JA  20071208105054  20150621190521  A5_23712_118                    2C  D   APEX                            1566                            013004                          846 @Ԫr�a�1   @Ԫ���^@2�S����b��S���1   ARGOS   A   A   A   Primary sampling: discrete [spot sampling CTD data]                                                                                                                                                                                                                @���A��Aa��A�ffA���A陚B	33B��B/��BC��BY33Bn  B�  B�33B���B���B���B�33B�ffB�33BЙ�Bڙ�B�  B���B�  C� C� CffCL�CffCL�CffC#�fC)ffC.ffC2��C833C=  CBffCG  CQffC[��Ce�CoL�Cy33C���C�ٚC���C�� C�� C�s3C��3C�s3C�s3C���C���C�ffC��3C¦fCǳ3C̦fCь�Cր Cۀ C���C�3C�ffC�s3C��3C���D��DٚD��D��D�3D��D��D$� D)�fD.ٚD3�fD8��D=��DB�fDG��DL��DQ��DV� D[�3D`ٚDe� DjٚDo��DtٚDy�fD�  D�ffD���D��D��D�i�D��3D�� D�&fD�i�D��3D���D�)�D�ffDک�D��3D�  D�` D� D�� 1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @�  AffA[33A�33A�ffA�ffB��B33B.  BB  BW��BlffB�33B�ffB���B�  B���B�ffB���B�ffB���B���B�33B�  B�33C�C�C  C�fC  C�fC  C#� C)  C.  C2ffC7��C<��CB  CF��CQ  C[33Cd�3Cn�fCx��C���C��fC�Y�C���C�L�C�@ C�� C�@ C�@ C�ffC�ffC�33C�� C�s3Cǀ C�s3C�Y�C�L�C�L�C�ffC� C�33C�@ C� C�ffD�3D� D�3D� D��D�3D�3D$�fD)��D.� D3��D8�3D=� DB��DG� DL�3DQ�3DV�fD[��D`� De�fDj� Do�3Dt� Dy��D�3D�Y�D�� D���D��D�\�D��fD��3D��D�\�D��fD�� D��D�Y�Dڜ�D��fD�3D�S3D�3D��31111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�C�A�E�A�G�A�C�A�G�A�I�A�K�A�M�A�O�A�A�A�9XA�7LA�7LA�7LA���A���A�33A���A��A��7A�A�ĜA�hsA���A��7A�`BA�|�A�$�A�XA�S�A��A���A�Q�A��RA�/A�  A�\)A��HA��+A�p�A��A|�AmG�Aa��ASp�AE�A=��A6�A+��A(z�A"��A\)A�
A�A
�HAM�A��A��A��A �@�`B@�P@�V@�
=@�b@�ƨ@�D@ܓu@׾w@��@���@��@͡�@�J@ț�@��m@�`B@�dZ@���@��h@�Z@�p�@��@��w@���@��\@��
@�x�@�S�@���@�b@�/@�dZ@���@��@���@��@{�
@r�!@ihs@`��@Xr�@P�@I7L@A�#@:�@2��@,�D@%@ A�@��@p�@X@�@
�\1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�C�A�E�A�G�A�C�A�G�A�I�A�K�A�M�A�O�A�A�A�9XA�7LA�7LA�7LA���A���A�33A���A��A��7A�A�ĜA�hsA���A��7A�`BA�|�A�$�A�XA�S�A��A���A�Q�A��RA�/A�  A�\)A��HA��+A�p�A��A|�AmG�Aa��ASp�AE�A=��A6�A+��A(z�A"��A\)A�
A�A
�HAM�A��A��A��A �@�`B@�P@�V@�
=@�b@�ƨ@�D@ܓu@׾w@��@���@��@͡�@�J@ț�@��m@�`B@�dZ@���@��h@�Z@�p�@��@��w@���@��\@��
@�x�@�S�@���@�b@�/@�dZ@���@��@���@��@{�
@r�!@ihs@`��@Xr�@P�@I7L@A�#@:�@2��@,�D@%@ A�@��@p�@X@�@
�\1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oBƨBƨBƨBƨBŢBƨBƨBŢBŢBƨBǮBȴBɺBɺB�
BVB^5BL�BK�BA�BP�B~�B�B�7B��B�oB�PB�hB�Bt�B�B^5BJ�B/B0!B#�BB�B�^B}�B
�B
1B	p�B	�BĜB��B��B�3BÖBŢB�?B��B�Bl�Bu�B�B��B�mB��B�B	B�B��B�B�B	B	�B	oB	oB	$�B	cTB	y�B	�DB	��B	��B	�hB	��B	�9B	�RB	�RB	��B	��B	��B	�B	�;B	�fB	�yB	�B	�B	��B	��B	��B
B
+B
DB
oB
�B
#�B
+B
/B
8RB
?}B
D�B
J�B
O�B
VB
]/B
aHB
gmB
l�B
q�B
v�B
y�B
~�B
�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 BƨBƨBƨBƨBŢBƨBƨBŢBŢBƨBǮBȴBɺB��B�BYBaHBQ�BN�BC�BR�B� B�+B�DB��B�uB�\B�{B�Bv�B�%B`BBL�B0!B1'B%�B%B�B�qB�B
��B
PB	s�B	 �BǮB��B��B�FBĜBǮB�LB��B�Bm�Bv�B�B��B�mB��B�B	B�B��B�B�B	B	�B	uB	oB	$�B	cTB	y�B	�DB	��B	��B	�hB	��B	�9B	�RB	�RB	��B	��B	��B	�B	�;B	�fB	�yB	�B	�B	��B	��B	��B
B
+B
DB
oB
�B
#�B
+B
/B
8RB
?}B
D�B
J�B
O�B
VB
]/B
aHB
gmB
l�B
q�B
v�B
y�B
~�B
�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - SP, where SP is SURFACE PRESSURE (minus 5 dbar for Apf-6,7,8) from next cycle.                                                                                                                                                           none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = RecalS = PSAL(PRES_ADJUSTED,TEMP,Conductivity)                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = celltm_sbe41(RecalS,TEMP,PRES_ADJUSTED,elapsed_time,alpha,tau),elapsed_time=P/mean_rise_rate,P= dbar since the start of the profile for each samples.                                                                                           none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            SP(NextCycle) = 0.4 dbar                                                                                                                                                                                                                                        none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using reported SURFACE PRESSURE. The quoted error is max [2.4, size of pressure adjustment] in dbar.                                                                                                                                      The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   Salinity Recalculation using PRES_ADJUSTED. PSAL_ADJ_ERR : SBE sensor accuracy & CTM adjustment                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         200712211338212007122113382120071221133821200712211352462007122113524620071221135246200809050000002008090500000020080905000000  JA  ARFMdecpA5_a                                                                20071208105052  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.4                                                                 20071208105054  IP                  G�O�G�O�G�O�                JA  ARCArsal2.1                                                                 20071208105055  IP  PSAL            G�O�G�O�G�O�                JA  ARGQrqcppo_a                                                                20071208105055  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19b                                                                20071208105059  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19b                                                                20071208105059  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.7c                                                                20071208105059  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.7c                                                                20071208105059  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16b                                                                20071208105100  QCP$                G�O�G�O�G�O�           10000JA  ARUP                                                                        20071208110449                      G�O�G�O�G�O�                JA  ARFMdecpA5_a                                                                20071212035127  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.4                                                                 20071212035132  IP                  G�O�G�O�G�O�                JA  ARCArsal2.1                                                                 20071212035133  IP  PSAL            G�O�G�O�G�O�                JA  ARGQrqcppo_a                                                                20071212035133  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19b                                                                20071212035137  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19b                                                                20071212035137  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.7c                                                                20071212035137  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.7c                                                                20071212035137  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16b                                                                20071212035138  QCP$                G�O�G�O�G�O�           10000JA  ARUP                                                                        20071212052022                      G�O�G�O�G�O�                JM  ARCAJMQC1.0                                                                 20071221133821  CV  PRES            G�O�G�O�G�O�                JM  ARCAJMQC1.0                                                                 20071221133821  IP  PSAL            G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20071221135246  CV  PSAL            G�O�G�O�G�O�                JM  ARSQWJO 2.0 SeHyD1.0                                                        20080905000000  IP  PSAL            G�O�G�O�G�O�                JA  RFMTcnvd2.1                                                                 20080911042226  IP                  G�O�G�O�G�O�                JA  ARDU                                                                        20080911054358                      G�O�G�O�G�O�                JM  AREVjmrvp1.2                                                                20090312120834  IP  PRES            G�O�G�O�G�O�                JA  RFMTcnvd2.1                                                                 20090318072246  IP                  G�O�G�O�G�O�                JA  ARDU                                                                        20090318072815                      G�O�G�O�G�O�                JM      JMFC1.0                                                                 20150609211953                      G�O�G�O�G�O�                JA  ARDU                                                                        20150621190521                      G�O�G�O�G�O�                