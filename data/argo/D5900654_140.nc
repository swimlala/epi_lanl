CDF   :   
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PARAM       N_PROF        N_CALIB       N_LEVELS   s   	N_HISTORY             	   title         Argo float vertical profile    institution       JAMSTEC    source        
Argo float     history       b2008-07-15T06:49:59Z creation;2009-09-01T08:43:28Z update;2015-06-09T21:24:10Z conversion to V3.1;     
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
_FillValue                    iArgo profile    3.1 1.2 19500101000000  5900654 J-ARGO                                                          JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA  20080715064959  20150621190525  A5_23712_140                    2C  D   APEX                            1566                            013004                          846 @����
1   @��~ܺ�@3/��-V�c���$�1   ARGOS   A   A   B   Primary sampling: discrete [spot sampling CTD data]                                                                                                                                                                                                                @�ffA  Ac33A�  A���A�ffB
  B��B1��BF  BY��Bl��B���B���B�33B���B���B���B���B�ffBЙ�B�33B�33B�ffB�  C �fC� C��CffCffCffCffC$33C)ffC.33C3� C8� C=�CBffCGL�CP�fC[  Ce� Co� Cy  C���C���C�s3C���C��fC�� C��3C�� C���C��3C�� C��3C�ٚC���Cǳ3Č�C�� C֙�CۦfC�fC��C� C� C�� C��fDٚD� D��D�fD� D�fD�3D$�fD)��D.��D3�fD8� D=��DB�3DG�3DL��DQ�3DV��D[�fD`� De�3Dj�fDo�3Dt�3Dy�3D�&fD�c3D��fD��fD�&fD�c3D���D��fD�fD�c3D���D���D�)�D�s3Dڣ3D���D�  D�Y�D�3D�Vf1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @�33AffAY��A�33A���A噚B��BffB/33BC��BW33BjffB~��B���B�  B�ffB���B�ffB�ffB�33B�ffB�  B�  B�33B���C L�C�fC  C��C��C��C��C#��C(��C-��C2�fC7�fC<� CA��CF�3CPL�CZffCd�fCn�fCxffC�@ C�@ C�&fC�� C�Y�C�33C�ffC�33C�@ C�ffC�33C�ffC���C C�ffC�@ C�s3C�L�C�Y�C�Y�C�@ C�33C�33C�s3C�Y�D�3D��D�fD� D��D� D��D$� D)�fD.�fD3� D8��D=�fDB��DG��DL�fDQ��DV�fD[� D`��De��Dj� Do��Dt��Dy��D�3D�P D��3D��3D�3D�P D��fD��3D�3D�P D��fD�ٚD�fD�` Dڐ D�ɚD��D�FfD� D�C31111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�v�A�XA�bA�&�A԰!A�Q�A��AҬA�ĜAхA�K�A���A�hsA�ZA�Q�A���A�~�Aȇ+A�ĜA�A�"�A�r�A��A�JA�ffA���A���A�x�A��TA�I�A��+A�v�A�n�A��A�A���A���A��A���A�Q�A�
=A��hA�&�A|5?AgdZAa%AU��AK��AH�uA<��A5�-A0�A*�HA%K�A!7LA~�AffA�RA	�7A�hA�T@�+@�E�@��@��/@���@���@�ƨ@��T@�O�@Ь@�E�@��@��w@� �@�A�@�C�@��w@�=q@�  @�@��@�x�@�A�@��@�K�@���@��@�V@���@��@��@���@�-@�Q�@��y@�n�@xr�@l�@d��@\�@U��@N��@G;d@AX@;��@2^5@+dZ@$�D@@x�@O�@S�@$�@�F1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111113 A�v�A�XA�bA�&�A԰!A�Q�A��AҬA�ĜAхA�K�A���A�hsA�ZA�Q�A���A�~�Aȇ+A�ĜA�A�"�A�r�A��A�JA�ffA���A���A�x�A��TA�I�A��+A�v�A�n�A��A�A���A���A��A���A�Q�A�
=A��hA�&�A|5?AgdZAa%AU��AK��AH�uA<��A5�-A0�A*�HA%K�A!7LA~�AffA�RA	�7A�hA�T@�+@�E�@��@��/@���@���@�ƨ@��T@�O�@Ь@�E�@��@��w@� �@�A�@�C�@��w@�=q@�  @�@��@�x�@�A�@��@�K�@���@��@�V@���@��@��@���@�-@�Q�@��y@�n�@xr�@l�@d��@\�@U��@N��@G;d@AX@;��@2^5@+dZ@$�D@@x�@O�@S�@$�@�F1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB
��B
�)B
�B
��B
��B
��B
ƨB
��B
�'B
�'B
�FB
B
�B
�B
��B
�B
�B+BE�BT�Bx�B�oB�PB�7B�+B��B��B�PB{�Bt�BI�B49B �B1B�B��BÖB�'B��B�%BK�B
=B
��B
DB	�7B	`BB	�B��B�B��B�wB�3B��B��B��B�hB�B{�By�Bt�Bq�Bs�Bv�Bu�Br�Bp�B�1B�oB��B��B��B�B	+B	�B	?}B	_;B	�%B	��B	��B	��B	�FB	�9B	ĜB	ɺB	�B	�#B	�;B	�NB	�sB	�B	��B	��B	��B
B
%B
VB
{B
�B
(�B
.B
7LB
<jB
A�B
G�B
L�B
Q�B
ZB
`BB
gmB
l�B
q�B
t�B
w�    B
� 1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111143 B
��B
�)B
�#B
��B
��B
��B
ƨB
B
�'B
�'B
�FB
B
�B
�#B
��B
�B
�#B+BL�BXBy�B�oB�VB�DB�=B��B��B�\B}�Bw�BK�B6FB"�BDB�B��BĜB�'B�B�1BM�BPB
��B
bB	�DB	cTB	!�B��B�B��B�}B�?B��B��B��B�uB�B}�Bz�Bv�Br�Bt�Bw�Bv�Bs�Bq�B�1B�oB��B��B��B�B	+B	�B	?}B	_;B	�%B	��B	��B	��B	�FB	�9B	ĜB	ɺB	�B	�#B	�;B	�NB	�sB	�B	��B	��B	��B
B
%B
VB
{B
�B
(�B
.B
7LB
<jB
A�B
G�B
L�B
Q�B
ZB
`BB
gmB
l�B
q�B
t�B
w�G�O�B
� 1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141 <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ = PRES-SP(NextCycle), where SP is SURFACE PRESSURE from next cycle                                                                                                                                                                                     TEMP_ADJUSTED = TEMP                                                                                                                                                                                                                                            PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,PRES_ADJ,elapsed_time,alpha,tau),elapsed_time=P/mean_rise_rate,P=dbar since the start of the profile for each samples                                                                                                       None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            SP(NextCycle)=0.6(dbar)                                                                                                                                                                                                                                         None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE; PRES_ADJ_ERR : Manufacture sensor accuracy                                                                                                                                                                 TEMP_ADJ_ERR : SBE sensor accuracy                                                                                                                                                                                                                              Salinity Recalculation using PRES_ADJ; PSAL_ADJ_ERR : max(sum of RecalS & CTM errors, 0.01(PSS-78))                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         200807281359552008072813595520080728135955200807281419002008072814190020080728141900200908260000002009082600000020090826000000  JA  ARFMdecpA5_a                                                                20080715064957  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.4                                                                 20080715064959  IP                  G�O�G�O�G�O�                JA  ARCArsal2.1                                                                 20080715064959  IP  PSAL            G�O�G�O�G�O�                JA  ARGQrqcppo_a                                                                20080715065000  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19b                                                                20080715065004  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19b                                                                20080715065004  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8a                                                                20080715065004  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcp2.8a                                                                20080715065004  QCF$                G�O�G�O�G�O�            4040JA  ARGQaqcp2.8a                                                                20080715065004  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8a                                                                20080715065004  QCF$                G�O�G�O�G�O�            4040JA  ARGQrqcpt16b                                                                20080715065004  QCP$                G�O�G�O�G�O�           10000JA  ARUP                                                                        20080715070514                      G�O�G�O�G�O�                JA  ARFMdecpA5_a                                                                20080719034822  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.4                                                                 20080719034827  IP                  G�O�G�O�G�O�                JA  ARCArsal2.1                                                                 20080719034827  IP  PSAL            G�O�G�O�G�O�                JA  ARGQrqcppo_a                                                                20080719034828  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19b                                                                20080719034832  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19b                                                                20080719034832  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8a                                                                20080719034832  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcp2.8a                                                                20080719034832  QCF$                G�O�G�O�G�O�            4040JA  ARGQaqcp2.8a                                                                20080719034832  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8a                                                                20080719034832  QCF$                G�O�G�O�G�O�            4040JA  ARGQrqcpt16b                                                                20080719034832  QCP$                G�O�G�O�G�O�           10000JA  ARUP                                                                        20080719052507                      G�O�G�O�G�O�                JA  ARCArsal2.1                                                                 20080912002136  IP  PSAL            G�O�G�O�G�O�                JA  ARGQrqcppo_a                                                                20080912002136  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19b                                                                20080912002140  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19b                                                                20080912002140  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8a                                                                20080912002140  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcp2.8a                                                                20080912002140  QCF$                G�O�G�O�G�O�            4000JA  ARGQaqcp2.8a                                                                20080912002140  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8a                                                                20080912002140  QCF$                G�O�G�O�G�O�            4000JA  ARGQrqcpt16b                                                                20080912002141  QCP$                G�O�G�O�G�O�           10000JA  ARUP                                                                        20080912015953                      G�O�G�O�G�O�                JA  ARFMdecpA5_a                                                                20080719034822  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.5                                                                 20090414042854  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.0                                                                 20090414042854  IP  PRES            G�O�G�O�G�O�                JA  ARCArsal2.1a                                                                20090414042854  IP  PSAL            G�O�G�O�G�O�                JA  ARGQrqcppo_a                                                                20090414042855  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19b                                                                20090414042856  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19b                                                                20090414042856  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8b                                                                20090414042856  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcp2.8b                                                                20090414042856  QCF$                G�O�G�O�G�O�            4040JA  ARGQaqcp2.8b                                                                20090414042856  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8b                                                                20090414042856  QCF$                G�O�G�O�G�O�            4040JA  ARGQrqcpt16b                                                                20090414042856  QCP$                G�O�G�O�G�O�           10000JA  ARUP                                                                        20090414042942                      G�O�G�O�G�O�                JM  ARGQJMQC1.0                                                                 20080721105624  CV  DAT$            G�O�G�O�F�3                JM  ARSQJMQC1.0                                                                 20080721105624  CF  TEMP            D�VfD�VfG�O�                JM  ARSQJMQC1.0                                                                 20080721105624  CF  PSAL            D�VfD�VfG�O�                JM  ARCAJMQC1.0                                                                 20080728135955  CV  PRES            G�O�G�O�G�O�                JM  ARCAJMQC1.0                                                                 20080728135955  IP  PSAL            G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20080728141900  CV  PSAL            G�O�G�O�G�O�                JM  ARSQOW  1.1 SeHyD1.0                                                        20090826000000  IP  PSAL            G�O�G�O�G�O�                JA  RFMTcnvd2.1                                                                 20090901084255  IP                  G�O�G�O�G�O�                JA  ARDU                                                                        20090901084328                      G�O�G�O�G�O�                JM      JMFC1.0                                                                 20150609212404                      G�O�G�O�G�O�                JA  ARDU                                                                        20150621190525                      G�O�G�O�G�O�                