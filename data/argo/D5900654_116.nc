CDF   "   
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PARAM       N_PROF        N_CALIB       N_LEVELS   s   	N_HISTORY             	   title         Argo float vertical profile    institution       JAMSTEC    source        
Argo float     history       b2007-11-18T09:03:14Z creation;2009-03-18T07:31:06Z update;2015-06-09T21:19:39Z conversion to V3.1;     
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
_FillValue                    iArgo profile    3.1 1.2 19500101000000  5900654 J-ARGO                                                          JAMSTEC                                                         PRES            TEMP            PSAL               tA   JA  20071118090314  20150621190526  A5_23712_116                    2C  D   APEX                            1566                            013004                          846 @ԥ�fP1   @ԥ���@2f�x����b�bM��1   ARGOS   A   A   A   Primary sampling: discrete [spot sampling CTD data]                                                                                                                                                                                                                @�  A��A`  A�33A�  A�  B��B��B0  BD��BZ  Bm33B���B�33B�  B�33B�ffB���B���B���B�33B�ffB�ffB�  B�33C33C��C�C� CffCffC� C$33C(�fC.  C333C7�fC=33CB  CF��CQ33C[� CeffCo33Cy33C�ffC�� C�� C�� C��3C��fC�� C��3C�ffC�ffC���C��fC���C¦fCǦfC̳3Cь�Cֳ3Cۙ�C�� C�3CꙚC�� C���C���DٚD� D��D��D�fD��D��D$��D)��D.��D3��D8� D=�3DB�3DGٚDL�fDQ��DV�3D[��D`�fDe��Dj�3DoٚDt��DyٚD�#3D�i�D���D�� D�)�D�i�D��3D�� D�,�D�i�D�� D�ٚD��D�i�Dک�D��D��D�` D� D���1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @�33AffAY��A�  A���A���B33B  B.ffBC33BXffBk��B�  B�ffB�33B�ffB���B�  B�  B�  B�ffBٙ�B㙚B�33B�ffC ��C33C
�3C�C  C  C�C#��C(� C-��C2��C7� C<��CA��CFffCP��C[�Ce  Cn��Cx��C�33C�L�C���C���C�� C�s3C���C�� C�33C�33C�Y�C�s3C���C�s3C�s3C̀ C�Y�Cր C�ffC�L�C� C�ffC��C���C�Y�D� D�fD�3D�3D��D�3D� D$�3D)�3D.�3D3�3D8�fD=��DB��DG� DL��DQ� DV��D[�3D`��De�3Dj��Do� Dt�3Dy� D�fD�\�D���D��3D��D�\�D��fD��3D�  D�\�D��3D���D� D�\�Dڜ�D���D� D�S3D�3D�� 1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A˶FA˴9A˕�A�S�A�A���A��A��mA��A��mA��#A���A���A�ƨAʥ�A�  A�ĜA���A��TA��jA��uA�K�A���A���A���A��A�/A��A��yA�-A�M�A���A��jA��A� �A�ĜA��A��PA�bA�/As�;Al�yA[��AO��AIp�AC�^A;ƨA5G�A.9XA%�
A �9A��A�Az�A�yA&�A$�A\)A
�/A��A5?@�n�@���@��y@�{@�\@�`B@�hs@�r�@��m@֗�@�  @�@˅@�p�@���@��
@�+@���@��y@�Ĝ@�x�@��-@�M�@�S�@�\)@�?}@��#@��m@�{@��@��T@��@�V@�K�@���@��/@v��@m�@ep�@\I�@U`B@MO�@Fv�@>��@8 �@0Ĝ@+�@%��@ ��@��@\)@�H@ff@
�\1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A˶FA˴9A˕�A�S�A�A���A��A��mA��A��mA��#A���A���A�ƨAʥ�A�  A�ĜA���A��TA��jA��uA�K�A���A���A���A��A�/A��A��yA�-A�M�A���A��jA��A� �A�ĜA��A��PA�bA�/As�;Al�yA[��AO��AIp�AC�^A;ƨA5G�A.9XA%�
A �9A��A�Az�A�yA&�A$�A\)A
�/A��A5?@�n�@���@��y@�{@�\@�`B@�hs@�r�@��m@֗�@�  @�@˅@�p�@���@��
@�+@���@��y@�Ĝ@�x�@��-@�M�@�S�@�\)@�?}@��#@��m@�{@��@��T@��@�V@�K�@���@��/@v��@m�@ep�@\I�@U`B@MO�@Fv�@>��@8 �@0Ĝ@+�@%��@ ��@��@\)@�H@ff@
�\1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oBw�Bw�Bu�Br�Bp�Bq�Bu�Bx�B�B�PB�oB�uB�{B��B��B��B�mB{BPB1BgmB�bB�\B��B��B�1B�\B_;B~�Bt�BffBB�BDB�NB�dB�+BW
B
��B
��B
E�B	�B	M�B�B�jB�B��B��B��B�VB�bB��B��B��B	\B	"�B	2-B	+B	#�B	�B	�B	%B	+B		7B�mB�)B�B��B	DB	%�B	>wB	q�B	�B	�VB	��B	��B	�B	�FB	�dB	�wB	ĜB	ȴB	��B	�B	�)B	�mB	�B	�B	��B	��B	��B
B
%B
	7B
JB
bB
�B
�B
&�B
,B
33B
;dB
@�B
G�B
L�B
S�B
YB
^5B
cTB
hsB
l�B
p�B
u�B
y�B
}�B
�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 Bw�Bw�Bu�Br�Bp�Bq�Bu�Bx�B�B�PB�oB�uB�{B��B��B�B�B�BbB
=BiyB�hB�hB��B��B�7B�hB`BB� Bv�BhsBE�BPB�`B�qB�7B[#B
��B
��B
J�B	�%B	Q�B�B�wB�B��B��B��B�bB�hB��B��B	  B	\B	#�B	33B	,B	$�B	�B	�B	+B	1B	DB�sB�/B�B��B	DB	%�B	>wB	q�B	�B	�VB	��B	��B	�B	�FB	�dB	�wB	ĜB	ȴB	��B	�B	�)B	�mB	�B	�B	��B	��B	��B
B
%B
	7B
JB
bB
�B
�B
&�B
,B
33B
;dB
@�B
G�B
L�B
S�B
YB
^5B
cTB
hsB
l�B
p�B
u�B
y�B
}�B
�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - SP, where SP is SURFACE PRESSURE (minus 5 dbar for Apf-6,7,8) from next cycle.                                                                                                                                                           none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = RecalS = PSAL(PRES_ADJUSTED,TEMP,Conductivity)                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = celltm_sbe41(RecalS,TEMP,PRES_ADJUSTED,elapsed_time,alpha,tau),elapsed_time=P/mean_rise_rate,P= dbar since the start of the profile for each samples.                                                                                           none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            SP(NextCycle) = 0.4 dbar                                                                                                                                                                                                                                        none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using reported SURFACE PRESSURE. The quoted error is max [2.4, size of pressure adjustment] in dbar.                                                                                                                                      The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   Salinity Recalculation using PRES_ADJUSTED. PSAL_ADJ_ERR : SBE sensor accuracy & CTM adjustment                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         200712040243272007120402432720071204024327200712050502402007120505024020071205050240200809050000002008090500000020080905000000  JA  ARFMdecpA5_a                                                                20071118090252  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.4                                                                 20071118090314  IP                  G�O�G�O�G�O�                JA  ARCArsal2.1                                                                 20071118090317  IP  PSAL            G�O�G�O�G�O�                JA  ARGQrqcppo_a                                                                20071118090319  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19b                                                                20071118090329  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19b                                                                20071118090329  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.7c                                                                20071118090329  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.7c                                                                20071118090329  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16b                                                                20071118090330  QCP$                G�O�G�O�G�O�           10000JA  ARUP                                                                        20071118102415                      G�O�G�O�G�O�                JA  ARFMdecpA5_a                                                                20071122035638  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.4                                                                 20071122035643  IP                  G�O�G�O�G�O�                JA  ARCArsal2.1                                                                 20071122035643  IP  PSAL            G�O�G�O�G�O�                JA  ARGQrqcppo_a                                                                20071122035644  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19b                                                                20071122035648  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19b                                                                20071122035648  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.7c                                                                20071122035648  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.7c                                                                20071122035648  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16b                                                                20071122035648  QCP$                G�O�G�O�G�O�           10000JA  ARUP                                                                        20071122052157                      G�O�G�O�G�O�                JM  ARGQJMQC1.0                                                                 20071121232002  CV  DAT$            G�O�G�O�F�(?                JM  ARGQJMQC1.0                                                                 20071121232002  CV  LAT$            G�O�G�O�A�33                JM  ARGQJMQC1.0                                                                 20071121232002  CV  LON$            G�O�G�O���                JM  ARCAJMQC1.0                                                                 20071204024327  CV  PRES            G�O�G�O�G�O�                JM  ARCAJMQC1.0                                                                 20071204024327  IP  PSAL            G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20071205050240  CV  PSAL            G�O�G�O�G�O�                JM  ARSQWJO 2.0 SeHyD1.0                                                        20080905000000  IP  PSAL            G�O�G�O�G�O�                JA  RFMTcnvd2.1                                                                 20080911042224  IP                  G�O�G�O�G�O�                JA  ARDU                                                                        20080911054315                      G�O�G�O�G�O�                JM  AREVjmrvp1.2                                                                20090312120834  IP  PRES            G�O�G�O�G�O�                JA  RFMTcnvd2.1                                                                 20090318072610  IP                  G�O�G�O�G�O�                JA  ARDU                                                                        20090318073106                      G�O�G�O�G�O�                JM      JMFC1.0                                                                 20150609211929                      G�O�G�O�G�O�                JA  ARDU                                                                        20150621190526                      G�O�G�O�G�O�                