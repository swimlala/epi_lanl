CDF   2   
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PARAM       N_PROF        N_CALIB       N_LEVELS   s   	N_HISTORY             	   title         Argo float vertical profile    institution       JAMSTEC    source        
Argo float     history       b2007-11-05T06:50:49Z creation;2013-09-24T05:25:46Z update;2015-06-09T19:25:27Z conversion to V3.1;     
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
_FillValue                    iArgo profile    3.1 1.2 19500101000000  5900648 J-ARGO                                                          JAMSTEC                                                         PRES            TEMP            PSAL               sA   JA  20071105065049  20150614052512  A5_28347_115                    2C  D   APEX                            1316                            013004                          846 @ԡ�1�b	1   @ԡ��:g�@7�Q���c�V�1   ARGOS   F   F   F   Primary sampling: discrete [spot sampling CTD data]                                                                                                                                                                                                                @���A  AfffA�  A�33A���B33B��B133BD��BY��BnffB�ffB���B�  B���B�33B�  B�  B�ffB�33B�ffB�33B�  B���C� CffCL�CL�C33C  C�C$33C)  C.� C3L�C7�fC=  CBL�CG  CQffC[33CeL�Co33Cx�fC���C�� C���C��3C�� C���C��3C��3C��fC��fC���C���C���C�s3Cǌ�C�ffC�s3C�� Cۙ�C�3C噚C��C� C���C���DٚD��DٚD�3D� D�fD�3D$� D)��D.� D3�3D8� D=�fDB��DG� DL�fDQ�fDV�fD[�3D`��De� Dj�fDo� Dt�3Dy� D�33D�ffD�� D���D�,�D�c3D��fD��D�0 D�` D���D�� D�)�D�l�Dڬ�D��fD��D�\�D�D�Vf1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @���A  AfffA�  A�33A���B33B��B133BD��BY��BnffB�ffB���B�  B���B�33B�  B�  B�ffB�33B�ffB�33B�  B���C� CffCL�CL�C33C  C�C$33C)  C.� C3L�C7�fC=  CBL�CG  CQffC[33CeL�Co33Cx�fC���C�� C���C��3C�� C���C��3C��3C��fC��fC���C���C���C�s3Cǌ�C�ffC�s3C�� Cۙ�C�3C噚C��C� C���C���DٚD��DٚD�3D� D�fD�3D$� D)��D.� D3�3D8� D=�fDB��DG� DL�fDQ�fDV�fD[�3D`��De� Dj�fDo� Dt�3Dy� D�33D�ffD�� D���D�,�D�c3D��fD��D�0 D�` D���D�� D�)�D�l�Dڬ�D��fD��D�\�D�D�Vf3333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�?}A���A�hsA�XA�=qA�33A�$�A�$�A�&�A�(�A�1'A�A�A�ZA��A���A��A�t�A�oA��jA�O�A�?}A�=qA���A�oA�ffA�ZA���A�`BA�ȴA��A��-A�ȴA�~�A��A���A���A���A�ZA��mA��/A��/A�JA��A�?}At~�Ap�RAe|�A_AS�-AL�AH=qAE�A@JA?S�A;�TA4�`A,�A)�7A'\)A$r�A!;dAz�A�wAS�A9XAn�AdZA��@�+@��@�h@�l�@��@��;@��F@�\)@���@��7@��+@�^5@�`B@�z�@��w@���@��@�%@���@�A�@��j@�~�@���@��^@�  @�~�@�1@u�T@n��@e�@\1@V5?@O��@H �@B-@;�F@6ff@1%@+dZ@&�R@ �`@V@|�@��@/@	�@
=1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�?}A���A�hsA�XA�=qA�33A�$�A�$�A�&�A�(�A�1'A�A�A�ZA��A���A��A�t�A�oA��jA�O�A�?}A�=qA���A�oA�ffA�ZA���A�`BA�ȴA��A��-A�ȴA�~�A��A���A���A���A�ZA��mA��/A��/A�JA��A�?}At~�Ap�RAe|�A_AS�-AL�AH=qAE�A@JA?S�A;�TA4�`A,�A)�7A'\)A$r�A!;dAz�A�wAS�A9XAn�AdZA��@�+@��@�h@�l�@��@��;@��F@�\)@���@��7@��+@�^5@�`B@�z�@��w@���@��@�%@���@�A�@��j@�~�@���@��^@�  @�~�@�1@u�T@n��@e�@\1@V5?@O��@H �@B-@;�F@6ff@1%@+dZ@&�R@ �`@V@|�@��@/@	�@
=3333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333 ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oBVB^5B�B�B��B��B��B��B�B�B�FB��B��BǮB��BǮB�
B��B�
B��B��B�
B�NB�yB�sB�ZB�B��BB�qB��B��B�DB{�Bl�BM�B�B�`BĜB�uB.B
�HB
�B
[#B	��B	��B	�LB	�+B	D�B	.B	!�B	oB		7B	\B��B��B�?B��B��B��B�VB}�Bx�Bp�BhsB`BBVBL�BI�BF�B=qB5?B=qBA�B`BBjBt�B�=B��B�dB�5B	\B	9XB	C�B	E�B	I�B	_;B	hsB	~�B	�oB	��B	�B	�jB	ƨB	��B	�B
  B
uB
 �B
'�B
0!B
9XB
@�B
E�B
M�B
R�B
YB
_;B
e`B
hsB
n�B
q�B
w�B
{�B
�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 BVB^5B�B�B��B��B��B��B�B�B�FB��B��B��BƨBɺB�B��B�B��B�B�B�ZB�B�yB�`B�B��BB�}B��B��B�JB|�Bn�BP�B!�B�fBǮB��B0!B
�TB
�B
^5B	��B	��B	�XB	�=B	F�B	/B	"�B	uB		7B	bB��B��B�FB��B��B��B�\B� By�Bq�BiyBaHBW
BM�BI�BG�B>wB6FB>wBB�BaHBjBt�B�=B��B�dB�5B	\B	9XB	D�B	E�B	I�B	_;B	hsB	~�B	�oB	��B	�B	�jB	ƨB	��B	�B
  B
uB
 �B
'�B
0!B
9XB
@�B
E�B
M�B
R�B
YB
_;B
e`B
hsB
n�B
q�B
w�B
{�B
�3333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333 <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ = PRES-SP(NextCycle), where SP is SURFACE PRESSURE from next cycle                                                                                                                                                                                     TEMP_ADJUSTED = TEMP                                                                                                                                                                                                                                            PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,PRES_ADJ,elapsed_time,alpha,tau),elapsed_time=P/mean_rise_rate,P=dbar since the start of the profile for each samples                                                                                                       None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            SP(NextCycle)=0.0(dbar)                                                                                                                                                                                                                                         None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE; PRES_ADJ_ERR : Manufacture sensor accuracy                                                                                                                                                                 TEMP_ADJ_ERR : SBE sensor accuracy                                                                                                                                                                                                                              Salinity Recalculation using PRES_ADJ; PSAL_ADJ_ERR : max(sum of RecalS & CTM errors, 0.01(PSS-78))                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            TNPD: APEX float that truncated negative pressure drift                                                                                                                                                                                                         None                                                                                                                                                                                                                                                            No adjustment is needed(maybe salty drift)                                                                                                                                                                                                                      200711180735172007111807351720071118073517200711180739412007111807394120071118073941201309120000002013091200000020130912000000  JA  ARFMdecpA5_a                                                                20071105065047  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.4                                                                 20071105065049  IP                  G�O�G�O�G�O�                JA  ARCArsal2.1                                                                 20071105065049  IP  PSAL            G�O�G�O�G�O�                JA  ARGQrqcppo_a                                                                20071105065050  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19b                                                                20071105065054  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19b                                                                20071105065054  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.7c                                                                20071105065054  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.7c                                                                20071105065054  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16b                                                                20071105065054  QCP$                G�O�G�O�G�O�           10000JA  ARUP                                                                        20071105070334                      G�O�G�O�G�O�                JA  ARFMdecpA5_a                                                                20071109034346  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.4                                                                 20071109034351  IP                  G�O�G�O�G�O�                JA  ARCArsal2.1                                                                 20071109034352  IP  PSAL            G�O�G�O�G�O�                JA  ARGQrqcppo_a                                                                20071109034352  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19b                                                                20071109034356  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19b                                                                20071109034356  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.7c                                                                20071109034356  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.7c                                                                20071109034356  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16b                                                                20071109034356  QCP$                G�O�G�O�G�O�           10000JA  ARGQrelo2.1                                                                 20071109034356  CV  TIME            G�O�G�O�F�/                JA  ARGQrelo2.1                                                                 20071109034356  CV  LAT$            G�O�G�O�A���                JA  ARGQrelo2.1                                                                 20071109034356  CV  LON$            G�O�G�O���s                JA  ARUP                                                                        20071109051001                      G�O�G�O�G�O�                JA  ARFMdecpA5_a                                                                20090401065104  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.5                                                                 20090401070125  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.0                                                                 20090401070125  IP  PRES            G�O�G�O�G�O�                JA  ARCArsal2.1a                                                                20090401070125  IP  PSAL            G�O�G�O�G�O�                JA  ARGQrqcppo_a                                                                20090401070126  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19b                                                                20090401070127  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19b                                                                20090401070127  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8b                                                                20090401070127  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8b                                                                20090401070127  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16b                                                                20090401070127  QCP$                G�O�G�O�G�O�           10000JA  ARGQrelo2.1                                                                 20090401070127  CV  TIME            G�O�G�O�F�/                JA  ARGQrelo2.1                                                                 20090401070127  CV  LAT$            G�O�G�O�A�                JA  ARGQrelo2.1                                                                 20090401070127  CV  LON$            G�O�G�O���s                JA  ARUP                                                                        20090401070504                      G�O�G�O�G�O�                JM  ARGQJMQC1.0                                                                 20071108172420  CV  DAT$            G�O�G�O�F�                JM  ARGQJMQC1.0                                                                 20071108172420  CV  LAT$            G�O�G�O�A���                JM  ARSQJMQC1.0                                                                 20071108172420  CF  PRES            @���D�VfG�O�                JM  ARSQJMQC1.0                                                                 20071108172420  CF  TEMP            @���D�VfG�O�                JM  ARSQJMQC1.0                                                                 20071108172420  CF  PSAL            @���D�VfG�O�                JM  ARCAJMQC1.0                                                                 20071118073517  IP  PRES            G�O�G�O�G�O�                JM  ARCAJMQC1.0                                                                 20071118073517  IP  PSAL            G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20071118073941  CV  PSAL            G�O�G�O�G�O�                JM  ARSQOW  1.1 CTD2013V01                                                      20130912000000  IP  PSAL            G�O�G�O�G�O�                JA  RFMTcnvd2.1                                                                 20130924052338  IP                  G�O�G�O�G�O�                JA  ARDU                                                                        20130924052546                      G�O�G�O�G�O�                JM      JMFC1.0                                                                 20150609192516                      G�O�G�O�G�O�                JA  ARDU                                                                        20150614052512                      G�O�G�O�G�O�                