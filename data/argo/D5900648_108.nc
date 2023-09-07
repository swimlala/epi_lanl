CDF   -   
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PARAM       N_PROF        N_CALIB       N_LEVELS   s   	N_HISTORY             	   title         Argo float vertical profile    institution       JAMSTEC    source        
Argo float     history       b2007-08-27T08:50:23Z creation;2013-09-24T05:26:23Z update;2015-06-09T19:24:04Z conversion to V3.1;     
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
_FillValue                    iArgo profile    3.1 1.2 19500101000000  5900648 J-ARGO                                                          JAMSTEC                                                         PRES            TEMP            PSAL               lA   JA  20070827085023  20150614052518  A5_28347_108                    2C  D   APEX                            1316                            013004                          846 @Ԑ@So�t1   @ԐAxj1D@6����o�c�x���1   ARGOS   F   F   F   Primary sampling: discrete [spot sampling CTD data]                                                                                                                                                                                                                @�ffAffA`  A�ffA���A陚B	33B33B0ffBF��BZ  Bm33B�  B���B���B���B���B���B���B�33B���B�ffB䙚B�  B�  C�C��CL�C� CL�CffCL�C$ffC)�C.�C333C7��C=�CBL�CG�CP�fC[� CeffCo� Cy�C��3C���C�� C�ffC��fC��3C���C���C��3C�� C��fC���C���C�ffCǳ3C̙�CѦfC֦fCۦfC�fC�fC�3CC���C�� D�fD� D�3DٚD�3D� D�3D$��D)��D.��D3�fD8� D=� DB�fDG��DL� DQ�fDV� D[� D`�fDe��Dj�fDo��Dt�3Dy� D�)�D�ffD��3D���D�0 D�p D�� D��D�  D�\�D��3D�ٚD�&fD�l�Dڣ3D��D�fD�S3D�fD�i�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @�ffAffA`  A�ffA���A陚B	33B33B0ffBF��BZ  Bm33B�  B���B���B���B���B���B���B�33B���B�ffB䙚B�  B�  C�C��CL�C� CL�CffCL�C$ffC)�C.�C333C7��C=�CBL�CG�CP�fC[� CeffCo� Cy�C��3C���C�� C�ffC��fC��3C���C���C��3C�� C��fC���C���C�ffCǳ3C̙�CѦfC֦fCۦfC�fC�fC�3CC���C�� D�fD� D�3DٚD�3D� D�3D$��D)��D.��D3�fD8� D=� DB�fDG��DL� DQ�fDV� D[� D`�fDe��Dj�fDo��Dt�3Dy� D�)�D�ffD��3D���D�0 D�p D�� D��D�  D�\�D��3D�ٚD�&fD�l�Dڣ3D��D�fD�S3D�fD�i�3333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��AԾwAӶFAә�A�p�A�"�A�`BA�$�A��A� �A�/Aщ7A�7LA̼jA�r�A�^5A�^5A�Q�A��TA�C�A��DA�;dA�M�A���A�VA��RA��A���A�A��/A��A���A�?}A�x�A���A��A�?}A�`BA��A���A�=qA��\A��-A�x�Ax�\AnA�Af�Ac;dAZ�+AVJAS�^AL�+ABz�A=�;A7��A333A/VA+t�A(JA$=qA!x�A�+A�DA��AffA�-A
��AA33@�%@�bN@�7L@��@��@Ο�@��m@�z�@�V@��@�9X@�+@��w@�`B@�M�@���@��@���@� �@�@��@�\)@��#@�r�@�E�@��@|1@o�P@f��@`�@\��@SdZ@Mp�@G;d@B=q@;t�@4�@-�@%`B@!x�@�@��@/@%@?}@
=q@ȴ1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 AԾwAӶFAә�A�p�A�"�A�`BA�$�A��A� �A�/Aщ7A�7LA̼jA�r�A�^5A�^5A�Q�A��TA�C�A��DA�;dA�M�A���A�VA��RA��A���A�A��/A��A���A�?}A�x�A���A��A�?}A�`BA��A���A�=qA��\A��-A�x�Ax�\AnA�Af�Ac;dAZ�+AVJAS�^AL�+ABz�A=�;A7��A333A/VA+t�A(JA$=qA!x�A�+A�DA��AffA�-A
��AA33@�%@�bN@�7L@��@��@Ο�@��m@�z�@�V@��@�9X@�+@��w@�`B@�M�@���@��@���@� �@�@��@�\)@��#@�r�@�E�@��@|1@o�P@f��@`�@\��@SdZ@Mp�@G;d@B=q@;t�@4�@-�@%`B@!x�@�@��@/@%@?}@
=q@ȴ3333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333 ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB.B&�B(�B%�B�B��B��BbB�BaHB��B�B%�B�BoB�B��B��B�B�sB�HB�)B��B��B��B��B��BhsBR�B<jB�B��B�B�RB�!B�B�+Bl�BQ�B%�B
�fB
�}B
�uB
J�B
1B	�ZB	ǮB	�uB	�=B	}�B	VB	JB��B�BB��BĜB�^B�'B��B��B�bB�7B~�Br�Bo�BcTBW
BI�BA�B=qB]/Be`B_;Bt�B�B�%B��BȴB�B�B	JB	+B	A�B	[#B	v�B	�=B	��B	�B	B	��B	�B	�5B	�NB	�yB	��B
B
�B
$�B
-B
7LB
=qB
B�B
F�B
N�B
T�B
\)B
e`B
jB
n�B
s�B
w�B
{�B
� B
�B
�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B/B&�B(�B%�B�B��B��BbB�BbNB��B�B'�B �B�B��B��B��B�B�B�ZB�/B�B��BBÖB��BjBS�B>wB�BB�)B�^B�-B�B�7Bm�BT�B'�B
�mB
��B
��B
M�B

=B	�`B	ɺB	�{B	�DB	� B	YB	PB��B�HB��BŢB�dB�-B��B��B�hB�=B�Br�Bp�BdZBYBJ�BB�B=qB^5BffB`BBu�B�B�+B��BɺB�B�B	JB	+B	A�B	[#B	v�B	�=B	��B	�B	B	��B	�B	�5B	�NB	�yB	��B
B
�B
$�B
-B
7LB
=qB
B�B
F�B
N�B
T�B
\)B
e`B
jB
n�B
s�B
w�B
{�B
� B
�B
�13333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333 <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ = PRES-SP(NextCycle), where SP is SURFACE PRESSURE from next cycle                                                                                                                                                                                     TEMP_ADJUSTED = TEMP                                                                                                                                                                                                                                            PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,PRES_ADJ,elapsed_time,alpha,tau),elapsed_time=P/mean_rise_rate,P=dbar since the start of the profile for each samples                                                                                                       None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            SP(NextCycle)=0.0(dbar)                                                                                                                                                                                                                                         None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE; PRES_ADJ_ERR : Manufacture sensor accuracy                                                                                                                                                                 TEMP_ADJ_ERR : SBE sensor accuracy                                                                                                                                                                                                                              Salinity Recalculation using PRES_ADJ; PSAL_ADJ_ERR : max(sum of RecalS & CTM errors, 0.01(PSS-78))                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            TNPD: APEX float that truncated negative pressure drift                                                                                                                                                                                                         None                                                                                                                                                                                                                                                            No adjustment is needed(maybe salty drift)                                                                                                                                                                                                                      200709090820192007090908201920070909082019200709090830262007090908302620070909083026201309120000002013091200000020130912000000  JA  ARFMdecpA5_a                                                                20070827085019  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.4                                                                 20070827085023  IP                  G�O�G�O�G�O�                JA  ARCArsal2.1                                                                 20070827085023  IP  PSAL            G�O�G�O�G�O�                JA  ARGQrqcppo_a                                                                20070827085024  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19b                                                                20070827085028  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19b                                                                20070827085028  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.7c                                                                20070827085028  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.7c                                                                20070827085028  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16a                                                                20070827085028  QCP$                G�O�G�O�G�O�           10000JA  ARGQaqcpt16a                                                                20070827085028  QCP$                G�O�G�O�G�O�           10000JA  ARUP                                                                        20070827090234                      G�O�G�O�G�O�                JA  ARFMdecpA5_a                                                                20070831034342  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.4                                                                 20070831034347  IP                  G�O�G�O�G�O�                JA  ARCArsal2.1                                                                 20070831034347  IP  PSAL            G�O�G�O�G�O�                JA  ARGQrqcppo_a                                                                20070831034348  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19b                                                                20070831034352  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19b                                                                20070831034352  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.7c                                                                20070831034352  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.7c                                                                20070831034352  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16a                                                                20070831034352  QCP$                G�O�G�O�G�O�           10000JA  ARGQaqcpt16a                                                                20070831034352  QCP$                G�O�G�O�G�O�           10000JA  ARUP                                                                        20070831042247                      G�O�G�O�G�O�                JA  ARFMdecpA5_a                                                                20090401065055  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.5                                                                 20090401070108  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.0                                                                 20090401070108  IP  PRES            G�O�G�O�G�O�                JA  ARCArsal2.1a                                                                20090401070108  IP  PSAL            G�O�G�O�G�O�                JA  ARGQrqcppo_a                                                                20090401070108  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19b                                                                20090401070110  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19b                                                                20090401070110  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8b                                                                20090401070110  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8b                                                                20090401070110  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16b                                                                20090401070110  QCP$                G�O�G�O�G�O�           10000JA  ARUP                                                                        20090401070447                      G�O�G�O�G�O�                JM  ARGQJMQC1.0                                                                 20070830170812  CV  DAT$            G�O�G�O�F��
                JM  ARSQJMQC1.0                                                                 20070830170812  CF  PRES            @�ffD�i�G�O�                JM  ARSQJMQC1.0                                                                 20070830170812  CF  TEMP            @�ffD�i�G�O�                JM  ARSQJMQC1.0                                                                 20070830170812  CF  PSAL            @�ffD�i�G�O�                JM  ARCAJMQC1.0                                                                 20070909082019  IP  PRES            G�O�G�O�G�O�                JM  ARCAJMQC1.0                                                                 20070909082019  IP  PSAL            G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20070909083026  CV  PSAL            G�O�G�O�G�O�                JM  ARSQOW  1.1 CTD2013V01                                                      20130912000000  IP  PSAL            G�O�G�O�G�O�                JA  RFMTcnvd2.1                                                                 20130924052454  IP                  G�O�G�O�G�O�                JA  ARDU                                                                        20130924052623                      G�O�G�O�G�O�                JM      JMFC1.0                                                                 20150609192358                      G�O�G�O�G�O�                JA  ARDU                                                                        20150614052518                      G�O�G�O�G�O�                