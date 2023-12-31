CDF   '   
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PARAM       N_PROF        N_CALIB       N_LEVELS   o   	N_HISTORY             	   title         Argo float vertical profile    institution       JAMSTEC    source        
Argo float     history       b2005-11-28T06:48:03Z creation;2011-06-06T00:20:06Z update;2015-06-09T09:32:18Z conversion to V3.1;     
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
_FillValue                  p  ;\   PRES_ADJUSTED         	         	   	long_name         )Sea water pressure, equals 0 at sea-level      
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   standard_name         sea_water_pressure       �  ;�   PRES_ADJUSTED_QC      	            	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  p  =�   PRES_ADJUSTED_ERROR       	            	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  =�   TEMP      	         	   	long_name         $Sea temperature in-situ ITS-90 scale   
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o   standard_name         sea_water_temperature        �  ?�   TEMP_QC       	            	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  p  Ap   TEMP_ADJUSTED         	         	   	long_name         $Sea temperature in-situ ITS-90 scale   
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o   standard_name         sea_water_temperature        �  A�   TEMP_ADJUSTED_QC      	            	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  p  C�   TEMP_ADJUSTED_ERROR       	            	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  D   PSAL      	         	   	long_name         Practical salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o   standard_name         sea_water_salinity       �  E�   PSAL_QC       	            	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  p  G�   PSAL_ADJUSTED         	         	   	long_name         Practical salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o   standard_name         sea_water_salinity       �  G�   PSAL_ADJUSTED_QC      	            	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  p  I�   PSAL_ADJUSTED_ERROR       	            	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  J    	PARAMETER         	   
               	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  K�   SCIENTIFIC_CALIB_EQUATION         	   
               	long_name         'Calibration equation for this parameter    
_FillValue                 	   Ll   SCIENTIFIC_CALIB_COEFFICIENT      	   
               	long_name         *Calibration coefficients for this equation     
_FillValue                 	   Ul   SCIENTIFIC_CALIB_COMMENT      	   
               	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   ^l   SCIENTIFIC_CALIB_DATE         	   
                	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  gl   HISTORY_INSTITUTION          	            	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    g�   HISTORY_STEP         	            	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    g�   HISTORY_SOFTWARE         	            	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    g�   HISTORY_SOFTWARE_RELEASE         	            	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    g�   HISTORY_REFERENCE            	            	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  g�   HISTORY_DATE         	             	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    h<   HISTORY_ACTION           	            	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    hL   HISTORY_PARAMETER            	            	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    hP   HISTORY_START_PRES           	         	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         h`   HISTORY_STOP_PRES            	         	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         hd   HISTORY_PREVIOUS_VALUE           	         	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        hh   HISTORY_QCTEST           	            	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    hlArgo profile    3.1 1.2 19500101000000  5900492 J-ARGO                                                          JAMSTEC                                                         PRES            TEMP            PSAL               BA   JA  20051128064803  20150621114513  A5_21055_066                    2C  D   APEX                            1091                            061703                          846 @�����1   @����-��@+6ȴ9X�d���"��1   ARGOS   F   F   F   Primary sampling: discrete [spot sampling CTD data]                                                                                                                                                                                                                @�ffAffAd��A�ffA���A�  B��B  B133BD��BY��Bn��B���B�33B�  B���B�  B�ffB�33B�33B�  B�  B�ffB�ffB�33C��CL�CL�C�CL�C�C��C$�3C)33C.� C3L�C8L�C=ffCB� CG�CQ� C[��Ce� Co33CyffC�� C�� C�� C��fC���C�� C���C�� C���C�� C���C�s3C�s3C�s3Cǳ3C���C�� C�� Cی�C�� C��C� C� C��C���DٚDٚD� D��D��D�D"` D(��D.� D53D;S3DA�3DG�fDN3DTL�DZ��D`�fDg  DmS3Ds� Dy��D�)�D�s3D�� D��D�&fD�Y�D���D�� D�  D�i�D��fD�� D�,�D�c3Dڬ�D��D�#3D�` D�fD��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @�ffAffAd��A�ffA���A�  B��B  B133BD��BY��Bn��B���B�33B�  B���B�  B�ffB�33B�33B�  B�  B�ffB�ffB�33C��CL�CL�C�CL�C�C��C$�3C)33C.� C3L�C8L�C=ffCB� CG�CQ� C[��Ce� Co33CyffC�� C�� C�� C��fC���C�� C���C�� C���C�� C���C�s3C�s3C�s3Cǳ3C���C�� C�� Cی�C�� C��C� C� C��C���DٚDٚD� D��D��D�D"` D(��D.� D53D;S3DA�3DG�fDN3DTL�DZ��D`�fDg  DmS3Ds� Dy��D�)�D�s3D�� D��D�&fD�Y�D���D�� D�  D�i�D��fD�� D�,�D�c3Dڬ�D��D�#3D�` D�fD��333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��AٍPAٍPAٍPAٕ�AٓuAٗ�AٓuAّhAّhA�~�A�n�A�XA�7LAѣ�A�+A��Aȴ9A��TA�M�A�x�A�+A��hA�M�A���A�r�A��A���A�;dA�jA��A�33A��PA���A��;A|�Aw�mAq�mAh�A`��A]33AS��ANI�AE��A:�A4bA0z�A,r�A(=qA$�AE�A33A��A�A�AdZA��A��A�RAl�A��A
�yA	��A1'A��AQ�A�A��A"�AI�A =q@��R@�+@�C�@�h@��;@ّh@ղ-@�1'@ʟ�@þw@�9X@���@���@�@��\@��@���@�Z@�"�@�r�@�7L@��@��@�~�@�{@xQ�@m�T@g
=@^�+@U�@K"�@E/@@�u@8  @3��@/;d@(Ĝ@$�D@"^5@/@�P111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 AٍPAٍPAٍPAٕ�AٓuAٗ�AٓuAّhAّhA�~�A�n�A�XA�7LAѣ�A�+A��Aȴ9A��TA�M�A�x�A�+A��hA�M�A���A�r�A��A���A�;dA�jA��A�33A��PA���A��;A|�Aw�mAq�mAh�A`��A]33AS��ANI�AE��A:�A4bA0z�A,r�A(=qA$�AE�A33A��A�A�AdZA��A��A�RAl�A��A
�yA	��A1'A��AQ�A�A��A"�AI�A =q@��R@�+@�C�@�h@��;@ّh@ղ-@�1'@ʟ�@þw@�9X@���@���@�@��\@��@���@�Z@�"�@�r�@�7L@��@��@�~�@�{@xQ�@m�T@g
=@^�+@U�@K"�@E/@@�u@8  @3��@/;d@(Ĝ@$�D@"^5@/@�P333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333 ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB�TB�TB�`B�ZB�`B�ZB�`B�fB�fB�yB�B��B	\B
$�B
�B
C�B
p�B)�B��B�)BDB(�B@�B>wB<jB9XB �B��B��BD�B
�B
ɺB
-B
B	ȴB	��B	�+B	cTB	D�B	7LB	'�B	$�B	S�B	�B	��B	��B	��B	�B	�3B	��B	�B	��B
VB
�B
<jB
D�B
K�B
O�B
P�B
P�B
P�B
P�B
O�B
N�B
L�B
L�B
I�B
G�B
E�B
@�B
9XB
2-B
)�B
%�B
 �B
�B
�B
�B
�B
uB
�B
�B
�B
�B
�B
�B
"�B
�B
#�B
%�B
&�B
)�B
-B
2-B
<jB
F�B
J�B
M�B
Q�B
ZB
_;B
aHB
cTB
e`B
gmB
iyB
n�B
q�B
s�B
x�B
~�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B�TB�TB�`B�ZB�`B�ZB�`B�fB�fB�yB�B��B	�B
'�B
�B
D�B
r�B)�B��B�5BVB-BB�BB�B>wB<jB%�B�B��BI�B
�B
��B
1'B
+B	��B	��B	�DB	gmB	F�B	9XB	(�B	&�B	W
B	�B	��B	��B	��B	�B	�?B	B	�B	��B
VB
�B
<jB
E�B
K�B
O�B
P�B
P�B
P�B
P�B
O�B
N�B
L�B
L�B
I�B
G�B
F�B
@�B
:^B
33B
)�B
%�B
 �B
�B
�B
�B
�B
uB
�B
�B
�B
�B
�B
�B
"�B
�B
#�B
%�B
&�B
)�B
-B
2-B
<jB
F�B
J�B
M�B
Q�B
ZB
_;B
aHB
cTB
e`B
gmB
iyB
n�B
q�B
s�B
x�B
~�333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333 <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<49X<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ = PRES-SP(NextCycle), where SP is SURFACE PRESSURE from next cycle                                                                                                                                                                                     TEMP_ADJUSTED = TEMP                                                                                                                                                                                                                                            PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,PRES_ADJ,elapsed_time,alpha,tau),elapsed_time=P/mean_rise_rate,P=dbar since the start of the profile for each samples                                                                                                       None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            SP(NextCycle)=0.0(dbar)                                                                                                                                                                                                                                         None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE; PRES_ADJ_ERR : Manufacture sensor accuracy                                                                                                                                                                 TEMP_ADJ_ERR : SBE sensor accuracy                                                                                                                                                                                                                              Salinity Recalculation using PRES_ADJ; PSAL_ADJ_ERR : max(sum of RecalS & CTM errors, 0.01(PSS-78))                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            TNPD: APEX float that truncated negative pressure drift                                                                                                                                                                                                         None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         200512111657062005121116570620051211165706200701262005312007012620053120070126200531201106020000002011060200000020110602000000  JA  ARFMfmtp2.2                                                                 20051128064803  IP                  G�O�G�O�G�O�                JA  ARGQrqcp2.3                                                                 20051128064804  QCP$                G�O�G�O�G�O�           1FB7CJA  ARUP                                                                        20051128070320                      G�O�G�O�G�O�                JA  ARFMfmtp2.2                                                                 20051202005249  IP                  G�O�G�O�G�O�                JA  ARGQrqcp2.3                                                                 20051202005250  QCP$                G�O�G�O�G�O�           1FB7CJA  ARUP                                                                        20051202012157                      G�O�G�O�G�O�                JA  ARFMdecpA5_a                                                                20081215014553  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.5                                                                 20081215014657  IP                  G�O�G�O�G�O�                JA  ARCArsal2.1a                                                                20081215014657  IP  PSAL            G�O�G�O�G�O�                JA  ARGQrqcppo_a                                                                20081215014657  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19b                                                                20081215014700  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19b                                                                20081215014700  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8a                                                                20081215014700  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8a                                                                20081215014700  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16b                                                                20081215014700  QCP$                G�O�G�O�G�O�           10000JA  ARUP                                                                        20081215015531                      G�O�G�O�G�O�                JA  ARFMdecpA5_a                                                                20090401083731  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.5                                                                 20090401085329  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.0                                                                 20090401085330  IP  PRES            G�O�G�O�G�O�                JA  ARCArsal2.1a                                                                20090401085330  IP  PSAL            G�O�G�O�G�O�                JA  ARGQrqcppo_a                                                                20090401085330  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19b                                                                20090401085331  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19b                                                                20090401085331  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8b                                                                20090401085331  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8b                                                                20090401085331  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16b                                                                20090401085331  QCP$                G�O�G�O�G�O�           10000JA  ARUP                                                                        20090401090118                      G�O�G�O�G�O�                JM  ARGQJMQC1.0                                                                 20051201163807  CV  DAT$            G�O�G�O�F���                JM  ARSQJMQC1.0                                                                 20051201163807  CF  PRES            @�ffD��G�O�                JM  ARSQJMQC1.0                                                                 20051201163807  CF  TEMP            @�ffD��G�O�                JM  ARSQJMQC1.0                                                                 20051201163807  CF  PSAL            @�ffD��G�O�                JM  ARCAJMQC1.0                                                                 20051211165706  IP  PRES            G�O�G�O�G�O�                JM  ARCAJMQC1.0                                                                 20051211165706  IP  PSAL            G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20070126200531  CV  PSAL            G�O�G�O�G�O�                JM  ARSQOW  1.1 CTD2010V1                                                       20110602000000  IP  PSAL            G�O�G�O�G�O�                JA  RFMTcnvd2.1                                                                 20110606001740  IP                  G�O�G�O�G�O�                JA  ARDU                                                                        20110606002006                      G�O�G�O�G�O�                JM      JMFC1.0                                                                 20150609093209                      G�O�G�O�G�O�                JA  ARDU                                                                        20150621114513                      G�O�G�O�G�O�                