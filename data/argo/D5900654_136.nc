CDF   0   
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PARAM       N_PROF        N_CALIB       N_LEVELS   s   	N_HISTORY             	   title         Argo float vertical profile    institution       JAMSTEC    source        
Argo float     history       b2008-06-05T08:49:49Z creation;2009-09-01T08:43:26Z update;2015-06-09T21:23:24Z conversion to V3.1;     
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
_FillValue                    iArgo profile    3.1 1.2 19500101000000  5900654 J-ARGO                                                          JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA  20080605084949  20150621190522  A5_23712_136                    2C  D   APEX                            1566                            013004                          846 @��i���1   @��L�Xe@2��1'�c���O�;1   ARGOS   A   A   A   Primary sampling: discrete [spot sampling CTD data]                                                                                                                                                                                                                @���A��Aa��A���A�  A�  BffB33B0ffBE��BXffBm��B���B�  B�33B���B���B�33B�  B�33B�33B�33B�33BB�  C� CffC��C� CL�CL�C33C$� C)L�C.L�C2�fC8�C=�CB  CG  CQ33C[� Ce� CoffCyL�C�s3C���C��3C��fC�� C�� C��fC�� C���C�� C���C�ffC��fC�ffC���C̦fC�� C֌�Cی�C�3C�3CꙚCC��fC��fD��D� DٚD� DٚD��D� D$�3D)��D.�fD3��D8��D=�fDBٚDG��DL�fDQ�fDV��D[�fD`� De�3Dj�3Do� Dt��Dy��D�#3D�p D�� D��3D�0 D�` D���D��3D�)�D�ffD���D��fD�)�D�p Dڣ3D��D�  D�ffD�D�  1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @y��AffAS33A���A���A���B��B��B,��BB  BT��Bj  B~  B�33B�ffB�  B�  B�ffB�33B�ffB�ffB�ffB�ffB���B�33C ��C� C
�3C��CffCffCL�C#��C(ffC-ffC2  C733C<33CA�CF�CPL�CZ��Cd��Cn� CxffC�  C�&fC�@ C�33C�L�C�L�C�33C�L�C�&fC��C�&fC��3C�33C��3C�Y�C�33C�L�C��C��C�@ C�@ C�&fC�&fC�33C�33D�3D�fD� D�fD� D�3D�fD$��D)�3D.��D3�3D8� D=��DB� DG� DL��DQ��DV� D[��D`�fDe��Dj��Do�fDt� Dy�3D�fD�S3D��3D��fD�3D�C3D���D��fD��D�I�D�� D�ɚD��D�S3DچfD���D�3D�I�D�|�D�31111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A��#A��;A��`A��/A��mAȲ-AȓuAȡ�AȑhA�ZA�9XA�/A�"�A�z�A�v�A���A�$�A�oA���A��DA�ȴA�ȴA�x�A�C�A�33A�t�A���A�E�A���A��A��jA�p�A�O�A�K�A��HA��!A��9A�O�A�A�JA��`A�Ax�\Ah�AW��APA�AJ��AD�A<�DA01A';dA"�RA��A��A��AƨA7LA
bNA-A��A�jA ��@�33@���@��@@�+@���@�p�@�S�@ى7@�?}@Ѓ@ʸR@�C�@ǝ�@őh@��@�J@��9@��`@�hs@�n�@��@��`@��#@�o@���@�Z@�$�@�r�@���@�\)@��@��w@�C�@��R@x��@q�^@h��@_|�@Up�@K��@D(�@;dZ@5?}@.V@)x�@!&�@�h@@�`@/@J@/1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A��#A��;A��`A��/A��mAȲ-AȓuAȡ�AȑhA�ZA�9XA�/A�"�A�z�A�v�A���A�$�A�oA���A��DA�ȴA�ȴA�x�A�C�A�33A�t�A���A�E�A���A��A��jA�p�A�O�A�K�A��HA��!A��9A�O�A�A�JA��`A�Ax�\Ah�AW��APA�AJ��AD�A<�DA01A';dA"�RA��A��A��AƨA7LA
bNA-A��A�jA ��@�33@���@��@@�+@���@�p�@�S�@ى7@�?}@Ѓ@ʸR@�C�@ǝ�@őh@��@�J@��9@��`@�hs@�n�@��@��`@��#@�o@���@�Z@�$�@�r�@���@�\)@��@��w@�C�@��R@x��@q�^@h��@_|�@Up�@K��@D(�@;dZ@5?}@.V@)x�@!&�@�h@@�`@/@J@/1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oBoBuBuBuB{BVBPBVBJB	7B+B+BDBJ�B�-B+BM�BZBVB?}BgmBx�B�B� B�7BcTBL�B=qB(�B�BPB��B�5BBp�B\)B8RB
��B
��B
�DB
=qB
�B	�fB	r�B	PB�ZBȴBĜB�B��B�{B�DB�PB��B��B�LB��BĜBƨB��B��B�#B��B��B��B	B��B��B��B��B	1B	�B	'�B	I�B	x�B	�PB	��B	��B	��B	�3B	�XB	�qB	�jB	ĜB	��B	��B	�HB	�yB	�B	�B	��B	��B
B
B
1B
hB
�B
"�B
'�B
.B
6FB
=qB
D�B
J�B
R�B
YB
_;B
cTB
jB
m�B
p�B
r�B
v�B
y�B
}�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 BoBuBuBuB{BVBPBVBJB	7B+B+BPBL�B�3B1BN�B[#BXBC�BhsBy�B�B�B�JBgmBN�B?}B+B�B\B��B�BBȴBq�B^5B<jB
��B
�B
�VB
>wB
 �B	�B	v�B	\B�`B��BƨB�B��B��B�PB�VB��B��B�RBBƨBȴB��B��B�)B��B��B��B	B��B��B��B��B	1B	�B	(�B	I�B	x�B	�VB	��B	��B	��B	�3B	�^B	�qB	�jB	ŢB	��B	��B	�HB	�yB	�B	�B	��B	��B
B
B
	7B
hB
�B
#�B
'�B
.B
6FB
=qB
E�B
J�B
S�B
ZB
_;B
cTB
jB
m�B
p�B
r�B
v�B
z�B
~�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ = PRES-SP(NextCycle), where SP is SURFACE PRESSURE from next cycle                                                                                                                                                                                     TEMP_ADJUSTED = TEMP                                                                                                                                                                                                                                            PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,PRES_ADJ,elapsed_time,alpha,tau),elapsed_time=P/mean_rise_rate,P=dbar since the start of the profile for each samples                                                                                                       None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            SP(NextCycle)=0.9(dbar)                                                                                                                                                                                                                                         None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE; PRES_ADJ_ERR : Manufacture sensor accuracy                                                                                                                                                                 TEMP_ADJ_ERR : SBE sensor accuracy                                                                                                                                                                                                                              Salinity Recalculation using PRES_ADJ; PSAL_ADJ_ERR : max(sum of RecalS & CTM errors, 0.01(PSS-78))                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         200806181443422008061814434220080618144342200806181501382008061815013820080618150138200908260000002009082600000020090826000000  JA  ARFMdecpA5_a                                                                20080605084947  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.4                                                                 20080605084949  IP                  G�O�G�O�G�O�                JA  ARCArsal2.1                                                                 20080605084950  IP  PSAL            G�O�G�O�G�O�                JA  ARGQrqcppo_a                                                                20080605084950  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19b                                                                20080605084954  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19b                                                                20080605084954  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8a                                                                20080605084955  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8a                                                                20080605084955  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16b                                                                20080605084955  QCP$                G�O�G�O�G�O�           10000JA  ARUP                                                                        20080605090354                      G�O�G�O�G�O�                JA  ARFMdecpA5_a                                                                20080609035048  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.4                                                                 20080609035053  IP                  G�O�G�O�G�O�                JA  ARCArsal2.1                                                                 20080609035054  IP  PSAL            G�O�G�O�G�O�                JA  ARGQrqcppo_a                                                                20080609035054  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19b                                                                20080609035058  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19b                                                                20080609035058  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8a                                                                20080609035058  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8a                                                                20080609035058  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16b                                                                20080609035059  QCP$                G�O�G�O�G�O�           10000JA  ARUP                                                                        20080609052524                      G�O�G�O�G�O�                JA  ARCArsal2.1                                                                 20080912002115  IP  PSAL            G�O�G�O�G�O�                JA  ARGQrqcppo_a                                                                20080912002115  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19b                                                                20080912002119  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19b                                                                20080912002119  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8a                                                                20080912002119  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8a                                                                20080912002119  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16b                                                                20080912002120  QCP$                G�O�G�O�G�O�           10000JA  ARUP                                                                        20080912015810                      G�O�G�O�G�O�                JA  ARFMdecpA5_a                                                                20080609035048  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.5                                                                 20090414042845  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.0                                                                 20090414042845  IP  PRES            G�O�G�O�G�O�                JA  ARCArsal2.1a                                                                20090414042846  IP  PSAL            G�O�G�O�G�O�                JA  ARGQrqcppo_a                                                                20090414042846  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19b                                                                20090414042847  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19b                                                                20090414042847  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8b                                                                20090414042847  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8b                                                                20090414042847  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16b                                                                20090414042847  QCP$                G�O�G�O�G�O�           10000JA  ARUP                                                                        20090414042946                      G�O�G�O�G�O�                JM  ARGQJMQC1.0                                                                 20080608224448  CV  DAT$            G�O�G�O�F��3                JM  ARCAJMQC1.0                                                                 20080618144342  CV  PRES            G�O�G�O�G�O�                JM  ARCAJMQC1.0                                                                 20080618144342  CV  PSAL            G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20080618150138  CV  PSAL            G�O�G�O�G�O�                JM  ARSQOW  1.1 SeHyD1.0                                                        20090826000000  IP  PSAL            G�O�G�O�G�O�                JA  RFMTcnvd2.1                                                                 20090901084249  IP                  G�O�G�O�G�O�                JA  ARDU                                                                        20090901084326                      G�O�G�O�G�O�                JM      JMFC1.0                                                                 20150609212319                      G�O�G�O�G�O�                JA  ARDU                                                                        20150621190522                      G�O�G�O�G�O�                