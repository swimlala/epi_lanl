CDF   4   
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PARAM       N_PROF        N_CALIB       N_LEVELS   s   	N_HISTORY             	   title         Argo float vertical profile    institution       JAMSTEC    source        
Argo float     history       b2008-01-04T06:50:38Z creation;2013-09-24T05:26:14Z update;2015-06-09T19:26:35Z conversion to V3.1;     
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
_FillValue                    iArgo profile    3.1 1.2 19500101000000  5900648 J-ARGO                                                          JAMSTEC                                                         PRES            TEMP            PSAL               yA   JA  20080104065038  20150614052516  A5_28347_121                    2C  D   APEX                            1316                            013004                          846 @԰�c�91   @԰���@7T�j~���c�E����1   ARGOS   F   F   F   Primary sampling: discrete [spot sampling CTD data]                                                                                                                                                                                                                @�33A33Aa��A�33A�  A�33B	��B33B133BC33BX  Bm��B33B�  B�33B�33B���B�ffB���B���B���B�ffB���B�  B�ffCffCffCffC� C33C  C�C$� C)ffC.� C3ffC8ffC=L�CBL�CGffCQ33C[ffCeL�CoffCy�C�s3C��3C�� C�� C���C�� C�� C���C���C�s3C��fC��fC��3C�Cǌ�C̦fCѦfC�� C���C�� C�3CꙚC�fC��3C�� D�3D��D�fDٚD� D�fD�3D$� D)� D.ٚD3�3D8�3D=�fDB� DG�fDL� DQٚDVٚD[� D`�3De� Dj�3Do�3Dt�3Dy��D�)�D�p D���D���D�)�D�p D���D���D�#3D�l�D��3D���D��D�l�Dڠ D�� D��D�` D�D��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @�33A33Aa��A�33A�  A�33B	��B33B133BC33BX  Bm��B33B�  B�33B�33B���B�ffB���B���B���B�ffB���B�  B�ffCffCffCffC� C33C  C�C$� C)ffC.� C3ffC8ffC=L�CBL�CGffCQ33C[ffCeL�CoffCy�C�s3C��3C�� C�� C���C�� C�� C���C���C�s3C��fC��fC��3C�Cǌ�C̦fCѦfC�� C���C�� C�3CꙚC�fC��3C�� D�3D��D�fDٚD� D�fD�3D$� D)� D.ٚD3�3D8�3D=�fDB� DG�fDL� DQٚDVٚD[� D`�3De� Dj�3Do�3Dt�3Dy��D�)�D�p D���D���D�)�D�p D���D���D�#3D�l�D��3D���D��D�l�Dڠ D�� D��D�` D�D��3333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�~�A�?}A��A�bA�A�%A�1A�JA�  A�%A���A�VA�bA��A��A� �A�$�A�(�A�1'A�/A�-A�(�A�-A�(�A���A��A���A�VA��jA��FA�jA�z�A���A�
=A�v�A�oA�jA�A�"�A� �A���A��DA�`BAx1Ar  Aj-Ae
=A^9XAXbAQO�AL�AE��AAp�A;�A7`BA2{A-�hA* �A&�yA v�A�\AC�A�
A��A/A
VA/Ahs@�C�@���@�7L@��@�/@Ǖ�@��^@�&�@���@��m@�z�@�`B@���@���@��R@�X@�E�@�M�@� �@�$�@��`@��@�@�ƨ@�@�@}�@u?}@k��@b��@Y��@R=q@Lz�@Gl�@?�@:-@4I�@.�+@*J@$�j@ �@M�@��@��@V@	G�@�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�~�A�?}A��A�bA�A�%A�1A�JA�  A�%A���A�VA�bA��A��A� �A�$�A�(�A�1'A�/A�-A�(�A�-A�(�A���A��A���A�VA��jA��FA�jA�z�A���A�
=A�v�A�oA�jA�A�"�A� �A���A��DA�`BAx1Ar  Aj-Ae
=A^9XAXbAQO�AL�AE��AAp�A;�A7`BA2{A-�hA* �A&�yA v�A�\AC�A�
A��A/A
VA/Ahs@�C�@���@�7L@��@�/@Ǖ�@��^@�&�@���@��m@�z�@�`B@���@���@��R@�X@�E�@�M�@� �@�$�@��`@��@�@�ƨ@�@�@}�@u?}@k��@b��@Y��@R=q@Lz�@Gl�@?�@:-@4I�@.�+@*J@$�j@ �@M�@��@��@V@	G�@�3333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333 ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB��B��B��B��B��B��B��B��B��B��B��B�
B�
B�
B�B�B�B�B�#B�#B�B�B�B��BĜB�7B}�Bp�BgmBYB8RB{B��B�sB��B�B�bB�BZB33B
��B
�9B
�B
Q�B
/B
%B	�B	B	��B	x�B	dZB	C�B	1'B	uB	B�B��BÖB�LB��B��B�JB�Bw�Bm�BaHBW
BR�BH�B?}B9XB8RB5?B9XBD�BW
Br�B�B��B��B�TB	B	�B	8RB	K�B	gmB	w�B	�B	�bB	��B	�B	�RB	ƨB	��B	�B	�B
  B
uB
�B
(�B
1'B
>wB
@�B
F�B
P�B
VB
YB
_;B
cTB
jB
o�B
y�B
|�B
z�B
�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B��B��B��B��B��B��B��B��B��B��B��B�
B�
B�
B�B�B�B�B�#B�#B�B�B�B�B��B�PB� Bq�BhsB\)B:^B�B��B�B��B�'B�oB�+B]/B5?B  B
�FB
�+B
S�B
1'B
+B	�B	ĜB	��B	y�B	ffB	D�B	33B	{B	B�B��BĜB�XB��B��B�PB�Bx�Bn�BbNBXBS�BJ�B@�B:^B9XB5?B:^BE�BXBr�B�B��B��B�TB	B	�B	8RB	K�B	gmB	w�B	�B	�bB	��B	�B	�RB	ƨB	��B	�B	�B
  B
uB
�B
(�B
1'B
>wB
@�B
F�B
P�B
VB
YB
_;B
cTB
jB
o�B
y�B
|�B
z�B
�3333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333 <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ = PRES-SP(NextCycle), where SP is SURFACE PRESSURE from next cycle                                                                                                                                                                                     TEMP_ADJUSTED = TEMP                                                                                                                                                                                                                                            PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,PRES_ADJ,elapsed_time,alpha,tau),elapsed_time=P/mean_rise_rate,P=dbar since the start of the profile for each samples                                                                                                       None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            SP(NextCycle)=0.0(dbar)                                                                                                                                                                                                                                         None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE; PRES_ADJ_ERR : Manufacture sensor accuracy                                                                                                                                                                 TEMP_ADJ_ERR : SBE sensor accuracy                                                                                                                                                                                                                              Salinity Recalculation using PRES_ADJ; PSAL_ADJ_ERR : max(sum of RecalS & CTM errors, 0.01(PSS-78))                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            TNPD: APEX float that truncated negative pressure drift                                                                                                                                                                                                         None                                                                                                                                                                                                                                                            No adjustment is needed(maybe salty drift)                                                                                                                                                                                                                      200801170744342008011707443420080117074434200801170751562008011707515620080117075156201309120000002013091200000020130912000000  JA  ARFMdecpA5_a                                                                20080104065035  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.4                                                                 20080104065038  IP                  G�O�G�O�G�O�                JA  ARCArsal2.1                                                                 20080104065038  IP  PSAL            G�O�G�O�G�O�                JA  ARGQrqcppo_a                                                                20080104065039  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19b                                                                20080104065043  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19b                                                                20080104065043  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.7d                                                                20080104065043  QCP$                G�O�G�O�G�O�            EB40JA  ARGQaqcp2.7d                                                                20080104065043  QCP$                G�O�G�O�G�O�            EB40JA  ARGQrqcpt16b                                                                20080104065043  QCP$                G�O�G�O�G�O�           10000JA  ARUP                                                                        20080104070127                      G�O�G�O�G�O�                JA  ARFMdecpA5_a                                                                20080108035158  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.4                                                                 20080108035203  IP                  G�O�G�O�G�O�                JA  ARCArsal2.1                                                                 20080108035204  IP  PSAL            G�O�G�O�G�O�                JA  ARGQrqcppo_a                                                                20080108035204  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19b                                                                20080108035208  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19b                                                                20080108035208  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.7d                                                                20080108035208  QCP$                G�O�G�O�G�O�            EB40JA  ARGQaqcp2.7d                                                                20080108035208  QCP$                G�O�G�O�G�O�            EB40JA  ARGQrqcpt16b                                                                20080108035209  QCP$                G�O�G�O�G�O�           10000JA  ARGQrelo2.1                                                                 20080108035209  CV  TIME            G�O�G�O�F��!                JA  ARGQrelo2.1                                                                 20080108035209  CV  LAT$            G�O�G�O�A���                JA  ARGQrelo2.1                                                                 20080108035209  CV  LON$            G�O�G�O����                JA  ARUP                                                                        20080108050557                      G�O�G�O�G�O�                JA  ARFMdecpA5_a                                                                20090401065111  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.5                                                                 20090401070141  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.0                                                                 20090401070141  IP  PRES            G�O�G�O�G�O�                JA  ARCArsal2.1a                                                                20090401070141  IP  PSAL            G�O�G�O�G�O�                JA  ARGQrqcppo_a                                                                20090401070141  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19b                                                                20090401070142  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19b                                                                20090401070142  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8b                                                                20090401070142  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8b                                                                20090401070142  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16b                                                                20090401070143  QCP$                G�O�G�O�G�O�           10000JA  ARGQrelo2.1                                                                 20090401070143  CV  TIME            G�O�G�O�F��!                JA  ARGQrelo2.1                                                                 20090401070143  CV  LAT$            G�O�G�O�A���                JA  ARGQrelo2.1                                                                 20090401070143  CV  LON$            G�O�G�O���-                JA  ARUP                                                                        20090401070450                      G�O�G�O�G�O�                JM  ARGQJMQC1.0                                                                 20080107171649  CV  DAT$            G�O�G�O�F��                JM  ARGQJMQC1.0                                                                 20080107171649  CV  DAT$            G�O�G�O�F��!                JM  ARGQJMQC1.0                                                                 20080107171649  CV  LAT$            G�O�G�O�A���                JM  ARGQJMQC1.0                                                                 20080107171649  CV  LON$            G�O�G�O����                JM  ARSQJMQC1.0                                                                 20080107171649  CF  PRES            @�33D��G�O�                JM  ARSQJMQC1.0                                                                 20080107171649  CF  TEMP            @�33D��G�O�                JM  ARSQJMQC1.0                                                                 20080107171649  CF  PSAL            @�33D��G�O�                JM  ARCAJMQC1.0                                                                 20080117074434  IP  PRES            G�O�G�O�G�O�                JM  ARCAJMQC1.0                                                                 20080117074434  IP  PSAL            G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20080117075156  CV  PSAL            G�O�G�O�G�O�                JM  ARSQOW  1.1 CTD2013V01                                                      20130912000000  IP  PSAL            G�O�G�O�G�O�                JA  RFMTcnvd2.1                                                                 20130924052434  IP                  G�O�G�O�G�O�                JA  ARDU                                                                        20130924052614                      G�O�G�O�G�O�                JM      JMFC1.0                                                                 20150609192630                      G�O�G�O�G�O�                JA  ARDU                                                                        20150614052516                      G�O�G�O�G�O�                