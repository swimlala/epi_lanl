CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS   s   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JAMSTEC    source        
Argo float     history       b2005-09-26T06:57:56Z creation;2013-09-24T05:26:03Z update;2015-06-09T19:10:40Z conversion to V3.1;     
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      
_FillValue               conventions       Argo reference table 1          6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    6�   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  6�   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  78   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7x   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    7�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     8   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8<   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8\   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
_FillValue        A.�~       standard_name         time   
resolution        >�����h�   axis      T           8`   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8h   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
_FillValue        A.�~       
resolution        >�����h�        8l   LATITUDE               	long_name         &Latitude of the station, best estimate     units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        standard_name         latitude   axis      Y           8t   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        standard_name         	longitude      axis      X           8|   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8�   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8�   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    8�   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    8�   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    8�   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8�   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9�   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   standard_name         sea_water_pressure     axis      Z        �  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  t  ;l   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   standard_name         sea_water_pressure       �  ;�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  t  =�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  >    TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o   standard_name         sea_water_temperature        �  ?�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  t  A�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o   standard_name         sea_water_temperature        �  B,   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  t  C�   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  Dl   PSAL         
      	   	long_name         Practical salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o   standard_name         sea_water_salinity       �  F8   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  t  H   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o   standard_name         sea_water_salinity       �  Hx   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  t  JD   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  J�   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  L�   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   M   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   V   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   _   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  h   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    h�   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    h�   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    h�   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    h�   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  h�   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    h�   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    h�   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    h�   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         i   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         i   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        i   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    iArgo profile    3.1 1.2 19500101000000  5900648 J-ARGO                                                          JAMSTEC                                                         PRES            TEMP            PSAL               &A   JA  20050926065756  20150614050512  A5_28347_038                    2C  D   APEX                            1316                            013004                          846 @��@����1   @��Ce�8 @6�1&��cS�O�;d1   ARGOS   A   A   A   Primary sampling: discrete [spot sampling CTD data]                                                                                                                                                                                                                @���A��AfffA�  A�  A���B	33B��B0��BE��BY��Bl��B�33B�  B���B�  B�33B���B���Bƙ�B���Bڙ�B�ffBB�  C�C�CffC� C33CL�C33C$33C)� C.ffC3  C8�C=L�CB33CG�CP��C[  CeL�CoffCy33C��3C�� C���C��fC�� C��3C�� C���C��3C���C�s3C�� C�� C�s3C�s3Č�Cѳ3C���CۦfC�� C�� C��C��C��C�� D��D��D�3DٚD� D�fD�fD$��D)��D.��D3�3D8�3D=� DB�3DGٚDL�fDQ��DV�3D[�3D`��De� Dj�fDo��Dt��Dy�3D�#3D�p D���D��3D�,�D�c3D���D��D�  D�p D��3D��fD�&fD�ffDک�D���D��D�i�D��D��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @���A��AfffA�  A�  A���B	33B��B0��BE��BY��Bl��B�33B�  B���B�  B�33B���B���Bƙ�B���Bڙ�B�ffBB�  C�C�CffC� C33CL�C33C$33C)� C.ffC3  C8�C=L�CB33CG�CP��C[  CeL�CoffCy33C��3C�� C���C��fC�� C��3C�� C���C��3C���C�s3C�� C�� C�s3C�s3Č�Cѳ3C���CۦfC�� C�� C��C��C��C�� D��D��D�3DٚD� D�fD�fD$��D)��D.��D3�3D8�3D=� DB�3DGٚDL�fDQ��DV�3D[�3D`��De� Dj�fDo��Dt��Dy�3D�#3D�p D���D��3D�,�D�c3D���D��D�  D�p D��3D��fD�&fD�ffDک�D���D��D�i�D��D��2222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�\)A�
=A���AҮA�E�A���AѾwAѡ�AѓuA�bNA��AЗ�A�%AΡ�A�Aš�A�hsA�%A�bA��wA�O�A�{A��mA���A���A��-A��DA�  A�G�A�n�A��DA�~�A�ffA���A��DA��^A��A�{A���A���A��A�A�A�ȴA��jA��9A�jAs�;Ao��Ae��AKt�A>�A:=qA4$�A.A'`BA#G�A�mA�9A?}A�!AbNA	hsA��A�A E�@�|�@�o@��#@�R@�5?@�{@թ�@�l�@�1'@Ý�@��P@�E�@�{@��;@��P@�Ĝ@���@�K�@��h@��#@��y@�ȴ@��@�J@��;@��@���@��T@�I�@�M�@��@y�#@r-@f�+@\��@U�h@P��@I�#@B~�@<j@5�T@/��@+�@%@�;@��@�h@��@�@��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�\)A�
=A���AҮA�E�A���AѾwAѡ�AѓuA�bNA��AЗ�A�%AΡ�A�Aš�A�hsA�%A�bA��wA�O�A�{A��mA���A���A��-A��DA�  A�G�A�n�A��DA�~�A�ffA���A��DA��^A��A�{A���A���A��A�A�A�ȴA��jA��9A�jAs�;Ao��Ae��AKt�A>�A:=qA4$�A.A'`BA#G�A�mA�9A?}A�!AbNA	hsA��A�A E�@�|�@�o@��#@�R@�5?@�{@թ�@�l�@�1'@Ý�@��P@�E�@�{@��;@��P@�Ĝ@���@�K�@��h@��#@��y@�ȴ@��@�J@��;@��@���@��T@�I�@�M�@��@y�#@r-@f�+@\��@U�h@P��@I�#@B~�@<j@5�T@/��@+�@%@�;@��@�h@��@�@��2222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222 ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oBBBBBBB1B+B+BB��B�B�B�yB��B�HB�jB�TB�B.B9XBL�Bu�B`BB`BBaHB�{B��B�FB��B�wBBǮB�
B��B��B�jB��B�+Bk�B>wBB��B�B
��B	�B	\)B	x�B	C�BXB�B�B.BZBXBy�Bv�Bl�BiyB`BBW
BN�BJ�BF�BD�B6FB,B(�B$�B"�B>wB��B��B�B��B��B�`B	B	uB	2-B	M�B	iyB	z�B	�B	��B	�'B	�qB	ŢB	��B	�
B	�BB	�mB	�B	�B	��B
B
PB
�B
$�B
0!B
6FB
:^B
A�B
F�B
L�B
R�B
YB
]/B
aHB
hsB
k�B
q�B
t�B
x�B
{�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 BBBBBBB1B+B+BB��B�B�B�B��B�`B�wB�fB�B/B:^BM�Bv�BaHBaHBaHB�{B��B�RB��B�}BÖBȴB�B��B��B�wB��B�7Bm�B@�B+BŢB�B
��B	�B	]/B	z�B	I�B\)B�B�B0!B\)BYBz�Bw�Bm�BjBbNBXBO�BK�BG�BE�B7LB-B)�B%�B#�B>wB��B��B�B��B��B�`B	B	uB	2-B	M�B	iyB	z�B	�B	��B	�'B	�qB	ŢB	��B	�
B	�BB	�mB	�B	�B	��B
B
PB
�B
$�B
0!B
6FB
:^B
A�B
F�B
L�B
R�B
YB
]/B
aHB
hsB
k�B
q�B
t�B
x�B
{�2222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222 <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ = PRES-SP(NextCycle), where SP is SURFACE PRESSURE from next cycle                                                                                                                                                                                     TEMP_ADJUSTED = TEMP                                                                                                                                                                                                                                            PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,PRES_ADJ,elapsed_time,alpha,tau),elapsed_time=P/mean_rise_rate,P=dbar since the start of the profile for each samples                                                                                                       None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            SP(NextCycle)=0.0(dbar)                                                                                                                                                                                                                                         None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                      None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE; PRES_ADJ_ERR : Manufacture sensor accuracy                                                                                                                                                                 TEMP_ADJ_ERR : SBE sensor accuracy                                                                                                                                                                                                                              Salinity Recalculation using PRES_ADJ; PSAL_ADJ_ERR : max(sum of RecalS & CTM errors, 0.01(PSS-78))                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            TNPD: APEX float that truncated negative pressure drift                                                                                                                                                                                                         None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         200510090000002005100900000020051009000000201107090013572011070900135720110709001357201309120000002013091200000020130912000000  JA  ARFMfmtp2.2                                                                 20050926065756  IP                  G�O�G�O�G�O�                JA  ARGQrqcp2.3                                                                 20050926065756  QCP$                G�O�G�O�G�O�           1FB7CJA  ARUP                                                                        20050926070750                      G�O�G�O�G�O�                JA  ARFMfmtp2.2                                                                 20050930005540  IP                  G�O�G�O�G�O�                JA  ARGQrqcp2.3                                                                 20050930005541  QCP$                G�O�G�O�G�O�           1FB7CJA  ARUP                                                                        20050930010424                      G�O�G�O�G�O�                JM  ARCAJMQC                                                                    20051009000000  IP  PRES            G�O�G�O�G�O�                JM  ARCAJMQC                                                                    20051009000000  IP  PSAL            G�O�G�O�G�O�                JM  ARSQWJO 1   SeHyD1                                                          20060419000000  IP  PSAL            G�O�G�O�G�O�                JA  RFMTcnvd2.1                                                                 20060906041923  IP                  G�O�G�O�G�O�                JA  ARDU                                                                        20060906050449                      G�O�G�O�G�O�                JM  AREVjmrvp1.2                                                                20090312120547  IP  PRES            G�O�G�O�G�O�                JA  RFMTcnvd2.1                                                                 20090318071829  IP                  G�O�G�O�G�O�                JA  ARDU                                                                        20090318071950                      G�O�G�O�G�O�                JM  ARGQREJM1.0                                                                 20130912000000  CV  JULD            G�O�G�O�F�
                JM  ARCAJMTM1.0                                                                 20110709001357  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  AREQREJM1.0                                                                 20130912000000  CF  PRES_ADJUSTED_QC@���D��G�O�                JM  AREQREJM1.0                                                                 20130912000000  CF  TEMP_ADJUSTED_QC@���D��G�O�                JM  AREQREJM1.0                                                                 20130912000000  CF  PSAL_ADJUSTED_QC@���D��G�O�                JA  RFMTcnvd2.1                                                                 20130924052407  IP                  G�O�G�O�G�O�                JA  ARDU                                                                        20130924052603                      G�O�G�O�G�O�                JM      JMFC1.0                                                                 20150609191029                      G�O�G�O�G�O�                JA  ARDU                                                                        20150614050512                      G�O�G�O�G�O�                