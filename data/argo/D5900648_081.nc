CDF       
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PARAM       N_PROF        N_CALIB       N_LEVELS   s   	N_HISTORY             	   title         Argo float vertical profile    institution       JAMSTEC    source        
Argo float     history       b2006-12-04T03:49:22Z creation;2013-09-24T05:25:50Z update;2015-06-09T19:18:53Z conversion to V3.1;     
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
_FillValue                    iArgo profile    3.1 1.2 19500101000000  5900648 J-ARGO                                                          JAMSTEC                                                         PRES            TEMP            PSAL               QA   JA  20061204034922  20150614052511  A5_28347_081                    2C  D   APEX                            1316                            013004                          846 @�L��P�1   @�L�4Vv@6��;dZ�c>�\(��1   ARGOS   A   A   A   Primary sampling: discrete [spot sampling CTD data]                                                                                                                                                                                                                @�33A33Aa��A���A�  A�33BffB��B/��BDffBX��Bn  B�  B�  B�33B���B�33B���B�33Bƙ�B�33B�ffB㙚B홚B�33C��C��C� CL�C��CL�C� C$L�C)33C.�C333C7��C=�CA�fCF��CQ�C[� CeL�Co� Cy33C���C���C��3C���C�� C��fC��3C�� C�� C���C���C��3C��3C�ffC�s3C̳3Cѳ3Cֳ3C�ffC�s3C�3C�� C�fC��fC��fD��DٚD� D�fD��D� D� D$��D)��D.��D3��D8�fD=�3DB�3DG�3DL� DQ�fDVٚD[�3D`��De�fDj�3Do� Dt� DyٚD��D�p D�� D�� D�&fD�s3D���D���D��D�ffD�� D��3D�  D�c3Dڠ D�� D�  D�\�D�D�0 1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @�33A33Aa��A���A�  A�33BffB��B/��BDffBX��Bn  B�  B�  B�33B���B�33B���B�33Bƙ�B�33B�ffB㙚B홚B�33C��C��C� CL�C��CL�C� C$L�C)33C.�C333C7��C=�CA�fCF��CQ�C[� CeL�Co� Cy33C���C���C��3C���C�� C��fC��3C�� C�� C���C���C��3C��3C�ffC�s3C̳3Cѳ3Cֳ3C�ffC�s3C�3C�� C�fC��fC��fD��DٚD� D�fD��D� D� D$��D)��D.��D3��D8�fD=�3DB�3DG�3DL� DQ�fDVٚD[�3D`��De�fDj�3Do� Dt� DyٚD��D�p D�� D�� D�&fD�s3D���D���D��D�ffD�� D��3D�  D�c3Dڠ D�� D�  D�\�D�D�0 2222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�1'A��
A�+A�1'A�33A�5?A�9XA�;dA�9XA�-A�-A�-A�33A�(�A� �A���A�t�A���A�ZA���A��A���A��A�7LA�A���A�dZA�"�A�bA�\)A�hsA��
A��jA���A��A��A�7LA�A��A�/A��/A�C�A���Ax�uAm��Ael�A`��A]��AV��AS�7AIK�AC`BA=�A: �A6z�A2�A,ĜA*9XA#�^AS�A��AƨAbNA+A�AV@��-@�\)@��@�!@�-@�\)@д9@�x�@�V@�
=@�@���@�Q�@��@�t�@�l�@�A�@��@�S�@�@�n�@�Q�@��@���@��@��T@�dZ@�x�@}��@t��@kt�@a��@Zn�@S��@M�T@G�;@>��@6�y@0 �@+��@%�h@ Ĝ@S�@;d@�D@��@ƨ@r�@E�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�1'A��
A�+A�1'A�33A�5?A�9XA�;dA�9XA�-A�-A�-A�33A�(�A� �A���A�t�A���A�ZA���A��A���A��A�7LA�A���A�dZA�"�A�bA�\)A�hsA��
A��jA���A��A��A�7LA�A��A�/A��/A�C�A���Ax�uAm��Ael�A`��A]��AV��AS�7AIK�AC`BA=�A: �A6z�A2�A,ĜA*9XA#�^AS�A��AƨAbNA+A�AV@��-@�\)@��@�!@�-@�\)@д9@�x�@�V@�
=@�@���@�Q�@��@�t�@�l�@�A�@��@�S�@�@�n�@�Q�@��@���@��@��T@�dZ@�x�@}��@t��@kt�@a��@Zn�@S��@M�T@G�;@>��@6�y@0 �@+��@%�h@ Ĝ@S�@;d@�D@��@ƨ@r�@E�2222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222 ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB0!B�B�!B�jB�wBǮB��B��B��B��B��B�B�)B�B;dBƨBƨBƨB�jB�B�B�XB�FB�LB��B�Bs�Bp�Bv�BZBB�B"�B%B�B�BB��BffB]/BT�B�B
��B
|�B
�B	�-B	�uB	�\B	�PB	dZB	Q�B	PB�mB�)B��BƨB�qB�B��B��B�=B�%B� Bu�BjB`BBI�BC�B?}B9XB33B8RB1'B,B9XBW
BiyBy�B�bB�BǮB�;B��B	�B	>wB	VB	k�B	�B	��B	��B	�RB	��B	��B	�B	�;B	�NB	��B
	7B
�B
"�B
.B
6FB
:^B
C�B
I�B
P�B
VB
]/B
aHB
hsB
l�B
p�B
t�B
y�B
|�B
�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B1'B�B�!B�jB�wBǮB��B��B��B��B��B�B�)B�B>wB��B��BȴB�wB�!B�B�dB�LB�XB��B�%Bt�Bq�Bx�B]/BD�B$�B+B��B�#BĜB��BgmB]/BW
B!�B
��B
� B
�B	�9B	�{B	�bB	�\B	e`B	T�B	\B�sB�/B��BǮB�}B�B��B��B�DB�+B�Bv�Bk�BcTBK�BD�B@�B:^B49B9XB2-B-B:^BXBiyBy�B�bB�BǮB�;B��B	�B	>wB	VB	k�B	�B	��B	��B	�RB	��B	��B	�B	�;B	�NB	��B
	7B
�B
"�B
.B
6FB
:^B
C�B
I�B
P�B
VB
]/B
aHB
hsB
l�B
p�B
t�B
y�B
|�B
�2222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222 <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ = PRES-SP(NextCycle), where SP is SURFACE PRESSURE from next cycle                                                                                                                                                                                     TEMP_ADJUSTED = TEMP                                                                                                                                                                                                                                            PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,PRES_ADJ,elapsed_time,alpha,tau),elapsed_time=P/mean_rise_rate,P=dbar since the start of the profile for each samples                                                                                                       None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            SP(NextCycle)=0.0(dbar)                                                                                                                                                                                                                                         None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE; PRES_ADJ_ERR : Manufacture sensor accuracy                                                                                                                                                                 TEMP_ADJ_ERR : SBE sensor accuracy                                                                                                                                                                                                                              Salinity Recalculation using PRES_ADJ; PSAL_ADJ_ERR : max(sum of RecalS & CTM errors, 0.01(PSS-78))                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            TNPD: APEX float that truncated negative pressure drift                                                                                                                                                                                                         None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         200612231755592006122317555920061223175559200701270207342007012702073420070127020734201309120000002013091200000020130912000000  JA  ARFMfmtp2.3                                                                 20061204034922  IP                  G�O�G�O�G�O�                JA  ARGQrqcp2.5                                                                 20061204034923  QCP$                G�O�G�O�G�O�           1FB7CJA  ARUP                                                                        20061204040253                      G�O�G�O�G�O�                JA  ARFMdecpA5_a                                                                20090401065020  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.5                                                                 20090401070002  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.0                                                                 20090401070002  IP  PRES            G�O�G�O�G�O�                JA  ARCArsal2.1a                                                                20090401070002  IP  PSAL            G�O�G�O�G�O�                JA  ARGQrqcppo_a                                                                20090401070003  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19b                                                                20090401070004  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19b                                                                20090401070004  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8b                                                                20090401070004  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8b                                                                20090401070004  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16b                                                                20090401070004  QCP$                G�O�G�O�G�O�           10000JA  ARGQrelo2.1                                                                 20090401070004  CV  TIME            G�O�G�O�F�e�                JA  ARGQrelo2.1                                                                 20090401070004  CV  LAT$            G�O�G�O�A�~�                JA  ARGQrelo2.1                                                                 20090401070004  CV  LON$            G�O�G�O���{                JA  ARUP                                                                        20090401070506                      G�O�G�O�G�O�                JM  ARGQJMQC1.0                                                                 20061207162112  CV  DAT$            G�O�G�O�F�e�                JM  ARGQJMQC1.0                                                                 20061207162112  CV  DAT$            G�O�G�O�F�e�                JM  ARGQJMQC1.0                                                                 20061207162112  CV  LAT$            G�O�G�O�A��                JM  ARGQJMQC1.0                                                                 20061207162112  CV  LON$            G�O�G�O���9                JM  ARSQJMQC1.0                                                                 20061207162112  CF  PRES            @�33D�0 G�O�                JM  ARSQJMQC1.0                                                                 20061207162112  CF  TEMP            @�33D�0 G�O�                JM  ARSQJMQC1.0                                                                 20061207162112  CF  PSAL            @�33D�0 G�O�                JM  ARCAJMQC1.0                                                                 20061223175559  IP  PRES            G�O�G�O�G�O�                JM  ARCAJMQC1.0                                                                 20061223175559  IP  PSAL            G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20070127020734  CV  PSAL            G�O�G�O�G�O�                JM  ARSQOW  1.1 CTD2013V01                                                      20130912000000  IP  PSAL            G�O�G�O�G�O�                JA  RFMTcnvd2.1                                                                 20130924052340  IP                  G�O�G�O�G�O�                JA  ARDU                                                                        20130924052550                      G�O�G�O�G�O�                JM      JMFC1.0                                                                 20150609191847                      G�O�G�O�G�O�                JA  ARDU                                                                        20150614052511                      G�O�G�O�G�O�                