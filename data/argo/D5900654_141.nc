CDF   3   
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PARAM       N_PROF        N_CALIB       N_LEVELS   s   	N_HISTORY             	   title         Argo float vertical profile    institution       JAMSTEC    source        
Argo float     history       b2008-07-25T04:49:40Z creation;2009-09-01T08:43:29Z update;2015-06-09T21:24:20Z conversion to V3.1;     
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
_FillValue                    iArgo profile    3.1 1.2 19500101000000  5900654 J-ARGO                                                          JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA  20080725044940  20150621190525  A5_23712_141                    2C  D   APEX                            1566                            013004                          846 @��c��1   @����Xe@2�/��w�c��O�;d1   ARGOS   A   A   A   Primary sampling: discrete [spot sampling CTD data]                                                                                                                                                                                                                @���A  Aa��A�  A���A陚B��B��B133BE33BX��Bl��B�33B���B�ffB�  B���B���B�  B���B�  Bۙ�B���B�ffB�33C  C33C� CL�C  CffCffC$ffC)� C.��C3��C8�C=ffCB� CGL�CQL�C[ffCe33Co  Cy33C�s3C���C���C�� C���C��fC��3C���C�ffC�s3C�� C��3C��fC���Cǳ3Č�Cљ�C֦fCۙ�C�fC��C�fC�fC��3C�� DٚD��D�fD�3D�3D� DٚD$�3D)�fD.� D3��D8�3D=� DB��DG��DL�3DQ�fDV�fD[�3D`�3De��Dj�fDo� Dt� Dy�3D�)�D�i�D���D��fD�&fD�c3D��fD��3D�#3D�VfD��3D�� D�,�D�l�Dک�D��fD�)�D�i�D�D�31111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @���A  AY��A�  A���A噚B��B��B/33BC33BV��Bj��B~ffB���B�ffB�  B���B���B�  B���B�  Bڙ�B���B�ffB�33C � C�3C  C��C� C�fC�fC#�fC)  C.�C3�C7��C<�fCB  CF��CP��CZ�fCd�3Cn� Cx�3C�33C�Y�C�Y�C�� C���C�ffC�s3C�L�C�&fC�33C�@ C�s3C�ffC�C�s3C�L�C�Y�C�ffC�Y�C�ffC�L�C�ffC�ffC�s3C�� D��D��D�fD�3D�3D� D��D$�3D)�fD.� D3��D8�3D=� DB��DG��DL�3DQ�fDV�fD[�3D`�3De��Dj�fDo� Dt� Dy�3D��D�Y�D���D��fD�fD�S3D��fD��3D�3D�FfD��3D�� D��D�\�Dڙ�D��fD��D�Y�D�D�31111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��Aէ�A՝�A�5?A���AԼjA�`BAӅA�?}A��AН�A�p�A�33A�\)A���A�-A�?}A�ffAǡ�AǗ�A�bAľwA���A��;A�A��A�Q�A��RA��wA�9XA���A�\)A�%A��9A�oA��A��HA���A�I�A���A���A��A��7A��Ax�!Am?}A[l�AP��AEO�A@��A=|�A1+A-��A)K�A(A%�-A �jAt�A��A�AƨAoAE�@��H@���@�I�@�&�@�r�@ް!@�5?@���@� �@�o@ǶF@��@���@�C�@��H@�9X@�p�@���@��R@��@�|�@�%@�J@�;d@�I�@���@�5?@�C�@��9@�ȴ@�Ĝ@�@��@�M�@}�h@wl�@n��@ix�@a�7@Up�@LZ@G�@=�T@6��@1�7@)�@$�/@+@��@?}@-@�T@
�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 Aէ�A՝�A�5?A���AԼjA�`BAӅA�?}A��AН�A�p�A�33A�\)A���A�-A�?}A�ffAǡ�AǗ�A�bAľwA���A��;A�A��A�Q�A��RA��wA�9XA���A�\)A�%A��9A�oA��A��HA���A�I�A���A���A��A��7A��Ax�!Am?}A[l�AP��AEO�A@��A=|�A1+A-��A)K�A(A%�-A �jAt�A��A�AƨAoAE�@��H@���@�I�@�&�@�r�@ް!@�5?@���@� �@�o@ǶF@��@���@�C�@��H@�9X@�p�@���@��R@��@�|�@�%@�J@�;d@�I�@���@�5?@�C�@��9@�ȴ@�Ĝ@�@��@�M�@}�h@wl�@n��@ix�@a�7@Up�@LZ@G�@=�T@6��@1�7@)�@$�/@+@��@?}@-@�T@
�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB
�BB
�;B
�B
��B
��B
��B
�wB
�9B
�B
�XB
�wB
��B
�%B
��B
�'B
�B
hsB
�B
��B
�BB
��BS�B��BK�Bn�B��B�'B�LB�LB�9B�dB�9B��B�Bk�Be`BJ�BVB��B�LB1'B
�^B
_;B
B	�-B	>wB	  B�HB�)BȴB�!B��B��B��B��B�uB�DB�B�PB�1B{�Bx�Bz�Bx�Bx�Bt�B|�B�B�DB�{BɺB�B�B��B	$�B	G�B	gmB	�B	��B	��B	�B	�RB	��B	ɺB	��B	�)B	�`B	�B	�B	��B	��B
B
B
+B
DB
�B
�B
!�B
&�B
,B
1'B
:^B
B�B
G�B
O�B
VB
[#B
W
B
ffB
k�B
p�B
t�B
w�B
|�B
�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B
�BB
�;B
�B
��B
��B
��B
�}B
�?B
�B
�XB
�wB
��B
�+B
��B
�3B
�B
hsB
�B
��B
�NBBVB��BK�Br�B��B�-B�RB�XB�9B�jB�LB��B�Bk�BgmBO�B\BB�jB5?B
�qB
bNB
+B	�LB	A�B	B�NB�/B��B�'B��B��B��B��B��B�JB�+B�VB�7B|�By�Bz�By�By�Bu�B}�B�B�JB�{BɺB�B�B��B	$�B	G�B	gmB	�B	��B	��B	�B	�RB	��B	ɺB	��B	�)B	�`B	�B	�B	��B	��B
B
B
+B
DB
�B
�B
!�B
&�B
,B
1'B
:^B
B�B
G�B
O�B
VB
[#B
W
B
ffB
k�B
p�B
t�B
w�B
|�B
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
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ = PRES-SP(NextCycle), where SP is SURFACE PRESSURE from next cycle                                                                                                                                                                                     TEMP_ADJUSTED = TEMP                                                                                                                                                                                                                                            PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,PRES_ADJ,elapsed_time,alpha,tau),elapsed_time=P/mean_rise_rate,P=dbar since the start of the profile for each samples                                                                                                       None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            SP(NextCycle)=0.5(dbar)                                                                                                                                                                                                                                         None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE; PRES_ADJ_ERR : Manufacture sensor accuracy                                                                                                                                                                 TEMP_ADJ_ERR : SBE sensor accuracy                                                                                                                                                                                                                              Salinity Recalculation using PRES_ADJ; PSAL_ADJ_ERR : max(sum of RecalS & CTM errors, 0.01(PSS-78))                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         200808071437452008080714374520080807143745200808071446202008080714462020080807144620200908260000002009082600000020090826000000  JA  ARFMdecpA5_a                                                                20080725044938  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.4                                                                 20080725044940  IP                  G�O�G�O�G�O�                JA  ARCArsal2.1                                                                 20080725044941  IP  PSAL            G�O�G�O�G�O�                JA  ARGQrqcppo_a                                                                20080725044941  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19b                                                                20080725044945  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19b                                                                20080725044945  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8a                                                                20080725044946  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8a                                                                20080725044946  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16b                                                                20080725044946  QCP$                G�O�G�O�G�O�           10000JA  ARUP                                                                        20080725050913                      G�O�G�O�G�O�                JA  ARFMdecpA5_a                                                                20080729035311  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.4                                                                 20080729035316  IP                  G�O�G�O�G�O�                JA  ARCArsal2.1                                                                 20080729035317  IP  PSAL            G�O�G�O�G�O�                JA  ARGQrqcppo_a                                                                20080729035317  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19b                                                                20080729035321  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19b                                                                20080729035321  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8a                                                                20080729035321  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8a                                                                20080729035321  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16b                                                                20080729035322  QCP$                G�O�G�O�G�O�           10000JA  ARGQrelo2.1                                                                 20080729035322  CV  TIME            G�O�G�O�                    JA  ARGQrelo2.1                                                                 20080729035322  CV  LAT$            G�O�G�O�A��                JA  ARGQrelo2.1                                                                 20080729035322  CV  LON$            G�O�G�O��j                JA  ARUP                                                                        20080729052349                      G�O�G�O�G�O�                JA  ARCArsal2.1                                                                 20080912002141  IP  PSAL            G�O�G�O�G�O�                JA  ARGQrqcppo_a                                                                20080912002141  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19b                                                                20080912002145  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19b                                                                20080912002145  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8a                                                                20080912002145  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8a                                                                20080912002145  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16b                                                                20080912002146  QCP$                G�O�G�O�G�O�           10000JA  ARUP                                                                        20080912020018                      G�O�G�O�G�O�                JA  ARFMdecpA5_a                                                                20080729035311  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.5                                                                 20090414042856  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.0                                                                 20090414042856  IP  PRES            G�O�G�O�G�O�                JA  ARCArsal2.1a                                                                20090414042857  IP  PSAL            G�O�G�O�G�O�                JA  ARGQrqcppo_a                                                                20090414042857  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19b                                                                20090414042858  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19b                                                                20090414042858  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8b                                                                20090414042858  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8b                                                                20090414042858  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16b                                                                20090414042858  QCP$                G�O�G�O�G�O�           10000JA  ARUP                                                                        20090414042943                      G�O�G�O�G�O�                JM  ARGQJMQC1.0                                                                 20080728225952  CV  DAT$            G�O�G�O�F��                JM  ARCAJMQC1.0                                                                 20080807143745  CV  PRES            G�O�G�O�G�O�                JM  ARCAJMQC1.0                                                                 20080807143745  IP  PSAL            G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20080807144620  CV  PSAL            G�O�G�O�G�O�                JM  ARSQOW  1.1 SeHyD1.0                                                        20090826000000  IP  PSAL            G�O�G�O�G�O�                JA  RFMTcnvd2.1                                                                 20090901084255  IP                  G�O�G�O�G�O�                JA  ARDU                                                                        20090901084329                      G�O�G�O�G�O�                JM      JMFC1.0                                                                 20150609212414                      G�O�G�O�G�O�                JA  ARDU                                                                        20150621190525                      G�O�G�O�G�O�                