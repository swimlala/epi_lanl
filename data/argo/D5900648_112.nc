CDF   +   
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PARAM       N_PROF        N_CALIB       N_LEVELS   s   	N_HISTORY             	   title         Argo float vertical profile    institution       JAMSTEC    source        
Argo float     history       b2007-10-06T04:50:50Z creation;2013-09-24T05:26:04Z update;2015-06-09T19:24:53Z conversion to V3.1;     
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
_FillValue                    iArgo profile    3.1 1.2 19500101000000  5900648 J-ARGO                                                          JAMSTEC                                                         PRES            TEMP            PSAL               pA   JA  20071006045050  20150614052513  A5_28347_112                    2C  D   APEX                            1316                            013004                          846 @Ԛ@`��1   @ԚA�g@7\(��c�/��w1   ARGOS   F   F   F   Primary sampling: discrete [spot sampling CTD data]                                                                                                                                                                                                                @�  A��Aa��A�ffA���A�33B��B��B133BE33BZffBm��B���B�  B�33B�  B�33B�  B���B�  B�33B�ffB�  B�  B���CffC33C� CL�CffC33CffC$L�C)ffC.L�C3�C833C=33CB33CG�CP�fC[ffCeL�Co33CyL�C��fC���C���C���C�s3C��3C�� C��3C�� C���C��3C�� C���C�Cǌ�C̦fCљ�Cր Cی�C�fC�fC�3CC��3C�s3D� DٚD��D��DٚD�3DٚD$�fD)�3D.ٚD3ٚD8��D=� DBٚDG��DL�3DQ�3DVٚD[��D`ٚDe�3Dj�3DoٚDt��Dy� D�0 D�i�D���D��fD�&fD�ffD��fD�� D�&fD�\�D�� D��fD�)�D�l�Dڬ�D���D�#3D�c3D�D�31111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @�  A��Aa��A�ffA���A�33B��B��B133BE33BZffBm��B���B�  B�33B�  B�33B�  B���B�  B�33B�ffB�  B�  B���CffC33C� CL�CffC33CffC$L�C)ffC.L�C3�C833C=33CB33CG�CP�fC[ffCeL�Co33CyL�C��fC���C���C���C�s3C��3C�� C��3C�� C���C��3C�� C���C�Cǌ�C̦fCљ�Cր Cی�C�fC�fC�3CC��3C�s3D� DٚD��D��DٚD�3DٚD$�fD)�3D.ٚD3ٚD8��D=� DBٚDG��DL�3DQ�3DVٚD[��D`ٚDe�3Dj�3DoٚDt��Dy� D�0 D�i�D���D��fD�&fD�ffD��fD�� D�&fD�\�D�� D��fD�)�D�l�Dڬ�D���D�#3D�c3D�D�33333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�A�A�E�A�A�A�C�A�/A�+A�(�A��A�1A�A���A��AоwA��TA�JA�bA��7A�A�A�p�A��-A���A�-A���A�t�A��A�I�A��uA�^5A�
=A�=qA���A��A� �A���A�(�A�VA�JA�C�A��A�oA��^A�Q�A�$�Ar�/Ai;dAd(�Aa�mAal�A`�HAR1AJffAC�A;�^A7��A3��A.��A*��A'�A"�`A�#A��A�!A��A��A
��A��A Z@�^5@���@��@ܓu@�z�@�7L@š�@���@�  @��P@�-@��@���@�$�@�v�@���@�p�@�O�@��j@��j@�I�@�E�@��P@�n�@�?}@�P@}p�@y��@r��@k@b�!@[33@Tz�@K��@A�#@:�\@4��@.ff@+S�@'
=@"�H@$�@33@V@��@5?@	��@�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�A�A�E�A�A�A�C�A�/A�+A�(�A��A�1A�A���A��AоwA��TA�JA�bA��7A�A�A�p�A��-A���A�-A���A�t�A��A�I�A��uA�^5A�
=A�=qA���A��A� �A���A�(�A�VA�JA�C�A��A�oA��^A�Q�A�$�Ar�/Ai;dAd(�Aa�mAal�A`�HAR1AJffAC�A;�^A7��A3��A.��A*��A'�A"�`A�#A��A�!A��A��A
��A��A Z@�^5@���@��@ܓu@�z�@�7L@š�@���@�  @��P@�-@��@���@�$�@�v�@���@�p�@�O�@��j@��j@�I�@�E�@��P@�n�@�?}@�P@}p�@y��@r��@k@b�!@[33@Tz�@K��@A�#@:�\@4��@.ff@+S�@'
=@"�H@$�@33@V@��@5?@	��@�3333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333 ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB�B��B��B��B��B��B��B��B�B�B�?B�5B%B1BBBB��B��B�B�)B��BŢB�XB��B��B�hB�DBv�BXBC�B.BuB�B�BĜB��Bz�B_;B�B
�sB
��B
Q�B	�B	�B	��B	�PB	�DB	�B	(�B	#�B	\B�TB�
B��B�B��B��B�{B�bB�7B�Bs�Bl�BaHBP�BK�BYBe`Be`BdZBk�Bq�By�B�\B��B�9B�-BƨB�yB��B	�B	+B	I�B	w�B	� B	�bB	��B	�LB	��B	ƨB	�B	�;B	�B	��B
B
bB
�B
&�B
0!B
<jB
D�B
K�B
Q�B
XB
\)B
`BB
e`B
iyB
m�B
r�B
w�B
{�B
~�B
�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B�B��B��B��B��B��B��B��B�B�B�?B�5B+B\B	7B%BB��B��B�B�BB��BǮB�dB��B��B�hB�PBx�BZBE�B0!B�B�B�BǮB��B{�BcTB!�B
�B
��B
T�B	�B	�B	��B	�PB	�DB	�B	+B	%�B	hB�ZB�B��B�B��B��B��B�hB�=B�Bt�Bm�BcTBQ�BM�BYBffBffBe`Bl�Br�Bz�B�bB��B�?B�3BƨB�yB��B	�B	+B	I�B	w�B	� B	�bB	��B	�LB	��B	ƨB	�B	�;B	�B	��B
B
bB
�B
&�B
0!B
<jB
D�B
K�B
Q�B
XB
\)B
`BB
e`B
iyB
m�B
r�B
w�B
{�B
~�B
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
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ = PRES-SP(NextCycle), where SP is SURFACE PRESSURE from next cycle                                                                                                                                                                                     TEMP_ADJUSTED = TEMP                                                                                                                                                                                                                                            PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,PRES_ADJ,elapsed_time,alpha,tau),elapsed_time=P/mean_rise_rate,P=dbar since the start of the profile for each samples                                                                                                       None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            SP(NextCycle)=0.0(dbar)                                                                                                                                                                                                                                         None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE; PRES_ADJ_ERR : Manufacture sensor accuracy                                                                                                                                                                 TEMP_ADJ_ERR : SBE sensor accuracy                                                                                                                                                                                                                              Salinity Recalculation using PRES_ADJ; PSAL_ADJ_ERR : max(sum of RecalS & CTM errors, 0.01(PSS-78))                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            TNPD: APEX float that truncated negative pressure drift                                                                                                                                                                                                         None                                                                                                                                                                                                                                                            No adjustment is needed(maybe salty drift)                                                                                                                                                                                                                      200710190752112007101907521120071019075211200710190805282007101908052820071019080528201309120000002013091200000020130912000000  JA  ARFMdecpA5_a                                                                20071006045048  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.4                                                                 20071006045050  IP                  G�O�G�O�G�O�                JA  ARCArsal2.1                                                                 20071006045051  IP  PSAL            G�O�G�O�G�O�                JA  ARGQrqcppo_a                                                                20071006045051  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19b                                                                20071006045055  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19b                                                                20071006045055  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.7c                                                                20071006045055  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.7c                                                                20071006045055  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16b                                                                20071006045056  QCP$                G�O�G�O�G�O�           10000JA  ARUP                                                                        20071006050450                      G�O�G�O�G�O�                JA  ARFMdecpA5_a                                                                20071010034925  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.4                                                                 20071010034929  IP                  G�O�G�O�G�O�                JA  ARCArsal2.1                                                                 20071010034930  IP  PSAL            G�O�G�O�G�O�                JA  ARGQrqcppo_a                                                                20071010034930  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19b                                                                20071010034934  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19b                                                                20071010034934  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.7c                                                                20071010034934  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.7c                                                                20071010034934  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16b                                                                20071010034935  QCP$                G�O�G�O�G�O�           10000JA  ARUP                                                                        20071010050711                      G�O�G�O�G�O�                JA  ARFMdecpA5_a                                                                20090401065100  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.5                                                                 20090401070117  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.0                                                                 20090401070118  IP  PRES            G�O�G�O�G�O�                JA  ARCArsal2.1a                                                                20090401070118  IP  PSAL            G�O�G�O�G�O�                JA  ARGQrqcppo_a                                                                20090401070118  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19b                                                                20090401070119  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19b                                                                20090401070119  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8b                                                                20090401070119  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8b                                                                20090401070119  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16b                                                                20090401070119  QCP$                G�O�G�O�G�O�           10000JA  ARUP                                                                        20090401070458                      G�O�G�O�G�O�                JM  ARGQJMQC1.0                                                                 20071009164234  CV  DAT$            G�O�G�O�F��                JM  ARSQJMQC1.0                                                                 20071009164234  CF  PRES            @�  D�3G�O�                JM  ARSQJMQC1.0                                                                 20071009164234  CF  TEMP            @�  D�3G�O�                JM  ARSQJMQC1.0                                                                 20071009164234  CF  PSAL            @�  D�3G�O�                JM  ARCAJMQC1.0                                                                 20071019075211  IP  PRES            G�O�G�O�G�O�                JM  ARCAJMQC1.0                                                                 20071019075211  IP  PSAL            G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20071019080528  CV  PSAL            G�O�G�O�G�O�                JM  ARSQOW  1.1 CTD2013V01                                                      20130912000000  IP  PSAL            G�O�G�O�G�O�                JA  RFMTcnvd2.1                                                                 20130924052412  IP                  G�O�G�O�G�O�                JA  ARDU                                                                        20130924052604                      G�O�G�O�G�O�                JM      JMFC1.0                                                                 20150609192442                      G�O�G�O�G�O�                JA  ARDU                                                                        20150614052513                      G�O�G�O�G�O�                