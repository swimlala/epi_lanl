CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PARAM       N_PROF        N_CALIB       N_LEVELS   s   	N_HISTORY             	   title         Argo float vertical profile    institution       JAMSTEC    source        
Argo float     history       b2006-03-26T06:50:04Z creation;2009-03-18T07:30:10Z update;2015-06-09T20:04:08Z conversion to V3.1;     
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
_FillValue                    iArgo profile    3.1 1.2 19500101000000  5900650 J-ARGO                                                          JAMSTEC                                                         PRES            TEMP            PSAL               8A   JA  20060326065004  20150614054512  A5_23632_056                    2C  D   APEX                            1557                            013004                          846 @���8 1   @���8 @5u\(��cex���1   ARGOS   A   A   A   Primary sampling: discrete [spot sampling CTD data]                                                                                                                                                                                                                @�  A��AfffA���A�  A�  B	��B��B2  BE33BZ  Bn  B���B�  B�ffB���B�  B�33B���B�33B�  B�ffB�  B�33B�  C�3C��C� CffCffCL�C33C$L�C)�C.ffC2�fC8ffC=L�CB��CG��CQ� C[ffCe33Co� Cy�C���C���C�� C��3C�Y�C���C���C�� C��3C�� C��fC���C�s3C�Cǀ C�ffCѦfC֌�C۳3C�fC�3C�fC��C�� C���D� D�fD�fD�3D��D��DٚD$� D)��D.�3D3�3D8�3D=��DBٚDG��DLٚDQ��DV� D[��D`��DeٚDj�3Do��DtٚDy��D�)�D�i�D���D��3D�)�D�l�D�� D��fD�)�D�ffD���D��fD�#3D�i�Dڬ�D��fD��D�Y�D�3D�  1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @�  A��AfffA���A�  A�  B	��B��B2  BE33BZ  Bn  B���B�  B�ffB���B�  B�33B���B�33B�  B�ffB�  B�33B�  C�3C��C� CffCffCL�C33C$L�C)�C.ffC2�fC8ffC=L�CB��CG��CQ� C[ffCe33Co� Cy�C���C���C�� C��3C�Y�C���C���C�� C��3C�� C��fC���C�s3C�Cǀ C�ffCѦfC֌�C۳3C�fC�3C�fC��C�� C���D� D�fD�fD�3D��D��DٚD$� D)��D.�3D3�3D8�3D=��DBٚDG��DLٚDQ��DV� D[��D`��DeٚDj�3Do��DtٚDy��D�)�D�i�D���D��3D�)�D�l�D�� D��fD�)�D�ffD���D��fD�#3D�i�Dڬ�D��fD��D�Y�D�3D�  1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�oA��hA�|�A��A�hsA�&�A�G�A�ƨA��7A�z�A�x�A�&�A��A��A��A��A�"�A��wA�p�A�7LA�ƨA��A�bNA�A���A��-A��A��^A���A�&�A��A�l�A��!A���A��TA���A���A��uA��Az�ArA�Ae�;Ad��A]�AS��AL��AD��AA�A9��A1�A.ȴA*�RA(^5A#��A��A`BAA��AS�A/AbNAo@��@�hs@���@�P@ܣ�@؃@պ^@��@Ų-@�r�@��@���@�G�@�K�@��@��R@�l�@�J@��T@�t�@���@���@��@�n�@�`B@��@���@�9X@�M�@��@��@��!@��D@��j@v�y@nv�@g;d@b=q@Y7L@S"�@I��@B-@<�/@7�w@1&�@+�m@(�u@#S�@�@��@|�@��@ff1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�oA��hA�|�A��A�hsA�&�A�G�A�ƨA��7A�z�A�x�A�&�A��A��A��A��A�"�A��wA�p�A�7LA�ƨA��A�bNA�A���A��-A��A��^A���A�&�A��A�l�A��!A���A��TA���A���A��uA��Az�ArA�Ae�;Ad��A]�AS��AL��AD��AA�A9��A1�A.ȴA*�RA(^5A#��A��A`BAA��AS�A/AbNAo@��@�hs@���@�P@ܣ�@؃@պ^@��@Ų-@�r�@��@���@�G�@�K�@��@��R@�l�@�J@��T@�t�@���@���@��@�n�@�`B@��@���@�9X@�M�@��@��@��!@��D@��j@v�y@nv�@g;d@b=q@Y7L@S"�@I��@B-@<�/@7�w@1&�@+�m@(�u@#S�@�@��@|�@��@ff1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB�B�B8RBA�BG�B^5BhsBn�Bp�Bz�B�+B��B��B�LB�wB�NBB��BB�B�#B�BB�;B��B�?B��Bz�BffB)�B1B�B��B��B�BF�B
��B
�)B
�RB
aHB	��B	ĜB	s�B	�XB	�hB	?}B	oB�/B�B�qB��B�oB�=B�bB�hB�=B{�BgmBaHBXBaHB]/BT�BG�B>wB33B0!B49B49B7LBH�B\)By�B�{B	�B	(�B	$�B	�B	#�B	�B	VB	w�B	�PB	��B	�oB	�oB	�RB	ǮB	�B	�5B	�TB	�yB	�B	�B	��B
B
1B
oB
�B
%�B
+B
2-B
6FB
>wB
D�B
L�B
R�B
YB
]/B
`BB
cTB
ffB
k�B
o�B
r�B
t�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B�B�B8RBB�BH�B_;BhsBn�Bp�Bz�B�+B��B��B�LB�}B�TBB��BB�B�#B�HB�BB��B�LB��B{�BjB,B
=B�B��B��B�7BM�BB
�5B
�jB
ffB	��B	ǮB	t�B	�dB	�{B	A�B	{B�5B�B�}B��B�uB�DB�hB�oB�DB}�BiyBbNBYBbNB^5BVBH�B@�B49B1'B5?B49B8RBI�B\)Bz�B�uB	�B	(�B	%�B	�B	$�B	�B	VB	w�B	�PB	��B	�oB	�oB	�RB	ǮB	�B	�5B	�TB	�yB	�B	�B	��B
B
1B
oB
�B
%�B
+B
2-B
6FB
>wB
D�B
L�B
R�B
YB
]/B
`BB
cTB
ffB
k�B
o�B
r�B
t�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - SP, where SP is SURFACE PRESSURE (minus 5 dbar for Apf-6,7,8) from next cycle.                                                                                                                                                           none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = RecalS = PSAL(PRES_ADJUSTED,TEMP,Conductivity)                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = celltm_sbe41(RecalS,TEMP,PRES_ADJUSTED,elapsed_time,alpha,tau),elapsed_time=P/mean_rise_rate,P= dbar since the start of the profile for each samples.                                                                                           none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            SP(NextCycle) = 0.0 dbar                                                                                                                                                                                                                                        none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using reported SURFACE PRESSURE. The quoted error is max [2.4, size of pressure adjustment] in dbar.                                                                                                                                      The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   Salinity Recalculation using PRES_ADJ; PSAL_ADJ_ERR : max(sum of RecalS & CTM errors, 0.01(PSS-78))                                                                                                                                                             none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         200706281405022007062814050220070628140502200706290310062007062903100620070629031006200707020000002007070200000020070702000000  JA  ARFMfmtp2.2                                                                 20060326065004  IP                  G�O�G�O�G�O�                JA  ARCAsspa2.0                                                                 20060326065004  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcp2.4                                                                 20060326065005  QCP$                G�O�G�O�G�O�           1F6BCJA  ARUP                                                                        20060326065732                      G�O�G�O�G�O�                JA  ARFMfmtp2.2                                                                 20060330034920  IP                  G�O�G�O�G�O�                JA  ARCAsspa2.0                                                                 20060330034921  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcp2.4                                                                 20060330034921  QCP$                G�O�G�O�G�O�           1F6BCJA  ARUP                                                                        20060330042358                      G�O�G�O�G�O�                JA  ARUP                                                                        20060602051135                      G�O�G�O�G�O�                JA  ARGQrqcp2.5                                                                 20060627031723  QCP$                G�O�G�O�G�O�            FB7CJA  ARGQaqcp2.5                                                                 20060627031723  QCP$                G�O�G�O�G�O�            FB40JA  ARUP                                                                        20060627035410                      G�O�G�O�G�O�                JM  ARCAJMQC1.0                                                                 20070628140502  IP  PRES            G�O�G�O�G�O�                JM  ARCAJMQC1.0                                                                 20070628140502  CV  PSAL            G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20070629031006  CV  PSAL            G�O�G�O�G�O�                JM  ARSQWJO 2.0 SeHyD1.0                                                        20070702000000  IP  PSAL            G�O�G�O�G�O�                JA  RFMTcnvd2.1                                                                 20071001072807  IP                  G�O�G�O�G�O�                JA  ARDU                                                                        20071001091741                      G�O�G�O�G�O�                JM  AREVjmrvp1.2                                                                20090312120641  IP  PRES            G�O�G�O�G�O�                JA  RFMTcnvd2.1                                                                 20090318072451  IP                  G�O�G�O�G�O�                JA  ARDU                                                                        20090318073010                      G�O�G�O�G�O�                JM      JMFC1.0                                                                 20150609200403                      G�O�G�O�G�O�                JA  ARDU                                                                        20150614054512                      G�O�G�O�G�O�                