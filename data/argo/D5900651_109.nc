CDF   !   
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PARAM       N_PROF        N_CALIB       N_LEVELS   s   	N_HISTORY             	   title         Argo float vertical profile    institution       JAMSTEC    source        
Argo float     history       b2007-09-07T18:50:35Z creation;2009-03-18T07:31:26Z update;2015-06-09T20:29:39Z conversion to V3.1;     
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
_FillValue                    iArgo profile    3.1 1.2 19500101000000  5900651 J-ARGO                                                          JAMSTEC                                                         PRES            TEMP            PSAL               mA   JA  20070907185035  20150621180540  A5_23694_109                    2C  D   APEX                            1561                            013004                          846 @ԓ����1   @ԓ�q�@5�r� Ĝ�b���R1   ARGOS   A   A   A   Primary sampling: discrete [spot sampling CTD data]                                                                                                                                                                                                                @���A33Ac33A�33A���A���B  B��B0  BE33BZffBnffB���B���B�ffB�33B���B�  B�33B���B���Bڙ�B�  B�  B���CL�CffC��C� CffC� C33C$L�C)33C.� C3L�C8ffC=  CB  CF�fCQ� C[L�Ce33Co�Cy33C��3C�� C���C��fC��3C�� C���C��fC���C�� C���C�� C���C�Cǳ3C̙�Cљ�C֙�C�� C�3C��C�s3C� C� C���D�3D��D� D��D�fD� D� D$� D)�3D.� D3��D8� D=� DB�fDG� DL� DQ� DV��D[�3D`�3De�fDj� DoٚDt� Dy��D�)�D�i�D��3D��D�#3D�` D���D��D��D�ffD���D�� D�&fD�c3Dک�D��D�&fD�Y�D�fD��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @�ffA  A`  A���A�33A�33B33B  B/33BDffBY��Bm��B�ffB�33B�  B���B�ffB���B���B�ffB�ffB�33B䙚B홚B�ffC�C33CffCL�C33CL�C  C$�C)  C.L�C3�C833C<��CA��CF�3CQL�C[�Ce  Cn�fCy  C���C�ffC�s3C���C���C��fC�� C���C�� C�ffC�� C�ffC�s3C�s3CǙ�C̀ Cр Cր CۦfC���C�s3C�Y�C�ffC�ffC�� D�fD��D�3D� D��D�3D�3D$�3D)�fD.�3D3� D8�3D=�3DB��DG�3DL�3DQ�3DV� D[�fD`�fDe��Dj�3Do��Dt�3Dy��D�#3D�c3D���D��3D��D�Y�D��3D��3D�3D�` D��fD�ٚD�  D�\�Dڣ3D��3D�  D�S3D� D��31111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��AϑhAϓuAϏ\Aϕ�Aϗ�A�~�A�O�A�9XA�1'A� �A��A�5?AɃA�jA���A�Q�A��A� �A�;dA�VA��wA��hA�M�A��-A���A��A�r�A�x�A�M�A�;dA�t�A��!A��yA��7A��^A��9A���A��A���A�^5A��TA��A��A|�jAu�Aj�9A]VAPQ�AJ�A@��A:$�A1�wA-��A*9XA%XA r�A�TA��A?}A�9A
�A�`Al�@�`B@�w@�(�@㕁@��T@�@��@ёh@�bN@�hs@��m@�b@�1'@�p�@�J@�ff@�  @�@���@��7@�dZ@�1'@���@�"�@�(�@��+@��`@��F@�@�o@���@�n�@��@�j@xA�@n��@g�w@]?}@V$�@L��@Fff@>�+@9��@4��@/��@*�@%�@�w@�@�y@��@�u1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 AϑhAϓuAϏ\Aϕ�Aϗ�A�~�A�O�A�9XA�1'A� �A��A�5?AɃA�jA���A�Q�A��A� �A�;dA�VA��wA��hA�M�A��-A���A��A�r�A�x�A�M�A�;dA�t�A��!A��yA��7A��^A��9A���A��A���A�^5A��TA��A��A|�jAu�Aj�9A]VAPQ�AJ�A@��A:$�A1�wA-��A*9XA%XA r�A�TA��A?}A�9A
�A�`Al�@�`B@�w@�(�@㕁@��T@�@��@ёh@�bN@�hs@��m@�b@�1'@�p�@�J@�ff@�  @�@���@��7@�dZ@�1'@���@�"�@�(�@��+@��`@��F@�@�o@���@�n�@��@�j@xA�@n��@g�w@]?}@V$�@L��@Fff@>�+@9��@4��@/��@*�@%�@�w@�@�y@��@�u1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oBG�BG�BF�BG�BG�BC�B?}B?}B>wB>wB6FB;dBC�BA�B/BT�BcTB�{B�/B�TB,B@�B2-BB�BL�BffB�1B�B�!B�jB�'B�LB��B�XB�B>wB��B�^B��B�BN�B
�wB
?}B	�;B	��B	O�B�)B��B�{Bu�Bl�B�B�B�B}�B�+B�B{�Bw�Br�B�Bw�BXBE�B:^BN�BT�Bq�Bq�B�B�DB��B�-B�TB	�B	(�B	K�B	cTB	u�B	�+B	�bB	��B	��B	�9B	�jB	��B	��B	�B	�#B	�ZB	�yB	�B	��B	��B	��B
DB
�B
 �B
(�B
/B
8RB
?}B
G�B
L�B
R�B
XB
\)B
aHB
e`B
iyB
o�B
s�B
w�B
z�B
}�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 BA�BA�B@�BA�BA�B=qB9XB9XB8RB9XB2-B8RBA�B=qB-BO�B^5B�\B�B�;B&�B<jB.B=qBG�BaHB�B��B�B�RB�B�-BȴB�?B�B;dB��B�?B�wB�BL�B
�dB
<jB	�#B	��B	M�B�B��B�hBq�BiyB{�B{�B�By�B�B}�Bv�Bs�Bm�B{�Bs�BS�BA�B5?BI�BO�Bl�Bl�B|�B�%B�oB�B�5B	uB	#�B	F�B	^5B	o�B	�B	�=B	�oB	��B	�B	�FB	ĜB	��B	��B	��B	�5B	�TB	�B	�B	��B	��B
B
hB
�B
"�B
(�B
2-B
9XB
A�B
F�B
L�B
Q�B
VB
[#B
_;B
cTB
iyB
m�B
q�B
t�B
w�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - SP, where SP is SURFACE PRESSURE (minus 5 dbar for Apf-6,7,8) from next cycle.                                                                                                                                                           none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = RecalS = PSAL(PRES_ADJUSTED,TEMP,Conductivity)                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = celltm_sbe41(RecalS,TEMP,PRES_ADJUSTED,elapsed_time,alpha,tau),elapsed_time=P/mean_rise_rate,P= dbar since the start of the profile for each samples.                                                                                           none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADUSTED=PSAL(corrected by SSP&CTM)+deltaS, where deltaS is calculated by WJO                                                                                                                                                                               SP(NextCycle) = 0.2 dbar                                                                                                                                                                                                                                        none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            r=0.9998(+-0.0001), deepest deltaS=-0.006(+-0.006)                                                                                                                                                                                                              Pressures adjusted by using reported SURFACE PRESSURE. The quoted error is max [2.4, size of pressure adjustment] in dbar.                                                                                                                                      The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   Salinity Recalculation using PRES_ADJUSTED. PSAL_ADJ_ERR : max(combines 1x WJO uncertainty & CTM adjustment , SBE sensor accuracy)                                                                                                                              none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            WJO(2003) salinity adjustment is adopted with SeHyD1.0; large scale 8/4, small scale 4/2; Use interpolate_float_valuesnov2003.m, map_data_grid.m, map_data_grid_t.m; Use T levels <= 4c; Run Const:18;                                                          200709202131192007092021311920070920213119200709202136162007092021361620070920213616200809300000002008093000000020080930000000  JA  ARFMdecpA5_a                                                                20070907185033  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.4                                                                 20070907185035  IP                  G�O�G�O�G�O�                JA  ARCArsal2.1                                                                 20070907185036  IP  PSAL            G�O�G�O�G�O�                JA  ARGQrqcppo_a                                                                20070907185036  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19b                                                                20070907185040  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19b                                                                20070907185040  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.7c                                                                20070907185041  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.7c                                                                20070907185041  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16a                                                                20070907185041  QCP$                G�O�G�O�G�O�           10000JA  ARGQaqcpt16a                                                                20070907185041  QCP$                G�O�G�O�G�O�           10000JA  ARUP                                                                        20070907190431                      G�O�G�O�G�O�                JA  ARFMdecpA5_a                                                                20070911155933  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.4                                                                 20070911155938  IP                  G�O�G�O�G�O�                JA  ARCArsal2.1                                                                 20070911155938  IP  PSAL            G�O�G�O�G�O�                JA  ARGQrqcppo_a                                                                20070911155939  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19b                                                                20070911155943  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19b                                                                20070911155943  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.7c                                                                20070911155943  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.7c                                                                20070911155943  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16a                                                                20070911155943  QCP$                G�O�G�O�G�O�           10000JA  ARGQaqcpt16a                                                                20070911155943  QCP$                G�O�G�O�G�O�           10000JA  ARUP                                                                        20070911161645                      G�O�G�O�G�O�                JM  ARCAJMQC1.0                                                                 20070920213119  CV  PRES            G�O�G�O�G�O�                JM  ARCAJMQC1.0                                                                 20070920213119  IP  PSAL            G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20070920213616  CV  PSAL            G�O�G�O�G�O�                JM  ARSQWJO 2.0 SeHyD1.0                                                        20080930000000  CV  PSAL            G�O�G�O�G�O�                JA  RFMTcnvd2.1                                                                 20081008001609  IP                  G�O�G�O�G�O�                JA  ARDU                                                                        20081008025537                      G�O�G�O�G�O�                JM  AREVjmrvp1.2                                                                20090312120708  IP  PRES            G�O�G�O�G�O�                JA  RFMTcnvd2.1                                                                 20090318072637  IP                  G�O�G�O�G�O�                JA  ARDU                                                                        20090318073126                      G�O�G�O�G�O�                JM      JMFC1.0                                                                 20150609202934                      G�O�G�O�G�O�                JA  ARDU                                                                        20150621180540                      G�O�G�O�G�O�                