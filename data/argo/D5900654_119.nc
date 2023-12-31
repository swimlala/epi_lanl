CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PARAM       N_PROF        N_CALIB       N_LEVELS   s   	N_HISTORY             	   title         Argo float vertical profile    institution       JAMSTEC    source        
Argo float     history       b2007-12-18T04:50:22Z creation;2009-03-18T07:28:12Z update;2015-06-09T21:20:14Z conversion to V3.1;     
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
_FillValue                    iArgo profile    3.1 1.2 19500101000000  5900654 J-ARGO                                                          JAMSTEC                                                         PRES            TEMP            PSAL               wA   JA  20071218045022  20150621190522  A5_23712_119                    2C  D   APEX                            1566                            013004                          846 @Ԭ���1   @Ԭ��(3�@2��Q��b�\(�1   ARGOS   A   A   A   Primary sampling: discrete [spot sampling CTD data]                                                                                                                                                                                                                @�  AffAc33A�  A�33A�ffBffB��B0ffBE��BXffBl  B�33B�  B���B�  B�33B�ffB�33Bƙ�B�  B�ffB�33B�33B���C� C� C33C�C33C  C  C$L�C)  C-�fC3  C8  C=33CBffCG  CQffC[��Ce� CoL�Cy  C���C�� C���C���C���C��fC��3C��fC��3C���C��fC���C���C¦fCǌ�C�� CѦfC֦fC۳3C���C�3C�3C� C�s3C�� D�3DٚD��D� D�3DٚD�fD$��D)�3D.ٚD3�fD8��D=� DBٚDG�fDL� DQٚDV� D[�3D`�fDe�fDj�3Do� Dt� Dy�3D�  D�i�D���D�ٚD�&fD�i�D��fD���D�&fD�p D���D��fD�fD�` Dڠ D���D�#3D�c3D�D�,�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @�  AffA[33A�  A�33A�ffBffB��B.ffBC��BVffBj  B~ffB�  B���B�  B�33B�ffB�33Bř�B�  B�ffB�33B�33B���C  C  C
�3C��C�3C� C� C#��C(� C-ffC2� C7� C<�3CA�fCF� CP�fC[�Ce  Cn��Cx� C�Y�C�� C���C���C�L�C�ffC�s3C�ffC�s3C�Y�C�ffC�L�C�L�C�ffC�L�C̀ C�ffC�ffC�s3C�L�C�s3C�s3C�@ C�33C�@ D�3D��D��D� D�3D��D�fD$��D)�3D.��D3�fD8��D=� DB��DG�fDL� DQ��DV� D[�3D`�fDe�fDj�3Do� Dt� Dy�3D� D�Y�D���D�ɚD�fD�Y�D��fD���D�fD�` D���D��fD�fD�P Dڐ D���D�3D�S3D�D��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A��HA��;A��mA��`A��`A��;A���A���A���A���A���A���A���A���A���AǼjA�\)A��A���A��A�O�A�=qA��A�jA���A�G�A��A��A�z�A�Q�A��9A��!A��DA��A��A�I�A��!A�v�A��mA��!A�+A|�/Aj�`A]�ARI�ACK�A;ƨA4��A2r�A.  A)�A�yA^5A;dA�AS�A��AbNA V@��@�ȴ@�?}@߶F@�&�@��@�J@��`@��@�-@��@�{@�?}@�z�@�Ĝ@Ł@�@���@��@�o@���@�-@�@�@���@�-@��@�A�@���@�"�@�1'@�X@��@��h@�l�@���@�dZ@��#@z-@sS�@i��@a��@Y�^@P  @Gl�@A�7@9�^@2��@,9X@&v�@!&�@��@+@S�@b@��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A��HA��;A��mA��`A��`A��;A���A���A���A���A���A���A���A���A���AǼjA�\)A��A���A��A�O�A�=qA��A�jA���A�G�A��A��A�z�A�Q�A��9A��!A��DA��A��A�I�A��!A�v�A��mA��!A�+A|�/Aj�`A]�ARI�ACK�A;ƨA4��A2r�A.  A)�A�yA^5A;dA�AS�A��AbNA V@��@�ȴ@�?}@߶F@�&�@��@�J@��`@��@�-@��@�{@�?}@�z�@�Ĝ@Ł@�@���@��@�o@���@�-@�@�@���@�-@��@�A�@���@�"�@�1'@�X@��@��h@�l�@���@�dZ@��#@z-@sS�@i��@a��@Y�^@P  @Gl�@A�7@9�^@2��@,9X@&v�@!&�@��@+@S�@b@��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB�/B�5B�5B�5B�)B�)B�#B�)B�)B�#B�)B�)B�)B�#B�#B�B��B�%B�+B�%B�FB�B��B�sB��B�BB,B=qB5?B�B�B �B�B�`B�^Br�B�B
�B
��B
�B	�yB	dZB	F�B�yB�B�1B�hB�'BĜBŢB��B��B�B� BcTBq�B�bB{�BR�B��B��BaHB��B�JB��B	B�B	\B	�B	P�B	W
B	o�B	s�B	� B	��B	��B	��B	�-B	�FB	B	��B	��B	�B	�;B	�fB	�mB	�B	��B	��B	��B	��B
B
+B
DB
hB
�B
 �B
&�B
.B
5?B
<jB
B�B
J�B
O�B
VB
\)B
bNB
ffB
k�B
p�B
t�B
x�B
{�B
{�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B�/B�5B�5B�5B�)B�)B�#B�)B�)B�#B�)B�)B�)B�#B�#B�B�5B�7B�=B�1B�RB�B��B�yB��B�BB-B>wB7LB�B�B!�B �B�mB�qBv�B"�B
��B
��B
�B	�B	hsB	I�B�B�%B�=B�oB�-BŢBȴB��B��B�%B�BdZBr�B�hB}�BR�B��B�B`BB��B�JB��B	B�B	\B	�B	P�B	W
B	p�B	s�B	� B	��B	��B	��B	�-B	�FB	B	��B	��B	�B	�;B	�fB	�mB	�B	��B	��B	��B	��B
B
+B
DB
hB
�B
 �B
&�B
.B
5?B
<jB
B�B
J�B
O�B
VB
\)B
bNB
ffB
k�B
p�B
t�B
x�B
{�B
{�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<7�4<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - SP, where SP is SURFACE PRESSURE (minus 5 dbar for Apf-6,7,8) from next cycle.                                                                                                                                                           none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = RecalS = PSAL(PRES_ADJUSTED,TEMP,Conductivity)                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = celltm_sbe41(RecalS,TEMP,PRES_ADJUSTED,elapsed_time,alpha,tau),elapsed_time=P/mean_rise_rate,P= dbar since the start of the profile for each samples.                                                                                           none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            SP(NextCycle) = 0.5 dbar                                                                                                                                                                                                                                        none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using reported SURFACE PRESSURE. The quoted error is max [2.4, size of pressure adjustment] in dbar.                                                                                                                                      The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   Salinity Recalculation using PRES_ADJUSTED. PSAL_ADJ_ERR : SBE sensor accuracy & CTM adjustment                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         200712311350182007123113501820071231135018200712311408152007123114081520071231140815200809050000002008090500000020080905000000  JA  ARFMdecpA5_a                                                                20071218045017  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.4                                                                 20071218045022  IP                  G�O�G�O�G�O�                JA  ARCArsal2.1                                                                 20071218045022  IP  PSAL            G�O�G�O�G�O�                JA  ARGQrqcppo_a                                                                20071218045023  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19b                                                                20071218045026  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19b                                                                20071218045026  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.7c                                                                20071218045027  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.7c                                                                20071218045027  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16b                                                                20071218045027  QCP$                G�O�G�O�G�O�           10000JA  ARUP                                                                        20071218050610                      G�O�G�O�G�O�                JA  ARFMdecpA5_a                                                                20071222035203  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.4                                                                 20071222035208  IP                  G�O�G�O�G�O�                JA  ARCArsal2.1                                                                 20071222035209  IP  PSAL            G�O�G�O�G�O�                JA  ARGQrqcppo_a                                                                20071222035209  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19b                                                                20071222035213  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19b                                                                20071222035213  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.7d                                                                20071222035213  QCP$                G�O�G�O�G�O�            EB40JA  ARGQaqcp2.7d                                                                20071222035213  QCP$                G�O�G�O�G�O�            EB40JA  ARGQrqcpt16b                                                                20071222035213  QCP$                G�O�G�O�G�O�           10000JA  ARUP                                                                        20071222051140                      G�O�G�O�G�O�                JM  ARCAJMQC1.0                                                                 20071231135018  CV  PRES            G�O�G�O�G�O�                JM  ARCAJMQC1.0                                                                 20071231135018  IP  PSAL            G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20071231140815  CV  PSAL            G�O�G�O�G�O�                JM  ARSQWJO 2.0 SeHyD1.0                                                        20080905000000  IP  PSAL            G�O�G�O�G�O�                JA  RFMTcnvd2.1                                                                 20080911042230  IP                  G�O�G�O�G�O�                JA  ARDU                                                                        20080911054546                      G�O�G�O�G�O�                JM  AREVjmrvp1.2                                                                20090312120835  IP  PRES            G�O�G�O�G�O�                JA  RFMTcnvd2.1                                                                 20090318072241  IP                  G�O�G�O�G�O�                JA  ARDU                                                                        20090318072812                      G�O�G�O�G�O�                JM      JMFC1.0                                                                 20150609212008                      G�O�G�O�G�O�                JA  ARDU                                                                        20150621190522                      G�O�G�O�G�O�                