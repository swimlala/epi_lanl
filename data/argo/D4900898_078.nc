CDF   $   
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PARAM       N_PROF        N_CALIB       N_LEVELS   s   	N_HISTORY             	   title         Argo float vertical profile    institution       JAMSTEC    source        
Argo float     history       b2008-08-25T12:55:40Z creation;2009-08-20T07:43:30Z update;2015-06-07T15:06:16Z conversion to V3.1;     
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
_FillValue                    iArgo profile    3.1 1.2 19500101000000  4900898 J-ARGO                                                          JAMSTEC                                                         PRES            TEMP            PSAL               NA   JA  20080825125540  20150613142516  A9_60144_078                    2C  D   APEX                            2414                            061305                          846 @��UW
=�1   @��VN��@DM����d�+1   ARGOS   A   A   A   Primary sampling: discrete [spot sampling CTD data]                                                                                                                                                                                                                @�33A  Ai��A���Ař�A�ffB
��B33B0  BDffBY��Bm��B���B�  B���B�33B���B�33B�ffB�33B�  B�  B䙚B�33B���CL�C�C33CL�C� CffCL�C$� C)33C.�C3L�C8  C=ffCB��CGL�CQL�C[L�Ce33Co33CyffC���C�� C��fC��3C���C���C��fC�� C�� C���C��fC��3C�s3C�Cǳ3C̦fC���C�� Cی�C�fC��C�3C�s3C�ffC�� D�3D��D�3D��D�fDٚD�3D$��D)�3D.�3D3�3D8�3D=�fDB��DG� DL�fDQ��DV��D[� D`�fDe�fDj�3DoٚDt�fDyٚD�)�D�ffD��3D��3D�,�D�c3D�� D���D��D�ffD��fD���D�  D�\�Dڬ�D�� D�&fD�\�D�fD�c31111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @�  AffA`  A���A���A陚BffB��B-��BB  BW33Bk33B33B���B�ffB�  B���B�  B�33B�  B���B���B�ffB�  B�ffC �3C� C
��C�3C�fC��C�3C#�fC(��C-� C2�3C7ffC<��CB  CF�3CP�3CZ�3Cd��Cn��Cx��C�@ C�33C�Y�C�ffC�@ C�@ C�Y�C�33C�33C�@ C�Y�C�ffC�&fC�@ C�ffC�Y�Cр C�s3C�@ C�Y�C�@ C�ffC�&fC��C�33D��D�3D��D�fD� D�3D��D$�fD)��D.��D3��D8��D=� DB�fDG��DL� DQ�3DV�fD[��D`� De� Dj��Do�3Dt� Dy�3D�fD�S3D�� D�� D��D�P D���D�ٚD�fD�S3D��3D�ٚD��D�I�Dڙ�D���D�3D�I�D�3D�P 1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�"�A�$�A�JA�`BA��Au�
Ad^5A\�AQ\)AIO�AA��A8�+A41A4�A4��A133A/hsA-��A,�/A+�TA*z�A(��A.~�A/�wA.�A-��A*��A'�7A%C�A �A!\)AA?}A�AE�A��AJAĜA�A&�AĜA�A+A��A5?AĜA��A��A��A�mA"�A	p�A
=A��A�@�ƨ@�hs@�7L@��@睲@��@���@��m@ԣ�@���@��@���@\@�\)@��;@�5?@��@�\)@�n�@��j@��F@��w@�(�@���@�5?@� �@�x�@��\@��@�A�@{@uV@p�`@l�@h�`@fv�@c�m@^�+@["�@V�R@P  @H��@A��@:M�@4��@/\)@*��@&�R@"��@�y@o@Q�@��@J@��@33@�@��@^5@ ��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�"�A�$�A�JA�`BA��Au�
Ad^5A\�AQ\)AIO�AA��A8�+A41A4�A4��A133A/hsA-��A,�/A+�TA*z�A(��A.~�A/�wA.�A-��A*��A'�7A%C�A �A!\)AA?}A�AE�A��AJAĜA�A&�AĜA�A+A��A5?AĜA��A��A��A�mA"�A	p�A
=A��A�@�ƨ@�hs@�7L@��@睲@��@���@��m@ԣ�@���@��@���@\@�\)@��;@�5?@��@�\)@�n�@��j@��F@��w@�(�@���@�5?@� �@�x�@��\@��@�A�@{@uV@p�`@l�@h�`@fv�@c�m@^�+@["�@V�R@P  @H��@A��@:M�@4��@/\)@*��@&�R@"��@�y@o@Q�@��@J@��@33@�@��@^5@ ��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB�B�B�BB%B�B�BA�B_;By�B�+B�oB��B�ZB�BɺB�dB�!B�B��B��B��BB�B$�B#�B{BB�HB�^BȴB�!B��B�BȴB�ZB�B
=BL�BT�BhsBaHBbNBXBJ�BF�B=qB;dB2-B.B$�B'�B�B�B�BoBDB%B��B�B�B�yB�sB�fB�`B�ZB�/B�B�#B�5B�ZB�B��B1B�B-B<jBK�BT�BbNBm�B{�B�VB��B�-B��B��B�HB�B��B	B	PB	�B	(�B	7LB	L�B	cTB	x�B	�\B	��B	�!B	�wB	��B	�B	�mB	��B
B
VB
�B
$�B
1'B
;dB
E�B
M�B
R�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B�B�B�BVB�B&�B�BG�BcTB}�B�JB�{B��B�ZB�B��B�jB�'B�B��B��B��BB�B%�B%�B�BB�TB�^B��B�'B��B�BȴB�ZB�B	7BL�BT�BiyBbNBcTBYBK�BG�B=qB<jB33B/B$�B(�B�B�B�BuBJB+B��B�B�B�B�sB�mB�`B�`B�5B�B�#B�5B�ZB�B��B1B�B-B<jBK�BT�BbNBm�B{�B�VB��B�-B��B��B�HB�B��B	B	PB	�B	(�B	7LB	L�B	cTB	x�B	�\B	��B	�!B	�wB	��B	�B	�mB	��B
B
VB
�B
$�B
1'B
;dB
E�B
M�B
R�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 <#�
<#�
<#�
<49X<���<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ = PRES-SP(NextCycle), where SP is SURFACE PRESSURE from next cycle                                                                                                                                                                                     TEMP_ADJUSTED = TEMP                                                                                                                                                                                                                                            PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,PRES_ADJ,elapsed_time,alpha,tau),elapsed_time=P/mean_rise_rate,P=dbar since the start of the profile for each samples                                                                                                       None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            SP(NextCycle)=0.6(dbar)                                                                                                                                                                                                                                         None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE; PRES_ADJ_ERR : Manufacture sensor accuracy                                                                                                                                                                 TEMP_ADJ_ERR : SBE sensor accuracy                                                                                                                                                                                                                              Salinity Recalculation using PRES_ADJ; PSAL_ADJ_ERR : max(sum of RecalS & CTM errors, 0.01(PSS-78))                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         200809071402262008090714022620080907140226200809071420482008090714204820080907142048200908190000002009081900000020090819000000  JA  ARFMdecpA9_b                                                                20080825125537  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.4                                                                 20080825125540  IP                  G�O�G�O�G�O�                JA  ARGQrqcppo_a                                                                20080825125541  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19b                                                                20080825125545  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8a                                                                20080825125545  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16b                                                                20080825125545  QCP$                G�O�G�O�G�O�           10000JA  ARUP                                                                        20080825130647                      G�O�G�O�G�O�                JA  ARFMdecpA9_b                                                                20080829161744  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.4                                                                 20080829161804  IP                  G�O�G�O�G�O�                JA  ARGQrqcppo_a                                                                20080829161807  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19b                                                                20080829161816  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8a                                                                20080829161817  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16b                                                                20080829161818  QCP$                G�O�G�O�G�O�           10000JA  ARUP                                                                        20080829190814                      G�O�G�O�G�O�                JA  ARFMdecpA9_b                                                                20080829161744  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.5                                                                 20090422045108  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.0                                                                 20090422045108  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_b                                                                20090422045108  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19b                                                                20090422045109  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19b                                                                20090422045109  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8b                                                                20090422045109  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8b                                                                20090422045109  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16b                                                                20090422045109  QCP$                G�O�G�O�G�O�           10000JA  ARUP                                                                        20090422045639                      G�O�G�O�G�O�                JM  ARGQJMQC1.0                                                                 20080831050317  CV  DAT$            G�O�G�O�F�Z�                JM  ARGQJMQC1.0                                                                 20080831050317  CV  DAT$            G�O�G�O�F�Z�                JM  ARGQJMQC1.0                                                                 20080831050317  CV  LAT$            G�O�G�O�B P                JM  ARGQJMQC1.0                                                                 20080831050317  CV  LON$            G�O�G�O�� ��                JM  ARCAJMQC1.0                                                                 20080907140226  CV  PRES            G�O�G�O�G�O�                JM  ARCAJMQC1.0                                                                 20080907140226  IP  PSAL            G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20080907142048  CV  PSAL            G�O�G�O�G�O�                JM  ARSQOW  1.1 SeHyD1.0                                                        20090819000000  IP  PSAL            G�O�G�O�G�O�                JA  RFMTcnvd2.1                                                                 20090820074148  IP                  G�O�G�O�G�O�                JA  ARDU                                                                        20090820074330                      G�O�G�O�G�O�                JM      JMFC1.0                                                                 20150607150608                      G�O�G�O�G�O�                JA  ARDU                                                                        20150613142516                      G�O�G�O�G�O�                