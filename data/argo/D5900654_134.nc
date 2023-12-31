CDF   0   
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PARAM       N_PROF        N_CALIB       N_LEVELS   s   	N_HISTORY             	   title         Argo float vertical profile    institution       JAMSTEC    source        
Argo float     history       b2008-05-16T06:50:31Z creation;2009-09-01T08:43:27Z update;2015-06-09T21:23:05Z conversion to V3.1;     
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
_FillValue                    iArgo profile    3.1 1.2 19500101000000  5900654 J-ARGO                                                          JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA  20080516065031  20150621190519  A5_23712_134                    2C  D   APEX                            1566                            013004                          846 @���g��1   @���7Z@2�C��%�c���O�;1   ARGOS   A   A   A   Primary sampling: discrete [spot sampling CTD data]                                                                                                                                                                                                                @�33A��A\��A�ffA���A�33B
ffB��B0ffBE33BZ  Bm33B�ffB�33B�ffB���B�  B�ffB���B�  B�  B�  B�33B�  B���C��CffCffCffC  CffC�3C$��C)L�C.ffC3L�C8�C=L�CBffCG  CQ��C[��Ce33Co33CyL�C���C�� C�s3C��fC���C�� C�� C�� C��fC�s3C���C�ffC�� C�� Cǳ3C̦fCь�C�� C۳3C���C�s3C� CC� C���D��DٚD�fD�3D��D��DٚD$�fD)�3D.��D3�3D8��D=�3DB��DG�3DL��DQ� DV��D[�3D`� DeٚDj��Do��Dt��Dy�3D�&fD�i�D��fD���D�,�D�l�D���D�ٚD�#3D�c3D���D���D�,�D�c3Dڣ3D��3D��D�\�D� D�S31111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @�33A��AT��A�ffA���A�33BffB��B.ffBC33BX  Bk33B~��B�33B�ffB���B�  B�ffB���B�  B�  B�  B�33B�  B���C�C�fC
�fC�fC� C�fC33C$�C(��C-�fC2��C7��C<��CA�fCF� CQ�C[�Cd�3Cn�3Cx��C�Y�C�@ C�33C�ffC���C�� C�@ C�� C�ffC�33C�Y�C�&fC�@ C C�s3C�ffC�L�Cր C�s3C�L�C�33C�@ C�Y�C�@ C���D��D��D�fD�3D��D��D��D$�fD)�3D.��D3�3D8��D=�3DB��DG�3DL��DQ� DV��D[�3D`� De��Dj��Do��Dt��Dy�3D�fD�Y�D��fD���D��D�\�D���D�ɚD�3D�S3D���D���D��D�S3Dړ3D��3D��D�L�D� D�C31111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A��A�?}A�?}A�hsA�M�A�+A�
=A���A°!A°!A¡�A©�A¥�A¬A�A�APAAA�jA��A��jA�A��A�
=A���A���A��A�+A�VA��\A�ƨA�t�A��A��`A���A���A��FA�{A� �A��A��A��A�ȴA�oAw�Ah�RAP1AG��A=&�A3|�A.�9A(�`A#\)A`BAp�A�/A-A&�A
�!A�A �@��@�x�@���@�9X@�  @��@ꟾ@�V@�%@�9X@�p�@�p�@�^5@�{@��;@Õ�@��P@�^5@���@�v�@�Z@��R@���@���@�j@��R@��F@���@�C�@�Q�@���@���@��9@�\)@���@zM�@sS�@e�h@\�j@S�@Ihs@D�@=/@5?}@/�@)X@#��@�T@^5@$�@��@@o1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A��A�?}A�?}A�hsA�M�A�+A�
=A���A°!A°!A¡�A©�A¥�A¬A�A�APAAA�jA��A��jA�A��A�
=A���A���A��A�+A�VA��\A�ƨA�t�A��A��`A���A���A��FA�{A� �A��A��A��A�ȴA�oAw�Ah�RAP1AG��A=&�A3|�A.�9A(�`A#\)A`BAp�A�/A-A&�A
�!A�A �@��@�x�@���@�9X@�  @��@ꟾ@�V@�%@�9X@�p�@�p�@�^5@�{@��;@Õ�@��P@�^5@���@�v�@�Z@��R@���@���@�j@��R@��F@���@�C�@�Q�@���@���@��9@�\)@���@zM�@sS�@e�h@\�j@S�@Ihs@D�@=/@5?}@/�@)X@#��@�T@^5@$�@��@@o1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB
YB
ZB
B�B
8RB
<jB
=qB
;dB
49B
2-B
5?B
33B
7LB
8RB
:^B
<jB
@�B
A�B
E�B
[#B
�yB��Be`Be`B�B�B�wB��B��B1BDB�BB�fB�5B�5B%B0!B+B#�B
=B�;B��B%�B
�3B
hsB	��B	�PB�B��BǮB�B��B��B�hB�\B�%B�Bz�B{�Bw�Bw�B�B�B�?B��B��B�yB	B�B�HB	�B	9XB	G�B	]/B	s�B	�+B	��B	��B	��B	��B	�B	�3B	��B	��B	�B	�#B	�NB	�TB	�B	�B	��B	��B	��B
B
1B
\B
�B
!�B
%�B
1'B
7LB
>wB
H�B
L�B
R�B
YB
^5B
cTB
hsB
n�B
q�B
u�B
y�B
|�B
� 1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B
ZB
[#B
C�B
8RB
<jB
=qB
;dB
49B
2-B
5?B
33B
7LB
8RB
:^B
<jB
@�B
A�B
E�B
[#B
�yB��BffBgmB�B�B��B��B��B
=BJB�BB�mB�;B�;B+B33B,B&�BJB�NB��B)�B
�?B
l�B
B	�uB�B�B��B�B��B��B�oB�bB�+B�B{�B|�By�Bx�B�%B�B�FB��B��B�yB	+B�B�NB	�B	:^B	G�B	]/B	s�B	�+B	��B	��B	��B	��B	�B	�3B	��B	��B	�B	�#B	�NB	�TB	�B	�B	��B	��B	��B
B
1B
\B
�B
!�B
%�B
1'B
7LB
>wB
H�B
L�B
R�B
YB
^5B
cTB
hsB
n�B
q�B
u�B
y�B
|�B
� 1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ = PRES-SP(NextCycle), where SP is SURFACE PRESSURE from next cycle                                                                                                                                                                                     TEMP_ADJUSTED = TEMP                                                                                                                                                                                                                                            PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,PRES_ADJ,elapsed_time,alpha,tau),elapsed_time=P/mean_rise_rate,P=dbar since the start of the profile for each samples                                                                                                       None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            SP(NextCycle)=0.5(dbar)                                                                                                                                                                                                                                         None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE; PRES_ADJ_ERR : Manufacture sensor accuracy                                                                                                                                                                 TEMP_ADJ_ERR : SBE sensor accuracy                                                                                                                                                                                                                              Salinity Recalculation using PRES_ADJ; PSAL_ADJ_ERR : max(sum of RecalS & CTM errors, 0.01(PSS-78))                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         200805291344242008052913442420080529134424200805291406232008052914062320080529140623200908260000002009082600000020090826000000  JA  ARFMdecpA5_a                                                                20080516065027  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.4                                                                 20080516065031  IP                  G�O�G�O�G�O�                JA  ARCArsal2.1                                                                 20080516065031  IP  PSAL            G�O�G�O�G�O�                JA  ARGQrqcppo_a                                                                20080516065032  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19b                                                                20080516065036  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19b                                                                20080516065036  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8a                                                                20080516065036  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8a                                                                20080516065036  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16b                                                                20080516065036  QCP$                G�O�G�O�G�O�           10000JA  ARUP                                                                        20080516070437                      G�O�G�O�G�O�                JA  ARFMdecpA5_a                                                                20080520035221  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.4                                                                 20080520035226  IP                  G�O�G�O�G�O�                JA  ARCArsal2.1                                                                 20080520035226  IP  PSAL            G�O�G�O�G�O�                JA  ARGQrqcppo_a                                                                20080520035227  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19b                                                                20080520035231  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19b                                                                20080520035231  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8a                                                                20080520035231  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8a                                                                20080520035231  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16b                                                                20080520035231  QCP$                G�O�G�O�G�O�           10000JA  ARUP                                                                        20080520052018                      G�O�G�O�G�O�                JA  ARCArsal2.1                                                                 20080912002104  IP  PSAL            G�O�G�O�G�O�                JA  ARGQrqcppo_a                                                                20080912002105  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19b                                                                20080912002109  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19b                                                                20080912002109  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8a                                                                20080912002109  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8a                                                                20080912002109  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16b                                                                20080912002109  QCP$                G�O�G�O�G�O�           10000JA  ARUP                                                                        20080912015716                      G�O�G�O�G�O�                JA  ARFMdecpA5_a                                                                20080520035221  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.5                                                                 20090414042841  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.0                                                                 20090414042841  IP  PRES            G�O�G�O�G�O�                JA  ARCArsal2.1a                                                                20090414042841  IP  PSAL            G�O�G�O�G�O�                JA  ARGQrqcppo_a                                                                20090414042841  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19b                                                                20090414042842  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19b                                                                20090414042842  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8b                                                                20090414042842  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8b                                                                20090414042842  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16b                                                                20090414042842  QCP$                G�O�G�O�G�O�           10000JA  ARUP                                                                        20090414042947                      G�O�G�O�G�O�                JM  ARGQJMQC1.0                                                                 20080519230601  CV  DAT$            G�O�G�O�F��=                JM  ARCAJMQC1.0                                                                 20080529134424  CV  PRES            G�O�G�O�G�O�                JM  ARCAJMQC1.0                                                                 20080529134424  IP  PSAL            G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20080529140623  CV  PSAL            G�O�G�O�G�O�                JM  ARSQOW  1.1 SeHyD1.0                                                        20090826000000  IP  PSAL            G�O�G�O�G�O�                JA  RFMTcnvd2.1                                                                 20090901084249  IP                  G�O�G�O�G�O�                JA  ARDU                                                                        20090901084327                      G�O�G�O�G�O�                JM      JMFC1.0                                                                 20150609212253                      G�O�G�O�G�O�                JA  ARDU                                                                        20150621190519                      G�O�G�O�G�O�                