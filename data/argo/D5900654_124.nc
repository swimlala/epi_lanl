CDF   3   
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PARAM       N_PROF        N_CALIB       N_LEVELS   s   	N_HISTORY             	   title         Argo float vertical profile    institution       JAMSTEC    source        
Argo float     history       b2008-02-06T04:53:37Z creation;2009-09-01T08:43:31Z update;2015-06-09T21:21:08Z conversion to V3.1;     
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
_FillValue                    iArgo profile    3.1 1.2 19500101000000  5900654 J-ARGO                                                          JAMSTEC                                                         PRES            TEMP            PSAL               |A   JA  20080206045337  20150621190524  A5_23712_124                    2C  D   APEX                            1566                            013004                          846 @Թ�0*�1   @Թ-RL�@2J��n��c&��"��1   ARGOS   A   A   A   Primary sampling: discrete [spot sampling CTD data]                                                                                                                                                                                                                @�  A��Ac33A�ffA���A�33B	33B33B133BE33BY��Bm33B�33B���B���B�33B�ffB�33B�ffB���BЙ�Bڙ�B�ffB�  B�ffC  C�C��CL�CffC��CffC$L�C)�C.�C3  C8�C=ffCB�CGffCQ�3C[L�Ce�Co  Cy� C�� C��fC���C��fC��fC���C��fC�� C���C���C�� C�� C�� C¦fCǦfC̙�Cљ�C֦fCی�C���C�3C�fC�ffC��C�� D��D��D�3D��D�3D� DٚD$ٚD)� D.ٚD3� D8�3D=�fDB� DG� DL��DQ��DV� D[��D`��De� DjٚDo� DtٚDy�3D�33D�c3D�� D�ٚD�&fD�ffD���D�� D�&fD�c3D��3D��D��D�p Dڜ�D��3D�  D�VfD�D�  1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @�33AffA\��A�33A�ffA�  B��B��B/��BC��BX  Bk��B~��B���B���B�ffB���B�ffB���B�  B���B���B㙚B�33B���C ��C�3C33C�fC  C33C  C#�fC(�3C-�3C2��C7�3C=  CA�3CG  CQL�CZ�fCd�3Cn��Cy�C���C�s3C�Y�C�s3C�s3C�Y�C�s3C���C�Y�C�Y�C���C���C���C�s3C�s3C�ffC�ffC�s3C�Y�C�Y�C� C�s3C�33C�Y�C���D�3D�3D��D�3D��D�fD� D$� D)�fD.� D3�fD8��D=��DB�fDG�fDL�3DQ�3DV�fD[�3D`�3De�fDj� Do�fDt� Dy��D�&fD�VfD��3D���D��D�Y�D���D��3D��D�VfD��fD���D� D�c3Dڐ D��fD�3D�I�D��D�31111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A� �A��A��A��A�"�A�(�A��A��A�bA�VA�VA�O�A��TA�E�A��/A��
A���A��jA��9A���A�G�A��mA�%A��`A�G�A���A�I�A��`A���A�{A���A�\)A���A�33A��A���A�$�A�r�A���A���A�M�Ao�Ac��AYƨAKl�AB��A<�A5��A+�wA&ȴA!
=A;dA&�AO�A�At�A�DA�AXA9X@�dZ@��!@�33@��@땁@� �@��H@��@��@��@ؼj@���@�"�@͉7@��@��@�C�@�(�@���@��@���@�(�@���@���@��m@��\@�A�@��@��@�I�@���@��@�A�@���@�ff@�ƨ@~E�@tZ@lI�@c33@\Z@T�j@L�D@F{@=��@6E�@.5?@&��@!x�@O�@��@+@(�@��@
�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A� �A��A��A��A�"�A�(�A��A��A�bA�VA�VA�O�A��TA�E�A��/A��
A���A��jA��9A���A�G�A��mA�%A��`A�G�A���A�I�A��`A���A�{A���A�\)A���A�33A��A���A�$�A�r�A���A���A�M�Ao�Ac��AYƨAKl�AB��A<�A5��A+�wA&ȴA!
=A;dA&�AO�A�At�A�DA�AXA9X@�dZ@��!@�33@��@땁@� �@��H@��@��@��@ؼj@���@�"�@͉7@��@��@�C�@�(�@���@��@���@�(�@���@���@��m@��\@�A�@��@��@�I�@���@��@�A�@���@�ff@�ƨ@~E�@tZ@lI�@c33@\Z@T�j@L�D@F{@=��@6E�@.5?@&��@!x�@O�@��@+@(�@��@
�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB
-B
.B
.B
/B
.B
.B
/B
0!B
2-B
49B
8RB
YB�VB��B�B��B��BJB\B�B&�B-B`BBe`Bn�B�1B�DB�JB��B�oBhsB=qB�B��B�TB�oBhsB2-B�B
�RB
(�B	e`B	\BȴBu�BcTBffBaHB�+B��B�!B��BƨB�-BŢB�B	  B	1B	VB�B�B�B	+B	B	PB	�B	.B	G�B	C�B	W
B	^5B	s�B	~�B	�JB	�hB	|�B	�=B	��B	��B	��B	��B	�9B	�FB	�qB	ŢB	�B	�/B	�B	�B	��B	��B
B

=B
VB
uB
�B
%�B
-B
1'B
6FB
;dB
B�B
I�B
N�B
VB
\)B
aHB
gmB
k�B
p�B
s�B
v�B
x�B
}�B
�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B
-B
.B
.B
/B
.B
.B
/B
0!B
2-B
49B
8RB
XB�PB��B�B��B��BJB\B�B'�B.BbNBhsBt�B�7B�JB�\B��B��BjB@�B�B��B�mB��BjB49B#�B
�jB
/B	hsB	oB��Bw�Be`BhsBdZB�1B��B�'B��BȴB�3BŢB�B	  B		7B	bB�#B�#B�B	1B	B	VB	�B	.B	H�B	C�B	W
B	^5B	s�B	~�B	�JB	�oB	|�B	�=B	��B	��B	��B	��B	�9B	�FB	�qB	ŢB	�B	�/B	�B	�B	��B	��B
B

=B
VB
uB
�B
%�B
-B
1'B
6FB
;dB
B�B
I�B
N�B
VB
\)B
aHB
gmB
k�B
p�B
s�B
v�B
x�B
}�B
�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ = PRES-SP(NextCycle), where SP is SURFACE PRESSURE from next cycle                                                                                                                                                                                     TEMP_ADJUSTED = TEMP                                                                                                                                                                                                                                            PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,PRES_ADJ,elapsed_time,alpha,tau),elapsed_time=P/mean_rise_rate,P=dbar since the start of the profile for each samples                                                                                                       None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            SP(NextCycle)=0.4(dbar)                                                                                                                                                                                                                                         None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE; PRES_ADJ_ERR : Manufacture sensor accuracy                                                                                                                                                                 TEMP_ADJ_ERR : SBE sensor accuracy                                                                                                                                                                                                                              Salinity Recalculation using PRES_ADJ; PSAL_ADJ_ERR : max(sum of RecalS & CTM errors, 0.01(PSS-78))                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         200802190755242008021907552420080219075524200802190801562008021908015620080219080156200908260000002009082600000020090826000000  JA  ARFMdecpA5_a                                                                20080206045332  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.4                                                                 20080206045337  IP                  G�O�G�O�G�O�                JA  ARCArsal2.1                                                                 20080206045337  IP  PSAL            G�O�G�O�G�O�                JA  ARGQrqcppo_a                                                                20080206045337  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19b                                                                20080206045341  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19b                                                                20080206045341  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8a                                                                20080206045342  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8a                                                                20080206045342  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16b                                                                20080206045342  QCP$                G�O�G�O�G�O�           10000JA  ARUP                                                                        20080206051042                      G�O�G�O�G�O�                JA  ARFMdecpA5_a                                                                20080210041807  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.4                                                                 20080210041825  IP                  G�O�G�O�G�O�                JA  ARCArsal2.1                                                                 20080210041827  IP  PSAL            G�O�G�O�G�O�                JA  ARGQrqcppo_a                                                                20080210041829  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19b                                                                20080210041837  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19b                                                                20080210041837  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8a                                                                20080210041838  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8a                                                                20080210041838  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16b                                                                20080210041839  QCP$                G�O�G�O�G�O�           10000JA  ARUP                                                                        20080210044523                      G�O�G�O�G�O�                JA  ARCArsal2.1                                                                 20080912002012  IP  PSAL            G�O�G�O�G�O�                JA  ARGQrqcppo_a                                                                20080912002012  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19b                                                                20080912002016  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19b                                                                20080912002016  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8a                                                                20080912002017  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8a                                                                20080912002017  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16b                                                                20080912002017  QCP$                G�O�G�O�G�O�           10000JA  ARUP                                                                        20080912015258                      G�O�G�O�G�O�                JA  ARFMdecpA5_a                                                                20080210041807  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.5                                                                 20090414042818  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.0                                                                 20090414042818  IP  PRES            G�O�G�O�G�O�                JA  ARCArsal2.1a                                                                20090414042818  IP  PSAL            G�O�G�O�G�O�                JA  ARGQrqcppo_a                                                                20090414042819  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19b                                                                20090414042820  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19b                                                                20090414042820  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8b                                                                20090414042820  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8b                                                                20090414042820  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16b                                                                20090414042820  QCP$                G�O�G�O�G�O�           10000JA  ARUP                                                                        20090414042944                      G�O�G�O�G�O�                JM  ARGQJMQC1.0                                                                 20080209172222  CV  DAT$            G�O�G�O�F��                JM  ARGQJMQC1.0                                                                 20080209172222  CV  DAT$            G�O�G�O�F��                JM  ARGQJMQC1.0                                                                 20080209172222  CV  LAT$            G�O�G�O�A�(�                JM  ARGQJMQC1.0                                                                 20080209172222  CV  LON$            G�O�G�O��?�                JM  ARCAJMQC1.0                                                                 20080219075524  CV  PRES            G�O�G�O�G�O�                JM  ARCAJMQC1.0                                                                 20080219075524  IP  PSAL            G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20080219080156  CV  PSAL            G�O�G�O�G�O�                JM  ARSQOW  1.1 SeHyD1.0                                                        20090826000000  IP  PSAL            G�O�G�O�G�O�                JA  RFMTcnvd2.1                                                                 20090901084251  IP                  G�O�G�O�G�O�                JA  ARDU                                                                        20090901084331                      G�O�G�O�G�O�                JM      JMFC1.0                                                                 20150609212103                      G�O�G�O�G�O�                JA  ARDU                                                                        20150621190524                      G�O�G�O�G�O�                