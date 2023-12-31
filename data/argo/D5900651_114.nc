CDF       
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PARAM       N_PROF        N_CALIB       N_LEVELS   s   	N_HISTORY             	   title         Argo float vertical profile    institution       JAMSTEC    source        
Argo float     history       b2007-10-27T10:49:29Z creation;2009-04-07T03:46:18Z update;2015-06-09T20:30:39Z conversion to V3.1;     
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
_FillValue                    iArgo profile    3.1 1.2 19500101000000  5900651 J-ARGO                                                          JAMSTEC                                                         PRES            TEMP            PSAL               rA   JA  20071027104929  20150621180532  A5_23694_114                    2C  D   APEX                            1561                            013004                          846 @ԟ��@1   @ԟ�1�֧@4�Z�1�b�p��
=1   ARGOS   A   A   A   Primary sampling: discrete [spot sampling CTD data]                                                                                                                                                                                                                @�ffA33AfffA���A�ffA�  B
ffBffB2ffBE��BZ  Bn  B���B���B�ffB�ffB���B���B�ffB�ffBЙ�B���B�33BB���CL�CffCffC33C�C�C  C$ffC)�C.ffC3L�C8L�C=� CBffCF�fCQ� C[� CeL�CoL�CyffC�� C��3C�� C�� C���C���C���C���C���C���C�� C���C���C CǙ�C�� Cѳ3C֙�C۳3C�3C��C�3C�fC��C��fD�3D� D��D� DٚD�3DٚD$��D)��D.��D3��D8��D=� DBٚDG�fDL� DQ��DV��D[��D`�fDe�fDj�3Do� Dt�fDy�fD�&fD�ffD���D�� D�)�D�i�D���D�� D��D�l�D��fD��3D��D�ffDک�D���D�#3D�Y�D��D��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @�33A��Ad��A���A���A�33B
  B  B2  BE33BY��Bm��B�ffB�ffB�33B�33B���B���B�33B�33B�ffBڙ�B�  B�ffB���C33CL�CL�C�C  C  C�fC$L�C)  C.L�C333C833C=ffCBL�CF��CQffC[ffCe33Co33CyL�C��3C��fC��3C��3C�� C�� C���C���C���C���C��3C���C���C�s3Cǌ�C̳3CѦfC֌�CۦfC�fC� C�fCC� C���D��D��D�fDٚD�3D��D�3D$�fD)�fD.�fD3�fD8�fD=��DB�3DG� DL��DQ�3DV�fD[�fD`� De� Dj��Do��Dt� Dy� D�#3D�c3D���D���D�&fD�ffD��fD���D��D�i�D��3D�� D�fD�c3DڦfD��D�  D�VfD�D�f1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��ȂhȀ\A̡�A̩�A̧�A̮A̶FA̸RA̼jA̼jA̶FA�r�A�oA�^5A�{A���A�ƨA��7A��PA� �A�VA�bA�p�A��PA��A���A�5?A�7LA�Q�A�jA��9A���A���A�VA���A�C�A�z�A�33A�=qA��A��A�/A{��Ap��A[VAP�RAI��AC33A>z�A;t�A6��A0�9A+�TA&��A"�A��A��A=qA;dA
�DA�AA�@�O�@��@���@�ȴ@�v�@�/@��@ӕ�@�Z@�`B@��/@�Z@�V@�J@���@�l�@�x�@�S�@��@���@�;d@��@���@��/@���@��@��@�Q�@��+@��9@��@���@��9@��@u?}@nV@f$�@_
=@W;d@P1'@Fff@>E�@8��@2^5@+��@'�w@"^5@�T@�#@�@��@�@
�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 ȂhȀ\A̡�A̩�A̧�A̮A̶FA̸RA̼jA̼jA̶FA�r�A�oA�^5A�{A���A�ƨA��7A��PA� �A�VA�bA�p�A��PA��A���A�5?A�7LA�Q�A�jA��9A���A���A�VA���A�C�A�z�A�33A�=qA��A��A�/A{��Ap��A[VAP�RAI��AC33A>z�A;t�A6��A0�9A+�TA&��A"�A��A��A=qA;dA
�DA�AA�@�O�@��@���@�ȴ@�v�@�/@��@ӕ�@�Z@�`B@��/@�Z@�V@�J@���@�l�@�x�@�S�@��@���@�;d@��@���@��/@���@��@��@�Q�@��+@��9@��@���@��9@��@u?}@nV@f$�@_
=@W;d@P1'@Fff@>E�@8��@2^5@+��@'�w@"^5@�T@�#@�@��@�@
�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB��B��B��B��B��B��BBBB\Br�B�FB�BoB\B�B��B�}B��B��B��B� Bt�Bv�B�B�bB�JB�B�B�=B�%B~�BgmBS�BO�BB�B+B�yB��B�RB`BB
��B	�B	�hB��BǮB��B�VB}�B�VB�7B}�B�B�+B�B� Bz�Bs�BiyB`BBT�BP�BQ�B@�BA�B=qB5?BK�BR�BcTB�JB��B�dB�9B�sB	.B	>wB	YB	n�B	�B	��B	��B	�B	�qB	��B	��B	�B	�BB	�sB	�B	��B	��B
  B
%B
PB
�B
"�B
)�B
1'B
6FB
=qB
D�B
L�B
R�B
XB
^5B
cTB
ffB
k�B
o�B
s�B
x�B
|�B
�B
�%1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B��B��B��B��B��B��B��B��B��B	7Bl�B�!B�ZBJB
=B�B��B�qB��B�{B��Bz�Bn�Br�B|�B�DB�%B|�B}�B�B�By�BbNBN�BJ�B=qB&�B�`B��B�9B^5B
��B	�yB	�bB��BÖB��B�7Bx�B�7B�Bx�B|�B�B� Bz�Bv�Bn�BdZB[#BO�BK�BL�B;dB<jB8RB/BF�BL�B]/B�%B�dB�FB�B�NB	'�B	8RB	R�B	hsB	{�B	�oB	��B	��B	�LB	ŢB	��B	��B	�B	�NB	�B	�B	��B	��B
  B
+B
hB
�B
#�B
+B
0!B
7LB
>wB
F�B
L�B
Q�B
XB
]/B
`BB
e`B
iyB
m�B
r�B
v�B
{�B
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
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - SP, where SP is SURFACE PRESSURE (minus 5 dbar for Apf-6,7,8) from next cycle.                                                                                                                                                           none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = RecalS = PSAL(PRES_ADJUSTED,TEMP,Conductivity)                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = celltm_sbe41(RecalS,TEMP,PRES_ADJUSTED,elapsed_time,alpha,tau),elapsed_time=P/mean_rise_rate,P= dbar since the start of the profile for each samples.                                                                                           none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADUSTED=PSAL(corrected by SSP&CTM)+deltaS, where deltaS is calculated by WJO                                                                                                                                                                               SP(NextCycle) = 0.1 dbar                                                                                                                                                                                                                                        none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            r=0.9998(+-0.0001), deepest deltaS=-0.006(+-0.006)                                                                                                                                                                                                              Pressures adjusted by using reported SURFACE PRESSURE. The quoted error is max [2.4, size of pressure adjustment] in dbar.                                                                                                                                      The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   Salinity Recalculation using PRES_ADJUSTED. PSAL_ADJ_ERR : max(combines 1x WJO uncertainty & CTM adjustment , SBE sensor accuracy)                                                                                                                              none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            WJO(2003) salinity adjustment is adopted with SeHyD1.0; large scale 8/4, small scale 4/2; Use interpolate_float_valuesnov2003.m, map_data_grid.m, map_data_grid_t.m; Use T levels <= 4c; Run Const:18;                                                          200711091942292007110919422920071109194229200711091958292007110919582920071109195829200809300000002008093000000020080930000000  JA  ARFMdecpA5_a                                                                20071027104927  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.4                                                                 20071027104929  IP                  G�O�G�O�G�O�                JA  ARCArsal2.1                                                                 20071027104929  IP  PSAL            G�O�G�O�G�O�                JA  ARGQrqcppo_a                                                                20071027104930  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19b                                                                20071027104934  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19b                                                                20071027104934  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.7c                                                                20071027104934  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.7c                                                                20071027104934  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16b                                                                20071027104934  QCP$                G�O�G�O�G�O�           10000JA  ARUP                                                                        20071027110217                      G�O�G�O�G�O�                JA  ARFMdecpA5_a                                                                20071031155410  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.4                                                                 20071031155419  IP                  G�O�G�O�G�O�                JA  ARCArsal2.1                                                                 20071031155419  IP  PSAL            G�O�G�O�G�O�                JA  ARGQrqcppo_a                                                                20071031155420  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19b                                                                20071031155427  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19b                                                                20071031155427  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.7c                                                                20071031155427  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.7c                                                                20071031155427  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16b                                                                20071031155428  QCP$                G�O�G�O�G�O�           10000JA  ARUP                                                                        20071031191210                      G�O�G�O�G�O�                JM  ARCAJMQC1.0                                                                 20071109194229  CV  PRES            G�O�G�O�G�O�                JM  ARCAJMQC1.0                                                                 20071109194229  IP  PSAL            G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20071109195829  CV  PSAL            G�O�G�O�G�O�                JM  ARSQWJO 2.0 SeHyD1.0                                                        20080930000000  CV  PSAL            G�O�G�O�G�O�                JA  RFMTcnvd2.1                                                                 20081008001608  IP                  G�O�G�O�G�O�                JA  ARDU                                                                        20081008025355                      G�O�G�O�G�O�                JM  AREVjmrvp1.2                                                                20090312120709  IP  PRES            G�O�G�O�G�O�                JA  RFMTcnvd2.1                                                                 20090318072259  IP                  G�O�G�O�G�O�                JA  ARDU                                                                        20090318072829                      G�O�G�O�G�O�                JA  ARDU                                                                        20090407034618                      G�O�G�O�G�O�                JM      JMFC1.0                                                                 20150609203033                      G�O�G�O�G�O�                JA  ARDU                                                                        20150621180532                      G�O�G�O�G�O�                