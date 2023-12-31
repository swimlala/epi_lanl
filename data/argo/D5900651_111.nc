CDF   $   
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PARAM       N_PROF        N_CALIB       N_LEVELS   s   	N_HISTORY             	   title         Argo float vertical profile    institution       JAMSTEC    source        
Argo float     history       b2007-09-28T00:52:34Z creation;2009-03-18T07:30:00Z update;2015-06-09T20:30:05Z conversion to V3.1;     
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
_FillValue                    iArgo profile    3.1 1.2 19500101000000  5900651 J-ARGO                                                          JAMSTEC                                                         PRES            TEMP            PSAL               oA   JA  20070928005234  20150621180534  A5_23694_111                    2C  D   APEX                            1561                            013004                          846 @Ԙ+�dw1   @Ԙ�{By@5O�;dZ�b�x���1   ARGOS   A   A   A   Primary sampling: discrete [spot sampling CTD data]                                                                                                                                                                                                                @�  A  Ad��A�  A�33A�33B  B��B2  BE��BY��Bn  B�33B���B�33B���B���B���B�ffB�33B�  Bڙ�B�  B�ffB���C� CffC�CL�C33CL�C� C$  C)�C.  C3ffC8  C=33CB� CG� CQ� C[ffCe� Co�CyffC���C��3C���C���C��fC��fC�s3C���C�s3C���C��fC��3C���C¦fC���C̦fCь�C֌�Cۀ C�� C噚C��C��C��3C��3D� D� D��D��DٚD�3D��D$�3D)�3D.�fD3ٚD8�3D=�3DB��DGٚDL�fDQ�3DV� D[��D`��De��Dj�fDo�3Dt��Dy�3D�  D�i�D���D�� D�#3D�i�D�� D���D�#3D�` D���D��3D��D�l�Dڣ3D��3D�#3D�ffD�D�ɚ1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @���A��Aa��A�ffA���A噚B33B  B133BD��BX��Bm33B���B�33B���B�33B�33B�33B�  B���BЙ�B�33B䙚B�  B�ffCL�C33C
�fC�C  C�CL�C#��C(�fC-��C333C7��C=  CBL�CGL�CQL�C[33CeL�Cn�fCy33C�� C���C��3C��3C���C���C�Y�C�s3C�Y�C�� C���C���C�s3C�Cǳ3Č�C�s3C�s3C�ffC�fC� C�s3C�s3C���C���D�3D�3D� D� D��D�fD� D$�fD)�fD.��D3��D8�fD=�fDB� DG��DL��DQ�fDV�3D[� D`� De� Dj��Do�fDt��Dy�fD��D�c3D��3D�ٚD��D�c3D���D��fD��D�Y�D��3D���D�3D�ffDڜ�D���D��D�` D�3D��31111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�A���A���A���Aκ^A�~�A�VA�M�A�K�A�S�AΝ�A��;A��A��TA�ƨA���A�C�A�/A���A�(�A�r�A� �A�r�A�&�A��FA��+A��`A�9XA�~�A���A��uA�?}A��/A�VA��yA�M�A�ƨA���A�A��A�jA�ffAzZAmO�A]�ATA�AI��A?�FA8bNA3C�A-t�A)��A&5?A"��Az�A�hA�TA��A\)A�A�`AK�@��/@�
=@�P@�|�@�ȴ@�&�@�ȴ@ו�@�z�@�A�@Ȭ@î@�o@��H@���@�@�n�@�`B@�p�@�(�@�b@��@��^@��@��@���@��@���@�j@�ff@�1@��@�l�@��@~ff@t�@l��@eV@Z~�@R^5@KS�@D��@<j@8b@2�@+�F@%�T@!%@�m@  @�@1'@��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�A���A���A���Aκ^A�~�A�VA�M�A�K�A�S�AΝ�A��;A��A��TA�ƨA���A�C�A�/A���A�(�A�r�A� �A�r�A�&�A��FA��+A��`A�9XA�~�A���A��uA�?}A��/A�VA��yA�M�A�ƨA���A�A��A�jA�ffAzZAmO�A]�ATA�AI��A?�FA8bNA3C�A-t�A)��A&5?A"��Az�A�hA�TA��A\)A�A�`AK�@��/@�
=@�P@�|�@�ȴ@�&�@�ȴ@ו�@�z�@�A�@Ȭ@î@�o@��H@���@�@�n�@�`B@�p�@�(�@�b@��@��^@��@��@���@��@���@�j@�ff@�1@��@�l�@��@~ff@t�@l��@eV@Z~�@R^5@KS�@D��@<j@8b@2�@+�F@%�T@!%@�m@  @�@1'@��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB|�B|�B|�B|�B|�B~�B�B�B�B�7B�jB�B�%B��B�LB�}B��B�ZB�B��B�B�B�ZB�B��B�7B�BJ�BW
B2-B1'Br�BjBJ�BC�B(�B�mB�BbB
�B
bNB
B	��B	bNB��B�wB��By�Bz�B}�B}�By�By�B}�B�Bz�Bq�Bm�Be`BdZB^5BZB[#B`BBhsBjBr�Bs�Bt�B�B��B��B��B�B	+B	!�B	33B	C�B	T�B	t�B	�DB	��B	�B	�RB	��B	��B	�B	�5B	�`B	�B	�B	��B	��B
B
%B
hB
�B
#�B
+B
2-B
;dB
B�B
H�B
M�B
T�B
YB
^5B
cTB
hsB
m�B
q�B
u�B
y�B
}�B
�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 Bv�Bv�Bv�Bv�Bv�Bx�Bz�B{�B|�B�B�FB�B�B��B�3B�dBǮB�;B�ZB�B�B�mB�BB��B��B�B~�BE�BQ�B-B+Bl�Be`BE�B>wB$�B�ZB�BJB
�B
_;B
B	�wB	`BB��B�dB��Bu�Bv�By�Bx�Bt�Bt�Bx�B~�Bv�Bl�BiyB`BB_;BYBT�BVB[#BcTBe`Bm�Bn�Bo�B{�B�hB��B��B�yB	B	�B	-B	=qB	N�B	n�B	�B	�oB	��B	�-B	ŢB	��B	��B	�B	�;B	�fB	�B	�B	��B	��B
  B
DB
{B
�B
$�B
,B
5?B
<jB
B�B
G�B
N�B
R�B
XB
]/B
bNB
gmB
k�B
o�B
s�B
w�B
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
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - SP, where SP is SURFACE PRESSURE (minus 5 dbar for Apf-6,7,8) from next cycle.                                                                                                                                                           none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = RecalS = PSAL(PRES_ADJUSTED,TEMP,Conductivity)                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = celltm_sbe41(RecalS,TEMP,PRES_ADJUSTED,elapsed_time,alpha,tau),elapsed_time=P/mean_rise_rate,P= dbar since the start of the profile for each samples.                                                                                           none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADUSTED=PSAL(corrected by SSP&CTM)+deltaS, where deltaS is calculated by WJO                                                                                                                                                                               SP(NextCycle) = 0.2 dbar                                                                                                                                                                                                                                        none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            r=0.9998(+-0.0001), deepest deltaS=-0.006(+-0.006)                                                                                                                                                                                                              Pressures adjusted by using reported SURFACE PRESSURE. The quoted error is max [2.4, size of pressure adjustment] in dbar.                                                                                                                                      The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   Salinity Recalculation using PRES_ADJUSTED. PSAL_ADJ_ERR : max(combines 1x WJO uncertainty & CTM adjustment , SBE sensor accuracy)                                                                                                                              none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            WJO(2003) salinity adjustment is adopted with SeHyD1.0; large scale 8/4, small scale 4/2; Use interpolate_float_valuesnov2003.m, map_data_grid.m, map_data_grid_t.m; Use T levels <= 4c; Run Const:18;                                                          200710102002042007101020020420071010200204200710102015592007101020155920071010201559200809300000002008093000000020080930000000  JA  ARFMdecpA5_a                                                                20070928005219  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.4                                                                 20070928005234  IP                  G�O�G�O�G�O�                JA  ARCArsal2.1                                                                 20070928005236  IP  PSAL            G�O�G�O�G�O�                JA  ARGQrqcppo_a                                                                20070928005237  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19b                                                                20070928005245  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19b                                                                20070928005245  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.7c                                                                20070928005245  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.7c                                                                20070928005245  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16b                                                                20070928005246  QCP$                G�O�G�O�G�O�           10000JA  ARUP                                                                        20070928014741                      G�O�G�O�G�O�                JA  ARFMdecpA5_a                                                                20071001154048  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.4                                                                 20071001154053  IP                  G�O�G�O�G�O�                JA  ARCArsal2.1                                                                 20071001154054  IP  PSAL            G�O�G�O�G�O�                JA  ARGQrqcppo_a                                                                20071001154054  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19b                                                                20071001154058  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19b                                                                20071001154058  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.7c                                                                20071001154059  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.7c                                                                20071001154059  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16b                                                                20071001154059  QCP$                G�O�G�O�G�O�           10000JA  ARGQrelo2.1                                                                 20071001154059  CV  TIME            G�O�G�O�F���                JA  ARGQrelo2.1                                                                 20071001154059  CV  LAT$            G�O�G�O�A�j                JA  ARGQrelo2.1                                                                 20071001154059  CV  LON$            G�O�G�O���                JA  ARUP                                                                        20071001160926                      G�O�G�O�G�O�                JM  ARGQJMQC1.0                                                                 20071003052818  CV  DAT$            G�O�G�O�F���                JM  ARGQJMQC1.0                                                                 20071003052818  CV  LAT$            G�O�G�O�A�l�                JM  ARCAJMQC1.0                                                                 20071010200204  CV  PRES            G�O�G�O�G�O�                JM  ARCAJMQC1.0                                                                 20071010200204  IP  PSAL            G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20071010201559  CV  PSAL            G�O�G�O�G�O�                JM  ARSQWJO 2.0 SeHyD1.0                                                        20080930000000  CV  PSAL            G�O�G�O�G�O�                JA  RFMTcnvd2.1                                                                 20081008001608  IP                  G�O�G�O�G�O�                JA  ARDU                                                                        20081008025428                      G�O�G�O�G�O�                JM  AREVjmrvp1.2                                                                20090312120708  IP  PRES            G�O�G�O�G�O�                JA  RFMTcnvd2.1                                                                 20090318072441  IP                  G�O�G�O�G�O�                JA  ARDU                                                                        20090318073000                      G�O�G�O�G�O�                JM      JMFC1.0                                                                 20150609202958                      G�O�G�O�G�O�                JA  ARDU                                                                        20150621180534                      G�O�G�O�G�O�                