CDF   !   
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PARAM       N_PROF        N_CALIB       N_LEVELS   s   	N_HISTORY             	   title         Argo float vertical profile    institution       JAMSTEC    source        
Argo float     history       b2007-08-28T12:49:20Z creation;2009-03-18T07:31:49Z update;2015-06-09T20:29:30Z conversion to V3.1;     
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
_FillValue                    iArgo profile    3.1 1.2 19500101000000  5900651 J-ARGO                                                          JAMSTEC                                                         PRES            TEMP            PSAL               lA   JA  20070828124920  20150621180540  A5_23694_108                    2C  D   APEX                            1561                            013004                          846 @Ԑ��韰1   @Ԑ�>� c@5�I�^5?�b��
=p�1   ARGOS   A   A   A   Primary sampling: discrete [spot sampling CTD data]                                                                                                                                                                                                                @�33A  Ai��A���A�  A�ffBffB33B0  BC��BW��Bl  B�33B���B�  B�  B�ffB�33B���B�33B�33B�ffB���B�  B�  C�C33C� C� CL�C� C33C$� C)��C.33C3� C8�C=� CBL�CG� CQ�C[33Ce� CoffCyL�C���C��3C�� C���C���C��3C�� C�� C���C��fC��3C�� C�� C�� C�s3C̀ C�� C�� Cۙ�C���C��C�3C�3C��C���D��D��D�3DٚD�fD��D�fD$ٚD)��D.�fD3�3D8�fD=��DB��DG�fDL��DQ��DV� D[� D`�fDe�fDj� Do�3Dt�3Dy��D��D�p D�� D��D�,�D�ffD�� D��fD�,�D�p D��fD���D�&fD�c3DڦfD���D��D�i�D�D�I�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @���A33Ad��A�ffA���A�  B33B  B.��BBffBVffBj��B33B�  B�ffB�ffB���B���B�  Bƙ�BЙ�B���B�33B�ffB�ffC ��C�fC33C33C  C33C�fC$33C)L�C-�fC333C7��C=33CB  CG33CP��CZ�fCe33Co�Cy  C�s3C���C���C�ffC�ffC���C���C���C�ffC�� C���C�Y�C���C�C�L�C�Y�Cљ�C֙�C�s3C�ffC�ffC��C��C�ffC�ffD��D��D� D�fD�3D�fD�3D$�fD)��D.�3D3� D8�3D=��DB��DG�3DL��DQ��DV��D[��D`�3De�3Dj��Do� Dt� Dy��D�3D�ffD��fD�� D�#3D�\�D��fD���D�#3D�ffD���D��3D��D�Y�Dڜ�D��3D� D�` D� D�@ 1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�|�Aϗ�A�x�A��mA���A͗�A�VA̟�A̙�A̛�A�G�A�1A���A��A��A��Aʰ!A��A���A�A�O�A��jA���A��A�`BA��^A�v�A��A���A���A�=qA�1'A�M�A�/A�hsA��mA��jA��A�9XA�ȴA��A� �A�
=Ay��An�`Ad�AZ$�ALz�AE��A>��A7�#A/��A*��A&�A"�`A�/A��AdZAC�A|�A�Ap�@��@���@��@�P@ޟ�@�;d@�bN@љ�@�ȴ@�%@�p�@���@�K�@��u@�^5@�C�@��`@�?}@��7@��^@�"�@���@��@�9X@��+@�bN@�J@�t�@�p�@�33@���@���@�V@�dZ@~$�@v�R@m�T@fv�@_�@Z�@R�H@I��@CS�@=O�@65?@/�P@*=q@$I�@|�@z�@��@��@1'1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�|�Aϗ�A�x�A��mA���A͗�A�VA̟�A̙�A̛�A�G�A�1A���A��A��A��Aʰ!A��A���A�A�O�A��jA���A��A�`BA��^A�v�A��A���A���A�=qA�1'A�M�A�/A�hsA��mA��jA��A�9XA�ȴA��A� �A�
=Ay��An�`Ad�AZ$�ALz�AE��A>��A7�#A/��A*��A&�A"�`A�/A��AdZAC�A|�A�Ap�@��@���@��@�P@ޟ�@�;d@�bN@љ�@�ȴ@�%@�p�@���@�K�@��u@�^5@�C�@��`@�?}@��7@��^@�"�@���@��@�9X@��+@�bN@�J@�t�@�p�@�33@���@���@�V@�dZ@~$�@v�R@m�T@fv�@_�@Z�@R�H@I��@CS�@=O�@65?@/�P@*=q@$I�@|�@z�@��@��@1'1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB
=B	7B	7B
=B�B-B(�BoBuB�BVB%B%BDB\B�B\B�BiyB�FBÖB�yB!�Bk�Bs�B\)B�B�uB�B�VB��B��Bk�BP�B�B�B��B��Bz�BR�B
�B
�wB
l�B	��B	E�B��B�dB�Bp�Bm�Bn�Bl�BhsBl�Bl�Bv�B�1B�1B�Bz�BbNBXBG�BF�B8RB2-B'�B$�B-B<jB�B�uB�3B�;B	%B	'�B	E�B	[#B	iyB	}�B	�PB	��B	�FB	�9B	�}B	ȴB	��B	�#B	�;B	�yB	�B	��B	��B	��B
B
\B
�B
"�B
(�B
0!B
6FB
<jB
B�B
H�B
O�B
S�B
ZB
`BB
e`B
jB
o�B
r�B
w�B
z�B
~�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 BBBBB�B&�B#�BJBPB{B1B  B  BB	7B�BbB�Be`B�'B�}B�`B�BgmBo�BW
B��B�\B� B�7B��B�{BgmBM�B{B�yB�qB��Bw�BO�B
�B
�dB
k�B	��B	B�B��B�XB}�Bl�BiyBk�BhsBdZBhsBhsBr�B�B�B~�Bv�B^5BS�BB�BB�B49B-B#�B�B'�B7LB}�B�VB�B�B	B	"�B	@�B	VB	dZB	x�B	�1B	��B	�'B	�B	�^B	ÖB	��B	�B	�B	�ZB	�sB	��B	��B	��B	��B

=B
�B
�B
#�B
)�B
0!B
7LB
=qB
B�B
I�B
N�B
S�B
ZB
_;B
dZB
iyB
l�B
q�B
t�B
x�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - SP, where SP is SURFACE PRESSURE (minus 5 dbar for Apf-6,7,8) from next cycle.                                                                                                                                                           none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = RecalS = PSAL(PRES_ADJUSTED,TEMP,Conductivity)                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = celltm_sbe41(RecalS,TEMP,PRES_ADJUSTED,elapsed_time,alpha,tau),elapsed_time=P/mean_rise_rate,P= dbar since the start of the profile for each samples.                                                                                           none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADUSTED=PSAL(corrected by SSP&CTM)+deltaS, where deltaS is calculated by WJO                                                                                                                                                                               SP(NextCycle) = 0.3 dbar                                                                                                                                                                                                                                        none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            r=0.9999(+-0.0001), deepest deltaS=-0.006(+-0.005)                                                                                                                                                                                                              Pressures adjusted by using reported SURFACE PRESSURE. The quoted error is max [2.4, size of pressure adjustment] in dbar.                                                                                                                                      The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   Salinity Recalculation using PRES_ADJUSTED. PSAL_ADJ_ERR : max(combines 1x WJO uncertainty & CTM adjustment , SBE sensor accuracy)                                                                                                                              none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            WJO(2003) salinity adjustment is adopted with SeHyD1.0; large scale 8/4, small scale 4/2; Use interpolate_float_valuesnov2003.m, map_data_grid.m, map_data_grid_t.m; Use T levels <= 4c; Run Const:18;                                                          200709102140442007091021404420070910214044200709102150522007091021505220070910215052200809300000002008093000000020080930000000  JA  ARFMdecpA5_a                                                                20070828124918  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.4                                                                 20070828124920  IP                  G�O�G�O�G�O�                JA  ARCArsal2.1                                                                 20070828124920  IP  PSAL            G�O�G�O�G�O�                JA  ARGQrqcppo_a                                                                20070828124921  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19b                                                                20070828124925  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19b                                                                20070828124925  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.7c                                                                20070828124925  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.7c                                                                20070828124925  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16a                                                                20070828124925  QCP$                G�O�G�O�G�O�           10000JA  ARGQaqcpt16a                                                                20070828124925  QCP$                G�O�G�O�G�O�           10000JA  ARUP                                                                        20070828130119                      G�O�G�O�G�O�                JA  ARFMdecpA5_a                                                                20070901155002  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.4                                                                 20070901155016  IP                  G�O�G�O�G�O�                JA  ARCArsal2.1                                                                 20070901155019  IP  PSAL            G�O�G�O�G�O�                JA  ARGQrqcppo_a                                                                20070901155020  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19b                                                                20070901155030  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19b                                                                20070901155030  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.7c                                                                20070901155031  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.7c                                                                20070901155031  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16a                                                                20070901155032  QCP$                G�O�G�O�G�O�           10000JA  ARGQaqcpt16a                                                                20070901155032  QCP$                G�O�G�O�G�O�           10000JA  ARUP                                                                        20070901164538                      G�O�G�O�G�O�                JM  ARCAJMQC1.0                                                                 20070910214044  CV  PRES            G�O�G�O�G�O�                JM  ARCAJMQC1.0                                                                 20070910214044  IP  PSAL            G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20070910215052  CV  PSAL            G�O�G�O�G�O�                JM  ARSQWJO 2.0 SeHyD1.0                                                        20080930000000  CV  PSAL            G�O�G�O�G�O�                JA  RFMTcnvd2.1                                                                 20081008001611  IP                  G�O�G�O�G�O�                JA  ARDU                                                                        20081008025719                      G�O�G�O�G�O�                JM  AREVjmrvp1.2                                                                20090312120708  IP  PRES            G�O�G�O�G�O�                JA  RFMTcnvd2.1                                                                 20090318072701  IP                  G�O�G�O�G�O�                JA  ARDU                                                                        20090318073149                      G�O�G�O�G�O�                JM      JMFC1.0                                                                 20150609202924                      G�O�G�O�G�O�                JA  ARDU                                                                        20150621180540                      G�O�G�O�G�O�                