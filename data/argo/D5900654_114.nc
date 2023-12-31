CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PARAM       N_PROF        N_CALIB       N_LEVELS   s   	N_HISTORY             	   title         Argo float vertical profile    institution       JAMSTEC    source        
Argo float     history       b2007-10-29T06:50:33Z creation;2009-03-18T07:30:23Z update;2015-06-09T21:19:16Z conversion to V3.1;     
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
_FillValue                    iArgo profile    3.1 1.2 19500101000000  5900654 J-ARGO                                                          JAMSTEC                                                         PRES            TEMP            PSAL               rA   JA  20071029065033  20150621190533  A5_23712_114                    2C  D   APEX                            1566                            013004                          846 @Ԡ�OQ1   @Ԡ���@23t�j~��b�����1   ARGOS   A   A   A   Primary sampling: discrete [spot sampling CTD data]                                                                                                                                                                                                                @�33AffA`  A���A���A�33B	33B��B1��BDffBY��Bn  B�33B���B���B�ffB���B�33B�33Bƙ�BЙ�B�ffB�ffB���B�  CL�C  C� C� C� CL�CffC$33C)� C.ffC3�C7�fC<�fCB�CF�fCQffC[��CeffCo� CyL�C�Y�C�s3C���C���C��3C�� C�� C��3C��fC�� C��3C���C��fC�Cǀ C̙�C�s3Cֳ3C۳3C�� C�fC�3C�3C�� C�� D��D� D�fD��D�fD� D�fD$�3D)�3D.� D3�fD8� D=ٚDB��DG��DL�fDQ��DV�3D[�fD`�3DeٚDj��Do�3Dt�3Dy�3D�0 D�ffD���D���D�)�D�i�D��3D���D�  D�l�D��fD���D�)�D�` DڦfD��3D�&fD�Y�D�D�� 1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @�  A��AVffA���A�  A�ffB��B33B/33BB  BW33Bk��B�  B�ffB�ffB�33B���B�  B�  B�ffB�ffB�33B�33B홚B���C �3CffC
�fC�fC�fC�3C��C#��C(�fC-��C2� C7L�C<L�CA� CFL�CP��C[  Cd��Cn�fCx�3C��C�&fC�L�C�L�C�ffC�s3C�33C�ffC�Y�C�s3C�ffC�� C�Y�C�@ C�33C�L�C�&fC�ffC�ffC�33C�Y�C�ffC�ffC�s3C�s3D�fD��D� D�3D� D��D� D$��D)��D.��D3� D8��D=�3DB�fDG�fDL� DQ�fDV��D[� D`��De�3Dj�3Do��Dt��Dy��D��D�S3D���D�ɚD�fD�VfD�� D�ٚD��D�Y�D��3D�ٚD�fD�L�Dړ3D�� D�3D�FfD�fD�|�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�C�A�C�A�=qA�E�A�C�A�C�A�=qA�?}A�;dA�5?A�/A�(�A��A�JA�33A�ZA�1'A�v�A���A�A��A�$�A�VA���A�I�A�Q�A�~�A���A�E�A��^A�n�A�{A��yA��A�9XA��DA�=qA�\)A���A�A�^5A~��Ai��AX�+AKK�AF(�A@A8VA/XA(��A$ȴA!�AO�A��A�+A��A�FA��A��A	K�A^5A�+AV@�C�@��@�ȴ@ߝ�@�-@�p�@�t�@��@ϕ�@��@�X@��@�1@¸R@���@�S�@�?}@��u@��u@�r�@�/@�5?@��@��@��+@��\@�S�@���@�+@���@�hs@���@���@+@x  @m�T@e/@]�h@T�/@N{@FE�@@1'@7K�@0�9@*M�@$�@V@�@�/@�`@O�@
J1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�C�A�C�A�=qA�E�A�C�A�C�A�=qA�?}A�;dA�5?A�/A�(�A��A�JA�33A�ZA�1'A�v�A���A�A��A�$�A�VA���A�I�A�Q�A�~�A���A�E�A��^A�n�A�{A��yA��A�9XA��DA�=qA�\)A���A�A�^5A~��Ai��AX�+AKK�AF(�A@A8VA/XA(��A$ȴA!�AO�A��A�+A��A�FA��A��A	K�A^5A�+AV@�C�@��@�ȴ@ߝ�@�-@�p�@�t�@��@ϕ�@��@�X@��@�1@¸R@���@�S�@�?}@��u@��u@�r�@�/@�5?@��@��@��+@��\@�S�@���@�+@���@�hs@���@���@+@x  @m�T@e/@]�h@T�/@N{@FE�@@1'@7K�@0�9@*M�@$�@V@�@�/@�`@O�@
J1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB��B��B��B��B��B��B��B��B��B��B��B��B��B��BgmB}�Bw�BaHB�=B��B��B1B�B+Bt�Bn�Bw�B`BB}�B`BB_;BXB[#BB�B"�B�B��B��BA�B
�`B
8RB
B	W
B�5B�?B�B��B�!B�+B�VB�DB��B�!B��B	B	�B	�B	�B	 �B	�B	/B	.B	'�B	49B	�B��B�NB��B	+B	�B	G�B	ffB	�oB	��B	�9B	�LB	�B	�3B	�^B	�wB	ĜB	��B	��B	�B	�;B	�`B	�B	�B	�B	��B	��B
B
%B
1B
\B
�B
�B
#�B
)�B
33B
9XB
?}B
F�B
M�B
S�B
ZB
`BB
e`B
gmB
o�B
s�B
w�B
{�B
~�B
�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B��B��B��B��B��B��B��B��B��B��B��B��B��B��Bk�B~�Bz�BdZB�PB��B�B
=B�B,Bu�Bo�Bx�BaHB~�B`BB`BBYB\)BC�B%�B�BÖB��BF�B
�yB
<jB
	7B	[#B�NB�LB�B��B�-B�7B�\B�JB��B�'B��B	%B	�B	�B	 �B	!�B	�B	0!B	/B	'�B	5?B	�B��B�NB��B	+B	�B	G�B	ffB	�oB	��B	�9B	�LB	�B	�3B	�^B	�wB	ĜB	��B	��B	�B	�;B	�`B	�B	�B	�B	��B	��B
B
%B
1B
\B
�B
�B
#�B
)�B
33B
9XB
?}B
F�B
M�B
S�B
ZB
`BB
e`B
gmB
o�B
s�B
w�B
{�B
~�B
�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - SP, where SP is SURFACE PRESSURE (minus 5 dbar for Apf-6,7,8) from next cycle.                                                                                                                                                           none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = RecalS = PSAL(PRES_ADJUSTED,TEMP,Conductivity)                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = celltm_sbe41(RecalS,TEMP,PRES_ADJUSTED,elapsed_time,alpha,tau),elapsed_time=P/mean_rise_rate,P= dbar since the start of the profile for each samples.                                                                                           none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            SP(NextCycle) = 0.6 dbar                                                                                                                                                                                                                                        none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using reported SURFACE PRESSURE. The quoted error is max [2.4, size of pressure adjustment] in dbar.                                                                                                                                      The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   Salinity Recalculation using PRES_ADJUSTED. PSAL_ADJ_ERR : SBE sensor accuracy & CTM adjustment                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         200711111410452007111114104520071111141045200711111423502007111114235020071111142350200809050000002008090500000020080905000000  JA  ARFMdecpA5_a                                                                20071029065030  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.4                                                                 20071029065033  IP                  G�O�G�O�G�O�                JA  ARCArsal2.1                                                                 20071029065033  IP  PSAL            G�O�G�O�G�O�                JA  ARGQrqcppo_a                                                                20071029065034  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19b                                                                20071029065038  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19b                                                                20071029065038  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.7c                                                                20071029065038  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.7c                                                                20071029065038  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16b                                                                20071029065038  QCP$                G�O�G�O�G�O�           10000JA  ARUP                                                                        20071029070546                      G�O�G�O�G�O�                JA  ARFMdecpA5_a                                                                20071102035104  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.4                                                                 20071102035109  IP                  G�O�G�O�G�O�                JA  ARCArsal2.1                                                                 20071102035109  IP  PSAL            G�O�G�O�G�O�                JA  ARGQrqcppo_a                                                                20071102035110  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19b                                                                20071102035114  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19b                                                                20071102035114  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.7c                                                                20071102035114  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.7c                                                                20071102035114  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16b                                                                20071102035114  QCP$                G�O�G�O�G�O�           10000JA  ARUP                                                                        20071102051028                      G�O�G�O�G�O�                JM  ARCAJMQC1.0                                                                 20071111141045  CV  PRES            G�O�G�O�G�O�                JM  ARCAJMQC1.0                                                                 20071111141045  IP  PSAL            G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20071111142350  CV  PSAL            G�O�G�O�G�O�                JM  ARSQWJO 2.0 SeHyD1.0                                                        20080905000000  IP  PSAL            G�O�G�O�G�O�                JA  RFMTcnvd2.1                                                                 20080911042222  IP                  G�O�G�O�G�O�                JA  ARDU                                                                        20080911054229                      G�O�G�O�G�O�                JM  AREVjmrvp1.2                                                                20090312120833  IP  PRES            G�O�G�O�G�O�                JA  RFMTcnvd2.1                                                                 20090318072509  IP                  G�O�G�O�G�O�                JA  ARDU                                                                        20090318073023                      G�O�G�O�G�O�                JM      JMFC1.0                                                                 20150609211911                      G�O�G�O�G�O�                JA  ARDU                                                                        20150621190533                      G�O�G�O�G�O�                