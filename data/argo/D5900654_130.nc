CDF   0   
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PARAM       N_PROF        N_CALIB       N_LEVELS   s   	N_HISTORY             	   title         Argo float vertical profile    institution       JAMSTEC    source        
Argo float     history       b2008-04-06T06:56:45Z creation;2009-09-01T08:43:28Z update;2015-06-09T21:22:15Z conversion to V3.1;     
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
_FillValue                    iArgo profile    3.1 1.2 19500101000000  5900654 J-ARGO                                                          JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA  20080406065645  20150621190525  A5_23712_130                    2C  D   APEX                            1566                            013004                          846 @���֩1   @��d�@2n��O�;�ch�9Xb1   ARGOS   A   A   A   Primary sampling: discrete [spot sampling CTD data]                                                                                                                                                                                                                @���A��Ac33A�33A�ffA���B  B��B/33BE��BY33Bl��B�  B�33B�33B�ffB���B�  B�  B���B���B�33B�  BB���CffC��CffC��C� CffC�C$ffC)� C.� C3��C8ffC=� CBL�CGL�CQL�C[�Ce�CoL�Cy� C��3C�� C�� C�s3C��3C��fC���C�� C��fC���C���C���C���C�� Cǀ C̙�C�� C֌�C�s3C�� C噚C� C� C��3C��3DٚD� D�3D�fD��D�3D� D$� D)ٚD.� D3� D8��D=��DB� DGٚDL�3DQ� DV�fD[��D`�3De��Dj� Do�fDt�fDy��D�&fD�i�D���D��fD�0 D�i�D���D��3D��D�l�D���D��D�  D�c3Dڠ D��fD�)�D�i�D�3D���1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @���A	��A[33A�33A�ffA���B  B��B-33BC��BW33Bj��B�  B�33B�33B�ffB���B�  B�  B���B���B�33B�  B홚B���C �fC�C
�fCL�C  C�fC��C#�fC)  C.  C3�C7�fC=  CA��CF��CP��CZ��Cd��Cn��Cy  C�s3C�� C�@ C�33C�s3C�ffC�L�C�� C�ffC�Y�C�L�C�L�C�L�C C�@ C�Y�Cр C�L�C�33C�@ C�Y�C�@ C�@ C�s3C�s3D��D� D�3D�fD��D�3D� D$� D)��D.� D3� D8��D=��DB� DG��DL�3DQ� DV�fD[��D`�3De��Dj� Do�fDt�fDy��D�fD�Y�D���D��fD�  D�Y�D���D��3D�	�D�\�D���D�ٚD� D�S3Dڐ D��fD��D�Y�D�3D���1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�
=A�A��#A�v�A�;dA�(�A�"�A��A�{A�{A�bA�bA�
=A�%A�A���A��yA���A�`BA��\A���A��DA�VA��A��wA�ZA��A���A�A�I�A���A�1A�I�A��A���A�E�A���A|�+Aw�Ao�A`ȴAWp�ALJA;+A8  A/O�A)�A$�A�DA��A�+A$�A�A
Q�A��AĜAv�An�AS�A =q@�`B@�\)@�S�@���@�{@���@���@�I�@���@�
=@ҏ\@�1@��m@�dZ@�%@�r�@�  @�`B@��@�~�@��!@�"�@�S�@�ȴ@�~�@�?}@�M�@�Z@�
=@�7L@��@�@��@�C�@�`B@���@�r�@tZ@l�D@e��@^{@V$�@Nff@D�@<1@4Z@0��@.@)�^@#��@E�@r�@�
@Q�@��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�
=A�A��#A�v�A�;dA�(�A�"�A��A�{A�{A�bA�bA�
=A�%A�A���A��yA���A�`BA��\A���A��DA�VA��A��wA�ZA��A���A�A�I�A���A�1A�I�A��A���A�E�A���A|�+Aw�Ao�A`ȴAWp�ALJA;+A8  A/O�A)�A$�A�DA��A�+A$�A�A
Q�A��AĜAv�An�AS�A =q@�`B@�\)@�S�@���@�{@���@���@�I�@���@�
=@ҏ\@�1@��m@�dZ@�%@�r�@�  @�`B@��@�~�@��!@�"�@�S�@�ȴ@�~�@�?}@�M�@�Z@�
=@�7L@��@�@��@�C�@�`B@���@�r�@tZ@l�D@e��@^{@V$�@Nff@D�@<1@4Z@0��@.@)�^@#��@E�@r�@�
@Q�@��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�5B
�?BXB�LBB�B��B
=B� B
�B
A�B	��B	�HB	��B	��B	�B	�NB	�;B	ŢB	�B	[#B	.B�yBÖB�9B��B��B��B�oB�+B{�B��B��B�qB�-BĜB��B�`B	hB	�B	(�B	49B	bB	�B��B	DB	\B	�B	�B	+B	9XB	D�B	YB	gmB	|�B	�=B	�JB	��B	��B	�'B	�dB	�}B	��B	��B	�/B	�ZB	�B	�B	�B	��B	��B	��B	��B	��B
B
JB
VB
�B
 �B
$�B
)�B
1'B
6FB
<jB
C�B
J�B
S�B
\)B
^5B
aHB
e`B
iyB
n�B
t�B
y�B
|�B
}�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�;B
�?BXB�LBĜB�B��BVB�7B
�B
J�B
  B	�ZB	��B	ĜB	�B	�fB	�BB	ɺB	�+B	_;B	2-B�BƨB�RB��B��B��B�uB�7B}�B��B��B�}B�3BĜB��B�`B	oB	�B	(�B	5?B	bB	�B��B	DB	\B	�B	�B	+B	9XB	D�B	YB	gmB	|�B	�=B	�JB	��B	��B	�'B	�dB	�}B	��B	��B	�/B	�ZB	�B	�B	�B	��B	��B	��B	��B	��B
B
JB
VB
�B
 �B
$�B
)�B
1'B
6FB
<jB
C�B
J�B
S�B
\)B
^5B
aHB
e`B
iyB
n�B
t�B
y�B
|�B
}�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ = PRES-SP(NextCycle), where SP is SURFACE PRESSURE from next cycle                                                                                                                                                                                     TEMP_ADJUSTED = TEMP                                                                                                                                                                                                                                            PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,PRES_ADJ,elapsed_time,alpha,tau),elapsed_time=P/mean_rise_rate,P=dbar since the start of the profile for each samples                                                                                                       None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            SP(NextCycle)=0.5(dbar)                                                                                                                                                                                                                                         None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE; PRES_ADJ_ERR : Manufacture sensor accuracy                                                                                                                                                                 TEMP_ADJ_ERR : SBE sensor accuracy                                                                                                                                                                                                                              Salinity Recalculation using PRES_ADJ; PSAL_ADJ_ERR : max(sum of RecalS & CTM errors, 0.01(PSS-78))                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         200804191429352008041914293520080419142935200804191442162008041914421620080419144216200908260000002009082600000020090826000000  JA  ARFMdecpA5_a                                                                20080406065633  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.4                                                                 20080406065645  IP                  G�O�G�O�G�O�                JA  ARCArsal2.1                                                                 20080406065647  IP  PSAL            G�O�G�O�G�O�                JA  ARGQrqcppo_a                                                                20080406065648  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19b                                                                20080406065655  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19b                                                                20080406065655  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8a                                                                20080406065656  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8a                                                                20080406065656  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16b                                                                20080406065657  QCP$                G�O�G�O�G�O�           10000JA  ARUP                                                                        20080406075247                      G�O�G�O�G�O�                JA  ARFMdecpA5_a                                                                20080410035041  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.4                                                                 20080410035045  IP                  G�O�G�O�G�O�                JA  ARCArsal2.1                                                                 20080410035046  IP  PSAL            G�O�G�O�G�O�                JA  ARGQrqcppo_a                                                                20080410035046  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19b                                                                20080410035050  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19b                                                                20080410035050  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8a                                                                20080410035050  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8a                                                                20080410035050  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16b                                                                20080410035051  QCP$                G�O�G�O�G�O�           10000JA  ARUP                                                                        20080410050844                      G�O�G�O�G�O�                JA  ARCArsal2.1                                                                 20080912002043  IP  PSAL            G�O�G�O�G�O�                JA  ARGQrqcppo_a                                                                20080912002044  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19b                                                                20080912002048  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19b                                                                20080912002048  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8a                                                                20080912002048  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8a                                                                20080912002048  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16b                                                                20080912002048  QCP$                G�O�G�O�G�O�           10000JA  ARUP                                                                        20080912015532                      G�O�G�O�G�O�                JA  ARFMdecpA5_a                                                                20080410035041  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.5                                                                 20090414042832  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.0                                                                 20090414042832  IP  PRES            G�O�G�O�G�O�                JA  ARCArsal2.1a                                                                20090414042832  IP  PSAL            G�O�G�O�G�O�                JA  ARGQrqcppo_a                                                                20090414042832  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19b                                                                20090414042833  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19b                                                                20090414042833  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8b                                                                20090414042833  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8b                                                                20090414042833  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16b                                                                20090414042833  QCP$                G�O�G�O�G�O�           10000JA  ARUP                                                                        20090414042942                      G�O�G�O�G�O�                JM  ARGQJMQC1.0                                                                 20080409233753  CV  DAT$            G�O�G�O�F�@.                JM  ARCAJMQC1.0                                                                 20080419142935  CV  PRES            G�O�G�O�G�O�                JM  ARCAJMQC1.0                                                                 20080419142935  IP  PSAL            G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20080419144216  CV  PSAL            G�O�G�O�G�O�                JM  ARSQOW  1.1 SeHyD1.0                                                        20090826000000  IP  PSAL            G�O�G�O�G�O�                JA  RFMTcnvd2.1                                                                 20090901084254  IP                  G�O�G�O�G�O�                JA  ARDU                                                                        20090901084328                      G�O�G�O�G�O�                JM      JMFC1.0                                                                 20150609212208                      G�O�G�O�G�O�                JA  ARDU                                                                        20150621190525                      G�O�G�O�G�O�                