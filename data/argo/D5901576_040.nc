CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PARAM       N_PROF        N_CALIB       N_LEVELS   t   	N_HISTORY             	   title         Argo float vertical profile    institution       JAMSTEC    source        
Argo float     history       b2009-07-03T09:57:44Z creation;2009-09-01T09:17:41Z update;2015-06-11T11:29:19Z conversion to V3.1;     
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
_FillValue                  t  ;p   PRES_ADJUSTED         	         	   	long_name         )Sea water pressure, equals 0 at sea-level      
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   standard_name         sea_water_pressure       �  ;�   PRES_ADJUSTED_QC      	            	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  t  =�   PRES_ADJUSTED_ERROR       	            	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  >(   TEMP      	         	   	long_name         $Sea temperature in-situ ITS-90 scale   
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o   standard_name         sea_water_temperature        �  ?�   TEMP_QC       	            	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  t  A�   TEMP_ADJUSTED         	         	   	long_name         $Sea temperature in-situ ITS-90 scale   
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o   standard_name         sea_water_temperature        �  B<   TEMP_ADJUSTED_QC      	            	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  t  D   TEMP_ADJUSTED_ERROR       	            	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  D�   PSAL      	         	   	long_name         Practical salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o   standard_name         sea_water_salinity       �  FP   PSAL_QC       	            	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  t  H    PSAL_ADJUSTED         	         	   	long_name         Practical salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o   standard_name         sea_water_salinity       �  H�   PSAL_ADJUSTED_QC      	            	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  t  Jd   PSAL_ADJUSTED_ERROR       	            	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  J�   	PARAMETER         	   
               	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  L�   SCIENTIFIC_CALIB_EQUATION         	   
               	long_name         'Calibration equation for this parameter    
_FillValue                 	   M8   SCIENTIFIC_CALIB_COEFFICIENT      	   
               	long_name         *Calibration coefficients for this equation     
_FillValue                 	   V8   SCIENTIFIC_CALIB_COMMENT      	   
               	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   _8   SCIENTIFIC_CALIB_DATE         	   
                	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  h8   HISTORY_INSTITUTION          	            	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    h�   HISTORY_STEP         	            	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    h�   HISTORY_SOFTWARE         	            	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    h�   HISTORY_SOFTWARE_RELEASE         	            	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    h�   HISTORY_REFERENCE            	            	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  h�   HISTORY_DATE         	             	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    i   HISTORY_ACTION           	            	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    i   HISTORY_PARAMETER            	            	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    i   HISTORY_START_PRES           	         	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         i,   HISTORY_STOP_PRES            	         	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         i0   HISTORY_PREVIOUS_VALUE           	         	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        i4   HISTORY_QCTEST           	            	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    i8Argo profile    3.1 1.2 19500101000000  5901576 J-ARGO                                                          JAMSTEC                                                         PRES            TEMP            PSAL               (A   JA  20090703095744  20150614170510  A9_76264_040                    2C  D   APEX                            3512                            070407                          846 @�9E��v1   @�9H�o�@+U�$�/�d�����1   ARGOS   A   A   A   Primary sampling: discrete [spot sampling CTD data]                                                                                                                                                                                                                @�  A��Ah  A�33A���A�33B
  BffB2  BF  BXffBn  B�33B���B���B�33B���B�33B�ffBƙ�B�  B�  B���B�ffB���CL�CL�C33C�fC�C�C33C$��C)ffC.33C3� C8�C=�3CB��CG� CQL�C[L�Cd�fCo� CyL�C��fC��3C�ffC���C�� C�� C���C���C��fC�� C�� C���C���C�C�� C�� C�� C֦fCی�C�fC�3C�ffC�ffC� C���D�fD�fD� D� D�3D�3D��D$ٚD)ٚD.�3D3�fD8ٚD=�3DB��DG�fDL�3DQ�3DVٚD[��D`��De��Dj� Do��DtٚDy� D�)�D�\�D���D��3D�,�D�ffD���D��D�,�D�c3D���D���D��D�Y�DڦfD��3D�)�D�l�D� D�� D�C311111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@���AffAd��A���A�33A陚B	33B��B133BE33BW��Bm33B��B�ffB�33B���B�ffB���B�  B�33Bϙ�Bٙ�B�ffB�  B�33C�C�C  C�3C�fC�fC  C$ffC)33C.  C3L�C7�fC=� CBffCGL�CQ�C[�Cd�3CoL�Cy�C���C���C�L�C�� C��fC��fC�� C�� C���C�ffC�ffC�s3C�� C CǦfC̦fCѦfC֌�C�s3C���C噚C�L�C�L�C�ffC�� DٚDٚD�3D�3D�fD�fD� D$��D)��D.�fD3��D8��D=�fDB� DG��DL�fDQ�fDV��D[��D`� De��Dj�3Do� Dt��Dy�3D�#3D�VfD��fD���D�&fD�` D��fD��3D�&fD�\�D��fD��fD�3D�S3Dڠ D���D�#3D�ffD�D�ɚD�<�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�A��HAי�A�XA�+Aև+A���Aț�A��;AĲ-A��A�^5A�C�A��A�/A�ƨA�r�A�-A���A��#A��DA�G�A���A��
A���A�M�A�$�A�\)A��A��A�{A��\A�G�A�+A�r�A�JA�r�A�A��A�t�A��^A�ĜA���Aq�hAat�AT��AO
=AH=qA?+A7|�A0bNA,1'A'�hA%�^A!��AA��AdZA�A\)A�-A��A
{AI�A�`Av�A\)A�;@���@�bN@��@��@蛦@ߥ�@ۮ@���@���@��T@�@�;d@�o@��\@�~�@�-@�  @��^@��@�1@��9@��#@�n�@��H@�@���@���@��@��@��@xbN@mO�@a�#@^�@S��@M/@G�;@@��@7��@2-@(Q�@"^5@�@�9@^5@+@(�@3311111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�A��HAי�A�XA�+Aև+A���Aț�A��;AĲ-A��A�^5A�C�A��A�/A�ƨA�r�A�-A���A��#A��DA�G�A���A��
A���A�M�A�$�A�\)A��A��A�{A��\A�G�A�+A�r�A�JA�r�A�A��A�t�A��^A�ĜA���Aq�hAat�AT��AO
=AH=qA?+A7|�A0bNA,1'A'�hA%�^A!��AA��AdZA�A\)A�-A��A
{AI�A�`Av�A\)A�;@���@�bN@��@��@蛦@ߥ�@ۮ@���@���@��T@�@�;d@�o@��\@�~�@�-@�  @��^@��@�1@��9@��#@�n�@��H@�@���@���@��@��@��@xbN@mO�@a�#@^�@S��@M/@G�;@@��@7��@2-@(Q�@"^5@�@�9@^5@+@(�@3311111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB�mBm�B��B�B�BȴB��BP�BH�BK�Bx�B|�B� B�B�B�B�%B�%B�=B�PB�JB�JB�PB�PB�PB�PB�bB�uB�{B��B��B��B��B�uB�+Bz�Be`BN�B,BB� B
��B
dZB	��B	l�B	2-B	�B	"�B	&�B	VB	�DB	�B	�LB	��B	�/B	��B	��B	��B	�dB	B	�TB	�B	��B	��B	��B
B
+B
1B
%B
	7B
B
  B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
	7B
DB
\B
hB
oB
{B
�B
�B
�B
�B
&�B
+B
1'B
9XB
?}B
F�B
G�B
N�B
R�B
VB
[#B
_;B
cTB
jB
n�B
s�B
v�B
{�B
� B
�B
�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B�Bm�B��B�B�
B��B��BR�BJ�BN�Bx�B}�B�B�B�B�B�%B�%B�=B�PB�JB�JB�PB�PB�PB�VB�hB�uB�{B��B��B��B��B��B�1B|�BgmBR�B0!B%B�%B
ĜB
iyB	��B	o�B	49B	�B	$�B	(�B	XB	�JB	�B	�RB	��B	�5B	��B	��B	��B	�jB	ÖB	�ZB	�B	��B	��B
  B
B
+B
	7B
%B

=B
B
  B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
	7B
DB
\B
hB
oB
{B
�B
�B
�B
�B
&�B
+B
1'B
9XB
?}B
F�B
G�B
N�B
R�B
VB
[#B
_;B
cTB
jB
n�B
s�B
v�B
{�B
� B
�B
�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ = PRES-SP(NextCycle), where SP is SURFACE PRESSURE from next cycle                                                                                                                                                                                     TEMP_ADJUSTED = TEMP                                                                                                                                                                                                                                            PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,PRES_ADJ,elapsed_time,alpha,tau),elapsed_time=P/mean_rise_rate,P=dbar since the start of the profile for each samples                                                                                                       None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            SP(NextCycle)=0.2(dbar)                                                                                                                                                                                                                                         None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE; PRES_ADJ_ERR : Manufacture sensor accuracy                                                                                                                                                                 TEMP_ADJ_ERR : SBE sensor accuracy                                                                                                                                                                                                                              Salinity Recalculation using PRES_ADJ; PSAL_ADJ_ERR : max(sum of RecalS & CTM errors, 0.01(PSS-78))                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         200907161422372009071614223720090716142237200907161531412009071615314120090716153141200908250000002009082500000020090825000000  JA  ARFMdecpA9_b                                                                20090703095743  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.5                                                                 20090703095744  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.0                                                                 20090703095744  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_b                                                                20090703095745  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19b                                                                20090703095746  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19b                                                                20090703095746  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8b                                                                20090703095746  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8b                                                                20090703095746  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16b                                                                20090703095746  QCP$                G�O�G�O�G�O�           10000JA  ARUP                                                                        20090703100341                      G�O�G�O�G�O�                JA  ARFMdecpA9_b                                                                20090707035654  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.5                                                                 20090707035826  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.0                                                                 20090707035826  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_b                                                                20090707035826  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19b                                                                20090707035827  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19b                                                                20090707035827  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8b                                                                20090707035828  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8b                                                                20090707035828  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16b                                                                20090707035828  QCP$                G�O�G�O�G�O�           10000JA  ARUP                                                                        20090707040258                      G�O�G�O�G�O�                JM  ARGQJMQC1.0                                                                 20090706232622  CV  DAT$            G�O�G�O�F��C                JM  ARCAJMQC1.0                                                                 20090716142237  CV  PRES            G�O�G�O�G�O�                JM  ARCAJMQC1.0                                                                 20090716142237  IP  PSAL            G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20090716153141  CV  PSAL            G�O�G�O�G�O�                JM  ARSQOW  1.1 SeHyD1.0                                                        20090825000000  IP  PSAL            G�O�G�O�G�O�                JA  RFMTcnvd2.1                                                                 20090901091719  IP                  G�O�G�O�G�O�                JA  ARDU                                                                        20090901091741                      G�O�G�O�G�O�                JM      JMFC1.0                                                                 20150611112909                      G�O�G�O�G�O�                JA  ARDU                                                                        20150614170510                      G�O�G�O�G�O�                