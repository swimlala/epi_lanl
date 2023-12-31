CDF   0   
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PARAM       N_PROF        N_CALIB       N_LEVELS   s   	N_HISTORY             	   title         Argo float vertical profile    institution       JAMSTEC    source        
Argo float     history       b2008-01-27T05:02:48Z creation;2009-09-01T08:43:27Z update;2015-06-09T21:20:59Z conversion to V3.1;     
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
_FillValue                    iArgo profile    3.1 1.2 19500101000000  5900654 J-ARGO                                                          JAMSTEC                                                         PRES            TEMP            PSAL               {A   JA  20080127050248  20150621190520  A5_23712_123                    2C  D   APEX                            1566                            013004                          846 @Զ~�C!1   @Զ�H��Y@2l������c9XbN1   ARGOS   A   A   A   Primary sampling: discrete [spot sampling CTD data]                                                                                                                                                                                                                @���A��Ad��A�33A�33A�ffB	33BffB0ffBF  BXffBk��B�  B�ffB���B���B�33B�33B�33B�33B�  B�ffB�33BB�ffCffC33C� C�C��C��C� C$L�C)ffC-��C333C8  C<��CA�fCF��CQ�3C[��CeffCn�fCx��C�� C�ٚC�� C��fC���C��3C���C�� C��3C�s3C���C��3C���C³3CǦfC̦fCљ�C֌�Cۀ C���C�fC�fC�3C���C��3D� D��D�3D�fD� D��D� D$�fD)�fD.�3D3��D8� D=ٚDBٚDGٚDL�3DQٚDV��D[ٚD`��De��Dj��Do�fDt��Dy�3D�0 D�s3D��fD���D�)�D�l�D�� D���D��D�ffD���D���D��D�i�DڦfD���D�&fD�\�D�3D�C31111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @���A��A\��A�33A�33A�ffB33BffB.ffBD  BVffBi��B~  B�ffB���B���B�33B�33B�33B�33B�  B�ffB�33B홚B�ffC �fC�3C  C��C�C�C  C#��C(�fC-L�C2�3C7� C<L�CAffCFL�CQ33C[�Cd�fCnffCxL�C�@ C���C�� C�ffC�Y�C�s3C�Y�C�@ C�s3C�33C�L�C�s3C���C�s3C�ffC�ffC�Y�C�L�C�@ C�L�C�ffC�ffC�s3C�Y�C�s3D� D��D�3D�fD� D��D� D$�fD)�fD.�3D3��D8� D=��DB��DG��DL�3DQ��DV��D[��D`��De��Dj��Do�fDt��Dy�3D�  D�c3D��fD���D��D�\�D�� D���D�	�D�VfD���D���D�	�D�Y�DږfD���D�fD�L�D�3D�331111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�5?A�5?A�7LA�33A�1'A�33A�33A�C�A��A��FA��A�1'A���A�~�A�jA��A��9A�/A�ffA�`BA�l�A���A��yA���A�XA��A�dZA�=qA���A�~�A���A��DA���A�&�A��+A��A�XA�5?A��A�dZA��At�RA`�AW�AI"�AEK�A<��A4z�A+��A(JA!�FA�A�\A~�A1'AA
  A ~�@��@�9@��@�Ĝ@�+@�\)@�E�@׾w@ָR@֗�@�M�@�ƨ@��@�  @��@�p�@���@��@�;d@�?}@���@���@���@�
=@� �@�b@���@�V@��R@�7L@��#@��-@�ƨ@��^@���@���@�  @�@�ff@|�@rJ@g�@^��@V��@MO�@DZ@:�@5�T@/�@)��@#��@5?@��@/@&�@��@
�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�5?A�5?A�7LA�33A�1'A�33A�33A�C�A��A��FA��A�1'A���A�~�A�jA��A��9A�/A�ffA�`BA�l�A���A��yA���A�XA��A�dZA�=qA���A�~�A���A��DA���A�&�A��+A��A�XA�5?A��A�dZA��At�RA`�AW�AI"�AEK�A<��A4z�A+��A(JA!�FA�A�\A~�A1'AA
  A ~�@��@�9@��@�Ĝ@�+@�\)@�E�@׾w@ָR@֗�@�M�@�ƨ@��@�  @��@�p�@���@��@�;d@�?}@���@���@���@�
=@� �@�b@���@�V@��R@�7L@��#@��-@�ƨ@��^@���@���@�  @�@�ff@|�@rJ@g�@^��@V��@MO�@DZ@:�@5�T@/�@)��@#��@5?@��@/@&�@��@
�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB	��B	��B	��B	��B	�B	�B	�B	�`B
�=BgmB�1B��B\B�B��B�B,B,B.B(�B1'B2-B=qBO�BG�BiyBm�B]/BVBL�B6FB
=BB�`B�9B�B�hBVBuB
��B
�1B	�B	{B�B��B��B~�B~�B�+B�DB�uB�oB�%B��B�-B��B�BBÖB��B�B�#BȴB�`B��B�B�B	1B	bB	!�B	'�B	T�B	e`B	jB	�B	��B	��B	��B	�{B	��B	�B	�!B	�FB	��B	�B	�sB	�B	�B	�B	��B	��B
B
1B
DB
VB
�B
�B
#�B
+B
2-B
9XB
?}B
H�B
P�B
W
B
\)B
`BB
e`B
jB
o�B
s�B
x�B
{�B
~�B
�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B	��B	��B	��B	��B	�B	�B	�B	�`B
�=BffB�1B��B\B�B��B�B,B-B33B1'B2-B2-B?}BP�BI�BjBo�B_;BW
BN�B8RBJB+B�sB�FB�B�{BYB�B
��B
�VB	�-B	�B�/B��B��B�B�B�1B�PB�{B�{B�1B��B�3B��B��BÖBŢB��B�
B�)BȴB�fB��B�B�B		7B	bB	"�B	'�B	T�B	e`B	jB	�B	��B	��B	��B	�{B	��B	�B	�!B	�FB	��B	�B	�sB	�B	�B	�B	��B	��B
B
1B
DB
VB
�B
�B
#�B
+B
2-B
9XB
?}B
H�B
P�B
W
B
\)B
`BB
e`B
jB
o�B
s�B
x�B
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
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ = PRES-SP(NextCycle), where SP is SURFACE PRESSURE from next cycle                                                                                                                                                                                     TEMP_ADJUSTED = TEMP                                                                                                                                                                                                                                            PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,PRES_ADJ,elapsed_time,alpha,tau),elapsed_time=P/mean_rise_rate,P=dbar since the start of the profile for each samples                                                                                                       None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            SP(NextCycle)=0.5(dbar)                                                                                                                                                                                                                                         None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE; PRES_ADJ_ERR : Manufacture sensor accuracy                                                                                                                                                                 TEMP_ADJ_ERR : SBE sensor accuracy                                                                                                                                                                                                                              Salinity Recalculation using PRES_ADJ; PSAL_ADJ_ERR : max(sum of RecalS & CTM errors, 0.01(PSS-78))                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         200802090822252008020908222520080209082225200802090836322008020908363220080209083632200908260000002009082600000020090826000000  JA  ARFMdecpA5_a                                                                20080127050226  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.4                                                                 20080127050248  IP                  G�O�G�O�G�O�                JA  ARCArsal2.1                                                                 20080127050251  IP  PSAL            G�O�G�O�G�O�                JA  ARGQrqcppo_a                                                                20080127050253  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19b                                                                20080127050305  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19b                                                                20080127050305  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8a                                                                20080127050305  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8a                                                                20080127050305  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16b                                                                20080127050307  QCP$                G�O�G�O�G�O�           10000JA  ARUP                                                                        20080127061704                      G�O�G�O�G�O�                JA  ARFMdecpA5_a                                                                20080131034910  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.4                                                                 20080131034915  IP                  G�O�G�O�G�O�                JA  ARCArsal2.1                                                                 20080131034916  IP  PSAL            G�O�G�O�G�O�                JA  ARGQrqcppo_a                                                                20080131034916  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19b                                                                20080131034920  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19b                                                                20080131034920  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8a                                                                20080131034920  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8a                                                                20080131034920  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16b                                                                20080131034921  QCP$                G�O�G�O�G�O�           10000JA  ARUP                                                                        20080131051147                      G�O�G�O�G�O�                JA  ARCArsal2.1                                                                 20080912002007  IP  PSAL            G�O�G�O�G�O�                JA  ARGQrqcppo_a                                                                20080912002007  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19b                                                                20080912002011  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19b                                                                20080912002011  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8a                                                                20080912002011  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8a                                                                20080912002011  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16b                                                                20080912002012  QCP$                G�O�G�O�G�O�           10000JA  ARUP                                                                        20080912015231                      G�O�G�O�G�O�                JA  ARFMdecpA5_a                                                                20080131034910  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.5                                                                 20090414042816  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.0                                                                 20090414042816  IP  PRES            G�O�G�O�G�O�                JA  ARCArsal2.1a                                                                20090414042816  IP  PSAL            G�O�G�O�G�O�                JA  ARGQrqcppo_a                                                                20090414042816  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19b                                                                20090414042817  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19b                                                                20090414042817  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8b                                                                20090414042817  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8b                                                                20090414042817  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16b                                                                20090414042818  QCP$                G�O�G�O�G�O�           10000JA  ARUP                                                                        20090414042948                      G�O�G�O�G�O�                JM  ARGQJMQC1.0                                                                 20080130172005  CV  DAT$            G�O�G�O�F��                JM  ARCAJMQC1.0                                                                 20080209082225  CV  PRES            G�O�G�O�G�O�                JM  ARCAJMQC1.0                                                                 20080209082225  IP  PSAL            G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20080209083632  CV  PSAL            G�O�G�O�G�O�                JM  ARSQOW  1.1 SeHyD1.0                                                        20090826000000  IP  PSAL            G�O�G�O�G�O�                JA  RFMTcnvd2.1                                                                 20090901084248  IP                  G�O�G�O�G�O�                JA  ARDU                                                                        20090901084327                      G�O�G�O�G�O�                JM      JMFC1.0                                                                 20150609212047                      G�O�G�O�G�O�                JA  ARDU                                                                        20150621190520                      G�O�G�O�G�O�                