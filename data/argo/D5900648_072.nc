CDF   !   
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PARAM       N_PROF        N_CALIB       N_LEVELS   s   	N_HISTORY             	   title         Argo float vertical profile    institution       JAMSTEC    source        
Argo float     history       b2006-09-01T04:51:39Z creation;2013-09-24T05:26:05Z update;2015-06-09T19:17:09Z conversion to V3.1;     
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
_FillValue                    iArgo profile    3.1 1.2 19500101000000  5900648 J-ARGO                                                          JAMSTEC                                                         PRES            TEMP            PSAL               HA   JA  20060901045139  20150614050515  A5_28347_072                    2C  D   APEX                            1316                            013004                          846 @�69�q��1   @�6<����@7P ě���cFȴ9X1   ARGOS   A   A   A   Primary sampling: discrete [spot sampling CTD data]                                                                                                                                                                                                                @���A  AfffA���A�  A���B��B33B0��BE��BZ  Bm33B�  B���B�33B�  B�33B�33B���Bƙ�B�33B�33B�33BB�33C  CffC33CL�C33CL�CffC$33C)33C.��C3ffC8  C=L�CB�3CG�3CQ33C[� CeffCoffCy33C�� C�s3C�� C��fC�� C��3C���C��fC���C��fC��3C���C��fC�Cǀ C�s3C�Y�Cր CۦfC�3C�fC�fC�� C��3C��3D�fDٚD� D��D�3D�3D�3D$��D)� D.�3D3�3D8� D=ٚDB��DG��DL��DQ�fDV�3D[��D`��De�3Dj�3Do� Dt� Dy��D�0 D�c3D���D���D�&fD�c3D�� D��D�#3D�ffD���D��3D�#3D�\�Dڜ�D�� D�&fD�VfD�fD�f1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @���A  AfffA���A�  A���B��B33B0��BE��BZ  Bm33B�  B���B�33B�  B�33B�33B���Bƙ�B�33B�33B�33BB�33C  CffC33CL�C33CL�CffC$33C)33C.��C3ffC8  C=L�CB�3CG�3CQ33C[� CeffCoffCy33C�� C�s3C�� C��fC�� C��3C���C��fC���C��fC��3C���C��fC�Cǀ C�s3C�Y�Cր CۦfC�3C�fC�fC�� C��3C��3D�fDٚD� D��D�3D�3D�3D$��D)� D.�3D3�3D8� D=ٚDB��DG��DL��DQ�fDV�3D[��D`��De�3Dj�3Do� Dt� Dy��D�0 D�c3D���D���D�&fD�c3D�� D��D�#3D�ffD���D��3D�#3D�\�Dڜ�D�� D�&fD�VfD�fD�f2222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�&�A��`Aк^Aв-AЬAХ�AС�A�~�A�VA�(�A�"�A�hsA�ffA�{A�bNA�bA���A�  A��A�"�A���A� �A�\)A�;dA�l�A�$�A���A���A���A�+A�z�A��A�`BA�bA��A�ƨA�S�A��!A��^A�^5A�E�A��uA��A��Ay�As�7Ak�mAg��AcS�A]S�AU;dAQ"�AJ��AEC�A@v�A;O�A7&�A3��A,��A&ĜA#hsA��AbAG�A��A5?A%A	��A�A�@�@�  @��@�z�@�@�t�@��w@�|�@�`B@�@�l�@��@��7@�t�@�ff@�v�@�\)@���@���@��+@�%@�A�@���@�X@�j@y�^@p �@j��@ax�@Y��@SdZ@K��@E��@?�@8Ĝ@4j@.ff@)�@#�
@��@�m@�w@��@bN@1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�&�A��`Aк^Aв-AЬAХ�AС�A�~�A�VA�(�A�"�A�hsA�ffA�{A�bNA�bA���A�  A��A�"�A���A� �A�\)A�;dA�l�A�$�A���A���A���A�+A�z�A��A�`BA�bA��A�ƨA�S�A��!A��^A�^5A�E�A��uA��A��Ay�As�7Ak�mAg��AcS�A]S�AU;dAQ"�AJ��AEC�A@v�A;O�A7&�A3��A,��A&ĜA#hsA��AbAG�A��A5?A%A	��A�A�@�@�  @��@�z�@�@�t�@��w@�|�@�`B@�@�l�@��@��7@�t�@�ff@�v�@�\)@���@���@��+@�%@�A�@���@�X@�j@y�^@p �@j��@ax�@Y��@SdZ@K��@E��@?�@8Ĝ@4j@.ff@)�@#�
@��@�m@�w@��@bN@2222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222 ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oBVBJBDBJBPBPBVB{B!�B+B-B/BW
B�PB��B�B�?B�wBƨB�}B�wB�jB�dB��BƨB�}BÖB��B�XB�-B��B��Bv�BB�B	7B�B�BɺB��B�BXB�B
�qB
bNB
;dB
�B	�B	�B	�jB	��B	e`B	ZB	7LB	�B��B�fB�;B�B��B�PB�%B~�Bu�Bo�BjBdZB^5BT�BO�BD�B6FB+B(�B2-BC�BVBjB�B��B�jB��B�B	DB	�B	"�B	1'B	S�B	o�B	~�B	��B	�!B	��B	�B	�fB	�B
B
VB
�B
"�B
)�B
0!B
8RB
<jB
C�B
I�B
O�B
XB
\)B
aHB
cTB
iyB
n�B
q�B
u�B
{�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 BVBJBDBJBPBPBVB�B#�B,B/B49BZB�VB��B�B�FB�}BǮB��B�}B�qB�jBBȴB��BĜB��B�^B�3B�B��By�BE�BDB��B�B��B��B�BZB�B
��B
dZB
<jB
�B	�B	�
B	�wB	��B	ffB	\)B	8RB	�B��B�mB�BB�B��B�VB�+B�Bv�Bo�Bk�Be`B_;BVBP�BF�B7LB,B)�B33BD�BW
BjB�B��B�jB��B�B	DB	�B	"�B	1'B	S�B	o�B	~�B	��B	�!B	��B	�B	�fB	�B
B
VB
�B
"�B
)�B
0!B
8RB
<jB
C�B
I�B
O�B
XB
\)B
aHB
cTB
iyB
n�B
q�B
u�B
{�2222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222 <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ = PRES-SP(NextCycle), where SP is SURFACE PRESSURE from next cycle                                                                                                                                                                                     TEMP_ADJUSTED = TEMP                                                                                                                                                                                                                                            PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,PRES_ADJ,elapsed_time,alpha,tau),elapsed_time=P/mean_rise_rate,P=dbar since the start of the profile for each samples                                                                                                       None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            SP(NextCycle)=0.0(dbar)                                                                                                                                                                                                                                         None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE; PRES_ADJ_ERR : Manufacture sensor accuracy                                                                                                                                                                 TEMP_ADJ_ERR : SBE sensor accuracy                                                                                                                                                                                                                              Salinity Recalculation using PRES_ADJ; PSAL_ADJ_ERR : max(sum of RecalS & CTM errors, 0.01(PSS-78))                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            TNPD: APEX float that truncated negative pressure drift                                                                                                                                                                                                         None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         200609141708122006091417081220060914170812200701270206352007012702063520070127020635201309120000002013091200000020130912000000  JA  ARFMfmtp2.3                                                                 20060901045139  IP                  G�O�G�O�G�O�                JA  ARCAsspa2.1                                                                 20060901045140  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcp2.5                                                                 20060901045140  QCP$                G�O�G�O�G�O�           1FB7CJA  ARGQaqcp2.5                                                                 20060901045140  QCP$                G�O�G�O�G�O�           1FB40JA  ARUP                                                                        20060901045805                      G�O�G�O�G�O�                JA  ARFMfmtp2.3                                                                 20060905034639  IP                  G�O�G�O�G�O�                JA  ARCAsspa2.1                                                                 20060905034639  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcp2.5                                                                 20060905034640  QCP$                G�O�G�O�G�O�           1FB7CJA  ARGQaqcp2.5                                                                 20060905034640  QCP$                G�O�G�O�G�O�           1FB40JA  ARUP                                                                        20060905035628                      G�O�G�O�G�O�                JA  ARFMdecpA5_a                                                                20090401065007  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.5                                                                 20090401065940  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.0                                                                 20090401065940  IP  PRES            G�O�G�O�G�O�                JA  ARCArsal2.1a                                                                20090401065940  IP  PSAL            G�O�G�O�G�O�                JA  ARGQrqcppo_a                                                                20090401065940  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19b                                                                20090401065941  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19b                                                                20090401065941  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8b                                                                20090401065942  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8b                                                                20090401065942  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16b                                                                20090401065942  QCP$                G�O�G�O�G�O�           10000JA  ARUP                                                                        20090401070453                      G�O�G�O�G�O�                JM  ARGQJMQC1.0                                                                 20060904174118  CV  DAT$            G�O�G�O�F���                JM  ARSQJMQC1.0                                                                 20060904174118  CF  PRES            @���D�fG�O�                JM  ARSQJMQC1.0                                                                 20060904174118  CF  TEMP            @���D�fG�O�                JM  ARSQJMQC1.0                                                                 20060904174118  CF  PSAL            @���D�fG�O�                JM  ARCAJMQC1.0                                                                 20060914170812  IP  PRES            G�O�G�O�G�O�                JM  ARCAJMQC1.0                                                                 20060914170812  IP  PSAL            G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20070127020635  CV  PSAL            G�O�G�O�G�O�                JM  ARSQOW  1.1 CTD2013V01                                                      20130912000000  IP  PSAL            G�O�G�O�G�O�                JA  RFMTcnvd2.1                                                                 20130924052348  IP                  G�O�G�O�G�O�                JA  ARDU                                                                        20130924052605                      G�O�G�O�G�O�                JM      JMFC1.0                                                                 20150609191703                      G�O�G�O�G�O�                JA  ARDU                                                                        20150614050515                      G�O�G�O�G�O�                