CDF   '   
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS   t   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JAMSTEC    source        
Argo float     history       b2009-08-12T10:00:03Z creation;2015-03-10T06:12:35Z update;2015-06-11T11:30:08Z conversion to V3.1;     
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      
_FillValue               conventions       Argo reference table 1          6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    6�   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  6�   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  78   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7x   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    7�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     8   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8<   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8\   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
_FillValue        A.�~       standard_name         time   
resolution        >�����h�   axis      T           8`   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8h   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
_FillValue        A.�~       
resolution        >�����h�        8l   LATITUDE               	long_name         &Latitude of the station, best estimate     units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        standard_name         latitude   axis      Y           8t   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        standard_name         	longitude      axis      X           8|   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8�   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8�   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    8�   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    8�   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    8�   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8�   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9�   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   standard_name         sea_water_pressure     axis      Z        �  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  t  ;p   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   standard_name         sea_water_pressure       �  ;�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  t  =�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  >(   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o   standard_name         sea_water_temperature        �  ?�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  t  A�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o   standard_name         sea_water_temperature        �  B<   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  t  D   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  D�   PSAL         
      	   	long_name         Practical salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o   standard_name         sea_water_salinity       �  FP   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  t  H    PSAL_ADJUSTED            
      	   	long_name         Practical salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o   standard_name         sea_water_salinity       �  H�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  t  Jd   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  J�   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  L�   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   M8   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   V8   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   _8   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  h8   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    h�   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    h�   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    h�   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    h�   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  h�   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    i   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    i   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    i   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         i,   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         i0   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        i4   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    i8Argo profile    3.1 1.2 19500101000000  5901576 J-ARGO                                                          JAMSTEC                                                         PRES            TEMP            PSAL               ,A   JA  20090812100003  20150614170514  A9_76264_044                    2C  D   APEX                            3512                            070407                          846 @�CJ�ך1   @�CK�b��@*�z�G��d�1&�x�1   ARGOS   A   A   A   Primary sampling: discrete [spot sampling CTD data]                                                                                                                                                                                                                @�33A��Ac33A�ffA�  A陚BffB��B2ffBG33BZ  Bn  B���B�ffB�33B�33B�ffB���B���B�  BЙ�Bڙ�B���B�  B�  C�C  C��C��CL�C� CffC$ffC)�C.�C3�C8ffC=��CB�3CG33CQL�C[  CeL�Co� Cy� C��3C��fC���C��3C���C�� C���C���C��fC��fC���C��3C���C�C�ffC�s3C�ffC֙�C۳3C���C噚CꙚCC��3C���D� D�fD�3D��D�3DٚD�3D$��D)ٚD.ٚD3�3D8�fD=��DBٚDG� DL��DQ��DV��D[� D`�fDe�fDjٚDo� DtٚDy��D��D�p D���D��D�#3D�VfD���D���D��D�ffD���D��fD��D�c3Dڬ�D��D��D�ffD� D��fD�3311111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@�  A33Aa��A���A�33A���B  BffB2  BF��BY��Bm��B�ffB�33B�  B�  B�33B���B�ffB���B�ffB�ffB䙚B���B���C  C�fC� C� C33CffCL�C$L�C)  C.  C3  C8L�C=� CB��CG�CQ33CZ�fCe33CoffCyffC��fC���C���C��fC���C�s3C�� C�� C���C���C���C��fC���C�C�Y�C�ffC�Y�C֌�CۦfC���C��C��C��C��fC�� DٚD� D��D�fD��D�3D��D$�fD)�3D.�3D3��D8� D=�fDB�3DG��DL�3DQ�3DV�3D[��D`� De� Dj�3DoٚDt�3Dy�fD��D�l�D���D��fD�  D�S3D��fD�ٚD��D�c3D���D��3D��D�` Dک�D��fD��D�c3D��D��3D�0 11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�-A�$�Aߝ�A�dZA�Q�A�=qA�7LA�33A�-A�v�A�A֡�Aҟ�A�v�AƃA�+A���A�C�A�9XA��A��-A��wA�Q�A�x�A���A���A���A�v�A���A�  A�dZA���A���A��FA���A��A�~�A�Ap(�Ab5?A[dZAXȴAIdZA2�A09XA&�A$jA!hsA�AA��Av�AjA%A��A
�A	\)A	�A�A�A��AVA�7@��R@�b@���@��@�@�V@�j@��@�%@�o@ԋD@�K�@��@��@�ff@�-@��@�@��@��@��@��@��H@�j@���@��;@���@��u@�`B@���@�ff@��\@�7L@w�;@t�@m?}@ahs@W+@OK�@FV@?K�@8r�@2=q@*J@#"�@�@b@�@��@��@	�^@/@9X11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�-A�$�Aߝ�A�dZA�Q�A�=qA�7LA�33A�-A�v�A�A֡�Aҟ�A�v�AƃA�+A���A�C�A�9XA��A��-A��wA�Q�A�x�A���A���A���A�v�A���A�  A�dZA���A���A��FA���A��A�~�A�Ap(�Ab5?A[dZAXȴAIdZA2�A09XA&�A$jA!hsA�AA��Av�AjA%A��A
�A	\)A	�A�A�A��AVA�7@��R@�b@���@��@�@�V@�j@��@�%@�o@ԋD@�K�@��@��@�ff@�-@��@�@��@��@��@��@��H@�j@���@��;@���@��u@�`B@���@�ff@��\@�7L@w�;@t�@m?}@ahs@W+@OK�@FV@?K�@8r�@2=q@*J@#"�@�@b@�@��@��@	�^@/@9X11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oBJB1B
=B	7B	7B+B+B
=B\Bz�B��B�?BŢB�`BJBl�B��B��B��B�FB�LB�B��B�DBp�Bw�Bs�BW
B;dB�B�yB��B�Bt�B�B
��B
=qB
uB	��B	_;B	D�B	33B��B	B	B	oB	�B	49B	]/B	�1B	��B	��B	�B	�;B	�B	�B	��B	��B
B	��B
B
B
B
%B
B
B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	��B	��B	��B	��B	��B	��B
  B
B
%B
JB
VB
bB
hB
�B
�B
�B
 �B
#�B
(�B
/B
:^B
;dB
@�B
F�B
L�B
P�B
T�B
ZB
^5B
bNB
gmB
m�B
r�B
v�B
{�B
|�B
�B
�B
�7B
�=11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111BJB1B
=B	7B	7B+B+B
=BhB{�B��B�XBɺB�BbBn�B��B��B��B�LB�XB�'B��B�\Br�By�Bu�BZB?}B�B�B��B�!Bz�B�B
�B
@�B
�B	�'B	aHB	E�B	7LB	B	B	+B	uB	 �B	5?B	^5B	�1B	��B	��B	�B	�BB	�B	�B	��B	��B
B	��B
B
B
B
%B
B
B
  B	��B	��B	��B	��B	��B	��B	��B	�B	�B	��B	��B	��B	��B	��B	��B
  B
B
%B
JB
VB
bB
hB
�B
�B
�B
 �B
#�B
(�B
/B
:^B
;dB
@�B
F�B
L�B
P�B
T�B
ZB
^5B
bNB
gmB
m�B
r�B
v�B
{�B
|�B
�B
�B
�7B
�=11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ = PRES-SP(NextCycle), where SP is SURFACE PRESSURE from next cycle                                                                                                                                                                                     TEMP_ADJUSTED = TEMP                                                                                                                                                                                                                                            PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,PRES_ADJ,elapsed_time,alpha,tau),elapsed_time=P/mean_rise_rate,P=dbar since the start of the profile for each samples                                                                                                       None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            SP(NextCycle)=0.1(dbar)                                                                                                                                                                                                                                         None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE; PRES_ADJ_ERR : Manufacture sensor accuracy                                                                                                                                                                 TEMP_ADJ_ERR : SBE sensor accuracy                                                                                                                                                                                                                              Salinity Recalculation using PRES_ADJ; PSAL_ADJ_ERR : max(sum of RecalS & CTM errors, 0.01(PSS-78))                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         200908251339362009082513393620090825133936200908251536132009082515361320090825153613201010040000002010100400000020101004000000  JA  ARFMdecpA9_b                                                                20090812100001  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.5                                                                 20090812100003  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.0                                                                 20090812100003  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_b                                                                20090812100004  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19b                                                                20090812100005  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19b                                                                20090812100005  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8b                                                                20090812100005  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8b                                                                20090812100005  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16b                                                                20090812100005  QCP$                G�O�G�O�G�O�           10000JA  ARUP                                                                        20090812100901                      G�O�G�O�G�O�                JA  ARFMdecpA9_b                                                                20090816035612  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.5                                                                 20090816035745  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.0                                                                 20090816035745  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_b                                                                20090816035746  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19b                                                                20090816035747  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19b                                                                20090816035747  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8b                                                                20090816035747  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8b                                                                20090816035747  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16b                                                                20090816035747  QCP$                G�O�G�O�G�O�           10000JA  ARUP                                                                        20090816040114                      G�O�G�O�G�O�                JA  ARCArsal2.1a                                                                20090902022010  IP  PSAL            G�O�G�O�G�O�                JA  ARGQrqcppo_b                                                                20090902022010  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19b                                                                20090902022012  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19b                                                                20090902022012  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8b                                                                20090902022012  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8b                                                                20090902022012  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16b                                                                20090902022012  QCP$                G�O�G�O�G�O�           10000JA  ARUP                                                                        20090902022120                      G�O�G�O�G�O�                JM  ARGQJMQC1.0                                                                 20090815224132  CV  DAT$            G�O�G�O�F�\                JM  ARCAJMQC1.0                                                                 20090825133936  CV  PRES            G�O�G�O�G�O�                JM  ARCAJMQC1.0                                                                 20090825133936  IP  PSAL            G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20090825153613  CV  PSAL            G�O�G�O�G�O�                JM  ARSQOW  1.1 CTD2010V1                                                       20101004000000  IP  PSAL            G�O�G�O�G�O�                JA  RFMTcnvd2.1                                                                 20101015025549  IP                  G�O�G�O�G�O�                JA  ARDU                                                                        20101015025639                      G�O�G�O�G�O�                JM  RENCREJM1.1c                                                                20150209100344  ED  SCIENTIFIC_CALIBG�O�G�O�G�O�                JA  ARDU                                                                        20150310061235                      G�O�G�O�G�O�                JM      JMFC1.0                                                                 20150611112959                      G�O�G�O�G�O�                JA  ARDU                                                                        20150614170514                      G�O�G�O�G�O�                