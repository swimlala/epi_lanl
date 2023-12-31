CDF   )   
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS   o   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JAMSTEC    source        
Argo float     history       b2008-02-07T10:51:03Z creation;2011-02-01T07:53:30Z update;2015-06-09T10:17:18Z conversion to V3.1;     
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
_FillValue                  p  ;\   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   standard_name         sea_water_pressure       �  ;�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  p  =�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  =�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o   standard_name         sea_water_temperature        �  ?�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  p  Ap   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o   standard_name         sea_water_temperature        �  A�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  p  C�   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  D   PSAL         
      	   	long_name         Practical salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o   standard_name         sea_water_salinity       �  E�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  p  G�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o   standard_name         sea_water_salinity       �  G�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  p  I�   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  J    	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  K�   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   Ll   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   Ul   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   ^l   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  gl   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    g�   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    g�   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    g�   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    g�   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  g�   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    h<   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    hL   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    hP   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         h`   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         hd   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        hh   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    hlArgo profile    3.1 1.2 19500101000000  5900493 J-ARGO                                                          JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA  20080207105103  20150621120513  A5_21018_146                    2C  D   APEX                            1090                            061703                          846 @ԹK�A��1   @ԹR�q�@24�j~���bְ ě�1   ARGOS   A   A   B   Primary sampling: discrete [spot sampling CTD data]                                                                                                                                                                                                                @�  AffA^ffA�  A���A�ffB
ffB33B133BF  BZ  BlffB���B�ffB�ffB�33B�  B���B���B�  B�ffB�33B���BB�  CL�C�C  C��CffCL�C� C$33C)L�C.33C3ffC7�fC=ffCA�3CG33CQL�C[L�CeL�Co��Cy� C���C���C��fC���C��3C��fC��3C��3C��fC���C�� C��3C���C³3C�ffČ�C�s3C֦fCۙ�C�fC噚C���C�fC���C��fDٚD�fD�3D��D�3D3D"Y�D(��D.�3D5�D;33DAy�DG��DM��DTY�DZ� D`ٚDg�DmFfDs�fDy� D�#3D�i�D���D��3D�,�D�ffD���D��3D�)�D�i�D��3D��3D�#3D�p Dڣ3D��D�  D�ffD��D�@ 111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @�  AffA^ffA�  A���A�ffB
ffB33B133BF  BZ  BlffB���B�ffB�ffB�33B�  B���B���B�  B�ffB�33B���BB�  CL�C�C  C��CffCL�C� C$33C)L�C.33C3ffC7�fC=ffCA�3CG33CQL�C[L�CeL�Co��Cy� C���C���C��fC���C��3C��fC��3C��3C��fC���C�� C��3C���C³3C�ffČ�C�s3C֦fCۙ�C�fC噚C���C�fC���C��fDٚD�fD�3D��D�3D3D"Y�D(��D.�3D5�D;33DAy�DG��DM��DTY�DZ� D`ٚDg�DmFfDs�fDy� D�#3D�i�D���D��3D�,�D�ffD���D��3D�)�D�i�D��3D��3D�#3D�p Dڣ3D��D�  D�ffD��D�@ 222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A��A��A���A��A��A���A��A��A��mA��mA��#A��
A���A��A��HA��HA�ffA�G�A��-A��/A��RA�  A�oA��mA���A�1A��A��A�p�Ax�Am��Aj$�AhQ�A\5?AT�yAOl�AGƨADn�A@=qA<�A6�A0��A-�PA'��A z�AE�A�yAA�9A�
A(�A��A��AȴA+A�
A �@��+@�/@�9X@�\@�\@� �@���@畁@��@� �@���@�%@�V@�?}@�Z@Ɨ�@� �@�I�@��R@���@�M�@�G�@�%@���@���@��@�M�@���@��/@���@��R@�r�@�^5@�"�@��`@xbN@l�@c�F@]�h@U��@O+@G;d@@��@:�!@3S�@-/@'��@ bN@�H@@�@�+@
n�@�+111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A��A��A���A��A��A���A��A��A��mA��mA��#A��
A���A��A��HA��HA�ffA�G�A��-A��/A��RA�  A�oA��mA���A�1A��A��A�p�Ax�Am��Aj$�AhQ�A\5?AT�yAOl�AGƨADn�A@=qA<�A6�A0��A-�PA'��A z�AE�A�yAA�9A�
A(�A��A��AȴA+A�
A �@��+@�/@�9X@�\@�\@� �@���@畁@��@� �@���@�%@�V@�?}@�Z@Ɨ�@� �@�I�@��R@���@�M�@�G�@�%@���@���@��@�M�@���@��/@���@��R@�r�@�^5@�"�@��`@xbN@l�@c�F@]�h@U��@O+@G;d@@��@:�!@3S�@-/@'��@ bN@�H@@�@�+@
n�@�+222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222 ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB	R�B	Q�B	O�B	S�B	S�B	T�B	VB	^5B	q�B	�JB	��B	��B	�B	�LB	��B	�B	��B
"�B
P�B
u�B
-B
(�B
oB	�B

=B
'�B
B	�TB	��B	?}B	'�B	�B	uB��B�B�HBĜB�wB�LB�dB�B�B��B�B��B�B�B	  B	  B	+B	uB	�B	"�B	/B	8RB	?}B	G�B	L�B	`BB	o�B	{�B	�B	�VB	��B	��B	��B	��B	�jB	��B	��B	B	��B	��B	��B	�B	�)B	�sB	�B	��B	��B	��B
B
1B

=B
	7B
VB
hB
{B
�B
�B
!�B
)�B
2-B
:^B
@�B
E�B
J�B
O�B
W
    B
`BB
gmB
k�B
o�B
v�B
{�B
�B
�B
�1B
�DB
�b111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111 B	R�B	Q�B	O�B	S�B	S�B	T�B	VB	^5B	q�B	�JB	��B	��B	�B	�LB	��B	�B	��B
,B
T�B
{�B
2-B
.B
�B	��B
DB
,B
B	�sB	�FB	D�B	)�B	 �B	�B��B�B�`BƨB��B�XB�jB�B�B��B�#B�B�B�B	B	B	1B	{B	�B	#�B	/B	8RB	@�B	H�B	M�B	`BB	o�B	|�B	�B	�VB	��B	�B	��B	��B	�jB	��B	��B	B	��B	��B	��B	�B	�)B	�sB	�B	��B	��B	��B
B
1B

=B
	7B
VB
hB
{B
�B
�B
!�B
)�B
2-B
:^B
@�B
E�B
J�B
O�B
W
G�O�B
`BB
gmB
k�B
o�B
v�B
{�B
�B
�B
�1B
�DB
�b222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222422222222222 <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<(�U<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<T��<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ = PRES-SP(NextCycle), where SP is SURFACE PRESSURE from next cycle                                                                                                                                                                                     TEMP_ADJUSTED = TEMP                                                                                                                                                                                                                                            PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,PRES_ADJ,elapsed_time,alpha,tau),elapsed_time=P/mean_rise_rate,P=dbar since the start of the profile for each samples                                                                                                       None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            SP(NextCycle)=0.0(dbar)                                                                                                                                                                                                                                         None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                      None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE; PRES_ADJ_ERR : Manufacture sensor accuracy                                                                                                                                                                 TEMP_ADJ_ERR : SBE sensor accuracy                                                                                                                                                                                                                              Salinity Recalculation using PRES_ADJ; PSAL_ADJ_ERR : max(sum of RecalS & CTM errors, 0.01(PSS-78))                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            TNPD: APEX float that truncated negative pressure drift                                                                                                                                                                                                         None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         200802202022572008022020225720080220202257200802202041522008022020415220080220204152200808110000002008081100000020080811000000  JA  ARFMdecpA5_a                                                                20080207105100  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.4                                                                 20080207105103  IP                  G�O�G�O�G�O�                JA  ARCArsal2.1                                                                 20080207105104  IP  PSAL            G�O�G�O�G�O�                JA  ARGQrqcppo_a                                                                20080207105104  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19b                                                                20080207105108  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19b                                                                20080207105108  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8a                                                                20080207105108  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcp2.8a                                                                20080207105108  QCF$                G�O�G�O�G�O�              40JA  ARGQaqcp2.8a                                                                20080207105108  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8a                                                                20080207105108  QCF$                G�O�G�O�G�O�              40JA  ARGQrqcpt16b                                                                20080207105109  QCP$                G�O�G�O�G�O�           10000JA  ARUP                                                                        20080207110541                      G�O�G�O�G�O�                JA  ARFMdecpA5_a                                                                20080211154259  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.4                                                                 20080211154304  IP                  G�O�G�O�G�O�                JA  ARCArsal2.1                                                                 20080211154305  IP  PSAL            G�O�G�O�G�O�                JA  ARGQrqcppo_a                                                                20080211154305  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19b                                                                20080211154309  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19b                                                                20080211154309  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8a                                                                20080211154309  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcp2.8a                                                                20080211154309  QCF$                G�O�G�O�G�O�              40JA  ARGQaqcp2.8a                                                                20080211154309  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8a                                                                20080211154309  QCF$                G�O�G�O�G�O�              40JA  ARGQrqcpt16b                                                                20080211154310  QCP$                G�O�G�O�G�O�           10000JA  ARUP                                                                        20080211190542                      G�O�G�O�G�O�                JM  ARCAJMQC1.0                                                                 20080220202257  IP  PRES            G�O�G�O�G�O�                JM  ARCAJMQC1.0                                                                 20080220202257  IP  PSAL            G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20080220204152  CV  PSAL            G�O�G�O�G�O�                JM  ARSQWJO 2.0 SeHyD1.0                                                        20080811000000  IP  PSAL            G�O�G�O�G�O�                JA  RFMTcnvd2.1                                                                 20080930080207  IP                  G�O�G�O�G�O�                JA  ARDU                                                                        20080930100020                      G�O�G�O�G�O�                JM  AREVjmrvp1.2                                                                20090312115748  IP  PRES            G�O�G�O�G�O�                JA  RFMTcnvd2.1                                                                 20090318063052  IP                  G�O�G�O�G�O�                JA  ARDU                                                                        20090318063541                      G�O�G�O�G�O�                JM  ARGQREJM1.0                                                                 20110124100901  CV  JULD            G�O�G�O�F��`                JM  AREQREJM1.0                                                                 20110124100901  CF  PRES_ADJUSTED_QC@�  D�@ G�O�                JM  AREQREJM1.0                                                                 20110124100901  CF  TEMP_ADJUSTED_QC@�  D�@ G�O�                JM  AREQREJM1.0                                                                 20110124100901  CF  PSAL_ADJUSTED_QC@�  D�@ G�O�                JA  RFMTcnvd2.1                                                                 20110201075218  IP                  G�O�G�O�G�O�                JA  ARDU                                                                        20110201075330                      G�O�G�O�G�O�                JM      JMFC1.0                                                                 20150609101707                      G�O�G�O�G�O�                JA  ARDU                                                                        20150621120513                      G�O�G�O�G�O�                