CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PARAM       N_PROF        N_CALIB       N_LEVELS   t   	N_HISTORY             	   title         Argo float vertical profile    institution       JAMSTEC    source        
Argo float     history       b2008-06-24T04:24:49Z creation;2009-09-01T09:17:51Z update;2015-06-11T11:21:39Z conversion to V3.1;     
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
_FillValue                    i8Argo profile    3.1 1.2 19500101000000  5901576 J-ARGO                                                          JAMSTEC                                                         PRES            TEMP            PSAL               A   JA  20080624042449  20150614164516  A9_76264_002                    2C  D   APEX                            3512                            070407                          846 @��F
�1   @��Hb:g�@)a���o�d�;dZ�1   ARGOS   A   A   A   Primary sampling: discrete [spot sampling CTD data]                                                                                                                                                                                                                @���A33Ad��A�ffA���A�  B	��B33B2  BFffBY��BnffB���B�ffB�33B���B���B���B�  B�  B�  B�  B���B�33B�  C��C� CffC�C  C� C� C$� C)L�C.  C3�C8L�C=ffCB33CG� CQffC[� Ce33Co� CyL�C���C���C��3C���C�� C���C��3C�� C��fC���C���C��fC���C���C�� Č�Cь�C�� CۦfC���C�3C�� C���C�� C��3D� DٚD�3D� DٚDٚD� D$�3D)�3D.�3D3��D8�3D=ٚDBٚDG��DL� DQ��DVٚD[� D`� De��Dj�3Do�3DtٚDy� D�&fD�c3D���D���D��D�` D�� D��3D�  D�ffD�� D��fD��D�ffDڠ D�ٚD�  D�S3D�3D���D�)�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@�ffA��Ac33A���A�  A�33B	33B��B1��BF  BY33Bn  B���B�33B�  B���B�ffB�ffB���B���B���B���B㙚B�  B���C� CffCL�C  C�fCffCffC$ffC)33C-�fC3  C833C=L�CB�CGffCQL�C[ffCe�CoffCy33C�� C���C��fC�� C�s3C�� C��fC�s3C���C���C�� C���C���C�� Cǳ3C̀ Cр Cֳ3Cۙ�C���C�fC�3C�� C��3C��fD��D�3D��DٚD�3D�3D��D$��D)��D.��D3�3D8��D=�3DB�3DG�fDLٚDQ�3DV�3D[��D`��De�fDj��Do��Dt�3DyٚD�#3D�` D���D��D��D�\�D���D�� D��D�c3D���D��3D��D�c3Dڜ�D��fD��D�P D� D�ٚD�&f11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A��`A���A�t�A���A��A��A��HA���A���AծA�oA�`BA��`AΟ�A��A��
Aɰ!A�$�A��A§�A�VA�  A�ffA�1'A��A��A��A�/A�ƨA�ȴA��A���A��yA��A��A��
AvbNAm�^Ak��AedZAVĜAL�yAC�7A9�A6�+A2VA.bA+XA'33A$1'A"Q�Ap�AI�Ar�A��A/A�jA"�A��A$�A%A"�A�AĜA
A�AĜAA$�A��A(�A �@��j@�"�@�+@�hs@�-@���@݉7@�C�@���@��@ȋD@���@�=q@�ff@�t�@��^@�&�@�"�@��y@�z�@��P@�5?@��R@�ȴ@��@��h@�A�@x��@qG�@e�-@[t�@T9X@O�w@Fv�@>��@49X@,�@&5?@ Ĝ@��@1'@��@^5@+@$�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A��`A���A�t�A���A��A��A��HA���A���AծA�oA�`BA��`AΟ�A��A��
Aɰ!A�$�A��A§�A�VA�  A�ffA�1'A��A��A��A�/A�ƨA�ȴA��A���A��yA��A��A��
AvbNAm�^Ak��AedZAVĜAL�yAC�7A9�A6�+A2VA.bA+XA'33A$1'A"Q�Ap�AI�Ar�A��A/A�jA"�A��A$�A%A"�A�AĜA
A�AĜAA$�A��A(�A �@��j@�"�@�+@�hs@�-@���@݉7@�C�@���@��@ȋD@���@�=q@�ff@�t�@��^@�&�@�"�@��y@�z�@��P@�5?@��R@�ȴ@��@��h@�A�@x��@qG�@e�-@[t�@T9X@O�w@Fv�@>��@49X@,�@&5?@ Ĝ@��@1'@��@^5@+@$�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB}�B}�By�Bt�Bs�Bt�Bs�Bs�Bs�Bm�BO�B
�jB
�bB
�%B
�wB%BI�B��B�B�HB��BhBu�B]/BE�B+B?}B8RB$�BVB��B��B}�B8RB
ffB
(�B	�BB	�B	��B	x�B	0!B	!�B	`BB	q�B	��B	��B	��B
B
$�B
1'B
H�B
^5B
hsB
gmB
k�B
n�B
jB
jB
ffB
dZB
_;B
ZB
VB
^5B
W
B
J�B
G�B
D�B
B�B
@�B
9XB
5?B
33B
.B
'�B
 �B
 �B
�B
�B
�B
{B
�B
�B
VB
bB
\B
\B
\B
VB
oB
oB
�B
�B
 �B
$�B
)�B
1'B
7LB
=qB
A�B
F�B
M�B
R�B
T�B
ZB
_;B
ffB
iyB
o�B
s�B
w�B
|�B
� B
�B
�B
�%11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B}�B}�By�Bt�Bs�Bt�Bs�Bs�Bs�Bn�BS�B
�qB
�bB
�7B
�}B+BJ�B��B�B�NB��BoBw�BaHBJ�B/BA�B<jB&�B{B��B��B�BF�B
jB
.B	�ZB	�B	��B	|�B	33B	$�B	cTB	r�B	��B	��B	��B
%B
%�B
2-B
I�B
^5B
hsB
hsB
k�B
o�B
jB
jB
ffB
dZB
`BB
ZB
VB
_;B
XB
J�B
H�B
D�B
B�B
@�B
9XB
5?B
33B
.B
(�B
 �B
 �B
�B
�B
�B
{B
�B
�B
VB
hB
\B
\B
\B
VB
oB
oB
�B
�B
 �B
$�B
)�B
1'B
7LB
=qB
A�B
F�B
M�B
R�B
T�B
ZB
_;B
ffB
iyB
o�B
s�B
w�B
|�B
� B
�B
�B
�%11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<e`B<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ = PRES-SP(NextCycle), where SP is SURFACE PRESSURE from next cycle                                                                                                                                                                                     TEMP_ADJUSTED = TEMP                                                                                                                                                                                                                                            PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,PRES_ADJ,elapsed_time,alpha,tau),elapsed_time=P/mean_rise_rate,P=dbar since the start of the profile for each samples                                                                                                       None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            SP(NextCycle)=0.1(dbar)                                                                                                                                                                                                                                         None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE; PRES_ADJ_ERR : Manufacture sensor accuracy                                                                                                                                                                 TEMP_ADJ_ERR : SBE sensor accuracy                                                                                                                                                                                                                              Salinity Recalculation using PRES_ADJ; PSAL_ADJ_ERR : max(sum of RecalS & CTM errors, 0.01(PSS-78))                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         200807011348372008070113483720080701134837200807011358462008070113584620080701135846200908250000002009082500000020090825000000  JA  ARFMdecpA9_b                                                                20080624042004  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.4                                                                 20080624042449  IP                  G�O�G�O�G�O�                JA  ARGQrqcppo_a                                                                20080624042450  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19b                                                                20080624042457  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8a                                                                20080624042457  QCP$                G�O�G�O�G�O�            FB40JA  ARUP                                                                        20080624051314                      G�O�G�O�G�O�                JA  ARFMdecpA9_b                                                                20080622065944  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.5                                                                 20090430090328  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.0                                                                 20090430090329  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_b                                                                20090430090329  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19b                                                                20090430090330  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19b                                                                20090430090330  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8b                                                                20090430090330  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8b                                                                20090430090330  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16b                                                                20090430090330  QCP$                G�O�G�O�G�O�           10000JA  ARUP                                                                        20090430090530                      G�O�G�O�G�O�                JM  ARGQJMQC1.0                                                                 20080621224312  CV  DAT$            G�O�G�O�F��C                JM  ARCAJMQC1.0                                                                 20080701134837  CV  PRES            G�O�G�O�G�O�                JM  ARCAJMQC1.0                                                                 20080701134837  IP  PSAL            G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20080701135846  CV  PSAL            G�O�G�O�G�O�                JM  ARSQOW  1.1 SeHyD1.0                                                        20090825000000  IP  PSAL            G�O�G�O�G�O�                JA  RFMTcnvd2.1                                                                 20090901091712  IP                  G�O�G�O�G�O�                JA  ARDU                                                                        20090901091751                      G�O�G�O�G�O�                JM      JMFC1.0                                                                 20150611112129                      G�O�G�O�G�O�                JA  ARDU                                                                        20150614164516                      G�O�G�O�G�O�                