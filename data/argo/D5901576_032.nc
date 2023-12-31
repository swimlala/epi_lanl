CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PARAM       N_PROF        N_CALIB       N_LEVELS   t   	N_HISTORY             	   title         Argo float vertical profile    institution       JAMSTEC    source        
Argo float     history       b2009-04-14T12:57:32Z creation;2009-09-01T09:17:39Z update;2015-06-11T11:27:46Z conversion to V3.1;     
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
_FillValue                    i8Argo profile    3.1 1.2 19500101000000  5901576 J-ARGO                                                          JAMSTEC                                                         PRES            TEMP            PSAL                A   JA  20090414125732  20150614170512  A9_76264_032                    2C  D   APEX                            3512                            070407                          846 @�%M?���1   @�%U�З�@+Tz�G��d�;dZ1   ARGOS   A   A   A   Primary sampling: discrete [spot sampling CTD data]                                                                                                                                                                                                                @�ffA33A`  A���A�ffA�  B
  B33B133BF  BY��Bn  B���B���B���B���B�33B�ffB���Bƙ�BЙ�B�  B�ffB�33B�ffC  CffC��CffCL�CffC� C$��C)ffC.ffC3��C8L�C=ffCB� CGffCQL�CZ�fCe�Co33Cy  C�� C���C���C�� C�� C�� C�� C��3C���C���C�� C�� C��3C³3C�s3C̀ CѦfC֦fC�s3C���C�s3CꙚC�ffC���C���D��D� DٚD� D� D�fD�fD$� D)� D.��D3�3D8�3D=� DB�fDG�3DL� DQ��DV� D[ٚD`� De��Dj� Do��Dt�fDy��D�,�D�l�D���D���D�  D�i�D��3D��3D�  D�Y�D���D���D�&fD�\�Dڰ D�� D�  D�Y�D�3D��3D�I�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@�  A  A\��A�33A���A�ffB	33BffB0ffBE33BX��Bm33B�33B�ffB�ffB�ffB���B�  B�ffB�33B�33Bڙ�B�  B���B�  C ��C33CffC33C�C33CL�C$��C)33C.33C3ffC8�C=33CBL�CG33CQ�CZ�3Cd�fCo  Cx��C�ffC�� C�s3C��fC��fC��fC��fC���C�� C��3C��fC�ffC���C�C�Y�C�ffCь�C֌�C�Y�C�� C�Y�C� C�L�C� C�� D� D�3D��D�3D�3D��D��D$�3D)�3D.� D3�fD8�fD=�3DB��DG�fDL�3DQ� DV�3D[��D`�3De� Dj�3Do� Dt��Dy� D�&fD�ffD��fD��fD��D�c3D���D���D��D�S3D��fD��fD�  D�VfDک�D�ٚD��D�S3D��D���D�C311111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��AɓuAɧ�Aɥ�Aɩ�AɮAɮAɋDA�A�A��A��A��A��A��A�oAƛ�A�"�Aĝ�AÏ\A�ZA�z�A�%A�p�A��A�jA��#A��uA�-A���A���A��A���A��
A�t�A��HA��
A�VAdZAn�\Ag��Ac�FASƨAG\)A?�mA9A2�A-�A-�FA*�`A&ĜA�AJA��A5?AS�A9XA7LAC�A�FAAZA�;A	VA�TAffA�HA7LA��A {@�G�@��u@�{@�;d@�R@�&�@�I�@�`B@��@�1'@� �@��9@�~�@���@���@��F@�(�@�/@�V@��@�%@���@���@�1'@��\@��P@��`@�-@��@x�9@n�@co@]�@Up�@JJ@B�H@=p�@6�y@2~�@,(�@%@�;@�\@V@��@ȴ@�@
M�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111AɓuAɧ�Aɥ�Aɩ�AɮAɮAɋDA�A�A��A��A��A��A��A�oAƛ�A�"�Aĝ�AÏ\A�ZA�z�A�%A�p�A��A�jA��#A��uA�-A���A���A��A���A��
A�t�A��HA��
A�VAdZAn�\Ag��Ac�FASƨAG\)A?�mA9A2�A-�A-�FA*�`A&ĜA�AJA��A5?AS�A9XA7LAC�A�FAAZA�;A	VA�TAffA�HA7LA��A {@�G�@��u@�{@�;d@�R@�&�@�I�@�`B@��@�1'@� �@��9@�~�@���@���@��F@�(�@�/@�V@��@�%@���@���@�1'@��\@��P@��`@�-@��@x�9@n�@co@]�@Up�@JJ@B�H@=p�@6�y@2~�@,(�@%@�;@�\@V@��@ȴ@�@
M�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�-B	��B
� B
�B
�B=qB��B��B�#B��BB%�B5?B?}B<jB��B�;B�RB��B�hBA�B
^5B
uB	�B	�XB	�+B	_;B	F�B��B�fB�sB��B	uB	#�B	`BB	r�B	��B	�dB	�HB	�sB	�B	��B	��B
1B
B

=B
PB
bB
oB
uB
{B
�B
oB
PB

=B
%B
B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
1B
PB
bB
uB
{B
�B
�B
�B
�B
�B
!�B
(�B
.B
7LB
>wB
D�B
H�B
M�B
S�B
YB
\)B
`BB
cTB
hsB
o�B
s�B
w�B
{�B
}�B
� B
�%B
�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�-B	��B
�B
�B
�#B=qB��B��B�#B��BB%�B6FBD�BE�B��B�NB�XB�B��BO�B
gmB
�B	�B	��B	�=B	aHB	J�B	B�sB�B��B	{B	#�B	aHB	s�B	��B	�dB	�HB	�yB	�B	��B	��B
	7B
B
DB
PB
hB
uB
uB
{B
�B
oB
PB

=B
+B
B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
1B
PB
bB
uB
{B
�B
�B
�B
�B
�B
!�B
(�B
.B
7LB
>wB
D�B
H�B
M�B
S�B
YB
\)B
`BB
cTB
hsB
o�B
s�B
w�B
{�B
}�B
� B
�%B
�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ = PRES-SP(NextCycle), where SP is SURFACE PRESSURE from next cycle                                                                                                                                                                                     TEMP_ADJUSTED = TEMP                                                                                                                                                                                                                                            PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,PRES_ADJ,elapsed_time,alpha,tau),elapsed_time=P/mean_rise_rate,P=dbar since the start of the profile for each samples                                                                                                       None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            SP(NextCycle)=0.2(dbar)                                                                                                                                                                                                                                         None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE; PRES_ADJ_ERR : Manufacture sensor accuracy                                                                                                                                                                 TEMP_ADJ_ERR : SBE sensor accuracy                                                                                                                                                                                                                              Salinity Recalculation using PRES_ADJ; PSAL_ADJ_ERR : max(sum of RecalS & CTM errors, 0.01(PSS-78))                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         200904271331422009042713314220090427133142200904271342272009042713422720090427134227200908250000002009082500000020090825000000  JA  ARFMdecpA9_b                                                                20090414125730  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.5                                                                 20090414125732  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.0                                                                 20090414125732  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_a                                                                20090414125732  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19b                                                                20090414125733  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19b                                                                20090414125733  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8b                                                                20090414125733  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8b                                                                20090414125733  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16b                                                                20090414125733  QCP$                G�O�G�O�G�O�           10000JA  ARUP                                                                        20090414130251                      G�O�G�O�G�O�                JA  ARFMdecpA9_b                                                                20090418065610  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.5                                                                 20090418065704  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.0                                                                 20090418065704  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_a                                                                20090418065704  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19b                                                                20090418065705  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19b                                                                20090418065705  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8b                                                                20090418065706  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8b                                                                20090418065706  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16b                                                                20090418065706  QCP$                G�O�G�O�G�O�           10000JA  ARUP                                                                        20090418070054                      G�O�G�O�G�O�                JM  ARGQJMQC1.0                                                                 20090417224928  CV  DAT$            G�O�G�O�F�*q                JM  ARCAJMQC1.0                                                                 20090427133142  CV  PRES            G�O�G�O�G�O�                JM  ARCAJMQC1.0                                                                 20090427133142  IP  PSAL            G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20090427134227  CV  PSAL            G�O�G�O�G�O�                JM  ARSQOW  1.1 SeHyD1.0                                                        20090825000000  IP  PSAL            G�O�G�O�G�O�                JA  RFMTcnvd2.1                                                                 20090901091720  IP                  G�O�G�O�G�O�                JA  ARDU                                                                        20090901091739                      G�O�G�O�G�O�                JM      JMFC1.0                                                                 20150611112737                      G�O�G�O�G�O�                JA  ARDU                                                                        20150614170512                      G�O�G�O�G�O�                