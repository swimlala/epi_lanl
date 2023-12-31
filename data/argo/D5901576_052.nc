CDF   !   
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS   t   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JAMSTEC    source        
Argo float     history       b2009-10-31T09:56:51Z creation;2015-03-10T06:12:36Z update;2015-06-11T11:31:37Z conversion to V3.1;     
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
_FillValue                    i8Argo profile    3.1 1.2 19500101000000  5901576 J-ARGO                                                          JAMSTEC                                                         PRES            TEMP            PSAL               4A   JA  20091031095651  20150614170513  A9_76264_052                    2C  D   APEX                            3512                            070407                          846 @�WL�%�1   @�WNf�}|@*F�-�d��\)1   ARGOS   A   A   A   Primary sampling: discrete [spot sampling CTD data]                                                                                                                                                                                                                @���A��AfffA�ffA�ffA�ffB  B  B2  BG��BY��Bl��B�33B���B�ffB���B�33B�33B�33B�  B�  B�  B�  BB�33C��C� C��CL�CL�CffC  C$L�C)33C.L�C2��C8ffC=ffCBL�CG� CQffC[ffCe33Co33Cy��C�� C���C�� C��fC�� C�� C��3C��fC�� C���C���C��fC���C�CǙ�C�� C�� C�� C۳3C���C� C���C�3C��3C�� D��D�fD� DٚD��D�3D�fD$� D)ٚD.�fD3��D8�3D=ٚDB��DG�fDL� DQ�3DV�3D[�fD`��De�fDj� Do�3DtٚDy� D�  D�i�D�� D�� D��D�l�D���D�ٚD�&fD�ffD��3D��D�&fD�\�Dڣ3D�� D�fD�\�D�D�ٚD���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@���A��AfffA�ffA�ffA�ffB  B  B2  BG��BY��Bl��B�33B���B�ffB���B�33B�33B�33B�  B�  B�  B�  BB�33C��C� C��CL�CL�CffC  C$L�C)33C.L�C2��C8ffC=ffCBL�CG� CQffC[ffCe33Co33Cy��C�� C���C�� C��fC�� C�� C��3C��fC�� C���C���C��fC���C�CǙ�C�� C�� C�� C۳3C���C� C���C�3C��3C�� D��D�fD� DٚD��D�3D�fD$� D)ٚD.�fD3��D8�3D=ٚDB��DG�fDL� DQ�3DV�3D[�fD`��De�fDj� Do�3DtٚDy� D�  D�i�D�� D�� D��D�l�D���D�ٚD�&fD�ffD��3D��D�&fD�\�Dڣ3D�� D�fD�\�D�D�ٚD���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�?}A�9XA�?}A�=qA�/A���A��A�+A�p�A��/A�|�A���A�5?A�XA�%A��^A��`A�v�A��jA�dZA��A�$�A��
A�K�A�  A�A�(�A���A�A��FA���A+AuS�Am�hA_hsA[l�AY�^AX^5AJ$�AHJA>�jA8��A-�7A&=qA�A
=AI�A�HA��AVA�!A��A�!A1A�A$�At�A ��A ��A ��@��@�`B@�z�@���@�1@��@�bN@ܣ�@�$�@ם�@�o@�/@�Z@�p�@�n�@��@�j@�/@��P@��;@��R@�ƨ@��@���@��@�b@���@��@��#@���@���@���@�V@�I�@�33@�;@s�
@ihs@ax�@X��@N�+@E�-@>�@6{@/�@+�@%�@ A�@33@+@��@hs@�@��@	�@�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�?}A�9XA�?}A�=qA�/A���A��A�+A�p�A��/A�|�A���A�5?A�XA�%A��^A��`A�v�A��jA�dZA��A�$�A��
A�K�A�  A�A�(�A���A�A��FA���A+AuS�Am�hA_hsA[l�AY�^AX^5AJ$�AHJA>�jA8��A-�7A&=qA�A
=AI�A�HA��AVA�!A��A�!A1A�A$�At�A ��A ��A ��@��@�`B@�z�@���@�1@��@�bN@ܣ�@�$�@ם�@�o@�/@�Z@�p�@�n�@��@�j@�/@��P@��;@��R@�ƨ@��@���@��@�b@���@��@��#@���@���@���@�V@�I�@�33@�;@s�
@ihs@ax�@X��@N�+@E�-@>�@6{@/�@+�@%�@ A�@33@+@��@hs@�@��@	�@�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB
\B
\B
VB
\B
PB
DB
�B
N�B
N�B
7LB
�3B
ŢB
�B�B��BȴB�BB��B6FBE�BG�B8RB�B  B�sBĜB�B�BB
�uB
O�B
B	��B	�bB	K�B	9XB	/B	!�B�B�B�B�mB��B�B�qB��B��B��B��B�wB�B	D�B	y�B	�JB	��B	��B	�}B	�}B	��B	�fB	�`B	�yB	�B	�`B	�5B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B
B
%B
+B
1B
DB
PB
JB
uB
uB
�B
�B
�B
�B
$�B
)�B
,B
-B
5?B
;dB
B�B
F�B
L�B
S�B
ZB
_;B
e`B
iyB
l�B
p�B
u�B
x�B
|�B
� B
�B
�B
�B
�1B
�711111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B
\B
\B
VB
\B
PB
DB
�B
O�B
O�B
>wB
�9B
��B
��B�B��B��B�NBB8RBH�BM�B<jB�BB�BŢB�9B�DB1B
��B
VB
%B	ŢB	��B	M�B	:^B	0!B	(�B�B�B��B�B��B�)B�}B��B��B��B��B�wB�B	D�B	y�B	�PB	��B	��B	�}B	�}B	��B	�mB	�fB	�B	�B	�fB	�;B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B
B
%B
+B
1B
DB
PB
JB
uB
uB
�B
�B
�B
�B
$�B
)�B
,B
-B
5?B
;dB
B�B
F�B
L�B
S�B
ZB
_;B
e`B
iyB
l�B
p�B
u�B
x�B
|�B
� B
�B
�B
�B
�1B
�711111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ = PRES-SP(NextCycle), where SP is SURFACE PRESSURE from next cycle                                                                                                                                                                                     TEMP_ADJUSTED = TEMP                                                                                                                                                                                                                                            PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,PRES_ADJ,elapsed_time,alpha,tau),elapsed_time=P/mean_rise_rate,P=dbar since the start of the profile for each samples                                                                                                       None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            SP(NextCycle)=0.0(dbar)                                                                                                                                                                                                                                         None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE; PRES_ADJ_ERR : Manufacture sensor accuracy                                                                                                                                                                 TEMP_ADJ_ERR : SBE sensor accuracy                                                                                                                                                                                                                              Salinity Recalculation using PRES_ADJ; PSAL_ADJ_ERR : max(sum of RecalS & CTM errors, 0.01(PSS-78))                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         200911131356042009111313560420091113135604200911131416592009111314165920091113141659201010040000002010100400000020101004000000  JA  ARFMdecpA9_b                                                                20091031095650  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.5                                                                 20091031095651  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.0                                                                 20091031095651  IP  PRES            G�O�G�O�G�O�                JA  ARCArsal2.1a                                                                20091031095651  IP  PSAL            G�O�G�O�G�O�                JA  ARGQrqcppo_b                                                                20091031095652  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19b                                                                20091031095653  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19b                                                                20091031095653  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8c                                                                20091031095653  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8c                                                                20091031095653  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16b                                                                20091031095653  QCP$                G�O�G�O�G�O�           10000JA  ARUP                                                                        20091031100143                      G�O�G�O�G�O�                JA  ARFMdecpA9_b                                                                20091104065605  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.5                                                                 20091104065718  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.0                                                                 20091104065718  IP  PRES            G�O�G�O�G�O�                JA  ARCArsal2.1a                                                                20091104065719  IP  PSAL            G�O�G�O�G�O�                JA  ARGQrqcppo_b                                                                20091104065719  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19b                                                                20091104065720  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19b                                                                20091104065720  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8c                                                                20091104065720  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8c                                                                20091104065720  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16b                                                                20091104065720  QCP$                G�O�G�O�G�O�           10000JA  ARUP                                                                        20091104070054                      G�O�G�O�G�O�                JM  ARGQJMQC1.0                                                                 20091103232101  CV  DAT$            G�O�G�O�F��q                JM  ARCAJMQC1.0                                                                 20091113135604  IP  PRES            G�O�G�O�G�O�                JM  ARCAJMQC1.0                                                                 20091113135604  IP  PSAL            G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20091113141659  CV  PSAL            G�O�G�O�G�O�                JM  ARSQOW  1.1 CTD2010V1                                                       20101004000000  IP  PSAL            G�O�G�O�G�O�                JA  RFMTcnvd2.1                                                                 20101015025552  IP                  G�O�G�O�G�O�                JA  ARDU                                                                        20101015025641                      G�O�G�O�G�O�                JM  RENCREJM1.1c                                                                20150209100355  ED  SCIENTIFIC_CALIBG�O�G�O�G�O�                JA  ARDU                                                                        20150310061236                      G�O�G�O�G�O�                JM      JMFC1.0                                                                 20150611113128                      G�O�G�O�G�O�                JA  ARDU                                                                        20150614170513                      G�O�G�O�G�O�                