CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS   t   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       ~2018-12-23T18:53:32Z creation;2018-12-23T18:53:34Z conversion to V3.1;2019-09-10T08:52:32Z update;2022-07-26T02:46:06Z update;     
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile      comment_dmqc_operator         BPRIMARY|https://orcid.org/0000-0001-9150-6442|Kanako Sato, JAMSTEC        @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    7H   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    7X   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    7\   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    7`   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    7p   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    7�   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  7�   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7�   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  8   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        8H   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    8L   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    8P   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     8T   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    8t   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    8x   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     8|   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     8�   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8�   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8�   JULD               standard_name         time   	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~       axis      T           8�   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8�   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~            8�   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8�   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8�   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    9   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    9   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    9   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    9   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    9   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    9   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        :   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  :    PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  t  ;�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  <d   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  t  >4   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  >�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  t  @x   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  @�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  t  B�   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  C0   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  t  E    PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  Et   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  t  GD   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  G�   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  I�   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  KX   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  M(   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   M�   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   V�   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   _�   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  h�   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    i8   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    i<   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    i@   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    iD   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  iH   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    i�   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    i�   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    i�   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         i�   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         i�   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        i�   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    i�Argo profile    3.1 1.2 19500101000000  20181223185332  20220818051505  5905047 J-ARGO                                                          JAMSTEC                                                         PRES            TEMP            PSAL               `A   JA  V4_131545_096                   2C  Dd;ARVOR                           OIN-13JAP-ARL-73                5607A07                         844 @ؙ{qf�1   @ؙ��؎�@3"�\(���d;dZ�1   ARGOS   A   A   B   Primary sampling: discrete [SBE41CP pumping for 19 seconds per measurements]                                                                                                                                                                                       ?���@�ffA4��Ax  A�ffAř�A�  B	33B!33B3��BK33BZ��Br��B���B���B���B�33B�ffB�  B�  B�33B�  Bܙ�B�  B�33B�  CL�C  C�3C��C��C��C L�C%��C*�3C0  C3��C933C=ffCC� CH�CQL�C[��Ce�fCp�fCy�fC�ffC�ٚC���C�33C���C��C�L�C�� C��fC�33C�ffC�&fC�Y�C�L�C���C�33C�ffC�L�C��3C�33C�33C�ffC�@ C�L�C��D�D3D�D3D�fD� DٚD%�D*  D.��D4�D9&fD>3DCfDG��DM  DQ��DW�D\3D`��De��DkfDp�Du�Dz3D�9�D���D���D� D�VfD��fD��3D��D�C3D���D�ɚD�#3D�FfDԉ�Dڳ3D��3D�C3D�vfD�� D�f11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111?���@�ffA4��Ax  A�ffAř�A�  B	33B!33B3��BK33BZ��Br��B���B���B���B�33B�ffB�  B�  B�33B�  Bܙ�B�  B�33B�  CL�C  C�3C��C��C��C L�C%��C*�3C0  C3��C933C=ffCC� CH�CQL�C[��Ce�fCp�fCy�fC�ffC�ٚC���C�33C���C��C�L�C�� C��fC�33C�ffC�&fC�Y�C�L�C���C�33C�ffC�L�C��3C�33C�33C�ffC�@ C�L�C��D�D3D�D3D�fD� DٚD%�D*  D.��D4�D9&fD>3DCfDG��DM  DQ��DW�D\3D`��De��DkfDp�Du�Dz3D�9�D���D���D� D�VfD��fD��3D��D�C3D���D�ɚD�#3D�FfDԉ�Dڳ3D��3D�C3D�vfD�� D�f11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�ƨAͼjAͶFA���A;wAͼjA;wA;wA͸RAͼjAͺ^Aͣ�A�~�A�$�A�\)A�dZA�S�A��A�JA���A�33A��A�Q�A�ĜA�VA�ZA��A���A�r�A���A���A��A�M�A�z�A�t�A���A�M�A��A�+A�G�A�ffA�ffAvVAm�PAep�A\�\AS�7AK��AC�TA<1A1��A,�RA%�PA"�A
=A1A�AQ�AO�A1At�AhsAbA�-@���@�V@���@��u@���@�
=@���@�hs@�M�@�ȴ@�r�@���@���@��P@��
@���@���@���@�b@�  @��@�;d@���@�S�@�7L@�+@�X@���@�{@�Q�@��-@�+@��#@u�@m@c�m@]��@W�@Q&�@E�-@>@5��@/K�@(  @#S�@�@o@5?@C�@A�@�m@��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�ƨAͼjAͶFA���A;wAͼjA;wA;wA͸RAͼjAͺ^Aͣ�A�~�A�$�A�\)A�dZA�S�A��A�JA���A�33A��A�Q�A�ĜA�VA�ZA��A���A�r�A���A���A��A�M�A�z�A�t�A���A�M�A��A�+A�G�A�ffA�ffAvVAm�PAep�A\�\AS�7AK��AC�TA<1A1��A,�RA%�PA"�A
=A1A�AQ�AO�A1At�AhsAbA�-@���@�V@���@��u@���@�
=@���@�hs@�M�@�ȴ@�r�@���@���@��P@��
@���@���@���@�b@�  @��@�;d@���@�S�@�7L@�+@�X@���@�{@�Q�@��-@�+@��#@u�@m@c�m@]��@W�@Q&�@E�-@>@5��@/K�@(  @#S�@�@o@5?@C�@A�@�m@��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111BuB�B�B�B{B{B{B{B�B�B�B!�B�B�BH�BO�Bl�Bt�Bz�By�By�Bt�Bq�Bq�B`BB33B�B{B�B��B�!B�bBD�B�B
��B
�fB
ȴB
ÖB
�3B
��B
�{B
~�B
$�B	��B	�^B	�oB	`BB	.B��B�)B�9B��B��B�uB�Bz�Bt�Bx�Bt�B�{B��B�qB�BǮB�5B�mBȴB��B�XB�;B�B�#B��B	$�B	S�B	bNB	m�B	w�B	|�B	�hB	��B	�LB	�wB	ɺB	��B	�BB	�`B	�B	��B	��B
B
%B
	7B
\B
{B
�B
!�B
-B
33B
:^B
>wB
B�B
F�B
M�B
R�B
[#B
cTB
iyB
n�B
r�B
v�B
z�B
}�B
�B
�B
�711111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B�B	�B
�B
�B�B�B�B�B
�B
�B�B1B�6B(B?.BHBb�BkQBo�BoBo�Bl�BgBh�BW�B(�B�B�B��B�mB�B��B<6B�B
�B
�IB
�B
��B
��B
��B
�7B
u�B
WB	��B	�'B	��B	V�B	$�B�9B�uB��B�CB�"B�RBz�Bp;Bi�Bn�Bi�B�XB�4B�hB��B��B�@B�/B�cB��B��B��B�B�.B�B	B	H�B	V�B	bNB	lqB	q�B	�B	�gB	��B	�B	�BB	ǔB	ԯB	��B	� B	�0B	�oB	��B	��B	��B
�B
�B
B
9B
!|B
'�B
.�B
2�B
6�B
;0B
BAB
G_B
O�B
W�B
]�B
cB
gB
kB
oOB
raB
u�B
y�B
}�31111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-PO1+PO2,  where PO1 is PRESSURE OFFSET from this cycle and PO2 is PRESSURE OFFSET from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ=PSAL(corrected by recalS & CTM)+deltaS, where deltaS is calculated by OW; PSAL_ADJ_ERR=max(RecalS & CTM & OW error , 0.01(PSS-78))                                                                                                                     PO1=0.2(dbar); PO2=0.2(dbar)                                                                                                                                                                                                                                    None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            r=0.9997(+-0.0000), deepest deltaS=-0.011(+-0.001)(PSS-78); Mapping scale = 8/4,4/2; 0-200(dbar) is excluded in mapping;                                                                                                                                        Pressure Correction using reported PRESSURE OFFSET                                                                                                                                                                                                              None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            OW(Ver.1.1) salinity adjustment is adopted                                                                                                                                                                                                                      201901030015282019010300152820190103001528202207232056472022072320564720220723205647202207261125002022072611250020220726112500  JA  ARFMdecpV4_b                                                                20181223185215  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.6                                                                 20181223185332  IP                  G�O�G�O�G�O�                JA  ARGQrqcppo_b                                                                20181223185332  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19c                                                                20181223185333  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8d                                                                20181223185333  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16b                                                                20181223185334  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20181223185334                      G�O�G�O�G�O�                JA  ARUP                                                                        20181223185656                      G�O�G�O�G�O�                JM  ARSQJMQC2.0                                                                 20181224000000  CF  PSAL_ADJUSTED_QC?���?���G�O�                JM  ARCAJMQC2.0                                                                 20190102151528  IP  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20190102151528  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20190103151728  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20190920011516                      G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20220723115647  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 CTD2021V2                                                       20220726022500  OW  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20220818051505                      G�O�G�O�G�O�                