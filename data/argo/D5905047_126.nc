CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS   t   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2019-10-16T06:52:52Z creation;2019-10-19T21:53:13Z conversion to V3.1;2022-07-26T02:44:53Z update;     
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile      comment_dmqc_operator         BPRIMARY|https://orcid.org/0000-0001-9150-6442|Kanako Sato, JAMSTEC        @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    7,   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    7<   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    7@   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    7D   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    7T   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7d   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    7t   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  7|   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7�   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        8,   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    80   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    84   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     88   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    8X   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    8\   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     8`   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
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
_FillValue                    8�   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8�   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    8�   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    8�   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    8�   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    9    CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        :    PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  :   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  t  ;�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  <H   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  t  >   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  >�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  t  @\   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  @�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  t  B�   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  C   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  t  D�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  EX   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  t  G(   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  G�   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  Il   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  K<   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  M   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   M�   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   V�   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   _�   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  h�   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    i   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    i    HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    i$   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    i(   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  i,   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    il   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    i|   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    i�   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         i�   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         i�   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        i�   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    i�Argo profile    3.1 1.2 19500101000000  20191016065252  20220818061505  5905047 J-ARGO                                                          JAMSTEC                                                         PRES            TEMP            PSAL               ~A   JA  V4_131545_126                   2C  D   ARVOR                           OIN-13JAP-ARL-73                5607A07                         844 @��{5I2�1   @��B^Ѐ@5��1&��c�1&�x�1   ARGOS   A   A   B   Primary sampling: discrete [SBE41CP pumping for 19 seconds per measurements]                                                                                                                                                                                       ?333@���A!��Aq��A�33A�  A���B33B$  B8ffBJ��B\  Br  B�ffB���B�  B�33B���B�  B�ffB�33B�ffB�ffB�ffB�  B���C��CL�C��CL�C� C�fC� C%� C)�fC/��C433C8�fC=��CBffCGffCQ��C[��Ce�3Co��Cz� C�L�C��C�  C��3C�s3C��C�ffC�@ C��3C�� C���C�33C��fC�C��fC�ٚC�L�C�ffC�@ C�ٚC�L�C�33C� C���C���D,�D�D&fD3D��D3D �D%&fD*  D/&fD4�D933D>,�DB�3DH  DM  DQٚDW3D\33Da  Df  Dj�3Dp  Du,�Dz,�D�VfD�ffD��fD��D�@ D�vfD���D���D�P D��fD���D��D�FfDԉ�D��fD�  D�C3D�3D�� D�311111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111?333@���A!��Aq��A�33A�  A���B33B$  B8ffBJ��B\  Br  B�ffB���B�  B�33B���B�  B�ffB�33B�ffB�ffB�ffB�  B���C��CL�C��CL�C� C�fC� C%� C)�fC/��C433C8�fC=��CBffCGffCQ��C[��Ce�3Co��Cz� C�L�C��C�  C��3C�s3C��C�ffC�@ C��3C�� C���C�33C��fC�C��fC�ٚC�L�C�ffC�@ C�ٚC�L�C�33C� C���C���D,�D�D&fD3D��D3D �D%&fD*  D/&fD4�D933D>,�DB�3DH  DM  DQٚDW3D\33Da  Df  Dj�3Dp  Du,�Dz,�D�VfD�ffD��fD��D�@ D�vfD���D���D�P D��fD���D��D�FfDԉ�D��fD�  D�C3D�3D�� D�311111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A��mA���AڶFAڋDAځA�|�A�z�A�v�A�`BA���A�ffA��/A��wA�VA�bA�oA�dZA���A���A�VA��A��A�JA�p�A��A�ĜA���A���A�~�A��A��A��HA���A�\)A�dZA�x�A��RA�1A��^A���A���A|�/AsƨAhffA\�!AU�-AM�;AI��AE�;A;�A5�^A'hsA~�A�AAdZAbAQ�AC�A�-A
=A(�@��@��@�!@�dZ@��/@��@�S�@�O�@۾w@��/@ͩ�@Å@�bN@��@�V@�I�@��@��
@�dZ@���@�E�@���@��-@��@�M�@��@��@��@���@��7@��P@��-@�1@��D@�O�@�A�@vE�@l�@fV@]�@U�h@N��@IG�@CdZ@;�
@3C�@-�@&v�@!�7@{@��@I�@�R@�
11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A��mA���AڶFAڋDAځA�|�A�z�A�v�A�`BA���A�ffA��/A��wA�VA�bA�oA�dZA���A���A�VA��A��A�JA�p�A��A�ĜA���A���A�~�A��A��A��HA���A�\)A�dZA�x�A��RA�1A��^A���A���A|�/AsƨAhffA\�!AU�-AM�;AI��AE�;A;�A5�^A'hsA~�A�AAdZAbAQ�AC�A�-A
=A(�@��@��@�!@�dZ@��/@��@�S�@�O�@۾w@��/@ͩ�@Å@�bN@��@�V@�I�@��@��
@�dZ@���@�E�@���@��-@��@�M�@��@��@��@���@��7@��P@��-@�1@��D@�O�@�A�@vE�@l�@fV@]�@U�h@N��@IG�@CdZ@;�
@3C�@-�@&v�@!�7@{@��@I�@�R@�
11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B5?B@�B@�B?}B?}B?}B@�B?}B=qBXB�%B�{B��B��B��B��B�{B�{B�JB�=B�%B|�B{�Bv�B}�Bv�B]/BA�B/B�BB�BɺB�dBv�B>wB�B
�B
�)B
�B
��B
m�B
 �B	��B	�JB	e`B	<jB	%�B	
=B��B�!B� B}�Bz�Bu�Bp�Bk�BdZB`BB\)BaHBiyBq�By�B}�B�%B��B��B��B��B��B�qBȴB�`B��B	"�B	33B	O�B	ffB	y�B	�JB	��B	�B	�?B	��B	��B	�
B	�)B	�5B	�B	�B	��B	��B
B
+B
DB
�B
�B
%�B
.B
49B
;dB
B�B
H�B
M�B
R�B
YB
`BB
e`B
l�B
q�B
t�B
x�B
{�B
�B
�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B#�B/B/B-�B-�B-�B/ B./B0�BRBz*B�B��B�RB��B�1B�3B��B�BzButBp�Bk6Bg8Bm�Bg�BM�B1�B 'B�B�B��B�>B�IBg�B/5BB
��B
�DB
�	B
�qB
^�B
�B	��B	}"B	VmB	,�B	B�BĶB�TBq�Bm�Bj�Be�B`BB[�BT�BO�BM6BQ�BY�Bb4Bi�Bm�BvFB�B��B�HB�[B��B�CB��B��B�kB	:B	"�B	?HB	U�B	i_B	{�B	��B	�QB	��B	��B	�B	�?B	�^B	̈́B	��B	�B	�$B	�IB	�GB	�zB	�xB
�B
�B
B
/B
#TB
*B
1�B
7�B
<�B
BB
H1B
O\B
T{B
[�B
`�B
c�B
g�B
kB
p!B
s31111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<7��<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-PO1+PO2,  where PO1 is PRESSURE OFFSET from this cycle and PO2 is PRESSURE OFFSET from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ=PSAL(corrected by recalS & CTM)+deltaS, where deltaS is calculated by OW; PSAL_ADJ_ERR=max(RecalS & CTM & OW error , 0.01(PSS-78))                                                                                                                     PO1=0.2(dbar); PO2=0.2(dbar)                                                                                                                                                                                                                                    None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            r=0.9996(+-0.0000), deepest deltaS=-0.017(+-0.001)(PSS-78); Mapping scale = 8/4,4/2; 0-200(dbar) is excluded in mapping;                                                                                                                                        Pressure Correction using reported PRESSURE OFFSET                                                                                                                                                                                                              None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            OW(Ver.1.1) salinity adjustment is adopted                                                                                                                                                                                                                      201910300015312019103000153120191030001531202207232057142022072320571420220723205714202207261126592022072611265920220726112659  JA  ARFMdecpV4_b                                                                20191016065252  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.6                                                                 20191016065252  IP                  G�O�G�O�G�O�                JA  ARGQrqcppo_b                                                                20191016065253  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19c                                                                20191016065253  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8d                                                                20191016065254  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16b                                                                20191016065254  QCP$                G�O�G�O�G�O�           10000JA  ARUP                                                                        20191016065509                      G�O�G�O�G�O�                JA  ARFMdecpV4_b                                                                20191019215215  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.6                                                                 20191019215311  IP                  G�O�G�O�G�O�                JA  ARGQrqcppo_b                                                                20191019215311  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19c                                                                20191019215312  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8d                                                                20191019215312  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16b                                                                20191019215312  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20191019215313                      G�O�G�O�G�O�                JA  ARUP                                                                        20191019215357                      G�O�G�O�G�O�                JM  ARSQJMQC2.0                                                                 20191020000000  CF  PSAL_ADJUSTED_QC?333?333G�O�                JM  ARCAJMQC2.0                                                                 20191029151531  IP  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20191029151531  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20220723115714  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 CTD2021V2                                                       20220726022659  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20220818061505                      G�O�G�O�G�O�                