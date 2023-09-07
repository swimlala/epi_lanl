CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS   t   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       ~2019-02-03T06:52:39Z creation;2019-02-06T21:53:12Z conversion to V3.1;2019-09-10T08:05:14Z update;2022-11-10T04:22:17Z update;     
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
_FillValue                    i�Argo profile    3.1 1.2 19500101000000  20190203065239  20221117231505  5905222 J-ARGO                                                          JAMSTEC                                                         PRES            TEMP            PSAL               A   JA  V4_131533_025                   2C  Dd`ARVOR                           OIN-13JAP-ARL-61                5607A07                         844 @ؤ���� 1   @ؤ�� @.i��l�D�d`A�7L1   ARGOS   A   A   B   Primary sampling: discrete [SBE41CP pumping for 19 seconds per measurements]                                                                                                                                                                                       ?���@�ffA&ffAq��A�ffA�ffA�33B��B!33B8ffBG33B\��Bm��B�33B�  B�  B���B�ffB�  B���B���Bә�B�33B癚B�  B���C33C� C��C� C��CffC �3C%�C*��C0L�C533C:�C>33CC��CH�fCQL�C[�fCeL�CpffCzL�C�� C��fC��C�@ C��fC�&fC�ٚC���C�  C���C��3C�33C���C��fC��C��C��3C֦fC��fC�  C��C�L�C��3C�L�C�L�D&fD�3D�fD  D�3D�D�3D$ٚD)�fD/�D4  D9�D=�3DC3DGٚDMfDR�DW,�D[� Da3De��Dk�Dp&fDu�Dz�D�L�D�s3D��fD��D�I�D���D��3D���D�S3D��fD�� D�fD�@ DԆfD���D�3D�Y�D��D���D��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111?���@ə�A(  As33A�33A�33A�  B  B!��B8��BG��B]33Bn  B�ffB�33B�33B���B���B�33B�  B�  B���B�ffB���B�33B���CL�C��C�3C��C�3C� C ��C%33C*�fC0ffC5L�C:33C>L�CC�fCI  CQffC\  CeffCp� CzffC���C��3C�&fC�L�C��3C�33C��fC�ٚC��C�ٚC�  C�@ C��fC��3C��C�&fC�  Cֳ3C��3C��C�&fC�Y�C�  C�Y�C�Y�D,�D��D��D&fD��D  DٚD$� D)��D/3D4&fD93D=��DC�DG� DM�DR  DW33D[�fDa�De�3Dk3Dp,�Du  Dz  D�P D�vfD�ɚD� D�L�D�� D��fD���D�VfD���D��3D��D�C3Dԉ�D�� D�fD�\�D� D�� D��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111AͲ-A͓uA�v�A�dZA�^5A�^5A�ZA�ZA�VA�\)A��/A�O�A�p�A���A͑hA��A�K�A�JA�z�A�p�A��A���A�ĜA��A�1'A��DA�A�A���A��
A�+A�M�A�=qA�hsA��PA�(�A�~�Ax�DAm��Ab�HA[&�AVv�AM\)AB��A4��A1�7A.�A*JA'
=A'G�A%�-A"$�AbNA��A�FAƨA
1'A
VA
A�A�wA�/A�AoA �RA ~�@���@��T@�Z@��m@�S�@�;d@�ƨ@��y@��@�\)@�M�@�/@���@��@�z�@��-@���@��#@�dZ@��@���@�/@�?}@�p�@�n�@�\)@�b@�O�@��R@�1'@��#@�=q@y7L@tj@lZ@bJ@XQ�@Nff@G��@@bN@8��@1�@,1@%�-@!�@�@��@�!@��@
�@�+11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111AͲ-A͓uA�v�A�dZA�^5A�^5A�ZA�ZA�VA�\)A��/A�O�A�p�A���A͑hA��A�K�A�JA�z�A�p�A��A���A�ĜA��A�1'A��DA�A�A���A��
A�+A�M�A�=qA�hsA��PA�(�A�~�Ax�DAm��Ab�HA[&�AVv�AM\)AB��A4��A1�7A.�A*JA'
=A'G�A%�-A"$�AbNA��A�FAƨA
1'A
VA
A�A�wA�/A�AoA �RA ~�@���@��T@�Z@��m@�S�@�;d@�ƨ@��y@��@�\)@�M�@�/@���@��@�z�@��-@���@��#@�dZ@��@���@�/@�?}@�p�@�n�@�\)@�b@�O�@��R@�1'@��#@�=q@y7L@tj@lZ@bJ@XQ�@Nff@G��@@bN@8��@1�@,1@%�-@!�@�@��@�!@��@
�@�+11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B�)B�BB�HB�`B�`B�fB�yB�B�B��B	�B	gmB	�5B
��B
�yBuBS�B�uB��B�fBB�B+B:^B�B�yB��B=qB9XB
��B
�{B
��B
�-B
��B
\)B	��B	��B	�B	C�B	+B	�B	+B��B�RB�wB�B��B	 �B	�uB	�B	��B	�FB	��B	��B	�%B	u�B	�hB	��B	�\B	�B	��B	��B	��B	�B	�qB	�9B	�B	�qB	��B	ȴB	��B	��B	��B	�5B	�B	�;B	�BB	�`B	�yB	�B	��B
  B
%B
1B
DB
PB
	7B

=B
DB
VB
oB
{B
�B
�B
�B
"�B
0!B
5?B
<jB
B�B
H�B
M�B
S�B
XB
YB
^5B
e`B
hsB
n�B
q�B
t�B
w�B
{�B
�B
�B
�=11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B�]B�\B�HB�`B�`B�fB�yB�eB�B 4B	B	f�B	��B
��B
��BBUB��B�XB�B
�B"B
#B;B#�B�)B�)B?�B?}B
�B
��B
�LB
�9B
�OB
`�B	�cB	�YB	�lB	G_B	-wB	eB		�B��B�XB�cB�$B��B	 �B	��B	�B	��B	�8B	�7B	�:B	��B	u�B	��B	��B	�.B	�[B	�B	��B	�B	�B	�B	��B	��B	��B	�^B	�RB	�DB	�HB	�:B	޸B	چB	ߊB	�vB	��B	�B	�B	�6B
 4B
YB
fB
�B
�B
	�B

rB
�B
�B
�B
�B
�B
�B
�B
"�B
0UB
5?B
<jB
B�B
H�B
NB
TB
XB
Y1B
^OB
e`B
hsB
n�B
q�B
t�B
w�B
{�B
�B
�B
�#31111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-PO1+PO2,  where PO1 is PRESSURE OFFSET from this cycle and PO2 is PRESSURE OFFSET from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              PO1=-0.2(dbar); PO2=-0.1(dbar)                                                                                                                                                                                                                                  None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported PRESSURE OFFSET                                                                                                                                                                                                              None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201902170016092019021700160920190217001609202210251311302022102513113020221025131130202210251804492022102518044920221025180449  JA  ARFMdecpV4_b                                                                20190203065238  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.6                                                                 20190203065239  IP                  G�O�G�O�G�O�                JA  ARGQrqcppo_b                                                                20190203065239  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19c                                                                20190203065240  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8d                                                                20190203065240  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16b                                                                20190203065240  QCP$                G�O�G�O�G�O�           10000JA  ARUP                                                                        20190203070149                      G�O�G�O�G�O�                JA  ARFMdecpV4_b                                                                20190206215156  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.6                                                                 20190206215310  IP                  G�O�G�O�G�O�                JA  ARGQrqcppo_b                                                                20190206215310  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19c                                                                20190206215311  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8d                                                                20190206215311  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16b                                                                20190206215311  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20190206215312                      G�O�G�O�G�O�                JA  ARUP                                                                        20190206215547                      G�O�G�O�G�O�                JM  ARSQJMQC2.0                                                                 20190207000000  CF  PSAL_ADJUSTED_QC?���?���G�O�                JM  ARCAJMQC2.0                                                                 20190216151609  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20190216151609  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20190217151446  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20190920071517                      G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20221025041130  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20221117231505                      G�O�G�O�G�O�                