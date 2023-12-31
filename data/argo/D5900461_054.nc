CDF      
      STRING16      STRING4       	DATE_TIME         N_PROF        STRING8       STRING64   @   N_PARAM       STRING2       STRING32       N_LEVELS   G   N_CALIB       	STRING256         	N_HISTORY                     <   	DATA_TYPE                   comment       	Data type      
_FillValue                    0�   FORMAT_VERSION                 comment       File format version    
_FillValue                    0�   HANDBOOK_VERSION               comment       Data handbook version      
_FillValue                    0�   REFERENCE_DATE_TIME                comment       !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    1    PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    1   PROJECT_NAME                  comment       Name of the project    
_FillValue                  @  1   PI_NAME                   comment       "Name of the principal investigator     
_FillValue                  @  1X   STATION_PARAMETERS                        	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  1�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       <0..N, 0 : launch cycle (if exists), 1 : first complete cycle   
_FillValue         ��        1�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    1�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    1�   DATE_CREATION                  comment       Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    1�   DATE_UPDATE                	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    1�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     1�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    2   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    2   INST_REFERENCE                    	long_name         Instrument type    conventions       Brand, type, serial number     
_FillValue                  @  2   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    2\   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
_FillValue        A.�~            2`   JULD_QC                	long_name         Quality on Date and Time   conventions       Argo reference table 2     
_FillValue                    2h   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
_FillValue        A.�~            2l   LATITUDE               	long_name         &Latitude of the station, best estimate     units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�             2t   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�             2|   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    2�   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    2�   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    2�   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    2�   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    2�   PRES         	      	   	long_name         SEA PRESSURE   
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    comment       $In situ measurement, sea surface = 0   C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���       2�   PRES_QC          	         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  H  3�   PRES_ADJUSTED            	      	   	long_name         SEA PRESSURE   
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    comment       $In situ measurement, sea surface = 0   C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���       4    PRES_ADJUSTED_QC         	         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  H  5   PRES_ADJUSTED_ERROR          	         	long_name         SEA PRESSURE   
_FillValue        G�O�   units         decibar    comment       WContains the error on the adjusted values as determined by the delayed mode QC process.    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���       5d   TEMP         	      	   	long_name         $SEA TEMPERATURE IN SITU ITS-90 SCALE   
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      comment       In situ measurement    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o       6�   TEMP_QC          	         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  H  7�   TEMP_ADJUSTED            	      	   	long_name         $SEA TEMPERATURE IN SITU ITS-90 SCALE   
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      comment       In situ measurement    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o       7�   TEMP_ADJUSTED_QC         	         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  H  9    TEMP_ADJUSTED_ERROR          	         	long_name         $SEA TEMPERATURE IN SITU ITS-90 SCALE   
_FillValue        G�O�   units         degree_Celsius     comment       WContains the error on the adjusted values as determined by the delayed mode QC process.    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o       9H   PSAL         	      	   	long_name         PRACTICAL SALINITY     
_FillValue        G�O�   units         psu    	valid_min                	valid_max         B(     comment       In situ measurement    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o       :d   PSAL_QC          	         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  H  ;�   PSAL_ADJUSTED            	      	   	long_name         PRACTICAL SALINITY     
_FillValue        G�O�   units         psu    	valid_min                	valid_max         B(     comment       In situ measurement    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o       ;�   PSAL_ADJUSTED_QC         	         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  H  <�   PSAL_ADJUSTED_ERROR          	         	long_name         PRACTICAL SALINITY     
_FillValue        G�O�   units         psu    comment       WContains the error on the adjusted values as determined by the delayed mode QC process.    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o       =,   	PARAMETER            
                	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  >H   SCIENTIFIC_CALIB_EQUATION            
               	long_name         'Calibration equation for this parameter    
_FillValue                    >x   SCIENTIFIC_CALIB_COEFFICIENT         
               	long_name         *Calibration coefficients for this equation     
_FillValue                    Ax   SCIENTIFIC_CALIB_COMMENT         
               	long_name         .Comment applying to this parameter calibration     
_FillValue                    Dx   CALIBRATION_DATE         
               
_FillValue                  ,  Gx   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    G�   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    G�   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    G�   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    G�   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  G�   HISTORY_DATE                     	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    G�   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    H   HISTORY_PARAMETER                         	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    H   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         H   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         H   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        H    HISTORY_QCTEST                        	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    H$Argo profile    2.2 1.2 19500101000000  5900461 US ARGO PROJECT                                                 GREGORY C. JOHNSON                                              PRES            TEMP            PSAL               6A   AO  20051219101547  20060715094731  0682_27623_054                  2C  D   APEX_SBE_1234                                                   846 @���  1   @����5� @(�    �c׍@   1   ARGOS   A   A   A   @�ff@���A   Ah  A���A�  A�  BffB��B133BE��BZffBn��B�ffB�ffB�ffB���B���B���B�33B�ffB���Bڙ�B���B���B�  C� C�CffCL�C#�C+  C4��C>��CHffCRffC]L�Ch  Ct�C��C�33C��C��C�33C�  C��fC�@ C��3C�� C��Cތ�C�  C�  D��DfD�DFfD3D&S3D.ٚD7�3DAL�DK�DU�3D`��Dl  Dx  D�Y�D��D�fD�&f11111111111111111111111111111111111111111111111111111111111111111111111 @�33@ə�AffAfffA�  A�33A�33B  BfgB0��BE34BZ  BnfgB�33B�33B�33B�fgB���B�fgB�  B�33BЙ�B�fgB晚B�B���CffC  CL�C33C#  C*�fC4� C>� CHL�CRL�C]33Cg�fCt  C�  C�&fC��C�  C�&fC��3C���C�33C��fCɳ3C��Cހ C��3C��3D�4D  D4D@ D�D&L�D.�4D7��DAFgDKgDU��D`�4Dk��Dw��D�VgD��D�3D�#311111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�ȴA�ȴAԼjA���A���A��
A���A��
A��A���A���A��A�x�A�M�A�&�A���A�G�A�C�A���A�`BA���A���AzI�AhjA_��AS�
AO��AK"�AG��AC��AA�A>��A9%A/VA1VA.��A,��A*�!A&z�A$bA"�A"v�A?}AS�A��A��A9XAĜA�A
��AJA��AZ@���@� �@��
@�t�@�5?@ҸR@���@�o@��#@���@��@�|�@���@���@�1@�@��h@�11111111111111111111111111111111111111111111111111111111111111111111111 A�ȴA�ȴAԼjA���A���A��
A���A��
A��A���A���A��A�x�A�M�A�&�A���A�G�A�C�A���A�`BA���A���AzI�AhjA_��AS�
AO��AK"�AG��AC��AA�A>��A9%A/VA1VA.��A,��A*�!A&z�A$bA"�A"v�A?}AS�A��A��A9XAĜA�A
��AJA��AZ@���@� �@��
@�t�@�5?@ҸR@���@�o@��#@���@��@�|�@���@���@�1@�@��h@�11111111111111111111111111111111111111111111111111111111111111111111111 ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB�3B�9B�9B�9B�3B�3B�3B�3B�3B�3B�3B�'B�XB	��B&�B
�VB
+B
+B
VB	�BB	�DB	z�B	^5B	$�B	�B	�B	0!B	dZB	��B	��B
�B
^5B
n�B
D�B
��B
�B
�!B
�-B
�B
��B
��B
�B
��B
��B
��B
�PB
�B
x�B
n�B
cTB
ZB
P�B
J�B
7LB
.B
$�B
�B
�B
�B
�B
�B
�B
�B
�B
#�B
(�B
,B
1'B
6FB
:^B
>w11111111111111111111111111111111111111111111111111111111111111111111111 B�EB�FB�5B�>B�6B�>B�8B�:B�@B�9B�FB�B�\B	��B.>B
��B
0JB

tB
`B	�B	��B	��B	d�B	(tB	$B	yB	1�B	e�B	��B	�rB
�B
_�B
qB
DYB
�LB
��B
��B
�B
��B
�B
��B
��B
�,B
�UB
�B
��B
��B
yDB
oB
c�B
Z�B
QBB
KjB
7�B
.�B
%TB
�B
B
B
B
�B
�B
�B
 	B
$'B
)-B
,RB
1fB
6wB
:�B
>�11111111111111111111111111111111111111111111111111111111111111111111111 ;ě�;ě�;ě�;ě�;ě�;ě�;ě�;ě�;ě�;ě�;ě�;ě�<T��<e`B<#�
<49X<o;�`B;�`B<T��;�`B<t�<t�;�`B<o;�`B;�`B;ě�;ě�;ě�;ě�;ě�;�`B;ě�;ě�;ě�;ě�;ě�;ě�;ě�;ě�;ě�;ě�;ě�;ě�;ě�;ě�;ě�;ě�;ě�;ě�;ě�;ě�;ě�;ě�;ě�;ě�;ě�;ě�;ě�;ě�;ě�;ě�;ě�;ě�;ě�;ě�;ě�;ě�;ě�;ě�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - (REPORTED_SURFACE_PRESSURE) for PADJ                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            PSAL_ADJ corrects CTD thermal lag (CTL) viz. Morison et al,1994,JAOT & effects of pressure adjustments                                                                                                                                                          PADJ REPORTED_SURFACE_PRESSURE =0.1 dbar                                                                                                                                                                                                                        none                                                                                                                                                                                                                                                            CTL alpha = 0.021 & tau = 21 s with error equal to the correction                                                                                                                                                                                               Pressures adjusted using reported surface pressure when warranted.                                                                                                                                                                                              none                                                                                                                                                                                                                                                            Salinity corrected for pressure sensor calibration drift and cell thermal lag                                                                                                                                                                                                               20060710143447  AO  ARGQ                                                                        20051219101547  QCP$                G�O�G�O�G�O�FFBFE           AO  ARGQ                                                                        20051219101547  QCF$                G�O�G�O�G�O�0               PM  ARSQPADJV1.0                                                                20060127112119  QC  PRES            @�ffD�&f@                  PM  ARSQCTL V1.0                                                                20060127112119  QC  PSAL            @�ffD�&f@                  PM  ARSQSIQCV2.0WOD2001 & Argo                                                  20060714114915  IP                  G�O�G�O�G�O�                