CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS   B   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2016-04-05T22:07:40Z creation      
references        (http://www.argodatamgt.org/Documentation   comment           user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      
_FillValue               conventions       Argo reference table 1          6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    6�   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  6�   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7(   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7h   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      
_FillValue               conventions       Argo reference table 23          7�   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    7�   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
_FillValue        A.�~       axis      T      
resolution        >�E�vQ�        7�   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    7�   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
_FillValue        A.�~       
resolution        >�E�vQ�        7�   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8    CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9    PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    9$   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    9(   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    9,   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z          90   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  D  :8   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���       :|   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  D  ;�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���       ;�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o       <�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  D  =�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o       >   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  D  ?$   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o       ?h   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o       @p   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  D  Ax   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o       A�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  D  B�   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o       C   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  D   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    D@   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    G@   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    J@   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    
_FillValue               conventions       YYYYMMDDHHMISS        ,  M@   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    M�   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    M�   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    M�   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    M�   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  M�   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    M�   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    N   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    N   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         N    HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         N$   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        N(   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    N,   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     Ml   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     M�         M�Argo profile    3.1 1.2 19500101000000  20160405220740  20191008173754  5901866 US ARGO PROJECT                                                 GREGORY C. JOHNSON                                              PRES            TEMP            PSAL               �A   AO  2530_37205_136                  2C  D   APEX                            846 @�:
�ǂ�1   @�:"�?2@-�^5?|��d�ȴ9X1   ARGOS   Primary sampling: discrete [discrete spot sampling by SBE-41 at reported pressures]                                                                                                                                                                                A   A   A   @�  A!��A�ffA�  A�  B��B5��BQ33Bm33B�  B�ffB�33B���B�33B���B晚B�33C�3C� C�CffC#  C+L�C4ffC>�CH33CRffC]�Ch33Ct�C�3C��C��fC���C�ffC��C�@ C�� C�L�CɦfC�L�CަfC��3C��3D,�DfD  DL�D3D&@ D.�fD7ٚDA�DK  DUy�D`�3Dl�DxfD�i�D�0 D�,�D���D�I�D���D���D���111111111111111111111111111111111111111111111111111111111111111111  @�  A!��A�ffA�  A�  B��B5��BQ33Bm33B�  B�ffB�33B���B�33B���B晚B�33C�3C� C�CffC#  C+L�C4ffC>�CH33CRffC]�Ch33Ct�C�3C��C��fC���C�ffC��C�@ C�� C�L�CɦfC�L�CަfC��3C��3D,�DfD  DL�D3D&@ D.�fD7ٚDA�DK  DUy�D`�3Dl�DxfD�i�D�0 D�,�D���D�I�D���D���D���222222222222222222222222222222222222222222222222222222222222222222  A�  A�  A�  A�  A�  A�  A�  A�  A�  A�  A�  A�  A�  A�  A�  A�  A�  A�  A�  A�  A�  A�  A�  A�  A�  A�  A�  A�  A�  A�  A�  A�  A�  A�  A�  A�  A�  A�  A�  A�  A�  A�  A�  A�  A�  A�  A�  A�  A�  A�  A�  A�  A�  A�  A�  A�  A�  A�  A�  A�  A�  A�  A�  A�  A�  A�  A��A�{A�bA��A�  A��A��A��`A̍PA�hsA�G�A��AǇ+A�"�A�$�A���A�ƨA�r�A���A��A�+A���A���A�&�A�ffA���A���Ah�A\r�AP(�AAS�A:-A*�DA�A�
AK�A��@�O�@��9@��7@��T@��@�@�V@�G�@̋D@�V@�^5@��@�G�@��+@���@��+@���@�ƨ@��@��;@�X@���@z�@s�
@l��@d1@\9X@K33@@  111111111111111111111111111111111111111111111111111111111111111111  A��A�{A�bA��A�  A��A��A��`A̍PA�hsA�G�A��AǇ+A�"�A�$�A���A�ƨA�r�A���A��A�+A���A���A�&�A�ffA���A���Ah�A\r�AP(�AAS�A:-A*�DA�A�
AK�A��@�O�@��9@��7@��T@��@�@�V@�G�@̋D@�V@�^5@��@�G�@��+@���@��+@���@�ƨ@��@��;@�X@���@z�@s�
@l��@d1@\9X@K33@@  222222222222222222222222222222222222222222222222222222222222222222  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB
�/B
�/B
�)B
�/B
�#B
�B
�B
�B
��B
��B
�BJ�BXB��B��B��B��B�`B�BƨB��B`BB�B��B%�B
�B
jB	ȴB	}�B	H�B	'�B	�B	�B	VB�B��B�
B��B	A�B	aHB	q�B	�uB	��B	��B	�dB	��B	�BB	��B	��B
B
hB
�B
�B
�B
!�B
)�B
/B
49B
;dB
A�B
F�B
K�B
Q�B
S�B
`BB
ff111111111111111111111111111111111111111111111111111111111111111111  B
��B
��B
��B
��B
��B
ɺB
ɺB
ɺB
ÖB
ÖB
��B:^BH�B�1B�uB�hB�1B�B�`B�^B�{BS�B�TBĜB�B
��B
aHB	�jB	q�B	<jB	�B	hB	oB	B��B�qBȴB�`B	2-B	Q�B	cTB	�B	�VB	��B	�B	�}B	��B	�`B	�B	��B
B
1B

=B
bB
oB
�B
�B
$�B
,B
2-B
7LB
<jB
B�B
D�B
P�B
W
222222222222222222222222222222222222222222222222222222222222222222  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - (REPORTED_SURFACE_PRESSURE) for PADJ                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            PSAL_ADJ corrects CTD thermal lag (CTL) viz. Johnson et al, 2007, JAOT, effects of pressure adjustments, and PSAL_ADJUSTED = PSAL + dS, where dS is calculated from a potential conductivity (ref to 0 dbar) multiplicative adjustment term r.                  PADJ REPORTED_SURFACE_PRESSURE =0 dbar                                                                                                                                                                                                                          none                                                                                                                                                                                                                                                            CTL alpha = 0.021 & tau = 21 s with error equal to |correction| and for OW r = 0.9996 (+/-0), vertically averaged dS = -0.015 (+/-0)                                                                                                                            Pressures adjusted using reported surface pressure when warranted.  TNPD: APEX float that truncated negative pressure drift.                                                                                                                                    none                                                                                                                                                                                                                                                            After pressure and cell thermal lag correction of salinity values, OW correction estimated using mapping scales of 8 & 4 long. and 4 & 2 lat., no PV constraint, and decorrelation time scale of 10 years.                                                      201909231626262019092316262620190923162626  3582                            052405                          AO  ARCAADJP                                                                    20160405220740    IP                G�O�G�O�G�O�                AO  ARCAADJS                                                                    20160405220740    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20160405220740  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20160405220740  QCF$                G�O�G�O�G�O�0               PM  ARSQPADJV1.1                                                                20171129091939  QC  PRES            @�  D���G�O�                PM  ARSQCTM V1.1                                                                20171129091939  QC  PSAL            @�  D���G�O�                PM  ARSQOWGUV1.0                                                                20191008173754  IP                  G�O�G�O�G�O�                