CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS   B   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2016-04-05T22:07:41Z creation      
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
_FillValue                     M�         M�Argo profile    3.1 1.2 19500101000000  20160405220741  20191008173755  5901866 US ARGO PROJECT                                                 GREGORY C. JOHNSON                                              PRES            TEMP            PSAL               �A   AO  2530_37205_139                  2C  D   APEX                            846 @�B��31   @�BJ�h,@-�z�G��d��+1   ARGOS   Primary sampling: discrete [discrete spot sampling by SBE-41 at reported pressures]                                                                                                                                                                                A   A   A   @���A   A���A���A�ffB��B4��BR  Bl��B���B�33B���B�33B���B�  B晚B���CL�C�3C��C  C#�3C+��C4L�C>ffCH�CRL�C\�fCh�Ct33C��C�  C��3C�  C�L�C��C���C�ٚC��fCə�C�L�C�L�C�  C��DL�D  D�3D` DfD&9�D.� D7�3DA  DK  DU��D`y�Dl�Dw� D�VfD�  D��D���D�L�D�y�D�  D�&f111111111111111111111111111111111111111111111111111111111111111111  @���A   A���A���A�ffB��B4��BR  Bl��B���B�33B���B�33B���B�  B晚B���CL�C�3C��C  C#�3C+��C4L�C>ffCH�CRL�C\�fCh�Ct33C��C�  C��3C�  C�L�C��C���C�ٚC��fCə�C�L�C�L�C�  C��DL�D  D�3D` DfD&9�D.� D7�3DA  DK  DU��D`y�Dl�Dw� D�VfD�  D��D���D�L�D�y�D�  D�&f222222222222222222222222222222222222222222222222222222222222222222  A�  A�  A�  A�  A�  A�  A�  A�  A�  A�  A�  A�  A�  A�  A�  A�  A�  A�  A�  A�  A�  A�  A�  A�  A�  A�  A�  A�  A�  A�  A�  A�  A�  A�  A�  A�  A�  A�  A�  A�  A�  A�  A�  A�  A�  A�  A�  A�  A�  A�  A�  A�  A�  A�  A�  A�  A�  A�  A�  A�  A�  A�  A�  A�  A�  A�  A�bA�"�A�"�A��A�(�A�/A�;dA�9XA�O�A�XA�Q�A��`A�^5A�  A�|�A���A��Aĕ�A���A���A�E�A�9XA���A��yA��`A��PA�ȴAvJAd~�AL��AB��A9�;A#�-A�7A��A-A�T@�@���@�x�@�h@�&�@��@�A�@�{@͙�@�J@��`@�x�@�\)@��@�/@�bN@��@��7@��\@���@�S�@�E�@�p�@x�u@g
=@X  @I%@@��@=�h111111111111111111111111111111111111111111111111111111111111111111  A�bA�"�A�"�A��A�(�A�/A�;dA�9XA�O�A�XA�Q�A��`A�^5A�  A�|�A���A��Aĕ�A���A���A�E�A�9XA���A��yA��`A��PA�ȴAvJAd~�AL��AB��A9�;A#�-A�7A��A-A�T@�@���@�x�@�h@�&�@��@�A�@�{@͙�@�J@��`@�x�@�\)@��@�/@�bN@��@��7@��\@���@�S�@�E�@�p�@x�u@g
=@X  @I%@@��@=�h222222222222222222222222222222222222222222222222222222222222222222  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB}�B~�B}�Bz�B{�B}�B~�B� B�B�+B�+B�hB9XB�\B��B��B�3B�!B�BhsB�^B~�B
=B��B0!B
iyB
K�B
  B	��B	8RB	,B	hB�HB��B�B�sB��B	E�B	R�B	S�B	q�B	��B	��B	�B	�B	ĜB	�BB	�yB	�B	��B
+B
	7B
\B
{B
�B
$�B
)�B
49B
9XB
>wB
D�B
N�B
W
B
_;B
dZB
gm111111111111111111111111111111111111111111111111111111111111111111  Bm�Bn�Bm�BjBk�Bm�Bn�Bo�Bt�Bv�Bv�B�B)�B� B�oB�bB��B��B��BZB�Bs�B��B��B&�B
ZB
?}B	�B	��B	+B	�B	B��BƨB��B�B�B	6FB	C�B	D�B	bNB	�{B	�bB	��B	��B	�?B	��B	�B	�;B	�B	��B	��B
  B
B
\B
�B
�B
#�B
(�B
.B
5?B
?}B
G�B
N�B
S�B
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - (REPORTED_SURFACE_PRESSURE) for PADJ                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            PSAL_ADJ corrects CTD thermal lag (CTL) viz. Johnson et al, 2007, JAOT, effects of pressure adjustments, and PSAL_ADJUSTED = PSAL + dS, where dS is calculated from a potential conductivity (ref to 0 dbar) multiplicative adjustment term r.                  PADJ REPORTED_SURFACE_PRESSURE =0 dbar                                                                                                                                                                                                                          none                                                                                                                                                                                                                                                            CTL alpha = 0.021 & tau = 21 s with error equal to |correction| and for OW r = 0.9996 (+/-0), vertically averaged dS = -0.016 (+/-0)                                                                                                                            Pressures adjusted using reported surface pressure when warranted.  TNPD: APEX float that truncated negative pressure drift.                                                                                                                                    none                                                                                                                                                                                                                                                            After pressure and cell thermal lag correction of salinity values, OW correction estimated using mapping scales of 8 & 4 long. and 4 & 2 lat., no PV constraint, and decorrelation time scale of 10 years.                                                      201909231626262019092316262620190923162626  3582                            052405                          AO  ARCAADJP                                                                    20160405220741    IP                G�O�G�O�G�O�                AO  ARCAADJS                                                                    20160405220741    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20160405220741  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20160405220741  QCF$                G�O�G�O�G�O�0               PM  ARSQPADJV1.1                                                                20171129091940  QC  PRES            @���D�&fG�O�                PM  ARSQCTM V1.1                                                                20171129091940  QC  PSAL            @���D�&fG�O�                PM  ARSQOWGUV1.0                                                                20191008173755  IP                  G�O�G�O�G�O�                