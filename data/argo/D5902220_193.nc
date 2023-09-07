CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS   H   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2014-11-29T23:00:34Z creation      
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
resolution        =���   axis      Z           90   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  H  :P   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���        :�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  H  ;�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���        <    TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o        =    TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  H  >@   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o        >�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  H  ?�   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o        ?�   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o        A   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  H  B0   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o        Bx   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  H  C�   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o        C�   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  E    SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    E0   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    H0   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    K0   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    
_FillValue               conventions       YYYYMMDDHHMISS        ,  N0   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    N�   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    N�   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    N�   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    N�   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  N�   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    N�   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    N�   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    O    HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         O   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         O   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        O   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    O   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     N\   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     N|         N|Argo profile    3.1 1.2 19500101000000  20141129230034  20221107185140  5902220 US ARGO PROJECT                                                 GREGORY C. JOHNSON                                              PRES            TEMP            PSAL               �A   AO  2992_89216_193                  2C  D   APEX                            846 @�&��U@ 1   @�&��� @26ȴ9X�cLj~��#1   ARGOS   Primary sampling: discrete [discrete spot sampling by SBE-41 at reported pressures]                                                                                                                                                                                A   A   A   @���A)��A���A�33A���B  B4��BR  BlffB�ffB�  B�33B���B�  B�  B�  B���C� CffC� CL�C#��C+33C4L�C>�3CH��CRffC]ffChL�CtffC�  C�@ C�33C��C�ٚC�&fC���C��C��3CɦfCӌ�Cޙ�C�L�C�33DL�D  DfDFfD  D&` D.� D7��DAfDK  DUy�D`�3Dl  Dx�D�i�D�)�D�33D��3D�FfD��fD��D�#3D¬�D̀ D�fD�)�D��fD��111111111111111111111111111111111111111111111111111111111111111111111111@�  A$��A�fgA���A�fgB��B3��BP��Bk33B���B�ffB���B�  B�ffB�ffB�ffB�33C33C�C33C  C#L�C*�fC4  C>ffCHL�CR�C]�Ch  Ct�C�3C��C��C��4C��4C�  C�s4C��gC���Cɀ C�fgC�s4C�&gC��D9�D��D�3D33D�D&L�D.��D7�gD@�3DJ��DUfgD`� Dl�DxgD�` D�  D�)�D�y�D�<�D�|�D�3D��D£3D�vfD���D�  D��D� 111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�=qA�A�A�7LA�33A�1'A�$�A�&�A�-A�(�A�+A�+A�33A�G�A�ZA��PA�
=A�x�A��A��DA�r�A�XA��A�bA���A��+A��7A�33A�ZAx��Af��AX�jA@�HA0VA-;dA&�\A"r�A%��AA�wA�Aj@�/@��@�&�@�9@�Ĝ@�5?@�@�n�@�n�@���@�1@���@�O�@���@�hs@�%@���@�l�@��@���@sdZ@i�7@`Ĝ@U�@J��@>�R@6E�@,��@!�@�@�111111111111111111111111111111111111111111111111111111111111111111111111A�=qA�A�A�7LA�33A�1'A�$�A�&�A�-A�(�A�+A�+A�33A�G�A�ZA��PA�
=A�x�A��A��DA�r�A�XA��A�bA���A��+A��7A�33A�ZAx��Af��AX�jA@�HA0VA-;dA&�\A"r�A%��AA�wA�Aj@�/@��@�&�@�9@�Ĝ@�5?@�@�n�@�n�@���@�1@���@�O�@���@�hs@�%@���@�l�@��@���@sdZ@i�7@`Ĝ@U�@J��@>�R@6E�@,��@!�@�@�111111111111111111111111111111111111111111111111111111111111111111111111;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oBŢBȴBȴBȴBɺB��B��B�BB��B��BB+B�
B{BL�B5?BB�B�BjBaHBm�BgmBk�Bn�B��B\)B
��B
bNB	�B	\)B	VB��B	1B	2-B	��B	�LB
!�B	�B	�B	��B	o�B	r�B	�%B	��B	��B	��B	�dB	��B	��B	��B	�`B	�B	�B	��B	�sB	��B
B
�B
�B
&�B
49B
@�B
I�B
Q�B
ZB
cTB
m�B
t�B
x�B
�B
�\B
��111111111111111111111111111111111111111111111111111111111111111111111111B��B��B��B��B�B�B�!B��B�B�BB�`B�B�qB��B1'B�B(�B�qBN�BD�BP�BJ�BN�BP�B��BA�B
�'B
G�B	�jB	A�B��B�FB�B	�B	x�B	��B
B	�
B	�\B	�7B	R�B	VB	hsB	�B	�JB	~�B	��B	��B	�B	�LB	ǮB	��B	��B	�B	��B	�;B	�fB	��B
  B
	7B
�B
"�B
,B
49B
<jB
E�B
O�B
W
B
[#B
gmB
q�B
{�111111111111111111111111111111111111111111111111111111111111111111111111?(�?(�?(�?(�?(�?(�?(�?j?j?j?j?�?(�?j?�?j?�?�m?dZ?dZ?dZ?dZ?dZ?"�?��?"�?^5?�#?�?Q�?b?�P?�P?��?Q�?Q�?��?�u?b?b?��?��?��?��?b?��?b?b?b?b?b?b?b?b?��?b?b?b?b?b?b?b?b?b?b?b?b?Q�?b?Q�?Q�?Q�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - (REPORTED_SURFACE_PRESSURE) for PADJ                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            PSAL_ADJ corrects CTD thermal lag (CTL) viz. Johnson et al, 2007, JAOT, effects of pressure adjustments, and PSAL_ADJUSTED = PSAL + dS, where dS is calculated from a potential conductivity (ref to 0 dbar) multiplicative adjustment term r.                  PADJ REPORTED_SURFACE_PRESSURE =0.3 dbar                                                                                                                                                                                                                        none                                                                                                                                                                                                                                                            CTL alpha = 0.021 & tau = 21 s with error equal to |correction| and for OW r = 0.9992 (+/-0.0155), vertically averaged dS = -0.029 (+/-0.6)                                                                                                                     Pressures adjusted using reported surface pressure when warranted.                                                                                                                                                                                              none                                                                                                                                                                                                                                                            After pressure and cell thermal lag correction of salinity values, OW correction estimated using mapping scales of 8 & 4 long. and 4 & 2 lat., no PV constraint, and decorrelation time scale of 10 years.                                                      202211032325562022110323255620221103232556  4306                            122707                          AO  ARCAADJP                                                                    20141129230034    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20141129230034  QCP$                G�O�G�O�G�O�DFB7E           AO  ARGQQCPL                                                                    20141129230034  QCF$                G�O�G�O�G�O�0                                                                                                                                   G�O�G�O�G�O�                PM  ARSQPADJV1.1                                                                20171129100134  QC  PRES            @���D��G�O�                PM  ARSQCTM V1.1                                                                20171129100134  QC  PSAL            @���D��G�O�                PM  ARSQCOWGV1.1CTD_2021v2 + Argo_2021v03                                       20221107185140  IP                  G�O�G�O�G�O�                