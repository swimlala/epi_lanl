CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS   H   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2016-04-06T04:19:02Z creation      
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
_FillValue                     N|         N|Argo profile    3.1 1.2 19500101000000  20160406041902  20221107185132  5902220 US ARGO PROJECT                                                 GREGORY C. JOHNSON                                              PRES            TEMP            PSAL               �A   AO  2992_89216_146                  2C  D   APEX                            846 @֩�{Bu�1   @֩�!/�U@2�33333�cA?|�h1   ARGOS   Primary sampling: discrete [discrete spot sampling by SBE-41 at reported pressures]                                                                                                                                                                                A   A   A   @�ffA(  A�33A�ffA���B��B333BP  BnffB�33B���B�ffB�  B�  B�33B���B�33C  C  CL�C�3C#ffC+L�C4L�C>��CHffCR� C]ffChL�Ct33C��C�@ C�33C�&fC�� C�33C��fC�L�C���C�s3Cӌ�Cޙ�C�@ C�33DL�D�D3D9�D�D&S3D.��D7� DA�DK&fDU��D`�fDlfDx  D�i�D�#3D�,�D�� D�FfD��fD�3D�33D¬�D�|�D�	�D�#3D��3D���111111111111111111111111111111111111111111111111111111111111111111111111@���A#33A���A�  A�fgBfgB2  BN��Bm33B���B�  B���B�ffB�ffB֙�B�33B���C�3C
�3C  CffC#�C+  C4  C>L�CH�CR33C]�Ch  Cs�fC��C��C��C�  C���C��C�� C�&gC�s4C�L�C�fgC�s4C��C��D9�DgD  D&gD��D&@ D.��D7��DAgDK3DU�gD`s3Dk�3Dw��D�` D��D�#3D�vfD�<�D�|�D���D�)�D£3D�s3D�  D��D홙D��3111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�z�A�~�A�~�A̅ÃA̋DȀ\A̋DA̋DA��Aȧ�A��PA�1A��A�`BA��-A�l�A�x�A��/A�
=A��mA��A�5?Ay��Ag�-AW�;AJ��A<��A7"�A-��A$E�A1A�PA1A�A	K�A
=AC�@���@�t�@��@އ+@�Q�@Ȭ@���@�/@��m@���@� �@���@��!@�X@�&�@�1'@�p�@���@�G�@���@��+@{C�@p�9@e�@Y7L@O�w@E`B@=�@5O�@*��@!��@bN@J@(�111111111111111111111111111111111111111111111111111111111111111111111111A�z�A�~�A�~�A̅ÃA̋DȀ\A̋DA̋DA��Aȧ�A��PA�1A��A�`BA��-A�l�A�x�A��/A�
=A��mA��A�5?Ay��Ag�-AW�;AJ��A<��A7"�A-��A$E�A1A�PA1A�A	K�A
=AC�@���@�t�@��@އ+@�Q�@Ȭ@���@�/@��m@���@� �@���@��!@�X@�&�@�1'@�p�@���@�G�@���@��+@{C�@p�9@e�@Y7L@O�w@E`B@=�@5O�@*��@!��@bN@J@(�111111111111111111111111111111111111111111111111111111111111111111111111;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB�-B�-B�9B�3B�3B�-B�-B�3B�-B��B��BH�BdZB�BR�B�B��B�BB��BS�B
�ZB
�B
+B	��B	6FB�NB�fB��B��B��B�\B�hB�oB�hB��B�jB��B��B�BBĜB	%B	  B	$�B	�B	@�B	T�B	cTB	�VB	��B	�dB	��B	�)B	�B	��B
B
\B
�B
�B
)�B
1'B
33B
=qB
E�B
S�B
\)B
ffB
jB
r�B
{�B
�B
�DB
�b111111111111111111111111111111111111111111111111111111111111111111111111B��B��B��B��B��B��B��B��B��B�uB�+B9XBT�B��BE�B	7B�B��B�}BD�B
��B
r�B	��B	�3B	'�B��B�
B�VB�{B�1B}�B� B�B� B�+B�B��B�yB��B�-B��B�B	uB	+B	.B	B�B	P�B	{�B	�1B	��B	��B	ɺB	�#B	�B	�B	��B
%B
	7B
�B
�B
 �B
+B
33B
A�B
I�B
S�B
XB
`BB
iyB
r�B
x�B
}�111111111111111111111111111111111111111111111111111111111111111111111111?(�?(�?(�?(�?(�?(�?(�?(�?(�?(�?�m?dZ?��?�m?dZ?�H?dZ?�m?��?"�?��?�?X?�?Q�?��?��?K�?K�?
=?
=?
=?
=?ȴ?ȴ?
=?ȴ?K�?
=?ȴ?K�?
=?K�?
=?K�?K�?�P?�P?�P?��?��?��?��?b?b?b?b?b?b?b?b?b?b?b?b?b?b?b?b?Q�?Q�?Q�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - (REPORTED_SURFACE_PRESSURE) for PADJ                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            PSAL_ADJ corrects CTD thermal lag (CTL) viz. Johnson et al, 2007, JAOT, effects of pressure adjustments, and PSAL_ADJUSTED = PSAL + dS, where dS is calculated from a potential conductivity (ref to 0 dbar) multiplicative adjustment term r.                  PADJ REPORTED_SURFACE_PRESSURE =0.3 dbar                                                                                                                                                                                                                        none                                                                                                                                                                                                                                                            CTL alpha = 0.021 & tau = 21 s with error equal to |correction| and for OW r = 0.9995 (+/-0.0155), vertically averaged dS = -0.018 (+/-0.597)                                                                                                                   Pressures adjusted using reported surface pressure when warranted.                                                                                                                                                                                              none                                                                                                                                                                                                                                                            After pressure and cell thermal lag correction of salinity values, OW correction estimated using mapping scales of 8 & 4 long. and 4 & 2 lat., no PV constraint, and decorrelation time scale of 10 years.                                                      202211032325542022110323255420221103232554  4306                            122707                          AO  ARCAADJP                                                                    20160406041902    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20160406041902  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20160406041902  QCF$                G�O�G�O�G�O�0               PM  ARSQPADJV1.1                                                                20171129095953  QC  PRES            @�ffD���G�O�                PM  ARSQCTM V1.1                                                                20171129095953  QC  PSAL            @�ffD���G�O�                PM  ARSQCOWGV1.1CTD_2021v2 + Argo_2021v03                                       20221107185132  IP                  G�O�G�O�G�O�                