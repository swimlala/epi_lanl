CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS   H   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2017-06-04T01:33:56Z creation      
references        (http://www.argodatamgt.org/Documentation   comment           user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
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
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     7�   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8,   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�E�vQ�   
_FillValue        A.�~       axis      T           80   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    88   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�E�vQ�   
_FillValue        A.�~            8<   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8D   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8L   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8T   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8X   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8`   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9`   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    9d   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    9h   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    9l   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z           9p   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  H  :�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���        :�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  H  ;�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���        <@   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o        =`   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  H  >�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o        >�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  H  ?�   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o        @0   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o        AP   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  H  Bp   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o        B�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  H  C�   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o        D    	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  E@   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    Ep   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    Hp   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    Kp   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  Np   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
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
_FillValue                    O        OArgo profile    3.1 1.2 19500101000000  20170604013356  20221107185154  5902220 US ARGO PROJECT                                                 GREGORY C. JOHNSON                                              PRES            TEMP            PSAL              A   AO  2992                            2C  D   APEX                            4306                            122707                          846 @�Q���1   @�Q�ZL6@3���E��cx���1   ARGOS   Primary sampling: discrete [discrete spot sampling by SBE-41 at reported pressures]                                                                                                                                                                                A   A   A   @�ffA(  A�33A���A�ffB��B5��BQ33Bo��B���B���B���B�33B�33B�33B�33B�  C� C33C��C�3C#�3C+��C4ffC>  CH� CR�3C]33Ch� CtffC�@ C�Y�C�@ C��C��fC�33C���C�33C�� CɌ�CӀ C�s3C��3C�@ DY�D�DfDL�D  D&S3D.ٚD7ٚDA�DJ��DU� D`� Dl�DxfD�i�D�,�D�33D���D�FfD���D� D�)�D³3D̃3D� D�,�D���D���111111111111111111111111111111111111111111111111111111111111111111111111@�ffA   A�33A���A�ffB��B3��BO33Bm��B���B���B���B�33B�33B�33B�33B�  C  C
�3C�C33C#33C+�C3�fC=� CH  CR33C\�3Ch  Cs�fC�  C��C�  C�ٚC�ffC��3C�L�C��3C�� C�L�C�@ C�33C�3C�  D9�D��D�fD,�D� D&33D.��D7��D@��DJٚDU� D`� Dk��Dw�fD�Y�D��D�#3D�y�D�6fD�|�D�  D��D£3D�s3D�  D��D��D�y�111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A���A�VA�oA��A��A��A�{A�bAʉ7AƮAÉ7A�%A��9A��A�ƨA�ƨA�S�A�9XA�&�A�I�A��/A�{A��-A�A��HA�z�A�(�A�|�Aw�hAe�AK�A>�A4��A+l�A%C�AA�^AȴA��A	"�A5?A(�@�Z@��@�@�J@�9X@� �@�;d@�M�@���@�O�@�^5@�1'@���@��@�%@�v�@��y@�p�@�\)@x��@l9X@b��@Yhs@Ol�@B��@6v�@,Z@"n�@Ĝ@�111111111111111111111111111111111111111111111111111111111111111111111111A���A�VA�oA��A��A��A�{A�bAʉ7AƮAÉ7A�%A��9A��A�ƨA�ƨA�S�A�9XA�&�A�I�A��/A�{A��-A�A��HA�z�A�(�A�|�Aw�hAe�AK�A>�A4��A+l�A%C�AA�^AȴA��A	"�A5?A(�@�Z@��@�@�J@�9X@� �@�;d@�M�@���@�O�@�^5@�1'@���@��@�%@�v�@��y@�p�@�\)@x��@l9X@b��@Yhs@Ol�@B��@6v�@,Z@"n�@Ĝ@�111111111111111111111111111111111111111111111111111111111111111111111111;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB	T�B	S�B	T�B	VB	VB	T�B	bNB	�JB	��Be`B�=B��B�B[#B@�Br�BffB7LBG�B��BiyB�B��B��B�B1'B
�-B
�B	�-B	0!B��B~�B�7B��B��B	1B	bB	1'B	.B	H�B	M�B	m�B	� B	�DB	��B	�dB	�LB	�RB	�wB	ƨB	�)B	�B	�B
B
�B
�B
#�B
"�B
%�B
,B
6FB
B�B
I�B
Q�B
YB
bNB
l�B
v�B
� B
�7B
�uB
��111111111111111111111111111111111111111111111111111111111111111111111111B	.B	.B	.B	0!B	/B	/B	<jB	ffB	�B?}BdZB�B�B5?B�BL�BA�BhB!�Bv�BD�B�B�
B�B^5BVB
�\B	��B	�\B	\Bs�B[#Be`B�B��B�TB�B	JB		7B	#�B	'�B	H�B	ZB	e`B	�B	��B	�hB	�oB	��B	��B	�FB	ŢB	��B	�;B	�B	��B	��B	��B
  B
%B
bB
�B
#�B
,B
33B
<jB
F�B
P�B
ZB
cTB
m�B
w�111111111111111111111111111111111111111111111111111111111111111111111111?X?X?X?X?X?X?X?��?�#?��?�m?(�?j?�?�?�?�?j?j?�?�?(�?�m?�m?dZ?�H?^5?X?��?Q�?K�?
=?
=?K�?K�?�P?�P?�P?�P?�P?�P?��?��?��?��?b?��?��?��?��?b?b?b?b?b?b?b?b?b?b?b?b?b?b?b?b?b?Q�?Q�?Q�?Q�?Q�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - (REPORTED_SURFACE_PRESSURE) for PADJ                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            PSAL_ADJ corrects CTD thermal lag (CTL) viz. Johnson et al, 2007, JAOT, effects of pressure adjustments, and PSAL_ADJUSTED = PSAL + dS, where dS is calculated from a potential conductivity (ref to 0 dbar) multiplicative adjustment term r.                  PADJ REPORTED_SURFACE_PRESSURE =0.5 dbar                                                                                                                                                                                                                        none                                                                                                                                                                                                                                                            CTL alpha = 0.021 & tau = 21 s with error equal to |correction| and for OW r = 0.999 (+/-0.0155), vertically averaged dS = -0.038 (+/-0.598)                                                                                                                    Pressures adjusted using reported surface pressure when warranted.                                                                                                                                                                                              none                                                                                                                                                                                                                                                            After pressure and cell thermal lag correction of salinity values, OW correction estimated using mapping scales of 8 & 4 long. and 4 & 2 lat., no PV constraint, and decorrelation time scale of 10 years.                                                      202211032326002022110323260020221103232600  AO  ARCAADJP                                                                    20170604013356    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20170604013356  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20170604013356  QCF$                G�O�G�O�G�O�0               PM  ARSQPADJV1.1                                                                20171129100436  QC  PRES            @�ffD���G�O�                PM  ARSQCTM V1.1                                                                20171129100436  QC  PSAL            @�ffD���G�O�                PM  ARSQCOWGV1.1CTD_2021v2 + Argo_2021v03                                       20221107185154  IP                  G�O�G�O�G�O�                