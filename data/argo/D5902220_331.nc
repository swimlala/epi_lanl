CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS   H   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-12-10T09:03:55Z creation      
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
_FillValue                    O        OArgo profile    3.1 1.2 19500101000000  20181210090355  20221107185202  5902220 US ARGO PROJECT                                                 GREGORY C. JOHNSON                                              PRES            TEMP            PSAL              KA   AO  2992                            2C  D   APEX                            4306                            122707                          846 @ؖ�F��1   @ؗ����@2}p��
=�cW"��`B1   ARGOS   Primary sampling: discrete [discrete spot sampling by SBE-41 at reported pressures]                                                                                                                                                                                A   A   A   @���A$��A�ffA�ffA�ffB33B6  BQ��Bn��B���B���B�33B���B�  B�33B�33B�ffC� C� C��C  C#��C+��C4��C>� CH� CRffC]L�Ch33CtffC��C�L�C�33C�L�C��fC�&fC���C�@ C���Cɳ3Cӌ�C޳3C��C�&fDS3D3D�DS3DfD&33D.�3D7�fDAfDK�DU�fD`��Dl  DxfD�\�D�6fD�,�D�|�D�I�D���D���D�)�D¦fD�|�D�fD�)�D홚D�@ 111111111111111111111111111111111111111111111111111111111111111111111111@���AfgA�33A�33A�33B��B4ffBP  Bm33B�  B�  B�ffB�  B�33B�ffB�ffB���C�C�C34C��C#34C+34C434C>�CH�CR  C\�gCg��Ct  C��C��C�  C��C�s3C��3C�fgC��C���Cɀ C�Y�Cހ C��gC��3D9�D��D  D9�D��D&�D.��D7��D@��DJ�3DU��D`� Dk�fDw��D�P D�)�D�  D�p D�<�D�� D�� D��D�D�p D���D��D��D�33111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�O�A�O�A�M�A�M�A�O�A�M�A�O�A�VA�1'A���A��A��A�l�A���A�|�A�ĜA�33A�1A��A���A�$�A���A���A�ZA���A���AyƨAq;dAc`BAX��AJ�yA<�jA/��A&��A�A�A|�AAb@���@��y@���@��@�o@��w@�O�@��y@��9@���@�@���@���@�J@�;d@�G�@��9@��u@��@{t�@q��@d�@YG�@LI�@D(�@<Z@4�D@+�F@"~�@��@�@	�@�111111111111111111111111111111111111111111111111111111111111111111111111A�O�A�O�A�M�A�M�A�O�A�M�A�O�A�VA�1'A���A��A��A�l�A���A�|�A�ĜA�33A�1A��A���A�$�A���A���A�ZA���A���AyƨAq;dAc`BAX��AJ�yA<�jA/��A&��A�A�A|�AAb@���@��y@���@��@�o@��w@�O�@��y@��9@���@�@���@���@�J@�;d@�G�@��9@��u@��@{t�@q��@d�@YG�@LI�@D(�@<Z@4�D@+�F@"~�@��@�@	�@�111111111111111111111111111111111111111111111111111111111111111111111111;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB
.B
/B
.B
-B
.B
.B
/B
2-B
C�B
J�BXB��B�B%�BB�B>wBe`BbNB:^B'�B�B��BgmBVB
��B
VB

=B	�mB	�B	E�B��B�B~�Bl�Bp�Bo�Bx�Bv�B�7B�uB��B�jB�!B�B��B��B	:^B	Q�B	|�B	�B	�qB	�fB	��B
JB
�B
�B
"�B
5?B
E�B
J�B
S�B
]/B
gmB
o�B
x�B
�B
�=B
�oB
��B
��B
��B
�-111111111111111111111111111111111111111111111111111111111111111111111111B
B
B
B
B
B
B
B
+B
�B
%�B0!B��BǮB��B�B{B;dB9XBbBB�'Bq�B?}B
�mB
r�B
/B	�HB	��B	\)B	�B��B�%BVBD�BG�BG�BO�BM�B`BBjB{�B�{B�+B�+B��B��B	bB	'�B	R�B	�B	�uB	�jB	��B	�NB	�B	�B	��B
DB
�B
 �B
)�B
33B
=qB
E�B
N�B
W
B
`BB
hsB
q�B
v�B
� B
�1111111111111111111111111111111111111111111111111111111111111111111111111?^5?^5?^5?^5?^5?^5?^5?^5?^5?��?��?(�?(�?j?j?j?�?�?j?(�?�m?dZ?"�?��?�?�#?X?�?�u?Q�?��?K�?
=?ȴ?ȴ?ȴ?ȴ?ȴ?ȴ?ȴ?ȴ?ȴ?ȴ?ȴ?ȴ?ȴ?K�?K�?�P?�P?��?��?��?b?b?b?b?b?b?b?b?b?b?Q�?Q�?Q�?Q�?Q�?Q�?Q�?Q�?Q�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - (REPORTED_SURFACE_PRESSURE) for PADJ                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            PSAL_ADJ corrects CTD thermal lag (CTL) viz. Johnson et al, 2007, JAOT, effects of pressure adjustments, and PSAL_ADJUSTED = PSAL + dS, where dS is calculated from a potential conductivity (ref to 0 dbar) multiplicative adjustment term r.                  PADJ REPORTED_SURFACE_PRESSURE =0.4 dbar                                                                                                                                                                                                                        none                                                                                                                                                                                                                                                            CTL alpha = 0.021 & tau = 21 s with error equal to |correction| and for OW r = 0.9989 (+/-0.0155), vertically averaged dS = -0.041 (+/-0.597)                                                                                                                   Pressures adjusted using reported surface pressure when warranted.                                                                                                                                                                                              none                                                                                                                                                                                                                                                            After pressure and cell thermal lag correction of salinity values, OW correction estimated using mapping scales of 8 & 4 long. and 4 & 2 lat., no PV constraint, and decorrelation time scale of 10 years.                                                      202211032326032022110323260320221103232603  AO  ARCAADJP                                                                    20181210090355    IP                G�O�G�O�G�O�                AO  ARCAADJS                                                                    20181210090355    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181210090355  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181210090355  QCF$                G�O�G�O�G�O�0               PM  ARSQPADJV1.1                                                                20190419165758  QC  PRES            @���D�@ G�O�                PM  ARSQCTM V1.1                                                                20190419165758  QC  PSAL            @���D�@ G�O�                PM  ARSQCOWGV1.1CTD_2021v2 + Argo_2021v03                                       20221107185202  IP                  G�O�G�O�G�O�                