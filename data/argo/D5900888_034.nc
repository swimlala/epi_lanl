CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS   H   N_CALIB       	N_HISTORY                     <   	DATA_TYPE                  comment       	Data type      
_FillValue                    0�   FORMAT_VERSION                 comment       File format version    
_FillValue                    0�   HANDBOOK_VERSION               comment       Data handbook version      
_FillValue                    0�   REFERENCE_DATE_TIME                 comment       !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    1    PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    1   PROJECT_NAME                  comment       Name of the project    
_FillValue                  @  1   PI_NAME                   comment       "Name of the principal investigator     
_FillValue                  @  1X   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  1�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       <0..N, 0 : launch cycle (if exists), 1 : first complete cycle   
_FillValue         ��        1�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    1�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    1�   DATE_CREATION                   comment       Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    1�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    1�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     1�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    2   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    2   INST_REFERENCE                    	long_name         Instrument type    conventions       Brand, type, serial number     
_FillValue                  @  2   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    2\   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
_FillValue        A.�~            2`   JULD_QC                	long_name         Quality on Date and Time   conventions       Argo reference table 2     
_FillValue                    2h   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
_FillValue        A.�~            2l   LATITUDE               	long_name         &Latitude of the station, best estimate     units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�             2t   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�             2|   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    2�   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    2�   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    2�   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    2�   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    2�   PRES         
      	   	long_name         SEA PRESSURE   
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    comment       $In situ measurement, sea surface = 0   C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���        2�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  H  3�   PRES_ADJUSTED            
      	   	long_name         SEA PRESSURE   
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    comment       $In situ measurement, sea surface = 0   C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���        4   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  H  5$   PRES_ADJUSTED_ERROR          
         	long_name         SEA PRESSURE   
_FillValue        G�O�   units         decibar    comment       WContains the error on the adjusted values as determined by the delayed mode QC process.    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���        5l   TEMP         
      	   	long_name         $SEA TEMPERATURE IN SITU ITS-90 SCALE   
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      comment       In situ measurement    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o        6�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  H  7�   TEMP_ADJUSTED            
      	   	long_name         $SEA TEMPERATURE IN SITU ITS-90 SCALE   
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      comment       In situ measurement    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o        7�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  H  9   TEMP_ADJUSTED_ERROR          
         	long_name         $SEA TEMPERATURE IN SITU ITS-90 SCALE   
_FillValue        G�O�   units         degree_Celsius     comment       WContains the error on the adjusted values as determined by the delayed mode QC process.    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o        9\   PSAL         
      	   	long_name         PRACTICAL SALINITY     
_FillValue        G�O�   units         psu    	valid_min                	valid_max         B(     comment       In situ measurement    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o        :|   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  H  ;�   PSAL_ADJUSTED            
      	   	long_name         PRACTICAL SALINITY     
_FillValue        G�O�   units         psu    	valid_min                	valid_max         B(     comment       In situ measurement    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o        ;�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  H  =   PSAL_ADJUSTED_ERROR          
         	long_name         PRACTICAL SALINITY     
_FillValue        G�O�   units         psu    comment       WContains the error on the adjusted values as determined by the delayed mode QC process.    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o        =L   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  >l   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    >�   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    A�   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    D�   CALIBRATION_DATE            	             
_FillValue                  ,  G�   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    G�   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    G�   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    G�   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    G�   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  G�   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    H   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    H(   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    H,   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         H<   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         H@   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        HD   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    HHArgo profile    2.2 1.2 19500101000000  5900888 US ARGO PROJECT                                                 GREGORY C. JOHNSON                                              PRES            TEMP            PSAL               "A   AO  20060508101802  20061211111058  0998_54093_034                  2C  D   APEX_SBE_1780                                                   846 @�*�� 1   @�*�@ @6K�   �cu��   1   ARGOS   A   A   A   @���AffA�ffA�33A�33BffB5��BQ33Bm33B�33B���B�ffB�ffBƙ�B���B晚B�33CL�CffC��CffC#ffC+ffC4�C>L�CH��CR33C]�ChL�Ct  C�33C�&fC�&fC��C��fC��C��fC��C��fCɳ3CӦfCަfC�&fC��3D` D  D�D9�D  D&Y�D.ٚD7ٚDA�DK3DU� D`� Dl  Dx�D�i�D�,�D�&fD�� D�I�D�� D�	�D�#3D¬�D̃3D��D�#3D홚D�  111111111111111111111111111111111111111111111111111111111111111111111111@���A  A~ffA�  A�  B��B2  BM��Bi��B�ffB���B���B���B���B�  B���B�ffCfgC
� C�4C� C"� C*� C334C=fgCG�4CQL�C\34CgfgCs�C� C��3C��3C��gC�33C���C�33C���C�33C�@ C�33C�33C�3C�� D&fD�fD�3D  D�fD&  D.� D7� D@� DJٙDUffD`ffDk�fDw�3D�L�D� D�	�D�c3D�,�D�c3D���D�fD D�ffD�� D�fD�|�D��3111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A���A���A��!A��!A��A���A�$�A�&�A��hA�ƨA��RA�|�A���A���A���A���A��A��^A��A��mA�JA�(�A�;dA�v�A�ƨA���As�TAix�Aa�AXĜAP�AGƨA8�A/|�A)`BA"�HAE�A��A%A	�FA�7A Z@��@�^@�o@��@ش9@�V@�b@�@��@�@��w@��@��F@�o@�{@�\)@�n�@��H@l�@t�j@j-@aX@U�@G\)@?l�@4j@)7L@ �u@r�@��111111111111111111111111111111111111111111111111111111111111111111111111A���A���A��!A��!A��A���A�$�A�&�A��hA�ƨA��RA�|�A���A���A���A���A��A��^A��A��mA�JA�(�A�;dA�v�A�ƨA���As�TAix�Aa�AXĜAP�AGƨA8�A/|�A)`BA"�HAE�A��A%A	�FA�7A Z@��@�^@�o@��@ش9@�V@�b@�@��@�@��w@��@��F@�o@�{@�\)@�n�@��H@l�@t�j@j-@aX@U�@G\)@?l�@4j@)7L@ �u@r�@��111111111111111111111111111111111111111111111111111111111111111111111111;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oBH�BG�BG�BG�BG�BC�BM�B�7B�PB�\B�-B��B��B�B�B�fB�5B�wBk�BE�B�B;dB#�B
�RB
��B
	7B	��B	YB	$�B	1'B	{B�B~�B�7B��B��B�!B�9B�'B��B��B�yB��B	DB	{B	:^B	0!B	=qB	YB	v�B	�VB	��B	�wB	��B	�)B	�yB	��B	��B
1B
bB
�B
 �B
+B
2-B
=qB
D�B
N�B
R�B
]/B
ffB
o�B
x�111111111111111111111111111111111111111111111111111111111111111111111111BH�BG�BG�BG�BHBF<BO�B�B��B�[B�B�(B�B�B�"B�0B߽B��Bl�BH;B��B<�B(PB
��B
��B
0B	��B	[B	':B	3iB	�B��B�tB��B�VB��B�pB��B��B�oB�}B�rB��B	�B	*B	;MB	1B	>iB	Y�B	w�B	�B	��B	�>B	ӡB	��B	�&B	�sB	��B
�B
B
%B
!fB
+�B
2�B
>B
E0B
OrB
S�B
]�B
f�B
p+B
yQ111111111111111111111111111111111111111111111111111111111111111111111111<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - (REPORTED_SURFACE_PRESSURE) for PADJ                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            PSAL_ADJ corrects Cnd. Thermal Mass (CTM), Johnson et al,2007,JAOT & effects of pressure adjustments                                                                                                                                                            PADJ REPORTED_SURFACE_PRESSURE =0.9 dbar                                                                                                                                                                                                                        none                                                                                                                                                                                                                                                            CTM alpha = 0.0267 & tau = 18.6 s with error equal to the correction                                                                                                                                                                                            Pressures adjusted using reported surface pressure when warranted.                                                                                                                                                                                              none                                                                                                                                                                                                                                                            Salinity corrected for pressure sensor calibration drift and cell thermal mass                                                                                                                                                                                  20061106125543              20061211021057  AO  ARGQ                                                                        20060508101802  QCP$                G�O�G�O�G�O�FFBFE           AO  ARGQ                                                                        20060508101802  QCF$                G�O�G�O�G�O�0               PM  ARSQPADJV1.1                                                                20061106125543  QC  PRES            @���D�  @                  PM  ARSQCTM V1.1                                                                20061106125543  QC  PSAL            @���D�  @                  PM  ARSQSIQCV2.0WOD2001 & Argo                                                  20061211111051  IP                  G�O�G�O�G�O�                