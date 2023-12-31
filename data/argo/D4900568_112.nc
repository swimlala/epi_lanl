CDF      
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
_FillValue                    HHArgo profile    2.2 1.2 19500101000000  4900568 US ARGO PROJECT                                                 GREGORY C. JOHNSON                                              PRES            TEMP            PSAL               pA   AO  20071108101734  20080829173254  0961_34242_112                  2C  D   APEX_SBE_1533                                                   846 @Ԣ?G@ 1   @ԢDm� @9�r�   �c���   1   ARGOS   A   A   A   @�  A!��A�  A�ffA���B33B3��BQ33Bn��B���B�33B�33B�  B�  B�ffB�  B�  CffC�3CffCffC#ffC+��C433C>� CHL�CR�C]�ChL�Ct  C�fC��C��C��3C�� C�&fC��3C��3C��3C�ffCӦfC޳3C�&fC�&fDY�DfD�DS3DfD&FfD.� D7��DAfDK  DU� D`��Dl�DxfD�i�D�&fD�#3D���D�FfD�� D��fD�0 D¦fD̀ D�3D�&fD��3D�  111111111111111111111111111111111111111111111111111111111111111111111111@�  A��A�  A�ffA���B33B1��BO33Bl��B���B�33B�33B�  B�  B�ffB�  B�  C�fC33C�fC�fC"�fC+�C3�3C>  CG��CQ��C\��Cg��Cs� CffC�ٚC���C��3C�� C��fC�s3C��3C�s3C�&fC�ffC�s3C��fC��fD9�D�fD��D33D�fD&&fD.� D7��D@�fDJ� DU` D`l�Dk��Dw�fD�Y�D�fD�3D�|�D�6fD�p D��fD�  DfD�p D��3D�fD�3D�� 111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��AɁA�z�A��A��A��A��
A���A���A��yA�7LA��A���A���A��#A���A�JA�9XA���A��A��uA���A��wA�n�A���A�M�A�-A|�\Av�/Ao��Ah�AaAZ�`ARĜALM�AB��A;|�A8�A2��A*^5A!AffAA�A\)@��9@@�ƨ@�v�@�ff@��7@��P@���@��@���@��H@���@+@u?}@m�h@g�w@^5?@Vȴ@O\)@E�@;33@/�P@+dZ@$1@1@�-@�-@+?��w111111111111111111111111111111111111111111111111111111111111111111111111AɁA�z�A��A��A��A��
A���A���A��yA�7LA��A���A���A��#A���A�JA�9XA���A��A��uA���A��wA�n�A���A�M�A�-A|�\Av�/Ao��Ah�AaAZ�`ARĜALM�AB��A;|�A8�A2��A*^5A!AffAA�A\)@��9@@�ƨ@�v�@�ff@��7@��P@���@��@���@��H@���@+@u?}@m�h@g�w@^5?@Vȴ@O\)@E�@;33@/�P@+dZ@$1@1@�-@�-@+?��w111111111111111111111111111111111111111111111111111111111111111111111111;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oBz�B{�Bz�B�B�B� B� B~�B�+B�Bv�BB�B33B{B��B��B��B�VBl�B8RB#�B�B
��B
��B
�LB
�7B
s�B
R�B
-B
DB	�yB	ǮB	��B	|�B	H�B	 �B	uB��B��B��B�\Bu�BQ�B9XB(�B �B'�B49B<jBW
By�B��B��B��B	-B	S�B	|�B	��B	�LB	�B	�B
B
�B
&�B
9XB
@�B
J�B
VB
^5B
iyB
r�B
{�111111111111111111111111111111111111111111111111111111111111111111111111B{!B|tB{B�?B�KB�7B�8B.B��B"�Bz�BC�B5�B�B�B��B��B�5BouB9�B$�BB
��B
�OB
��B
�{B
u;B
T�B
.�B
B	��B	�wB	�:B	~�B	J.B	!�B	�B�8B�mB�UB��BwuBScB:�B*�B!�B(�B5B=vBW�Bz�B��B�oB�kB	-�B	T{B	}cB	�B	��B	�{B	�B
�B
�B
'XB
9�B
@�B
K B
V]B
^�B
i�B
sB
|0111111111111111111111111111111111111111111111111111111111111111111111111;�`B;�`B;�`B;�`B;�`B;�`B;�`B;�`B<o<49X<o;�`B;�`B;�`B;�`B;�`B;�`B;�`B;�`B;�`B;ě�;�`B;�`B;�`B;�`B;�`B;�`B;�`B;�`B;�`B;ě�;�`B;�`B;�`B;ě�;ě�;ě�;ě�;ě�;ě�;ě�;ě�;ě�;ě�;ě�;ě�;ě�;ě�;ě�;ě�;ě�;ě�;ě�;ě�;ě�;ě�;ě�;ě�;ě�;ě�;ě�;ě�;ě�;ě�;ě�;ě�;ě�;ě�;ě�;ě�;ě�;ě�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - (REPORTED_SURFACE_PRESSURE) for PADJ                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            PSAL_ADJ corrects Cnd. Thermal Mass (CTM), Johnson et al,2007,JAOT & effects of pressure adjustments                                                                                                                                                            PADJ REPORTED_SURFACE_PRESSURE =0.5 dbar                                                                                                                                                                                                                        none                                                                                                                                                                                                                                                            CTM alpha = 0.0267 & tau = 18.6 s with error equal to the correction                                                                                                                                                                                            Pressures adjusted using reported surface pressure when warranted.                                                                                                                                                                                              none                                                                                                                                                                                                                                                            Salinity corrected for pressure sensor calibration drift and cell thermal mass                                                                                                                                                                                  20080714141038              20080825204836  AO  ARGQ                                                                        20071108101734  QCP$                G�O�G�O�G�O�FFBFE           AO  ARGQ                                                                        20071108101734  QCF$                G�O�G�O�G�O�0               AO  ARCAADJS                                                                    20071108101734    IP                G�O�G�O�G�O�                PM  ARSQPADJV1.1                                                                20080714141038  QC  PRES            @�  D�                      PM  ARSQCTM V1.1                                                                20080714141038  QC  PSAL            @�  D�                      PM  ARSQSIQCV2.0WOD2001 & Argo                                                  20080829173156  IP                  G�O�G�O�G�O�                