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
_FillValue                    HHArgo profile    2.2 1.2 19500101000000  2900509 US ARGO PROJECT                                                 GREGORY C. JOHNSON                                              PRES            TEMP            PSAL               A   AO  20060517101813  20120702160701  1424_58999_012                  2C  D   APEX_SBE_2232                                                   846 @�?1@ 1   @�A�� @3<��   �c�E�   1   ARGOS   A   A   A   @���A)��A�  A�ffA���B  B4��BP  Bm33B���B�33B�ffB���B�  B�  B晚B�  C��CL�CL�CffC#  C+�C4�C>ffCHL�CR�C\�fCg�fCt��C�@ C�  C��C��fC�Y�C��C���C��C���C�Y�CӀ Cހ C�33C��DFfD  D�D@ D�D&@ D.�3D7�fDA�DK  DUl�D`��Dl3DxfD�ffD�&fD�)�D��fD�6fD���D�fD�#3D¦fD̃3D�fD��D�fD�3111111111111111111111111111111111111111111111111111111111111111111111111@���A)��A�  A�ffA���B  B4��BP  Bm33B���B�33B�ffB���B�  B�  B晚B�  C��CL�CL�CffC#  C+�C4�C>ffCHL�CR�C\�fCg�fCt��C�@ C�  C��C��fC�Y�C��C���C��C���C�Y�CӀ Cހ C�33C��DFfD  D�D@ D�D&@ D.�3D7�fDA�DK  DUl�D`��Dl3DxfD�ffD�&fD�)�D��fD�6fD���D�fD�#3D¦fD̃3D�fD��D�fD�3222222222222222222222222222222222222222222222222222222222222222222222222@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�jA�p�A�=qA�1'A�/A�^5Aɣ�A��A��TA��#A�ȴAȧ�A�ȴAđhA�ƨA�A��!A��\A�{A�\)A���A��mA��+A���A��A�|�A��jA�z�A�VA��Ak��AX�ANZAH�AE�hA;�A.�!A)�Ap�A�AK�A��@��-@�  @߅@�M�@��@Ƨ�@�1'@��@���@���@��w@�/@�bN@��@�M�@��w@�v�@x�@kS�@co@Yhs@O|�@E/@=�@0�`@$��@9X@��@l�@	hs111111111111111111111111111111111111111111111111111111111111111111111111A�jA�p�A�=qA�1'A�/A�^5Aɣ�A��A��TA��#A�ȴAȧ�A�ȴAđhA�ƨA�A��!A��\A�{A�\)A���A��mA��+A���A��A�|�A��jA�z�A�VA��Ak��AX�ANZAH�AE�hA;�A.�!A)�Ap�A�AK�A��@��-@�  @߅@�M�@��@Ƨ�@�1'@��@���@���@��w@�/@�bN@��@�M�@��w@�v�@x�@kS�@co@Yhs@O|�@E/@=�@0�`@$��@9X@��@l�@	hs222222222222222222222222222222222222222222222222222222222222222222222222;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB
��B
��B
��B
��B
��B
�'BhB-B.B0!B@�B�)B�BPBe`B33B�BXBQ�BXBdZBdZB]/BI�B+B��B^5B�B
��B
y�B	�7B	�B�fB�;B��B�RB��B��B��B��B��B��B�=B	  B	bB	 �B	�B	bB	�B	6FB	[#B	�bB	B	ÖB	�5B	�B	��B
	7B
�B
!�B
-B
5?B
=qB
F�B
N�B
T�B
`BB
k�B
s�B
z�B
�B
�+111111111111111111111111111111111111111111111111111111111111111111111111B
��B
��B
��B
��B
��B
�BPB(�B)�B,B=qB�B�B
=BbNB1'B�BT�BN�BW
BbNBaHB[#BH�B(�B��B]/B�B
��B
|�B	�7B	�B�ZB�)B��B�FB��B��B��B��B��B�uB�+B��B	PB	�B	�B	PB	�B	33B	XB	�PB	�}B	��B	�#B	�sB	��B
%B
oB
�B
(�B
2-B
:^B
C�B
J�B
Q�B
\)B
gmB
o�B
v�B
|�B
�222222222222222222222222222222222222222222222222222222222222222222222222<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - (REPORTED_SURFACE_PRESSURE) for PADJ                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            PSAL_ADJ corrects CTD thermal lag (CTL) viz. Johnson et al, 2007, JAOT, effects of pressure adjustments, and PSAL_ADJUSTED = PSAL + dS, where dS is calculated from a potential conductivity (ref to 0 dbar) multiplicative adjustment term r.                  PADJ REPORTED_SURFACE_PRESSURE =0 dbar                                                                                                                                                                                                                          none                                                                                                                                                                                                                                                            CTL alpha = 0.021 & tau = 21 s with error equal to |correction| and for OW r = 0.9999 (+/-0.0001), vertically averaged dS = -0.004 (+/-0.003)                                                                                                                   Pressures adjusted using reported surface pressure when warranted.  TNPD: APEX float that truncated negative pressure drift.                                                                                                                                    none                                                                                                                                                                                                                                                            After pressure and cell thermal lag correction of salinity values, OW correction estimated using mapping scales of 8 & 4 long. and 4 & 2 lat., no PV constraint, and decorrelation time scale of 10 years.                                                      20061106111550              20120618223408  AO  ARGQ                                                                        20060517101813  QCP$                G�O�G�O�G�O�FFBFE           AO  ARGQ                                                                        20060517101813  QCF$                G�O�G�O�G�O�0               PM  ARSQPADJV1.1                                                                20061106111550  QC  PRES            @���D�3@                  PM  ARSQCTM V1.1                                                                20061106111550  QC  PSAL            @���D�3@                  PM  ARSQSIQCV2.0WOD2001 & Argo                                                  20120702160555  IP                  G�O�G�O�G�O�                