CDF      
      STRING16      STRING4       	DATE_TIME         N_PROF        STRING8       STRING64   @   N_PARAM       STRING2       STRING32       N_LEVELS   ;   N_CALIB       	STRING256         	N_HISTORY                     <   	DATA_TYPE                   comment       	Data type      
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
resolution        =���      �  2�   PRES_QC          	         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  <  3�   PRES_ADJUSTED            	      	   	long_name         SEA PRESSURE   
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    comment       $In situ measurement, sea surface = 0   C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���      �  3�   PRES_ADJUSTED_QC         	         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  <  4�   PRES_ADJUSTED_ERROR          	         	long_name         SEA PRESSURE   
_FillValue        G�O�   units         decibar    comment       WContains the error on the adjusted values as determined by the delayed mode QC process.    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���      �  4�   TEMP         	      	   	long_name         $SEA TEMPERATURE IN SITU ITS-90 SCALE   
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      comment       In situ measurement    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o      �  5�   TEMP_QC          	         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  <  6�   TEMP_ADJUSTED            	      	   	long_name         $SEA TEMPERATURE IN SITU ITS-90 SCALE   
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      comment       In situ measurement    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o      �  7    TEMP_ADJUSTED_QC         	         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  <  7�   TEMP_ADJUSTED_ERROR          	         	long_name         $SEA TEMPERATURE IN SITU ITS-90 SCALE   
_FillValue        G�O�   units         degree_Celsius     comment       WContains the error on the adjusted values as determined by the delayed mode QC process.    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o      �  8(   PSAL         	      	   	long_name         PRACTICAL SALINITY     
_FillValue        G�O�   units         psu    	valid_min                	valid_max         B(     comment       In situ measurement    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o      �  9   PSAL_QC          	         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  <  :    PSAL_ADJUSTED            	      	   	long_name         PRACTICAL SALINITY     
_FillValue        G�O�   units         psu    	valid_min                	valid_max         B(     comment       In situ measurement    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o      �  :<   PSAL_ADJUSTED_QC         	         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  <  ;(   PSAL_ADJUSTED_ERROR          	         	long_name         PRACTICAL SALINITY     
_FillValue        G�O�   units         psu    comment       WContains the error on the adjusted values as determined by the delayed mode QC process.    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o      �  ;d   	PARAMETER            
                	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  <P   SCIENTIFIC_CALIB_EQUATION            
               	long_name         'Calibration equation for this parameter    
_FillValue                    <�   SCIENTIFIC_CALIB_COEFFICIENT         
               	long_name         *Calibration coefficients for this equation     
_FillValue                    ?�   SCIENTIFIC_CALIB_COMMENT         
               	long_name         .Comment applying to this parameter calibration     
_FillValue                    B�   CALIBRATION_DATE         
               
_FillValue                  ,  E�   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    E�   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    E�   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    E�   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    E�   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  E�   HISTORY_DATE                     	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    E�   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    F   HISTORY_PARAMETER                         	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    F   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         F    HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         F$   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        F(   HISTORY_QCTEST                        	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    F,Argo profile    2.2 1.2 19500101000000  5900641 US ARGO PROJECT                                                 GREGORY C. JOHNSON                                              PRES            TEMP            PSAL               A   AO  20050509101652  20130425193212  0954_33060_026                  2C  D   APEX_SBE_1526                                                   846 @ӽ�� 1   @ӽ�_@ @;�K�   �c�;`   1   ARGOS   A   A   A   @���A(  A�ffA���A�  B33B4��BR  Bl��B�  B�ffB�  B�ffB���B�  B晚B�ffC33C  C�fC�fC#� C+� C4ffC>33CH33CR  C]  Ch33CtL�C�fC��fC��C�&fC��fC�L�C��fC�33C�� CɦfCә�Cހ C�  C�&fD` D�DfD9�D�3D&S3D.��D7� DA�DK3DU�3D`��Dk��DxfDy� 11111111111111111111111111111111111111111111111111111111111 @�fgA$��A���A�33A�ffBffB4  BQ33Bl  B���B�  B���B�  B�fgB֙�B�34B�  C  C
��C�3C�3C#L�C+L�C433C>  CH  CQ��C\��Ch  Ct�C�3C���C��3C��C���C�33C���C��C�ffCɌ�CӀ C�ffC��fC��DS3D�D��D,�D�fD&FfD.� D7�3DA  DKfDU�fD`��Dk��Dw��Dys311111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A���A� �A��7A�{A��mA��/A���A��DA�O�A�A��FA��A��-A��DA�K�A��HA��7A�`BA�5?A�A�A�n�A��A}�Az��Ap~�Ag��AcXA_XA\I�AY��AWXAP-AK�AC��A?/A9��A49XA0v�A.M�A&�A"^5AƨAZA33@��;@�-@��y@�V@�p�@�l�@��/@��@�hs@��@���@~��@t(�@ol�@n��11111111111111111111111111111111111111111111111111111111111 A���A� �A��7A�{A��mA��/A���A��DA�O�A�A��FA��A��-A��DA�K�A��HA��7A�`BA�5?A�A�A�n�A��A}�Az��Ap~�Ag��AcXA_XA\I�AY��AWXAP-AK�AC��A?/A9��A49XA0v�A.M�A&�A"^5AƨAZA33@��;@�-@��y@�V@�p�@�l�@��/@��@�hs@��@���@~��@t(�@ol�@n��11111111111111111111111111111111111111111111111111111111111 ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB�{Bo�BT�BQ�BR�BP�BVBW
BO�BM�B8RB�BB��B�jB�XB�9Bl�BN�B:^B
�B
��B
�=B
p�B
0!B
  B	�yB	��B	�}B	�!B	��B	~�B	dZB	C�B	0!B	�B��B�B�HBĜB�B�bBq�BI�B49B!�B{B"�B0!BA�Bo�B�oBȴB�B	�B	?}B	t�B	�JB	�\11111111111111111111111111111111111111111111111111111111111 B��Bv�BXBT�BVBT�BZB[#BR�BQ�B<jB �B%B��B�}B�jB�XBp�BQ�B?}B
��B
�B
�VB
u�B
5?B
B	�B	��B	B	�3B	��B	�B	hsB	G�B	49B	�B	B�B�`BȴB�3B�{Bu�BM�B8RB%�B�B&�B33BE�Br�B��B��B�B	 �B	B�B	w�B	�\B	�o11111111111111111111111111111111111111111111111111111111111 <#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - (REPORTED_SURFACE_PRESSURE) for PADJ                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            PSAL_ADJ corrects CTD thermal lag (CTL) viz. Johnson et al, 2007, JAOT, effects of pressure adjustments, and PSAL_ADJUSTED = PSAL + dS, where dS is calculated from a potential conductivity (ref to 0 dbar) multiplicative adjustment term r.                  PADJ REPORTED_SURFACE_PRESSURE =0.2 dbar                                                                                                                                                                                                                        none                                                                                                                                                                                                                                                            CTL alpha = 0.021 & tau = 21 s with error equal to |correction| and for OW r = 1.0001 (+/-0), vertically averaged dS = 0.004 (+/-0.002)                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            After pressure and cell thermal lag correction of salinity values, OW correction estimated using mapping scales of 8 & 4 long. and 4 & 2 lat., no PV constraint, and decorrelation time scale of 10 years.                                                                                  20121126101915  AO  ARGQ                                                                        20050509101652  QCP$                G�O�G�O�G�O�FFBFE           AO  ARGQ                                                                        20050509101652  QCF$                G�O�G�O�G�O�0               PM  ARSQPADJV1.0                                                                20050927153923  QC  PRES            @���Dy� @                  PM  ARSQCTL V1.0                                                                20050927153923  QC  PSAL            @���Dy� @                  PM  ARSQSIQCV2.0WOD2001 & Argo                                                  20121127160643  IP                  G�O�G�O�G�O�                