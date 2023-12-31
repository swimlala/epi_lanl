CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2015-05-01T02:16:15Z AOML 3.0 creation; 2016-08-07T21:17:36Z UW 3.1 conversion     
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7    PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    7   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  7   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7X   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     8   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8<   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8\   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >Ey��0�:   
_FillValue        A.�~       axis      T           8`   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8h   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >Ey��0�:   
_FillValue        A.�~            8l   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8t   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8|   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8�   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8�   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8�   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9�   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    9�   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    9�   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    9�   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  Ax   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  Cp   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  KH   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M@   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  U   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ^�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  f�   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  h�   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  p�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  xh   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  z`   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �8   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �0   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �8   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �8   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �8   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �8   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �d   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �h   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �l   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �p   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �t   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20150501021615  20160807141736  5904460 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               /A   AO  5285_8895_047                   2C  D   APEX                            6487                            072314                          846 @�M?ۗ?�1   @�M?ۗ?�@.      �c���Q�8   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    /A   B   B   @�ff@�  A   A   A@  A`  A�  A�33A�  A�  A�  A�  A�  A�  B   B  BffB  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh��BnffBw��B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cm�fCp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ�fDK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR�fDS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dys3D�fD�<�D��3D��3D��D�@ D�y�D�ٚD�3D�FfD�s3Dǩ�D�� 1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@ӅA	A)AIAiA��HA�{A��HA��HA��HA��HA��HA��HBp�B
p�B�
Bp�B"p�B*p�B2p�B:p�BBp�BJp�BRp�BZp�Bbp�Bk=qBp�
Bz
>B�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�k�B�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�k�B�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RC �)C�)C�)C�)C�)C
�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C �)C"�)C$�)C&�)C(�)C*�)C,�)C.�)C0�)C2�)C4�)C6�)C8�)C:�)C<�)C>�)C@�)CB�)CD�)CF�)CH�)CJ�)CL�)CN�)CP�)CR�)CT�)CV�)CX�)CZ�)C\�)C^�)C`�)Cb�)Cd�)Cf�)Ch�)Cj�)Cl�)Cn��Cp�)Cr�)Ct�)Cv�)Cx�)Cz�)C|�)C~�)C�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�ND '
D �
D'
D�
D'
D�
D'
D�
D'
D�
D'
D�
D'
D�
D'
D�
D'
D�
D	'
D	�
D
'
D
�
D'
D�
D'
D�
D'
D�
D'
D�
D'
D�
D'
D�
D'
D�
D'
D�
D'
D�
D'
D�
D'
D�
D'
D�
D'
D�
D'
D�
D'
D�
D'
D�
D'
D�
D'
D�
D'
D�
D'
D�
D'
D�
D '
D �
D!'
D!�
D"'
D"�
D#'
D#�
D$'
D$�
D%'
D%�
D&'
D&�
D''
D'�
D('
D(�
D)'
D)�
D*'
D*�
D+'
D+�
D,'
D,�
D-'
D-�
D.'
D.�
D/'
D/�
D0'
D0�
D1'
D1�
D2'
D2�
D3'
D3�
D4'
D4�
D5'
D5�
D6'
D6�
D7'
D7�
D8'
D8�
D9'
D9�
D:'
D:�
D;'
D;�
D<'
D<�
D='
D=�
D>'
D>�
D?'
D?�
D@'
D@�
DA'
DA�
DB'
DB�
DC'
DC�
DD'
DD�
DE'
DE�
DF'
DF�
DG'
DG�
DH'
DH�
DI'
DI�
DJ'
DJ�pDK'
DK�
DL'
DL�
DM'
DM�
DN'
DN�
DO'
DO�
DP'
DP�
DQ'
DQ�
DR'
DR�pDS'
DS�
DT'
DT�
DU'
DU�
DV'
DV�
DW'
DW�
DX'
DX�
DY'
DY�
DZ'
DZ�
D['
D[�
D\'
D\�
D]'
D]�
D^'
D^�
D_'
D_�
D`'
D`�
Da'
Da�
Db'
Db�
Dc'
Dc�
Dd'
Dd�
De'
De�
Df'
Df�
Dg'
Dg�
Dh'
Dh�
Di'
Di�
Dj'
Dj�
Dk'
Dk�
Dl'
Dl�
Dm'
Dm�
Dn'
Dn�
Do'
Do�
Dp'
Dp�
Dq'
Dq�
Dr'
Dr�
Ds'
Ds�
Dt'
Dt�
Dy�=D�)�D�PRD���D�ָD�0RD�S�D��D��D��D�Y�D���DǽD�Ӆ1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A��A��A� �A��A��A��A��A�"�A� �A� �A� �A�bA�
=A�A�A�A�A�A�%A�A��A��yA��mA��`A��`A��`A��HA��;A��mA�5?Aʴ9A�oAȋDAǕ�A�`BA�?}Aƛ�A��A���A�+A���A�VA���A��jA�dZA�7LA�bA�Q�A�I�A�~�A��uA�=qA���A�;dA�/A���A�1A��
A�(�A��-A�bNA��A�
=A��A�JA��A��HA�M�A��7A�$�A��DA��!A�A�A��hA��9A���A�$�A��FA�oA�O�A���A��/A�v�A�E�A��-A�{A�v�A�/A��A{O�AudZApAmt�Aj�Ai�-Ah-Af�uAc�A_+A\�AY�mAX �AV��AUƨATI�AO��AK33AGAD1'A@�HA>1'A<$�A;�A:�A5�A3�TA2r�A0-A/�A-&�A+�A*��A*-A'ƨA%+A$=qA"��A�
Az�A~�A~�A�HAA��A�7A�RA��AG�A�HA=qAK�A�A�hAI�A�mAK�A��A�`A�!A��AI�A�wA�FAoA��A~�A�RA��A`BA`BAp�A(�A �A��A��AAI�A+AS�A
��AdZA�A
��A	�PA	XA	dZA	�mA	��A	�;A	�TA	��A	�AE�AJA��AO�A33A$�A �A�\A�9A��A�A�A��A��A{AZA�PA��A��A�AE�A/AZA�AbNA=qA�mA��A�hAK�A"�A �A bN@��F@���@��\@�$�@��^@���@�K�@��H@���@�5?@�p�@�bN@�o@�%@�@��@�z�@��9@�(�@�@�|�@�v�@�A�@�;d@@@���@�-@���@��@�A�@�@���@�E�@��@��@�r�@���@�o@�~�@��@�@�?}@���@㕁@�
=@�n�@���@�x�@��@��D@��
@�l�@ޟ�@�@��@ە�@�E�@��#@���@���@�@ى7@��`@�Ĝ@أ�@؃@� �@���@�\)@���@�V@��T@�@ղ-@ա�@Ցh@Ձ@�hs@�?}@��@�%@ԃ@�j@�I�@�9X@�(�@�b@�1@�1@Ӆ@ҧ�@�5?@ѡ�@�O�@�O�@�&�@�Ĝ@�(�@�S�@�"�@���@θR@�V@��@�x�@���@�j@���@�dZ@�"�@��@ʇ+@���@�@�z�@�(�@��
@�l�@��@Ɨ�@�E�@��@���@�G�@���@��@Ĭ@�r�@���@�dZ@�o@°!@�E�@��@���@�X@��j@�(�@�dZ@�^5@�@�&�@���@��D@�A�@���@�
=@��@��H@���@�V@��@��7@�V@���@��9@��9@�r�@� �@�1@�K�@��H@���@��\@�~�@�=q@�@���@��@���@�;d@��@�V@��-@�X@�O�@�/@���@��u@�  @���@���@�v�@�{@�p�@���@���@�Z@�1'@�1@��@��@�ȴ@�ff@��#@��-@���@�hs@�O�@��@�Z@���@��w@���@��P@�K�@��y@�V@��@���@�`B@��@��j@���@���@��D@�bN@�(�@�  @��@��w@���@�33@�o@�@���@��H@���@��+@�ff@�M�@�5?@�{@��@��-@�O�@��@��`@�r�@�1'@�|�@�S�@�C�@�o@��H@�v�@�E�@�-@�J@���@�&�@�1'@��@��m@�|�@�+@�o@��H@��\@�v�@�-@���@�p�@���@���@�bN@�b@�1@���@�|�@�
=@��y@��m@���@��@��@�w@tI�@i��@a��@Y&�@Q�^@Ko@BM�@:^5@2^51111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111  A��A��A� �A��A��A��A��A�"�A� �A� �A� �A�bA�
=A�A�A�A�A�A�%A�A��A��yA��mA��`A��`A��`A��HA��;A��mA�5?Aʴ9A�oAȋDAǕ�A�`BA�?}Aƛ�A��A���A�+A���A�VA���A��jA�dZA�7LA�bA�Q�A�I�A�~�A��uA�=qA���A�;dA�/A���A�1A��
A�(�A��-A�bNA��A�
=A��A�JA��A��HA�M�A��7A�$�A��DA��!A�A�A��hA��9A���A�$�A��FA�oA�O�A���A��/A�v�A�E�A��-A�{A�v�A�/A��A{O�AudZApAmt�Aj�Ai�-Ah-Af�uAc�A_+A\�AY�mAX �AV��AUƨATI�AO��AK33AGAD1'A@�HA>1'A<$�A;�A:�A5�A3�TA2r�A0-A/�A-&�A+�A*��A*-A'ƨA%+A$=qA"��A�
Az�A~�A~�A�HAA��A�7A�RA��AG�A�HA=qAK�A�A�hAI�A�mAK�A��A�`A�!A��AI�A�wA�FAoA��A~�A�RA��A`BA`BAp�A(�A �A��A��AAI�A+AS�A
��AdZA�A
��A	�PA	XA	dZA	�mA	��A	�;A	�TA	��A	�AE�AJA��AO�A33A$�A �A�\A�9A��A�A�A��A��A{AZA�PA��A��A�AE�A/AZA�AbNA=qA�mA��A�hAK�A"�A �A bN@��F@���@��\@�$�@��^@���@�K�@��H@���@�5?@�p�@�bN@�o@�%@�@��@�z�@��9@�(�@�@�|�@�v�@�A�@�;d@@@���@�-@���@��@�A�@�@���@�E�@��@��@�r�@���@�o@�~�@��@�@�?}@���@㕁@�
=@�n�@���@�x�@��@��D@��
@�l�@ޟ�@�@��@ە�@�E�@��#@���@���@�@ى7@��`@�Ĝ@أ�@؃@� �@���@�\)@���@�V@��T@�@ղ-@ա�@Ցh@Ձ@�hs@�?}@��@�%@ԃ@�j@�I�@�9X@�(�@�b@�1@�1@Ӆ@ҧ�@�5?@ѡ�@�O�@�O�@�&�@�Ĝ@�(�@�S�@�"�@���@θR@�V@��@�x�@���@�j@���@�dZ@�"�@��@ʇ+@���@�@�z�@�(�@��
@�l�@��@Ɨ�@�E�@��@���@�G�@���@��@Ĭ@�r�@���@�dZ@�o@°!@�E�@��@���@�X@��j@�(�@�dZ@�^5@�@�&�@���@��D@�A�@���@�
=@��@��H@���@�V@��@��7@�V@���@��9@��9@�r�@� �@�1@�K�@��H@���@��\@�~�@�=q@�@���@��@���@�;d@��@�V@��-@�X@�O�@�/@���@��u@�  @���@���@�v�@�{@�p�@���@���@�Z@�1'@�1@��@��@�ȴ@�ff@��#@��-@���@�hs@�O�@��@�Z@���@��w@���@��P@�K�@��y@�V@��@���@�`B@��@��j@���@���@��D@�bN@�(�@�  @��@��w@���@�33@�o@�@���@��H@���@��+@�ff@�M�@�5?@�{@��@��-@�O�@��@��`@�r�@�1'@�|�@�S�@�C�@�o@��H@�v�@�E�@�-@�J@���@�&�@�1'@��@��m@�|�@�+@�o@��H@��\@�v�@�-@���@�p�@���@���@�bN@�b@�1@���@�|�@�
=G�O�@��m@���@��@��@�w@tI�@i��@a��@Y&�@Q�^@Ko@BM�@:^5@2^51111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;oB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B
��Bk�B�?B�}B�yB+B$�BB�BE�Bk�B�B��B�XB�9B��B~�B�B��BŢB�B��B��B	7BPBJBDBJB%B�B�mB�#B��BƨB�dB�'B��B�JBx�B_;BB�B�BB
�sB
��B
�B
��B
�B
��B
�mB
ȴB
�'B
��B
y�B
XB
49B	��B	�TB	�sB	�B	��B	�sB	ƨB	��B	��B	�JB	�B	|�B	r�B	aHB	L�B	A�B	7LB	0!B	)�B	$�B	�B	
=B��B�B�`B�NB�HB�BB�5B�NB�B�B�
B�
B�B�
B�B��B�BɺB�wB��B�jB�9B�XBÖB��B�B��B	JB	DB	DB	DB	�B	$�B	!�B	�B	�B	VB��B	B	B��B��B	B	
=B	hB	�B	 �B	�B	�B	!�B	0!B	=qB	N�B	W
B	ffB	z�B	� B	{�B	|�B	~�B	�JB	�+B	|�B	z�B	�1B	��B	�uB	�bB	�{B	��B	��B	��B	��B	�'B	�'B	�9B	�XB	��B	��B	B	ĜB	B	ŢB	��B	�B	�HB	�fB	�sB	�fB	�)B	�B	�B	�B	�B	�B	��B	��B	��B	�B	�B	��B	��B	��B	��B
  B
B
B
B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�mB	�`B	�sB	�B	��B	��B	��B	��B	��B	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
+B
+B
+B
+B
+B
+B
1B
	7B
	7B
	7B
	7B
	7B
	7B
DB
PB
PB
VB
VB
bB
hB
hB
hB
hB
hB
hB
hB
hB
hB
hB
bB
hB
hB
hB
hB
hB
hB
hB
bB
\B
bB
bB
bB
bB
\B
\B
\B
\B
\B
\B
\B
\B
\B
\B
\B
bB
bB
bB
hB
hB
hB
hB
hB
hB
hB
hB
hB
hB
oB
uB
uB
uB
uB
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
#�B
'�B
.B
2-B
8RB
>wB
C�B
F�B
K�B
P�B
XB
^5B
cT1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111  B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B
��BkSB�
B�MB�EB�B$�BB_BEmBkSB��B��B�&B�B��B~�BJBʍB�nB�B��B��B	 BBBBB�B�B�3B��B̕B�sB�0B��B��B�Bx�B_BBXB~B�B
�=B
ѳB
��B
��B
��B
��B
�6B
�|B
��B
�\B
y�B
W�B
4B	��B	�B	�>B	�B	��B	�@B	�sB	��B	�dB	�B	��B	|�B	r{B	aB	L�B	AUB	7B	/�B	)�B	$�B	oB	
B��B�]B�,B�B�B�B�B�B��B��B��B��B��B��B��B��B��BɆB�EB�VB�6B�B�&B�cB��B�~B��B	B	B	B	B	\B	$�B	!�B	nB	HB	B��B	�B	�B��B��B	�B	
B	-B	PB	 �B	B	lB	!�B	/�B	=4B	N�B	V�B	f*B	z�B	�B	{�B	|�B	~�B	�
B	��B	|�B	z�B	��B	�MB	�3B	�#B	�<B	�LB	��B	��B	��B	��B	��B	��B	�B	�BB	�GB	�NB	�ZB	�NB	�aB	ԼB	��B	�B	�"B	�0B	�!B	��B	�9B	�sB	�YB	�fB	�OB	��B	��B	�B	�`B	�kB	��B	��B	��B	��B	��B
�B
�B
�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�lB	�NB	�)B	�B	�.B	�jB	��B	��B	��B	��B	��B	�xB	�_B	�wB	��B	��B	��B	��B	��B	��B	��B	�B	�}B	�yB	�wB	�vB	�xB	�wB	�pB	�sB	�qB	�jB	�gB	�hB	�jB	�jB	�eB	�fB	�dB	�dB	�_B	�_B	�YB	�KB	�EB	�AB	�@B	�?B	�9B	�9B	�7B	�>B	�AB	�>B	�6B	�7B	�8B	�9B	�7B	�>B	�@B	�GB	�NB	�SB	�PB	�TB	�SB	�QB	�ZB	�_B	�jB	�kB	�fB	�jB	�nB	�xB	�wB	�vB	�wB	�pB	�bB	�dB	�dB	�]B	�cB	�oB	�vB	�wB	�}B	�B	�}B	�}B	�zB	�~B	�|B	��B	��B	��B	��B	��B	��B	��B	��B	��B
�B
�B
 �B
�B
�B
�B
 �B
�B
 �B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B

�B
	B
B
B
B
B
B
!B
 B
B
B
B
 B
!B
"B
!B
B
B
B
B
B
B
 B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
!B
"B
B
B
B
B
B
B
&B
-B
-B
/B
.B
1B
:B
7B
:B
7B
9B
8B
8B
9B
>B
>B
>B
@B
>B
CB
EB
DB
CB
DB
JB
KB
LB
LB
KB
LB
PB
RB
RB
VB
WB
]B
]B
dB
dB
cB
bB
dB
iB
jB
oB
oB
mB
nB
oB
iG�O�B
mB
#�B
'�B
-�B
1�B
8B
>+B
CKB
F[B
K|B
P�B
W�B
]�B
c1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.61 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201608071417362016080714173620160807141736  AO  ARCAADJP                                                                    20150501021615    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20150501021615  QCP$                G�O�G�O�G�O�0               AO  ARGQQCPL                                                                    20150501021615  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160807141736  IP                  G�O�G�O�G�O�                