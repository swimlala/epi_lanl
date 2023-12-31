CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2015-06-12T02:15:41Z AOML 3.0 creation; 2016-08-07T21:17:37Z UW 3.1 conversion     
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
_FillValue                 �  A�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  Cx   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  KX   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  MP   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  U0   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ]   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  _   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  f�   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  h�   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  p�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  x�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  z�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �x   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �p   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �P   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �    HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �$   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �(   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �,Argo profile    3.1 1.2 19500101000000  20150612021541  20160807141737  5904460 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               7A   AO  5285_8895_055                   2C  D   APEX                            6487                            072314                          846 @�W����1   @�W�UU�@.6ȴ9X�c�KƧ�1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    7A   B   B   @333@�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A���B ffB  B  B  B   B'��B0  B8ffB?��BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6�C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP�CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� DtٚDyy�D�3D�S3D���D���D�3D�I�D�� D�ٚD�fD�<�D�l�D��3D�  111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@Z=p@��@ӅA	A)AIAiA��HA��HA��HA��HA��HA��HA��HA��B�
B
p�Bp�Bp�B"p�B*
>B2p�B:�
BB
>BJp�BRp�BZp�Bbp�Bjp�Brp�Bzp�B�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�k�B�8RB�8RC �)C�)C�)C�)C�)C
�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C �)C"�)C$�)C&�)C(�)C*�)C,�)C.�)C0�)C2�)C4�)C6��C8�)C:�)C<�)C>�)C@�)CB�)CD�)CF�)CH�)CJ�)CL�)CN�)CP��CR�)CT�)CV�)CX�)CZ�)C\�)C^�)C`�)Cb�)Cd�)Cf�)Ch�)Cj�)Cl�)Cn�)Cp�)Cr�)Ct�)Cv�)Cx�)Cz�)C|�)C~�)C�NC�NC�Z�C�NC�NC�NC�NC�NC�NC�NC�NC�AGC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�AGC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�ND '
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
DJ�
DK'
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
DR�
DS'
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
Du �Dy��D��D�f�D��D��RD��D�]D���D��D��D�PRD��RD�ָD��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A���A��`A��A���AԸRAԴ9Aԧ�Aԡ�Aԙ�AԑhAԋDAԋDAԋDAԋDAԋDAԉ7Aԉ7Aԉ7Aԉ7AԅA���A�hsA�Aͩ�A��/A�?}Aˏ\A�JAʩ�A�?}A�
=A�S�Aȟ�A�^5A�t�A�I�AǋDA�G�A�&�A��TA�~�A��Aŧ�A�hsA�&�A��TAđhA��A���A�oA���A��A��DA���A��HA�XA���A��jA�bA�ƨA�1A���A��A��HA��PA�A�A��A�G�A��A��A���A�I�A�|�A��;A�n�A�bNA��HA��A���A���A�;dA���A�XA� �A�z�A�5?A�p�A�A��uA��#A�v�A��A��A�/A��A��A�O�A�hsA�M�A�oA|�Aw��Ap$�Ai��AdbAbE�A^ĜAZQ�AVjAU�ARjAQ?}APr�ANĜAL�yAKVAH��AE33ACG�AA��AAƨAA`BA@v�A?x�A=�TA<�\A:��A9��A8�HA7�A6Q�A5
=A3l�A3
=A3�A2��A1�wA1|�A1�^A/�^A.ffA-p�A,=qA,1A,�A*�+A'A%A#A#��A#�A"��A#7LA"�HA!"�A �`A bNAl�A��A�7A�`A5?A�AO�A�RA�A��A�`An�A$�A�hAC�A��A1'A�7AS�A�jAO�A��A��A��A5?AJA�A�A��AȴA=qA�A�AXA�A�A�!A �A  A��A��A�AoAK�A"�AA�!Ar�A��AS�A�yA��Ar�A�A��A�-A�A��A�A9XA�A�hA;dA
��A
�HA
ȴA
�jA
��A
jA
E�A
�A	��A	K�A	�A�/A��An�AbNA1AAx�A+A�/A��A�AbNA  A��A&�A�HA��A9XAbA�-AoA��A9XA��A\)A7LA
=AA ��A ^5A ��A r�@���@���@��#@�`B@���@�bN@�j@�b@��P@�+@��+@�/@� �@���@�33@�
=@��@��-@���@�Z@���@��@�ȴ@�R@�E�@�V@�9@��@��H@�R@�=q@�`B@�D@�t�@�ff@�J@��@���@柾@��@�-@�X@�r�@��@���@�\@�E�@��@�t�@��@ݺ^@��@�
=@���@��H@ڏ\@�=q@�@ج@�(�@���@��m@�ƨ@�K�@֗�@�~�@�ff@��@��/@��@�"�@�V@Ѳ-@��`@У�@�j@�1'@Ͼw@��y@��@��@�A�@��
@ʟ�@�V@�/@�bN@�b@�1@���@��;@���@ǶF@ǝ�@�l�@�@���@�=q@���@��@�j@�b@�  @�  @�  @�b@ÍP@���@�v�@��@�V@���@���@��D@�z�@�Q�@�A�@�(�@��m@�ƨ@�C�@��H@�^5@�5?@�@�hs@��/@�Z@��@��;@���@�dZ@���@�E�@�^5@�^5@�-@���@��T@��h@�V@���@�(�@� �@���@�+@��@���@���@�ff@�5?@�{@��@���@��^@��h@�x�@�hs@�O�@�/@�V@��`@�j@��@��@��y@�-@�hs@���@��u@�A�@�1'@��@���@�l�@�V@���@��-@��/@� �@��w@���@�33@��@��R@�v�@��@�@�hs@���@���@�K�@���@�$�@�J@���@��@��`@��@�I�@���@���@�C�@�@��R@��!@��R@��\@�5?@���@��h@��@�z�@�1@�|�@�dZ@�S�@�33@��H@�M�@�p�@���@��D@�I�@��@���@�dZ@��-@�J@���@��\@�hs@x  @n5?@e�-@]O�@W�P@Q�7@H  @BM�@9hs111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111A���A��`A��A���AԸRAԴ9Aԧ�Aԡ�Aԙ�AԑhAԋDAԋDAԋDAԋDAԋDAԉ7Aԉ7Aԉ7Aԉ7AԅA���A�hsA�Aͩ�A��/A�?}Aˏ\A�JAʩ�A�?}A�
=A�S�Aȟ�A�^5A�t�A�I�AǋDA�G�A�&�A��TA�~�A��Aŧ�A�hsA�&�A��TAđhA��A���A�oA���A��A��DA���A��HA�XA���A��jA�bA�ƨA�1A���A��A��HA��PA�A�A��A�G�A��A��A���A�I�A�|�A��;A�n�A�bNA��HA��A���A���A�;dA���A�XA� �A�z�A�5?A�p�A�A��uA��#A�v�A��A��A�/A��A��A�O�A�hsA�M�A�oA|�Aw��Ap$�Ai��AdbAbE�A^ĜAZQ�AVjAU�ARjAQ?}APr�ANĜAL�yAKVAH��AE33ACG�AA��AAƨAA`BA@v�A?x�A=�TA<�\A:��A9��A8�HA7�A6Q�A5
=A3l�A3
=A3�A2��A1�wA1|�A1�^A/�^A.ffA-p�A,=qA,1A,�A*�+A'A%A#A#��A#�A"��A#7LA"�HA!"�A �`A bNAl�A��A�7A�`A5?A�AO�A�RA�A��A�`An�A$�A�hAC�A��A1'A�7AS�A�jAO�A��A��A��A5?AJA�A�A��AȴA=qA�A�AXA�A�A�!A �A  A��A��A�AoAK�A"�AA�!Ar�A��AS�A�yA��Ar�A�A��A�-A�A��A�A9XA�A�hA;dA
��A
�HA
ȴA
�jA
��A
jA
E�A
�A	��A	K�A	�A�/A��An�AbNA1AAx�A+A�/A��A�AbNA  A��A&�A�HA��A9XAbA�-AoA��A9XA��A\)A7LA
=AA ��A ^5A ��A r�@���@���@��#@�`B@���@�bN@�j@�b@��P@�+@��+@�/@� �@���@�33@�
=@��@��-@���@�Z@���@��@�ȴ@�R@�E�@�V@�9@��@��H@�R@�=q@�`B@�D@�t�@�ff@�J@��@���@柾@��@�-@�X@�r�@��@���@�\@�E�@��@�t�@��@ݺ^@��@�
=@���@��H@ڏ\@�=q@�@ج@�(�@���@��m@�ƨ@�K�@֗�@�~�@�ff@��@��/@��@�"�@�V@Ѳ-@��`@У�@�j@�1'@Ͼw@��y@��@��@�A�@��
@ʟ�@�V@�/@�bN@�b@�1@���@��;@���@ǶF@ǝ�@�l�@�@���@�=q@���@��@�j@�b@�  @�  @�  @�b@ÍP@���@�v�@��@�V@���@���@��D@�z�@�Q�@�A�@�(�@��m@�ƨ@�C�@��H@�^5@�5?@�@�hs@��/@�Z@��@��;@���@�dZ@���@�E�@�^5@�^5@�-@���@��T@��h@�V@���@�(�@� �@���@�+@��@���@���@�ff@�5?@�{@��@���@��^@��h@�x�@�hs@�O�@�/@�V@��`@�j@��@��@��y@�-@�hs@���@��u@�A�@�1'@��@���@�l�@�V@���@��-@��/@� �@��w@���@�33@��@��R@�v�@��@�@�hs@���@���@�K�@���@�$�@�J@���@��@��`@��@�I�@���@���@�C�@�@��R@��!@��R@��\@�5?@���@��h@��@�z�@�1@�|�@�dZ@�S�@�33@��H@�M�@�p�@���@��D@�I�@��@���G�O�@��-@�J@���@��\@�hs@x  @n5?@e�-@]O�@W�P@Q�7@H  @BM�@9hs111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;oB	��B	�B	�B	�B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�dB
:^B
�%B
��B
ĜB
�B0!BK�BaHB�1B�dB��B
=B�B/B=qB:^BJ�BO�BO�BS�BXB]/BaHBcTBffBu�B�=B��B�}B��BǮB��B�sB�B�B�B�B  BB�B�)B�LB��B��B�=Bz�Be`BXBk�B`BB_;BK�B=qB.B�BVBB��B�B�mB�`B�5B�;B�ZB��BƨB�9B��B�bBy�BiyBB�B  B
��B
�dB
��B
v�B
J�B
\B	�)B	�?B	z�B	M�B	0!B	"�B	PB��B�B�mB�BB�5B�#B�B�B�B�)B�B��B	  B��B��B��B	B	PB	�B	49B	A�B	M�B	ffB	z�B	��B	��B	�'B	B	ǮB	ȴB	�#B	�B	��B
%B
B
  B
  B
	7B
B	�B	�/B	�#B	�5B	�HB	�B	��B	��B	�B	��B	��B	�B	�B
PB
PB
DB
oB
VB
PB
hB
)�B
.B
-B
.B
-B
0!B
2-B
/B
-B
-B
)�B
�B
�B
"�B
"�B
#�B
$�B
%�B
'�B
+B
&�B
$�B
'�B
+B
,B
,B
,B
)�B
+B
.B
0!B
5?B
1'B
5?B
<jB
=qB
=qB
<jB
<jB
;dB
;dB
:^B
:^B
9XB
:^B
9XB
9XB
8RB
8RB
7LB
7LB
6FB
5?B
49B
49B
33B
33B
33B
33B
2-B
2-B
1'B
0!B
.B
-B
,B
+B
+B
+B
)�B
(�B
(�B
+B
)�B
(�B
(�B
(�B
'�B
'�B
&�B
%�B
$�B
#�B
"�B
!�B
 �B
�B
�B
�B
�B
 �B
 �B
 �B
 �B
!�B
)�B
,B
'�B
!�B
!�B
%�B
%�B
'�B
)�B
+B
)�B
)�B
(�B
'�B
&�B
$�B
$�B
$�B
#�B
"�B
�B
�B
�B
 �B
�B
 �B
�B
�B
�B
�B
�B
�B
�B
�B
uB
hB
\B
VB
JB
DB

=B
	7B
	7B
1B
1B
+B
%B
B
B
B
  B	��B	��B	��B	��B	��B
B
+B
%B
B
B
B
%B
+B
+B
+B
+B
+B
%B
B
B
  B
  B
B
B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
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
B
B
B
B
%B
+B
+B
1B
1B
1B
1B
1B
	7B
	7B
	7B
	7B

=B

=B

=B

=B

=B

=B

=B

=B

=B

=B

=B

=B

=B

=B

=B

=B

=B
DB
DB
JB
JB
JB
JB
JB
JB
DB
DB
DB

=B
	7B
1B
1B

=B

=B

=B
DB
JB
JB
JB
JB
PB
JB
DB
JB
JB
DB
DB

=B
	7B

=B

=B
DB
DB
DB
DB
JB
PB
VB
\B
hB
oB
oB
oB
uB
uB
uB
{B
{B
{B
{B
{B
uB
uB
uB
uB
uB
{B
{B
�B
�B
"�B
(�B
,B
49B
<jB
A�B
F�B
I�B
K�B
O�B
Q�B
VB
\)111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�BB
:3B
��B
�tB
�oB
�sB/�BK�BaB� B�2B��B
B�B.�B=AB:)BJ�BO�BO�BS�BW�B\�BaBc"Bf6Bu�B�
B��B�JB˕B�yBΧB�AB�`B�cB�lB�B��B�B�fB��B�B�{B�iB�Bz�Be&BW�BkNB`B_BK�B=<B-�BNBB�B��B�fB�5B�(B��B�B�BϥB�oB��B�uB�(By�Bi?BBWB
��B
ΞB
�-B
�vB
v�B
J�B
$B	��B	�B	z�B	M�B	/�B	"�B	"B��B�ZB�?B�B�B��B��B��B��B��B�TB��B��B��B��B��B	�B	B	pB	4B	ARB	M�B	f-B	z�B	�wB	��B	��B	�UB	�sB	�xB	��B	�rB	��B
�B
�B	��B	��B
�B
�B	�RB	��B	��B	��B	�	B	�EB	��B	��B	�]B	��B	��B	�uB	�uB
B
B
 B
-B
B
B
$B
)�B
-�B
,�B
-�B
,�B
/�B
1�B
.�B
,�B
,�B
)�B
uB
oB
"�B
"�B
#�B
$�B
%�B
'�B
*�B
&�B
$�B
'�B
*�B
+�B
+�B
+�B
)�B
*�B
-�B
/�B
4�B
0�B
4�B
<&B
=0B
=-B
<(B
<'B
;!B
; B
:B
:B
9B
:B
9B
9B
8B
8B
7B
7	B
6B
4�B
3�B
3�B
2�B
2�B
2�B
2�B
1�B
1�B
0�B
/�B
-�B
,�B
+�B
*�B
*�B
*�B
)�B
(�B
(�B
*�B
)�B
(�B
(�B
(�B
'�B
'�B
&�B
%�B
$�B
#�B
"�B
!�B
 �B
|B
tB
|B
zB
 �B
 �B
 �B
 �B
!�B
)�B
+�B
'�B
!�B
!�B
%�B
%�B
'�B
)�B
*�B
)�B
)�B
(�B
'�B
&�B
$�B
$�B
$�B
#�B
"�B
wB
xB
qB
 �B
yB
 B
xB
mB
`B
ZB
OB
OB
PB
>B
/B
"B
B
B
B
 B
	�B
�B
�B
�B
�B
�B
�B
�B
�B
�B	��B	��B	��B	��B	��B	��B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B	��B	��B
�B
 �B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
 �B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
	�B
	�B

�B

�B
B
B
 B
B
B
B

�B

�B

�B
	�B
�B
�B
�B
	�B
	�B
	�B

�B
B
B
B
B
	B
B

�B
B
B

�B

�B
	�B
�B
	�B
	�B

�B

�B

�B

�B
B
B
B
B
B
'B
%B
&B
-B
,B
,B
2B
/B
2B
1B
1B
0B
,B
,B
+B
,B
2B
2G�O�B
jB
"�B
(�B
+�B
3�B
< B
A=B
F^B
IrB
K}B
O�B
Q�B
U�B
[�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.61 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201608071417372016080714173720160807141737  AO  ARCAADJP                                                                    20150612021541    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20150612021541  QCP$                G�O�G�O�G�O�0               AO  ARGQQCPL                                                                    20150612021541  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160807141737  IP                  G�O�G�O�G�O�                