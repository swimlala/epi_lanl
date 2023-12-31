CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2015-08-03T09:16:18Z AOML 3.0 creation; 2016-08-07T21:17:39Z UW 3.1 conversion     
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
_FillValue                 �  A|   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  Ct   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  KP   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  MH   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  U$   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ]    TEMP_ADJUSTED            
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
_FillValue                 �  x�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  z|   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �X   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �P   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �,   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �\   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �\   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �\   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �\   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �    HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20150803091618  20160807141739  5904460 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               AA   AO  5285_8895_065                   2C  D   APEX                            6487                            072314                          846 @�d�z��1   @�d��`@*�`A�7L�c�"��`B1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    AA   B   B   @�33@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(ffB0  B7��B@  BH  BP  BX  B`  Bh  Bp  Bx  B�33B�33B���B���B���B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C�C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0�C2�C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dt� Dy��D�  D�P D��3D�ٚD�fD�9�D��fD��3D��D�I�D�� D�� D��311111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��R@ӅA	A)AIAiA��HA��HA��HA��HA��HA��HA��HA��HBp�B
p�Bp�Bp�B"p�B*�
B2p�B:
>BBp�BJp�BRp�BZp�Bbp�Bjp�Brp�Bzp�B�k�B�k�B���B�B�B�8RB�k�B�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�B�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RC �)C�)C�)C�)C�)C
�)C�)C�)C�)C�)C�)C�)C��C�)C�)C�)C �)C"�)C$�)C&�)C(�)C*�)C,�)C.�)C0��C2��C4�)C6�)C8�)C:�)C<�)C>�)C@�)CB�)CD�)CF�)CH�)CJ�)CL�)CN�)CP�)CR�)CT�)CV�)CX�)CZ�)C\�)C^�)C`�)Cb�)Cd�)Cf�)Ch�)Cj�)Cl�)Cn�)Cp�)Cr�)Ct�)Cv�)Cx�)Cz�)C|�)C~�)C�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�Z�C�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�ND '
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
Dt�
Dy��D�3�D�c�D���D��D��D�MD���D�ָD�-D�]D���D�ӅD��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�1'A���A�uA��A�A�p�A�dZA�ZA�I�A�E�A�?}A�7LA�7LA�-A�1A��HA�FA��A���A��DA�~�A�ZA�jA�VA���A�ffA�7LA��#A؏\A�ZA��A�5?Aԇ+A�1'A��#A�&�A�t�A�9XA��#A�XA�33A�p�AˬA��AǍPA���A�;dA���A��A��A��+A���A��A�S�A�/A���A���A�JA�G�A���A��A�
=A���A�9XA��A�
=A���A�;dA��A���A�
=A��yA�hsA�
=A��/A�+A�1A�VA��Az~�At�Ap�9Al5?Ag�AedZAb�`Aa��A_|�AY��AS�-AOVAL��AJVAG?}AB(�A?l�A>~�A=�-A:1A9|�A9?}A8�A8�DA7C�A45?A2=qA1G�A.Q�A+A*�A(��A(VA&=qA%C�A"-A �A r�A I�A VA n�A v�A ��A"�yA#&�A$r�A%33A%C�A%"�A$$�A"�9A#G�A#hsA!C�A��A�A�7AI�A�wA��A��A�A�\A��A�`A�!A�7A+Ap�A��A�A5?AbA�#A33A�uA�A��A�+A�FA|�AS�A��A��AI�A�A�A�uA$�A"�A�RA�DA�+An�AffA��A��A��Az�A5?A5?A�A?}A
��A	��A	G�A�DAA�A�;AdZA�AJA��A\)A�uA�PA��A�A��A�+AbA��A�hAG�A v�A   @��;@���@��@�@�@��@���@��P@���@��@�V@���@�A�@�A�@�@�ff@��#@�x�@��@�C�@��y@�\@�$�@���@�X@�/@�Ĝ@�r�@�  @�t�@�ȴ@��#@�O�@���@��;@�dZ@�R@�-@�/@�j@䛦@�1@�33@��H@���@���@���@��D@��@�S�@�@�v�@�V@�5?@���@�r�@��@�K�@��@�E�@�G�@���@�r�@���@�t�@�o@��H@���@�=q@��@�1@ӶF@�"�@�-@�7L@Гu@�I�@���@�o@�J@�V@���@�ƨ@��H@�M�@ɩ�@��@��/@�A�@��
@Ǯ@�K�@���@�V@���@ř�@�/@���@ă@�j@��@Ý�@���@�{@���@�%@�A�@��;@�\)@��@���@�n�@���@��-@��7@�?}@��u@��@��F@���@�|�@�
=@���@��!@��\@�E�@��T@��7@��@���@�j@�(�@�  @���@�dZ@�
=@���@�ff@�{@��#@���@�p�@��@��/@���@��D@��m@���@�\)@��@�ff@�@��@���@��D@�9X@�  @��m@���@�@�n�@��@��@��^@�`B@�%@���@�bN@�A�@��@�  @�ƨ@�|�@�"�@�^5@���@�G�@��j@��u@�bN@�(�@�ƨ@���@�S�@�
=@��R@�~�@�^5@�{@�`B@��@���@�r�@��@�j@�I�@�1'@��@��
@��P@�|�@�dZ@�K�@��H@�n�@�@��-@���@�x�@��@��D@�A�@�b@���@�l�@�o@�
=@��y@��\@�=q@�J@�@�x�@��`@�r�@�(�@���@��m@�ƨ@��F@��P@�\)@�;d@�"�@�
=@��H@��!@�n�@�@��h@��@��u@��@��;@��P@�dZ@�"�@��R@��\@�5?@��#@���@�X@�7L@���@���@�Q�@�(�@�  @�l�@�S�@�K�@�"�@�ȴ@�^5@�5?@�J@��#@��@���@�I�@�1@���@���@�;d@�o@��\@�M�@��@���@��^@�G�@��@�
=@���@}�@r^5@kƨ@a��@X��@Q�^@KdZ@Ct�@<�j@5/@.v�@%�T11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111 A�1'A���A�uA��A�A�p�A�dZA�ZA�I�A�E�A�?}A�7LA�7LA�-A�1A��HA�FA��A���A��DA�~�A�ZA�jA�VA���A�ffA�7LA��#A؏\A�ZA��A�5?Aԇ+A�1'A��#A�&�A�t�A�9XA��#A�XA�33A�p�AˬA��AǍPA���A�;dA���A��A��A��+A���A��A�S�A�/A���A���A�JA�G�A���A��A�
=A���A�9XA��A�
=A���A�;dA��A���A�
=A��yA�hsA�
=A��/A�+A�1A�VA��Az~�At�Ap�9Al5?Ag�AedZAb�`Aa��A_|�AY��AS�-AOVAL��AJVAG?}AB(�A?l�A>~�A=�-A:1A9|�A9?}A8�A8�DA7C�A45?A2=qA1G�A.Q�A+A*�A(��A(VA&=qA%C�A"-A �A r�A I�A VA n�A v�A ��A"�yA#&�A$r�A%33A%C�A%"�A$$�A"�9A#G�A#hsA!C�A��A�A�7AI�A�wA��A��A�A�\A��A�`A�!A�7A+Ap�A��A�A5?AbA�#A33A�uA�A��A�+A�FA|�AS�A��A��AI�A�A�A�uA$�A"�A�RA�DA�+An�AffA��A��A��Az�A5?A5?A�A?}A
��A	��A	G�A�DAA�A�;AdZA�AJA��A\)A�uA�PA��A�A��A�+AbA��A�hAG�A v�A   @��;@���@��@�@�@��@���@��P@���@��@�V@���@�A�@�A�@�@�ff@��#@�x�@��@�C�@��y@�\@�$�@���@�X@�/@�Ĝ@�r�@�  @�t�@�ȴ@��#@�O�@���@��;@�dZ@�R@�-@�/@�j@䛦@�1@�33@��H@���@���@���@��D@��@�S�@�@�v�@�V@�5?@���@�r�@��@�K�@��@�E�@�G�@���@�r�@���@�t�@�o@��H@���@�=q@��@�1@ӶF@�"�@�-@�7L@Гu@�I�@���@�o@�J@�V@���@�ƨ@��H@�M�@ɩ�@��@��/@�A�@��
@Ǯ@�K�@���@�V@���@ř�@�/@���@ă@�j@��@Ý�@���@�{@���@�%@�A�@��;@�\)@��@���@�n�@���@��-@��7@�?}@��u@��@��F@���@�|�@�
=@���@��!@��\@�E�@��T@��7@��@���@�j@�(�@�  @���@�dZ@�
=@���@�ff@�{@��#@���@�p�@��@��/@���@��D@��m@���@�\)@��@�ff@�@��@���@��D@�9X@�  @��m@���@�@�n�@��@��@��^@�`B@�%@���@�bN@�A�@��@�  @�ƨ@�|�@�"�@�^5@���@�G�@��j@��u@�bN@�(�@�ƨ@���@�S�@�
=@��R@�~�@�^5@�{@�`B@��@���@�r�@��@�j@�I�@�1'@��@��
@��P@�|�@�dZ@�K�@��H@�n�@�@��-@���@�x�@��@��D@�A�@�b@���@�l�@�o@�
=@��y@��\@�=q@�J@�@�x�@��`@�r�@�(�@���@��m@�ƨ@��F@��P@�\)@�;d@�"�@�
=@��H@��!@�n�@�@��h@��@��u@��@��;@��P@�dZ@�"�@��R@��\@�5?@��#@���@�X@�7L@���@���@�Q�@�(�@�  @�l�@�S�@�K�@�"�@�ȴ@�^5@�5?@�J@��#@��@���@�I�@�1@���@���@�;d@�o@��\@�M�@��@���@��^@�G�G�O�@�
=@���@}�@r^5@kƨ@a��@X��@Q�^@KdZ@Ct�@<�j@5/@.v�@%�T11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111 ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;oB	�uB	�hB	�VB	�=B	�7B	�=B	�7B	�7B	�7B	�7B	�7B	�=B	�=B	�=B	�=B	�=B	�=B	�=B	�7B	�7B	�1B	�%B	�uB	��B	��B	�uB	�oB	�\B	�PB	�=B	�?B	ÖB	��B
\B
P�B
�+B
��B
��B
��BC�BH�Bw�B�B�BB�B,B:^Be`B|�Bu�Bu�Bt�B{�B�B��B��B��B�oB��B��B�oB�VB� Bu�BjB@�B�B�5B�7BM�B"�B
�B
�B
�3B
��B
ZB
$�B
%B	��B	��B	�bB	}�B	l�B	XB	M�B	@�B	6FB	-B	\B�B�/B��B��BȴB��B�LB�?B�^BBȴB��B��B��BɺBȴB��B��B�B�mB�B	+B	VB	+B	1B��B��B	
=B	hB	�B	�B	�B	1'B	q�B	z�B	�B	��B	��B	�B	��B	ÖB	��B	�ZB	�B	ȴB	�jB	�B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�^B	ƨB	��B	��B	��B	��B	�B	�#B	�#B	�)B	�B	�B	�#B	�5B	�5B	�;B	�BB	�TB	�ZB	�fB	�NB	�;B	�/B	�B	�B	�B	�B	�B	�;B	�mB	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	��B	��B	�B	�B	�B	�B	�B	�yB	�B	�B	�B	�B	�B	�B	�yB	�B	�B	�B	�B	�sB	�NB	�5B	�5B	�HB	�TB	�NB	�mB	�fB	�NB	�fB	�mB	�B	�sB	�ZB	�ZB	�`B	�mB	�fB	�fB	�`B	�ZB	�`B	�mB	�B	�B	�B	�B	�B	�B	�yB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
%B
+B
+B
+B
+B
+B
	7B

=B
DB
DB
DB
DB
DB
DB

=B
DB
DB
DB
DB
JB
PB
PB
VB
bB
bB
bB
bB
bB
bB
hB
bB
bB
bB
hB
hB
oB
oB
oB
oB
hB
hB
hB
hB
hB
oB
oB
oB
oB
uB
oB
uB
uB
uB
uB
uB
uB
{B
{B
{B
{B
{B
{B
{B
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
!�B
 �B
!�B
"�B
"�B
#�B
#�B
#�B
#�B
-B
)�B
0!B
5?B
;dB
B�B
J�B
O�B
S�B
XB
\)B
_;B
dZB
gmB
m�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111 B	�SB	�DB	�6B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�SB	��B	�hB	�TB	�LB	�9B	�,B	�B	�B	�qB	̧B
4B
P�B
��B
�WB
ͤB
��BCdBH�Bw�B��B�BZB+�B:+Be*B|�Bu�Bu�Bt�B{�B��B�WB�iB�fB�:B�lB��B�<B� B�Bu�BjKB@MB�B��B�BM�B"�B
�wB
��B
��B
�]B
Y�B
$�B
�B	ѶB	��B	�/B	}�B	lXB	W�B	M�B	@RB	6B	,�B	*B�oB��B��BΪBȇB�TB�B�B�.B�]BȂB˖B̚B˓BɈBȄBҿBϪB��B�8B�zB	�B	B	�B	�B��B��B	
B	2B	ZB	rB	�B	0�B	qoB	z�B	��B	̏B	ԿB	��B	ΜB	�XB	��B	�B	��B	�tB	�+B	��B	��B	��B	�{B	�B	�gB	�]B	��B	��B	��B	�B	�hB	ФB	̋B	͒B	ѬB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	� B	�B	�B	�"B	�B	��B	��B	��B	��B	��B	��B	��B	��B	�,B	�DB	�PB	�kB	��B	��B	��B	��B	��B	��B	��B	�xB	�kB	�sB	��B	�xB	�lB	�\B	�YB	�ZB	�HB	�6B	�NB	�iB	�YB	�VB	�IB	�>B	�8B	�[B	�IB	�AB	�FB	�-B	�	B	��B	��B	�B	�B	�B	�)B	�#B	�	B	�#B	�*B	�9B	�/B	�B	�B	�B	�(B	� B	�!B	�B	�B	�B	�)B	�9B	�FB	�DB	�AB	�AB	�;B	�4B	�4B	�4B	�BB	�CB	�BB	�BB	�;B	�@B	�XB	�SB	�RB	�RB	�KB	�KB	�YB	�WB	�RB	�RB	�KB	�KB	�UB	�fB	�ZB	�dB	�dB	�dB	�eB	�^B	�\B	�^B	�WB	�_B	�^B	�]B	�^B	�^B	�\B	�eB	�cB	�]B	�YB	�VB	�ZB	�PB	�QB	�RB	�RB	�KB	�KB	�CB	�FB	�EB	�KB	�RB	�WB	�WB	�_B	�bB	�cB	�dB	�dB	�cB	�^B	�dB	�aB	�eB	�gB	�gB	�iB	�gB	�mB	�oB	�uB	�uB	�pB	�mB	�uB	�tB	�wB	�{B	�{B	��B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
 �B
 �B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
$B
$B
%B
$B
!B
 B
B
B
B
$B
'B
%B
$B
/B
&B
,B
,B
-B
,B
,B
-B
0B
1B
0B
0B
0B
1B
/B
0B
2B
8B
6B
5B
7B
:B
>B
CB
EB
JB
JB
IB
JB
IB
KB
JB
JB
NB
QB
QB
SB
VB
VB
WB
TB
`B
bB
bB
jB
uB
sB
 zB
vB
sB
uB
uB
uB
tB
 |B
!B
 zB
!~B
"�B
"�B
#�B
#�B
#�B
#�G�O�B
)�B
/�B
4�B
;B
BEB
JtB
O�B
S�B
W�B
[�B
^�B
dB
g!B
mD11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111 <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.61 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201608071417392016080714173920160807141739  AO  ARCAADJP                                                                    20150803091618    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20150803091618  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20150803091618  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160807141739  IP                  G�O�G�O�G�O�                