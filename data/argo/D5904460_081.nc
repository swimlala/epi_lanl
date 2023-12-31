CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2015-10-26T09:16:13Z AOML 3.0 creation; 2016-08-07T21:17:42Z UW 3.1 conversion     
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20151026091613  20160807141742  5904460 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               QA   AO  5285_8895_081                   2C  D   APEX                            6487                            072314                          846 @�y�U��z1   @�y��s�T@+�E�����c�Q��1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    QA   B   B   @y��@���A   A   A@  Aa��A�  A�  A�  A�  A�  A�  A�  A�  B ffB  BffB33B ��B'33B0  B8ffB>ffBG��BP  BX  B`  Bh  Bp  BxffB�  B�  B�ffB���B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dt��Dy��D�3D�VfD�� D�� D�  D�I�D���D���D��D�0 D�|�DǖfD��311111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @�Q�@�Q�A	A)AIAk\)A��HA��HA��HA��HA��HA��HA��HA��HB�
B
p�B�
B��B#=qB)��B2p�B:�
B@�
BJ
>BRp�BZp�Bbp�Bjp�Brp�Bz�
B�8RB�8RB���B���B�B�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RC �)C�)C�)C�)C�)C
�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C �)C"�)C$�)C&�)C(�)C*�)C,�)C.�)C0�)C2�)C4�)C6�)C8�)C:�)C<�)C>�)C@�)CB�)CD�)CF�)CH�)CJ�)CL�)CN�)CP�)CR�)CT�)CV�)CX�)CZ�)C\�)C^�)C`�)Cb�)Cd�)Cf�)Ch�)Cj�)Cl�)Cn�)Cp�)Cr�)Ct�)Cv�)Cx�)Cz�)C|�)C~�)C�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�Z�C�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�ND '
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
Dt��Dy��D��D�i�D���D�ӅD�3�D�]D��RD��D� RD�C�D��RDǩ�D�ָ11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A��A�^5A�9XA��A��A�JA�A��A�oA�Q�A�1'A�A�A�ZA�A�jA�  AٸRA�9XA˥�A��A�A��A�E�A��A��+A�%A�M�A��A���A���A��+A��A�XA�z�A�5?A��DA�Q�Ax�`At�yAsVAp��Ai%AcC�A_oAY7LAUt�APbNAK�-AGC�AD�9ABȴA>M�A;��A7�;A4�A3�A2VA1��A1+A0�9A/�7A/|�A01'A0~�A0ȴA0VA/A-`BA'VAXA��AG�A�yA��Ar�A1'A1A�A�;A�A�9A��AoA`BA��A�PA`BA`BA�FA�A�A�`A�/A�!A^5A�A�!AK�A�DA�TA33At�A�A��AI�AXA�/AffA��AbA�RA�;A~�Ar�A�!A�yA�yAz�A��A�PA�/A$�A�PA7LA��A��A��A;dA
�A
��A
ZA	�A	��A	+A��Az�AƨAO�A"�A��AM�AA�A1A{A�AdZAI�A�A��A��A�7A n�@�K�@��@��@���@�V@��@�9X@�+@��\@�p�@�j@��;@�R@��@�+@@�\@�n�@�\@���@�ff@�p�@��@��@�\)@��y@�n�@�hs@�z�@畁@�ȴ@��@�A�@⟾@�{@��T@��@�7L@ߝ�@�=q@ݙ�@��@�b@ۥ�@�K�@ڗ�@���@���@�Z@�1'@�  @ץ�@�+@���@ա�@��@��/@Դ9@ԛ�@ԋD@�z�@�r�@�bN@�Z@�(�@��m@�;d@ҧ�@�=q@��@���@�hs@�V@Гu@��
@�l�@�C�@��@��y@Ώ\@�hs@�bN@�b@���@�l�@��@ʸR@��@ɑh@�hs@�G�@���@ț�@�bN@�9X@ǶF@�S�@Ə\@��T@š�@ŉ7@Ł@�`B@��@Ĵ9@�b@�|�@��@¸R@�=q@�V@��@��@�r�@�A�@���@�
=@��R@���@�^5@���@�z�@�Z@�(�@��@���@�l�@�\)@�C�@��R@�{@��T@��h@�/@�V@�%@���@��P@�\)@�C�@�C�@�;d@�o@���@�ff@�J@��@�@�`B@���@�Q�@���@�S�@�
=@��@���@�-@���@�`B@�&�@���@�(�@���@��F@�K�@��@��T@��-@�?}@���@���@�j@��P@�o@���@��H@�v�@�V@�5?@��@�J@��@���@��-@���@��h@���@���@��@�?}@�V@��`@���@���@��@�z�@�Z@�(�@���@��@�C�@�
=@���@�^5@��^@�X@�%@���@�Z@�1@��F@��@���@�~�@�@��T@���@���@��h@�/@��`@���@�Z@�1@��@�\)@�;d@�+@�
=@���@���@��\@�n�@�ff@��#@�hs@�G�@��@�I�@� �@�1@��;@�K�@�"�@��@��@�o@���@�^5@��h@��@�z�@��@��
@��@��@�t�@�+@�ȴ@���@�v�@�V@�E�@�E�@�-@�J@��^@���@���@�p�@�&�@��`@��@�1'@��@��
@�l�@�@��@���@�M�@�$�@�{@��@���@�hs@�V@���@���@��@�bN@� �@��m@�ƨ@�t�@�33@��H@���@�-@�x�@�7L@���@��`@���@��@�b@���@�"�@���@�v�@�M�@�E�@�{@��@�%@��9@�I�@��;@��P@��@�t�@�\)@�"�@��y@���@�5?@��^@�?}@��9@�9X@��;@��P@�S�@�o@��y@��R@�v�@�r�@x �@pbN@fV@^V@X1'@O��@H �@A�@9X@2n�@.V@*�@%`B11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111 A��A�^5A�9XA��A��A�JA�A��A�oA�Q�A�1'A�A�A�ZA�A�jA�  AٸRA�9XA˥�A��A�A��A�E�A��A��+A�%A�M�A��A���A���A��+A��A�XA�z�A�5?A��DA�Q�Ax�`At�yAsVAp��Ai%AcC�A_oAY7LAUt�APbNAK�-AGC�AD�9ABȴA>M�A;��A7�;A4�A3�A2VA1��A1+A0�9A/�7A/|�A01'A0~�A0ȴA0VA/A-`BA'VAXA��AG�A�yA��Ar�A1'A1A�A�;A�A�9A��AoA`BA��A�PA`BA`BA�FA�A�A�`A�/A�!A^5A�A�!AK�A�DA�TA33At�A�A��AI�AXA�/AffA��AbA�RA�;A~�Ar�A�!A�yA�yAz�A��A�PA�/A$�A�PA7LA��A��A��A;dA
�A
��A
ZA	�A	��A	+A��Az�AƨAO�A"�A��AM�AA�A1A{A�AdZAI�A�A��A��A�7A n�@�K�@��@��@���@�V@��@�9X@�+@��\@�p�@�j@��;@�R@��@�+@@�\@�n�@�\@���@�ff@�p�@��@��@�\)@��y@�n�@�hs@�z�@畁@�ȴ@��@�A�@⟾@�{@��T@��@�7L@ߝ�@�=q@ݙ�@��@�b@ۥ�@�K�@ڗ�@���@���@�Z@�1'@�  @ץ�@�+@���@ա�@��@��/@Դ9@ԛ�@ԋD@�z�@�r�@�bN@�Z@�(�@��m@�;d@ҧ�@�=q@��@���@�hs@�V@Гu@��
@�l�@�C�@��@��y@Ώ\@�hs@�bN@�b@���@�l�@��@ʸR@��@ɑh@�hs@�G�@���@ț�@�bN@�9X@ǶF@�S�@Ə\@��T@š�@ŉ7@Ł@�`B@��@Ĵ9@�b@�|�@��@¸R@�=q@�V@��@��@�r�@�A�@���@�
=@��R@���@�^5@���@�z�@�Z@�(�@��@���@�l�@�\)@�C�@��R@�{@��T@��h@�/@�V@�%@���@��P@�\)@�C�@�C�@�;d@�o@���@�ff@�J@��@�@�`B@���@�Q�@���@�S�@�
=@��@���@�-@���@�`B@�&�@���@�(�@���@��F@�K�@��@��T@��-@�?}@���@���@�j@��P@�o@���@��H@�v�@�V@�5?@��@�J@��@���@��-@���@��h@���@���@��@�?}@�V@��`@���@���@��@�z�@�Z@�(�@���@��@�C�@�
=@���@�^5@��^@�X@�%@���@�Z@�1@��F@��@���@�~�@�@��T@���@���@��h@�/@��`@���@�Z@�1@��@�\)@�;d@�+@�
=@���@���@��\@�n�@�ff@��#@�hs@�G�@��@�I�@� �@�1@��;@�K�@�"�@��@��@�o@���@�^5@��h@��@�z�@��@��
@��@��@�t�@�+@�ȴ@���@�v�@�V@�E�@�E�@�-@�J@��^@���@���@�p�@�&�@��`@��@�1'@��@��
@�l�@�@��@���@�M�@�$�@�{@��@���@�hs@�V@���@���@��@�bN@� �@��m@�ƨ@�t�@�33@��H@���@�-@�x�@�7L@���@��`@���@��@�b@���@�"�@���@�v�@�M�@�E�@�{@��@�%@��9@�I�@��;@��P@��@�t�@�\)@�"�@��y@���@�5?@��^@�?}@��9@�9X@��;@��P@�S�@�o@��y@��RG�O�@�r�@x �@pbN@fV@^V@X1'@O��@H �@A�@9X@2n�@.V@*�@%`B11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111 ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;oB�B�B�B�B��B��B��B  B1BL�B�B��B!�B?}BVBu�B�BS�B�-B	\)B	�DB	�B	��B	�B	s�B	o�B	n�B	n�B	n�B	m�B	m�B	jB	bNB	YB	P�B	O�B	I�B	K�B	M�B	L�B	K�B	Q�B	J�B	VB	n�B	~�B	�%B	�%B	s�B	gmB	\)B	H�B	:^B	%�B	�B	�B	�B	�B	�B	�B	"�B	@�B	[#B	aHB	ffB	bNB	[#B	B�B	bB�/B��B��B�wB�wB�wB�}B��B��BÖB�sB	�B	$�B	0!B	?}B	H�B	P�B	VB	bNB	iyB	l�B	n�B	p�B	p�B	m�B	aHB	P�B	C�B	;dB	7LB	1'B	.B	9XB	jB	��B	��B	��B	��B	��B	��B	�B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�!B	�-B	�-B	�'B	�?B	�LB	�XB	�^B	�^B	�wB	�wB	�wB	�wB	�qB	�jB	�^B	�^B	�^B	�XB	�RB	�XB	�dB	B	ÖB	ĜB	�wB	�dB	�jB	ÖB	�RB	�9B	�-B	�3B	�-B	�-B	�!B	�'B	�!B	�B	�B	�!B	�3B	�LB	�-B	�-B	�-B	�'B	�'B	�-B	�3B	�FB	�RB	�LB	�FB	�FB	�RB	�^B	�dB	�qB	�wB	�wB	�}B	�}B	B	ƨB	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�B	�B	�B	�#B	�#B	�)B	�)B	�)B	�/B	�)B	�;B	�BB	�BB	�BB	�HB	�HB	�HB	�HB	�BB	�BB	�BB	�BB	�HB	�NB	�NB	�TB	�TB	�TB	�TB	�TB	�TB	�TB	�TB	�TB	�TB	�NB	�TB	�ZB	�ZB	�`B	�sB	�sB	�sB	�sB	�sB	�sB	�sB	�yB	�yB	�yB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
B
B
B
B
B
B
B
B
+B
+B
+B
1B
1B
1B
1B
1B
1B
1B
	7B

=B
PB
PB
VB
\B
\B
\B
\B
bB
\B
bB
bB
bB
bB
hB
hB
oB
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
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
�B
 �B
 �B
 �B
 �B
 �B
 �B
!�B
!�B
!�B
 �B
!�B
!�B
!�B
!�B
!�B
!�B
"�B
!�B
!�B
"�B
"�B
"�B
"�B
"�B
!�B
"�B
!�B
!�B
"�B
"�B
$�B
$�B
%�B
&�B
&�B
'�B
'�B
'�B
'�B
'�B
(�B
(�B
(�B
)�B
)�B
)�B
(�B
)�B
)�B
)�B
)�B
+B
+B
+B
+B
+B
+B
+B
+B
+B
,B
,B
,B
,B
-B
-B
.B
/B
/B
/B
5?B
33B
8RB
>wB
C�B
H�B
M�B
S�B
YB
]/B
`BB
e`B
iyB
l�B
o�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111 B�xB�yB�}B�B��B��B��B��BBL�B��B��B!�B?NBU�Bu�B��BS�B�B	\B	�B	��B	��B	��B	s�B	otB	noB	nlB	nlB	mhB	mhB	jSB	b!B	X�B	P�B	O�B	I�B	K�B	M�B	L�B	K�B	Q�B	J�B	U�B	nbB	~�B	��B	��B	s}B	g6B	[�B	H~B	:&B	%�B	\B	TB	jB	zB	zB	yB	"�B	@JB	Z�B	aB	f*B	bB	Z�B	BVB	+B��BʍB�LB�BB�BB�DB�FB�MB�OB�_B�<B	nB	$�B	/�B	?DB	HvB	P�B	U�B	bB	i<B	lMB	n]B	piB	pgB	mTB	aB	P�B	CXB	;)B	7B	0�B	-�B	9B	jBB	�OB	��B	�qB	�nB	�aB	��B	��B	��B	�wB	�RB	�sB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�6B	�7B	�8B	�8B	�1B	�'B	�B	�B	�B	�B	�B	�B	�"B	�LB	�QB	�\B	�7B	�#B	�(B	�SB	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�	B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�	B	� B	�B	�B	�B	�"B	�/B	�2B	�3B	�6B	�9B	�JB	�dB	�B	ΓB	ѧB	ѨB	ѨB	ӲB	սB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�,B	�+B	�-B	�,B	�,B	�-B	�+B	�4B	�2B	�2B	�1B	�1B	�7B	�6B	�8B	�=B	�?B	�=B	�>B	�CB	�EB	�JB	�JB	�IB	�JB	�HB	�PB	�OB	�VB	�WB	�TB	�UB	�WB	�VB	�VB	�QB	�QB	�OB	�RB	�OB	�RB	�OB	�XB	�PB	�PB	�UB	�\B	�]B	�\B	�bB	�bB	�]B	�aB	�mB	�nB	�nB	�nB	�nB	�uB	�mB	�yB	�zB	�{B	�{B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
 �B
 �B
 �B
 �B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
	�B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
,B
0B
1B
1B
1B
0B
.B
.B
1B
1B
8B
6B
=B
BB
CB
AB
DB
QB
MB
NB
OB
RB
HB
PB
^B
bB
bB
kB
iB
nB
nB
nB
nB
kB
sB
rB
tB
tB
tB
uB
uB
 |B
 {B
 {B
 }B
 zB
 {B
!B
!�B
!�B
 {B
!�B
!~B
!�B
!�B
!�B
!�B
"�B
!�B
!}B
"�B
"�B
"�B
"�B
"�B
!}B
"�B
!�B
!�B
"�B
"�B
$�B
$�B
%�B
&�B
&�B
'�B
'�B
'�B
'�B
'�B
(�B
(�B
(�B
)�B
)�B
)�B
(�B
)�B
)�B
)�B
)�B
*�B
*�B
*�B
*�B
*�B
*�B
*�B
*�B
*�B
+�B
+�B
+�B
+�B
,�B
,�B
-�B
.�B
.�B
.�G�O�B
2�B
8B
>)B
CKB
HiB
M�B
S�B
X�B
\�B
_�B
eB
i-B
l@B
oQ11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111 <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.61 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201608071417422016080714174220160807141742  AO  ARCAADJP                                                                    20151026091613    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20151026091613  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20151026091613  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160807141742  IP                  G�O�G�O�G�O�                