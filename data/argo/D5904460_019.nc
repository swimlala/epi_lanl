CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2015-02-26T22:12:57Z AOML 3.0 creation; 2016-08-07T21:17:31Z UW 3.1 conversion     
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
_FillValue                 �  At   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  Cl   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  K@   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M8   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  U   TEMP_QC          
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
resolution        :�o     �  px   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  xL   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  zD   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �@   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �D   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �H   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �L   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �P   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20150226221257  20160807141731  5904460 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               A   AO  5285_8895_019                   2C  D   APEX                            6487                            072314                          846 @�(�=�1   @�(���`@+�XbM��c�Z�11   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   B   B   @�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  Bb  Bg��Bp  Bw��B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�33B�ffB�  B�  B���C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*�C,�C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C��C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Do��Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dtl�Dy` D�fD�Y�D���D�� D�� D�FfD�vfD���D� D�<�D�y�D��f111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@ӅA	A)AIAiA��HA��HA��HA��HA��HA��HA��HA��HBp�B
p�Bp�Bp�B"p�B*p�B2p�B:p�BBp�BJp�BRp�BZp�Bdp�Bj
>Brp�Bz
>B�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�k�B�8RB�8RB�8RB�8RB�8RB�k�B���B�8RB�8RC ��C�)C�)C�)C�)C
�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C �)C"�)C$�)C&�)C(�)C*��C,��C.�)C0�)C2�)C4�)C6�)C8�)C:�)C<�)C>�)C@�)CB�)CD�)CF�)CH�)CJ�)CL�)CN�)CP�)CR�)CT�)CV�)CX�)CZ�)C\�)C^�)C`�)Cb�)Cd�)Cf�)Ch�)Cj�)Cl�)Cn�)Cp�)Cr�)Ct�)Cv�)Cx�)Cz�)C|�)C~�)C�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�Z�C�NC�NC�NC�NC�Z�C�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�AGC�NC�NC�Z�C�NC�NC�NC�NC�Z�C�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�ND '
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
Dp �Dp�
Dq'
Dq�
Dr'
Dr�
Ds'
Ds�
Dt'
Dt��Dy�
D��D�mD��D��D��D�Y�D���D��D�#�D�PRD��D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�G�A�\)AՕ�A՗�AՓuAՑhAՏ\AՑhAՙ�A՛�A՟�A՝�Aՙ�AՓuAՋDAՇ+AՅAՅAՇ+AՉ7AՋDAՍPAՉ7AՉ7A�|�A�C�A��A͝�A�&�A�v�A��A�;dA�$�A�1A���A�33A���A���A��uA��#A�n�A�;dA�z�A��9A���A��A�$�A��A�S�A�C�A�S�A�ZA���A��DA�C�A�p�A��!A��\A���A��A�S�A�5?A�/A�n�A��`A�^5A��A���A���A��-A�~�A�ffA�#A}S�Az�AyO�AxE�AwS�Av-At=qAo�;Am33Ajv�Afr�Ad�yAc
=A_�7A\ȴAX�AT��AO&�AMG�AL��AK�#AIG�AGG�AC��AA�hA@v�A>�HA=�;A<M�A:�jA9�mA8ȴA6�HA5/A4bNA3��A1�mA.��A-`BA,jA,=qA*��A)p�A)�A(��A'�A'��A'C�A&��A& �A$�A"9XA!K�A M�Ar�A��A��AC�A�A�A�wAdZA�RA��A��A�AI�A�A�TA�FA\)An�A��Ax�A%A�\A�A��Ar�AVA
ffA	��A	K�A�DAA�AZA��AVA�7A$�A�A%A�!A��A��A �H@�33@�@�/@���@�1'@��w@� �@��D@���@���@��A �@��@�%@�33@�\)@�
=@�C�@�v�@���@���@��@�M�@��@�
=@�O�@�$�@��-@�?}@�p�@���@�7L@�Ĝ@���@�@���@�w@���@���@�@�x�@��@�(�@���@蛦@��y@旍@�@�/@�@��@�?}@���@�l�@�@�p�@�%@��
@�t�@�t�@߅@ߥ�@�33@�C�@�o@�S�@��@ާ�@�^5@���@�`B@��@�9X@ۮ@�dZ@�33@�33@�ȴ@�$�@���@�b@�
=@�v�@���@���@�Q�@�ƨ@��@�=q@���@��#@ѡ�@�p�@�G�@��@�z�@�I�@��;@�;d@�
=@�ȴ@�V@�@͙�@�%@���@��`@̼j@̛�@�j@�9X@��
@�33@ʰ!@�-@ə�@�X@��@��/@ȋD@��m@�ƨ@ǍP@�S�@�@Ə\@�@�X@�/@ģ�@�ƨ@�dZ@�@§�@+@�n�@�M�@���@��/@��D@�r�@�A�@��@�|�@�=q@���@�X@�X@�7L@���@��D@�z�@���@���@��@�M�@��@���@��`@�1'@�l�@�"�@���@�n�@�-@���@�`B@��@��u@�1'@�  @���@�C�@�^5@�J@��@���@���@��@��@�Ĝ@�j@��;@��P@�+@�ȴ@�n�@���@���@�Z@�(�@�b@�  @��@�+@���@�v�@�-@���@��@��@�z�@�r�@�Z@�9X@�b@��
@�|�@�C�@��@��\@�ff@�@��@��@�Z@��;@�l�@��H@��\@��@�@���@���@�p�@���@�bN@�b@��;@��@�l�@�S�@�33@�@��H@�n�@���@�p�@�G�@�7L@���@���@�Ĝ@��j@��@��@�Z@�1@���@�+@���@��@��!@�v�@�V@�@�`B@��@�Ĝ@�1'@��;@���@���@�l�@�33@�"�@�o@��y@���@���@�n�@���@�@�`B@�&�@��@�Z@�  @��@��;@�ƨ@�C�@��y@���@��\@�E�@�$�@�{@��T@�`B@���@��`@��j@�1'@�b@���@�t�@��H@���@�n�@��@���@��7@�x�@�p�@�`B@�O�@�7L@��@��`@�Ĝ@��@�(�@��@��@�\)@�+@�@�ȴ@��u@��y@yhs@m�T@e�h@^{@W
=@O�@Hr�@AX@9&�@1�^@+S�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111   A�G�A�\)AՕ�A՗�AՓuAՑhAՏ\AՑhAՙ�A՛�A՟�A՝�Aՙ�AՓuAՋDAՇ+AՅAՅAՇ+AՉ7AՋDAՍPAՉ7AՉ7A�|�A�C�A��A͝�A�&�A�v�A��A�;dA�$�A�1A���A�33A���A���A��uA��#A�n�A�;dA�z�A��9A���A��A�$�A��A�S�A�C�A�S�A�ZA���A��DA�C�A�p�A��!A��\A���A��A�S�A�5?A�/A�n�A��`A�^5A��A���A���A��-A�~�A�ffA�#A}S�Az�AyO�AxE�AwS�Av-At=qAo�;Am33Ajv�Afr�Ad�yAc
=A_�7A\ȴAX�AT��AO&�AMG�AL��AK�#AIG�AGG�AC��AA�hA@v�A>�HA=�;A<M�A:�jA9�mA8ȴA6�HA5/A4bNA3��A1�mA.��A-`BA,jA,=qA*��A)p�A)�A(��A'�A'��A'C�A&��A& �A$�A"9XA!K�A M�Ar�A��A��AC�A�A�A�wAdZA�RA��A��A�AI�A�A�TA�FA\)An�A��Ax�A%A�\A�A��Ar�AVA
ffA	��A	K�A�DAA�AZA��AVA�7A$�A�A%A�!A��A��A �H@�33@�@�/@���@�1'@��w@� �@��D@���@���@��A �@��@�%@�33@�\)@�
=@�C�@�v�@���@���@��@�M�@��@�
=@�O�@�$�@��-@�?}@�p�@���@�7L@�Ĝ@���@�@���@�w@���@���@�@�x�@��@�(�@���@蛦@��y@旍@�@�/@�@��@�?}@���@�l�@�@�p�@�%@��
@�t�@�t�@߅@ߥ�@�33@�C�@�o@�S�@��@ާ�@�^5@���@�`B@��@�9X@ۮ@�dZ@�33@�33@�ȴ@�$�@���@�b@�
=@�v�@���@���@�Q�@�ƨ@��@�=q@���@��#@ѡ�@�p�@�G�@��@�z�@�I�@��;@�;d@�
=@�ȴ@�V@�@͙�@�%@���@��`@̼j@̛�@�j@�9X@��
@�33@ʰ!@�-@ə�@�X@��@��/@ȋD@��m@�ƨ@ǍP@�S�@�@Ə\@�@�X@�/@ģ�@�ƨ@�dZ@�@§�@+@�n�@�M�@���@��/@��D@�r�@�A�@��@�|�@�=q@���@�X@�X@�7L@���@��D@�z�@���@���@��@�M�@��@���@��`@�1'@�l�@�"�@���@�n�@�-@���@�`B@��@��u@�1'@�  @���@�C�@�^5@�J@��@���@���@��@��@�Ĝ@�j@��;@��P@�+@�ȴ@�n�@���@���@�Z@�(�@�b@�  @��@�+@���@�v�@�-@���@��@��@�z�@�r�@�Z@�9X@�b@��
@�|�@�C�@��@��\@�ff@�@��@��@�Z@��;@�l�@��H@��\@��@�@���@���@�p�@���@�bN@�b@��;@��@�l�@�S�@�33@�@��H@�n�@���@�p�@�G�@�7L@���@���@�Ĝ@��j@��@��@�Z@�1@���@�+@���@��@��!@�v�@�V@�@�`B@��@�Ĝ@�1'@��;@���@���@�l�@�33@�"�@�o@��y@���@���@�n�@���@�@�`B@�&�@��@�Z@�  @��@��;@�ƨ@�C�@��y@���@��\@�E�@�$�@�{@��T@�`B@���@��`@��j@�1'@�b@���@�t�@��H@���@�n�@��@���@��7@�x�@�p�@�`B@�O�@�7L@��@��`@�Ĝ@��@�(�@��@��@�\)@�+@�G�O�@��u@��y@yhs@m�T@e�h@^{@W
=@O�@Hr�@AX@9&�@1�^@+S�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;oB
<jB
�yBbNBm�Bn�Bo�Bp�Bq�Bt�Bv�Bw�By�Bx�Bx�Bw�Bw�Bx�Bx�By�Bz�B{�B|�B|�B}�B}�Bz�Bt�Bq�Bw�Bn�BW
BZBv�B��B�B�B�}BɺB49B7LB+B��BB  B��B�B�`B�jB�{B�BYBC�B<jB8RBH�B`BB�B�B��BÖB�B�B
�#B
ƨB
�-B
��B
y�B
o�B
aHB
T�B
H�B
;dB
33B
&�B
�B
PB
%B	��B	��B	�`B	ǮB	�9B	��B	�=B	� B	s�B	aHB	O�B	9XB	"�B	JB	B	B��B�B�sB�ZB�TB�NB�TB�ZB�fB�sB�sB�sB�yB�yB�mB�ZB�/B��BɺB��B�#B�)B�)B�`B�mB�mB�sB�sB�sB�B�mB�HB�BB�B��B��B��B��B��B��B��BǮB�B�B�B�B�B�B��B��B	B	B	B	B	B	B	1B	1B	{B	\B	\B	\B	DB	%B	
=B	PB	\B	oB	PB	1B	%B	%B	%B	+B	
=B	
=B	VB	bB	oB	 �B	'�B	.B	6FB	<jB	M�B	_;B	u�B	~�B	r�B	o�B	k�B	l�B	n�B	z�B	�B	�uB	��B	�?B	��B	�qB	�dB	�XB	��B	��B	��B	��B	��B	�)B	�)B	�ZB	�yB	�B	�B	�sB	�;B	�B	�B	�B	��B	��B	ǮB	B	��B	��B	��B	��B	ĜB	��B	��B	ƨB	ĜB	ŢB	ŢB	ÖB	B	ȴB	��B	��B	��B	��B	��B	�
B	�B	�5B	�;B	�BB	�NB	�TB	�NB	�NB	�`B	�sB	�B	�B	�B	�yB	�mB	�ZB	�NB	�BB	�HB	�BB	�BB	�HB	�HB	�HB	�HB	�HB	�fB	�fB	�mB	�sB	�sB	�sB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
  B
  B
  B
  B
  B
B
B
B
B
B
B
B
B
B
B
1B
1B
1B
+B

=B

=B

=B

=B

=B

=B
	7B
	7B
	7B
	7B
	7B
1B
+B
+B
+B
%B
%B
%B
%B
+B
+B
1B
1B
1B
1B
1B
1B
1B
1B
	7B
	7B

=B
DB
PB
VB
VB
VB
VB
VB
VB
VB
VB
VB
VB
VB
VB
VB
bB
bB
bB
bB
bB
hB
oB
oB
uB
{B
�B
�B
�B
�B
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
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
!�B
!�B
!�B
!�B
!�B
"�B
"�B
!�B
"�B
"�B
"�B
"�B
"�B
"�B
$�B
%�B
0!B
8RB
A�B
G�B
K�B
P�B
T�B
YB
^5B
bNB
gmB
l�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111   B
<>B
�LBbBmaBngBonBpsBqyBt�Bv�Bw�By�Bx�Bx�Bw�Bw�Bx�Bx�By�Bz�B{�B|�B|�B}�B}�Bz�Bt�BqzBw�BnfBV�BY�Bv�B�bB��B��B�HBɆB4B7B�B��B �B��B��B�mB�)B�4B�EB��BX�BC`B<5B8BH}B`B��B��B�PB�]B��BQB
��B
�pB
��B
�MB
y�B
ofB
aB
T�B
HB
;,B
2�B
&�B
OB
B
�B	��B	��B	�)B	�wB	�B	��B	�B	�B	s�B	aB	O�B	9%B	"�B	B	�B	�B��B�zB�BB�)B�"B�B�#B�(B�3B�>B�@B�@B�FB�GB�:B�&B��BΦBɆB��B��B��B��B�)B�9B�7B�<B�>B�>B�KB�8B�B�B��B͞B͝BϫBΡB̘BʏBѵB�vB��B�WB�dB�XB�jB�}B��B��B	�B	�B	�B	�B	�B	�B	�B	�B	@B	!B	 B	B	
B	�B	
B	B	!B	4B	B	�B	�B	�B	�B	�B	
B	
B	B	'B	2B	 �B	'�B	-�B	6B	<*B	M�B	^�B	u�B	~�B	rpB	o_B	kDB	lKB	nWB	z�B	��B	�5B	��B	��B	�GB	�,B	�"B	�B	�AB	�FB	̈B	ұB	ԻB	��B	��B	�B	�5B	�LB	�HB	�,B	��B	��B	��B	��B	ӲB	ΔB	�kB	�LB	�DB	�CB	�EB	�FB	�YB	˃B	ˁB	�eB	�XB	�^B	�^B	�PB	�IB	�qB	ΕB	СB	СB	ҮB	ӵB	��B	��B	��B	��B	��B	�B	�B	�	B	�B	�B	�-B	�EB	�EB	�?B	�4B	�'B	�B	�B	��B	� B	��B	��B	�B	�B	�B	�B	�B	�B	� B	�%B	�+B	�/B	�.B	�2B	�5B	�8B	�?B	�EB	�FB	�PB	�XB	�XB	�UB	�VB	�YB	�UB	�XB	�YB	�YB	�WB	�RB	�QB	�UB	�VB	�WB	�bB	�bB	�cB	�bB	�eB	�eB	�dB	�gB	�gB	�qB	�vB	�uB	�tB	�}B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�nB	�fB	�dB	�gB	�vB	�vB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
 �B
 �B
 �B
 �B
 �B
 �B
�B
�B
�B
�B
�B
�B
�B
�B
	�B
	�B
	�B
	�B
	�B
	�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
	�B

�B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
 B
#B
%B
-B
/B
7B
8B
7B
8B
<B
=B
DB
CB
BB
DB
BB
LB
KB
JB
PB
OB
QB
PB
VB
UB
WB
UB
XB
UB
WB
VB
\B
]B
^B
`B
aB
cB
iB
jB
jB
hB
gB
mB
pB
nB
rB
vB
tB
uB
tB
 |B
 yB
 }B
 {B
 yB
 zB
 {B
 zB
 zB
 |B
 {B
!B
!B
!B
!�B
!�B
!�B
!�B
!�B
"�B
"�B
!}B
"�B
"�B
"�B
"�B
"�B
"�G�O�B
%�B
/�B
8B
A<B
GdB
KyB
P�B
T�B
X�B
]�B
bB
g!B
l>111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.61 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201608071417312016080714173120160807141731  AO  ARCAADJP                                                                    20150226221257    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20150226221257  QCP$                G�O�G�O�G�O�DFB5E           AO  ARGQQCPL                                                                    20150226221257  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160807141731  IP                  G�O�G�O�G�O�                