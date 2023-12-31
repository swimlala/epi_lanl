CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2015-03-08T19:17:27Z AOML 3.0 creation; 2016-08-07T21:36:33Z UW 3.1 conversion     
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
_FillValue                    A�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  C�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    K�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  U|   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    ]p   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  _p   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    gd   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  id   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  qX   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    yL   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  {L   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    �@   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �@   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �4   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �d   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �d   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �d   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �d   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20150308191727  20160807143633  5904461 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               #A   AO  5286_8897_035                   2C  D   APEX                            6531                            072314                          846 @�?%��1   @�?&[�@2g-�c��Q�1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    #A   B   B   @�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BHffBP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C�C  C
  C  C  C�fC  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<�C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX�CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DC��DD� DE  DE� DF  DF� DG  DG� DHfDH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dtl�Dyy�D� D�I�D�i�D��fD�3D�C3D�)�D��3D�3D�,�D�` D�ٚD� D�<�Dڀ D��fD�  D�C3D�c3D�� 11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@ÅAA!AAAaA��HA��HA��HA��HA��HA��HA��HA��HB p�Bp�Bp�Bp�B p�B(p�B0p�B8p�B@p�BH�
BPp�BXp�B`p�Bhp�Bpp�Bxp�B�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�k�B�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RC )C)C)C5�C)C
)C)C)C�C)C)C)C)C)C)C)C )C")C$)C&)C()C*)C,)C.)C0)C2)C4)C6)C8)C:)C<5�C>)C@)CB)CD)CF)CH)CJ)CL)CN)CP)CR)CT)CV)CX5�CZ)C\)C^)C`)Cb)Cd)Cf)Ch)Cj)Cl)Cn)Cp)Cr)Ct)Cv)Cx)Cz)C|)C~)C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C��C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�GC�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C��C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�D 
D �
D
D�
D
D�
D
D�
D
D�
D
D�
D
D�
D
D�
D
D�
D	
D	�
D

D
�
D
D�
D
D�
D
D�
D
D�
D
D�
D
D�
D
D�
D
D�
D
D�
D
D�
D
D�
D
D�
D
D�
D
D�
D
D�
D
D�
D
D�
D
D�
D
D�
D
D�
D
D�
D 
D �
D!
D!�
D"
D"�
D#
D#�
D$
D$�
D%
D%�
D&
D&�
D'
D'�
D(
D(�
D)
D)�
D*
D*�
D+
D+�
D,
D,�
D-
D-�
D.
D.�
D/
D/�
D0
D0�
D1
D1�
D2
D2�
D3
D3�
D4
D4�
D5
D5�
D6
D6�
D7
D7�
D8
D8�
D9
D9�
D:
D:�
D;
D;�
D<
D<�
D=
D=�
D>
D>�
D?
D?�
D@
D@�
DA
DA�
DB
DB�
DC
DC�
DD �DD�
DE
DE�
DF
DF�
DG
DG�
DHpDH�
DI
DI�
DJ
DJ�
DK
DK�
DL
DL�
DM
DM�
DN
DN�
DO
DO�
DP
DP�
DQ
DQ�
DR
DR�
DS
DS�
DT
DT�
DU
DU�
DV
DV�
DW
DW�
DX
DX�
DY
DY�
DZ
DZ�
D[
D[�
D\
D\�
D]
D]�
D^
D^�
D_
D_�
D`
D`�
Da
Da�
Db
Db�
Dc
Dc�
Dd
Dd�
De
De�
Df
Df�
Dg
Dg�
Dh
Dh�
Di
Di�
Dj
Dj�
Dk
Dk�
Dl
Dl�
Dm
Dm�
Dn
Dn�
Do
Do�
Dp
Dp�
Dq
Dq�
Dr
Dr�
Ds
Ds�
Dt
Dts�Dy��D��D�MD�mD���D��D�F�D�-D�ָD��D�0RD�c�D��D��D�@RDڃ�D���D��D�F�D�f�D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�I�A�O�A�Q�A�S�A�VA�VA�O�A�K�A�K�A�K�A�K�A�A�A�9XA�33A�&�A�9XA�K�A��A��mA��/A��`A��A���A��yA��TA��#A��A���A��AʾwA�t�A�I�A���Aɟ�A�5?A�
=A��TA�ƨAȟ�A�v�A�jA�I�A�1'A��A�AǼjA�M�A� �A�JA���A���Aƛ�A�x�A�G�AŲ-A�+A�XA���A�~�A�l�A�?}A�%A��TA���A�S�A���A�  A��DA�`BA�G�A��A�{A��
A�E�A�K�A�ĜA��A��mA�JA��DA��wA���A��A�ȴA��\A��A��A���A� �A���A�\)A��A��7A��A�x�A���A�t�A�K�A�7LA��FA�A��A|�Ay�;AwVAv1'AuoAq��Aj��Af��AaXA`E�A\9XA[�AZbAW|�AVv�AU�ARQ�AQ�7AP��AOXAJ�!AE�mAEoAD(�AC��AB��A@=qA>��A<�yA;�^A9x�A7�#A4�HA4jA3O�A1��A/�7A.��A-�^A+�
A)�#A&�!A%ƨA%&�A#�#A"�+A!`BA ĜA�FA�A=qA{A�PA�
Az�A?}A��AbNA5?AC�A��AXAx�AE�A�A|�A
~�A
JA	��A��A��AdZA��AA�Al�A�uAffA"�@�l�@�$�@�O�@��@�`B@��D@�S�@�7L@���@�X@��@��H@�/@��`@�dZ@�7@��@��@�"�@�$�@�p�@�A�@�l�@��@��@�X@�X@�hs@�(�@�"�@⟾@�J@��/@��;@�E�@ܣ�@��y@ڇ+@��@ٲ-@�7L@�A�@��@��/@�1'@ӥ�@�K�@�J@�V@���@д9@�j@�ƨ@�+@��@���@�v�@�?}@���@̼j@��@ˍP@˕�@�l�@�
=@�ȴ@�V@�J@ə�@�/@��@�Z@���@ǍP@�@�@��y@Ə\@�@��#@ŉ7@ċD@ēu@���@§�@°!@���@¸R@�V@�@��^@��7@�?}@�$�@°!@��@�ȴ@��7@�r�@��u@���@���@�r�@�j@��@�(�@�  @��F@��@���@�O�@��@���@�I�@�S�@�v�@�{@���@�x�@�hs@���@�z�@�I�@���@�33@��\@�5?@���@��@��-@��@�`B@���@�Z@�(�@�1'@�1'@�(�@�b@���@�o@��\@�v�@�~�@��@��@�{@��T@��@���@�X@�Z@���@�  @�1'@���@��y@��!@�~�@�^5@�5?@��^@��`@� �@��;@��@�S�@��H@��!@�~�@�^5@��@��@�p�@�%@�r�@�1@��m@��w@���@�|�@�\)@�;d@��@�~�@�J@��T@��7@��@��@��/@�Ĝ@��u@��@�r�@�9X@��m@���@�l�@�;d@��H@�^5@�{@���@�p�@�G�@��@���@��@��D@�A�@���@���@�\)@�+@�"�@���@���@�n�@�E�@��@��h@�?}@�V@��`@��@�j@�(�@�b@��
@��F@��F@��@���@��@�t�@�C�@���@���@�=q@��@���@�x�@�X@��@��@�j@�1@���@��
@��P@�dZ@�;d@�33@�+@�C�@�S�@�
=@���@��@��T@��#@���@���@��-@���@�?}@��@�Q�@�I�@�A�@�A�@��@��
@�\)@��@���@��\@��\@��+@���@���@�@��-@�x�@�?}@���@�1@��
@�t�@�K�@�+@���@���@�v�@�=q@���@���@���@�?}@�Ĝ@��D@��@�Z@� �@���@��@���@��u@��@v�y@n�+@d�/@\(�@Sƨ@Kƨ@Fȴ@@Q�@8Q�@2�@-p�@%��@ Ĝ@��@l�@�H@|�@"�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   A�I�A�O�A�Q�A�S�A�VA�VA�O�A�K�A�K�A�K�A�K�A�A�A�9XA�33A�&�A�9XA�K�A��A��mA��/A��`A��A���A��yA��TA��#A��A���A��AʾwA�t�A�I�A���Aɟ�A�5?A�
=A��TA�ƨAȟ�A�v�A�jA�I�A�1'A��A�AǼjA�M�A� �A�JA���A���Aƛ�A�x�A�G�AŲ-A�+A�XA���A�~�A�l�A�?}A�%A��TA���A�S�A���A�  A��DA�`BA�G�A��A�{A��
A�E�A�K�A�ĜA��A��mA�JA��DA��wA���A��A�ȴA��\A��A��A���A� �A���A�\)A��A��7A��A�x�A���A�t�A�K�A�7LA��FA�A��A|�Ay�;AwVAv1'AuoAq��Aj��Af��AaXA`E�A\9XA[�AZbAW|�AVv�AU�ARQ�AQ�7AP��AOXAJ�!AE�mAEoAD(�AC��AB��A@=qA>��A<�yA;�^A9x�A7�#A4�HA4jA3O�A1��A/�7A.��A-�^A+�
A)�#A&�!A%ƨA%&�A#�#A"�+A!`BA ĜA�FA�A=qA{A�PA�
Az�A?}A��AbNA5?AC�A��AXAx�AE�A�A|�A
~�A
JA	��A��A��AdZA��AA�Al�A�uAffA"�@�l�@�$�@�O�@��@�`B@��D@�S�@�7L@���@�X@��@��H@�/@��`@�dZ@�7@��@��@�"�@�$�@�p�@�A�@�l�@��@��@�X@�X@�hs@�(�@�"�@⟾@�J@��/@��;@�E�@ܣ�@��y@ڇ+@��@ٲ-@�7L@�A�@��@��/@�1'@ӥ�@�K�@�J@�V@���@д9@�j@�ƨ@�+@��@���@�v�@�?}@���@̼j@��@ˍP@˕�@�l�@�
=@�ȴ@�V@�J@ə�@�/@��@�Z@���@ǍP@�@�@��y@Ə\@�@��#@ŉ7@ċD@ēu@���@§�@°!@���@¸R@�V@�@��^@��7@�?}@�$�@°!@��@�ȴ@��7@�r�@��u@���@���@�r�@�j@��@�(�@�  @��F@��@���@�O�@��@���@�I�@�S�@�v�@�{@���@�x�@�hs@���@�z�@�I�@���@�33@��\@�5?@���@��@��-@��@�`B@���@�Z@�(�@�1'@�1'@�(�@�b@���@�o@��\@�v�@�~�@��@��@�{@��T@��@���@�X@�Z@���@�  @�1'@���@��y@��!@�~�@�^5@�5?@��^@��`@� �@��;@��@�S�@��H@��!@�~�@�^5@��@��@�p�@�%@�r�@�1@��m@��w@���@�|�@�\)@�;d@��@�~�@�J@��T@��7@��@��@��/@�Ĝ@��u@��@�r�@�9X@��m@���@�l�@�;d@��H@�^5@�{@���@�p�@�G�@��@���@��@��D@�A�@���@���@�\)@�+@�"�@���@���@�n�@�E�@��@��h@�?}@�V@��`@��@�j@�(�@�b@��
@��F@��F@��@���@��@�t�@�C�@���@���@�=q@��@���@�x�@�X@��@��@�j@�1@���@��
@��P@�dZ@�;d@�33@�+@�C�@�S�@�
=@���@��@��T@��#@���@���@��-@���@�?}@��@�Q�@�I�@�A�@�A�@��@��
@�\)@��@���@��\@��\@��+@���@���@�@��-@�x�@�?}@���@�1@��
@�t�@�K�@�+@���@���@�v�@�=q@���@���@���@�?}@�Ĝ@��D@��@�Z@� �@���G�O�@���@��u@��@v�y@n�+@d�/@\(�@Sƨ@Kƨ@Fȴ@@Q�@8Q�@2�@-p�@%��@ Ĝ@��@l�@�H@|�@"�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
	7B
VB
�B
N�B
��B�B33B?}BJ�BN�B]/Bo�Bn�Bp�B�=BƨB�B�yB�B��B1B
=BhB�B�B�B(�B-B1'B;dBC�BF�BG�BG�BM�BVBXBXB]/BbNBhsBn�Bq�Bt�B�Bx�BVB��B��BĜB��BĜB�qB�3B�'B��B��B��B��B��B�BcTBM�B;dB33B/B+B�B1B
��B
�B
�B
�fB
�/B
��B
�jB
��B
��B
��B
��B
��B
�+B
~�B
s�B
XB
8RB
�B
B	�B	�FB	��B	�bB	�B	{�B	s�B	]/B	/B	�B	B��B�fB�;B�fB�B��B��B�B�`B�BB��BǮB�}B�jB�jB�dB�dB�?B�!B�B�9B�RB�}B��B�wB�dB��BƨBĜBB�wB�dB��B��B��B��BɺBŢBŢB��B�NB�B�B�B��B��B��B��B	B	B	
=B	DB	%B��B�B�B�BB�B�B��B�#B�ZB�ZB�NB�5B�B�B�#B�5B�B��B��B��B��B��B��BƨBB�^B�LB�9B�!B��B�B�B�-B��B��B�#B�ZB�ZB�fB�HB�mB�yB�B�B��B	  B	B	B	B	1B	
=B	VB	bB	oB	uB	�B	�B	�B	�B	%�B	.B	1'B	49B	5?B	9XB	<jB	<jB	=qB	>wB	?}B	@�B	E�B	F�B	F�B	H�B	J�B	K�B	L�B	N�B	P�B	P�B	Q�B	S�B	XB	[#B	]/B	`BB	aHB	bNB	dZB	dZB	e`B	iyB	m�B	o�B	p�B	q�B	p�B	n�B	r�B	w�B	x�B	y�B	y�B	{�B	|�B	|�B	}�B	~�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�!B	�!B	�!B	�!B	�!B	�-B	�-B	�3B	�3B	�9B	�?B	�?B	�?B	�FB	�FB	�?B	�FB	�?B	�3B	�3B	�9B	�?B	�RB	�dB	�dB	�XB	�RB	�LB	�RB	�RB	�XB	�^B	�XB	�^B	�dB	�jB	�wB	�wB	�}B	��B	��B	B	ÖB	ǮB	ȴB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�#B	�#B	�#B	�#B	�)B	�)B	�)B	�/B	�5B	�;B	�;B	�;B	�BB	�BB	�BB	�HB	�HB	�HB	�HB	�NB	�TB	�ZB	�ZB	�ZB	�`B	�fB	�mB	�sB	�sB	�yB	�yB	�B	�B	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
+B
+B
+B
+B
1B
1B
1B
	7B
1B
1B
1B
	7B

=B

=B

=B

=B
	7B
	7B
	7B
	7B
JB
PB
VB
hB
uB
�B
�B
$�B
+B
2-B
8RB
?}B
B�B
I�B
P�B
S�B
XB
_;B
cTB
hsB
k�B
p�B
r�B
w�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
	9B
UB
�B
N�B
��B|B3+B?uBJ�BN�B]&Bo�Bn�Bp�B�6BƞB�B�qB�B��B'B
4B`BtB�B�B(�B-B1B;\BC�BF�BG�BG�BM�BU�BXBX
B](BbFBhmBn�Bq�Bt�B�Bx�BU�B��BʸBēB��BēB�eB�,B�B��B��B��B��B��B�	BcKBM�B;[B3,B/B*�B�B)B
��B
�B
�wB
�`B
�%B
��B
�aB
��B
��B
��B
��B
�yB
�&B
~�B
s�B
XB
8LB
�B
B	�B	�CB	��B	�eB	�
B	{�B	s�B	]2B	/!B	�B	B��B�kB�CB�lB�B��B��B�B�dB�IB�BǶB��B�rB�rB�kB�mB�FB�)B�B�>B�YB��B��B�B�kB��BƯBĢBB�B�lB��B��B��B��B��BťBŧB��B�QB�B�B�B��B��B��B��B	B	B	
?B	FB	(B��B�B�B�EB�"B�	B��B�(B�_B�]B�RB�9B� B�B�%B�9B�B��B��B��B��B��B��BƪBB�dB�PB�?B�$B�B�B�B�0B��B��B�'B�]B�^B�hB�IB�qB�|B�B�B��B	 B	B	B	B	1B	
;B	XB	cB	pB	tB	�B	�B	�B	�B	%�B	.B	1&B	4:B	5>B	9VB	<hB	<kB	=pB	>wB	?|B	@�B	E�B	F�B	F�B	H�B	J�B	K�B	L�B	N�B	P�B	P�B	Q�B	S�B	XB	["B	],B	`?B	aFB	bKB	dWB	dXB	e_B	ivB	m�B	o�B	p�B	q�B	p�B	n�B	r�B	w�B	x�B	y�B	y�B	{�B	|�B	|�B	}�B	~�B	�B	�|B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�)B	�*B	�-B	�-B	�4B	�9B	�9B	�:B	�@B	�BB	�;B	�@B	�:B	�.B	�-B	�5B	�8B	�JB	�^B	�_B	�RB	�LB	�DB	�JB	�JB	�PB	�[B	�QB	�XB	�`B	�eB	�tB	�rB	�vB	��B	��B	B	ÑB	ǨB	ȭB	ȭB	ɵB	˿B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�	B	�B	�B	�B	�B	�B	�"B	�!B	�!B	�(B	�.B	�4B	�5B	�4B	�;B	�<B	�<B	�@B	�?B	�@B	�@B	�FB	�NB	�SB	�RB	�SB	�WB	�_B	�dB	�mB	�mB	�vB	�sB	�vB	�wB	�rB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
 �B
 �B
 B
 �B
 �B
 �B
 �B
B
 �B
B
 �B
B
B
B
B
B
$B
"B
#B
$B
+B
)B
+B
	.B
*B
)B
)B
	-B

4B

4B

5B

4B
	-B
	0B
	.B
	+B
AB
HG�O�B
]B
nB
~B
�B
$�B
*�B
2%B
8IB
?qB
B�B
I�B
P�B
S�B
XB
_1B
cHB
hfB
k{B
p�B
r�B
w�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.11 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201608071436332016080714363320160807143633  AO  ARCAADJP                                                                    20150308191727    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20150308191727  QCP$                G�O�G�O�G�O�DFB5E           AO  ARGQQCPL                                                                    20150308191727  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160807143633  IP                  G�O�G�O�G�O�                