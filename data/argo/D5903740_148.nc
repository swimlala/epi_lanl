CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2016-04-19T19:16:07Z AOML 3.0 creation; 2016-06-01T00:08:30Z UW 3.1 conversion     
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20160419191607  20160531170830  5903740 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               �A   AO  4055_7112_148                   2C  D   APEX                            5374                            041511                          846 @ץ�6��1   @ץ���@;������c���n�1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    �A   A   A   @�ff@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/fD/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dtl�DyffD�#3D�P D��fD��fD�	�D�VfD�s3D��3D���D�9�D��3D�� D�fD�FfDڃ3D���D��D�33D�i�D�� 11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@˅AA%AEAeA��HA��HA��HA��HA��HA��HA��HA��HBp�B	p�Bp�Bp�B!p�B)p�B1p�B9p�BAp�BIp�BQp�BYp�Bap�Bip�Bqp�Byp�B��RB��RB��B��RB��RB��RB��RB��RB��RB��RB��RB��RB��RB��RB��RB��RB��RBĸRBȸRB̸RBиRBԸRBظRBܸRB�RB�RB�RB�RB�RB��RB��RB��RC \)C\)C\)C\)C\)C
\)C\)C\)C\)C\)C\)C\)C\)C\)C\)C\)C \)C"\)C$\)C&\)C(\)C*\)C,\)C.\)C0\)C2\)C4\)C6\)C8\)C:\)C<\)C>\)C@\)CB\)CD\)CF\)CH\)CJ\)CL\)CN\)CP\)CR\)CT\)CV\)CX\)CZ\)C\\)C^\)C`\)Cb\)Cd\)Cf\)Ch\)Cj\)Cl\)Cn\)Cp\)Cr\)Ct\)Cv\)Cx\)Cz\)C|\)C~\)C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.D 
D �
D
D�
D
D�
D
D�
D
D�
D
D�
D
D�
D
D�
D
D�
D	
D	�
D

D
�
D
D�
D
D�
D
D�
D
D�
D
D�
D
D�
D
D�
D
D�
D
D�
D
D�
D
D�
D
D�
D
D�
D
D�
D
D�
D
D�
D
D�
D
D�
D
D�
D
D�
D
D�
D 
D �
D!
D!�
D"
D"�
D#
D#�
D$
D$�
D%
D%�
D&
D&�
D'
D'�
D(
D(�
D)
D)�
D*
D*�
D+
D+�
D,
D,�
D-
D-�
D.
D.�
D/pD/�
D0
D0�
D1
D1�
D2
D2�
D3
D3�
D4
D4�
D5
D5�
D6
D6�
D7
D7�
D8
D8�
D9
D9�
D:
D:�
D;
D;�
D<
D<�
D=
D=�
D>
D>�
D?
D?�
D@
D@�
DA
DA�
DB
DB�
DC
DC�
DD
DD�
DE
DE�
DF
DF�
DG
DG�
DH
DH�
DI
DI�
DJ
DJ�
DK
DK�
DL
DL�
DM
DM�
DN
DN�
DO
DO�
DP
DP�
DQ
DQ�
DR
DR�
DS
DS�
DT
DT�
DU
DU�
DV
DV�
DW
DW�
DX
DX�
DY
DY�
DZ
DZ�
D[
D[�
D\
D\�
D]
D]�
D^
D^�
D_
D_�
D`
D`�
Da
Da�
Db
Db�
Dc
Dc�
Dd
Dd�
De
De�
Df
Df�
Dg
Dg�
Dh
Dh�
Di
Di�
Dj
Dj�
Dk
Dk�
Dl
Dl�
Dm
Dm�
Dn
Dn�
Do
Do�
Dp
Dp�
Dq
Dq�
Dr
Dr�
Ds
Ds�
Dt
Dt��Dy}pD�.�D�[�D���D���D�D�a�D�~�D�޸D�RD�ED���D�ۅD��D�Q�Dڎ�D��RD�RD�>�D�uD���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�bA�1A� �A� �A� �A��A�"�A�$�A�$�A�$�A�$�A�"�A�"�A� �A��A��A��A��A��A� �A�"�A� �A� �A� �A�"�A�$�A�$�A� �A� �A�"�A��A�oA�%A�A���A��/A��A�Q�A��TA�I�A��yA�x�A���A�33A��A�\)A�jA�A���A�bA�VA��FA�VA��^A��yA��hA���A���A��+A���A�-A��9A�^5A��A�%A���A�bA�9XA��A��`A��A�C�A�=qA��DA���A�$�A���A��9A�ĜA�
=A�;dA�/A�ƨA�7LA�{A�bA���A�I�A��A�VA���A�oA�7LA�n�A��`A���A��DA��A�/A�"�A��A�1'A�XA��AC�A|��A{�AzI�AwAu�Au/At��At�/At�DAsAr �Ap�yAn�+Am7LAlAj��Ai/Ae�Ab��Aa\)A`�A`^5A^��AXĜAV�AUt�AT�/AT�uAS`BAQ�^AO�7AN^5AMS�AL~�AK7LAIXAIAG��AE;dAC�;ACVAB�AA��AA;dA@�RA@A?l�A?A>{A=��A<�!A<5?A;�
A;;dA:r�A:  A9�A8n�A8-A7"�A6v�A5�wA4��A3+A1�A1�A0��A0  A/oA.-A-7LA,E�A+
=A)�#A)�FA)��A)�7A)33A'�A&�HA&ȴA&�RA&�\A&A%?}A$�A#hsA"=qA �A 5?A�A�A{A��A�hA��AffA��Ap�A33A�yAjA�^A��AM�A�^A�A�PA5?A1A;dAQ�At�A+A	
=A�7A=qA�A�uAA��AhsAoA�jAn�AA�A �A��A��AC�A z�A ZA E�A {A 1@���@�M�@��7@�b@�J@�!@�j@�ff@�@�!@�5?@�7@���@�"�@�!@��@�hs@䛦@��m@�l�@�-@�%@�A�@�ff@�/@ڧ�@ّh@�-@Դ9@��@�C�@ҟ�@�=q@�@��@��T@Ѻ^@с@�G�@Гu@ύP@ͺ^@�ƨ@�$�@�X@���@� �@�\)@�@���@�{@�A�@�@��@�ƨ@��@��u@��P@���@�ȴ@���@��@���@���@��@�@�v�@�ƨ@�n�@��@���@��u@�I�@���@�5?@�bN@�(�@���@��@�\)@��@���@���@��+@�n�@�M�@��T@�/@� �@�;d@��y@�
=@���@���@���@�X@�&�@��@�%@���@�%@��@�j@��w@��@��@��R@��\@�M�@��@�{@���@��#@���@�X@�(�@���@�o@���@�n�@�ff@�5?@��-@�`B@��@�%@���@���@���@�
=@�v�@�5?@���@��#@��#@��-@��`@�j@�1'@�b@�ƨ@�|�@�\)@�S�@�"�@��@�v�@���@��h@�/@���@�A�@�  @��m@�ƨ@��P@�C�@�33@���@��!@�5?@���@�@��^@�p�@���@�Ĝ@���@�z�@��;@��@��y@��!@���@���@��+@��+@�n�@�E�@�$�@�{@�{@�J@���@��T@���@��@���@�(�@�  @�|�@�o@���@�^5@�E�@�=q@�5?@��@�@��7@�7L@�%@���@�A�@���@���@��@�dZ@�S�@�+@��@�@��@��y@��@���@�~�@�ff@�=q@�J@��@���@���@�x�@�X@�7L@��j@�A�@��@�  @K�@~ȴ@~v�@~{@}��@}��@}�h@}`B@}?}@}V@|�@|�j@|j@{ƨ@{dZ@{"�@z~�@z�@x�@pA�@hĜ@_��@[�@T�@Nv�@Ep�@@  @:�H@2-@*~�@%V@ �u@�@�w@��@�@
��@ �@3311111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�bA�1A� �A� �A� �A��A�"�A�$�A�$�A�$�A�$�A�"�A�"�A� �A��A��A��A��A��A� �A�"�A� �A� �A� �A�"�A�$�A�$�A� �A� �A�"�A��A�oA�%A�A���A��/A��A�Q�A��TA�I�A��yA�x�A���A�33A��A�\)A�jA�A���A�bA�VA��FA�VA��^A��yA��hA���A���A��+A���A�-A��9A�^5A��A�%A���A�bA�9XA��A��`A��A�C�A�=qA��DA���A�$�A���A��9A�ĜA�
=A�;dA�/A�ƨA�7LA�{A�bA���A�I�A��A�VA���A�oA�7LA�n�A��`A���A��DA��A�/A�"�A��A�1'A�XA��AC�A|��A{�AzI�AwAu�Au/At��At�/At�DAsAr �Ap�yAn�+Am7LAlAj��Ai/Ae�Ab��Aa\)A`�A`^5A^��AXĜAV�AUt�AT�/AT�uAS`BAQ�^AO�7AN^5AMS�AL~�AK7LAIXAIAG��AE;dAC�;ACVAB�AA��AA;dA@�RA@A?l�A?A>{A=��A<�!A<5?A;�
A;;dA:r�A:  A9�A8n�A8-A7"�A6v�A5�wA4��A3+A1�A1�A0��A0  A/oA.-A-7LA,E�A+
=A)�#A)�FA)��A)�7A)33A'�A&�HA&ȴA&�RA&�\A&A%?}A$�A#hsA"=qA �A 5?A�A�A{A��A�hA��AffA��Ap�A33A�yAjA�^A��AM�A�^A�A�PA5?A1A;dAQ�At�A+A	
=A�7A=qA�A�uAA��AhsAoA�jAn�AA�A �A��A��AC�A z�A ZA E�A {A 1@���@�M�@��7@�b@�J@�!@�j@�ff@�@�!@�5?@�7@���@�"�@�!@��@�hs@䛦@��m@�l�@�-@�%@�A�@�ff@�/@ڧ�@ّh@�-@Դ9@��@�C�@ҟ�@�=q@�@��@��T@Ѻ^@с@�G�@Гu@ύP@ͺ^@�ƨ@�$�@�X@���@� �@�\)@�@���@�{@�A�@�@��@�ƨ@��@��u@��P@���@�ȴ@���@��@���@���@��@�@�v�@�ƨ@�n�@��@���@��u@�I�@���@�5?@�bN@�(�@���@��@�\)@��@���@���@��+@�n�@�M�@��T@�/@� �@�;d@��y@�
=@���@���@���@�X@�&�@��@�%@���@�%@��@�j@��w@��@��@��R@��\@�M�@��@�{@���@��#@���@�X@�(�@���@�o@���@�n�@�ff@�5?@��-@�`B@��@�%@���@���@���@�
=@�v�@�5?@���@��#@��#@��-@��`@�j@�1'@�b@�ƨ@�|�@�\)@�S�@�"�@��@�v�@���@��h@�/@���@�A�@�  @��m@�ƨ@��P@�C�@�33@���@��!@�5?@���@�@��^@�p�@���@�Ĝ@���@�z�@��;@��@��y@��!@���@���@��+@��+@�n�@�E�@�$�@�{@�{@�J@���@��T@���@��@���@�(�@�  @�|�@�o@���@�^5@�E�@�=q@�5?@��@�@��7@�7L@�%@���@�A�@���@���@��@�dZ@�S�@�+@��@�@��@��y@��@���@�~�@�ff@�=q@�J@��@���@���@�x�@�X@�7L@��j@�A�@��@�  @K�@~ȴ@~v�@~{@}��@}��@}�h@}`B@}?}@}V@|�@|�j@|j@{ƨ@{dZ@{"�@z~�@z�@x�@pA�@hĜ@_��@[�@T�@Nv�@Ep�@@  @:�H@2-@*~�@%V@ �u@�@�w@��@�@
��@ �@3311111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB5?B5?B5?B5?B5?B5?B5?B5?B5?B5?B5?B5?B5?B5?B49B49B49B49B49B49B49B49B49B49B49B49B49B49B49B49B49B33B2-B1'B1'B/B.B+B+B,B-B-B+B+B)�B%�B �B-B/B.B-B,B)�B&�B�B�BJB�B�/B��B�jB�!B��B��B�PB}�Br�BffB\)BN�B8RB"�BoB+B��B�B�HB�-Bu�BdZBW
BH�BA�B7LB#�B1BB
��B
��B
�B
�ZB
��B
ƨB
�dB
�-B
��B
�VB
�B
w�B
iyB
aHB
XB
T�B
N�B
H�B
<jB
2-B
)�B
�B
JB
+B
B
B
  B	��B	�B	�fB	��B	ŢB	�XB	�B	��B	x�B	bNB	VB	Q�B	L�B	=qB	�B	�B	�B	�B	�B	hB	B��B�B�B�sB�TB�)B�B��BǮBB�}B�wB�qB�^B�RB�FB�9B�3B�'B�!B�B�B�B��B��B��B��B��B��B��B��B��B��B�uB�hB�hB�\B�VB�\B�\B�VB�PB�DB�DB�DB�DB�=B�+B�+B�%B�%B�B�B�B� B|�By�Bu�Br�Bp�Bm�Bk�BgmBbNB^5B[#BZBYBXBW
BVBT�BR�BO�BN�BK�BJ�BG�BC�B?}B;dB9XB6FB33B.B,B+B)�B(�B(�B(�B'�B'�B&�B&�B&�B&�B%�B%�B$�B$�B$�B$�B#�B"�B"�B!�B�B�B�B�B�BuBuBuBuBoBhBoBoBoBoBuB�B{BuBoBoB{B{B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B �B#�B(�B,B.B.B0!B1'B2-B1'B2-B7LB9XB9XB8RB8RB8RB7LB6FB7LB7LB=qBD�BK�BQ�BP�BO�BN�BL�BJ�BI�BH�BG�BE�BD�BG�BH�BI�BK�BM�BO�BQ�BR�BT�BVBW
BW
BVBT�BZB\)B_;BdZBe`BhsBhsBjBjBk�Bl�Bm�Bn�Bz�B{�B�B�B�B�%B�1B�=B�=B�DB�DB�JB�PB�oB��B��B��B��B��B��B��B��B�B�B�B�B�B�'B�-B�3B�9B�9B�9B�?B�dB�}B��BĜBƨBȴB��B��B��B��B��B�B�
B�B�;B�TB�fB�fB�sB�B�B�B�B�B��B��B��B��B��B	B	B	B	1B	PB	oB	{B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	%�B	+B	,B	0!B	49B	9XB	<jB	=qB	=qB	=qB	>wB	B�B	D�B	G�B	I�B	L�B	P�B	T�B	W
B	XB	YB	ZB	[#B	\)B	]/B	^5B	^5B	_;B	aHB	bNB	cTB	e`B	gmB	hsB	k�B	l�B	m�B	n�B	o�B	t�B	y�B	{�B	|�B	� B	�B	�B	�B	�+B	�1B	�1B	�7B	�7B	�=B	�=B	�DB	�JB	�\B	�hB	�oB	��B	��B	��B	�jB	�B	�B	��B
	7B
{B
!�B
(�B
1'B
:^B
B�B
J�B
M�B
VB
[#B
bNB
hsB
k�B
o�B
u�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B5"B5!B5"B5"B5"B5"B5"B5$B5"B5"B5 B5"B5$B5"B4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B3B2B1B1B.�B-�B*�B*�B+�B,�B,�B*�B*�B)�B%�B �B,�B.�B-�B,�B+�B)�B&�B�B{B*B�B�B��B�LB��B��B�yB�.B}�Br�BfHB\BN�B83B"�BPB
B��B�nB�(B�Bu�Bd5BV�BH�BAjB7)B#�BB �B
��B
��B
�jB
�;B
��B
ƅB
�CB
�	B
��B
�6B
��B
w�B
iZB
a%B
W�B
T�B
N�B
H�B
<IB
2B
)�B
vB
-B
B
B
�B	��B	��B	�B	�LB	��B	ŇB	�;B	��B	��B	x�B	b6B	U�B	Q�B	L�B	=XB	�B	tB	�B	�B	zB	RB		B��B�B�oB�_B�@B�B��B��BǘB�zB�iB�dB�[B�JB�?B�3B�%B�B�B�B� B��B��B��B��B��B��B��B��B��B��B��B�sB�bB�VB�TB�KB�DB�KB�KB�DB�<B�2B�0B�2B�2B�*B�B�B�B�B�B�B��B�B|�By�Bu�Br�Bp�Bm}BkuBg\Bb<B^%B[BZ
BYBW�BV�BU�BT�BR�BO�BN�BK�BJ�BG�BC�B?kB;RB9GB65B3"B.B+�B*�B)�B(�B(�B(�B'�B'�B&�B&�B&�B&�B%�B%�B$�B$�B$�B$�B#�B"�B"�B!�B�B�B_B[BTBeBbBGBHBDB;BZB[BBB@BdBnBjBcBBB]BNBNBzBWB�ByB�B�B�B�B�B�B�B�B�B�B�B �B#�B(�B+�B-�B-�B0B1B2B1B2B77B9DB9BB8<B8<B8=B74B6.B75B75B=[BD�BK�BQ�BP�BO�BN�BL�BJ�BI�BH�BG�BE�BD�BG�BH�BI�BK�BM�BO�BQ�BR�BT�BU�BV�BV�BU�BT�BZB\B_#Bd@BeHBh[BhXBjfBjfBkmBlrBmxBn�Bz�B{�B��B��B�B�	B�B�"B�"B�)B�'B�/B�5B�TB�eB�oB��B��B��B��B��B��B��B��B��B��B� B�	B�B�B�B�B�B�"B�EB�`B�kBĀBƊBȖBʤBʦB˩B͵B��B��B��B��B�B�3B�EB�EB�TB�`B�vB�~B��B�B��B��B��B��B��B	�B	�B	�B	B	/B	KB	[B	aB	jB	mB	pB	qB	xB	�B	�B	�B	�B	�B	�B	�B	�B	�B	%�B	*�B	+�B	/�B	4B	94B	<HB	=PB	=NB	=PB	>TB	BmB	DyB	G�B	I�B	L�B	P�B	T�B	V�B	W�B	X�B	Y�B	Z�B	\B	]
B	^B	^B	_B	a%B	b)B	c2B	e;B	gIB	hMB	kaB	lfB	mmB	nqB	ozB	t�B	y�B	{�B	|�B	�B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�#B	�4B	�AB	�IB	�YB	�fB	��B	�DB	��B	�B	��B
	B
OB
!�B
(�B
0�B
:3B
BeB
J�B
M�B
U�B
Z�B
b#B
hHB
kYB
osB
u�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.36 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201605311708302016053117083020160531170830  AO  ARCAADJP                                                                    20160419191607    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20160419191607  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20160419191607  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160531170830  IP                  G�O�G�O�G�O�                