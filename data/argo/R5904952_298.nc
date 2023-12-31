CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-05T19:06:13Z creation      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.2    Conventions       Argo-3.2 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    6�   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  6�   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7(   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7h   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     7�   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8,   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~       axis      T           80   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    88   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~            8<   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8D   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8L   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8T   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8X   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8`   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9`   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    9d   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    9h   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    9l   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  9p   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  A<   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  C0   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  J�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  L�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  T�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ^|   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  fH   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  h<   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  p   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  w�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  y�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �T   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �    HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �$   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �(   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �,   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �0Argo profile    3.1 1.2 19500101000000  20181005190613  20181005190613  5904952 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL              *A   AO  6431                            2B  A   APEX                            7466                            062512                          846 @�����1   @��J	�@0�"��`B�c}��R1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                     *A   A   A   @@  @�  @�  A   A   A@  A`  A~ffA�  A�  A�  A�33A�33A�  A�  A�33B��B  B  B   B(ffB0ffB8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B���B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33C   C  C  C�fC  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C#�fC&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:�C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb�Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C{�fC~  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C��C��C��C�  C��3C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C��3C��3C��3C�  C�  C��C�  C�  C�  C�  C��C�  C�  C�  C�  C��C��C��C�  C��3C��3C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��D fD �fDfD� D  D� D  D�fD  D� D  Dy�D  D�fD  D� D��D� D	  D	y�D	��D
� D  D� D  D� D  D�fDfD� D  D� D  D� D  D� D  D� D  D� D  D� D  D� DfD� D  D� D  Dy�D  D� D  D� D  D� DfD� DfD�fDfD�fD  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%fD%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D*��D+y�D,  D,� D-  D-� D.  D.� D.��D/� D0  D0� D1  D1� D2  D2� D2��D3� D4  D4� D5  D5� D5��D6y�D7  D7�fD8  D8� D9  D9�fD:fD:� D:��D;y�D;��D<y�D=  D=� D>  D>� D?  D?� D?��D@� DA  DA� DB  DB� DC  DC� DDfDD� DE  DE� DF  DF� DG  DG�fDHfDH� DH��DI� DJ  DJ� DKfDK� DL  DL� DM  DM� DM��DN� DO  DOy�DP  DP�fDQ  DQ� DR  DR�fDS  DS� DS��DT� DUfDU� DVfDV� DW  DW� DX  DXy�DY  DY�fDZfDZ�fD[fD[�fD\fD\� D\��D]y�D]��D^y�D^��D_� D`  D`� Da  Da�fDb  Db� Dc  Dc� Dd  Dd� De  De� Df  Df�fDgfDg�fDhfDh� DifDi� Di��Djy�Dj��Dky�Dk��Dl� DmfDm� Dm��Dn� Do  Do� Dp  Dp� Dq  Dqy�Dq��Dr� DsfDs�fDt  Dt� Du  Du�fDv  Dv� Dw  Dw� Dw�3Dy�RD�<{D��\1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @W
>@��@˅AA%AEAeA�{A��HA��HA��HA�{A�{A��HA��HB
=B	
>Bp�Bp�B!p�B)�
B1�
B9p�BAp�BIp�BQp�BYp�Bap�Bip�Bqp�Byp�B��RB��B��B��RB��RB��RB��RB��RB��RB��RB��RB��RB��RB��RB��RB��B��BĸRBȸRB̸RBиRBԸRBظRBܸRB�RB�RB�RB�RB�RB��RB��RB��C \)C\)C\)CB�C\)C
\)C\)C\)C\)C\)C\)C\)C\)C\)C\)C\)C \)C"\)C$B�C&\)C(\)C*\)C,\)C.\)C0\)C2\)C4\)C6\)C8\)C:u�C<\)C>\)C@\)CB\)CD\)CF\)CH\)CJ\)CL\)CN\)CP\)CR\)CT\)CV\)CX\)CZ\)C\\)C^\)C`\)Cbu�Cd\)Cf\)Ch\)Cj\)Cl\)Cn\)Cp\)Cr\)Ct\)Cv\)Cx\)Cz\)C|B�C~\)C�.C�.C�.C�.C�.C�.C�.C�.C�!GC�.C�.C�!GC�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�:�C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�:�C�.C�.C�:�C�:�C�:�C�.C�!GC�.C�.C�.C�.C�.C�:�C�.C�.C�.C�.C�.C�.C�:�C�.C�.C�.C�.C�.C�.C�!GC�!GC�!GC�.C�.C�:�C�.C�.C�.C�.C�:�C�.C�.C�.C�.C�:�C�:�C�:�C�.C�!GC�!GC�.C�.C�:�C�.C�.C�.C�.C�.C�.C�.C�:�C�:�C�.C�.C�.C�.C�:�C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�:�D pD �pDpD�
D
D�
D
D�pD
D�
D
D��D
D�pD
D�
D�D�
D	
D	��D
�D
�
D
D�
D
D�
D
D�pDpD�
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
DpD�
D
D�
D
D��D
D�
D
D�
D
D�
DpD�
DpD�pDpD�pD
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
D%pD%�
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
D+�D+��D,
D,�
D-
D-�
D.
D.�
D/�D/�
D0
D0�
D1
D1�
D2
D2�
D3�D3�
D4
D4�
D5
D5�
D6�D6��D7
D7�pD8
D8�
D9
D9�pD:pD:�
D;�D;��D<�D<��D=
D=�
D>
D>�
D?
D?�
D@�D@�
DA
DA�
DB
DB�
DC
DC�
DDpDD�
DE
DE�
DF
DF�
DG
DG�pDHpDH�
DI�DI�
DJ
DJ�
DKpDK�
DL
DL�
DM
DM�
DN�DN�
DO
DO��DP
DP�pDQ
DQ�
DR
DR�pDS
DS�
DT�DT�
DUpDU�
DVpDV�
DW
DW�
DX
DX��DY
DY�pDZpDZ�pD[pD[�pD\pD\�
D]�D]��D^�D^��D_�D_�
D`
D`�
Da
Da�pDb
Db�
Dc
Dc�
Dd
Dd�
De
De�
Df
Df�pDgpDg�pDhpDh�
DipDi�
Dj�Dj��Dk�Dk��Dl�Dl�
DmpDm�
Dn�Dn�
Do
Do�
Dp
Dp�
Dq
Dq��Dr�Dr�
DspDs�pDt
Dt�
Du
Du�pDv
Dv�
Dw
Dw�
Dw�=Dy�\D�H D���1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A;wA�A�ƨA�ƨA�ȴA���A�ȴA�ƨA���Aͧ�A�r�A�VA�I�A�9XA�-A�"�A��A�1A���A��A���A̍PAˁA�(�AʃA��AɾwA�I�A�K�A���Aǩ�AǇ+A�;dAƛ�A���Aũ�A�C�Aĺ^A�M�A�-A�A�bNA���A�n�A�C�A��^A��jA���A�\)A�\)A��A��yA�S�A��-A��A�VA���A��uA�M�A��yA�XA���A�l�A��A�~�A�M�A��A���A�O�A�-A��A��A�^5A��A���A�~�A��jA��HA��A��A�  A���A�|�A��#A��-A��A�l�A�33A��A�ƨA�{A��A�x�A�^5A�?}A�hsA���A�"�A�~�A��A�"�A�ffA�"�A��;A�?}A�C�A&�A|��Ax�Aw��Av�!ApA�Al{AjffAg��Ad  Aat�A_��A^�!A\$�AW�AS;dAN$�AL-AI�AE�^AB�A@�uA?�A=�;A;��A8�+A6$�A4$�A3�mA3?}A1��A/&�A,�yA+dZA* �A)dZA)�A({A&�HA&I�A$bNA#+A"M�A ��An�An�A�uA�#A��A�/A��A��A�AhsAv�A"�A`BAĜA�wA�/An�A;dAE�AO�A
�HA
��A
jA
^5A
bNA
Q�A	�A	��A	�A	��A	�A	�A	�AJA+AĜA�A�DAZA1'A��A%A   @��@���@���@���@�x�@�`B@�t�@�G�@��j@�Z@��@�V@�z�@���@�r�@�ƨ@�\)@�{@���@��@�?}@�M�@�V@�b@��@�C�@���@�w@���@ꟾ@���@�$�@��@��@�A�@��@���@�x�@�t�@�ff@٩�@���@١�@�/@ش9@׶F@ٲ-@�M�@�I�@�J@��y@���@�A�@��y@�@�@�"�@�
=@�
=@��H@�@���@��@�G�@�9@�Q�@�  @߶F@ߝ�@ߕ�@ߕ�@ޟ�@���@�@���@ݩ�@ܛ�@�Z@�b@�K�@ڰ!@٩�@ش9@���@ם�@�S�@�+@�=q@���@���@�r�@��;@�o@�^5@��#@�G�@д9@�b@�+@�o@�o@�o@�o@�
=@�
=@�V@��@�9X@�l�@ʸR@�E�@��T@�V@Ǿw@�$�@�V@�ȴ@Ə\@���@�&�@�r�@��;@�ƨ@�C�@�S�@�K�@�33@��@��@���@�@�ff@�M�@�$�@��#@�7L@���@��u@�1'@���@�l�@��R@�ff@�@��^@�O�@���@��`@��u@�(�@���@�l�@�"�@���@���@��+@�=q@�%@�1'@�Q�@� �@��;@�l�@�;d@�v�@�&�@���@�Z@�A�@� �@�dZ@��@���@��+@�V@��^@��@�`B@��@��9@��@��
@�\)@��@�@��R@��7@��@��@���@��^@��^@��7@�G�@��/@�r�@��m@�+@���@�V@�X@�z�@�  @���@�ff@�-@���@���@��`@��@�/@��@���@��-@��h@�`B@�?}@���@�Z@�dZ@��@��H@�M�@�-@��@��-@�p�@�&�@��@�%@�Ĝ@�I�@���@�{@��#@��-@��7@�O�@�7L@��@��@��j@�1@���@��R@��T@�X@��@��j@��D@�9X@��m@��
@�\)@�33@�t�@���@�t�@�+@�"�@���@��H@�n�@���@�ƨ@��m@��@��
@��@��P@�"�@��@���@���@�=q@��@�@��7@�`B@��@��u@�A�@��;@��P@�S�@�33@��y@���@�5?@�{@���@��@��@��T@�X@�1'@��F@�l�@��H@�@��
@�\)@��@�~@�Y�@w��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A;wA�A�ƨA�ƨA�ȴA���A�ȴA�ƨA���Aͧ�A�r�A�VA�I�A�9XA�-A�"�A��A�1A���A��A���A̍PAˁA�(�AʃA��AɾwA�I�A�K�A���Aǩ�AǇ+A�;dAƛ�A���Aũ�A�C�Aĺ^A�M�A�-A�A�bNA���A�n�A�C�A��^A��jA���A�\)A�\)A��A��yA�S�A��-A��A�VA���A��uA�M�A��yA�XA���A�l�A��A�~�A�M�A��A���A�O�A�-A��A��A�^5A��A���A�~�A��jA��HA��A��A�  A���A�|�A��#A��-A��A�l�A�33A��A�ƨA�{A��A�x�A�^5A�?}A�hsA���A�"�A�~�A��A�"�A�ffA�"�A��;A�?}A�C�A&�A|��Ax�Aw��Av�!ApA�Al{AjffAg��Ad  Aat�A_��A^�!A\$�AW�AS;dAN$�AL-AI�AE�^AB�A@�uA?�A=�;A;��A8�+A6$�A4$�A3�mA3?}A1��A/&�A,�yA+dZA* �A)dZA)�A({A&�HA&I�A$bNA#+A"M�A ��An�An�A�uA�#A��A�/A��A��A�AhsAv�A"�A`BAĜA�wA�/An�A;dAE�AO�A
�HA
��A
jA
^5A
bNA
Q�A	�A	��A	�A	��A	�A	�A	�AJA+AĜA�A�DAZA1'A��A%A   @��@���@���@���@�x�@�`B@�t�@�G�@��j@�Z@��@�V@�z�@���@�r�@�ƨ@�\)@�{@���@��@�?}@�M�@�V@�b@��@�C�@���@�w@���@ꟾ@���@�$�@��@��@�A�@��@���@�x�@�t�@�ff@٩�@���@١�@�/@ش9@׶F@ٲ-@�M�@�I�@�J@��y@���@�A�@��y@�@�@�"�@�
=@�
=@��H@�@���@��@�G�@�9@�Q�@�  @߶F@ߝ�@ߕ�@ߕ�@ޟ�@���@�@���@ݩ�@ܛ�@�Z@�b@�K�@ڰ!@٩�@ش9@���@ם�@�S�@�+@�=q@���@���@�r�@��;@�o@�^5@��#@�G�@д9@�b@�+@�o@�o@�o@�o@�
=@�
=@�V@��@�9X@�l�@ʸR@�E�@��T@�V@Ǿw@�$�@�V@�ȴ@Ə\@���@�&�@�r�@��;@�ƨ@�C�@�S�@�K�@�33@��@��@���@�@�ff@�M�@�$�@��#@�7L@���@��u@�1'@���@�l�@��R@�ff@�@��^@�O�@���@��`@��u@�(�@���@�l�@�"�@���@���@��+@�=q@�%@�1'@�Q�@� �@��;@�l�@�;d@�v�@�&�@���@�Z@�A�@� �@�dZ@��@���@��+@�V@��^@��@�`B@��@��9@��@��
@�\)@��@�@��R@��7@��@��@���@��^@��^@��7@�G�@��/@�r�@��m@�+@���@�V@�X@�z�@�  @���@�ff@�-@���@���@��`@��@�/@��@���@��-@��h@�`B@�?}@���@�Z@�dZ@��@��H@�M�@�-@��@��-@�p�@�&�@��@�%@�Ĝ@�I�@���@�{@��#@��-@��7@�O�@�7L@��@��@��j@�1@���@��R@��T@�X@��@��j@��D@�9X@��m@��
@�\)@�33@�t�@���@�t�@�+@�"�@���@��H@�n�@���@�ƨ@��m@��@��
@��@��P@�"�@��@���@���@�=q@��@�@��7@�`B@��@��u@�A�@��;@��P@�S�@�33@��y@���@�5?@�{@���@��@��@��T@�X@�1'@��F@�l�@��H@�@��
@�\)@��@�~@�Y�@w��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
%�B
`BB
z�B
� B
�B
�\B
��B
ɺB
�`B  BB"�B49B5?BF�B]/Be`BhsBo�Bz�B� B�7B�hB�uB{�Bm�Bp�B�{B��B��B��B�B��B�HB�B��BBPBhBhBuB�B+B-B'�B33B5?B9XBE�BI�BVBaHBYBVBO�BK�BD�BB�BA�BA�B9XB9XB7LB%�BB�/B��BǮB�dB��B�BcTBA�B33B�B
��B
�#B
ĜB
��B
�bB
\)B
49B
oB	�B	��B	�B	�B	��B	��B	z�B	aHB	P�B	A�B	.B	�B	{B	DB��B�HB��B�FB�B��B��B��B�hB�\B�PB�1B�B�%B�7B�{B��B��B�VB�1B�PB�hB��B��B��B��B��B�B�9B�9B�?B�FB�FB�^B�qB�wB�}B�wB�jB�}B��BBǮBɺB��B��B��B��B��B�#B�/B�/B�BB�NB�TB�ZB�ZB�`B�fB�sB�B�B�B�B�B�B�B�B�B�B��B�B�;B�B�HB�TB�B�B		7B	
=B	%B	  B	B	+B	VB		7B	�B	/B	M�B	P�B	O�B	N�B	O�B	R�B	O�B	H�B	C�B	@�B	=qB	2-B	,B	)�B	(�B	-B	)�B	%�B	$�B	#�B	!�B	�B	oB	PB	+B	+B		7B	oB	�B	�B	�B	�B	2-B	9XB	K�B	\)B	cTB	cTB	o�B	�+B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�!B	�'B	�-B	�?B	�RB	�^B	�dB	�jB	�qB	�wB	�wB	��B	�}B	��B	��B	ÖB	ĜB	ĜB	ƨB	ǮB	ƨB	ƨB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	��B	��B	�
B	�B	�B	�B	�#B	�/B	�;B	�BB	�HB	�HB	�NB	�NB	�`B	�mB	�sB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�sB	�`B	�TB	�NB	�HB	�BB	�;B	�/B	�/B	�/B	�5B	�;B	�;B	�;B	�BB	�HB	�HB	�HB	�BB	�HB	�HB	�HB	�NB	�fB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
%B
%B
%B
B
B
B
B
B
B
B
B
B
  B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
+B
+B
%B
+B
	7B
+B
%B
	7B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
{B
oB
hB
bB
oB
�B
�B
�B
�B
 �B
4T2222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222 B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
%�B
`BB
z�B
� B
�B
�\B
��B
ɺB
�`B  BB"�B49B5?BF�B]/Be`BhsBo�Bz�B� B�7B�hB�uB{�Bm�Bp�B�{B��B��B��B�B��B�HB�B��BBPBhBhBuB�B+B-B'�B33B5?B9XBE�BI�BVBaHBYBVBO�BK�BD�BB�BA�BA�B9XB9XB7LB%�BB�/B��BǮB�dB��B�BcTBA�B33B�B
��B
�#B
ĜB
��B
�bB
\)B
49B
oB	�B	��B	�B	�B	��B	��B	z�B	aHB	P�B	A�B	.B	�B	{B	DB��B�HB��B�FB�B��B��B��B�hB�\B�PB�1B�B�%B�7B�{B��B��B�VB�1B�PB�hB��B��B��B��B��B�B�9B�9B�?B�FB�FB�^B�qB�wB�}B�wB�jB�}B��BBǮBɺB��B��B��B��B��B�#B�/B�/B�BB�NB�TB�ZB�ZB�`B�fB�sB�B�B�B�B�B�B�B�B�B�B��B�B�;B�B�HB�TB�B�B		7B	
=B	%B	  B	B	+B	VB		7B	�B	/B	M�B	P�B	O�B	N�B	O�B	R�B	O�B	H�B	C�B	@�B	=qB	2-B	,B	)�B	(�B	-B	)�B	%�B	$�B	#�B	!�B	�B	oB	PB	+B	+B		7B	oB	�B	�B	�B	�B	2-B	9XB	K�B	\)B	cTB	cTB	o�B	�+B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�!B	�'B	�-B	�?B	�RB	�^B	�dB	�jB	�qB	�wB	�wB	��B	�}B	��B	��B	ÖB	ĜB	ĜB	ƨB	ǮB	ƨB	ƨB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	��B	��B	�
B	�B	�B	�B	�#B	�/B	�;B	�BB	�HB	�HB	�NB	�NB	�`B	�mB	�sB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�sB	�`B	�TB	�NB	�HB	�BB	�;B	�/B	�/B	�/B	�5B	�;B	�;B	�;B	�BB	�HB	�HB	�HB	�BB	�HB	�HB	�HB	�NB	�fB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
%B
%B
%B
B
B
B
B
B
B
B
B
B
  B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
+B
+B
%B
+B
	7B
+B
%B
	7B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
{B
oB
hB
bB
oB
�B
�B
�B
�B
 �B
4T2222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.36 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181005190613                              AO  ARCAADJP                                                                    20181005190613    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181005190613  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181005190613  QCF$                G�O�G�O�G�O�8000            