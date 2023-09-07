CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2016-03-14T09:16:35Z AOML 3.0 creation; 2016-08-07T21:36:44Z UW 3.1 conversion     
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
resolution        :�o     �  U�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    ]�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  _�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    gx   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ix   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  qp   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    yh   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  {h   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    �`   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �`   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �X   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �(   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �,   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �0   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �4Argo profile    3.1 1.2 19500101000000  20160314091635  20160807143644  5904461 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               jA   AO  5286_8897_106                   2C  D   APEX                            6531                            072314                          846 @ל�%��[1   @ל�W:͌@2[�l�C��ce7KƧ�1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    jA   B   B   @9��@�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B���C   C  C  C  C  C
33C��C�fC�fC  C  C�fC  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4�C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd�Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$fD$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN�fDO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Dh��Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt` Dy9�D� D�C3D�p D�� D�fD�L�D�� D��3D�  D�P D�� D��3D�fD�<�Dڃ3D���D�3D�I�D�p D��f111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @@��@��@ÅAA!AAAaA��HA��HA��HA��HA��HA��HA��HA��HB p�Bp�Bp�Bp�B p�B(p�B0p�B8p�B@p�BHp�BPp�BXp�B`p�Bhp�Bpp�Bxp�B�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�k�B�C )C)C)C)C)C
O\C��C�C�C)C)C�C)C)C)C)C )C")C$)C&)C()C*)C,)C.)C0)C2)C45�C6)C8)C:)C<)C>)C@)CB)CD)CF)CH)CJ)CL)CN)CP)CR)CT)CV)CX)CZ)C\)C^)C`)Cb)Cd5�Cf)Ch)Cj)Cl)Cn)Cp)Cr)Ct)Cv)Cx)Cz)C|)C~)C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�D 
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
D$pD$�
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
DD
DD�
DE
DE�
DF
DF�
DG
DG�
DH
DH�
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
DN�pDO
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
Di �Di�
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
Dtg
Dy@�D��D�F�D�s�D��D��D�PRD���D�ָD�#�D�S�D���D�ƸD�	�D�@RDچ�D��RD��D�MD�s�D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��AƾwA���AƼjA���A���Aơ�Aƣ�A���AƶFAƥ�AƝ�AƟ�Aƣ�AƝ�Aƛ�AƟ�AƧ�AƩ�Aƙ�Aƣ�AƮA�t�A�I�A�G�A�E�A�E�A�G�A�I�A�G�A�G�A�G�A�G�A�E�A�E�A�I�A�K�A�S�A�XA�ZA�^5A�`BA�`BA�bNA�hsA�l�A�n�A�p�A�n�A�l�A�n�A�n�A�p�A�p�A�p�A�r�A�r�A�r�A�r�A�r�A�p�A�|�A��
A�M�A��A�bNA�\)A�p�A�ȴA�r�A�S�A�l�A�ZA�5?A�`BA��\A�`BA�  A�ƨA���A��PA��7A��A�p�A��A���A�
=A��A�l�A�K�A��A�?}A�S�A�p�A�I�A�dZA��A��yA���A�5?A��RA��FA��FA���A�+A�bA��/A�~�A���A�z�A�^5A�x�A�dZA�jA�x�A���A���A��RA;dA~Ay�At��As/Aq�wAm+AihsAd�HAa%AY��AW�PAS`BAQhsAO+AK�;AI�^AF�AC33AAƨA?x�A>��A=XA="�A=t�A=
=A<��A;?}A8M�A7G�A7%A6�+A6 �A5�^A5�A4I�A3�#A3��A0v�A-��A,�A)�
A'��A&�yA$��A"�A"A�A M�A��AhsAAbNAx�A��A��A��A�A��A�Ap�A;dA�RA��A��At�A�A��At�A �AjAp�A�yA(�A��AQ�A�mA��AXA
��A
n�A�A	�^A%A
9XA	7LA��A��AQ�AVA�wAp�A&�AVAM�A ȴA �\A�PAVA��A�RA��@�
=@��@���@�K�@��h@�1'@�ff@��;@���@�p�@�A�@���@蛦@�r�@�j@�bN@�I�@�I�@�A�@�(�@� �@�(�@�(�@�(�@�(�@���@���@�V@�(�@���@�@���@�+@݉7@�Q�@��@�z�@թ�@�A�@�S�@�=q@�Ĝ@�z�@·+@�(�@�+@ʏ\@��@���@�?}@��@�t�@Ĭ@\@Å@�b@�;d@§�@���@�r�@�S�@�J@�Ĝ@�33@��@���@��#@��-@�?}@��@�O�@���@��@��w@��@�n�@���@�@�E�@�-@���@�`B@�%@��j@��D@�9X@���@�ƨ@�|�@��@�
=@��@�E�@�`B@�/@�Ĝ@��m@��w@��@�C�@�33@�@���@�
=@�o@�o@�o@�"�@��y@��y@���@���@���@�5?@�x�@���@��;@�l�@��H@�5?@��#@�@��-@�/@�Ĝ@�j@�1@��;@�ƨ@��@���@��@�|�@�dZ@�K�@�"�@��@�v�@�J@���@�hs@���@�j@�Q�@�9X@�1'@�(�@� �@��@�1@��;@��P@�S�@�+@�n�@���@���@�X@�&�@�%@��u@���@�@��\@���@��@�bN@�j@��@�33@��@���@�-@��-@�?}@���@��/@��j@��u@�A�@�ƨ@�t�@�C�@�+@�@��R@���@��\@��+@�v�@�V@�=q@�5?@�-@��@��@�?}@��/@��j@��@��D@�z�@�r�@�j@�bN@�bN@�Z@�Z@�|�@�o@�
=@��@���@�ȴ@���@��!@���@���@��+@�n�@�V@�$�@��@��-@��h@��@�hs@�7L@�V@��`@��9@�r�@�I�@�  @���@�t�@�"�@�
=@�
=@��@��R@��+@�ff@�=q@��@��h@�?}@��@�V@�%@���@��j@� �@���@�ƨ@�dZ@�+@�+@�
=@���@��\@�E�@�@���@��7@�G�@��@��`@�r�@�1'@��@|��@r=q@i��@dZ@\�D@VV@M�h@G�P@?K�@7�P@0��@+��@%/@�@��@{@^5@�h@C�@��@�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  AƾwA���AƼjA���A���Aơ�Aƣ�A���AƶFAƥ�AƝ�AƟ�Aƣ�AƝ�Aƛ�AƟ�AƧ�AƩ�Aƙ�Aƣ�AƮA�t�A�I�A�G�A�E�A�E�A�G�A�I�A�G�A�G�A�G�A�G�A�E�A�E�A�I�A�K�A�S�A�XA�ZA�^5A�`BA�`BA�bNA�hsA�l�A�n�A�p�A�n�A�l�A�n�A�n�A�p�A�p�A�p�A�r�A�r�A�r�A�r�A�r�A�p�A�|�A��
A�M�A��A�bNA�\)A�p�A�ȴA�r�A�S�A�l�A�ZA�5?A�`BA��\A�`BA�  A�ƨA���A��PA��7A��A�p�A��A���A�
=A��A�l�A�K�A��A�?}A�S�A�p�A�I�A�dZA��A��yA���A�5?A��RA��FA��FA���A�+A�bA��/A�~�A���A�z�A�^5A�x�A�dZA�jA�x�A���A���A��RA;dA~Ay�At��As/Aq�wAm+AihsAd�HAa%AY��AW�PAS`BAQhsAO+AK�;AI�^AF�AC33AAƨA?x�A>��A=XA="�A=t�A=
=A<��A;?}A8M�A7G�A7%A6�+A6 �A5�^A5�A4I�A3�#A3��A0v�A-��A,�A)�
A'��A&�yA$��A"�A"A�A M�A��AhsAAbNAx�A��A��A��A�A��A�Ap�A;dA�RA��A��At�A�A��At�A �AjAp�A�yA(�A��AQ�A�mA��AXA
��A
n�A�A	�^A%A
9XA	7LA��A��AQ�AVA�wAp�A&�AVAM�A ȴA �\A�PAVA��A�RA��@�
=@��@���@�K�@��h@�1'@�ff@��;@���@�p�@�A�@���@蛦@�r�@�j@�bN@�I�@�I�@�A�@�(�@� �@�(�@�(�@�(�@�(�@���@���@�V@�(�@���@�@���@�+@݉7@�Q�@��@�z�@թ�@�A�@�S�@�=q@�Ĝ@�z�@·+@�(�@�+@ʏ\@��@���@�?}@��@�t�@Ĭ@\@Å@�b@�;d@§�@���@�r�@�S�@�J@�Ĝ@�33@��@���@��#@��-@�?}@��@�O�@���@��@��w@��@�n�@���@�@�E�@�-@���@�`B@�%@��j@��D@�9X@���@�ƨ@�|�@��@�
=@��@�E�@�`B@�/@�Ĝ@��m@��w@��@�C�@�33@�@���@�
=@�o@�o@�o@�"�@��y@��y@���@���@���@�5?@�x�@���@��;@�l�@��H@�5?@��#@�@��-@�/@�Ĝ@�j@�1@��;@�ƨ@��@���@��@�|�@�dZ@�K�@�"�@��@�v�@�J@���@�hs@���@�j@�Q�@�9X@�1'@�(�@� �@��@�1@��;@��P@�S�@�+@�n�@���@���@�X@�&�@�%@��u@���@�@��\@���@��@�bN@�j@��@�33@��@���@�-@��-@�?}@���@��/@��j@��u@�A�@�ƨ@�t�@�C�@�+@�@��R@���@��\@��+@�v�@�V@�=q@�5?@�-@��@��@�?}@��/@��j@��@��D@�z�@�r�@�j@�bN@�bN@�Z@�Z@�|�@�o@�
=@��@���@�ȴ@���@��!@���@���@��+@�n�@�V@�$�@��@��-@��h@��@�hs@�7L@�V@��`@��9@�r�@�I�@�  @���@�t�@�"�@�
=@�
=@��@��R@��+@�ff@�=q@��@��h@�?}@��@�V@�%@���@��j@� �@���@�ƨ@�dZ@�+@�+@�
=@���@��\@�E�@�@���@��7@�G�@��@��`@�r�@�1'G�O�@|��@r=q@i��@dZ@\�D@VV@M�h@G�P@?K�@7�P@0��@+��@%/@�@��@{@^5@�h@C�@��@�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB
/B
/B
/B
/B
/B
.B
.B
0!B
/B
.B
.B
.B
.B
-B
-B
.B
.B
/B
/B
/B
.B
.B
.B
.B
.B
/B
0!B
1'B
2-B
2-B
2-B
2-B
2-B
49B
5?B
6FB
8RB
:^B
:^B
;dB
;dB
<jB
<jB
>wB
?}B
@�B
@�B
A�B
A�B
A�B
A�B
A�B
B�B
B�B
B�B
B�B
B�B
B�B
C�B
C�B
M�B
��BB8RB�B��B�B\B!�B�B%�B�BB��B(�B!�B<jB)�B)�B$�B"�B!�B �B�B{B	7B��BBBBB$�B?}B�B�B�B��B�!B�^B�3B��B��B��B�uB�B[#B$�BbBB
�B
�;B
��B
��B
��B
�7B
r�B
YB
D�B
33B
&�B
+B	�fB	�B	��B	�B	�hB	s�B	YB	2-B	$�B	uB	%B��B�TB��B�wB�B��B��B��B��B��B�B�?B�9B�3B�9B�?B�LB�XB�^B�qB�dB�LB�3B�!B�'B�'B�B��B��B��B��B��B��B��B��B��B��B��B��B�B�dBƨBƨBĜB��B�jBB��B�^B�qB�dB�dB�dB�'B��B�oB�7B�B|�Bz�Bz�B|�B}�B{�Bz�By�B�B��B�B��BÖB��B�wB�qB��B��B��B��B�wB�LB��B��B�-BÖB��B��BǮB��B��B��BɺBƨBB��BĜBB�}B�qB�wB�wB�wB�wB�wB�wB�wB�}B�}B�}B�}B�}B�}B�}B�}B��BÖBBBBŢBŢBŢBƨBȴB��B��B��B��B��B��B�
B�B�/B�BB�HB�HB�NB�TB�ZB�HB�/B�HB�B��B	  B	B	+B	
=B	DB	1B	B��B��B�B�B�B�B�B�B�B��B��B��B	B	DB	�B	#�B	$�B	(�B	-B	0!B	2-B	33B	6FB	8RB	9XB	;dB	=qB	=qB	=qB	>wB	?}B	?}B	=qB	@�B	C�B	I�B	L�B	M�B	N�B	Q�B	Q�B	Q�B	R�B	R�B	S�B	S�B	S�B	T�B	T�B	VB	YB	\)B	bNB	dZB	e`B	ffB	l�B	n�B	p�B	p�B	s�B	w�B	|�B	� B	�B	�B	�B	�B	�B	�B	�B	�%B	�1B	�7B	�=B	�JB	�PB	�\B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�!B	�9B	�FB	�^B	�jB	�}B	B	ĜB	ŢB	ŢB	ŢB	ǮB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�#B	�)B	�)B	�)B	�/B	�/B	�5B	�HB	�TB	�TB	�ZB	�`B	�`B	�`B	�`B	�`B	�fB	�fB	�`B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
%B

=B

=B
{B
�B
"�B
)�B
/B
7LB
=qB
D�B
J�B
Q�B
W
B
]/B
`BB
ffB
k�B
n�B
s�B
v�B
z�B
~�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  B
/B
/B
/B
/B
/B
.B
.B
0B
/B
.B
.B
.B
.B
-B
-B
.B
.B
/B
/B
/B
.B
.B
.B
.B
.B
/B
0!B
1&B
2.B
2.B
2-B
2.B
2,B
4:B
5;B
6GB
8OB
:]B
:_B
;cB
;cB
<hB
<hB
>uB
?zB
@�B
@�B
A�B
A�B
A�B
A�B
A�B
B�B
B�B
B�B
B�B
B�B
B�B
C�B
C�B
M�B
��BB8KB�B��B�BNB!�B�B%�B�8B��B(�B!�B<aB)�B)�B$�B"�B!�B �B�BsB	-B��BB�B �BB$�B?sB�B��B�B��B�B�RB�(B��B��B��B�jB�B[B$�BWBB
�B
�5B
��B
�{B
��B
�.B
r�B
YB
D�B
3/B
&�B
&B	�dB	�B	��B	�B	�hB	s�B	YB	2/B	$�B	yB	)B��B�\B��B�}B�B��B��B��B��B��B�$B�HB�AB�:B�CB�HB�SB�`B�cB�yB�nB�UB�:B�(B�/B�0B�B��B��B��B��B��B��B��B��B��B��B��B��B�B�kBƭBƫBĢB��B�oBB��B�fB�uB�kB�jB�mB�,B��B�vB�AB�B|�Bz�Bz�B|�B}�B{�Bz�By�B�#B��B�B��BÜB��B�|B�vB��B��B��B��B�}B�RB��B��B�4BÜB��B��BǰB��B��B��BɾBƪBB��BĢBB��B�wB�{B�zB�zB�{B�{B�{B�zB��B��B�~B��B��B��B��B��B��BÙBBBBŤBŦBťBƬBȶB��B��B��B��B��B��B�B�B�1B�CB�KB�LB�PB�VB�]B�KB�1B�JB�B��B	 B	B	*B	
@B	FB	3B	B��B��B�B�B�B�B�B�B�B��B��B��B	B	DB	�B	#�B	$�B	(�B	-B	0 B	2,B	33B	6EB	8OB	9UB	;cB	=qB	=pB	=oB	>uB	?~B	?}B	=pB	@�B	C�B	I�B	L�B	M�B	N�B	Q�B	Q�B	Q�B	R�B	R�B	S�B	S�B	S�B	T�B	T�B	VB	YB	\(B	bIB	dXB	e[B	fbB	l�B	n�B	p�B	p�B	s�B	w�B	|�B	�B	�B	�B	�B	�B	�B	�B	�B	�!B	�,B	�1B	�7B	�EB	�LB	�WB	�yB	�}B	�|B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�4B	�?B	�YB	�dB	�vB	B	ĔB	ŝB	ŝB	ŜB	ǩB	ɴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	� B	�"B	�"B	�'B	�(B	�,B	�AB	�KB	�KB	�TB	�WB	�WB	�YB	�YB	�ZB	�]B	�^B	�VB	�xB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
 �B
 �B
 �B
 B
B
B
B
B
B
B
B
B
B
B
G�O�B

5B
sB
�B
"�B
)�B
/B
7BB
=gB
D�B
J�B
Q�B
V�B
]#B
`9B
f_B
k{B
n�B
s�B
v�B
z�B
~�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.11 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201608071436442016080714364420160807143644  AO  ARCAADJP                                                                    20160314091635    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20160314091635  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20160314091635  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160807143644  IP                  G�O�G�O�G�O�                