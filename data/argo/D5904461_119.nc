CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2016-05-21T19:15:41Z AOML 3.0 creation; 2016-08-07T21:36:46Z UW 3.1 conversion     
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
_FillValue                    g�   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  i�   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  q�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    y�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  {�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �|   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �(   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �8   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �<   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �L   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �P   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �T   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �XArgo profile    3.1 1.2 19500101000000  20160521191541  20160825183352  5904461 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               wA   AO  5286_8897_119                   2C  D   APEX                            6531                            072314                          846 @׭�/ߦ1   @׭��Q�N@4��1'�c>�t�1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    wA   B   B   @333@�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(ffB0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C�C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?fD?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dly�Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dt�3Dy��D�fD�L�D���D��fD�fD�I�D�vfD���D�3D�VfD�vfD��fD�  D�<�Dڙ�D��3D��D�<�D�fD��31111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @:=p@��@ÅAA!AAAaA��HA��HA��HA��HA��HA��HA��HA��HB p�Bp�Bp�Bp�B p�B(�
B0p�B8p�B@p�BHp�BPp�BXp�B`p�Bhp�Bpp�Bxp�B�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�k�B�k�B�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RC )C)C)C)C)C
)C)C)C)C)C5�C)C)C)C)C)C )C")C$)C&)C()C*)C,)C.)C0)C2)C4)C6)C8)C:)C<)C>)C@)CB)CD)CF)CH)CJ)CL)CN)CP)CR)CT)CV)CX)CZ)C\)C^)C`)Cb)Cd)Cf)Ch)Cj)Cl)Cn)Cp)Cr)Ct)Cv)Cx)Cz)C|)C~)C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C��C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C��C��C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�D 
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
D?pD?�
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
Dl��Dm
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
Dt�
Dt�=Dy��D�	�D�PRD��RD���D��D�MD�y�D��RD��D�Y�D�y�D���D��D�@RDڝD�ƸD�RD�@RD��D�Ƹ1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�=qA�=qA�=qA�A�A�C�A�E�A�I�A�G�A�I�A�K�A�K�A�M�A�O�A�O�A�O�A�O�A�K�A�K�A�K�A�Q�A�K�A�O�A�Q�A�Q�A�Q�A͗�A�dZA���A�z�Aʟ�A�JA�+A�"�A�dZA�9XA���Aĝ�A��/A�{A�t�A���A�jA�&�A���A�+A���A�%A�K�A�jA�/A�M�A� �A��A�1'A���A�A��\A�ĜA�5?A���A�^5A�z�A� �A�x�A�`BA���A�"�A�`BA���A���A�I�A��FA���A�z�A��A��RA���A�I�A�t�A�ffA���A�bNA�v�A��TA�?}A�jA��A���A���A�v�A�%A�5?A�/A�t�A�5?A��#A�VA��FA���A�\)A��!A�/A�x�A��A��PA���A���A�z�A��wAz�HAu�TAr�yAo33AmoAk\)Ai/Adv�Ab9XAa�wAadZA_�A]C�A\�AZI�AV��AR�`AP �AM��AL�AI��AH�AH~�AF��AD��ACdZAB~�AA��A@�yA@1A>�A>{A=�TA=�FA=O�A<1A:1'A8�9A7�mA6��A5&�A1�A0��A/�A.�/A-�-A+S�A(��A(�DA'�
A&$�A$�HA$ffA$JA#�A"��A"1A!��A!C�A ��A JAl�A+A�A�RA��A��AA�AA�A�RA5?A�A��An�A�A�
A+AJA`BA��A�uAbA/AE�A�A�jAE�AbA�;A�PA;dA�A��A��Az�A�A
�A
�A
  A	��A��A33A��A��A��Av�A�;A�HA�9A(�A��A
=A ��A ��A Ĝ@��;@�@���@�1'@���@��H@�%@��9@�M�@�&�@�I�@��;@�;d@�V@�Ĝ@�7@��@�ƨ@��@�+@�?}@��/@��@�?}@�A�@�F@�^5@�Ĝ@�1@�^5@�G�@���@�hs@ׅ@�
=@�J@�/@�V@��`@ԛ�@�(�@�K�@���@��/@��@��@��#@�x�@�Ĝ@̓u@�;d@ʧ�@�%@��@�V@ȴ9@��@�(�@��@�b@��;@ǥ�@�ȴ@�V@��#@�@��;@��@���@�z�@���@�@���@��@���@�C�@��@���@�5?@��7@��@��F@��F@��w@��w@���@��@���@���@�~�@�M�@��^@�V@��F@���@�v�@���@���@�Ĝ@��9@���@��@���@��H@��@��^@�x�@��@���@�Z@��F@�l�@�
=@���@���@���@�V@��u@��@��@�\)@��@�@�@���@��#@��-@���@�G�@��`@��@�bN@��@��w@��@��P@�dZ@�l�@�C�@�
=@���@�~�@�ff@��@��@���@�5?@���@��7@���@�G�@���@��/@�r�@�Q�@���@��@��u@�(�@�S�@��R@���@�=q@��#@�@���@���@�x�@�hs@�7L@�Ĝ@�z�@�(�@�ƨ@�+@���@���@�n�@�~�@�ff@��@�x�@�z�@�9X@�A�@��;@�K�@���@��+@���@�K�@�l�@�S�@���@�M�@���@��^@��@�`B@��@�Ĝ@��@��@�Q�@�1'@��@��;@�ƨ@�K�@��H@���@��R@���@���@�ȴ@�@��\@�J@�7L@��/@��u@��D@���@���@��@�j@� �@���@���@��@�\)@�33@�o@��y@��!@�^5@�J@�?}@��@���@��`@�Ĝ@�Z@��@�ƨ@�|�@��H@���@�~�@�5?@�{@���@���@�G�@�V@��`@��9@�Z@�(�@���@�S�@�33@�o@��H@�ȴ@�O�@��@w�P@l�j@fE�@]?}@V�@Q��@Ihs@B�@?
=@8Ĝ@0  @*��@$�@!�@@��@�@�R@11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111 A�=qA�=qA�=qA�A�A�C�A�E�A�I�A�G�A�I�A�K�A�K�A�M�A�O�A�O�A�O�A�O�A�K�A�K�A�K�A�Q�A�K�A�O�A�Q�A�Q�A�Q�A͗�A�dZA���A�z�Aʟ�A�JA�+A�"�A�dZA�9XA���Aĝ�A��/A�{A�t�A���A�jA�&�A���A�+A���A�%A�K�A�jA�/A�M�A� �A��A�1'A���A�A��\A�ĜA�5?A���A�^5A�z�A� �A�x�A�`BA���A�"�A�`BA���A���A�I�A��FA���A�z�A��A��RA���A�I�A�t�A�ffA���A�bNA�v�A��TA�?}A�jA��A���A���A�v�A�%A�5?A�/A�t�A�5?A��#A�VA��FA���A�\)A��!A�/A�x�A��A��PA���A���A�z�A��wAz�HAu�TAr�yAo33AmoAk\)Ai/Adv�Ab9XAa�wAadZA_�A]C�A\�AZI�AV��AR�`AP �AM��AL�AI��AH�AH~�AF��AD��ACdZAB~�AA��A@�yA@1A>�A>{A=�TA=�FA=O�A<1A:1'A8�9A7�mA6��A5&�A1�A0��A/�A.�/A-�-A+S�A(��A(�DA'�
A&$�A$�HA$ffA$JA#�A"��A"1A!��A!C�A ��A JAl�A+A�A�RA��A��AA�AA�A�RA5?A�A��An�A�A�
A+AJA`BA��A�uAbA/AE�A�A�jAE�AbA�;A�PA;dA�A��A��Az�A�A
�A
�A
  A	��A��A33A��A��A��Av�A�;A�HA�9A(�A��A
=A ��A ��A Ĝ@��;@�@���@�1'@���@��H@�%@��9@�M�@�&�@�I�@��;@�;d@�V@�Ĝ@�7@��@�ƨ@��@�+@�?}@��/@��@�?}@�A�@�F@�^5@�Ĝ@�1@�^5@�G�@���@�hs@ׅ@�
=@�J@�/@�V@��`@ԛ�@�(�@�K�@���@��/@��@��@��#@�x�@�Ĝ@̓u@�;d@ʧ�@�%@��@�V@ȴ9@��@�(�@��@�b@��;@ǥ�@�ȴ@�V@��#@�@��;@��@���@�z�@���@�@���@��@���@�C�@��@���@�5?@��7@��@��F@��F@��w@��w@���@��@���@���@�~�@�M�@��^@�V@��F@���@�v�@���@���@�Ĝ@��9@���@��@���@��H@��@��^@�x�@��@���@�Z@��F@�l�@�
=@���@���@���@�V@��u@��@��@�\)@��@�@�@���@��#@��-@���@�G�@��`@��@�bN@��@��w@��@��P@�dZ@�l�@�C�@�
=@���@�~�@�ff@��@��@���@�5?@���@��7@���@�G�@���@��/@�r�@�Q�@���@��@��u@�(�@�S�@��R@���@�=q@��#@�@���@���@�x�@�hs@�7L@�Ĝ@�z�@�(�@�ƨ@�+@���@���@�n�@�~�@�ff@��@�x�@�z�@�9X@�A�@��;@�K�@���@��+@���@�K�@�l�@�S�@���@�M�@���@��^@��@�`B@��@�Ĝ@��@��@�Q�@�1'@��@��;@�ƨ@�K�@��H@���@��R@���@���@�ȴ@�@��\@�J@�7L@��/@��u@��D@���@���@��@�j@� �@���@���@��@�\)@�33@�o@��y@��!@�^5@�J@�?}@��@���@��`@�Ĝ@�Z@��@�ƨ@�|�@��H@���@�~�@�5?@�{@���@���@�G�@�V@��`@��9@�Z@�(�@���@�S�@�33@�o@��HG�O�@�O�@��@w�P@l�j@fE�@]?}@V�@Q��@Ihs@B�@?
=@8Ĝ@0  @*��@$�@!�@@��@�@�R@11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111 ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
�JB
y�B
w�B
�7B
�=B
��B
�5BBuB%�B>wBW
B}�B��B�LB��B�yB��B��B�B(�B6FB�B
=B�BXBhsB�7B�bB�\B�%B}�B{�B�%B� B�B�B�B�TB�/B�fB�mBB,B^5BdZBR�B�fB��B�jB��B��B�Bl�BG�B,B%�B�B%B
��B
�yB
�`B
�B
�B
��B+BDB
��B
�B
�B
�B
�fB
�ZB
�B
�B
�BB
ƨB
�B
��B
�PB
}�B
]/B
E�B
1'B	��B	��B	�FB	��B	�B	x�B	k�B	L�B	=qB	:^B	6FB	,B	�B	bB	  B�B�
BȴB�wB�LB�!B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�hB�PB�DB�1B�B�1B�1B�7B�7B�7B�DB�JB�DB�=B�JB�VB�bB�\B�\B�oB�{B�{B��B��B��B��B��B��B��B�B�B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�?B�?B�?B�?B�XB�}B�jB�}B��B��BBB��BB�jB�XB�RB�^B�XB�RB�?B�B��B��B��B��B�B�B�!B�3B�LB�qB�dB�dB�dB�^B�dB�qB�}B��BŢBŢBȴB��B��B��B��B��B��B�B�B�sB�B�B�B�B�B��B��B��B	  B	B	B	B	JB	\B	\B	bB	bB	hB	�B	�B	VB	B	B	%B	uB	{B	hB	VB	PB	oB	�B	�B	�B	�B	�B	$�B	'�B	(�B	)�B	)�B	+B	6FB	9XB	9XB	8RB	7LB	6FB	49B	49B	7LB	;dB	>wB	A�B	A�B	C�B	I�B	K�B	I�B	L�B	Q�B	T�B	VB	XB	[#B	[#B	`BB	bNB	cTB	dZB	iyB	k�B	l�B	o�B	r�B	t�B	u�B	v�B	t�B	t�B	w�B	{�B	~�B	�B	�B	�B	�B	�+B	�7B	�DB	�JB	�JB	�PB	�bB	�uB	�uB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�'B	�3B	�9B	�9B	�FB	�FB	�FB	�FB	�RB	�XB	�jB	�wB	�wB	�wB	�wB	�}B	�}B	�}B	�}B	�}B	��B	��B	B	ÖB	ÖB	B	ÖB	ǮB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�
B	�B	�B	�B	�B	�B	�#B	�/B	�5B	�5B	�5B	�;B	�HB	�ZB	�mB	�mB	�`B	�`B	�mB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
DB
{B
�B
"�B
+B
1'B
5?B
=qB
A�B
E�B
M�B
Q�B
W
B
\)B
`BB
cTB
gmB
l�B
r�B
u�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111 B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
�FB
y�B
w�B
�3B
�7B
��B
�1BBpB%�B>mBWB}�B��B�EB��B�rB��B��B�B(�B6?B�B
4B�BXBhiB�,B�ZB�QB�B}�B{�B�B�B�B�B�tB�HB�'B�[B�aBB+�B^*BdMBR�B�]B��B�_B��B�xB�Bl�BG�B, B%�B�BB
��B
�pB
�XB
�B
�B
��B$B9B
��B
�B
�B
�|B
�^B
�NB
�B
�|B
�6B
ơB
�B
��B
�IB
}�B
]&B
E�B
1%B	��B	��B	�FB	��B	�B	x�B	k�B	L�B	=tB	:bB	6HB	,B	�B	iB	 B�B�BȼB�B�SB�(B�B� B��B��B��B��B��B��B��B��B��B��B��B��B��B�sB�ZB�NB�<B�'B�:B�:B�BB�@B�AB�PB�QB�LB�EB�TB�^B�kB�fB�gB�vB��B��B��B��B��B��B��B��B��B�B�B�B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�EB�FB�CB�CB�\B��B�nB��B��B��BBB��BB�pB�]B�WB�cB�\B�WB�EB�"B��B��B��B��B�B�B�%B�6B�OB�tB�kB�jB�iB�dB�jB�tB��B��BŦBŨBȹB��B��B��B��B��B� B�B� B�vB�B�B�B�B�B��B��B��B	 B	B	B	B	JB	]B	[B	dB	eB	jB	�B	�B	YB	 B	B	'B	sB	|B	jB	WB	TB	pB	�B	�B	�B	�B	�B	$�B	'�B	(�B	)�B	)�B	*�B	6DB	9UB	9UB	8PB	7JB	6CB	47B	47B	7KB	;cB	>uB	A�B	A�B	C�B	I�B	K�B	I�B	L�B	Q�B	T�B	VB	XB	[ B	[!B	`?B	bJB	cQB	dXB	itB	k�B	l�B	o�B	r�B	t�B	u�B	v�B	t�B	t�B	w�B	{�B	~�B	�B	�B	�B	�B	�(B	�2B	�?B	�GB	�FB	�KB	�]B	�qB	�rB	�pB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	��B	�B	�B	�B	�#B	�-B	�4B	�1B	�BB	�?B	�>B	�AB	�KB	�QB	�eB	�rB	�rB	�qB	�rB	�tB	�vB	�vB	�yB	�vB	�|B	��B	B	ÏB	ÑB	B	ÏB	ǧB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�)B	�0B	�.B	�.B	�4B	�?B	�RB	�dB	�fB	�XB	�ZB	�fB	�mB	�vB	�|B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��G�O�B
B
=B
qB
�B
"�B
*�B
1 B
56B
=fB
A�B
E�B
M�B
Q�B
V�B
\B
`7B
cIB
gdB
l�B
r�B
u�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111 <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.11 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201608071436472016080714364720160807143647  AO  ARCAADJP                                                                    20160521191541    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20160521191541  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20160521191541  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160807143647  IP                  G�O�G�O�G�O�                