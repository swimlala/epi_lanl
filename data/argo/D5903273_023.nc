CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2019-02-19T18:16:36Z creation      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.2    Conventions       Argo-3.2 CF-1.6    featureType       trajectoryProfile      comment_dmqc_operator         ZPRIMARY | https://orcid.org/0000-0001-7324-3159 | Matthew Alkire, University of Washington        @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    7   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    7(   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    7,   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    70   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    7@   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7P   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    7`   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  7h   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7�   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        8   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    8   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    8    DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     8$   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    8D   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    8H   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     8L   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     8l   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8�   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8�   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~       axis      T           8�   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8�   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~            8�   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8�   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8�   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
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
_FillValue                  0  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �(   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �,   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �0   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �4   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �8   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �x   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20190219181636  20200831164622  5903273 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               A   AO  3334                            2C  D   APEX                            4917                            041310                          846 @էes�?1   @է�b�\@79XbM��cI7KƧ�1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   B   B   @9��@�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A���A�  B   B  B  B  B   B(ffB0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C�C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  Dy�D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dp��Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dt�fDy��D���D�M�D�o�D��fD��D�T)D���D��HD�3D�O\D�r�D�D���D�.D�{�D๚D��RD�<)D�r=D�˅1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @@��@��@ÅAA!AAAaA��HA��HA��HA��HA��HA��HA�A��HB p�Bp�Bp�Bp�B p�B(�
B0p�B8p�B@p�BHp�BPp�BXp�B`p�Bhp�Bpp�Bxp�B�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RC )C5�C)C)C)C
)C)C)C)C)C)C)C)C)C)C)C )C")C$)C&)C()C*)C,)C.)C0)C2)C4)C6)C8)C:)C<)C>)C@)CB)CD)CF)CH)CJ)CL)CN)CP)CR)CT)CV)CX)CZ)C\)C^)C`)Cb)Cd)Cf)Ch)Cj)Cl)Cn)Cp)Cr)Ct)Cv)Cx)Cz)C|)C~)C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C��C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�D 
D �
D
D�
D
D�
D
D��D
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
Dl�
Dm
Dm�
Dn
Dn�
Do
Do�
Dp
Dp�
Dq �Dq�
Dr
Dr�
Ds
Ds�
Dt
Dt�
Dt�pDy��D��
D�QHD�s3D���D� �D�W�D��HD���D��D�R�D�vD��D��RD�1�D�
D�D���D�?�D�u�D��
1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�l�A�jA�jA�jA�jA�l�A�l�A�l�A�bNA�VA�33A��A�oA�JA�JA�
=A�A�  A���A��A��yA��;A���A�|�AɼjA�ffA�VA�%AȼjAȅAǅAƃAŸRA�M�A�x�A�
=A�XA�(�A�l�A�dZA� �A�JA��TA�E�A���A���A�1'A�~�A��hA��A�bA�jA�;dA� �A���A�?}A���A�S�A��A�`BA�7LA��^A���A�  A� �A��#A��A��A�S�A��A�ZA��jA��PA��FA��jA��PA�^5A�C�A��mA��TA��A��A���A�dZA�XA�G�A���A�hsA�  A�z�A��#A���A��RA�t�A� �A��+A�Q�A�K�A�"�A��A��DA�ȴA��;A�VA��A�ffA�t�A��A�=qA��
A�;dA��A���A�ƨA�C�A���A��uA�`BA�I�A��;A���A�r�A���A�$�A���A�~�A~E�Ay�TAwAt~�Aq?}AoAm��AlȴAk?}Ai�7Af=qAc\)A`��A^r�A[�FAY��AW�AT=qAP�AO"�AL�AK��AH��AF�jAFI�AE�AE+AD�DAB�HAA�A@��A>�RA<��A9�A6$�A5t�A5/A2��A2Q�A1��A0�jA0VA/7LA-A,�yA,A+��A*�yA*=qA)��A(ȴA'33A&(�A%�mA%x�A$��A$�DA#�A"��A �Ax�A��AQ�A33A{Al�Av�A`BA�9AI�A��A��A^5A�A��A�\A9XAbAA�AE�A�9A\)A�A9XAAI�A�wA	�A	O�A	VA�/AƨA  AĜA�AO�AVA�A��A�9A$�At�A �+A @��H@�bN@�S�@�r�@��F@��R@��u@�@�R@�u@@�x�@�j@�P@���@��m@�p�@�I�@�\@��@��@�|�@ݩ�@ܣ�@�A�@ڇ+@��@��@��@أ�@�I�@��;@�
=@��#@�bN@��@�X@���@Ь@�(�@ϥ�@��@�^5@��T@υ@�E�@�7L@���@�+@�^5@�(�@�b@�x�@�j@̓u@�  @�1@�t�@�~�@�{@ǶF@���@�p�@��@�9X@�Z@�r�@��@�n�@�-@��@��@�$�@�b@���@���@�G�@���@��\@�O�@���@��u@�  @�ƨ@��@�dZ@�
=@��y@���@�+@��@�@�/@�I�@���@�^5@�p�@���@�9X@�  @��
@��@�l�@�"�@���@�5?@��T@���@�x�@�X@�?}@�&�@�V@��`@��u@��D@��u@���@��u@��D@�r�@�I�@�K�@��R@�M�@�{@��@��@�{@�{@��@��@��-@��h@��-@��@�@���@�`B@�&�@�%@���@��9@��u@�r�@�9X@�t�@�E�@��h@�hs@���@���@���@�`B@�%@��u@�bN@�A�@�1'@���@�;d@��@��!@���@��7@��\@��R@�ff@�5?@�n�@��^@�`B@��u@���@�1'@���@�/@��w@���@��h@�7L@���@�j@��@��;@�o@�=q@��h@�/@���@��/@��9@��@�Q�@�9X@�1@��@�K�@�C�@�;d@�o@�n�@�$�@��T@��#@���@�G�@�%@���@��9@�z�@�b@��m@���@�K�@��@�ff@�=q@�E�@�-@�5?@�V@�V@�E�@�5?@�E�@�~�@���@�o@��@��R@�v�@�J@�x�@�r�@��
@��@��@��w@���@��@�K�@�"�@��@�ff@�J@���@��^@�X@��@���@��j@��9@��j@��9@�Q�@�I�@�A�@�A�@��u@}�d@o@O@h �@b �@\U2@R�1@Mc�@FQ@?��@8��@2�@-��@)7L@$�K@n/@O�@}�@�/@Vm@��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111 A�l�A�jA�jA�jA�jA�l�A�l�A�l�A�bNA�VA�33A��A�oA�JA�JA�
=A�A�  A���A��A��yA��;A���A�|�AɼjA�ffA�VA�%AȼjAȅAǅAƃAŸRA�M�A�x�A�
=A�XA�(�A�l�A�dZA� �A�JA��TA�E�A���A���A�1'A�~�A��hA��A�bA�jA�;dA� �A���A�?}A���A�S�A��A�`BA�7LA��^A���A�  A� �A��#A��A��A�S�A��A�ZA��jA��PA��FA��jA��PA�^5A�C�A��mA��TA��A��A���A�dZA�XA�G�A���A�hsA�  A�z�A��#A���A��RA�t�A� �A��+A�Q�A�K�A�"�A��A��DA�ȴA��;A�VA��A�ffA�t�A��A�=qA��
A�;dA��A���A�ƨA�C�A���A��uA�`BA�I�A��;A���A�r�A���A�$�A���A�~�A~E�Ay�TAwAt~�Aq?}AoAm��AlȴAk?}Ai�7Af=qAc\)A`��A^r�A[�FAY��AW�AT=qAP�AO"�AL�AK��AH��AF�jAFI�AE�AE+AD�DAB�HAA�A@��A>�RA<��A9�A6$�A5t�A5/A2��A2Q�A1��A0�jA0VA/7LA-A,�yA,A+��A*�yA*=qA)��A(ȴA'33A&(�A%�mA%x�A$��A$�DA#�A"��A �Ax�A��AQ�A33A{Al�Av�A`BA�9AI�A��A��A^5A�A��A�\A9XAbAA�AE�A�9A\)A�A9XAAI�A�wA	�A	O�A	VA�/AƨA  AĜA�AO�AVA�A��A�9A$�At�A �+A @��H@�bN@�S�@�r�@��F@��R@��u@�@�R@�u@@�x�@�j@�P@���@��m@�p�@�I�@�\@��@��@�|�@ݩ�@ܣ�@�A�@ڇ+@��@��@��@أ�@�I�@��;@�
=@��#@�bN@��@�X@���@Ь@�(�@ϥ�@��@�^5@��T@υ@�E�@�7L@���@�+@�^5@�(�@�b@�x�@�j@̓u@�  @�1@�t�@�~�@�{@ǶF@���@�p�@��@�9X@�Z@�r�@��@�n�@�-@��@��@�$�@�b@���@���@�G�@���@��\@�O�@���@��u@�  @�ƨ@��@�dZ@�
=@��y@���@�+@��@�@�/@�I�@���@�^5@�p�@���@�9X@�  @��
@��@�l�@�"�@���@�5?@��T@���@�x�@�X@�?}@�&�@�V@��`@��u@��D@��u@���@��u@��D@�r�@�I�@�K�@��R@�M�@�{@��@��@�{@�{@��@��@��-@��h@��-@��@�@���@�`B@�&�@�%@���@��9@��u@�r�@�9X@�t�@�E�@��h@�hs@���@���@���@�`B@�%@��u@�bN@�A�@�1'@���@�;d@��@��!@���@��7@��\@��R@�ff@�5?@�n�@��^@�`B@��u@���@�1'@���@�/@��w@���@��h@�7L@���@�j@��@��;@�o@�=q@��h@�/@���@��/@��9@��@�Q�@�9X@�1@��@�K�@�C�@�;d@�o@�n�@�$�@��T@��#@���@�G�@�%@���@��9@�z�@�b@��m@���@�K�@��@�ff@�=q@�E�@�-@�5?@�V@�V@�E�@�5?@�E�@�~�@���@�o@��@��R@�v�@�J@�x�@�r�@��
@��@��@��w@���@��@�K�@�"�@��@�ff@�J@���@��^@�X@��@���@��j@��9@��j@��9@�Q�@�I�@�A�G�O�@��u@}�d@o@O@h �@b �@\U2@R�1@Mc�@FQ@?��@8��@2�@-��@)7L@$�K@n/@O�@}�@�/@Vm@��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111 ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B&�B+B0!B33B/B�BB��B\BPBB	7B.BF�BG�B/B'�B:^BA�BH�BK�BP�B_;BgmBjBs�By�Bw�Bt�Bq�Bu�B� B�LB��B�}B�jB�FB��B�\Bz�BaHBhsB�JB}�B��B�?B�B�B��B�VB#�B�ZB�B�ZB�B9XBq�BR�B"�B!�B"�BhB
=B1'BT�B�B�
B�5BB%�B=qB:^BD�BJ�BP�BC�B-B�B�B�BZBA�B8RB0!B+B&�B�B1B
��B
�yB
�B
��B
�BB
��B
�7B
�B
}�B
q�B
cTB
;dB
#�B
DB	�B	�
B	�wB	��B	��B	�\B	�7B	|�B	l�B	T�B	@�B	/B	�B	PB	B�B�#BŢB�}B�FB�!B��B��B�B�-B�9B�'B�B�B��B��B��B�PB�B�+B�7B�oB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�bB�PB�JB�=B�B{�Bz�B�%B�B�DBz�Bu�B�=B��B�B�B~�Bz�Bq�Be`BaHBcTBcTBcTBdZBcTBcTBbNBbNBcTBcTBcTBcTBaHBaHB`BB^5B]/B]/B[#B[#B\)B\)B\)B[#B\)BZB[#B[#B[#B\)B\)B\)B]/B^5B]/B^5B`BB`BB_;B_;B`BB`BB`BB`BBaHBbNBe`BffBe`BgmBm�Bm�Bk�Bm�By�B� B�=B��B��B��B��B��B��B�B�-B�9B�FB�FB�?B�9B�3B�9B�dBÖBĜB��B��B��B��B�B�B��B��B��B��B��B�B�
B�B��B��B�B�B�B�B�B�B�#B�;B�B�B�B�B�B�B�B��B��B	B	B	%B	1B	JB	VB	\B	oB	{B	�B	�B	�B	�B	�B	�B	�B	�B	�B	!�B	#�B	&�B	&�B	&�B	'�B	,B	33B	7LB	;dB	>wB	A�B	B�B	E�B	J�B	W
B	[#B	^5B	bNB	gmB	iyB	jB	k�B	m�B	o�B	q�B	s�B	s�B	s�B	t�B	t�B	u�B	u�B	u�B	y�B	{�B	{�B	|�B	|�B	� B	�B	�B	�B	�B	�1B	�DB	�DB	�=B	�VB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�uB	�\B	�JB	�JB	�DB	�=B	�=B	�PB	�VB	�hB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�'B	�9B	�LB	�RB	�dB	�qB	�}B	��B	��B	��B	��B	B	ŢB	ɺB	��B	��B	��B	��B	��B	ȴB	ƨB	ǮB	ȴB	ȴB	ɺB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	��B	�?B
B
�B
YB
CB
 \B
(�B
5B
>�B
BuB
G�B
QhB
R�B
T{B
ZkB
bhB
e,B
hXB
l�B
s1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111 B�B�B�B�B�B�B�B�B�B�B~B{B~B{B{B~B~B~B~B~B{B~B{B�B�B&�B*�B0B3+B/B�BB��BRBEBB	,B.BF�BG�B/B'�B:RBA�BH�BK�BP�B_/BgfBjvBs�By�Bw�Bt�Bq�Bu�B�B�?BʸB�pB�eB�<B��B�TBz�Ba;BhhB�>B}�B��B�2B�B�|B��B�MB#�B�TB�uB�OB�B9MBq�BR�B"�B!�B"�B_B
1B1 BT�B�B��B�+BB%�B=fB:SBD�BJ�BP�BC�B-B|B��B�BZBA~B8IB0B*�B&�B�B&B
��B
�oB
�B
��B
�5B
�{B
�-B
�B
}�B
q�B
cJB
;\B
#�B
:B	�sB	�B	�mB	��B	��B	�QB	�+B	|�B	l~B	T�B	@wB	/B	�B	CB	 �B�B�BŖB�rB�;B�B��B��B��B�"B�,B�B��B�B��B��B��B�EB�B�B�)B�dB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�vB�SB�EB�=B�1B�B{�Bz�B�B�B�8Bz�Bu�B�-B�|B�B��B~�Bz�Bq�BeTBa;BcGBcHBcFBdLBcFBcGBb@Bb@BcGBcFBcFBcEBa;Ba:B`4B^(B]%B]$B[B[B\B\B\B[B\BZB[B[B[B\B\B\B]!B^'B]!B^)B`5B`6B_/B_.B`4B`5B`3B`5Ba9Bb?BeRBfXBeRBg^Bm�Bm�BkyBm�By�B�B�0B��B��B��B�yB��B��B�B� B�+B�9B�8B�2B�*B�$B�+B�XBÊBďB��B��B��B̾B��B�	B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�B�B�.B�wB�B�B�B�B�B�B��B��B	�B		B	B	#B	=B	GB	MB	`B	mB	qB	xB	�B	�B	�B	�B	�B	�B	�B	!�B	#�B	&�B	&�B	&�B	'�B	+�B	3$B	7>B	;UB	>iB	AxB	BB	E�B	J�B	V�B	[B	^'B	b>B	gaB	ilB	jpB	kwB	m�B	o�B	q�B	s�B	s�B	s�B	t�B	t�B	u�B	u�B	u�B	y�B	{�B	{�B	|�B	|�B	�B	��B	��B	��B	�B	�!B	�6B	�5B	�0B	�GB	�sB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�hB	�QB	�<B	�>B	�6B	�1B	�0B	�AB	�GB	�[B	�oB	�yB	�}B	�}B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�*B	�?B	�CB	�UB	�dB	�nB	�zB	�zB	�|B	�zB	B	ŒB	ɫB	��B	̾B	̿B	˷B	ʵB	ȦB	ƛB	ǝB	ȦB	ȤB	ɬB	ɬB	ʳB	˹B	̿B	̿B	̽B	̾B	̽B	��B	��B	��B	��B	��B	��B	��B	��B	� B	� B	� G�O�B	��B	�2B
B
�B
LB
2B
 NB
(�B
4�B
>�B
BhB
G�B
QWB
R�B
TnB
Z]B
bXB
eB
hJB
l�B
s
1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111 <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.11 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                                                                                                                                             202008311646222020083116462220200831164622  AO  ARCAADJP                                                                    20190219181636    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20190219181636  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20190219181636  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20200831164622  IP                  G�O�G�O�G�O�                