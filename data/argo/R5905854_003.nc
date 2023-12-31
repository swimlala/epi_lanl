CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2022-06-04T17:44:54Z creation;2022-06-04T17:44:55Z conversion to V3.1      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    6�   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  6�   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  78   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7x   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     7�   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8<   JULD               standard_name         time   	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~       axis      T           8@   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8H   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~            8L   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8T   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8\   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8d   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8h   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    8p   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    8t   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    8x   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8|   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9|   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  I   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  L�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `x   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  s�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �p   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  �h   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ͐   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �$   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �T   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �T   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �T   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �T   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    � Argo profile    3.1 1.2 19500101000000  20220604174454  20220610141505  5905854                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               A   JA                                  2B  A   APEX                            8422                            2.11.2                          846 @ث�b�1   @ث�W��$@.��S���d7�E���1   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @   @�  @�  A   AffA@  A`  A~ffA�  A�  A�  A�  A�  A�  A�  B   B  B  B��B��B'��B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�33B���B���B�  B���B�  B�  B�  B�  B�  B�33B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C�C  C
�C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>�C@  CB  CD�CF�CH  CJ  CK�fCN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct�Cv  Cx  Cz�C|33C}�fC�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'�fD(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D6��D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR�fDS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� DefDe� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�C3D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�3D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�331111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @%@��H@��HAp�A�
AAp�Aap�A�
A��RA��RA��RA��RAиRA�RA�RB \)B\)B\)B(�B��B'��B0\)B8\)B@\)BH\)BP\)BX\)B`\)Bh\)Bp\)Bx\)B�.B�.B�.B�.B�.B�.B�aGB�ǮB���B�.B�ǮB�.B�.B�.B�.B�.B�aGB�.B���B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.C 
C
C
C0�C
C
0�C
C
C
C
C
C
C
C
C
C
C 
C"
C$
C&
C(
C*
C,
C.
C0
C2
C4
C6
C8
C:
C<
C>0�C@
CB
CD0�CF0�CH
CJ
CK�pCN
CP
CR
CT
CV
CX
CZ
C\
C^
C`
Cb
Cd
Cf
Ch
Cj
Cl
Cn
Cp
Cr
Ct0�Cv
Cx
Cz0�C|J=C}�pC��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C���C���C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�RC��C��D �D ��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D	�D	��D
�D
��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D �D ��D!�D!��D"�D"��D#�D#��D$�D$��D%�D%��D&�D&��D'�D'�)D(�D(��D)�D)��D*�D*��D+�D+��D,�D,��D-�D-��D.�D.��D/�D/��D0�D0��D1�D1��D2�D2��D3�D3��D4�D4��D5�D5��D6�D6��D6�]D7��D8�D8��D9�D9��D:�D:��D;�D;��D<�D<��D=�D=��D>�D>��D?�D?��D@�D@��DA�DA��DB�DB��DC�DC��DD�DD��DE�DE��DF�DF��DG�DG��DH�DH��DI�DI��DJ�DJ��DK�DK��DL�DL��DM�DM��DN�DN��DO�DO��DP�DP��DQ�DQ��DR�DR�)DS�DS��DT�DT��DU�DU��DV�DV��DW�DW��DX�DX��DY�DY��DZ�DZ��D[�D[��D\�D\��D]�D]��D^�D^��D_�D_��D`�D`��Da�Da��Db�Db��Dc�Dc��Dd�Dd��De)De��Df�Df��Dg�Dg��Dh�Dh��Di�Di��Dj�Dj��Dk�Dk��Dl�Dl��Dm�Dm��Dn�Dn��Do�Do��Dp�Dp��Dq�Dq��Dr�Dr��Ds�Ds��Dt�Dt��Du�Du��Dv�Dv��Dw�Dw��Dx�Dx��Dy�Dy��Dz�Dz��D{�D{��D|�D|��D}�D}��D~�D~��D�D��D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D��D�D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D�D���D��D�B�DÂ�D���D��D�B�DĂ�D���D��D�B�Dł�D���D��D�B�DƂ�D���D��D�B�Dǂ�D���D��D�B�DȂ�D���D��D�B�Dɂ�D���D��D�B�Dʂ�D���D��D�B�D˂�D���D��D�B�D̂�D���D��D�B�D͂�D���D��D�B�D΂�D���D��D�B�Dς�D���D��D�B�DЂ�D���D��D�B�Dт�D���D��D�B�D҂�D���D��D�B�Dӂ�D���D��D�B�DԂ�D���D��D�B�DՂ�D���D��D�B�Dւ�D���D��D�B�Dׂ�D���D��D�FD؂�D���D��D�B�Dق�D���D��D�B�Dڂ�D���D��D�B�Dۂ�D���D��D�B�D܂�D���D��D�B�D݂�D���D��D�B�Dނ�D���D��D�B�D߂�D���D��D�B�D���D���D��D�B�D��D���D��D�B�D��D���D�D�B�D��D���D��D�B�D��D���D��D�B�D��D���D��D�B�D��D���D��D�B�D��D���D��D�B�D��D���D��D�B�D��D���D��D�B�D��D���D��D�B�D��D���D��D�B�D��D���D��D�B�D��D���D��D�B�D��D���D��D�B�D��D���D��D�B�D���D���D��D�B�D��D���D��D�B�D��D���D��D�B�D��D���D��D�B�D��D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�61111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A��HA��&A�̘A��[A�ӏA��^A��dA�ҽA�ϫA���A��aA��[A���A��A��A��EA��BA���A�TaAˤ�A��[A˺�A˄MA�G�A���AɻdA��A�~AǆYA�:�A�4�AƩ�AƋAƑ4AƓ�A�iA�P�A�;dA�\)A��A�H�A�<A�%A��VA��A�8�A��wA�iDA�MjA�GA�a�A��A� A�,�A��A��A���A�?�A�:�A�u�A��A�}�A�� A��A��A���A��6A���A�\A��dA�1A��A���A��A�_�A�`vA���A���A�A���A��$A� 'A��$A�*eA�;�A���A��A�e�A�� A�oA�@A{QAzXyAye�Ax�DAx��Aw��Auk�Aq]dAm��Aj��Ai?}AfffAc��A`K^A^
�A\�AZ�AX�	AU�AQخAM�QAJ�AG��AEdZAC�AAB��A@�A>�A<��A;ƨA9�A8C�A7�AA6�A4��A3�`A0�A. \A,�HA+��A+^5A)�A(��A'@OA&0�A$��A"�A!�_A��A��A�A��A�wA��A�A($AFA��A�A�A�A�Aj�AF�A��A4A��A�AxA�Ac AX�A@�A�A6A�A�HAVA��A~�A�$A	�A�}An�A�6A��A��A�8A�A&A�A��A�A�OA/AϫA�'A�KAVA��Ap�A��A
��A	�QA	+�A��Ay>A33A��A"�A��A7�A��AK^A��A�"Ag8A�A��A�A�7AVA!-ASA�XAHA7A��A �AA �_A J�@�x�@�q�@�($@��@���@��@��$@��F@�l�@��	@��@�Xy@��@���@���@��m@�M�@�v`@�{�@�c@�@�Z@�@��Y@�S�@�b@�d�@�=@�J�@�Dg@���@�6�@��@�@�*@��|@��@��@�'@�xl@ዬ@�<@��@��"@�tT@��@�|�@�2a@�m�@۬q@��p@ډ�@�,=@ٍP@�/�@���@�R�@��@�4@���@��@ԣ�@�j@�(�@ӷ@ӣn@�{J@�Dg@�;@҃@��@�� @�`B@ІY@��@�y�@�RT@�4@�B[@ͩ�@͆�@�.I@��B@̓u@��@ʰ!@�O@ɾw@�x@��@�:�@�8@��)@Ƅ�@�L0@�J@�t�@��"@�n�@��}@�m]@���@��@�Dg@��|@���@�>B@��)@���@�zx@�=@��F@��@��@��+@�/�@��6@�7L@�ی@��<@��@�H@�F@�o@��@�"h@���@�ߤ@�i�@��d@���@��@��9@�@���@�j�@��@���@�:�@���@�7L@���@���@�)_@��U@�ϫ@��@�ff@�?�@�4@���@���@� i@���@�Ft@�*�@��@�u@��+@��9@�o @��@�c @�-@�GE@��@��q@��'@�|@�iD@�8@��_@�:�@���@��0@�$t@�C-@���@��@�x�@�\�@��"@�h�@�$@�� @��0@���@���@�P�@���@�1'@���@���@��$@�m]@�&@�+@��U@�kQ@�2�@��w@�B�@��+@�	@��@�<6@��@��}@��x@�Q@�4@���@��@��@���@�q@�@���@�O@�S@��E@��D@�I�@�J@�?}@��@��e@�Ta@�_@��N@�X�@�0�@�-w@�*0@�)_@�,�@�&@��@��y@��b@��@���@��@�l"@�B[@��@��@��n@�8�@���@�e�@�@���@�$t@��@���@��<@���@�:�@�!�@��w@�B�@��@���@�p;@��@��;@��*@�\�@��2@���@���@�L0@�4n@�
�@��&@���@��:@�]�@�$t@��f@���@��o@�M@�3�@��@��@��@���@�K�@�q@���@��)@��_@�n�@�<�@�&�@�@��@��;@��	@�1�@�V@���@��B@���@���@�l�@�H@�!@��@��a@��^@���@���@�:�@���@��Y@��@�F@E9@~�y@~��@~_�@~-@~{@}��@|�D@|�@{��@{&@{o@{(@z��@z�@z=q@z
�@y�"@x�$@x��@xm�@x4n@x%�@w�
@wa@wS@v҉@v��@v��@v-@u��@u?}@t�@tz�@s��@r͟@rB[@ru@q�~@qj@q�@p��@p��@pbN@o��@oo@n�}@n{�@nc @n)�@m�>@m�@m?}@l��@l�@lI�@l�@k�A@k��@kZ�@jߤ@j��@jL0@j0U@j4@i�T@i�-@is�@iS&@i:�@i!�@h�	@h��@g��@gqv@g9�@g
=@f��@f��@fH�@f�@e�@eϫ@e?}@d�@d�z@d�o@dC-@c��@c��@ca@c$t@b��@b�2@b�@b��@ba|@a��@am]@a?}@a0�@a*0@a!�@a�@`��@`$@_خ@_�$@_H�@_o@^��@^GE@]�'@]e,@]Q�@]�@\��@\��@\�@\_@\D�@[��@Z�8@Z��@Z�x@Z5?@Y�@Y��@X�@XtT@X9X@W/�@V͟@V�'@VkQ@V&�@U�T@U��@U��@U=�@TɆ@T��@T�I@T��@T�_@TC-@S��@R�}@Rff@R1�@Q��@Q��@Q�7@Qp�@QG�@P�E@P~(@PN�@P-�@P�@O�]@O��@N��@N�B@Ni�@NGE@M�z@Mc@M:�@L�$@Lu�@L%�@K�@K@O@J�}@I�@I�@I�h@I(�@H�D@H>B@G�m@GF�@G�@F�s@F0U@E��@E5�@D��@D�o@C�+@C�@B^5@A�j@A��@A=�@@Ĝ@@PH@?ƨ@>�r@=�)@=?}@<�/@<>B@<G@;o�@;J#@;Mj@;F�@:�8@:V@9��@9^�@9 \@8�@8V�@7�m@7�*@7'�@6��@5�@5��@4e�@41@3��@3ݘ@3��@3�$@3\)@3\)@38@2�y@2�@2�@1�@1��@1w2@1!�@0�`@0��@0A�@/��@/��@/��@/1�@.�h@.��@.3�@-�D@-�@-@-IR@,�?@,�z@,�.@,]d@,D�@,*�@,�@+�a@+�V@+~�@+K�@+C@*�8@*ں@*�R@*n�@*u@)ԕ@)�z@)��@)�@)x�@)Y�@(�@(��@(U2@(~@'��@'��@'@O@&�M@&��@&�<@&v�@&�@%�h@%Q�@%	l@$��@$1@#�g@#��@#l�@#U�@#+@#�@#Y@#(@"��@"��@"�@"H�@!k�@!V@ �o@ !@خ@��@�$@!-@�@��@YK@�.@�#@�H@zx@A @�P@�@�v@�@A�@�@�K@�@@�:@.I@��@��@�r@i�@C�@e@Y�@@�@�@�@�z@z�@q@]d@Q�@PH@9X@G@��@RT@;d@�@^5@�@o @a�@^�@7L@	l@ѷ@�O@��@Z@�A@�@�&@�Q@�q@v`@E9@�"@}V@M�@&�@�.@��@��@��@o @S&@@�@ \@�@@@��@Ɇ@�$@�_@Q�@�@�]@�0@��@�@�@�q@��@dZ@@O@6z@o@�y@�!@�1@^5@?@+k@e@_@�Z@�T@�@J�@�@��@��@�@r�@V�@9X@!@�g@�P@�f@6z@�@
�@
��@
s�@
#:@	��@	��@	��@	k�@	Y�@	A @	(�@�/@�U@�D@ �@�@�&@�Q@�g@ƨ@��@�@@g�@�@��@ߤ1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A��HA��&A�̘A��[A�ӏA��^A��dA�ҽA�ϫA���A��aA��[A���A��A��A��EA��BA���A�TaAˤ�A��[A˺�A˄MA�G�A���AɻdA��A�~AǆYA�:�A�4�AƩ�AƋAƑ4AƓ�A�iA�P�A�;dA�\)A��A�H�A�<A�%A��VA��A�8�A��wA�iDA�MjA�GA�a�A��A� A�,�A��A��A���A�?�A�:�A�u�A��A�}�A�� A��A��A���A��6A���A�\A��dA�1A��A���A��A�_�A�`vA���A���A�A���A��$A� 'A��$A�*eA�;�A���A��A�e�A�� A�oA�@A{QAzXyAye�Ax�DAx��Aw��Auk�Aq]dAm��Aj��Ai?}AfffAc��A`K^A^
�A\�AZ�AX�	AU�AQخAM�QAJ�AG��AEdZAC�AAB��A@�A>�A<��A;ƨA9�A8C�A7�AA6�A4��A3�`A0�A. \A,�HA+��A+^5A)�A(��A'@OA&0�A$��A"�A!�_A��A��A�A��A�wA��A�A($AFA��A�A�A�A�Aj�AF�A��A4A��A�AxA�Ac AX�A@�A�A6A�A�HAVA��A~�A�$A	�A�}An�A�6A��A��A�8A�A&A�A��A�A�OA/AϫA�'A�KAVA��Ap�A��A
��A	�QA	+�A��Ay>A33A��A"�A��A7�A��AK^A��A�"Ag8A�A��A�A�7AVA!-ASA�XAHA7A��A �AA �_A J�@�x�@�q�@�($@��@���@��@��$@��F@�l�@��	@��@�Xy@��@���@���@��m@�M�@�v`@�{�@�c@�@�Z@�@��Y@�S�@�b@�d�@�=@�J�@�Dg@���@�6�@��@�@�*@��|@��@��@�'@�xl@ዬ@�<@��@��"@�tT@��@�|�@�2a@�m�@۬q@��p@ډ�@�,=@ٍP@�/�@���@�R�@��@�4@���@��@ԣ�@�j@�(�@ӷ@ӣn@�{J@�Dg@�;@҃@��@�� @�`B@ІY@��@�y�@�RT@�4@�B[@ͩ�@͆�@�.I@��B@̓u@��@ʰ!@�O@ɾw@�x@��@�:�@�8@��)@Ƅ�@�L0@�J@�t�@��"@�n�@��}@�m]@���@��@�Dg@��|@���@�>B@��)@���@�zx@�=@��F@��@��@��+@�/�@��6@�7L@�ی@��<@��@�H@�F@�o@��@�"h@���@�ߤ@�i�@��d@���@��@��9@�@���@�j�@��@���@�:�@���@�7L@���@���@�)_@��U@�ϫ@��@�ff@�?�@�4@���@���@� i@���@�Ft@�*�@��@�u@��+@��9@�o @��@�c @�-@�GE@��@��q@��'@�|@�iD@�8@��_@�:�@���@��0@�$t@�C-@���@��@�x�@�\�@��"@�h�@�$@�� @��0@���@���@�P�@���@�1'@���@���@��$@�m]@�&@�+@��U@�kQ@�2�@��w@�B�@��+@�	@��@�<6@��@��}@��x@�Q@�4@���@��@��@���@�q@�@���@�O@�S@��E@��D@�I�@�J@�?}@��@��e@�Ta@�_@��N@�X�@�0�@�-w@�*0@�)_@�,�@�&@��@��y@��b@��@���@��@�l"@�B[@��@��@��n@�8�@���@�e�@�@���@�$t@��@���@��<@���@�:�@�!�@��w@�B�@��@���@�p;@��@��;@��*@�\�@��2@���@���@�L0@�4n@�
�@��&@���@��:@�]�@�$t@��f@���@��o@�M@�3�@��@��@��@���@�K�@�q@���@��)@��_@�n�@�<�@�&�@�@��@��;@��	@�1�@�V@���@��B@���@���@�l�@�H@�!@��@��a@��^@���@���@�:�@���@��Y@��@�F@E9@~�y@~��@~_�@~-@~{@}��@|�D@|�@{��@{&@{o@{(@z��@z�@z=q@z
�@y�"@x�$@x��@xm�@x4n@x%�@w�
@wa@wS@v҉@v��@v��@v-@u��@u?}@t�@tz�@s��@r͟@rB[@ru@q�~@qj@q�@p��@p��@pbN@o��@oo@n�}@n{�@nc @n)�@m�>@m�@m?}@l��@l�@lI�@l�@k�A@k��@kZ�@jߤ@j��@jL0@j0U@j4@i�T@i�-@is�@iS&@i:�@i!�@h�	@h��@g��@gqv@g9�@g
=@f��@f��@fH�@f�@e�@eϫ@e?}@d�@d�z@d�o@dC-@c��@c��@ca@c$t@b��@b�2@b�@b��@ba|@a��@am]@a?}@a0�@a*0@a!�@a�@`��@`$@_خ@_�$@_H�@_o@^��@^GE@]�'@]e,@]Q�@]�@\��@\��@\�@\_@\D�@[��@Z�8@Z��@Z�x@Z5?@Y�@Y��@X�@XtT@X9X@W/�@V͟@V�'@VkQ@V&�@U�T@U��@U��@U=�@TɆ@T��@T�I@T��@T�_@TC-@S��@R�}@Rff@R1�@Q��@Q��@Q�7@Qp�@QG�@P�E@P~(@PN�@P-�@P�@O�]@O��@N��@N�B@Ni�@NGE@M�z@Mc@M:�@L�$@Lu�@L%�@K�@K@O@J�}@I�@I�@I�h@I(�@H�D@H>B@G�m@GF�@G�@F�s@F0U@E��@E5�@D��@D�o@C�+@C�@B^5@A�j@A��@A=�@@Ĝ@@PH@?ƨ@>�r@=�)@=?}@<�/@<>B@<G@;o�@;J#@;Mj@;F�@:�8@:V@9��@9^�@9 \@8�@8V�@7�m@7�*@7'�@6��@5�@5��@4e�@41@3��@3ݘ@3��@3�$@3\)@3\)@38@2�y@2�@2�@1�@1��@1w2@1!�@0�`@0��@0A�@/��@/��@/��@/1�@.�h@.��@.3�@-�D@-�@-@-IR@,�?@,�z@,�.@,]d@,D�@,*�@,�@+�a@+�V@+~�@+K�@+C@*�8@*ں@*�R@*n�@*u@)ԕ@)�z@)��@)�@)x�@)Y�@(�@(��@(U2@(~@'��@'��@'@O@&�M@&��@&�<@&v�@&�@%�h@%Q�@%	l@$��@$1@#�g@#��@#l�@#U�@#+@#�@#Y@#(@"��@"��@"�@"H�@!k�@!V@ �o@ !@خ@��@�$@!-@�@��@YK@�.@�#@�H@zx@A @�P@�@�v@�@A�@�@�K@�@@�:@.I@��@��@�r@i�@C�@e@Y�@@�@�@�@�z@z�@q@]d@Q�@PH@9X@G@��@RT@;d@�@^5@�@o @a�@^�@7L@	l@ѷ@�O@��@Z@�A@�@�&@�Q@�q@v`@E9@�"@}V@M�@&�@�.@��@��@��@o @S&@@�@ \@�@@@��@Ɇ@�$@�_@Q�@�@�]@�0@��@�@�@�q@��@dZ@@O@6z@o@�y@�!@�1@^5@?@+k@e@_@�Z@�T@�@J�@�@��@��@�@r�@V�@9X@!@�g@�P@�f@6z@�@
�@
��@
s�@
#:@	��@	��@	��@	k�@	Y�@	A @	(�@�/@�U@�D@ �@�@�&@�Q@�g@ƨ@��@�@@g�@�@��@ߤ1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B�OB��B��B�OB�OB��B��B�B�OB��B� B��B�B��B�MB��B��B� B	=VB	��B
#�B
c�B
~�B
�{B
�LB
��B
��B
�]B
�\B
��B
�B
��B
� B
��B
�B
�qB
��B
��BGBg�Bx�B��B�BیB��B��BdBdBBFB$�B&�B'RB'�B'�B(�B#:B�B,B�BB�BYB�BtB�B��B��B��B��B�B�BĜB� B��B�B��B��Bx�BeB=�B~B
�CB
� B
��B
��B
h�B
4�B
)�B
�B
�B	�B	��B	�.B	�B	�	B	��B	��B	�,B	cB	ncB	g�B	ZkB	N�B	A�B	9>B	33B	.IB	%,B	]B	.B	�B��B�B�@B�B�B��B�YBуB�NB��B�(B�jB�B��B�YB�=B�DB��B��B�FB�pB�[B��BѷB��B�oB�B�?BǮB�,B޸B�XB�[B��B�B	B	%FB	1�B	($B	\B	  B	 �B	[B	B	�B	 B	a�B	��B	�}B	�;B	�	B	��B	��B	�TB	��B	׍B	�B	�TB	�@B	�B	�B	̳B	�B	�6B	��B	�[B	�,B	ϑB	�B	��B	�_B	�HB	��B	�B	��B	�B	��B	ĶB	ňB	�gB	��B	��B	�0B	�DB	��B	�rB	��B	��B	�XB	�qB	�cB	�B	��B	��B	�;B	��B	�B	��B	��B	�OB	��B	��B	��B	�AB	�aB	�aB	ňB	�YB	��B	�B	��B	�mB	��B	�B	�7B	�~B	�"B	��B	�BB	уB	�B	�bB	�bB	��B	�4B	�hB	ѷB	�&B	�@B	өB	�@B	��B	��B	҉B	҉B	уB	�hB	�B	�}B	�\B	�B	�B	οB	�hB	ѷB	�:B	� B	҉B	��B	�,B	ԯB	�2B	ՁB	ևB	�$B	�YB	רB	רB	��B	�EB	�yB	�+B	�_B	خB	خB	��B	��B	��B	�B	�kB	�QB	�B	ٚB	��B	�QB	�QB	�kB	�QB	�7B	ڠB	یB	�	B	�WB	��B	��B	�]B	�)B	��B	�dB	�dB	�/B	ݘB	��B	�~B	��B	�!B	�VB	�pB	�;B	ߊB	�\B	��B	��B	�-B	�HB	�B	�hB	�B	�B	�B	�B	�mB	�fB	�2B	�B	�B	�B	��B	��B	�B	��B	��B	�B	�cB	�B	�UB	��B	��B	��B	��B	�B	�?B	�B	�?B	�lB	��B	�$B	�fB	��B	�*B	�dB	�B	�<B	��B	��B	��B	�B	�(B	�]B	�(B	�B	�jB	�B	�dB	�xB	��B	��B	�lB	�B	�B	��B	��B	�rB	�xB	�xB	�DB	�^B	�^B	�DB	�^B	�JB	��B	�0B	��B	��B	��B	��B	��B	�.B
 �B
'B
�B
�B
1B
�B
�B
B

�B
�B
JB
~B
�B
�B
VB
�B
�B
�B
�B
�B
BB
B
(B
B
(B
BB
�B
�B
�B
�B
\B
�B
�B
�B
�B
�B
B
�B
�B
4B
�B
�B
�B
B
HB
bB
}B
�B
4B
B
B
B
�B
�B
�B
�B
�B
�B
�B
@B
&B
aB
B
B
,B
�B
�B
�B
�B
�B
B
B
SB
SB
SB
�B
�B
�B
�B
�B
�B
9B
�B

B
9B
SB
mB
�B

B

B
�B
�B
�B
�B
B
�B
�B
�B
7B
#B
�B
/B
~B
�B
B
�B
�B
pB
�B
 vB
 �B
!bB
!�B
"NB
"�B
"�B
"�B
"�B
# B
#�B
$ZB
$�B
$�B
%,B
%�B
%�B
&2B
&2B
&2B
&�B
&�B
'RB
(
B
($B
(XB
(�B
)*B
)*B
)_B
)�B
)�B
*eB
*eB
*eB
*KB
*0B
+B
+kB
+�B
,WB
,�B
,�B
-CB
-wB
-�B
-�B
-�B
.IB
/OB
/OB
/�B
1'B
1[B
1[B
1vB
1�B
2|B
2aB
2�B
2�B
2�B
2�B
33B
3�B
4B
3�B
49B
4�B
4nB
4�B
4�B
4�B
4�B
5ZB
5?B
6+B
72B
7�B
7�B
88B
8B
7�B
7�B
7�B
7fB
8�B
9XB
9�B
9�B
:B
:*B
:^B
:DB
:�B
:�B
:�B
;JB
;B
;�B
;�B
<6B
<�B
="B
=<B
=<B
=<B
=qB
=�B
=�B
=�B
=�B
=�B
=�B
>BB
>�B
>�B
>�B
?B
?B
?cB
?�B
?�B
?�B
?�B
@iB
@�B
@�B
@�B
AB
A;B
AUB
A�B
A�B
A�B
A�B
A�B
A�B
BB
B[B
B�B
B�B
B�B
B�B
B�B
B�B
B�B
CGB
C{B
C{B
C�B
C�B
C�B
D3B
D�B
D�B
D�B
EB
EB
EB
E9B
EB
EB
E�B
E�B
E�B
E�B
E�B
F%B
F�B
G+B
G+B
GB
HKB
H1B
HB
HfB
H�B
H�B
H�B
H�B
H�B
IB
IB
IB
IB
IB
IB
I�B
JXB
J=B
JrB
J�B
KB
K�B
K�B
LB
L~B
L�B
L~B
LJB
L0B
K�B
K�B
L~B
L~B
MB
M�B
M�B
N�B
O(B
OB
O�B
PB
PB
PbB
P.B
P�B
QhB
RB
R B
R�B
R�B
RoB
R�B
SB
S@B
S�B
S�B
S�B
TB
TFB
T{B
UB
VB
VmB
VB
VB
VB
V9B
V�B
W�B
W�B
W�B
W�B
W�B
W�B
W�B
W�B
W�B
W�B
W�B
W�B
X_B
X�B
X�B
YB
Y�B
ZB
ZB
Z�B
Z�B
[#B
Z�B
\B
[�B
[�B
\B
\B
\)B
\�B
\�B
]~B
]�B
]�B
^B
^B
^jB
^�B
^�B
_!B
_pB
_�B
_�B
_�B
`B
`\B
`�B
`�B
`�B
aB
aB
aHB
bNB
b�B
b�B
b�B
c B
c B
c:B
cnB
c�B
c�B
c�B
c�B
c�B
c�B
c�B
c�B
c�B
c�B
c�B
dB
c�B
dB
c�B
c�B
d�B
eFB
e�B
f2B
fLB
f�B
f�B
gB
f�B
gB
gB
g�B
g�B
g�B
h
B
hXB
h�B
h�B
iB
i_B
iyB
i�B
i�B
i�B
i�B
i�B
i�B
i�B
i�B
j�B
kB
kkB
k�B
lB
lB
l=B
l�B
l�B
l�B
mB
m]B
mwB
m�B
m�B
m�B
nIB
n/B
nB
n/B
n�B
n�B
n�B
o B
o B
o�B
o�B
pB
p!B
p;B
pUB
p;B
qB
p�B
qB
qB
q'B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
rB
r|B
r�B
r�B
r�B
s�B
tTB
tnB
tTB
tTB
t�B
t�B
t�B
t�B
t�B
u?B
u�B
u�B
u�B
u�B
u�B
vB
v+B
v`B
v�B
wB
wLB
wfB
w�B
w�B
w�B
w�B
xB
xRB
x8B
x8B
x8B
x8B
xRB
xRB
x�B
x�B
x�B
x�B
y>B
y>B
y�B
y�B
y�B
y�B
y�B
y�B
y�B
zB
zB
z*B
z^B
z�B
z�B
z�B
z�B
{B
z�B
{B
{B
{B
{B
{�B
|PB
|jB
|�B
|�B
|�B
|�B
|�B
|�B
}VB
}�B
}�B
~B
~B
~(B
~]B
~�B
~�B
HB
}B
�B
�B
�B
�B
�B
�4B
�4B
��B
�B
� B
�;B
�;B
�;B
�UB
�UB
�UB
��B
�B
�'B
�A1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B�OB��B��B�OB�OB��B��B�B�OB��B� B��B�B��B�MB��B��B� B	=VB	��B
#�B
c�B
~�B
�{B
�LB
��B
��B
�]B
�\B
��B
�B
��B
� B
��B
�B
�qB
��B
��BGBg�Bx�B��B�BیB��B��BdBdBBFB$�B&�B'RB'�B'�B(�B#:B�B,B�BB�BYB�BtB�B��B��B��B��B�B�BĜB� B��B�B��B��Bx�BeB=�B~B
�CB
� B
��B
��B
h�B
4�B
)�B
�B
�B	�B	��B	�.B	�B	�	B	��B	��B	�,B	cB	ncB	g�B	ZkB	N�B	A�B	9>B	33B	.IB	%,B	]B	.B	�B��B�B�@B�B�B��B�YBуB�NB��B�(B�jB�B��B�YB�=B�DB��B��B�FB�pB�[B��BѷB��B�oB�B�?BǮB�,B޸B�XB�[B��B�B	B	%FB	1�B	($B	\B	  B	 �B	[B	B	�B	 B	a�B	��B	�}B	�;B	�	B	��B	��B	�TB	��B	׍B	�B	�TB	�@B	�B	�B	̳B	�B	�6B	��B	�[B	�,B	ϑB	�B	��B	�_B	�HB	��B	�B	��B	�B	��B	ĶB	ňB	�gB	��B	��B	�0B	�DB	��B	�rB	��B	��B	�XB	�qB	�cB	�B	��B	��B	�;B	��B	�B	��B	��B	�OB	��B	��B	��B	�AB	�aB	�aB	ňB	�YB	��B	�B	��B	�mB	��B	�B	�7B	�~B	�"B	��B	�BB	уB	�B	�bB	�bB	��B	�4B	�hB	ѷB	�&B	�@B	өB	�@B	��B	��B	҉B	҉B	уB	�hB	�B	�}B	�\B	�B	�B	οB	�hB	ѷB	�:B	� B	҉B	��B	�,B	ԯB	�2B	ՁB	ևB	�$B	�YB	רB	רB	��B	�EB	�yB	�+B	�_B	خB	خB	��B	��B	��B	�B	�kB	�QB	�B	ٚB	��B	�QB	�QB	�kB	�QB	�7B	ڠB	یB	�	B	�WB	��B	��B	�]B	�)B	��B	�dB	�dB	�/B	ݘB	��B	�~B	��B	�!B	�VB	�pB	�;B	ߊB	�\B	��B	��B	�-B	�HB	�B	�hB	�B	�B	�B	�B	�mB	�fB	�2B	�B	�B	�B	��B	��B	�B	��B	��B	�B	�cB	�B	�UB	��B	��B	��B	��B	�B	�?B	�B	�?B	�lB	��B	�$B	�fB	��B	�*B	�dB	�B	�<B	��B	��B	��B	�B	�(B	�]B	�(B	�B	�jB	�B	�dB	�xB	��B	��B	�lB	�B	�B	��B	��B	�rB	�xB	�xB	�DB	�^B	�^B	�DB	�^B	�JB	��B	�0B	��B	��B	��B	��B	��B	�.B
 �B
'B
�B
�B
1B
�B
�B
B

�B
�B
JB
~B
�B
�B
VB
�B
�B
�B
�B
�B
BB
B
(B
B
(B
BB
�B
�B
�B
�B
\B
�B
�B
�B
�B
�B
B
�B
�B
4B
�B
�B
�B
B
HB
bB
}B
�B
4B
B
B
B
�B
�B
�B
�B
�B
�B
�B
@B
&B
aB
B
B
,B
�B
�B
�B
�B
�B
B
B
SB
SB
SB
�B
�B
�B
�B
�B
�B
9B
�B

B
9B
SB
mB
�B

B

B
�B
�B
�B
�B
B
�B
�B
�B
7B
#B
�B
/B
~B
�B
B
�B
�B
pB
�B
 vB
 �B
!bB
!�B
"NB
"�B
"�B
"�B
"�B
# B
#�B
$ZB
$�B
$�B
%,B
%�B
%�B
&2B
&2B
&2B
&�B
&�B
'RB
(
B
($B
(XB
(�B
)*B
)*B
)_B
)�B
)�B
*eB
*eB
*eB
*KB
*0B
+B
+kB
+�B
,WB
,�B
,�B
-CB
-wB
-�B
-�B
-�B
.IB
/OB
/OB
/�B
1'B
1[B
1[B
1vB
1�B
2|B
2aB
2�B
2�B
2�B
2�B
33B
3�B
4B
3�B
49B
4�B
4nB
4�B
4�B
4�B
4�B
5ZB
5?B
6+B
72B
7�B
7�B
88B
8B
7�B
7�B
7�B
7fB
8�B
9XB
9�B
9�B
:B
:*B
:^B
:DB
:�B
:�B
:�B
;JB
;B
;�B
;�B
<6B
<�B
="B
=<B
=<B
=<B
=qB
=�B
=�B
=�B
=�B
=�B
=�B
>BB
>�B
>�B
>�B
?B
?B
?cB
?�B
?�B
?�B
?�B
@iB
@�B
@�B
@�B
AB
A;B
AUB
A�B
A�B
A�B
A�B
A�B
A�B
BB
B[B
B�B
B�B
B�B
B�B
B�B
B�B
B�B
CGB
C{B
C{B
C�B
C�B
C�B
D3B
D�B
D�B
D�B
EB
EB
EB
E9B
EB
EB
E�B
E�B
E�B
E�B
E�B
F%B
F�B
G+B
G+B
GB
HKB
H1B
HB
HfB
H�B
H�B
H�B
H�B
H�B
IB
IB
IB
IB
IB
IB
I�B
JXB
J=B
JrB
J�B
KB
K�B
K�B
LB
L~B
L�B
L~B
LJB
L0B
K�B
K�B
L~B
L~B
MB
M�B
M�B
N�B
O(B
OB
O�B
PB
PB
PbB
P.B
P�B
QhB
RB
R B
R�B
R�B
RoB
R�B
SB
S@B
S�B
S�B
S�B
TB
TFB
T{B
UB
VB
VmB
VB
VB
VB
V9B
V�B
W�B
W�B
W�B
W�B
W�B
W�B
W�B
W�B
W�B
W�B
W�B
W�B
X_B
X�B
X�B
YB
Y�B
ZB
ZB
Z�B
Z�B
[#B
Z�B
\B
[�B
[�B
\B
\B
\)B
\�B
\�B
]~B
]�B
]�B
^B
^B
^jB
^�B
^�B
_!B
_pB
_�B
_�B
_�B
`B
`\B
`�B
`�B
`�B
aB
aB
aHB
bNB
b�B
b�B
b�B
c B
c B
c:B
cnB
c�B
c�B
c�B
c�B
c�B
c�B
c�B
c�B
c�B
c�B
c�B
dB
c�B
dB
c�B
c�B
d�B
eFB
e�B
f2B
fLB
f�B
f�B
gB
f�B
gB
gB
g�B
g�B
g�B
h
B
hXB
h�B
h�B
iB
i_B
iyB
i�B
i�B
i�B
i�B
i�B
i�B
i�B
i�B
j�B
kB
kkB
k�B
lB
lB
l=B
l�B
l�B
l�B
mB
m]B
mwB
m�B
m�B
m�B
nIB
n/B
nB
n/B
n�B
n�B
n�B
o B
o B
o�B
o�B
pB
p!B
p;B
pUB
p;B
qB
p�B
qB
qB
q'B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
rB
r|B
r�B
r�B
r�B
s�B
tTB
tnB
tTB
tTB
t�B
t�B
t�B
t�B
t�B
u?B
u�B
u�B
u�B
u�B
u�B
vB
v+B
v`B
v�B
wB
wLB
wfB
w�B
w�B
w�B
w�B
xB
xRB
x8B
x8B
x8B
x8B
xRB
xRB
x�B
x�B
x�B
x�B
y>B
y>B
y�B
y�B
y�B
y�B
y�B
y�B
y�B
zB
zB
z*B
z^B
z�B
z�B
z�B
z�B
{B
z�B
{B
{B
{B
{B
{�B
|PB
|jB
|�B
|�B
|�B
|�B
|�B
|�B
}VB
}�B
}�B
~B
~B
~(B
~]B
~�B
~�B
HB
}B
�B
�B
�B
�B
�B
�4B
�4B
��B
�B
� B
�;B
�;B
�;B
�UB
�UB
�UB
��B
�B
�'B
�A1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20220604104935  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20220604174454  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20220604174454  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20220604174455                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20220605024502  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20220605024502  QCF$                G�O�G�O�G�O�               0JA  ARUP                                                                        20220610141505                      G�O�G�O�G�O�                