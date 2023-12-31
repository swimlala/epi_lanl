CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2016-02-27T10:16:49Z AOML 3.0 creation; 2016-08-07T21:36:44Z UW 3.1 conversion     
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20160227101649  20160807143644  5904461 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               gA   AO  5286_8897_103                   2C  D   APEX                            6531                            072314                          846 @טӅ��B1   @ט�q��@3�"��`�cG��-V1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    gA   B   B   @�ff@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt` Dy��D���D�C3D�|�D���D��D�L�D�� D�� D�	�D�  D���D�� D�� D�9�Dڐ D���D�  D�FfD�y�D��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@ÅAA!AAAaA��HA��HA��HA��HA��HA��HA��HA��HB p�Bp�Bp�Bp�B p�B(p�B0p�B8p�B@p�BHp�BPp�BXp�B`p�Bhp�Bpp�Bxp�B�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RC )C)C)C)C)C
)C)C)C)C)C)C)C)C)C)C)C )C")C$)C&)C()C*)C,)C.)C0)C2)C4)C6)C8)C:)C<)C>)C@)CB)CD)CF)CH)CJ)CL)CN)CP)CR)CT)CV)CX)CZ)C\)C^)C`)Cb)Cd)Cf)Ch)Cj)Cl)Cn)Cp)Cr)Ct)Cv)Cx)Cz)C|)C~)C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C��C��C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�GC�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�D 
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
Dq
Dq�
Dr
Dr�
Ds
Ds�
Dt
Dtg
Dy��D��D�F�D��RD��D�RD�PRD���D��D�D�#�D��D�ÅD��D�=Dړ�D��RD��D�I�D�}D�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��Aɉ7AɁA�^5A��A��AǓuAǍPAǅAǇ+AǕ�Aǣ�AǴ9A���A��`A���A�VA��;AǴ9AǸRA�ĜA�ȴAǼjAǗ�A�|�A�r�A�\)A�K�A�?}A�9XA�7LA�(�A�"�A�bA���A�  A�A���A���A��A��A��/A��#A���A� �A�"�AĲ-A�A�A�1A��A���A�{A���A��
A���A�`BA��A�?}A�hsA���A���A�&�A��wA��\A���A��A�x�A�bNA���A���A��A�7LA��A���A�1A�G�A�p�A�G�A�z�A�n�A�+A��hA�A�"�A�bNA��;A�$�A�jA��uA���A���A���A�n�A�%A}XAx��AsG�Ap5?AoG�AjM�Af��Ae��Ae`BAdv�AcA`  A]�PA[�7AWx�AS
=AQ��AM��AJ��AIK�AHr�AC�;ABA@��A<5?A;dZA:�9A8M�A6M�A2ȴA.1'A*jA(�!A(z�A'�FA'l�A&�jA$�yA$��A#�^A"�uA!�A��AO�A%A�HA9XA �A�^A��AE�A�TA��AO�A"�AĜAVA�7A^5A�A�FAO�A�!AM�A�
Ax�AĜA�
A��A+A1A1AA��A�FA+A�DAjA{A
��A�DA1'A�A��A�wA+A��AI�AG�A �jA ��A �RA ��A �9A ��@�K�@� �@��w@�@��7@�p�@���@� �@��@�ȴ@�$�@�?}@�@�-@� �@�t�@�n�@�p�@�Z@�ȴ@�ff@��#@陚@�7L@��`@��@�M�@�V@�@ᙚ@߮@�J@��/@ܣ�@�z�@�Z@�9X@�b@�v�@� �@�V@ՙ�@���@ӕ�@�+@���@� �@Χ�@Ο�@�V@�@�Ĝ@��@��T@ȴ9@��@��H@Ł@��@ě�@�Q�@�1@�K�@�@�$�@�?}@��
@�\)@�o@��\@�{@��`@�I�@�ƨ@�S�@���@�/@�Ĝ@��D@�j@�I�@��@��@�;d@���@�=q@���@��@�bN@�  @��m@��y@�?}@���@��R@�-@��@��-@��h@�hs@���@���@�9X@�b@��w@�t�@�K�@��y@�5?@��@�@��h@�`B@�?}@�V@���@��/@��m@��P@��@�v�@���@��^@��h@���@�bN@���@�|�@�K�@��R@�ff@�=q@��7@�&�@��9@�A�@��@�|�@�\)@�+@��+@�J@�@���@�`B@�%@�Ĝ@��9@��u@��@�r�@�bN@��@���@�\)@�K�@�C�@�33@�o@���@�n�@�-@���@�`B@�G�@�7L@�&�@��@���@�9X@���@�ƨ@��P@�;d@�@��H@��@��!@�ff@�E�@��@��T@��@���@�Ĝ@���@��D@�Z@�1@���@��F@�|�@�"�@���@�ȴ@�=q@���@���@��@�7L@��`@��j@���@��D@�I�@���@�+@��+@�5?@�{@�@���@��#@���@���@��-@�`B@��@���@�j@�9X@�(�@�1@��m@���@�C�@�@���@��+@�^5@�M�@�E�@�-@��@�J@��@���@�`B@���@�Ĝ@��u@�r�@�1@�S�@�"�@��@��+@�5?@�@��T@���@��@���@���@�A�@��@��
@��F@���@�|�@�C�@�"�@�+@�+@��H@���@�J@�p�@�`B@�X@�O�@�?}@�V@�Ĝ@��@�I�@�1@�P@;d@~ȴ@~V@}�-@}`B@}/@|�/@|�@|Z@{��@{S�@{"�@z��@z~�@z�\@zM�@y�#@yX@x�`@xA�@w�w@w\)@r=q@hr�@c"�@[��@Q�#@J��@AX@9X@2M�@-O�@)x�@#�
@�P@�
@�@(�@  @�D@	7L@p�@M�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   Aɉ7AɁA�^5A��A��AǓuAǍPAǅAǇ+AǕ�Aǣ�AǴ9A���A��`A���A�VA��;AǴ9AǸRA�ĜA�ȴAǼjAǗ�A�|�A�r�A�\)A�K�A�?}A�9XA�7LA�(�A�"�A�bA���A�  A�A���A���A��A��A��/A��#A���A� �A�"�AĲ-A�A�A�1A��A���A�{A���A��
A���A�`BA��A�?}A�hsA���A���A�&�A��wA��\A���A��A�x�A�bNA���A���A��A�7LA��A���A�1A�G�A�p�A�G�A�z�A�n�A�+A��hA�A�"�A�bNA��;A�$�A�jA��uA���A���A���A�n�A�%A}XAx��AsG�Ap5?AoG�AjM�Af��Ae��Ae`BAdv�AcA`  A]�PA[�7AWx�AS
=AQ��AM��AJ��AIK�AHr�AC�;ABA@��A<5?A;dZA:�9A8M�A6M�A2ȴA.1'A*jA(�!A(z�A'�FA'l�A&�jA$�yA$��A#�^A"�uA!�A��AO�A%A�HA9XA �A�^A��AE�A�TA��AO�A"�AĜAVA�7A^5A�A�FAO�A�!AM�A�
Ax�AĜA�
A��A+A1A1AA��A�FA+A�DAjA{A
��A�DA1'A�A��A�wA+A��AI�AG�A �jA ��A �RA ��A �9A ��@�K�@� �@��w@�@��7@�p�@���@� �@��@�ȴ@�$�@�?}@�@�-@� �@�t�@�n�@�p�@�Z@�ȴ@�ff@��#@陚@�7L@��`@��@�M�@�V@�@ᙚ@߮@�J@��/@ܣ�@�z�@�Z@�9X@�b@�v�@� �@�V@ՙ�@���@ӕ�@�+@���@� �@Χ�@Ο�@�V@�@�Ĝ@��@��T@ȴ9@��@��H@Ł@��@ě�@�Q�@�1@�K�@�@�$�@�?}@��
@�\)@�o@��\@�{@��`@�I�@�ƨ@�S�@���@�/@�Ĝ@��D@�j@�I�@��@��@�;d@���@�=q@���@��@�bN@�  @��m@��y@�?}@���@��R@�-@��@��-@��h@�hs@���@���@�9X@�b@��w@�t�@�K�@��y@�5?@��@�@��h@�`B@�?}@�V@���@��/@��m@��P@��@�v�@���@��^@��h@���@�bN@���@�|�@�K�@��R@�ff@�=q@��7@�&�@��9@�A�@��@�|�@�\)@�+@��+@�J@�@���@�`B@�%@�Ĝ@��9@��u@��@�r�@�bN@��@���@�\)@�K�@�C�@�33@�o@���@�n�@�-@���@�`B@�G�@�7L@�&�@��@���@�9X@���@�ƨ@��P@�;d@�@��H@��@��!@�ff@�E�@��@��T@��@���@�Ĝ@���@��D@�Z@�1@���@��F@�|�@�"�@���@�ȴ@�=q@���@���@��@�7L@��`@��j@���@��D@�I�@���@�+@��+@�5?@�{@�@���@��#@���@���@��-@�`B@��@���@�j@�9X@�(�@�1@��m@���@�C�@�@���@��+@�^5@�M�@�E�@�-@��@�J@��@���@�`B@���@�Ĝ@��u@�r�@�1@�S�@�"�@��@��+@�5?@�@��T@���@��@���@���@�A�@��@��
@��F@���@�|�@�C�@�"�@�+@�+@��H@���@�J@�p�@�`B@�X@�O�@�?}@�V@�Ĝ@��@�I�@�1@�P@;d@~ȴ@~V@}�-@}`B@}/@|�/@|�@|Z@{��@{S�@{"�@z��@z~�@z�\@zM�@y�#@yX@x�`@xA�@w�wG�O�@r=q@hr�@c"�@[��@Q�#@J��@AX@9X@2M�@-O�@)x�@#�
@�P@�
@�@(�@  @�D@	7L@p�@M�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB	ŢB	ŢB	ƨB	ȴB	��B	��B	�
B	�B	�5B	�fB	�B	�B	��B
B
VB
?}B
e`B
ffB
x�B
�oB
��B
��B
�B
�'B
�-B
�B
�B
�B
�B
�9B
�qB
�qB
�wB
�wB
ÖB
ĜB
ŢB
ŢB
ŢB
ƨB
��B
��B
�TB�BN�B�PB{�Bv�Br�Bv�B�JB�{B�JB�=BǮB�B��B�B��BB�B49B7LB.B@�B>wB33B)�B,B-B�B�BDB��B�sB��B�RB��B�=B�Bz�Bp�Be`BXB>wB�B
�B
��B
x�B
33B
�B	�B	ǮB	�-B	��B	r�B	]/B	Q�B	5?B	!�B	�B	�B	�B	JB��B�B�ZB��BƨB��B�XB�3B��B��B��B��B�hB�+B�B{�Bu�Bz�Bv�Bt�Bz�B�B�B�B�B�B�+B�%B�B� B~�B~�B~�B}�B}�B|�B{�Bz�Bz�B{�B{�B{�B{�B{�Bz�By�Bw�Bu�Bz�By�Bz�B{�B}�B~�B�B�B�B~�Bz�B{�B|�B}�B�B�B�B�%B�1B�B�By�Br�Bq�Bo�Bs�Bu�B{�B~�B{�B|�B}�B�B�%B�1B�7B�%B~�B~�By�Bx�Bx�Bx�Bx�Bv�Bs�Bs�Bq�Bp�Bq�Bp�Bp�Bp�Bp�Bq�Bs�Bt�Bt�Bu�Bu�Bt�Bs�Bu�Bu�Bw�Bx�B|�B{�B{�B}�B~�B~�B� B�B�B�B�B�7B�bB�oB�oB�hB�uB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�!B�-B�3B�9B�FB�RB�qB�}B��BBǮB��B��B��B��B��B��B��B��B�B�B�#B�/B�;B�HB�HB�`B�B�B��B	B	B	%B	%B	1B	DB	PB	\B	hB	�B	�B	�B	�B	!�B	#�B	$�B	&�B	(�B	)�B	,B	,B	,B	49B	6FB	9XB	>wB	C�B	D�B	E�B	I�B	P�B	VB	VB	XB	^5B	_;B	`BB	bNB	dZB	ffB	k�B	r�B	s�B	t�B	u�B	x�B	{�B	|�B	|�B	~�B	�B	�B	�B	�B	�B	�B	�B	�7B	�DB	�PB	�VB	�VB	�VB	�\B	�hB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�'B	�'B	�-B	�-B	�3B	�9B	�9B	�?B	�XB	�^B	�dB	�jB	�jB	�qB	�wB	�}B	�}B	��B	��B	B	B	ŢB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�
B	�B	�B	�B	�)B	�/B	�5B	�;B	�BB	�HB	�NB	�NB	�`B	�fB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
%B
%B
+B
+B
+B
1B
1B
	7B
	7B
	7B
	7B

=B
DB
DB
DB
JB
JB
JB
JB
PB
PB
PB
VB
VB
hB
{B
�B
#�B
)�B
49B
;dB
B�B
I�B
O�B
T�B
XB
\)B
aHB
e`B
iyB
l�B
p�B
t�B
w�B
|�B
�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   B	ťB	ťB	ƩB	ȷB	��B	� B	�	B	�B	�5B	�hB	�B	�B	��B
B
VB
?yB
eZB
fdB
x�B
�kB
��B
��B
�B
�"B
�(B
�B
�
B
�B
�B
�9B
�lB
�nB
�pB
�pB
ÐB
ĘB
ŜB
śB
śB
ƠB
ʻB
��B
�LB�BN�B�HB{�Bv�Br�Bv�B�@B�sB�@B�4BǦB�B��B�B��BB�B40B7@B.B@{B>nB3*B)�B+�B-B�B�B7B��B�kB��B�HB��B�4B�Bz�Bp�BeZBXB>qB�B
�B
ʻB
x�B
30B
�B	�{B	ǬB	�-B	��B	r�B	]3B	Q�B	5BB	!�B	�B	�B	�B	MB��B�B�bB��BƮB��B�aB�9B�B��B��B��B�tB�7B�B{�Bu�Bz�Bv�Bt�Bz�B�B�B�'B�B�B�4B�.B�B�	BBBB}�B}�B|�B{�Bz�Bz�B{�B{�B{�B{�B{�Bz�By�Bw�Bu�Bz�By�Bz�B{�B}�BB�B�"B�"BBz�B{�B|�B}�B�B�B�'B�,B�:B�'B�By�Br�Bq�Bo�Bs�Bu�B{�BB{�B|�B}�B�B�+B�8B�@B�-BBBy�Bx�Bx�Bx�Bx�Bv�Bs�Bs�Bq�Bp�Bq�Bp�Bp�Bp�Bp�Bq�Bs�Bt�Bt�Bu�Bu�Bt�Bs�Bu�Bu�Bw�Bx�B|�B{�B{�B}�BBB�B�B�!B�B�!B�>B�fB�uB�uB�nB�{B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�	B�B�&B�3B�4B�;B�LB�TB�uB��B��BBǯB��B��B��B��B��B��B��B��B�B�B�$B�1B�>B�KB�IB�aB�B�B��B	B	B	'B	(B	1B	DB	QB	^B	gB	B	�B	�B	�B	!�B	#�B	$�B	&�B	(�B	)�B	,
B	,B	,	B	47B	6EB	9TB	>tB	C�B	D�B	E�B	I�B	P�B	V B	VB	XB	^3B	_6B	`?B	bMB	dWB	feB	k�B	r�B	s�B	t�B	u�B	x�B	{�B	|�B	|�B	~�B	�B	�B	�B	�B	�B	�B	�B	�1B	�>B	�MB	�RB	�RB	�QB	�VB	�dB	�vB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�	B	�B	�!B	�!B	�(B	�(B	�.B	�3B	�3B	�;B	�RB	�XB	�]B	�fB	�dB	�lB	�pB	�vB	�tB	�{B	��B	B	B	ŜB	ȬB	ɲB	ʻB	ʼB	˽B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�"B	�(B	�-B	�5B	�;B	�?B	�GB	�FB	�YB	�_B	�kB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
 �B
 �B
 �B
 B
B
B
B
B
B
B
	B
B
B
B
B
B
B
B
B
B
B
#B
$B
#B
'B
*B
	.B
	.B
	/B
	-B

6B
;B
<B
<B
AB
@B
@B
BB
FB
FB
GB
MB
LG�O�B
sB
�B
#�B
)�B
4/B
;\B
B�B
I�B
O�B
T�B
XB
\B
a;B
eXB
ioB
lB
p�B
t�B
w�B
|�B
��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.11 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201608071436442016080714364420160807143644  AO  ARCAADJP                                                                    20160227101649    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20160227101649  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20160227101649  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160807143644  IP                  G�O�G�O�G�O�                