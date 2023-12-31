CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-24T14:08:06Z creation      
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
resolution        =���   axis      Z        l  9p   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ?�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     l  Ax   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  G�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     l  I�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     l  O�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  VX   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     l  W�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ^`   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     l  _�   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     l  fh   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  l�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     l  np   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  t�   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     l  vx   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  |�   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    }   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
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
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20181024140806  20181024140806  5904957 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               A   AO  6560                            2B  A   APEX                            7471                            062512                          846 @צ�A;��1   @צ���nL@4&fffff�c����o1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      A   A   A   B   B(  B0  B8  B@  BHffBP  BXffB`  Bh  Bo��Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C  C�C  C  C
  C  C  C  C  C�C  C  C  C�fC  C   C"  C$  C&  C(  C*�C,  C.  C0  C2  C4  C6  C8  C:  C<�C=�fC@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd�Cf  Ch  Cj  Cl  Cn  Cp�Cr  Ct�Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C��3C��3C��3C�  C�  C�  C�  C��3C�  C��C�  C�  C�  C�  C��3C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C��C�  C��3C��3C�  C�  C�  C�  D   D y�D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D��D	y�D
  D
� D  D� D  D� D  Dy�D  D� D  D� D  D� D  Dy�D  D� D  D� D  D� D  D�fD  D� D  D� D��Dy�D��D� D  D�fDfD� D  D� D��D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&y�D'  D'� D3� D4  D4� D5  D5� D5��D6� D7  D7� D8  D8� D9  D9� D:  D:� D:��D;� D<  D<� D<��D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DB��DC� DD  DD� DE  DE� DE��DFy�DG  DG�fDH  DHy�DI  DI� DJ  DJ� DK  DK� DK��DL� DM  DM� DN  DN� DO  DO� DPfDP� DQ  DQ� DR  DRy�DR��DS� DT  DT�fDUfDU� DV  DV� DW  DW� DX  DX� DX��DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dg��Dhy�Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� DufDu� DvfDv� DwfDw� Dw��Dyw
D�3�D�њ111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B p�B(p�B0p�B8p�B@p�BH�
BPp�BX�
B`p�Bhp�Bp
>Bxp�B�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RC)C5�C)C)C
)C)C)C)C)C5�C)C)C)C�C)C )C")C$)C&)C()C*5�C,)C.)C0)C2)C4)C6)C8)C:)C<5�C>�C@)CB)CD)CF)CH)CJ)CL)CN)CP)CR)CT)CV)CX)CZ)C\)C^)C`)Cb)Cd5�Cf)Ch)Cj)Cl)Cn)Cp5�Cr)Ct5�Cv)Cx)Cz)C|)C~)C�C�C�C�C�C�GC�GC�GC�C�C�C�C�GC�C��C�C�C�C�C�GC�C�C�C��C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C��C�C�C��C��C�C�C�C�C�C�C�C�C��C�C�C�C�C�C�C�C�C�C�C�C�C�C�C��C��C�C�C�C�C�C�C�C�C�C��C�C�GC�C�C�C�C�C�C�C�C�GC�GC�C�C��C�C�GC�GC�C�C�C�D 
D ��D
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
D	 �D	��D

D
�
D
D�
D
D�
D
D��D
D�
D
D�
D
D�
D
D��D
D�
D
D�
D
D�
D
D�pD
D�
D
D�
D �D��D �D�
D
D�pDpD�
D
D�
D �D�
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
D&��D'
D'�
D3�
D4
D4�
D5
D5�
D6 �D6�
D7
D7�
D8
D8�
D9
D9�
D:
D:�
D; �D;�
D<
D<�
D= �D=�
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
DC �DC�
DD
DD�
DE
DE�
DF �DF��DG
DG�pDH
DH��DI
DI�
DJ
DJ�
DK
DK�
DL �DL�
DM
DM�
DN
DN�
DO
DO�
DPpDP�
DQ
DQ�
DR
DR��DS �DS�
DT
DT�pDUpDU�
DV
DV�
DW
DW�
DX
DX�
DY �DY�
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
Dh �Dh��Di
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
Dt�
DupDu�
DvpDv�
DwpDw�
Dw��Dy~D�7\D��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�`BA�^5A�bNA�bNA�`BA�^5A�\)A�^5A�\)A�ZA�ZA�S�A�Q�A�O�A�A�A�C�A�E�A�C�A�?}A�=qA�9XA�5?A�5?A�
=A�\)A�l�A��A�A�~�A�A�A��A���A��+A�ĜA�M�A�K�A�O�A�^5A�A�A��9A�1A��RA�
=A��A���A�A�A�r�A��#A��A�O�A��A�
=A�bA���A��A�jA�"�A��\A�?}A��jA���A�\)A���A�33A�%A�JA���Az�DAxZAu�Ar~�Aq�-AqC�Ap��Aop�Am�;Al �Ak&�Ai��Ag��Af��Aet�Acx�AaXA`��A_dZA[7LAX~�AW�TAUC�AS7LAR�AP��APz�AOhsAN��ANE�AM��AL�`AI��AG+AE\)ADĜADVACl�A>M�A<ZA;�A8ĜA5
=A25?A2-AffA\)A�AI�A�
A
=A�A�\A9XAƨA��Av�A��A"�AM�A�AQ�AK�A��A�7A�A �Ap�A
r�A	�A	Al�A�mAS�A��A�jA�A��Av�Al�A�A�A��A��AG�A A�@�$�@��/@��j@���@���@��H@��@�1'@��@���@�"�@���@���@��@���@���@�@�/@�j@��@��@�hs@�O�@�A�@��;@@�$�@��^@�  @�ff@�`B@�A�@睲@��@�\@�{@���@���@�P@�r�@��@݉7@��/@��@۶F@�|�@�n�@ٙ�@��@�M�@�@�p�@�?}@�M�@� �@θR@�E�@ͺ^@́@̃@�l�@�n�@ɲ-@��`@ț�@�Z@�t�@Ɨ�@�E�@��@š�@Ĵ9@�+@§�@���@��@��m@�l�@�K�@�33@��@�
=@�ȴ@��h@��@�\)@��@�^5@�$�@��-@���@�ƨ@��@�\)@�33@�
=@�ȴ@�v�@��T@�?}@�  @��w@�;d@��y@���@���@��!@�~�@�-@��T@���@���@��
@��@��@�|�@���@�?}@���@���@�A�@���@�t�@�S�@�+@���@��H@���@��R@���@�v�@�v�@�ff@��-@�hs@�O�@�7L@���@��F@���@�ƨ@��w@���@��
@���@�t�@�;d@��@�v�@��@���@��#@��-@�hs@�X@�`B@�Q�@�j@���@��@���@��@�A�@��@�E�@�J@�@�@�p�@���@�bN@���@��@��@�"�@�~�@�{@��@���@��@���@�r�@�I�@��;@���@���@��@�l�@�"�@�
=@�@���@��@���@�5?@�p�@�G�@�V@�z�@�r�@�r�@�j@�Z@�Q�@�A�@�(�@��F@�K�@���@��R@��\@�v�@�V@�J@��@��@��@��#@��h@�p�@�X@�7L@��@���@��D@�j@�9X@���@��@��@��H@��@���@�^5@�=q@�-@�$�@��@�{@���@��#@���@�?}@�&�@��@��@��@���@�j@�Z@� �@�ƨ@�K�@�@���@�1'@}�9@h�$111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�`BA�^5A�bNA�bNA�`BA�^5A�\)A�^5A�\)A�ZA�ZA�S�A�Q�A�O�A�A�A�C�A�E�A�C�A�?}A�=qA�9XA�5?A�5?A�
=A�\)A�l�A��A�A�~�A�A�A��A���A��+A�ĜA�M�A�K�A�O�A�^5A�A�A��9A�1A��RA�
=A��A���A�A�A�r�A��#A��A�O�A��A�
=A�bA���A��A�jA�"�A��\A�?}A��jA���A�\)A���A�33A�%A�JA���Az�DAxZAu�Ar~�Aq�-AqC�Ap��Aop�Am�;Al �Ak&�Ai��Ag��Af��Aet�Acx�AaXA`��A_dZA[7LAX~�AW�TAUC�AS7LAR�AP��APz�AOhsAN��ANE�AM��AL�`AI��AG+AE\)ADĜADVACl�A>M�A<ZA;�A8ĜA5
=A25?A2-AffA\)A�AI�A�
A
=A�A�\A9XAƨA��Av�A��A"�AM�A�AQ�AK�A��A�7A�A �Ap�A
r�A	�A	Al�A�mAS�A��A�jA�A��Av�Al�A�A�A��A��AG�A A�@�$�@��/@��j@���@���@��H@��@�1'@��@���@�"�@���@���@��@���@���@�@�/@�j@��@��@�hs@�O�@�A�@��;@@�$�@��^@�  @�ff@�`B@�A�@睲@��@�\@�{@���@���@�P@�r�@��@݉7@��/@��@۶F@�|�@�n�@ٙ�@��@�M�@�@�p�@�?}@�M�@� �@θR@�E�@ͺ^@́@̃@�l�@�n�@ɲ-@��`@ț�@�Z@�t�@Ɨ�@�E�@��@š�@Ĵ9@�+@§�@���@��@��m@�l�@�K�@�33@��@�
=@�ȴ@��h@��@�\)@��@�^5@�$�@��-@���@�ƨ@��@�\)@�33@�
=@�ȴ@�v�@��T@�?}@�  @��w@�;d@��y@���@���@��!@�~�@�-@��T@���@���@��
@��@��@�|�@���@�?}@���@���@�A�@���@�t�@�S�@�+@���@��H@���@��R@���@�v�@�v�@�ff@��-@�hs@�O�@�7L@���@��F@���@�ƨ@��w@���@��
@���@�t�@�;d@��@�v�@��@���@��#@��-@�hs@�X@�`B@�Q�@�j@���@��@���@��@�A�@��@�E�@�J@�@�@�p�@���@�bN@���@��@��@�"�@�~�@�{@��@���@��@���@�r�@�I�@��;@���@���@��@�l�@�"�@�
=@�@���@��@���@�5?@�p�@�G�@�V@�z�@�r�@�r�@�j@�Z@�Q�@�A�@�(�@��F@�K�@���@��R@��\@�v�@�V@�J@��@��@��@��#@��h@�p�@�X@�7L@��@���@��D@�j@�9X@���@��@��@��H@��@���@�^5@�=q@�-@�$�@��@�{@���@��#@���@�?}@�&�@��@��@��@���@�j@�Z@� �@�ƨ@�K�@�@���@�1'@}�9@h�$111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
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
��BhBhB�B.B>wBQ�BS�B\)Bt�B�+B�Br�BdZB[#BS�BH�B;dBR�BVBT�BO�BM�BJ�BF�B<jB1'B�B��B��B�B�
BŢB��B�=Bt�BC�BVB
��B
�B
��B
p�B
ffB
XB
=qB
�B	�B	�;B	ĜB	�B	��B	��B	��B	��B	�DB	�%B	�PB	�hB	� B	~�B	q�B	hsB	cTB	^5B	K�B	$�B	bB	
=B��B�B�`B�B�)B�B��BȴBŢB�}B�3B��B��B��B��B��B�bB�{B��B��B�=B�DB�oB�VB�\B�oB�{B�{B��B��B��B��B��B��B��B��B��B��B��B�hB�DB�1B�B|�B{�B|�B|�B{�Bz�B|�B�B�B�1B�DB�oB��B��B�uB�VB�JB�JB�VB�hB�VB�VB�hB�hB�hB�uB�uB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�3B�?B�?B�LB�RB�^B�wB��B��B��BɺB��B�
B�B�B�B�/B�HB�TB�`B�yB�B�B�B�B��B��B��B��B	  B	B	%B		7B	JB	PB	PB	PB	PB	PB	JB	PB	hB	�B	�B	 �B	#�B	%�B	-B	1'B	33B	49B	5?B	6FB	8RB	9XB	=qB	@�B	K�B	M�B	S�B	XB	YB	YB	YB	[#B	]/B	^5B	_;B	bNB	cTB	e`B	ffB	iyB	K�B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�!B	�-B	�9B	�9B	�?B	�9B	�9B	�3B	�FB	�LB	�LB	�RB	�^B	�jB	�jB	�jB	�qB	��B	ŢB	ǮB	ȴB	ȴB	ȴB	ȴB	ɺB	ɺB	ɺB	ɺB	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�
B	�
B	�B	�B	�#B	�B	�/B	�5B	�)B	�#B	�B	�B	�#B	�/B	�5B	�;B	�;B	�;B	�BB	�HB	�HB	�HB	�NB	�TB	�TB	�TB	�TB	�TB	�TB	�ZB	�mB	�mB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
aB
�B
/O111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B
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
��BhBhB�B.B>wBQ�BS�B\)Bt�B�+B�Br�BdZB[#BS�BH�B;dBR�BVBT�BO�BM�BJ�BF�B<jB1'B�B��B��B�B�
BŢB��B�=Bt�BC�BVB
��B
�B
��B
p�B
ffB
XB
=qB
�B	�B	�;B	ĜB	�B	��B	��B	��B	��B	�DB	�%B	�PB	�hB	� B	~�B	q�B	hsB	cTB	^5B	K�B	$�B	bB	
=B��B�B�`B�B�)B�B��BȴBŢB�}B�3B��B��B��B��B��B�bB�{B��B��B�=B�DB�oB�VB�\B�oB�{B�{B��B��B��B��B��B��B��B��B��B��B��B�hB�DB�1B�B|�B{�B|�B|�B{�Bz�B|�B�B�B�1B�DB�oB��B��B�uB�VB�JB�JB�VB�hB�VB�VB�hB�hB�hB�uB�uB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�3B�?B�?B�LB�RB�^B�wB��B��B��BɺB��B�
B�B�B�B�/B�HB�TB�`B�yB�B�B�B�B��B��B��B��B	  B	B	%B		7B	JB	PB	PB	PB	PB	PB	JB	PB	hB	�B	�B	 �B	#�B	%�B	-B	1'B	33B	49B	5?B	6FB	8RB	9XB	=qB	@�B	K�B	M�B	S�B	XB	YB	YB	YB	[#B	]/B	^5B	_;B	bNB	cTB	e`B	ffB	iyB	K�B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�!B	�-B	�9B	�9B	�?B	�9B	�9B	�3B	�FB	�LB	�LB	�RB	�^B	�jB	�jB	�jB	�qB	��B	ŢB	ǮB	ȴB	ȴB	ȴB	ȴB	ɺB	ɺB	ɺB	ɺB	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�
B	�
B	�B	�B	�#B	�B	�/B	�5B	�)B	�#B	�B	�B	�#B	�/B	�5B	�;B	�;B	�;B	�BB	�HB	�HB	�HB	�NB	�TB	�TB	�TB	�TB	�TB	�TB	�ZB	�mB	�mB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
aB
�B
/O111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.11 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181024140806                              AO  ARCAADJP                                                                    20181024140806    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181024140806  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181024140806  QCF$                G�O�G�O�G�O�0               