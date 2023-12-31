CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2014-07-21T23:08:41Z AOML 3.0 creation; 2016-06-01T00:08:17Z UW 3.1 conversion     
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20140721230841  20160531170817  5903740 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               FA   AO  4055_7112_070                   2C  D   APEX                            5374                            041511                          846 @��(/ 1   @��(Ϳ�@;0��
=q�dƧ1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    FA   A   A   @�  @�  A   A   A@  A^ffA�  A���A�  A�  A�  A���A�  A�  B   B  B  B  B   B(  B0  B8ffB@  BH  BP  BXffB`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D�fD  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dty�Dys3D� D�L�D�s3D�� D��D�<�D�y�D�� D�fD�C3D���D��fD�fD�@ Dڌ�D��3D� D�6fD�ffD���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��H@��HAp�A!p�AAp�A_�
A��RA��A��RA��RA��RAхA�RA�RB \)B\)B\)B\)B \)B(\)B0\)B8B@\)BH\)BP\)BXB`\)Bh\)Bp\)Bx\)B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B���B�.B�.B�.B�.B�.B�.B�.B�.C 
C
C
C
C
C

C
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
C>
C@
CB
CD
CF
CH
CJ
CL
CN
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
Ct
Cv
Cx
Cz
C|
C~
C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C���C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��D �D ��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D	�D	��D
�D
��D�D��D�D��D�D��D�D��D�D��D�D��D�D�)D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D �D ��D!�D!��D"�D"��D#�D#��D$�D$��D%�D%��D&�D&��D'�D'��D(�D(��D)�D)��D*�D*��D+�D+��D,�D,��D-�D-��D.�D.��D/�D/��D0�D0��D1�D1��D2�D2��D3�D3��D4�D4��D5�D5��D6�D6��D7�D7��D8�D8��D9�D9��D:�D:��D;�D;��D<�D<��D=�D=��D>�D>��D?�D?��D@�D@��DA�DA��DB�DB��DC�DC��DD�DD��DE�DE��DF�DF��DG�DG��DH�DH��DI�DI��DJ�DJ��DK�DK��DL�DL��DM�DM��DN�DN��DO�DO��DP�DP��DQ�DQ��DR�DR��DS�DS��DT�DT��DU�DU��DV�DV��DW�DW��DX�DX��DY�DY��DZ�DZ��D[�D[��D\�D\��D]�D]��D^�D^��D_�D_��D`�D`��Da�Da��Db�Db��Dc�Dc��Dd�Dd��De�De��Df�Df��Dg�Dg��Dh�Dh��Di�Di��Dj�Dj��Dk�Dk��Dl�Dl��Dm�Dm��Dn�Dn��Do�Do��Dp�Dp��Dq�Dq��Dr�Dr��Ds�Ds��Dt�Dt]Dyx�D��D�O�D�vD���D��D�?�D�|{D���D�GD�FD��{D��GD�	GD�B�Dڏ�D��D��D�9GD�iGD���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�?}A�E�A�E�A�E�A�E�A�C�A�G�A�G�A�G�A�G�A�I�A�I�A�O�A�ZA�\)A�dZA�ffA�dZA�hsA�hsA�n�A�z�A�~�A�~�A�|�A��A��+A��+A��+A��DA��DA��7A��PA��PA��DA��DA��7A��DA��7A��7A��PA��7A��DA��\A��PA��PA��PA��hA��\A��+A��A��A��7A��uA��hA��uA��uA��\A��\A���A���A���A���A���A��hA��A��A�z�A�x�A�|�A��A�t�A�-A��A���A��7A���A��A���A�  A�l�A�K�A���A�7LA�dZA���A���A���A���A�9XA��A�
=A��A�Q�A�Q�A�\)A���A��A�1'A�G�A~  A{�
A{XAz�jAx��Ax�Awl�Av�+AtE�Ap(�An=qAl�jAk�
Ai��Ah�yAh�Ag�hAf��AeK�Ac�Aa�
A`ĜA`jA`�A`  A_�PA_+A^�+A]
=AZJAW%AUC�ASƨAR�9ARn�ARQ�ARA�AQ�
AQ7LAQoAQAP�jAPz�APVAN��AMXAL �AI�PAHbNAH  AGt�AFĜAF�uAFv�AF�AE�mAEx�AD��AD��AD��AD(�AC��AB�jAA+A?��A?&�A>z�A>A�A>A=��A<�yA;XA:��A:bNA9�hA8��A8=qA8A7�mA7G�A6�+A6  A5��A5�-A5��A5�A5p�A5XA533A5A4~�A3"�A1�A.z�A,��A+��A+��A*��A)��A)|�A)"�A(�yA(�!A(n�A(  A%�A#C�A"z�A!��A VAXA��AbNA�A�hA�/A�#AO�A{A�A�A��AE�A�;Al�A~�Al�AM�A�A�9A`BA�A9XA�;Al�A�A��AbNA��AK�AVA	p�A��A�!AffAƨAS�A�`A��Az�A��AC�A��AZA��A`BA ��@�5?@�hs@���@��h@��;@�E�@�D@���@�F@��@�n�@�E�@���@���@�x�@��@�9X@�t�@���@��@�!@�n�@�@���@���@�u@�"�@⟾@�^5@�hs@�@�t�@�~�@�`B@��y@أ�@Դ9@ϝ�@�/@���@�C�@�E�@��/@�1'@ǥ�@�5?@��#@�l�@��T@�Ĝ@�S�@�^5@��j@���@��\@��T@���@�Q�@��m@�S�@�33@�@��R@�V@�@���@���@���@���@�x�@�O�@��@��H@�9X@��!@���@�{@��@�r�@�A�@�b@��@�C�@�;d@�@���@��!@�~�@�V@��@�@�@��@���@�x�@�G�@��@���@��/@���@���@���@���@���@��D@�z�@�r�@�Z@�1'@�(�@�  @��w@�o@�o@�@��y@���@�n�@�5?@�@��T@���@��@� �@��
@��P@��H@��+@�V@�$�@��@���@��h@�hs@�G�@��/@�r�@�Q�@��m@��@�v�@���@��@� �@��F@���@��P@�\)@�"�@���@�5?@��^@��7@�x�@�p�@�hs@�G�@�%@�j@�b@�o@��@���@��+@�^5@���@��-@�O�@�/@�&�@�V@��@�Q�@�|�@�+@��@�@��-@���@�x�@�/@�Ĝ@��u@�z�@�j@�1'@�  @�o@�~�@�V@�@���@�V@�r�@�I�@�1@�+@��@���@�-@���@��h@�p�@�X@�O�@�&�@�V@�%@��@���@��9@��D@�Z@�1'@� �@�b@�@��@�@~ȴ@~V@~5?@}�@}?}@|�@|1@{"�@z�\@z-@y�@y7L@s��@j=q@ct�@\9X@VV@O�@K�@E��@@1'@8�@1G�@+"�@&�R@!�#@�7@5?@x�@�h@
�H@bN@I�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�?}A�E�A�E�A�E�A�E�A�C�A�G�A�G�A�G�A�G�A�I�A�I�A�O�A�ZA�\)A�dZA�ffA�dZA�hsA�hsA�n�A�z�A�~�A�~�A�|�A��A��+A��+A��+A��DA��DA��7A��PA��PA��DA��DA��7A��DA��7A��7A��PA��7A��DA��\A��PA��PA��PA��hA��\A��+A��A��A��7A��uA��hA��uA��uA��\A��\A���A���A���A���A���A��hA��A��A�z�A�x�A�|�A��A�t�A�-A��A���A��7A���A��A���A�  A�l�A�K�A���A�7LA�dZA���A���A���A���A�9XA��A�
=A��A�Q�A�Q�A�\)A���A��A�1'A�G�A~  A{�
A{XAz�jAx��Ax�Awl�Av�+AtE�Ap(�An=qAl�jAk�
Ai��Ah�yAh�Ag�hAf��AeK�Ac�Aa�
A`ĜA`jA`�A`  A_�PA_+A^�+A]
=AZJAW%AUC�ASƨAR�9ARn�ARQ�ARA�AQ�
AQ7LAQoAQAP�jAPz�APVAN��AMXAL �AI�PAHbNAH  AGt�AFĜAF�uAFv�AF�AE�mAEx�AD��AD��AD��AD(�AC��AB�jAA+A?��A?&�A>z�A>A�A>A=��A<�yA;XA:��A:bNA9�hA8��A8=qA8A7�mA7G�A6�+A6  A5��A5�-A5��A5�A5p�A5XA533A5A4~�A3"�A1�A.z�A,��A+��A+��A*��A)��A)|�A)"�A(�yA(�!A(n�A(  A%�A#C�A"z�A!��A VAXA��AbNA�A�hA�/A�#AO�A{A�A�A��AE�A�;Al�A~�Al�AM�A�A�9A`BA�A9XA�;Al�A�A��AbNA��AK�AVA	p�A��A�!AffAƨAS�A�`A��Az�A��AC�A��AZA��A`BA ��@�5?@�hs@���@��h@��;@�E�@�D@���@�F@��@�n�@�E�@���@���@�x�@��@�9X@�t�@���@��@�!@�n�@�@���@���@�u@�"�@⟾@�^5@�hs@�@�t�@�~�@�`B@��y@أ�@Դ9@ϝ�@�/@���@�C�@�E�@��/@�1'@ǥ�@�5?@��#@�l�@��T@�Ĝ@�S�@�^5@��j@���@��\@��T@���@�Q�@��m@�S�@�33@�@��R@�V@�@���@���@���@���@�x�@�O�@��@��H@�9X@��!@���@�{@��@�r�@�A�@�b@��@�C�@�;d@�@���@��!@�~�@�V@��@�@�@��@���@�x�@�G�@��@���@��/@���@���@���@���@���@��D@�z�@�r�@�Z@�1'@�(�@�  @��w@�o@�o@�@��y@���@�n�@�5?@�@��T@���@��@� �@��
@��P@��H@��+@�V@�$�@��@���@��h@�hs@�G�@��/@�r�@�Q�@��m@��@�v�@���@��@� �@��F@���@��P@�\)@�"�@���@�5?@��^@��7@�x�@�p�@�hs@�G�@�%@�j@�b@�o@��@���@��+@�^5@���@��-@�O�@�/@�&�@�V@��@�Q�@�|�@�+@��@�@��-@���@�x�@�/@�Ĝ@��u@�z�@�j@�1'@�  @�o@�~�@�V@�@���@�V@�r�@�I�@�1@�+@��@���@�-@���@��h@�p�@�X@�O�@�&�@�V@�%@��@���@��9@��D@�Z@�1'@� �@�b@�@��@�@~ȴ@~V@~5?@}�@}?}@|�@|1@{"�@z�\@z-@y�@y7L@s��@j=q@ct�@\9X@VV@O�@K�@E��@@1'@8�@1G�@+"�@&�R@!�#@�7@5?@x�@�h@
�H@bN@I�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oBZB\)B\)B\)BZBYB[#BZBXBW
BXBZB_;Be`BgmBl�Bm�Bk�Bn�Bn�Bp�Bx�Bz�Bz�Bz�B{�B|�B|�B|�B|�B|�B|�B|�B|�B|�B|�B|�B|�B{�B|�B|�B{�B{�B|�B|�B{�B{�B{�B{�Bz�Bz�Bz�Bz�B{�B{�B{�B{�B{�Bz�B{�B{�B{�B{�B{�Bz�Bx�Bx�Bw�Bw�Bw�Bw�Bt�BhsB:^Bn�B��B�HB��BŢB�3B��B�%B_;B%�B{B
��B
�)B
ȴB
�B
��B
��B
��B
��B
�oB
z�B
m�B
dZB
ZB
N�B
A�B
%�B
�B
{B
\B
B	��B	��B	�B	�BB	ɺB	�wB	�?B	�B	��B	��B	��B	��B	��B	�\B	�1B	� B	|�B	z�B	y�B	x�B	v�B	s�B	o�B	gmB	]/B	R�B	L�B	G�B	D�B	D�B	D�B	D�B	C�B	?}B	>wB	=qB	<jB	:^B	8RB	33B	-B	&�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	{B	uB	oB	bB	VB	DB	1B	B��B��B��B��B�B�B�TB�BB�;B�#B�
B�B��B��B��B��BǮBƨBŢBĜBÖBÖBB��B�}B�dB�FB�B��B��B��B��B��B��B��B��B��B��B��B��B�VB�7B�%B�B� B}�B{�Bz�Bx�Bv�Bt�Br�Bp�Bn�Bm�Bk�BiyBffBe`BbNB`BB\)BW
BQ�BK�BI�BH�BF�BE�BD�BC�BB�B@�B?}B=qB;dB9XB8RB7LB6FB5?B49B33B33B1'B0!B/B.B,B'�B%�B"�B"�B!�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�BuB�B�B�B�B�B�B�B�B�B#�B(�B+B-B/B0!B33B5?B7LB8RB;dB<jB=qB?}B?}B?}B@�BA�BB�BB�BB�BC�BB�BB�BB�BA�BF�BN�BR�B_;Be`BiyBl�Bm�Bm�Bp�Bq�Bq�Br�Bs�Bs�Bt�Bu�Bv�Bw�Bw�Bw�Bx�By�Bz�B{�B{�B|�B|�B}�B}�B}�B}�B~�B~�B~�B� B�B�B�B�B�%B�%B�%B�+B�7B�7B�DB�JB�JB�PB�bB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�9B�^B�qBĜBǮBǮBǮBɺB��B��B��B��B�B�
B�
B�
B�B�B�5B�BB�sB�yB�B�B�B�B�B��B��B��B��B��B��B	B	B	VB	oB	oB	oB	{B	�B	�B	�B	�B	�B	�B	�B	'�B	-B	.B	0!B	33B	9XB	>wB	?}B	A�B	I�B	K�B	N�B	Q�B	T�B	VB	XB	YB	YB	[#B	]/B	]/B	^5B	_;B	`BB	aHB	cTB	dZB	e`B	ffB	ffB	hsB	k�B	m�B	n�B	o�B	p�B	q�B	s�B	u�B	x�B	z�B	{�B	|�B	� B	�{B	�}B	��B	�mB	��B
B
JB
{B
�B
,B
7LB
?}B
D�B
J�B
T�B
YB
aHB
ffB
jB
m�B
s�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   BZB\"B\"B\"BZBYB[BZBXBWBXBZB_7Be]BggBl�Bm�BkBn�Bn�Bp�Bx�Bz�Bz�Bz�B{�B|�B|�B|�B|�B|�B|�B|�B|�B|�B|�B|�B|�B{�B|�B|�B{�B{�B|�B|�B{�B{�B{�B{�Bz�Bz�Bz�Bz�B{�B{�B{�B{�B{�Bz�B{�B{�B{�B{�B{�Bz�Bx�Bx�Bw�Bw�Bw�Bw�Bt�BhpB:YBn�B��B�?B��BřB�(B��B�B_3B%�BrB
��B
�%B
ȰB
�B
��B
��B
��B
��B
�jB
z�B
m�B
dXB
ZB
N�B
A�B
%�B
�B
{B
ZB
B	��B	��B	�B	�BB	ɺB	�zB	�>B	�B	��B	��B	��B	��B	��B	�`B	�3B	�B	|�B	z�B	y�B	x�B	v�B	s�B	o�B	gqB	]4B	R�B	L�B	G�B	D�B	D�B	D�B	D�B	C�B	?�B	>{B	=uB	<nB	:dB	8WB	38B	-B	&�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	}B	tB	jB	\B	IB	9B	B� B��B��B��B�B�B�^B�JB�BB�.B�B�B�B��B��B��BǷBƱBūBĨBàBáBB��B��B�lB�OB�&B�B��B��B��B��B��B��B��B��B��B��B��B�aB�AB�0B�B�B}�B{�Bz�Bx�Bv�Bt�Br�Bp�Bn�Bm�Bk�Bi�BfrBenBb[B`OB\6BWBQ�BK�BI�BH�BF�BE�BD�BC�BB�B@�B?�B=gB;rB9eB8`B7YB6RB5NB4HB3AB3@B17B00B/B.#B,B'�B%�B"�B"�B!�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�BzB|B�B�B�B�B�B�B�B~B�B�B�B�B�B�B�B�B�B�B�B�B�B�B#�B)B+B-B/)B00B3?B5MB7WB8^B;pB<vB=B?�B?�B?�B@�BA�BB�BB�BB�BC�BB�BB�BB�BA�BF�BN�BR�B_FBekBi�Bl�Bm�Bm�Bp�Bq�Bq�Br�Bs�Bs�Bt�Bu�Bv�Bw�Bw�Bw�Bx�By�Bz�B{�B{�B|�B|�B}�B}�B}�B}�BBBB�	B�B�B�B�B�,B�,B�.B�4B�?B�@B�KB�RB�RB�YB�iB��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�?B�eB�wBĢBǲBǳBǲBɾB��B��B��B�B�B�B�B�B�B�B�;B�FB�uB�}B�B�B�B�B�B��B��B��B��B��B��B	B	B	XB	rB	qB	qB	~B	�B	�B	�B	�B	�B	�B	�B	'�B	-B	.B	0 B	34B	9WB	>vB	?~B	A�B	I�B	K�B	N�B	Q�B	T�B	VB	XB	YB	YB	[#B	]-B	]-B	^6B	_<B	`DB	aFB	cRB	dZB	eaB	ffB	fcB	hsB	k�B	m�B	n�B	o�B	p�B	q�B	s�B	u�B	x�B	z�B	{�B	|�B	�B	�{B	�xB	��B	�hB	��B
B
EB
vB
�B
+�B
7CB
?xB
D�B
J�B
T�B
YB
aAB
f^B
jwB
m�B
s�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.09 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201605311708172016053117081720160531170817  AO  ARCAADJP                                                                    20140721230841    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20140721230841  QCP$                G�O�G�O�G�O�DFB7E           AO  ARGQQCPL                                                                    20140721230841  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160531170817  IP                  G�O�G�O�G�O�                