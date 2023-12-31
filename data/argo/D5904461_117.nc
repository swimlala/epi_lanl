CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2016-05-11T09:17:12Z AOML 3.0 creation; 2016-08-07T21:36:46Z UW 3.1 conversion     
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20160511091712  20160825183352  5904461 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               uA   AO  5286_8897_117                   2C  D   APEX                            6531                            072314                          846 @׫IA�1   @׫I�I�S@3���`A��cIx���1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    uA   B   B   @�33@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�33A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"�C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DP��DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dty�Dy��D���D�<�D�y�D��fD�	�D�I�D��fD��fD�3D�FfD��3D�� D�3D�<�Dڐ D��3D��D�,�D�p D�<�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @���@�p�A�RA"�RAB�RAb�RA�\)A�\)A�\)A�\)A�\)AЏ\A�\)A�\)B �B�B�B�B �B(�B0�B8�B@�BH�BP�BX�B`�Bh�Bp�Bx�B�W
B�W
B�W
B�W
B�W
B�W
B�W
B�W
B�W
B�W
B�W
B�W
B�W
B�W
B�W
B�W
B�W
B�W
B�W
B�W
B�W
B�W
B�W
B�W
B�W
B�W
B�W
B�W
B�W
B�W
B�W
B�W
C +�C+�C+�C+�C+�C
+�C+�C+�C+�C+�C+�C+�C+�C+�C+�C+�C +�C"EC$+�C&+�C(+�C*+�C,+�C.+�C0+�C2+�C4+�C6+�C8+�C:+�C<+�C>+�C@+�CB+�CD+�CF+�CH+�CJ+�CL+�CN+�CP+�CR+�CT+�CV+�CX+�CZ+�C\+�C^+�C`+�Cb+�Cd+�Cf+�Ch+�Cj+�Cl+�Cn+�Cp+�Cr+�Ct+�Cv+�Cx+�Cz+�C|+�C~+�C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��D 
�D ��D
�D��D
�D��D
�D��D
�D��D
�D��D
�D��D
�D��D
�D��D	
�D	��D

�D
��D
�D��D
�D��D
�D��D
�D��D
�D��D
�D��D
�D��D
�D��D
�D��D
�D��D
�D��D
�D��D
�D��D
�D��D
�D��D
�D��D
�D��D
�D��D
�D��D
�D��D
�D��D 
�D ��D!
�D!��D"
�D"��D#
�D#��D$
�D$��D%
�D%��D&
�D&��D'
�D'��D(
�D(��D)
�D)��D*
�D*��D+
�D+��D,
�D,��D-
�D-��D.
�D.��D/
�D/��D0
�D0��D1
�D1��D2
�D2��D3
�D3��D4
�D4��D5
�D5��D6
�D6��D7
�D7��D8
�D8��D9
�D9��D:
�D:��D;
�D;��D<
�D<��D=
�D=��D>
�D>��D?
�D?��D@
�D@��DA
�DA��DB
�DB��DC
�DC��DD
�DD��DE
�DE��DF
�DF��DG
�DG��DH
�DH��DI
�DI��DJ
�DJ��DK
�DK��DL
�DL��DM
�DM��DN
�DN��DO
�DO��DP
�DP��DQ{DQ��DR
�DR��DS
�DS��DT
�DT��DU
�DU��DV
�DV��DW
�DW��DX
�DX��DY
�DY��DZ
�DZ��D[
�D[��D\
�D\��D]
�D]��D^
�D^��D_
�D_��D`
�D`��Da
�Da��Db
�Db��Dc
�Dc��Dd
�Dd��De
�De��Df
�Df��Dg
�Dg��Dh
�Dh��Di
�Di��Dj
�Dj��Dk
�Dk��Dl
�Dl��Dm
�Dm��Dn
�Dn��Do
�Do��Dp
�Dp��Dq
�Dq��Dr
�Dr��Ds
�Ds��Dt
�Dt�{Dy�{D�>D�B>D�D���D�D�OD���D���D��D�K�D���D��qD��D�B>DڕqD�ȤD�>D�2>D�uqD�B>11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��AϓuA�ffA�9XA�=qA�A�AͼjA͇+A�\)A�7LA���A�x�A�C�A��A˴9A�;dA�-A�K�A�ZA�I�A�-A���A��HAʟ�A�+A��TAɑhA���A�I�A�K�A�JA��Aɛ�AɃA�S�A�?}A�9XA�VA�|�AǇ+A�p�A�ffA��TAğ�A�?}A��
A��A�/A��RA�x�A�-A�jA���A�  A�(�A��uA���A�;dA�&�A�A�r�A��A�hsA�VA�oA��7A��A�\)A���A��-A��A��A���A��wA�^5A��uA�^5A�{A��jA�JA��A��7A���A�hsA�XA��A�ffA���A���A��DA��mA��TA�|�A�XA�JA�v�A�n�A���A��A���A��mA��A�ffA�t�A���A~{A{33At�At1Ar9XAo33Al��AhA�Ab��Aa"�A_&�A];dA[�7AZbNAZ(�AYVAU�;AT�9ASl�AM�#AK�hAJ9XAIC�AG�AE�mAE�AB�AB1'AA��A@��A?VA=dZA;+A:bA9%A4��A3A2I�A1
=A/��A.�uA-S�A+dZA*bNA)�#A);dA(��A(9XA'�A'dZA&{A%;dA$ZA#��A#�A"9XA!�Ap�AI�A  A�AC�AĜAjA�A�A�A�+A�AXAbNA�FAA{A�AhsAO�A&�A�jAQ�A�AA�
AK�A�AK�A�A��A��Ap�A
��A
1'A	��A5?A��A$�A�A �A ��A J@�V@�|�@��@�G�@�(�@���@��P@�|�@�\)@��^@�ƨ@�C�@��@��@���@�?}@� �@띲@�!@�M�@�h@�7@��`@�D@�1@�P@��H@�5?@�@��@㕁@�=q@ߕ�@�/@�"�@ى7@�G�@���@�b@ָR@�{@պ^@�/@ԣ�@��@���@�^5@�$�@�J@�-@҇+@љ�@д9@�;d@�ff@͡�@��/@�Z@��
@�C�@�
=@���@��/@ȋD@�z�@�I�@��@���@��;@Ǿw@Ǯ@�l�@�?}@ÍP@�V@��T@��^@�p�@��@��@�(�@�K�@��!@�J@��9@�bN@�b@��
@��P@��@��\@��@�hs@�%@���@�z�@� �@���@��+@��7@�J@�ff@��R@�ȴ@��+@�/@��m@�n�@�X@�V@�%@��/@��u@��@��m@�  @���@��F@��m@���@��@�+@��H@���@��#@��j@��D@�r�@� �@�l�@�33@��@��@�ff@�-@��#@��-@��^@�@���@�p�@�`B@�V@���@��j@��@�%@���@��9@��@�j@�Q�@�A�@��@��@��
@��F@��w@�ƨ@��w@���@�C�@��@�ȴ@�{@�V@�z�@�j@��@�z�@�  @��
@�ƨ@��F@���@�33@���@��\@���@��!@�-@���@��/@�r�@��;@��@���@�ƨ@��w@��
@�  @���@��@���@�z�@�z�@�r�@�I�@�1@��@�ƨ@��w@��F@�t�@�S�@�;d@��@��R@��+@�^5@���@���@�Z@��@�
=@���@��@��@���@�Q�@�dZ@�^5@�E�@�5?@�5?@�-@�-@�-@�$�@�$�@�$�@�J@��@��#@�@��@��@�I�@� �@�1@���@�|�@�+@�^5@��-@�&�@�/@�X@���@���@�(�@�33@��H@���@�=q@�J@���@��-@���@���@�x�@�V@��@�Z@�I�@�1'@���@�+@���@���@�=q@��@��@��T@��#@���@���@��7@�&�@��j@��@���@��D@�j@���@�r�@u��@mO�@i&�@^ȴ@S�
@P  @JJ@A�7@;ƨ@4�/@.ȴ@( �@"-@@��@�
@�`@��@��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   AϓuA�ffA�9XA�=qA�A�AͼjA͇+A�\)A�7LA���A�x�A�C�A��A˴9A�;dA�-A�K�A�ZA�I�A�-A���A��HAʟ�A�+A��TAɑhA���A�I�A�K�A�JA��Aɛ�AɃA�S�A�?}A�9XA�VA�|�AǇ+A�p�A�ffA��TAğ�A�?}A��
A��A�/A��RA�x�A�-A�jA���A�  A�(�A��uA���A�;dA�&�A�A�r�A��A�hsA�VA�oA��7A��A�\)A���A��-A��A��A���A��wA�^5A��uA�^5A�{A��jA�JA��A��7A���A�hsA�XA��A�ffA���A���A��DA��mA��TA�|�A�XA�JA�v�A�n�A���A��A���A��mA��A�ffA�t�A���A~{A{33At�At1Ar9XAo33Al��AhA�Ab��Aa"�A_&�A];dA[�7AZbNAZ(�AYVAU�;AT�9ASl�AM�#AK�hAJ9XAIC�AG�AE�mAE�AB�AB1'AA��A@��A?VA=dZA;+A:bA9%A4��A3A2I�A1
=A/��A.�uA-S�A+dZA*bNA)�#A);dA(��A(9XA'�A'dZA&{A%;dA$ZA#��A#�A"9XA!�Ap�AI�A  A�AC�AĜAjA�A�A�A�+A�AXAbNA�FAA{A�AhsAO�A&�A�jAQ�A�AA�
AK�A�AK�A�A��A��Ap�A
��A
1'A	��A5?A��A$�A�A �A ��A J@�V@�|�@��@�G�@�(�@���@��P@�|�@�\)@��^@�ƨ@�C�@��@��@���@�?}@� �@띲@�!@�M�@�h@�7@��`@�D@�1@�P@��H@�5?@�@��@㕁@�=q@ߕ�@�/@�"�@ى7@�G�@���@�b@ָR@�{@պ^@�/@ԣ�@��@���@�^5@�$�@�J@�-@҇+@љ�@д9@�;d@�ff@͡�@��/@�Z@��
@�C�@�
=@���@��/@ȋD@�z�@�I�@��@���@��;@Ǿw@Ǯ@�l�@�?}@ÍP@�V@��T@��^@�p�@��@��@�(�@�K�@��!@�J@��9@�bN@�b@��
@��P@��@��\@��@�hs@�%@���@�z�@� �@���@��+@��7@�J@�ff@��R@�ȴ@��+@�/@��m@�n�@�X@�V@�%@��/@��u@��@��m@�  @���@��F@��m@���@��@�+@��H@���@��#@��j@��D@�r�@� �@�l�@�33@��@��@�ff@�-@��#@��-@��^@�@���@�p�@�`B@�V@���@��j@��@�%@���@��9@��@�j@�Q�@�A�@��@��@��
@��F@��w@�ƨ@��w@���@�C�@��@�ȴ@�{@�V@�z�@�j@��@�z�@�  @��
@�ƨ@��F@���@�33@���@��\@���@��!@�-@���@��/@�r�@��;@��@���@�ƨ@��w@��
@�  @���@��@���@�z�@�z�@�r�@�I�@�1@��@�ƨ@��w@��F@�t�@�S�@�;d@��@��R@��+@�^5@���@���@�Z@��@�
=@���@��@��@���@�Q�@�dZ@�^5@�E�@�5?@�5?@�-@�-@�-@�$�@�$�@�$�@�J@��@��#@�@��@��@�I�@� �@�1@���@�|�@�+@�^5@��-@�&�@�/@�X@���@���@�(�@�33@��H@���@�=q@�J@���@��-@���@���@�x�@�V@��@�Z@�I�@�1'@���@�+@���@���@�=q@��@��@��T@��#@���@���@��7@�&�@��j@��@���@��DG�O�@���@�r�@u��@mO�@i&�@^ȴ@S�
@P  @JJ@A�7@;ƨ@4�/@.ȴ@( �@"-@@��@�
@�`@��@��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB	�B	�wB	�}B	��B	�;B
%B
bB
�B
!�B
33B
Q�B
}�B
��B
��B
�TB
�fB
�B
��BB+B+B%BB
�B
�fB
�/B
�BuB�B�B�B�B�B�B�B�B�B$�B7LBO�Bz�B�\B��B��B��B�dB�BB��BBhB;dBbNB�=B��B�!B�jBƨBƨB��B��B��B�
BɺB�9B��B��B��B��B��B��B��B��B��B��B��B�{B�bB�=By�Be`BS�BA�B0!B!�BbB  B��B�B�sB�/B��B�XB��B~�BR�BB�B%�BhB
�B
��B
�B
s�B
C�B
�B
B	�yB	�B	�B	��B	}�B	dZB	D�B	)�B	 �B	�B	JB	B	  B��B��B�sB�TB�)B��BB�wB�^B�?B�-B�B�B�B��B��B��B��B��B��B��B��B��B��B��B�hB�VB�=B�1B�1B�1B�7B�7B�=B�\B�hB�JB�DB�JB�JB�JB�uB�{B��B��B��B��B��B��B��B��B��B��B�B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�{B�hB�oB�uB�{B�{B�{B��B��B��B��B��B��B��B��B��B��B��B�{B�uB�{B�{B��B��B��B��B�B�'B�FB�RB�LB�FB�LB�RB�XB�RB�LB�?B�9B�RB�wB��BÖBÖBÖBÖBÖBĜBĜBĜBŢBƨBȴBɺB��B��B��B��B�)B�/B�;B�BB�HB�TB�ZB�`B�mB�mB�B�B�B�B�B�B�B�B�B�B�B��B��B��B	B	B	B	B	B	%B	+B	1B		7B	\B	bB	hB	uB	{B	�B	�B	�B	�B	�B	�B	�B	�B	 �B	!�B	'�B	1'B	8RB	9XB	<jB	<jB	<jB	9XB	7LB	5?B	5?B	6FB	7LB	9XB	;dB	;dB	=qB	>wB	>wB	@�B	A�B	C�B	C�B	C�B	E�B	G�B	I�B	J�B	K�B	L�B	O�B	R�B	S�B	R�B	R�B	R�B	T�B	VB	W
B	XB	W
B	XB	XB	YB	ZB	[#B	^5B	aHB	bNB	dZB	ffB	l�B	m�B	n�B	o�B	p�B	v�B	y�B	z�B	}�B	}�B	}�B	�B	�B	�B	�1B	�%B	�+B	�1B	�DB	�PB	�\B	�bB	�hB	�oB	�oB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�'B	�?B	�dB	�}B	��B	ŢB	ƨB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	ɺB	ȴB	ǮB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�B	�B	�B	�B	�#B	�/B	�BB	�TB	�mB	�B	�B	�B	�B	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
1B
oB
�B
!�B
)�B
33B
6FB
<jB
C�B
I�B
P�B
VB
[#B
aHB
e`B
hsB
m�B
q�B
u�B
y�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   B	�B	�uB	�{B	ʽB	�9B
!B
\B
�B
!�B
3/B
Q�B
}�B
��B
�B
�HB
�YB
�B
��B �BB BBB
�B
�[B
�#B
�BjByBuB�B�B|BzB�B�B�B$�B7=BO�Bz�B�NB��B��B��B�VB�3B��BBVB;WBb?B�.B��B�B�`BƛBƝBʳB��B��B��BɬB�-B��B��B��B��B��B��B��B��B��B��B��B�jB�SB�-By�BeQBS�BA}B0B!�BRB��B��B�B�cB�B̿B�IB��B~�BR�BB�B%�BXB
�B
�~B
��B
s�B
C�B
�B
 �B	�rB	�B	�B	��B	}�B	dWB	D�B	)�B	 �B	�B	HB	B	 B��B��B�sB�UB�*B��BB�xB�`B�AB�/B�B�B�B��B��B��B��B��B��B��B��B��B��B��B�jB�XB�?B�5B�4B�4B�;B�:B�@B�`B�jB�LB�EB�LB�KB�KB�vB�|B��B��B��B��B��B��B��B��B��B��B�B�B� B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�zB�jB�nB�sB�}B�{B�yB��B��B��B��B��B��B��B��B�B��B��B�zB�tB�yB�yB��B��B��B��B�B�$B�CB�QB�IB�DB�KB�MB�SB�OB�IB�=B�6B�LB�uB��BÕBÑBÒBÓBÒBęBĘBěBŜBƢBȰBɶB��B��B��B��B�#B�+B�7B�>B�CB�OB�VB�[B�hB�gB�B�B�B�B�B�B�B�B�B�B�B��B��B��B	B	B	B	B	B	B	#B	)B		1B	VB	]B	_B	mB	tB	}B	}B	�B	�B	�B	�B	�B	�B	 �B	!�B	'�B	1B	8GB	9NB	<bB	<_B	<aB	9MB	7DB	56B	55B	6=B	7BB	9LB	;[B	;[B	=hB	>nB	>mB	@zB	AB	C�B	C�B	C�B	E�B	G�B	I�B	J�B	K�B	L�B	O�B	R�B	S�B	R�B	R�B	R�B	T�B	U�B	W B	XB	W B	XB	XB	YB	ZB	[B	^)B	a@B	bEB	dNB	fZB	lB	m�B	n�B	o�B	p�B	v�B	y�B	z�B	}�B	}�B	}�B	��B	�B	�B	�%B	�B	�B	�&B	�8B	�FB	�OB	�WB	�[B	�aB	�bB	�rB	�|B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�/B	�XB	�pB	�zB	œB	ƜB	ȥB	ʴB	˷B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	̿B	ʴB	ɮB	ȤB	ǡB	ȧB	ʳB	ʵB	ʲB	ʴB	̾B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�!B	�3B	�FB	�^B	�|B	�wB	�nB	�nB	�jB	�tB	�xB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��G�O�B	��B
!B
\B
�B
!�B
)�B
3%B
66B
<ZB
C�B
I�B
P�B
U�B
[B
a4B
eMB
haB
m~B
q�B
u�B
y�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.17 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201608071436462016080714364620160807143646  AO  ARCAADJP                                                                    20160511091712    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20160511091712  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20160511091712  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160807143646  IP                  G�O�G�O�G�O�                