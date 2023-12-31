CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2015-07-21T02:15:47Z AOML 3.0 creation; 2016-08-07T21:36:37Z UW 3.1 conversion     
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20150721021547  20160807143637  5904461 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               =A   AO  5286_8897_061                   2C  D   APEX                            6531                            072314                          846 @�ar���	1   @�asQ��@2�33333�cQ�Q�1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    =A   B   B   @���@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A���B ��B33B  B��B   B(  B0  B8ffB@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2�C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C��3C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
y�D  D� D  D� D  D� D  D� D  D� D  Dy�D��D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.fD.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dty�Dy�fD��fD�FfD�|�D���D���D�6fD�� D��3D�3D�@ D��fDǠ D� D�FfDډ�D�ɚD��D�@ D� D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @�
>@�p�A�RA"�RAB�RAb�RA�\)A�\)A�\)A�\)A�\)A�\)A�\)A�(�Bz�B�GB�BG�B �B(�B0�B9zB@�BH�BP�BX�B`�Bh�Bp�Bx�B�W
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
B��=B�W
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
+�C+�C+�C+�C+�C+�C+�C+�C+�C+�C+�C +�C"+�C$+�C&+�C(+�C*+�C,+�C.+�C0+�C2EC4+�C6+�C8+�C:+�C<+�C>+�C@+�CB+�CD+�CF+�CH+�CJ+�CL+�CN+�CP+�CR+�CT+�CV+�CX+�CZ+�C\+�C^+�C`+�Cb+�Cd+�Cf+�Ch+�Cj+�Cl+�Cn+�Cp+�Cr+�Ct+�Cv+�Cx+�Cz+�C|+�C~+�C��C��C��C��C��C��C��C��C�"�C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��D 
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
�{D
�D��D
�D��D
�D��D
�D��D
�D��D
�D�{D{D��D
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
�D-��D.GD.��D/
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
�DP��DQ
�DQ��DR
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
�Dt�{Dy�GD���D�K�D��>D��>D�>D�;�D��qD�ؤD��D�EqD���DǥqD�qD�K�DڏD��D�>D�EqD�qD��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��Aإ�A���Aه+A١�A١�A٣�A١�A١�A١�A١�A٣�A٥�A٥�A٣�A١�A٥�A�A�=qA�E�AғuA�"�A�1A�  A���A�M�A�"�AЧ�A�+A�^5A���Aț�A�oA��/A��9A�=qA�A�VA���A�^5A���A���A���A���A��FA���A��yA��9A���A��PA�$�A�A��9A��jA�ƨA��TA��/A��;A�I�A���A�%A�O�A�S�A�bA�r�A���A�(�A���A�ZA�A���A�z�A�dZA��A��FA���A�7LA��A�A�A�VA���A��;A�^5A��-A��`A�oA���A�jA�(�A��jA�dZA���A�/A�ȴA���A�%A�VA�v�A�+A���A���A�n�A�I�A�&�A��A�C�A���A�ƨA�?}A�n�A��A�oA��A��A~r�Ay7LAwx�Au�Arv�AoO�Al�Ai�-AfJA`��AZ�\AU�AL�yAK?}AJ�AG|�ADffAA�^A@�9A?�-A<9XA7�FA6��A6A�A5�mA5A5�PA5%A4=qA3l�A2�uA1��A0ȴA/"�A.bNA-G�A,VA+��A*�HA);dA(I�A(  A'�^A&�/A&�uA$Q�A!�
A ~�A�;AȴA|�A`BA�HA{AS�A�A�AS�A�TA�mA�jAjA�mAXA�A�A��A�A�A
M�A	l�Ar�A�^A�hA�hAS�A��AA�DA��A�jA"�AK�A�A�yAv�Ax�A�DAA J@�\)@�J@��@��\@�=q@��@��R@��h@��`@�ƨ@�"�@�bN@��@��@�@�|�@�@�+@�z�@@�7@�9@��D@��/@���@��@�  @�;d@�@�t�@�l�@�+@�5?@�J@���@���@���@�I�@�ƨ@��@� �@�!@�\)@܃@��m@ۥ�@�+@�E�@�@��@�A�@؋D@ج@�j@��;@׮@֗�@Ցh@�`B@�dZ@Ӆ@���@�Z@�A�@�bN@��@�Ĝ@�b@��
@Ͼw@�S�@��H@�@�r�@�%@���@��y@�  @�=q@��@�ƨ@��@ӥ�@���@Ѻ^@�@̼j@��@��@�A�@�/@�K�@�Z@�  @ϥ�@��m@��
@���@�v�@��@�"�@Õ�@���@��@�
=@�~�@��7@���@�X@ļj@�9X@���@���@�X@�@ēu@�;d@�r�@�t�@�@�-@��@���@�n�@���@��7@�hs@��/@�@�O�@�@�33@��`@�?}@�`B@�hs@�7L@�7L@�7L@�V@�b@�@��/@�1@�t�@��H@�$�@�A�@�9X@�A�@�(�@���@�ff@�`B@�hs@�x�@�V@���@�Z@�b@��w@�K�@�S�@�1'@�A�@��7@��H@�K�@���@�5?@�^5@�J@���@�O�@��@��D@��P@�`B@���@���@��@�1@�b@��@�bN@��/@�j@��F@�\)@���@�X@���@��H@��+@�V@�J@��@�X@��@���@�9X@��w@�\)@��H@���@�ff@�{@���@�7L@���@�z�@�j@�(�@��@���@�o@��!@�ff@�E�@��@���@���@�9X@�t�@�
=@��H@��@���@�^5@�{@��@���@�p�@�O�@�%@��@��@�Z@�1'@� �@���@�|�@�\)@�+@���@�^5@�@��@��@���@���@��u@�j@� �@��
@��F@�dZ@�C�@�;d@���@�V@�=q@�5?@��T@���@��h@�p�@�G�@�V@��`@���@��j@��D@�j@�Z@�9X@�b@��m@��@�@��+@�v�@�ff@�^5@�J@�S�@�dZ@��#@yG�@q%@h��@`�u@W|�@PbN@J=q@AG�@97L@2=q@*�!@$z�@�@�m@��@=q@v�@	hs11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   Aإ�A���Aه+A١�A١�A٣�A١�A١�A١�A١�A٣�A٥�A٥�A٣�A١�A٥�A�A�=qA�E�AғuA�"�A�1A�  A���A�M�A�"�AЧ�A�+A�^5A���Aț�A�oA��/A��9A�=qA�A�VA���A�^5A���A���A���A���A��FA���A��yA��9A���A��PA�$�A�A��9A��jA�ƨA��TA��/A��;A�I�A���A�%A�O�A�S�A�bA�r�A���A�(�A���A�ZA�A���A�z�A�dZA��A��FA���A�7LA��A�A�A�VA���A��;A�^5A��-A��`A�oA���A�jA�(�A��jA�dZA���A�/A�ȴA���A�%A�VA�v�A�+A���A���A�n�A�I�A�&�A��A�C�A���A�ƨA�?}A�n�A��A�oA��A��A~r�Ay7LAwx�Au�Arv�AoO�Al�Ai�-AfJA`��AZ�\AU�AL�yAK?}AJ�AG|�ADffAA�^A@�9A?�-A<9XA7�FA6��A6A�A5�mA5A5�PA5%A4=qA3l�A2�uA1��A0ȴA/"�A.bNA-G�A,VA+��A*�HA);dA(I�A(  A'�^A&�/A&�uA$Q�A!�
A ~�A�;AȴA|�A`BA�HA{AS�A�A�AS�A�TA�mA�jAjA�mAXA�A�A��A�A�A
M�A	l�Ar�A�^A�hA�hAS�A��AA�DA��A�jA"�AK�A�A�yAv�Ax�A�DAA J@�\)@�J@��@��\@�=q@��@��R@��h@��`@�ƨ@�"�@�bN@��@��@�@�|�@�@�+@�z�@@�7@�9@��D@��/@���@��@�  @�;d@�@�t�@�l�@�+@�5?@�J@���@���@���@�I�@�ƨ@��@� �@�!@�\)@܃@��m@ۥ�@�+@�E�@�@��@�A�@؋D@ج@�j@��;@׮@֗�@Ցh@�`B@�dZ@Ӆ@���@�Z@�A�@�bN@��@�Ĝ@�b@��
@Ͼw@�S�@��H@�@�r�@�%@���@��y@�  @�=q@��@�ƨ@��@ӥ�@���@Ѻ^@�@̼j@��@��@�A�@�/@�K�@�Z@�  @ϥ�@��m@��
@���@�v�@��@�"�@Õ�@���@��@�
=@�~�@��7@���@�X@ļj@�9X@���@���@�X@�@ēu@�;d@�r�@�t�@�@�-@��@���@�n�@���@��7@�hs@��/@�@�O�@�@�33@��`@�?}@�`B@�hs@�7L@�7L@�7L@�V@�b@�@��/@�1@�t�@��H@�$�@�A�@�9X@�A�@�(�@���@�ff@�`B@�hs@�x�@�V@���@�Z@�b@��w@�K�@�S�@�1'@�A�@��7@��H@�K�@���@�5?@�^5@�J@���@�O�@��@��D@��P@�`B@���@���@��@�1@�b@��@�bN@��/@�j@��F@�\)@���@�X@���@��H@��+@�V@�J@��@�X@��@���@�9X@��w@�\)@��H@���@�ff@�{@���@�7L@���@�z�@�j@�(�@��@���@�o@��!@�ff@�E�@��@���@���@�9X@�t�@�
=@��H@��@���@�^5@�{@��@���@�p�@�O�@�%@��@��@�Z@�1'@� �@���@�|�@�\)@�+@���@�^5@�@��@��@���@���@��u@�j@� �@��
@��F@�dZ@�C�@�;d@���@�V@�=q@�5?@��T@���@��h@�p�@�G�@�V@��`@���@��j@��D@�j@�Z@�9X@�b@��m@��@�@��+@�v�@�ff@�^5G�O�@�S�@�dZ@��#@yG�@q%@h��@`�u@W|�@PbN@J=q@AG�@97L@2=q@*�!@$z�@�@�m@��@=q@v�@	hs11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB	�DB	��B	�/B	�mB	�mB	�mB	�sB	�mB	�sB	�sB	�sB	�yB	�yB	�yB	�B	�B
C�B
k�B
�^B  BhB�B$�Bp�B��B�!B��B�B�B��B�B��BǮB�wB�qB�dB�?B�3B�?B�dBǮB��B�B�B~�B�B�BBVB�B �B�B"�B!�B�B�BhBhBhBuBuBoB\B1BBBBB	7B1B��B�sB�;B�B��B�RB�3B�B�LB�'B��B�{B�%Bz�B~�B~�B�B�VB�hB�VB�By�Bs�Bq�BcTBG�B8RB!�BJB��B�B�BĜB��B�LB��B�JBbNBPB
��B
�oB
XB
'�B
B	�B	ȴB	�dB	��B	�=B	r�B	bNB	L�B	0!B	�B	B�TB�/B�B��B��BÖB�}B�dB�dB�9B�-B�'B�-B�-B�-B�9B�LB�XB�^B�FB�XB��B�}B�}B��B��B�}B��B��B��B�#B�B�
B��B��B�^B�FB�FB�9B�B�dB�}B�wB�}B�qB�XB�?B�3B�B�B��B�B�?B�3B�B��B��B��B��B��B��B�B�9B�dB�RB�?B�B�^B��BȴB��B��B��B��B�
B��B��BɺBȴBĜB�wB�^B�dBƨB��B��B�B��B��BȴB�wB�LB�9B�FB�LB�dB��B�sB��B	B	B	DB	PB	PB	PB	�B	#�B	'�B	(�B	,B	.B	1'B	2-B	1'B	,B	&�B	%�B	 �B	�B	�B	hB	JB	JB	JB	PB	DB	DB	JB	PB	oB	�B	�B	�B	�B	�B	�B	�B	oB	�B	�B	�B	�B	!�B	�B	�B	�B	�B	�B	�B	 �B	(�B	O�B	[#B	YB	XB	dZB	aHB	k�B	s�B	u�B	v�B	u�B	t�B	jB	cTB	iyB	u�B	p�B	w�B	�7B	�hB	��B	��B	��B	��B	��B	��B	�uB	|�B	k�B	S�B	O�B	K�B	J�B	K�B	XB	�+B	��B	��B	��B	��B	�9B	�XB	�XB	�dB	ŢB	ƨB	B	ÖB	��B	��B	��B	��B	��B	��B	��B	�wB	�B	�9B	�wB	ɺB	��B	��B	��B	��B	��B	��B	��B	ɺB	ŢB	��B	�qB	�dB	�RB	�?B	�3B	�9B	�9B	�9B	�-B	�-B	�!B	�'B	�-B	�'B	�B	�B	�B	�B	�B	�9B	�jB	��B	��B	�
B	�#B	�B	�B	�)B	�#B	�#B	�B	�B	�B	�B	��B	��B	��B	��B	��B	�B	�B	�B	�BB	�HB	�HB	�BB	�BB	�/B	�B	�B	�B	�B	�B	�B	�B	�#B	�)B	�/B	�5B	�5B	�;B	�;B	�BB	�HB	�HB	�NB	�TB	�`B	�sB	�sB	�sB	�sB	�mB	�mB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
+B
	7B

=B
�B
�B
!�B
%�B
.B
33B
6FB
:^B
>wB
A�B
H�B
Q�B
XB
_;B
cTB
jB
k�B
n�B
s�B
x�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   B	�CB	��B	�+B	�hB	�hB	�iB	�nB	�kB	�qB	�pB	�mB	�xB	�xB	�vB	�|B	�B
C�B
k{B
�TB
��B]B�B$�Bp�B��B�B��B�B�sB��B�B��BǠB�jB�dB�XB�/B�$B�2B�XBǝB��B�B�B~�B�B�BBIB�B �B�B"�B!�B�BtBXBZBYBhBhB^BOB$BBBBB	*B'B��B�jB�-B�	B˺B�DB�'B�B�>B�B��B�lB�Bz�B~�B~�B�B�IB�WB�GB�
By�Bs�Bq�BcBBG�B8CB!�B7B��B�yB��BĎB�xB�:B��B�;Bb?B@B
��B
�`B
XB
'�B
 �B	�B	ȭB	�^B	��B	�5B	r�B	bKB	L�B	0B	�B	B�TB�/B�B��B��B×B�B�eB�cB�:B�-B�+B�.B�-B�/B�;B�NB�YB�_B�IB�XB��B�|B�{B��B��B�}B��B��B��B� B�B�
B��B��B�^B�GB�EB�9B�B�dB�{B�vB�{B�oB�VB�?B�5B�B�B��B�B�?B�1B�B��B��B��B��B��B��B�B�8B�bB�RB�>B�B�^B�BȱB��B��B��B��B�B��B��BɷBȱBĝB�uB�[B�bBƣB��B��B�B��B��BȲB�uB�JB�5B�CB�IB�cB��B�pB��B	B	B	=B	IB	JB	JB	�B	#�B	'�B	(�B	,B	.B	1B	2$B	1B	+�B	&�B	%�B	 �B	�B	|B	aB	CB	DB	AB	GB	?B	>B	DB	LB	hB	�B	�B	�B	�B	�B	zB	~B	fB	B	�B	�B	�B	!�B	�B	�B	�B	�B	�B	�B	 �B	(�B	O�B	[B	YB	XB	dSB	a>B	kzB	s�B	u�B	v�B	u�B	t�B	jsB	cKB	imB	u�B	p�B	w�B	�*B	�\B	�sB	��B	��B	��B	��B	��B	�hB	|�B	k|B	S�B	O�B	K�B	J�B	K�B	XB	�B	��B	��B	��B	�tB	�*B	�KB	�KB	�YB	œB	ƚB	B	ÈB	��B	��B	��B	��B	��B	��B	ʳB	�jB	�B	�.B	�kB	ɬB	˸B	��B	̿B	̿B	̿B	˸B	˹B	ɭB	œB	�{B	�dB	�XB	�DB	�/B	�%B	�+B	�)B	�,B	�!B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�+B	�[B	�}B	˷B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�
B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�4B	�:B	�9B	�4B	�4B	� B	�B	� B	�B	�B	�B	�B	�B	�B	�B	�"B	�)B	�%B	�+B	�.B	�2B	�9B	�;B	�>B	�GB	�SB	�fB	�dB	�hB	�fB	�^B	�_B	�cB	�B	�uB	�|B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
 �B
 �B
 �B
�B
B

B

B
B
	B

B
B

B
	B
	B

B
B
B
B
B
B
G�O�B

,B
qB
�B
!�B
%�B
.B
3$B
65B
:MB
>eB
AxB
H�B
Q�B
X B
_*B
cEB
jmB
krB
n�B
s�B
x�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.17 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201608071436372016080714363720160807143637  AO  ARCAADJP                                                                    20150721021547    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20150721021547  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20150721021547  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160807143637  IP                  G�O�G�O�G�O�                