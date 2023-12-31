CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2015-09-06T09:16:22Z AOML 3.0 creation; 2016-08-07T21:36:38Z UW 3.1 conversion     
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
_FillValue                    �4Argo profile    3.1 1.2 19500101000000  20150906091622  20160807143638  5904461 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               FA   AO  5286_8897_070                   2C  D   APEX                            6531                            072314                          846 @�mOiv�1   @�mP@yd�@4�bM���c2I�^51   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    FA   B   B   @9��@�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B ffB(ffB0  B8  B@  BH  BP  BX  Ba33Bg33Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dtl�Dy�fD�3D�P D�vfD�� D�3D�FfD���D�ɚD�  D�C3D�|�D��3D��fD�33D�l�D�� D��D�FfD�p D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @Dz�@�p�@�p�A�RA"�RAB�RAb�RA�\)A�\)A�\)A�\)A�\)A�\)A�\)A�\)B �B�B�B�B!zB)zB0�B8�B@�BH�BP�BX�Ba�GBg�GBp�Bx�B�W
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
+�C+�C+�C+�C+�C+�C+�C+�C+�C+�C+�C +�C"+�C$+�C&+�C(+�C*+�C,+�C.+�C0+�C2+�C4+�C6+�C8+�C:+�C<+�C>+�C@+�CB+�CD+�CF+�CH+�CJ+�CL+�CN+�CP+�CR+�CT+�CV+�CX+�CZ+�C\+�C^+�C`+�Cb+�Cd+�Cf+�Ch+�Cj+�Cl+�Cn+�Cp+�Cr+�Ct+�Cv+�Cx+�Cz+�C|+�C~+�C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��D 
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
�Dtw�Dy�GD��D�UqD�{�D��qD��D�K�D��D��D�qD�H�D��>D�ؤD���D�8�D�r>D��qD�>D�K�D�uqD��>111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�%A�1A�oA��A��A�
=A⟾A�C�A�
=A�ȴA�RA�A��AᛦAᕁA�|�A�t�A�7A�~�A�dZA��A�`BA�{A߶FA�~�A�;dA��HAڇ+A�JAхA��HA��#A̺^A�O�A��A�v�A��A�bNAǛ�AƓuA��yAħ�A�-A��A��#A¼jA�^5A��A��`A��A��hA���A�  A�{A��A���A�S�A��RA���A��A��+A��RA���A�/A��hA� �A�C�A�hsA���A�{A�9XA�hsA���A���A�dZA���A�ĜA�hsA�1'A� �A���A��/A�VA�A���A��`A�E�A��uA��;A��\A�A��A�t�A�A�bA��uA��-A��A�A�A���A��A���A���A���A��`A��jA��A��wA�K�A��A�A���A�A}&�Ax�AsƨAp9XAj�AfQ�Ad5?A`�!A_��A^�9A^bA]S�A[ƨAY\)AW33AV��AU��AT��AR~�AN�RAM7LAJ�AG�hAD�+AC7LABbNA@A�A>�A=+A<�DA:ĜA9C�A7hsA6�\A5��A5oA3��A2�9A1?}A0ZA,��A+O�A+ƨA*�A)%A(9XA'/A'p�A'��A'��A&��A&A$��A#\)A"-A!�A JA��Av�A��A?}A�RAVAoA�9A�FA1'A��A�A��Av�A��A�A�A
1A	�#A�A�A~�A-A�AG�A��AjAA �`A 5?A {@���@���@��^@�A E�AXAx�AK�A��AQ�A�jAA�@�@�+@�"�@���A $�@��7@��@��m@�;d@��@�F@�j@��@���@��@��@� �@�+@�@@�M�@��@���@�9@�w@�dZ@�"�@��H@�~�@�V@��@�l�@�1'@�1@�ƨ@�@�r�@�l�@�~�@�J@�
=@�5?@�hs@�ƨ@�+@�p�@���@���@�(�@���@�5?@�?}@�$�@�K�@ٙ�@ۮ@���@ڇ+@ڗ�@ؼj@ץ�@��@���@У�@Ͼw@�;d@�"�@�\)@�n�@�A�@��@ʏ\@̃@͑h@���@�A�@�^5@�33@��
@�^5@�J@���@ź^@��#@ȓu@��/@�/@ȴ9@ǝ�@��@˥�@�ȴ@š�@š�@Å@ź^@�l�@Ȭ@�I�@���@Ə\@��m@���@�z�@�9X@�I�@�Z@�Z@�^5@���@��@� �@ȼj@�z�@�j@ț�@��@ŉ7@�r�@�bN@�9X@��@�S�@�$�@���@��-@�v�@��@��P@���@��@�x�@���@�Ĝ@��-@��@\@�ȴ@���@¸R@�~�@�J@���@���@�bN@���@���@�$�@��#@���@��@�  @�\)@�ȴ@��@���@��@��/@�S�@�^5@�&�@��`@���@�;d@�C�@�l�@�K�@���@���@�7L@��@���@��@��j@�33@�C�@� �@�;d@��T@�p�@�?}@���@�1@���@�;d@�o@���@��w@���@��@���@�
=@���@�5?@��T@���@��h@�`B@��@��9@�z�@�9X@���@�\)@�C�@�@�@��h@�X@��/@��@�ƨ@��y@���@���@�G�@���@��D@�Z@�Q�@�I�@�1'@�1@��w@�t�@�K�@��@�@��@���@�v�@���@�K�@���@�A�@�(�@�hs@��@�-@�5?@��@�p�@��@��9@��@���@�A�@��w@�dZ@���@���@�j@�b@�ƨ@��F@�C�@���@�^5@�-@���@���@���@��@��@�7L@���@��;@�C�@�
=@��H@���@��\@�v�@�=q@�V@�ȴ@���@�ff@y��@pĜ@h��@`b@Vff@Pb@G��@?|�@8�@.5?@(��@#��@!7L@Z@bN@��@��@��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  A�%A�1A�oA��A��A�
=A⟾A�C�A�
=A�ȴA�RA�A��AᛦAᕁA�|�A�t�A�7A�~�A�dZA��A�`BA�{A߶FA�~�A�;dA��HAڇ+A�JAхA��HA��#A̺^A�O�A��A�v�A��A�bNAǛ�AƓuA��yAħ�A�-A��A��#A¼jA�^5A��A��`A��A��hA���A�  A�{A��A���A�S�A��RA���A��A��+A��RA���A�/A��hA� �A�C�A�hsA���A�{A�9XA�hsA���A���A�dZA���A�ĜA�hsA�1'A� �A���A��/A�VA�A���A��`A�E�A��uA��;A��\A�A��A�t�A�A�bA��uA��-A��A�A�A���A��A���A���A���A��`A��jA��A��wA�K�A��A�A���A�A}&�Ax�AsƨAp9XAj�AfQ�Ad5?A`�!A_��A^�9A^bA]S�A[ƨAY\)AW33AV��AU��AT��AR~�AN�RAM7LAJ�AG�hAD�+AC7LABbNA@A�A>�A=+A<�DA:ĜA9C�A7hsA6�\A5��A5oA3��A2�9A1?}A0ZA,��A+O�A+ƨA*�A)%A(9XA'/A'p�A'��A'��A&��A&A$��A#\)A"-A!�A JA��Av�A��A?}A�RAVAoA�9A�FA1'A��A�A��Av�A��A�A�A
1A	�#A�A�A~�A-A�AG�A��AjAA �`A 5?A {@���@���@��^@�A E�AXAx�AK�A��AQ�A�jAA�@�@�+@�"�@���A $�@��7@��@��m@�;d@��@�F@�j@��@���@��@��@� �@�+@�@@�M�@��@���@�9@�w@�dZ@�"�@��H@�~�@�V@��@�l�@�1'@�1@�ƨ@�@�r�@�l�@�~�@�J@�
=@�5?@�hs@�ƨ@�+@�p�@���@���@�(�@���@�5?@�?}@�$�@�K�@ٙ�@ۮ@���@ڇ+@ڗ�@ؼj@ץ�@��@���@У�@Ͼw@�;d@�"�@�\)@�n�@�A�@��@ʏ\@̃@͑h@���@�A�@�^5@�33@��
@�^5@�J@���@ź^@��#@ȓu@��/@�/@ȴ9@ǝ�@��@˥�@�ȴ@š�@š�@Å@ź^@�l�@Ȭ@�I�@���@Ə\@��m@���@�z�@�9X@�I�@�Z@�Z@�^5@���@��@� �@ȼj@�z�@�j@ț�@��@ŉ7@�r�@�bN@�9X@��@�S�@�$�@���@��-@�v�@��@��P@���@��@�x�@���@�Ĝ@��-@��@\@�ȴ@���@¸R@�~�@�J@���@���@�bN@���@���@�$�@��#@���@��@�  @�\)@�ȴ@��@���@��@��/@�S�@�^5@�&�@��`@���@�;d@�C�@�l�@�K�@���@���@�7L@��@���@��@��j@�33@�C�@� �@�;d@��T@�p�@�?}@���@�1@���@�;d@�o@���@��w@���@��@���@�
=@���@�5?@��T@���@��h@�`B@��@��9@�z�@�9X@���@�\)@�C�@�@�@��h@�X@��/@��@�ƨ@��y@���@���@�G�@���@��D@�Z@�Q�@�I�@�1'@�1@��w@�t�@�K�@��@�@��@���@�v�@���@�K�@���@�A�@�(�@�hs@��@�-@�5?@��@�p�@��@��9@��@���@�A�@��w@�dZ@���@���@�j@�b@�ƨ@��F@�C�@���@�^5@�-@���@���@���@��@��@�7L@���@��;@�C�@�
=@��H@���@��\@�v�@�=qG�O�@�ȴ@���@�ff@y��@pĜ@h��@`b@Vff@Pb@G��@?|�@8�@.5?@(��@#��@!7L@Z@bN@��@��@��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB
uB
uB
uB
uB
uB
hB
%B	��B	��B	��B	��B	��B	��B	��B	��B
\B
s�B
�dB
��B
�B
��B
�B
�B
�B
�B
�sB
��B�B2-B~�B��B�!B�wB�}B��BVBT�BaHBgmB��B�'B��B�dBǮB��B��B�B�B�B�B��B	7B�B�B�B�B!�B)�B-B.B.B1'B49B5?B33B49B9XB6FB,B �BbB��B��B��B�B�B�B�B�B�B�B�B�B�B��B��BŢB�FB��B��B�1BjBA�B&�B{B1B��B�B�RB��B�BW
B7LB{B
�HB
��B
�'B
��B
�{B
�DB
{�B
dZB
P�B
0!B
uB	�B	ŢB	�uB	u�B	ffB	Q�B	J�B	D�B	@�B	9XB	-B	�B	hB	JB	+B	B��B�mB�NB��BŢBÖB�}B�FB�B��B��B��B�JB�JB��B�B�B�-B�-B�!B�B��B��B��B�9B�-B�B�B�9BŢB��B�B�B��B��B�B�#B�
B��B��B��B��B�{B�{B��B��B��B�XB��B�jB��B��B��B� BiyB]/BR�BaHBW
BH�BF�BG�B@�B6FB33B33B33B33B5?B6FB<jB;dB@�BL�B]/B��B��B��B��B��B�BŢBĜB��B�uB��B�LB��B�}B�9B��B�B��B��B��B�^B�dB�^BŢB��B�;B�fB�B�B�B�B��B��B��B��B��B��B��B	JB	�B	-B	1'B	6FB	9XB	C�B	B�B	A�B	C�B	N�B	L�B	I�B	F�B	J�B	G�B	I�B	O�B	M�B	C�B	<jB	:^B	E�B	@�B	A�B	O�B	S�B	Q�B	\)B	XB	R�B	J�B	B�B	@�B	?}B	?}B	@�B	D�B	D�B	B�B	?}B	?}B	N�B	W
B	S�B	R�B	M�B	T�B	\)B	ZB	ZB	T�B	O�B	R�B	l�B	n�B	s�B	r�B	p�B	~�B	�+B	s�B	p�B	v�B	n�B	|�B	�=B	�oB	�hB	�JB	�7B	�B	z�B	w�B	w�B	x�B	y�B	z�B	�JB	��B	�B	�RB	�qB	�jB	�jB	�jB	�-B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	�9B	�?B	�qB	��B	�wB	ƨB	��B	��B	��B	�
B	�B	�B	�)B	�/B	�)B	�)B	�#B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	ǮB	ĜB	�}B	�wB	�}B	�jB	�jB	�}B	�}B	�qB	�jB	�dB	�dB	�jB	�FB	�'B	�B	�!B	�FB	�3B	�B	�B	�B	�'B	�'B	�'B	�'B	�9B	�LB	�RB	�^B	�dB	�jB	�wB	�}B	�}B	��B	ÖB	ĜB	ĜB	ĜB	ŢB	ƨB	ŢB	ǮB	ǮB	ƨB	ƨB	ƨB	ǮB	ǮB	ǮB	ɺB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�;B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	��B	��B	��B
	7B	��B
1B
oB
�B
 �B
,B
0!B
6FB
<jB
C�B
I�B
S�B
W
B
^5B
aHB
e`B
iyB
n�B
r�B
u�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  B
pB
pB
pB
pB
nB
cB
#B	��B	��B	��B	��B	��B	��B	��B	��B
ZB
s�B
�ZB
��B
�B
��B
�B
�B
�B
�B
�iB
��B�B2B~�B��B�B�jB�rB��BHBT�Ba6Bg`B�tB�B��B�UBǝBʵB��B��B�B�tB�B��B	(BzB�B�B�B!�B)�B-B.B.B1B4-B53B3(B4+B9LB6:B+�B �BSB��B��B��B�B�B�B�B�yB�yB�B�B�tB� B��B��BŐB�:B��B��B�!BjqBA}B&�BkBB��B��B�?B��B�BV�B7<BlB
�;B
˸B
�B
��B
�lB
�7B
{�B
dNB
P�B
0B
mB	�zB	ŝB	�oB	u�B	faB	Q�B	J�B	D�B	@�B	9TB	-B	�B	hB	IB	)B	B��B�lB�MB��BŢBØB�}B�EB�B��B��B��B�LB�OB��B�	B�B�/B�-B�"B�B��B��B��B�<B�,B�B�B�:BšB��B�B�B��B��B�|B�B�B��B��B��B��B�|B�|B��B��B��B�WB��B�hB��B��B��B�Bi{B]1BR�BaHBWBH�BF�BG�B@�B6GB38B3B38B38B5BB6IB<nB;hB@�BL�B]1B��B��B��B��B��B�BşBĚB��B�sB��B�JB��B�|B�8B��B�B��B��B��B�[B�aB�[BŞB��B�8B�bB�B�B�B�B��B��B��B��B��B��B��B	EB	�B	-B	1B	6@B	9NB	C�B	B�B	A�B	C�B	N�B	L�B	I�B	F�B	J�B	G�B	I�B	O�B	M�B	C�B	<bB	:WB	E�B	@zB	A�B	O�B	S�B	Q�B	\ B	XB	R�B	J�B	B�B	@{B	?sB	?tB	@{B	D�B	D�B	B�B	?uB	?vB	N�B	WB	S�B	R�B	M�B	T�B	\ B	ZB	ZB	T�B	O�B	R�B	l�B	n�B	s�B	r�B	p�B	~�B	� B	s�B	p�B	v�B	n�B	|�B	�1B	�dB	�[B	�=B	�+B	�B	z�B	w�B	w�B	x�B	y�B	z�B	�=B	��B	�B	�EB	�cB	�]B	�^B	�_B	�!B	�B	�B	�B	�B	��B	��B	�|B	�zB	�|B	��B	��B	��B	�+B	�/B	�fB	�|B	�jB	ƛB	˹B	��B	��B	��B	�B	�B	�B	�!B	�B	�B	�B	� B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	˸B	ǢB	ČB	�qB	�kB	�rB	�\B	�^B	�pB	�nB	�dB	�^B	�YB	�YB	�^B	�8B	�B	�
B	�B	�8B	�&B	�B	�B	�B	�B	�B	�B	�B	�*B	�?B	�DB	�PB	�ZB	�\B	�kB	�qB	�qB	�zB	ÉB	čB	ďB	čB	ŕB	ƘB	ŔB	ǢB	ǡB	ƚB	ƛB	ƚB	ǡB	ǢB	ǠB	ɬB	ɭB	ʴB	ʳB	˹B	̽B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�,B	�wB	�vB	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	��B	��G�O�B
	'B	��B
B
]B
�B
 �B
+�B
0B
66B
<ZB
C�B
I�B
S�B
V�B
^$B
a5B
eLB
igB
n�B
r�B
u�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.17 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201608071436382016080714363820160807143638  AO  ARCAADJP                                                                    20150906091622    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20150906091622  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20150906091622  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160807143638  IP                  G�O�G�O�G�O�                