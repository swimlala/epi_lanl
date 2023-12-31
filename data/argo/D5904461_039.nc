CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2015-03-26T18:51:01Z AOML 3.0 creation; 2016-08-07T21:36:33Z UW 3.1 conversion     
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
_FillValue                    �XArgo profile    3.1 1.2 19500101000000  20150326185101  20160807143634  5904461 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               'A   AO  5286_8897_039                   2C  D   APEX                            6531                            072314                          846 @�Dj�N�
1   @�Dk-��@2u�$�/�c��Q�1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    'A   B   B   @9��@�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A���A���B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D�fD  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dt�fDy� D�	�D�@ D���D��3D��D�@ D�� D��fD��fD�I�D��fD�ɚD�fD�C3Dړ3D��fD���D�L�D�D���1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @Dz�@�p�@�p�A�RA"�RAB�RAb�RA�\)A�\)A�\)A�\)A�\)A�\)A�(�A�(�B �B�B�B�B �B(�B0�B8�B@�BH�BP�BX�B`�Bh�Bp�Bx�B�W
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
+�C+�C+�C+�C+�C+�C+�C+�C+�C+�C+�C +�C"+�C$+�C&+�C(+�C*+�C,+�C.+�C0+�C2+�C4+�C6+�C8+�C:+�C<+�C>+�C@+�CB+�CD+�CF+�CH+�CJ+�CL+�CN+�CP+�CR+�CT+�CV+�CX+�CZ+�C\+�C^+�C`+�Cb+�Cd+�Cf+�Ch+�Cj+�Cl+�Cn+�Cp+�Cr+�Ct+�Cv+�Cx+�Cz+�C|+�C~+�C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��D 
�D ��D
�D��D
�D��D
�D��D
�D��D
�D��D
�D�GD
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
�Dt��Dt�GDy��D�D�EqD��>D�ȤD�>D�EqD��qD���D���D�OD���D��D��D�H�Dژ�D���D��D�R>D�D��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�oA�oA�oA�oA�oA�oA�oA�oA�{A�bA�VA�oA��A��A��A���AȑhA�;dA�/A�G�Aȩ�A���A�-A�5?A�?}A� �A�bNAǥ�A�;dA���A�AƝ�A�M�A�%A�ȴAť�Aş�AœuAŅA�dZA�$�A���A��yA��HA��#Aĺ^A�O�A�VA���A��A��A��TAþwAé�Aò-AüjAÅA�1'A�A���A�"�A��DA��jA�=qA��uA�O�A�1'A���A���A�{A��!A�1'A��A�-A��+A�1'A�dZA�ȴA�%A�1'A��A�JA�1'A�K�A���A�7LA���A�dZA���A���A�%A�7LA��A���A��;A�E�A�ĜA�A�Q�A�A�ƨA� �A���A���A���A��A}ƨAz�AwC�As�Ao��Ao
=Ak�AeG�Aa��A^9XA[G�AW��AV  AT�jAQ��AN�+AM�AK��AJ �AHffAEC�AC�hAA|�A?��A=�mA<��A:��A9�A6M�A3A2n�A1��A0I�A.��A-A,��A+ƨA+?}A*A�A)�FA)\)A)ƨA*ZA*I�A*bA)A(��A'�hA&I�A%O�A#/A#x�A!G�A��A�A��A|�A��A�`Az�A��A�A�A�A�HA��A�A�9A"�AJA�^AXAv�A  A�TAjA�AdZAAE�A�AA�A�AhsA\)AG�A/A�/A9XA�A�-A7LA
��A	��A�A�uA5?A�7A�A�A��AM�A$�A��A�PA �jA �@�ƨ@���@��-@��@�v�A �A  �A {@�
=@�{@�/@�ƨ@���@��@��j@�33@�^5@�X@�j@�n�@��#@��T@�-@@�ȴ@�V@�7@�@�C�@�7L@�bN@�z�@�  @�~�@��@�%@�P@�C�@�E�@�9X@��@�&�@�I�@���@�C�@�K�@�S�@��#@؛�@�V@�V@� �@Լj@ԋD@��m@��#@���@���@��T@̋D@�1@�ȴ@��@Ǿw@�S�@���@�Ĝ@�Q�@�  @Å@Õ�@�t�@��@���@��@�K�@��y@§�@�v�@��^@��@��@���@��P@�dZ@�@�^5@�ff@�{@�@�hs@�?}@�/@���@���@�dZ@�K�@�C�@�@�l�@��
@�z�@�A�@�dZ@�$�@���@�%@�bN@��@���@�b@��F@�\)@�;d@��@�n�@��@��#@���@�hs@�/@�V@���@���@���@��D@��;@���@��P@�\)@�;d@�o@��@�n�@�{@���@���@�X@�&�@��/@�bN@�(�@��
@���@�dZ@�K�@�+@�@�ȴ@�v�@�E�@���@�`B@���@���@�Z@�ƨ@��@�C�@��H@�M�@�-@�@���@�G�@�/@���@�j@�  @��m@��w@���@��@�S�@�@�ȴ@�^5@���@�O�@��@��@�V@���@�z�@�Q�@� �@��@���@�S�@�;d@��@���@��!@��+@�=q@��h@�G�@���@�Ĝ@�9X@��w@�dZ@�
=@��@���@�o@��@��+@�$�@��@���@�p�@�hs@�G�@��u@��@��m@��@��;@���@��!@��+@�@�+@��m@���@��w@��@�o@��@���@��!@�v�@�@���@���@�x�@�?}@�b@�C�@�
=@�ȴ@�v�@�E�@�M�@��h@�?}@�x�@��@���@�Q�@�1@���@���@��F@�t�@�S�@�33@�K�@�l�@���@�$�@�@�x�@�`B@�/@�/@�&�@���@���@��@�A�@���@���@��F@�\)@���@��@�K�@��F@�@v5?@n@d9X@Z=q@SdZ@K��@D(�@=@4�/@.ff@'�@#@v�@��@ff@&�@I�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111 A�oA�oA�oA�oA�oA�oA�oA�oA�{A�bA�VA�oA��A��A��A���AȑhA�;dA�/A�G�Aȩ�A���A�-A�5?A�?}A� �A�bNAǥ�A�;dA���A�AƝ�A�M�A�%A�ȴAť�Aş�AœuAŅA�dZA�$�A���A��yA��HA��#Aĺ^A�O�A�VA���A��A��A��TAþwAé�Aò-AüjAÅA�1'A�A���A�"�A��DA��jA�=qA��uA�O�A�1'A���A���A�{A��!A�1'A��A�-A��+A�1'A�dZA�ȴA�%A�1'A��A�JA�1'A�K�A���A�7LA���A�dZA���A���A�%A�7LA��A���A��;A�E�A�ĜA�A�Q�A�A�ƨA� �A���A���A���A��A}ƨAz�AwC�As�Ao��Ao
=Ak�AeG�Aa��A^9XA[G�AW��AV  AT�jAQ��AN�+AM�AK��AJ �AHffAEC�AC�hAA|�A?��A=�mA<��A:��A9�A6M�A3A2n�A1��A0I�A.��A-A,��A+ƨA+?}A*A�A)�FA)\)A)ƨA*ZA*I�A*bA)A(��A'�hA&I�A%O�A#/A#x�A!G�A��A�A��A|�A��A�`Az�A��A�A�A�A�HA��A�A�9A"�AJA�^AXAv�A  A�TAjA�AdZAAE�A�AA�A�AhsA\)AG�A/A�/A9XA�A�-A7LA
��A	��A�A�uA5?A�7A�A�A��AM�A$�A��A�PA �jA �@�ƨ@���@��-@��@�v�A �A  �A {@�
=@�{@�/@�ƨ@���@��@��j@�33@�^5@�X@�j@�n�@��#@��T@�-@@�ȴ@�V@�7@�@�C�@�7L@�bN@�z�@�  @�~�@��@�%@�P@�C�@�E�@�9X@��@�&�@�I�@���@�C�@�K�@�S�@��#@؛�@�V@�V@� �@Լj@ԋD@��m@��#@���@���@��T@̋D@�1@�ȴ@��@Ǿw@�S�@���@�Ĝ@�Q�@�  @Å@Õ�@�t�@��@���@��@�K�@��y@§�@�v�@��^@��@��@���@��P@�dZ@�@�^5@�ff@�{@�@�hs@�?}@�/@���@���@�dZ@�K�@�C�@�@�l�@��
@�z�@�A�@�dZ@�$�@���@�%@�bN@��@���@�b@��F@�\)@�;d@��@�n�@��@��#@���@�hs@�/@�V@���@���@���@��D@��;@���@��P@�\)@�;d@�o@��@�n�@�{@���@���@�X@�&�@��/@�bN@�(�@��
@���@�dZ@�K�@�+@�@�ȴ@�v�@�E�@���@�`B@���@���@�Z@�ƨ@��@�C�@��H@�M�@�-@�@���@�G�@�/@���@�j@�  @��m@��w@���@��@�S�@�@�ȴ@�^5@���@�O�@��@��@�V@���@�z�@�Q�@� �@��@���@�S�@�;d@��@���@��!@��+@�=q@��h@�G�@���@�Ĝ@�9X@��w@�dZ@�
=@��@���@�o@��@��+@�$�@��@���@�p�@�hs@�G�@��u@��@��m@��@��;@���@��!@��+@�@�+@��m@���@��w@��@�o@��@���@��!@�v�@�@���@���@�x�@�?}@�b@�C�@�
=@�ȴ@�v�@�E�@�M�@��h@�?}@�x�@��@���@�Q�@�1@���@���@��F@�t�@�S�@�33@�K�@�l�@���@�$�@�@�x�@�`B@�/@�/@�&�@���@���@��@�A�@���@���@��F@�\)G�O�@��@�K�@��F@�@v5?@n@d9X@Z=q@SdZ@K��@D(�@=@4�/@.ff@'�@#@v�@��@ff@&�@I�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111 ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB	�jB	�jB	�jB	�jB	�jB	�qB	�qB	�qB	�qB	�qB	�qB	�qB	�qB	�jB	�dB	�B
B
�B
�B
7LB
}�B
�qBB?}Bk�Bv�B�oB��BǮB��B��B�
B�B�;B�NB�ZB�fB�B�B�B�B��B��B��B��BBuB�B�B �B%�B+B0!B>wBC�BG�BbNBp�Bv�B�JB��B��B�VB]/BM�BE�B;dB,B1'B7LB5?B,BPB�B�fBɺB�VBW
B�+B�oB��B��B�\B�Bo�By�B�bBt�BXB7LB �BDBuBB
�B
�B
��B
�LB
��B
�B
gmB
J�B
9XB
'�B
�B
B	�B	�B	��B	��B	�+B	{�B	]/B	8RB	'�B	{B	B�B�ZB�)B��BĜB�}B�qB�dB�^B�wB�qB�jB�LB�'B�!B��B��B��B��B��B��B��B��B��B�B�dB��BƨBĜBȴB�B�B		7B	
=B	DB	DB	PB	\B	1B	B	�B	�B	PB	hB	uB	{B	uB	�B	�B	{B	�B	�B	5?B	J�B	I�B	E�B	8RB	.B	-B	0!B	.B	.B	6FB	<jB	I�B	YB	e`B	l�B	jB	jB	k�B	jB	jB	q�B	t�B	s�B	s�B	o�B	ffB	]/B	ZB	VB	M�B	H�B	G�B	H�B	@�B	;dB	5?B	2-B	1'B	/B	.B	2-B	2-B	1'B	1'B	0!B	0!B	49B	:^B	F�B	K�B	P�B	O�B	M�B	K�B	I�B	F�B	E�B	E�B	C�B	C�B	A�B	>wB	A�B	D�B	F�B	H�B	F�B	F�B	H�B	Q�B	P�B	J�B	E�B	L�B	XB	XB	YB	XB	YB	ZB	`BB	cTB	`BB	]/B	ZB	[#B	^5B	^5B	aHB	gmB	ffB	gmB	hsB	iyB	hsB	q�B	s�B	r�B	m�B	hsB	dZB	_;B	\)B	[#B	YB	S�B	S�B	S�B	P�B	M�B	M�B	N�B	T�B	ZB	^5B	`BB	e`B	iyB	n�B	n�B	p�B	p�B	p�B	m�B	l�B	n�B	o�B	p�B	q�B	s�B	w�B	x�B	x�B	z�B	{�B	~�B	�B	�B	�B	�B	�B	�B	�1B	�JB	�uB	�uB	�hB	�VB	�DB	�\B	�bB	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�!B	�'B	�'B	�'B	�9B	�9B	�?B	�FB	�LB	�LB	�LB	�RB	�XB	�^B	�^B	�qB	�}B	��B	ÖB	ĜB	ŢB	ƨB	ǮB	ȴB	ɺB	ɺB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�
B	�
B	�B	�B	�B	�B	�B	�B	�#B	�/B	�5B	�5B	�5B	�;B	�BB	�BB	�HB	�NB	�NB	�NB	�NB	�TB	�ZB	�`B	�fB	�fB	�fB	�fB	�mB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B
  B
B
B
  B
B
B
B
B
B
B
B
B
B
B	��B	��B
  B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
1B

=B
	7B
	7B
1B
1B
	7B
	7B
	7B
	7B
	7B
	7B
	7B
	7B
	7B
	7B

=B
DB
bB
VB
bB
�B
�B
 �B
&�B
-B
49B
8RB
=qB
C�B
I�B
P�B
W
B
\)B
`BB
dZB
hsB
k�B
p�B
t�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111 B	�iB	�gB	�hB	�fB	�gB	�mB	�pB	�mB	�mB	�pB	�mB	�pB	�pB	�hB	�bB	�B
B
�B
�B
7EB
}�B
�gBB?oBkwBv�B�bB��BǢB��B��B��B�
B�,B�@B�IB�WB�xB�|B�B�B��B��B��B��BBiB�B�B �B%�B*�B0B>lBC�BG�Bb>Bp�Bv�B�;B��B��B�FB]!BM�BE�B;VB+�B1B7<B50B+�BAB�	B�WBɫB�IBV�B�B�aB��B��B�MB�Bo�By�B�QBt�BX B7<B �B6BiBB
�B
�tB
��B
�=B
�zB
��B
gcB
J�B
9PB
'�B
�B
B	�B	�B	�{B	��B	�%B	{�B	]+B	8PB	'�B	yB	B�B�ZB�(B��BĞB�~B�sB�hB�aB�vB�rB�jB�MB�(B�#B��B��B��B��B��B��B��B��B��B�B�eB��BƧBĝBȵB�B�B		3B	
:B	>B	BB	OB	ZB	/B	B	�B	�B	NB	eB	oB	xB	pB	{B	}B	uB	�B	�B	59B	J�B	I�B	E�B	8LB	.B	-B	0B	.B	.B	6>B	<eB	I�B	YB	eWB	l�B	jwB	jvB	k~B	jvB	jwB	q�B	t�B	s�B	s�B	o�B	f\B	]$B	ZB	U�B	M�B	H�B	G�B	H�B	@|B	;\B	5:B	2&B	1"B	/B	.B	2%B	2$B	1B	1B	0B	0B	43B	:WB	F�B	K�B	P�B	O�B	M�B	K�B	I�B	F�B	E�B	E�B	C�B	C�B	A�B	>nB	AB	D�B	F�B	H�B	F�B	F�B	H�B	Q�B	P�B	J�B	E�B	L�B	XB	XB	YB	XB	YB	ZB	`:B	cLB	`9B	]$B	ZB	[B	^,B	^-B	a>B	g`B	f]B	gbB	hkB	imB	hhB	q�B	s�B	r�B	m�B	hkB	dNB	_1B	\ B	[B	YB	S�B	S�B	S�B	P�B	M�B	M�B	N�B	T�B	ZB	^*B	`<B	eVB	inB	n�B	n�B	p�B	p�B	p�B	m�B	l�B	n�B	o�B	p�B	q�B	s�B	w�B	x�B	x�B	z�B	{�B	~�B	��B	�B	�B	�B	�B	�B	�#B	�;B	�hB	�iB	�[B	�IB	�9B	�NB	�XB	�eB	�tB	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�,B	�-B	�1B	�9B	�=B	�?B	�>B	�GB	�KB	�SB	�QB	�dB	�oB	�|B	ÇB	đB	ŔB	ƜB	ǡB	ȦB	ɭB	ɭB	ɬB	ʴB	ʳB	˸B	̿B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�	B	�B	�	B	�B	�B	� B	�(B	�'B	�(B	�,B	�5B	�6B	�<B	�@B	�?B	�@B	�>B	�DB	�KB	�PB	�WB	�XB	�UB	�XB	�_B	�eB	�nB	�}B	�vB	�nB	�wB	�B	�B	�B	�B	�B	��B	��B	��B	��B
 �B
 �B	��B
�B
B
B
�B
B
B
B
B
B
�B	��B	��B	��B	��B
�B
B
B
�B
B
B
B
B
	B
B
B
B
B
B
B
!B

,B
	'B
	'B
$B
"B
	&B
	(B
	'B
	&B
	&B
	%B
	&B
	'B
	&B
	)B

.B
5G�O�B
FB
QB
|B
�B
 �B
&�B
,�B
4'B
8?B
=`B
C�B
I�B
P�B
V�B
\B
`1B
dHB
haB
ksB
p�B
t�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111 <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.17 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201608071436342016080714363420160807143634  AO  ARCAADJP                                                                    20150326185101    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20150326185101  QCP$                G�O�G�O�G�O�DFB5E           AO  ARGQQCPL                                                                    20150326185101  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160807143634  IP                  G�O�G�O�G�O�                