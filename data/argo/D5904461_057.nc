CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2015-06-29T19:17:29Z AOML 3.0 creation; 2016-08-07T21:36:36Z UW 3.1 conversion     
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20150629191729  20160807143636  5904461 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               9A   AO  5286_8897_057                   2C  D   APEX                            6531                            072314                          846 @�\-�p��1   @�\.O���@1�p��
=�cdz�G�1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    9A   B   B   @�33@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B ffB(ffB/33B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`�Cb�Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'�fD(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dtl�Dy` D��D�I�D��3D�� D�3D�FfD�p D��3D�	�D�L�D��3D�� D�	�D�FfDڌ�D�� D� D�C3D� D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @���@�p�A�RA"�RAB�RAb�RA�\)A�\)A�\)A�\)A�\)A�\)A�\)A�\)B �B�B�B�B!zB)zB/�GB8�B@�BH�BP�BX�B`�Bh�Bp�Bx�B�W
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
B�W
B�W
B�W
B�W
B�W
B�W
B�W
C +�C+�C+�C+�C+�C
+�C+�C+�C+�C+�C+�C+�C+�C+�C+�C+�C +�C"+�C$+�C&+�C(+�C*+�C,+�C.+�C0+�C2+�C4+�C6+�C8+�C:+�C<+�C>+�C@+�CB+�CD+�CF+�CH+�CJ+�CL+�CN+�CP+�CR+�CT+�CV+�CX+�CZ+�C\+�C^+�C`ECbECd+�Cf+�Ch+�Cj+�Cl+�Cn+�Cp+�Cr+�Ct+�Cv+�Cx+�Cz+�C|+�C~+�C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��D 
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
�D'�GD(
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
�Dtw�Dyj�D�>D�OD���D��qD��D�K�D�uqD�ؤD�D�R>D���D��qD�D�K�Dڒ>D��qD�qD�H�D�qD��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��Aӗ�Aӕ�Aӗ�Aӕ�Aӕ�Aӕ�Aӗ�Aӗ�Aӗ�Aӗ�Aӗ�Aӛ�Aӟ�Aә�AӇ+A�r�A�S�A�$�A��A�VA���A�/A͏\A�33A�p�A�/A�hsA� �A�ȴA�z�A�oAȼjAȍPA�VA��AǑhA�A�A��AƇ+AŴ9A�5?A�  Aĉ7AÏ\A\A�S�A�`BA�I�A���A��yA�G�A��A��A�A���A�;dA�|�A��A�`BA��;A��A���A�1'A��;A�n�A�  A���A�%A�1A�  A��;A��A��+A�;dA�v�A��PA�;dA��
A��A��`A���A��A�9XA�S�A���A�C�A��`A�oA�&�A��A��wA�p�A�G�A���A���A�O�A���A��A�hsA�1A���A�C�A���A�A�A�A�`BA�~�A�1'A}��A{
=Aw�7At�DAs�
Ao��Ak33Aj�DAjZAi�Ag�FAeoAc�A`��A_XA\�AXv�AU��AS�
AP��AN=qAM�-AL�`AJ��AH��AF��AEx�ADQ�A?+A<�A<�!A<��A;?}A7
=A3/A1�mA3�^A5�A5
=A0bA,�9A+��A+�7A+33A*�yA)��A'�^A&�9A%C�A#��A"�HA"ĜA"��A"{A ~�AƨA\)A%AhsA��A�-A\)A��A5?A��A(�A�A��AK�A�HA�wA��A�#A
=A%AA5?A"�A�A�A�A%A
1A(�AZA�9A�`A��A�
A�hA��A�A��A��AM�A�
A �j@��P@�{@���@�b@���@�V@�S�@��@�P@�1'@��@�@�p�@�hs@�/@�z�@��@�ff@�&�@�O�@�@� �@�P@�ȴ@�M�@�!@�"�@��y@�5?@�?}@�^5@�7@�K�@��@�7L@�=q@���@���@旍@��
@�dZ@�A�@���@�h@�S�@�V@އ+@�ƨ@�Z@���@�|�@�+@ާ�@ް!@ް!@�v�@�$�@݉7@�z�@��;@�t�@��@���@ڧ�@�^5@�-@�G�@׾w@��H@ָR@֗�@�v�@�E�@��T@թ�@�/@�1'@�1@�z�@�Q�@�1@ӶF@�K�@�33@�~�@љ�@��m@ϝ�@��H@·+@��#@�&�@���@�z�@���@��
@ˮ@�33@��H@ʏ\@�M�@��@�7L@ȼj@�r�@�I�@�b@ǶF@�t�@�;d@�o@��@Ɨ�@�n�@��@Ł@��/@ēu@�r�@�(�@���@���@î@�S�@��H@�V@��@��-@�p�@��@�z�@�dZ@��!@�M�@��#@��@�?}@��@��@���@�l�@�;d@�
=@��R@�v�@�E�@��@���@���@��D@�Q�@���@���@�t�@�l�@�K�@�33@�@��y@���@�=q@���@�x�@�`B@�hs@�hs@�hs@�X@�7L@��`@��@�z�@�Z@�1@��
@��
@��F@�o@��R@�5?@�@��7@��@�  @��@��@��!@�^5@�-@���@�p�@�?}@�/@���@�Z@��@���@�K�@�
=@���@�v�@�5?@�$�@���@���@�x�@�/@�%@��@��j@��D@��@�\)@��@�~�@�5?@���@��@���@���@�Q�@��F@�
=@���@�ȴ@�v�@��@���@�x�@��@�(�@��F@�t�@�\)@�+@�ȴ@�v�@�5?@�J@�@�x�@�?}@�%@��/@��j@�j@�A�@�  @��w@�l�@�C�@�
=@���@��+@�$�@��7@��j@�Z@�A�@��;@�;d@���@�~�@�n�@�E�@�J@���@�x�@�`B@�X@�G�@�?}@�?}@�7L@���@�bN@���@�\)@�
=@���@��+@�ff@�5?@���@��@�n�@��`@vE�@kC�@b�\@Z��@P�`@I��@A&�@:n�@0  @)�@$�D@!&�@/@�^@�@�@��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   Aӗ�Aӕ�Aӗ�Aӕ�Aӕ�Aӕ�Aӗ�Aӗ�Aӗ�Aӗ�Aӗ�Aӛ�Aӟ�Aә�AӇ+A�r�A�S�A�$�A��A�VA���A�/A͏\A�33A�p�A�/A�hsA� �A�ȴA�z�A�oAȼjAȍPA�VA��AǑhA�A�A��AƇ+AŴ9A�5?A�  Aĉ7AÏ\A\A�S�A�`BA�I�A���A��yA�G�A��A��A�A���A�;dA�|�A��A�`BA��;A��A���A�1'A��;A�n�A�  A���A�%A�1A�  A��;A��A��+A�;dA�v�A��PA�;dA��
A��A��`A���A��A�9XA�S�A���A�C�A��`A�oA�&�A��A��wA�p�A�G�A���A���A�O�A���A��A�hsA�1A���A�C�A���A�A�A�A�`BA�~�A�1'A}��A{
=Aw�7At�DAs�
Ao��Ak33Aj�DAjZAi�Ag�FAeoAc�A`��A_XA\�AXv�AU��AS�
AP��AN=qAM�-AL�`AJ��AH��AF��AEx�ADQ�A?+A<�A<�!A<��A;?}A7
=A3/A1�mA3�^A5�A5
=A0bA,�9A+��A+�7A+33A*�yA)��A'�^A&�9A%C�A#��A"�HA"ĜA"��A"{A ~�AƨA\)A%AhsA��A�-A\)A��A5?A��A(�A�A��AK�A�HA�wA��A�#A
=A%AA5?A"�A�A�A�A%A
1A(�AZA�9A�`A��A�
A�hA��A�A��A��AM�A�
A �j@��P@�{@���@�b@���@�V@�S�@��@�P@�1'@��@�@�p�@�hs@�/@�z�@��@�ff@�&�@�O�@�@� �@�P@�ȴ@�M�@�!@�"�@��y@�5?@�?}@�^5@�7@�K�@��@�7L@�=q@���@���@旍@��
@�dZ@�A�@���@�h@�S�@�V@އ+@�ƨ@�Z@���@�|�@�+@ާ�@ް!@ް!@�v�@�$�@݉7@�z�@��;@�t�@��@���@ڧ�@�^5@�-@�G�@׾w@��H@ָR@֗�@�v�@�E�@��T@թ�@�/@�1'@�1@�z�@�Q�@�1@ӶF@�K�@�33@�~�@љ�@��m@ϝ�@��H@·+@��#@�&�@���@�z�@���@��
@ˮ@�33@��H@ʏ\@�M�@��@�7L@ȼj@�r�@�I�@�b@ǶF@�t�@�;d@�o@��@Ɨ�@�n�@��@Ł@��/@ēu@�r�@�(�@���@���@î@�S�@��H@�V@��@��-@�p�@��@�z�@�dZ@��!@�M�@��#@��@�?}@��@��@���@�l�@�;d@�
=@��R@�v�@�E�@��@���@���@��D@�Q�@���@���@�t�@�l�@�K�@�33@�@��y@���@�=q@���@�x�@�`B@�hs@�hs@�hs@�X@�7L@��`@��@�z�@�Z@�1@��
@��
@��F@�o@��R@�5?@�@��7@��@�  @��@��@��!@�^5@�-@���@�p�@�?}@�/@���@�Z@��@���@�K�@�
=@���@�v�@�5?@�$�@���@���@�x�@�/@�%@��@��j@��D@��@�\)@��@�~�@�5?@���@��@���@���@�Q�@��F@�
=@���@�ȴ@�v�@��@���@�x�@��@�(�@��F@�t�@�\)@�+@�ȴ@�v�@�5?@�J@�@�x�@�?}@�%@��/@��j@�j@�A�@�  @��w@�l�@�C�@�
=@���@��+@�$�@��7@��j@�Z@�A�@��;@�;d@���@�~�@�n�@�E�@�J@���@�x�@�`B@�X@�G�@�?}@�?}@�7L@���@�bN@���@�\)@�
=@���@��+@�ffG�O�@���@��@�n�@��`@vE�@kC�@b�\@Z��@P�`@I��@A&�@:n�@0  @)�@$�D@!&�@/@�^@�@�@��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB
o�B
o�B
o�B
o�B
o�B
o�B
o�B
o�B
o�B
o�B
o�B
p�B
q�B
q�B
q�B
r�B
t�B
y�B
�B
��B
��B+B,B:^B^5B��BĜB��B�B��BBbB�B�B+B49B=qB@�BI�BW
B`BBbNBhsBx�B�JB�\B��B�!B�LB��B�fB�B��BB+BDBuB�B�B �B#�B.B9XB;dB6FB0!B%�BDB!�B(�B5?B6FB)�BbB��B�sB�BB�`B��B�B�B��Bx�BaHBB�B-B�B1B��B�B��B�LB��B`BBG�B�B1B
��B
�B
ȴB
��B
n�B
H�B
(�B
$�B
8RB
�B	��B	�B	�#B	ÖB	�'B	��B	�=B	s�B	p�B	n�B	iyB	^5B	Q�B	I�B	8RB	.B	�B	DB	  B��B�B�`B�NB�5B�B��B��B��B��B�)B�;B��B	�B	�B	
=B�B��B	'�B	L�B	K�B	7LB	)�B	$�B	 �B	�B	�B	oB	oB	�B	�B	�B	{B	�B	�B	"�B	1'B	9XB	9XB	6FB	1'B	0!B	0!B	/B	33B	:^B	7LB	9XB	8RB	=qB	?}B	<jB	33B	$�B	�B	�B	�B	!�B	,B	-B	2-B	2-B	+B	%�B	 �B	�B	%�B	8RB	/B	"�B	!�B	!�B	(�B	.B	1'B	D�B	E�B	E�B	C�B	?}B	=qB	6FB	,B	(�B	!�B	�B	 �B	.B	6FB	A�B	O�B	P�B	Q�B	Q�B	P�B	L�B	=qB	:^B	I�B	P�B	N�B	R�B	YB	]/B	aHB	gmB	k�B	iyB	dZB	H�B	F�B	XB	cTB	o�B	w�B	}�B	}�B	z�B	p�B	p�B	x�B	�%B	{�B	s�B	p�B	t�B	�B	�=B	�VB	�VB	�\B	�hB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�3B	�-B	�B	�!B	�-B	�-B	�FB	�FB	�FB	�FB	�9B	�9B	�?B	�^B	�dB	�wB	�qB	�qB	�qB	�qB	�qB	�^B	�XB	�XB	�dB	�jB	�dB	�dB	�dB	�jB	�jB	�qB	�}B	�}B	��B	��B	��B	B	ÖB	ĜB	ĜB	ŢB	ǮB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�
B	�
B	�B	�B	�B	�B	�B	�B	�)B	�)B	�)B	�)B	�)B	�)B	�#B	�#B	�#B	�#B	�#B	�)B	�/B	�;B	�;B	�5B	�/B	�/B	�5B	�5B	�5B	�;B	�BB	�HB	�NB	�NB	�TB	�TB	�TB	�TB	�TB	�TB	�TB	�TB	�TB	�TB	�TB	�TB	�ZB	�ZB	�ZB	�ZB	�`B	�ZB	�`B	�ZB	�ZB	�ZB	�TB	�TB	�TB	�NB	�TB	�ZB	�ZB	�ZB	�ZB	�ZB	�`B	�`B	�`B	�`B	�ZB	�`B	�`B	�fB	�mB	�mB	�mB	�sB	�sB	�sB	�sB	�sB	�yB	�yB	�yB	�B	�B	�B	�B	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
B
B
B
B
B
B
B
B
B
B
  B	��B
  B
B
B
B
B
B
B
	7B
{B
�B
$�B
'�B
.B
33B
9XB
@�B
E�B
I�B
O�B
XB
\)B
_;B
dZB
gmB
k�B
p�B
t�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   B
o�B
o�B
o�B
o�B
o�B
o�B
o�B
o�B
o�B
o�B
o�B
p�B
q�B
q�B
q�B
r�B
t�B
y�B
�B
�~B
��BB+�B:OB^)B��BēB��B�pB��BBTBtB�B*�B4+B=eB@rBI�BV�B`4Bb@BhgBx�B�9B�LB��B�B�AB˻B�[B�B��BBB6BhB�B�B �B#�B.B9IB;XB6:B0B%�B8B!�B(�B53B6=B)�BVB��B�gB�1B�UB��B�B��B��Bx�Ba:BB�B,�B|BB��B�sB��B�:B�pB`3BG�ByB$B
��B
�uB
ȥB
��B
n�B
H�B
(�B
$�B
8CB
�B	��B	�B	�B	ÎB	�B	��B	�8B	s�B	p�B	n�B	irB	^2B	Q�B	I�B	8PB	.B	�B	CB��B��B�B�`B�QB�8B�B��B��B��B��B�(B�:B��B	B	�B	
9B�B��B	'�B	L�B	K�B	7GB	)�B	$�B	 �B	�B	�B	kB	kB	�B	�B	�B	uB	�B	�B	"�B	1"B	9PB	9QB	6>B	1 B	0B	0B	/B	3,B	:XB	7EB	9SB	8LB	=iB	?wB	<cB	3.B	$�B	�B	�B	�B	!�B	,B	-B	2'B	2%B	*�B	%�B	 �B	�B	%�B	8KB	/B	"�B	!�B	!�B	(�B	.B	1B	D�B	E�B	E�B	C�B	?vB	=iB	6<B	,B	(�B	!�B	�B	 �B	.B	6>B	A�B	O�B	P�B	Q�B	Q�B	P�B	L�B	=kB	:YB	I�B	P�B	N�B	R�B	YB	]$B	a?B	gcB	k|B	ioB	dPB	H�B	F�B	X	B	cJB	o�B	w�B	}�B	}�B	z�B	p�B	p�B	x�B	�B	{�B	s�B	p�B	t�B	�B	�3B	�IB	�KB	�PB	�\B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�(B	� B	�B	�B	�"B	� B	�7B	�9B	�7B	�7B	�-B	�,B	�1B	�QB	�YB	�jB	�eB	�gB	�cB	�dB	�aB	�QB	�LB	�MB	�XB	�]B	�YB	�ZB	�YB	�\B	�]B	�dB	�nB	�pB	�vB	�yB	�yB	B	ÈB	ĎB	ĐB	ŔB	ǟB	ȦB	ɮB	ʵB	ʶB	ʶB	˹B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	� B	�B	�B	�	B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�"B	�.B	�/B	�'B	� B	� B	�'B	�'B	�(B	�-B	�3B	�8B	�?B	�<B	�DB	�HB	�FB	�EB	�EB	�FB	�EB	�GB	�HB	�GB	�EB	�EB	�LB	�KB	�LB	�LB	�TB	�MB	�SB	�MB	�LB	�LB	�FB	�FB	�EB	�BB	�FB	�KB	�LB	�JB	�LB	�LB	�QB	�QB	�RB	�OB	�LB	�OB	�PB	�WB	�]B	�]B	�^B	�fB	�eB	�eB	�cB	�gB	�iB	�jB	�mB	�nB	�oB	�pB	�rB	�jB	�qB	�vB	�|B	�xB	�~B	�}B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
 �B
�B
�B
B
B
B
B
B
B
B
�B
 �B	��B	��B	��B
 �B
 �B
 �B
 �G�O�B
B
	'B
mB
�B
$�B
'�B
.B
3%B
9HB
@qB
E�B
I�B
O�B
W�B
\B
_)B
dHB
g\B
ktB
p�B
t�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.17 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201608071436362016080714363620160807143636  AO  ARCAADJP                                                                    20150629191729    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20150629191729  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20150629191729  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160807143636  IP                  G�O�G�O�G�O�                