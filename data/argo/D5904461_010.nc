CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2015-02-26T22:13:29Z AOML 3.0 creation; 2016-08-07T21:36:28Z UW 3.1 conversion     
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20150226221329  20160807143629  5904461 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               
A   AO  5286_8897_010                   2C  D   APEX                            6531                            072314                          846 @�5�9�1   @�6�@
@1V�+J�c�I�^5?1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    
A   B   B   @�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�33B�33B�  B���B���B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$�C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR�CT�CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&y�D&��D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-fD-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DE��DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dyy�D��fD�C3D�vfD�ٚD�fD�6fD�|�D�� D�	�D�<�D�y�D��3D��fD�9�D�vfD��fD�  D�<�D�fD�� 11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @�p�@�p�A�RA"�RAB�RAb�RA�\)A�\)A�\)A�\)A�\)A�\)A�\)A�\)B �B�B�B�B �B(�B0�B8�B@�BH�BP�BX�B`�Bh�Bp�Bx�B�W
B�W
B�W
B��=B��=B�W
B�#�B�#�B�W
B�W
B�W
B�W
B�W
B�W
B��=B�W
B�W
B�W
B�W
B�#�B�W
B�W
B�W
B�W
B�W
B�=B�W
B�W
B�W
B�W
B�W
B�W
C +�C+�C+�C+�C+�C
+�C+�C+�C+�C+�C+�C+�C+�C+�C+�C+�C +�C"+�C$EC&+�C(+�C*+�C,+�C.+�C0+�C2+�C4+�C6+�C8+�C:+�C<+�C>+�C@+�CB+�CD+�CF+�CH+�CJ+�CL+�CN+�CP+�CRECTECV+�CX+�CZ+�C\+�C^+�C`+�Cb+�Cd+�Cf+�Ch+�Cj+�Cl+�Cn+�Cp+�Cr+�Ct+�Cv+�Cx+�Cz+�C|+�C~+�C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�"�C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�"�C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��D 
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
�D&�{D'{D'��D(
�D(��D)
�D)��D*
�D*��D+
�D+��D,
�D,��D-GD-��D.
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
�DE��DF{DF��DG
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
�Dt��Dy�{D���D�H�D�{�D��D��D�;�D��>D��qD�D�B>D�D�ؤD���D�?D�{�D���D�qD�B>D��D��q11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A���A��A��A��A��A��A��yA��A��A��mA��mA��mA��mA��mA��mA��`A��mA��mA��mA��mA��yA��yA��A��A��A��A��A��A��A��TA۰!A�^5AڬA�n�AԸRAӁA��A��A��A��A��AɼjAɸRA��A���A��TA�%A�\)AüjA\A���A�;dA��A���A���A�|�A��+A���A�n�A�ZA�33A�^5A��A��/A�v�A�G�A��/A���A�VA���A��uA�r�A�I�A�C�A�l�A� �A���A�K�A�%A���A�`BA�5?A��-A�M�A���A���A�&�A���A�hsA�ƨA�$�A��A�r�A���A�M�A��A��A�t�A`BA|��AwAs\)Amp�Ak��Aj�HAhn�Adv�Ab1'A_�#A[�AW�FAS�mAR{APĜAM��AJr�AFȴAA��A??}A<�9A<^5A;/A:v�A9�hA7��A5A4z�A3��A1��A0A�A/oA.-A-+A+&�A)&�A(��A(�/A'��A'K�A&��A%�^A%VA$ffA"��A!��A �A�mA%An�A��A1A�A��A��A�+A��A|�A(�AhsAdZA-A��Az�A�mA;dA��A$�A|�Av�A�uA��A�A�AO�AbNA�A|�A\)AoA
��A	�Az�A�mA�7AĜA��AhsA�`AĜA-A��A�A��A(�A��Al�AA
=AAA ��A �HA ��A  �@��;@��+@���@���@�1@���@�S�@��y@�G�@�dZ@�`B@�ƨ@�p�@�(�@��@�F@�D@��/@�9X@���@�/@�@��y@�h@�1'@��@�=q@�@�R@�t�@�\@��@��@���@��@�J@�C�@���@�b@���@߅@�o@�~�@�$�@���@ݑh@�G�@�j@��@�@�`B@�Ĝ@�~�@��`@с@�dZ@�?}@̼j@̣�@�I�@��@˕�@�S�@�;d@�@���@���@ʰ!@�n�@ɡ�@���@���@Ǖ�@��H@�ȴ@�ff@�$�@��@ř�@�?}@��`@�9X@��;@�ƨ@þw@�ƨ@���@öF@°!@�@�ff@�@���@��T@���@��@��9@��;@��@�t�@��@�ȴ@�$�@��-@�X@���@���@��u@�  @��
@��F@�S�@��R@��@��@�`B@�X@�O�@���@��@�(�@��;@�K�@��@�V@��h@�hs@�%@�j@�z�@�Z@�b@��@�t�@���@�^5@�$�@��^@�7L@���@�bN@�9X@���@��@�\)@�;d@�
=@��y@���@��T@���@���@�J@�V@�@��j@��@�z�@�I�@���@�;d@�@���@�ȴ@�=q@��@��^@�hs@�O�@�/@�Z@��P@���@�n�@�$�@��#@�V@��D@�z�@� �@��;@��P@��y@�~�@�@���@��7@�?}@�%@��j@���@��F@�S�@�ȴ@���@�V@��#@���@��7@�x�@��@��/@�Ĝ@��j@��@��@���@�Z@�(�@��w@���@�-@�5?@�-@��@�@�@��^@�x�@���@�Z@�9X@�(�@� �@�1@��@���@���@�t�@�S�@�"�@���@�ȴ@���@��\@�E�@��@���@���@�G�@�V@���@���@�A�@� �@���@�t�@�dZ@�\)@�
=@���@���@���@���@�~�@��@��^@�x�@�G�@�V@�Ĝ@��@�Z@�9X@��;@�C�@���@��@���@�V@�$�@��@�J@�@��@��-@�x�@�/@���@���@��@��@���@� �@��@���@{ƨ@t��@k��@cƨ@[o@R��@K�@A��@<z�@6�+@/
=@*�!@%�h@!&�@o@l�@@��@t�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   A���A��A��A��A��A��A��yA��A��A��mA��mA��mA��mA��mA��mA��`A��mA��mA��mA��mA��yA��yA��A��A��A��A��A��A��A��TA۰!A�^5AڬA�n�AԸRAӁA��A��A��A��A��AɼjAɸRA��A���A��TA�%A�\)AüjA\A���A�;dA��A���A���A�|�A��+A���A�n�A�ZA�33A�^5A��A��/A�v�A�G�A��/A���A�VA���A��uA�r�A�I�A�C�A�l�A� �A���A�K�A�%A���A�`BA�5?A��-A�M�A���A���A�&�A���A�hsA�ƨA�$�A��A�r�A���A�M�A��A��A�t�A`BA|��AwAs\)Amp�Ak��Aj�HAhn�Adv�Ab1'A_�#A[�AW�FAS�mAR{APĜAM��AJr�AFȴAA��A??}A<�9A<^5A;/A:v�A9�hA7��A5A4z�A3��A1��A0A�A/oA.-A-+A+&�A)&�A(��A(�/A'��A'K�A&��A%�^A%VA$ffA"��A!��A �A�mA%An�A��A1A�A��A��A�+A��A|�A(�AhsAdZA-A��Az�A�mA;dA��A$�A|�Av�A�uA��A�A�AO�AbNA�A|�A\)AoA
��A	�Az�A�mA�7AĜA��AhsA�`AĜA-A��A�A��A(�A��Al�AA
=AAA ��A �HA ��A  �@��;@��+@���@���@�1@���@�S�@��y@�G�@�dZ@�`B@�ƨ@�p�@�(�@��@�F@�D@��/@�9X@���@�/@�@��y@�h@�1'@��@�=q@�@�R@�t�@�\@��@��@���@��@�J@�C�@���@�b@���@߅@�o@�~�@�$�@���@ݑh@�G�@�j@��@�@�`B@�Ĝ@�~�@��`@с@�dZ@�?}@̼j@̣�@�I�@��@˕�@�S�@�;d@�@���@���@ʰ!@�n�@ɡ�@���@���@Ǖ�@��H@�ȴ@�ff@�$�@��@ř�@�?}@��`@�9X@��;@�ƨ@þw@�ƨ@���@öF@°!@�@�ff@�@���@��T@���@��@��9@��;@��@�t�@��@�ȴ@�$�@��-@�X@���@���@��u@�  @��
@��F@�S�@��R@��@��@�`B@�X@�O�@���@��@�(�@��;@�K�@��@�V@��h@�hs@�%@�j@�z�@�Z@�b@��@�t�@���@�^5@�$�@��^@�7L@���@�bN@�9X@���@��@�\)@�;d@�
=@��y@���@��T@���@���@�J@�V@�@��j@��@�z�@�I�@���@�;d@�@���@�ȴ@�=q@��@��^@�hs@�O�@�/@�Z@��P@���@�n�@�$�@��#@�V@��D@�z�@� �@��;@��P@��y@�~�@�@���@��7@�?}@�%@��j@���@��F@�S�@�ȴ@���@�V@��#@���@��7@�x�@��@��/@�Ĝ@��j@��@��@���@�Z@�(�@��w@���@�-@�5?@�-@��@�@�@��^@�x�@���@�Z@�9X@�(�@� �@�1@��@���@���@�t�@�S�@�"�@���@�ȴ@���@��\@�E�@��@���@���@�G�@�V@���@���@�A�@� �@���@�t�@�dZ@�\)@�
=@���@���@���@���@�~�@��@��^@�x�@�G�@�V@�Ĝ@��@�Z@�9X@��;@�C�@���@��@���@�V@�$�@��@�J@�@��@��-@�x�@�/@���@���@��@��@���G�O�@��@���@{ƨ@t��@k��@cƨ@[o@R��@K�@A��@<z�@6�+@/
=@*�!@%�h@!&�@o@l�@@��@t�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB6FB5?B5?B6FB6FB6FB6FB6FB6FB5?B5?B5?B5?B6FB6FB6FB6FB6FB6FB6FB7LB7LB6FB6FB7LB7LB7LB8RB;dBF�B[#BffBiyBo�B~�B�=B��B�3B�B�BB�B!�BF�B`BBk�BgmB\)BZBaHBiyBhsBhsBiyBk�Bx�B��B�?B��B��B^5BF�BD�B)�B�B�B�BuB��B�sB�B��B�B��B�{B�{B�Bk�BS�BS�BcTBhsB`BBF�B"�B�B
=B
��B
�B
�5B
�
B
��B
�qB
��B
�7B
_;B
2-B
B	�sB	�#B	ŢB	��B	�%B	gmB	ZB	S�B	D�B	0!B	"�B	{B	B�B�/B�B��BŢB�jB�'B�B��B�B�B�B�B�B�B�!B�'B�'B�3B�FB�3B�'B�'B�LB�jB�dB�^B�dB�dB�^B�RB�dB��B��B��B�}B�qBBƨB��B��B��B��B�B�B�#B�BB�B�B��B	
=B	hB	PB	�B	�B	 �B	"�B	#�B	!�B	#�B	 �B	�B	�B	�B	�B	�B	�B	�B	 �B	�B	�B	�B	�B	�B	#�B	&�B	%�B	'�B	1'B	5?B	7LB	9XB	;dB	=qB	A�B	@�B	C�B	K�B	N�B	O�B	O�B	O�B	O�B	O�B	Q�B	T�B	VB	XB	YB	ZB	ZB	YB	W
B	R�B	N�B	L�B	G�B	E�B	M�B	J�B	?}B	F�B	I�B	C�B	F�B	D�B	@�B	=qB	:^B	5?B	7LB	6FB	B�B	M�B	O�B	P�B	S�B	M�B	I�B	R�B	_;B	ffB	jB	jB	iyB	iyB	jB	k�B	k�B	jB	iyB	ffB	`BB	^5B	\)B	YB	R�B	L�B	C�B	;dB	:^B	<jB	=qB	>wB	@�B	B�B	D�B	F�B	G�B	I�B	J�B	J�B	L�B	M�B	N�B	O�B	O�B	P�B	P�B	Q�B	Q�B	R�B	S�B	S�B	T�B	W
B	XB	ZB	\)B	aHB	ffB	gmB	gmB	k�B	n�B	r�B	s�B	s�B	u�B	}�B	� B	�B	�%B	�+B	�+B	�1B	�DB	�JB	�PB	�bB	�oB	�{B	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�'B	�!B	�B	�'B	�3B	�3B	�9B	�FB	�LB	�XB	�dB	�dB	�^B	�RB	�RB	�RB	�dB	�jB	�qB	�qB	�wB	�wB	�wB	�}B	��B	ÖB	ĜB	ǮB	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�#B	�#B	�B	�B	�#B	�B	�B	�B	�#B	�#B	�)B	�)B	�)B	�)B	�)B	�)B	�/B	�/B	�5B	�BB	�BB	�HB	�NB	�NB	�NB	�NB	�NB	�TB	�TB	�`B	�mB	�sB	�sB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
	7B
	7B
	7B
VB
�B
�B
 �B
&�B
.B
2-B
8RB
>wB
E�B
J�B
O�B
VB
ZB
_;B
cTB
hsB
l�B
p�B
t�B
x�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   B66B53B52B67B66B6;B68B6:B6:B53B51B53B52B68B6;B6:B68B6;B6:B6;B7>B7AB6:B6:B7?B7?B7AB8FB;XBF�B[Bf[BimBo�B~�B�/B��B�%B�B�6B�|B!�BF�B`4BkyBg`B\BZBa9BikBheBhdBijBkyBx�B��B�4B��B��B^"BF�BD�B)�B�B�B�BeB��B�eB�|B˵B��B��B�lB�nB�
BkxBS�BS�BcDBheB`4BF�B"�B{B
-B
��B
�wB
�(B
��B
��B
�cB
��B
�)B
_.B
2#B
 �B	�lB	�B	řB	��B	�B	ghB	ZB	S�B	D�B	0B	"�B	yB	B�B�/B�B��BŤB�oB�*B�B��B�B�B�B�B�B�B�"B�)B�)B�3B�HB�2B�'B�(B�KB�kB�eB�^B�eB�dB�^B�QB�dB��B��B��B�zB�mBBƦB��B��B��B��B�B�B� B�>B�B�B��B	
8B	cB	MB	�B	�B	 �B	"�B	#�B	!�B	#�B	 �B	�B	�B	�B	�B	�B	�B	�B	 �B	�B	�B	�B	�B	�B	#�B	&�B	%�B	'�B	1B	58B	7DB	9PB	;_B	=jB	A�B	@{B	C�B	K�B	N�B	O�B	O�B	O�B	O�B	O�B	Q�B	T�B	U�B	XB	YB	ZB	ZB	YB	V�B	R�B	N�B	L�B	G�B	E�B	M�B	J�B	?vB	F�B	I�B	C�B	F�B	D�B	@{B	=gB	:VB	55B	7CB	6<B	B�B	M�B	O�B	P�B	S�B	M�B	I�B	R�B	_1B	f^B	jvB	jvB	ipB	ioB	jvB	k{B	k|B	jtB	ioB	f\B	`9B	^+B	\ B	YB	R�B	L�B	C�B	;[B	:TB	<_B	=iB	>nB	@{B	B�B	D�B	F�B	G�B	I�B	J�B	J�B	L�B	M�B	N�B	O�B	O�B	P�B	P�B	Q�B	Q�B	R�B	S�B	S�B	T�B	WB	XB	ZB	\ B	a@B	fZB	geB	gcB	k|B	n�B	r�B	s�B	s�B	u�B	}�B	�B	�B	�B	�!B	�B	�$B	�8B	�>B	�DB	�XB	�aB	�rB	�hB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�	B	�B	�B	�B	�B	�&B	�'B	�+B	�8B	�@B	�MB	�YB	�XB	�OB	�EB	�CB	�CB	�ZB	�^B	�eB	�eB	�iB	�jB	�jB	�pB	�}B	ÈB	ďB	ǡB	ʴB	˸B	˺B	̼B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�!B	�B	�'B	�2B	�5B	�9B	�@B	�?B	�@B	�@B	�@B	�EB	�GB	�RB	�`B	�fB	�eB	�cB	�iB	�xB	�nB	�uB	�wB	�uB	�vB	�uB	�wB	�wB	�zB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
 �B
 �B
 �B
 �B
�B
B
 B
B
B
B
B
	B

B
B
B
B
	(B
	&G�O�B
HB
vB
�B
 �B
&�B
.B
2B
8CB
>fB
E�B
J�B
O�B
U�B
ZB
_(B
c@B
h`B
lzB
p�B
t�B
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.17 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201608071436292016080714362920160807143629  AO  ARCAADJP                                                                    20150226221329    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20150226221329  QCP$                G�O�G�O�G�O�DFB5E           AO  ARGQQCPL                                                                    20150226221329  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160807143629  IP                  G�O�G�O�G�O�                