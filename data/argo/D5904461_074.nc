CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2015-09-27T19:15:42Z AOML 3.0 creation; 2016-08-07T21:36:39Z UW 3.1 conversion     
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20150927191542  20160807143639  5904461 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               JA   AO  5286_8897_074                   2C  D   APEX                            6531                            072314                          846 @�r����+1   @�r�F)��@3
~��"��c?Ƨ1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    JA   B   B   @�33@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0ffB8  B?��BH  BP  BX  B`  BhffBo��Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct�Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D�fDfD� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DFy�DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  DtffDys3D��D�FfD��fD��3D� D�<�D�y�D��3D�	�D�I�D�� D��fD���D�I�Dڌ�D�3D� D�C3D��D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @���@�p�A�RA"�RAB�RAb�RA�\)A�\)A�\)A�\)A�\)A�\)A�\)A�\)B �B�B�B�B �B(�B1zB8�B@G�BH�BP�BX�B`�BizBpG�Bx�B�W
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
B�#�B�W
C +�C+�C+�C+�C+�C
+�C+�C+�C+�C+�C+�C+�C+�C+�C+�C+�C +�C"+�C$+�C&+�C(+�C*+�C,+�C.+�C0+�C2+�C4+�C6+�C8+�C:+�C<+�C>+�C@+�CB+�CD+�CF+�CH+�CJ+�CL+�CN+�CP+�CR+�CT+�CV+�CX+�CZ+�C\+�C^+�C`+�Cb+�Cd+�Cf+�Ch+�Cj+�Cl+�Cn+�Cp+�Cr+�CtECv+�Cx+�Cz+�C|+�C~+�C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�"�C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��D 
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
�D�GDGD��D
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
�DF�{DG
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
�DtqGDy~D�D�K�D���D�ؤD�qD�B>D�D�ؤD�D�OD��qD���D�>D�ODڒ>DฤD�qD�H�D�>D��>11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A���A���A���A���A��#A��TA��TA��TA��TA��HA��;A��;A��HA��TA��HA��TA��`A��mA��`A��TA��`A��`A���A��A��Aܧ�A�5?A�(�A���A�-A�l�A�bA�1'A�r�A̴9AˑhA�bA�p�AȅA���A��A���A�x�Aŕ�Aĝ�A���A�t�A�?}A� �A�jA��7A�=qA�XA�33A�/A��A��A��`A�M�A���A�/A�hsA��jA�1'A��A�Q�A�z�A��A�n�A��A�$�A��A���A���A�VA���A�VA�z�A�
=A�{A�M�A��A���A�"�A�9XA���A�v�A��A�ȴA�VA��mA�7LA���A���A�~�A��+A�ffA��TA��jA��/A���A�O�A���A�~�A�ffA�$�A�9XA��^A���A�VA���A�hsA��A��RA�;dA��A~�+A|E�A{�Aw�
AtM�AqS�An�/AlbNAf�Ac�PAa��A_%A[�hAX�/AW�^AW7LAVI�ASƨAR�uAQp�AQoAP�+AOC�AJ �AGt�ADQ�AB�AA�#A@~�A?oA=dZA;�^A9��A8$�A6 �A4  A3��A1A0��A/x�A.jA-�A+��A*�A(ffA'|�A'&�A&JA%�A%�;A%��A%��A%�wA%hsA%G�A%/A$�!A$�A$  A#A!;dA��A�
A��AXA%A��A��A�AM�A�wA1'A�RA��A��AbNA�A1A|�AĜA5?At�A~�A&�A�A&�A
ZA	G�A	�A	"�A	AjAAbAoA�yA��A�#A�PAp�A�A�A��A�A��A"�A j@�+@�r�@���@�I�@�x�@�l�@���@�5?@��`@� �@�l�@�
=@�1'@��y@��/@��/@��@�Z@�^5@���@��-@��@� �@�Ĝ@�D@�ȴ@��;@���@�@�D@��@�M�@���@�Ĝ@�w@�"�@�p�@�ƨ@ܛ�@�C�@�+@��H@��@�O�@��@�A�@�b@��@ՙ�@���@�@�^5@�1'@Ϯ@�C�@���@Χ�@�M�@���@��@ɲ-@�p�@�?}@�X@�Z@ǝ�@�K�@�+@�ƨ@ǶF@��y@�ff@ũ�@��@��@�{@Ȭ@��#@�1@��y@�n�@���@��@�  @�t�@���@�^5@���@�  @��D@��j@�j@�1@��F@�ƨ@���@��#@�X@��@�/@���@��
@�\)@�
=@��@���@�~�@�n�@�ff@�E�@�@��@��@���@��@�(�@��w@�;d@���@���@��F@��@�j@�j@���@�
=@�+@��@��H@�~�@�p�@�V@��D@���@��@�1@��@�n�@�5?@��D@��@�O�@��@��#@���@���@���@��@���@��@��m@�Z@��@���@�5?@�$�@��@�=q@�ff@��!@�n�@��!@�V@�M�@�&�@�r�@��P@�V@���@��7@��`@��u@��/@��@�b@�1@�  @��w@��@���@�O�@��@��`@���@�V@��`@���@�1@�|�@�C�@�@���@��@�o@��@�\)@��H@�M�@�@���@��7@���@�9X@� �@��w@�l�@��H@��@���@��H@��@��@���@���@���@��h@��@��/@��@�1@��w@�j@�z�@���@�K�@�
=@�;d@�o@��y@��\@�$�@��#@�p�@�?}@�7L@�%@��@�bN@� �@�(�@�1'@�I�@�A�@��m@�ƨ@���@�\)@�\)@��@��P@���@���@�t�@��R@�^5@�5?@�5?@�5?@�=q@�E�@�E�@���@�hs@��u@�l�@�I�@��j@~v�@vE�@ol�@eO�@]@X �@O+@E@<��@7�@2�!@,Z@(1'@$I�@�@��@X@5?@
J11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   A���A���A���A���A��#A��TA��TA��TA��TA��HA��;A��;A��HA��TA��HA��TA��`A��mA��`A��TA��`A��`A���A��A��Aܧ�A�5?A�(�A���A�-A�l�A�bA�1'A�r�A̴9AˑhA�bA�p�AȅA���A��A���A�x�Aŕ�Aĝ�A���A�t�A�?}A� �A�jA��7A�=qA�XA�33A�/A��A��A��`A�M�A���A�/A�hsA��jA�1'A��A�Q�A�z�A��A�n�A��A�$�A��A���A���A�VA���A�VA�z�A�
=A�{A�M�A��A���A�"�A�9XA���A�v�A��A�ȴA�VA��mA�7LA���A���A�~�A��+A�ffA��TA��jA��/A���A�O�A���A�~�A�ffA�$�A�9XA��^A���A�VA���A�hsA��A��RA�;dA��A~�+A|E�A{�Aw�
AtM�AqS�An�/AlbNAf�Ac�PAa��A_%A[�hAX�/AW�^AW7LAVI�ASƨAR�uAQp�AQoAP�+AOC�AJ �AGt�ADQ�AB�AA�#A@~�A?oA=dZA;�^A9��A8$�A6 �A4  A3��A1A0��A/x�A.jA-�A+��A*�A(ffA'|�A'&�A&JA%�A%�;A%��A%��A%�wA%hsA%G�A%/A$�!A$�A$  A#A!;dA��A�
A��AXA%A��A��A�AM�A�wA1'A�RA��A��AbNA�A1A|�AĜA5?At�A~�A&�A�A&�A
ZA	G�A	�A	"�A	AjAAbAoA�yA��A�#A�PAp�A�A�A��A�A��A"�A j@�+@�r�@���@�I�@�x�@�l�@���@�5?@��`@� �@�l�@�
=@�1'@��y@��/@��/@��@�Z@�^5@���@��-@��@� �@�Ĝ@�D@�ȴ@��;@���@�@�D@��@�M�@���@�Ĝ@�w@�"�@�p�@�ƨ@ܛ�@�C�@�+@��H@��@�O�@��@�A�@�b@��@ՙ�@���@�@�^5@�1'@Ϯ@�C�@���@Χ�@�M�@���@��@ɲ-@�p�@�?}@�X@�Z@ǝ�@�K�@�+@�ƨ@ǶF@��y@�ff@ũ�@��@��@�{@Ȭ@��#@�1@��y@�n�@���@��@�  @�t�@���@�^5@���@�  @��D@��j@�j@�1@��F@�ƨ@���@��#@�X@��@�/@���@��
@�\)@�
=@��@���@�~�@�n�@�ff@�E�@�@��@��@���@��@�(�@��w@�;d@���@���@��F@��@�j@�j@���@�
=@�+@��@��H@�~�@�p�@�V@��D@���@��@�1@��@�n�@�5?@��D@��@�O�@��@��#@���@���@���@��@���@��@��m@�Z@��@���@�5?@�$�@��@�=q@�ff@��!@�n�@��!@�V@�M�@�&�@�r�@��P@�V@���@��7@��`@��u@��/@��@�b@�1@�  @��w@��@���@�O�@��@��`@���@�V@��`@���@�1@�|�@�C�@�@���@��@�o@��@�\)@��H@�M�@�@���@��7@���@�9X@� �@��w@�l�@��H@��@���@��H@��@��@���@���@���@��h@��@��/@��@�1@��w@�j@�z�@���@�K�@�
=@�;d@�o@��y@��\@�$�@��#@�p�@�?}@�7L@�%@��@�bN@� �@�(�@�1'@�I�@�A�@��m@�ƨ@���@�\)@�\)@��@��P@���@���@�t�@��R@�^5@�5?@�5?@�5?@�=q@�E�@�E�@���@�hs@��uG�O�@�I�@��j@~v�@vE�@ol�@eO�@]@X �@O+@E@<��@7�@2�!@,Z@(1'@$I�@�@��@X@5?@
J11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB
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
��B
��B
�BB�BH�B}�B��B�RB�B�B�B��B1B��B��B%B0!B33BA�BH�BP�Be`Bx�B|�B�+B��B�B��B��B�/B��B�`B�ZB�B�fB�B�B�B��B��BBB	7B
=B+BB  B��B��B��B��B��B��B�B�B�HB��BŢB��B�LB�!B��B�bB�B�VB�%Bq�BffBgmB_;BO�B+B��B��B�jB�B��B�Bp�BVB8RB!�BPB
��B
�HB
B
�B
��B
��B
�B
bNB
R�B
<jB
'�B
�B
	7B	��B	�ZB	ȴB	�'B	��B	�=B	iyB	Q�B	D�B	0!B	�B	JB	B	B��B�B�sB�ZB�HB�)B��B�^B�B��B��B��B��B��B��B�bB�=B�B{�B� B~�B�B�%B�JB�oB��B�DB�1By�Bu�Bw�B�B�B�%B�1B�=B�DB�oB��B��B��B��B�B��B��BȴBɺB��B��B��B��BB�^B�?B�-B�B��B��B��B��B��B�B�B�B��B�!B�FB�?B�9B�-B�-B�B�B�9B��BȴB�qB�RBɺB��B��B��B��B��BƨB��B�B�)B��B��B��B��B��B�FB��B��B�^BƨB�}B�qBÖB��B	�B	DB	%B	�B	9XB	?}B	>wB	9XB	8RB	8RB	;dB	<jB	J�B	N�B	J�B	?}B	9XB	@�B	?}B	7LB	:^B	6FB	1'B	2-B	1'B	,B	+B	!�B	$�B	+B	+B	,B	)�B	)�B	+B	.B	1'B	/B	-B	0!B	1'B	1'B	1'B	2-B	49B	5?B	5?B	6FB	6FB	49B	5?B	7LB	;dB	:^B	:^B	;dB	?}B	D�B	I�B	H�B	G�B	E�B	F�B	I�B	S�B	ffB	r�B	k�B	hsB	hsB	cTB	bNB	e`B	dZB	cTB	dZB	cTB	e`B	jB	m�B	m�B	m�B	n�B	r�B	r�B	q�B	r�B	r�B	t�B	u�B	v�B	v�B	w�B	x�B	y�B	y�B	y�B	z�B	z�B	{�B	� B	�B	�B	�B	�B	�B	�%B	�%B	�B	�B	�B	�7B	�JB	�DB	�\B	�hB	�hB	�oB	�hB	�\B	�VB	�VB	�PB	�{B	��B	��B	��B	��B	�B	�9B	�9B	�LB	�jB	�jB	�jB	�qB	�qB	�qB	��B	ĜB	ȴB	ɺB	ǮB	ƨB	ƨB	ǮB	ɺB	��B	��B	��B	�B	�B	�#B	�B	�B	�B	�B	��B	��B	��B	��B	�B	�
B	�B	�B	�)B	�#B	�#B	�B	�B	�B	�)B	�;B	�ZB	�sB	�mB	�`B	�ZB	�TB	�TB	�ZB	�fB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�yB	�TB	�;B	�5B	�5B	�5B	�BB	�HB	�ZB	�ZB	�NB	�;B	�BB	�NB	�TB	�NB	�TB	�ZB	�ZB	�ZB	�ZB	�`B	�fB	�mB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
1B
hB
�B
 �B
)�B
8RB
8RB
>wB
F�B
K�B
N�B
T�B
[#B
_;B
e`B
k�B
p�B
u�B
y�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   B
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
��B
��B
�~BB�BH�B}�B��B�FB��B�yB�B��B%B��B��BB0B3"BAzBH�BP�BeNBx�B|�B�B��B�B��B�{B�B��B�TB�KB�B�YB�rB�B�B��B��BBB	)B
.BBB��B��B��B��B��B��B��B�B�B�=B��BœB�wB�?B�B��B�TB�B�HB�Bq�BfVBg_B_-BO�B*�B��B��B�YB�B�zB�Bp�BU�B8CB!�BDB
��B
�>B
�B
��B
��B
��B
�B
bDB
R�B
<]B
'�B
�B
	.B	��B	�PB	ȫB	�B	��B	�8B	iuB	Q�B	D�B	0!B	�B	FB	B	B��B�B�tB�[B�IB�+B��B�`B�B��B��B��B��B��B��B�gB�>B�B{�B�B~�B�B�&B�MB�qB��B�HB�3By�Bu�Bw�B�!B�"B�%B�4B�@B�HB�qB��B��B��B��B�B��B��BȲBɹB��B��B��B��BB�_B�@B�.B�B��B��B��B��B��B�	B�B� B��B� B�EB�@B�:B�)B�-B�B�B�9B�BȲB�lB�RBɸB��B��B��B��B��BƤB��B�B�&B��B��B��B��B��B�DB��B��B�]BƣB�zB�jBÔB��B	�B	>B	 B	~B	9OB	?vB	>nB	9NB	8LB	8MB	;[B	<dB	J�B	N�B	J�B	?uB	9OB	@}B	?uB	7EB	:WB	6=B	1 B	2&B	1B	, B	*�B	!�B	$�B	*�B	*�B	,B	)�B	)�B	*�B	.B	1 B	/B	-B	0B	1 B	1B	1B	2%B	41B	56B	57B	6>B	6;B	4/B	56B	7DB	;[B	:VB	:UB	;[B	?uB	D�B	I�B	H�B	G�B	E�B	F�B	I�B	S�B	f\B	r�B	k{B	hhB	hhB	cKB	bDB	eWB	dPB	cJB	dPB	cKB	eUB	jtB	m�B	m�B	m�B	n�B	r�B	r�B	q�B	r�B	r�B	t�B	u�B	v�B	v�B	w�B	x�B	y�B	y�B	y�B	z�B	z�B	{�B	�B	��B	��B	�B	�B	�B	�B	�B	�B	��B	�B	�*B	�>B	�9B	�QB	�ZB	�[B	�cB	�[B	�PB	�JB	�JB	�BB	�pB	��B	��B	��B	��B	��B	�+B	�,B	�@B	�\B	�^B	�^B	�cB	�eB	�dB	�vB	ďB	ȤB	ɪB	ǠB	ƜB	ƜB	ǡB	ɬB	̾B	��B	��B	��B	� B	�B	�B	�B	�	B	��B	��B	��B	��B	��B	��B	��B	� B	�
B	�B	�B	�B	�B	�B	�B	�B	�+B	�KB	�dB	�[B	�SB	�KB	�EB	�CB	�LB	�VB	�iB	�B	�B	�B	�B	�B	�B	�}B	�uB	�pB	�nB	�lB	�lB	�qB	�zB	�zB	�B	�B	�B	�B	�jB	�DB	�/B	�(B	�)B	�(B	�5B	�;B	�JB	�JB	�>B	�,B	�3B	�>B	�CB	�>B	�EB	�JB	�KB	�JB	�JB	�OB	�XB	�_B	�fB	�iB	�vB	�|B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��G�O�B	��B	��B
"B
XB
�B
 �B
)�B
8AB
8AB
>gB
F�B
K�B
N�B
T�B
[B
_(B
eOB
ksB
p�B
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.17 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201608071436392016080714363920160807143639  AO  ARCAADJP                                                                    20150927191542    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20150927191542  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20150927191542  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160807143639  IP                  G�O�G�O�G�O�                