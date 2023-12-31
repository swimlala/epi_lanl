CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-05T19:05:08Z creation      
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
resolution        =���   axis      Z        �  9p   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  A8   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  C,   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  J�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  L�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  T�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \x   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ^l   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  f4   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  h(   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  o�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  w�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  y�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �t   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �h   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �0   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �`   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �`   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �`   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �`   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �    HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20181005190508  20181005190508  5904952 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               A   AO  6431                            2B  A   APEX                            7466                            062512                          846 @ק��ֻL1   @ק�:�B@31&�x��c��l�C�1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      A   A   A   @@  @�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B_��Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C�C  C
  C  C  C  C�fC  C  C  C  C  C  C   C"  C$  C&  C(  C*  C+�fC.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF�CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Ce�fCg�fCj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~�C�  C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C��C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C��3C�  C�  C�  C�  C�  C��3C��3C�  C�  C��C��C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C��C�  C��3C�  C�  C��3C�  C�  C�  C��C�  C�  C�  C��3C�  C�  C�  C�  D   D � DfD� D  D� D  D� D  D� D  D�fD  D� D  D� D  D�fD	  D	� D
fD
�fD  D� D  D� D��Dy�D  D� D  D� D  D� D  D�fD  D� D  Dy�D��Dy�D��D� D  D� D  D� D  D� D  D�fD  Dy�D  D�fDfD� D��D� D  D� D��D� D   D � D!  D!�fD"  D"� D#  D#� D$  D$y�D%  D%�fD&  D&� D'  D'� D(  D(� D)fD)� D*  D*� D+  D+� D+��D,� D-  D-�fD.  D.� D/  D/y�D0  D0� D1  D1� D2  D2� D2��D3� D3��D4� D5  D5�fD6  D6y�D7  D7� D8  D8� D8��D9� D:  D:y�D;  D;�fD;��D<y�D=  D=�fD>  D>y�D?  D?�fD@  D@� DA  DA� DB  DB� DC  DCy�DD  DD�fDE  DE� DF  DF� DGfDG�fDH  DHy�DH��DI� DJ  DJ� DK  DK�fDL  DL� DM  DM� DNfDN� DOfDO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DT��DU� DV  DV� DW  DWy�DX  DX�fDYfDY� DZ  DZ� D[fD[� D\fD\�fD]fD]� D^  D^� D^��D_y�D_��D`y�D`��Da�fDb  Dby�Dc  Dc� Dd  Dd� De  Dey�Df  Df� Dg  Dg� Dh  Dh� Dh��Diy�Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn�fDo  Do� Dp  Dp� Dq  Dq� Dq��Dr� Ds  Ds� Dt  Dty�Dt��Duy�Dv  Dv�fDwfDw�fDw� Dy��D�G\111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @J�H@�p�@�p�A�RA"�RAB�RAb�RA�\)A�\)A�\)A�\)A�\)A�\)A�\)A�\)B �B�B�B�B �B(�B0�B8�B@�BH�BP�BX�B`G�Bh�Bp�Bx�B�W
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
C +�C+�C+�CEC+�C
+�C+�C+�C+�C�C+�C+�C+�C+�C+�C+�C +�C"+�C$+�C&+�C(+�C*+�C,�C.+�C0+�C2+�C4+�C6+�C8+�C:+�C<+�C>+�C@+�CB+�CD+�CFECH+�CJ+�CL+�CN+�CP+�CR+�CT+�CV+�CX+�CZ+�C\+�C^+�C`+�Cb+�Cd+�Cf�Ch�Cj+�Cl+�Cn+�Cp+�Cr+�Ct+�Cv+�Cx+�Cz+�C|+�C~EC��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�"�C�"�C�"�C��C��C��C��C��C��C��C��C��C�"�C�"�C�"�C��C��C��C��C��C��C��C��C��C��C��C��C�"�C�"�C��C��C��C��C��C��C��C�"�C��C��C��C��C��C��C��C��C��C��C��C��C��C�"�C�"�C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�"�C�"�C��C��C��C��C�"�C��C��C��C��C��C��C��C��C��C��C��C��C��C�"�C��C��C��C��C��C��C��C��C�"�C��C��C��C��C��C��C��C��D 
�D ��DGD��D
�D��D
�D��D
�D��D
�D�GD
�D��D
�D��D
�D�GD	
�D	��D
GD
�GD
�D��D
�D��D{D�{D
�D��D
�D��D
�D��D
�D�GD
�D��D
�D�{D{D�{D{D��D
�D��D
�D��D
�D��D
�D�GD
�D�{D
�D�GDGD��D{D��D
�D��D{D��D 
�D ��D!
�D!�GD"
�D"��D#
�D#��D$
�D$�{D%
�D%�GD&
�D&��D'
�D'��D(
�D(��D)GD)��D*
�D*��D+
�D+��D,{D,��D-
�D-�GD.
�D.��D/
�D/�{D0
�D0��D1
�D1��D2
�D2��D3{D3��D4{D4��D5
�D5�GD6
�D6�{D7
�D7��D8
�D8��D9{D9��D:
�D:�{D;
�D;�GD<{D<�{D=
�D=�GD>
�D>�{D?
�D?�GD@
�D@��DA
�DA��DB
�DB��DC
�DC�{DD
�DD�GDE
�DE��DF
�DF��DGGDG�GDH
�DH�{DI{DI��DJ
�DJ��DK
�DK�GDL
�DL��DM
�DM��DNGDN��DOGDO��DP
�DP��DQ
�DQ��DR
�DR��DS
�DS��DT
�DT��DU{DU��DV
�DV��DW
�DW�{DX
�DX�GDYGDY��DZ
�DZ��D[GD[��D\GD\�GD]GD]��D^
�D^��D_{D_�{D`{D`�{Da{Da�GDb
�Db�{Dc
�Dc��Dd
�Dd��De
�De�{Df
�Df��Dg
�Dg��Dh
�Dh��Di{Di�{Dj
�Dj��Dk
�Dk��Dl
�Dl��Dm
�Dm��Dn
�Dn�GDo
�Do��Dp
�Dp��Dq
�Dq��Dr{Dr��Ds
�Ds��Dt
�Dt�{Du{Du�{Dv
�Dv�GDwGDw�GDw��Dy��D�L�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�E�A�K�A�O�A�Q�A�VA�VA�ZA�S�A�VA�VA�VA�XA�ZA�ZA�ZA�ZA�bNA�hsA�t�A�z�A�~�ÁA̋DA̗�A̩�A���A���A��`A��A̴9A�~�A�hsA�^5A�S�A��A�A˰!A�&�A���Aʉ7A��mAɅA�|�A�1'A���A��
A���A�oA���A��A���AȰ!A�~�A�\)A�"�A���A�VA�1A�VA��TAƟ�A�O�A��A��A���AżjA�~�A�A�A�"�A�$�A�+A��
A�t�A�^5A���A���A��`A��A��\A��HA�XA��;A��A���A���A�A�A�33A�bNA��A�{A���A��A�O�A�A�hsA���A�A�A���A���A�;dA��A�JA��A���A��FA�%A���A���A��9A��mA��A�l�A�A�A�l�A��PA���As�;Aq�^ApbNAo�AnbAmƨAm��Am?}AiG�Af-A`�A^bNA\I�AYt�AWG�ATȴAS��ARbAP�DAPJAO��AO�AN��ALA�AJ��AH~�AF�AE�^AC�mA?x�A=dZA<ZA;t�A:=qA9�A8�RA8n�A6��A5�A41'A2�A0r�A,��A,-A+�wA+oA*�A(M�A(�+A(�uA(r�A&�yA$��A$A"�/A��A�7A7LA�HA��A�wA�A=qA&�A�TA�A=qA��A��AVA9XA  A�TA�mAS�A�Az�A;dA
r�A	`BA?}AJA�PAO�A��AA�AA @��@��@��#@���@�x�@��@�p�@��`@�Q�@���@�C�@��@��+@�|�@�z�@�@��@�O�@�@�hs@��@�hs@�@�9X@��
@�t�@�|�@�
=@���@�Z@��;@��@��#@ܣ�@�(�@ܛ�@�5?@��
@߮@�Z@��@�V@�Ĝ@ݲ-@�33@���@�"�@�E�@ѩ�@Гu@�dZ@�^5@�G�@˅@ʗ�@ɩ�@���@�Z@���@�"�@Ɵ�@�~�@�J@��#@���@Õ�@�t�@�C�@��H@��-@��@��j@�Z@�(�@��@��
@���@��!@�5?@���@�G�@��9@���@�ƨ@��@���@�|�@�@��!@�v�@�M�@�$�@���@�X@�z�@�C�@��+@��#@���@�`B@�&�@��@�z�@�bN@�Q�@�A�@�b@��F@���@���@�V@�j@�b@�ƨ@�t�@�K�@�+@�-@��h@�`B@�?}@�%@���@��D@�I�@��/@��@�A�@��
@�\)@��@���@�v�@�n�@�v�@���@���@���@���@��+@�ff@���@��-@��^@���@��@�$�@�^5@��!@���@���@��@��@�@�ȴ@��\@�M�@���@�G�@�7L@�/@��@�Ĝ@�ƨ@�l�@�V@�`B@�7L@�7L@�7L@�&�@��@�V@���@���@�z�@�1@��P@�;d@�@��@���@��\@�E�@��@���@�&�@��/@�j@�ƨ@�33@��H@��R@��\@�E�@��h@�O�@��@���@���@��@���@��@�I�@��@��F@�+@�n�@���@��`@���@��/@�Ĝ@���@���@�C�@��@���@�^5@�V@�=q@�5?@�-@�-@�-@�$�@��@��^@��-@��-@���@��7@�O�@�7L@�%@���@��@�@�$�@���@���@�x�@�O�@�7L@�V@���@�9X@�9X@�1@�S�@�33@��@��@���@�^5@���@���@�hs@���@�z�@�r�@�Q�@�1'@���@��;@���@��w@���@���@�|�@�33@��@���@�ff@�5?@��@��#@���@��^@��-@���@�X@�?}@��@�Ĝ@�j@�(�@���@��
@�t�@�"�@���@y+111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�E�A�K�A�O�A�Q�A�VA�VA�ZA�S�A�VA�VA�VA�XA�ZA�ZA�ZA�ZA�bNA�hsA�t�A�z�A�~�ÁA̋DA̗�A̩�A���A���A��`A��A̴9A�~�A�hsA�^5A�S�A��A�A˰!A�&�A���Aʉ7A��mAɅA�|�A�1'A���A��
A���A�oA���A��A���AȰ!A�~�A�\)A�"�A���A�VA�1A�VA��TAƟ�A�O�A��A��A���AżjA�~�A�A�A�"�A�$�A�+A��
A�t�A�^5A���A���A��`A��A��\A��HA�XA��;A��A���A���A�A�A�33A�bNA��A�{A���A��A�O�A�A�hsA���A�A�A���A���A�;dA��A�JA��A���A��FA�%A���A���A��9A��mA��A�l�A�A�A�l�A��PA���As�;Aq�^ApbNAo�AnbAmƨAm��Am?}AiG�Af-A`�A^bNA\I�AYt�AWG�ATȴAS��ARbAP�DAPJAO��AO�AN��ALA�AJ��AH~�AF�AE�^AC�mA?x�A=dZA<ZA;t�A:=qA9�A8�RA8n�A6��A5�A41'A2�A0r�A,��A,-A+�wA+oA*�A(M�A(�+A(�uA(r�A&�yA$��A$A"�/A��A�7A7LA�HA��A�wA�A=qA&�A�TA�A=qA��A��AVA9XA  A�TA�mAS�A�Az�A;dA
r�A	`BA?}AJA�PAO�A��AA�AA @��@��@��#@���@�x�@��@�p�@��`@�Q�@���@�C�@��@��+@�|�@�z�@�@��@�O�@�@�hs@��@�hs@�@�9X@��
@�t�@�|�@�
=@���@�Z@��;@��@��#@ܣ�@�(�@ܛ�@�5?@��
@߮@�Z@��@�V@�Ĝ@ݲ-@�33@���@�"�@�E�@ѩ�@Гu@�dZ@�^5@�G�@˅@ʗ�@ɩ�@���@�Z@���@�"�@Ɵ�@�~�@�J@��#@���@Õ�@�t�@�C�@��H@��-@��@��j@�Z@�(�@��@��
@���@��!@�5?@���@�G�@��9@���@�ƨ@��@���@�|�@�@��!@�v�@�M�@�$�@���@�X@�z�@�C�@��+@��#@���@�`B@�&�@��@�z�@�bN@�Q�@�A�@�b@��F@���@���@�V@�j@�b@�ƨ@�t�@�K�@�+@�-@��h@�`B@�?}@�%@���@��D@�I�@��/@��@�A�@��
@�\)@��@���@�v�@�n�@�v�@���@���@���@���@��+@�ff@���@��-@��^@���@��@�$�@�^5@��!@���@���@��@��@�@�ȴ@��\@�M�@���@�G�@�7L@�/@��@�Ĝ@�ƨ@�l�@�V@�`B@�7L@�7L@�7L@�&�@��@�V@���@���@�z�@�1@��P@�;d@�@��@���@��\@�E�@��@���@�&�@��/@�j@�ƨ@�33@��H@��R@��\@�E�@��h@�O�@��@���@���@��@���@��@�I�@��@��F@�+@�n�@���@��`@���@��/@�Ĝ@���@���@�C�@��@���@�^5@�V@�=q@�5?@�-@�-@�-@�$�@��@��^@��-@��-@���@��7@�O�@�7L@�%@���@��@�@�$�@���@���@�x�@�O�@�7L@�V@���@�9X@�9X@�1@�S�@�33@��@��@���@�^5@���@���@�hs@���@�z�@�r�@�Q�@�1'@���@��;@���@��w@���@���@�|�@�33@��@���@�ff@�5?@��@��#@���@��^@��-@���@�X@�?}@��@�Ĝ@�j@�(�@���@��
@�t�@�"�@���@y+111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B	��B	��B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�!B	��B	�B	�;B	�B
�B
(�B
6FB
C�B
M�B
o�B
�DB
��B
��B
��B
��B
��B
��B
��B
��B
�B
�3B
�RB
ǮB
��BPBbB\BbB�B�B/BA�BS�Bm�By�B�+B��B��B��B�FB�^B��B��B	7BDBB  B+B)�BT�B\)BVBM�BN�BB�B[#Bp�Bq�B�hB�\B�uB�BdZB=qB0!B �BB�B�HBŢB��B{�BdZBQ�BL�BE�B;dB%�BuB  B
�fB
�)B
�B
��B
�^B
�B
��B
��B
�%B
w�B
e`B
'�B	�B	ƨB	�qB	�?B	�B	�B	�B	��B	�JB	s�B	Q�B	A�B	7LB	(�B	�B	{B	bB	JB	B	B��B��B��B�B�mB�sB�yB�yB�sB�`B�`B�`B�`B�mB�sB�yB�sB�yB�ZB�/B�ZB�#B��B��BB�jB�FB�XB��B�B�TB�`B�HB�B��B�}B�XB�LB�9B�-B�B��B��B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�hB�PB�PB�VB�\B�hB��B��B��B�{B�DB�JB�VB�VB�\B�hB��B��B�B�!B�!B�'B�'B�!B�!B�B�B��B��B��B��B��B��B�B�B�B��B�B��B��B��B��B��B�B�B�B�?BĜB�
B�BB�fB�sB�B�B�HB��BƨBŢBB��B�wB�wB��BĜBɺB��B��B��B��B�
B�B�/B�/B�5B�5B�TB�yB�B�B�B�B��B��B��B��B��B��B��B��B	B	B	+B		7B	PB	\B	\B	bB	bB	uB	�B	�B	�B	�B	�B	"�B	'�B	1'B	7LB	;dB	=qB	?}B	A�B	F�B	I�B	J�B	K�B	L�B	O�B	T�B	ZB	\)B	\)B	aHB	e`B	ffB	ffB	ffB	gmB	hsB	iyB	jB	k�B	l�B	l�B	m�B	s�B	~�B	�B	~�B	|�B	|�B	{�B	z�B	z�B	{�B	{�B	}�B	�B	�DB	�PB	�\B	�uB	��B	��B	��B	��B	��B	��B	�B	�'B	�-B	�FB	�RB	�XB	�^B	�^B	�^B	�jB	��B	ĜB	ĜB	ŢB	ŢB	ƨB	ȴB	ǮB	ǮB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�B	�B	�)B	�)B	�/B	�;B	�HB	�HB	�HB	�NB	�NB	�TB	�ZB	�`B	�fB	�fB	�fB	�fB	�mB	�sB	�yB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
B
B
B
B
B
B
B
B
B
B
+B
+B
+B
+B
+B
1B
1B
	7B
	7B

=B
DB
DB
DB
DB
JB
JB
JB
JB
JB
JB
JB
JB
PB
VB
VB
VB
VB
\B
\B
\B
\B
\B
\B
\B
\B
bB
bB
bB
\B
\B
VB
oB
B
 v222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222  B	��B	��B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�!B	��B	�B	�;B	�B
�B
(�B
6FB
C�B
M�B
o�B
�DB
��B
��B
��B
��B
��B
��B
��B
��B
�B
�3B
�RB
ǮB
��BPBbB\BbB�B�B/BA�BS�Bm�By�B�+B��B��B��B�FB�^B��B��B	7BDBB  B+B)�BT�B\)BVBM�BN�BB�B[#Bp�Bq�B�hB�\B�uB�BdZB=qB0!B �BB�B�HBŢB��B{�BdZBQ�BL�BE�B;dB%�BuB  B
�fB
�)B
�B
��B
�^B
�B
��B
��B
�%B
w�B
e`B
'�B	�B	ƨB	�qB	�?B	�B	�B	�B	��B	�JB	s�B	Q�B	A�B	7LB	(�B	�B	{B	bB	JB	B	B��B��B��B�B�mB�sB�yB�yB�sB�`B�`B�`B�`B�mB�sB�yB�sB�yB�ZB�/B�ZB�#B��B��BB�jB�FB�XB��B�B�TB�`B�HB�B��B�}B�XB�LB�9B�-B�B��B��B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�hB�PB�PB�VB�\B�hB��B��B��B�{B�DB�JB�VB�VB�\B�hB��B��B�B�!B�!B�'B�'B�!B�!B�B�B��B��B��B��B��B��B�B�B�B��B�B��B��B��B��B��B�B�B�B�?BĜB�
B�BB�fB�sB�B�B�HB��BƨBŢBB��B�wB�wB��BĜBɺB��B��B��B��B�
B�B�/B�/B�5B�5B�TB�yB�B�B�B�B��B��B��B��B��B��B��B��B	B	B	+B		7B	PB	\B	\B	bB	bB	uB	�B	�B	�B	�B	�B	"�B	'�B	1'B	7LB	;dB	=qB	?}B	A�B	F�B	I�B	J�B	K�B	L�B	O�B	T�B	ZB	\)B	\)B	aHB	e`B	ffB	ffB	ffB	gmB	hsB	iyB	jB	k�B	l�B	l�B	m�B	s�B	~�B	�B	~�B	|�B	|�B	{�B	z�B	z�B	{�B	{�B	}�B	�B	�DB	�PB	�\B	�uB	��B	��B	��B	��B	��B	��B	�B	�'B	�-B	�FB	�RB	�XB	�^B	�^B	�^B	�jB	��B	ĜB	ĜB	ŢB	ŢB	ƨB	ȴB	ǮB	ǮB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�B	�B	�)B	�)B	�/B	�;B	�HB	�HB	�HB	�NB	�NB	�TB	�ZB	�`B	�fB	�fB	�fB	�fB	�mB	�sB	�yB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
B
B
B
B
B
B
B
B
B
B
+B
+B
+B
+B
+B
1B
1B
	7B
	7B

=B
DB
DB
DB
DB
JB
JB
JB
JB
JB
JB
JB
JB
PB
VB
VB
VB
VB
\B
\B
\B
\B
\B
\B
\B
\B
bB
bB
bB
\B
\B
VB
oB
B
 v222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.17 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181005190508                              AO  ARCAADJP                                                                    20181005190508    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181005190508  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181005190508  QCF$                G�O�G�O�G�O�8000            