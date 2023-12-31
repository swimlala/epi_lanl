CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-05T19:05:51Z creation      
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
resolution        =���   axis      Z        h  9p   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  @�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     h  B�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  J   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     h  K�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     h  S`   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  Z�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     h  \�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  d   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     h  e�   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     h  mP   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  t�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     h  v�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  }�   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     h  �   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �@   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �p   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �p   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �p   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �p   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �    HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20181005190551  20181005190551  5904952 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               �A   AO  6431                            2B  A   APEX                            7466                            062512                          846 @��M��)1   @�����@1��Q��c�1&�x�1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      �A   A   B   @�ff@�  A   A   A@  A`  A�  A�  A�  A�  A���A�  A�  A�  B   B  B  BffB   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B���B�  B�  B�  B�  B�  B���B���B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C�C�C  C  C  C  C  C   C!�fC#�fC&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CC�fCF  CH  CJ  CL  CN  CP  CR  CT  CV  CW�fCZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn�Cp  Cr�Ct  Cv  Cx�Cz  C|  C~  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C��3C��3C��3C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C��C��D fD �fD  D� D  D� DfD� D  D�fD  D� D  D� D  D� D  D� D	  D	� D
  D
y�D  D� D  D�fD  D� D��D� D  D� DfD�fD  D� D  D� DfD� D  D� D��D� DfD� D��Dy�D  D� D  D� D  D� DfD� D  Dy�D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D&��D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,y�D,��D-y�D.  D.� D/fD/�fD0fD0� D0��D1y�D2  D2� D2��D3� D4  D4� D5  D5�fD6  D6y�D7  D7� D8  D8� D9  D9y�D9��D:� D;fD;�fD<fD<� D=  D=� D>  D>� D?  D?� D@  D@� DAfDA� DB  DB� DC  DC� DD  DD� DE  DE� DFfDF�fDG  DGy�DG��DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DL��DM� DN  DN� DO  DO� DPfDP�fDQ  DQy�DQ��DR� DSfDS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY�fDZ  DZy�DZ��D[� D\  D\� D]  D]� D^  D^� D_  D_� D`fD`� D`��Day�Db  Db� Dc  Dc� Dc��Dd� De  De� Df  Df� DgfDg�fDh  Dh�fDi  Di� Dj  Dj� Dj��Dk� Dl  Dl�fDm  Dm� Dn  Dn�fDo  Do� Dp  Dp� DqfDq�fDrfDr� Dr��Ds� DtfDt�fDufDu�fDvfDv�fDwfDw� Dw��DxS3D�]q111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��
@�p�A�RA"�RAB�RAb�RA�\)A�\)A�\)A�\)A�(�A�\)A�\)A�\)B �B�B�BzB �B(�B0�B8�B@�BH�BP�BX�B`�Bh�Bp�Bx�B�W
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
B�#�B�#�B�W
B�W
B�W
B�W
B�W
B�#�B�#�B�W
B�W
B�W
B�W
B�W
B�W
B�W
B�W
B�W
C +�C+�C+�C+�C+�C
+�C+�C+�C+�CECEC+�C+�C+�C+�C+�C +�C"�C$�C&+�C(+�C*+�C,+�C.+�C0+�C2+�C4+�C6+�C8+�C:+�C<+�C>+�C@+�CB+�CD�CF+�CH+�CJ+�CL+�CN+�CP+�CR+�CT+�CV+�CX�CZ+�C\+�C^+�C`+�Cb+�Cd+�Cf+�Ch+�Cj+�Cl+�CnECp+�CrECt+�Cv+�CxECz+�C|+�C~+�C��C��C��C��C�"�C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�"�C��C��C�"�C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�"�C�"�C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�"�C�"�C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�"�C�"�D GD �GD
�D��D
�D��DGD��D
�D�GD
�D��D
�D��D
�D��D
�D��D	
�D	��D

�D
�{D
�D��D
�D�GD
�D��D{D��D
�D��DGD�GD
�D��D
�D��DGD��D
�D��D{D��DGD��D{D�{D
�D��D
�D��D
�D��DGD��D
�D�{D
�D��D
�D��D
�D��D 
�D ��D!
�D!��D"
�D"��D#
�D#��D$
�D$��D%
�D%��D&
�D&��D'{D'��D(
�D(��D)
�D)��D*
�D*��D+
�D+��D,
�D,�{D-{D-�{D.
�D.��D/GD/�GD0GD0��D1{D1�{D2
�D2��D3{D3��D4
�D4��D5
�D5�GD6
�D6�{D7
�D7��D8
�D8��D9
�D9�{D:{D:��D;GD;�GD<GD<��D=
�D=��D>
�D>��D?
�D?��D@
�D@��DAGDA��DB
�DB��DC
�DC��DD
�DD��DE
�DE��DFGDF�GDG
�DG�{DH{DH��DI
�DI��DJ
�DJ��DK
�DK��DL
�DL��DM{DM��DN
�DN��DO
�DO��DPGDP�GDQ
�DQ�{DR{DR��DSGDS��DT
�DT��DU
�DU��DV
�DV��DW
�DW��DX
�DX��DY
�DY�GDZ
�DZ�{D[{D[��D\
�D\��D]
�D]��D^
�D^��D_
�D_��D`GD`��Da{Da�{Db
�Db��Dc
�Dc��Dd{Dd��De
�De��Df
�Df��DgGDg�GDh
�Dh�GDi
�Di��Dj
�Dj��Dk{Dk��Dl
�Dl�GDm
�Dm��Dn
�Dn�GDo
�Do��Dp
�Dp��DqGDq�GDrGDr��Ds{Ds��DtGDt�GDuGDu�GDvGDv�GDwGDw��Dx{Dx^D�b�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�=qA�=qA�?}A�?}A�A�A�C�A�G�A�I�A�I�A�I�A�K�A�I�A�G�A�G�A�S�A�ZA�VA�S�A�S�A�S�A�XA�\)A�ffA�bNA�`BA�bNA�hsA�t�AȁAȏ\Aȩ�AȸRA�ȴA���A���A��/A���A�/AɼjA�`BA��`A�7LA�VAǇ+A�=qA�;dA�A�A�?}A�;dA�;dA�C�A�^5A�hsA�dZA�O�A�C�A� �A�`BA���A�bNA�ȴA��/A�+A�Q�A�1'A��+A���A�ƨA�x�A���A��
A�33A���A���A�Q�A��A�v�A���A�ƨA�x�A�(�A���A��HA���A�t�A�z�A���A�(�A���A�(�A��PA�%A�&�A��A�x�A�%A�1A��A�~�A��HA��9A���A�hsA���A��
A�dZA�l�A�A���A�~�A�x�A� �A��
A��A�1'A��#A�;dA���A�dZA���A���A�1A�\)A}?}Ax=qAu�wAtQ�Ap�RAjĜAgp�Ae`BAc��Aa��AaVA^��AZ�!AX��AV�9AR��AP��AN^5ALr�AK33AJr�AI�#AH��AE\)AB�AA�-AA�PAA\)A?��A=33A;�
A:�+A:A8n�A7O�A6v�A5�wA2�HA1S�A0��A/�
A.bNA,�A+%A)�mA'�A&1A$jA"�+A!p�A ��A   A&�AZA&�A�#A?}A�^A �A�AE�An�A  A`BA��A�!AƨA^5A�A�TA
ffA�AG�A^5A��A�A�mA+A��A�Az�AVAA�wA�7A7LA �`@��w@���@�`B@�(�@���@��`@� �@���@��;@�hs@�+@�&�@٩�@�/@�Ĝ@؋D@�ƨ@���@֗�@�{@ղ-@�G�@�?}@�7L@�/@��@���@�z�@�t�@�{@�G�@�9X@�l�@��@Ώ\@�E�@�$�@�J@ͺ^@�bN@˅@��@�hs@�Z@ǶF@�C�@�ff@ź^@�X@���@Ĭ@�b@�+@°!@�V@��@���@�hs@���@�Z@�1@�  @�(�@�9X@�b@��w@���@��@��F@�\)@��H@��!@��\@�ff@�$�@���@���@�J@�{@�J@��@��^@���@���@���@�X@�b@���@���@��m@� �@�1@�  @��@��F@��@�|�@��@���@�O�@��^@���@��@���@��@���@��#@�A�@�|�@�E�@�{@�M�@��+@���@��@�5?@��@�&�@��m@��w@��P@�;d@�~�@���@�X@�b@��P@���@�^5@�-@��@�E�@�=q@��T@�p�@��@��@��@��;@��w@�33@�V@�$�@��T@�I�@��
@��@�K�@��@��y@���@���@�v�@�J@���@��#@��@���@�x�@�&�@��@��@�b@���@���@�dZ@�33@�
=@�^5@�$�@��#@���@���@���@���@���@���@���@��h@��@�x�@�hs@�7L@�%@�Q�@�+@�o@�n�@�{@�@���@���@���@���@���@��@�O�@�V@���@�A�@�b@��m@��
@��w@���@�t�@�C�@��@�n�@�E�@�@���@�X@�V@���@��j@��@��@�I�@�1'@��@���@��F@��P@�\)@��H@�=q@�@�hs@�X@�?}@���@��@���@��@��u@�z�@�Z@�1'@�1@�@�ff@��@��-@���@��7@�p�@�`B@�O�@�?}@��@��`@�r�@�ƨ@�C�@�o@���@��@���@��@���@�7L@��@y�d111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�=qA�=qA�?}A�?}A�A�A�C�A�G�A�I�A�I�A�I�A�K�A�I�A�G�A�G�A�S�A�ZA�VA�S�A�S�A�S�A�XA�\)A�ffA�bNA�`BA�bNA�hsA�t�AȁAȏ\Aȩ�AȸRA�ȴA���A���A��/A���A�/AɼjA�`BA��`A�7LA�VAǇ+A�=qA�;dA�A�A�?}A�;dA�;dA�C�A�^5A�hsA�dZA�O�A�C�A� �A�`BA���A�bNA�ȴA��/A�+A�Q�A�1'A��+A���A�ƨA�x�A���A��
A�33A���A���A�Q�A��A�v�A���A�ƨA�x�A�(�A���A��HA���A�t�A�z�A���A�(�A���A�(�A��PA�%A�&�A��A�x�A�%A�1A��A�~�A��HA��9A���A�hsA���A��
A�dZA�l�A�A���A�~�A�x�A� �A��
A��A�1'A��#A�;dA���A�dZA���A���A�1A�\)A}?}Ax=qAu�wAtQ�Ap�RAjĜAgp�Ae`BAc��Aa��AaVA^��AZ�!AX��AV�9AR��AP��AN^5ALr�AK33AJr�AI�#AH��AE\)AB�AA�-AA�PAA\)A?��A=33A;�
A:�+A:A8n�A7O�A6v�A5�wA2�HA1S�A0��A/�
A.bNA,�A+%A)�mA'�A&1A$jA"�+A!p�A ��A   A&�AZA&�A�#A?}A�^A �A�AE�An�A  A`BA��A�!AƨA^5A�A�TA
ffA�AG�A^5A��A�A�mA+A��A�Az�AVAA�wA�7A7LA �`@��w@���@�`B@�(�@���@��`@� �@���@��;@�hs@�+@�&�@٩�@�/@�Ĝ@؋D@�ƨ@���@֗�@�{@ղ-@�G�@�?}@�7L@�/@��@���@�z�@�t�@�{@�G�@�9X@�l�@��@Ώ\@�E�@�$�@�J@ͺ^@�bN@˅@��@�hs@�Z@ǶF@�C�@�ff@ź^@�X@���@Ĭ@�b@�+@°!@�V@��@���@�hs@���@�Z@�1@�  @�(�@�9X@�b@��w@���@��@��F@�\)@��H@��!@��\@�ff@�$�@���@���@�J@�{@�J@��@��^@���@���@���@�X@�b@���@���@��m@� �@�1@�  @��@��F@��@�|�@��@���@�O�@��^@���@��@���@��@���@��#@�A�@�|�@�E�@�{@�M�@��+@���@��@�5?@��@�&�@��m@��w@��P@�;d@�~�@���@�X@�b@��P@���@�^5@�-@��@�E�@�=q@��T@�p�@��@��@��@��;@��w@�33@�V@�$�@��T@�I�@��
@��@�K�@��@��y@���@���@�v�@�J@���@��#@��@���@�x�@�&�@��@��@�b@���@���@�dZ@�33@�
=@�^5@�$�@��#@���@���@���@���@���@���@���@��h@��@�x�@�hs@�7L@�%@�Q�@�+@�o@�n�@�{@�@���@���@���@���@���@��@�O�@�V@���@�A�@�b@��m@��
@��w@���@�t�@�C�@��@�n�@�E�@�@���@�X@�V@���@��j@��@��@�I�@�1'@��@���@��F@��P@�\)@��H@�=q@�@�hs@�X@�?}@���@��@���@��@��u@�z�@�Z@�1'@�1@�@�ff@��@��-@���@��7@�p�@�`B@�O�@�?}@��@��`@�r�@�ƨ@�C�@�o@���@��@���@��@���@�7L@��@y�d111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
H�B
H�B
H�B
H�B
H�B
H�B
H�B
H�B
H�B
H�B
H�B
H�B
H�B
H�B
I�B
K�B
I�B
H�B
H�B
H�B
I�B
J�B
M�B
L�B
J�B
K�B
M�B
Q�B
W
B
[#B
e`B
l�B
s�B
u�B
w�B
|�B
�7B
��B
�B��B��B��B�\B�%B�+B�oB��B��B��B��B�B�wBǮBɺBɺB��B��B��B�B�#B�5B��BhB"�BE�BS�BXBVBO�BI�BD�B?}B=qB;dB:^B:^B8RB?}BD�BF�BD�BB�B?}B;dB=qB5?B1'B/B,B&�B�B%B�B�BB��B�qB��B~�Bm�BffB\)BR�BI�BB�B2-B{B%B
�NB
��B
��B
�B
��B
��B
��B
�bB
z�B
_;B
J�B
-B
�B
JB
B	��B	�TB	ĜB	�3B	��B	�hB	w�B	jB	_;B	T�B	J�B	D�B	6FB	"�B	�B	DB��B�B�/B��B��BǮBÖB�jB�3B�B��B��B��B��B��B��B��B��B��B��B�{B�hB�uB�oB�bB�\B�bB�{B��B��B��B��B��B��B�B�B�B�B�B�B�!B�!B�B�B��B�B�B�B�B�'B�-B�LB�FB�-B�B�B�!B�'B�'B�B�!B�FB�dBB��B�B�B�B�B�#B�/B�/B�BB�BB�HB�HB�TB�TB�TB�NB�TB�HB�;Aa�-B	B	+B	1B	1B	
=B	JB	PB	\B	bB	hB	hB	hB	hB	hB	hB	uB	{B	�B	�B	�B	�B	"�B	$�B	%�B	&�B	&�B	'�B	,B	-B	)�B	(�B	,B	/B	2-B	9XB	>wB	B�B	C�B	F�B	J�B	O�B	S�B	W
B	XB	XB	W
B	VB	ZB	\)B	]/B	`BB	dZB	dZB	e`B	e`B	ffB	gmB	iyB	hsB	hsB	iyB	k�B	m�B	p�B	s�B	u�B	v�B	x�B	z�B	}�B	� B	�B	�B	�B	�B	�B	�%B	�DB	�\B	�oB	�uB	�uB	�{B	�{B	�uB	�uB	�\B	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�-B	�FB	�FB	�LB	�XB	�XB	�^B	�^B	�qB	�}B	�}B	�qB	�qB	�qB	�}B	B	ŢB	��B	��B	��B	��B	��B	�B	�
B	�
B	�
B	�B	�
B	�B	�B	�B	�B	�B	�B	�B	�#B	�)B	�/B	�/B	�;B	�BB	�BB	�BB	�BB	�HB	�NB	�NB	�NB	�ZB	�ZB	�ZB	�ZB	�ZB	�TB	�fB	�fB	�fB	�mB	�mB	�sB	�sB	�mB	�mB	�mB	�mB	�mB	�sB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
  B	��B	��B	��B	��B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
%B
+B
+B
1B
1B
1B
1B
1B
1B
1B

=B

=B
DB
DB
DB
DB
DB
JB
JB
PB
\B
 �222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222224222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222  B
H�B
H�B
H�B
H�B
H�B
H�B
H�B
H�B
H�B
H�B
H�B
H�B
H�B
H�B
I�B
K�B
I�B
H�B
H�B
H�B
I�B
J�B
M�B
L�B
J�B
K�B
M�B
Q�B
W
B
[#B
e`B
l�B
s�B
u�B
w�B
|�B
�7B
��B
�B��B��B��B�\B�%B�+B�oB��B��B��B��B�B�wBǮBɺBɺB��B��B��B�B�#B�5B��BhB"�BE�BS�BXBVBO�BI�BD�B?}B=qB;dB:^B:^B8RB?}BD�BF�BD�BB�B?}B;dB=qB5?B1'B/B,B&�B�B%B�B�BB��B�qB��B~�Bm�BffB\)BR�BI�BB�B2-B{B%B
�NB
��B
��B
�B
��B
��B
��B
�bB
z�B
_;B
J�B
-B
�B
JB
B	��B	�TB	ĜB	�3B	��B	�hB	w�B	jB	_;B	T�B	J�B	D�B	6FB	"�B	�B	DB��B�B�/B��B��BǮBÖB�jB�3B�B��B��B��B��B��B��B��B��B��B��B�{B�hB�uB�oB�bB�\B�bB�{B��B��B��B��B��B��B�B�B�B�B�B�B�!B�!B�B�B��B�B�B�B�B�'B�-B�LB�FB�-B�B�B�!B�'B�'B�B�!B�FB�dBB��B�B�B�B�B�#B�/B�/B�BB�BB�HB�HB�TB�TB�TB�NB�TB�HB�;Aa�-B	B	+B	1B	1B	
=B	JB	PB	\B	bB	hB	hB	hB	hB	hB	hB	uB	{B	�B	�B	�B	�B	"�B	$�B	%�B	&�B	&�B	'�B	,B	-B	)�B	(�B	,B	/B	2-B	9XB	>wB	B�B	C�B	F�B	J�B	O�B	S�B	W
B	XB	XB	W
B	VB	ZB	\)B	]/B	`BB	dZB	dZB	e`B	e`B	ffB	gmB	iyB	hsB	hsB	iyB	k�B	m�B	p�B	s�B	u�B	v�B	x�B	z�B	}�B	� B	�B	�B	�B	�B	�B	�%B	�DB	�\B	�oB	�uB	�uB	�{B	�{B	�uB	�uB	�\B	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�-B	�FB	�FB	�LB	�XB	�XB	�^B	�^B	�qB	�}B	�}B	�qB	�qB	�qB	�}B	B	ŢB	��B	��B	��B	��B	��B	�B	�
B	�
B	�
B	�B	�
B	�B	�B	�B	�B	�B	�B	�B	�#B	�)B	�/B	�/B	�;B	�BB	�BB	�BB	�BB	�HB	�NB	�NB	�NB	�ZB	�ZB	�ZB	�ZB	�ZB	�TB	�fB	�fB	�fB	�mB	�mB	�sB	�sB	�mB	�mB	�mB	�mB	�mB	�sB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
  B	��B	��B	��B	��B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
%B
+B
+B
1B
1B
1B
1B
1B
1B
1B

=B

=B
DB
DB
DB
DB
DB
JB
JB
PB
\B
 �222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222224222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.17 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181005190551                              AO  ARCAADJP                                                                    20181005190551    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181005190551  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181005190551  QCF$                G�O�G�O�G�O�8000            