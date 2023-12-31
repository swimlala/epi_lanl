CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-24T14:08:08Z creation      
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20181024140808  20181024140808  5904957 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               A   AO  6560                            2B  A   APEX                            7471                            062512                          846 @ײ�����1   @ײ�$@3�33333�c�I�^1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      A   A   A   @�33@�  A   A   A@  A`  A���A���A���A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�33B�33B�  B���B�  B�  B�  B�  B���B�  B�  B�  B���B�  B�  B�33C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN�CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Co�fCr  Ct  Cv  Cx  Cz  C{�fC}�fC�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C��C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C��C��C��C�  C�  C�  C��C�  C�  C�  C��C��C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C��C�  C��3C�  C�  C�  C�  C��3C��3C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C��C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C��3C��3C�  C��C�  C�  C��3D � DfD� D��D� DfD�fDfD� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  Dy�D��Dy�D��D� D  D� DfD� D  D� D  D� DfD� D  D�fD  D� DfD� D  D� D  D� DfD� D  D� D  D� D  D� D  D� D  D� D  D� DfD�fD   D � D!  D!�fD"  D"y�D#  D#� D$  D$�fD%  D%� D&  D&�fD'  D'y�D(  D(� D)  D)� D*  D*�fD+  D+y�D,  D,� D-  D-� D.  D.� D/  D/y�D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<�fD=  D=y�D>  D>� D>��D?� D@  D@� D@��DA� DBfDB�fDCfDC�fDD  DD� DE  DE� DF  DF�fDG  DG�fDHfDH� DI  DI� DJ  DJ� DK  DK� DL  DL� DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� DdfDd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dm��Dn� Do  Do� Dp  Dp� Dq  Dq�fDrfDr�fDs  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw�fDw�fDy�fD�L)111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�G�@�{A
=A#
=AC
=Ac
=A�Q�A�Q�A�Q�A��A��AхA�A�B BBBB B(B0B8B@BHBPBXB`BhBpBxB�aHB�aHB��{B�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB��{B��{BĔ{B�aHB�.B�aHB�aHB�aHB�aHB�.B�aHB�aHB�aHB�.B�aHB�aHB��{C 0�C0�C0�C0�C0�C
0�C0�C0�C0�C0�C0�C0�C0�C0�C0�C0�C 0�C"0�C$0�C&0�C(0�C*0�C,0�C.0�C00�C20�C40�C60�C80�C:0�C<0�C>0�C@0�CB0�CD0�CF0�CH0�CJ0�CL0�CNJ>CP0�CR0�CT0�CV0�CX0�CZ0�C\0�C^0�C`0�Cb0�Cd0�Cf0�Ch0�Cj0�Cl0�Cn0�Cp
Cr0�Ct0�Cv0�Cx0�Cz0�C|
C~
C�RC�%C�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�%C�RC�RC�RC�RC�RC�RC�RC��C�RC�RC�RC�RC�%C�RC�RC�RC��C�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC��C�RC�RC�RC�RC�RC�RC�RC�%C�%C�%C�RC�RC�RC�%C�RC�RC�RC�%C�%C�%C�%C�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�%C�RC�RC�RC�RC�RC�%C�RC��C�RC�RC�RC�RC��C��C�RC�RC��C�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC��C�RC�RC�%C�RC�RC�RC�RC��C��C�RC�RC�RC�RC��C��C�RC�%C�RC�RD �D �)D�D�)D�D�)D�D��D�D�)D)D�)D)D�)D)D�)D)D�)D	)D	�)D
)D
�)D)D��D�D��D�D�)D)D�)D�D�)D)D�)D)D�)D�D�)D)D��D)D�)D�D�)D)D�)D)D�)D�D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D�D��D )D �)D!)D!��D")D"��D#)D#�)D$)D$��D%)D%�)D&)D&��D')D'��D()D(�)D))D)�)D*)D*��D+)D+��D,)D,�)D-)D-�)D.)D.�)D/)D/��D0)D0�)D1)D1�)D2)D2�)D3)D3�)D4)D4�)D5)D5�)D6)D6�)D7)D7�)D8)D8�)D9)D9�)D:)D:�)D;)D;�)D<)D<��D=)D=��D>)D>�)D?�D?�)D@)D@�)DA�DA�)DB�DB��DC�DC��DD)DD�)DE)DE�)DF)DF��DG)DG��DH�DH�)DI)DI�)DJ)DJ�)DK)DK�)DL)DL�)DX�)DY)DY�)DZ)DZ�)D[)D[�)D\)D\�)D])D]�)D^)D^�)D_)D_�)D`)D`�)Da)Da�)Db)Db�)Dc)Dc�)Dd�Dd�)De)De�)Df)Df�)Dg)Dg�)Dh)Dh�)Di)Di�)Dj)Dj�)Dk)Dk�)Dl)Dl�)Dm)Dm�)Dn�Dn�)Do)Do�)Dp)Dp�)Dq)Dq��Dr�Dr��Ds)Ds�)Dt)Dt�)Du)Du�)Dv)Dv�)Dw)Dw��DwҏDy��D�R=111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�Aѥ�Aѩ�AѲ-AѲ-AѲ-AѲ-AѴ9AѴ9AѴ9AѴ9AѰ!AѰ!AѲ-AѰ!AѰ!AѶFAѺ^AѺ^AѸRAѼjAѾwAѺ^AѼjAѰ!A�~�A��A��AϑhA���A�AξwA���A��A�ĜAμjA���A΃AΉ7A��
AΥ�A�1AͮA��A�+A�~�A���Ḁ�A�S�A�C�A˼jA��A�^5A�^5A���AɍPAȩ�A��`A��A�x�A�  A��A�A�A�A�I�A�7LA�~�A��`A�Q�A��mA�?}A�%A���A�VA���A�Q�A��A�5?A��TA��7A�M�A�A�jA�ȴA���A�VA�K�A��DA�z�A�\)A�v�A���A�hsA��A���A�ȴA��DA�-A�+A��!A��7A��A�n�A�z�A�x�A�
=A��A�  A���A�1'A�^5A�p�A��-A��jA�E�A�9XA��mA�?}A�{A�
=A�ȴA�A���A��wA�"�A�A��FA&�A}��A{XAuG�Aqp�Ao�mAl�Ah��AeK�Act�A`I�A^Q�A]�AZr�AVZAU�PAT��AS|�ARI�AQC�AN��AI�PAE33AC�FAA��A=��A:JA8VA7|�A6�DA5/A4n�A2�A1A/x�A-A+��A*�!A)��A)|�A)K�A(�\A%�#A%p�A%dZA%\)A%XA%;dA$�RA$E�A#��A"�RA!��A!�7A!?}A ��AbNA�yA��A��A5?A�hAz�A`BA��AbA��A`BAVA�uA1AXA�!A��A�A  A��A�yA1'AK�A	�TA��A��A�A/A�Az�A9XA�TAO�A��A-A ��A j@��y@���@�"�@��@���@�bN@��@��@�9X@���@���@��
@��@��@�ȴ@�ff@�J@��#@�%@�9@�l�@���@�@ߥ�@���@�bN@��@�K�@�X@�bN@�|�@�
=@��H@�V@�{@���@�@Չ7@���@��@��T@�p�@�/@��@��;@���@�@�Z@ˍP@�33@�@ʸR@ɲ-@�Ĝ@�Z@���@�l�@���@ư!@�ff@�@ŉ7@�&�@ēu@�C�@�@\@�G�@��D@�Z@�I�@�|�@���@���@���@�%@�j@� �@�ƨ@���@�l�@�@�^5@�=q@�=q@�-@�$�@�$�@�?}@�7L@��@��`@���@���@��@�b@���@�33@�=q@�p�@��
@���@��T@��`@��m@�^5@�x�@�Ĝ@��@�|�@���@�Q�@�Q�@�ƨ@��@���@���@�S�@�+@�+@�
=@���@��@��@���@��@�X@�9X@�dZ@���@���@�v�@�$�@��7@�x�@�p�@�&�@���@���@�Z@�b@��m@���@�t�@��!@�M�@�{@���@�x�@�hs@���@��@�1'@�S�@�+@�@���@�J@��T@��#@��-@���@��7@�hs@��@�G�@���@��#@�/@���@��@��@�v�@�{@��@�$�@��@��@���@��@��u@�  @�S�@��@���@�=q@�{@�J@�1'@�(�@��@��@�ƨ@���@�t�@�\)@�K�@�+@��\@�=q@��#@�@���@�?}@�%@���@��D@�j@�I�@�1'@��@��
@���@�dZ@�C�@�ȴ@��@��@��^@�X@��@��@���@��9@��u@��D@��@��@�z�@�z�@�Q�@� �@��;@�33@��@�O�@�/@��@��@���@��`@���@��@�j@�(�@�ƨ@�dZ@�+@��@��@��R@��+@�]d@p�4111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  Aѥ�Aѩ�AѲ-AѲ-AѲ-AѲ-AѴ9AѴ9AѴ9AѴ9AѰ!AѰ!AѲ-AѰ!AѰ!AѶFAѺ^AѺ^AѸRAѼjAѾwAѺ^AѼjAѰ!A�~�A��A��AϑhA���A�AξwA���A��A�ĜAμjA���A΃AΉ7A��
AΥ�A�1AͮA��A�+A�~�A���Ḁ�A�S�A�C�A˼jA��A�^5A�^5A���AɍPAȩ�A��`A��A�x�A�  A��A�A�A�A�I�A�7LA�~�A��`A�Q�A��mA�?}A�%A���A�VA���A�Q�A��A�5?A��TA��7A�M�A�A�jA�ȴA���A�VA�K�A��DA�z�A�\)A�v�A���A�hsA��A���A�ȴA��DA�-A�+A��!A��7A��A�n�A�z�A�x�A�
=A��A�  A���A�1'A�^5A�p�A��-A��jA�E�A�9XA��mA�?}A�{A�
=A�ȴA�A���A��wA�"�A�A��FA&�A}��A{XAuG�Aqp�Ao�mAl�Ah��AeK�Act�A`I�A^Q�A]�AZr�AVZAU�PAT��AS|�ARI�AQC�AN��AI�PAE33AC�FAA��A=��A:JA8VA7|�A6�DA5/A4n�A2�A1A/x�A-A+��A*�!A)��A)|�A)K�A(�\A%�#A%p�A%dZA%\)A%XA%;dA$�RA$E�A#��A"�RA!��A!�7A!?}A ��AbNA�yA��A��A5?A�hAz�A`BA��AbA��A`BAVA�uA1AXA�!A��A�A  A��A�yA1'AK�A	�TA��A��A�A/A�Az�A9XA�TAO�A��A-A ��A j@��y@���@�"�@��@���@�bN@��@��@�9X@���@���@��
@��@��@�ȴ@�ff@�J@��#@�%@�9@�l�@���@�@ߥ�@���@�bN@��@�K�@�X@�bN@�|�@�
=@��H@�V@�{@���@�@Չ7@���@��@��T@�p�@�/@��@��;@���@�@�Z@ˍP@�33@�@ʸR@ɲ-@�Ĝ@�Z@���@�l�@���@ư!@�ff@�@ŉ7@�&�@ēu@�C�@�@\@�G�@��D@�Z@�I�@�|�@���@���@���@�%@�j@� �@�ƨ@���@�l�@�@�^5@�=q@�=q@�-@�$�@�$�@�?}@�7L@��@��`@���@���@��@�b@���@�33@�=q@�p�@��
@���@��T@��`@��m@�^5@�x�@�Ĝ@��@�|�@���@�Q�@�Q�@�ƨ@��@���@���@�S�@�+@�+@�
=@���@��@��@���@��@�X@�9X@�dZ@���@���@�v�@�$�@��7@�x�@�p�@�&�@���@���@�Z@�b@��m@���@�t�@��!@�M�@�{@���@�x�@�hs@���@��@�1'@�S�@�+@�@���@�J@��T@��#@��-@���@��7@�hs@��@�G�@���@��#@�/@���@��@��@�v�@�{@��@�$�@��@��@���@��@��u@�  @�S�@��@���@�=q@�{@�J@�1'@�(�@��@��@�ƨ@���@�t�@�\)@�K�@�+@��\@�=q@��#@�@���@�?}@�%@���@��D@�j@�I�@�1'@��@��
@���@�dZ@�C�@�ȴ@��@��@��^@�X@��@��@���@��9@��u@��D@��@��@�z�@�z�@�Q�@� �@��;@�33@��@�O�@�/@��@��@���@��`@���@��@�j@�(�@�ƨ@�dZ@�+@��@��@��R@��+@�]d@p�4111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
8RB
8RB
8RB
8RB
8RB
8RB
8RB
8RB
8RB
8RB
8RB
8RB
8RB
8RB
8RB
8RB
8RB
8RB
7LB
8RB
8RB
7LB
7LB
6FB
33B
,B
�B
�B
.B
@�B
I�B
N�B
_;B
cTB
iyB
t�B
s�B
�B
��B
�B
�jB
�qB
�B  BB
��B
�B
�B
�B
�BPB{B�B.BF�BM�Bx�B~�B�wB�ZB+B9XB<jB?}BN�B\)Bk�Bv�B|�B�B�+B�DB�bB��B��B��B��B�B�B�B�B�B��B��B��B�Bv�BffB\)BS�BM�BK�BO�BO�BT�B`BBXBH�B.BJB��B��B�B�fB�
BǮB��B��B��B�\B� Be`BS�B;dB�B
=B
��B
�B
�B
�B
�-B
��B
�B
k�B
e`B
T�B
6FB
+B
{B	�B	��B	ǮB	�-B	��B	�+B	z�B	iyB	^5B	VB	F�B	33B	.B	(�B	!�B	�B	uB	B�`B��B��BÖB�RB�'B�B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�{B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�{B�hB�PB�bB�oB�uB�uB�uB�{B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B��B�B��B��B��B��B��B�B��B�B�B�!B�-B�?B�RB�RB�RB�jB�qB�}B��B��BBÖBĜBÖBÖBÖBŢBȴBɺBɺBɺBɺBǮBĜBĜBǮBɺBɺBɺB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�)B�5B�;B�ZB�`B�yB�B�B�B�B�B�B�B��B��B��B��B��B��B��B��B��B	B	B	B	B	B	%B	1B	
=B	oB	�B	�B	�B	!�B	$�B	&�B	/B	33B	49B	9XB	@�B	G�B	J�B	J�B	K�B	T�B	]/B	aHB	dZB	e`B	ffB	iyB	n�B	s�B	u�B	w�B	w�B	{�B	z�B	y�B	{�B	� B	�B	�+B	�1B	�=B	�=B	�VB	�oB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�-B	�3B	�XB	�}B	��B	��B	��B	B	B	B	ŢB	ƨB	ǮB	ǮB	ȴB	ȴB	ȴB	ȴB	ȴB	ɺB	ɺB	��B	��B	��B	��B	�mB	�mB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
%B
+B
+B
+B
+B
+B
+B
+B
+B
+B
+B
+B
+B
1B
	7B
DB
PB
VB
VB
VB
VB
VB
VB
VB
\B
\B
bB
hB
oB
oB
oB
oB
�B
B
&L111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B
8RB
8RB
8RB
8RB
8RB
8RB
8RB
8RB
8RB
8RB
8RB
8RB
8RB
8RB
8RB
8RB
8RB
8RB
7LB
8RB
8RB
7LB
7LB
6FB
33B
,B
�B
�B
.B
@�B
I�B
N�B
_;B
cTB
iyB
t�B
s�B
�B
��B
�B
�jB
�qB
�B  BB
��B
�B
�B
�B
�BPB{B�B.BF�BM�Bx�B~�B�wB�ZB+B9XB<jB?}BN�B\)Bk�Bv�B|�B�B�+B�DB�bB��B��B��B��B�B�B�B�B�B��B��B��B�Bv�BffB\)BS�BM�BK�BO�BO�BT�B`BBXBH�B.BJB��B��B�B�fB�
BǮB��B��B��B�\B� Be`BS�B;dB�B
=B
��B
�B
�B
�B
�-B
��B
�B
k�B
e`B
T�B
6FB
+B
{B	�B	��B	ǮB	�-B	��B	�+B	z�B	iyB	^5B	VB	F�B	33B	.B	(�B	!�B	�B	uB	B�`B��B��BÖB�RB�'B�B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�{B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�{B�hB�PB�bB�oB�uB�uB�uB�{B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B��B�B��B��B��B��B��B�B��B�B�B�!B�-B�?B�RB�RB�RB�jB�qB�}B��B��BBÖBĜBÖBÖBÖBŢBȴBɺBɺBɺBɺBǮBĜBĜBǮBɺBɺBɺB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�)B�5B�;B�ZB�`B�yB�B�B�B�B�B�B�B��B��B��B��B��B��B��B��B��B	B	B	B	B	B	%B	1B	
=B	oB	�B	�B	�B	!�B	$�B	&�B	/B	33B	49B	9XB	@�B	G�B	J�B	J�B	K�B	T�B	]/B	aHB	dZB	e`B	ffB	iyB	n�B	s�B	u�B	w�B	w�B	{�B	z�B	y�B	{�B	� B	�B	�+B	�1B	�=B	�=B	�VB	�oB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�-B	�3B	�XB	�}B	��B	��B	��B	B	B	B	ŢB	ƨB	ǮB	ǮB	ȴB	ȴB	ȴB	ȴB	ȴB	ɺB	ɺB	��B	��B	��B	��B	�mB	�mB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
%B
+B
+B
+B
+B
+B
+B
+B
+B
+B
+B
+B
+B
1B
	7B
DB
PB
VB
VB
VB
VB
VB
VB
VB
\B
\B
bB
hB
oB
oB
oB
oB
�B
B
&L111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.19 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181024140808                              AO  ARCAADJP                                                                    20181024140808    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181024140808  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181024140808  QCF$                G�O�G�O�G�O�0               