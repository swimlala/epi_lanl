CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-24T14:08:47Z creation      
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20181024140847  20181024140847  5904957 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               �A   AO  6560                            2B  A   APEX                            7471                            062512                          846 @����^ �1   @���hK��@5��v��d����1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      �A   A   B   @�ff@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�33B�33B�  B�33B�33B�33B�  B���B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C�fC  C  C  C  C  C �C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CE�fCH  CJ  CL  CM�fCP  CR  CT  CV�CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv�Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C��3C��3C�  C�  C�  C�  C�  C�  C��C�  C��3C��3C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C��3C�  C�  C�  C��3C��3C�  C�  C��3C�  C�  C�  C��C�  D   D � D  D� D  D� D  Dy�D  D� D  D� D��D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  Dy�D  D� D  D� D  D� D  D� D  D� D   D � D ��D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.�fD/  D/� D0  D0� D1  D1� D2  D2� D2��D3� D4  D4� D4��D5y�D6  D6� D7  D7� D8  D8� D9  D9�fD:  D:� D;  D;� D<fD<� D=  D=� D>fD>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DC��DDy�DD��DEy�DF  DF� DGfDG� DG��DT  DT� DT��DU� DVfDV� DW  DW� DXfDX�fDY  DY� DZfDZ� DZ��D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� DbfDb� Dc  Dc� Dd  Dd� De  Dey�Df  Df� Dg  Dg� Dh  Dh� Di  Di� Di��Dj� Dj��Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp�fDq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dy�)D�B�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�z�@�{A
=A#
=AC
=Ac
=A��A��A��A��A��AхA�A�B BBBB B(B0B8B@BHBPBXB`BhBpBxB�aHB�aHB�aHB�aHB��{B��{B�aHB��{B��{B��{B�aHB�.B�.B�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHC 0�C0�C0�C0�C0�C
0�C0�C0�C0�C0�C
C0�C0�C0�C0�C0�C J>C"0�C$0�C&0�C(0�C*0�C,0�C.0�C00�C20�C40�C60�C80�C:0�C<0�C>0�C@0�CB0�CD0�CF
CH0�CJ0�CL0�CN
CP0�CR0�CT0�CVJ>CX0�CZ0�C\0�C^0�C`0�Cb0�Cd0�Cf0�Ch0�Cj0�Cl0�Cn0�Cp0�Cr0�Ct0�CvJ>Cx0�Cz0�C|0�C~0�C�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC��C�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC��C�RC�RC�RC��C��C�RC�RC�RC�RC�RC�RC�%C�RC��C��C�RC�RC�RC�RC��C�RC�RC�RC�RC�RC�RC�RC�RC�%C�RC�RC�RC�RC�RC�RC�RC�RC�RC��C�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC��C��C��C�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�%C�RC��C�RC�RC�RC��C��C�RC�RC��C�RC�RC�RC�%C�RD )D �)D)D�)D)D�)D)D��D)D�)D)D�)D�D�)D)D�)D)D�)D	)D	�)D
)D
�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D��D)D�)D)D�)D)D�)D)D�)D)D�)D )D �)D!�D!�)D")D"�)D#)D#�)D$)D$�)D%)D%�)D&)D&�)D')D'�)D()D(�)D))D)�)D*)D*�)D+)D+�)D,)D,�)D-)D-�)D.)D.��D/)D/�)D0)D0�)D1)D1�)D2)D2�)D3�D3�)D4)D4�)D5�D5��D6)D6�)D7)D7�)D8)D8�)D9)D9��D:)D:�)D;)D;�)D<�D<�)D=)D=�)D>�D>�)D?)D?�)D@)D@�)DA)DA�)DB)DB�)DC)DC�)DD�DD��DE�DE��DF)DF�)DG�DG�)DH�DT)DT�)DU�DU�)DV�DV�)DW)DW�)DX�DX��DY)DY�)DZ�DZ�)D[�D[�)D\)D\�)D])D]�)D^)D^�)D_)D_�)D`)D`�)Da)Da�)Db�Db�)Dc)Dc�)Dd)Dd�)De)De��Df)Df�)Dg)Dg�)Dh)Dh�)Di)Di�)Dj�Dj�)Dk�Dk�)Dl)Dl�)Dm)Dm�)Dn)Dn�)Do)Do�)Dp)Dp��Dq)Dq�)Dr)Dr�)Ds)Ds�)Dt)Dt�)Du)Du�)Dv)Dv�)Dw)Dw�)Dx)Dy�RD�H�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�%A�%A�%A�1A�G�A�p�A�ZA�"�A���A��/Aȡ�A�p�A�VA�jAȃA�p�A�S�A�C�A�C�A�E�A�=qA�;dA�;dA�G�A�Q�A�VA�^5Aȝ�A��A��/A��#A���AȋDA�^5A��A�A�A�ȴA��AŬA�v�A�&�Aã�A�%A�oA�t�A���A���A�M�A�;dA��mA�t�A��A��
A�ƨA��A��A�33A��A���A�VA��A��TA�`BA��A�bNA�1'A��!A��hA��\A��A��;A��9A�C�A��A�v�A�M�A��A�/A��9A�"�A�&�A�-A���A�+A��DA� �A�A�VA�Q�A�ĜA���A�Q�A��PA���A�A�A��^A�ȴA�(�A�G�A�jA��A��jA���A�r�A���A��RA�M�A�hsA�M�A��A|E�Ayl�AyS�AvȴAr��Ar�Apv�Ao�7An~�Alr�Ak�TAk+Aj1'AhjAg&�Ad��Aa�A`�A\�yA[`BAY�PAX�AV$�AS�AR��ARJANA�AJbNAI�AH��AH�RAH��AHAF��AEx�AD�ADbAC�hAB��AAl�A?ƨA?;dA=��A<�RA<M�A<  A;dZA:�A9�;A7�A7VA4�jA3G�A2�A1XA0��A0��A0r�A/�A/�A.��A-O�A,�A+�A*jA(��A'�#A&�yA$��A#O�A ��AQ�AG�AA�A��AS�A"�A��A-A�\A�A%AĜAbNA�#A�A�TA"�AE�A"�A
~�A	�TA	G�A��Av�AhsAȴA��An�A�A��A�`A�\A�A n�A J@��@���@��@���@�z�@�+@�r�@���@���@��@�@��H@���@��-@�A�@��@�v�@��T@��;@���@�F@�E�@�&�@�1'@߮@�
=@�E�@ܛ�@�+@�ȴ@��#@�`B@�j@�o@��H@�v�@�`B@Դ9@��;@�S�@�^5@��@��@�l�@��H@��@�1'@ʧ�@���@�%@ȃ@�9X@�9X@�1'@ǶF@�5?@�p�@��`@��/@�1'@�Z@Ý�@�33@\@�E�@���@�G�@�Ĝ@���@�I�@��F@�ff@��^@��@���@��F@���@��/@��@�Q�@��9@��-@�J@�ff@��-@��-@�O�@�/@�7L@�?}@��@�r�@��@��+@�-@���@�G�@���@���@�n�@��@���@�ƨ@�@���@�v�@�^5@�{@�X@�%@��9@��
@�33@��+@�^5@�$�@���@��h@��#@�{@�E�@�^5@�n�@�E�@�@��-@�p�@�O�@�/@���@��@�Ĝ@��@�Q�@�b@��
@�;d@�V@��@��@���@�\)@���@��+@���@�`B@�%@���@���@�Ĝ@��/@�p�@�hs@��/@��u@��D@���@��D@�z�@�j@�Z@�(�@���@�  @��
@��y@�V@�J@�@��@��h@�O�@�7L@�&�@��@��`@���@�(�@�"�@��!@�V@���@��@��#@�x�@���@���@���@��@�Ĝ@��@��D@�1@���@�|�@�
=@�n�@�$�@��@��#@���@��h@�`B@�/@��@��@�%@�r�@��m@���@���@�^5@���@�hs@�x�@�`B@��@���@���@�V@��@�Q�@�9X@�bN@�r�@�j@�r�@�A�@�(�@�9X@�9X@�9X@��@���@�l�@�"�@�@���@�@�ȴ@���@��+@�=q@���@��@���@���@�x�@�O�@�V@���@�bN@�(�@�ƨ@�dZ@�"�@��v@s|�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�%A�%A�%A�1A�G�A�p�A�ZA�"�A���A��/Aȡ�A�p�A�VA�jAȃA�p�A�S�A�C�A�C�A�E�A�=qA�;dA�;dA�G�A�Q�A�VA�^5Aȝ�A��A��/A��#A���AȋDA�^5A��A�A�A�ȴA��AŬA�v�A�&�Aã�A�%A�oA�t�A���A���A�M�A�;dA��mA�t�A��A��
A�ƨA��A��A�33A��A���A�VA��A��TA�`BA��A�bNA�1'A��!A��hA��\A��A��;A��9A�C�A��A�v�A�M�A��A�/A��9A�"�A�&�A�-A���A�+A��DA� �A�A�VA�Q�A�ĜA���A�Q�A��PA���A�A�A��^A�ȴA�(�A�G�A�jA��A��jA���A�r�A���A��RA�M�A�hsA�M�A��A|E�Ayl�AyS�AvȴAr��Ar�Apv�Ao�7An~�Alr�Ak�TAk+Aj1'AhjAg&�Ad��Aa�A`�A\�yA[`BAY�PAX�AV$�AS�AR��ARJANA�AJbNAI�AH��AH�RAH��AHAF��AEx�AD�ADbAC�hAB��AAl�A?ƨA?;dA=��A<�RA<M�A<  A;dZA:�A9�;A7�A7VA4�jA3G�A2�A1XA0��A0��A0r�A/�A/�A.��A-O�A,�A+�A*jA(��A'�#A&�yA$��A#O�A ��AQ�AG�AA�A��AS�A"�A��A-A�\A�A%AĜAbNA�#A�A�TA"�AE�A"�A
~�A	�TA	G�A��Av�AhsAȴA��An�A�A��A�`A�\A�A n�A J@��@���@��@���@�z�@�+@�r�@���@���@��@�@��H@���@��-@�A�@��@�v�@��T@��;@���@�F@�E�@�&�@�1'@߮@�
=@�E�@ܛ�@�+@�ȴ@��#@�`B@�j@�o@��H@�v�@�`B@Դ9@��;@�S�@�^5@��@��@�l�@��H@��@�1'@ʧ�@���@�%@ȃ@�9X@�9X@�1'@ǶF@�5?@�p�@��`@��/@�1'@�Z@Ý�@�33@\@�E�@���@�G�@�Ĝ@���@�I�@��F@�ff@��^@��@���@��F@���@��/@��@�Q�@��9@��-@�J@�ff@��-@��-@�O�@�/@�7L@�?}@��@�r�@��@��+@�-@���@�G�@���@���@�n�@��@���@�ƨ@�@���@�v�@�^5@�{@�X@�%@��9@��
@�33@��+@�^5@�$�@���@��h@��#@�{@�E�@�^5@�n�@�E�@�@��-@�p�@�O�@�/@���@��@�Ĝ@��@�Q�@�b@��
@�;d@�V@��@��@���@�\)@���@��+@���@�`B@�%@���@���@�Ĝ@��/@�p�@�hs@��/@��u@��D@���@��D@�z�@�j@�Z@�(�@���@�  @��
@��y@�V@�J@�@��@��h@�O�@�7L@�&�@��@��`@���@�(�@�"�@��!@�V@���@��@��#@�x�@���@���@���@��@�Ĝ@��@��D@�1@���@�|�@�
=@�n�@�$�@��@��#@���@��h@�`B@�/@��@��@�%@�r�@��m@���@���@�^5@���@�hs@�x�@�`B@��@���@���@�V@��@�Q�@�9X@�bN@�r�@�j@�r�@�A�@�(�@�9X@�9X@�9X@��@���@�l�@�"�@�@���@�@�ȴ@���@��+@�=q@���@��@���@���@�x�@�O�@�V@���@�bN@�(�@�ƨ@�dZ@�"�@��v@s|�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
��B
��B
��B
��BbB33BB�BD�BF�BH�BVBcTBo�Bu�B}�B~�B|�By�By�By�Bw�Bv�By�B�%B�PB�hB��B�B��B�BB�BB(�B:^B6FB.B#�B�B�B�B!�B�BDB�BbBbBoB�B.B\)Bz�B�%B�=B�DB�JB�PB�bB�uB�uB��B��B��B��B��B��B��B�{B�oB�JB�Br�BffB_;BM�B7LB�BJB��B�B�TB��B��B��B�DB~�BhsBn�BjB^5BT�BC�B(�B�BJBPB\B,B�B
�B
��B
��B
ǮB
�jB
��B
�B
o�B
T�B
E�B
9XB
VB
A�B
(�B
.B
�B
1B
%B	��B	�B	�`B	�B	��B	��B	ÖB	�RB	�B	��B	�VB	�B	o�B	e`B	YB	O�B	A�B	5?B	.B	'�B	�B	JB	%B	B	B	B	B	  B��B��B��B�B�B�fB�5B�#B�B��B��B��B��B��BȴBĜB�}B�RB�!B�B�B��B��B��B��B��B��B��B��B��B�{B��B��B��B�oB�VB�B|�Br�Bl�Bk�BjBjBiyBgmBdZBdZBdZBdZBcTBaHB^5B\)BXBS�BS�B[#B^5B_;B_;B`BB^5B\)B\)B]/B^5B^5B_;BaHBe`Be`BdZBe`BffBhsBiyBiyBgmBiyBhsBhsBffBffBhsBk�Bo�Br�Bt�Bs�Bs�Bs�Br�Bq�Bs�Bw�B|�B� B�B�%B�7B�7B�PB�uB��B��B��B��B��B��B��B��B��B��B�B�9B�RB�^B�^B�^B�^B�dB�jB�qB�wB�wB�wB�}BBĜBȴB��B��B��B��B�
B�#B�/B�5B�BB�`B�sB�yB�B�B�yB�NB�B��B��B��B��B��B�;B�yB�B�B��B	B	%B	JB	oB	{B	�B	�B	�B	�B	�B	�B	�B	!�B	"�B	%�B	(�B	-B	0!B	49B	6FB	7LB	7LB	8RB	:^B	;dB	;dB	>wB	B�B	F�B	I�B	M�B	Q�B	S�B	W
B	YB	[#B	\)B	^5B	cTB	e`B	ffB	ffB	gmB	gmB	iyB	q�B	y�B	|�B	� B	�B	�B	�B	�B	~�B	}�B	�B	�B	�B	�%B	�B	�B	�B	�+B	�1B	�1B	�=B	�\B	�hB	�hB	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�'B	�9B	�FB	�FB	�FB	�LB	�R@E�-B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
1B
1B
1B
1B
	7B
DB
DB
JB
JB
JB
JB
JB
PB
PB
PB
PB
PB
VB
\B
\B
\B
\B
bB
bB
\B
oB
oB
oB
oB
oB
oB
oB
oB
oB
uB
uB
uB
9B
$�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111  B
��B
��B
��B
��BbB33BB�BD�BF�BH�BVBcTBo�Bu�B}�B~�B|�By�By�By�Bw�Bv�By�B�%B�PB�hB��B�B��B�BB�BB(�B:^B6FB.B#�B�B�B�B!�B�BDB�BbBbBoB�B.B\)Bz�B�%B�=B�DB�JB�PB�bB�uB�uB��B��B��B��B��B��B��B�{B�oB�JB�Br�BffB_;BM�B7LB�BJB��B�B�TB��B��B��B�DB~�BhsBn�BjB^5BT�BC�B(�B�BJBPB\B,B�B
�B
��B
��B
ǮB
�jB
��B
�B
o�B
T�B
E�B
9XB
VB
A�B
(�B
.B
�B
1B
%B	��B	�B	�`B	�B	��B	��B	ÖB	�RB	�B	��B	�VB	�B	o�B	e`B	YB	O�B	A�B	5?B	.B	'�B	�B	JB	%B	B	B	B	B	  B��B��B��B�B�B�fB�5B�#B�B��B��B��B��B��BȴBĜB�}B�RB�!B�B�B��B��B��B��B��B��B��B��B��B�{B��B��B��B�oB�VB�B|�Br�Bl�Bk�BjBjBiyBgmBdZBdZBdZBdZBcTBaHB^5B\)BXBS�BS�B[#B^5B_;B_;B`BB^5B\)B\)B]/B^5B^5B_;BaHBe`Be`BdZBe`BffBhsBiyBiyBgmBiyBhsBhsBffBffBhsBk�Bo�Br�Bt�Bs�Bs�Bs�Br�Bq�Bs�Bw�B|�B� B�B�%B�7B�7B�PB�uB��B��B��B��B��B��B��B��B��B��B�B�9B�RB�^B�^B�^B�^B�dB�jB�qB�wB�wB�wB�}BBĜBȴB��B��B��B��B�
B�#B�/B�5B�BB�`B�sB�yB�B�B�yB�NB�B��B��B��B��B��B�;B�yB�B�B��B	B	%B	JB	oB	{B	�B	�B	�B	�B	�B	�B	�B	!�B	"�B	%�B	(�B	-B	0!B	49B	6FB	7LB	7LB	8RB	:^B	;dB	;dB	>wB	B�B	F�B	I�B	M�B	Q�B	S�B	W
B	YB	[#B	\)B	^5B	cTB	e`B	ffB	ffB	gmB	gmB	iyB	q�B	y�B	|�B	� B	�B	�B	�B	�B	~�B	}�B	�B	�B	�B	�%B	�B	�B	�B	�+B	�1B	�1B	�=B	�\B	�hB	�hB	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�'B	�9B	�FB	�FB	�FB	�LB	�R@E�-B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
1B
1B
1B
1B
	7B
DB
DB
JB
JB
JB
JB
JB
PB
PB
PB
PB
PB
VB
\B
\B
\B
\B
bB
bB
\B
oB
oB
oB
oB
oB
oB
oB
oB
oB
uB
uB
uB
9B
$�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.19 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181024140847                              AO  ARCAADJP                                                                    20181024140847    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181024140847  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181024140847  QCF$                G�O�G�O�G�O�0               