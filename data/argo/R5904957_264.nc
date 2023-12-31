CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-24T14:08:55Z creation      
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20181024140855  20181024140855  5904957 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL              A   AO  6560                            2B  A   APEX                            7471                            062512                          846 @�񤴇Q1   @��I���@5�M����c�����1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                     A   A   A   @�ff@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�33A�  B   B  B  B��B   B(  B/��B7��B@  BH  BP  BX  B`  Bh  BpffBx  B�  B���B���B�  B�  B�  B�  B�  B�  C �C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP�CR  CT  CV  CX  CZ  C\  C]�fC`  Cb  Cd  Cf  Ch  Cj  Cl�Cn  Cp  Cr�Ct  Cv  Cx  Cz  C|  C~�C�  C�  C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C��3C��3C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� DfD� D	  D	� D
  D
� D  D� D  D� D  D� D  Dy�D  Dy�D  D� D  D� D  D� D��D� D  D�fD  D� D��D� D  D� D��Dy�D  D� D  Dy�D��D� D  D� D  D�fD  D�fD  D� D   D � D ��D!� D"fD"� D#  D#y�D#��D$� D%  D%� D&  D&y�D&��D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-�fD.  D.� D/  D/� D0  D0� D1  D1� D2  D2�fD3  D3� D4  D4� D5  D5� D6  D6�fD7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DBfDB� DC  DC�fDD  DDy�DE  DEy�DE��DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DLy�DM  DM� DN  DN� DOfDO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DTy�DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[fD[� D[��D\� D]  D]� D^  D^� D_  D_y�D_��D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df�fDg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dmy�Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dvy�Dw  Dw�fDy��D�-D��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�z�@�{A
=A#
=AC
=Ac
=A��A��A��A��A��AхA�RA�B BBB\)B B(B0\)B8\)B@BHBPBXB`BhBq(�BxB�aHB�.B�.B�aHB�aHB�aHB�aHB�aHB�aHC J>C0�C0�C0�C0�C
0�C0�C0�C0�C0�C0�C0�C0�C0�C0�C0�C 0�C"0�C$0�C&0�C(0�C*0�C,0�C.0�C00�C20�C40�C60�C80�C:0�C<0�C>0�C@0�CB0�CD0�CF0�CH0�CJ0�CL0�CN0�CPJ>CR0�CT0�CV0�CX0�CZ0�C\0�C^
C`0�Cb0�Cd0�Cf0�Ch0�Cj0�ClJ>Cn0�Cp0�CrJ>Ct0�Cv0�Cx0�Cz0�C|0�C~J>C�RC�RC��C��C�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC��C�RC�RC�RC�RC�RC�RC�RC��C��C�RC�RC�RC�RC�RC�%C�RC�RC�RC�RC�RC�RC�%C�RC�RC�RC�RC�RC�RC�%C�RC�RC�RC�RC�RC�RC��C�RC�RC�RC��C��C�RC�RC�RC�RC�RC�RC�%C�RC�RC�RC�RC�RC�RC�RC��C�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�%C�%C�RC�RC�RC�RC�RC�RC�RC��C�RC�RC�RC�RC�RC�RC�RC�RC�RC�%C�RC�RC�RC�%C�RC�RC�RC�RC�RC�RC�RC�RC�RD )D �)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D�D�)D	)D	�)D
)D
�)D)D�)D)D�)D)D�)D)D��D)D��D)D�)D)D�)D)D�)D�D�)D)D��D)D�)D�D�)D)D�)D�D��D)D�)D)D��D�D�)D)D�)D)D��D)D��D)D�)D )D �)D!�D!�)D"�D"�)D#)D#��D$�D$�)D%)D%�)D&)D&��D'�D'�)D()D(�)D))D)�)D*)D*�)D+)D+�)D,)D,�)D-)D-��D.)D.�)D/)D/�)D0)D0�)D1)D1�)D2)D2��D3)D3�)D4)D4�)D5)D5�)D6)D6��D7)D7�)D8)D8�)D9)D9�)D:)D:�)D;)D;�)D<)D<�)D=)D=�)D>)D>�)D?)D?�)D@)D@�)DA)DA�)DB�DB�)DC)DC��DD)DD��DE)DE��DF�DF�)DG)DG�)DH)DH�)DI)DI�)DJ)DJ�)DK)DK�)DL)DL��DM)DM�)DN)DN�)DO�DO�)DP)DP�)DQ)DQ�)DR)DR�)DS)DS�)DT)DT��DU)DU�)DV)DV�)DW)DW�)DX)DX�)DY)DY�)DZ)DZ�)D[�D[�)D\�D\�)D])D]�)D^)D^�)D_)D_��D`�D`�)Da)Da�)Db)Db�)Dc)Dc�)Dd)Dd�)De)De�)Df)Df��Dg)Dg�)Dh)Dh�)Di)Di�)Dj)Dj�)Dk)Dk�)Dl)Dl�)Dm)Dm��Dn)Dn�)Do)Do�)Dp)Dp�)Dq)Dq�)Dr)Dr�)Ds)Ds�)Dt)Dt�)Du)Du�)Dv)Dv��Dw)Dw��Dy��D�33D� �111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�bA�bA�bA�bA�VA�bA�bA�
=A���A���A��RA��A��DA��9A�jA�XA�;dA�$�A��A�VA�A���A��A��A��A���A�5?A��`A��/A��TA��
A��+A�=qA��A���A�ĜA�ƨA�t�A�C�A��uA�%A�{A��A�A��A�A�VA��/A���A�?}A�VA��A��A�$�A��FA�9XA��wA��yA��`A�33A��RA�ZA���A�(�A�A�oA�S�A��A�ƨA�%A��HA���A��wA��hA�C�A��#A�  A��hA��A���A�K�A��TA��-A�hsA�#A~��A~Az��An �Ak�^AkC�AkVAj�uAiG�AhĜAhz�Agt�Af�!AeVAb9XA`$�A]ƨA\z�A[��AZ��AY�
AV5?AUG�AU/AU&�AU"�AT^5AR�AR1APZAOdZAM�^AL1AKhsAJ1'AI��AI|�AH9XAD��AB�/AB�DABr�AA�A@n�A>��A=�A<��A:5?A6{A2�`A1��A0�A0I�A.�HA-�hA,�A+VA)��A)�A'C�A&�DA$��A#?}A!��A E�A-Av�A�+A�AK�An�A��AXA��A�A�DAp�AhsA33A=qA�AE�A��A�9AXA
JAQ�A�PA�A�/A9XAĜA �A��AjA��AƨA�7AC�@��w@��+@��@�X@��9@�bN@��w@�^5@���@��\@�{@�A�@�o@@�n�@�u@�A�@�@�R@��@�^@�X@��m@���@�bN@��
@ާ�@��@�@�x�@��@��m@ّh@�1@׾w@ו�@�dZ@��y@�{@ՙ�@��`@���@�K�@��H@�^5@�J@��T@д9@Ͼw@��@�v�@�@̛�@�|�@�33@ʸR@�{@�hs@�bN@�
=@��#@�7L@��@���@öF@�n�@���@�Z@��m@���@�C�@�C�@�@�@� �@�ƨ@�K�@�-@���@�7L@��@�j@��w@�K�@��R@�-@��#@��-@�7L@�r�@���@���@��@���@��h@�O�@�z�@�b@���@��@�|�@�33@��@���@�=q@��T@���@�G�@�&�@��@�%@��j@�I�@�(�@��@��@��+@�M�@�5?@�-@�-@�-@�$�@��@�{@��T@���@�/@���@��@���@�ȴ@��+@�V@��@�Q�@��@��H@��+@��@�G�@�V@��@���@��@�v�@�^5@�@���@��@���@�K�@�"�@�
=@��y@��y@��H@���@�ff@�{@�@���@�p�@�G�@�V@�Ĝ@��D@�I�@�(�@�1@���@�C�@�
=@�ȴ@���@�n�@�5?@�J@���@�p�@�X@�X@�V@��j@��u@�(�@�  @��m@�l�@�+@��H@���@���@�-@�@��@��^@�x�@�p�@�`B@�O�@�?}@�/@�&�@�r�@���@�|�@�l�@�\)@�33@���@��\@�ff@�5?@�J@�@��T@��#@�@��@�X@��@�bN@�j@�Q�@�1'@�(�@� �@��@�1@���@��;@��@��H@��+@�v�@��@��T@��^@��h@�hs@�?}@��@��/@���@�Z@�b@���@�+@��@�v�@�5?@��T@�G�@��/@��@��@�bN@�A�@�9X@�(�@���@��y@���@�E�@�$�@�@��@�`B@��@��j@��u@�z�@�bN@�Z@�Q�@�A�@�w@~��@~v�@~ff@}�@}�h@}`B@}�@|�/@|9X@{(@r�!@\|�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�bA�bA�bA�bA�VA�bA�bA�
=A���A���A��RA��A��DA��9A�jA�XA�;dA�$�A��A�VA�A���A��A��A��A���A�5?A��`A��/A��TA��
A��+A�=qA��A���A�ĜA�ƨA�t�A�C�A��uA�%A�{A��A�A��A�A�VA��/A���A�?}A�VA��A��A�$�A��FA�9XA��wA��yA��`A�33A��RA�ZA���A�(�A�A�oA�S�A��A�ƨA�%A��HA���A��wA��hA�C�A��#A�  A��hA��A���A�K�A��TA��-A�hsA�#A~��A~Az��An �Ak�^AkC�AkVAj�uAiG�AhĜAhz�Agt�Af�!AeVAb9XA`$�A]ƨA\z�A[��AZ��AY�
AV5?AUG�AU/AU&�AU"�AT^5AR�AR1APZAOdZAM�^AL1AKhsAJ1'AI��AI|�AH9XAD��AB�/AB�DABr�AA�A@n�A>��A=�A<��A:5?A6{A2�`A1��A0�A0I�A.�HA-�hA,�A+VA)��A)�A'C�A&�DA$��A#?}A!��A E�A-Av�A�+A�AK�An�A��AXA��A�A�DAp�AhsA33A=qA�AE�A��A�9AXA
JAQ�A�PA�A�/A9XAĜA �A��AjA��AƨA�7AC�@��w@��+@��@�X@��9@�bN@��w@�^5@���@��\@�{@�A�@�o@@�n�@�u@�A�@�@�R@��@�^@�X@��m@���@�bN@��
@ާ�@��@�@�x�@��@��m@ّh@�1@׾w@ו�@�dZ@��y@�{@ՙ�@��`@���@�K�@��H@�^5@�J@��T@д9@Ͼw@��@�v�@�@̛�@�|�@�33@ʸR@�{@�hs@�bN@�
=@��#@�7L@��@���@öF@�n�@���@�Z@��m@���@�C�@�C�@�@�@� �@�ƨ@�K�@�-@���@�7L@��@�j@��w@�K�@��R@�-@��#@��-@�7L@�r�@���@���@��@���@��h@�O�@�z�@�b@���@��@�|�@�33@��@���@�=q@��T@���@�G�@�&�@��@�%@��j@�I�@�(�@��@��@��+@�M�@�5?@�-@�-@�-@�$�@��@�{@��T@���@�/@���@��@���@�ȴ@��+@�V@��@�Q�@��@��H@��+@��@�G�@�V@��@���@��@�v�@�^5@�@���@��@���@�K�@�"�@�
=@��y@��y@��H@���@�ff@�{@�@���@�p�@�G�@�V@�Ĝ@��D@�I�@�(�@�1@���@�C�@�
=@�ȴ@���@�n�@�5?@�J@���@�p�@�X@�X@�V@��j@��u@�(�@�  @��m@�l�@�+@��H@���@���@�-@�@��@��^@�x�@�p�@�`B@�O�@�?}@�/@�&�@�r�@���@�|�@�l�@�\)@�33@���@��\@�ff@�5?@�J@�@��T@��#@�@��@�X@��@�bN@�j@�Q�@�1'@�(�@� �@��@�1@���@��;@��@��H@��+@�v�@��@��T@��^@��h@�hs@�?}@��@��/@���@�Z@�b@���@�+@��@�v�@�5?@��T@�G�@��/@��@��@�bN@�A�@�9X@�(�@���@��y@���@�E�@�$�@�@��@�`B@��@��j@��u@�z�@�bN@�Z@�Q�@�A�@�w@~��@~v�@~ff@}�@}�h@}`B@}�@|�/@|9X@{(@r�!@\|�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B��B��B��B��B��B��B��B��B��B�B�B�B��BB+B+B%B%BB	7BDBPB\BoB+B1'B1'B=qBC�BM�Bu�Bo�BjBk�Bm�Bq�Br�Br�Bq�Bq�Bn�Bl�BgmB]/BXBQ�BH�B>wB1'B+B�BB�ZBĜB�'B��B��B��B�JB�By�BffB9XB"�BoB
��B
�B
�/B
ɺB
�wB
�B
��B
~�B
{�B
x�B
s�B
iyB
cTB
[#B
R�B
L�B
G�B
B�B
>wB
7LB
0!B
(�B
uB	��B	ĜB	��B	�}B	�jB	�LB	�9B	�-B	�B	��B	��B	�+B	x�B	l�B	dZB	`BB	ZB	S�B	E�B	@�B	?}B	?}B	=qB	9XB	2-B	-B	'�B	!�B	�B	�B	{B	hB	VB	DB	B��B�B�B�B�B�ZB�;B�#B��BɺB�jB�3B�B�B��B��B��B��B��B�oB�\B�DB�DB�+B�B�B~�Bz�Bu�Bo�Bk�BgmBdZBbNB`BB\)BYBT�BR�BS�BT�BS�BR�BQ�BT�BbNBffBe`BdZBdZBe`BdZBdZBgmBgmBhsBhsBgmBffBe`BcTBffBgmBhsBiyBk�Bm�Bp�Br�Bp�Bk�Bk�BiyBiyBiyBhsBe`BffBe`BffBgmBgmBgmBjBs�Bt�Bu�Bw�Bx�Bx�Bx�Bx�Bx�B|�B� B�B�B�B�B�+B�1B�=B�PB�VB�\B�bB�bB�bB�uB��B��B��B��B��B��B��B��B��B��B�-B�}BÖBƨBǮBȴBǮBǮBƨBƨBŢBŢB��B��B��B��B�)B�5B�BB�`B�yB�B�B�B�B��B��B��B��B��B��B��B��B	B	B	B	B	B	+B	1B		7B	
=B	
=B	PB	\B	hB	�B	�B	�B	!�B	#�B	$�B	$�B	'�B	-B	/B	33B	9XB	=qB	D�B	F�B	G�B	H�B	H�B	I�B	I�B	J�B	K�B	M�B	N�B	Q�B	S�B	S�B	VB	W
B	W
B	W
B	\)B	]/B	_;B	`BB	aHB	dZB	e`B	gmB	k�B	m�B	o�B	p�B	r�B	x�B	|�B	}�B	�B	�B	�B	�%B	�%B	�+B	�+B	�=B	�VB	�hB	�oB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�-B	�3B	�3B	�?B	�FB	�LB	�LB	�RB	�RB	�RB	�RB	�qB	B	ĜB	ŢB	ŢB	ƨB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�
B	�B	�B	�#B	�)B	�/B	�/B	�5B	�;B	�HB	�NB	�NB	�ZB	�`B	�fB	�mB	�sB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
SB
_B
(�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B��B��B��B��B��B��B��B��B��B�B�B�B��BB+B+B%B%BB	7BDBPB\BoB+B1'B1'B=qBC�BM�Bu�Bo�BjBk�Bm�Bq�Br�Br�Bq�Bq�Bn�Bl�BgmB]/BXBQ�BH�B>wB1'B+B�BB�ZBĜB�'B��B��B��B�JB�By�BffB9XB"�BoB
��B
�B
�/B
ɺB
�wB
�B
��B
~�B
{�B
x�B
s�B
iyB
cTB
[#B
R�B
L�B
G�B
B�B
>wB
7LB
0!B
(�B
uB	��B	ĜB	��B	�}B	�jB	�LB	�9B	�-B	�B	��B	��B	�+B	x�B	l�B	dZB	`BB	ZB	S�B	E�B	@�B	?}B	?}B	=qB	9XB	2-B	-B	'�B	!�B	�B	�B	{B	hB	VB	DB	B��B�B�B�B�B�ZB�;B�#B��BɺB�jB�3B�B�B��B��B��B��B��B�oB�\B�DB�DB�+B�B�B~�Bz�Bu�Bo�Bk�BgmBdZBbNB`BB\)BYBT�BR�BS�BT�BS�BR�BQ�BT�BbNBffBe`BdZBdZBe`BdZBdZBgmBgmBhsBhsBgmBffBe`BcTBffBgmBhsBiyBk�Bm�Bp�Br�Bp�Bk�Bk�BiyBiyBiyBhsBe`BffBe`BffBgmBgmBgmBjBs�Bt�Bu�Bw�Bx�Bx�Bx�Bx�Bx�B|�B� B�B�B�B�B�+B�1B�=B�PB�VB�\B�bB�bB�bB�uB��B��B��B��B��B��B��B��B��B��B�-B�}BÖBƨBǮBȴBǮBǮBƨBƨBŢBŢB��B��B��B��B�)B�5B�BB�`B�yB�B�B�B�B��B��B��B��B��B��B��B��B	B	B	B	B	B	+B	1B		7B	
=B	
=B	PB	\B	hB	�B	�B	�B	!�B	#�B	$�B	$�B	'�B	-B	/B	33B	9XB	=qB	D�B	F�B	G�B	H�B	H�B	I�B	I�B	J�B	K�B	M�B	N�B	Q�B	S�B	S�B	VB	W
B	W
B	W
B	\)B	]/B	_;B	`BB	aHB	dZB	e`B	gmB	k�B	m�B	o�B	p�B	r�B	x�B	|�B	}�B	�B	�B	�B	�%B	�%B	�+B	�+B	�=B	�VB	�hB	�oB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�-B	�3B	�3B	�?B	�FB	�LB	�LB	�RB	�RB	�RB	�RB	�qB	B	ĜB	ŢB	ŢB	ƨB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�
B	�B	�B	�#B	�)B	�/B	�/B	�5B	�;B	�HB	�NB	�NB	�ZB	�`B	�fB	�mB	�sB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
SB
_B
(�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.19 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181024140855                              AO  ARCAADJP                                                                    20181024140855    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181024140855  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181024140855  QCF$                G�O�G�O�G�O�0               