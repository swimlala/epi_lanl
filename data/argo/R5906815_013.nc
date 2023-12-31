CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2022-10-20T10:01:43Z creation      
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
resolution        =���   axis      Z        l  9p   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  H�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     l  L�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \$   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     l  `    TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     l  ol   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ~�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     l  ��   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �    TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     l  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     l  �h   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     l  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     l  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �d   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ۔   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ޔ   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �    HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �$   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �4   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �8   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �<   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �@Argo profile    3.1 1.2 19500101000000  20221020100143  20221020100143  5906815 Argo PMEL                                                       GREGORY C. JOHNSON                                              PRES            TEMP            PSAL               A   AO  8808                            2B  A   NAVIS_A                         1396                            170425                          863 @���Y�_�1   @����>�F@7Z�G�{�c��G�{1   GPS     Primary sampling: averaged                                                                                                                                                                                                                                         A   A   A   @�33@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  C   C  C  C  C  C
  C�C�C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DGy�DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D���D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�C3D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @�G�@�{A
=A#
=AC
=Ac
=A��A��A��A��A��AхA�A�B BBBB B(B0B8B@BHBPBXB`BhBpBxB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�.B�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB��{B�aHC 0�C0�C0�C0�C0�C
0�CJ>CJ>C0�C0�C0�C0�C0�C0�C0�C0�C 0�C"0�C$0�C&0�C(0�C*0�C,0�C.0�C00�C20�C40�C60�C80�C:0�C<0�C>0�C@0�CB0�CD0�CF0�CH0�CJ0�CL0�CN0�CP0�CR0�CT0�CV0�CX0�CZ0�C\0�C^0�C`0�Cb0�Cd0�Cf0�Ch0�Cj0�Cl0�Cn0�Cp0�Cr0�Ct0�Cv0�Cx0�Cz0�C|0�C~0�C�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�%C�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RD )D �)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D	)D	�)D
)D
�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D )D �)D!)D!�)D")D"�)D#)D#�)D$)D$�)D%)D%�)D&)D&�)D')D'�)D()D(�)D))D)�)D*)D*�)D+)D+�)D,)D,�)D-)D-�)D.)D.�)D/)D/�)D0)D0�)D1)D1�)D2)D2�)D3)D3�)D4)D4�)D5)D5�)D6)D6�)D7)D7�)D8)D8�)D9)D9�)D:)D:�)D;)D;�)D<)D<�)D=)D=�)D>)D>�)D?)D?�)D@)D@�)DA)DA�)DB)DB�)DC)DC�)DD)DD�)DE)DE�)DF)DF�)DG)DG��DH)DH�)DI)DI�)DJ)DJ�)DK)DK�)DL)DL�)DM)DM�)DN)DN�)DO)DO�)DP)DP�)DQ)DQ�)DR)DR�)DS)DS�)DT)DT�)DU)DU�)DV)DV�)DW)DW�)DX)DX�)DY)DY�)DZ)DZ�)D[)D[�)D\)D\�)D])D]�)D^)D^�)D_)D_�)D`)D`�)Da)Da�)Db)Db�)Dc)Dc�)Dd)Dd�)De)De�)Df)Df�)Dg)Dg�)Dh)Dh�)Di)Di�)Dj)Dj�)Dk)Dk�)Dl)Dl�)Dm)Dm�)Dn)Dn�)Do)Do�)Dp)Dp�)Dq)Dq�)Dr)Dr�)Ds)Ds�)Dt)Dt�)Du)Du�)Dv)Dv�)Dw)Dw�)Dx)Dx�)Dy)Dy�)Dz)Dz�)D{)D{�)D|)D|�)D})D}�)D~)D~�)D)D�)D�D�FD��D��D�D�FD��D��D�D�FD��D��D�D�FD��D��D�D�FD��D��D�D�FD��D��D�D�FD��D��D�D�FD��D��D�D�FD��D��D�D�FD��D��D�D�FD��D��D�D�FD��D��D�D�FD��D��D�D�FD��D��D�D�FD��D��D�D�FD��D��D�D�FD��D��D�D�FD��D��D�D�FD��D��D�D�FD��D��D�D�FD��D��D�D�FD��D��D�D�FD��D��D�D�FD��D��D�D�FD��D��D�D�FD��D��D�D�FD��D��D�D�FD��D��D�D�FD��D��D��D�FD��D��D�D�FD��D��D�D�FD��D��D�D�FD��D��D�D�FD��D��D�D�FD��D��D�D�FD��D��D�D�FD��D��D�D�FD��D��D�D�FD��D��D�D�FD��D��D�D�FD��D��D�D�FD��D��D�D�FD��D��D�D�FD��D��D�D�FD��D��D�D�FD��D��D�D�FD��D��D�D�FD��D��D�D�FD��D��D�D�FD��D��D�D�FD��D��D�D�FD��D��D�D�FD��D��D�D�FD��D��D�D�FD��D��D�D�FD��D��D�D�FD��D��D�D�FD��D��D�D�FD��D��D�D�FD��D��D�D�FD��D��D�D�FD��D��D�D�FD��D��D�D�FD��D��D�D�FD��D��D�D�FD��D��D�D�FDD��D�D�FDÆD��D�D�FDĆD��D�D�FDņD��D�D�FDƆD��D�D�FDǆD��D�D�FDȆD��D�D�FDɆD��D�D�FDʆD��D�D�FDˆD��D�D�FD̆D��D�D�FD͆D��D�D�FDΆD��D�D�FDφD��D�D�FDІD��D�D�FDцD��D�D�FD҆D��D�D�FDӆD��D�D�FDԆD��D�D�FDՆD��D�D�FDֆD��D�D�FD׆D��D�D�FD؆D��D�D�FDنD��D�D�FDچD��D�D�FDۆD��D�D�FD܆D��D�D�FD݆D��D�D�FDކD��D�D�FD߆D��D�D�FD��D��D�D�FD�D��D�D�FD�D��D�D�FD�D��D�D�FD�D��D�D�FD�D��D�D�FD�D��D�D�FD�D��D�D�FD�D��D�D�FD�D��D�D�FD�D��D�D�FD�D��D�D�IGD�D��D�D�FD�D��D�D�FD�D��D�D�FD�D��D�D�FD��D��D�D�FD�D��D�D�FD�D��D�D�FD�D��D�D�FD�D��D�D�FD��D��D�D�FD��D��D��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�AדuA׏\A׏\A׏\A׏\AבhAבhAבhAדuAדuAבhA׏\AדuA׏\AבhA׉7A׉7A׋DA׏\A׉7A׍PA׉7A׋DA׉7A׉7A�hsAґhA��A�bNA�~�AʃA��Ař�A�`BA�t�A���A�ƨA�t�A��`A���A��7A�bA�z�A��+A�  A���A���A�t�A�A���A��HA���A���A�dZA��A�5?A���A���A�A�A��/A�VA��PA�A�A�?}A� �A��A���A�O�A��FA��HA�VA� �A���A�`BA���A��A�"�A��!A��PA���A��\A��RA���A���A�^5A�  A���A�  A��`A���A�=qA���A��\A�v�A�M�A��
A�l�A��^A��yA�7LA��RA�/A��TA��A�VA���A�hsA��`A��A�A�A�K�A�p�A��A� �A�5?A���A�|�A�A��A�A}|�Az�yAx�jAv�As��Ap�yAnAmK�Al�9Ajz�AgVAe�FAd  AbZA_x�A^(�A\��AZ�!AY�
AWO�ATbNAQ�
AN�`AMt�AMVAK�7AJ�HAJ��AJ��AI��AGAE�hAD-AC�AA�A>�A=�A<M�A;S�A:�+A9��A8��A7�A7G�A6~�A5G�A3/A1�^A0��A0E�A/�A.v�A-/A,�A+|�A*�HA* �A)dZA(ĜA(I�A'�hA%�-A$��A$�!A$ZA$=qA#�hA"M�A!�A 5?A��A��AJAG�A�uAJA+AE�AAhsA��A1'A�+A��A�+AbNA;dAE�AAI�A�hA
Q�A	;dA��AbNA��AbNAI�AƨAC�A��AbNA��A��A(�A�A 1@��
@��@���@�p�@�p�@�&�@�C�@�G�@�Q�@�+@��T@�@��@�(�@�dZ@�!@�ff@�M�@�{@���@�h@�O�@��
@�\@���@�u@�l�@噚@��
@�E�@��T@�?}@���@�33@��#@ۍP@ڸR@٩�@ف@�`B@�V@�\)@Չ7@�A�@ӶF@���@�$�@Ѻ^@��@��
@�ff@�E�@�7L@�Q�@��@�@�%@��@�ƨ@Ǖ�@�n�@ļj@�(�@�%@�K�@��@�?}@�V@��`@��u@��@�^5@�x�@���@�o@��@�@�O�@�bN@� �@���@�33@��+@��@���@�b@�C�@��@�@�/@�Ĝ@�r�@��
@��R@��`@� �@��
@�|�@�"�@��y@�{@�hs@���@��@�C�@��@���@�33@�o@���@�ȴ@���@�v�@�-@���@�@�?}@��/@���@��@��@�r�@�A�@�b@��@���@��F@�S�@���@�~�@���@���@�$�@���@���@���@��h@�x�@��9@���@�t�@�;d@��T@��@�hs@�G�@�&�@���@�j@���@���@��@��@�dZ@�S�@��H@�@�
=@��@��\@�M�@�{@��T@���@���@�O�@���@��`@��@�9X@��@�1@�  @��m@��@�|�@�\)@��@��!@��+@�$�@��-@�x�@��j@�j@�b@��@�l�@�;d@�+@��@���@���@���@���@�~�@�^5@�5?@�{@�{@�@�O�@�?}@�?}@�?}@�G�@��@���@��/@��u@��@�Z@�(�@�1@��
@�+@��H@��@�
=@�"�@��@�M�@�E�@��T@��7@���@���@�J@�^5@�~�@��\@��\@���@�M�@�M�@�V@�=q@�$�@�J@���@�&�@��/@�Ĝ@��@�r�@�j@�r�@�bN@�Z@�bN@�I�@� �@� �@� �@��@�1@���@��@�l�@�K�@��!@��+@�~�@�v�@�ff@��^@�X@�7L@���@�r�@�Z@���@���@�\)@��R@�~�@�V@�$�@��@��T@��@�@��@�$�@�$�@�@��T@���@�&�@��/@��9@��@��9@��u@�I�@�(�@�@+@~��@~$�@}�@~{@~$�@}p�@|j@{�m@z�H@y�#@y��@yG�@x��@x�9@x��@x�@xbN@x �@w�@w�@w+@w�@vV@u�-@u?}@t��@t9X@s�@s"�@r�H@r��@r��@rn�@qx�@o�@o�@n�@n�@nȴ@nȴ@n{@m��@mV@lz�@l1@kƨ@k��@kS�@j�@j�@j��@i�^@iX@i%@hQ�@h  @g�;@g�@g|�@g\)@g\)@g+@f��@f�y@f$�@e�T@eO�@d�/@d��@d9X@c�
@c��@c�@cC�@co@b~�@a�#@ahs@aG�@a7L@a7L@`�`@`Ĝ@`��@`Ĝ@`�u@`r�@`1'@_l�@^�@^ff@]�@]@]�-@]�h@]�@]`B@]V@\��@[�F@[S�@[33@Z��@Z^5@Z=q@YX@X�u@XQ�@X1'@W�w@V�+@V5?@U�@Up�@UO�@U?}@UV@T��@T��@T��@T�j@T�D@S�m@SC�@S"�@S@R��@R��@Rn�@R^5@RM�@Q�@Q��@Qhs@QX@QX@Q7L@Q�@P��@P�`@P�@O��@O+@Nȴ@NE�@N{@N@M@L��@LI�@L1@K�
@K�F@KC�@J�H@J��@J�!@J�!@J�!@J��@J~�@J-@I�@I��@I�^@I��@I��@I7L@I�@H��@H�`@HĜ@H�@HA�@G�@G�P@G
=@F�y@F��@E�@E��@EO�@D�@D�D@D(�@D1@C�
@C��@CS�@CC�@C33@C"�@B�H@B�!@BM�@B�@A�#@AG�@@��@@�u@@bN@?�w@?�@?��@?��@?|�@?�@>�R@>ff@>@=�T@=��@=�-@=��@=`B@=?}@<��@<�D@<9X@<1@;�
@;��@;S�@;33@;"�@;@:��@:�\@:M�@9��@9G�@97L@9�@8�u@8Q�@8Q�@8A�@8A�@8 �@7�;@7�w@7��@7�P@7|�@7;d@7+@7+@7
=@7
=@6�@6��@6�+@6ff@5��@5`B@4�@4�@4z�@4Z@49X@3�
@3�@3t�@3dZ@333@3o@2��@2�!@2~�@2-@1��@1�#@1��@1�@0��@0�@0Q�@01'@0 �@/�w@/+@.�@.��@.V@.{@-�@-�T@-�T@-�T@-��@-��@-@-�@-`B@-`B@-O�@-�@,��@,�/@,�/@,�j@,Z@,�@+�m@+��@+S�@+"�@+@*��@*��@*M�@*-@*J@)�^@)hs@)7L@)�@)%@(�`@(��@(Ĝ@(Ĝ@(�9@(bN@'�@'��@'�@'��@'|�@'l�@';d@'
=@&�y@&ȴ@&��@&ff@&E�@&5?@%�T@%�@%V@$�/@$��@$z�@$(�@#�m@#��@#�@#�@#dZ@#C�@"�@"��@"��@"~�@"n�@"^5@"=q@"-@"�@!��@!�@!�7@ ��@ �9@ �u@ ��@ �u@ r�@ Q�@   @��@K�@+@
=@�y@�R@ff@5?@$�@@�@��@��@V@�/@z�@j@Z@Z@I�@9X@(�@�m@dZ@C�@"�@@@��@^5@M�@-@��@��@�^@hs@Ĝ@��@bN@A�@1'@b@�@�w@|�@;d@�@��@��@ff@5?@$�@{@�@@��@�@p�@/@�@�@�@�/@�/@z�@��@"�@��@�\@^5@�@�^@hs@G�@Ĝ@��@�u@bN@A�@b@|�@K�@;d@�@
=@�y@��@v�@E�@{@{111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 AדuA׏\A׏\A׏\A׏\AבhAבhAבhAדuAדuAבhA׏\AדuA׏\AבhA׉7A׉7A׋DA׏\A׉7A׍PA׉7A׋DA׉7A׉7A�hsAґhA��A�bNA�~�AʃA��Ař�A�`BA�t�A���A�ƨA�t�A��`A���A��7A�bA�z�A��+A�  A���A���A�t�A�A���A��HA���A���A�dZA��A�5?A���A���A�A�A��/A�VA��PA�A�A�?}A� �A��A���A�O�A��FA��HA�VA� �A���A�`BA���A��A�"�A��!A��PA���A��\A��RA���A���A�^5A�  A���A�  A��`A���A�=qA���A��\A�v�A�M�A��
A�l�A��^A��yA�7LA��RA�/A��TA��A�VA���A�hsA��`A��A�A�A�K�A�p�A��A� �A�5?A���A�|�A�A��A�A}|�Az�yAx�jAv�As��Ap�yAnAmK�Al�9Ajz�AgVAe�FAd  AbZA_x�A^(�A\��AZ�!AY�
AWO�ATbNAQ�
AN�`AMt�AMVAK�7AJ�HAJ��AJ��AI��AGAE�hAD-AC�AA�A>�A=�A<M�A;S�A:�+A9��A8��A7�A7G�A6~�A5G�A3/A1�^A0��A0E�A/�A.v�A-/A,�A+|�A*�HA* �A)dZA(ĜA(I�A'�hA%�-A$��A$�!A$ZA$=qA#�hA"M�A!�A 5?A��A��AJAG�A�uAJA+AE�AAhsA��A1'A�+A��A�+AbNA;dAE�AAI�A�hA
Q�A	;dA��AbNA��AbNAI�AƨAC�A��AbNA��A��A(�A�A 1@��
@��@���@�p�@�p�@�&�@�C�@�G�@�Q�@�+@��T@�@��@�(�@�dZ@�!@�ff@�M�@�{@���@�h@�O�@��
@�\@���@�u@�l�@噚@��
@�E�@��T@�?}@���@�33@��#@ۍP@ڸR@٩�@ف@�`B@�V@�\)@Չ7@�A�@ӶF@���@�$�@Ѻ^@��@��
@�ff@�E�@�7L@�Q�@��@�@�%@��@�ƨ@Ǖ�@�n�@ļj@�(�@�%@�K�@��@�?}@�V@��`@��u@��@�^5@�x�@���@�o@��@�@�O�@�bN@� �@���@�33@��+@��@���@�b@�C�@��@�@�/@�Ĝ@�r�@��
@��R@��`@� �@��
@�|�@�"�@��y@�{@�hs@���@��@�C�@��@���@�33@�o@���@�ȴ@���@�v�@�-@���@�@�?}@��/@���@��@��@�r�@�A�@�b@��@���@��F@�S�@���@�~�@���@���@�$�@���@���@���@��h@�x�@��9@���@�t�@�;d@��T@��@�hs@�G�@�&�@���@�j@���@���@��@��@�dZ@�S�@��H@�@�
=@��@��\@�M�@�{@��T@���@���@�O�@���@��`@��@�9X@��@�1@�  @��m@��@�|�@�\)@��@��!@��+@�$�@��-@�x�@��j@�j@�b@��@�l�@�;d@�+@��@���@���@���@���@�~�@�^5@�5?@�{@�{@�@�O�@�?}@�?}@�?}@�G�@��@���@��/@��u@��@�Z@�(�@�1@��
@�+@��H@��@�
=@�"�@��@�M�@�E�@��T@��7@���@���@�J@�^5@�~�@��\@��\@���@�M�@�M�@�V@�=q@�$�@�J@���@�&�@��/@�Ĝ@��@�r�@�j@�r�@�bN@�Z@�bN@�I�@� �@� �@� �@��@�1@���@��@�l�@�K�@��!@��+@�~�@�v�@�ff@��^@�X@�7L@���@�r�@�Z@���@���@�\)@��R@�~�@�V@�$�@��@��T@��@�@��@�$�@�$�@�@��T@���@�&�@��/@��9@��@��9@��u@�I�@�(�@�@+@~��@~$�@}�@~{@~$�@}p�@|j@{�m@z�H@y�#@y��@yG�@x��@x�9@x��@x�@xbN@x �@w�@w�@w+@w�@vV@u�-@u?}@t��@t9X@s�@s"�@r�H@r��@r��@rn�@qx�@o�@o�@n�@n�@nȴ@nȴ@n{@m��@mV@lz�@l1@kƨ@k��@kS�@j�@j�@j��@i�^@iX@i%@hQ�@h  @g�;@g�@g|�@g\)@g\)@g+@f��@f�y@f$�@e�T@eO�@d�/@d��@d9X@c�
@c��@c�@cC�@co@b~�@a�#@ahs@aG�@a7L@a7L@`�`@`Ĝ@`��@`Ĝ@`�u@`r�@`1'@_l�@^�@^ff@]�@]@]�-@]�h@]�@]`B@]V@\��@[�F@[S�@[33@Z��@Z^5@Z=q@YX@X�u@XQ�@X1'@W�w@V�+@V5?@U�@Up�@UO�@U?}@UV@T��@T��@T��@T�j@T�D@S�m@SC�@S"�@S@R��@R��@Rn�@R^5@RM�@Q�@Q��@Qhs@QX@QX@Q7L@Q�@P��@P�`@P�@O��@O+@Nȴ@NE�@N{@N@M@L��@LI�@L1@K�
@K�F@KC�@J�H@J��@J�!@J�!@J�!@J��@J~�@J-@I�@I��@I�^@I��@I��@I7L@I�@H��@H�`@HĜ@H�@HA�@G�@G�P@G
=@F�y@F��@E�@E��@EO�@D�@D�D@D(�@D1@C�
@C��@CS�@CC�@C33@C"�@B�H@B�!@BM�@B�@A�#@AG�@@��@@�u@@bN@?�w@?�@?��@?��@?|�@?�@>�R@>ff@>@=�T@=��@=�-@=��@=`B@=?}@<��@<�D@<9X@<1@;�
@;��@;S�@;33@;"�@;@:��@:�\@:M�@9��@9G�@97L@9�@8�u@8Q�@8Q�@8A�@8A�@8 �@7�;@7�w@7��@7�P@7|�@7;d@7+@7+@7
=@7
=@6�@6��@6�+@6ff@5��@5`B@4�@4�@4z�@4Z@49X@3�
@3�@3t�@3dZ@333@3o@2��@2�!@2~�@2-@1��@1�#@1��@1�@0��@0�@0Q�@01'@0 �@/�w@/+@.�@.��@.V@.{@-�@-�T@-�T@-�T@-��@-��@-@-�@-`B@-`B@-O�@-�@,��@,�/@,�/@,�j@,Z@,�@+�m@+��@+S�@+"�@+@*��@*��@*M�@*-@*J@)�^@)hs@)7L@)�@)%@(�`@(��@(Ĝ@(Ĝ@(�9@(bN@'�@'��@'�@'��@'|�@'l�@';d@'
=@&�y@&ȴ@&��@&ff@&E�@&5?@%�T@%�@%V@$�/@$��@$z�@$(�@#�m@#��@#�@#�@#dZ@#C�@"�@"��@"��@"~�@"n�@"^5@"=q@"-@"�@!��@!�@!�7@ ��@ �9@ �u@ ��@ �u@ r�@ Q�@   @��@K�@+@
=@�y@�R@ff@5?@$�@@�@��@��@V@�/@z�@j@Z@Z@I�@9X@(�@�m@dZ@C�@"�@@@��@^5@M�@-@��@��@�^@hs@Ĝ@��@bN@A�@1'@b@�@�w@|�@;d@�@��@��@ff@5?@$�@{@�@@��@�@p�@/@�@�@�@�/@�/@z�@��@"�@��@�\@^5@�@�^@hs@G�@Ĝ@��@�u@bN@A�@b@|�@K�@;d@�@
=@�y@��@v�@E�@{@{111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B�LB�LB�LB�RB�RB�LB�LB�RB�RB�RB�LB�LB�LB�LB�LB�LB�LB�LB�LB�LB�LB�LB�LB�FB�-B�B��Bx�BhsB`BBYBL�B1'B{BB��B��B��B��B��B��B��B�B�B�wBŢBƨBǮBŢBŢBǮB�B�
B��B��B�5B�sB�B�sB�fB�#B��B�;B�B�B�B�B�B�sB�;B�B��B��B�^B��B�JB~�BhsBR�BH�BD�B<jB(�B�B��B��B�B�yB�fB�TB�)B��B�^B��B�{B�7B�BgmBG�B;dB1'B(�B!�B�B{BJBB
�ZB
��B
��B
�-B
��B
��B
� B
p�B
cTB
XB
@�B
0!B
 �B
hB
  B	�B	�;B	��B	�jB	��B	��B	��B	�oB	}�B	r�B	hsB	_;B	N�B	C�B	;dB	)�B	#�B	{B	B��B�ZB�;B�5B�#B�B��B��BɺBÖB��BĜBÖB�qB�'B��B��B��B��B��B��B�uB�hB�\B�VB�7B�%B�B�B�B~�Bz�Bv�Bu�Bu�Bs�Br�Bp�Bn�Bn�BiyBgmBe`BdZBbNB`BB]/BXBVBR�BQ�BM�BL�BJ�BI�BJ�BH�BG�BF�BD�BC�BA�B>wB<jB9XB7LB6FB49B49B1'B1'B0!B.B,B-B)�B)�B(�B'�B'�B&�B&�B$�B#�B#�B"�B"�B#�B$�B#�B#�B#�B(�B%�B$�B%�B$�B$�B$�B$�B$�B$�B$�B$�B$�B#�B#�B"�B%�B$�B%�B%�B%�B&�B(�B(�B'�B'�B'�B(�B'�B)�B)�B+B+B)�B)�B.B33B7LB8RB9XB:^B:^B=qB?}BA�BE�BH�BI�BM�BO�BO�BS�BXB^5B]/B`BBcTBjBo�Bv�Bw�Bx�Bz�B}�B�B�+B�7B�=B�VB�hB�oB��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�B�B�B�!B�'B�3B�^B�jB�jB��BǮB��B��B��B�B�)B�5B�BB�ZB�fB�B�B�B�B��B��B��B��B��B��B	  B	B	B		7B	
=B	PB	oB	uB	{B	�B	�B	�B	 �B	#�B	$�B	$�B	'�B	(�B	(�B	'�B	%�B	%�B	&�B	&�B	(�B	,B	0!B	33B	5?B	6FB	9XB	:^B	<jB	?}B	B�B	E�B	G�B	K�B	M�B	N�B	O�B	N�B	O�B	R�B	VB	XB	\)B	`BB	bNB	cTB	dZB	ffB	iyB	k�B	m�B	o�B	q�B	r�B	t�B	u�B	v�B	z�B	~�B	~�B	�B	�B	�B	�B	�B	�B	�B	�B	�%B	�+B	�7B	�7B	�=B	�DB	�VB	�hB	�uB	�{B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�'B	�?B	�FB	�XB	�qB	�wB	�}B	ĜB	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�5B	�5B	�5B	�;B	�BB	�HB	�HB	�HB	�HB	�HB	�NB	�NB	�NB	�TB	�ZB	�`B	�`B	�`B	�mB	�mB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
%B
+B
+B
1B
	7B

=B
DB
JB
JB
PB
VB
\B
\B
bB
hB
hB
hB
hB
hB
hB
oB
oB
oB
oB
oB
oB
oB
uB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 �B
 �B
!�B
!�B
"�B
"�B
#�B
#�B
#�B
#�B
$�B
$�B
$�B
$�B
$�B
$�B
$�B
$�B
%�B
%�B
&�B
&�B
&�B
'�B
(�B
(�B
(�B
)�B
)�B
)�B
)�B
)�B
)�B
)�B
+B
-B
-B
-B
.B
.B
.B
/B
0!B
0!B
0!B
0!B
2-B
2-B
2-B
33B
33B
33B
33B
33B
33B
33B
33B
49B
49B
5?B
5?B
5?B
5?B
5?B
6FB
5?B
6FB
6FB
6FB
6FB
6FB
6FB
6FB
6FB
7LB
6FB
7LB
8RB
8RB
8RB
9XB
9XB
9XB
9XB
9XB
:^B
;dB
;dB
;dB
<jB
<jB
<jB
<jB
<jB
<jB
<jB
<jB
<jB
<jB
<jB
<jB
<jB
<jB
=qB
<jB
=qB
=qB
=qB
=qB
=qB
=qB
>wB
>wB
>wB
>wB
?}B
?}B
?}B
@�B
@�B
@�B
@�B
@�B
@�B
@�B
@�B
@�B
@�B
@�B
@�B
A�B
A�B
A�B
B�B
B�B
B�B
B�B
C�B
C�B
C�B
C�B
C�B
D�B
D�B
D�B
E�B
E�B
E�B
E�B
E�B
E�B
E�B
F�B
F�B
F�B
F�B
G�B
G�B
G�B
G�B
G�B
G�B
G�B
G�B
H�B
H�B
H�B
H�B
I�B
I�B
I�B
I�B
I�B
I�B
J�B
J�B
J�B
J�B
J�B
J�B
J�B
J�B
J�B
K�B
K�B
K�B
K�B
K�B
K�B
L�B
L�B
M�B
M�B
M�B
M�B
M�B
N�B
N�B
N�B
N�B
N�B
N�B
N�B
N�B
N�B
N�B
O�B
O�B
O�B
O�B
P�B
P�B
P�B
P�B
P�B
Q�B
Q�B
Q�B
R�B
R�B
R�B
R�B
R�B
R�B
R�B
R�B
R�B
R�B
S�B
S�B
S�B
S�B
S�B
S�B
T�B
S�B
S�B
T�B
T�B
T�B
T�B
VB
VB
VB
VB
VB
W
B
W
B
W
B
W
B
XB
XB
XB
XB
XB
XB
XB
XB
XB
YB
YB
YB
YB
YB
ZB
YB
ZB
ZB
ZB
ZB
ZB
ZB
ZB
[#B
[#B
[#B
[#B
\)B
\)B
\)B
\)B
]/B
]/B
]/B
]/B
]/B
]/B
]/B
]/B
]/B
]/B
^5B
^5B
^5B
^5B
^5B
^5B
^5B
^5B
_;B
_;B
_;B
_;B
_;B
_;B
_;B
`BB
`BB
aHB
aHB
aHB
aHB
aHB
aHB
bNB
bNB
bNB
bNB
bNB
bNB
cTB
cTB
cTB
cTB
cTB
cTB
cTB
cTB
dZB
dZB
dZB
dZB
e`B
e`B
e`B
ffB
e`B
e`B
e`B
ffB
ffB
ffB
ffB
gmB
gmB
gmB
hsB
hsB
hsB
hsB
hsB
hsB
iyB
iyB
iyB
iyB
iyB
jB
jB
jB
jB
jB
jB
k�B
k�B
k�B
k�B
k�B
k�B
k�B
k�B
k�B
m�B
m�B
m�B
n�B
n�B
n�B
n�B
o�B
o�B
p�B
p�B
p�B
p�B
p�B
p�B
q�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
s�B
s�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B�LB�LB�LB�RB�RB�LB�LB�RB�RB�RB�LB�LB�LB�LB�LB�LB�LB�LB�LB�LB�LB�LB�LB�FB�-B�B��Bx�BhsB`BBYBL�B1'B{BB��B��B��B��B��B��B��B�B�B�wBŢBƨBǮBŢBŢBǮB�B�
B��B��B�5B�sB�B�sB�fB�#B��B�;B�B�B�B�B�B�sB�;B�B��B��B�^B��B�JB~�BhsBR�BH�BD�B<jB(�B�B��B��B�B�yB�fB�TB�)B��B�^B��B�{B�7B�BgmBG�B;dB1'B(�B!�B�B{BJBB
�ZB
��B
��B
�-B
��B
��B
� B
p�B
cTB
XB
@�B
0!B
 �B
hB
  B	�B	�;B	��B	�jB	��B	��B	��B	�oB	}�B	r�B	hsB	_;B	N�B	C�B	;dB	)�B	#�B	{B	B��B�ZB�;B�5B�#B�B��B��BɺBÖB��BĜBÖB�qB�'B��B��B��B��B��B��B�uB�hB�\B�VB�7B�%B�B�B�B~�Bz�Bv�Bu�Bu�Bs�Br�Bp�Bn�Bn�BiyBgmBe`BdZBbNB`BB]/BXBVBR�BQ�BM�BL�BJ�BI�BJ�BH�BG�BF�BD�BC�BA�B>wB<jB9XB7LB6FB49B49B1'B1'B0!B.B,B-B)�B)�B(�B'�B'�B&�B&�B$�B#�B#�B"�B"�B#�B$�B#�B#�B#�B(�B%�B$�B%�B$�B$�B$�B$�B$�B$�B$�B$�B$�B#�B#�B"�B%�B$�B%�B%�B%�B&�B(�B(�B'�B'�B'�B(�B'�B)�B)�B+B+B)�B)�B.B33B7LB8RB9XB:^B:^B=qB?}BA�BE�BH�BI�BM�BO�BO�BS�BXB^5B]/B`BBcTBjBo�Bv�Bw�Bx�Bz�B}�B�B�+B�7B�=B�VB�hB�oB��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�B�B�B�!B�'B�3B�^B�jB�jB��BǮB��B��B��B�B�)B�5B�BB�ZB�fB�B�B�B�B��B��B��B��B��B��B	  B	B	B		7B	
=B	PB	oB	uB	{B	�B	�B	�B	 �B	#�B	$�B	$�B	'�B	(�B	(�B	'�B	%�B	%�B	&�B	&�B	(�B	,B	0!B	33B	5?B	6FB	9XB	:^B	<jB	?}B	B�B	E�B	G�B	K�B	M�B	N�B	O�B	N�B	O�B	R�B	VB	XB	\)B	`BB	bNB	cTB	dZB	ffB	iyB	k�B	m�B	o�B	q�B	r�B	t�B	u�B	v�B	z�B	~�B	~�B	�B	�B	�B	�B	�B	�B	�B	�B	�%B	�+B	�7B	�7B	�=B	�DB	�VB	�hB	�uB	�{B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�'B	�?B	�FB	�XB	�qB	�wB	�}B	ĜB	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�5B	�5B	�5B	�;B	�BB	�HB	�HB	�HB	�HB	�HB	�NB	�NB	�NB	�TB	�ZB	�`B	�`B	�`B	�mB	�mB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
%B
+B
+B
1B
	7B

=B
DB
JB
JB
PB
VB
\B
\B
bB
hB
hB
hB
hB
hB
hB
oB
oB
oB
oB
oB
oB
oB
uB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 �B
 �B
!�B
!�B
"�B
"�B
#�B
#�B
#�B
#�B
$�B
$�B
$�B
$�B
$�B
$�B
$�B
$�B
%�B
%�B
&�B
&�B
&�B
'�B
(�B
(�B
(�B
)�B
)�B
)�B
)�B
)�B
)�B
)�B
+B
-B
-B
-B
.B
.B
.B
/B
0!B
0!B
0!B
0!B
2-B
2-B
2-B
33B
33B
33B
33B
33B
33B
33B
33B
49B
49B
5?B
5?B
5?B
5?B
5?B
6FB
5?B
6FB
6FB
6FB
6FB
6FB
6FB
6FB
6FB
7LB
6FB
7LB
8RB
8RB
8RB
9XB
9XB
9XB
9XB
9XB
:^B
;dB
;dB
;dB
<jB
<jB
<jB
<jB
<jB
<jB
<jB
<jB
<jB
<jB
<jB
<jB
<jB
<jB
=qB
<jB
=qB
=qB
=qB
=qB
=qB
=qB
>wB
>wB
>wB
>wB
?}B
?}B
?}B
@�B
@�B
@�B
@�B
@�B
@�B
@�B
@�B
@�B
@�B
@�B
@�B
A�B
A�B
A�B
B�B
B�B
B�B
B�B
C�B
C�B
C�B
C�B
C�B
D�B
D�B
D�B
E�B
E�B
E�B
E�B
E�B
E�B
E�B
F�B
F�B
F�B
F�B
G�B
G�B
G�B
G�B
G�B
G�B
G�B
G�B
H�B
H�B
H�B
H�B
I�B
I�B
I�B
I�B
I�B
I�B
J�B
J�B
J�B
J�B
J�B
J�B
J�B
J�B
J�B
K�B
K�B
K�B
K�B
K�B
K�B
L�B
L�B
M�B
M�B
M�B
M�B
M�B
N�B
N�B
N�B
N�B
N�B
N�B
N�B
N�B
N�B
N�B
O�B
O�B
O�B
O�B
P�B
P�B
P�B
P�B
P�B
Q�B
Q�B
Q�B
R�B
R�B
R�B
R�B
R�B
R�B
R�B
R�B
R�B
R�B
S�B
S�B
S�B
S�B
S�B
S�B
T�B
S�B
S�B
T�B
T�B
T�B
T�B
VB
VB
VB
VB
VB
W
B
W
B
W
B
W
B
XB
XB
XB
XB
XB
XB
XB
XB
XB
YB
YB
YB
YB
YB
ZB
YB
ZB
ZB
ZB
ZB
ZB
ZB
ZB
[#B
[#B
[#B
[#B
\)B
\)B
\)B
\)B
]/B
]/B
]/B
]/B
]/B
]/B
]/B
]/B
]/B
]/B
^5B
^5B
^5B
^5B
^5B
^5B
^5B
^5B
_;B
_;B
_;B
_;B
_;B
_;B
_;B
`BB
`BB
aHB
aHB
aHB
aHB
aHB
aHB
bNB
bNB
bNB
bNB
bNB
bNB
cTB
cTB
cTB
cTB
cTB
cTB
cTB
cTB
dZB
dZB
dZB
dZB
e`B
e`B
e`B
ffB
e`B
e`B
e`B
ffB
ffB
ffB
ffB
gmB
gmB
gmB
hsB
hsB
hsB
hsB
hsB
hsB
iyB
iyB
iyB
iyB
iyB
jB
jB
jB
jB
jB
jB
k�B
k�B
k�B
k�B
k�B
k�B
k�B
k�B
k�B
m�B
m�B
m�B
n�B
n�B
n�B
n�B
o�B
o�B
p�B
p�B
p�B
p�B
p�B
p�B
q�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
s�B
s�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.19 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20221020100143                              AO  ARCAADJP                                                                    20221020100143    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20221020100143  QCP$                G�O�G�O�G�O�1F83E           AO  ARGQQCPL                                                                    20221020100143  QCF$                G�O�G�O�G�O�4000            