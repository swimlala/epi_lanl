CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS     N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-24T14:15:37Z creation      
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
resolution        =���   axis      Z           9p   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   A�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���        C�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   K�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���        M�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o        U�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   ^    TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o        `   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   h(   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o        j0   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o        rP   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   zp   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o        |x   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o        ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �    HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �$   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �(   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �,   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �l   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �|   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20181024141537  20181024141537  5904970 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               �A   AO  6785                            2B  A   APEX                            7726                            111215                          846 @��x9��1   @��-��@7�\(��c����m1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      �B   B   B   @�  @�33A   A   A>ffA`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  D   C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  Dy�D  D� D  D� D	  D	� D
  D
� D  D� D  D� D��D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D$��D%y�D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DN��DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]y�D]��D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� DgfDg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Duy�Dv  Dv� Dw  Dw� Dw�fDy�=D�.D�Mq1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@���@��
A Q�A Q�A>�RA`Q�A�(�A�(�A�(�A�(�A�(�A�(�A�(�A�(�B {B{B{B{B {B({B0{B8{B@{BH{BP{BX{B`{Bh{Bp{Bx{B�
=B�
=B�
=B�
=B�
=B�
=B�
=B�
=B�
=B�
=B�
=B�
=B�
=B�
=B�
=B�
=B�
=B�
=B�
=B�
=B�
=B�
=B�
=B�
=B�
=B��
B�
=B�
=B�
=B�
=B�
=B�
=C CCCCC
CCCCCCCCCCC C"C$C&C(C*C,C.C0C2C4C6C8C:C<C>C@CBCDCFCHCJCLCNCPCRCTCVCXCZC\C^C`CbCdCfChCjClCnCpCrCtCvCxCzC|C~C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�\C��C��C��D HC��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�\C��C��C��D HD �HDHD�HDHD�HDHD�HDHD�HDHD�HDHDz�DHD�HDHD�HD	HD	�HD
HD
�HDHD�HDHD�HD��D�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HD HD �HD!HD!�HD"HD"�HD#HD#�HD$HD$�HD$��D%z�D&HD&�HD'HD'�HD(HD(�HD)HD)�HD*HD*�HD+HD+�HD,HD,�HD-HD-�HD.HD.�HD/HD/�HD0HD0�HD1HD1�HD2HD2�HD3HD3�HD4HD4�HD5HD5�HD6HD6�HD7HD7�HD8HD8�HD9HD9�HD:HD:�HD;HD;�HD<HD<�HD=HD=�HD>HD>�HD?HD?�HD@HD@�HDAHDA�HDBHDB�HDCHDC�HDDHDD�HDEHDE�HDFHDF�HDGHDG�HDHHDH�HDIHDI�HDJHDJ�HDKHDK�HDLHDL�HDMHDM�HDNHDN�HDN��DO�HDPHDP�HDQHDQ�HDRHDR�HDSHDS�HDTHDT�HDUHDU�HDVHDV�HDWHDW�HDXHDX�HDYHDY�HDZHDZ�HD[HD[�HD\HD\�HD]HD]z�D]��D^�HD_HD_�HD`HD`�HDaHDa�HDbHDb�HDcHDc�HDdHDd�HDeHDe�HDfHDf�HDg�Dg�HDhHDh�HDiHDi�HDjHDj�HDkHDk�HDlHDl�HDmHDm�HDnHDn�HDoHDo�HDpHDp�HDqHDq�HDrHDr�HDsHDs�HDtHDt�HDuHDuz�DvHDv�HDwHDw�HDw�Dy��D�.�D�N1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A���A�A�ƨA�ĜA�ȴA���A���A���A���A���A��#A��#A��#A��/A��/A��/A��;A��;A��;A��;A��;A�~�A�;dA�oA���AìAÙ�AîAÙ�AËDA�ZA�&�A��yA´9A�ffA��A��A�"�A�E�A�33A�XA���A�A���A�oA�&�A�33A�"�A���A�z�A��A�x�A�x�A��-A��A�=qA���A���A�|�A�-A�/A���A���A�33A�v�A���A�9XA�M�A��DA�A���A���A�A�-A���A�ĜA��;A�G�A��/A�?}A���A��wA��hA�A�A��A�z�A��;A�K�A�1'A�+A���A�t�A�+A��yA��hA�
=A�E�A���A�9XA��/A��DA���A��FA���A�I�A~��A}\)A{Az�/Ay�Aw/Ar�DAqO�Aq�Ap��Ap=qAo�FAoG�AnZAm|�Al�Ak/Ai��Ah~�Agl�Af�AehsAdn�Acp�Ac?}Ac�AbĜAb�\Ab$�AaO�A[�
AY��AX�yAW�AV�yATQ�AS7LAR��APĜAL�\AJ��AIG�AG�hAE�AEC�AD�+AC�mAC��AB��AA��AA�AA�mAA��A@��A?�PA>$�A<��A;��A:��A:{A9A9
=A81A7�A7C�A6^5A3��A1�A1;dA0�yA/�7A/K�A/33A.�jA.�A-�FA,�A,Q�A*�!A)K�A($�A'O�A&M�A$�A$A�A#C�A"z�A!ƨA ��A Q�AhsAĜA��A�A��AXA��A5?A�A
=A��A=qA�AA�A��AE�AoA$�A|�AoA
1'A	33A��A��AA�A�PA+A�A1Ap�A�AƨA��A&�@�S�@���@�/@�dZ@�X@�A�@���@�o@�O�@�j@�!@�G�@�(�@�@��@�h@��@��@�Q�@�=q@�hs@蛦@��@�C�@��@�E�@�X@�A�@���@�o@�O�@�j@�!@�G�@�(�@�@��@�h@��@��@�Q�@�=q@�hs@蛦@��@�C�@��@�E�@�hs@�hs@�/@���@��;@�l�@���@�5?@��@ߝ�@�=q@�`B@ܣ�@�(�@�~�@ى7@؃@��m@�$�@�V@�Z@�t�@�-@Ѓ@��@�v�@���@��`@�9X@ˍP@���@�^5@�=q@ə�@ț�@Ǯ@�+@��@��@�G�@��`@Ĵ9@���@ļj@ēu@ă@��@�33@�o@�?}@�C�@��P@���@�j@��y@�-@��@�bN@��@���@�t�@�
=@���@��+@�n�@�{@���@��^@�x�@�O�@���@��j@�r�@�(�@�  @��F@�\)@��@��H@��!@�^5@��+@�M�@�hs@�A�@�b@�  @�(�@�Q�@��P@��7@���@��@��@�33@��\@�^5@�=q@�O�@�%@��@�t�@��@���@�/@��/@��9@��@�(�@��m@��;@��F@�\)@��!@���@���@��-@���@���@�x�@�&�@��j@��@�Z@�I�@���@���@���@�33@�"�@�33@�33@�;d@�@�v�@�v�@�^5@�E�@�5?@�$�@���@�%@��`@��j@�Ĝ@��@��D@�Q�@�1@�  @��m@���@�dZ@�dZ@��@�;d@�o@��@���@��\@�n�@�~�@�V@��^@�^5@�"�@�V@�X@��`@�G�@�?}@�/@�Ĝ@��m@��@���@�t�@�1@�r�@��@��P@�K�@��@�
=@��y@�ȴ@���@�^5@�M�@��@��-@�hs@�%@��/@�r�@�I�@�A�@�z�@�j@� �@��j@�G�@�O�@��/@�z�@��@�j@�Ĝ@��`@��/@��`@���@� �@��@��y@�n�@���@�ff@�5?@�{@���@���@���@��9@��@�7L@�V@�Q�@�l�@�"�@���@��\@�E�@�J@�@���@��@��-@�&�@��@��k@z�"@i�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111414111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A���A�A�ƨA�ĜA�ȴA���A���A���A���A���A��#A��#A��#A��/A��/A��/A��;A��;A��;A��;A��;A�~�A�;dA�oA���AìAÙ�AîAÙ�AËDA�ZA�&�A��yA´9A�ffA��A��A�"�A�E�A�33A�XA���A�A���A�oA�&�A�33A�"�A���A�z�A��A�x�A�x�A��-A��A�=qA���A���A�|�A�-A�/A���A���A�33A�v�A���A�9XA�M�A��DA�A���A���A�A�-A���A�ĜA��;A�G�A��/A�?}A���A��wA��hA�A�A��A�z�A��;A�K�A�1'A�+A���A�t�A�+A��yA��hA�
=A�E�A���A�9XA��/A��DA���A��FA���A�I�A~��A}\)A{Az�/Ay�Aw/Ar�DAqO�Aq�Ap��Ap=qAo�FAoG�AnZAm|�Al�Ak/Ai��Ah~�Agl�Af�AehsAdn�Acp�Ac?}Ac�AbĜAb�\Ab$�AaO�A[�
AY��AX�yAW�AV�yATQ�AS7LAR��APĜAL�\AJ��AIG�AG�hAE�AEC�AD�+AC�mAC��AB��AA��AA�AA�mAA��A@��A?�PA>$�A<��A;��A:��A:{A9A9
=A81A7�A7C�A6^5A3��A1�A1;dA0�yA/�7A/K�A/33A.�jA.�A-�FA,�A,Q�A*�!A)K�A($�A'O�A&M�A$�A$A�A#C�A"z�A!ƨA ��A Q�AhsAĜA��A�A��AXA��A5?A�A
=A��A=qA�AA�A��AE�AoA$�A|�AoA
1'A	33A��A��AA�A�PA+A�A1Ap�A�AƨA��A&�@�S�@���@�/@�dZ@�X@�A�@���@�o@�O�@�j@�!@�G�@�(�@�@��@�h@��@��@�Q�@�=q@�hs@蛦@��@�C�@��@�E�@�X@�A�@���@�o@�O�@�j@�!@�G�@�(�@�@��@�h@��@��@�Q�@�=q@�hs@蛦@��@�C�@��@�E�@�hs@�hs@�/@���@��;@�l�@���@�5?@��@ߝ�@�=q@�`B@ܣ�@�(�@�~�@ى7@؃@��m@�$�@�V@�Z@�t�@�-@Ѓ@��@�v�@���@��`@�9X@ˍP@���@�^5@�=q@ə�@ț�@Ǯ@�+@��@��@�G�@��`@Ĵ9@���@ļj@ēu@ă@��@�33@�o@�?}@�C�@��P@���@�j@��y@�-@��@�bN@��@���@�t�@�
=@���@��+@�n�@�{@���@��^@�x�@�O�@���@��j@�r�@�(�@�  @��F@�\)@��@��H@��!@�^5@��+@�M�@�hs@�A�@�b@�  @�(�@�Q�@��P@��7@���@��@��@�33@��\@�^5@�=q@�O�@�%@��@�t�@��@���@�/@��/@��9@��@�(�@��m@��;@��F@�\)@��!@���@���@��-@���@���@�x�@�&�@��j@��@�Z@�I�@���@���@���@�33@�"�@�33@�33@�;d@�@�v�@�v�@�^5@�E�@�5?@�$�@���@�%@��`@��j@�Ĝ@��@��D@�Q�@�1@�  @��m@���@�dZ@�dZ@��@�;d@�o@��@���@��\@�n�@�~�@�V@��^@�^5@�"�@�V@�X@��`@�G�@�?}@�/@�Ĝ@��m@��@���@�t�@�1@�r�@��@��P@�K�@��@�
=@��y@�ȴ@���@�^5@�M�@��@��-@�hs@�%@��/@�r�@�I�@�A�@�z�@�j@� �@��j@�G�@�O�@��/@�z�@��@�j@�Ĝ@��`@��/@��`@���@� �@��@��y@�n�@���@�ff@�5?@�{@���@���@���@��9@��@�7L@�V@�Q�@�l�@�"�@���@��\@�E�@�J@�@���@��@��-@�&�@��@��k@z�"@i�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111414111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�BYBXBYBYBYBZBZBZBYBYB[#B[#B[#B[#B[#B[#B\)B\)B\)B\)B\)B�B�LB��B��BǮBƨB��B��B��B��B��B��B��B��B��B�
B�;B�B�B��B�B�B�mB�NB�NB�HB�;B�)B�B�B��B��B��B��BȴB��B�jB�XB�3B�B��B�VB{�Bq�Bm�BdZB[#BQ�BI�B>wB'�B�B{BVBB��B�B�HBɺB�}B�3B��B�\Bx�Be`BYBL�B.B�BuBhBVBDB+BB
��B
�B
�B
�sB
�`B
�)B
��B
�}B
�B
��B
�hB
�B
{�B
o�B
]/B
.B
�B
�B
�B
�B
bB
VB
DB
B
B
B	��B	�B	�ZB	�B	��B	��B	B	��B	B	��B	�qB	�LB	�B	� B	l�B	e`B	^5B	]/B	O�B	D�B	?}B	7LB	#�B	�B	�B	
=B��B��B��B�B��B��B�B�B�B�B�B�B�sB�BB�B�B��B��B��B��B��B��BŢB�RB�B�B�!B�B��B��B��B��B��B��B��B��B�\B�DB�JB�\B�=B�1B�+B�B�B� B}�B{�Bx�By�BjBgmBhsBdZBcTB^5B]/B\)B[#BYBVBQ�BQ�BJ�BG�BD�BB�B?}B<jB8RB5?B49B49B2-B1'B0!B+B+B+B)�B+B+B'�B&�B%�B%�B#�B"�B"�B"�B!�B!�B �B!�B�B!�B �B �B �B �B"�B!�B!�B!�B!�B!�B$�B%�B#�B"�B"�B"�B!�B!�B �B!�B�B!�B �B �B �B �B"�B!�B!�B!�B!�B!�B!�B!�B!�B �B �B"�B!�B"�B"�B#�B#�B$�B%�B&�B&�B'�B'�B(�B(�B,B+B,B-B.B/B0!B0!B1'B1'B2-B2-B33B33B33B5?B6FB:^BA�BF�BI�BL�BO�BR�BW
BYB]/B`BBgmBq�Br�Bu�Bt�By�Bw�Bu�Br�Br�Bp�Bo�Bp�Bs�Bw�By�B~�B�B�B�B�1B�7B�JB�PB�uB��B��B��B��B��B��B��B��B��B��B��B��B�'B�!B�'B�9B�XB�wB��B�jB�jB�jB�}BǮBɺBɺB��B��B��B��B��B�B�B�)B�/B�;B�HB�TB�TB�ZB�`B�B�B�B�B�B�B�B�B��B	B	B	%B	%B		7B	JB	VB	hB	�B	�B	�B	�B	�B	 �B	!�B	$�B	)�B	-B	-B	0!B	2-B	49B	6FB	9XB	:^B	;dB	=qB	@�B	A�B	A�B	E�B	H�B	J�B	S�B	YB	[#B	[#B	\)B	]/B	`BB	cTB	ffB	iyB	p�B	u�B	w�B	t�B	t�B	x�B	{�B	�B	� B	� B	�B	�B	�%B	�VB	�uB	�uB	�oB	�oB	�oB	�oB	�oB	�oB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�3B	�?B	�FB	�RB	�qB	�}B	B	ŢB	ƨB	ǮB	��B	��B	ɺB	ȴB	ƨB	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�)B	�#B	�/B	�;B	�/B	�5B	�;B	�;B	�;B	�BB	�HB	�BB	�NB	��B	�'B
�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111414111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111BYBXBYBYBYBZBZBZBYBYB[#B[#B[#B[#B[#B[#B\)B\)B\)B\)B\)B�B�LB��B��BǮBƨB��B��B��B��B��B��B��B��B��B�
B�;B�B�B��B�B�B�mB�NB�NB�HB�;B�)B�B�B��B��B��B��BȴB��B�jB�XB�3B�B��B�VB{�Bq�Bm�BdZB[#BQ�BI�B>wB'�B�B{BVBB��B�B�HBɺB�}B�3B��B�\Bx�Be`BYBL�B.B�BuBhBVBDB+BB
��B
�B
�B
�sB
�`B
�)B
��B
�}B
�B
��B
�hB
�B
{�B
o�B
]/B
.B
�B
�B
�B
�B
bB
VB
DB
B
B
B	��B	�B	�ZB	�B	��B	��B	B	��B	B	��B	�qB	�LB	�B	� B	l�B	e`B	^5B	]/B	O�B	D�B	?}B	7LB	#�B	�B	�B	
=B��B��B��B�B��B��B�B�B�B�B�B�B�sB�BB�B�B��B��B��B��B��B��BŢB�RB�B�B�!B�B��B��B��B��B��B��B��B��B�\B�DB�JB�\B�=B�1B�+B�B�B� B}�B{�Bx�By�BjBgmBhsBdZBcTB^5B]/B\)B[#BYBVBQ�BQ�BJ�BG�BD�BB�B?}B<jB8RB5?B49B49B2-B1'B0!B+B+B+B)�B+B+B'�B&�B%�B%�B#�B"�B"�B"�B!�B!�B �B!�B�B!�B �B �B �B �B"�B!�B!�B!�B!�B!�B$�B%�B#�B"�B"�B"�B!�B!�B �B!�B�B!�B �B �B �B �B"�B!�B!�B!�B!�B!�B!�B!�B!�B �B �B"�B!�B"�B"�B#�B#�B$�B%�B&�B&�B'�B'�B(�B(�B,B+B,B-B.B/B0!B0!B1'B1'B2-B2-B33B33B33B5?B6FB:^BA�BF�BI�BL�BO�BR�BW
BYB]/B`BBgmBq�Br�Bu�Bt�By�Bw�Bu�Br�Br�Bp�Bo�Bp�Bs�Bw�By�B~�B�B�B�B�1B�7B�JB�PB�uB��B��B��B��B��B��B��B��B��B��B��B��B�'B�!B�'B�9B�XB�wB��B�jB�jB�jB�}BǮBɺBɺB��B��B��B��B��B�B�B�)B�/B�;B�HB�TB�TB�ZB�`B�B�B�B�B�B�B�B�B��B	B	B	%B	%B		7B	JB	VB	hB	�B	�B	�B	�B	�B	 �B	!�B	$�B	)�B	-B	-B	0!B	2-B	49B	6FB	9XB	:^B	;dB	=qB	@�B	A�B	A�B	E�B	H�B	J�B	S�B	YB	[#B	[#B	\)B	]/B	`BB	cTB	ffB	iyB	p�B	u�B	w�B	t�B	t�B	x�B	{�B	�B	� B	� B	�B	�B	�%B	�VB	�uB	�uB	�oB	�oB	�oB	�oB	�oB	�oB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�3B	�?B	�FB	�RB	�qB	�}B	B	ŢB	ƨB	ǮB	��B	��B	ɺB	ȴB	ƨB	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�)B	�#B	�/B	�;B	�/B	�5B	�;B	�;B	�;B	�BB	�HB	�BB	�NB	��B	�'B
�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111414111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.02 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181024141537                              AO  ARCAADJP                                                                    20181024141537    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181024141537  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181024141537  QCF$                G�O�G�O�G�O�4000            