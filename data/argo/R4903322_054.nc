CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2021-10-01T11:00:43Z creation      
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
resolution        =���   axis      Z        X  9p   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  H�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     X  L�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  [�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     X  _�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     X  o(   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ~�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     X  �X   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     X  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     X  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �8   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     X  �   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �h   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     X  �@   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ژ   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �    HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �D   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �T   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �X   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �h   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �l   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �p   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �tArgo profile    3.1 1.2 19500101000000  20211001110043  20211001110043  4903322 US ARGO PROJECT                                                 GREGORY C. JOHNSON                                              PRES            TEMP            PSAL               6A   AO  8286                            2B  A   NAVIS_A                         1165                            170425                          863 @ٗ�@H�Q1   @ٗ���_�@9j~��#�d�;dZ�1   GPS     Primary sampling: averaged                                                                                                                                                                                                                                         6A   A   A   @���@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�33B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7y�D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� 1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�=q@���A Q�A Q�A@Q�A`Q�A�(�A�(�A�(�A�(�A�(�A�(�A�(�A�\)B {B{B{B{B {B({B0{B8{B@{BH{BP{BX{B`{Bh{Bp{Bx{B�
=B�
=B�=pB�
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
=B�
=B�
=B�
=B�
=C CCCCC
CCCCCCCCCCC C"C$C&C(C*C,C.C0C2C4C6C8C:C<C>C@CBCDCFCHCJCLCNCPCRCTCVCXCZC\C^C`CbCdCfChCjClCnCpCrCtCvCxCzC|C~C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C���C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��D HD �HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HD	HD	�HD
HD
�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HD HD �HD!HD!�HD"HD"�HD#HD#�HD$HD$�HD%HD%�HD&HD&�HD'HD'�HD(HD(�HD)HD)�HD*HD*�HD+HD+�HD,HD,�HD-HD-�HD.HD.�HD/HD/�HD0HD0�HD1HD1�HD2HD2�HD3HD3�HD4HD4�HD5HD5�HD6HD6�HD7HD7z�D8HD8�HD9HD9�HD:HD:�HD;HD;�HD<HD<�HD=HD=�HD>HD>�HD?HD?�HD@HD@�HDAHDA�HDBHDB�HDCHDC�HDDHDD�HDEHDE�HDFHDF�HDGHDG�HDHHDH�HDIHDI�HDJHDJ�HDKHDK�HDLHDL�HDMHDM�HDNHDN�HDOHDO�HDPHDP�HDQHDQ�HDRHDR�HDSHDS�HDTHDT�HDUHDU�HDVHDV�HDWHDW�HDXHDX�HDYHDY�HDZHDZ�HD[HD[�HD\HD\�HD]HD]�HD^HD^�HD_HD_�HD`HD`�HDaHDa�HDbHDb�HDcHDc�HDdHDd�HDeHDe�HDfHDf�HDgHDg�HDhHDh�HDiHDi�HDjHDj�HDkHDk�HDlHDl�HDmHDm�HDnHDn�HDoHDo�HDpHDp�HDqHDq�HDrHDr�HDsHDs�HDtHDt�HDuHDu�HDvHDv�HDwHDw�HDxHDx�HDyHDy�HDzHDz�HD{HD{�HD|HD|�HD}HD}�HD~HD~�HDHD�HD� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D�D���D� �D�@�DÀ�D���D� �D�@�DĀ�D���D� �D�@�Dŀ�D���D� �D�@�Dƀ�D���D� �D�@�Dǀ�D���D� �D�@�DȀ�D���D� �D�@�Dɀ�D���D� �D�@�Dʀ�D���D� �D�@�Dˀ�D���D� �D�@�D̀�D���D� �D�@�D̀�D���D� �D�@�D΀�D���D� �D�@�Dπ�D���D� �D�@�DЀ�D���D� �D�@�Dр�D���D� �D�@�DҀ�D���D� �D�@�DӀ�D���D� �D�@�DԀ�D���D� �D�@�DՀ�D���D� �D�@�Dր�D���D� �D�@�D׀�D���D� �D�@�D؀�D���D� �D�@�Dـ�D���D� �D�@�Dڀ�D���D� �D�@�Dۀ�D���D� �D�@�D܀�D���D� �D�@�D݀�D���D� �D�@�Dހ�D���D� �D�@�D߀�D���D� �D�@�D���D���D� �D�@�DဤD���D� �D�@�D․D���D� �D�@�D〤D���D� �D�@�D䀤D���D� �D�@�D値D���D� �D�@�D怤D���D� �D�@�D瀤D���D� �D�@�D耤D���D� �D�@�D逤D���D� �D�@�DꀤD���D� �D�@�D뀤D���D� �D�@�D쀤D���D� �D�@�D퀤D���D� �D�@�DD���D� �D�@�DD���D� �D�@�D���D���D� �D�@�D�D���D� �D�@�D�D���D� �D�@�D�D���D� �D�@�D�D���D� �D�@�D���D�Ф1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A��mA��yA��yA��yA��yA��A��A��mA��`A��yA��A��mA��mA��mA��;A��;A��/A��#A��#A��AּjAָRAִ9A֬A֩�A֧�A֣�A֓uA֏\A�z�A�;dA�$�A��/AΧ�A���AʑhA���A�A�ȴA�$�A�t�A�;dA�Q�A�ĜA�33A��HA���A�5?A�jA��TA�  A�|�A�S�A��PA���A�oA��A�A�A��;A�-A���A�^5A��\A�-A�9XA���A��A��\A�v�A��A��wA��A�S�A��A�/A�S�A�?}A���A�33A�ffA��A��A��yA�bA��A�E�A���A�{A��A��RA�A�z�A�ƨA��uA��`A��#A��DA�oA�~�A�t�A�5?A��A��A��A�XA��A���A��A���A�p�A�M�A�ZA��A�7LA�ƨA��A���A��RA�M�A���A�hsA��A��A���A�l�A�5?A|��A{Ayl�Aw�Aw?}Av�uAt~�Aq�FAm�FAk|�AjZAh��Ag�Ag+Ae�hAc�A`�uA_K�A_�A_
=A^��A^�jA^n�A]��A]�A[�mA[;dAY��AW&�AU\)AS��AS�AR��AQC�APJAO`BAN��AL�!AK\)AJ��AI�AIC�AH1AE
=AD$�AB�DAA��AA%A@ �A?+A>��A=�A=VA<A�A;�hA: �A8r�A6��A5O�A4��A2��A/��A.��A.-A-�-A-�hA-`BA,�A,�A+p�A*r�A)S�A(E�A'S�A&=qA%�A%l�A%
=A$�yA$��A$�DA#�wA"�+A ��AffA��AXA��AS�A�A9XA��A��A1A�A�AI�A�HA�PA�jA�A�AI�Ax�A�A�PA
1'A	�A	%A1A�A(�A+A�A�A�!A��AVA �jA =q@�|�@�M�@�t�@�^5@�{@��@���@�bN@��@�
=@��T@��@���@���@�Z@�|�@��@�\@�X@�9@�b@�|�@�-@�7L@�9@�Q�@� �@�-@��@�O�@ڸR@��#@�;d@��`@�5?@��@�Z@��@ϕ�@�t�@�C�@ΰ!@�-@�X@�9X@��;@��@�X@ȣ�@��
@�S�@�K�@�S�@�S�@��@Ł@�%@ċD@î@���@��@�J@�r�@��!@���@�hs@�&�@���@��
@�~�@�@��/@�1@��F@�@�@��-@�&�@�1'@��@�`B@�r�@��
@��P@�@��T@��j@�l�@���@�$�@��h@���@��j@��@��m@�dZ@��T@�I�@�1@���@��@��@��;@���@��w@�K�@��@�E�@���@��@�7L@��@���@��u@�r�@�I�@�(�@�1@�ƨ@�|�@�33@�@�5?@�r�@�(�@���@��@�ƨ@�K�@���@��!@�^5@��7@�Z@��
@���@�+@�"�@��H@�^5@�J@��@���@���@�p�@���@�j@�A�@�A�@�I�@�I�@�I�@�b@��
@��@��y@��@�7L@��@��@��m@���@�K�@�33@��@��\@�V@�{@���@�&�@���@���@���@�j@�bN@�bN@�Q�@�9X@� �@�  @���@��@��
@��@�K�@�@��@���@�v�@�ff@�ff@�V@�M�@�M�@�@�@�&�@���@��u@��@��F@�t�@�\)@�33@�@�ȴ@���@�^5@��@���@���@���@���@��@�X@�&�@�r�@�1'@�1@��;@��F@��@�K�@��@��R@���@�n�@�-@��#@���@�p�@�X@�7L@�&�@��@�%@���@��@��/@���@�Q�@��@�w@~v�@}�T@}�T@}�T@}�T@}�T@}@|z�@{��@z��@y�7@x�`@xĜ@x�@xr�@x1'@w�;@w�@w\)@w;d@w�@vȴ@v��@vff@u��@u?}@t�@s�F@sS�@q��@q&�@p��@pĜ@pQ�@p �@p  @o��@o��@o�w@o�@o��@ol�@n��@nE�@n@m�T@m�T@m@m?}@m/@l��@l�/@l�j@l�@lj@k33@j=q@jJ@i�@i��@i��@i��@ix�@ihs@iG�@i7L@i�@h��@h�`@h�9@h�@g�@f��@e@e�-@e�@d�j@dZ@d9X@c33@c@c@c@c@b�H@b��@b^5@a�#@a��@a7L@`Ĝ@`Q�@_�@_l�@_
=@^�R@^�+@^5?@]�T@]��@]O�@]�@\��@\��@\Z@\9X@\�@[t�@[33@[o@Z�@Z��@YG�@X�@X �@Xb@X �@Xb@W�;@V�y@Vv�@V$�@U?}@T�D@TI�@T(�@S�m@S�F@S33@R��@R�\@RM�@R-@Q�@Q&�@P��@O��@O\)@O
=@N�@Nȴ@N�R@N��@Nff@M�@M@MO�@L�@Lj@L�@K��@KC�@K@JM�@I��@I��@I�7@H��@Hr�@G��@G+@F�@F��@Fv�@FE�@F{@E�T@E�T@E�-@E�h@EO�@E�@D�@D��@D�@C��@C@B�@B��@A��@A%@@�u@@�@@r�@@r�@@r�@@Q�@@Q�@@ �@@  @?��@?
=@>ff@>$�@>{@=�@=�@=�@=�@=�@=�T@=�-@=`B@<��@<�@<j@<9X@;�m@;t�@:�@:�\@:=q@9�^@97L@8��@8��@8�u@8 �@7\)@7�@6�@6��@6v�@6V@6ff@6ff@6V@6V@6V@6E�@65?@6$�@6@5�@5��@5�h@5�@5`B@5/@4��@4�j@4z�@4Z@4Z@4(�@3��@3ƨ@3��@3dZ@333@2��@2��@2n�@2M�@2M�@2=q@2�@1�@1��@1��@1��@1��@1�7@1hs@1G�@0��@0�u@0A�@0  @/�P@/;d@/
=@/
=@.��@.��@.�y@.�@.ȴ@.V@.5?@.@-p�@,��@,��@,�@+�
@+ƨ@+dZ@*�H@*��@*�\@*^5@*=q@*-@*-@*-@*J@)�#@)��@)x�@)G�@)G�@)%@(Ĝ@(��@(bN@( �@'�@'�;@'�w@'�P@'��@'+@&ff@&5?@&$�@%��@%p�@%?}@%�@$�/@$j@#�m@#�@#33@"�@"�\@"=q@"�@!��@!�7@!hs@!G�@ �`@ �u@ 1'@�@�@�P@\)@�@��@�@�@�@��@V@{@�-@�-@��@?}@V@��@I�@ƨ@ƨ@��@S�@C�@C�@"�@@�!@�\@~�@n�@�@�@�#@�^@�7@&�@��@1'@  @�;@�w@��@;d@
=@��@�y@��@V@@�-@?}@��@z�@j@Z@Z@I�@(�@1@�m@�
@�
@��@33@�H@�\@n�@^5@^5@^5@�@�@��@x�@X@&�@��@bN@1'@�;@�w@�P@l�@l�@K�@�y@��@�+@ff@$�@�@@��@�@p�@V@��@ƨ@��@t�@S�@S�@C�@"�@
��@
M�@
=q@
-@	�#@	�#@	��@	��@	�^@	��@	x�@�`@Ĝ@�9@�9@��@�u@bN@  @��@�P@+@�@�@ȴ@�@��@v�@V@$�@@@�@@��@�h@p�@p�@`B@?}@/@��@�/@�j@z�@9X@(�@1@��@��@�m@��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A��mA��yA��yA��yA��yA��A��A��mA��`A��yA��A��mA��mA��mA��;A��;A��/A��#A��#A��AּjAָRAִ9A֬A֩�A֧�A֣�A֓uA֏\A�z�A�;dA�$�A��/AΧ�A���AʑhA���A�A�ȴA�$�A�t�A�;dA�Q�A�ĜA�33A��HA���A�5?A�jA��TA�  A�|�A�S�A��PA���A�oA��A�A�A��;A�-A���A�^5A��\A�-A�9XA���A��A��\A�v�A��A��wA��A�S�A��A�/A�S�A�?}A���A�33A�ffA��A��A��yA�bA��A�E�A���A�{A��A��RA�A�z�A�ƨA��uA��`A��#A��DA�oA�~�A�t�A�5?A��A��A��A�XA��A���A��A���A�p�A�M�A�ZA��A�7LA�ƨA��A���A��RA�M�A���A�hsA��A��A���A�l�A�5?A|��A{Ayl�Aw�Aw?}Av�uAt~�Aq�FAm�FAk|�AjZAh��Ag�Ag+Ae�hAc�A`�uA_K�A_�A_
=A^��A^�jA^n�A]��A]�A[�mA[;dAY��AW&�AU\)AS��AS�AR��AQC�APJAO`BAN��AL�!AK\)AJ��AI�AIC�AH1AE
=AD$�AB�DAA��AA%A@ �A?+A>��A=�A=VA<A�A;�hA: �A8r�A6��A5O�A4��A2��A/��A.��A.-A-�-A-�hA-`BA,�A,�A+p�A*r�A)S�A(E�A'S�A&=qA%�A%l�A%
=A$�yA$��A$�DA#�wA"�+A ��AffA��AXA��AS�A�A9XA��A��A1A�A�AI�A�HA�PA�jA�A�AI�Ax�A�A�PA
1'A	�A	%A1A�A(�A+A�A�A�!A��AVA �jA =q@�|�@�M�@�t�@�^5@�{@��@���@�bN@��@�
=@��T@��@���@���@�Z@�|�@��@�\@�X@�9@�b@�|�@�-@�7L@�9@�Q�@� �@�-@��@�O�@ڸR@��#@�;d@��`@�5?@��@�Z@��@ϕ�@�t�@�C�@ΰ!@�-@�X@�9X@��;@��@�X@ȣ�@��
@�S�@�K�@�S�@�S�@��@Ł@�%@ċD@î@���@��@�J@�r�@��!@���@�hs@�&�@���@��
@�~�@�@��/@�1@��F@�@�@��-@�&�@�1'@��@�`B@�r�@��
@��P@�@��T@��j@�l�@���@�$�@��h@���@��j@��@��m@�dZ@��T@�I�@�1@���@��@��@��;@���@��w@�K�@��@�E�@���@��@�7L@��@���@��u@�r�@�I�@�(�@�1@�ƨ@�|�@�33@�@�5?@�r�@�(�@���@��@�ƨ@�K�@���@��!@�^5@��7@�Z@��
@���@�+@�"�@��H@�^5@�J@��@���@���@�p�@���@�j@�A�@�A�@�I�@�I�@�I�@�b@��
@��@��y@��@�7L@��@��@��m@���@�K�@�33@��@��\@�V@�{@���@�&�@���@���@���@�j@�bN@�bN@�Q�@�9X@� �@�  @���@��@��
@��@�K�@�@��@���@�v�@�ff@�ff@�V@�M�@�M�@�@�@�&�@���@��u@��@��F@�t�@�\)@�33@�@�ȴ@���@�^5@��@���@���@���@���@��@�X@�&�@�r�@�1'@�1@��;@��F@��@�K�@��@��R@���@�n�@�-@��#@���@�p�@�X@�7L@�&�@��@�%@���@��@��/@���@�Q�@��@�w@~v�@}�T@}�T@}�T@}�T@}�T@}@|z�@{��@z��@y�7@x�`@xĜ@x�@xr�@x1'@w�;@w�@w\)@w;d@w�@vȴ@v��@vff@u��@u?}@t�@s�F@sS�@q��@q&�@p��@pĜ@pQ�@p �@p  @o��@o��@o�w@o�@o��@ol�@n��@nE�@n@m�T@m�T@m@m?}@m/@l��@l�/@l�j@l�@lj@k33@j=q@jJ@i�@i��@i��@i��@ix�@ihs@iG�@i7L@i�@h��@h�`@h�9@h�@g�@f��@e@e�-@e�@d�j@dZ@d9X@c33@c@c@c@c@b�H@b��@b^5@a�#@a��@a7L@`Ĝ@`Q�@_�@_l�@_
=@^�R@^�+@^5?@]�T@]��@]O�@]�@\��@\��@\Z@\9X@\�@[t�@[33@[o@Z�@Z��@YG�@X�@X �@Xb@X �@Xb@W�;@V�y@Vv�@V$�@U?}@T�D@TI�@T(�@S�m@S�F@S33@R��@R�\@RM�@R-@Q�@Q&�@P��@O��@O\)@O
=@N�@Nȴ@N�R@N��@Nff@M�@M@MO�@L�@Lj@L�@K��@KC�@K@JM�@I��@I��@I�7@H��@Hr�@G��@G+@F�@F��@Fv�@FE�@F{@E�T@E�T@E�-@E�h@EO�@E�@D�@D��@D�@C��@C@B�@B��@A��@A%@@�u@@�@@r�@@r�@@r�@@Q�@@Q�@@ �@@  @?��@?
=@>ff@>$�@>{@=�@=�@=�@=�@=�@=�T@=�-@=`B@<��@<�@<j@<9X@;�m@;t�@:�@:�\@:=q@9�^@97L@8��@8��@8�u@8 �@7\)@7�@6�@6��@6v�@6V@6ff@6ff@6V@6V@6V@6E�@65?@6$�@6@5�@5��@5�h@5�@5`B@5/@4��@4�j@4z�@4Z@4Z@4(�@3��@3ƨ@3��@3dZ@333@2��@2��@2n�@2M�@2M�@2=q@2�@1�@1��@1��@1��@1��@1�7@1hs@1G�@0��@0�u@0A�@0  @/�P@/;d@/
=@/
=@.��@.��@.�y@.�@.ȴ@.V@.5?@.@-p�@,��@,��@,�@+�
@+ƨ@+dZ@*�H@*��@*�\@*^5@*=q@*-@*-@*-@*J@)�#@)��@)x�@)G�@)G�@)%@(Ĝ@(��@(bN@( �@'�@'�;@'�w@'�P@'��@'+@&ff@&5?@&$�@%��@%p�@%?}@%�@$�/@$j@#�m@#�@#33@"�@"�\@"=q@"�@!��@!�7@!hs@!G�@ �`@ �u@ 1'@�@�@�P@\)@�@��@�@�@�@��@V@{@�-@�-@��@?}@V@��@I�@ƨ@ƨ@��@S�@C�@C�@"�@@�!@�\@~�@n�@�@�@�#@�^@�7@&�@��@1'@  @�;@�w@��@;d@
=@��@�y@��@V@@�-@?}@��@z�@j@Z@Z@I�@(�@1@�m@�
@�
@��@33@�H@�\@n�@^5@^5@^5@�@�@��@x�@X@&�@��@bN@1'@�;@�w@�P@l�@l�@K�@�y@��@�+@ff@$�@�@@��@�@p�@V@��@ƨ@��@t�@S�@S�@C�@"�@
��@
M�@
=q@
-@	�#@	�#@	��@	��@	�^@	��@	x�@�`@Ĝ@�9@�9@��@�u@bN@  @��@�P@+@�@�@ȴ@�@��@v�@V@$�@@@�@@��@�h@p�@p�@`B@?}@/@��@�/@�j@z�@9X@(�@1@��@��@�m@��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B-B	7BB%B1BJBuB{B�B�B�B �B�B�B�B"�B&�B+B@�B>wB=qBA�BB�BD�BH�BS�BaHBbNBaHBaHB`BB[#B^5BaHBdZB`BBbNBbNB`BB\)BXBVBS�BO�BJ�BG�BE�BD�B<jB5?B&�B"�B�BbBDBB��B�B�BŢB�!B�B��B�1B�B{�Bs�BdZBS�BL�BF�B33B�B�B�B�BuB\BJBB
�B
�ZB
�/B
��B
��B
ĜB
�-B
��B
��B
��B
�oB
�=B
|�B
m�B
VB
I�B
?}B
7LB
2-B
,B
�B
VB	��B	�mB	�BB	�B	��B	��B	B	�XB	��B	��B	��B	��B	��B	��B	��B	�uB	�oB	�JB	�=B	�B	t�B	hsB	]/B	ZB	W
B	W
B	R�B	N�B	J�B	=qB	2-B	-B	)�B	$�B	�B	oB		7B	B��B��B��B�B�B�B�B�`B�NB�B��B��BĜB�}B�dB�B��B��B��B��B��B��B��B��B��B��B�hB�\B�JB�=B�7B�1B�+B�+B�%B�B�B}�Bw�Bq�Bm�BiyBhsBffBdZBaHB_;B^5B\)B[#BZBW
BS�BP�BO�BL�BK�BI�BG�BF�BB�BA�B@�B@�B>wB>wB;dB9XB9XB6FB49B49B33B2-B1'B1'B1'B1'B2-B1'B1'B33B2-B2-B2-B/B2-B5?B2-B0!B.B(�B)�B(�B'�B'�B'�B+B.B.B.B0!B-B(�B'�B&�B,B.B33B33B49B7LB7LB:^B9XB:^B:^B;dB9XB8RB9XB9XB9XB9XB8RB8RB7LB7LB8RB8RB9XB9XB:^B;dB:^B<jB>wBB�BD�BD�BD�BF�BH�BL�BM�BP�BQ�BR�BT�BVBW
BW
B[#BaHBbNBe`BgmBhsBiyBm�Bp�Bv�By�B{�B}�B� B�B�B�B�B�DB�hB�oB�uB�uB�uB�{B�{B�{B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�B�9B�wB��B��B��BBŢBƨBȴB��B��B�
B�B�#B�5B�;B�ZB�B�B�B�B�B�B��B��B��B��B��B	  B	B	B	%B	1B	
=B	VB	VB	bB	oB	uB	�B	�B	�B	�B	 �B	#�B	&�B	+B	33B	49B	5?B	8RB	:^B	:^B	:^B	;dB	<jB	=qB	?}B	@�B	A�B	B�B	C�B	F�B	H�B	J�B	K�B	L�B	L�B	L�B	M�B	M�B	M�B	P�B	S�B	YB	[#B	]/B	bNB	e`B	gmB	gmB	hsB	iyB	jB	jB	k�B	m�B	n�B	o�B	p�B	q�B	r�B	s�B	t�B	x�B	z�B	{�B	|�B	}�B	}�B	~�B	� B	�B	�B	�%B	�%B	�%B	�%B	�B	�%B	�%B	�+B	�1B	�1B	�1B	�7B	�7B	�JB	�hB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�!B	�'B	�3B	�9B	�XB	�dB	�dB	�dB	�jB	�qB	�wB	�}B	�}B	��B	��B	��B	��B	ÖB	ĜB	ĜB	ŢB	ŢB	ŢB	ǮB	ǮB	ǮB	ȴB	ȴB	ȴB	ɺB	��B	��B	��B	��B	��B	�B	�B	�
B	�
B	�
B	�B	�B	�B	�B	�B	�B	�#B	�5B	�BB	�BB	�NB	�NB	�TB	�TB	�fB	�fB	�fB	�fB	�fB	�mB	�mB	�mB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
B
B
B
B
B
%B
+B
+B
1B
1B
	7B
	7B

=B

=B
JB
PB
VB
VB
VB
VB
VB
VB
\B
\B
bB
bB
hB
hB
oB
uB
uB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
"�B
#�B
$�B
$�B
$�B
$�B
$�B
$�B
$�B
%�B
%�B
%�B
&�B
'�B
(�B
(�B
(�B
(�B
(�B
(�B
(�B
(�B
)�B
)�B
)�B
+B
+B
+B
,B
-B
-B
.B
.B
/B
0!B
0!B
0!B
0!B
1'B
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
33B
33B
33B
33B
49B
49B
49B
49B
49B
49B
5?B
5?B
6FB
6FB
6FB
6FB
6FB
6FB
7LB
7LB
7LB
8RB
8RB
8RB
8RB
8RB
8RB
9XB
9XB
9XB
9XB
9XB
9XB
9XB
9XB
:^B
:^B
:^B
;dB
;dB
<jB
<jB
=qB
=qB
=qB
=qB
=qB
=qB
=qB
>wB
>wB
>wB
?}B
@�B
A�B
A�B
A�B
A�B
B�B
B�B
B�B
C�B
C�B
C�B
C�B
C�B
C�B
C�B
C�B
C�B
D�B
D�B
D�B
D�B
D�B
E�B
E�B
E�B
F�B
F�B
F�B
F�B
F�B
G�B
G�B
H�B
H�B
H�B
I�B
I�B
I�B
I�B
J�B
J�B
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
Q�B
Q�B
R�B
R�B
R�B
R�B
S�B
S�B
S�B
T�B
T�B
T�B
VB
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
W
B
XB
XB
XB
XB
XB
XB
ZB
ZB
ZB
ZB
ZB
[#B
[#B
[#B
[#B
[#B
\)B
\)B
\)B
]/B
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
`BB
`BB
`BB
aHB
aHB
aHB
aHB
aHB
aHB
aHB
bNB
bNB
bNB
cTB
cTB
cTB
dZB
dZB
dZB
dZB
dZB
dZB
e`B
e`B
e`B
e`B
ffB
ffB
ffB
ffB
ffB
ffB
gmB
hsB
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
jB
k�B
k�B
k�B
k�B
l�B
l�B
l�B
l�B
m�B
m�B
n�B
n�B
n�B
o�B
n�B
o�B
o�B
o�B
o�B
p�B
p�B
p�B
p�B
p�B
p�B
q�B
q�B
q�B
q�B
q�B
q�B
r�B
r�B
r�B
r�B
r�B
s�B
s�B
s�B
s�B
s�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B-B	7BB%B1BJBuB{B�B�B�B �B�B�B�B"�B&�B+B@�B>wB=qBA�BB�BD�BH�BS�BaHBbNBaHBaHB`BB[#B^5BaHBdZB`BBbNBbNB`BB\)BXBVBS�BO�BJ�BG�BE�BD�B<jB5?B&�B"�B�BbBDBB��B�B�BŢB�!B�B��B�1B�B{�Bs�BdZBS�BL�BF�B33B�B�B�B�BuB\BJBB
�B
�ZB
�/B
��B
��B
ĜB
�-B
��B
��B
��B
�oB
�=B
|�B
m�B
VB
I�B
?}B
7LB
2-B
,B
�B
VB	��B	�mB	�BB	�B	��B	��B	B	�XB	��B	��B	��B	��B	��B	��B	��B	�uB	�oB	�JB	�=B	�B	t�B	hsB	]/B	ZB	W
B	W
B	R�B	N�B	J�B	=qB	2-B	-B	)�B	$�B	�B	oB		7B	B��B��B��B�B�B�B�B�`B�NB�B��B��BĜB�}B�dB�B��B��B��B��B��B��B��B��B��B��B�hB�\B�JB�=B�7B�1B�+B�+B�%B�B�B}�Bw�Bq�Bm�BiyBhsBffBdZBaHB_;B^5B\)B[#BZBW
BS�BP�BO�BL�BK�BI�BG�BF�BB�BA�B@�B@�B>wB>wB;dB9XB9XB6FB49B49B33B2-B1'B1'B1'B1'B2-B1'B1'B33B2-B2-B2-B/B2-B5?B2-B0!B.B(�B)�B(�B'�B'�B'�B+B.B.B.B0!B-B(�B'�B&�B,B.B33B33B49B7LB7LB:^B9XB:^B:^B;dB9XB8RB9XB9XB9XB9XB8RB8RB7LB7LB8RB8RB9XB9XB:^B;dB:^B<jB>wBB�BD�BD�BD�BF�BH�BL�BM�BP�BQ�BR�BT�BVBW
BW
B[#BaHBbNBe`BgmBhsBiyBm�Bp�Bv�By�B{�B}�B� B�B�B�B�B�DB�hB�oB�uB�uB�uB�{B�{B�{B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�B�9B�wB��B��B��BBŢBƨBȴB��B��B�
B�B�#B�5B�;B�ZB�B�B�B�B�B�B��B��B��B��B��B	  B	B	B	%B	1B	
=B	VB	VB	bB	oB	uB	�B	�B	�B	�B	 �B	#�B	&�B	+B	33B	49B	5?B	8RB	:^B	:^B	:^B	;dB	<jB	=qB	?}B	@�B	A�B	B�B	C�B	F�B	H�B	J�B	K�B	L�B	L�B	L�B	M�B	M�B	M�B	P�B	S�B	YB	[#B	]/B	bNB	e`B	gmB	gmB	hsB	iyB	jB	jB	k�B	m�B	n�B	o�B	p�B	q�B	r�B	s�B	t�B	x�B	z�B	{�B	|�B	}�B	}�B	~�B	� B	�B	�B	�%B	�%B	�%B	�%B	�B	�%B	�%B	�+B	�1B	�1B	�1B	�7B	�7B	�JB	�hB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�!B	�'B	�3B	�9B	�XB	�dB	�dB	�dB	�jB	�qB	�wB	�}B	�}B	��B	��B	��B	��B	ÖB	ĜB	ĜB	ŢB	ŢB	ŢB	ǮB	ǮB	ǮB	ȴB	ȴB	ȴB	ɺB	��B	��B	��B	��B	��B	�B	�B	�
B	�
B	�
B	�B	�B	�B	�B	�B	�B	�#B	�5B	�BB	�BB	�NB	�NB	�TB	�TB	�fB	�fB	�fB	�fB	�fB	�mB	�mB	�mB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
B
B
B
B
B
%B
+B
+B
1B
1B
	7B
	7B

=B

=B
JB
PB
VB
VB
VB
VB
VB
VB
\B
\B
bB
bB
hB
hB
oB
uB
uB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
"�B
#�B
$�B
$�B
$�B
$�B
$�B
$�B
$�B
%�B
%�B
%�B
&�B
'�B
(�B
(�B
(�B
(�B
(�B
(�B
(�B
(�B
)�B
)�B
)�B
+B
+B
+B
,B
-B
-B
.B
.B
/B
0!B
0!B
0!B
0!B
1'B
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
33B
33B
33B
33B
49B
49B
49B
49B
49B
49B
5?B
5?B
6FB
6FB
6FB
6FB
6FB
6FB
7LB
7LB
7LB
8RB
8RB
8RB
8RB
8RB
8RB
9XB
9XB
9XB
9XB
9XB
9XB
9XB
9XB
:^B
:^B
:^B
;dB
;dB
<jB
<jB
=qB
=qB
=qB
=qB
=qB
=qB
=qB
>wB
>wB
>wB
?}B
@�B
A�B
A�B
A�B
A�B
B�B
B�B
B�B
C�B
C�B
C�B
C�B
C�B
C�B
C�B
C�B
C�B
D�B
D�B
D�B
D�B
D�B
E�B
E�B
E�B
F�B
F�B
F�B
F�B
F�B
G�B
G�B
H�B
H�B
H�B
I�B
I�B
I�B
I�B
J�B
J�B
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
Q�B
Q�B
R�B
R�B
R�B
R�B
S�B
S�B
S�B
T�B
T�B
T�B
VB
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
W
B
XB
XB
XB
XB
XB
XB
ZB
ZB
ZB
ZB
ZB
[#B
[#B
[#B
[#B
[#B
\)B
\)B
\)B
]/B
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
`BB
`BB
`BB
aHB
aHB
aHB
aHB
aHB
aHB
aHB
bNB
bNB
bNB
cTB
cTB
cTB
dZB
dZB
dZB
dZB
dZB
dZB
e`B
e`B
e`B
e`B
ffB
ffB
ffB
ffB
ffB
ffB
gmB
hsB
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
jB
k�B
k�B
k�B
k�B
l�B
l�B
l�B
l�B
m�B
m�B
n�B
n�B
n�B
o�B
n�B
o�B
o�B
o�B
o�B
p�B
p�B
p�B
p�B
p�B
p�B
q�B
q�B
q�B
q�B
q�B
q�B
r�B
r�B
r�B
r�B
r�B
s�B
s�B
s�B
s�B
s�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.02 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20211001110043                              AO  ARCAADJP                                                                    20211001110043    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20211001110043  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20211001110043  QCF$                G�O�G�O�G�O�0               