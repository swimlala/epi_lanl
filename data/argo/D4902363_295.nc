CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2018-10-29T00:35:20Z creation;2018-10-29T00:35:27Z conversion to V3.1;2019-12-19T07:29:22Z update;     
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    7   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  7   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7T   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     8   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     88   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8X   JULD               standard_name         time   	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~       axis      T           8\   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8d   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~            8h   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8p   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8x   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8�   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8�   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    8�   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    8�   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    8�   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8�   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9�   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        H  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  H�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     H  L�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \    TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     H  _�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  o   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     H  r�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �8   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     H  �   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �T   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     H  �(   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �p   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     H  �D   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     H  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     H  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  �   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   ڬ   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �,   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �0   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �4   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �8   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �<   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �|   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20181029003520  20200115131517  4902363                                                                 JAMSTEC                                                         PRES            TEMP            PSAL              'A   JA  I2_0576_295                     2C  D   NAVIS_A                         0576                            ARGO 092515                     863 @،u�N� 1   @،v}'Ҁ@9�b���d1�A [�1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @���@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C!�fC$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D�fD  D� D  D� D  D� D  D� D  D� D��D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$�fD%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV�fDW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D���D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D޼�D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D��3D�3D�@ D� D�� D�  D�@ D�fD���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�=q@���A Q�A Q�A@Q�A`Q�A�(�A�(�A�(�A�(�A�(�A�(�A�(�A�(�B {B{B{B{B {B({B0{B8{B@{BH{BP{BX{B`{Bh{Bp{Bx{B�
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
=B�
=B�
=B�
=B�
=B�
=B�
=C CCCCC
CCCCCCCCCCC C!�C$C&C(C*C,C.C0C2C4C6C8C:C<C>C@CBCDCFCHCJCLCNCPCRCTCVCXCZC\C^C`CbCdCfChCjClCnCpCrCtCvCxCzC|C~C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��D HD �HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HD	HD	�HD
HD
�HDHD�HDHD�HDHD�HDHD�HDHD��DHD�HDHD�HDHD�HDHD�HDHD�HD��D�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HD HD �HD!HD!�HD"HD"�HD#HD#�HD$HD$��D%HD%�HD&HD&�HD'HD'�HD(HD(�HD)HD)�HD*HD*�HD+HD+�HD,HD,�HD-HD-�HD.HD.�HD/HD/�HD0HD0�HD1HD1�HD2HD2�HD3HD3�HD4HD4�HD5HD5�HD6HD6�HD7HD7�HD8HD8�HD9HD9�HD:HD:�HD;HD;�HD<HD<�HD=HD=�HD>HD>�HD?HD?�HD@HD@�HDAHDA�HDBHDB�HDCHDC�HDDHDD�HDEHDE�HDFHDF�HDGHDG�HDHHDH�HDIHDI�HDJHDJ�HDKHDK�HDLHDL�HDMHDM�HDNHDN�HDOHDO�HDPHDP�HDQHDQ�HDRHDR�HDSHDS�HDTHDT�HDUHDU�HDVHDV��DWHDW�HDXHDX�HDYHDY�HDZHDZ�HD[HD[�HD\HD\�HD]HD]�HD^HD^�HD_HD_�HD`HD`�HDaHDa�HDbHDb�HDcHDc�HDdHDd�HDeHDe�HDfHDf�HDgHDg�HDhHDh�HDiHDi�HDjHDj�HDkHDk�HDlHDl�HDmHDm�HDnHDn�HDoHDo�HDpHDp�HDqHDq�HDrHDr�HDsHDs�HDtHDt�HDuHDu�HDvHDv�HDwHDw�HDxHDx�HDyHDy�HDzHDz�HD{HD{�HD|HD|�HD}HD}�HD~HD~�HDHD�HD� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D�D���D� �D�@�DÀ�D���D� �D�@�DĀ�D���D� �D�@�Dŀ�D���D� �D�@�Dƀ�D���D� �D�@�Dǀ�D���D� �D�@�DȀ�D���D� �D�@�Dɀ�D���D� �D�@�Dʀ�D���D� �D�@�Dˀ�D���D� �D�@�D̀�D���D� �D�@�D̀�D���D� �D�@�D΀�D���D� �D�@�Dπ�D���D� �D�@�DЀ�D���D� �D�@�Dр�D���D� �D�@�DҀ�D���D� �D�@�DӀ�D���D� �D�@�DԀ�D���D� �D�@�DՀ�D���D� �D�@�Dր�D���D� �D�@�D׀�D���D��qD�@�D؀�D���D� �D�@�Dـ�D���D� �D�@�Dڀ�D���D� �D�@�Dۀ�D���D� �D�@�D܀�D���D� �D�@�D݀�D���D� �D�@�Dހ�D޽qD� �D�@�D߀�D���D� �D�@�D���D���D� �D�@�DဤD���D� �D�@�D․D���D� �D�@�D〤D���D� �D�@�D䀤D���D� �D�@�D値D���D� �D�@�D怤D���D� �D�@�D瀤D���D� �D�@�D耤D���D� �D�@�D逤D���D� �D�@�DꀤD���D� �D�@�D뀤D���D� �D�@�D쀤D���D� �D�@�D퀤D���D� �D�@�DD���D� �D�@�DD���D� �D�@�D���D���D� �D�@�D�D���D� �D�@�D�D���D��D�@�D�D���D� �D�@�D�
D��=111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A��A��mA��TA��/A��;A��A��
A��
A���A���A�ȴA���A�ȴA���A���A�ȴA�ƨA�ĜA̰!A�`BA�
=AɾwA�`BA��Aǲ-A�  Aš�A�A��A��A7A���A��+A�G�A�9XA�Q�A���A�\)A�Q�A���A��A�C�A�"�A��9A���A���A���A�XA��A�t�A�bA��A���A�Q�A�O�A���A��RA��\A�bNA���A�ƨA���A��yA�ZA�VA��7A� �A�v�A��A��A�JA���A���A�XA��#A�%A�"�A�1'A��-A�I�A��9A�v�A�z�A��A�jA�ȴA���A��FA�|�A��7A�(�A�
=A��A~�A}/Az��Az-AzAy�TAy��Ay?}Ax(�Awp�AvE�As��Ar=qAqhsAp�Ao�An�9An�Al~�Ak%Aj(�Ag��Af��Ad��Ac33Ab�DAa��A`��A]ƨA[33AY��AX�`AW��AVbNAU�7AU7LAT�HAT�!AT�DATQ�AS�TAR�AQ��AP�+AP1AOK�AN��AMƨAK�-AJȴAI�FAHĜAG+AEl�ADn�AD �AB��A@��A@  A?\)A>�`A>z�A=��A<��A;dZA9�
A8=qA6��A6{A5�#A4I�A3��A3S�A2^5A0�jA0v�A/S�A.�!A.  A-VA,{A+�A*{A)/A(r�A'�wA&�`A%��A$�A$�A#�;A#p�A#/A"��A!��A �DAȴAA`BA~�A�hAƨA��An�A�A�AĜAr�A�A��A�DA=qA�^A�jA1'Al�A�A�#A?}A�A�
A+A�A��A5?A
1'A	��A�DA33A�AVA{A�/A��AA^5A�A�TA�7AC�A �A I�@�J@��@���@�&�@��
@�\)@��@��@���@�{@��@�b@�F@�@�|�@��@�I�@�E�@�9@띲@�ff@�9@�;d@�G�@�@��@�@ߕ�@��@��y@ާ�@ݲ-@�  @ۥ�@�n�@��@ف@��@أ�@أ�@�j@ׅ@��@�%@�o@·+@�%@�l�@��@�~�@��@� �@Ǯ@�33@ư!@���@�n�@�j@��!@�$�@�{@��#@���@�?}@�z�@��P@�;d@��@�1'@���@���@��@�x�@�1'@�dZ@�~�@�-@�@�X@��@��j@�33@�$�@�hs@�(�@�dZ@�C�@�@�ȴ@��\@�M�@��T@�G�@��@��/@�bN@��R@�=q@��@���@��@�p�@��9@��D@�bN@�Q�@�  @���@�\)@��@�
=@��@��!@��\@��\@�~�@��@�V@��@�$�@��`@�33@��!@�{@���@���@�1@�ȴ@��@��@�Z@��@�C�@��R@�E�@���@���@���@�hs@�j@��w@��P@���@�|�@�C�@�ȴ@���@���@��7@�p�@�%@��9@�j@���@�|�@�S�@�33@���@�J@�p�@�/@�&�@�V@��/@�Z@�b@���@���@�C�@��@�
=@�@��y@�ȴ@���@�^5@�J@���@���@��@�x�@�O�@�V@��@��@��P@�+@���@��H@��@���@���@���@�~�@�n�@�^5@�M�@�E�@�=q@�5?@�-@�-@�@���@��@��@��j@��j@���@���@���@��j@��@��@���@�z�@�bN@�A�@��@�  @�w@��@K�@
=@~��@~��@~��@
=@
=@~��@~��@~��@~��@~��@~�y@~ȴ@~ȴ@~ȴ@~ȴ@~�R@~�R@~��@~��@~ff@~$�@}�T@}O�@|�D@{��@z�H@z~�@zn�@z-@zJ@zJ@zJ@zJ@y�#@y�#@y�#@y�#@y�@y�@y�@y��@yx�@y�@x��@xQ�@w�@w��@w�w@wK�@w�@vȴ@v�R@vv�@u��@t�j@t(�@s��@s��@sC�@rJ@p�9@pbN@pb@o��@o|�@o�@nv�@n{@m�@lI�@k��@jJ@ihs@iX@i�@h��@h�u@h�@h�@hbN@hbN@hQ�@hb@g�@f�+@e�@eV@d��@d�@d��@d��@d�j@c�@b�H@b�@aG�@a&�@`�`@`�u@_�;@_\)@_+@_
=@^�@^��@^ff@^V@^$�@]�-@]V@\�@\z�@\1@[33@[o@Z��@Z��@Y��@Yx�@X�9@XA�@W;d@V{@U��@U�-@U�@UV@S�
@R�@RM�@RJ@QG�@P��@P�9@P�u@PbN@PQ�@PA�@P1'@P �@P �@Pb@P  @P  @P  @O�@O�w@O\)@O\)@N�@N��@N�+@N�+@Nff@N{@M�@M��@M��@M�h@M?}@L�@L�j@L�@L��@L�D@Lj@LZ@LI�@LI�@LI�@L9X@LI�@L�@Kƨ@KdZ@KS�@KC�@Ko@J��@I�@Ihs@G�w@F�R@F��@F�+@F��@F��@F�R@F�y@F�@F�R@Fȴ@G
=@G�@G
=@F�y@F5?@D�@D�@D��@C��@Ct�@C"�@B��@B�!@B��@BJ@A�7@A�@@A�@@  @?��@?��@?\)@?;d@>�@>ff@>5?@>{@=@=p�@=V@<�/@<9X@;�m@;ƨ@;�@;S�@;o@:��@:~�@:M�@:-@:�@:�@:J@:J@:J@:J@9�@9��@97L@8��@8�u@8�u@8�u@8�@6�y@5�T@5��@5O�@5/@4��@4Z@2n�@1hs@1�@0�u@01'@/|�@/�@.�@.�@.ȴ@.��@.$�@-�h@,�/@,z�@,�@+��@+dZ@+33@+33@+"�@+33@+"�@+o@*�H@*��@*~�@*-@*J@)�#@)�^@)�^@)��@)�^@)��@)x�@)hs@)X@)7L@)&�@(�`@(�@(  @'�@'�P@'\)@'+@&��@&ȴ@&��@&ff@&ff@&V@&$�@&{@&{@&@&@%�T@%@%�-@%�-@%��@%`B@%/@%/@%�@$�/@$�D@$j@$z�@$j@$j@$Z@$I�@$I�@$I�@$(�@#ƨ@#�@#S�@#o@"�H@"��@"��@"n�@"^5@"M�@"-@!��@!�@!�#@!�^@!��@!��@!��@!��@!x�@!X@!7L@ Ĝ@ Q�@ A�@ 1'@   @��@�P@��@�+@v�@E�@�@�h@�@�@��@�j@z�@I�@1@�m@ƨ@��@t�@dZ@S�@33@"�@o@�@�H@�H@��@��@�!@��@�\@n�@M�@=q@-@�@�#@��@��@�^@�^@��@G�@  @�@��@K�@�y@�R@��@��@�+@�+@v�@v�@ff@E�@5?@{@@��@��@��@��@��@�@`B@O�@?}@V@��@��@�@�@�/@�j@�D@z�@(�@�F@�@C�@�@��@��@��@n�@J@7L@r�@�y@��@�+@�+@�+@�+@�+@�+@v�@V@$�@$�@�@�@�T@@@�-@?}@/@�@��@�@�/@��@�j@�j@�@�@C�@
�\@	�^@	��@	7L@��@��@��@Q�@A�@�@�@K�@�y@�R@ff@V@{@�T@��@�h@�@O�@O�@�@��@�/@��@j@j@Z@I�@Z@Z@Z@I�@9X@9X@9X@9X111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A��A��mA��TA��/A��;A��A��
A��
A���A���A�ȴA���A�ȴA���A���A�ȴA�ƨA�ĜA̰!A�`BA�
=AɾwA�`BA��Aǲ-A�  Aš�A�A��A��A7A���A��+A�G�A�9XA�Q�A���A�\)A�Q�A���A��A�C�A�"�A��9A���A���A���A�XA��A�t�A�bA��A���A�Q�A�O�A���A��RA��\A�bNA���A�ƨA���A��yA�ZA�VA��7A� �A�v�A��A��A�JA���A���A�XA��#A�%A�"�A�1'A��-A�I�A��9A�v�A�z�A��A�jA�ȴA���A��FA�|�A��7A�(�A�
=A��A~�A}/Az��Az-AzAy�TAy��Ay?}Ax(�Awp�AvE�As��Ar=qAqhsAp�Ao�An�9An�Al~�Ak%Aj(�Ag��Af��Ad��Ac33Ab�DAa��A`��A]ƨA[33AY��AX�`AW��AVbNAU�7AU7LAT�HAT�!AT�DATQ�AS�TAR�AQ��AP�+AP1AOK�AN��AMƨAK�-AJȴAI�FAHĜAG+AEl�ADn�AD �AB��A@��A@  A?\)A>�`A>z�A=��A<��A;dZA9�
A8=qA6��A6{A5�#A4I�A3��A3S�A2^5A0�jA0v�A/S�A.�!A.  A-VA,{A+�A*{A)/A(r�A'�wA&�`A%��A$�A$�A#�;A#p�A#/A"��A!��A �DAȴAA`BA~�A�hAƨA��An�A�A�AĜAr�A�A��A�DA=qA�^A�jA1'Al�A�A�#A?}A�A�
A+A�A��A5?A
1'A	��A�DA33A�AVA{A�/A��AA^5A�A�TA�7AC�A �A I�@�J@��@���@�&�@��
@�\)@��@��@���@�{@��@�b@�F@�@�|�@��@�I�@�E�@�9@띲@�ff@�9@�;d@�G�@�@��@�@ߕ�@��@��y@ާ�@ݲ-@�  @ۥ�@�n�@��@ف@��@أ�@أ�@�j@ׅ@��@�%@�o@·+@�%@�l�@��@�~�@��@� �@Ǯ@�33@ư!@���@�n�@�j@��!@�$�@�{@��#@���@�?}@�z�@��P@�;d@��@�1'@���@���@��@�x�@�1'@�dZ@�~�@�-@�@�X@��@��j@�33@�$�@�hs@�(�@�dZ@�C�@�@�ȴ@��\@�M�@��T@�G�@��@��/@�bN@��R@�=q@��@���@��@�p�@��9@��D@�bN@�Q�@�  @���@�\)@��@�
=@��@��!@��\@��\@�~�@��@�V@��@�$�@��`@�33@��!@�{@���@���@�1@�ȴ@��@��@�Z@��@�C�@��R@�E�@���@���@���@�hs@�j@��w@��P@���@�|�@�C�@�ȴ@���@���@��7@�p�@�%@��9@�j@���@�|�@�S�@�33@���@�J@�p�@�/@�&�@�V@��/@�Z@�b@���@���@�C�@��@�
=@�@��y@�ȴ@���@�^5@�J@���@���@��@�x�@�O�@�V@��@��@��P@�+@���@��H@��@���@���@���@�~�@�n�@�^5@�M�@�E�@�=q@�5?@�-@�-@�@���@��@��@��j@��j@���@���@���@��j@��@��@���@�z�@�bN@�A�@��@�  @�w@��@K�@
=@~��@~��@~��@
=@
=@~��@~��@~��@~��@~��@~�y@~ȴ@~ȴ@~ȴ@~ȴ@~�R@~�R@~��@~��@~ff@~$�@}�T@}O�@|�D@{��@z�H@z~�@zn�@z-@zJ@zJ@zJ@zJ@y�#@y�#@y�#@y�#@y�@y�@y�@y��@yx�@y�@x��@xQ�@w�@w��@w�w@wK�@w�@vȴ@v�R@vv�@u��@t�j@t(�@s��@s��@sC�@rJ@p�9@pbN@pb@o��@o|�@o�@nv�@n{@m�@lI�@k��@jJ@ihs@iX@i�@h��@h�u@h�@h�@hbN@hbN@hQ�@hb@g�@f�+@e�@eV@d��@d�@d��@d��@d�j@c�@b�H@b�@aG�@a&�@`�`@`�u@_�;@_\)@_+@_
=@^�@^��@^ff@^V@^$�@]�-@]V@\�@\z�@\1@[33@[o@Z��@Z��@Y��@Yx�@X�9@XA�@W;d@V{@U��@U�-@U�@UV@S�
@R�@RM�@RJ@QG�@P��@P�9@P�u@PbN@PQ�@PA�@P1'@P �@P �@Pb@P  @P  @P  @O�@O�w@O\)@O\)@N�@N��@N�+@N�+@Nff@N{@M�@M��@M��@M�h@M?}@L�@L�j@L�@L��@L�D@Lj@LZ@LI�@LI�@LI�@L9X@LI�@L�@Kƨ@KdZ@KS�@KC�@Ko@J��@I�@Ihs@G�w@F�R@F��@F�+@F��@F��@F�R@F�y@F�@F�R@Fȴ@G
=@G�@G
=@F�y@F5?@D�@D�@D��@C��@Ct�@C"�@B��@B�!@B��@BJ@A�7@A�@@A�@@  @?��@?��@?\)@?;d@>�@>ff@>5?@>{@=@=p�@=V@<�/@<9X@;�m@;ƨ@;�@;S�@;o@:��@:~�@:M�@:-@:�@:�@:J@:J@:J@:J@9�@9��@97L@8��@8�u@8�u@8�u@8�@6�y@5�T@5��@5O�@5/@4��@4Z@2n�@1hs@1�@0�u@01'@/|�@/�@.�@.�@.ȴ@.��@.$�@-�h@,�/@,z�@,�@+��@+dZ@+33@+33@+"�@+33@+"�@+o@*�H@*��@*~�@*-@*J@)�#@)�^@)�^@)��@)�^@)��@)x�@)hs@)X@)7L@)&�@(�`@(�@(  @'�@'�P@'\)@'+@&��@&ȴ@&��@&ff@&ff@&V@&$�@&{@&{@&@&@%�T@%@%�-@%�-@%��@%`B@%/@%/@%�@$�/@$�D@$j@$z�@$j@$j@$Z@$I�@$I�@$I�@$(�@#ƨ@#�@#S�@#o@"�H@"��@"��@"n�@"^5@"M�@"-@!��@!�@!�#@!�^@!��@!��@!��@!��@!x�@!X@!7L@ Ĝ@ Q�@ A�@ 1'@   @��@�P@��@�+@v�@E�@�@�h@�@�@��@�j@z�@I�@1@�m@ƨ@��@t�@dZ@S�@33@"�@o@�@�H@�H@��@��@�!@��@�\@n�@M�@=q@-@�@�#@��@��@�^@�^@��@G�@  @�@��@K�@�y@�R@��@��@�+@�+@v�@v�@ff@E�@5?@{@@��@��@��@��@��@�@`B@O�@?}@V@��@��@�@�@�/@�j@�D@z�@(�@�F@�@C�@�@��@��@��@n�@J@7L@r�@�y@��@�+@�+@�+@�+@�+@�+@v�@V@$�@$�@�@�@�T@@@�-@?}@/@�@��@�@�/@��@�j@�j@�@�@C�@
�\@	�^@	��@	7L@��@��@��@Q�@A�@�@�@K�@�y@�R@ff@V@{@�T@��@�h@�@O�@O�@�@��@�/@��@j@j@Z@I�@Z@Z@Z@I�@9X@9X@9X@9X111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  BĜBĜBŢBŢBŢBŢBŢBŢBĜBŢBŢBŢBŢBŢBĜBŢBĜBB�wBÖB�NB��B��B�B��B��B��B%B��BbB%BB
=B/B�B�B$�BK�BE�BbNBe`B^5BF�B �B �B+B#�B7LBF�BH�B=qB1'B�B!�B�BPB��B�`B�B�;B��B��B��B��B�FB�^B�B��B�=B�BjB?}BD�BH�B9XB.B�B�B
��B
��B
�NB
��B
��B
��B
ÖB
�?B
�FB
�B
��B
�bB
�B
l�B
n�B
[#B
L�B
ZB
]/B
ZB
T�B
K�B
<jB
8RB
(�B
�B
JB
oB
+B
B
B	��B	�B	�5B	�#B	ɺB	ÖB	�^B	�B	�-B	��B	��B	�B	u�B	v�B	z�B	s�B	m�B	m�B	r�B	q�B	q�B	o�B	l�B	ffB	_;B	W
B	Q�B	T�B	N�B	K�B	A�B	1'B	/B	-B	"�B	�B	JB	bB	hB	B�B	  B��B��B��B�B�B�)B��B��B��B��B��BĜBĜBƨB�qB�!B�jB�-B�B�B��B��B��B��B��B��B�hB�hB�+B�7B�1B�JB�1B�%B� Bs�Bl�B`BBiyBffB`BB\)BO�BT�BbNBZB[#B`BBZBN�B^5B\)BYBT�BN�BO�BN�BM�BJ�BI�BN�BG�BG�BN�BK�BB�B/B@�B=qB8RBB�BE�BC�B9XB6FB<jB>wBC�BB�B@�B?}B=qB6FB/B49B49B49B7LB<jB=qB=qB;dB7LB5?B5?B9XB:^B6FB,B+B)�B,B.B.B)�B+B(�B%�B/B1'B49B8RB9XB7LB33B.B8RB49B9XB9XB8RB;dB;dB8RB1'B-B,B%�B�B+B/B8RB8RB2-B1'B8RB7LB49B,B&�B.B<jBG�BL�BJ�BI�BG�BE�BC�BH�BC�BC�BN�BO�BP�BVBQ�BZB[#B_;B`BB_;B`BB_;BZB]/BdZBdZBiyBu�Bv�Bw�Bv�Bv�Bv�Bv�B{�By�Bx�Bt�B�%B�DB�\B�uB�uB�hB�uB�{B��B��B��B��B��B��B��B��B��B�B�B��B��B��B��B�B�B�3B�9B�?B�-B��B�B�B�dB�jB�dB�^B�jB�}BÖBǮBɺBȴB��B��B�)B�HB�HB�NB�TB�mB�`B�B�B�B�B�B�B��B��B��B�B��B��B��B	B	B	B	B	
=B	VB	oB	uB	�B	�B	%�B	'�B	(�B	+B	,B	-B	/B	2-B	33B	6FB	6FB	7LB	9XB	8RB	8RB	C�B	K�B	N�B	Q�B	R�B	S�B	T�B	W
B	YB	YB	ZB	[#B	\)B	\)B	\)B	]/B	\)B	^5B	dZB	e`B	jB	r�B	u�B	v�B	w�B	x�B	z�B	{�B	|�B	|�B	� B	�B	�B	�%B	�%B	�1B	�1B	�=B	�JB	�PB	�VB	�VB	�VB	�VB	�VB	�VB	�VB	�VB	�PB	�VB	�\B	�bB	�bB	�bB	�bB	�hB	�hB	�hB	�hB	�oB	�oB	�{B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�!B	�!B	�!B	�B	�B	�B	�B	�B	�'B	�-B	�9B	�FB	�LB	�FB	�LB	�LB	�RB	�RB	�LB	�LB	�jB	�wB	�wB	�qB	�jB	�jB	��B	��B	��B	B	��B	��B	��B	��B	�}B	ŢB	ĜB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�B	�B	�B	�B	��B	�B	�#B	�/B	�HB	�HB	�HB	�HB	�TB	�fB	�fB	�fB	�mB	�mB	�sB	�sB	�sB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B
B
B
B
B
+B
+B
+B
1B
1B
1B
1B
	7B
	7B
	7B
	7B
	7B

=B

=B

=B
PB
PB
VB
\B
bB
bB
\B
\B
bB
\B
bB
\B
\B
bB
hB
hB
hB
hB
hB
oB
oB
oB
hB
oB
oB
uB
�B
�B
�B
�B
�B
{B
�B
uB
�B
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
"�B
!�B
 �B
�B
�B
#�B
%�B
#�B
#�B
%�B
&�B
'�B
'�B
%�B
%�B
&�B
%�B
+B
,B
-B
-B
-B
,B
-B
.B
.B
-B
-B
.B
/B
.B
/B
0!B
0!B
1'B
1'B
1'B
2-B
33B
33B
33B
49B
49B
49B
49B
33B
33B
2-B
2-B
2-B
33B
5?B
49B
2-B
/B
0!B
5?B
5?B
5?B
49B
1'B
.B
2-B
9XB
8RB
9XB
9XB
;dB
<jB
>wB
=qB
=qB
;dB
;dB
;dB
=qB
=qB
=qB
>wB
?}B
?}B
?}B
@�B
?}B
?}B
?}B
>wB
?}B
>wB
?}B
@�B
@�B
A�B
A�B
@�B
@�B
@�B
A�B
A�B
A�B
A�B
@�B
?}B
@�B
A�B
B�B
B�B
B�B
C�B
C�B
C�B
D�B
D�B
D�B
D�B
D�B
E�B
D�B
E�B
E�B
E�B
E�B
E�B
E�B
E�B
E�B
F�B
F�B
E�B
E�B
G�B
G�B
G�B
G�B
G�B
G�B
G�B
G�B
G�B
F�B
G�B
H�B
H�B
I�B
I�B
I�B
J�B
K�B
J�B
K�B
K�B
K�B
L�B
L�B
L�B
L�B
L�B
L�B
L�B
L�B
L�B
K�B
L�B
N�B
N�B
N�B
N�B
M�B
M�B
N�B
P�B
P�B
P�B
O�B
P�B
Q�B
R�B
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
T�B
T�B
VB
VB
VB
VB
VB
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
XB
XB
XB
W
B
W
B
VB
R�B
ZB
YB
YB
ZB
[#B
\)B
\)B
\)B
\)B
\)B
\)B
\)B
\)B
\)B
\)B
\)B
]/B
^5B
^5B
^5B
^5B
]/B
]/B
^5B
^5B
^5B
^5B
_;B
_;B
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
`BB
aHB
aHB
`BB
_;B
^5B
^5B
^5B
e`B
ffB
gmB
gmB
gmB
gmB
gmB
gmB
ffB
gmB
gmB
gmB
hsB
gmB
gmB
hsB
gmB
gmB
hsB
hsB
hsB
iyB
hsB
hsB
hsB
hsB
ffB
ffB
hsB
gmB
hsB
k�B
k�B
l�B
l�B
l�B
m�B
m�B
l�B
m�B
m�B
m�B
n�B
o�B
p�B
p�B
p�B
q�B
p�B
q�B
q�B
r�B
q�B
q�B
q�B
q�B
r�B
s�B
s�B
s�B
t�B
t�B
s�B
s�B
s�B
t�B
t�B
t�B
t�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  BĶBĜBŢBŢBŢBŢBŢBŢBĜBŢBŢBŢBŢBŢBĜBżB��B�B��B�tB�B��B�BB�?B��B�B��B�B  B�B1B�B�B/�B"B�B(sBN"BH�Bc�Bg�Ba�BL�B(
B'�B�B(XB9�BG�BI�B>�B3hB �B#�ByB�B��B�sB��B�HB�9B��B�HB��B�B�B��B��B�B�BnBD3BG+BJ#B;dB0!B�B
B
��B
�zB
�B
�,B
�MB
�6B
�B
��B
��B
�B
��B
��B
��B
n�B
pUB
]dB
OvB
Z�B
]dB
ZQB
UMB
L�B
=�B
9rB
*�B
1B
VB
�B
�B
3B
�B	�B	�]B	�B	ܒB	�0B	�9B	��B	��B	�B	�6B	��B	��B	x�B	x�B	|6B	u?B	oB	n�B	sB	rB	q�B	o�B	l�B	gB	`�B	X�B	SuB	U�B	O�B	L�B	C-B	3�B	0oB	.}B	$ZB	�B	VB	�B	:B	�B�9B	 �B��B��B��B��B�B�OB�B��B�vBοB�}B�tBňB�_B��B�-B�B��B�!B�!B�FB�B�B��B��B��B��B��B��B�XB�7B��B��B��B��ButBn/Bb�BjBgRBa�B]�BR BVmBb�B[WB[�B`�BZ�BP�B^jB\�BY�BU�BP.BP�BO�BN�BLBJ�BOvBIBH�BOBBLJBC�B1�BA�B>�B:BCGBF?BDMB;B7�B=VB?HBC�BB�BA B@ B>(B7�B0�B5ZB5tB5ZB8B<�B=�B=�B;�B7�B5�B5�B9�B:�B6�B-]B,"B+kB-)B/ B/ B+QB,=B*eB'RB/�B1�B4�B8�B9�B7�B3�B/5B8�B5B9�B9�B8�B;�B;�B8�B2B.B-B'�BB,B0!B8�B8�B3B1�B8�B7�B4�B-�B(�B/�B=VBG�BMBKBJ	BHBF%BDMBIBD�BD�BO\BP�BQ�BV�BR�BZ�B[�B_pB`�B_�B`�B_�B[#B^Bd�BeFBi�Bu�BwBxBv�BwBw2Bw2B|Bz*ByrBu�B�tB��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�*B�"B�6B�sB��B��B�FB��B�5B��B��B�tB��B�6B��B�B��B��B��B��B��B��B��B��B��B�7B�xB�aB�]B�-B�|B�B�B�B��B�B��B��B��B��B�B�B��B��B�3B�?B�8B�(B	-B	GB	GB	aB	
rB	�B	�B	�B	�B	�B	%�B	(
B	)*B	+B	,"B	-]B	/5B	2GB	3MB	6`B	6zB	7�B	9�B	8�B	9	B	C�B	K�B	N�B	Q�B	R�B	TB	UB	W$B	Y1B	Y1B	Z7B	[=B	\CB	\CB	\CB	]IB	\]B	^�B	d�B	e�B	j�B	r�B	u�B	v�B	w�B	x�B	z�B	{�B	}B	}"B	�B	�B	�3B	�?B	�?B	�1B	�1B	�XB	�JB	�6B	�<B	�VB	�<B	�pB	�VB	�<B	�VB	�VB	�jB	�pB	�\B	�HB	�bB	�}B	�bB	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	�B	�B	�)B	�B	�B	�B	�/B	�B	�!B	�!B	�!B	�B	�5B	�5B	�OB	�OB	�[B	�GB	�TB	�FB	�fB	�zB	�fB	�fB	�lB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	ªB	��B	��B	��B	��B	��B	��B	�9B	��B	��B	��B	� B	� B	��B	��B	�B	��B	�B	�B	�B	�BB	�:B	�$B	�B	�7B	�B	�1B	�+B	�gB	�KB	�qB	�dB	�bB	�bB	�|B	�B	�B	�fB	�B	�fB	�B	�B	�B	�sB	�B	�B	�B	�B	��B	��B	�B	�B	�B	��B	��B	��B	��B	�B	�2B	�B	�B	�B	�"B	�jB	�.B
AB
3B
aB
SB
EB
EB
+B
1B
1B
1B
KB
	7B
	B
	7B
	7B
	7B

XB

XB

XB
6B
�B
VB
vB
bB
}B
vB
vB
}B
\B
}B
vB
vB
bB
hB
hB
�B
�B
�B
oB
TB
oB
�B
oB
�B
uB
�B
�B
�B
�B
�B
�B
�B
B
�B
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
"�B
!�B
 �B
�B
B
#�B
%�B
$B
$B
%�B
'B
(
B
(
B
&B
&B
'B
&2B
+B
,"B
-)B
-)B
-)B
,=B
-CB
.B
./B
-)B
-CB
./B
/B
.IB
/5B
0;B
0;B
1AB
1AB
1AB
2GB
33B
33B
33B
49B
49B
49B
4B
33B
3MB
2-B
2aB
2GB
3MB
5?B
4TB
2aB
/�B
0�B
5ZB
5ZB
5?B
4TB
1�B
.�B
2|B
9rB
8lB
9�B
9�B
;dB
<�B
>]B
=qB
=�B
;B
;�B
;�B
=�B
=�B
=�B
>�B
?�B
?}B
?}B
@�B
?�B
?�B
?�B
>�B
?�B
>�B
?�B
@�B
@�B
A�B
A�B
@�B
@�B
@�B
A�B
A�B
A�B
A�B
@�B
?�B
@�B
A�B
B�B
B�B
B�B
C�B
C�B
C�B
D�B
D�B
D�B
D�B
D�B
E�B
D�B
E�B
E�B
E�B
E�B
E�B
E�B
E�B
E�B
F�B
F�B
E�B
E�B
G�B
G�B
G�B
G�B
G�B
G�B
G�B
G�B
G�B
F�B
G�B
H�B
H�B
I�B
I�B
I�B
J�B
K�B
J�B
K�B
K�B
K�B
L�B
L�B
L�B
L�B
L�B
L�B
L�B
L�B
L�B
K�B
MB
N�B
N�B
N�B
N�B
NB
M�B
OB
Q B
Q B
P�B
PB
QB
RB
SB
SB
SB
SB
SB
S�B
TB
TB
UB
T�B
T�B
UB
T�B
VB
VB
U�B
VB
VB
VB
VB
VB
VB
V9B
VB
W
B
W$B
W$B
W
B
XB
XB
XB
W$B
W$B
VB
S[B
ZB
Y1B
Y1B
Z7B
[#B
\)B
\)B
\B
\)B
\)B
\)B
\)B
\CB
\CB
\CB
\CB
]IB
^5B
^5B
^5B
^5B
]IB
]IB
^5B
^5B
^OB
^5B
_;B
_;B
^5B
^5B
^OB
^OB
^OB
^OB
^jB
_VB
_VB
_;B
`\B
abB
aHB
`\B
_pB
^jB
^�B
^�B
e`B
fLB
gRB
gRB
gmB
gmB
gmB
gmB
f�B
g�B
gmB
g�B
hsB
g�B
g�B
h�B
gmB
g�B
hsB
hsB
h�B
iyB
h�B
hXB
h�B
h�B
f�B
f�B
h�B
g�B
h�B
k�B
k�B
l�B
l�B
l�B
m�B
m�B
l�B
m�B
m�B
m�B
n�B
o�B
p�B
p�B
p�B
q�B
p�B
q�B
q�B
r�B
q�B
q�B
q�B
q�B
r�B
s�B
s�B
s�B
t�B
t�B
s�B
s�B
s�B
t�B
t�B
t�B
t�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=-0.02(dbar)                                                                                                                                                                                                                                                  None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201811020033382018110200333820181102003338201811020200152018110202001520181102020015201811030024102018110300241020181103002410  JA  ARFMdecpA19c                                                                20181029093517  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20181029003520  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20181029003524  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20181029003524  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20181029003525  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20181029003525  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20181029003525  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20181029003525  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20181029003526  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20181029003527                      G�O�G�O�G�O�                JA  ARUP                                                                        20181029005838                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20181029153531  CV  JULD            G�O�G�O�F�c�                JM  ARCAJMQC2.0                                                                 20181101153338  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20181101153338  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20181101170015  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20181102152410  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200115131517                      G�O�G�O�G�O�                