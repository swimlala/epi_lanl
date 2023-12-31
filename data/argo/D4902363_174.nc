CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2017-10-31T00:35:08Z creation;2017-10-31T00:35:11Z conversion to V3.1;2019-12-19T07:57:55Z update;     
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
resolution        =���   axis      Z        X  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  H�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     X  L�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \$   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     X  _�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  oT   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     X  s,   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     X  �\   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     X  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     X  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     X  �   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     X  �l   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   �T   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   �T   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   �T   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  �T   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �$   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �4   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �8   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �H   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �L   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �P   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �TArgo profile    3.1 1.2 19500101000000  20171031003508  20200115121517  4902363                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA  I2_0576_174                     2C  D   NAVIS_A                         0576                            ARGO 092515                     863 @�1����1   @�1��8�@; ѷY�dt��`A�1   GPS     A   B   B   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�33B�ffB���B���B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D��D� D  D� D  D� D  D� D  D� D  D� D  D� D  Dy�D  D� D  D� D  D� D fD � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@�fDA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DPfDP�fDQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�|�D���D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր Dּ�D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D��3D��f1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @���@���A Q�A Q�A@Q�A`Q�A�(�A�(�A�(�A�(�A�(�A�(�A�(�A�(�B {B{B{B{B {B({B0{B8{B@{BH{BP{BX{B`{Bh{Bp{Bx{B�
=B�=qB�p�B��
B��
B��
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
CCCCCCCCCCC C"C$C&C(C*C,C.C0C2C4C6C8C:C<C>C@CBCDCFCHCJCLCNCPCRCTCVCXCZC\C^C`CbCdCfChCjClCnCpCrCtCvCxCzC|C~C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C���C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�\C��C��C��C��D HD �HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HD	HD	�HD
HD
�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HD��D�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHDz�DHD�HDHD�HDHD�HD �D �HD!HD!�HD"HD"�HD#HD#�HD$HD$�HD%HD%�HD&HD&�HD'HD'�HD(HD(�HD)HD)�HD*HD*�HD+HD+�HD,HD,�HD-HD-�HD.HD.�HD/HD/�HD0HD0�HD1HD1�HD2HD2�HD3HD3�HD4HD4�HD5HD5�HD6HD6�HD7HD7�HD8HD8�HD9HD9�HD:HD:�HD;HD;�HD<HD<�HD=HD=�HD>HD>�HD?HD?�HD@HD@��DAHDA�HDBHDB�HDCHDC�HDDHDD�HDEHDE�HDFHDF�HDGHDG�HDHHDH�HDIHDI�HDJHDJ�HDKHDK�HDLHDL�HDMHDM�HDNHDN�HDOHDO�HDP�DP��DQHDQ�HDRHDR�HDSHDS�HDTHDT�HDUHDU�HDVHDV�HDWHDW�HDXHDX�HDYHDY�HDZHDZ�HD[HD[�HD\HD\�HD]HD]�HD^HD^�HD_HD_�HD`HD`�HDaHDa�HDbHDb�HDcHDc�HDdHDd�HDeHDe�HDfHDf�HDgHDg�HDhHDh�HDiHDi�HDjHDj�HDkHDk�HDlHDl�HDmHDm�HDnHDn�HDoHDo�HDpHDp�HDqHDq�HDrHDr�HDsHDs�HDtHDt�HDuHDu�HDvHDv�HDwHDw�HDxHDx�HDyHDy�HDzHDz�HD{HD{�HD|HD|�HD}HD}�HD~HD~�HDHD�HD� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D�}qD��qD� �D�C�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D�D���D� �D�@�DÀ�D���D� �D�@�DĀ�D���D� �D�@�Dŀ�D���D� �D�@�Dƀ�D���D� �D�@�Dǀ�D���D� �D�@�DȀ�D���D� �D�@�Dɀ�D���D� �D�@�Dʀ�D���D� �D�@�Dˀ�D���D� �D�@�D̀�D���D� �D�@�D̀�D���D� �D�@�D΀�D���D� �D�@�Dπ�D���D� �D�@�DЀ�D���D� �D�@�Dр�D���D� �D�@�DҀ�D���D� �D�@�DӀ�D���D� �D�@�DԀ�D���D� �D�@�DՀ�D���D� �D�@�Dր�DֽqD� �D�@�D׀�D���D� �D�@�D؀�D���D� �D�@�Dـ�D���D� �D�@�Dڀ�D���D� �D�@�Dۀ�D���D� �D�@�D܀�D���D� �D�@�D݀�D���D� �D�@�Dހ�D���D� �D�@�D߀�D���D� �D�@�D���D���D� �D�@�DဤD���D� �D�@�D․D���D� �D�@�D〤D���D� �D�@�D䀤D���D� �D�@�D値D���D� �D�@�D怤D���D� �D�@�D瀤D���D� �D�@�D耤D���D� �D�@�D逤D���D� �D�@�DꀤD���D� �D�@�D뀤D���D� �D�@�D쀤D���D� �D�@�D퀤D���D� �D�@�DD���D� �D�@�DD���D� �D�@�D���D���D� �D�@�D�D���D� �D�@�D�D���D� �D�@�D�D���D� �D�@�D�D���D� �D�@�D���D��
1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A��A��A��yA��yA��yA��mA��A��mA��`A��mA��mA��mA��`A��mA��mA��mA��mA��yA��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A�33AƩ�A��
A��A��A��`A�x�A���A�`BA�l�A�E�A�-A���A��-A��TA��^A��RA���A���A�E�A�z�A�`BA�7LA��hA���A�=qA��`A�hsA��uA��A�bNA�  A�=qA���A�1'A�|�A��RA� �A��yA���A��jA��`A�ƨA���A��9A���A��uA��A��;A���A�`BA���A�"�A���A��A�dZA��A�p�A��A���A��hA�p�A�VA��A�ZA��RA�C�A��
A�C�A��-A�=qA��A~��A{x�AzbAyhsAy
=Ax �Aw"�Au�wAt1'Ar��Aq��AqS�AoAn�!Am�AlĜAkp�Ak/Aj�HAi��AghsAfAe�FAe7LAd��AdffAc�^Ac�Ab�AbjAb{Aa�TAaS�A`�A`=qA_�TA_`BA^I�A\�uA[�AZz�AZ�AY%AWl�AV��AVbAU��AU"�ASdZAP�`AN�HAM��AL�yAK�AJ��AJ1'AH-AF�jAE��ADbACK�AB1'AA��A?ƨA>ffA>-A>1A<z�A;�;A:��A:^5A:�A9S�A8�`A8ZA7+A6�DA6I�A5��A4��A4�\A4(�A3�FA3;dA2��A2 �A0ȴA.�/A-�;A-+A,��A,��A,�DA,^5A+�;A*��A*VA*�A)VA(JA'33A&�DA&M�A%�
A$ȴA$5?A#x�A"�HA"�\A!��A �A��A�RA��A��A�
A��A��A+A�!A�A�PA�A �A��A��A�A9XA��AAdZAG�A�Az�Al�A�uAdZA
n�A�A^5A�TA`BA"�AJA"�A
=AA��A�AO�A�TA �@��R@�(�@��@�V@���@��@�Z@�l�@�F@���@�O�@�Q�@��m@�@���@���@���@���@�ff@��@��@���@�@�bN@��m@�\)@���@ᙚ@���@�Ĝ@�I�@ߝ�@�l�@�t�@�C�@�33@�+@�E�@۾w@ّh@�K�@��H@���@ָR@֗�@�-@���@��@�"�@���@�n�@�M�@�E�@��@�@���@љ�@д9@�7L@�\)@�V@�(�@�t�@�v�@�O�@�Z@�E�@�z�@��@�;d@�@�X@�b@�S�@�
=@���@�@�X@��j@�bN@���@��H@��#@�&�@��@��P@�S�@���@�G�@���@�ȴ@���@��@�1'@�dZ@��@��u@�j@�z�@�j@�Z@�Z@��D@��@�Q�@��@��R@��7@�  @�"�@�M�@���@��@���@�@��-@���@��@�b@���@�7L@��@�  @��@�\)@�o@�~�@�v�@�v�@�ff@�M�@�-@��@�@��7@�bN@�S�@�o@�|�@�33@�ȴ@�p�@��9@� �@�ƨ@�1'@�t�@�l�@�\)@�t�@�ƨ@��@��h@��@���@���@�C�@��@�=q@�x�@��F@��@�&�@�(�@��P@�o@��y@���@��!@��\@�^5@�M�@�E�@�=q@�=q@�=q@��@��@��@�J@���@���@��9@�j@�1@��@�+@��@��@���@��\@�V@�=q@��#@�x�@�&�@�j@�1@�  @��@l�@~ȴ@~$�@}�-@}�@{�m@{S�@z�!@z^5@zJ@y��@yx�@y7L@x�@x �@w�@w\)@v��@v��@vv�@v5?@u?}@tI�@s"�@r��@r�\@rn�@q��@qG�@q&�@q%@p�`@pr�@o�@o�@o|�@o+@nȴ@n5?@m��@m�h@m`B@lZ@l(�@kƨ@j�H@j�H@jJ@j�@jM�@j^5@j=q@i�@i��@ix�@ix�@ix�@i��@ix�@ihs@iX@i&�@h��@h�@hb@g�;@g|�@f��@f@e@e�h@eO�@d�@d�@d�@dz�@dj@c��@c��@c�@b��@bJ@ax�@`�@`A�@`Q�@`bN@` �@_��@_�w@_�@_l�@_\)@_|�@_+@^��@^��@^{@]p�@\�j@\j@\9X@[�F@[��@["�@Z�H@Z��@Zn�@Z-@Y�@Y��@Yhs@Y&�@Y%@X�@W|�@WK�@Vȴ@VE�@U��@U�@U�@U�h@U�@T�@T��@Tz�@T�@Sƨ@Sƨ@SC�@So@R�@R��@R~�@Q��@Q��@Qhs@Q%@P��@P�u@PbN@P �@Pb@P  @O�w@O��@O|�@Nȴ@N��@Nv�@Nv�@NE�@NV@N�+@N�@O;d@OK�@O+@O
=@N�@N�R@N��@N@M@M��@M�h@MO�@L�j@K�
@J�\@I�@IG�@Hr�@HQ�@HA�@H  @G;d@G�@G
=@F�y@Fȴ@F{@E�@D�@D1@C��@B�@B�H@B�\@A�@AG�@A%@@�9@?�@>�R@>��@>v�@=�-@=O�@=/@<��@<�/@<�/@<�/@<�j@<��@<�/@=�@=`B@=/@<��@=�@<�@<Z@;��@;t�@;33@:-@9hs@97L@9�@8��@8�u@8Q�@7��@7\)@7;d@7�@7
=@6�y@6ȴ@6��@6�+@6{@5p�@4�/@4�j@4�D@4Z@49X@4(�@41@3�m@3�@333@2�!@2M�@2J@1�@1��@1X@0�`@0Ĝ@0bN@01'@0b@/��@/�@/|�@/\)@/K�@/+@/�@.��@.�y@.ȴ@.��@.�+@.v�@.v�@.v�@.ff@.V@.E�@.$�@.{@-�T@-�T@-�T@-@-�h@-O�@-�@,�/@,��@,��@,z�@,Z@,�@+��@+��@+C�@*�H@*^5@*=q@)��@)��@)hs@)%@(��@(r�@(r�@(Q�@(A�@(A�@(A�@(1'@(1'@(b@'�;@'��@'K�@'�@&��@&ȴ@&��@&�+@&v�@&V@&{@%�T@%��@%��@%@%��@%?}@%V@$Z@#ƨ@#��@#C�@#33@#o@"�!@"M�@"�@!�@!�#@!hs@!X@!hs@!hs@!hs@!hs@!x�@!�7@!�7@!7L@ �9@ �@  �@|�@;d@;d@+@�@
=@
=@�y@�+@E�@��@�@?}@�@V@��@�@�/@�j@�j@��@�D@�D@�D@z�@j@I�@1@��@33@��@n�@=q@J@�@�^@x�@X@G�@&�@Ĝ@�9@�9@��@r�@1'@b@�;@��@�@�@�@�+@V@{@�@�T@��@@@�-@��@�@?}@�@j@I�@(�@1@��@�
@��@dZ@o@�H@~�@-@�@��@�@�@�#@��@X@�@��@��@��@��@��@��@��@�u@r�@bN@Q�@A�@A�@  @�;@��@|�@;d@
=@��@v�@ff@V@E�@5?@$�@@�T@��@`B@?}@V@�@��@�@�D@�@�@�m@ƨ@�F@��@C�@33@C�@C�@33@o@
�H@
��@
��@
^5@	��@	�#@	�#@	��@	�7@	X@	7L@	7L@	&�@	%@��@��@�9@��@�u@r�@bN@�@��@�w@�P@;d@�y@�R@��@v�@ff@V@E�@$�@$�@@O�@V@��@��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A��A��A��yA��yA��yA��mA��A��mA��`A��mA��mA��mA��`A��mA��mA��mA��mA��yA��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A�33AƩ�A��
A��A��A��`A�x�A���A�`BA�l�A�E�A�-A���A��-A��TA��^A��RA���A���A�E�A�z�A�`BA�7LA��hA���A�=qA��`A�hsA��uA��A�bNA�  A�=qA���A�1'A�|�A��RA� �A��yA���A��jA��`A�ƨA���A��9A���A��uA��A��;A���A�`BA���A�"�A���A��A�dZA��A�p�A��A���A��hA�p�A�VA��A�ZA��RA�C�A��
A�C�A��-A�=qA��A~��A{x�AzbAyhsAy
=Ax �Aw"�Au�wAt1'Ar��Aq��AqS�AoAn�!Am�AlĜAkp�Ak/Aj�HAi��AghsAfAe�FAe7LAd��AdffAc�^Ac�Ab�AbjAb{Aa�TAaS�A`�A`=qA_�TA_`BA^I�A\�uA[�AZz�AZ�AY%AWl�AV��AVbAU��AU"�ASdZAP�`AN�HAM��AL�yAK�AJ��AJ1'AH-AF�jAE��ADbACK�AB1'AA��A?ƨA>ffA>-A>1A<z�A;�;A:��A:^5A:�A9S�A8�`A8ZA7+A6�DA6I�A5��A4��A4�\A4(�A3�FA3;dA2��A2 �A0ȴA.�/A-�;A-+A,��A,��A,�DA,^5A+�;A*��A*VA*�A)VA(JA'33A&�DA&M�A%�
A$ȴA$5?A#x�A"�HA"�\A!��A �A��A�RA��A��A�
A��A��A+A�!A�A�PA�A �A��A��A�A9XA��AAdZAG�A�Az�Al�A�uAdZA
n�A�A^5A�TA`BA"�AJA"�A
=AA��A�AO�A�TA �@��R@�(�@��@�V@���@��@�Z@�l�@�F@���@�O�@�Q�@��m@�@���@���@���@���@�ff@��@��@���@�@�bN@��m@�\)@���@ᙚ@���@�Ĝ@�I�@ߝ�@�l�@�t�@�C�@�33@�+@�E�@۾w@ّh@�K�@��H@���@ָR@֗�@�-@���@��@�"�@���@�n�@�M�@�E�@��@�@���@љ�@д9@�7L@�\)@�V@�(�@�t�@�v�@�O�@�Z@�E�@�z�@��@�;d@�@�X@�b@�S�@�
=@���@�@�X@��j@�bN@���@��H@��#@�&�@��@��P@�S�@���@�G�@���@�ȴ@���@��@�1'@�dZ@��@��u@�j@�z�@�j@�Z@�Z@��D@��@�Q�@��@��R@��7@�  @�"�@�M�@���@��@���@�@��-@���@��@�b@���@�7L@��@�  @��@�\)@�o@�~�@�v�@�v�@�ff@�M�@�-@��@�@��7@�bN@�S�@�o@�|�@�33@�ȴ@�p�@��9@� �@�ƨ@�1'@�t�@�l�@�\)@�t�@�ƨ@��@��h@��@���@���@�C�@��@�=q@�x�@��F@��@�&�@�(�@��P@�o@��y@���@��!@��\@�^5@�M�@�E�@�=q@�=q@�=q@��@��@��@�J@���@���@��9@�j@�1@��@�+@��@��@���@��\@�V@�=q@��#@�x�@�&�@�j@�1@�  @��@l�@~ȴ@~$�@}�-@}�@{�m@{S�@z�!@z^5@zJ@y��@yx�@y7L@x�@x �@w�@w\)@v��@v��@vv�@v5?@u?}@tI�@s"�@r��@r�\@rn�@q��@qG�@q&�@q%@p�`@pr�@o�@o�@o|�@o+@nȴ@n5?@m��@m�h@m`B@lZ@l(�@kƨ@j�H@j�H@jJ@j�@jM�@j^5@j=q@i�@i��@ix�@ix�@ix�@i��@ix�@ihs@iX@i&�@h��@h�@hb@g�;@g|�@f��@f@e@e�h@eO�@d�G�O�G�O�@dz�@dj@c��@c��@c�@b��@bJ@ax�@`�@`A�@`Q�@`bN@` �@_��@_�w@_�@_l�@_\)@_|�@_+@^��@^��@^{@]p�@\�j@\j@\9X@[�F@[��@["�@Z�H@Z��@Zn�@Z-@Y�@Y��@Yhs@Y&�@Y%@X�@W|�@WK�@Vȴ@VE�@U��@U�@U�@U�h@U�@T�@T��@Tz�@T�@Sƨ@Sƨ@SC�@So@R�@R��@R~�@Q��@Q��@Qhs@Q%@P��@P�u@PbN@P �@Pb@P  @O�w@O��@O|�@Nȴ@N��@Nv�@Nv�@NE�@NV@N�+@N�@O;d@OK�@O+@O
=@N�@N�R@N��@N@M@M��@M�h@MO�@L�j@K�
@J�\@I�@IG�@Hr�@HQ�@HA�@H  @G;d@G�@G
=@F�y@Fȴ@F{@E�@D�@D1@C��@B�@B�H@B�\@A�@AG�@A%@@�9@?�@>�R@>��@>v�@=�-@=O�@=/@<��@<�/@<�/@<�/@<�j@<��@<�/@=�@=`B@=/@<��@=�@<�@<Z@;��@;t�@;33@:-@9hs@97L@9�@8��@8�u@8Q�@7��@7\)@7;d@7�@7
=@6�y@6ȴ@6��@6�+@6{@5p�@4�/@4�j@4�D@4Z@49X@4(�@41@3�m@3�@333@2�!@2M�@2J@1�@1��@1X@0�`@0Ĝ@0bN@01'@0b@/��@/�@/|�@/\)@/K�@/+@/�@.��@.�y@.ȴ@.��@.�+@.v�@.v�@.v�@.ff@.V@.E�@.$�@.{@-�T@-�T@-�T@-@-�h@-O�@-�@,�/@,��@,��@,z�@,Z@,�@+��@+��@+C�@*�H@*^5@*=q@)��@)��@)hs@)%@(��@(r�@(r�@(Q�@(A�@(A�@(A�@(1'@(1'@(b@'�;@'��@'K�@'�@&��@&ȴ@&��@&�+@&v�@&V@&{@%�T@%��@%��@%@%��@%?}@%V@$Z@#ƨ@#��@#C�@#33@#o@"�!@"M�@"�@!�@!�#@!hs@!X@!hs@!hs@!hs@!hs@!x�@!�7@!�7@!7L@ �9@ �@  �@|�@;d@;d@+@�@
=@
=@�y@�+@E�@��@�@?}@�@V@��@�@�/@�j@�j@��@�D@�D@�D@z�@j@I�@1@��@33@��@n�@=q@J@�@�^@x�@X@G�@&�@Ĝ@�9@�9@��@r�@1'@b@�;@��@�@�@�@�+@V@{@�@�T@��@@@�-@��@�@?}@�@j@I�@(�@1@��@�
@��@dZ@o@�H@~�@-@�@��@�@�@�#@��@X@�@��@��@��@��@��@��@��@�u@r�@bN@Q�@A�@A�@  @�;@��@|�@;d@
=@��@v�@ff@V@E�@5?@$�@@�T@��@`B@?}@V@�@��@�@�D@�@�@�m@ƨ@�F@��@C�@33@C�@C�@33@o@
�H@
��@
��@
^5@	��@	�#@	�#@	��@	�7@	X@	7L@	7L@	&�@	%@��@��@�9@��@�u@r�@bN@�@��@�w@�P@;d@�y@�R@��@v�@ff@V@E�@$�@$�@@O�@V@��@��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111144111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�BƨB�BaHBv�BiyBl�Bs�Bq�BgmB^5BI�BM�B@�B@�B9XB�B�B�B�B�B�B�BJB��B��B��B�B�fB�/B�B��B��B�wB�'B��B��B��B�=Bv�BjBYBL�BA�B49B(�B�BuB�BhB
=BB
�B
�yB
�yB
�mB
�HB
�
B
��B
��B
��B
��B
B
�dB
�RB
�B
��B
��B
��B
��B
�uB
�DB
�B
jB
e`B
ffB
cTB
\)B
S�B
H�B
@�B
5?B
33B
/B
!�B
�B
�B
oB

=B
JB
+B	��B	�B	�B	�B	�B	�B	�B	�fB	�ZB	�ZB	�HB	�BB	�5B	�B	��B	��B	��B	ȴB	�wB	�3B	�B	�B	�B	��B	��B	��B	��B	�uB	�VB	�B	n�B	cTB	bNB	[#B	T�B	P�B	N�B	<jB	:^B	5?B	,B	)�B	$�B	!�B	�B	PB	�B	{B	+B	+B	B	B	B��B��B��B�B�B�B�B�B�B�mB�fB�HB�/B�B��B��BƨBȴB��B��B��BǮB��B�^B�jB�jB�-B�B�B�B�B�B��B��B��B��B��B�hB�JB�B~�B{�B� By�B{�Bs�BiyBp�Bl�Bm�BjBdZBcTB_;BVBO�BM�BT�BYBZBVBN�BG�BG�BA�BB�B;dBC�BE�BB�BC�B<jB<jBE�BD�BA�B<jB:^B1'B/B,B+B33B5?B6FB49B/B+B�B�B"�B'�B)�B(�B"�B$�B&�B%�B(�B+B(�B-B,B-B.B/B.B5?B7LB:^B9XB:^B=qB>wB>wB=qB;dB49B+B(�B&�B2-B5?B5?B49B1'B.B1'B7LB:^B=qB?}B@�B?}B>wB=qB:^B7LB+B5?B7LB?}B@�B>wB<jB;dB49B33B7LB49B0!B1'B/B49B9XB9XB9XB9XB;dB<jB;dB=qB?}BE�BD�BL�BK�BI�BF�BD�BI�BI�BH�BN�BM�BL�BT�B]/BgmBm�Bm�Bo�Bq�Br�Bp�BgmBdZBr�Bt�Bs�Bu�Bw�Bx�Bx�Bx�Bx�Bv�Bs�Bq�Bq�Bw�B�B�%B�7B�DB�PB�PB�uB�uB�oB�oB�uB�uB�oB�bB�bB��B��B��B�B�B�B�!B�?B�FB��B�}BŢBǮB��B��B�
B�BB�B�B�B�B��B��B�B�B�B�B�B�B��B��B��B��B��B��B	B	B	B	B	B	B	B	B	B	B	B	
=B	
=B	
=B	PB	uB	�B	�B	�B	�B	�B	 �B	 �B	%�B	(�B	+B	0!B	6FB	9XB	;dB	;dB	;dB	=qB	=qB	=qB	A�B	C�B	G�B	H�B	I�B	K�B	L�B	K�B	O�B	P�B	R�B	S�B	T�B	W
B	W
B	VB	[#B	^5B	dZB	ffB	ffB	ffB	hsB	k�B	l�B	o�B	p�B	q�B	q�B	q�B	q�B	q�B	r�B	t�B	v�B	y�B	x�B	{�B	~�B	�B	�B	�B	�=B	�JB	�PB	�\B	�bB	�oB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�3B	�-B	�-B	�-B	�9B	�9B	�?B	�?B	�?B	�RB	�dB	�dB	�dB	�jB	�qB	�wB	�}B	��B	ÖB	ÖB	ŢB	ŢB	ƨB	ǮB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�
B	�
B	�B	�B	�B	�B	�#B	�/B	�;B	�BB	�HB	�HB	�TB	�ZB	�`B	�fB	�mB	�fB	�mB	�mB	�mB	�mB	�mB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
%B
%B
+B
1B
1B
+B
%B
B
B
%B
%B
B
1B
	7B
	7B
1B
	7B
	7B
1B
1B
%B
B
B
B
%B
B
+B
1B
1B
	7B
DB
DB

=B
	7B
DB
DB

=B
DB
DB
JB
JB
JB
JB
JB
PB
\B
\B
bB
bB
hB
hB
hB
bB
bB
\B
\B
VB
\B
oB
uB
uB
uB
uB
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
#�B
#�B
$�B
%�B
%�B
%�B
%�B
&�B
%�B
&�B
&�B
&�B
&�B
'�B
'�B
'�B
'�B
(�B
(�B
(�B
(�B
(�B
)�B
)�B
(�B
(�B
(�B
)�B
)�B
+B
+B
+B
+B
+B
,B
+B
+B
,B
,B
.B
.B
/B
/B
/B
0!B
1'B
2-B
2-B
2-B
33B
33B
33B
33B
2-B
2-B
33B
33B
49B
49B
49B
5?B
5?B
6FB
5?B
6FB
6FB
7LB
7LB
7LB
7LB
6FB
7LB
6FB
8RB
:^B
:^B
;dB
;dB
;dB
;dB
<jB
=qB
=qB
=qB
?}B
?}B
?}B
?}B
?}B
?}B
?}B
?}B
>wB
>wB
@�B
?}B
?}B
B�B
D�B
E�B
E�B
E�B
E�B
E�B
D�B
D�B
D�B
E�B
F�B
F�B
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
J�B
K�B
L�B
M�B
N�B
O�B
O�B
P�B
P�B
P�B
P�B
Q�B
R�B
Q�B
Q�B
Q�B
R�B
R�B
R�B
R�B
Q�B
S�B
S�B
T�B
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
VB
VB
VB
VB
XB
YB
YB
YB
YB
YB
YB
YB
ZB
ZB
[#B
\)B
\)B
\)B
\)B
\)B
\)B
\)B
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
^5B
_;B
_;B
_;B
_;B
_;B
`BB
aHB
aHB
aHB
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
e`B
ffB
ffB
ffB
ffB
ffB
ffB
ffB
ffB
ffB
ffB
gmB
ffB
ffB
ffB
gmB
gmB
gmB
gmB
gmB
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
gmB
gmB
hsB
hsB
iyB
iyB
iyB
iyB
iyB
iyB
hsB
gmB
jB
jB
k�B
l�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B��B��B�3B�B�BյB/ Bf�By�Bn}BoOBu%Br�Bi�BabBNVBP�BB�BB�B<�B�BxB"�B�B$B�BYBVB�DB�B��B��B�>B��B�1B�2BΥB�iB�B��B��B�_B��By>BmCB[WBOvBC�B6�B+BjB�B
B B^BB
��B
�B
��B
��B
�4B
ؓB
�B
�vB
�B
�DB
�{B
�jB
�>B
��B
�B
�B
�-B
��B
��B
��B
��B
m�B
f�B
g8B
dB
]dB
UgB
J�B
B[B
6�B
49B
0B
#�B
�B
�B
�B
�B
�B
�B	��B	� B	�B	�B	�5B	�)B	�B	�8B	�B	�B	��B	�B	ޞB	��B	��B	�[B	�pB	ɠB	�B	�ZB	��B	��B	��B	�ZB	�xB	��B	�sB	�FB	�vB	�gB	q�B	e�B	c�B	\�B	V�B	Q�B	O�B	>�B	<6B	6�B	-�B	+6B	&LB	"�B	�B	�B	�B	B		B	B	MB	�B	�B��B��B��B�B�B�TB�B�WB�B�
B�B�B�B�$B��B��B��BɆB�)B�B��B�B�uB��B�B��B��B�iB�;B��B��B��B�B��B��B�xB�7B��B�jB��B��B}VB�B{0B|�BuBkQBq[Bm�BnIBk�BezBd@B`vBXBQ�BO�BVBY�BZkBV�BO�BI7BIBC-BC�B=VBDgBFYBCaBD3B=�B=qBE�BD�BBAB=�B;�B33B0�B-�B,�B3�B5�B6�B4�B0!B,=B \B=B#�B(�B*eB)yB$B%zB'�B&�B)yB+kB)yB-CB,WB-]B.�B/�B/ B5tB7�B:�B9�B:�B=�B>�B>�B=�B;�B5%B,�B*B(XB2|B5tB5tB4�B1�B/ B1�B7�B:�B=�B?�B@�B?�B>�B=�B:�B8RB-]B6�B8�B@BA B?HB=VB<PB5�B4TB7�B4�B1B1�B0B4�B9�B9�B9�B9�B;�B<�B<B>B@4BF?BE�BL�BL0BJrBG�BE�BJXBJrBI�BO\BN�BNBU�B]dBgmBm�Bm�Bo�Bq�Br�Bq[BiDBffBs�Bu�BtnBvFBxBx�Bx�Bx�Bx�BwBtTBraBr�Bx�B��B��B��B�xB��B��B��B��B��B��B��B��B��B��B�NB�?B��B��B�WB��B��B��B��B��B�iB� BŢB��BʦB�jBևB��B�B�IB�kB�B�B�ZB�B��B�B�;B�[B�3B�B�B��B�B�B�(B	B	B	'B	-B	-B	-B	9B	9B	MB	{B	�B	
rB	
�B	
�B	�B	�B	�B	�B	�B	�B	�B	 �B	!-B	&B	)DB	+�B	0oB	6`B	9�B	;�B	;�B	;�B	=�B	=�B	=�B	A�B	C�B	G�B	H�B	I�B	K�B	MB	LB	PB	QB	S&B	T,B	U2B	W$B	W?B	VmB	[�B	^�B	dtB	f�B	f�B	f�B	h�B	k�B	l�B	o�B	p�B	q�B	q�B	q�B	q�B	q�B	r�B	t�B	v�B	y�B	y>B	{�B	.B	�;B	�3B	�gB	�#B	�JB	�PB	�vB	��B	��B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	��B	�B	�B	��B	�
G�O�G�O�B	�GB	�aB	�aB	�9B	��B	�tB	�tB	��B	�lB	�dB	�JB	�B	�jB	�qB	�wB	��B	��B	ÖB	ðB	żB	��B	��B	��B	��B	��B	��B	�B	� B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�$B	�?B	�mB	�B	�1B	�QB	�WB	�IB	�;B	�'B	�bB	�|B	�nB	�tB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
-B
3B
B
?B
?B
EB
KB
KB
_B
tB
�B
�B
tB
?B
mB
KB
	RB
	RB
�B
	7B
	RB
KB
1B
tB
mB
mB
SB
YB
mB
EB
fB
�B
	lB
^B
xB

�B
	�B
^B
^B

�B
^B
^B
dB
JB
JB
JB
dB
PB
BB
\B
bB
}B
�B
hB
�B
}B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
#�B
#�B
$�B
%�B
%�B
%�B
%�B
&�B
%�B
&�B
'B
'B
'B
'�B
'�B
'�B
(
B
(�B
(�B
)B
)B
(�B
)�B
)�B
)B
)B
)B
*B
*B
+B
+B
+B
+B
+B
,"B
+6B
+6B
,=B
,=B
./B
.IB
/5B
/5B
/5B
0!B
1AB
2-B
2GB
2-B
33B
33B
33B
33B
2GB
2GB
3MB
33B
4TB
4TB
4TB
5ZB
5ZB
6FB
5?B
6FB
6`B
7LB
7LB
7fB
7LB
6zB
7fB
6�B
8�B
:xB
:^B
;B
;B
;�B
;�B
<�B
=�B
=�B
=�B
?}B
?}B
?}B
?}B
?}B
?cB
?cB
?}B
>�B
>�B
@�B
?�B
?�B
B�B
D�B
E�B
E�B
E�B
E�B
E�B
D�B
D�B
D�B
E�B
F�B
F�B
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
J�B
K�B
L�B
M�B
N�B
O�B
O�B
P�B
Q B
Q B
P�B
Q�B
R�B
RB
RB
RB
SB
SB
SB
SB
R B
TB
TB
UB
VB
VB
VB
W
B
V�B
W
B
W
B
V�B
VB
VB
VB
V9B
X+B
Y1B
Y1B
YB
Y1B
Y1B
Y1B
Y1B
Z7B
Z7B
[=B
\)B
\CB
\)B
\)B
\CB
\CB
\CB
]/B
]IB
^5B
^5B
^5B
^5B
^OB
^OB
^5B
^OB
_;B
_;B
_;B
_!B
^5B
_VB
_VB
_VB
_VB
_VB
`\B
aHB
aHB
a-B
aHB
aHB
aHB
abB
abB
aHB
abB
bhB
bhB
bhB
b�B
bhB
bhB
bhB
cTB
cnB
cnB
cnB
cnB
cnB
e`B
ffB
ffB
ffB
ffB
f�B
f�B
f�B
ffB
f�B
f�B
gRB
f�B
f�B
f�B
gmB
gmB
gmB
g�B
gmB
g�B
g�B
gmB
gmB
g�B
g�B
f�B
gmB
g�B
gmB
gmB
g�B
h�B
h�B
i_B
iyB
i�B
iyB
i�B
iyB
h�B
g�B
j�B
jB
k�B
l�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111144111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�G�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<p�E<���<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=-0.02(dbar)                                                                                                                                                                                                                                                  None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201711040036522017110400365220171104003652201806221232502018062212325020180622123250201804050428362018040504283620180405042836  JA  ARFMdecpA19c                                                                20171031093506  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20171031003508  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20171031003510  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20171031003510  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20171031003511  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20171031003511  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20171031003511  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20171031003511  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20171031003511  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20171031003511                      G�O�G�O�G�O�                JA  ARUP                                                                        20171031005559                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20171101154329  CV  JULD            G�O�G�O�F���                JM  ARSQJMQC2.0                                                                 20171102000000  CF  PSAL_ADJUSTED_QCD�  D�@ G�O�                JM  ARSQJMQC2.0                                                                 20171102000000  CF  TEMP_ADJUSTED_QCD�  D�@ G�O�                JM  ARCAJMQC2.0                                                                 20171103153652  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20171103153652  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180404192836  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20180622033250  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200115121517                      G�O�G�O�G�O�                