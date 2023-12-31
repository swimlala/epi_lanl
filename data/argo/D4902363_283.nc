CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2018-09-23T00:35:27Z creation;2018-09-23T00:35:32Z conversion to V3.1;2019-12-19T07:32:04Z update;     
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
resolution        =���   axis      Z        �  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  I<   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M$   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  pL   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  t4   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �\   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �D   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �l   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  ݬ   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   �<   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   �<   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   �<   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  �<   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �    HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �0   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �4   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �8   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �<Argo profile    3.1 1.2 19500101000000  20180923003527  20200115131517  4902363                                                                 JAMSTEC                                                         PRES            TEMP            PSAL              A   JA  I2_0576_283                     2C  D   NAVIS_A                         0576                            ARGO 092515                     863 @؃s�� 1   @؃t�[�@9�m��8��dF1&�y1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @333@�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D�fD  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� DfD�fD  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�|�D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�<�D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�C3D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�3D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�f1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@4z�@���@���A Q�A Q�A@Q�A`Q�A�(�A�(�A�(�A�(�A�(�A�(�A�(�A�(�B {B{B{B{B {B({B0{B8{B@{BH{BP{BX{B`{Bh{Bp{Bx{B�
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
CCCCCCCCCCC C"C$C&C(C*C,C.C0C2C4C6C8C:C<C>C@CBCDCFCHCJCLCNCPCRCTCVCXCZC\C^C`CbCdCfChCjClCnCpCrCtCvCxCzC|C~C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�\C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��D HD �HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD��DHD�HD	HD	�HD
HD
�HDHD�HDHD�HDHD�HDHD�HD�D��DHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HD HD �HD!HD!�HD"HD"�HD#HD#�HD$HD$�HD%HD%�HD&HD&�HD'HD'�HD(HD(�HD)HD)�HD*HD*�HD+HD+�HD,HD,�HD-HD-�HD.HD.�HD/HD/�HD0HD0�HD1HD1�HD2HD2�HD3HD3�HD4HD4�HD5HD5�HD6HD6�HD7HD7�HD8HD8�HD9HD9�HD:HD:�HD;HD;�HD<HD<�HD=HD=�HD>HD>�HD?HD?�HD@HD@�HDAHDA�HDBHDB�HDCHDC�HDDHDD�HDEHDE�HDFHDF�HDGHDG�HDHHDH�HDIHDI�HDJHDJ�HDKHDK�HDLHDL�HDMHDM�HDNHDN�HDOHDO�HDPHDP�HDQHDQ�HDRHDR�HDSHDS�HDTHDT�HDUHDU�HDVHDV�HDWHDW�HDXHDX�HDYHDY�HDZHDZ�HD[HD[�HD\HD\�HD]HD]�HD^HD^�HD_HD_�HD`HD`�HDaHDa�HDbHDb�HDcHDc�HDdHDd�HDeHDe�HDfHDf�HDgHDg�HDhHDh�HDiHDi�HDjHDj�HDkHDk�HDlHDl�HDmHDm�HDnHDn�HDoHDo�HDpHDp�HDqHDq�HDrHDr�HDsHDs�HDtHDt�HDuHDu�HDvHDv�HDwHDw�HDxHDx�HDyHDy�HDzHDz�HD{HD{�HD|HD|�HD}HD}�HD~HD~�HDHD�HD� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D�}qD���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�=qD���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D�D���D� �D�@�DÀ�D���D� �D�@�DĀ�D���D� �D�@�Dŀ�D���D� �D�@�Dƀ�D���D� �D�@�Dǀ�D���D� �D�@�DȀ�D���D� �D�@�Dɀ�D���D� �D�@�Dʀ�D���D� �D�@�Dˀ�D���D� �D�C�D̀�D���D� �D�@�D̀�D���D� �D�@�D΀�D���D� �D�@�Dπ�D���D� �D�@�DЀ�D���D� �D�@�Dр�D���D� �D�@�DҀ�D���D� �D�@�DӀ�D���D� �D�@�DԀ�D���D� �D�@�DՀ�D���D� �D�@�Dր�D���D� �D�@�D׀�D���D��D�@�D؀�D���D� �D�@�Dـ�D���D� �D�@�Dڀ�D���D� �D�@�Dۀ�D���D� �D�@�D܀�D���D� �D�@�D݀�D���D� �D�@�Dހ�D���D� �D�@�D߀�D���D� �D�@�D���D���D� �D�@�DဤD���D� �D�@�D․D���D� �D�@�D〤D���D� �D�@�D䀤D���D� �D�@�D値D���D� �D�@�D怤D���D� �D�@�D瀤D���D� �D�@�D耤D���D� �D�@�D逤D���D� �D�@�DꀤD���D� �D�@�D뀤D���D� �D�@�D쀤D���D� �D�@�D퀤D���D� �D�@�DD���D� �D�@�DD���D� �D�@�D���D���D� �D�@�D�D���D� �D�@�D�D���D� �D�@�D�D���D� �D�@�D�D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D�
1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�=qA�=qA�;dA�?}A�?}A�;dA�;dA�=qA�=qA�A�A�A�A�A�A�C�A�C�A�A�A�C�A�E�A�E�A�C�A�E�A�E�A�E�A�E�A�=qA�A���A�C�Aң�A���A���A���A��
A˃A���A�&�AŇ+AĶFA���A��A���A��A���A���A��A���A�x�A��FA�bNA��^A�+A��A��RA��A��A��A��!A���A�t�A���A���A��PA��mA�  A�VA���A��TA�O�A���A���A�1A�M�A�l�A��mA�l�A���A�r�A�+A�?}A�/A���A��+A��A��A��A�hsA���A�\)A���A���A�9XA�`BA���A��A�x�A�`BA��mA�?}A��-A��A���A��^A�VA���A�\)A��A��\A��-A���A33A~Q�A}ƨA|��A{&�Ay�Ax�HAxjAx�Aw�Av�!Av�Au?}At�!At1Ar�Aq/Ao
=An �Al��Ak�Aj�\Ah�yAf��Ae
=Ac�Ac`BAb�`Ab-A_�
A^�+A]�7A\��A[�AZ��AZ5?AY��AX�/AW�-AU��ATĜATr�AS��AR��AQ�AN�HAMl�AM%AL-AKVAJ5?AH�AH�AFȴAFVAE
=AB�AA/AA�A@��A@�A?��A?�A>�jA=�A;�#A9�FA8v�A7��A7dZA6��A6�`A6��A5�PA3&�A2VA1+A.�HA.E�A-�mA,�uA,=qA+��A+hsA+�A*�`A*��A*A�A* �A*JA)��A(�!A'�A&��A%�;A%dZA$�yA$I�A"�jA!G�A ��A =qAAG�A��A�!A�TA�uA�hA�AAdZA�A��A�A^5A-A  A�TAXAVA�AAhsA��AG�Al�A�;A�A
ȴA
n�A
5?A	�FA�/A-A�A�HA~�A�mA��AE�A�7A�A�DAffA��A �`A �DA -@���@��+@� �@�^5@��@���@��@�K�@�S�@�C�@�"�@��@�@���@�@�1@�t�@�ȴ@�{@�o@���@�C�@�$�@��#@��@�Z@�ff@�7@�;d@ܴ9@��m@ۥ�@�C�@��H@ڧ�@�E�@�%@׶F@ָR@�@Ӯ@�M�@�O�@�  @�S�@�~�@��@��@ʧ�@�V@�b@�l�@��y@Ɨ�@š�@�&�@�bN@�@��T@���@���@��H@�@��/@��@��y@�Z@���@�@��@��j@�9X@��y@��^@��@��`@��u@��@���@���@�
=@�E�@��@���@���@�p�@�/@��D@��w@��@��@�9X@�dZ@�@���@��^@�C�@�=q@���@�x�@�hs@�X@�O�@�&�@��@�;d@��y@���@�J@���@�hs@�O�@�&�@���@���@��/@��@��@�|�@�C�@��H@��@���@���@�9X@�1@�dZ@��+@�5?@��@��@�r�@�j@�1'@���@�|�@�\)@���@��\@�=q@��^@�?}@���@�Z@��@��@���@���@�
=@�ȴ@��!@���@�n�@��@�&�@���@� �@��@�t�@�S�@�;d@��R@��^@���@���@��7@�x�@�`B@�&�@��9@�z�@�A�@� �@�b@�  @��@���@�l�@�"�@���@�v�@�M�@�5?@�5?@�-@�$�@��@�@���@��@��/@�Ĝ@��@�j@�1'@�1@|�@�@~�@~V@~@}@}`B@|�@|z�@|(�@{�
@{��@{��@{t�@{�@{ƨ@{ƨ@{�
@|(�@|I�@{��@{33@{C�@{��@z�\@z�\@z=q@yX@y%@xr�@xA�@xA�@x �@w�@v��@v5?@vE�@v5?@u��@u/@t�j@t��@tz�@tj@tI�@t(�@t1@t1@t9X@s�F@s�F@sdZ@r�!@rM�@q�@o\)@nff@n$�@m��@nV@o�P@o��@n$�@l�@lI�@k�
@kdZ@kS�@kC�@j�@j�!@j-@jM�@jJ@i�^@i��@ix�@iX@i%@h��@h�@h1'@g�@g�;@g|�@g;d@g;d@g+@f�@f��@f�R@f{@e�@dI�@cƨ@co@b��@b��@bn�@bM�@b=q@b-@a�@a��@a�7@aX@a7L@`�`@`�@`A�@`b@_�@_�P@_\)@_K�@_�@^�R@^�+@^E�@]�T@]��@]�h@\�/@\�@\�D@\I�@\(�@[��@[ƨ@[��@[S�@["�@Z�H@Z��@Z�!@Z�!@Z�!@Z~�@Z-@Y��@Y��@Y��@Yx�@YG�@Y7L@Y�@X�9@Xr�@Xr�@X1'@W�w@Wl�@Vv�@V5?@U�T@U?}@T��@TI�@T�@T1@S��@S��@S�@SdZ@S"�@So@So@R�H@R��@R�!@R~�@RJ@QG�@P�u@P  @O�P@O|�@OK�@O�@O
=@N�y@N�R@Nȴ@Nv�@M@M�@M�@L�/@L��@L�/@L�/@L�@L��@L9X@K��@Kt�@KS�@KS�@KC�@Ko@J�!@Jn�@JM�@J-@JJ@I��@I�^@I��@JJ@J�@JJ@I��@I7L@IX@H�9@H  @G�;@Hb@G
=@F��@F$�@E�@E�-@Ep�@EO�@E�@E`B@Ep�@Ep�@EO�@D�@D��@C��@CS�@C"�@B~�@A�#@A�^@Ax�@AX@A7L@A7L@@Ĝ@@��@@A�@?��@?�P@?+@>�R@>5?@=p�@<�j@<1@;ƨ@;�F@;��@;��@;�@;t�@;S�@;o@:�@:�!@:^5@9�@9X@9��@9�7@8�u@8��@8Ĝ@7�w@7;d@6�R@6��@6E�@5�@5`B@5p�@5`B@5/@4�/@4�@4Z@49X@4j@4�@4z�@4j@49X@41@3ƨ@3S�@3C�@3o@2�!@2~�@2M�@1��@1��@1hs@1X@1G�@0Ĝ@0  @/�@/�w@.�R@.@.@-@,�/@,Z@,(�@,�@,1@+��@+�
@+��@+�@+S�@*�H@*�@)x�@)�@)%@)�@)�@)�@)G�@)&�@(��@(��@(r�@'\)@'
=@&��@&ff@&$�@%��@%@%�h@%�h@%�h@%`B@$��@$j@#��@#33@#33@#o@"�H@!��@!hs@!7L@ ��@ ��@ Ĝ@ �@ b@   @�@�;@�;@��@��@l�@;d@��@�@�R@�+@V@$�@{@�@@�@�@��@z�@�m@�F@��@33@�!@^5@J@�@�@�#@�#@��@��@x�@7L@�9@r�@1'@1'@b@�;@�@|�@;d@;d@+@��@ȴ@�+@E�@@�@p�@p�@`B@O�@?}@V@V@V@V@��@�j@�@�@��@��@��@��@�@�j@�j@��@�@1@1@��@��@��@��@�m@ƨ@�F@�F@�F@��@dZ@S�@33@o@o@@�@��@��@�\@^5@^5@^5@M�@=q@=q@�@J@��@�@��@��@��@x�@G�@��@�@ �@�@�w@l�@K�@K�@;d@�@�@�R@��@��@v�@ff@$�@��@?}@�@�j@�@��@j@�@��@�F@��@�@S�@
��@
�\@
n�@
^5@
M�@
=q@
�@
J@	��@	��@	��@	�@	�^@	�^@	��@	�7@	�@	%@�`@Ĝ@�9@��@��@bN@1'@�;@|�@K�@+@
=@��@�y@�@ȴ@��@E�@@�T@@p�@`B@O�@?}@/@V1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�=qA�=qA�;dA�?}A�?}A�;dA�;dA�=qA�=qA�A�A�A�A�A�A�C�A�C�A�A�A�C�A�E�A�E�A�C�A�E�A�E�A�E�A�E�A�=qA�A���A�C�Aң�A���A���A���A��
A˃A���A�&�AŇ+AĶFA���A��A���A��A���A���A��A���A�x�A��FA�bNA��^A�+A��A��RA��A��A��A��!A���A�t�A���A���A��PA��mA�  A�VA���A��TA�O�A���A���A�1A�M�A�l�A��mA�l�A���A�r�A�+A�?}A�/A���A��+A��A��A��A�hsA���A�\)A���A���A�9XA�`BA���A��A�x�A�`BA��mA�?}A��-A��A���A��^A�VA���A�\)A��A��\A��-A���A33A~Q�A}ƨA|��A{&�Ay�Ax�HAxjAx�Aw�Av�!Av�Au?}At�!At1Ar�Aq/Ao
=An �Al��Ak�Aj�\Ah�yAf��Ae
=Ac�Ac`BAb�`Ab-A_�
A^�+A]�7A\��A[�AZ��AZ5?AY��AX�/AW�-AU��ATĜATr�AS��AR��AQ�AN�HAMl�AM%AL-AKVAJ5?AH�AH�AFȴAFVAE
=AB�AA/AA�A@��A@�A?��A?�A>�jA=�A;�#A9�FA8v�A7��A7dZA6��A6�`A6��A5�PA3&�A2VA1+A.�HA.E�A-�mA,�uA,=qA+��A+hsA+�A*�`A*��A*A�A* �A*JA)��A(�!A'�A&��A%�;A%dZA$�yA$I�A"�jA!G�A ��A =qAAG�A��A�!A�TA�uA�hA�AAdZA�A��A�A^5A-A  A�TAXAVA�AAhsA��AG�Al�A�;A�A
ȴA
n�A
5?A	�FA�/A-A�A�HA~�A�mA��AE�A�7A�A�DAffA��A �`A �DA -@���@��+@� �@�^5@��@���@��@�K�@�S�@�C�@�"�@��@�@���@�@�1@�t�@�ȴ@�{@�o@���@�C�@�$�@��#@��@�Z@�ff@�7@�;d@ܴ9@��m@ۥ�@�C�@��H@ڧ�@�E�@�%@׶F@ָR@�@Ӯ@�M�@�O�@�  @�S�@�~�@��@��@ʧ�@�V@�b@�l�@��y@Ɨ�@š�@�&�@�bN@�@��T@���@���@��H@�@��/@��@��y@�Z@���@�@��@��j@�9X@��y@��^@��@��`@��u@��@���@���@�
=@�E�@��@���@���@�p�@�/@��D@��w@��@��@�9X@�dZ@�@���@��^@�C�@�=q@���@�x�@�hs@�X@�O�@�&�@��@�;d@��y@���@�J@���@�hs@�O�@�&�@���@���@��/@��@��@�|�@�C�@��H@��@���@���@�9X@�1@�dZ@��+@�5?@��@��@�r�@�j@�1'@���@�|�@�\)@���@��\@�=q@��^@�?}@���@�Z@��@��@���@���@�
=@�ȴ@��!@���@�n�@��@�&�@���@� �@��@�t�@�S�@�;d@��R@��^@���@���@��7@�x�@�`B@�&�@��9@�z�@�A�@� �@�b@�  @��@���@�l�@�"�@���@�v�@�M�@�5?@�5?@�-@�$�@��@�@���@��@��/@�Ĝ@��@�j@�1'@�1@|�@�@~�@~V@~@}@}`B@|�@|z�@|(�@{�
@{��@{��@{t�@{�@{ƨ@{ƨ@{�
@|(�@|I�@{��@{33@{C�@{��@z�\@z�\@z=q@yX@y%@xr�@xA�@xA�@x �@w�@v��@v5?@vE�@v5?@u��@u/@t�j@t��@tz�@tj@tI�@t(�@t1@t1@t9X@s�F@s�F@sdZ@r�!@rM�@q�@o\)@nff@n$�@m��@nV@o�P@o��@n$�@l�@lI�@k�
@kdZ@kS�@kC�@j�@j�!@j-@jM�@jJ@i�^@i��@ix�@iX@i%@h��@h�@h1'@g�@g�;@g|�@g;d@g;d@g+@f�@f��@f�R@f{@e�@dI�@cƨ@co@b��@b��@bn�@bM�@b=q@b-@a�@a��@a�7@aX@a7L@`�`@`�@`A�@`b@_�@_�P@_\)@_K�@_�@^�R@^�+@^E�@]�T@]��@]�h@\�/@\�@\�D@\I�@\(�@[��@[ƨ@[��@[S�@["�@Z�H@Z��@Z�!@Z�!@Z�!@Z~�@Z-@Y��@Y��@Y��@Yx�@YG�@Y7L@Y�@X�9@Xr�@Xr�@X1'@W�w@Wl�@Vv�@V5?@U�T@U?}@T��@TI�@T�@T1@S��@S��@S�@SdZ@S"�@So@So@R�H@R��@R�!@R~�@RJ@QG�@P�u@P  @O�P@O|�@OK�@O�@O
=@N�y@N�R@Nȴ@Nv�@M@M�@M�@L�/@L��@L�/@L�/@L�@L��@L9X@K��@Kt�@KS�@KS�@KC�@Ko@J�!@Jn�@JM�@J-@JJ@I��@I�^@I��@JJ@J�@JJ@I��@I7L@IX@H�9@H  @G�;@Hb@G
=@F��@F$�@E�@E�-@Ep�@EO�@E�@E`B@Ep�@Ep�@EO�@D�@D��@C��@CS�@C"�@B~�@A�#@A�^@Ax�@AX@A7L@A7L@@Ĝ@@��@@A�@?��@?�P@?+@>�R@>5?@=p�@<�j@<1@;ƨ@;�F@;��@;��@;�@;t�@;S�@;o@:�@:�!@:^5@9�@9X@9��@9�7@8�u@8��@8Ĝ@7�w@7;d@6�R@6��@6E�@5�@5`B@5p�@5`B@5/@4�/@4�@4Z@49X@4j@4�@4z�@4j@49X@41@3ƨ@3S�@3C�@3o@2�!@2~�@2M�@1��@1��@1hs@1X@1G�@0Ĝ@0  @/�@/�w@.�R@.@.@-@,�/@,Z@,(�@,�@,1@+��@+�
@+��@+�@+S�@*�H@*�@)x�@)�@)%@)�@)�@)�@)G�@)&�@(��@(��@(r�@'\)@'
=@&��@&ff@&$�@%��@%@%�h@%�h@%�h@%`B@$��@$j@#��@#33@#33@#o@"�H@!��@!hs@!7L@ ��@ ��@ Ĝ@ �@ b@   @�@�;@�;@��@��@l�@;d@��@�@�R@�+@V@$�@{@�@@�@�@��@z�@�m@�F@��@33@�!@^5@J@�@�@�#@�#@��@��@x�@7L@�9@r�@1'@1'@b@�;@�@|�@;d@;d@+@��@ȴ@�+@E�@@�@p�@p�@`B@O�@?}@V@V@V@V@��@�j@�@�@��@��@��@��@�@�j@�j@��@�@1@1@��@��@��@��@�m@ƨ@�F@�F@�F@��@dZ@S�@33@o@o@@�@��@��@�\@^5@^5@^5@M�@=q@=q@�@J@��@�@��@��@��@x�@G�@��@�@ �@�@�w@l�@K�@K�@;d@�@�@�R@��@��@v�@ff@$�@��@?}@�@�j@�@��@j@�@��@�F@��@�@S�@
��@
�\@
n�@
^5@
M�@
=q@
�@
J@	��@	��@	��@	�@	�^@	�^@	��@	�7@	�@	%@�`@Ĝ@�9@��@��@bN@1'@�;@|�@K�@+@
=@��@�y@�@ȴ@��@E�@@�T@@p�@`B@O�@?}@/@V1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B� B� B�uB�\B�1B�VB�^B�BB/BL�B`BBK�B�VBv�BbNB�{B~�B�Bt�BQ�Bq�B�+B��B��B��B�+B�B�B�B|�B[#Bu�Bl�Be`BYB=qB1'B7LB-B-B"�B�B�BPB  B��B�B�B�B�B�BB�/B��B�wB��B�B�oBv�BjBv�BiyB`BBK�B$�B�B%BBJB�B�BhBB
�B
�ZB
��B
��B
�?B
�-B
�XB
�B
��B
��B
�+B
t�B
hsB
p�B
p�B
cTB
T�B
G�B
R�B
R�B
Q�B
I�B
A�B
?}B
9XB
7LB
/B
 �B
PB	��B
%B	��B	�B	�sB	�B	ȴB	��B	ÖB	ƨB	B	�RB	��B	��B	��B	�oB	�JB	�1B	�B	~�B	x�B	k�B	bNB	q�B	v�B	gmB	W
B	I�B	-B	<jB	D�B	:^B	/B	)�B	�B	�B	JB	oB��B�B�`B	B	B��B�B�B�yB�;BƨB�qBÖBȴBɺBȴBǮB�}B�B��B��B��B�\B�B�'B��B�3B�-B�9B�?B�3B�-B�!B�'B�B��B��B�VB��B�VB�hB�VB�+Bt�Br�B�B~�Bz�Bu�BcTBhsBhsB_;B]/B[#B_;BbNB_;BXBR�B\)BcTBbNB`BBZBQ�BYB[#BT�BJ�B;dB/B5?B>wBD�BD�BB�B=qB49B6FB7LB5?B8RB49B-B0!B2-B.B:^B8RB/B+B2-B2-B.B'�B�B�B!�B�B�B,B6FB49B33B1'B.B)�B%�B)�B(�B%�B �BuBoB�B �B'�B%�B�BoB�BVBuB �B'�B'�B&�B&�B"�B�B�B�B�B�B�B�B�B!�B �B�B�B�B�B&�B)�B-B.B)�B-B)�B#�B,B-B)�B+B,B/B1'B'�B!�B-B8RB<jB>wB<jB8RB;dBD�BG�BF�BC�B@�B5?BB�BT�B[#B]/B]/B\)BZBXBW
BS�B]/BgmBgmBl�Bk�BgmBbNBs�B� B�B�%B�%B�B�B{�B�B�JB�VB�VB�uB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B��B��B�-B�-B�LBÖBǮBƨBǮB��B��B��B��B��B��B�B�B�)B�BB�NB�NB�NB�HB�fB�yB�B�yB�sB�yB�B��B��B��B��B��B��B	  B	VB	bB	bB	hB	oB	hB	hB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	#�B	%�B	'�B	)�B	)�B	(�B	'�B	)�B	+B	-B	5?B	:^B	;dB	;dB	=qB	?}B	A�B	F�B	H�B	H�B	J�B	K�B	L�B	M�B	P�B	R�B	VB	YB	]/B	]/B	aHB	cTB	dZB	ffB	jB	k�B	jB	k�B	m�B	q�B	o�B	t�B	u�B	u�B	|�B	}�B	�B	�B	�B	�B	�B	�%B	�PB	�bB	�hB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�!B	�B	��B	��B	�B	�B	�!B	�3B	�?B	�9B	�9B	�?B	�RB	�dB	�jB	�qB	�wB	�wB	�wB	�}B	��B	��B	��B	ÖB	B	ÖB	ƨB	ƨB	ŢB	ŢB	ŢB	ÖB	B	B	ƨB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�
B	�
B	�B	�B	�#B	�B	�/B	�5B	�5B	�;B	�HB	�HB	�HB	�NB	�ZB	�NB	�`B	�fB	�fB	�mB	�mB	�sB	�sB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
%B
%B
B
B
+B
+B
1B

=B

=B

=B

=B
	7B
1B
	7B
JB
JB
PB
JB
JB
DB
JB
PB
JB
JB
PB
VB
hB
hB
hB
hB
bB
bB
hB
bB
\B
hB
uB
bB
bB
oB
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
 �B
�B
�B
�B
�B
�B
!�B
#�B
#�B
#�B
#�B
#�B
#�B
#�B
$�B
$�B
$�B
%�B
$�B
(�B
)�B
$�B
)�B
+B
'�B
)�B
)�B
+B
+B
(�B
,B
-B
-B
-B
-B
-B
.B
/B
2-B
33B
33B
33B
33B
49B
33B
33B
5?B
5?B
6FB
7LB
7LB
7LB
7LB
9XB
9XB
9XB
7LB
6FB
:^B
:^B
7LB
8RB
;dB
:^B
7LB
9XB
;dB
<jB
=qB
=qB
=qB
=qB
=qB
=qB
<jB
;dB
=qB
?}B
B�B
B�B
C�B
C�B
C�B
C�B
C�B
C�B
B�B
?}B
A�B
B�B
C�B
C�B
B�B
D�B
D�B
E�B
E�B
E�B
E�B
E�B
D�B
E�B
G�B
F�B
F�B
D�B
F�B
G�B
G�B
H�B
H�B
G�B
G�B
J�B
J�B
J�B
J�B
J�B
I�B
I�B
J�B
J�B
K�B
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
N�B
O�B
N�B
N�B
O�B
P�B
R�B
R�B
R�B
R�B
R�B
R�B
Q�B
Q�B
Q�B
S�B
S�B
VB
T�B
T�B
T�B
VB
VB
W
B
W
B
VB
VB
VB
VB
VB
XB
YB
ZB
YB
YB
YB
YB
ZB
ZB
ZB
ZB
YB
ZB
[#B
[#B
[#B
[#B
[#B
[#B
[#B
ZB
ZB
YB
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
^5B
_;B
_;B
^5B
_;B
_;B
^5B
_;B
_;B
_;B
^5B
_;B
_;B
_;B
^5B
^5B
^5B
_;B
`BB
aHB
aHB
bNB
cTB
bNB
bNB
bNB
bNB
cTB
cTB
cTB
cTB
bNB
bNB
bNB
cTB
dZB
e`B
e`B
dZB
dZB
e`B
e`B
e`B
ffB
e`B
dZB
ffB
gmB
hsB
hsB
hsB
hsB
iyB
iyB
iyB
iyB
iyB
hsB
iyB
iyB
hsB
hsB
iyB
jB
jB
jB
jB
jB
iyB
iyB
jB
jB
k�B
l�B
l�B
m�B
m�B
l�B
m�B
l�B
k�B
l�B
n�B
m�B
m�B
n�B
o�B
o�B
o�B
o�B
p�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�'B�4B��B�,B�NB��B�oB��B�B?B2GBPbBdBQ�B�B{Bg�B�$B��B��Bz�BXyBvFB��B�LB�B��B��B��B��B�BHB_!Bv�Bn/Bf�BZ�B@iB3�B8�B/B.�B$�B!bBB�BB��B�nB�iB��B�B�B�B��B��B�RB��B�gBzxBmwBw�BkBa|BM�B(sB \B	�B%B�B�B�B�BSB
�AB
��B
�aB
��B
��B
��B
��B
�;B
�B
��B
�RB
wB
kB
q�B
q�B
d�B
V�B
I�B
S�B
S�B
RoB
J�B
B�B
@OB
:^B
8B
0!B
"hB
vB
oB
EB	�}B	�nB	��B	�CB	�)B	ðB	��B	�_B	�aB	��B	�5B	�/B	��B	��B	�jB	�B	�%B	�B	y�B	mCB	d�B	r�B	wLB	h�B	X�B	K�B	0B	=�B	EmB	;�B	0�B	+6B	OB	 �B	"B	&B��B�B�mB	B	[B��B��B�B�eB��B�B� B�B�lB�=B�7B��B�4B��B�7B� B�nB�:B��B��B��B��B��B��B��B��B��B��B�vB�cB��B�	B��B�eB��B� B�(B�fBv�Bt�B��B�B{�Bv�Be�Bi�Bi�B`�B^�B\�B`BBb�B`'BYBTFB\�Bc�Bb�B`�B[	BS@BY�B[�BU�BLB=�B1�B7B?cBEBEBCB>]B5tB7LB88B6+B9	B5?B.}B1'B33B/5B:xB8�B0!B,"B2�B2�B.�B(�B;B�B#BB!HB,�B6FB4nB3hB1vB.}B*�B&�B*B)yB&�B!�BgB�B�B!�B(>B&LB�B�BkB�B�B!HB($B(XB'RB'8B#TB�B�B�B�B�B�B�B�B"hB!�B�B vB �B�B'�B*eB-wB.}B*�B-�B*�B%,B,�B-�B*�B+�B,�B/�B1�B)_B#�B./B8�B<�B>�B=B9XB<6BEBG�BGBDBAUB72BC�BUgB[qB]dB]dB\xBZ�BX�BW�BU2B]�Bg�Bg�Bl�BlBh>Bc�BtTB�B�AB�?B�%B�SB�AB|�B��B��B��B��B��B��B��B��B��B��B��B��B�B��B�B��B�qB�jB�>B�KB�WB��B��B�aB��B��BðB��B��B�B��B�B�<B�HB�@B�[B�SB�eB�xB�vB�B�B�B�B�B�B�B�B��B��B�B�B�B�8B�"B�B�jB	 �B	pB	}B	}B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	B	!B	#�B	%�B	(
B	)�B	*B	)B	(
B	*0B	+B	-wB	5ZB	:xB	;B	;�B	=�B	?�B	A�B	F�B	H�B	H�B	J�B	K�B	MB	NB	QB	SB	VB	YB	]/B	]IB	aHB	c:B	dZB	ffB	jeB	k�B	j�B	k�B	m�B	q�B	pB	t�B	u�B	v+B	}B	~(B	�'B	�-B	�B	�AB	�aB	�YB	�PB	�}B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�HB	�OB	�B	��B	��B	��B	��B	�/B	�sB	�fB	�5B	�5B	�UB	�MB	�?B	�nB	�TB	�tB	�RB	�B	�jB	�qB	��B	��B	��B	��B	��B	��B	��B	ðB	��B	ðB	ƎB	��B	żB	żB	żB	��B	��B	��B	��B	�B	��B	��B	��B	� B	��B	��B	�B	�B	�B	�B	�B	�$B	�?B	�1B	�7B	�#B	�QB	�IB	�OB	�OB	�VB	�bB	�bB	�|B	�NB	�ZB	�B	�zB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	�B	�B	�B	�B	��B	��B	��B	��B	��B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	��B	�B	�B	�B	�B	�0B	�6B	�.B
 4B
B
3B
3B
9B
9B
?B
%B
SB
gB
EB
_B
KB

=B

=B

=B

#B
	RB
�B
	RB
JB
dB
PB
dB
dB
xB
dB
jB
dB
dB
jB
VB
NB
NB
hB
�B
�B
}B
hB
�B
�B
hB
uB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
 �B
�B
�B
B
B
�B
!�B
#�B
#�B
#�B
#�B
#�B
#�B
#�B
$�B
$�B
$�B
&B
%B
(�B
)�B
%,B
)�B
+B
(XB
*0B
*0B
+B
+B
)*B
+�B
-B
-)B
-)B
-)B
-)B
./B
/5B
2B
3B
3MB
3MB
3MB
4TB
33B
3hB
5?B
5ZB
6`B
7fB
7fB
7fB
7fB
9XB
9rB
9rB
7�B
6�B
:xB
:xB
7�B
8lB
;dB
:�B
7�B
9�B
;B
<jB
=qB
=�B
=�B
=�B
=�B
=�B
<�B
;�B
=�B
?�B
B�B
B�B
C�B
C�B
C{B
C�B
C�B
C�B
B�B
?�B
A�B
B�B
C�B
C�B
B�B
D�B
D�B
E�B
E�B
E�B
E�B
E�B
D�B
E�B
G�B
F�B
F�B
D�B
F�B
G�B
G�B
H�B
H�B
G�B
G�B
J�B
J�B
J�B
J�B
J�B
I�B
I�B
J�B
J�B
K�B
K�B
K�B
K�B
L�B
L�B
L�B
L�B
L�B
MB
L�B
L�B
MB
N�B
O�B
OB
OB
O�B
Q B
R�B
R�B
R�B
R�B
SB
SB
RB
RB
R B
TB
TB
VB
UB
UB
UB
VB
VB
V�B
W
B
VB
VB
VB
VB
V9B
X+B
YB
ZB
YB
YB
X�B
Y1B
ZB
ZB
ZB
ZB
Y1B
ZB
[#B
[#B
[	B
[#B
[#B
[#B
[#B
ZB
Z7B
YKB
[#B
\)B
\)B
\)B
\)B
\B
\)B
\CB
\)B
\)B
\)B
\)B
\CB
\)B
\]B
]B
]/B
]/B
]/B
]IB
]IB
]IB
^OB
_!B
_;B
^OB
_;B
_;B
^5B
_!B
_;B
_;B
^5B
_;B
_;B
_VB
^OB
^OB
^jB
_VB
`\B
abB
abB
bNB
cTB
bhB
bhB
bhB
bhB
c:B
cTB
cnB
cnB
bhB
b�B
b�B
cnB
dZB
e`B
ezB
dtB
dtB
ezB
e`B
ezB
ffB
ezB
d�B
ffB
g�B
hsB
hsB
hsB
h�B
i_B
iyB
iyB
iyB
i_B
h�B
iyB
iyB
h�B
h�B
iyB
jB
j�B
jB
jB
jeB
i�B
iyB
j�B
j�B
k�B
l�B
l�B
m�B
m�B
l�B
m�B
l�B
k�B
l�B
n�B
m�B
m�B
n�B
o�B
o�B
o�B
o�B
p�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=-0.02(dbar)                                                                                                                                                                                                                                                  None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201809270038192018092700381920180927003819201809270200152018092702001520180927020015201809280022192018092800221920180928002219  JA  ARFMdecpA19c                                                                20180923093514  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20180923003527  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20180923003531  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20180923003531  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20180923003532  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20180923003532  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20180923003532  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20180923003532  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20180923003532  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20180923003532                      G�O�G�O�G�O�                JA  ARUP                                                                        20180923005622                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20180923153341  CV  JULD            G�O�G�O�F��                JM  ARGQJMQC2.0                                                                 20180923153341  CV  JULD_LOCATION   G�O�G�O�F��                JM  ARGQJMQC2.0                                                                 20180923153341  CV  LONGITUDE       G�O�G�O��"/\                JM  ARCAJMQC2.0                                                                 20180926153819  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20180926153819  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20180926170015  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180927152219  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200115131517                      G�O�G�O�G�O�                