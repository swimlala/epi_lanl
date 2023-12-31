CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       ~2018-01-28T18:35:47Z creation;2018-01-28T18:35:50Z conversion to V3.1;2019-12-18T07:25:17Z update;2022-11-21T05:31:19Z update;     
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile      comment_dmqc_operator         BPRIMARY|https://orcid.org/0000-0001-9150-6442|Kanako Sato, JAMSTEC        @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    7H   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    7X   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    7\   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    7`   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    7p   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    7�   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  7�   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7�   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  8   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        8H   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    8L   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    8P   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     8T   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    8t   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    8x   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     8|   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     8�   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8�   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8�   JULD               standard_name         time   	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~       axis      T           8�   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8�   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~            8�   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8�   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8�   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    9   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    9   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    9   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    9   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    9   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    9   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        :   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        P  :    PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  Ip   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     P  MD   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     P  `h   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  o�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     P  s�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     P  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �    PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     P  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �$   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     P  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     P  �H   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     P  ˘   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   �x   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   �x   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   �x   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  �x   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �    HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �H   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �X   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �\   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �l   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �p   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �t   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �xArgo profile    3.1 1.2 19500101000000  20180128183547  20221123111507  4902148 J-ARGO                                                          JAMSTEC                                                         PRES            TEMP            PSAL               ~A   JA  I1_0397_126                     2C  Dd.�NAVIS_A                         0397                            ARGO 011514                     863 @�H"��1   @�H#�W @;�64��d.�W���1   GPS     A   A   B   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @���@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D8��D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DFfDF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ DԼ�D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�<�D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�3D�6f11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@�=q@���A Q�A Q�A@Q�A`Q�A�(�A�(�A�(�A�(�A�(�A�(�A�(�A�(�B {B{B{B{B {B({B0{B8{B@{BH{BP{BX{B`{Bh{Bp{Bx{B�
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
=B�
=B�
=B�
=B�
=B�
=B�
=B�
=C CCCCC
CCCCCCCCCCC C"C$C&C(C*C,C.C0C2C4C6C8C:C<C>C@CBCDCFCHCJCLCNCPCRCTCVCXCZC\C^C`CbCdCfChCjClCnCpCrCtCvCxCzC|C~C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C���C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��D HD �HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HD	HD	�HD
HD
�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HD HD �HD!HD!�HD"HD"�HD#HD#�HD$HD$�HD%HD%�HD&HD&�HD'HD'�HD(HD(�HD)HD)�HD*HD*�HD+HD+�HD,HD,�HD-HD-�HD.HD.�HD/HD/�HD0HD0�HD1HD1�HD2HD2�HD3HD3�HD4HD4�HD5HD5�HD6HD6�HD7HD7�HD8HD8�HD8��D9�HD:HD:�HD;HD;�HD<HD<�HD=HD=�HD>HD>�HD?HD?�HD@HD@�HDAHDA�HDBHDB�HDCHDC�HDDHDD�HDEHDE�HDF�DF�HDGHDG�HDHHDH�HDIHDI�HDJHDJ�HDKHDK�HDLHDL�HDMHDM�HDNHDN�HDOHDO�HDPHDP�HDQHDQ�HDRHDR�HDSHDS�HDTHDT�HDUHDU�HDVHDV�HDWHDW�HDXHDX�HDYHDY�HDZHDZ�HD[HD[�HD\HD\�HD]HD]�HD^HD^�HD_HD_�HD`HD`�HDaHDa�HDbHDb�HDcHDc�HDdHDd�HDeHDe�HDfHDf�HDgHDg�HDhHDh�HDiHDi�HDjHDj�HDkHDk�HDlHDl�HDmHDm�HDnHDn�HDoHDo�HDpHDp�HDqHDq�HDrHDr�HDsHDs�HDtHDt�HDuHDu�HDvHDv�HDwHDw�HDxHDx�HDyHDy�HDzHDz�HD{HD{�HD|HD|�HD}HD}�HD~HD~�HDHD�HD� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D�D���D� �D�@�DÀ�D���D� �D�@�DĀ�D���D� �D�@�Dŀ�D���D� �D�@�Dƀ�D���D� �D�@�Dǀ�D���D� �D�@�DȀ�D���D� �D�@�Dɀ�D���D� �D�@�Dʀ�D���D� �D�@�Dˀ�D���D� �D�@�D̀�D���D� �D�@�D̀�D���D� �D�@�D΀�D���D� �D�@�Dπ�D���D� �D�@�DЀ�D���D� �D�@�Dр�D���D� �D�@�DҀ�D���D� �D�@�DӀ�D���D� �D�@�DԀ�DԽqD� �D�@�DՀ�D���D� �D�@�Dր�D���D� �D�@�D׀�D���D� �D�@�D؀�D���D� �D�@�Dـ�D���D� �D�@�Dڀ�D���D� �D�@�Dۀ�D���D� �D�@�D܀�D���D� �D�@�D݀�D���D� �D�@�Dހ�D���D� �D�@�D߀�D���D� �D�@�D���D���D� �D�@�DဤD���D� �D�@�D․D���D� �D�@�D〤D���D� �D�@�D䀤D���D� �D�@�D値D���D� �D�=qD怤D���D� �D�@�D瀤D���D� �D�@�D耤D���D� �D�@�D逤D���D� �D�@�DꀤD���D� �D�@�D뀤D���D� �D�@�D쀤D���D� �D�@�D퀤D���D� �D�@�DD���D� �D�@�DD���D� �D�@�D���D���D� �D�@�D�D���D� �D�@�D�D���D� �D�@�D�D���D� �D�@�D�D���D��D�7
11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�VA�{A�oA�{A�{A�{A�oA�oA�oA�JA�1A�oA��A���A�I�A�{A�1A���A��;A���A��^A��hA��A�t�A�l�A�\)A�E�A�7LA�+A�$�A�$�A�"�A�"�A� �A��A�VA�A���A���A��A��mA��;A���A�-A���A�E�A��!A�hsA��A�~�A��
A��A�oA�A���A��A��A���A�K�A��HA�$�A��TA�ZA��A�n�A��HA���A�9XA�bNA��TA���A��uA� �A���A�n�A��`A�M�A��9A���A�n�A&�A}��A|�yA|~�A|A{A{�7A{�Az��Ay|�AwhsAu��Ar�Ap��Ap~�Ao|�Am�;Al�uAi�hAhz�Ag�
AeAdA�AbA�Aa?}A_`BA^(�A\�/A\1'A[��A[�FA[�7AZ�`AX��AX�uAW�hAW%AV�`AV�AVr�AU��AUAT~�AR=qAQp�AP�/APM�AP  AO��AOXAO&�ANz�AN  AM��AM�AMS�AL9XAKl�AJ��AJI�AI`BAH�AGx�AG%AF�AE�ADv�AC��ABbA@  A>�A=&�A<5?A;��A;hsA:VA9C�A8ĜA8~�A8$�A7��A7%A6��A6~�A5ƨA5p�A5hsA4�/A4M�A3��A3t�A2�A2v�A1ƨA0�\A/?}A.�jA,jA)+A($�A'XA&��A&��A&�RA& �A%�A%G�A$VA#VA"��A"ffA"E�A"1A!�A!O�A!/A!�A ��A 1A+A��AȴAJA�A?}A��A��A��A��AE�A�TA��A��A�+A9XA  A�wAdZA��AZA�wA{A��A  A�At�A��A1A{A
��A
{A	��A�AhsAffAE�A��A�`A��A��@�@�ƨ@�l�@�+@�@��@��#@��;@�C�@��@�1@��@�C�@��-@��@�F@땁@�|�@�C�@�o@���@�ff@��`@�7L@�@�9@�1@߾w@ߝ�@�|�@�\)@���@�I�@�+@ى7@�r�@�t�@�5?@���@ԓu@�t�@�ff@��@�r�@�A�@�9X@��
@ΰ!@Ͳ-@̼j@��H@��@��@�ff@���@š�@�O�@Ĭ@þw@�l�@�;d@�33@��@\@�O�@�9X@���@�5?@�V@��/@�r�@���@�V@��@��#@���@�p�@�?}@��@��@��;@��7@�I�@��P@�^5@��@���@��@�`B@���@�I�@���@��@��@�\)@�\)@�K�@���@��R@�M�@���@��h@�Z@�@�j@���@���@��@��T@���@�X@�7L@��@�Ĝ@���@�  @�t�@�C�@�5?@�/@�j@��P@�@���@�5?@�hs@�Q�@��@��P@�o@�E�@�?}@��/@�Q�@�dZ@���@��+@�$�@��T@��h@�G�@��@���@�9X@���@��@��P@�t�@�33@��@���@���@���@�n�@���@��7@�hs@�hs@�x�@�x�@�`B@�?}@���@��`@�Ĝ@��D@��m@���@��R@�^5@�$�@��7@�X@���@�Q�@�9X@��@�  @���@��m@���@���@�S�@�
=@��H@��!@�v�@�E�@�$�@���@��`@�bN@�A�@�1@���@�t�@��@��y@�ȴ@���@�-@��-@�p�@���@��u@�j@�bN@�I�@�9X@�(�@�w@
=@~ff@~{@}O�@|�@|1@{dZ@z��@z��@z^5@y�@y�^@y��@y��@y��@yx�@yhs@x��@xbN@v��@v�@v�@v�@u�@u�h@u`B@u?}@u�@t�/@t��@t9X@s�F@st�@sS�@r�@r^5@q�^@qx�@q7L@p��@p�u@o�;@o
=@n�+@m�T@m�@l�@l�@k�
@kƨ@k�F@k��@k��@k�@k��@k�@k��@k��@k��@k�@kt�@kC�@j�H@j^5@i��@iG�@hb@g�P@g\)@g+@f��@f�+@fV@fV@fV@fV@f{@e�T@e@e�-@e`B@e�@d��@d�/@d�j@dz�@dZ@d�@c��@c�m@cƨ@cS�@b��@b=q@a��@a��@a��@a��@a��@aX@a�@`��@`r�@` �@_�@_�w@_��@_|�@_+@^��@^5?@^@]�-@]�h@]p�@]O�@]�@]�@]�@]�@]�@\�/@\��@\�j@\z�@\I�@[��@[��@[t�@[S�@[S�@[S�@[S�@[C�@[o@Z��@Z�\@Z=q@ZJ@Y�@Yhs@Y7L@XĜ@X�u@X�@XbN@X  @W�;@W��@W��@W��@W�w@W�w@W�P@WK�@W+@V�y@V�+@V@U�T@U�h@U�@T�/@S�F@R��@R�@Q�#@Q��@Q�7@Qx�@QG�@Q&�@Q�@P��@P�@PbN@P1'@Pb@O�@O�@O�P@OK�@N��@N$�@N$�@N@M�@M�T@M�h@M/@L�/@L�j@L�@Lz�@Lj@L(�@Kƨ@K"�@Jn�@Ix�@I7L@I%@H1'@Fv�@E@E�-@E�-@E�-@E��@E`B@Dz�@D�@C�@Co@B�H@B�H@B�!@Bn�@BM�@A��@A�@A��@A�7@A&�@@r�@@  @?�@?;d@>�y@>ff@=@=�@=O�@=/@<�/@<�D@<(�@;t�@;@:�H@:��@9�@9��@9x�@9X@9X@9&�@8Ĝ@8�@8bN@8b@7�@7\)@7+@7
=@6�@6��@6��@6��@6��@6��@6v�@6v�@6v�@6ff@6@5�-@4�/@4�D@4j@4I�@41@3�F@3t�@3S�@3"�@2�H@2��@2n�@2^5@2�@1�^@0r�@/�@/�w@/|�@/K�@-�@-�h@-�h@-��@-��@-��@-��@-��@-�@-?}@,��@,�@,�@,�D@,Z@,�@+C�@+33@+"�@*��@*=q@*J@)��@)hs@(��@(Ĝ@(��@(�@(Q�@(A�@( �@(  @'�@'|�@'\)@'
=@&ȴ@&�R@&��@&�+@&ff@&ff@&V@&@%?}@$��@$��@$�D@$9X@#�m@#�m@#�
@#��@#"�@#@"�H@"��@"��@"=q@"-@"�@"J@"�@!��@!�#@!�@��@\)@K�@�@��@�@ȴ@�R@��@v�@ff@E�@E�@5?@$�@�T@@�h@�h@p�@`B@�j@j@��@dZ@o@�H@��@J@��@&�@�9@�u@�u@r�@A�@��@��@��@v�@V@E�@5?@{@{@@��@�h@�@O�@?}@?}@/@/@V@V@��@��@(�@��@@��@��@�!@J@�#@�^@��@X@&�@&�@&�@�@%@Ĝ@bN@�@l�@�@�+@ff@$�@�T@�-@�@O�@��@�D@Z@9X@(�@�@�@1@��@ƨ@C�@o@
�H@
��@
�!@
�\@
n�@
=q@	��@	G�@�9@�@bN@bN@bN@bN@bN@bN@A�@Q�@Q�@Q�@ �@�@��@�P@\)@+@��@ȴ@��@��@v�@5?@@�-@��@��@`B@V@��@�D@�D@z�@j@j@Z@I�@�@��@��@��@�m@��@��@��@�m@�F@�F@��@��@dZ@S�@S�@S�@33@o@�H@�H@��@��@��@��@~�@n�@n�@=q@�#@��@��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�VA�{A�oA�{A�{A�{A�oA�oA�oA�JA�1A�oA��A���A�I�A�{A�1A���A��;A���A��^A��hA��A�t�A�l�A�\)A�E�A�7LA�+A�$�A�$�A�"�A�"�A� �A��A�VA�A���A���A��A��mA��;A���A�-A���A�E�A��!A�hsA��A�~�A��
A��A�oA�A���A��A��A���A�K�A��HA�$�A��TA�ZA��A�n�A��HA���A�9XA�bNA��TA���A��uA� �A���A�n�A��`A�M�A��9A���A�n�A&�A}��A|�yA|~�A|A{A{�7A{�Az��Ay|�AwhsAu��Ar�Ap��Ap~�Ao|�Am�;Al�uAi�hAhz�Ag�
AeAdA�AbA�Aa?}A_`BA^(�A\�/A\1'A[��A[�FA[�7AZ�`AX��AX�uAW�hAW%AV�`AV�AVr�AU��AUAT~�AR=qAQp�AP�/APM�AP  AO��AOXAO&�ANz�AN  AM��AM�AMS�AL9XAKl�AJ��AJI�AI`BAH�AGx�AG%AF�AE�ADv�AC��ABbA@  A>�A=&�A<5?A;��A;hsA:VA9C�A8ĜA8~�A8$�A7��A7%A6��A6~�A5ƨA5p�A5hsA4�/A4M�A3��A3t�A2�A2v�A1ƨA0�\A/?}A.�jA,jA)+A($�A'XA&��A&��A&�RA& �A%�A%G�A$VA#VA"��A"ffA"E�A"1A!�A!O�A!/A!�A ��A 1A+A��AȴAJA�A?}A��A��A��A��AE�A�TA��A��A�+A9XA  A�wAdZA��AZA�wA{A��A  A�At�A��A1A{A
��A
{A	��A�AhsAffAE�A��A�`A��A��@�@�ƨ@�l�@�+@�@��@��#@��;@�C�@��@�1@��@�C�@��-@��@�F@땁@�|�@�C�@�o@���@�ff@��`@�7L@�@�9@�1@߾w@ߝ�@�|�@�\)@���@�I�@�+@ى7@�r�@�t�@�5?@���@ԓu@�t�@�ff@��@�r�@�A�@�9X@��
@ΰ!@Ͳ-@̼j@��H@��@��@�ff@���@š�@�O�@Ĭ@þw@�l�@�;d@�33@��@\@�O�@�9X@���@�5?@�V@��/@�r�@���@�V@��@��#@���@�p�@�?}@��@��@��;@��7@�I�@��P@�^5@��@���@��@�`B@���@�I�@���@��@��@�\)@�\)@�K�@���@��R@�M�@���@��h@�Z@�@�j@���@���@��@��T@���@�X@�7L@��@�Ĝ@���@�  @�t�@�C�@�5?@�/@�j@��P@�@���@�5?@�hs@�Q�@��@��P@�o@�E�@�?}@��/@�Q�@�dZ@���@��+@�$�@��T@��h@�G�@��@���@�9X@���@��@��P@�t�@�33@��@���@���@���@�n�@���@��7@�hs@�hs@�x�@�x�@�`B@�?}@���@��`@�Ĝ@��D@��m@���@��R@�^5@�$�@��7@�X@���@�Q�@�9X@��@�  @���@��m@���@���@�S�@�
=@��H@��!@�v�@�E�@�$�@���@��`@�bN@�A�@�1@���@�t�@��@��y@�ȴ@���@�-@��-@�p�@���@��u@�j@�bN@�I�@�9X@�(�@�w@
=@~ff@~{@}O�@|�@|1@{dZ@z��@z��@z^5@y�@y�^@y��@y��@y��@yx�@yhs@x��@xbN@v��@v�@v�@v�@u�@u�h@u`B@u?}@u�@t�/@t��@t9X@s�F@st�@sS�@r�@r^5@q�^@qx�@q7L@p��@p�u@o�;@o
=@n�+@m�T@m�@l�@l�@k�
@kƨ@k�F@k��@k��@k�@k��@k�@k��@k��@k��@k�@kt�@kC�@j�H@j^5@i��@iG�@hb@g�P@g\)@g+@f��@f�+@fV@fV@fV@fV@f{@e�T@e@e�-@e`B@e�@d��@d�/@d�j@dz�@dZ@d�@c��@c�m@cƨ@cS�@b��@b=q@a��@a��@a��@a��@a��@aX@a�@`��@`r�@` �@_�@_�w@_��@_|�@_+@^��@^5?@^@]�-@]�h@]p�@]O�@]�@]�@]�@]�@]�@\�/@\��@\�j@\z�@\I�@[��@[��@[t�@[S�@[S�@[S�@[S�@[C�@[o@Z��@Z�\@Z=q@ZJ@Y�@Yhs@Y7L@XĜ@X�u@X�@XbN@X  @W�;@W��@W��@W��@W�w@W�w@W�P@WK�@W+@V�y@V�+@V@U�T@U�h@U�@T�/@S�F@R��@R�@Q�#@Q��@Q�7@Qx�@QG�@Q&�@Q�@P��@P�@PbN@P1'@Pb@O�@O�@O�P@OK�@N��@N$�@N$�@N@M�@M�T@M�h@M/@L�/@L�j@L�@Lz�@Lj@L(�@Kƨ@K"�@Jn�@Ix�@I7L@I%@H1'@Fv�@E@E�-@E�-@E�-@E��@E`B@Dz�@D�@C�@Co@B�H@B�H@B�!@Bn�@BM�@A��@A�@A��@A�7@A&�@@r�@@  @?�@?;d@>�y@>ff@=@=�@=O�@=/@<�/@<�D@<(�@;t�@;@:�H@:��@9�@9��@9x�@9X@9X@9&�@8Ĝ@8�@8bN@8b@7�@7\)@7+@7
=@6�@6��@6��@6��@6��@6��@6v�@6v�@6v�@6ff@6@5�-@4�/@4�D@4j@4I�@41@3�F@3t�@3S�@3"�@2�H@2��@2n�@2^5@2�@1�^@0r�@/�@/�w@/|�@/K�@-�@-�h@-�h@-��@-��@-��@-��@-��@-�@-?}@,��@,�@,�@,�D@,Z@,�@+C�@+33@+"�@*��@*=q@*J@)��@)hs@(��@(Ĝ@(��@(�@(Q�@(A�@( �@(  @'�@'|�@'\)@'
=@&ȴ@&�R@&��@&�+@&ff@&ff@&V@&@%?}@$��@$��@$�D@$9X@#�m@#�m@#�
@#��@#"�@#@"�H@"��@"��@"=q@"-@"�@"J@"�@!��@!�#@!�@��@\)@K�@�@��@�@ȴ@�R@��@v�@ff@E�@E�@5?@$�@�T@@�h@�h@p�@`B@�j@j@��@dZ@o@�H@��@J@��@&�@�9@�u@�u@r�@A�@��@��@��@v�@V@E�@5?@{@{@@��@�h@�@O�@?}@?}@/@/@V@V@��@��@(�@��@@��@��@�!@J@�#@�^@��@X@&�@&�@&�@�@%@Ĝ@bN@�@l�@�@�+@ff@$�@�T@�-@�@O�@��@�D@Z@9X@(�@�@�@1@��@ƨ@C�@o@
�H@
��@
�!@
�\@
n�@
=q@	��@	G�@�9@�@bN@bN@bN@bN@bN@bN@A�@Q�@Q�@Q�@ �@�@��@�P@\)@+@��@ȴ@��@��@v�@5?@@�-@��@��@`B@V@��@�D@�D@z�@j@j@Z@I�@�@��@��@��@�m@��@��@��@�m@�F@�F@��@��@dZ@S�@S�@S�@33@o@�H@�H@��@��@��@��@~�@n�@n�@=q@�#@��@��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B�B�B�B�B�B�B�B�B�B�B�B�B�B�B!�B#�B#�B#�B"�B!�B �B �B �B!�B"�B#�B$�B$�B$�B$�B$�B$�B$�B$�B#�B#�B"�B"�B!�B �B �B�B�BhB%B�B�HB�B�RB��B�\B�BXBC�B)�B��B��B�-B��B�By�Bt�Bm�BffB^5BT�BC�B9XB+B	7B
��B
��B
�B
�fB
��B
��B
��B
�?B
��B
��B
�1B
� B
x�B
v�B
s�B
o�B
n�B
l�B
gmB
`BB
M�B
@�B
33B
'�B
#�B
�B
�B
PB	��B	�B	�sB	��B	��B	B	�XB	��B	��B	�bB	�DB	�1B	�1B	�+B	�B	x�B	�B	�%B	�B	�B	�B	� B	|�B	z�B	s�B	hsB	cTB	aHB	_;B	_;B	_;B	_;B	^5B	[#B	YB	W
B	VB	S�B	N�B	J�B	F�B	C�B	=qB	7LB	49B	1'B	.B	)�B	%�B	!�B	�B	oB	JB	B	B��B��B��B�B�B�B�B�B�B�sB�mB�ZB�TB�NB�BB�/B�#B�B�
B��B��BȴB��B�^B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�{B�uB�\B�JB�7B�%B�B�B�B�B� B� B� B}�B|�Bz�By�By�Bx�Bw�Bv�Bu�Bt�Br�Bp�Bl�BiyBffBdZB`BB^5B[#BW
BT�BR�BP�BM�BK�BI�BH�BF�BD�B@�B=qB:^B9XB9XB9XB9XB8RB7LB6FB5?B49B2-B1'B0!B0!B/B/B/B/B/B/B/B.B,B)�B(�B)�B)�B)�B)�B)�B(�B'�B(�B(�B)�B)�B)�B+B,B+B,B-B.B/B/B.B.B.B/B.B.B1'B2-B33B49B5?B5?B5?B6FB6FB7LB6FB6FB6FB5?B6FB6FB8RB:^B:^B;dB<jB?}B@�B@�B@�BA�BA�BA�BA�BA�BG�BI�BK�BM�BR�BYBZB\)B]/B^5B_;B`BBaHBaHBaHBaHBbNBcTBdZBe`Be`BhsBp�Bv�Bw�B|�B~�B~�B� B�B�B�B�B�B�B�%B�B�=B�VB�hB��B��B��B��B��B��B��B��B��B�B�'B�3B�?B�dB�wB��BBÖBŢBǮBȴB��B��B��B��B��B��B�B�B�
B�
B�B�B�/B�;B�BB�BB�;B�;B�BB�BB�NB�TB�TB�ZB�mB�B�B�B�B��B��B��B	B	B	B	B	B	B	B	%B	1B	
=B	DB	PB	VB	bB	bB	{B	�B	�B	�B	!�B	$�B	'�B	+B	,B	-B	/B	2-B	6FB	8RB	=qB	?}B	@�B	A�B	A�B	B�B	B�B	D�B	E�B	H�B	J�B	L�B	N�B	P�B	S�B	VB	W
B	XB	ZB	[#B	[#B	[#B	[#B	\)B	\)B	^5B	_;B	e`B	e`B	e`B	e`B	hsB	jB	k�B	l�B	l�B	n�B	o�B	q�B	s�B	t�B	u�B	v�B	x�B	{�B	|�B	}�B	}�B	� B	�B	�%B	�7B	�JB	�\B	�hB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�'B	�-B	�-B	�3B	�9B	�9B	�9B	�?B	�?B	�FB	�FB	�FB	�XB	�^B	�dB	�qB	�wB	�}B	��B	��B	ÖB	ÖB	ĜB	ŢB	ƨB	ƨB	ǮB	ǮB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�
B	�B	�B	�B	�B	�#B	�)B	�)B	�/B	�5B	�5B	�5B	�;B	�BB	�BB	�BB	�BB	�BB	�BB	�BB	�HB	�HB	�NB	�TB	�ZB	�ZB	�`B	�fB	�fB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
1B
	7B
	7B
	7B
	7B
	7B
	7B
DB
JB
VB
VB
\B
\B
\B
bB
bB
hB
hB
hB
hB
oB
uB
{B
�B
�B
�B
�B
�B
�B
�B
�B
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
!�B
"�B
#�B
#�B
#�B
$�B
$�B
$�B
$�B
$�B
%�B
%�B
%�B
%�B
%�B
%�B
%�B
%�B
%�B
%�B
&�B
'�B
(�B
)�B
)�B
+B
+B
,B
,B
-B
-B
-B
.B
.B
.B
.B
0!B
1'B
2-B
2-B
2-B
49B
5?B
5?B
5?B
5?B
5?B
5?B
5?B
5?B
5?B
6FB
6FB
6FB
6FB
7LB
7LB
8RB
8RB
8RB
9XB
9XB
:^B
:^B
;dB
<jB
<jB
<jB
<jB
=qB
=qB
=qB
=qB
>wB
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
A�B
B�B
B�B
C�B
C�B
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
G�B
G�B
G�B
G�B
G�B
G�B
H�B
J�B
K�B
K�B
K�B
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
M�B
M�B
M�B
M�B
M�B
M�B
N�B
N�B
O�B
P�B
P�B
P�B
Q�B
Q�B
R�B
S�B
T�B
T�B
T�B
T�B
T�B
VB
W
B
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
ZB
ZB
ZB
ZB
ZB
ZB
ZB
ZB
[#B
\)B
]/B
]/B
]/B
]/B
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
bNB
bNB
cTB
cTB
cTB
dZB
dZB
dZB
e`B
e`B
ffB
ffB
ffB
ffB
ffB
ffB
ffB
ffB
gmB
gmB
hsB
hsB
hsB
hsB
hsB
hsB
iyB
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
k�B
k�B
k�B
l�B
l�B
l�B
l�B
m�B
m�B
m�B
m�B
n�B
n�B
n�B
n�B
o�B
o�B
o�B
o�B
o�B
p�B
p�B
q�B
q�B
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
r�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
t�B
t�B
t�B
t�B
u�B
u�B
u�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B�B�B�B�B�B�B�B�B�B�B�BBdBpB"4B#�B$B$B#B"B!B �B �B!�B#B$B$�B$�B$�B$�B$�B$�B$�B$�B#�B#�B"�B"�B!�B �B �B \BB&B�B�B��B�xB��B��B�hB��B[WBG�B0oB��B�B�ZB�~B��Bz�Bu�Bn�Bg�B_�BWYBEmB<6B/�BxB
��B
��B
�;B
��B
�mB
�0B
�AB
��B
��B
�=B
��B
� B
yXB
wLB
tB
pB
oOB
mwB
i*B
b�B
P.B
C�B
5%B
(�B
%`B
 �B
�B
HB	�qB	�-B	�kB	ևB	�B	�3B	�B	�kB	�B	�4B	��B	��B	��B	�fB	�B	yrB	�3B	��B	�MB	�aB	��B	��B	}�B	|�B	vB	iyB	d&B	a�B	_�B	_�B	_�B	_�B	_B	[�B	YeB	WYB	V�B	UMB	O�B	K�B	G_B	D�B	>�B	8B	4�B	2B	/iB	+kB	'mB	#�B	�B	B	VB	%B	�B��B�]B�	B�nB�-B�AB�OB�CB��B�B�>B��B�B�B�B��B��B��B��B�2B�hB�rB�B��B��B�FB��B�#B��B�B��B��B�|B�B�'B�CB��B��B�B�B�B��B��B�2B��B��B�B��B�B��B��B��B�;B�B�4B��B~�B}�B{dBz*Bz^By>BxRBwfBv�Bu�BtBr�Bn/Bj�Bh
Bf2Ba�B_�B]�BX�BU�BS�BR BO�BMBJ=BI�BH1BF�BC�B@iB;�B9�B9�B9�B9�B9>B8�B6�B6B6+B4B2|B1AB1B/iB/OB/OB/iB/iB/�B/�B/�B.IB+QB*�B*B*KB*0B*0B*eB)�B)yB)�B*B*�B*�B*�B+�B,qB+�B,�B-�B.}B/5B/5B.�B/ B/ B0!B/�B/�B1�B2�B3�B4�B5�B5�B5�B6zB6zB7fB6�B6�B72B6B6�B7LB9	B:�B:�B<B=<B?�B@�B@�B@�BA�BA�BB'BBuBCBH�BJrBL�BO(BTBY�BZ�B\�B]�B^�B_�B`vBa|BabBa|Ba�Bb�Bc�Bd�Be�Bf�BjBq�BwfBx�B}<B.BHB�4B�;B�UB�AB�[B�{B��B��B��B��B��B�B��B��B�B�QB�]B�-B�B�ZB��B��B��B��B��B��B��B��B��B��B��B��B�B�B�"B�B�&B�,B�2B�B�9B�$B�?B�_B�eB�dB�VB�BB�BB�VB�pB�vB�vB�hB�B�B��B��B�B��B��B�B�B�XB�.B	B	'B	-B	3B	B	3B	SB	YB	fB	
rB	xB	�B	�B	�B	�B	�B	�B	�B	�B	!�B	%,B	(>B	+6B	,=B	-)B	/�B	2�B	6�B	8�B	=�B	?�B	@�B	A�B	A�B	B�B	B�B	D�B	E�B	H�B	KB	MB	O(B	Q4B	TB	VB	W
B	XEB	Z7B	[#B	[#B	[=B	[=B	\CB	\xB	^�B	_�B	e`B	ezB	ezB	e�B	h�B	j�B	k�B	l�B	l�B	n�B	o�B	q�B	s�B	t�B	u�B	wB	y	B	|B	}B	~B	~(B	�OB	�aB	�YB	�lB	��B	��B	��B	��B	��B	��B	��B	�mB	��B	��B	��B	�sB	��B	��B	��B	��B	��B	��B	��B	��B	�B	�'B	�B	�B	�
B	��B	�*B	�B	�B	�B	�"B	�"B	�)B	�)B	�/B	�IB	�5B	�AB	�-B	�GB	�3B	�TB	�TB	�9B	�ZB	�?B	�zB	�zB	�`B	�XB	�xB	�B	�qB	�wB	�}B	��B	��B	ðB	ðB	ĶB	żB	��B	ƨB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	� B	� B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�$B	�+B	�1B	�7B	�7B	�=B	�CB	�]B	�/B	�OB	�OB	�5B	�VB	�BB	�'B	�BB	�\B	�BB	�BB	�BB	�bB	�bB	�B	�B	�tB	�B	�B	�B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	�B	�B	�B	��B	�.B
 B
 B
 B
B
B
 B
 B
UB
[B
aB
B
MB
�B
�B
fB
	7B
	7B
	7B
	RB
	lB
	lB
xB
~B
pB
pB
\B
vB
vB
bB
}B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
!�B
"�B
#�B
#�B
$B
$�B
$�B
$�B
$�B
$�B
%�B
%�B
%�B
%�B
%�B
%�B
%�B
%�B
&B
&B
'8B
(
B
(�B
*B
)�B
+B
+B
,"B
,"B
-)B
-)B
-)B
./B
.IB
.IB
.}B
0UB
1'B
2GB
2aB
2�B
4TB
5?B
5?B
5?B
5?B
5?B
5?B
5ZB
5ZB
5tB
6`B
6FB
6FB
6`B
7fB
7�B
8RB
8lB
8�B
9�B
9rB
:xB
:�B
;�B
<�B
<jB
<�B
<�B
=�B
=�B
=�B
=qB
>�B
?}B
?�B
@�B
@iB
@�B
@�B
@�B
@�B
@�B
@�B
A�B
B�B
B�B
C�B
C�B
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
G�B
G�B
G�B
G�B
G�B
G�B
IB
J�B
K�B
K�B
K�B
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
M�B
M�B
M�B
M�B
M�B
NB
OB
OB
O�B
Q B
Q B
QB
R B
R B
S&B
TB
T�B
T�B
UB
UB
U2B
VSB
W$B
X+B
XEB
XB
XB
X+B
XB
X�B
YB
Y1B
Y1B
Y1B
ZB
ZB
Z7B
ZB
Z7B
ZB
Z7B
Z7B
ZQB
[WB
\]B
]IB
]IB
]IB
]dB
^OB
^OB
^5B
^OB
_VB
_;B
_;B
_;B
_VB
_VB
_pB
`vB
`\B
a|B
bhB
bhB
cnB
cnB
cnB
dZB
dZB
dtB
ezB
e`B
f�B
ffB
ffB
ffB
ffB
f�B
f�B
f�B
g�B
g�B
hsB
hsB
h�B
h�B
h�B
h�B
i�B
j�B
k�B
k�B
k�B
kkB
kkB
k�B
k�B
k�B
k�B
k�B
kkB
k�B
l�B
l�B
l�B
l�B
m�B
m�B
m�B
m�B
n�B
n�B
n�B
n�B
o�B
o�B
o�B
o�B
o�B
p�B
p�B
q�B
q�B
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
r�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
t�B
t�B
t�B
t�B
u�B
u�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=-0.02(dbar)                                                                                                                                                                                                                                                  None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201802080038412018020800384120180208003841202211182133272022111821332720221118213327201804031939062018040319390620180403193906  JA  ARFMdecpA19c                                                                20180129033516  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20180128183547  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20180128183548  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20180128183549  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20180128183549  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20180128183549  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20180128183550  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20180128183550  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20180128183550  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20180128183550                      G�O�G�O�G�O�                JA  ARUP                                                                        20180128185707                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20180129153122  CV  JULD            G�O�G�O�F�A                JM  ARSQJMQC2.0                                                                 20180131000000  CF  PSAL_ADJUSTED_QCD�6fD�6fG�O�                JM  ARCAJMQC2.0                                                                 20180207153841  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20180207153841  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180403103906  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200114171535                      G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20221118123327  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20221123111507                      G�O�G�O�G�O�                