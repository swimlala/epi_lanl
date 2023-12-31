CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-05T19:05:02Z creation      
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
resolution        =���   axis      Z        l  9p   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  @�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     l  B�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  J$   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     l  L    TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     l  Sl   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  Z�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     l  \�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  d    TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     l  e�   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     l  mh   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  t�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     l  v�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ~   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     l  �   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �d   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
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
_FillValue                    �@Argo profile    3.1 1.2 19500101000000  20181005190502  20181005190502  5904951 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               A   AO  6430                            2B  A   APEX                            7465                            062512                          846 @כ�[��1   @כ�   @4y�"��`�c��+1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      A   A   A   @�ff@�  A   A   A@  A^ffA~ffA�  A�  A�  A�33A�33A�33A�  B   B  B  B  B   B(  B0ffB8  B@  BG��BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C	�fC  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8y�D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[�fD\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dw� Dy~D�,)D���1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @nz@���@��AQ�A8Q�AV�RAv�RA�(�A�(�A�(�A�\)A�\)A�\)A�(�A�(�B{B{B{B{B&{B.z�B6{B>{BE�BN{BV{B^{Bf{Bn{Bv{B~{B�
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
=B�
=B�
=C�C�C�C�C	k�C�C�C�C�C�C�C�C�C�C�C�C!�C#�C%�C'�C)�C+�C-�C/�C1�C3�C5�C7�C9�C;�C=�C?�CA�CC�CE�CG�CI�CK�CM�CO�CQ�CS�CU�CW�CY�C[�C]�C_�Ca�Cc�Ce�Cg�Ci�Ck�Cm�Co�Cq�Cs�Cu�Cw�Cy�C{�C}�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C���C�C�C�C�C�C�C�C�C��\C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C��\C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�D aHD �HDaHD�HDaHD�HDaHD�HDaHD�HDaHD�HDaHD�HDaHD�HDaHD�HD	aHD	�HD
aHD
�HDaHD�HDaHD�HDaHD�HDaHD�HDaHD�HDaHD�HDaHD�HDaHD�HDaHD�HDaHD�HDaHD�HDaHD�HDaHD�HDaHD�HDaHD�HDaHD�HDaHD�HDaHD�HDaHD�HDaHD�HDaHD�HD aHD �HD!aHD!�HD"aHD"�HD#aHD#�HD$aHD$�HD%aHD%�HD&aHD&�HD'aHD'�HD(aHD(�HD)aHD)�HD*aHD*�HD+aHD+�HD,aHD,�HD-aHD-�HD.aHD.�HD/aHD/�HD0aHD0�HD1aHD1�HD2aHD2�HD3aHD3�HD4aHD4�HD5aHD5�HD6aHD6�HD7aHD7�HD8Z�D8�HD9aHD9�HD:aHD:�HD;aHD;�HD<aHD<�HD=aHD=�HD>aHD>�HD?aHD?�HD@aHD@�HDAaHDA�HDBaHDB�HDCaHDC�HDDaHDD�HDEaHDE�HDFaHDF�HDGaHDG�HDHaHDH�HDIaHDI�HDJaHDJ�HDKaHDK�HDLaHDL�HDMaHDM�HDNaHDN�HDOaHDO�HDPaHDP�HDQaHDQ�HDRaHDR�HDSaHDS�HDTaHDT�HDUaHDU�HDVaHDV�HDWaHDW�HDXaHDX�HDYaHDY�HDZaHDZ�HD[g�D[�HD\aHD\�HD]aHD]�HD^aHD^�HD_aHD_�HD`aHD`�HDaaHDa�HDbaHDb�HDcaHDc�HDdaHDd�HDeaHDe�HDfaHDf�HDgaHDg�HDhaHDh�HDiaHDi�HDjaHDj�HDkaHDk�HDlaHDl�HDmaHDm�HDnaHDn�HDoaHDo�HDpaHDp�HDqaHDq�HDraHDr�HDsaHDs�HDtaHDt�HDuaHDu�HDvaHDv�HDwaHDw�HDy_\D��D���1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�K�A�I�A�E�A�A�A�?}A�C�A�K�A�K�A�I�A�K�A�O�A�Q�A�ZA�A�A�;dA�=qA�1'A��A�JA��#A�AɸRAɁA���A�5?A�9XAǺ^A�%A��HA���A�ƨA��`A��A��A��A��;A��A�%A��AŅA�=qA��A�1'A��AøRA���A�bNA���A�%A�bNA��-A�A�&�A�n�A��A�33A��A�-A�z�A���A�v�A�+A��A���A��A�-A��A���A��TA��hA���A��RA���A�+A�r�A���A�`BA�v�A��FA�/A�\)A�9XA�~�A��FA��A��A��A���A�r�A�"�A��!A��jA�~�A�dZA�^5A�dZA��PA���A�~�A�&�A�O�A���A�dZA��A{7LAv�jAr�An^5AmC�Al��Al��AkdZAj~�Aj1'Ah��Ac�hA`�HA^�A[��AYl�AU|�AT��AT�HAT�HAT��ATv�ASVAN-AI�AHr�AG?}AE\)AD��AAA=A;�A9��A8�A6��A3�PA-�wA*M�A%C�A"�9A ȴA��A�jA{AdZAn�A=qAJA��A�A"�A�A��AK�A��A�jA=qAt�AVA~�AƨA33A7LA&�A+A�AoA ��@���@�n�@���@���@���@�@�@��@�@�9@�9@�@��@���@�!@���@�&�@�9X@�ȴ@�x�@�@��@�O�@�z�@�r�@�@��T@�bN@��@�K�@�@�ƨ@��m@��;@�D@��-@��@�1'@�E�@�p�@�r�@���@�(�@�w@���@��@��@߅@�~�@݉7@�{@��#@ݡ�@ܼj@�C�@���@�
=@��H@�%@��
@�X@���@˅@�|�@�@���@ȴ9@�Ĝ@�I�@�dZ@��@��@�p�@�%@�Ĝ@Ĭ@��@�S�@�M�@��7@�O�@��`@��9@��j@�r�@� �@��
@�S�@���@�E�@��T@���@���@��@�?}@��`@��j@��u@�Q�@�A�@�1@���@�@�-@���@�I�@��@�^5@�E�@�V@�~�@�E�@��h@��D@��@�C�@���@���@��T@�hs@���@�v�@�%@�r�@���@�l�@�dZ@�\)@�;d@��H@�$�@�X@�?}@�7L@�/@��@�&�@��7@�O�@���@�z�@��D@��D@���@���@�Ĝ@��/@��D@��u@�$�@�A�@��u@�Z@�b@� �@���@�o@�33@��\@�`B@�?}@���@��j@�j@�1@���@�1@�(�@� �@��m@���@���@�S�@���@���@�E�@�{@���@��@�I�@���@�K�@��y@��y@�J@�p�@�@�@��h@�`B@�x�@��@�`B@�hs@�G�@�7L@�/@��j@��F@���@��@�l�@�dZ@�"�@�"�@�"�@��@���@�=q@�@�G�@���@�z�@��@��@��w@�\)@�~�@���@�hs@���@��9@��9@�Ĝ@��j@��9@��9@��@���@�(�@�ƨ@��@���@���@���@��P@�|�@�K�@�33@��@��@�@��-@�`B@� �@�C�@�ȴ@���@�V@�E�@�J@��@��T@��T@��T@�@��h@�O�@�&�@�7L@�G�@�X@�`B@�`B@�`B@�hs@�p�@���@��@���@��@�`B@�V@�&�@�&�@���@�Ĝ@���@��D@�1@��;@��
@���@��@�C�@��@��H@�^5@�@���@���@��@�Q�@���@�K�@�+@�
=@�@��@��@��r@p��@`�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�K�A�I�A�E�A�A�A�?}A�C�A�K�A�K�A�I�A�K�A�O�A�Q�A�ZA�A�A�;dA�=qA�1'A��A�JA��#A�AɸRAɁA���A�5?A�9XAǺ^A�%A��HA���A�ƨA��`A��A��A��A��;A��A�%A��AŅA�=qA��A�1'A��AøRA���A�bNA���A�%A�bNA��-A�A�&�A�n�A��A�33A��A�-A�z�A���A�v�A�+A��A���A��A�-A��A���A��TA��hA���A��RA���A�+A�r�A���A�`BA�v�A��FA�/A�\)A�9XA�~�A��FA��A��A��A���A�r�A�"�A��!A��jA�~�A�dZA�^5A�dZA��PA���A�~�A�&�A�O�A���A�dZA��A{7LAv�jAr�An^5AmC�Al��Al��AkdZAj~�Aj1'Ah��Ac�hA`�HA^�A[��AYl�AU|�AT��AT�HAT�HAT��ATv�ASVAN-AI�AHr�AG?}AE\)AD��AAA=A;�A9��A8�A6��A3�PA-�wA*M�A%C�A"�9A ȴA��A�jA{AdZAn�A=qAJA��A�A"�A�A��AK�A��A�jA=qAt�AVA~�AƨA33A7LA&�A+A�AoA ��@���@�n�@���@���@���@�@�@��@�@�9@�9@�@��@���@�!@���@�&�@�9X@�ȴ@�x�@�@��@�O�@�z�@�r�@�@��T@�bN@��@�K�@�@�ƨ@��m@��;@�D@��-@��@�1'@�E�@�p�@�r�@���@�(�@�w@���@��@��@߅@�~�@݉7@�{@��#@ݡ�@ܼj@�C�@���@�
=@��H@�%@��
@�X@���@˅@�|�@�@���@ȴ9@�Ĝ@�I�@�dZ@��@��@�p�@�%@�Ĝ@Ĭ@��@�S�@�M�@��7@�O�@��`@��9@��j@�r�@� �@��
@�S�@���@�E�@��T@���@���@��@�?}@��`@��j@��u@�Q�@�A�@�1@���@�@�-@���@�I�@��@�^5@�E�@�V@�~�@�E�@��h@��D@��@�C�@���@���@��T@�hs@���@�v�@�%@�r�@���@�l�@�dZ@�\)@�;d@��H@�$�@�X@�?}@�7L@�/@��@�&�@��7@�O�@���@�z�@��D@��D@���@���@�Ĝ@��/@��D@��u@�$�@�A�@��u@�Z@�b@� �@���@�o@�33@��\@�`B@�?}@���@��j@�j@�1@���@�1@�(�@� �@��m@���@���@�S�@���@���@�E�@�{@���@��@�I�@���@�K�@��y@��y@�J@�p�@�@�@��h@�`B@�x�@��@�`B@�hs@�G�@�7L@�/@��j@��F@���@��@�l�@�dZ@�"�@�"�@�"�@��@���@�=q@�@�G�@���@�z�@��@��@��w@�\)@�~�@���@�hs@���@��9@��9@�Ĝ@��j@��9@��9@��@���@�(�@�ƨ@��@���@���@���@��P@�|�@�K�@�33@��@��@�@��-@�`B@� �@�C�@�ȴ@���@�V@�E�@�J@��@��T@��T@��T@�@��h@�O�@�&�@�7L@�G�@�X@�`B@�`B@�`B@�hs@�p�@���@��@���@��@�`B@�V@�&�@�&�@���@�Ĝ@���@��D@�1@��;@��
@���@��@�C�@��@��H@�^5@�@���@���@��@�Q�@���@�K�@�+@�
=@�@��@��@��r@p��@`�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
�9B
�9B
�9B
�9B
�9B
�9B
�9B
�9B
�9B
�9B
�9B
�9B
�3B
�?B
�?B
�?B
�FB
�LB
�RB
�^B
�dB
�jB
�}B
ȴB
�B
�B
�HBbB�B �BC�BR�B[#BaHB�B��B��B��BB�B-B6FB7LB8RBL�BE�B49B'�B$�B�B�B�B"�B-B8RBD�BF�BffB{�B�\B��B��B��B��B��B��B��B�uB�%B� Bp�BL�BF�BI�B?}B-B$�B�B&�B:^B#�BbB��B��B��B�RB�oB�BS�B!�B
��B
�HB
�#B
ƨB
��B
�B
^5B
P�B
P�B
M�B
D�B
;dB
7LB
0!B
{B	��B	�B	�^B	�-B	�B	�B	��B	�{B	�\B	|�B	R�B	B�B	6FB	%�B	uB	B	  B	B	B	B	B	+B	B�B�B�sB�;B�B��BÖB��B�wB�dB�FB�B��B�BiyBffBcTBffBgmBhsBiyBk�Bl�Bm�Bo�Bq�Bs�B}�B�bB�\B�PB�JB�DB�JB�JB�PB��B��B��B��B��B��B��B��B�JB�1B�B�B�B�B� B}�B|�B|�B|�B}�B}�B� B� B�B�B�B�7B�DB��B��B��B��B��B��B�9B��B�B�B�B�B�B�B��B	B	�B	�B	�B	hB	JB	B��B��B��B�)B�B�sB�mB�`B�B�B�B�B�mB�NB�5B�)B�B�B�B�B�#B�)B�5B�TB�fB�yB�yB�B�B��B��B��B��B	  B	B		7B	1B	DB	VB	bB	�B	�B	�B	�B	�B	"�B	"�B	$�B	%�B	&�B	&�B	&�B	(�B	)�B	)�B	+B	,B	-B	.B	0!B	2-B	33B	33B	1'B	0!B	/B	0!B	1'B	2-B	2-B	33B	33B	49B	49B	49B	5?B	7LB	8RB	9XB	;dB	@�B	C�B	G�B	I�B	J�B	J�B	K�B	L�B	Q�B	W
B	XB	YB	]/B	dZB	ffB	l�B	o�B	p�B	t�B	w�B	x�B	x�B	y�B	z�B	|�B	� B	�B	}�B	y�B	~�B	�%B	�DB	�\B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�-B	�-B	�'B	�!B	�'B	�3B	�?B	�?B	�RB	�RB	�LB	�XB	�^B	�qB	��B	��B	B	ĜB	ɺB	ɺB	��B	��B	��B	ȴB	��B	��B	��B	��B	�
B	�
B	�
B	�B	�B	�#B	�#B	�B	�B	�)B	�/B	�5B	�5B	�;B	�;B	�5B	�5B	�5B	�HB	�NB	�TB	�TB	�TB	�ZB	�ZB	�ZB	�fB	�sB	�yB	�yB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
%B
%B
%B
%B
+B
1B
1B

=B

=B
DB
JB
JB
JB
PB
PB
PB
bB
BB
KB
'�2222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222 B
�9B
�9B
�9B
�9B
�9B
�9B
�9B
�9B
�9B
�9B
�9B
�9B
�3B
�?B
�?B
�?B
�FB
�LB
�RB
�^B
�dB
�jB
�}B
ȴB
�B
�B
�HBbB�B �BC�BR�B[#BaHB�B��B��B��BB�B-B6FB7LB8RBL�BE�B49B'�B$�B�B�B�B"�B-B8RBD�BF�BffB{�B�\B��B��B��B��B��B��B��B�uB�%B� Bp�BL�BF�BI�B?}B-B$�B�B&�B:^B#�BbB��B��B��B�RB�oB�BS�B!�B
��B
�HB
�#B
ƨB
��B
�B
^5B
P�B
P�B
M�B
D�B
;dB
7LB
0!B
{B	��B	�B	�^B	�-B	�B	�B	��B	�{B	�\B	|�B	R�B	B�B	6FB	%�B	uB	B	  B	B	B	B	B	+B	B�B�B�sB�;B�B��BÖB��B�wB�dB�FB�B��B�BiyBffBcTBffBgmBhsBiyBk�Bl�Bm�Bo�Bq�Bs�B}�B�bB�\B�PB�JB�DB�JB�JB�PB��B��B��B��B��B��B��B��B�JB�1B�B�B�B�B� B}�B|�B|�B|�B}�B}�B� B� B�B�B�B�7B�DB��B��B��B��B��B��B�9B��B�B�B�B�B�B�B��B	B	�B	�B	�B	hB	JB	B��B��B��B�)B�B�sB�mB�`B�B�B�B�B�mB�NB�5B�)B�B�B�B�B�#B�)B�5B�TB�fB�yB�yB�B�B��B��B��B��B	  B	B		7B	1B	DB	VB	bB	�B	�B	�B	�B	�B	"�B	"�B	$�B	%�B	&�B	&�B	&�B	(�B	)�B	)�B	+B	,B	-B	.B	0!B	2-B	33B	33B	1'B	0!B	/B	0!B	1'B	2-B	2-B	33B	33B	49B	49B	49B	5?B	7LB	8RB	9XB	;dB	@�B	C�B	G�B	I�B	J�B	J�B	K�B	L�B	Q�B	W
B	XB	YB	]/B	dZB	ffB	l�B	o�B	p�B	t�B	w�B	x�B	x�B	y�B	z�B	|�B	� B	�B	}�B	y�B	~�B	�%B	�DB	�\B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�-B	�-B	�'B	�!B	�'B	�3B	�?B	�?B	�RB	�RB	�LB	�XB	�^B	�qB	��B	��B	B	ĜB	ɺB	ɺB	��B	��B	��B	ȴB	��B	��B	��B	��B	�
B	�
B	�
B	�B	�B	�#B	�#B	�B	�B	�)B	�/B	�5B	�5B	�;B	�;B	�5B	�5B	�5B	�HB	�NB	�TB	�TB	�TB	�ZB	�ZB	�ZB	�fB	�sB	�yB	�yB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
%B
%B
%B
%B
+B
1B
1B

=B

=B
DB
JB
JB
JB
PB
PB
PB
bB
BB
KB
'�2222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=0.48 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181005190502                              AO  ARCAADJP                                                                    20181005190502    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181005190502  QCP$                G�O�G�O�G�O�F03E            AO  ARGQQCPL                                                                    20181005190502  QCF$                G�O�G�O�G�O�8000            