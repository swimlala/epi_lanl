CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2016-12-29T00:35:16Z creation;2016-12-29T00:35:19Z conversion to V3.1;2019-12-19T08:22:03Z update;     
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
resolution        =���   axis      Z        L  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  H�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     L  L�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     L  _�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  o(   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     L  r�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �H   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     L  �   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �h   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     L  �<   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     L  �\   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     L  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     L  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  �@   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �P   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �T   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �X   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �\   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �`   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20161229003516  20200115111516  4902363                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               HA   JA  I2_0576_072                     2C  D   NAVIS_A                         0576                            ARGO 092515                     863 @��5�x9�1   @��6�� @:��H˒�d�I�^1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @�  @�  A   A   A@  A`  A���A�  A�  A�  A�  A�  A�  A�  B   B��B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C��C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�C3D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�3D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�C3D�3D��3D�  1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @aG�@���@��AQ�A8Q�AXQ�Ay�A�(�A�(�A�(�A�(�A�(�A�(�A�(�A�(�B�B{B{B{B&{B.{B6{B>{BF{BN{BV{B^{Bf{Bn{Bv{B~{B�
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
=C�C�C�C�C	�C�C�C�C�C�C�C�C�C�C�C�C!�C#�C%�C'�C)�C+�C-�C/�C1�C3�C5�C7�C9�C;�C=�C?�CA�CC�CE�CG�CI�CK�CM�CO�CQ�CS�CU�CW�CY�C[�C]�C_�Ca�Cc�Ce�Cg�Ci�Ck�Cm�Co�Cq�Cs�Cu�Cw�Cy�C{�C}�C�C�C�C�C�C��\C��\C��\C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�D aHD �HDaHD�HDaHD�HDaHD�HDaHD�HDaHD�HDaHD�HDaHD�HDaHD�HD	aHD	�HD
aHD
�HDaHD�HDaHD�HDaHD�HDaHD�HDaHD�HDaHD�HDaHD�HDaHD�HDaHD�HDaHD�HDaHD�HDaHD�HDaHD�HDaHD�HDaHD�HDaHD�HDaHD�HDaHD�HDaHD�HDaHD�HDaHD�HD aHD �HD!aHD!�HD"aHD"�HD#aHD#�HD$aHD$�HD%aHD%�HD&aHD&�HD'aHD'�HD(aHD(�HD)aHD)�HD*aHD*�HD+aHD+�HD,aHD,�HD-aHD-�HD.aHD.�HD/aHD/�HD0aHD0�HD1aHD1�HD2aHD2�HD3aHD3�HD4aHD4�HD5aHD5�HD6aHD6�HD7aHD7�HD8aHD8�HD9aHD9�HD:aHD:�HD;aHD;�HD<aHD<�HD=aHD=�HD>aHD>�HD?aHD?�HD@aHD@�HDAaHDA�HDBaHDB�HDCaHDC�HDDaHDD�HDEaHDE�HDFaHDF�HDGaHDG�HDHaHDH�HDIaHDI�HDJaHDJ�HDKaHDK�HDLaHDL�HDMaHDM�HDNaHDN�HDOaHDO�HDPaHDP�HDQaHDQ�HDRaHDR�HDSaHDS�HDTaHDT�HDUaHDU�HDVaHDV�HDWaHDW�HDXaHDX�HDYaHDY�HDZaHDZ�HD[aHD[�HD\aHD\�HD]aHD]�HD^aHD^�HD_aHD_�HD`aHD`�HDaaHDa�HDbaHDb�HDcaHDc�HDdaHDd�HDeaHDe�HDfaHDf�HDgaHDg�HDhaHDh�HDiaHDi�HDjaHDj�HDkaHDk�HDlaHDl�HDmaHDm�HDnaHDn�HDoaHDo�HDpaHDp�HDqaHDq�HDraHDr�HDsaHDs�HDtaHDt�HDuaHDu�HDvaHDv�HDwaHDw�HDxaHDx�HDyaHDy�HDzaHDz�HD{aHD{�HD|aHD|�HD}aHD}�HD~aHD~�HDaHD�HD�0�D�p�D���D��D�0�D�p�D���D��D�0�D�p�D���D��D�0�D�p�D���D��D�0�D�p�D���D��D�0�D�p�D���D��D�0�D�p�D���D��D�0�D�p�D���D��D�0�D�p�D���D��D�0�D�p�D���D��D�0�D�p�D���D��D�0�D�p�D���D��D�0�D�p�D���D��D�0�D�p�D���D��D�0�D�p�D���D��D�0�D�p�D���D��D�0�D�p�D���D��D�0�D�p�D���D��D�0�D�p�D���D��D�0�D�p�D���D��D�0�D�p�D���D��D�0�D�p�D���D��D�0�D�p�D���D��D�0�D�p�D���D��D�0�D�p�D���D��D�0�D�p�D���D��D�0�D�p�D���D��D�0�D�p�D���D��D�0�D�p�D���D��D�0�D�p�D���D��D�0�D�p�D���D��D�0�D�p�D���D��D�0�D�p�D���D��D�0�D�p�D���D��D�0�D�p�D���D��D�0�D�p�D���D��D�0�D�p�D���D��D�0�D�p�D���D��D�0�D�p�D���D��D�0�D�s�D���D��D�0�D�p�D���D��D�0�D�p�D���D��D�0�D�p�D���D��D�0�D�p�D���D��D�0�D�p�D���D��D�0�D�p�D���D��D�0�D�p�D���D��D�0�D�p�D���D��D�0�D�p�D���D��D�0�D�p�D���D��D�0�D�p�D���D��D�0�D�p�D���D��D�0�D�p�D���D��D�0�D�p�D���D��D�0�D�p�D���D��D�0�D�p�D���D��D�0�D�p�D���D��D�0�D�p�D���D��D�0�D�p�D���D��D�0�D�p�D���D��D�0�D�p�D���D��D�0�D�p�D���D��D�0�D�p�D���D��D�0�D�p�D���D��D�0�D�p�D���D��D�0�D�p�D���D��D�0�D�p�D°�D��D�0�D�p�Dð�D��D�0�D�p�Dİ�D��D�0�D�p�DŰ�D��D�0�D�p�Dư�D��D�0�D�p�Dǰ�D��D�0�D�p�DȰ�D��D�0�D�p�Dɰ�D��D�0�D�p�Dʰ�D��D�0�D�p�D˰�D��D�0�D�p�D̰�D��D�0�D�p�DͰ�D��D�0�D�p�Dΰ�D��D�0�D�p�Dϰ�D��D�0�D�p�Dа�D��D�0�D�p�DѰ�D��D�0�D�p�DҰ�D��D�0�D�p�DӰ�D��D�0�D�p�D԰�D��D�0�D�p�Dհ�D��D�0�D�p�Dְ�D��D�0�D�p�Dװ�D��D�3�D�p�Dذ�D��D�0�D�p�Dٰ�D��D�0�D�p�Dڰ�D��D�0�D�p�D۰�D��D�0�D�p�Dܰ�D��D�0�D�p�Dݰ�D��D�0�D�p�Dް�D��D�0�D�p�D߰�D��D�0�D�p�DతD��D�0�D�p�DᰤD��D�0�D�p�DⰤD��D�0�D�p�D㰤D��D�0�D�p�D䰤D��D�0�D�p�D尤D��D�0�D�p�D氤D��D�0�D�p�D簤D��D�0�D�p�D谤D��D�0�D�p�D鰤D��D�0�D�p�D갤D��D�0�D�p�D밤D��D�0�D�p�D찤D��D�0�D�p�D���D��D�0�D�p�DD��D�0�D�p�DﰤD��D�0�D�p�D�D��D�0�D�s�D�D��D�0�D�p�D�D��D�0�D�p�D�D��D�3�D�s�D���D��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�ZA�\)A�^5A�^5A�ZA�\)A�^5A�bNA�dZA�dZA�ffA�hsA�ffA�dZA�dZA�ffA�n�A�p�A�p�A�r�A�r�A�r�A�r�A�v�A�v�A�t�A�t�A�v�A�v�A�t�A�t�A�v�A�x�A�z�A�v�A�v�A�z�A�x�A�v�A�Q�A�?}A��A���A�^5A��A�JA���A���A�ȴA�+A��A��TA�bA��DA��A��DA��^A��9A�%A�~�A���A���A��A���A�G�A�p�A���A�A�A��A�t�A��A���A�5?A�A��\A�~�A��#A��!A�\)A��TA��A�Q�A��;A�`BA��A�Q�A�
A}l�Az�/Ay��Ax��AwXAt�HArM�Ao��Ao/An �Al�9Aj��AiƨAiXAh�HAh�!Ah��AhbNAg��AgXAgoAf�9AfbNAf9XAe�mAe?}Ad~�AbĜAa�^A`�A_�A_�A_�
A_7LA^$�A]dZA\ĜA[x�AYAV��AU+AT�jATz�AT1'AS\)ARz�AQ��AP��AOAL��AK?}AIƨAI�AG|�AFJAEADM�AC��AC+AAhsA@bNA?��A?
=A>�A>ĜA>~�A>�A=;dA;O�A:�DA:E�A9ƨA9XA8��A8~�A8�+A8~�A8n�A7x�A5�PA4ZA41'A3��A2��A2{A1�^A1XA0��A0�jA/�-A.�A-;dA-"�A-"�A-"�A-"�A-�A-
=A,��A,�`A,Q�A*9XA)/A)
=A(��A'�
A'�A&Q�A%A%G�A$�A$�9A$n�A#O�A"E�A"�A!�PA ��A n�A (�AK�A��A�A&�A5?AhsA��Az�A1'A��A�Ap�A  A
=A�
A��Av�AA��A�!A+A9XA�AC�A
��A
v�A
1'A	�A�+A�AbNA�-A��A��AXA�DA��AhsAA ��A ��A I�A �@�l�@���@�?}@�ƨ@�@�r�@��;@���@�"�@���@�hs@� �@�n�@��@�j@�n�@�5?@�V@�33@�Q�@�ȴ@�v�@��@���@�j@�\)@�`B@ޏ\@��#@�/@�bN@���@���@�dZ@�`B@ԋD@�K�@��`@��;@�|�@�+@Η�@�Ĝ@�v�@���@�hs@ȣ�@�z�@��;@���@�V@�x�@ļj@�
=@�=q@�V@���@��m@�@�r�@��P@�"�@�J@��@��;@�dZ@���@�5?@���@�V@��@�Ĝ@�A�@���@�+@�x�@��@�9X@�ƨ@�dZ@�33@�J@�7L@��@�$�@��@��-@�?}@�b@�;d@�V@��H@�\)@��@�V@�n�@��h@�\)@�33@���@��@��@�ƨ@��@��D@���@�|�@��@�@���@��u@�bN@���@���@��;@��
@���@��F@�C�@��@��R@���@�V@��#@�`B@�?}@�`B@���@�j@�I�@�I�@� �@���@�;d@���@�^5@�$�@��T@�%@�bN@�(�@� �@�1@���@��m@���@�ƨ@�\)@�@���@�J@��@��-@�`B@�G�@��@���@�1'@�S�@�{@��T@��^@�p�@�&�@�%@��/@�z�@�b@��P@��@�o@���@�5?@��@�$�@�E�@�v�@���@�j@�b@��@��P@�l�@�@��y@��H@��@���@�^5@�-@��T@��7@���@�z�@�r�@�bN@�bN@�bN@�Q�@��@�;@��@\)@
=@~�R@~�+@~V@~E�@~E�@~V@~ff@~v�@~v�@~�+@~�+@~��@~��@~��@~��@~��@~�+@~v�@~ff@~ff@~V@~E�@~$�@}�T@}�@|��@|��@|�D@|z�@|9X@{@z�!@z�\@z~�@z~�@zM�@y�@y��@y�7@yG�@y7L@y&�@x��@xbN@x �@w�P@wl�@w;d@w
=@v��@v@u`B@tz�@t1@s�F@s"�@r��@r-@r�@r�@rJ@q��@q��@qX@pĜ@p��@p��@p�9@pbN@pbN@pQ�@o�;@o+@n�R@n��@n�+@nȴ@n�+@m��@l�@k33@j��@jM�@i�@i��@i�7@i�7@iG�@i7L@h��@hĜ@h�u@hr�@h1'@h  @g�;@g��@g�w@g|�@g\)@g\)@g�@g
=@g
=@f�@fE�@e�T@e�h@e�@d��@d�@d��@dj@d�@c�
@c��@ct�@cC�@c33@b~�@a�#@aX@a&�@a�@`��@`�u@_��@_�@^��@]@]p�@\��@\9X@[��@[��@[C�@Zn�@Y�^@Y�7@Y7L@X��@X�@XQ�@X  @W��@W;d@V�@V$�@U�h@U?}@T�/@T��@T��@TZ@S�F@SS�@R��@Q��@Q�#@Q�^@Q��@Qx�@Q7L@PQ�@O|�@Ol�@O
=@N�y@N��@N�y@N��@N��@NE�@N@M��@M�h@M/@L�j@K��@K��@KdZ@K33@K@J�!@J�\@J~�@J~�@Jn�@JM�@I��@I�7@IX@I7L@I�@I�@I%@I%@H��@H�9@H�u@H1'@F�+@D��@D9X@D9X@D9X@D(�@D�@D1@C�m@C�
@C��@Ct�@C"�@C@B�H@B��@B^5@B�@A�#@A��@A7L@A�@A�@@�`@@Ĝ@@��@@r�@@b@?��@?|�@?K�@?�@>��@>{@=�@=��@=@=�h@=p�@=O�@<��@<I�@;�
@;�@;33@;@:��@:�@9hs@8��@8�u@8Q�@81'@8  @8  @8  @7��@7|�@7;d@7
=@6�@6ȴ@7\)@7
=@6�+@6ff@6$�@5�T@5��@5V@4�@4(�@3dZ@2�H@2��@2M�@1�^@1�7@1X@1�@0Ĝ@0Ĝ@0��@0 �@0  @0 �@01'@0  @/�P@/K�@/+@/+@.�@.V@.{@.@-�T@-p�@-/@-�@,�@,�j@,�D@+�m@+�m@+ƨ@+��@+dZ@*�\@)��@)�7@)��@)hs@)G�@)�@(Ĝ@(Q�@'�;@'��@'\)@'+@&�@&ȴ@&ff@&{@%/@$�j@$�@$��@$z�@$I�@$9X@$(�@$(�@$9X@#�m@#ƨ@#�
@#�F@#��@#t�@#t�@#33@"��@"~�@"^5@"-@"J@!��@!X@ ��@ ��@ Q�@�;@K�@�@�@ȴ@��@�+@ff@V@$�@{@@��@`B@�@��@��@I�@(�@�m@�
@�@33@�H@��@�!@��@M�@�@G�@%@��@Ĝ@bN@A�@  @��@;d@
=@�@�R@v�@ff@{@@�@�@V@�/@��@I�@(�@��@��@��@dZ@S�@o@�H@��@n�@-@�#@x�@&�@&�@��@r�@A�@b@�@�;@�P@�@��@V@5?@$�@{@@@�@�T@��@��@�-@�h@/@j@(�@�@�@��@ƨ@��@33@@
�!@
�\@
�\@
�\@
�@	��@	��@	X@	�@	�@��@�u@�@r�@r�@Q�@ �@�;@�w@�P@l�@
=@��@v�@ff@V@5?@$�@{@@��@��@��@��@��@�@p�@`B@��@��@Z@9X@(�@�@�m@�
@��@��@��@�@t�@dZ@dZ@"�@o@�@��@~�@n�@M�@-@�@��@�@�#@�^@�^@��@��@hs@G�@�@ �`@ �`1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�ZA�\)A�^5A�^5A�ZA�\)A�^5A�bNA�dZA�dZA�ffA�hsA�ffA�dZA�dZA�ffA�n�A�p�A�p�A�r�A�r�A�r�A�r�A�v�A�v�A�t�A�t�A�v�A�v�A�t�A�t�A�v�A�x�A�z�A�v�A�v�A�z�A�x�A�v�A�Q�A�?}A��A���A�^5A��A�JA���A���A�ȴA�+A��A��TA�bA��DA��A��DA��^A��9A�%A�~�A���A���A��A���A�G�A�p�A���A�A�A��A�t�A��A���A�5?A�A��\A�~�A��#A��!A�\)A��TA��A�Q�A��;A�`BA��A�Q�A�
A}l�Az�/Ay��Ax��AwXAt�HArM�Ao��Ao/An �Al�9Aj��AiƨAiXAh�HAh�!Ah��AhbNAg��AgXAgoAf�9AfbNAf9XAe�mAe?}Ad~�AbĜAa�^A`�A_�A_�A_�
A_7LA^$�A]dZA\ĜA[x�AYAV��AU+AT�jATz�AT1'AS\)ARz�AQ��AP��AOAL��AK?}AIƨAI�AG|�AFJAEADM�AC��AC+AAhsA@bNA?��A?
=A>�A>ĜA>~�A>�A=;dA;O�A:�DA:E�A9ƨA9XA8��A8~�A8�+A8~�A8n�A7x�A5�PA4ZA41'A3��A2��A2{A1�^A1XA0��A0�jA/�-A.�A-;dA-"�A-"�A-"�A-"�A-�A-
=A,��A,�`A,Q�A*9XA)/A)
=A(��A'�
A'�A&Q�A%A%G�A$�A$�9A$n�A#O�A"E�A"�A!�PA ��A n�A (�AK�A��A�A&�A5?AhsA��Az�A1'A��A�Ap�A  A
=A�
A��Av�AA��A�!A+A9XA�AC�A
��A
v�A
1'A	�A�+A�AbNA�-A��A��AXA�DA��AhsAA ��A ��A I�A �@�l�@���@�?}@�ƨ@�@�r�@��;@���@�"�@���@�hs@� �@�n�@��@�j@�n�@�5?@�V@�33@�Q�@�ȴ@�v�@��@���@�j@�\)@�`B@ޏ\@��#@�/@�bN@���@���@�dZ@�`B@ԋD@�K�@��`@��;@�|�@�+@Η�@�Ĝ@�v�@���@�hs@ȣ�@�z�@��;@���@�V@�x�@ļj@�
=@�=q@�V@���@��m@�@�r�@��P@�"�@�J@��@��;@�dZ@���@�5?@���@�V@��@�Ĝ@�A�@���@�+@�x�@��@�9X@�ƨ@�dZ@�33@�J@�7L@��@�$�@��@��-@�?}@�b@�;d@�V@��H@�\)@��@�V@�n�@��h@�\)@�33@���@��@��@�ƨ@��@��D@���@�|�@��@�@���@��u@�bN@���@���@��;@��
@���@��F@�C�@��@��R@���@�V@��#@�`B@�?}@�`B@���@�j@�I�@�I�@� �@���@�;d@���@�^5@�$�@��T@�%@�bN@�(�@� �@�1@���@��m@���@�ƨ@�\)@�@���@�J@��@��-@�`B@�G�@��@���@�1'@�S�@�{@��T@��^@�p�@�&�@�%@��/@�z�@�b@��P@��@�o@���@�5?@��@�$�@�E�@�v�@���@�j@�b@��@��P@�l�@�@��y@��H@��@���@�^5@�-@��T@��7@���@�z�@�r�@�bN@�bN@�bN@�Q�@��@�;@��@\)@
=@~�R@~�+@~V@~E�@~E�@~V@~ff@~v�@~v�@~�+@~�+@~��@~��@~��@~��@~��@~�+@~v�@~ff@~ff@~V@~E�@~$�@}�T@}�@|��@|��@|�D@|z�@|9X@{@z�!@z�\@z~�@z~�@zM�@y�@y��@y�7@yG�@y7L@y&�@x��@xbN@x �@w�P@wl�@w;d@w
=@v��@v@u`B@tz�@t1@s�F@s"�@r��@r-@r�@r�@rJ@q��@q��@qX@pĜ@p��@p��@p�9@pbN@pbN@pQ�@o�;@o+@n�R@n��@n�+@nȴ@n�+@m��@l�@k33@j��@jM�@i�@i��@i�7@i�7@iG�@i7L@h��@hĜ@h�u@hr�@h1'@h  @g�;@g��@g�w@g|�@g\)@g\)@g�@g
=@g
=@f�@fE�@e�T@e�h@e�@d��@d�@d��@dj@d�@c�
@c��@ct�@cC�@c33@b~�@a�#@aX@a&�@a�@`��@`�u@_��@_�@^��@]@]p�@\��@\9X@[��@[��@[C�@Zn�@Y�^@Y�7@Y7L@X��@X�@XQ�@X  @W��@W;d@V�@V$�@U�h@U?}@T�/@T��@T��@TZ@S�F@SS�@R��@Q��@Q�#@Q�^@Q��@Qx�@Q7L@PQ�@O|�@Ol�@O
=@N�y@N��@N�y@N��@N��@NE�@N@M��@M�h@M/@L�j@K��@K��@KdZ@K33@K@J�!@J�\@J~�@J~�@Jn�@JM�@I��@I�7@IX@I7L@I�@I�@I%@I%@H��@H�9@H�u@H1'@F�+@D��@D9X@D9X@D9X@D(�@D�@D1@C�m@C�
@C��@Ct�@C"�@C@B�H@B��@B^5@B�@A�#@A��@A7L@A�@A�@@�`@@Ĝ@@��@@r�@@b@?��@?|�@?K�@?�@>��@>{@=�@=��@=@=�h@=p�@=O�@<��@<I�@;�
@;�@;33@;@:��@:�@9hs@8��@8�u@8Q�@81'@8  @8  @8  @7��@7|�@7;d@7
=@6�@6ȴ@7\)@7
=@6�+@6ff@6$�@5�T@5��@5V@4�@4(�@3dZ@2�H@2��@2M�@1�^@1�7@1X@1�@0Ĝ@0Ĝ@0��@0 �@0  @0 �@01'@0  @/�P@/K�@/+@/+@.�@.V@.{@.@-�T@-p�@-/@-�@,�@,�j@,�D@+�m@+�m@+ƨ@+��@+dZ@*�\@)��@)�7@)��@)hs@)G�@)�@(Ĝ@(Q�@'�;@'��@'\)@'+@&�@&ȴ@&ff@&{@%/@$�j@$�@$��@$z�@$I�@$9X@$(�@$(�@$9X@#�m@#ƨ@#�
@#�F@#��@#t�@#t�@#33@"��@"~�@"^5@"-@"J@!��@!X@ ��@ ��@ Q�@�;@K�@�@�@ȴ@��@�+@ff@V@$�@{@@��@`B@�@��@��@I�@(�@�m@�
@�@33@�H@��@�!@��@M�@�@G�@%@��@Ĝ@bN@A�@  @��@;d@
=@�@�R@v�@ff@{@@�@�@V@�/@��@I�@(�@��@��@��@dZ@S�@o@�H@��@n�@-@�#@x�@&�@&�@��@r�@A�@b@�@�;@�P@�@��@V@5?@$�@{@@@�@�T@��@��@�-@�h@/@j@(�@�@�@��@ƨ@��@33@@
�!@
�\@
�\@
�\@
�@	��@	��@	X@	�@	�@��@�u@�@r�@r�@Q�@ �@�;@�w@�P@l�@
=@��@v�@ff@V@5?@$�@{@@��@��@��@��@��@�@p�@`B@��@��@Z@9X@(�@�@�m@�
@��@��@��@�@t�@dZ@dZ@"�@o@�@��@~�@n�@M�@-@�@��@�@�#@�^@�^@��@��@hs@G�@�@ �`@ �`1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 BR�BR�BR�BR�BR�BS�BR�BR�BR�BR�BR�BR�BR�BR�BR�BR�BR�BR�BR�BR�BR�BR�BR�BQ�BR�BR�BR�BR�BS�BS�BS�BS�BS�BS�BS�BS�BS�BS�BS�BR�BQ�BO�BJ�B:^BhB�B�NB��B�9B�B��B~�BffBF�B'�BB�B�;B�B��BɺB�jB��B�oB�Bo�BdZB_;B\)BW
BP�BK�BF�B6FB#�B%B
��B
�B
�sB
�HB
�B
��B
ŢB
�qB
�?B
�B
��B
�hB
}�B
p�B
gmB
\)B
O�B
@�B
2-B
-B
&�B
�B
hB
+B
B
  B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�fB	�NB	�B	��B	ɺB	ĜB	ÖB	ÖB	��B	�XB	�9B	�'B	��B	��B	�DB	}�B	{�B	x�B	v�B	s�B	m�B	iyB	dZB	^5B	Q�B	L�B	C�B	?}B	7LB	.B	'�B	"�B	�B	�B	�B	�B	bB	PB	PB	JB	DB	
=B	%B	B��B��B��B��B��B��B��B�B�B�B�yB�NB�HB�;B�5B�B�B�
B��B��B��B��BƨBŢBŢBŢBŢBĜBĜBĜBÖB�}B�jB�?B�9B�3B�-B�B�B��B��B��B��B��B��B��B��B��B��B��B�{B�uB�\B�VB�JB�7B�B�B�B�B� B|�Bx�Bs�Bq�Bm�BjBhsBffBe`BdZB^5B[#BZBVBR�BR�BP�BP�BM�BK�BI�BG�BD�BB�B?}B?}B>wB=qB=qB<jB<jB;dB;dB;dB:^B:^B9XB8RB8RB7LB7LB7LB6FB7LB6FB7LB5?B49B2-B0!B/B.B.B/B/B0!B/B/B/B/B/B/B/B/B.B-B.B-B-B-B0!B1'B0!B0!B0!B5?B7LB5?B8RB8RB=qBC�BC�BC�BC�BD�BF�BJ�BN�BO�BQ�BR�BR�BR�BR�BW
BVBT�BVBW
BXBYB[#B\)B\)B_;B_;BbNBgmBhsBjBk�Bk�Bk�Bn�Bo�Bn�BjBiyBiyBhsBffBdZBffBn�Bz�By�B� B�B~�Bw�By�B|�B{�B{�Bz�B�B�1B�%B�1B�JB�hB�oB�oB��B��B�bB�bB�bB�bB�hB��B��B��B��B��B��B��B��B�B�B�'B�3B�3B�9B�XB�dB�wBBBĜB��B��B��B��B��B��B��B��B��B��B��B��B�B�5B�HB�NB�TB�TB�TB�ZB�fB�B�B�B�B�B�B�B��B��B��B	  B	B	B	B	B	1B	
=B	PB	\B	bB	{B	�B	�B	�B	�B	 �B	!�B	!�B	$�B	)�B	.B	49B	7LB	;dB	>wB	@�B	B�B	B�B	B�B	C�B	D�B	F�B	I�B	L�B	O�B	Q�B	T�B	VB	W
B	XB	ZB	[#B	\)B	^5B	aHB	ffB	jB	m�B	n�B	n�B	p�B	r�B	t�B	t�B	t�B	u�B	u�B	v�B	x�B	{�B	|�B	~�B	~�B	~�B	�B	�B	�+B	�1B	�1B	�7B	�=B	�PB	�VB	�bB	�oB	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�'B	�3B	�3B	�3B	�3B	�RB	�^B	�jB	��B	��B	��B	��B	��B	��B	B	ÖB	ÖB	ĜB	ĜB	ĜB	ƨB	ƨB	ǮB	ȴB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�#B	�)B	�)B	�)B	�/B	�/B	�/B	�/B	�/B	�/B	�5B	�5B	�5B	�5B	�;B	�BB	�HB	�HB	�NB	�NB	�TB	�NB	�ZB	�fB	�sB	�sB	�sB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
+B
+B
+B
+B
1B
1B

=B
DB
DB
JB
JB
JB
JB
PB
PB
PB
VB
VB
VB
\B
\B
hB
hB
oB
oB
uB
uB
uB
uB
{B
{B
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
 �B
!�B
!�B
!�B
"�B
"�B
"�B
"�B
#�B
#�B
#�B
#�B
$�B
$�B
%�B
%�B
%�B
&�B
'�B
'�B
'�B
'�B
'�B
'�B
'�B
(�B
(�B
)�B
)�B
)�B
+B
+B
+B
,B
,B
,B
,B
,B
-B
-B
-B
-B
-B
-B
,B
-B
.B
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
33B
33B
49B
49B
49B
49B
49B
6FB
7LB
7LB
8RB
9XB
:^B
:^B
;dB
<jB
<jB
<jB
<jB
<jB
=qB
=qB
=qB
>wB
?}B
?}B
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
A�B
B�B
B�B
A�B
A�B
A�B
A�B
A�B
A�B
A�B
A�B
A�B
A�B
@�B
@�B
@�B
?}B
?}B
?}B
@�B
@�B
A�B
A�B
A�B
B�B
C�B
C�B
D�B
D�B
D�B
D�B
D�B
E�B
F�B
F�B
F�B
F�B
E�B
E�B
E�B
E�B
E�B
E�B
E�B
E�B
E�B
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
G�B
G�B
H�B
H�B
I�B
I�B
J�B
K�B
L�B
L�B
L�B
L�B
M�B
M�B
M�B
M�B
N�B
N�B
N�B
O�B
O�B
O�B
O�B
P�B
Q�B
Q�B
Q�B
Q�B
R�B
R�B
R�B
S�B
S�B
S�B
S�B
T�B
T�B
VB
VB
VB
VB
VB
VB
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
[#B
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
]/B
]/B
]/B
]/B
]/B
^5B
^5B
_;B
^5B
^5B
_;B
_;B
_;B
_;B
`BB
`BB
`BB
`BB
aHB
aHB
bNB
bNB
bNB
bNB
dZB
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
ffB
ffB
ffB
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
jB
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
l�B
l�B
l�B
m�B
m�B
m�B
m�B
m�B
m�B
m�B
n�B
n�B
n�B
n�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 BS&BS&BS&BS&BSBT,BS&BSBSBS&BS&BSBSBS&BSBS&BS&BS&BS&BS&BS&BSBS&BRBS&BSBS&BS&BT,BT,BT,BT,BT,BT,BT,BTBTFBT,BTaBS�BR�BRBO�BC-B�B�`B��B�#B��B�;B�WB��BmBN�B/OBzB��B�-BרB��B�B��B�sB�B��Bq�Be�B`\B]dBX_BRBM�BI�B:*B($BfB OB
��B
��B
�nB
רB
�B
�B
��B
��B
��B
��B
�FB
�B
r|B
i�B
_pB
S&B
CGB
3�B
.�B
(�B
�B
�B
�B
�B
 �B	�]B	��B	��B	��B	�ZB	�hB	�GB	�/B	�]B	�B	��B	�tB	ٚB	�oB	ʌB	�B	�3B	ĶB	��B	��B	��B	��B	�B	�jB	�B	~�B	|�B	y�B	xB	u%B	oB	kQB	f�B	aB	S�B	N�B	EB	A�B	9>B	/�B	)B	#�B	!-B	�B	B	�B	NB	�B	�B	B	JB	�B	fB	-B��B��B��B��B�fB�B�%B�B�nB��B�B�B�hB�vB�;B��B��B��B�BյB��B��B�B��B��B��B�B�B�B�SB�B�B��B��B��B��B�hB�cB�B��B��B��B��B�tB�B�dB��B��B�yB�mB��B��B��B��B��B�rB�B�B��B�B��BHBz�BuZBshBn�Bk�Bi_Bg�BgBf�B_�B]/B\)BW
BS�BS�BR:BR�BO�BM�BKBIBF%BC�B@�B@�B?HB>]B>B=B="B<B<PB<�B;�B;�B:�B9rB9	B7�B8B88B7fB8�B7�B8lB6FB5�B2�B1vB0�B0!B/5B/�B/�B1'B0B0UB0�B1B/�B0B0;B0oB/OB.�B/�B.B.}B.�B1'B1�B0�B1AB1�B6�B8B5�B9$B9	B>BBD�BDgBD�BD�BE�BG_BKDBO�BQ�BSuBT,BS�BS�BT,BXyBV�BU�BV�BW�BX�BY�B[�B\�B\�B`B`'Bc�Bh>Bh�BkBl"Bl"Bl�Bo�Bq'BoiBkBi�Bj0Bi�BgRBd�Bf�Bn�B{�Bz*B�iB�B�iBx�Bz�B}�B|�B|�Bz�B��B��B��B�B�jB�TB�B�B��B�?B�NB��B��B��B� B�B��B�B�/B�\B�tB�`B�XB��B��B��B��B��B��B��B�B��B��B�-B�mB�^B�\B�(B�(B�(B�(B�BB�HBЗB�hB�oBԕBٚBޞB��B��B�B�B�&B�`B�RB��B��B�B�B�B�B�3B�ZB��B�<B	 �B	�B	�B	aB	mB	�B	
�B	<B	HB	�B	�B	�B	B	B	 B	!B	"B	"4B	%`B	*eB	.�B	4�B	8B	;�B	>�B	@�B	B�B	B�B	B�B	DB	EB	GB	J#B	M6B	P.B	RTB	UMB	VSB	W?B	XEB	ZkB	[qB	\xB	^jB	a�B	f�B	j�B	m�B	n�B	o B	qB	r�B	t�B	uB	uB	v+B	vB	wLB	yXB	|6B	}VB	cB	HB	}B	��B	�mB	�zB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�-B	�-B	�ZB	��B	��B	�kB	�qB	��B	��B	��B	�hB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	��B	�B	�_B	�fB	�RB	�7B	�JB	�<B	�BB	�.B	�.B	�HB	�NB	�4B	�NB	�:B	�[B	�aB	�eB	�eB	�eB	�kB	�qB	�xB	�xB	�xB	�~B	ݘB	�~B	ݘB	ݲB	ݘB	ބB	ބB	ޞB	ޞB	߾B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	�B	�B	��B	�B	�B	�5B	�B	��B	�-B	�3B	�3B	�9B	�B	�B	�?B	�LB	�8B	�$B	�XB	�^B	�JB	�jB	�wB
 iB
UB
UB
[B
uB
�B
�B
�B
�B
�B
�B
�B
zB
�B
�B

�B
�B
�B
~B
~B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
B
eB
]B
B
�B
�B
B
�B
B
!B
!B
 B
 'B
 'B
!B
!-B
!B
!B
"B
"B
"4B
# B
# B
# B
# B
$@B
$&B
$&B
$@B
%,B
%,B
&2B
&LB
&fB
'RB
(XB
(>B
(>B
(XB
(>B
(XB
(sB
)_B
)_B
*eB
*KB
*eB
+kB
+�B
+�B
,qB
,WB
,WB
,qB
,WB
-]B
-CB
-]B
-]B
-]B
-]B
,WB
-CB
./B
0oB
2�B
2|B
2|B
3�B
3�B
3�B
3�B
3�B
3�B
3�B
3�B
3�B
3�B
4�B
4�B
4�B
4�B
4nB
6�B
7�B
7�B
8�B
9�B
:�B
:�B
;�B
<�B
<�B
<�B
<�B
<�B
=�B
=�B
=�B
>�B
?�B
?�B
?�B
?�B
?�B
@�B
@�B
@�B
@�B
AB
@�B
@�B
A�B
B�B
B�B
A�B
A�B
A�B
BB
A�B
A�B
A�B
A�B
A�B
A�B
@�B
A B
@�B
?�B
?�B
?�B
@�B
@�B
A�B
A�B
A�B
B�B
C�B
C�B
D�B
EB
D�B
D�B
EB
FB
GB
GB
GB
GB
E�B
F%B
F%B
FB
FB
F%B
F%B
FB
E�B
E�B
E�B
E�B
E�B
FB
E�B
E�B
FB
GB
F�B
HB
G�B
IB
IB
J#B
J#B
KB
L0B
M6B
MB
MB
MB
N"B
N<B
N"B
NVB
O(B
OBB
O(B
P.B
P.B
P.B
PHB
Q4B
R:B
R:B
R:B
R:B
S[B
S@B
S[B
TaB
TaB
TaB
TFB
UMB
UgB
VmB
VSB
VSB
VSB
VSB
VmB
VmB
WYB
XyB
X_B
X_B
X_B
XyB
XyB
YeB
YB
YeB
YeB
YeB
ZkB
ZkB
Z�B
[�B
[�B
\xB
\�B
\]B
\xB
\]B
\xB
\xB
]dB
]~B
]dB
]~B
]~B
]�B
]�B
^�B
^�B
_pB
^�B
^�B
_�B
_�B
_�B
_�B
`vB
`�B
`�B
`�B
a�B
a�B
b�B
b�B
b�B
b�B
d�B
d�B
d�B
d�B
d�B
d�B
d�B
e�B
e�B
e�B
e�B
e�B
e�B
e�B
e�B
f�B
f�B
f�B
f�B
f�B
f�B
f�B
f�B
f�B
g�B
g�B
g�B
g�B
h�B
h�B
i�B
i�B
i�B
i�B
j�B
j�B
j�B
j�B
j�B
j�B
j�B
j�B
k�B
k�B
k�B
k�B
l�B
l�B
l�B
l�B
l�B
l�B
l�B
m�B
m�B
m�B
m�B
m�B
m�B
m�B
n�B
n�B
n�B
n�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.48(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201701020038282017010200382820170102003828201806221218572018062212185720180622121857201804050412072018040504120720180405041207  JA  ARFMdecpA19c                                                                20161229093506  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20161229003516  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20161229003517  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20161229003517  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20161229003518  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20161229003518  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20161229003518  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20161229003518  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20161229003518  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20161229003519                      G�O�G�O�G�O�                JA  ARUP                                                                        20161229010207                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20161229153139  CV  JULD            G�O�G�O�F�)�                JM  ARCAJMQC2.0                                                                 20170101153828  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20170101153828  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180404191207  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20180622031857  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200115111516                      G�O�G�O�G�O�                