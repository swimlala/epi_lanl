CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2016-06-29T00:35:24Z creation;2016-06-29T00:35:26Z conversion to V3.1;2019-12-19T08:37:00Z update;     
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
_FillValue                 �  I,   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  s�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �l   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �p   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  �T   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �t   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  �   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   ݔ   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �    HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �$   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �d   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �t   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �x   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20160629003524  20200115101517  4902363                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               A   JA  I2_0576_011                     2C  D   NAVIS_A                         0576                            ARGO 092515                     863 @׷v� 1   @׷v�r @;�l"h	��dv�1&�1   GPS     A   A   B   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @���@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B��B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  C   C  C  C  C  C	�fC�fC  C  C  C  C  C  C  C�C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;fD;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� DgfDg�fDh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� DwfDw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�|�D���D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���D���D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�<�DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�C3D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D� D�  111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@���@�33@�33A��A=��A]��A}��A���A���A���A���A���A���A���A���B  BffBffBffB'ffB/ffB7ffB?ffBGffBOffBWffB_ffBgffBoffBwffBffB��3B��3B��3B��3B��3B��3B��3B��3B��3B��3B��3B��3B��3B��3B��3B��3Bó3Bǳ3B˳3Bϳ3Bӳ3B׳3B۳3B߳3B�3B�3B�3B�3B�3B�� B��3B��3CٚCٚCٚCٚC	� C� CٚCٚCٚCٚCٚCٚCٚC�3CٚCٚC!ٚC#ٚC%ٚC'ٚC)ٚC+ٚC-ٚC/ٚC1ٚC3ٚC5ٚC7ٚC9ٚC;ٚC=ٚC?ٚCAٚCCٚCEٚCGٚCIٚCKٚCMٚCOٚCQٚCSٚCUٚCWٚCYٚC[ٚC]ٚC_ٚCaٚCcٚCeٚCgٚCiٚCkٚCmٚCoٚCqٚCsٚCuٚCwٚCyٚC{ٚC}ٚCٚC���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D vfD �fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fD	vfD	�fD
vfD
�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fD vfD �fD!vfD!�fD"vfD"�fD#vfD#�fD$vfD$�fD%vfD%�fD&vfD&�fD'vfD'�fD(vfD(�fD)vfD)�fD*vfD*�fD+vfD+�fD,vfD,�fD-vfD-�fD.vfD.�fD/vfD/�fD0vfD0�fD1vfD1�fD2vfD2�fD3vfD3�fD4vfD4�fD5vfD5�fD6vfD6�fD7vfD7�fD8vfD8�fD9vfD9�fD:vfD:��D;vfD;�fD<vfD<�fD=vfD=�fD>vfD>�fD?vfD?�fD@vfD@�fDAvfDA�fDBvfDB�fDCvfDC�fDDvfDD�fDEvfDE�fDFvfDF�fDGvfDG�fDHvfDH�fDIvfDI�fDJvfDJ�fDKvfDK�fDLvfDL�fDMvfDM�fDNvfDN�fDOvfDO�fDPvfDP�fDQvfDQ�fDRvfDR�fDSvfDS�fDTvfDT�fDUvfDU�fDVvfDV�fDWvfDW�fDXvfDX�fDYvfDY�fDZvfDZ�fD[vfD[�fD\vfD\�fD]vfD]�fD^vfD^�fD_vfD_�fD`vfD`�fDavfDa�fDbvfDb�fDcvfDc�fDdvfDd�fDevfDe�fDfvfDf��Dg|�Dg�fDhvfDh�fDivfDi�fDjvfDj�fDkvfDk�fDlvfDl�fDmvfDm�fDnvfDn�fDovfDo�fDpvfDp�fDqvfDq�fDrvfDr�fDsvfDs�fDtvfDt�fDuvfDu�fDvvfDv��DwvfDw�fDxvfDx�fDyvfDy�fDzvfDz�fD{vfD{�fD|vfD|�fD}vfD}�fD~vfD~�fDvfD�fD�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�x D�� D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�>fD�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D�� D�� D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D»3D��3D�;3D�{3Dû3D��3D�;3D�{3DĻ3D��3D�;3D�{3DŻ3D��3D�;3D�{3Dƻ3D��3D�;3D�{3Dǻ3D��3D�;3D�{3DȻ3D��3D�;3D�{3Dɻ3D��3D�;3D�{3Dʻ3D��3D�;3D�{3D˻3D��3D�;3D�{3D̻3D��3D�;3D�{3Dͻ3D��3D�;3D�{3Dλ3D��3D�;3D�{3Dϻ3D��3D�;3D�{3Dл3D��3D�;3D�{3Dѻ3D��3D�;3D�{3Dһ3D��3D�;3D�{3Dӻ3D��3D�;3D�{3DԻ3D��3D�8 D�{3Dջ3D��3D�;3D�{3Dֻ3D��3D�;3D�{3D׻3D��3D�;3D�{3Dػ3D��3D�;3D�{3Dٻ3D��3D�;3D�{3Dڻ3D��3D�;3D�{3Dۻ3D��3D�;3D�{3Dܻ3D��3D�;3D�{3Dݻ3D��3D�;3D�{3D޻3D��3D�;3D�{3D߻3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�{3D��3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�>fD�{3D�3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D�3D�3111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111Aǝ�Aǝ�Aǝ�Aǝ�Aǝ�Aǝ�Aǝ�Aǟ�Aǝ�Aǟ�Aǟ�Aǡ�Aǡ�Aǡ�AǙ�Aơ�A��A�G�A�dZA�O�A���A�`BA��A���A���A�ffA�5?A���A��A��A���A��\A��TA�=qA��A��RA���A��A���A��!A�\)A���A��;A�C�A��-A��;A��A�ƨA��A���A���A�A�"�A�I�A��TA�`BA�"�A�S�A���A���A��A�5?A�G�A��A�l�A�bA��A��9A�x�A�=qA��A��jA�bNA��A�M�A�1A�VA��yA�x�A���A�l�A�E�A�  A���A�jA�&�A���A�A�x�A�  A��A~E�A{t�Azv�Ay��Ay�Ax~�Aw�wAt��Ar�`Aq�Ap9XAo�An1Ak��Ah��Ag/Af��AfVAe\)Ad$�Aa�PA`�+A_A^��A^�jA]�;A\�jA[�AY"�AXffAXE�AW��AW��AWG�AVn�AU�AS�ARn�AP��AP �AOoAM�
AM&�AL��ALA�AK`BAKC�AKC�AI��AH��AH�RAHAG"�AF�AE�mAE�AES�AD�DAC�mAC"�AA;dA@jA@5?A?�wA>v�A<��A;��A;"�A:jA9��A9S�A9�A8��A7�A6��A6�A5��A5`BA4�/A3��A2�A21A1l�A/��A.�HA.n�A-�;A-t�A,�A+l�A*bA'
=A&M�A%��A%x�A%S�A%G�A%;dA%/A%�A$�A#�A#�A!��A ��A JA;dAffA9XA��A��AhsAO�A�!A��A�TAK�A�A�A�A�AVA{AC�A%AQ�A�A�DA�A%A
 �A	�A�RA�wAA^5Ap�A?}A�RA5?Ap�A+A �yA ff@��m@�o@�p�@���@��@���@��h@�O�@��j@�@�E�@��@�@�
=@���@�K�@��@�@�(�@�l�@���@�P@�$�@�7L@���@ߥ�@�$�@ܓu@� �@�
=@�&�@�I�@֟�@�$�@�{@�{@���@�&�@ԃ@ӥ�@�K�@���@ѡ�@�K�@��@�{@��@̛�@��@�t�@�@ʧ�@ɺ^@���@�A�@ǶF@ũ�@�V@ģ�@�  @�"�@\@��7@�b@���@�ff@�G�@��9@�t�@���@�x�@��/@��P@���@���@���@�b@���@��@��@��@���@�bN@���@�ƨ@��@�33@�"�@���@�@���@�A�@��F@�"�@��-@��/@���@�33@��@��\@�V@�$�@��#@���@��@�&�@�r�@�I�@��@��P@�ȴ@�v�@�=q@��^@�x�@�`B@�7L@���@���@�j@���@�dZ@�C�@�33@��@���@�-@�hs@�V@�Ĝ@�9X@�
=@�$�@���@���@�O�@��/@���@�j@��m@�l�@�o@��y@���@�@��7@��@��@�j@�1'@���@��P@�o@���@�5?@���@�X@��@�9X@�1'@�(�@��@��m@�|�@�K�@�"�@�o@��!@��^@���@��;@��@���@��@���@�V@�@�hs@�G�@���@��@��/@��@��@�1@�t�@�33@���@�~�@��^@�7L@�Ĝ@���@�j@�Q�@�A�@� �@�b@��;@��P@�l�@�S�@�K�@�+@���@��!@�{@��T@��^@��@��`@�(�@�w@~�y@}@|z�@|9X@|1@{ƨ@{S�@{33@{"�@{o@z�H@z�\@y�#@y�@y�@y�#@yhs@y%@xr�@x1'@w�@w|�@wK�@w+@w�@v��@vȴ@v��@v�+@vff@u�@u�@t��@t�/@t��@s�
@sC�@r�H@r��@r�!@r�!@r~�@r^5@r-@q��@q7L@p�u@p �@o��@oK�@n��@n�@n�R@n��@n��@n��@n�+@nE�@m�@m�T@m�-@m�@m?}@l�@l�j@l��@lz�@lz�@lz�@l�D@l�D@l�D@l�D@l�D@lz�@l�@k�m@kƨ@k�@k33@j�\@j�@i�@i��@ix�@iX@i&�@h�`@hQ�@g�;@g|�@gK�@g�@f�y@fff@e��@e��@e�@ep�@e`B@eO�@e?}@eV@dz�@c�m@cdZ@c@bn�@a��@a��@a�7@aG�@aG�@ahs@a�7@a�7@ax�@a�7@a�7@a��@aX@a7L@`�9@`�@`1'@`bN@_�;@_��@_��@_��@_��@`  @`b@_��@^�y@^�@^�+@^v�@^E�@^$�@^@]@]��@]��@]�-@]`B@\��@\Z@\(�@[��@[�F@[33@Z��@Z��@Z~�@Z-@Z-@Z=q@Y��@Y�@Y�7@Y7L@X��@X��@X�9@X1'@W�w@Wl�@W\)@V��@Vȴ@Vȴ@V�+@V$�@U�T@U�h@U?}@U�@T�@T(�@SdZ@R�@R�\@R~�@Q�@Qx�@Q�@PĜ@P�@O�;@O|�@O;d@O�@N��@N�y@N�R@Nv�@M��@M`B@MO�@MO�@MO�@M�@L��@Lz�@K��@K�
@Kt�@K"�@J��@J-@JJ@I�#@I�7@Ihs@IX@H�`@H�u@H�@HbN@HQ�@HA�@H  @G��@G��@G\)@G
=@F��@F��@Fv�@FV@E�T@EO�@D�@D�j@D��@DZ@D(�@C�m@C�F@C��@C��@CdZ@CS�@CS�@CS�@CC�@C@B��@B��@B~�@BM�@BJ@Ax�@A7L@A%@@��@@�`@@��@@Ĝ@@�u@@r�@@A�@?��@>�y@>��@>v�@>@=p�@<��@<�@<�@<�j@<�@<��@<�D@<Z@<1@;��@;�
@;��@;o@:��@:�!@:^5@:=q@:J@9�7@9x�@97L@8��@8b@7�;@7�@7|�@7l�@7\)@7\)@7\)@7\)@7�@6v�@65?@6$�@6{@5�@5�T@5�T@5�T@5p�@5�@4�@4I�@49X@4(�@41@3ƨ@333@3o@2�@2�\@2M�@2J@1�@1��@17L@0�`@0�u@0�@0 �@/��@/|�@/+@.��@.��@.ff@.E�@.$�@.@-�T@-@-�@-/@,��@,j@,j@,Z@,Z@,Z@,Z@,I�@,�@+�F@+��@+S�@*�@*��@*=q@*�@)�^@)�7@)X@)7L@(�u@'��@'|�@'K�@'
=@'
=@&��@&�@&�+@&v�@&V@%��@%�-@%p�@%O�@%/@%V@%V@%V@$��@$��@%V@%V@$�/@$�j@$�j@$�@$��@$j@$�@#�m@#��@#C�@"�!@"^5@"=q@"�@!�#@!�^@!��@!x�@!X@!G�@!&�@ ��@ ��@ b@�;@��@��@�P@l�@;d@��@��@��@��@�+@ff@{@�-@��@�h@�@�@p�@�@��@�j@�j@�@j@9X@�@�
@��@t�@t�@C�@�@�!@�\@M�@��@�#@�#@��@7L@Ĝ@�u@bN@Q�@Q�@A�@A�@1'@b@��@��@�w@�@l�@
=@�@�@ȴ@��@5?@��@O�@/@V@�@��@��@�@��@ƨ@ƨ@�F@�F@�F@�F@�F@��@dZ@"�@��@�\@n�@=q@J@�#@��@X@7L@%@Ĝ@�u@r�@ �@�@�;@�;@�w@|�@+@�@�y@��@��@E�@��@�@?}@�@V@�@��@j@1@��@�m@ƨ@��@�@t�@dZ@S�@"�@
�@
��@
��@
��@
n�@
=q@
�@	��@	�7@	hs@	hs@	X@	7L@��@�`111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111Aǝ�Aǝ�Aǝ�Aǝ�Aǝ�Aǝ�Aǝ�Aǟ�Aǝ�Aǟ�Aǟ�Aǡ�Aǡ�Aǡ�AǙ�Aơ�A��A�G�A�dZA�O�A���A�`BA��A���A���A�ffA�5?A���A��A��A���A��\A��TA�=qA��A��RA���A��A���A��!A�\)A���A��;A�C�A��-A��;A��A�ƨA��A���A���A�A�"�A�I�A��TA�`BA�"�A�S�A���A���A��A�5?A�G�A��A�l�A�bA��A��9A�x�A�=qA��A��jA�bNA��A�M�A�1A�VA��yA�x�A���A�l�A�E�A�  A���A�jA�&�A���A�A�x�A�  A��A~E�A{t�Azv�Ay��Ay�Ax~�Aw�wAt��Ar�`Aq�Ap9XAo�An1Ak��Ah��Ag/Af��AfVAe\)Ad$�Aa�PA`�+A_A^��A^�jA]�;A\�jA[�AY"�AXffAXE�AW��AW��AWG�AVn�AU�AS�ARn�AP��AP �AOoAM�
AM&�AL��ALA�AK`BAKC�AKC�AI��AH��AH�RAHAG"�AF�AE�mAE�AES�AD�DAC�mAC"�AA;dA@jA@5?A?�wA>v�A<��A;��A;"�A:jA9��A9S�A9�A8��A7�A6��A6�A5��A5`BA4�/A3��A2�A21A1l�A/��A.�HA.n�A-�;A-t�A,�A+l�A*bA'
=A&M�A%��A%x�A%S�A%G�A%;dA%/A%�A$�A#�A#�A!��A ��A JA;dAffA9XA��A��AhsAO�A�!A��A�TAK�A�A�A�A�AVA{AC�A%AQ�A�A�DA�A%A
 �A	�A�RA�wAA^5Ap�A?}A�RA5?Ap�A+A �yA ff@��m@�o@�p�@���@��@���@��h@�O�@��j@�@�E�@��@�@�
=@���@�K�@��@�@�(�@�l�@���@�P@�$�@�7L@���@ߥ�@�$�@ܓu@� �@�
=@�&�@�I�@֟�@�$�@�{@�{@���@�&�@ԃ@ӥ�@�K�@���@ѡ�@�K�@��@�{@��@̛�@��@�t�@�@ʧ�@ɺ^@���@�A�@ǶF@ũ�@�V@ģ�@�  @�"�@\@��7@�b@���@�ff@�G�@��9@�t�@���@�x�@��/@��P@���@���@���@�b@���@��@��@��@���@�bN@���@�ƨ@��@�33@�"�@���@�@���@�A�@��F@�"�@��-@��/@���@�33@��@��\@�V@�$�@��#@���@��@�&�@�r�@�I�@��@��P@�ȴ@�v�@�=q@��^@�x�@�`B@�7L@���@���@�j@���@�dZ@�C�@�33@��@���@�-@�hs@�V@�Ĝ@�9X@�
=@�$�@���@���@�O�@��/@���@�j@��m@�l�@�o@��y@���@�@��7@��@��@�j@�1'@���@��P@�o@���@�5?@���@�X@��@�9X@�1'@�(�@��@��m@�|�@�K�@�"�@�o@��!@��^@���@��;@��@���@��@���@�V@�@�hs@�G�@���@��@��/@��@��@�1@�t�@�33@���@�~�@��^@�7L@�Ĝ@���@�j@�Q�@�A�@� �@�b@��;@��P@�l�@�S�@�K�@�+@���@��!@�{@��T@��^@��@��`@�(�@�w@~�y@}@|z�@|9X@|1@{ƨ@{S�@{33@{"�@{o@z�H@z�\@y�#@y�@y�@y�#@yhs@y%@xr�@x1'@w�@w|�@wK�@w+@w�@v��@vȴ@v��@v�+@vff@u�@u�@t��@t�/@t��@s�
@sC�@r�H@r��@r�!@r�!@r~�@r^5@r-@q��@q7L@p�u@p �@o��@oK�@n��@n�@n�R@n��@n��@n��@n�+@nE�@m�@m�T@m�-@m�@m?}@l�@l�j@l��@lz�@lz�@lz�@l�D@l�D@l�D@l�D@l�D@lz�@l�@k�m@kƨ@k�@k33@j�\@j�@i�@i��@ix�@iX@i&�@h�`@hQ�@g�;@g|�@gK�@g�@f�y@fff@e��@e��@e�@ep�@e`B@eO�@e?}@eV@dz�@c�m@cdZ@c@bn�@a��@a��@a�7@aG�@aG�@ahs@a�7@a�7@ax�@a�7@a�7@a��@aX@a7L@`�9@`�@`1'@`bN@_�;@_��@_��@_��@_��@`  @`b@_��@^�y@^�@^�+@^v�@^E�@^$�@^@]@]��@]��@]�-@]`B@\��@\Z@\(�@[��@[�F@[33@Z��@Z��@Z~�@Z-@Z-@Z=q@Y��@Y�@Y�7@Y7L@X��@X��@X�9@X1'@W�w@Wl�@W\)@V��@Vȴ@Vȴ@V�+@V$�@U�T@U�h@U?}@U�@T�@T(�@SdZ@R�@R�\@R~�@Q�@Qx�@Q�@PĜ@P�@O�;@O|�@O;d@O�@N��@N�y@N�R@Nv�@M��@M`B@MO�@MO�@MO�@M�@L��@Lz�@K��@K�
@Kt�@K"�@J��@J-@JJ@I�#@I�7@Ihs@IX@H�`@H�u@H�@HbN@HQ�@HA�@H  @G��@G��@G\)@G
=@F��@F��@Fv�@FV@E�T@EO�@D�@D�j@D��@DZ@D(�@C�m@C�F@C��@C��@CdZ@CS�@CS�@CS�@CC�@C@B��@B��@B~�@BM�@BJ@Ax�@A7L@A%@@��@@�`@@��@@Ĝ@@�u@@r�@@A�@?��@>�y@>��@>v�@>@=p�@<��@<�@<�@<�j@<�@<��@<�D@<Z@<1@;��@;�
@;��@;o@:��@:�!@:^5@:=q@:J@9�7@9x�@97L@8��@8b@7�;@7�@7|�@7l�@7\)@7\)@7\)@7\)@7�@6v�@65?@6$�@6{@5�@5�T@5�T@5�T@5p�@5�@4�@4I�@49X@4(�@41@3ƨ@333@3o@2�@2�\@2M�@2J@1�@1��@17L@0�`@0�u@0�@0 �@/��@/|�@/+@.��@.��@.ff@.E�@.$�@.@-�T@-@-�@-/@,��@,j@,j@,Z@,Z@,Z@,Z@,I�@,�@+�F@+��@+S�@*�@*��@*=q@*�@)�^@)�7@)X@)7L@(�u@'��@'|�@'K�@'
=@'
=@&��@&�@&�+@&v�@&V@%��@%�-@%p�@%O�@%/@%V@%V@%V@$��@$��@%V@%V@$�/@$�j@$�j@$�@$��@$j@$�@#�m@#��@#C�@"�!@"^5@"=q@"�@!�#@!�^@!��@!x�@!X@!G�@!&�@ ��@ ��@ b@�;@��@��@�P@l�@;d@��@��@��@��@�+@ff@{@�-@��@�h@�@�@p�@�@��@�j@�j@�@j@9X@�@�
@��@t�@t�@C�@�@�!@�\@M�@��@�#@�#@��@7L@Ĝ@�u@bN@Q�@Q�@A�@A�@1'@b@��@��@�w@�@l�@
=@�@�@ȴ@��@5?@��@O�@/@V@�@��@��@�@��@ƨ@ƨ@�F@�F@�F@�F@�F@��@dZ@"�@��@�\@n�@=q@J@�#@��@X@7L@%@Ĝ@�u@r�@ �@�@�;@�;@�w@|�@+@�@�y@��@��@E�@��@�@?}@�@V@�@��@j@1@��@�m@ƨ@��@�@t�@dZ@S�@"�@
�@
��@
��@
��@
n�@
=q@
�@	��@	�7@	hs@	hs@	X@	7L@��@�`111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111BoBoBoBoBoBoBoBoBuBuBuBuBuBuB{BF�B�mB�;BB��B�7Bm�BL�B?}B8RB-B'�B%�B�B	7B�B�B�yBŢB�wB��B�oB�DB�Bq�Bm�BiyB`BBXBO�BE�B9XB+B �BhB��B�B�B�)B��B��B�jB�B��B��B��B�Bt�B`BBT�BN�BL�BH�BE�BA�B>wB8RB2-B%�B�BuBJB	7BB
��B
�B
�B
�B
�mB
�HB
�/B
��B
��B
�}B
�B
��B
��B
{�B
n�B
ffB
`BB
_;B
]/B
I�B
8RB
(�B
�B
�B
PB	��B	�`B	�
B	��B	��B	��B	ÖB	�RB	�B	�B	��B	��B	��B	��B	�oB	�B	w�B	v�B	r�B	n�B	k�B	ffB	^5B	O�B	M�B	J�B	F�B	B�B	:^B	7LB	6FB	7LB	33B	1'B	5?B	7LB	/B	,B	)�B	$�B	)�B	+B	(�B	'�B	'�B	&�B	"�B	�B	oB	\B	bB	JB	  B��B��B��B��B��B��B�B�B�B�B�yB�`B�NB�5B�B��B��B��BɺBǮBƨBÖB��B�^B�'B��B��B��B��B�{B�{B�{B�{B�{B�uB�bB�=B�+B�B� B~�B{�BZBy�Bu�Bt�Bt�Bs�Bo�Bk�BhsBffBcTBaHBXBT�BR�BP�BP�BP�BM�BN�BI�BF�BC�BA�B>wB:^B:^B9XB7LB6FB6FB49B33B2-B2-B0!B0!B/B.B-B,B)�B)�B)�B)�B)�B+B+B+B)�B'�B&�B&�B%�B$�B$�B$�B#�B$�B#�B#�B#�B#�B#�B!�B#�B#�B$�B%�B$�B$�B#�B$�B#�B#�B!�B �B�B!�B�B�B�B�B�B�B�B�B�B�B�B�B�B"�B!�B!�B"�B#�B#�B$�B%�B&�B'�B+B,B0!B1'B2-B33B49B5?B6FB7LB9XB:^B<jB>wB?}B?}B@�BA�BA�BB�BC�BE�BG�BK�BN�BP�BP�BP�BQ�BR�BT�BVBW
BXBYBZBZB[#B[#B]/B`BBaHBcTBdZBffBgmBgmBhsBjBjBk�Bk�Bl�Bl�Bn�Bp�Bp�Bq�Br�Bt�Bv�B|�B}�B� B�B�=B�DB�JB�PB�\B�bB�hB�uB��B��B��B��B��B��B��B��B��B��B��B�B�B�!B�-B�?B�XB�^B�wB��B��B��B��B��BŢBǮBȴBȴB��B��B��B�#B�/B�NB�NB�`B�mB�B�B�B�B�B�B�B�B��B��B��B��B��B	B	%B		7B	
=B	JB	PB	PB	VB	\B	bB	uB	{B	{B	{B	�B	�B	�B	�B	�B	!�B	$�B	+B	0!B	1'B	49B	8RB	;dB	=qB	>wB	?}B	B�B	D�B	D�B	E�B	G�B	I�B	J�B	M�B	N�B	N�B	P�B	P�B	S�B	VB	XB	YB	ZB	ZB	ZB	[#B	[#B	[#B	\)B	\)B	^5B	aHB	bNB	bNB	cTB	gmB	jB	m�B	o�B	p�B	p�B	r�B	r�B	t�B	v�B	y�B	{�B	}�B	� B	�B	�B	�B	�B	�B	�B	�%B	�%B	�1B	�DB	�DB	�PB	�PB	�\B	�hB	�oB	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�3B	�?B	�LB	�XB	�dB	�dB	�dB	�dB	�jB	�qB	�wB	�wB	�wB	�}B	��B	B	ƨB	ƨB	ƨB	ǮB	ǮB	ȴB	ȴB	ȴB	ȴB	ȴB	ȴB	ȴB	ȴB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�#B	�)B	�)B	�)B	�)B	�/B	�HB	�`B	�sB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
%B
+B
+B
1B
1B
1B
1B
	7B

=B
DB
DB
DB
DB
DB
JB
VB
\B
\B
\B
bB
hB
oB
oB
uB
uB
uB
uB
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
�B
�B
�B
�B
 �B
!�B
!�B
"�B
"�B
"�B
"�B
"�B
#�B
#�B
#�B
$�B
%�B
&�B
&�B
'�B
(�B
)�B
)�B
)�B
)�B
)�B
)�B
)�B
+B
+B
,B
,B
,B
-B
-B
-B
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
2-B
2-B
33B
33B
33B
33B
33B
49B
49B
49B
49B
5?B
5?B
5?B
49B
5?B
6FB
6FB
7LB
7LB
7LB
7LB
7LB
8RB
8RB
8RB
9XB
9XB
9XB
9XB
:^B
;dB
;dB
;dB
;dB
<jB
<jB
=qB
>wB
>wB
>wB
>wB
?}B
?}B
?}B
?}B
?}B
@�B
@�B
A�B
A�B
A�B
A�B
A�B
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
D�B
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
H�B
H�B
H�B
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
J�B
J�B
J�B
J�B
J�B
K�B
K�B
K�B
L�B
L�B
L�B
M�B
M�B
M�B
M�B
M�B
M�B
M�B
M�B
M�B
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
Q�B
Q�B
Q�B
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
S�B
S�B
S�B
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
XB
XB
XB
XB
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
[#B
[#B
[#B
[#B
[#B
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
^5B
^5B
_;B
`BB
`BB
`BB
`BB
`BB
`BB
`BB
`BB
`BB
`BB
`BB
aHB
aHB
aHB
bNB
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
e`B
e`B
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
l�B
l�B
l�B
m�B
m�B
m�B
m�B
m�B
m�B
n�B
n�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B�BoB�BoBoB�BoBoB�BuB�B�B�B�B_BM�B��B�B�B�B� BtBO�BA�B:�B.B)�B)*BjB�B�tB�/B�B��B�{B�`B�FB�"B�Br�BoBk�Ba�BY�BQ�BHB;�B-B#�B�B��B��B�=B�~B֡B�jB�BB��B�qB��B�B��BwfBabBU�BO\BMjBIlBF?BBB?}B9XB3hB'RB�B�B�B
XBMB
�"B
�GB
�cB
�B
�XB
�4B
ޞB
ևB
�0B
��B
�!B
�B
�pB
}qB
o�B
gmB
abB
`�B
`'B
LJB
:DB
*eB
pB
sB
HB
;B	�8B	��B	��B	�hB	̳B	�YB	��B	�;B	�B	��B	�0B	�LB	�/B	�2B	�B	xRB	wfB	sMB	oOB	l�B	hXB	`�B	Q4B	O�B	K�B	HB	C�B	;JB	8B	7B	8RB	3�B	1�B	6�B	88B	/�B	-B	+B	%�B	*�B	+kB	)�B	)B	)B	(>B	$�B	�B	B	bB	:B	"B	UB��B�B��B�xB�lB��B�B��B�wB�qB�0B�fB��BߊB�7B�MB��B��BʌBȚBǔB��BªB��B�9B��B�9B�B��B��B��B��B��B�2B��B��B��B��B��B�UB�OB}<G�O�B{BvFBu?BvBu�Bp�BlqBi_Bg�Bf�Bc�BY�BVmBTBQ�BR BR BO\BP.BJ�BH1BEmBDMB?�B;�B;dB:�B7�B72B7B5?B3�B2�B3B1B1B0�B/�B/�B-)B*KB*B*�B+B+B+�B,B,WB+kB)DB(
B'�B&�B%�B&�B%�B$�B%�B$�B$�B%B$�B$�B"�B%,B$�B%�B&2B%,B%,B$ZB%`B$�B$�B"4B!bB �B#BIB~B�BOBOBB5B5B�B�B \B�B!B#TB"NB"�B#�B$�B$�B%�B&�B'�B(�B+�B-B0�B2B2�B49B5B6B7B8B9�B;B=<B?B?�B@ B@�BA�BA�BB�BC�BF?BH�BL�BOvBQ�BQ�BRBR�BS�BU�BVmBWsBX_BYeBZkBZkB[WB[�B]�B`vBa�Bc�Bd�Bf�Bg�Bg�Bh�Bj�Bj�Bk�Bk�Bl�BmBoBp�Bp�Bq�Bs3Bu?Bw�B}VB~wB��B�B��B��B��B��B��B��B��B��B�B��B��B�B�OB�HB�ZB�LB�XB�_B�KB�qB��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�RB˒BϫBөBیBݲB�hB�B��B��B��B��B��B��B��B��B��B�3B�?B�$B�DB�VB��B	�B	�B		RB	
rB	dB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	B	�B	B	"4B	%`B	+�B	0�B	1�B	4�B	8�B	;�B	=�B	>�B	?�B	B�B	D�B	D�B	E�B	G�B	I�B	J�B	M�B	N�B	O(B	Q4B	Q4B	T,B	V9B	X_B	YKB	ZQB	ZQB	ZQB	[=B	[WB	[=B	\]B	\xB	^�B	abB	b�B	b�B	c�B	g�B	j�B	m�B	o�B	p�B	p�B	r�B	r�B	uB	w2B	z*B	|B	~BB	�OB	�AB	�GB	�MB	�9B	�SB	�SB	�YB	�YB	�fB	�^B	�^B	�jB	��B	��B	��B	��B	��B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�!B	��B	�B	�B	�B	�
B	�
B	�$B	�>B	�KB	�KB	�6B	�"B	�CB	�iB	��B	�tB	��B	��B	�B	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	��B	�B	�B	��B	� B	�B	�B	�,B	�MB	�MB	�B	�B	�?B	�+B	�KB	�=B	�]B	�]B	�CB	�]B	�~B	�B	�B	�B	�B	�B	��B	�B	�B	��B	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	��B	��B	��B	�B	�	B	��B	�B	�B	��B	�B	�PB	�BB	�HB
 4B
;B
[B
aB
aB
-B
gB
mB
YB
_B
_B
KB
fB
fB
�B
	lB

XB
^B
DB
^B
xB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
�B
�B
�B
�B
 �B
!�B
!�B
"�B
"�B
"�B
"�B
#B
#�B
$B
$&B
%,B
%�B
'B
'8B
($B
)*B
)�B
*B
*0B
*B
*B
*B
*0B
+6B
+6B
,=B
,"B
,=B
-CB
-CB
-CB
.IB
.IB
/iB
/5B
/iB
0UB
1vB
2aB
2aB
2aB
2GB
2GB
33B
3MB
3hB
3MB
3hB
4TB
4TB
49B
4nB
5ZB
5?B
5tB
4�B
5tB
6�B
6zB
7fB
7fB
7�B
7�B
7�B
8�B
8lB
8�B
9rB
9rB
9�B
9rB
:�B
;B
;�B
;B
;�B
<�B
<�B
=�B
>�B
>�B
>�B
>�B
?�B
?�B
?�B
?�B
?�B
@�B
@�B
A�B
A�B
A�B
A�B
A�B
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
D�B
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
H�B
H�B
IB
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
J�B
J�B
J�B
J�B
J�B
K�B
K�B
K�B
MB
MB
L�B
NB
NB
M�B
NB
M�B
NB
M�B
NB
NB
OB
O(B
P.B
O�B
O�B
PB
Q B
Q B
QB
QB
R B
RB
RB
R B
R B
R B
S&B
R�B
SB
R�B
SB
S&B
SB
T,B
S�B
TB
T,B
TB
U2B
U2B
U2B
U2B
VB
VB
V9B
V9B
V9B
W?B
W?B
W?B
X+B
XB
XEB
X_B
YKB
YKB
ZQB
Z7B
ZB
Z7B
Z7B
Z7B
Z7B
ZQB
[=B
[=B
[WB
[WB
[WB
\]B
\CB
\]B
\CB
\xB
]~B
^jB
^jB
^jB
^jB
^jB
^jB
_pB
`vB
`vB
`\B
`\B
`\B
`\B
`\B
`BB
`\B
`vB
`vB
abB
a|B
a|B
b�B
b�B
b�B
b�B
cnB
cnB
c�B
dtB
dtB
dtB
d�B
e�B
ezB
ezB
e�B
e�B
f�B
f�B
f�B
f�B
g�B
g�B
g�B
h�B
h�B
h�B
i�B
i�B
i�B
i�B
i�B
jB
j�B
j�B
j�B
j�B
j�B
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
m�B
m�B
m�B
m�B
m�B
m�B
n�B
n�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.15(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201607030036502016070300365020160703003650201806221210162018062212101620180622121016201804050402322018040504023220180405040232  JA  ARFMdecpA19c                                                                20160629093506  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20160629003524  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20160629003525  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20160629003525  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20160629003526  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20160629003526  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20160629003526  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20160629003526  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20160629003526  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20160629003526                      G�O�G�O�G�O�                JA  ARUP                                                                        20160629012154                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20160629153532  CV  JULD            G�O�G�O�F���                JM  ARSQJMQC2.0                                                                 20160630000000  CF  PSAL_ADJUSTED_QCC�  C�  G�O�                JM  ARCAJMQC2.0                                                                 20160702153650  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20160702153650  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180404190232  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20180622031016  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200115101517                      G�O�G�O�G�O�                